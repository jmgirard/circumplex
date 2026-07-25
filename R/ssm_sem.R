# SEM-based SSM (M5): latent-variable estimation + circular-aware CIs (T3).
#
# Estimates the latent (disattenuated) SSM profile of one or more external
# measures from a fitted fixed-theoretical-angle lavaan measurement model, and
# constructs confidence intervals by the draws-through-the-transform route:
# draws of the model's free parameters map to draws of the latent profile,
# which push through the OLS transform and the package's existing
# ssm_replicate_intervals() + circular-quantile machinery unchanged. No new
# interval-assembly code lives here (spec devel/m5-sem-design.md section 5.1);
# section references below trace to that spec.
#
# What is never delegated to lavaan (spec section 2.2): delta-method or
# percentile intervals for amplitude/displacement (the atan2 branch problem).
# lavaan supplies estimates and a covariance (or bootstrap replicates) of its
# free parameters -- nothing else.

# The invariance rung ladder, in order: THE single source for match.arg
# choices, gate/required arithmetic, and the ladder loop (spec section 6.2).
sem_invariance_rungs <- function() {
  c("configural", "metric", "scalar", "strict_residuals")
}

# Under the strict tier every loading is fixed, so the "metric" rung (which
# only constrains loadings across groups) is VACUOUS: it holds by
# construction, is never fitted, and the configural fit already IS the metric
# model. THE single source of that rule, shared by the ladder loop, the
# comparability verdict, and the estimation-fit selection (spec section 6.2).
sem_strict_metric_vacuous <- function(model, rung) {
  model == "strict" && rung == "metric"
}

# THE single source of the contrast-arity messages shared by ssm_sem() and
# ssm_sem_parameters(). A second-minus-first contrast is defined only for
# exactly two things: two groups (with the latent-mean path, or a single
# measure) or, ungrouped, two measures. `n_groups` is 1 on the single-group
# path; `path` is "means" or "measures"; `n_measures` is length(measures)
# (0 for the latent-mean path). Preserves the original base `stop(call. =
# FALSE)` conditions verbatim -- callers keep their surrounding checks and
# call this only when `contrast` is TRUE.
sem_check_contrast_arity <- function(n_groups, path, n_measures) {
  if (n_groups > 1) {
    if (n_groups != 2) {
      stop("Contrast requires exactly two groups (second level minus ",
        "first).",
        call. = FALSE
      )
    }
    if (path == "measures" && n_measures != 1) {
      stop("A group contrast requires exactly one measure (or none, for ",
        "the latent mean path).",
        call. = FALSE
      )
    }
  } else if (n_measures != 2) {
    stop("Contrast requires exactly two measures (second minus first).",
      call. = FALSE
    )
  }
}

# Robust-preferring fitMeasures lookup, shared by the invariance-ladder table
# and the print method so the two surfaces cannot report different flavors of
# the same index for the same object.
sem_fm_pick <- function(fm, ...) {
  for (nm in c(...)) {
    if (nm %in% names(fm)) {
      return(fm[[nm]])
    }
  }
  NA_real_
}

# p-value display, shared by the verdict strings, the ladder table, and the
# print method: a p that rounds to zero at `digits` is displayed as a bound
# ("< 0.001"), never as the statistically improper "p = 0". With
# prose = TRUE the comparator is included ("= 0.043" / "< 0.001") so callers
# can write sprintf("p %s", ...).
sem_fmt_p <- function(p, digits = 3, prose = FALSE) {
  thr <- 10^(-digits)
  vapply(p, function(pi) {
    if (is.na(pi)) {
      return(NA_character_)
    }
    if (pi < thr) {
      paste0("< ", format(thr, scientific = FALSE))
    } else if (prose) {
      paste0("= ", format(round(pi, digits)))
    } else {
      format(round(pi, digits))
    }
  }, character(1))
}

# Structure extraction (spec section 4.1 / 7.2) ---------------------------------

# Locate one parameter-table row by (lhs, op, rhs) within one group's block,
# tolerating either orientation for symmetric (~~) rows. Returns the row index
# or 0 if absent.
sem_pt_row <- function(pt, lhs, op, rhs, group = 1L) {
  in_g <- pt$group == group
  hit <- which(in_g & pt$lhs == lhs & pt$op == op & pt$rhs == rhs)
  if (length(hit) == 0 && op == "~~") {
    hit <- which(in_g & pt$lhs == rhs & pt$op == op & pt$rhs == lhs)
  }
  if (length(hit) == 0) 0L else hit[[1]]
}

# From a fitted lavaan model's parameter table, build the index/value maps the
# estimand map consumes: for every needed parameter, its free-parameter index
# (0 = fixed) and its fixed value. Compatibility is checked structurally --
# the named parameters must be present -- not by provenance (spec section 7.2),
# so fits of user-modified syntax (the partial-invariance escape hatch) work.
sem_structure <- function(fit, scales, measures, group = 1L, means = FALSE) {
  pt <- lavaan::parameterTable(fit)
  p <- length(scales)
  m <- length(measures)
  factors <- c("g", "cx", "cy")

  need <- function(lhs, op, rhs) {
    i <- sem_pt_row(pt, lhs, op, rhs, group = group)
    if (i == 0L) {
      stop(
        "The lavaan fit is not structurally compatible with a fixed-angle ",
        "circumplex SSM measurement model: parameter `", lhs, " ", op, " ",
        rhs, "` is absent",
        if (group > 1L) paste0(" in group ", group) else "",
        ". Fit a model generated by ssm_sem_syntax() (or one preserving its ",
        "parameter structure).",
        call. = FALSE
      )
    }
    i
  }

  lam_row <- matrix(0L, p, 3)
  for (i in seq_len(p)) {
    for (j in seq_len(3)) {
      lam_row[i, j] <- need(factors[[j]], "=~", scales[[i]])
    }
  }
  phi_row <- matrix(0L, 3, 3)
  for (j in seq_len(3)) {
    for (k in j:3) {
      phi_row[j, k] <- phi_row[k, j] <- need(factors[[j]], "~~", factors[[k]])
    }
  }
  sm_row <- matrix(0L, 3, m)
  vm_row <- integer(m)
  for (k in seq_len(m)) {
    for (j in seq_len(3)) {
      sm_row[j, k] <- need(measures[[k]], "~~", factors[[j]])
    }
    vm_row[[k]] <- need(measures[[k]], "~~", measures[[k]])
  }

  # Mean structure (the multi-group latent-mean path, spec section 6.4):
  # scale intercepts nu_i and latent means alpha = (a_g, a_x, a_y)
  nu_row <- alpha_row <- NULL
  if (means) {
    nu_row <- integer(p)
    for (i in seq_len(p)) nu_row[[i]] <- need(scales[[i]], "~1", "")
    alpha_row <- integer(3)
    for (j in seq_len(3)) alpha_row[[j]] <- need(factors[[j]], "~1", "")
  }

  shape <- function(rows, nr) {
    list(
      idx = matrix(pt$free[rows], nr),
      val = matrix(pt$est[rows], nr)
    )
  }

  # Tier is structural: any free loading = scaled, all fixed = strict
  tier <- if (any(pt$free[lam_row] > 0)) "scaled" else "strict"

  # Refuse the known-unidentified configuration: free loadings WITH free
  # g-plane covariances is locally unidentified exactly at phi_g = 0 (the
  # a_i <-> phi_g trade documented in devel/m5-sem-design.md section 3.1), so
  # a user-modified fit that frees them must not be summarized -- its MVN
  # draws run down the flat ridge into garbage.
  if (tier == "scaled" &&
    any(pt$free[c(phi_row[1, 2], phi_row[1, 3])] > 0)) {
    stop(
      "This fit frees the general-plane covariance(s) (g ~~ cx / g ~~ cy) ",
      "alongside free loadings; that model is locally unidentified at zero ",
      "covariance and cannot be summarized. To model a general factor ",
      "leaning into the plane, use the \"strict\" tier (fixed loadings, ",
      "free factor covariance matrix).",
      call. = FALSE
    )
  }

  out <- list(
    lambda = shape(lam_row, p),
    phi = shape(phi_row, 3),
    sm = shape(sm_row, 3),
    vm = shape(vm_row, 1),
    tier = tier
  )
  if (means) {
    out$nu <- shape(nu_row, 1)
    out$alpha <- shape(alpha_row, 1)
  }
  out
}

# Evaluate one structure component over a draws matrix (boots x npar): free
# parameters take their drawn column, fixed parameters their constant value.
# Returns a boots x length(idx) matrix in the component's column-major order.
sem_component <- function(comp, draws) {
  n <- nrow(draws)
  idx <- as.integer(comp$idx)
  val <- as.numeric(comp$val)
  out <- matrix(rep(val, each = n), n, length(val))
  free <- which(idx > 0L)
  if (length(free) > 0) out[, free] <- draws[, idx[free], drop = FALSE]
  out
}

# The estimand map (spec section 4.1): psi -> latent profile rho*. For each
# draw, scale i's common part t_i has Var(t_i) = lambda_i' Phi lambda_i and
# Cov(M_k, t_i) = lambda_i' sigma_Mk; every ingredient (including Var(M),
# model-implied) is evaluated from the drawn parameter vector, so the draws
# propagate standardization uncertainty (the section 4.4 trap). Returns, over
# a boots x npar draws matrix: rho (list per measure of boots x p), var_t
# (boots x p), and v_m (boots x m).
sem_profiles <- function(draws, struct) {
  n <- nrow(draws)
  lam <- sem_component(struct$lambda, draws) # n x 3p (col-major: g, cx, cy blocks)
  phi <- sem_component(struct$phi, draws) # n x 9
  sm <- sem_component(struct$sm, draws) # n x 3m
  vm <- sem_component(struct$vm, draws) # n x m
  p <- ncol(lam) / 3
  m <- ncol(vm)

  lg <- lam[, seq_len(p), drop = FALSE]
  lx <- lam[, p + seq_len(p), drop = FALSE]
  ly <- lam[, 2 * p + seq_len(p), drop = FALSE]
  # Phi entries as draw vectors (symmetric; column-major 3x3)
  f_gg <- phi[, 1]
  f_gx <- phi[, 2]
  f_gy <- phi[, 3]
  f_xx <- phi[, 5]
  f_xy <- phi[, 6]
  f_yy <- phi[, 9]

  # Var(t_i) = lambda_i' Phi lambda_i, vectorized over draws (a boots-length
  # vector recycles down each column of a boots x p matrix)
  var_t <- lg^2 * f_gg + lx^2 * f_xx + ly^2 * f_yy +
    2 * (lg * lx * f_gx + lg * ly * f_gy + lx * ly * f_xy)

  rho <- vector("list", m)
  for (k in seq_len(m)) {
    s_g <- sm[, 3 * (k - 1) + 1]
    s_x <- sm[, 3 * (k - 1) + 2]
    s_y <- sm[, 3 * (k - 1) + 3]
    cov_mt <- lg * s_g + lx * s_x + ly * s_y
    # A nonpositive variance product makes the draw inadmissible (section 4.5);
    # mask it to NA here so no sqrt warning fires -- the engine-side filter
    # downstream drops and reports these draws by cause.
    den <- var_t * vm[, k]
    den[den <= 0] <- NA_real_
    rho[[k]] <- cov_mt / sqrt(den)
  }
  list(rho = rho, var_t = var_t, v_m = vm)
}

# The latent MEAN profile map (multi-group mean path, spec section 6.4):
# mu*_g = nu_g + Lambda_g alpha_g per draw. Linear in the drawn parameters,
# raw-score metric; no standardization ratios, hence no admissibility filter
# beyond finiteness. Returns an n x p matrix of profile draws.
sem_mean_profiles <- function(draws, struct) {
  lam <- sem_component(struct$lambda, draws)
  nu <- sem_component(struct$nu, draws)
  alpha <- sem_component(struct$alpha, draws)
  p <- ncol(lam) / 3
  lg <- lam[, seq_len(p), drop = FALSE]
  lx <- lam[, p + seq_len(p), drop = FALSE]
  ly <- lam[, 2 * p + seq_len(p), drop = FALSE]
  nu + lg * alpha[, 1] + lx * alpha[, 2] + ly * alpha[, 3]
}

# The SSM transform (spec section 2) ---------------------------------------------

# OLS profile -> (e, x, y, a, d, fit) in ssm_param_names() order, d in radians
# in [0, 2*pi). The linear stage is the section 2.1 projection W (equal to the
# closed form at equally spaced angles; genuinely different off harmonic
# balance, where this -- not ssm_parameters_cpp()'s closed form -- is the
# latent estimand's functional). Degenerate-NA semantics mirror
# src/parameters.cpp at the same machine-noise tolerance: flat profile ->
# displacement and fit NA; zero first-harmonic amplitude with real variance ->
# displacement NA, fit exactly 0. Under OLS the fit is a genuine bounded
# R-squared in [0, 1] at any spacing.
sem_ssm_transform <- function(profile, weights, angles_rad) {
  exy <- as.numeric(weights %*% profile)
  e <- exy[[1]]
  x <- exy[[2]]
  y <- exy[[3]]
  a <- sqrt(x^2 + y^2)
  n <- length(profile)
  tol <- 8 * .Machine$double.eps * n * max(abs(profile))
  if (!(stats::sd(profile) > tol)) {
    d <- NA_real_
    fit <- NA_real_
  } else if (a <= tol) {
    d <- NA_real_
    fit <- 0
  } else {
    d <- atan2(y, x) %% (2 * pi)
    pred <- e + x * cos(angles_rad) + y * sin(angles_rad)
    fit <- 1 - sum((pred - profile)^2) / sum((profile - mean(profile))^2)
  }
  c(e = e, x = x, y = y, a = a, d = d, fit = fit)
}

# Vectorized form of sem_ssm_transform() over a draws x k matrix of profiles
# (spec section 9: the transform is vectorized R over a boots x k matrix, no
# per-draw apply). Returns a draws x 6 matrix of (e, x, y, a, d, fit) in
# ssm_param_names() order, reproducing the scalar reference row for row,
# including the section 5.5 degenerate-NA semantics. sem_ssm_transform() stays
# the reference (tested against ssm_parameters()); this is its matrix pass.
sem_ssm_transform_mat <- function(profiles, weights, angles_rad) {
  exy <- profiles %*% t(weights) # draws x 3 = (e, x, y)
  e <- exy[, 1L]
  x <- exy[, 2L]
  y <- exy[, 3L]
  a <- sqrt(x^2 + y^2)
  n <- ncol(profiles)
  mu <- rowMeans(profiles)
  sst <- rowSums((profiles - mu)^2) # (n - 1) * var per row
  sdev <- sqrt(sst / (n - 1))
  # per-row max(abs()) via a column-wise reduction (n small; no per-draw apply,
  # and no asplit() so R (>= 3.4) holds)
  rowmax <- do.call(pmax, lapply(seq_len(n), function(j) abs(profiles[, j])))
  tol <- 8 * .Machine$double.eps * n * rowmax
  d <- atan2(y, x) %% (2 * pi)
  pred <- e + outer(x, cos(angles_rad)) + outer(y, sin(angles_rad))
  fit <- 1 - rowSums((pred - profiles)^2) / sst
  # Degenerate branches mirror the scalar path: flat (no real variance) ->
  # d, fit NA; else zero first-harmonic amplitude -> d NA, fit 0.
  flat <- !(sdev > tol)
  zeroamp <- !flat & (a <= tol)
  d[flat | zeroamp] <- NA_real_
  fit[flat] <- NA_real_
  fit[zeroamp] <- 0
  cbind(e = e, x = x, y = y, a = a, d = d, fit = fit)
}

# Draw engines (spec section 5.1) -------------------------------------------------

# MVN propagation: psi ~ MVN(psi-hat, V-hat) via the package's single
# draw-root convention (mvn_draws/mvn_root). lavaan's vcov of a model with
# linear equality constraints (the fixed-angle direction constraints) is
# singular along the constraints, so the draws satisfy them exactly.
# Consumes the global RNG stream (spec section 5.4).
sem_draws_mvn <- function(fit, boots) {
  psi <- lavaan::coef(fit)
  mvn_draws(boots, as.numeric(psi), as.matrix(lavaan::vcov(fit)))
}

# lavaan bootstrap: a full refit per resample; each replicate's free-parameter
# vector goes through the same map. The seed handed to lavaan is drawn from
# the global stream, so the entry point keeps the package's RNG contract
# (set.seed() immediately before the call reproduces results) regardless of
# lavaan's internal parallel RNG machinery (iseed drives L'Ecuyer streams, so
# results are also reproducible under parallel = "multicore"/"snow").
sem_draws_boot <- function(fit, boots, parallel = "no", ncpus = 1) {
  iseed <- sample.int(.Machine$integer.max, 1)
  reps <- lavaan::bootstrapLavaan(
    fit,
    R = boots, FUN = "coef", iseed = iseed,
    parallel = parallel, ncpus = ncpus
  )
  # Failed/nonconverged replicates: current lavaan keeps all R rows and fills
  # failures with NA (indices also in attr(reps, "error.idx")); older versions
  # removed the rows instead. Handle both by keeping complete rows only -- an
  # NA parameter row must never reach the estimand map.
  keep <- stats::complete.cases(reps)
  n_fail <- (boots - nrow(reps)) + sum(!keep)
  reps <- reps[keep, , drop = FALSE]
  if (nrow(reps) == 0) {
    stop(
      "Every bootstrap replicate failed to converge; no intervals can be ",
      "constructed. The measurement model is likely too unstable at this ",
      "sample size.",
      call. = FALSE
    )
  }
  if (n_fail > 0) {
    warning(
      n_fail, " of ", boots, " bootstrap replicates failed to converge and ",
      "were dropped before interval assembly; the intervals are conditional ",
      "on convergence.",
      call. = FALSE
    )
  }
  reps
}

# Estimation core (spec sections 4.5, 5) -------------------------------------------

# Shared by ssm_sem() and ssm_sem_parameters(): point profiles + guards,
# draws, engine-side admissibility filter, transform, and interval assembly
# through ssm_replicate_intervals() verbatim. Group-aware (T4): a multi-group
# fit yields one profile block per group (per measure on the correlation
# path; the latent mean profile on the mean path), with all draws taken
# JOINTLY from the one free-parameter vector so cross-group dependence -- in
# particular the coupling induced by invariance equality constraints -- is
# carried into every contrast automatically (spec section 5.3).
sem_estimate <- function(fit, scales, angles_deg, measures, ci_method, boots,
                         interval, contrast, parallel = "no", ncpus = 1,
                         path = c("measures", "means")) {
  path <- match.arg(path)
  G <- lavaan::lavInspect(fit, "ngroups")
  group_labels <- if (G > 1) {
    as.character(lavaan::lavInspect(fit, "group.label"))
  } else {
    "All"
  }
  if (path == "means" && G < 2) {
    stop(
      "The latent mean path requires a multi-group fit (factor means are ",
      "not identified in one group).",
      call. = FALSE
    )
  }

  th <- as.numeric(as_radian(as_degree(angles_deg)))
  p <- length(scales)
  m <- length(measures)
  n_prof <- if (path == "measures") m else 1L # profile rows per group
  # Single provenance for the projection weights: derived here from the
  # supplied scales/angles for both entry points, so the two can never
  # silently estimate with different projections for the same fit.
  weights <- sem_ols_weights(th, names = scales)

  structs <- lapply(seq_len(G), function(g) {
    sem_structure(
      fit, scales,
      measures = if (path == "measures") measures else character(0),
      group = g, means = (path == "means")
    )
  })

  # Guard against ACCIDENTAL cross-group equality of the measure blocks: no
  # legitimate rung of the invariance ladder ever constrains the
  # measure-factor covariances or the measure variance across groups (they
  # are the group-specific estimand ingredients). Shared free indices there
  # mean single-group syntax (whose plain labels lavaan replicates as
  # equality constraints) was fitted with group=, forcing the group profiles
  # equal by construction -- the contrast would be spuriously ~0 with a
  # near-zero interval, an estimand nobody intended.
  if (G > 1 && path == "measures") {
    for (g in 2:G) {
      # Two lavaan encodings to detect: label equality as a SHARED free
      # index, or as distinct free parameters tied by explicit `==`
      # constraint rows -- the latter leaves indices distinct but forces the
      # estimates to coincide to solver precision, which independent free
      # parameters estimated from continuous data never do.
      shared <- (structs[[g]]$sm$idx > 0 &
        structs[[g]]$sm$idx == structs[[1]]$sm$idx) |
        (structs[[g]]$sm$idx > 0 & structs[[1]]$sm$idx > 0 &
          abs(structs[[g]]$sm$val - structs[[1]]$sm$val) < 1e-8)
      # The sm block ALONE being fully constrained is already the hazard:
      # with the covariances (and, in the plain-label accident, the
      # loadings) forced equal, the group profiles can differ only by the
      # scalar sqrt(Var(M)_g) -- identical direction, spurious ~0 contrast --
      # whether or not the unlabeled measure variance stayed free.
      if (all(shared)) {
        stop(
          "The measure-factor covariances are ",
          "equality-constrained across groups in this fit. That usually ",
          "means single-group ssm_sem_syntax() output (whose plain labels ",
          "lavaan replicates as cross-group equality constraints) was ",
          "fitted with `group =`; the group profiles are then forced equal ",
          "by construction and no group contrast is estimable. Regenerate ",
          "the model with ssm_sem_syntax(n_groups = ", G, ", invariance = ",
          "...) or use ssm_sem(grouping = ). If the equality was ",
          "intentional, note that it makes the groups' latent profiles ",
          "identical up to a variance rescaling, so neither separate ",
          "profiles nor a contrast are meaningful to report.",
          call. = FALSE
        )
      }
    }
  }

  # The supplied angles must be the angles the model was generated with: the
  # plane loadings lie on the ray at theta_i (up to the harmless reflection),
  # so a mismatch is detectable from the fitted direction, per group. Without
  # this, a forgotten `angles =` silently projects onto the wrong basis.
  dir_th <- th %% pi
  for (g in seq_len(G)) {
    lx <- structs[[g]]$lambda$val[, 2]
    ly <- structs[[g]]$lambda$val[, 3]
    checkable <- sqrt(lx^2 + ly^2) > 1e-8
    dir_fit <- atan2(ly, lx) %% pi
    gap <- pmin(
      abs(dir_fit - dir_th), abs(dir_fit - dir_th - pi),
      abs(dir_fit - dir_th + pi)
    )
    off <- which(checkable & gap > 1e-3)
    if (length(off) > 0) {
      stop(
        "The supplied `angles` do not match the fixed angles in the lavaan ",
        "fit (first mismatch: scale `", scales[[off[[1]]]], "`",
        if (G > 1) paste0(", group `", group_labels[[g]], "`") else "",
        ", fitted direction ", round(dir_fit[[off[[1]]]] * 180 / pi, 2),
        " vs supplied ", round(dir_th[[off[[1]]]] * 180 / pi, 2),
        " degrees, modulo 180). Pass the angles the model was generated ",
        "with.",
        call. = FALSE
      )
    }
  }

  # Point profiles per group, with the section 4.5 point guards (correlation
  # path only: the mean path has no standardization ratios to go inadmissible)
  psi_hat <- matrix(as.numeric(lavaan::coef(fit)), nrow = 1)
  profiles0 <- matrix(NA_real_, G * n_prof, p) # group-major, measure within
  for (g in seq_len(G)) {
    if (path == "measures") {
      point <- sem_profiles(psi_hat, structs[[g]])
      rho0_g <- do.call(rbind, lapply(point$rho, as.numeric)) # m x p
      # Variance conditions checked directly (not just through NaN): with
      # BOTH variances negative their product is positive and rho0 comes out
      # finite but sign-corrupted (the double-Heywood case).
      if (any(!is.finite(rho0_g)) || any(point$var_t <= 0) ||
        any(point$v_m <= 0)) {
        stop(
          "The fitted measurement model implies an undefined latent profile",
          if (G > 1) paste0(" in group `", group_labels[[g]], "`") else "",
          " (nonpositive common-part or measure variance); the model is not ",
          "admissible for latent SSM estimation.",
          call. = FALSE
        )
      }
      over <- which(abs(rho0_g) >= 1 - 1e-12, arr.ind = TRUE)
      if (nrow(over) > 0) {
        stop(
          "The model-implied disattenuated correlation reaches |1| for ",
          "measure `", measures[[over[1, 1]]], "` with scale `",
          scales[[over[1, 2]]], "`",
          if (G > 1) paste0(" in group `", group_labels[[g]], "`") else "",
          "; disattenuated correlations at or beyond 1 indicate ",
          "misspecification of the measurement model, not a profile to ",
          "summarize.",
          call. = FALSE
        )
      }
      profiles0[(g - 1) * n_prof + seq_len(m), ] <- rho0_g
    } else {
      profiles0[g, ] <- as.numeric(sem_mean_profiles(psi_hat, structs[[g]]))
    }
  }

  # Parameter draws: ONE joint block for the whole model (both engines yield
  # rows of the full free-parameter vector)
  draws <- switch(ci_method,
    mvn = sem_draws_mvn(fit, boots),
    boot = sem_draws_boot(fit, boots, parallel = parallel, ncpus = ncpus)
  )
  n_draws <- nrow(draws)

  # Profile draws per group + the engine-side admissibility filter (spec
  # section 4.5): a draw is dropped WHOLE (across all groups and measures)
  # when any of its parameter blocks is inadmissible -- never routed through
  # ssm_replicate_intervals()'s degenerate-replicate warning, whose wording
  # is cause-specific to flat/zero-amplitude profiles. All comparisons are
  # non-finite-safe: this filter is the safety net.
  prof_draws <- vector("list", G * n_prof) # group-major, measure within
  bad_vt <- bad_vm <- bad_rho <- rep(FALSE, n_draws)
  for (g in seq_len(G)) {
    if (path == "measures") {
      prof <- sem_profiles(draws, structs[[g]])
      bad_vt <- bad_vt |
        rowSums(!is.finite(prof$var_t) | prof$var_t <= 0) > 0
      bad_vm <- bad_vm | rowSums(!is.finite(prof$v_m) | prof$v_m <= 0) > 0
      for (k in seq_len(m)) {
        r <- abs(prof$rho[[k]])
        bad_rho <- bad_rho | rowSums(!is.finite(r) | r >= 1) > 0
        prof_draws[[(g - 1) * n_prof + k]] <- prof$rho[[k]]
      }
    } else {
      mu <- sem_mean_profiles(draws, structs[[g]])
      bad_vt <- bad_vt | rowSums(!is.finite(mu)) > 0
      prof_draws[[g]] <- mu
    }
  }
  bad_rho <- bad_rho & !(bad_vt | bad_vm) # undefined rho: the variance's fault
  bad <- bad_vt | bad_vm | bad_rho
  n_bad <- sum(bad)
  if (n_bad > 0) {
    # Share of the draws actually in hand: under ci_method = "boot" the
    # engine may already have dropped nonconvergent replicates, and diluting
    # the denominator with the requested count would loosen the escalation
    # threshold exactly when the bootstrap is struggling.
    frac <- n_bad / n_draws
    msg <- paste0(
      n_bad, " of ", n_draws, " parameter draws were inadmissible (",
      sum(bad_vt), " with a nonpositive common-part variance or non-finite ",
      "profile, ", sum(bad_vm), " with a nonpositive measure variance, ",
      sum(bad_rho), " with a disattenuated correlation at or beyond 1) and ",
      "were dropped whole before interval assembly."
    )
    if (frac > 0.05) {
      stop(
        msg, " More than 5% of draws are inadmissible, so the intervals ",
        "would be unreliable; consider ci_method = \"boot\" or revising the ",
        "measurement model.",
        call. = FALSE
      )
    }
    warning(msg, call. = FALSE)
  }

  # Transform: one replicate row of (e, x, y, a, d, fit) per admissible draw,
  # per profile row (group-major); then the existing interval machinery.
  keep <- which(!bad)
  n_blocks <- G * n_prof
  par_list <- vector("list", n_blocks)
  t0_list <- vector("list", n_blocks)
  for (b in seq_len(n_blocks)) {
    pk <- prof_draws[[b]][keep, , drop = FALSE]
    par_list[[b]] <- sem_ssm_transform_mat(pk, weights, th)
    t0_list[[b]] <- sem_ssm_transform(profiles0[b, ], weights, th)
  }
  t <- do.call(cbind, par_list)
  t0 <- unlist(t0_list, use.names = FALSE)
  if (contrast) {
    # Second minus first (displacement via angular distance), sharing
    # param_diff() so the contrast convention has one definition. Arity is
    # validated upstream: exactly two blocks (two measures in one group, or
    # two groups with one profile row each).
    stopifnot(n_blocks == 2L)
    t <- cbind(t, param_diff(par_list[[2]], par_list[[1]]))
    t0 <- c(t0, param_diff(t0_list[[2]], t0_list[[1]]))
  }

  results <- ssm_replicate_intervals(
    t0 = t0,
    t = t,
    interval = interval,
    contrast = contrast,
    replicate_label = if (ci_method == "mvn") "MVN draws" else
      "bootstrap replicates"
  )

  list(
    results = results, profiles = profiles0, tier = structs[[1]]$tier,
    weights = weights, group_labels = group_labels, path = path
  )
}

# Shared details constructor for the two entry points, so the fields the
# subclass print/summary methods read can never drift between them.
sem_details <- function(boots, interval, missing, angles_deg, contrast,
                        ci_method, path) {
  list(
    boots = boots,
    interval = interval,
    listwise = missing == "listwise",
    missing = missing,
    angles = as_degree(angles_deg),
    contrast = contrast,
    score_type = if (path == "means") "Latent mean" else "Latent",
    method = ci_method
  )
}

# THE label seam for the ssm_sem summary detail lines: maps the stored detail
# CODES (method, missing) to their display labels, so the display vocabulary
# lives in one place rather than inline in summary(). `replicate` is the
# tab-aligned label for the replicate-count line; `missing` names the
# missing-data scheme. Kept out of sem_details() (which stores codes, not
# prose) so the seam is independently testable.
sem_detail_labels <- function(details) {
  list(
    replicate = if (identical(details$method, "mvn")) {
      "\nMVN Draws:\t\t"
    } else {
      "\nBootstrap Refits:\t"
    },
    missing = if (identical(details$missing, "fiml")) {
      "FIML"
    } else {
      "Listwise deletion"
    }
  )
}

# Assemble the labeled results/scores data frames (one row per profile block
# in group-major order plus an optional contrast row), mirroring
# ssm_analyze_means()/ssm_analyze_corrs()'s labeling conventions.
sem_assemble <- function(est, scales, measures, contrast) {
  glabs <- est$group_labels
  G <- length(glabs)
  scores <- as.data.frame(est$profiles)
  colnames(scores) <- scales

  if (est$path == "means") {
    Group <- glabs
    Measure <- rep(NA_character_, G)
    if (contrast) {
      Group <- c(Group, paste0(glabs[[2]], " - ", glabs[[1]]))
      Measure <- c(Measure, NA_character_)
      scores <- rbind(scores, scores[2, ] - scores[1, ])
    }
    Label <- Group
  } else {
    m <- length(measures)
    Group <- rep(glabs, each = m)
    Measure <- rep(measures, times = G)
    if (contrast) {
      if (G > 1) {
        # Group contrast (one measure): second minus first group level
        Group <- c(Group, paste0(glabs[[2]], " - ", glabs[[1]]))
        Measure <- c(Measure, measures[[1]])
      } else {
        # Measure contrast (one group): second minus first measure
        Group <- c(Group, "All")
        Measure <- c(Measure, paste0(measures[[2]], " - ", measures[[1]]))
      }
      scores <- rbind(scores, scores[2, ] - scores[1, ])
    }
    Label <- if (G > 1) paste0(Measure, ": ", Group) else Measure
  }
  list(
    results = cbind(Label, Group, Measure, est$results),
    scores = cbind(Label, Group, Measure, scores)
  )
}

# The invariance ladder (spec section 6.2, as amended at T4) -----------------------

# THE single lavaan::cfa chokepoint for the SEM fit paths: owns the fiml ->
# "ml" / listwise `missing` translation and, for multi-group fits, the explicit
# `group.label` ordering. lavaan's default group order is order of APPEARANCE
# in the data, not factor-level order -- pinning group.label = levels(...)
# keeps the reference group (and the second-minus-first contrast direction)
# tied to factor-level order, per the package's grouping contract (CLAUDE.md).
# `syn` is a ready ssm_sem_syntax() string; `grouping` is a column name in
# `dat` or NULL for the single-group path; `...` forwards user cfa arguments.
sem_fit_cfa <- function(syn, dat, grouping = NULL, estimator, se, missing,
                        ...) {
  args <- list(
    model = syn, data = dat,
    estimator = estimator, se = se,
    missing = if (missing == "fiml") "ml" else "listwise",
    ...
  )
  if (!is.null(grouping)) {
    args$group <- grouping
    args$group.label <- levels(dat[[grouping]])
  }
  do.call(lavaan::cfa, args)
}

# Cheung & Rensvold (2002), general Delta-GFI criterion, pp. 250-251: a CFI
# drop of more than .01 across an adjacent pair of ladder rungs rejects that
# invariance step (alpha = .01, two-group ML simulation; their Table 5, p. 248,
# holds the per-hypothesis 1% tails this rounds).
#
# The DIRECTION is taken from cairn/references/cheung2002.md, not from the
# article's own p. 251 sentence, which states it backwards: it reads
# "smaller than or equal to -0.01 indicates that the null hypothesis of
# invariance should not be rejected", contradicting the Table 5 construction
# its critical values come from (they are the 1% LOWER tails of the simulated
# null-hypothesis Delta-GFI distributions, so a value at or below one is the
# 1%-level evidence AGAINST invariance).
sem_dcfi_cutoff <- -0.01

# Apply the criterion. `in_scope` is the two-group/plain-ML envelope Cheung &
# Rensvold actually simulated (their "Limitations of the Simulation", p. 251:
# two groups, ML estimation, multivariate normal data, Type I error only);
# outside it the difference is still reported but NO verdict is attached --
# they simulated neither >2 groups nor robust indices (which did not exist in
# their study), and inventing a cutoff there would be fabrication, not
# extrapolation. Boundary: a value exactly AT the cutoff RETAINS
# ("Delta-CFI >= -.01 -> the step is retained by this criterion").
sem_dcfi_flag <- function(dcfi, in_scope) {
  out <- rep(NA_character_, length(dcfi))
  if (!isTRUE(in_scope)) {
    return(out)
  }
  ok <- !is.na(dcfi)
  out[ok] <- ifelse(dcfi[ok] < sem_dcfi_cutoff, "reject", "retain")
  out
}

# The attribution + scope block printed beneath the ladder table. The
# attribution and the published scope label accompany the value ALWAYS; out of
# scope the block additionally names why no verdict is given.
sem_dcfi_note <- function(scope) {
  dcfi <- "\u0394CFI"
  head <- paste0(
    dcfi, ": Cheung & Rensvold (2002) criterion, alpha = .01, two-group ML\n",
    "  simulation scope"
  )
  tail <- paste0(
    "Secondary and reported only -- the verdict below gates on the nested\n",
    "  chi-square difference test alone.\n"
  )
  if (isTRUE(scope$in_scope)) {
    paste0(
      head, "; ", dcfi, " < ", format(sem_dcfi_cutoff),
      " rejects that invariance step.\n  ", tail
    )
  } else {
    why <- c(
      if (!isTRUE(scope$cfi_plain)) "robust CFI",
      if (!identical(scope$n_groups, 2L)) paste(scope$n_groups, "groups")
    )
    paste0(
      head, ". The cutoff is NOT validated for this configuration\n  (",
      paste(why, collapse = "; "), "): the value is descriptive only, with ",
      "no binary verdict.\n  ", tail
    )
  }
}

# Fit the rung sequence up to `gate`, run lavaan's own nested-model test
# between adjacent rungs (the scaled difference test under robust estimators,
# via lavTestLRT), and return the table, the verdict, and the fit the
# estimation layer should consume. Under the strict tier the metric rung is
# VACUOUS (all loadings fixed; spec section 6.2 table): it is reported as
# such, never fitted, and holds by construction.
#
# Gating semantics (spec section 6.2, "Step 4 is reported, never required"):
# `required` is the rung the contrast's estimand needs (metric for the
# measure path, scalar for the mean path); `gate` >= `required` is the
# highest rung fitted and REPORTED. Comparability demands that EVERY tested
# rung up through `required` is retained -- a rejection at a lower rung is a
# rejection of the constraints the contrast would be computed under, even if
# a later increment happens to pass. Rungs above `required` never gate; a
# rejection there is reported only. When comparable, the estimation model is
# the REQUIRED rung's fit (exactly the constraints the estimand is defined
# under); when not, the configural fit (separate per-group profiles only).
sem_fit_ladder <- function(dat, scales, angles_deg, measures, grouping,
                           model, gate, required, alpha, estimator, se,
                           missing, ...) {
  n_groups <- nlevels(dat[[grouping]])
  rung_order <- sem_invariance_rungs()
  gate_i <- match(gate, rung_order)
  rungs <- rung_order[seq_len(gate_i)]
  # Without a contrast the caller may legitimately fit less than the path's
  # required rung; comparability is then judged (and reported) at the highest
  # rung actually fitted.
  required <- rung_order[min(match(required, rung_order), gate_i)]

  fits <- list()
  syns <- list()
  rows <- list()
  prev_fit <- NULL
  prev_cfi <- NA_real_
  cfi_plain <- logical(0)
  for (r in rungs) {
    if (sem_strict_metric_vacuous(model, r)) {
      rows[[r]] <- data.frame(
        rung = r, chisq = NA_real_, df = NA_real_, cfi = NA_real_,
        rmsea = NA_real_, dchisq = NA_real_, ddf = NA_real_, p = NA_real_,
        dcfi = NA_real_, cr = NA_character_,
        note = "vacuous (all loadings fixed under the strict tier)"
      )
      next
    }
    syn <- ssm_sem_syntax(
      scales = scales, angles = angles_deg, measures = measures,
      model = model, n_groups = n_groups, invariance = r
    )
    fit <- sem_fit_cfa(
      syn, dat, grouping = grouping,
      estimator = estimator, se = se, missing = missing, ...
    )
    if (!lavaan::lavInspect(fit, "converged")) {
      stop(
        "The ", r, " invariance model did not converge; the ladder cannot ",
        "be evaluated. Consider model = \"strict\", larger groups, or ",
        "examining the instrument's geometry with cpm_fit().",
        call. = FALSE
      )
    }
    fits[[r]] <- fit
    syns[[r]] <- syn
    fm <- lavaan::fitMeasures(fit)
    dchisq <- ddf <- pval <- NA_real_
    if (!is.null(prev_fit)) {
      # lavaan's own nested test: plain LRT under ML, the scaled difference
      # test under robust (MLR-family) estimators -- never a naive
      # difference of scaled chi-squares (spec section 6.2)
      lrt <- lavaan::lavTestLRT(prev_fit, fit)
      dchisq <- lrt[2, "Chisq diff"]
      ddf <- lrt[2, "Df diff"]
      pval <- lrt[2, "Pr(>Chisq)"]
    }
    # Delta-CFI (Cheung & Rensvold 2002) differences exactly the CFI the table
    # DISPLAYS -- plain cfi under ML, cfi.robust under MLR -- against the last
    # FITTED rung, the same pairing the nested test above uses. Whether that
    # CFI is the plain normal-theory one decides the criterion's scope below.
    cfi <- sem_fm_pick(fm, "cfi.robust", "cfi.scaled", "cfi")
    cfi_plain <- c(
      cfi_plain, !any(c("cfi.robust", "cfi.scaled") %in% names(fm))
    )
    rows[[r]] <- data.frame(
      rung = r,
      chisq = sem_fm_pick(fm, "chisq.scaled", "chisq"),
      df = sem_fm_pick(fm, "df.scaled", "df"),
      cfi = cfi,
      rmsea = sem_fm_pick(fm, "rmsea.robust", "rmsea.scaled", "rmsea"),
      dchisq = dchisq, ddf = ddf, p = pval,
      dcfi = if (is.null(prev_fit)) NA_real_ else cfi - prev_cfi,
      cr = NA_character_,
      note = ""
    )
    prev_fit <- fit
    prev_cfi <- cfi
  }
  table <- do.call(rbind, rows)
  rownames(table) <- NULL

  # The Cheung & Rensvold envelope: two groups AND the plain (normal-theory)
  # CFI. Gating on the STATISTIC rather than on `estimator` keeps the label
  # tied to the quantity actually differenced, so a robust index can never be
  # flagged against a cutoff simulated for a normal-theory one.
  dcfi_scope <- list(
    n_groups = as.integer(n_groups),
    cfi_plain = length(cfi_plain) > 0 && all(cfi_plain),
    in_scope = FALSE
  )
  dcfi_scope$in_scope <- identical(dcfi_scope$n_groups, 2L) &&
    dcfi_scope$cfi_plain
  table$cr <- sem_dcfi_flag(table$dcfi, dcfi_scope$in_scope)

  # Comparability: EVERY tested rung up through `required` must be retained.
  # Configural has no test; the strict tier's vacuous metric rung holds by
  # construction. A rejection at any tested rung <= required rejects the
  # constraints the contrast would be computed under -- even if a later
  # increment happens to pass, gating on that later increment alone would be
  # anti-conservative. Rungs above `required` are reported, never required.
  req_i <- match(required, rung_order)
  fmt_test <- function(row) {
    sprintf(
      "%s(%g) = %s, p %s", "\u0394\u03c7\u00b2", row$ddf,
      format(round(row$dchisq, 2)), sem_fmt_p(row$p, 4, prose = TRUE)
    )
  }
  gating <- table[match(table$rung, rung_order) <= req_i &
    !nzchar(table$note) & table$rung != "configural", , drop = FALSE]
  untestable <- gating[is.na(gating$p), , drop = FALSE]
  tested <- gating[!is.na(gating$p), , drop = FALSE]
  failed <- tested[tested$p < alpha, , drop = FALSE]
  # An NA p at a gating rung (e.g., a scaled-difference test lavaan could
  # not compute) is NOT a rejection -- but it is also not a retention:
  # comparability cannot be established, and the verdict must say that
  # rather than assert a hypothesis test that never happened.
  comparable <- nrow(failed) == 0 && nrow(untestable) == 0
  if (required == "configural" ||
    sem_strict_metric_vacuous(model, required)) {
    # Nothing testable at or below the required rung
    comparable <- TRUE
    verdict <- if (required == "configural") {
      "configural gate: no cross-group constraints required"
    } else {
      paste0(
        "metric invariance is imposed by the strict tier's fixed loadings ",
        "(not testable); it holds by construction"
      )
    }
  } else if (comparable) {
    verdict <- paste0(
      paste(
        sprintf(
          "%s invariance retained (%s >= alpha = %s)",
          tested$rung, vapply(seq_len(nrow(tested)), function(i) {
            fmt_test(tested[i, ])
          }, character(1)), format(alpha)
        ),
        collapse = "; "
      )
    )
  } else if (nrow(failed) > 0) {
    first_fail <- failed[1, ]
    verdict <- sprintf(
      "%s invariance rejected (%s, alpha = %s): these groups cannot be compared on this instrument's latent metric",
      first_fail$rung, fmt_test(first_fail), format(alpha)
    )
  } else {
    verdict <- paste0(
      "the ", untestable$rung[[1]], " nested test could not be computed ",
      "(lavaan returned NA); comparability cannot be established, so the ",
      "latent contrast is not computed. Inspect the ladder fits directly."
    )
  }
  # Rejections ABOVE the required rung: reported, never gating
  above <- table[!is.na(table$p) &
    match(table$rung, rung_order) > req_i & table$p < alpha, , drop = FALSE]
  if (comparable && nrow(above) > 0) {
    verdict <- paste0(
      verdict, "; the ", paste(above$rung, collapse = ", "),
      " rung(s) were additionally rejected (reported only -- not required ",
      "for this contrast, whose estimand is defined at the ", required,
      " level)"
    )
  }

  # The estimation model: the REQUIRED rung's fit when comparable (exactly
  # the constraints the estimand is defined under; for the strict tier's
  # vacuous metric requirement, the configural fit IS the metric model);
  # the configural fit -- separate per-group profiles only -- when the gate
  # fails (spec section 6.3).
  req_fit_name <- if (sem_strict_metric_vacuous(model, required)) {
    "configural"
  } else {
    required
  }
  fit_est <- if (comparable) fits[[req_fit_name]] else fits[["configural"]]
  syn_est <- if (comparable) syns[[req_fit_name]] else syns[["configural"]]

  list(
    table = table, comparable = comparable, verdict = verdict,
    gate = gate, required = required, alpha = alpha,
    dcfi_scope = dcfi_scope, fit = fit_est, syntax = syn_est
  )
}

# Subclass constructor (spec section 7.3) -----------------------------------------

new_ssm_sem <- function(results, scores, details, call, sem, invariance,
                        model) {
  out <- new_ssm(
    results = results,
    scores = scores,
    details = details,
    call = call,
    sem = sem,
    invariance = invariance,
    model = model
  )
  class(out) <- c("circumplex_ssm_sem", class(out))
  out
}

# Exported entry points -----------------------------------------------------------

#' Perform SEM-based (latent-variable) SSM analyses
#'
#' Estimate the Structural Summary Method profile that one or more external
#' measures show against the \emph{latent} circumplex content of a set of
#' scales -- the disattenuated analog of the correlation-based
#' [ssm_analyze()] -- from a structural equation model with the scale angles
#' held fixed at their theoretical values. The measurement model is generated
#' by [ssm_sem_syntax()] and fitted with \pkg{lavaan} on raw covariances;
#' confidence intervals for all SSM parameters are constructed in-package by
#' propagating draws of the model's free parameters through the profile and
#' SSM transforms and applying the same percentile/circular-quantile machinery
#' as [ssm_analyze()]. No lavaan delta-method or percentile interval is ever
#' used for amplitude or displacement (their intervals must respect the
#' angular branch cut, which lavaan's `:=` machinery does not).
#'
#' The latent profile of a measure is its vector of model-implied
#' \emph{disattenuated} correlations with each scale's common (circumplex)
#' content: the scale's error and unique parts are removed from the
#' denominator, and the covariance is restricted to common content in the
#' numerator. All latent quantities are conditional on the fixed-angle
#' measurement model being adequate: global fit is reported by `print()`, and
#' a poorly fitting measurement model makes the latent SSM parameters
#' uninterpretable, not merely imprecise. The fixed angles are theoretical
#' claims, not estimates (use [cpm_fit()] to examine an instrument's real
#' geometry). Latent displacement is the first-harmonic direction of the
#' saturation-modulated disattenuated profile -- heterogeneous scale
#' saturations rotate it exactly as they rotate the observed displacement;
#' the latent layer removes the reliability modulation, nothing more.
#' Model-implied disattenuated correlations at or beyond 1 indicate
#' misspecification and are refused rather than summarized.
#'
#' The point estimates and intervals are reported for elevation, x-value,
#' y-value, amplitude, and displacement (no standard errors are printed
#' anywhere, matching the package's estimate-plus-interval reporting surface).
#' Unlike [ssm_analyze()]'s closed-form estimator, the latent transform is the
#' ordinary-least-squares projection onto the cosine basis, so for unequally
#' spaced angles the two functionals genuinely differ (they coincide exactly
#' for equally spaced angles, and more generally under first- and
#' second-harmonic balance); under OLS the fit value is a bounded R-squared in
#' `[0, 1]` at any spacing.
#'
#' With `grouping`, the latent contrast this function computes and the observed
#' contrast that [ssm_analyze()] computes answer different questions and are not
#' substitutes. The *observed* contrast ([ssm_analyze()] with grouping) asks
#' whether the groups' *measured* profiles differ: it is a difference of SSM
#' parameters computed from each group's observed scores or correlations. It
#' confounds structural difference, differential reliability, and measurement
#' non-invariance -- that is a property of its estimand, documented rather than
#' a defect, and it requires no invariance assumption. The *latent* contrast
#' (`ssm_sem()` with grouping) asks whether the groups' *constructs* differ,
#' granted the instrument measures the same thing in both groups: it is a
#' contrast on latent SSM parameters computed under cross-group equality
#' constraints, disattenuated and conditional on measurement invariance. When
#' the required invariance rung is rejected the latent contrast is not "more
#' principled" -- it is misspecified, and the function therefore returns an
#' explicit non-comparison (the verdict plus each group's separate configural
#' profile; no contrast is computed or rendered by any method). Neither estimand
#' replaces the other; they answer different questions and can legitimately
#' disagree.
#'
#' The displacement contrast is reported as the second group level minus the
#' first, in `(-180, 180]` degrees, with branch-aligned circular intervals
#' (endpoints may legitimately exceed +/-180 degrees near the boundary). Under
#' the scaled tier the general-plane covariances are fixed to zero in all groups
#' at all rungs (a stationarity-type assumption): a cross-group difference in a
#' general factor's lean into the plane surfaces as misfit, and the strict tier
#' is the tier that can express it. Under the strict tier the metric rung is
#' vacuous (all loadings fixed) and is reported as such.
#'
#' @param data Required. A data frame or matrix containing at least the
#'   circumplex scales and measures.
#' @param scales Required. A character vector of column names, or a numeric
#'   vector of column indexes, from `data` that contains the circumplex scale
#'   scores.
#' @param angles Optional. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales`, in degrees
#'   (default = `octants()`). The angles are fixed theoretical constants in the
#'   measurement model, never free parameters.
#' @param measures Optional with `grouping`, required otherwise. A character
#'   vector (or numeric indexes) of one or more columns of `data` to be
#'   related to the latent circumplex content (the disattenuated correlation
#'   path). With `grouping` and `measures = NULL`, the latent MEAN path is
#'   analyzed instead: each group's model-implied latent mean profile, on the
#'   raw-score metric. (A single-group latent mean profile is not a product:
#'   factor means are not identified in one group.)
#' @param grouping Optional. A string naming the column of `data` indicating
#'   group membership. With `grouping`, the fixed-angle measurement model is
#'   fitted as a multi-group model under an invariance ladder
#'   (configural, then metric, then -- when required -- scalar), and the
#'   latent SSM profiles are reported per group. The FIRST factor level is
#'   the reference group. With `measures = NULL` and `grouping`, the latent
#'   MEAN path is analyzed (each group's model-implied latent mean profile).
#' @param invariance Optional. The highest invariance rung to fit and REPORT
#'   (`"configural"`, `"metric"`, `"scalar"`, or `"strict_residuals"`).
#'   `NULL` (default) uses the path's required rung: `"metric"` for the
#'   measure-profile path, `"scalar"` for the latent mean path. A group
#'   contrast is only computed if EVERY tested rung up through the path's
#'   required rung is retained by lavaan's own nested test (the scaled
#'   difference test under robust estimators) -- a rejection at any lower
#'   rung rejects the constraints the contrast would be computed under.
#'   Rungs fitted ABOVE the required one are reported but never gate the
#'   contrast. On rejection, the returned object states the non-comparison
#'   and reports each group's separate configural profile instead (no
#'   contrast is rendered by any method). Under the strict tier the metric
#'   rung is vacuous (all loadings fixed) and is reported as such.
#'
#'   The ladder table also reports `dcfi`, the change in CFI from the previous
#'   fitted rung (`NA` for configural and for the strict tier's vacuous metric
#'   rung), as a labeled **secondary** criterion: Cheung and Rensvold's (2002)
#'   general rule rejects an invariance step when CFI drops by more than .01
#'   (their alpha = .01). It is reported and never gates: `comparable`, the
#'   verdict, and the fit the estimation layer consumes are decided by the
#'   nested test alone, and the two criteria can legitimately disagree (a
#'   change in CFI is insensitive to sample size where the nested test is not).
#'   The retain/reject label prints **only inside the envelope that simulation
#'   covers** -- exactly two groups and the plain normal-theory CFI (that is,
#'   `estimator = "ML"`; the default `"MLR"` yields a robust CFI). Under a
#'   robust CFI or more than two groups the `dcfi` value still prints, with a
#'   note that the cutoff is not validated for that configuration and no
#'   verdict attached. Cheung and Rensvold simulated two groups, ML estimation,
#'   multivariate normal data, and Type I error only; robust CFI variants were
#'   not in their study, so no cutoff here was validated for one.
#' @param invariance_alpha Optional. The alpha level for the invariance
#'   gating decision (default = 0.05). The gate is a modeling decision with
#'   a default test, not an oracle; the invariance table is always returned
#'   so other criteria can be applied.
#' @param contrast Optional. A logical (default = FALSE) requesting a
#'   difference of latent SSM parameters, always second minus first with the
#'   displacement contrast in `(-180, 180]` degrees. Without `grouping`:
#'   exactly two `measures` (second measure minus first). With `grouping`:
#'   exactly two groups (second factor level minus first) and at most one
#'   measure -- one measure gives the group contrast on that measure's
#'   latent profile, none gives the latent mean-path group contrast. The
#'   group contrast is invariance-gated; see `invariance`.
#' @param model Optional. The measurement-model tier passed to
#'   [ssm_sem_syntax()]: `"scaled"` (default) or `"strict"`.
#' @param ci_method Optional. How to generate parameter replicates: `"mvn"`
#'   (default) draws from a multivariate normal with lavaan's asymptotic
#'   covariance of the free parameters (fast; one model fit); `"boot"` refits
#'   the model on `boots` bootstrap resamples via
#'   [lavaan::bootstrapLavaan()] (slow; robust to the normal approximation).
#'   Both engines feed the same in-package interval machinery.
#' @param boots Optional. A single positive whole number indicating how many
#'   draws or bootstrap refits to use (default = 2000).
#' @param interval Optional. A single number between 0 and 1 (exclusive)
#'   indicating the confidence level (default = 0.95).
#' @param estimator Optional. The lavaan estimator (default = "MLR": maximum
#'   likelihood with robust "Huber-White" standard errors and a scaled test
#'   statistic, the standard choice for the skewed distributions typical of
#'   circumplex scale scores). The parameter estimates are identical to
#'   `"ML"`; what changes is the covariance the `"mvn"` engine propagates
#'   (already robust via `se`) and the test statistic behind the global fit
#'   indices that `print()` reports (robust/scaled versions are used when
#'   available).
#' @param se Optional. The lavaan standard-error method for the fitted model
#'   (default = "robust.huber.white", the sandwich estimator). This does not
#'   affect the parameter estimates, only the covariance the `"mvn"` engine
#'   propagates: the fixed-angle measurement model is an approximation for
#'   real data, and the package's coverage validation found that
#'   sandwich-based draws keep the intervals calibrated for the
#'   model-conditional estimand under that misspecification where the plain
#'   ML covariance undercovers (displacement ~0.88 instead of 0.95, not
#'   improving with n). Set `se = "standard"` for the classical ML
#'   covariance.
#' @param missing Optional. Either `"listwise"` (default; complete cases) or
#'   `"fiml"` (full-information maximum likelihood via lavaan's
#'   `missing = "ml"`).
#' @param parallel,ncpus Optional. Passed to [lavaan::bootstrapLavaan()] when
#'   `ci_method = "boot"` (defaults `"no"` and 1): the bootstrap refits are
#'   independent and can be distributed across cores. Results for a given
#'   `set.seed()` are reproducible regardless of these settings (the seed
#'   lavaan receives drives its own parallel-safe RNG streams). Ignored by
#'   the `"mvn"` engine.
#' @param ... Optional. Additional arguments passed to [lavaan::cfa()] (e.g.,
#'   `bounds` or estimator-control settings).
#' @return A `circumplex_ssm_sem` object (a subclass of `circumplex_ssm`, so
#'   [ssm_table()] and the `ssm_plot_*` functions work on it), containing
#'   `results` (estimates and intervals), `scores` (the latent profile
#'   vectors), `details`, `call`, plus `sem` (the fitted lavaan model: the
#'   gate rung's fit for grouped analyses, or the configural fit when the
#'   gate was rejected), `invariance` (for grouped analyses: the ladder
#'   table -- including the `dcfi` column and its `cr` retain/reject label,
#'   `NA` outside the criterion's validated scope -- the `comparable` flag,
#'   the verdict text, the gate and required rungs, the alpha used, and
#'   `dcfi_scope` recording that scope; `NULL` for single-group analyses),
#'   and `model` (tier,
#'   generated syntax for single-group fits, and the OLS projection
#'   weights).
#'
#'   Inadmissible parameter draws (a nonpositive common-part or measure
#'   variance, or a disattenuated correlation at or beyond 1) are dropped
#'   whole with a warning naming the causes; if more than 5% of draws are
#'   inadmissible the analysis stops with advice to use `ci_method = "boot"`
#'   or revise the model. Degenerate profiles (flat or zero-amplitude) keep
#'   the same per-parameter `NA` contract as [ssm_analyze()].
#' @section Reproducibility:
#'   This function consumes R's random number stream for both `ci_method`
#'   settings (`"mvn"` through the package's own draws; `"boot"` through a
#'   seed handed to lavaan's bootstrap). Call `set.seed()` immediately before
#'   `ssm_sem()` for reproducible confidence intervals.
#' @references Cheung, G. W., & Rensvold, R. B. (2002). Evaluating
#'   goodness-of-fit indexes for testing measurement invariance.
#'   \emph{Structural Equation Modeling}, 9(2), 233-255. (The `dcfi` secondary
#'   criterion. Their p. 251 sentence states the direction of the -.01 rule
#'   backwards relative to their own Table 5, whose critical values are the 1%
#'   lower tails of the simulated null distributions; this package follows the
#'   simulation.)
#' @family ssm functions
#' @family analysis functions
#' @seealso [ssm_analyze()] for the observed-score SSM, [ssm_sem_syntax()] for
#'   the generated measurement model, and [ssm_sem_parameters()] to reuse a
#'   lavaan fit you have modified or fitted yourself.
#' @export
#' @examplesIf requireNamespace("lavaan", quietly = TRUE)
#' \donttest{
#' data("jz2017")
#' set.seed(12345)
#' res <- ssm_sem(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   measures = "NARPD",
#'   boots = 500
#' )
#' res
#' summary(res)
#' }
ssm_sem <- function(data, scales, angles = octants(), measures = NULL,
                    grouping = NULL, contrast = FALSE,
                    model = c("scaled", "strict"), invariance = NULL,
                    invariance_alpha = 0.05,
                    ci_method = c("mvn", "boot"), boots = 2000,
                    interval = 0.95, estimator = "MLR",
                    se = "robust.huber.white",
                    missing = c("listwise", "fiml"),
                    parallel = "no", ncpus = 1, ...) {
  call <- match.call()
  require_lavaan()

  model <- match.arg(model)
  ci_method <- match.arg(ci_method)
  missing <- match.arg(missing)
  stopifnot(is_char(se, n = 1))
  parallel <- match.arg(parallel, c("no", "multicore", "snow"))
  stopifnot(is_scalar_count(ncpus))
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(is_null_or_var(measures))
  stopifnot(is_flag(contrast))
  stopifnot(is_scalar_count(boots))
  stopifnot(is.numeric(interval) && interval > 0 && interval < 1)
  stopifnot(is_char(estimator, n = 1))
  stopifnot(is_null_or_var(grouping, n = 1))
  stopifnot(
    is.numeric(invariance_alpha), invariance_alpha > 0, invariance_alpha < 1
  )
  if (!is.null(invariance)) {
    invariance <- match.arg(invariance, sem_invariance_rungs())
  }
  if (is.null(grouping)) {
    if (!is.null(invariance)) {
      stop("`invariance` requires `grouping`: the invariance ladder is a ",
        "multi-group workflow.",
        call. = FALSE
      )
    }
    if (is.null(measures)) {
      stop(
        "`measures` is required without `grouping`: the single-group latent ",
        "SSM is the correlation path. (A single-group latent mean profile ",
        "is not a product: factor means are not identified in one group.)",
        call. = FALSE
      )
    }
    if (contrast) {
      sem_check_contrast_arity(1L, "measures", length(measures))
    }
  }

  if (is.matrix(data)) data <- as.data.frame(data)

  # Drop observations with missing grouping values (mirrors ssm_analyze())
  if (!is.null(grouping)) {
    na_group <- is.na(data[[grouping]])
    if (any(na_group)) {
      message(
        sum(na_group),
        " observation(s) removed due to missing values in the grouping ",
        "variable."
      )
      data <- data[!na_group, , drop = FALSE]
      if (nrow(data) == 0) {
        stop("No observations remain after removing missing grouping values.")
      }
    }
  }

  scales_data <- data[scales]
  scales_names <- colnames(scales_data)
  measures_data <- data[measures]
  measures_names <- colnames(measures_data)
  dat <- cbind(scales_data, measures_data)
  path <- if (is.null(measures)) "means" else "measures"

  if (!is.null(grouping)) {
    dat$.ssm_group <- factor(data[[grouping]])
    n_groups <- nlevels(dat$.ssm_group)
    if (n_groups < 2) {
      stop("`grouping` must have at least two levels.", call. = FALSE)
    }
    if (contrast) {
      sem_check_contrast_arity(n_groups, path, length(measures))
    }
  }
  if (missing == "listwise") dat <- stats::na.omit(dat)
  if (!is.null(grouping)) {
    # Listwise deletion can empty a whole group; an empty level passed to
    # lavaan as a group.label fails cryptically, so recheck here.
    dat$.ssm_group <- droplevels(dat$.ssm_group)
    if (nlevels(dat$.ssm_group) < n_groups) {
      stop(
        "After listwise deletion, ", n_groups - nlevels(dat$.ssm_group),
        " group(s) have no complete observations left; a multi-group model ",
        "cannot be fitted. Check the missing-data pattern or use ",
        "missing = \"fiml\".",
        call. = FALSE
      )
    }
  }

  if (is.null(grouping)) {
    # Single-group correlation path (T3), unchanged
    syn <- ssm_sem_syntax(
      scales = scales_names, angles = as.numeric(angles),
      measures = measures_names, model = model
    )
    fit <- sem_fit_cfa(
      syn, dat, estimator = estimator, se = se, missing = missing, ...
    )
    sem_health_gate(fit)
    ladder <- NULL
    eff_contrast <- contrast
  } else {
    # Multi-group: fit the invariance ladder up to the gate -- the path's
    # required rung by default (metric for the measure-profile contrast,
    # scalar for the latent mean path; spec section 6.2 gating rule /
    # review F8), or the user-requested rung.
    gate <- if (is.null(invariance)) {
      if (path == "means") "scalar" else "metric"
    } else {
      invariance
    }
    required <- if (path == "means") "scalar" else "metric"
    gate_i <- match(gate, sem_invariance_rungs())
    req_i <- match(required, sem_invariance_rungs())
    if (contrast && gate_i < req_i) {
      stop(
        "The latent ", if (path == "means") "mean" else "measure-profile",
        " contrast requires the ladder to reach the \"", required,
        "\" rung; `invariance = \"", gate, "\"` is insufficient for ",
        "`contrast = TRUE` (spec gating rule).",
        call. = FALSE
      )
    }
    ladder <- sem_fit_ladder(
      dat,
      scales = scales_names, angles_deg = as.numeric(angles),
      measures = measures_names, grouping = ".ssm_group", model = model,
      gate = gate, required = required, alpha = invariance_alpha,
      estimator = estimator, se = se, missing = missing, ...
    )
    fit <- ladder$fit
    syn <- ladder$syntax
    sem_health_gate(fit)
    eff_contrast <- contrast && ladder$comparable
    if (contrast && !ladder$comparable) {
      warning(
        "The latent contrast was NOT computed: ", ladder$verdict, ". The ",
        "returned object reports each group's separate (configural) latent ",
        "profile; the observed-score contrast (ssm_analyze()) remains ",
        "available and answers its own, different question.",
        call. = FALSE
      )
    }
  }

  est <- sem_estimate(
    fit,
    scales = scales_names, angles_deg = as.numeric(angles),
    measures = measures_names, ci_method = ci_method, boots = boots,
    interval = interval, contrast = eff_contrast, parallel = parallel,
    ncpus = ncpus, path = path
  )
  parts <- sem_assemble(est, scales_names, measures_names, eff_contrast)

  details <- sem_details(
    boots, interval, missing, as.numeric(angles), eff_contrast, ci_method,
    path
  )

  new_ssm_sem(
    results = parts$results,
    scores = parts$scores,
    details = details,
    call = call,
    sem = fit,
    invariance = if (is.null(ladder)) NULL else c(
      ladder[c(
        "table", "comparable", "verdict", "gate", "required", "alpha",
        "dcfi_scope"
      )],
      list(contrast_requested = contrast)
    ),
    model = list(tier = model, syntax = syn, weights = est$weights)
  )
}

# Global model health before any SSM output (spec section 4.5); shared by the
# single-group and ladder paths.
sem_health_gate <- function(fit) {
  if (!lavaan::lavInspect(fit, "converged")) {
    stop(
      "The measurement model did not converge; no latent SSM parameters ",
      "can be reported. Consider model = \"strict\", a different estimator, ",
      "or examining the instrument's geometry with cpm_fit().",
      call. = FALSE
    )
  }
  post_ok <- tryCatch(
    suppressWarnings(lavaan::lavInspect(fit, "post.check")),
    error = function(e) TRUE
  )
  if (!isTRUE(post_ok)) {
    warning(
      "lavaan's post-estimation checks flagged the fitted measurement model ",
      "(e.g., a negative variance estimate); interpret the latent SSM ",
      "parameters with caution and inspect the fit stored in `$sem`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Calculate latent SSM parameters from a fitted lavaan measurement model
#'
#' The low-level adapter behind [ssm_sem()]: take an already fitted
#' \pkg{lavaan} model of a fixed-angle circumplex measurement structure (as
#' generated by [ssm_sem_syntax()], possibly user-modified -- e.g., a
#' partial-invariance respecification) and compute latent SSM parameter
#' estimates with in-package confidence intervals. Compatibility with the
#' expected parameter structure is checked structurally (the named loading,
#' factor-covariance, and measure-covariance parameters must be present), not
#' by provenance.
#'
#' **Important:** multi-group fits are supported here as the partial-invariance
#' escape hatch, and this path *bypasses* the invariance gating that
#' [ssm_sem()] applies. Where [ssm_sem()] fits a configural-metric-scalar
#' ladder and refuses a latent group contrast when the required rung is
#' rejected, `ssm_sem_parameters()` computes the contrast from whatever
#' multi-group fit you supply without testing invariance at all. You own the
#' comparability claim: the groups are compared on this instrument's latent
#' metric only to the extent the model you fitted makes them comparable.
#'
#' @param fit Required. A fitted lavaan object whose model preserves the
#'   [ssm_sem_syntax()] parameter structure (factors `g`, `cx`, `cy`; the
#'   measures covarying with them). For `ci_method = "mvn"`, fit the model
#'   with robust (sandwich) standard errors (lavaan's
#'   `se = "robust.huber.white"`, [ssm_sem()]'s default) so the propagated
#'   covariance stays valid when the fixed-angle model is an approximation;
#'   see the `se` argument of [ssm_sem()].
#' @param scales Required. A character vector with the scale (indicator)
#'   names, in the same order as `angles`.
#' @param angles Optional. A numeric vector of the scales' theoretical angles
#'   in degrees (default = `octants()`). Must be the angles the model was
#'   generated with.
#' @param measures Optional for multi-group fits, required otherwise. A
#'   character vector of the measure names; `NULL` on a multi-group fit
#'   selects the latent MEAN path (the fit must carry the mean structure:
#'   scale intercepts and latent means).
#' @param ci_method,boots,interval,contrast,parallel,ncpus See [ssm_sem()].
#'   Note that for a multi-group fit the CONTRAST DIRECTION (and the group
#'   labels in the output) follows the fit's own group order -- lavaan's
#'   default is order of appearance in the data unless `group.label` was
#'   supplied at fitting time -- so read the direction from the output's
#'   `Group` column, not from factor-level conventions.
#' @return A `circumplex_ssm_sem` object; see [ssm_sem()].
#' @inheritSection ssm_sem Reproducibility
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examplesIf requireNamespace("lavaan", quietly = TRUE)
#' \donttest{
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' syn <- ssm_sem_syntax(scales = scales, angles = octants(), measures = "NARPD")
#' # Robust (sandwich) SEs so the mvn engine propagates a
#' # misspecification-consistent covariance (ssm_sem()'s default)
#' fit <- lavaan::cfa(syn, data = jz2017, se = "robust.huber.white")
#' set.seed(12345)
#' ssm_sem_parameters(fit, scales = scales, measures = "NARPD", boots = 500)
#' }
ssm_sem_parameters <- function(fit, scales, angles = octants(),
                               measures = NULL,
                               ci_method = c("mvn", "boot"), boots = 2000,
                               interval = 0.95, contrast = FALSE,
                               parallel = "no", ncpus = 1) {
  call <- match.call()
  require_lavaan()
  ci_method <- match.arg(ci_method)
  stopifnot(inherits(fit, "lavaan"))
  stopifnot(is_char(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(is_null_or_char(measures))
  stopifnot(is_scalar_count(boots))
  stopifnot(is.numeric(interval) && interval > 0 && interval < 1)
  stopifnot(is_flag(contrast))
  ngroups <- lavaan::lavInspect(fit, "ngroups")
  path <- if (is.null(measures)) "means" else "measures"
  if (path == "means" && ngroups < 2) {
    stop(
      "`measures` is required for a single-group fit: the latent mean path ",
      "needs a multi-group fit (factor means are not identified in one ",
      "group).",
      call. = FALSE
    )
  }
  if (contrast) {
    # NOTE a grouped user-supplied fit bypasses the invariance gating that
    # ssm_sem() applies -- that is the escape hatch's documented purpose
    # (partial-invariance respecifications).
    sem_check_contrast_arity(ngroups, path, length(measures))
  }
  # The same health gate ssm_sem() applies: convergence is a hard stop, and
  # lavaan's post-estimation flags (e.g., a negative variance estimate) get
  # the same caution here -- a user-supplied fit is not exempt from
  # "global model health is surfaced before any SSM output" (spec 4.5).
  sem_health_gate(fit)
  parallel <- match.arg(parallel, c("no", "multicore", "snow"))
  stopifnot(is_scalar_count(ncpus))

  # Engine preconditions checked up front with actionable errors, rather
  # than letting lavaan abort mid-engine with an internal message: the mvn
  # engine needs a parameter covariance (absent under se = "none"), and the
  # bootstrap engine needs raw data to resample (absent for summary-moment
  # fits).
  fit_se <- tryCatch(
    lavaan::lavInspect(fit, "options")$se,
    error = function(e) NULL
  )
  has_raw_data <- !inherits(
    tryCatch(lavaan::lavInspect(fit, "data"), error = function(e) e),
    "error"
  )
  if (ci_method == "mvn" && identical(fit_se, "none")) {
    stop(
      "This lavaan fit was made with se = \"none\", so it carries no ",
      "parameter covariance and the \"mvn\" engine cannot draw from it. ",
      "Refit with standard errors (se = \"robust.huber.white\", ssm_sem()'s ",
      "default, is recommended) or use ci_method = \"boot\".",
      call. = FALSE
    )
  }
  if (ci_method == "boot" && !has_raw_data) {
    stop(
      "Bootstrap resampling requires a fit made from raw data; this fit ",
      "was made from summary moments (sample.cov), which cannot be ",
      "resampled. Use ci_method = \"mvn\".",
      call. = FALSE
    )
  }

  est <- sem_estimate(
    fit,
    scales = scales, angles_deg = as.numeric(angles), measures = measures,
    ci_method = ci_method, boots = boots, interval = interval,
    contrast = contrast, parallel = parallel, ncpus = ncpus, path = path
  )

  # The mvn engine propagates whatever covariance the fit carries; the
  # package's coverage validation found the plain ML covariance undercovers
  # displacement (about 0.88, not improving with n) when the fixed-angle
  # model is an approximation, so a non-robust fit gets a warning here
  # (after the hard guards, so refusals are not preceded by advice about
  # intervals that will never exist). Only raw-data fits are warned: with
  # summary-moment (sample.cov) input, lavaan cannot compute a sandwich, so
  # the plain covariance is all there is. (fit_se and has_raw_data were
  # extracted with the engine preconditions above.)
  if (ci_method == "mvn" && identical(fit_se, "standard") && has_raw_data) {
    warning(
      "This lavaan fit uses se = \"standard\", so the \"mvn\" intervals ",
      "propagate the plain ML covariance, which the package's coverage ",
      "validation found to undercover displacement when the fixed-angle ",
      "model is only an approximation. Refit with ",
      "se = \"robust.huber.white\" (ssm_sem()'s default).",
      call. = FALSE
    )
  }
  if (ci_method == "mvn" && identical(fit_se, "bootstrap")) {
    warning(
      "This lavaan fit uses se = \"bootstrap\", so the \"mvn\" intervals ",
      "propagate a bootstrap-estimated covariance: their quality depends ",
      "on the number of bootstrap draws behind that estimate. With few ",
      "draws, prefer refitting with se = \"robust.huber.white\" or using ",
      "ci_method = \"boot\" directly.",
      call. = FALSE
    )
  }
  parts <- sem_assemble(est, scales, measures, contrast)

  lav_missing <- tryCatch(
    lavaan::lavInspect(fit, "options")$missing,
    error = function(e) "listwise"
  )
  missing <- if (identical(lav_missing, "ml") ||
    identical(lav_missing, "ml.x")) "fiml" else "listwise"

  details <- sem_details(
    boots, interval, missing, as.numeric(angles), contrast, ci_method, path
  )

  new_ssm_sem(
    results = parts$results,
    scores = parts$scores,
    details = details,
    call = call,
    sem = fit,
    invariance = NULL,
    model = list(tier = est$tier, syntax = NULL, weights = est$weights)
  )
}

# Subclass methods (spec section 7.3) ----------------------------------------------

# Print method for objects of ssm_sem class: prepend the measurement-model
# block (tier, global fit indices -- computed quantities printed without
# verdicts attached), then delegate to the inherited profile printer (which
# carries the amplitude-CI and fit guardrail notes unchanged).
#' @method print circumplex_ssm_sem
#' @export
print.circumplex_ssm_sem <- function(x, digits = 3, ...) {
  cat("\n# Latent (SEM-based) SSM\n\n")
  cat("Measurement model:\t", x$model$tier, "fixed-angle circumplex\n")
  if (has_lavaan()) {
    n <- lavaan::lavInspect(x$sem, "ntotal")
    fm <- lavaan::fitMeasures(x$sem)
    # Prefer the robust/scaled fit statistics when the fit carries them
    # (estimator = "MLR", ssm_sem()'s default): under the skewed
    # distributions typical of circumplex scores, the naive chi-square
    # over-rejects and the indices computed from it are distorted.
    robust <- "chisq.scaled" %in% names(fm)
    cat(
      sprintf("Global fit (N = %d%s):", n, if (robust) ", robust" else ""),
      sprintf(
        "chisq(%g) = %s, p %s",
        sem_fm_pick(fm, "df.scaled", "df"),
        format(round(sem_fm_pick(fm, "chisq.scaled", "chisq"), digits)),
        sem_fmt_p(sem_fm_pick(fm, "pvalue.scaled", "pvalue"), digits,
          prose = TRUE
        )
      ), "\n"
    )
    cat(sprintf(
      "\t\t\tCFI = %s, RMSEA = %s, SRMR = %s\n",
      format(round(sem_fm_pick(fm, "cfi.robust", "cfi.scaled", "cfi"), digits)),
      format(round(sem_fm_pick(fm, "rmsea.robust", "rmsea.scaled", "rmsea"), digits)),
      format(round(sem_fm_pick(fm, "srmr"), digits))
    ))
  }
  if (!is.null(x$invariance)) {
    inv <- x$invariance
    cat(
      "\nInvariance ladder (gate: ", inv$gate, ", alpha = ",
      format(inv$alpha), "):\n",
      sep = ""
    )
    tab <- inv$table
    show <- data.frame(
      rung = tab$rung,
      chisq = round(tab$chisq, digits),
      df = tab$df,
      cfi = round(tab$cfi, digits),
      rmsea = round(tab$rmsea, digits),
      dchisq = round(tab$dchisq, digits),
      ddf = tab$ddf,
      p = sem_fmt_p(tab$p, digits),
      dcfi = round(tab$dcfi, digits)
    )
    # The retain/reject column appears ONLY inside the criterion's validated
    # scope; outside it the value stands alone and the note below says why.
    if (any(!is.na(tab$cr))) {
      show$cr <- tab$cr
    }
    print(show, row.names = FALSE, na.print = "")
    if (any(nzchar(tab$note))) {
      for (i in which(nzchar(tab$note))) {
        cat("  note [", tab$rung[i], "]: ", tab$note[i], "\n", sep = "")
      }
    }
    if (any(!is.na(tab$dcfi))) {
      cat(sem_dcfi_note(inv$dcfi_scope))
    }
    if (isTRUE(inv$comparable)) {
      cat("Verdict: ", inv$verdict, "\n", sep = "")
    } else {
      cat(
        "Verdict: ", inv$verdict, ".\n",
        if (isTRUE(inv$contrast_requested)) {
          paste0(
            "The requested latent contrast was therefore not computed; the ",
            "rows below are each group's separate (configural) latent ",
            "profile. The observed-score contrast (ssm_analyze()) answers ",
            "its own, different question and remains available.\n"
          )
        } else {
          paste0(
            "The rows below are each group's separate (configural) latent ",
            "profile; a latent contrast would not be computable on this ",
            "instrument's latent metric.\n"
          )
        },
        sep = ""
      )
    }
  }
  NextMethod()
}

# Summary method for objects of ssm_sem class: owns the inferential-method and
# missing-data lines (the inherited summary would print "Bootstrap Resamples"
# for MVN draws and a Listwise Deletion line -- a false statement of the
# method; spec section 7.3).
#' @method summary circumplex_ssm_sem
#' @export
summary.circumplex_ssm_sem <- function(object, digits = 3, ...) {
  labs <- sem_detail_labels(object$details)
  cat(
    "\nStatistical Basis:\t", object$details$score_type, "Scores",
    labs$replicate, object$details$boots,
    "\nConfidence Level:\t", object$details$interval,
    "\nMissing Data:\t\t", labs$missing,
    "\nScale Displacements:\t", as.numeric(object$details$angles),
    "\n\n"
  )
  print(object)
}
