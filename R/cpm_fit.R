# =============================================================================
# CPM engine core (Browne 1992 circular stochastic process model).
#
# Internal only -- NOTHING here is exported; the user-facing cpm_fit() API is a
# later task. Implements the design in devel/m4-browne-design.md:
#   - the correlation function rho(delta) and its derivative (sec. 1.2)
#   - the model-implied correlation matrix P (sec. 1.3)
#   - model variants A-D and their df (sec. 1.4)
#   - the unconstrained parameterization (sec. 3.3) with forward/inverse maps
#   - the ML discrepancy F (sec. 3.1) and its analytic gradient (sec. 3.4)
#   - starting values, deterministic multi-start, and the optimizer (sec. 3.5)
#   - convergence acceptance, beta boundary polish, reflection
#     canonicalization, and the diagnostics (sec. 2.3, sec. 3.5)
#
# All angle handling internal to the engine is in RADIANS on the unwrapped
# real line; angles are wrapped to [0, 360) only in the reported fields. The
# engine never touches R's global RNG stream (multi-start jitter is
# deterministic).
# =============================================================================

# ---- correlation function ---------------------------------------------------

#' Truncated Fourier cosine correlation function rho(delta)
#'
#' `rho(delta) = sum_{k=0}^{m} beta_k cos(k delta)` (design sec. 1.2). `beta` is
#' indexed from k = 0, so `length(beta) == m + 1`. Vectorized over `delta`;
#' returns an object shaped like `delta` (scalar, vector, or matrix).
#'
#' @noRd
cpm_rho <- function(delta, beta) {
  k <- seq_along(beta) - 1L
  d <- as.vector(delta)
  # outer over k: rows = delta entries, cols = harmonics
  out <- cos(outer(d, k)) %*% beta
  out <- as.numeric(out)
  if (!is.null(dim(delta))) dim(out) <- dim(delta)
  out
}

#' Derivative of the correlation function, rho'(delta)
#'
#' `rho'(delta) = -sum_{k=0}^{m} k beta_k sin(k delta)` (design sec. 3.4). Same
#' shape contract as [cpm_rho()].
#'
#' @noRd
cpm_rho_deriv <- function(delta, beta) {
  k <- seq_along(beta) - 1L
  d <- as.vector(delta)
  out <- -(sin(outer(d, k)) %*% (k * beta))
  out <- as.numeric(out)
  if (!is.null(dim(delta))) dim(out) <- dim(delta)
  out
}

# ---- model-implied correlation matrix ---------------------------------------

#' Model-implied correlation matrix P (design sec. 1.3)
#'
#' `P = D_zeta C D_zeta + (I - D_zeta^2)` with `C_ij = rho(theta_i - theta_j)`.
#' The diagonal is exactly 1 by construction.
#'
#' @param theta length-p numeric, angles in radians (unwrapped).
#' @param zeta length-p numeric in (0, 1].
#' @param beta length-(m+1) numeric, non-negative, summing to 1.
#' @noRd
cpm_implied_cor <- function(theta, zeta, beta) {
  p <- length(theta)
  Delta <- outer(theta, theta, `-`)
  C <- cpm_rho(Delta, beta)                 # p x p, C_ii = 1
  P <- (zeta %o% zeta) * C
  diag(P) <- 1                              # I - D_zeta^2 exactly restores 1
  P
}

# ---- ML discrepancy ---------------------------------------------------------

#' ML discrepancy F (design sec. 3.1)
#'
#' `F = ln|P| - ln|R| + tr(R P^-1) - p`. `F >= 0`, `= 0` iff `P == R`.
#'
#' @noRd
cpm_discrepancy <- function(R, P, ldR = NULL) {
  p <- nrow(R)
  ldP <- as.numeric(determinant(P, logarithm = TRUE)$modulus)
  if (is.null(ldR)) {
    ldR <- as.numeric(determinant(R, logarithm = TRUE)$modulus)
  }
  ldP - ldR + sum(diag(solve(P, R))) - p
}

# ---- model spec and degrees of freedom (design sec. 1.4) ------------------------

#' Build a model spec (variant, dimensions, df, parameter layout)
#'
#' Variants (design sec. 1.4):
#'   A quasi-circumplex : p-1 free angles, p free zeta, m free beta
#'   B constrained-angles: 0 free angles, p free zeta, m free beta
#'   C equal-communality : p-1 free angles, 1 free zeta, m free beta
#'   D circulant         : 0 free angles, 1 free zeta, m free beta
#'
#' The free-parameter vector gamma* is laid out as
#'   [ free-angle radians ] [ zeta logits u ] [ beta free logits v_1..v_m ].
#'
#' @noRd
cpm_spec <- function(p, m, variant = c("A", "B", "C", "D"), reference = 1) {
  variant <- match.arg(variant)
  stopifnot(is_count(p), p >= 3)
  stopifnot(is_count(m), m >= 1)
  stopifnot(is_count(reference), reference >= 1, reference <= p)

  # m-cap (design sec. 1.4): default floor((p-1)/2) for A/C; floor(p/2) for B/D.
  cap <- if (variant %in% c("B", "D")) floor(p / 2) else floor((p - 1) / 2)
  if (m > cap) {
    stop(
      sprintf(
        "m = %d exceeds the identification cap (%d) for variant %s at p = %d.",
        m, cap, variant, p
      ),
      call. = FALSE
    )
  }

  free_angles <- if (variant %in% c("A", "C")) (p - 1L) else 0L
  n_zeta <- if (variant %in% c("A", "B")) p else 1L
  n_beta_free <- m

  q <- free_angles + n_zeta + n_beta_free
  df <- p * (p - 1L) / 2L - q

  # index blocks within gamma*
  i_angle <- if (free_angles > 0) seq_len(free_angles) else integer(0)
  i_zeta <- if (n_zeta > 0) free_angles + seq_len(n_zeta) else integer(0)
  i_beta <- free_angles + n_zeta + seq_len(n_beta_free)

  # which theta positions are free (all but the reference), in order
  free_pos <- if (free_angles > 0) setdiff(seq_len(p), reference) else integer(0)

  list(
    p = p, m = m, variant = variant, reference = reference,
    free_angles = free_angles, n_zeta = n_zeta, n_beta_free = n_beta_free,
    q = q, df = df,
    i_angle = i_angle, i_zeta = i_zeta, i_beta = i_beta,
    free_pos = free_pos,
    keep_k = 0:m,             # harmonics with a free beta (0..m unless polished)
    theta_ref_val = NA_real_  # filled by cpm_engine (reference theoretical angle)
  )
}

# ---- unconstrained parameterization: forward and inverse maps ---------------

#' Pack natural parameters into the unconstrained vector gamma* (design sec. 3.3)
#'
#' zeta -> logit u; beta -> softmax free logits v (v_0 = 0 fixed); free angles
#' pass through unchanged (identity map, held on the real line).
#'
#' @noRd
cpm_pack <- function(theta, zeta, beta, spec) {
  g <- numeric(spec$q)
  if (spec$free_angles > 0) {
    g[spec$i_angle] <- theta[spec$free_pos]
  }
  if (spec$n_zeta > 0) {
    z <- if (spec$n_zeta == 1L) zeta[[1]] else zeta
    g[spec$i_zeta] <- stats::qlogis(z)
  }
  # softmax inverse with v_0 = 0 over the KEPT harmonics (keep_k; identity
  # 0..m unless polished): v_k = log(beta_k) - log(beta_0). A zero kept beta
  # has no finite preimage -- fail loudly rather than emit -Inf.
  b_keep <- beta[spec$keep_k + 1L]
  stopifnot(all(b_keep > 0))
  v <- log(b_keep) - log(b_keep[1])
  g[spec$i_beta] <- v[-1]
  g
}

#' Unpack the unconstrained vector gamma* back to natural parameters
#'
#' Returns theta (radians, unwrapped, reference restored), zeta (length p),
#' beta (length m+1). Angles are NEVER wrapped here -- wrapping happens only at
#' report time.
#'
#' @noRd
cpm_unpack <- function(gstar, spec) {
  p <- spec$p
  theta <- numeric(p)
  theta[spec$reference] <- spec$theta_ref_val
  if (spec$free_angles > 0) {
    theta[spec$free_pos] <- gstar[spec$i_angle]
  } else {
    # B/D: angles are fixed at their theoretical values, supplied via
    # theta_fixed on the spec (filled by cpm_engine; a spec built by
    # cpm_spec() alone lacks it -- fail loudly, not with NULL arithmetic).
    stopifnot(!is.null(spec$theta_fixed))
    theta <- spec$theta_fixed
  }
  if (spec$n_zeta == p) {
    zeta <- stats::plogis(gstar[spec$i_zeta])
  } else {
    zeta <- rep(stats::plogis(gstar[spec$i_zeta]), p)
  }
  v <- c(0, gstar[spec$i_beta])
  ev <- exp(v - max(v))                     # softmax (max-shift for stability)
  b_keep <- ev / sum(ev)
  # Scatter the kept-harmonic betas into a full length-(m+1) vector; removed
  # (polished-out) harmonics are 0. When keep_k == 0:m this is the identity.
  beta <- numeric(spec$m + 1L)
  beta[spec$keep_k + 1L] <- b_keep
  list(theta = theta, zeta = zeta, beta = beta)
}

# ---- objective and gradient in unconstrained coordinates --------------------

#' Objective: F evaluated at gamma* (design sec. 3.1)
#' @noRd
cpm_objective <- function(gstar, R, spec, ldR = NULL) {
  nat <- cpm_unpack(gstar, spec)
  P <- cpm_implied_cor(nat$theta, nat$zeta, nat$beta)
  cpm_discrepancy(R, P, ldR = ldR)
}

#' Analytic gradient of F at gamma* (design sec. 3.4)
#'
#' Natural-scale gradients:
#'   dF/dtheta_i = 2 sum_{j!=i} A_ij zeta_i zeta_j rho'(delta_ij)
#'   dF/dzeta_i  = 2 sum_{j!=i} A_ij zeta_j rho(delta_ij)
#'   dF/dbeta_k  = sum_{i!=j} A_ij zeta_i zeta_j cos(k delta_ij)
#' with A = P^-1 - P^-1 R P^-1 (symmetric). Chained through the logit/softmax
#' Jacobians to gamma*.
#'
#' @noRd
cpm_gradient <- function(gstar, R, spec) {
  p <- spec$p
  nat <- cpm_unpack(gstar, spec)
  theta <- nat$theta; zeta <- nat$zeta; beta <- nat$beta

  Delta <- outer(theta, theta, `-`)
  P <- cpm_implied_cor(theta, zeta, beta)
  Pinv <- solve(P)
  A <- Pinv - Pinv %*% R %*% Pinv           # symmetric
  A <- (A + t(A)) / 2

  # B = A * (zeta zeta^T), diagonal zeroed (only off-diagonal dP enter).
  B <- A * (zeta %o% zeta)
  diag(B) <- 0

  Rho <- cpm_rho(Delta, beta)               # p x p (diag 1, but zeroed below)
  Rhod <- cpm_rho_deriv(Delta, beta)        # p x p

  # ---- natural-scale gradients ----
  # dF/dtheta_i (length p); reference component is dropped later.
  dF_dtheta <- 2 * rowSums(B * Rhod)

  # dF/dzeta_i: 2 * (A * rho with diag 0) %*% zeta
  Azero <- A * Rho
  diag(Azero) <- 0
  dF_dzeta <- 2 * as.numeric(Azero %*% zeta)

  # dF/dbeta_k over the kept harmonics (keep_k; identity 0..m unless polished)
  dF_dbeta <- vapply(spec$keep_k, function(k) sum(B * cos(k * Delta)), numeric(1))

  # ---- chain to unconstrained coordinates ----
  g <- numeric(spec$q)

  if (spec$free_angles > 0) {
    g[spec$i_angle] <- dF_dtheta[spec$free_pos]   # angle Jacobian = 1
  }

  if (spec$n_zeta == p) {
    # dF/du_i = zeta_i (1 - zeta_i) * dF/dzeta_i
    g[spec$i_zeta] <- zeta * (1 - zeta) * dF_dzeta
  } else {
    # single shared u: sum the per-item zeta gradients through the shared
    # Jacobian (all zeta equal, so use the common value).
    z <- zeta[1]
    g[spec$i_zeta] <- z * (1 - z) * sum(dF_dzeta)
  }

  # dF/dv_l via the softmax Jacobian on the KEPT harmonics (v_0 = 0 fixed):
  # dF/dv = (diag(b) - b b^T) %*% dF/dbeta over keep_k, then drop the l=0 entry.
  b_keep <- beta[spec$keep_k + 1L]
  # diag() needs nrow pinned: with a single kept harmonic (all k >= 1
  # polished out), diag(scalar) would build a 0 x 0 matrix.
  Jt <- (diag(b_keep, nrow = length(b_keep)) - b_keep %o% b_keep) %*% dF_dbeta
  g[spec$i_beta] <- Jt[-1]

  g
}

# ---- starting values (design sec. 3.5) ------------------------------------------

#' Starting values for one orientation (design sec. 3.5)
#'
#' theta0 = user angles; zeta0_i = sqrt(max_{j!=i} |r_ij|) clipped to
#' [0.3, 0.95]; beta0 = LS fit of off-diagonal r_ij on {cos(k delta0_ij)},
#' negatives clipped to 0.01 and renormalized; singular-LS fallback
#' (0.4, 0.3, 0.2, 0.1, ...) truncated to m+1 and renormalized.
#'
#' @noRd
cpm_start_values <- function(R, theta0, m) {
  p <- nrow(R)

  # zeta0
  absR <- abs(R)
  diag(absR) <- 0
  zeta0 <- sqrt(apply(absR, 1, max))
  zeta0 <- pmin(pmax(zeta0, 0.3), 0.95)

  # beta0 via LS on off-diagonal entries
  Delta <- outer(theta0, theta0, `-`)
  ut <- upper.tri(R)
  d_off <- Delta[ut]
  r_off <- R[ut]
  X <- vapply(0:m, function(k) cos(k * d_off), numeric(length(d_off)))
  beta0 <- tryCatch({
    coef <- solve(crossprod(X), crossprod(X, r_off))
    as.numeric(coef)
  }, error = function(e) rep(NA_real_, m + 1))

  # Documented degenerate fallback (design sec. 3.5): 0.4, 0.3, 0.2, 0.1, ...
  # truncated to m + 1 and renormalized (extra entries decay to 0.05).
  patt <- c(0.4, 0.3, 0.2, 0.1)
  if (m + 1 <= length(patt)) {
    fallback <- patt[seq_len(m + 1)]
  } else {
    fallback <- c(patt, rep(0.05, (m + 1) - length(patt)))
  }
  if (anyNA(beta0)) {
    beta0 <- fallback
  }
  beta0[beta0 < 0] <- 0.01
  if (sum(beta0) <= 0) beta0 <- fallback
  beta0 <- beta0 / sum(beta0)

  list(zeta = zeta0, beta = beta0)
}

# ---- deterministic multi-start jitter (design sec. 3.5) -------------------------

# Fixed, documented offset patterns applied to the free angles (degrees).
# NO random numbers anywhere: alternating +/-15 and +/-30 degree patterns.
cpm_jitter_offsets_deg <- function(n_free) {
  base15 <- rep(c(15, -15), length.out = n_free)
  base30 <- rep(c(30, -30), length.out = n_free)
  list(
    base15,
    base30,
    -base15,
    rep(c(15, -30), length.out = n_free)
  )
}

# ---- single optimization run ------------------------------------------------

cpm_optimize_one <- function(gstar0, R, spec) {
  # ln|R| is constant within a fit: hoist it out of the hot loop (the
  # gradient never sees it -- an additive constant drops out).
  ldR <- as.numeric(determinant(R, logarithm = TRUE)$modulus)
  fit <- stats::nlminb(
    start = gstar0,
    objective = function(g) cpm_objective(g, R, spec, ldR = ldR),
    gradient = function(g) cpm_gradient(g, R, spec),
    control = list(rel.tol = 1e-12, iter.max = 500L, eval.max = 1000L)
  )
  # nlminb's $objective is F evaluated at $par; cpm_objective is pure, so a
  # re-evaluation would be byte-identical wasted work.
  list(par = fit$par, F = fit$objective, code = fit$convergence,
       message = fit$message)
}

# ---- reflection canonicalization (design sec. 2.3) ------------------------------

# Reflect all free angles about the reference: theta_i -> 2 theta_ref - theta_i.
# Returns the reflected gamma* (only angle block changes; zeta/beta untouched
# because rho is even). For B/D there are no free angles, so reflection is a
# no-op on the parameter vector.
cpm_reflect_par <- function(gstar, spec) {
  if (spec$free_angles == 0) return(gstar)
  g <- gstar
  g[spec$i_angle] <- 2 * spec$theta_ref_val - gstar[spec$i_angle]
  g
}

# Sum of absolute shortest-arc distances between fitted and theoretical angles.
cpm_theory_distance <- function(theta, theta_theory) {
  sum(abs(angle_dist(as_radian(theta), as_radian(theta_theory))))
}

# Choose the reflection minimizing distance to theory; CCW tie-break.
cpm_canonicalize <- function(gstar, spec, theta_theory) {
  if (spec$free_angles == 0) {
    return(list(par = gstar, warn = FALSE))
  }
  g1 <- gstar
  g2 <- cpm_reflect_par(gstar, spec)
  th1 <- cpm_unpack(g1, spec)$theta
  th2 <- cpm_unpack(g2, spec)$theta
  d1 <- cpm_theory_distance(th1, theta_theory)
  d2 <- cpm_theory_distance(th2, theta_theory)

  tie_tol <- 1e-8
  if (d1 < d2 - tie_tol) {
    return(list(par = g1, warn = FALSE))
  }
  if (d2 < d1 - tie_tol) {
    return(list(par = g2, warn = FALSE))
  }

  # Tie: CCW tie-break. Choose the branch whose first deciding scale
  # (theta_hat_s - theta_hat_ref) mod 360 in (0, 180).
  for (g in list(g1, g2)) {
    th <- cpm_unpack(g, spec)$theta
    ref <- th[spec$reference]
    for (s in setdiff(seq_len(spec$p), spec$reference)) {
      rel <- ((th[s] - ref) %% (2 * pi))
      if (rel > 1e-9 && rel < pi - 1e-9) {
        return(list(par = g, warn = FALSE))
      }
      if (rel > pi + 1e-9 && rel < 2 * pi - 1e-9) {
        break  # this branch decided against; try the other
      }
      # exactly at 0 or pi: undecided, fall through to next scale
    }
  }
  # Nothing decided: warn and report as-is.
  list(par = g1, warn = TRUE)
}

# ---- top-level engine entry point (design sec. 3.5, sec. 5.4 details) ---------------

#' CPM engine: fit Browne's circular process model to a correlation matrix
#'
#' Internal engine core (the exported `cpm_fit()` API is a later task). Returns
#' a plain list with the natural-scale estimates (angles wrapped to [0, 360)
#' only in the reported fields; unwrapped radians kept for downstream tasks)
#' and the full diagnostic set required by design sec. 5.4.
#'
#' @param R p x p sample correlation matrix (positive definite).
#' @param angles length-p numeric, theoretical/start angles in DEGREES.
#' @param m number of harmonics (design sec. 1.4/sec. 1.5).
#' @param variant one of "A","B","C","D" (design sec. 1.4).
#' @param reference index of the scale whose angle is fixed (design sec. 2.1).
#' @noRd
cpm_engine <- function(R, angles, m = 3, variant = c("A", "B", "C", "D"),
                       reference = 1) {
  variant <- match.arg(variant)
  R <- as.matrix(R)
  p <- nrow(R)

  # ---- input validation (house style) ----
  stopifnot(is.matrix(R), nrow(R) == ncol(R), isSymmetric(unname(R), tol = 1e-8))
  stopifnot(is_num(angles, n = p))
  stopifnot(is_count(reference), reference >= 1, reference <= p)

  # PD check (design sec. 4): smallest eigenvalue > 1e-10, else refuse.
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) <= 1e-10) {
    stop(
      "R is not positive definite (smallest eigenvalue <= 1e-10); ",
      "ln|R| is undefined. Refusing to fit a singular/near-singular matrix.",
      call. = FALSE
    )
  }

  # Symmetrize after validation: the tolerance-based isSymmetric() admits
  # asymmetry up to 1e-8, but F would then be computed on the raw matrix,
  # making F(R, R) != 0 by the asymmetry magnitude.
  R <- (R + t(R)) / 2

  spec <- cpm_spec(p, m, variant, reference)

  # df >= 1 required; df = 0 allowed but warns (design sec. 1.4/sec. 4).
  if (spec$df < 0) {
    stop(
      sprintf("Model has negative df (%d): over-parameterized.", spec$df),
      call. = FALSE
    )
  }
  if (spec$df == 0) {
    warning(
      "Model has df = 0 (saturated): fits perfectly and tests nothing.",
      call. = FALSE
    )
  }

  # convert angles once (degrees in, radians internal), reference held fixed.
  # Wrapped to [0, 2*pi) at entry: every consumer is branch-safe (trig or
  # angle_dist), but the invariant should hold rather than be tolerated.
  theta_theory <- as.numeric(as_radian(as_degree(angles))) %% (2 * pi)
  spec$theta_ref_val <- theta_theory[reference]
  spec$theta_fixed <- theta_theory        # used by variants B/D (fixed angles)

  # ---- starting values and multi-start set ----
  sv <- cpm_start_values(R, theta_theory, m)
  g0 <- cpm_pack(theta_theory, sv$zeta, sv$beta, spec)

  # Mirror start only when angles are free: with fixed angles (B/D) the
  # reflection is a no-op and would duplicate g0, letting the acceptance
  # criterion's "reproduced by >= 2 starts" pass vacuously off the duplicate.
  starts <- list(g0)
  if (spec$free_angles > 0) {
    starts[[2L]] <- cpm_reflect_par(g0, spec)
    offs <- cpm_jitter_offsets_deg(spec$free_angles)
    for (off in offs) {
      theta_j <- theta_theory
      theta_j[spec$free_pos] <- theta_theory[spec$free_pos] + off * pi / 180
      starts[[length(starts) + 1L]] <-
        cpm_pack(theta_j, sv$zeta, sv$beta, spec)
    }
  } else {
    # No free angles: jitter zeta start instead (deterministic).
    for (fac in c(0.85, 1.1, 0.7)) {
      z <- pmin(pmax(sv$zeta * fac, 0.05), 0.99)
      starts[[length(starts) + 1L]] <-
        cpm_pack(theta_theory, z, sv$beta, spec)
    }
  }

  runs <- lapply(starts, cpm_optimize_one, R = R, spec = spec)
  Fs <- vapply(runs, function(r) r$F, numeric(1))
  best <- which.min(Fs)
  fit <- runs[[best]]

  # ---- multimodality flag (design sec. 3.5, refined during B1) ----
  # Mirror pairs with equal F are expected, not multimodality. The flag fires
  # when a NON-mirror run lands on a DISTINCT parameter point whose F-hat is
  # competitive with the best (within max(1e-6, 1e-6 * |F-hat|)): near-tied
  # distinct optima are the start-dependence / non-identification signature,
  # and the hazard for warm-started bootstrap replicates. A strictly worse
  # secondary basin (ordinary nonconvexity of a periodic objective; e.g. a
  # wide jitter start getting stuck) does NOT flag -- the reported solution
  # is still the unambiguous multi-start winner, and flagging it would fire
  # on clean data whenever any jitter finds any worse basin. Deviation from
  # the design's literal "differ in F-hat by > 1e-6" limb, recorded in
  # devel/m4-browne-design.md sec. 11.
  best_par <- fit$par
  comp_tol <- 1e-6 * max(1, abs(fit$F))
  multimodal <- FALSE
  best_ref_rel <- cpm_ref_relative(best_par, spec)
  nat_best <- cpm_unpack(best_par, spec)
  for (i in setdiff(seq_along(runs), best)) {
    par_i <- runs[[i]]$par
    nat_i <- cpm_unpack(par_i, spec)
    # zeta/beta compared on the NATURAL scale: an absolute tolerance on the
    # logit scale explodes near a Heywood boundary (d logit/d zeta ~ 200 at
    # zeta = 0.995), where a true mirror would be misread as distinct.
    other_eq <- max(abs(nat_i$zeta - nat_best$zeta)) < 1e-4 &&
      max(abs(nat_i$beta - nat_best$beta)) < 1e-4
    if (spec$free_angles > 0) {
      # Angle comparisons must be circular: a scale exactly opposite the
      # reference has relative angle +pi in BOTH mirrors (angle_dist maps the
      # -pi atom to +pi), so a plain sign test would misread a true mirror as
      # a distinct optimum on exact-octant configurations.
      rr <- cpm_ref_relative(par_i, spec)
      mir_gap <- max(abs(as.numeric(
        angle_dist(as_radian(rr), as_radian(-best_ref_rel))
      )))
      same_gap <- max(abs(as.numeric(
        angle_dist(as_radian(rr), as_radian(best_ref_rel))
      )))
      is_mirror <- other_eq && mir_gap < 1e-4    # reflection => negated angles
      same_point <- other_eq && same_gap < 1e-4
    } else {
      # B/D: angles fixed, so reflection is a no-op and no run is a mirror;
      # zeta/beta (natural scale) are the whole comparison.
      is_mirror <- FALSE
      same_point <- other_eq
    }
    if (is_mirror) next
    competitive <- (runs[[i]]$F - fit$F) <= comp_tol
    if (competitive && !same_point) multimodal <- TRUE
  }

  # ---- beta boundary polish (design sec. 3.5) ----
  polished <- cpm_polish_beta(fit, R, spec)
  fit <- polished$fit
  spec <- polished$spec
  removed <- polished$removed

  # ---- convergence acceptance (design sec. 3.5) ----
  # (a) scaled gradient norm at the REPORTED (possibly polished) solution;
  # (b) the multi-start best F-hat reproduced (+/- 1e-8) by >= 2 starts.
  # (b) deliberately uses the pre-polish Fs: the reduced model is NESTED in
  # the full one (beta_k = 0 is a boundary point of the full space), so its
  # optimum cannot undercut the full-model minimum, and the polish gate
  # (F_red <= F_full + 1e-8) pins the reported F-hat to the reproduced
  # multi-start optimum from both sides. A warm-started polish refit that
  # strayed to a worse basin fails the gate and is reverted, so the reported
  # model inherits (b) transitively. nlminb's convergence code is NOT used
  # here (advisory only, design sec. 3.5).
  gnorm <- max(abs(cpm_gradient(fit$par, R, spec)))
  grad_ok <- gnorm <= 1e-6 * max(1, abs(fit$F))
  reproduced <- sum(abs(Fs - min(Fs)) <= 1e-8) >= 2
  accepted <- grad_ok && reproduced

  if (!accepted) {
    warning(
      "CPM fit did not meet the convergence acceptance criterion ",
      sprintf("(gradient norm %.2e, reproduced = %s).", gnorm, reproduced),
      call. = FALSE
    )
  }

  # ---- canonicalization (design sec. 2.3) ----
  canon <- cpm_canonicalize(fit$par, spec, theta_theory)
  if (canon$warn) {
    warning(
      "CPM reflection canonicalization could not be decided; ",
      "reporting the solution as-is.",
      call. = FALSE
    )
  }
  nat <- cpm_unpack(canon$par, spec)

  # ---- diagnostics ----
  # Hessian condition number at the optimum via analytic-gradient FD.
  # Hessian at the canonicalized par; gnorm above was taken at fit$par --
  # equivalent today because reflection is an exact isometry of F (rho even),
  # but keep both in mind if canonicalization ever stops being one.
  H <- stats::optimHess(
    canon$par,
    fn = function(g) cpm_objective(g, R, spec),
    gr = function(g) cpm_gradient(g, R, spec)
  )
  H <- (H + t(H)) / 2
  hev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  hcond <- if (min(abs(hev)) > 0) max(abs(hev)) / min(abs(hev)) else Inf
  if (is.finite(hcond) && hcond > 1e8) {
    warning(
      sprintf(
        "CPM Hessian is ill-conditioned (condition number %.2e): angles may ",
        hcond
      ),
      "be clustered or parameters weakly determined.",
      call. = FALSE
    )
  } else if (!is.finite(hcond)) {
    warning(
      "CPM Hessian is singular (ill-conditioned): parameters weakly determined.",
      call. = FALSE
    )
  }

  heywood <- any(nat$zeta > 0.995)

  # wrapped-to-[0,360) reported angles; keep unwrapped radians too.
  theta_deg <- as.numeric(as_degree(as_radian(nat$theta %% (2 * pi))))

  list(
    theta = theta_deg,                 # degrees in [0, 360), canonicalized
    theta_rad = nat$theta %% (2 * pi), # wrapped radians (reported)
    theta_rad_unwrapped = nat$theta,   # unwrapped radians (later tasks)
    theta_theory = as.numeric(as_degree(as_radian(theta_theory %% (2 * pi)))),
    zeta = nat$zeta,
    beta = nat$beta,
    F = fit$F,
    df = spec$df,
    # m as FITTED (design sec. 3.5/sec. 5.4): decreases iff the polish removed the
    # top harmonic; beta stays at its nominal length with zeros at removed k.
    m = max(spec$keep_k),
    variant = spec$variant,
    reference = reference,
    P = cpm_implied_cor(nat$theta, nat$zeta, nat$beta),
    accepted = accepted,
    nlminb_code = fit$code,            # ADVISORY only
    gradient_norm = gnorm,
    hessian_condition = hcond,
    heywood = heywood,
    removed_harmonics = removed,
    multimodal = multimodal,
    par = canon$par,
    spec = spec
  )
}

# Reference-relative angle vector (radians, wrapped to (-pi, pi]) for
# multimodality / mirror detection. For B/D (fixed angles) returns zeros.
cpm_ref_relative <- function(gstar, spec) {
  if (spec$free_angles == 0) return(numeric(spec$p - 1L))
  th <- cpm_unpack(gstar, spec)$theta
  rel <- angle_dist(as_radian(th), as_radian(rep(th[spec$reference], spec$p)))
  as.numeric(rel[spec$free_pos])
}

# Rebuild a spec's beta bookkeeping for a reduced harmonic support set
# (design sec. 3.5 boundary polish). Owns the SAME q/df/index derivation as
# cpm_spec so the two cannot drift apart; only the trailing beta block
# changes -- i_angle/i_zeta/free_pos are position-stable by layout.
cpm_spec_reduce <- function(spec, keep_k) {
  keep_k <- sort(unique(as.integer(keep_k)))
  stopifnot(0L %in% keep_k, all(keep_k %in% 0:spec$m))
  rspec <- spec
  rspec$keep_k <- keep_k
  rspec$n_beta_free <- length(keep_k) - 1L
  rspec$q <- spec$free_angles + spec$n_zeta + rspec$n_beta_free
  rspec$df <- spec$p * (spec$p - 1L) / 2L - rspec$q
  rspec$i_beta <- spec$free_angles + spec$n_zeta + seq_len(rspec$n_beta_free)
  rspec
}

# ---- beta boundary polish (design sec. 3.5) -------------------------------------

# After convergence, for any beta_hat_k < 1e-2 (k >= 1), refit with that
# harmonic removed (fix beta_k = 0, drop its v). If F increases < 1e-8, accept
# the harmonic-removed model with the corresponding df: removing a boundary
# parameter shrinks q, so df INCREASES (deliberate convention, design sec. 3.5 --
# a boundary parameter is not free; the reduced-df... i.e. reduced-q reference
# is the conservative-leaning choice under the chi-square-mixture asymptotics).
# Records removed harmonics.
cpm_polish_beta <- function(fit, R, spec) {
  removed <- integer(0)
  nat <- cpm_unpack(fit$par, spec)
  small <- which(nat$beta[-1] < 1e-2)       # k indices (1-based over 1..m)
  if (length(small) == 0) {
    return(list(fit = fit, spec = spec, removed = removed))
  }

  # Refit with the flagged harmonics removed. The reduced model keeps beta
  # supported on keep_k only; because every engine function (unpack, gradient,
  # objective, canonicalize) reads spec$keep_k, the SAME code path handles the
  # reduced model -- only the spec's beta bookkeeping is rebuilt.
  keep_k <- setdiff(0:spec$m, small)        # harmonics kept (always includes 0)
  rspec <- cpm_spec_reduce(spec, keep_k)

  # warm-started par for the reduced model: keep angle/zeta blocks; rebuild v
  # from the surviving betas (renormalized over keep_k).
  beta_keep <- nat$beta[keep_k + 1L]
  beta_keep <- pmax(beta_keep, 1e-6)
  beta_keep <- beta_keep / sum(beta_keep)
  g0 <- numeric(rspec$q)
  if (spec$free_angles > 0) g0[rspec$i_angle] <- fit$par[spec$i_angle]
  if (spec$n_zeta > 0) g0[rspec$i_zeta] <- fit$par[spec$i_zeta]
  v <- log(beta_keep) - log(beta_keep[1])
  g0[rspec$i_beta] <- v[-1]

  rfit <- cpm_optimize_one(g0, R, rspec)

  if (rfit$F <= fit$F + 1e-8) {
    return(list(fit = rfit, spec = rspec, removed = small))
  }
  list(fit = fit, spec = spec, removed = integer(0))
}

# =============================================================================
# cpm_fit() user-facing API (design sec. 4-5, sec. 7) and the fit-index / analytic-CI
# machinery it needs. The engine above is internal; this layer adds input
# handling, fit indices from the discrepancy, analytic (Wald) confidence
# intervals, and the circumplex_cpm object (constructor + methods in
# R/cpm_oop.R).
# =============================================================================

# ---- fit indices (design sec. 5.3) ------------------------------------------

#' RMSEA 90% confidence interval by noncentral chi-square inversion
#'
#' Finds `lambda_L`, `lambda_U` with `pchisq(T, df, ncp = lambda_L) = 1 - a` and
#' `pchisq(T, df, ncp = lambda_U) = a` (a = .05 for a 90% interval), then maps
#' to the RMSEA scale via `sqrt(lambda / (n * df))` (design sec. 5.3). BOTH edge
#' guards are applied: the lower ncp collapses to 0 for good fits
#' (`pchisq(T, df) < 1 - a`), and the upper ncp collapses to 0 for excellent
#' fits (`pchisq(T, df) < a`), for which the `lambda_U` equation has no positive
#' root and an unguarded uniroot would error -- with the guard the interval is
#' correctly `[0, 0]`.
#'
#' Note: the design doc sec. 5.3 states the lower-guard inequality as
#' "lambda_L = 0 when pchisq(T, df) >= .95", which is the opposite of the
#' condition its own worked example (T = 20, df = 40 -> [0, 0]) requires. The
#' standard condition implemented here (`pchisq(T, df) < 1 - a`) reproduces that
#' example; the design change log records the correction.
#'
#' @noRd
cpm_rmsea_ci <- function(Tstat, df, n, level = 0.90) {
  a <- (1 - level) / 2
  lower_fun <- function(l) stats::pchisq(Tstat, df, ncp = l) - (1 - a)
  upper_fun <- function(l) stats::pchisq(Tstat, df, ncp = l) - a

  # Both functions are strictly decreasing in the ncp; a guarded uniroot with an
  # expanding upper bracket locates the root when f(0) > 0.
  find_ncp <- function(f) {
    hi <- 1
    while (f(hi) > 0 && hi < 1e7) hi <- hi * 2
    if (f(hi) > 0) return(hi)                 # capped (pathological); rare
    stats::uniroot(f, c(0, hi))$root
  }

  lambda_l <- if (lower_fun(0) < 0) 0 else find_ncp(lower_fun)
  lambda_u <- if (upper_fun(0) < 0) 0 else find_ncp(upper_fun)
  c(sqrt(lambda_l / (n * df)), sqrt(lambda_u / (n * df)))
}

#' Fit indices from the ML discrepancy (design sec. 5.3)
#'
#' `T = n * F_hat` with `n = N - 1` (Wishart df; design sec. 3.1). The null model
#' is independence (`P0 = I`), for which `F0 = -ln|R|` and `df0 = p(p-1)/2`.
#' SRMR uses the off-diagonal-only denominator `p(p-1)/2` (design sec. 5.3;
#' the diagonal residuals are identically 0 here). AIC/BIC use `ln N`.
#'
#' @param q number of free parameters (as fitted, after any boundary polish).
#' @noRd
cpm_fit_indices <- function(Fhat, df, p, N, R, Phat, q) {
  n <- N - 1L                                 # Wishart multiplier (design sec. 3.1)
  Tstat <- n * Fhat
  npair <- p * (p - 1) / 2
  F0 <- -as.numeric(determinant(R, logarithm = TRUE)$modulus)  # tr(R) = p
  T0 <- n * F0
  df0 <- npair

  has_df <- df >= 1
  pvalue <- if (has_df) stats::pchisq(Tstat, df, lower.tail = FALSE) else NA_real_
  rmsea <- if (has_df) sqrt(max(Fhat / df - 1 / n, 0)) else NA_real_
  rmsea_ci <- if (has_df) cpm_rmsea_ci(Tstat, df, n) else c(NA_real_, NA_real_)

  # SRMR: off-diagonal only, denominator p(p-1)/2 (design sec. 5.3 / sec. 6.3).
  resid <- R - Phat
  srmr <- sqrt(sum(resid[upper.tri(resid)]^2) / npair)

  # Incremental indices vs the independence null.
  cfi <- if (has_df) 1 - max(Tstat - df, 0) / max(T0 - df0, Tstat - df, 0) else NA_real_
  tli <- if (has_df) ((T0 / df0) - (Tstat / df)) / ((T0 / df0) - 1) else NA_real_

  aic <- Tstat + 2 * q
  bic <- Tstat + q * log(N)

  list(
    chisq = Tstat, df = df, pvalue = pvalue,
    rmsea = rmsea, rmsea_ci = rmsea_ci, srmr = srmr,
    cfi = cfi, tli = tli, aic = aic, bic = bic,
    F = Fhat, n = n, N = N
  )
}

# ---- analytic (Wald) confidence intervals (design sec. 5.2) -----------------

#' Hessian of F in the unconstrained coordinates via FD of the analytic gradient
#'
#' Central finite differences of [cpm_gradient()] (step 1e-5), symmetrized
#' (design sec. 5.2). Computed fresh at the reported (canonicalized) solution;
#' the engine's own condition-number Hessian is left untouched.
#'
#' @noRd
cpm_hessian_fd <- function(par, R, spec, step = 1e-5) {
  q <- length(par)
  H <- matrix(0, q, q)
  for (i in seq_len(q)) {
    pp <- par; pm <- par
    pp[i] <- pp[i] + step
    pm[i] <- pm[i] - step
    H[, i] <- (cpm_gradient(pp, R, spec) - cpm_gradient(pm, R, spec)) / (2 * step)
  }
  (H + t(H)) / 2
}

#' Analytic standard errors for the natural parameters (design sec. 5.2)
#'
#' `avar(gamma*) = (2/n) H^-1`, `n = N - 1`, delta-method back to natural
#' parameters: angles have Jacobian 1 (SE reported in degrees); zeta via the
#' logit Jacobian `zeta(1 - zeta)`; beta via the softmax Jacobian. Returns
#' per-scale angle/zeta SEs and a length-(m+1) beta SE vector (0 for the
#' reference angle and any polished-out harmonic). These are Wald SEs and may
#' imply intervals outside the natural range near a boundary -- that is itself a
#' signal the analytic CI is untrustworthy (design sec. 5.2; the N-conditional
#' `summary()` caution).
#'
#' @noRd
cpm_analytic_se <- function(engine, R, N) {
  spec <- engine$spec
  par <- engine$par
  p <- spec$p
  n <- N - 1L

  H <- cpm_hessian_fd(par, R, spec)
  Hinv <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(Hinv)) {
    # Singular information: SEs undefined. Return NA so CIs surface as NA rather
    # than crash (the Hessian-condition warning already fired in the engine).
    return(list(
      angle = rep(NA_real_, p), zeta = rep(NA_real_, p),
      beta = rep(NA_real_, spec$m + 1L)
    ))
  }
  avar <- (2 / n) * Hinv
  se_g <- sqrt(pmax(diag(avar), 0))

  # angles (degrees); reference is fixed => SE 0
  se_angle <- numeric(p)
  if (spec$free_angles > 0) {
    se_angle[spec$free_pos] <- se_g[spec$i_angle] * (180 / pi)
  }

  # zeta via logit Jacobian
  zeta <- engine$zeta
  se_zeta <- numeric(p)
  if (spec$n_zeta == p) {
    se_u <- se_g[spec$i_zeta]
    se_zeta <- zeta * (1 - zeta) * se_u
  } else {
    z <- zeta[1]
    se_zeta <- rep(z * (1 - z) * se_g[spec$i_zeta], p)
  }

  # beta via the softmax Jacobian over the KEPT harmonics
  beta <- engine$beta                         # length m+1 (0 at removed k)
  se_beta <- numeric(spec$m + 1L)
  if (spec$n_beta_free > 0) {
    keep <- spec$keep_k
    bk <- beta[keep + 1L]                      # kept betas, length L
    L <- length(bk)
    avar_v <- avar[spec$i_beta, spec$i_beta, drop = FALSE]  # (L-1) x (L-1)
    # J[a, l] = d beta_a / d v_l, column l -> kept position l + 1 (v_0 fixed).
    J <- matrix(0, L, L - 1L)
    for (a in seq_len(L)) {
      for (l in seq_len(L - 1L)) {
        J[a, l] <- bk[a] * ((a == (l + 1L)) - bk[l + 1L])
      }
    }
    covb <- J %*% avar_v %*% t(J)
    se_beta[keep + 1L] <- sqrt(pmax(diag(covb), 0))
  }

  list(angle = se_angle, zeta = se_zeta, beta = se_beta)
}

# ---- cpm_fit(): the user-facing constructor ---------------------------------

# Below this N, summary() cautions that analytic CIs may materially mis-cover
# and points to the bootstrap (design sec. 5.2). Provisional value pending the
# B6 coverage-oracle calibration.
cpm_analytic_ci_n_caution <- 2000L

#' Fit Browne's circular stochastic process model (circumplex fit statistics)
#'
#' Estimate Browne's (1992) circular stochastic process model (CPM) for the
#' correlational structure of a set of circumplex scales or items, the native
#' replacement for the archived CircE package. Each variable is modeled as a
#' point on a circle at an estimated angle, with a communality index and a
#' shared correlation function; the fit of that structure is summarized with the
#' usual covariance-structure indices (chi-square, RMSEA, SRMR, CFI, TLI).
#'
#' @param data A data frame or matrix containing the circumplex scales (raw-data
#'   path). Supply exactly one of `data` or `cormat`.
#' @param scales For the raw-data path, a character vector of column names (or a
#'   numeric vector of column indexes) selecting the circumplex scales. For the
#'   `cormat` path, optional labels for the variables (defaults to the matrix
#'   dimnames, or `V1`, `V2`, ...).
#' @param angles A numeric vector of the theoretical angular displacement of
#'   each scale, in degrees, used both as the reference/identifying angle and as
#'   optimization start values (default = [octants()]). Its length must match
#'   the number of scales.
#' @param cormat A correlation matrix (the matrix-input path, CircE-style).
#'   Supply exactly one of `data` or `cormat`. Must be symmetric with a unit
#'   diagonal and positive definite.
#' @param n For the `cormat` path, the sample size (number of observations) the
#'   correlation matrix was computed from. The test statistic uses `N - 1` (the
#'   Wishart degrees of freedom); pass the raw sample size here.
#' @param m The number of harmonics in the correlation function (default = 3,
#'   the octant-scale convention). Capped at `floor((p - 1) / 2)` for the
#'   free-angle variants and `floor(p / 2)` for the fixed-angle variants.
#' @param model The model variant (design of Browne 1992): `"quasi-circumplex"`
#'   (default; free angles and communalities), `"constrained-angles"` (angles
#'   fixed at their theoretical values), `"equal-communality"` (a single shared
#'   communality), or `"circulant"` (both constraints).
#' @param reference The index into `scales` of the variable whose angle is fixed
#'   at its theoretical value to identify the rotation (default = 1).
#' @param interval The confidence level for the parameter intervals (default =
#'   0.95). The RMSEA interval is always the conventional 90 percent.
#' @param ci_method How to construct confidence intervals: `"analytic"`
#'   (default) uses Wald intervals from the information matrix. `"bootstrap"`
#'   (raw-data path) is added in a later release. On the `cormat` path only
#'   `"analytic"` is available (there is no raw data to resample).
#' @param boots The number of bootstrap resamples (reserved for the bootstrap
#'   method; default = 2000).
#' @param listwise Whether to handle missing values by listwise deletion. Only
#'   listwise deletion is supported in this release (default = TRUE).
#' @return A `circumplex_cpm` object: a list with `results` (a data frame of
#'   estimated angles and communality indices with confidence intervals),
#'   `betas` (the correlation-function weights), `fit` (the fit indices),
#'   `corfun` (the estimated correlation function), `matrices` (the sample and
#'   model-implied matrices and residuals), and `details` (model, diagnostics,
#'   and settings). See [print.circumplex_cpm()] and [summary.circumplex_cpm()].
#' @section Confidence intervals:
#'   Analytic (Wald) intervals are asymptotically valid but can materially
#'   mis-cover at field-typical sample sizes; `summary()` prints a caution below
#'   `n = 2000`. Prefer the bootstrap on the raw-data path when available.
#' @references Browne, M. W. (1992). Circumplex models for correlation matrices.
#'   \emph{Psychometrika, 57}(4), 469-497.
#' @family analysis functions
#' @export
#' @examples
#' # Raw-data path on the eight IIP-SC octant scales
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' fit <- cpm_fit(jz2017, scales = scales)
#' fit
#'
#' # Matrix-input path (supply the sample size)
#' R <- cor(jz2017[scales])
#' cpm_fit(cormat = R, scales = scales, n = nrow(jz2017))
#'
cpm_fit <- function(data = NULL, scales = NULL, angles = octants(),
                    cormat = NULL, n = NULL, m = 3,
                    model = c("quasi-circumplex", "constrained-angles",
                              "equal-communality", "circulant"),
                    reference = 1, interval = 0.95,
                    ci_method = c("analytic", "bootstrap"),
                    boots = 2000, listwise = TRUE) {

  call <- match.call()
  model <- match.arg(model)
  ci_method <- match.arg(ci_method)
  variant <- switch(model,
    "quasi-circumplex"   = "A",
    "constrained-angles" = "B",
    "equal-communality"  = "C",
    "circulant"          = "D"
  )

  # Exactly one of data / cormat (design sec. 4).
  has_data <- !is.null(data)
  has_cormat <- !is.null(cormat)
  if (has_data == has_cormat) {
    stop("Supply exactly one of `data` or `cormat`.", call. = FALSE)
  }

  # Scalar-argument validation via the house is_*() helpers.
  stopifnot(is_count(reference), reference >= 1)
  stopifnot(is_count(m), m >= 1)
  stopifnot(is.numeric(interval), length(interval) == 1, interval > 0, interval < 1)
  stopifnot(is_flag(listwise))
  stopifnot(is.numeric(boots), length(boots) == 1, boots > 0,
            ceiling(boots) == floor(boots))
  if (!isTRUE(listwise)) {
    stop("Only listwise deletion is supported (`listwise = TRUE`).", call. = FALSE)
  }

  angles <- as.numeric(angles)                # accept circumplex_degree or numeric
  stopifnot(is.numeric(angles))

  if (has_cormat) {
    R <- as.matrix(cormat)
    stopifnot(is.matrix(R), nrow(R) == ncol(R))
    p <- nrow(R)
    if (!isSymmetric(unname(R), tol = 1e-8)) {
      stop("`cormat` must be symmetric.", call. = FALSE)
    }
    if (max(abs(diag(R) - 1)) > 1e-8) {
      stop("`cormat` must have a unit diagonal (a correlation matrix).",
           call. = FALSE)
    }
    if (is.null(n)) {
      stop("`n` (the sample size) is required with `cormat`.", call. = FALSE)
    }
    stopifnot(is_count(n), length(n) == 1, n > p)
    N <- as.integer(n)
    if (is.null(scales)) {
      scales <- if (!is.null(colnames(R))) colnames(R) else paste0("V", seq_len(p))
    }
    stopifnot(length(scales) == p)
    if (ci_method == "bootstrap") {
      stop("`ci_method = \"bootstrap\"` needs raw `data`; the `cormat` path ",
           "supports only \"analytic\".", call. = FALSE)
    }
  } else {
    stopifnot(is.data.frame(data) || is.matrix(data))
    if (is.matrix(data)) data <- as.data.frame(data)
    stopifnot(is_var(scales))
    sdata <- data[scales]
    p <- ncol(sdata)
    sdata <- stats::na.omit(sdata)            # listwise (only option; design sec. 4)
    N <- nrow(sdata)
    if (N <= p) {
      stop("Too few complete observations (", N, ") for ", p, " scales.",
           call. = FALSE)
    }
    R <- stats::cor(as.matrix(sdata))
    scales <- colnames(sdata)
  }

  stopifnot(length(angles) == p)
  if (reference > p) {
    stop("`reference` (", reference, ") exceeds the number of scales (", p, ").",
         call. = FALSE)
  }

  # Bootstrap CIs arrive in a later milestone task (M4/B3); until then the
  # raw-data path also uses analytic CIs, and this branch is unreachable via the
  # default. (When B3 lands, bootstrap becomes the raw-data default per the
  # design sec. 5.2 / sec. 10 decision.)
  if (ci_method == "bootstrap") {
    stop("bootstrap confidence intervals are not yet implemented; ",
         "use `ci_method = \"analytic\"`.", call. = FALSE)
  }

  # ---- fit the engine (deterministic; no RNG on this path) ----
  engine <- cpm_engine(R, angles = angles, m = m, variant = variant,
                       reference = reference)

  # ---- fit indices and analytic CIs ----
  q <- engine$spec$q
  fit <- cpm_fit_indices(engine$F, engine$df, p, N, R, engine$P, q)
  se <- cpm_analytic_se(engine, R, N)
  z <- stats::qnorm(1 - (1 - interval) / 2)

  # ---- results table (design sec. 5.4) ----
  # Angle CIs are Wald on the branch of the reported estimate (estimate always
  # inside; endpoints may print < 0 or >= 360 near the 0/360 pole, mirroring the
  # unwrapped-branch convention of the displacement CIs; design sec. 2.4).
  results <- data.frame(
    Scale = scales,
    # Echo the user's supplied theoretical angles (LM = 360 per octants() and
    # the CLAUDE.md convention); the engine wraps 360 -> 0 internally for the
    # trig, which would otherwise misreport the top pole as 0.
    Angle_theory = angles,
    Angle = engine$theta,
    Angle_lci = engine$theta - z * se$angle,
    Angle_uci = engine$theta + z * se$angle,
    Zeta = engine$zeta,
    Zeta_lci = engine$zeta - z * se$zeta,
    Zeta_uci = engine$zeta + z * se$zeta,
    Communality = engine$zeta^2,
    stringsAsFactors = FALSE
  )

  betas <- data.frame(
    k = 0:engine$spec$m,
    Beta = engine$beta,
    Beta_lci = engine$beta - z * se$beta,
    Beta_uci = engine$beta + z * se$beta
  )

  corfun <- local({
    beta_hat <- engine$beta
    function(delta_deg) cpm_rho(as.numeric(delta_deg) * (pi / 180), beta_hat)
  })

  matrices <- list(R = R, Phat = engine$P, residuals = R - engine$P)

  details <- list(
    m = engine$m,                             # as fitted (after any polish)
    m_requested = m,
    model = model,
    variant = variant,
    reference = reference,
    scales = scales,
    ci_method = ci_method,
    interval = interval,
    boots = boots,
    listwise = listwise,
    N = N,
    accepted = engine$accepted,
    nlminb_code = engine$nlminb_code,         # advisory only (design sec. 3.5)
    gradient_norm = engine$gradient_norm,
    hessian_condition = engine$hessian_condition,
    heywood = engine$heywood,
    removed_harmonics = engine$removed_harmonics,
    multimodal = engine$multimodal,
    # internal handles for the later bootstrap / simulate tasks (B3/B4)
    spec = engine$spec,
    par = engine$par,
    theta_rad = engine$theta_rad,
    theta_rad_unwrapped = engine$theta_rad_unwrapped
  )

  new_cpm(
    results = results,
    betas = betas,
    fit = fit,
    corfun = corfun,
    matrices = matrices,
    details = details,
    call = call
  )
}
