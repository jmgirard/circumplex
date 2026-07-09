# CPM CI simulation study -- config-table-driven factorial (plan sec. 10.1).
#
# Replaces the B6 oracle's two hardcoded configs with a generated cell table.
# Each cell carries: the generating population (angle set, zeta/beta config, or
# an out-of-family perturbation), the fitted model (variant, m), N, stage tag,
# the COVERAGE TRUTH aligned to the fitted parameterization, and -- under
# misspecification -- the pseudo-true projection gamma*(P0) with its guards,
# F*, population RMSEA, and boundary-status column (sec. 2.4).
#
# NOTHING here runs the study: build_config_table() only does the deterministic,
# large-n limit fits (projection to the exact population matrix). No simulated
# data, no bootstrap. Cache the returned object; run.R consumes it.

# ---- angle sets (sec. 3.1; pinned numerically -- these are generating,
# hence pre-registration content) --------------------------------------------
ANGLE_SETS <- list(
  p8_equal     = c(45, 90, 135, 180, 225, 270, 315, 360),          # octants
  p8_perturbed = c(60, 75, 150, 165, 240, 255, 330, 345),          # +/-15 offsets
  p8_clustered = c(45, 90, 100, 110, 200, 245, 290, 360),          # 20-deg cluster + 90-deg gap
  p16_equal    = seq(22.5, 360, by = 22.5)                          # 16 equal
)
# Equally-spaced pathology cells flagged for the RQ5 interaction analysis.
EQUAL_SPACED_SETS <- c("p8_equal", "p16_equal")

# ---- beta configurations (sec. 3.1); m0 = length - 1 ------------------------
BETA_CONFIGS <- list(
  interior    = c(0.35, 0.30, 0.20, 0.15),           # m0 = 3, all interior
  b0_dominant = c(0.70, 0.15, 0.10, 0.05),           # m0 = 3, general factor
  m2_truth    = c(0.45, 0.35, 0.20)                  # m0 = 2 (misspec arm)
)
# Trailing-harmonic ladder beta = (.50 - t, .35, .15, t): beta_0 absorbs what
# the trailing harmonic gives up (t = .05 reproduces B6 "boundary" exactly).
TRAILING_T <- c(t000 = 0.00, t002 = 0.02, t005 = 0.05, t010 = 0.10)
for (nm in names(TRAILING_T)) {
  t <- TRAILING_T[[nm]]
  BETA_CONFIGS[[paste0("trail_", nm)]] <- c(0.50 - t, 0.35, 0.15, t)
}

# ---- zeta configurations (sec. 3.1) -----------------------------------------
# Homogeneous levels + heterogeneity patterns (crossed only at zeta-bar = .75).
zeta_homo <- function(p, level) rep(level, p)
zeta_alt  <- function(p) rep(c(0.6, 0.9), length.out = p)           # alternating
zeta_weak <- function(p) c(rep(0.9, p - 1L), 0.4)                   # one weak item

# ---- N grids (sec. 3.1) -----------------------------------------------------
N_FIELD    <- c(100, 250, 500, 1000, 2000)          # bootstrap-armed
N_ANALYTIC <- c(5000, 10000, 20000, 50000)          # analytic-only extension
N_ALL      <- c(N_FIELD, N_ANALYTIC)

# ---------------------------------------------------------------------------
# Population construction
# ---------------------------------------------------------------------------

# Exact population correlation matrix P0 = P(theta0, zeta0, beta0).
make_population_matrix <- function(angles_deg, zeta0, beta0) {
  cpm_implied_cor(deg2rad_wrapped(angles_deg), zeta0, beta0)
}

# Out-of-family population (sec. 3.3): P0' = nearest-PSD repair of P0 + s*E0,
# unit-diagonal rescaled, with the scalar s solved so the population RMSEA of
# the fitted projection ~= target (computed, not assumed, via sec. 2.4). E0 is
# a fixed seeded symmetric off-diagonal perturbation. Returns the matrix and s.
make_population_oof <- function(angles_deg, zeta0, beta0, fit_angles_deg,
                                variant, m_fit, reference = 1,
                                target_rmsea = 0.05, seed = BASE_SEED + 99L) {
  P0 <- make_population_matrix(angles_deg, zeta0, beta0)
  p <- nrow(P0)
  set.seed(seed)                                     # fixed, seeded (sec. 3.3)
  E0 <- matrix(stats::rnorm(p * p), p, p)
  E0 <- (E0 + t(E0)) / 2
  diag(E0) <- 0
  E0 <- E0 / max(abs(E0[upper.tri(E0)]))             # unit max off-diagonal
  near_psd_cor <- function(M) {                      # eigen-clip + unit diag
    M <- (M + t(M)) / 2
    e <- eigen(M, symmetric = TRUE)
    v <- pmax(e$values, 1e-6)
    M2 <- e$vectors %*% (v * t(e$vectors))
    d <- sqrt(diag(M2))
    M2 / tcrossprod(d)
  }
  pop_rmsea <- function(s) {
    Ps <- near_psd_cor(P0 + s * E0)
    eng <- tryCatch(suppressWarnings(
      cpm_engine(Ps, angles = fit_angles_deg, m = m_fit, variant = variant,
                 reference = reference)), error = function(e) NULL)
    if (is.null(eng) || !eng$accepted) return(NA_real_)
    sqrt(max(eng$F, 0) / eng$df)
  }
  # Bracket s so pop_rmsea(s_hi) >= target: EXPAND upward when RMSEA undershoots
  # (halving would push it further below target -- review S1); halve only on NA
  # (projection failing on an over-perturbed matrix).
  s_hi <- 0.5
  for (k in seq_len(40)) {
    r <- pop_rmsea(s_hi)
    if (is.na(r)) { s_hi <- s_hi / 2 }
    else if (r < target_rmsea) { s_hi <- s_hi * 2 }
    else break
    if (s_hi > 1e6 || s_hi < 1e-6) break
  }
  lo <- 0; hi <- s_hi
  for (it in seq_len(40)) {
    mid <- (lo + hi) / 2
    r <- pop_rmsea(mid)
    if (is.na(r)) { hi <- mid; next }
    if (r < target_rmsea) lo <- mid else hi <- mid
    if (abs(r - target_rmsea) < 1e-3) break
  }
  s <- (lo + hi) / 2
  list(P0 = near_psd_cor(P0 + s * E0), s = s)
}

# ---------------------------------------------------------------------------
# Pseudo-truth projection and coverage-truth alignment (sec. 2.4)
# ---------------------------------------------------------------------------

# ---- well-definedness guard (sec. 2.4, amended + ratified 2026-07-09) --------
# The engine's `accepted` flag is NOT the design-time key: accepted = grad_ok &&
# reproduced (R/cpm_fit.R), and its REPRODUCED limb certifies sample-fit
# start-independence, not estimand existence -- a legitimate boundary projection
# can have a unique global minimum only one deterministic start finds. So the
# guard keys on convergence + KKT-at-ceiling + statistical unimodality + (for a
# circulant population) symmetry preservation. (Fable ratification, N1/N2.)

# ML discrepancy core: monotone in the engine's F (constants -log|R|-p dropped),
# so its zeta-derivative SIGN matches the engine objective. tr(P0 P^-1) via the
# symmetric elementwise product.
proj_ml_F <- function(P0, P)
  as.numeric(determinant(P, logarithm = TRUE)$modulus) + sum(P0 * solve(P))
.is_pd <- function(P) min(eigen(P, symmetric = TRUE, only.values = TRUE)$values) > 1e-10

# (ii) KKT sign at ceiling communalities: any zeta*_i > 0.999 must have
# natural-scale dF/dzeta_i <= 0 (F non-increasing toward the bound) -- then the
# ceiling is a constrained minimizer and zeta*_i = 1 is the true value. A
# saturated coordinate with dF/dzeta_i > 0 is an optimizer artifact -> drop.
proj_kkt_ok <- function(P0, theta_rad, zeta, beta, h = 1e-6) {
  ceil <- which(zeta > 0.999)
  if (!length(ceil)) return(TRUE)
  for (i in ceil) {
    zm <- zeta; zm[i] <- zeta[i] - h
    zp <- zeta; zp[i] <- zeta[i] + h
    Pm <- cpm_implied_cor(theta_rad, zm, beta)
    Pp <- cpm_implied_cor(theta_rad, zp, beta)
    dF <- if (.is_pd(Pp)) (proj_ml_F(P0, Pp) - proj_ml_F(P0, Pm)) / (2 * h)
          else (proj_ml_F(P0, cpm_implied_cor(theta_rad, zeta, beta)) -
                  proj_ml_F(P0, Pm)) / h
    if (dF > 1e-6) return(FALSE)
  }
  TRUE
}

# (iii) statistical unimodality: min separation (N_max - 1) * dF over converged
# DISTINCT basins from the engine start set + pinned extras (+/-7.5/22.5/45 deg
# angle jitters; zeta-starts 0.5, 0.9). Returns Inf when no distinct basin is
# found; unimodal iff >= 10 (LR-scale separation, ~exp(-10/2) mis-selection).
proj_basin_sep <- function(P0, spec, best_par, best_F, n_max) {
  theta_theory <- spec$theta_fixed
  sv <- cpm_start_values(P0, theta_theory, spec$m)
  starts <- list(cpm_pack(theta_theory, sv$zeta, sv$beta, spec))
  if (spec$free_angles > 0) for (off in c(-45, -22.5, -7.5, 7.5, 22.5, 45)) {
    th <- theta_theory
    th[spec$free_pos] <- theta_theory[spec$free_pos] + off * pi / 180
    starts[[length(starts) + 1L]] <- cpm_pack(th, sv$zeta, sv$beta, spec)
  }
  for (fac in c(0.5, 0.9)) {
    z <- pmin(pmax(sv$zeta * fac, 0.05), 0.999)
    starts[[length(starts) + 1L]] <- cpm_pack(theta_theory, z, sv$beta, spec)
  }
  ref_rel_best <- cpm_ref_relative(best_par, spec)
  nat_best <- cpm_unpack(best_par, spec)
  sep <- Inf
  for (st in starts) {
    run <- tryCatch(cpm_optimize_one(st, P0, spec), error = function(e) NULL)
    if (is.null(run)) next
    if (max(abs(cpm_gradient(run$par, P0, spec))) > 1e-6 * max(1, abs(run$F)))
      next                                                # not converged
    guard <- cpm_mirror_guard(run$par, spec, ref_rel_best)
    nat <- cpm_unpack(guard$par, spec)
    dang <- if (spec$free_angles > 0) max(abs(as.numeric(angle_dist(
      as_radian(cpm_ref_relative(guard$par, spec)), as_radian(ref_rel_best)))))
      else 0
    dnat <- max(abs(nat$zeta - nat_best$zeta), abs(nat$beta - nat_best$beta), dang)
    if (dnat > 1e-3) sep <- min(sep, (n_max - 1) * abs(run$F - best_F))
  }
  sep
}

# (iv) circulance: P0 invariant under the cyclic index shift (test the MATRIX --
# out-of-family perturbations break circulance). A symmetry-broken projection of
# a circulant population is defined only up to the cyclic orbit -> no per-item truth.
is_circulant <- function(P0) {
  p <- nrow(P0); sh <- c(2:p, 1L)
  max(abs(P0 - P0[sh, sh])) < 1e-8
}
proj_symmetry_ok <- function(P0, eng) {
  if (!is_circulant(P0)) return(TRUE)
  (max(eng$zeta) - min(eng$zeta)) <= 1e-6 &&
    max(abs(ang_signed(eng$theta, eng$theta_theory))) <= 1e-4
}

# Project the population matrix onto the FITTED family (sec. 2.4). `full_guard`
# runs the (ii)-(iv) checks -- needed only when the projection DEFINES the
# estimand (truth_source == "projection"); generating cells take the truth from
# gamma0 and use the projection only as a sanity gate. `n_max` is the largest N
# in the cell family (drives the unimodality separation threshold).
project_truth <- function(P0, fit_angles_deg, variant, m_fit, reference = 1,
                        full_guard = FALSE, n_max = max(N_ALL)) {
  canon_warn <- FALSE
  eng <- tryCatch(
    withCallingHandlers(
      cpm_engine(P0, angles = fit_angles_deg, m = m_fit, variant = variant,
                 reference = reference),
      warning = function(w) {
        if (grepl("canonicaliz", conditionMessage(w))) canon_warn <<- TRUE
        invokeRestart("muffleWarning")
      }),
    error = function(e) structure(list(msg = conditionMessage(e)),
                                  class = "cpm_proj_error"))
  if (inherits(eng, "cpm_proj_error"))
    return(list(ok = FALSE, drop_reason = paste("projection error:", eng$msg)))

  polished <- length(eng$removed_harmonics) > 0
  near_bound <- polished || isTRUE(eng$heywood) ||
    (length(eng$beta) > 0 && min(eng$beta[eng$beta > 0]) < 0.02) ||
    max(eng$zeta) > 0.99
  converged <- isTRUE(eng$gradient_norm < 1e-6 * max(1, abs(eng$F)))

  kkt_ok <- unimodal <- symmetry_ok <- TRUE; sep <- Inf
  if (full_guard && converged) {
    kkt_ok <- proj_kkt_ok(P0, eng$theta_rad, eng$zeta, eng$beta)
    sep <- proj_basin_sep(P0, eng$spec, eng$par, eng$F, n_max)
    unimodal <- is.infinite(sep) || sep >= 10
    symmetry_ok <- proj_symmetry_ok(P0, eng)
  }
  guard_reason <- if (!converged) "projection did not converge" else
    if (!kkt_ok) "ceiling communality fails KKT (optimizer artifact)" else
    if (!symmetry_ok) "symmetry-broken projection of circulant population" else
    if (!unimodal) sprintf("near-tied basins ((Nmax-1)dF = %.2g < 10)", sep) else
    NA_character_

  list(
    ok = TRUE, engine = eng,
    theta = eng$theta, zeta = eng$zeta, beta = eng$beta,
    Fstar = eng$F, df = eng$df, rmsea_pop = sqrt(max(eng$F, 0) / eng$df),
    accepted = isTRUE(eng$accepted), converged = converged,
    kkt_ok = kkt_ok, unimodal = unimodal, symmetry_ok = symmetry_ok,
    basin_sep = sep, multimodal = isTRUE(eng$multimodal),
    heywood = isTRUE(eng$heywood), polished = polished, canon_warn = canon_warn,
    boundary_status = if (polished) "polished" else if (near_bound) "near-bound"
                      else "interior",
    well_defined = converged && kkt_ok && unimodal && symmetry_ok,
    guard_reason = guard_reason
  )
}

# ---------------------------------------------------------------------------
# Cell records
# ---------------------------------------------------------------------------
# A cell is a plain list. `truth_source`:
#   "generating" -- correct spec / overfit: coverage truth is gamma0 aligned to
#                   the fitted m (a trailing 0 for the overfit harmonic); the
#                   projection is still computed as a sanity gate.
#   "projection" -- underfit / wrong-fixed / out-of-family: coverage truth IS
#                   the pseudo-truth gamma*(P0).
new_cell <- function(id, stage, angle_set, zeta0, beta0, m0,
                     variant_fit, fit_angles_deg, m_fit, spec_note,
                     truth_source, N, arm, flags = list()) {
  angles0 <- ANGLE_SETS[[angle_set]]
  list(
    id = id, stage = stage, arm = arm, spec_note = spec_note,
    angle_set = angle_set, angles0 = angles0, p = length(angles0),
    reference = 1L,
    zeta0 = zeta0, beta0 = beta0, m0 = m0,
    variant_fit = variant_fit,               # engine variant code "A"/"B"
    fit_angles = fit_angles_deg,             # deg vector supplied to the fit
    m_fit = m_fit, truth_source = truth_source, N = N,
    equal_spaced = angle_set %in% EQUAL_SPACED_SETS,
    flags = flags
  )
}

# Attach the resolved coverage truth + projection guards to a cell (the only
# place the large-n limit fit is called). Populations are cached by key so a
# cell family sharing a population pays the projection once per (population,
# fitted model), not once per N.
resolve_cell <- function(cell, pop_cache = new.env(parent = emptyenv()),
                        n_max = max(N_ALL)) {
  # population matrix (out-of-family cells carry a prebuilt P0 in flags)
  if (!is.null(cell$flags$P0)) {
    P0 <- cell$flags$P0
  } else {
    pkey <- paste(cell$angle_set, paste(cell$zeta0, collapse = ","),
                  paste(cell$beta0, collapse = ","), sep = "|")
    if (is.null(pop_cache[[pkey]])) {
      pop_cache[[pkey]] <- make_population_matrix(cell$angles0, cell$zeta0,
                                                  cell$beta0)
    }
    P0 <- pop_cache[[pkey]]
  }
  # The (ii)-(iv) guard runs only when the projection DEFINES the estimand;
  # generating cells take the truth from gamma0 (the projection is a sanity gate).
  full_guard <- cell$truth_source == "projection"
  proj <- project_truth(P0, cell$fit_angles, cell$variant_fit, cell$m_fit,
                        cell$reference, full_guard = full_guard, n_max = n_max)
  if (!isTRUE(proj$ok)) {                # infeasible model -> recorded drop (M2)
    cell$P0 <- P0; cell$well_defined <- FALSE
    cell$drop_reason <- proj$drop_reason
    cell$bootstrap <- FALSE
    return(cell)
  }

  if (cell$truth_source == "generating") {
    # coverage truth from gamma0, aligned to the fitted parameterization.
    theta_truth <- cell$angles0 %% 360
    zeta_truth  <- cell$zeta0
    beta_truth  <- numeric(cell$m_fit + 1L)
    kmax <- min(cell$m0, cell$m_fit)
    beta_truth[seq_len(kmax + 1L)] <- cell$beta0[seq_len(kmax + 1L)]
    # overfit: trailing harmonic(s) truth is exactly 0 (already zero-padded).
    # sanity gate: the projection must recover this truth (m_fit = m0) or, for
    # overfit, leave the extra harmonic ~0. Tolerances match B6's make_truth
    # exactness (1e-6 on zeta/beta; theta to the optimizer's angle precision --
    # review S9).
    sane <- max(abs(ang_signed(proj$theta, theta_truth))) < 1e-4 &&
      max(abs(proj$zeta - zeta_truth)) < 1e-6 &&
      max(abs(proj$beta - beta_truth)) < 1e-6
    boundary_status <- proj$boundary_status
    well_defined <- TRUE            # generating truth is defined by construction
  } else {
    theta_truth <- proj$theta
    zeta_truth  <- proj$zeta
    beta_truth  <- proj$beta
    sane <- TRUE
    boundary_status <- proj$boundary_status
    well_defined <- proj$well_defined
  }

  # bootstrap-armed cells are the field-N grid; analytic-only cells never
  # bootstrap (sec. 3.4, sec. 4).
  cell$bootstrap <- cell$stage != "1" && cell$N %in% N_FIELD
  cell$P0 <- P0
  cell$truth <- list(theta = theta_truth, zeta = zeta_truth, beta = beta_truth)
  cell$Fstar <- proj$Fstar
  cell$rmsea_pop <- proj$rmsea_pop
  cell$boundary_status <- boundary_status
  cell$proj_multimodal <- proj$multimodal
  cell$proj_polished <- proj$polished
  cell$proj_canon_warn <- isTRUE(proj$canon_warn)
  cell$proj_accepted <- proj$accepted            # recorded (boundary projections
  cell$proj_heywood <- proj$heywood              # can converge yet be unaccepted)
  cell$basin_sep <- proj$basin_sep
  cell$well_defined <- well_defined && sane
  # projection-source cells carry the amended (ii)-(iv) guard verdict. Generating
  # cells take the truth from gamma0, so the (iii) STATISTICAL-scale unimodality
  # check does not apply -- but a NUMERICAL-scale multimodal flag at the F~0
  # population optimum means gamma0 is not identified (a distinct non-mirror
  # gamma' reproduces P0 to ~1e-6), which IS a reason to drop even a correct-spec
  # cell (Fable: the b0_dominant x perturbed x zeta=.5 drops are legitimate).
  cell$drop_reason <- if (full_guard) {
    if (!is.na(proj$guard_reason)) proj$guard_reason else NA_character_
  } else if (!proj$converged) {
    "projection did not converge (sanity gate)"
  } else if (proj$multimodal) {
    "population fit non-identified (near-tied distinct optima at F~0)"
  } else if (!sane) {
    "projection did not recover generating truth"
  } else NA_character_
  cell
}

# ---------------------------------------------------------------------------
# Stage cell builders (sec. 3.4)
# ---------------------------------------------------------------------------
# Stage 1 -- analytic screening, full core factorial, variant A, correct m.
# No bootstrap. Yields RQ4/RQ5, analytic RQ1/RQ3, the Wald peak curve.
build_stage1 <- function() {
  cells <- list()
  add <- function(...) cells[[length(cells) + 1L]] <<- new_cell(...)

  # core: angle sets x beta configs x zeta levels, variant A, m0. Field N for
  # every config; the large-N analytic extension runs only on a config SUBSET
  # (sec. 3.4 "extension on a config subset"; hygiene) to avoid a quiet blow-up.
  core_beta <- setdiff(names(BETA_CONFIGS), "m2_truth")
  ext_beta <- c("interior", "trail_t005", "b0_dominant")
  for (aset in names(ANGLE_SETS)) {
    p <- length(ANGLE_SETS[[aset]])
    for (bname in core_beta) {
      beta0 <- BETA_CONFIGS[[bname]]; m0 <- length(beta0) - 1L
      for (zlev in c(0.5, 0.75, 0.9)) {
        in_ext <- aset %in% c("p8_equal", "p8_perturbed") &&
          bname %in% ext_beta && zlev == 0.75
        Ns <- if (in_ext) N_ALL else N_FIELD
        for (N in Ns) {
          add(id = sprintf("s1_%s_%s_z%02.0f_N%d", aset, bname, zlev * 100, N),
              stage = "1", angle_set = aset, zeta0 = zeta_homo(p, zlev),
              beta0 = beta0, m0 = m0, variant_fit = "A",
              fit_angles_deg = ANGLE_SETS[[aset]], m_fit = m0,
              spec_note = "correct", truth_source = "generating",
              N = N, arm = "core")
        }
      }
    }
  }
  # zeta heterogeneity at zeta-bar = .75 (octants + interior beta only, N_ALL).
  for (het in c("alt", "weak")) {
    p <- 8L; zeta0 <- if (het == "alt") zeta_alt(p) else zeta_weak(p)
    for (N in N_ALL) {
      add(id = sprintf("s1_p8_equal_interior_%s_N%d", het, N),
          stage = "1", angle_set = "p8_equal", zeta0 = zeta0,
          beta0 = BETA_CONFIGS$interior, m0 = 3L, variant_fit = "A",
          fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
          spec_note = "correct", truth_source = "generating", N = N,
          arm = "het")
    }
  }
  # provocation (sec. 3.1): zeta = .97 homogeneous x interior x octants x N_ALL.
  for (N in N_ALL) {
    add(id = sprintf("s1_provocation_N%d", N), stage = "1",
        angle_set = "p8_equal", zeta0 = zeta_homo(8L, 0.97),
        beta0 = BETA_CONFIGS$interior, m0 = 3L, variant_fit = "A",
        fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
        spec_note = "correct", truth_source = "generating", N = N,
        arm = "provocation")
  }
  # misspecification arms, analytic screen (variant A free-angle unless B):
  #  (a) overfit: true m0 = 2, fitted m = 3 -- manufactures a true boundary
  #      (RQ6(a); m = 4 at p = 8 exceeds the variant-A identification cap).
  #  (b) underfit: true m0 = 3 interior, fitted m = 2 (projection estimand).
  #  (c) wrong-fixed B: generate perturbed, fit B at theory octants.
  for (N in N_ALL) {
    add(id = sprintf("s1_overfit_N%d", N), stage = "1", angle_set = "p8_equal",
        zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$m2_truth, m0 = 2L,
        variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
        spec_note = "overfit", truth_source = "generating", N = N,
        arm = "misspec")
    # underfit generating config = trail_t010 (NOT interior): the interior
    # underfit projection was measured ill-defined (symmetry-broken cyclic orbit
    # + a second basin at ~1 deviance unit); trail_t010 -> m2 is one clean,
    # symmetric, interior basin (Fable ratification N3).
    add(id = sprintf("s1_underfit_N%d", N), stage = "1", angle_set = "p8_equal",
        zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$trail_t010, m0 = 3L,
        variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 2L,
        spec_note = "underfit", truth_source = "projection", N = N,
        arm = "misspec")
    add(id = sprintf("s1_wrongfix_N%d", N), stage = "1",
        angle_set = "p8_perturbed", zeta0 = zeta_homo(8L, 0.75),
        beta0 = BETA_CONFIGS$interior, m0 = 3L, variant_fit = "B",
        fit_angles_deg = ANGLE_SETS$p8_equal,   # theory octants (wrong)
        m_fit = 3L, spec_note = "wrong-fixed", truth_source = "projection",
        N = N, arm = "misspec")
  }
  # correctly-fixed B control (octants generated, B fixed at octants).
  for (N in N_ALL) {
    add(id = sprintf("s1_fixB_N%d", N), stage = "1", angle_set = "p8_equal",
        zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$interior, m0 = 3L,
        variant_fit = "B", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
        spec_note = "correct-fixed", truth_source = "generating", N = N,
        arm = "variantB")
  }
  cells
}

# Stage 2 -- bootstrap-family core (sec. 3.4): all 6 core beta configs x
# zeta .75 homogeneous x {octants, perturbed} x 5 field N = 60 cells, plus one
# B-sensitivity marker cell (handled by run.R via boots override). The +<=12
# selection-rule admissions are resolved by run.R from stage-1 output.
build_stage2_core <- function() {
  cells <- list()
  add <- function(...) cells[[length(cells) + 1L]] <<- new_cell(...)
  core_beta <- setdiff(names(BETA_CONFIGS), "m2_truth")   # 6 configs
  for (bname in core_beta) {
    beta0 <- BETA_CONFIGS[[bname]]; m0 <- length(beta0) - 1L
    for (aset in c("p8_equal", "p8_perturbed")) {
      for (N in N_FIELD) {
        add(id = sprintf("s2_%s_%s_N%d", aset, bname, N), stage = "2",
            angle_set = aset, zeta0 = zeta_homo(8L, 0.75), beta0 = beta0,
            m0 = m0, variant_fit = "A", fit_angles_deg = ANGLE_SETS[[aset]],
            m_fit = m0, spec_note = "correct", truth_source = "generating",
            N = N, arm = "core")
      }
    }
  }
  # B-sensitivity cell (sec. 3.4): trailing-t=.05 x octants x N=500 at B=2000
  # (the shipped default) via the boots2000 flag (review M6).
  cells[[length(cells) + 1L]] <- new_cell(
    id = "s2_bsens_trail_t005_N500", stage = "2", angle_set = "p8_equal",
    zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$trail_t005, m0 = 3L,
    variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
    spec_note = "correct", truth_source = "generating", N = 500L, arm = "bsens",
    flags = list(boots2000 = TRUE))
  cells
}

# Stage 3 -- targeted bootstrap arms (sec. 3.4 c-g); the selection-driven arms
# (a studentized, b BCa validation) are resolved by run.R. Here: het/level
# slices, misspec bootstrap slice, out-of-family, provocation-multimodal, and
# the large-N bootstrap extension.
build_stage3_fixed <- function() {
  cells <- list()
  add <- function(...) cells[[length(cells) + 1L]] <<- new_cell(...)
  slice_N <- c(250, 500, 1000)
  # (c) zeta heterogeneity + level bootstrap slices (ladder t=.05 + interior).
  bslice <- list(trail_t005 = BETA_CONFIGS$trail_t005,
                 interior = BETA_CONFIGS$interior)
  for (bn in names(bslice)) {
    for (het in c("alt", "weak")) {
      zeta0 <- if (het == "alt") zeta_alt(8L) else zeta_weak(8L)
      for (N in slice_N) {
        add(id = sprintf("s3c_%s_%s_N%d", bn, het, N), stage = "3",
            angle_set = "p8_equal", zeta0 = zeta0, beta0 = bslice[[bn]],
            m0 = 3L, variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal,
            m_fit = 3L, spec_note = "correct", truth_source = "generating",
            N = N, arm = "het")
      }
    }
    for (zlev in c(0.5, 0.9)) {
      for (N in slice_N) {
        add(id = sprintf("s3c_%s_z%02.0f_N%d", bn, zlev * 100, N), stage = "3",
            angle_set = "p8_equal", zeta0 = zeta_homo(8L, zlev),
            beta0 = bslice[[bn]], m0 = 3L, variant_fit = "A",
            fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
            spec_note = "correct", truth_source = "generating", N = N,
            arm = "zlevel")
      }
    }
  }
  # (d) misspecification bootstrap slice: 2 configs x 3 N x 3 specs = 18 cells
  # (sec. 3.4d; review S10). overfit varies the m0 = 2 generating truth; under/
  # wrong-fixed vary the m0 = 3 config (interior / b0_dominant).
  m2_pair <- list(a = BETA_CONFIGS$m2_truth, b = c(0.60, 0.25, 0.15))
  # underfit pair uses trail_t010 (the interior underfit projection is ill-defined
  # -- Fable ratification N3); wrong-fixed pair keeps interior + b0_dominant.
  underfit_pair <- list(trail_t010 = BETA_CONFIGS$trail_t010,
                        b0_dominant = BETA_CONFIGS$b0_dominant)
  wrongfix_pair <- list(interior = BETA_CONFIGS$interior,
                        b0_dominant = BETA_CONFIGS$b0_dominant)
  for (N in slice_N) {
    for (cn in names(m2_pair)) {
      add(id = sprintf("s3d_overfit_%s_N%d", cn, N), stage = "3",
          angle_set = "p8_equal", zeta0 = zeta_homo(8L, 0.75),
          beta0 = m2_pair[[cn]], m0 = 2L, variant_fit = "A",
          fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
          spec_note = "overfit", truth_source = "generating", N = N,
          arm = "misspec")
    }
    for (cn in names(underfit_pair)) {
      add(id = sprintf("s3d_underfit_%s_N%d", cn, N), stage = "3",
          angle_set = "p8_equal", zeta0 = zeta_homo(8L, 0.75),
          beta0 = underfit_pair[[cn]], m0 = 3L, variant_fit = "A",
          fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 2L,
          spec_note = "underfit", truth_source = "projection", N = N,
          arm = "misspec")
    }
    for (cn in names(wrongfix_pair)) {
      add(id = sprintf("s3d_wrongfix_%s_N%d", cn, N), stage = "3",
          angle_set = "p8_perturbed", zeta0 = zeta_homo(8L, 0.75),
          beta0 = wrongfix_pair[[cn]], m0 = 3L, variant_fit = "B",
          fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
          spec_note = "wrong-fixed", truth_source = "projection", N = N,
          arm = "misspec")
    }
  }
  # (a) BCa acceleration validation: full-vs-grouped jackknife on two cells
  # (one small-N, one N=1000). jack_validate makes the kernel also compute the
  # full delete-1 acceleration for the gate (sec. 4.3; review M6).
  for (N in c(250L, 1000L)) {
    cells[[length(cells) + 1L]] <- new_cell(
      id = sprintf("s3a_jackval_N%d", N), stage = "3", angle_set = "p8_equal",
      zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$trail_t005, m0 = 3L,
      variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
      spec_note = "correct", truth_source = "generating", N = N, arm = "jackval",
      flags = list(jack_validate = TRUE))
  }
  # (f) provocation-multimodal bootstrap cells, reps sized for >= 400 expected
  # `multimodal` firings (sec. 3.4f / 6.1): G measured ~5% firing at N <= 5000,
  # so ceil(400/.05) = 8000 reps (capped at the seed ceiling; review S10).
  firing_rate_guess <- 0.05
  target_firings <- 400L
  reps_f <- min(as.integer(ceiling(target_firings / firing_rate_guess)), SEED_MAX_I)
  for (N in c(2000L, 5000L)) {
    cells[[length(cells) + 1L]] <- new_cell(
      id = sprintf("s3f_provocation_N%d", N), stage = "3", angle_set = "p8_equal",
      zeta0 = zeta_homo(8L, 0.97), beta0 = BETA_CONFIGS$interior, m0 = 3L,
      variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
      spec_note = "correct", truth_source = "generating", N = N,
      arm = "provocation", flags = list(reps = reps_f))
  }
  # (g) large-N bootstrap extension: trailing-t=.05 x octants x {5000, 10000}.
  # N is in the analytic-only grid, so mark it bootstrap-forced via a flag that
  # resolve_cell honors below.
  for (N in c(5000, 10000)) {
    cell <- new_cell(
      id = sprintf("s3g_trail_t005_N%d", N), stage = "3", angle_set = "p8_equal",
      zeta0 = zeta_homo(8L, 0.75), beta0 = BETA_CONFIGS$trail_t005, m0 = 3L,
      variant_fit = "A", fit_angles_deg = ANGLE_SETS$p8_equal, m_fit = 3L,
      spec_note = "correct", truth_source = "generating", N = N,
      arm = "largeN", flags = list(force_bootstrap = TRUE))
    cells[[length(cells) + 1L]] <- cell
  }
  cells
}

# (e) out-of-family arm (sec. 3.3): a few Gaussian cells with population
# RMSEA ~= .05. Built separately because each needs a solved perturbation.
build_stage3_oof <- function(target_rmsea = 0.05) {
  cells <- list()
  base_beta <- BETA_CONFIGS$interior; m0 <- 3L
  for (aset in c("p8_equal", "p8_perturbed")) {
    for (N in c(500, 2000)) {
      oof <- make_population_oof(ANGLE_SETS[[aset]], zeta_homo(8L, 0.75),
                                 base_beta, ANGLE_SETS[[aset]], "A", m0,
                                 target_rmsea = target_rmsea)
      cell <- new_cell(
        id = sprintf("s3e_oof_%s_N%d", aset, N), stage = "3", angle_set = aset,
        zeta0 = zeta_homo(8L, 0.75), beta0 = base_beta, m0 = m0,
        variant_fit = "A", fit_angles_deg = ANGLE_SETS[[aset]], m_fit = m0,
        spec_note = "out-of-family", truth_source = "projection", N = N,
        arm = "oof", flags = list(P0 = oof$P0, oof_scale = oof$s))
      cells[[length(cells) + 1L]] <- cell
    }
  }
  cells
}

# ---------------------------------------------------------------------------
# Assemble + resolve
# ---------------------------------------------------------------------------
# Returns list(cells = <resolved, kept>, dropped = <data.frame reasons>,
# n_by_stage). Honors force_bootstrap. Cells whose pseudo-truth is ill-defined
# are DROPPED here at design time with the reason recorded (sec. 2.4), never run.
build_config_table <- function(include_oof = TRUE, verbose = TRUE,
                              cache_file = NULL, target_rmsea = 0.05) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    if (verbose) cat("config table: loaded from cache", cache_file, "\n")
    return(readRDS(cache_file))
  }
  raw <- c(build_stage1(), build_stage2_core(), build_stage3_fixed(),
           if (include_oof) build_stage3_oof(target_rmsea) else list())
  # M1 range guard: every per-replicate seed BASE_SEED + SEED_MULT*cell_index + i
  # must stay a valid 32-bit integer for set.seed().
  stopifnot(BASE_SEED + SEED_MULT * length(raw) + SEED_MAX_I < .Machine$integer.max)
  # N_max per cell family (same population + fitted model) drives the sec. 2.4-iii
  # unimodality threshold (N2): a basin distinct at the family's largest N is
  # kept; near-tied even there is dropped.
  proj_key <- function(c) paste(if (!is.null(c$flags$P0)) c$id else c$angle_set,
    paste(c$zeta0, collapse = ","), paste(c$beta0, collapse = ","),
    c$variant_fit, c$m_fit, paste(c$fit_angles, collapse = ","), sep = "|")
  keys <- vapply(raw, proj_key, "")
  nmax_by_key <- tapply(vapply(raw, `[[`, 0, "N"), keys, max)
  pop_cache <- new.env(parent = emptyenv())
  kept <- list(); dropped <- list()
  for (cell in raw) {
    rc <- resolve_cell(cell, pop_cache, n_max = nmax_by_key[[proj_key(cell)]])
    if (isTRUE(rc$flags$force_bootstrap)) rc$bootstrap <- TRUE
    # OOF "computed, not assumed" (sec. 3.3) deserves an assertion, not just a
    # recorded column (review S1).
    if (identical(rc$arm, "oof") && is.na(rc$drop_reason))
      stopifnot(abs(rc$rmsea_pop - target_rmsea) < 5e-3)
    if (is.na(rc$drop_reason)) {
      kept[[length(kept) + 1L]] <- rc
    } else {
      dropped[[length(dropped) + 1L]] <-
        data.frame(id = rc$id, stage = rc$stage, N = rc$N,
                   reason = rc$drop_reason, stringsAsFactors = FALSE)
    }
  }
  dropped_df <- if (length(dropped)) do.call(rbind, dropped) else
    data.frame(id = character(), stage = character(), N = integer(),
               reason = character())
  n_by_stage <- table(vapply(kept, `[[`, "", "stage"))
  if (verbose) {
    cat(sprintf("config table: %d cells kept, %d dropped\n",
                length(kept), nrow(dropped_df)))
    print(n_by_stage)
    if (nrow(dropped_df)) { cat("dropped (ill-defined estimand):\n"); print(dropped_df) }
  }
  out <- list(cells = kept, dropped = dropped_df, n_by_stage = n_by_stage,
              base_seed = BASE_SEED, date = Sys.Date())
  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_file)                    # hygiene: cache the resolved table
  }
  out
}
