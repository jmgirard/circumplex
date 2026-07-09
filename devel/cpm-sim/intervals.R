# CPM CI simulation study -- interval methods on a shared replicate matrix
# (plan sec. 4, sec. 10.2).
#
# ONE bootstrap pass per fitted dataset produces the raw replicate parameter
# matrix; percentile / basic / BCa / studentized are scoring passes over it, so
# competitor intervals are paired by construction (same data, same draws --
# sec. 6.2). The shipped cpm_bootstrap() only returns quantiles, so this module
# re-implements its resample loop VERBATIM (same idx-draw pattern, acceptance,
# deterministic restart, mirror guard) but RETURNS the replicates -- the
# percentile arm is therefore the shipped default method, not an approximation
# of it. No package code is changed (sec. 10).
#
# Circular theta is excluded from the order-statistic refinements (BCa, basic,
# studentized) by the recorded M2 geometry: bias-correction and acceleration
# re-index ordered replicates and a circle has no order (sec. 4.1). theta is
# compared across exactly two methods: circular percentile and analytic Wald.

# ---- shared replicate generator (mirrors cpm_bootstrap, sec. 5.2) -----------
# engine: a cpm_engine() result (reported post-polish par/spec).
# sdata:  complete-case numeric matrix (rows resampled).
# with_se: also compute a per-replicate analytic SE (the studentized arm's
#          input); NA/unstable in the boundary regime is itself an outcome
#          (sec. 4.4), so it is returned, not dropped here.
# Returns raw matrices (NA rows for excluded replicates) + the `ok` mask + the
# exclusion counts + the kept-harmonic map, so every downstream method reads the
# identical replicate set.
sim_replicates <- function(engine, sdata, boots, with_se = FALSE) {
  spec <- engine$spec
  par_hat <- engine$par
  N <- nrow(sdata)
  p <- spec$p
  ref_rel_hat <- cpm_ref_relative(par_hat, spec)

  idx <- matrix(sample.int(N, N * boots, replace = TRUE), nrow = boots)

  theta_reps <- matrix(NA_real_, boots, p)          # radians %% 2pi
  zeta_reps  <- matrix(NA_real_, boots, p)
  beta_reps  <- matrix(NA_real_, boots, spec$m + 1L) # 0 at removed harmonics
  se_zeta_reps <- if (with_se) matrix(NA_real_, boots, p) else NULL
  se_beta_reps <- if (with_se) matrix(NA_real_, boots, spec$m + 1L) else NULL
  reflected <- logical(boots)
  n_degenerate <- 0L; n_nonconvergent <- 0L

  for (b in seq_len(boots)) {
    Rb <- suppressWarnings(stats::cor(sdata[idx[b, ], , drop = FALSE]))
    if (anyNA(Rb) ||
        min(eigen(Rb, symmetric = TRUE, only.values = TRUE)$values) <= 1e-10) {
      n_degenerate <- n_degenerate + 1L
      next
    }
    Rb <- (Rb + t(Rb)) / 2
    run <- cpm_optimize_one(par_hat, Rb, spec)
    gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
    if (gnorm > 1e-6 * max(1, abs(run$F))) {         # one deterministic restart
      run <- cpm_optimize_one(run$par, Rb, spec)
      gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
      if (gnorm > 1e-6 * max(1, abs(run$F))) {
        n_nonconvergent <- n_nonconvergent + 1L
        next
      }
    }
    guard <- cpm_mirror_guard(run$par, spec, ref_rel_hat)
    reflected[b] <- guard$reflected
    nat <- cpm_unpack(guard$par, spec)
    theta_reps[b, ] <- nat$theta %% (2 * pi)
    zeta_reps[b, ]  <- nat$zeta
    beta_reps[b, ]  <- nat$beta
    if (with_se) {
      eng_b <- list(spec = spec, par = guard$par, zeta = nat$zeta,
                    beta = nat$beta)
      se <- tryCatch(suppressWarnings(cpm_analytic_se(eng_b, Rb, N)),
                     error = function(e) NULL)
      if (!is.null(se)) {
        se_zeta_reps[b, ] <- se$zeta
        se_beta_reps[b, ] <- se$beta
      }
    }
  }

  ok <- stats::complete.cases(theta_reps, zeta_reps, beta_reps)
  list(
    theta_rad = theta_reps, zeta = zeta_reps, beta = beta_reps,
    se_zeta = se_zeta_reps, se_beta = se_beta_reps,
    ok = ok, reflected = reflected,
    boots = boots, boots_used = sum(ok),
    boots_degenerate = n_degenerate, boots_nonconvergent = n_nonconvergent,
    keep_k = spec$keep_k, m = spec$m
  )
}

# BCa acceleration from jackknife pseudo-values: the PLAIN skewness formula
# (NO delete-d correction -- sec. 4.3). a is invariant to any common rescaling
# of the deviations (cubes over squares^1.5), so grouped delete-d pseudo-values
# give the full delete-1 value to first order. Point-mass -> 0.
bca_acceleration <- function(v) {
  v <- v[is.finite(v)]
  d <- mean(v) - v
  s2 <- sum(d^2)
  if (s2 <= 0) return(0)
  sum(d^3) / (6 * s2^1.5)
}

# ---- grouped (delete-d) jackknife for the BCa acceleration (sec. 4.3) --------
# g contiguous blocks of the seeded dataset (rows exchangeable). The statistic
# is refit warm-started on the data minus block i; the returned pseudo-values
# feed the PLAIN skewness formula (NO delete-d correction -- sec. 4.3). Failed
# refits (acceptance / degenerate) are excluded with a counted rate; < g_used
# survivors -> a is NA for that parameter.
# g_used_floor scales with g so SMOKE runs (g = 25/50) don't force every
# acceleration to NA and blank out BCa in the stage-0 gate (review S6).
grouped_jackknife <- function(engine, sdata, g = 100L,
                            g_used_floor = max(10L, min(50L, g %/% 2L))) {
  spec <- engine$spec
  par_hat <- engine$par
  N <- nrow(sdata)
  ref_rel_hat <- cpm_ref_relative(par_hat, spec)
  g <- min(g, N)
  # contiguous blocks (arbitrary composition -- rows are exchangeable, sec. 4.3)
  block <- ceiling(seq_len(N) / (N / g))
  zeta_ps <- matrix(NA_real_, g, spec$p)
  beta_ps <- matrix(NA_real_, g, spec$m + 1L)
  n_fail <- 0L
  for (i in seq_len(g)) {
    keep_rows <- block != i
    Rb <- suppressWarnings(stats::cor(sdata[keep_rows, , drop = FALSE]))
    if (anyNA(Rb) ||
        min(eigen(Rb, symmetric = TRUE, only.values = TRUE)$values) <= 1e-10) {
      n_fail <- n_fail + 1L; next
    }
    Rb <- (Rb + t(Rb)) / 2
    run <- cpm_optimize_one(par_hat, Rb, spec)
    gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
    if (gnorm > 1e-6 * max(1, abs(run$F))) {
      run <- cpm_optimize_one(run$par, Rb, spec)
      gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
      if (gnorm > 1e-6 * max(1, abs(run$F))) { n_fail <- n_fail + 1L; next }
    }
    guard <- cpm_mirror_guard(run$par, spec, ref_rel_hat)
    nat <- cpm_unpack(guard$par, spec)
    zeta_ps[i, ] <- nat$zeta
    beta_ps[i, ] <- nat$beta
  }
  # a per column from surviving pseudo-values (formula in bca_acceleration).
  accel <- function(ps_mat) {
    apply(ps_mat, 2, function(v) {
      v <- v[is.finite(v)]
      if (length(v) < g_used_floor) return(NA_real_)
      bca_acceleration(v)
    })
  }
  list(a_zeta = accel(zeta_ps), a_beta = accel(beta_ps),
       g = g, g_fail = n_fail, g_used = g - n_fail)
}

# ---- percentile intervals ---------------------------------------------------
# Linear (zeta/beta): plain quantiles over the used replicates, per column.
ci_percentile_linear <- function(reps, ok, level) {
  a <- (1 - level) / 2
  probs <- c(a, 1 - a)
  x <- reps[ok, , drop = FALSE]
  lci <- apply(x, 2, stats::quantile, probs = probs[1], names = FALSE)
  uci <- apply(x, 2, stats::quantile, probs = probs[2], names = FALSE)
  list(lci = lci, uci = uci)
}

# Circular theta: the package's own circular quantile machinery, so a CI
# straddling 0/360 is reported wrapped (lci > uci) -- reproduces the shipped
# default exactly. Input radians %% 2pi; output degrees.
ci_percentile_theta <- function(theta_rad, ok, level) {
  a <- (1 - level) / 2
  probs <- c(a, 1 - a)
  x <- theta_rad[ok, , drop = FALSE]
  p <- ncol(x)
  lci <- uci <- numeric(p)
  for (i in seq_len(p)) {
    q <- quantile.circumplex_radian(new_radian(x[, i]), probs = probs)
    q <- as.numeric(as_degree(q))
    lci[i] <- q[1]; uci[i] <- q[2]
  }
  list(lci = lci, uci = uci)
}

# ---- basic (reflected) interval (sec. 4.1) ----------------------------------
# 2*t_hat - percentile, RAW (untruncated) -- truncating can flip a boundary
# miss to a cover, so the truncation RATE is reported separately (sec. 5.2).
ci_basic_linear <- function(t_hat, reps, ok, level) {
  pct <- ci_percentile_linear(reps, ok, level)
  # basic lower uses the UPPER percentile and vice versa.
  list(lci = 2 * t_hat - pct$uci, uci = 2 * t_hat - pct$lci)
}

# ---- BCa (sec. 4.2) ---------------------------------------------------------
# Per linear parameter: t_hat, replicate vector t_star (used replicates), a from
# the grouped jackknife. Mid-rank z0 tie convention; endpoint clamping and z0
# saturation counted, not silently floored. Degenerate guard (B_used < 100 or
# point-mass kept parameter) -> NA with reason.
bca_one <- function(t_hat, t_star, a, level, b_used_floor = 100L) {
  # `saturated` distinguishes z0-saturation (all replicate mass one side of
  # t_hat) from other NA reasons so it stays a SEPARATELY measurable outcome
  # (sec. 4.2 / RQ3; review S2). It is TRUE only on the saturation path.
  na_ret <- function(reason, saturated = FALSE) list(lci = NA_real_,
    uci = NA_real_, z0 = NA_real_, a = a, saturated = saturated,
    clamped_lo = FALSE, clamped_hi = FALSE, na = TRUE, reason = reason)
  t_star <- t_star[is.finite(t_star)]
  B <- length(t_star)
  if (B < b_used_floor) return(na_ret("B_used < floor"))
  if (is.na(a)) return(na_ret("acceleration NA (g_used < floor)"))
  if (stats::sd(t_star) == 0) return(na_ret("point-mass replicate set"))
  z0 <- stats::qnorm((sum(t_star < t_hat) + 0.5 * sum(t_star == t_hat)) / B)
  saturated <- !is.finite(z0)               # all mass on one side of t_hat
  a2 <- (1 - level) / 2
  zl <- stats::qnorm(a2); zu <- stats::qnorm(1 - a2)
  adj <- function(zq) stats::pnorm(z0 + (z0 + zq) / (1 - a * (z0 + zq)))
  alo <- adj(zl); ahi <- adj(zu)
  if (!is.finite(alo) || !is.finite(ahi)) return(na_ret("z0 saturated", TRUE))
  # clamp adjusted probabilities; a clamp means the endpoint index hit the
  # extreme order statistic -- an informative failure, counted.
  clamped_lo <- alo <= 1 / B; clamped_hi <- ahi >= 1 - 1 / B
  alo <- min(max(alo, 1 / B), 1 - 1 / B)
  ahi <- min(max(ahi, 1 / B), 1 - 1 / B)
  list(lci = stats::quantile(t_star, alo, names = FALSE),
       uci = stats::quantile(t_star, ahi, names = FALSE),
       z0 = z0, a = a, saturated = saturated,
       clamped_lo = clamped_lo, clamped_hi = clamped_hi, na = FALSE,
       reason = NA_character_)
}

# Vectorized over columns of a replicate matrix `reps` (kept columns only).
ci_bca_linear <- function(t_hat_vec, reps, ok, a_vec, level) {
  x <- reps[ok, , drop = FALSE]
  lapply(seq_along(t_hat_vec), function(j)
    bca_one(t_hat_vec[j], x[, j], a_vec[j], level))
}

# ---- studentized / bootstrap-t (sec. 4.4) -----------------------------------
# z*_b = (t*_b - t_hat)/se*_b over replicates with finite se; interval
# [t_hat - q_{1-a/2} se_hat, t_hat - q_{a/2} se_hat], RAW. Feasibility (finite-se
# rate) is an outcome; a cell where the NA-se rate exceeds 20% reports the
# method infeasible rather than its coverage (handled by the summarizer).
studentized_one <- function(t_hat, se_hat, t_star, se_star, level) {
  keep <- is.finite(t_star) & is.finite(se_star) & se_star > 0
  n_keep <- sum(keep); n_tot <- sum(is.finite(t_star))
  na_rate <- if (n_tot > 0) 1 - n_keep / n_tot else NA_real_
  if (n_keep < 100 || !is.finite(se_hat) || se_hat <= 0) {
    return(list(lci = NA_real_, uci = NA_real_, na = TRUE, na_rate = na_rate))
  }
  z <- (t_star[keep] - t_hat) / se_star[keep]
  a2 <- (1 - level) / 2
  qz <- stats::quantile(z, c(a2, 1 - a2), names = FALSE)
  list(lci = t_hat - qz[2] * se_hat, uci = t_hat - qz[1] * se_hat,
       na = FALSE, na_rate = na_rate)
}

# ---- analytic Wald (shipped cormat default) ---------------------------------
# Linear parameters: est +/- z*SE. Returns NA endpoints where SE is NA (an
# outcome, sec. 5.2). theta: Wald on the unwrapped branch is scored directly by
# |signed error| <= z*SE in the kernel, so no wrapped endpoints are needed.
ci_wald_linear <- function(est, se, level) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  list(lci = est - z * se, uci = est + z * se)
}
