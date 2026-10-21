# M167: interval-method helpers shared by devel/m167-interval-oracle.R and
# devel/m167-interval-checks.R. Prototypes only: nothing here is exported or
# called by the package. Run under devtools::load_all() so the CPM internals
# (cpm_optimize_one, cpm_gradient, cpm_mirror_guard, cpm_unpack, ...) resolve.
#
# The method definitions these functions implement are fixed in the
# pre-registration header of devel/m167-interval-oracle.R; read that first.

# ---- the shipped percentile bootstrap, returning its replicates ----------------

# A copy of cpm_bootstrap() (R/cpm_fit.R) that also returns the replicate
# matrices, so F3 can be formed on the B1 replicates. The RNG use, the
# exclusion rules, the restart and the mirror guard are the shipped ones; the
# checks script asserts that its intervals equal cpm_fit()'s exactly.
m167_boot_reps <- function(engine, sdata, boots) {
  spec <- engine$spec
  par_hat <- engine$par
  N <- nrow(sdata)
  idx <- matrix(sample.int(N, N * boots, replace = TRUE), nrow = boots)
  m167_refit_reps(engine, boots, function(b) {
    suppressWarnings(stats::cor(sdata[idx[b, ], , drop = FALSE]))
  })
}

# F2's replicates: correlation matrices drawn from the fitted Phat at the fit's
# N. The sample correlation of N multivariate-normal rows is cov2cor() of a
# Wishart(N - 1, Phat) draw divided by N - 1, so one rWishart() call per
# replicate gives its exact distribution without drawing N rows.
m167_param_reps <- function(engine, N, boots) {
  P <- engine$P
  W <- stats::rWishart(boots, df = N - 1, Sigma = P)
  m167_refit_reps(engine, boots, function(b) stats::cov2cor(W[, , b]))
}

# The shipped warm-started refit loop (cpm_bootstrap()'s body), fed one
# correlation matrix per replicate by `draw_R(b)`.
m167_refit_reps <- function(engine, boots, draw_R) {
  spec <- engine$spec
  par_hat <- engine$par
  p <- spec$p
  ref_rel_hat <- cpm_ref_relative(par_hat, spec)
  theta_reps <- matrix(NA_real_, boots, p)
  zeta_reps <- matrix(NA_real_, boots, p)
  beta_reps <- matrix(NA_real_, boots, spec$m + 1L)
  n_degenerate <- 0L
  n_nonconvergent <- 0L
  for (b in seq_len(boots)) {
    Rb <- draw_R(b)
    if (anyNA(Rb) ||
        min(eigen(Rb, symmetric = TRUE, only.values = TRUE)$values) <= 1e-10) {
      n_degenerate <- n_degenerate + 1L
      next
    }
    Rb <- (Rb + t(Rb)) / 2
    run <- cpm_optimize_one(par_hat, Rb, spec)
    gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
    if (gnorm > 1e-6 * max(1, abs(run$F))) {
      run <- cpm_optimize_one(run$par, Rb, spec)
      gnorm <- max(abs(cpm_gradient(run$par, Rb, spec)))
      if (gnorm > 1e-6 * max(1, abs(run$F))) {
        n_nonconvergent <- n_nonconvergent + 1L
        next
      }
    }
    guard <- cpm_mirror_guard(run$par, spec, ref_rel_hat)
    nat <- cpm_unpack(guard$par, spec)
    theta_reps[b, ] <- nat$theta %% (2 * pi)
    zeta_reps[b, ] <- nat$zeta
    beta_reps[b, ] <- nat$beta
  }
  ok <- stats::complete.cases(theta_reps, zeta_reps, beta_reps)
  list(theta = theta_reps[ok, , drop = FALSE],
       zeta = zeta_reps[ok, , drop = FALSE],
       beta = beta_reps[ok, , drop = FALSE],
       n_ok = sum(ok), n_degenerate = n_degenerate,
       n_nonconvergent = n_nonconvergent)
}

# ---- interval forms ------------------------------------------------------------

# Percentile intervals exactly as cpm_bootstrap() forms them: circular quantile
# for angles (degrees, wrapped, lci > uci when the arc straddles 0/360), plain
# quantile for zeta and beta.
m167_percentile <- function(reps, level = 0.95) {
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  p <- ncol(reps$theta)
  out <- m167_empty_ci(p, ncol(reps$beta))
  if (reps$n_ok == 0) return(out)
  for (i in seq_len(p)) {
    q <- quantile.circumplex_radian(new_radian(reps$theta[, i]), probs = probs)
    q <- as.numeric(as_degree(q))
    out$angle[i, ] <- q
    out$zeta[i, ] <- stats::quantile(reps$zeta[, i], probs = probs,
                                     names = FALSE)
  }
  for (k in seq_len(ncol(reps$beta))) {
    out$beta[k, ] <- stats::quantile(reps$beta[, k], probs = probs,
                                     names = FALSE)
  }
  out
}

# Bias-corrected percentile interval (Efron 1981; Efron & Tibshirani 1993,
# sec. 14.3 with acceleration a = 0) for one linear parameter:
#   z0 = qnorm(share of replicates below the estimate, ties counted half),
#   interval = type-7 quantiles at pnorm(2 z0 + qnorm(alpha/2)) and
#   pnorm(2 z0 + qnorm(1 - alpha/2)).
# The share is clamped to [1/(2B), 1 - 1/(2B)] so z0 stays finite when every
# replicate falls on one side. Angles get no BC form (circular parameter).
m167_bc_one <- function(reps, est, level = 0.95) {
  B <- length(reps)
  if (B == 0) return(c(NA_real_, NA_real_))
  share <- (sum(reps < est) + 0.5 * sum(reps == est)) / B
  share <- min(max(share, 1 / (2 * B)), 1 - 1 / (2 * B))
  z0 <- stats::qnorm(share)
  za <- stats::qnorm(c((1 - level) / 2, 1 - (1 - level) / 2))
  stats::quantile(reps, probs = stats::pnorm(2 * z0 + za), names = FALSE)
}

m167_bc <- function(reps, engine, level = 0.95) {
  p <- ncol(reps$zeta)
  out <- m167_empty_ci(p, ncol(reps$beta))
  out$angle[] <- NA_real_                      # not formed for angles
  if (reps$n_ok == 0) return(out)
  for (i in seq_len(p)) {
    out$zeta[i, ] <- m167_bc_one(reps$zeta[, i], engine$zeta[i], level)
  }
  for (k in seq_len(ncol(reps$beta))) {
    out$beta[k, ] <- m167_bc_one(reps$beta[, k], engine$beta[k], level)
  }
  out
}

# B2: the shipped Wald interval (cpm_fit()'s analytic branch).
m167_wald <- function(engine, se, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- length(engine$zeta)
  out <- m167_empty_ci(p, length(engine$beta))
  out$angle <- cbind(engine$theta - z * se$angle, engine$theta + z * se$angle)
  out$zeta <- cbind(engine$zeta - z * se$zeta, engine$zeta + z * se$zeta)
  out$beta <- cbind(engine$beta - z * se$beta, engine$beta + z * se$beta)
  out
}

# F1, the zeta arm: browne1982 eqs. 1.6.30-1.6.32 (p. 95) with h = ln v,
# v = (1 - zeta^2) / zeta^2 (CircE's interval; test-cpm_oracles.R:136).
# SE(ln v) = 2 SE(zeta) / (zeta (1 - zeta^2)) by the delta method. ln v falls
# as zeta rises, so the back-transformed endpoints swap.
m167_lnv_ci <- function(zeta, se_zeta, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  lv <- log((1 - zeta^2) / zeta^2)
  se_lv <- 2 * se_zeta / (zeta * (1 - zeta^2))
  zeta_of <- function(l) 1 / sqrt(1 + exp(l))
  cbind(zeta_of(lv + z * se_lv), zeta_of(lv - z * se_lv))
}

# F1, the beta arm: the same method with h = logit for each beta_k in (0, 1),
# SE(logit beta) = SE(beta) / (beta (1 - beta)). A removed harmonic
# (beta-hat = 0, SE 0) gets the degenerate interval [0, 0], as B1 and B2 do.
m167_logit_ci <- function(b, se_b, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- cbind(b, b)
  live <- b > 0 & b < 1
  lb <- stats::qlogis(b[live])
  se_l <- se_b[live] / (b[live] * (1 - b[live]))
  out[live, ] <- cbind(stats::plogis(lb - z * se_l), stats::plogis(lb + z * se_l))
  out
}

# F1: angles stay on the identity scale (an unbounded parameter needs no
# transform), so F1's angle interval equals B2's.
m167_transformed_wald <- function(engine, se, level = 0.95) {
  out <- m167_wald(engine, se, level)
  out$zeta <- m167_lnv_ci(engine$zeta, se$zeta, level)
  out$beta <- m167_logit_ci(engine$beta, se$beta, level)
  out
}

m167_empty_ci <- function(p, nb) {
  list(angle = matrix(NA_real_, p, 2), zeta = matrix(NA_real_, p, 2),
       beta = matrix(NA_real_, nb, 2))
}

# ---- scoring -------------------------------------------------------------------

# One interval per component -> side of the truth: 0 covered, -1 the interval
# lies below the truth, +1 above, NA no interval. For angles "above" means the
# interval lies counterclockwise of the truth: the signed shortest rotation
# from the truth to the arc's midpoint is positive. Angle arcs run
# counterclockwise from lci to uci, which also reads a Wald interval correctly
# (its endpoints are unwrapped around the estimate).
m167_side_linear <- function(ci, truth) {
  out <- ifelse(ci[, 1] <= truth & truth <= ci[, 2], 0,
                ifelse(ci[, 1] > truth, 1, -1))
  out[is.na(ci[, 1]) | is.na(ci[, 2])] <- NA
  out
}

m167_side_angle <- function(ci, truth) {
  lci <- ci[, 1] %% 360
  span <- (ci[, 2] - ci[, 1]) %% 360
  # a Wald interval wider than a full turn covers every direction
  full <- !is.na(ci[, 1]) & (ci[, 2] - ci[, 1]) >= 360
  inside <- ((truth - lci) %% 360) <= span
  mid <- lci + span / 2
  rot <- ((mid - truth + 180) %% 360) - 180
  out <- ifelse(inside | full, 0, ifelse(rot > 0, 1, -1))
  out[is.na(ci[, 1]) | is.na(ci[, 2])] <- NA
  out
}
