# Monte Carlo alternative to bootstrapping for SSM confidence intervals -------
# Draws SSM parameter replicates from the asymptotic sampling distribution of
# the group mean vector (mean-based path) or the measure-scale correlation
# vector (correlation-based path), then reuses the same interval-assembly back
# end as the bootstrap (ssm_replicate_intervals). The asymptotic covariance is
# estimated empirically -- sample covariance of the observations for means;
# influence-function covariance for correlations -- so no normality of the raw
# data is assumed, only asymptotic normality of the estimators themselves
# (which is also what makes the method comparable to the nonparametric
# bootstrap). Correlations are drawn on the Fisher z scale and back-
# transformed, which respects the (-1, 1) range and improves the normal
# approximation. Within a group, all measures' correlation vectors are drawn
# jointly (they share the sample and are dependent -- essential for measure
# contrasts); distinct groups are independent.
ssm_montecarlo <- function(bs_input, scales, measures = NULL, angles,
                           boots, interval, contrast, listwise, obs_scores) {

  if (anyNA(bs_input)) {
    stop(
      "The Monte Carlo method requires complete data to estimate the ",
      "asymptotic covariance. Use listwise = TRUE (the default) or switch ",
      "to method = \"bootstrap\", which supports pairwise deletion.",
      call. = FALSE
    )
  }

  grp <- as.integer(bs_input[[ncol(bs_input)]])
  group_ids <- sort(unique(grp))
  if (min(tabulate(grp)) < 2) {
    stop(
      "The Monte Carlo method requires at least two observations per group ",
      "to estimate the asymptotic covariance.",
      call. = FALSE
    )
  }

  cs_all <- as.matrix(bs_input[scales])
  p <- length(scales)
  if (!is.null(measures)) mv_all <- as.matrix(bs_input[measures])

  # Observed scores and point-estimate parameter vector; matches the bootstrap's
  # t0 (boot::boot evaluates the statistic on the full sample). obs_scores is the
  # caller's already-computed group mean/correlation score matrix (same inputs),
  # reused here to avoid a second mean_scores()/corr_scores() pass. It also
  # supplies the observed correlations used by the draw loop below.
  scores <- obs_scores
  t0 <- ssm_by_group(scores, angles, contrast)

  # Generate score draws, one R x p matrix per profile row -------------------
  draw_list <- list()
  for (g in seq_along(group_ids)) {
    rows_g <- grp == group_ids[g]
    n_g <- sum(rows_g)
    cs_g <- cs_all[rows_g, , drop = FALSE]
    if (is.null(measures)) {
      # Sampling distribution of the group mean vector (CLT): the sample
      # covariance of the observations scaled by 1/n
      draw_list[[length(draw_list) + 1]] <-
        mvn_draws(boots, colMeans(cs_g), stats::cov(cs_g) / n_g)
    } else {
      q <- length(measures)
      mv_g <- mv_all[rows_g, , drop = FALSE]
      # This group's observed correlations (rows = measures, cols = scales)
      rmat <- scores[((g - 1) * q + 1):(g * q), , drop = FALSE]
      if (any(!is.finite(rmat)) || any(abs(rmat) >= 1 - 1e-12)) {
        stop(
          "One or more scale-measure correlations are undefined or equal to ",
          "+/-1 (e.g., a zero-variance variable); the Monte Carlo asymptotic ",
          "distribution is undefined. Use method = \"bootstrap\".",
          call. = FALSE
        )
      }
      # Empirical influence-function covariance of the stacked correlation
      # vector (measure-major blocks of p scales). For Pearson r the influence
      # value of observation i is z_x * z_y - (r / 2) * (z_x^2 + z_y^2)
      # (Hampel), whose sample mean is exactly zero at the estimate; for
      # multivariate normal data its covariance reduces to the classic
      # Pearson-Filon expressions, but the empirical version stays valid for
      # non-normal data (like the bootstrap it is compared against).
      zc <- scale(cs_g)
      zm <- scale(mv_g)
      psi <- matrix(NA_real_, n_g, q * p)
      for (m in seq_len(q)) {
        for (j in seq_len(p)) {
          psi[, (m - 1) * p + j] <- zm[, m] * zc[, j] -
            (rmat[m, j] / 2) * (zm[, m]^2 + zc[, j]^2)
        }
      }
      acov_r <- crossprod(psi) / n_g^2
      # Draw on the Fisher z scale (delta-method covariance), back-transform
      r_vec <- as.vector(t(rmat))
      dz <- 1 / (1 - r_vec^2)
      acov_z <- acov_r * tcrossprod(dz)
      r_draws <- tanh(mvn_draws(boots, atanh(r_vec), acov_z))
      for (m in seq_len(q)) {
        draw_list[[length(draw_list) + 1]] <-
          r_draws[, ((m - 1) * p + 1):(m * p), drop = FALSE]
      }
    }
  }

  # Propagate each profile row's draws through the SSM transformation --------
  n_par <- length(ssm_param_names())
  par_list <- lapply(draw_list, function(draws) {
    matrix(group_parameters(draws, angles), ncol = n_par, byrow = TRUE)
  })

  t <- do.call(cbind, par_list)
  if (contrast) {
    # Second profile row minus first (displacement via angular distance),
    # sharing param_diff() with the bootstrap path so the contrast convention
    # has one definition. Contrasts require exactly two profile rows (validated
    # in ssm_analyze).
    t <- cbind(t, param_diff(par_list[[2]], par_list[[1]]))
  }

  ssm_replicate_intervals(
    t0 = t0,
    t = t,
    interval = interval,
    contrast = contrast,
    replicate_label = "Monte Carlo draws"
  )
}

# Draw R samples from a multivariate normal via the symmetric eigendecomposition
# square root, which tolerates positive-semidefinite covariances (e.g.,
# ipsatized scales are sum-constrained, making the covariance singular) where a
# Cholesky factor would fail. Negative eigenvalues from floating-point noise
# are clamped to zero.
mvn_draws <- function(R, mu, sigma) {
  p <- length(mu)
  eig <- eigen(sigma, symmetric = TRUE)
  root <- eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
  z <- matrix(stats::rnorm(R * p), nrow = R, ncol = p)
  sweep(z %*% root, 2, mu, "+")
}
