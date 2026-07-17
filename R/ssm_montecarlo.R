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
                           boots, interval, contrast, listwise, obs_scores,
                           occ_k = NULL) {

  if (anyNA(bs_input)) {
    stop(
      "The Monte Carlo method requires complete data to estimate the ",
      "asymptotic covariance. Use listwise = TRUE (the default) or switch ",
      "to method = \"bootstrap\", which supports pairwise deletion.",
      call. = FALSE
    )
  }

  grp <- as.integer(bs_input[[ncol(bs_input)]])
  if (min(tabulate(grp)) < 2) {
    stop(
      "The Monte Carlo method requires at least two observations per group ",
      "to estimate the asymptotic covariance.",
      call. = FALSE
    )
  }

  cs_all <- as.matrix(bs_input[scales])
  mv_all <- if (is.null(measures)) NULL else as.matrix(bs_input[measures])

  # Observed scores and point-estimate parameter vector; matches the bootstrap's
  # t0 (boot::boot evaluates the statistic on the full sample). obs_scores is the
  # caller's already-computed group mean/correlation score matrix (same inputs),
  # reused here to avoid a second mean_scores()/corr_scores() pass. It also
  # supplies the observed correlations used by the draw core below.
  t0 <- ssm_by_group(obs_scores, angles, contrast)
  t <- ssm_mc_replicates(cs_all, mv_all, grp, obs_scores, boots, angles,
                         contrast, occ_k = occ_k)

  ssm_replicate_intervals(
    t0 = t0,
    t = t,
    interval = interval,
    contrast = contrast,
    replicate_label = "Monte Carlo draws"
  )
}

# Generate the Monte Carlo SSM parameter replicate matrix ----------------------
# The draw core of ssm_montecarlo(), separated so the CI-accuracy diagnostic
# (ssm_ci_accuracy) replays exactly the same procedure on simulated data: one
# asymptotic draw block per group in sorted-group order, jointly across a
# group's measures, propagated through the closed-form SSM transformation.
# Returns the replicate matrix t (one row per draw; 6 columns per profile row
# in ssm_param_names() order, plus a contrast block when contrasting).
# `scores` is the observed group mean/correlation score matrix (one row per
# profile row); it is consumed only on the correlation path.
ssm_mc_replicates <- function(cs, mv = NULL, grp, scores, boots, angles,
                              contrast, occ_k = NULL) {
  group_ids <- sort(unique(grp))
  p <- ncol(cs)
  q <- if (is.null(mv)) 0L else ncol(mv)

  # Name the profile rows and the stacked correlation vector once, so every
  # block extraction below is name-driven rather than positional arithmetic
  # (the M2 results-assembly convention, extended to this path). Name lookup
  # trades positional robustness for readability, so ambiguity must be an
  # error, never a silent first-match: keys are required to be unique (they
  # could collide if a variable name contained the separator, or if a caller
  # passed duplicate column names).
  if (!is.null(mv)) {
    if (is.null(colnames(mv))) colnames(mv) <- paste0("M", seq_len(q))
    if (is.null(colnames(cs))) colnames(cs) <- paste0("S", seq_len(p))
    row_keys <- paste(rep(group_ids, each = q),
                      rep(colnames(mv), times = length(group_ids)),
                      sep = " ~ ")
    r_keys <- paste(colnames(mv)[rep(seq_len(q), each = p)],
                    colnames(cs)[rep(seq_len(p), times = q)], sep = " ~ ")
    if (anyDuplicated(row_keys) || anyDuplicated(r_keys)) {
      stop("Scale/measure names produce ambiguous internal keys (duplicate ",
           "names, or a name containing \" ~ \"); rename the columns.",
           call. = FALSE)
    }
    rownames(scores) <- row_keys
  }

  # Generate score draws, one boots x p matrix per profile row ---------------
  draw_list <- list()
  for (g in seq_along(group_ids)) {
    rows_g <- grp == group_ids[g]
    n_g <- sum(rows_g)
    cs_g <- cs[rows_g, , drop = FALSE]
    if (is.null(mv)) {
      # Sampling distribution of the group mean vector (CLT): the sample
      # covariance of the observations scaled by 1/n. For occasions (occ_k
      # occasion blocks in contiguous strides), cs holds the stacked k*p
      # person vectors: the joint draw's off-diagonal p x p blocks carry the
      # within-person cross-occasion covariance (spec sec. 2.2), and the
      # draw matrix is then split back into per-occasion blocks so each
      # profile row's block holds exactly `boots` rows for the batched
      # transform below.
      d_g <- mvn_draws(boots, colMeans(cs_g), stats::cov(cs_g) / n_g)
      if (is.null(occ_k)) {
        draw_list[[length(draw_list) + 1]] <- d_g
      } else {
        p_occ <- ncol(cs) / occ_k
        for (j in seq_len(occ_k)) {
          draw_list[[length(draw_list) + 1]] <-
            d_g[, (j - 1) * p_occ + seq_len(p_occ), drop = FALSE]
        }
      }
    } else {
      mv_g <- mv[rows_g, , drop = FALSE]
      # This group's observed correlations (rows = measures, cols = scales)
      rmat <- scores[paste(group_ids[g], colnames(mv), sep = " ~ "), ,
                     drop = FALSE]
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
      # non-normal data (like the bootstrap it is compared against). Built in
      # one vectorized pass over precomputed squares; per element this is the
      # same arithmetic (a*b - c*(d+e)) as the original per-column loop.
      zc <- scale(cs_g)
      zm <- scale(mv_g)
      zc2 <- zc^2
      zm2 <- zm^2
      mi <- rep(seq_len(q), each = p)
      ji <- rep(seq_len(p), times = q)
      r_vec <- as.vector(t(rmat))
      names(r_vec) <- paste(colnames(mv)[mi], colnames(cs)[ji], sep = " ~ ")
      psi <- zm[, mi, drop = FALSE] * zc[, ji, drop = FALSE] -
        (zm2[, mi, drop = FALSE] + zc2[, ji, drop = FALSE]) *
          matrix(r_vec / 2, n_g, q * p, byrow = TRUE)
      acov_r <- crossprod(psi) / n_g^2
      # Draw on the Fisher z scale (delta-method covariance), back-transform
      dz <- 1 / (1 - r_vec^2)
      acov_z <- acov_r * tcrossprod(dz)
      r_draws <- tanh(mvn_draws(boots, atanh(r_vec), acov_z))
      colnames(r_draws) <- names(r_vec)
      for (m in seq_len(q)) {
        cols <- paste(colnames(mv)[m], colnames(cs), sep = " ~ ")
        draw_list[[length(draw_list) + 1]] <-
          r_draws[, cols, drop = FALSE]
      }
    }
  }

  # Propagate every profile row's draws through the SSM transformation in one
  # batched compiled call (row order within each block is unchanged, so the
  # values are identical to per-block calls). The fixed-stride split below
  # requires every block to hold exactly `boots` rows.
  n_par <- length(ssm_param_names())
  all_draws <- do.call(rbind, draw_list)
  stopifnot(nrow(all_draws) == length(draw_list) * boots)
  all_pars <- matrix(group_parameters(all_draws, angles), ncol = n_par,
                     byrow = TRUE)
  par_list <- lapply(seq_along(draw_list), function(i) {
    all_pars[(i - 1) * boots + seq_len(boots), , drop = FALSE]
  })

  t <- do.call(cbind, par_list)
  if (contrast) {
    # Second profile row minus first (displacement via angular distance),
    # sharing param_diff() with the bootstrap path so the contrast convention
    # has one definition. Contrasts require exactly two profile rows (validated
    # in ssm_analyze).
    t <- cbind(t, param_diff(par_list[[2]], par_list[[1]]))
  }
  t
}

# Symmetric eigendecomposition square root with PSD clamping: tolerates
# positive-semidefinite covariances (e.g., ipsatized scales are sum-
# constrained, making the covariance singular) where a Cholesky factor would
# fail; negative eigenvalues from floating-point noise are clamped to zero.
# THE single draw-root convention -- shared by the Monte Carlo engine's
# mvn_draws() and ssm_ci_accuracy()'s plug-in population generator, so the
# populations the diagnostic simulates from cannot drift numerically from the
# engine it assesses.
mvn_root <- function(sigma) {
  eig <- eigen(sigma, symmetric = TRUE)
  eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
}

# Draw R samples from a multivariate normal via mvn_root()
mvn_draws <- function(R, mu, sigma) {
  p <- length(mu)
  z <- matrix(stats::rnorm(R * p), nrow = R, ncol = p)
  sweep(z %*% mvn_root(sigma), 2, mu, "+")
}
