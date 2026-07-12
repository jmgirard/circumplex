# Dispatch to the requested confidence-interval engine. Both the mean-based and
# correlation-based analysis paths call this so the engine choice lives in one
# place; `method` is already validated (match.arg) in ssm_analyze(). The
# bootstrap-only arguments (bs_function, parallel, ncpus, strata) are ignored by
# the Monte Carlo engine, and obs_scores (the caller's already-computed observed
# score matrix) is ignored by the bootstrap engine.
ssm_estimate_intervals <- function(method, bs_input, bs_function, scales,
                                   measures = NULL, angles, boots, interval,
                                   contrast, listwise, parallel, ncpus, strata,
                                   obs_scores) {
  if (method == "montecarlo") {
    ssm_montecarlo(
      bs_input = bs_input, scales = scales, measures = measures,
      angles = angles, boots = boots, interval = interval,
      contrast = contrast, listwise = listwise, obs_scores = obs_scores
    )
  } else {
    ssm_bootstrap(
      bs_input = bs_input, bs_function = bs_function, scales = scales,
      measures = measures, angles = angles, boots = boots, interval = interval,
      contrast = contrast, listwise = listwise, parallel = parallel,
      ncpus = ncpus, strata = strata
    )
  }
}

# Perform bootstrap to get confidence intervals around SSM parameters
ssm_bootstrap <- function(bs_input, bs_function, scales, measures = NULL,
                          angles, boots, interval, contrast, listwise,
                          parallel = "no", ncpus = 1, ...) {

  # Perform bootstrapping ------------------------------------------------------
  # Note on parallel reproducibility: for this nonparametric bootstrap,
  # boot::boot() draws the full resample index array in the master process
  # (master RNG) before dispatching, and bs_function is deterministic, so
  # results for a given seed are identical for any parallel/ncpus setting.
  bs_results <-
    boot::boot(
      data = bs_input,
      statistic = bs_function,
      R = boots,
      scales = scales,
      measures = measures,
      angles = angles,
      contrast = contrast,
      listwise = listwise,
      parallel = parallel,
      ncpus = ncpus,
      ...
    )

  ssm_replicate_intervals(
    t0 = bs_results$t0,
    t = bs_results$t,
    interval = interval,
    contrast = contrast,
    replicate_label = "bootstrap resamples"
  )
}

# Turn a matrix of SSM parameter replicates into estimates and intervals ------
# Shared interval-assembly back end for the bootstrap and Monte Carlo engines:
# t0 is the observed parameter vector (6 per group, in ssm_param_names() order,
# displacement in radians) and t is the replicate matrix with one row per
# resample/draw and the same columns as t0.
ssm_replicate_intervals <- function(t0, t, interval, contrast,
                                    replicate_label) {

  # Extract point estimates from the observed parameter vector ----------------
  bs_est <- reshape_params(t0, suffix = "est")
  bs_t <- as.data.frame(t)
  # Name every replicate column by its parameter and group, so displacement
  # columns can be located by name below instead of by positional arithmetic.
  pnames <- ssm_param_names()
  n_groups <- nrow(bs_est)
  param_of_col <- rep(pnames, times = n_groups)
  colnames(bs_t) <- paste(
    param_of_col,
    rep(seq_len(n_groups), each = length(pnames)),
    sep = "_"
  )

  # Degenerate profiles (flat or zero-amplitude) carry NA parameters -----------
  if (any(is.na(bs_est$d_est))) {
    warning(
      "One or more observed profiles are flat or have zero amplitude; ",
      "their displacement (and fit, if flat) is undefined and reported as NA.",
      call. = FALSE
    )
  }
  n_bad <- sum(!stats::complete.cases(bs_t))
  if (n_bad > 0) {
    warning(
      n_bad, " of ", nrow(bs_t), " ", replicate_label, " produced degenerate ",
      "(flat or zero-amplitude) profiles; their undefined parameter(s) ",
      "(displacement, and fit if flat) were excluded from that parameter's ",
      "confidence interval only, which is therefore conditional on ",
      "estimability. Their other, well-defined parameters still contribute ",
      "to their confidence intervals.",
      call. = FALSE
    )
  }

  # Set the units of the displacement results to radians -----------------------
  # Locate displacement columns by name. When contrasting, the final group is
  # the contrast: its displacement takes the contrast radian class (which
  # permits negative values); every other displacement takes the standard class.
  d_cols <- which(param_of_col == "d")
  if (contrast) {
    contrast_d_col <- d_cols[length(d_cols)]
    bs_t[contrast_d_col] <- lapply(bs_t[contrast_d_col], new_contrast_radian)
    d_cols <- d_cols[-length(d_cols)]
  }
  bs_t[d_cols] <- lapply(bs_t[d_cols], new_radian)

  # Calculate the lower bounds of the confidence intervals ---------------------
  bs_lci <- sapply(bs_t, quantile, probs = ((1 - interval) / 2), na.rm = TRUE)
  bs_lci <- reshape_params(bs_lci, suffix = "lci")
  bs_lci$fit_lci <- NULL

  # Calculate the upper bounds of the confidence intervals ---------------------
  bs_uci <- sapply(bs_t, quantile, probs = (1 - (1 - interval) / 2), na.rm = TRUE)
  bs_uci <- reshape_params(bs_uci, suffix = "uci")
  bs_uci$fit_uci <- NULL

  # Combine the results in one data frame and convert radians to degrees -------
  out <- cbind(bs_est, bs_lci, bs_uci)

  # Report the contrast displacement CI on the same branch as its estimate -----
  # The estimate (angle_dist) lives on the principal branch (-pi, pi]; the CI
  # (circular-mean centering) lives on its own contiguous branch. Near +/-pi
  # these can disagree, leaving the estimate numerically outside an interval
  # it is geometrically inside. Shifting both CI endpoints by the same
  # multiple of 2*pi preserves the interval's width and contiguity and is the
  # identity whenever the two already share a branch.
  if (contrast) {
    i <- nrow(out)
    if (all(is.finite(c(out$d_est[i], out$d_lci[i], out$d_uci[i])))) {
      mid <- (out$d_lci[i] + out$d_uci[i]) / 2
      k <- round((out$d_est[i] - mid) / (2 * pi))
      out$d_lci[i] <- out$d_lci[i] + 2 * pi * k
      out$d_uci[i] <- out$d_uci[i] + 2 * pi * k
    }
  }

  out[c("d_est", "d_lci", "d_uci")] <- lapply(
    out[c("d_est", "d_lci", "d_uci")],
    function(x) as_degree(as_radian(x))
  )

  out
}

# Calculate SSM parameters per group (or parameter differences)
ssm_by_group <- function(scores, angles, contrast) {

  # Calculate SSM parameters per group
  results <- group_parameters(scores, angles)

  # If contrasting, append SSM parameter differences
  if (contrast) {
    results <- c(results, param_diff(results[7:12], results[1:6]))
  }

  results
}

# Calculate quantiles for circular data in radians
#' @export
quantile.circumplex_radian <- function(x, na.rm = TRUE, ...) {
  if (all(is.na(x))) return(NA_real_)
  x <- unclass(x)
  mean_angle <- atan2(mean(sin(x), na.rm = na.rm), mean(cos(x), na.rm = na.rm))
  angles_centered <- (x - mean_angle + pi) %% (2 * pi) - pi
  quantiles_centered <- stats::quantile(angles_centered, na.rm = na.rm, ...)
  out <- (quantiles_centered + mean_angle) %% (2 * pi)
  out[abs(out - (2 * pi)) < (.Machine$double.eps * 2)] <- 0
  as_radian(out)
}

# Calculate quantiles for circular contrast data in radians (allowing negatives)
#' @export
quantile.circumplex_contrast_radian <- function(x, na.rm = TRUE, ...) {
  if (all(is.na(x))) return(NA_real_)
  x <- unclass(x)
  mean_angle <- atan2(mean(sin(x), na.rm = na.rm), mean(cos(x), na.rm = na.rm))
  angles_centered <- (x - mean_angle + pi) %% (2 * pi) - pi
  quantiles_centered <- stats::quantile(angles_centered, na.rm = na.rm, ...)
  quantiles_centered + mean_angle
}
