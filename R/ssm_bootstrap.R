# Perform bootstrap to get confidence intervals around SSM parameters
ssm_bootstrap <- function(bs_input, bs_function, scales, measures = NULL,
                          angles, boots, interval, contrast, listwise, ...) {

  # Perform bootstrapping ------------------------------------------------------
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
      ...
    )

  # Extract point estimates from bootstrap results -----------------------------
  bs_est <- reshape_params(bs_results$t0, suffix = "est")
  bs_t <- bs_results$t
  bs_t <- as.data.frame(bs_t)
  colnames(bs_t) <- paste0(
    c("e", "x", "y", "a", "d", "fit"),
    rep(1:nrow(bs_est), each = 6)
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
      n_bad, " of ", nrow(bs_t), " bootstrap resamples produced degenerate ",
      "(flat or zero-amplitude) profiles and were excluded from the ",
      "confidence intervals, which are therefore conditional on estimability.",
      call. = FALSE
    )
  }

  # Set the units of the displacement results to radians -----------------------
  if (contrast) {
    # Convert individual group d variables to standard radian class
    d_vars <- 1:((ncol(bs_t) - 6) / 6) * 6 - 1

    # Target the contrasted d parameter and apply the contrast class
    contrast_d_vars <- ncol(bs_t) - 1
    bs_t[contrast_d_vars] <- lapply(bs_t[contrast_d_vars], function(x) {
      structure(x, class = c("circumplex_contrast_radian", "numeric"))
    })
  } else {
    d_vars <- 1:(ncol(bs_t) / 6) * 6 - 1
  }
  bs_t[d_vars] <- lapply(bs_t[d_vars], new_radian)

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
  if (all(is.na(x))) return(NA)
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
  if (all(is.na(x))) return(NA)
  x <- unclass(x)
  mean_angle <- atan2(mean(sin(x), na.rm = na.rm), mean(cos(x), na.rm = na.rm))
  angles_centered <- (x - mean_angle + pi) %% (2 * pi) - pi
  quantiles_centered <- stats::quantile(angles_centered, na.rm = na.rm, ...)
  quantiles_centered + mean_angle
}
