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
  
  # Set the units of the displacement results to radians -----------------------
  if (contrast) {
    # Don't set to rad for contrasted d parameter (we want to allow negatives)
    d_vars <- 1:((ncol(bs_t) - 6) / 6) * 6 - 1
  } else {
    d_vars <- 1:(ncol(bs_t) / 6) * 6 - 1
  }
  bs_t[d_vars] <- lapply(bs_t[d_vars], new_radian)

  # Calculate the lower bounds of the confidence intervals ---------------------
  bs_lci <- sapply(bs_t, quantile, probs = ((1 - interval) / 2))
  bs_lci <- reshape_params(bs_lci, suffix = "lci")
  bs_lci$fit_lci <- NULL

  # Calculate the upper bounds of the confidence intervals ---------------------
  bs_uci <- sapply(bs_t, quantile, probs = (1 - (1 - interval) / 2))
  bs_uci <- reshape_params(bs_uci, suffix = "uci")
  bs_uci$fit_uci <- NULL

  # Combine the results in one data frame and convert radians to degrees -------
  out <- cbind(bs_est, bs_lci, bs_uci)
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
  as_radian(out)
}
