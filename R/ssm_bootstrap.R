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

  # Reshape parameters from wide to long format --------------------------------
  reshape_params <- function(v, suffix) {
    # Convert vector to matrix
    out <- matrix(v, ncol = 6, byrow = TRUE)
    # Add column names
    colnames(out) <- paste0(c("e_", "x_", "y_", "a_", "d_", "fit_"), suffix)
    # Convert to data frame
    as.data.frame(out)
  }

  # Extract point estimates from bootstrap results -----------------------------
  bs_est <- reshape_params(bs_results$t0, suffix = "est")
  bs_t <- bs_results$t
  bs_t <- as.data.frame(bs_t)
  colnames(bs_t) <- paste0("t", 1:ncol(bs_t))
  
  # Set the units of the displacement results to radians -----------------------

  if (contrast == "none" || contrast == "model") {
    d_vars <- 1:(ncol(bs_t) / 6) * 6 - 1
  } else if (contrast == "test") {
    d_vars <- 1:((ncol(bs_t) - 6) / 6) * 6 - 1
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

  # Combine the results in one tibble and convert radians to degrees -----------
  out <- cbind(bs_est, bs_lci, bs_uci)
  out[c("d_est", "d_lci", "d_uci")] <- lapply(
    out[c("d_est", "d_lci", "d_uci")], 
    function(x) as_degree(as_radian(x))
  )
  
  out
}

#
ssm_by_group <- function(scores, angles, contrast) {

  # To model contrast, subtract scores then SSM --------------------------------
  if (contrast == "model") {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
  }

  # Calculate parameters per group ---------------------------------------------
  results <- group_parameters(scores, angles)

  # To test contrast, SSM then subtract parameters -----------------------------
  if (contrast == "test") {
    results <- c(results, param_diff(results[7:12], results[1:6]))
  }

  results
}

# Calculate quantiles for circular data in radians
#' @export
quantile.circumplex_radian <- function(x, na.rm = TRUE, ...) {
  if (all(is.na(x))) return(NA)
  mdn <- angle_median(x)
  tx <- (x - mdn) %% (2 * pi)
  tx <- compare_pi(tx)
  class(tx) <- "numeric"
  qtl <- quantile(x = tx, na.rm = na.rm, ...)
  as_radian((qtl + mdn) %% (2 * pi))
}
