# Perform bootstrap to get confidence intervals around SSM parameters
ssm_bootstrap <- function(bs_input, bs_function, angles, boots, interval,
                          contrast, listwise, ...) {

  # Perform bootstrapping ------------------------------------------------------
  bs_results <- boot::boot(
    data = bs_input,
    statistic = bs_function,
    R = boots,
    angles = angles,
    contrast = contrast,
    listwise = listwise,
    ...
  )

  # Reshape parameters from wide to long format --------------------------------
  reshape_params <- function(df, suffix) {
    df %>%
      matrix(ncol = 6, byrow = TRUE) %>%
      `colnames<-`(paste0(c("e_", "x_", "y_", "a_", "d_", "fit_"), suffix)) %>%
      tibble::as_tibble(nrow = nrow(.))
  }

  # Extract point estimates from bootstrap results -----------------------------
  bs_est <- bs_results$t0 %>%
    reshape_params(suffix = "est")

  # Set the units of the displacement results to radians -----------------------
  bs_t <- bs_results$t %>%
    `colnames<-`(paste0("t", 1:ncol(.))) %>%
    tibble::as_tibble(nrow = nrow(.))
  if (contrast == "none" || contrast == "model") {
    d_vars <- 1:(ncol(bs_t) / 6) * 6 - 1
  } else if (contrast == "test") {
    d_vars <- 1:((ncol(bs_t) - 6) / 6) * 6 - 1
  }
  bs_t <- bs_t %>% dplyr::mutate_at(.funs = as_radian, .vars = d_vars)

  # Calculate the lower bounds of the confidence intervals ---------------------
  bs_lci <- bs_t %>%
    purrr::map_dbl(.f = quantile, probs = ((1 - interval) / 2)) %>%
    reshape_params(suffix = "lci") %>%
    dplyr::select(-fit_lci)

  # Calculate the upper bounds of the confidence intervals ---------------------
  bs_uci <- bs_t %>%
    purrr::map_dbl(.f = quantile, probs = (1 - (1 - interval) / 2)) %>%
    reshape_params(suffix = "uci") %>%
    dplyr::select(-fit_uci)

  # Combine the results in one tibble and convert radians to degrees -----------
  dplyr::bind_cols(bs_est, bs_lci, bs_uci) %>%
    dplyr::mutate(
      d_est = as_degree(as_radian(d_est)),
      d_lci = as_degree(as_radian(d_lci)),
      d_uci = as_degree(as_radian(d_uci))
    )
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
quantile.radian <- function(x, na.rm = TRUE, ...) {
  if (all(is.na(x))) return(NA)
  mdn <- angle_median(x)
  tx <- (x - mdn) %% (2 * pi)
  tx <- compare_pi(tx)
  class(tx) <- "numeric"
  qtl <- quantile(x = tx, na.rm = na.rm, ...)
  as_radian((qtl + mdn) %% (2 * pi))
}
