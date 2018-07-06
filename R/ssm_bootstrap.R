# Perform bootstrap to get confidence intervals around SSM parameters
ssm_bootstrap <- function(bs_input, bs_function, angles, boots, interval,
                          contrasts, ...) {

  # Perform bootstrapping ------------------------------------------------------
  bs_results <- boot::boot(
    data = bs_input,
    statistic = bs_function,
    R = boots,
    angles = angles,
    contrasts = contrasts,
    ...
  )

  # Reshape parameters from wide to long format --------------------------------
  reshape_params <- function(df) {
    df %>%
      matrix(ncol = 6, byrow = TRUE) %>%
      tibble::as_tibble()
  }

  # Extract point estimates from bootstrap results -----------------------------
  bs_est <- bs_results$t0 %>%
    reshape_params() %>%
    `colnames<-`(c("e_est", "x_est", "y_est", "a_est", "d_est", "fit"))

  # Set the units of the displacement results to radians -----------------------
  bs_t <- bs_results$t %>%
    tibble::as_tibble()
  if (contrasts == "none" || contrasts == "model") {
    d_vars <- 1:(ncol(bs_t) / 6) * 6 - 1
  } else if (contrasts == "test") {
    d_vars <- 1:((ncol(bs_t) - 6) / 6) * 6 - 1
  }
  bs_t <- bs_t %>% dplyr::mutate_at(.funs = as_radian, .vars = d_vars)

  # Calculate the lower bounds of the confidence intervals ---------------------
  bs_lci <- bs_t %>%
    purrr::map_dbl(.f = stats::quantile, probs = ((1 - interval) / 2)) %>%
    reshape_params() %>%
    `colnames<-`(c("e_lci", "x_lci", "y_lci", "a_lci", "d_lci", "f_lci")) %>%
    dplyr::select(-f_lci)

  # Calculate the upper bounds of the confidence intervals ---------------------
  bs_uci <- bs_t %>%
    purrr::map_dbl(.f = stats::quantile, probs = (1 - (1 - interval) / 2)) %>%
    reshape_params() %>%
    `colnames<-`(c("e_uci", "x_uci", "y_uci", "a_uci", "d_uci", "f_uci")) %>%
    dplyr::select(-f_uci)

  # Combine the results in one tibble and convert radians to degrees -----------
  dplyr::bind_cols(bs_est, bs_lci, bs_uci) %>%
    dplyr::mutate(
      d_est = as_degree(as_radian(d_est)),
      d_lci = as_degree(as_radian(d_lci)),
      d_uci = as_degree(as_radian(d_uci))
    )
}

#
ssm_by_group <- function(scores, angles, contrasts) {

  # To model contrast, subtract scores then SSM --------------------------------
  if (contrasts == "model") {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
  }

  # Calculate parameters per group ---------------------------------------------
  results <- group_parameters(scores, angles)

  # To test contrast, SSM then subtract parameters -----------------------------
  if (contrasts == "test") {
    results <- c(results, param_diff(results[7:12], results[1:6]))
  }

  results
}

# Calculate quantiles for circular data in radians
quantile.radian <- function(x, ...) {
  mdn <- angle_median(x)
  if (is.na(mdn)) return(NA)
  tx <- (x - mdn) %% (2 * pi)
  tx <- compare_pi(tx)
  class(tx) <- "numeric"
  qtl <- stats::quantile(x = tx, ...)
  as_radian((qtl + mdn) %% (2 * pi))
}
