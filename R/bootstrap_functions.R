#' Calculate bootstrap confidence intervals given data and function
#'
#' @param .data A matrix or data frame containing circumplex scales and possibly
#'   a measure (if called from ssm_measures).
#' @param bs_function A function that calculates the variables to be resampled.
#' @param ssm The best-guess SSM parameter estimates prior to bootstrapping.
#' @param angles A numerical vector specifying the angular displacement of each
#'   circumplex scale (in degrees).
#' @param boots A positive integer specifying the number of bootstrap resamples.
#' @param interval The confidence intervals' percentage level (e.g., 0.95).
#' @param ... Additional parameters to be passed to the \code{boot()} function.
#' @return A tibble containing SSM parameters (point and interval estimates).

ssm_bootstrap <- function(.data, statistic, angles, boots, interval, ...) {
  
  # Perform bootstrapping ---------------------------------------------------
  bs_results <- boot::boot(
    data = .data,
    statistic = statistic, 
    R = boots,
    angles = angles,
    ...
  )
  
  # Reshape parameters from wide to long format -----------------------------
  reshape_params <- function(df) {
    df %>%
      matrix(nrow = 6) %>% 
      t() %>% 
      tibble::as_tibble()
  }
  
  # Calculate point and interval estimates from bootstrap results -----------
  bs_est <- bs_results$t0 %>%
    reshape_params() %>% 
    `colnames<-`(c("e_est", "x_est", "y_est", "a_est", "d_est", "fit"))
  
  bs_t <- bs_results$t %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_at(.funs = as_angle, .vars = (1:(ncol(.) / 6) * 6 - 1))
  
  bs_lci <- bs_t %>%
    purrr::map_dbl(quantile, probs = ((1 - interval) / 2)) %>% 
    reshape_params() %>% 
    `colnames<-`(c("e_lci", "x_lci", "y_lci", "a_lci", "d_lci", "f_lci")) %>% 
    dplyr::select(-f_lci)
  
  bs_uci <- bs_t %>% 
    purrr::map_dbl(quantile, probs = (1 - (1 - interval) / 2)) %>% 
    reshape_params() %>% 
    `colnames<-`(c("e_uci", "x_uci", "y_uci", "a_uci", "d_uci", "f_uci")) %>% 
    dplyr::select(-f_uci)
  
  dplyr::bind_cols(bs_est, bs_lci, bs_uci)
  
}

ssm_by_group <- function(scores, angles, contrast) {

  # To model contrast, subtract scores then SSM -----------------------------
  if (contrast == "model") {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
  }
  
  # Calculate parameters per group ------------------------------------------
  results <- group_parameters(scores, angles)
  
  # To test contrast, SSM then subtract parameters --------------------------
  if (contrast == "test") {
    results <- c(results, param_diff(results[7:12], results[1:6]))
  }
  
  results
}

quantile.angle <- function(x, probs) {
  mdn <- angle_median(x)
  if (is.na(mdn)) return(NA)
  tx <- (x - mdn) %% (2 * pi)
  tx <- compare_pi(tx)
  qtl <- stats::quantile(x = tx, probs = probs)
  (qtl + mdn) %% (2 * pi)
}
