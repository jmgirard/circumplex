#' Correlational Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for any
#' number of measures (based on their correlations with the circumplex scales).
#'
#' @param .data A matrix or data frame containing at least circumplex scales and
#'   numeric measures to be evaluated.
#' @param scales A list of variables in \code{.data} that contain circumplex
#'   circumplex scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param measures A list of variables in \code{.data} that contain numeric
#'   values (in tidyverse-style NSE specification, see examples).
#' @param pairwise A logical determining whether the measures should be
#'   compared. If TRUE, the difference between each unique pairwise combination
#'   of measures will be output (default = FALSE).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @param plot A logical determining whether a plot should be created (default =
#'   TRUE).
#' @param ... Additional parameters to be passed to \code{ssm_plot()}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each measure (based on their correaltions with the circumplex scales).

ssm_measures <- function(.data, scales, angles, measures, pairwise = FALSE, 
  boots = 2000, interval = 0.95, plot = TRUE, ...) {
  
  # Enable column specification using tidyverse-style NSE -------------------
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), !missing(measures))
  assert_that(is.numeric(interval), interval > 0, interval < 1)
  
  # Iterate over measures to calculate SSM paramters ------------------------
  data_scales <- .data %>%
    dplyr::select(!!scales_en)
  data_measures <- .data %>%
    dplyr::select(!!measures_en)
  results <- data_measures %>%
    purrr::map_dfr(~ssm_measures_one(mutate(data_scales, measure = .),
      angles, boots, interval), .id = "Measure")
  
  # Generate plot if requested ----------------------------------------------
  if (plot == TRUE) {
    p <- ssm_plot(results, angles, type = "Measure")
    print(p)
  }

  # Calculate pairwise contrasts if requested -------------------------------
  if (pairwise == TRUE) {
    m_pairs <- unique_pairs(factor(colnames(data_measures)))
    cmp_function <- function(pair, data_m, data_s) {
      data_compare <- data_m %>%
        dplyr::select(as.character(pair)) %>%
        dplyr::bind_cols(data_s)
      ssm_measures_two(data_compare, angles, boots, interval)
    }
    c_results <- m_pairs %>%
      purrrlyr::by_row(cmp_function, data_measures, data_scales,
        .collate = "rows") %>%
      dplyr::mutate(Contrast = sprintf("%s-%s", V2, V1)) %>%
      dplyr::select(-c(V1, V2, .row))
    if (plot == TRUE) {
      pd <- diff_plot(c_results)
      print(pd)
    }
    results <- dplyr::bind_rows(results, c_results) %>%
      dplyr::select(Measure, Contrast, dplyr::everything())
  }
  # TODO: Create htmlTable from results (after tidying it a bit)
  results
}

#' Correlational Structural Summary Method
#'
#' Worker function for ssm_measures, calculates point and interval estimates for
#' SSM parameters for a single measure.
#'
#' @param .data A matrix or data frame containing only circumplex scales and one
#'   measure to be evaluated.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the measure.

ssm_measures_one <- function(.data, angles, boots, interval) {
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(are_equal(length(.data) - 1, length(angles)))
  
  # Get SSM parameter estimates for mean profile ----------------------------
  rmat <- .data %>% cor()
  scores <- rmat["measure", 1:(ncol(rmat) - 1)]
  ssm <- ssm_parameters(scores, angles, tibble = FALSE)
  
  # Perform bootstrap on SSM parameters -------------------------------------
  bs_function <- function(.data, index, angles) {
    resample <- .data[index, ]
    rmat_r <- resample %>% stats::cor()
    scores_r <- rmat_r["measure", 1:(ncol(rmat_r) - 1)]
    ssm_parameters(scores_r, angles, tibble = FALSE)
  }
  ssm_bootstrap(.data, bs_function, ssm, angles, boots, interval)

}

#' Correlational Comparison Structural Summary Method
#'
#' Worker function for ssm_measures, calculates point and interval estimates for
#' the difference between two measures' correlational SSM parameters.
#'
#' @param .data A matrix or data frame containing exactly two measure variables
#'   and any number of circumplex scales.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{.data} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals.
#' @param interval The confidence intervals' percentage level.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the difference between two measures (based on correlations).

ssm_measures_two <- function(.data, angles, boots, interval) {
  
  # TODO: Can increase speed of pairwise comparisons by doing a single
  # stratified bootstrapping with all pairwise comparisons built in, rather than
  # separate bootstraps for each pairwise combination.
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(are_equal(length(.data) - 2, length(angles)))
  
  # Get SSM parameter estimates for mean profile ----------------------------
  rmat <- .data %>% stats::cor()
  scores_m1 <- rmat[1, 3:ncol(rmat)]
  scores_m2 <- rmat[2, 3:ncol(rmat)]
  ssm_m1 <- ssm_parameters(scores_m1, angles, tibble = FALSE)
  ssm_m2 <- ssm_parameters(scores_m2, angles, tibble = FALSE)
  ssm_md <- param_diff(ssm_m1, ssm_m2)
  
  # Perform bootstrap on SSM parameters -------------------------------------
  bs_function <- function(.data, index, angles) {
    resample <- .data[index, ]
    rmat_r <- resample %>% stats::cor()
    scores_r1 <- rmat_r[1, 3:ncol(rmat_r)]
    scores_r2 <- rmat_r[2, 3:ncol(rmat_r)]
    ssm_r1 <- ssm_parameters(scores_r1, angles, tibble = FALSE)
    ssm_r2 <- ssm_parameters(scores_r2, angles, tibble = FALSE)
    param_diff(ssm_r1, ssm_r2)
  }
  ssm_bootstrap(.data, bs_function, ssm_md, angles, boots, interval)

}
