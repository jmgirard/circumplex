#' Mean Profile Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' of any number of groups.
#'
#' @param .data A matrix or data frame containing at least circumplex scales.
#' @param scales A list of the variables in \code{.data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @param grouping Optional argument: the variable in \code{.data} that contains
#'   the group membership of each observation. To assess the mean profile of all
#'   observations, do not supply this argument (see examples).
#' @param plot A logical determining whether a plot should be created (default =
#'   TRUE).
#' @param ... Additional parameters to be passed to \code{ssm_plot()}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).

ssm_profiles <- function(.data, scales, angles,
  boots = 2000, interval = 0.95, grouping, plot = TRUE, ...) {
  
  # Enable column specification using tidyverse-style NSE -------------------
  scales_en <- rlang::enquo(scales)
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(is.numeric(angles), is.count(boots))
  assert_that(is.numeric(interval), interval > 0, interval < 1)
  
  # Handle the presence or absence of a grouping variable -------------------
  if (base::missing(grouping)) {
    results <- .data %>%
      dplyr::select(!!scales_en) %>%
      ssm_profiles_one(angles, boots, interval)
  } else {
    grouping_en <- rlang::enquo(grouping)
    results <- .data %>% 
      dplyr::select(!!grouping_en, !!scales_en) %>% 
      dplyr::mutate(Group = factor(!!grouping_en)) %>% 
      dplyr::select(-!!grouping_en) %>%
      dplyr::group_by(Group) %>%
      purrrlyr::by_slice(ssm_profiles_one, angles, boots, interval,
        .collate = "rows")
  }
  results
}

#' Mean Profile Structural Summary Method
#'
#' Worker function for ssm_profiles, calculates point and interval estimates for
#'   SSM parameters in a single group's mean profile.
#'
#' @param .data A matrix or data frame containing only circumplex scales.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the group's mean profile.

ssm_profiles_one <- function(.data, angles, boots, interval) {
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(are_equal(length(.data), length(angles)))
    
  # Get SSM parameter estimates for mean profile ----------------------------
  scores <- .data %>% colMeans()
  ssm <- ssm_parameters(scores, angles, tibble = FALSE)
  
  # Perform bootstrap on SSM parameters -------------------------------------
  bs_function <- function(.data, index, angles) {
    resample <- .data[index, ]
    scores_r <- colMeans(resample)
    ssm_r <- ssm_parameters(scores_r, angles, tibble = FALSE)
    return(ssm_r)
  }
  results <- ssm_bootstrap(.data, bs_function, ssm, angles, boots, interval)

  results
}

# To be reformatted -------------------------------------------------------

# ssm_profile2 <- function(data, grouping, scales, angles, bs_number = 2000) {
#   # Select variables using tidyverse style NSE
#   grouping <- rlang::enquo(grouping)
#   scales <- rlang::enquo(scales)
#   data_use <- data %>% 
#     dplyr::select(!!grouping, !!scales) %>% 
#     dplyr::mutate(group = factor(!!grouping)) %>% 
#     dplyr::select(-!!grouping)
#   # Abort if the number of levels is not 2
#   stopifnot(nlevels(data_use$group) == 2)
#   # Get SSM estimates in each group and their difference
#   scores <- data_use %>% 
#     dplyr::group_by(group) %>% 
#     dplyr::summarize_all(mean)
#   ssm_g1 <- ssm_parameters(as.double(scores[1, 2:ncol(scores)]), angles, FALSE)
#   ssm_g2 <- ssm_parameters(as.double(scores[2, 2:ncol(scores)]), angles, FALSE)
#   ssm_gd <- param_diff(ssm_g1, ssm_g2)
#   # Perform bootstrapping on SSM parameters
#   bs_function <- function(data, index, angles) {
#     resample <- data[index, ]
#     scores_r <- resample %>% 
#       dplyr::group_by(group) %>% 
#       dplyr::summarize_all(mean)
#     ssm_r1 <- ssm_parameters(as.double(scores_r[1, 2:ncol(scores)]), angles, FALSE)
#     ssm_r2 <- ssm_parameters(as.double(scores_r[2, 2:ncol(scores)]), angles, FALSE)
#     ssm_rd <- param_diff(ssm_r1, ssm_r2)
#     ssm_rs <- c(ssm_r1, ssm_r2, ssm_rd)
#     return(ssm_rs)
#   }
#   bs_results <- boot::boot(
#     data = data_use,
#     statistic = bs_function, 
#     R = bs_number,
#     angles = angles,
#     strata = data_use$group
#   )
#   # Prepare bootstrap results to calculate quantiles
#   bs_t <- tibble::as_tibble(bs_results$t)
#   bs_t <- bs_t %>% 
#     dplyr::mutate(
#       V5 = make_circular(V5), 
#       V11 = make_circular(V11),
#       V17 = make_circular(V17))
#   # Create output including 95\% confidence intervals
#   results <- tibble::tibble(
#     group = c(
#       rep(paste0("Group=", levels(data_use$group)[[1]]), 6),
#       rep(paste0("Group=", levels(data_use$group)[[2]]), 6),
#       rep("Difference", 6)),
#     parameter = rep(c("Elevation", "X-Value", "Y-Value",
#       "Amplitude", "Displacement", "Model Fit"), 3),
#     estimate = c(ssm_g1, ssm_g2, ssm_gd),
#     lower_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .025),
#     upper_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .975)
#   )
#   results
# }
