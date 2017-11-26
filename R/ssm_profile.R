#' Mean Profile Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' of any number of groups.
#'
#' @param data A matrix or data frame containing at least circumplex scales.
#' @param scales A list of the variables in \code{data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param grouping The variable in \code{data} that contains each observation's
#'   group membership (in tidyverse-style NSE specification, see examples).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile.
#' @examples 
#' ssm_profiles(girard2017, ZPA:ZNO, octants, bordl, 2000, 0.95)

ssm_profiles <- function(data, scales, angles,
                         grouping = NULL, boots = 2000, interval = 0.95) {
  # Enable tidyverse-style NSE column specification -------------------------
  grouping_en <- rlang::enquo(grouping)
  scales_en <- rlang::enquo(scales)
  
  # Coerce grouping variable to a factor named group ------------------------
  data_use <- data %>% 
    dplyr::select(!!grouping_en, !!scales_en) %>% 
    dplyr::mutate(group = factor(!!grouping_en)) %>% 
    dplyr::select(-!!grouping_en)
  
  # Calculate SSM parameters and confidence intervals for each group --------
  results <- data_use %>% 
    group_by(group) %>% 
    by_slice(ssm_profile_basic, angles, boots, interval, .collate = "rows")
  
  return(results)
}

#' Mean Profile Structural Summary Method
#'
#' Worker function for ssm_profiles, calculates point and interval estimates for
#'   SSM parameters in a single group's mean profile.
#'
#' @param data_use A matrix or data frame containing only circumplex scales.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the group's mean profile.
#' @examples
#' ssm_profiles_one(wright2009, octants, 2000, 0.95)

ssm_profiles_one <- function(data_use, angles, boots, interval) {
  # Get SSM parameter estimates for mean profile ----------------------------
  scores <- data_use %>% colMeans()
  ssm <- ssm_parameters(scores, angles, FALSE)
  
  # Perform bootstrap on SSM parameters -------------------------------------
  bs_function <- function(data, index, angles) {
    resample <- data[index, ]
    scores_r <- colMeans(resample)
    ssm_r <- ssm_parameters(scores_r, angles, FALSE)
    return(ssm_r)
  }
  bs_results <- boot::boot(
    data = data_use,
    statistic = bs_function, 
    R = boots,
    angles = angles
  )
  
  # Prepare bootstrap results for quantile calculation ----------------------
  bs_t <- tibble::as_tibble(bs_results$t)
  bs_t <- dplyr::mutate(bs_t, V5 = make_circular(V5))
  
  # Create output including confidence intervals ----------------------------
  low_p <- (1 - interval) / 2
  upp_p <- 1 - (1 - interval) / 2 
  results <- tibble::tibble(
    Parameter = c("Elevation", "X-Value", "Y-Value", 
      "Amplitude", "Displacement", "Model Fit"),
    Estimate = ssm,
    Lower_CI = purrr::map_dbl(bs_t, smart_quantile, probs = low_p),
    Upper_CI = purrr::map_dbl(bs_t, smart_quantile, probs = upp_p)
  )
  
  return(results)
}

#' Mean Profile Comparison Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' profile of two groups in a sample and their difference.
#'
#' @param data A matrix or data frame containing circumplex scales and group
#'   membership.
#' @param grouping The column name in \code{data} that specifies each
#'   observation's group. Be sure to enter the name as is (i.e., not as a string
#'   or number). This variable can be a factor with two levels or a numeric or
#'   character variable with two unique values.
#' @param scales A vector that contains the column names in \code{data} that
#'   correspond to scores on each circumplex scale. Be sure to enter the names
#'   as is (i.e., not as strings or a character vector). For convenience,
#'   consecutive columns can be captured using a colon (e.g., \code{x1:x8}).
#' @param angles A vector of angles, in degrees, of the circumplex scales.
#' @param bs_number The number of bootstrap resamples (default = 2000).
#' @return A tibble (data frame) containing estimates and bootstrapped 95\%
#'   confidence intervals for the mean profile's structural summary parameters:
#'   elevation, x-value, y-value, amplitude, displacement, and model fit.
#' @examples
#' ssm_profile2(girard2017, isFemale, ZPA:ZNO, octants, 2000)

ssm_profile2 <- function(data, grouping, scales, angles, bs_number = 2000) {
  # Select variables using tidyverse style NSE
  grouping <- rlang::enquo(grouping)
  scales <- rlang::enquo(scales)
  data_use <- data %>% 
    dplyr::select(!!grouping, !!scales) %>% 
    dplyr::mutate(group = factor(!!grouping)) %>% 
    dplyr::select(-!!grouping)
  # Abort if the number of levels is not 2
  stopifnot(nlevels(data_use$group) == 2)
  # Get SSM estimates in each group and their difference
  scores <- data_use %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarize_all(mean)
  ssm_g1 <- ssm_parameters(as.double(scores[1, 2:ncol(scores)]), angles, FALSE)
  ssm_g2 <- ssm_parameters(as.double(scores[2, 2:ncol(scores)]), angles, FALSE)
  ssm_gd <- param_diff(ssm_g1, ssm_g2)
  # Perform bootstrapping on SSM parameters
  bs_function <- function(data, index, angles) {
    resample <- data[index, ]
    scores_r <- resample %>% 
      dplyr::group_by(group) %>% 
      dplyr::summarize_all(mean)
    ssm_r1 <- ssm_parameters(as.double(scores_r[1, 2:ncol(scores)]), angles, FALSE)
    ssm_r2 <- ssm_parameters(as.double(scores_r[2, 2:ncol(scores)]), angles, FALSE)
    ssm_rd <- param_diff(ssm_r1, ssm_r2)
    ssm_rs <- c(ssm_r1, ssm_r2, ssm_rd)
    return(ssm_rs)
  }
  bs_results <- boot::boot(
    data = data_use,
    statistic = bs_function, 
    R = bs_number,
    angles = angles,
    strata = data_use$group
  )
  # Prepare bootstrap results to calculate quantiles
  bs_t <- tibble::as_tibble(bs_results$t)
  bs_t <- bs_t %>% dplyr::mutate(
    bs_t, V5 = make_circular(V5), 
    V11 = make_circular(V11),
    V17 = make_circular(V17))
  # Create output including 95\% confidence intervals
  results <- tibble::tibble(
    group = c(
      rep(paste0("Group=", levels(data_use$group)[[1]]), 6),
      rep(paste0("Group=", levels(data_use$group)[[2]]), 6),
      rep("Difference", 6)),
    parameter = rep(c("Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit"), 3),
    estimate = c(ssm_g1, ssm_g2, ssm_gd),
    lower_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .025),
    upper_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .975)
  )
  return(results)
}
