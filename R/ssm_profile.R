#' Mean Profile Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' profile of a sample.
#'
#' @param data A matrix or data frame containing circumplex scales.
#' @param scales A vector of column numbers or names for the circumplex scales.
#' @param angles A vector of angles, in degrees, of the circumplex scales.
#' @param bs_number The number of bootstrap resamples (default = 2000).
#' @return A tibble (data frame) containing estimates and bootstrapped 95\%
#'   confidence intervals for the mean profile's structural summary parameters:
#'   elevation, x-value, y-value, amplitude, displacement, and model fit.
#' @examples 
#' # Enter scales using column numbers in a continuous range
#' ssm_profile(wright2009, 1:8, octants)
#' 
#' # Enter scales using column numbers in a discontinuous set
#' ssm_profile(wright2009, c(1,3,5,7), poles)
#' 
#' # Enter scales using column names in a continuous range
#' ssm_profile(wright2009, PA:NO, octants)
#' 
#' # Enter scales using column names in a discontinuous set
#' ssm_profile(wright2009, c(BC, FG, JK, NO), quadrants)
#' 
#' # Enter angles using a vector of numbers
#' ssm_profile(wright2009, c(BC, FG, JK, NO), c(90, 180, 270, 360))
#' 
#' # Change the number of bootstrap resamples
#' ssm_profile(wright2009, 1:8, octants, 3000)

ssm_profile <- function(data, scales, angles, bs_number = 2000) {
  scales <- rlang::enquo(scales)
  # Get estimates for SSM parameters
  data_use <- data %>% dplyr::select(!!scales)
  scores <- data_use %>% colMeans()
  #scores <- colMeans(select(data, !!scales))
  ssm <- ssm_parameters(scores, angles, FALSE)
  # Perform bootstrapping on SSM parameters
  bs_function <- function(data, index, angles) {
    resample <- data[index, ]
    scores_r <- colMeans(resample)
    ssm_r <- ssm_parameters(scores_r, angles, FALSE)
    return(ssm_r)
  }
  bs_results <- boot::boot(
    data = data_use,
    statistic = bs_function, 
    R = bs_number,
    angles = angles
  )
  # Prepare bootstrap results to calculate quantiles
  bs_t <- tibble::as_tibble(bs_results$t)
  bs_t <- dplyr::mutate(bs_t, V5 = make_circular(V5))
  # Create output including 95\% confidence intervals
  results <- tibble::tibble(
    parameter = c("Elevation", "X-Value", "Y-Value", 
      "Amplitude", "Displacement", "Model Fit"),
    estimate = ssm,
    lower_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .025),
    upper_ci = purrr::map_dbl(bs_t, smart_quantile, probs = .975)
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
