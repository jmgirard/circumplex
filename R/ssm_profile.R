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
  # Get estimates for SSM parameters
  scores <- colMeans(select(data, scales))
  ssm <- ssm_parameters(scores, angles)
  # Perform bootstrapping on SSM parameters
  bs_function <- function(data, index, angles) {
    resample <- data[index, ]
    scores_rs <- colMeans(resample)
    ssm_rs <- ssm_parameters(scores_rs, angles)
    return(ssm_rs)
  }
  bs_results <- boot(
    data = select(data, scales),
    statistic = bs_function, 
    R = bs_number,
    angles = angles
  )
  # Prepare bootstrap results to calculate quantiles
  bs_t <- as_tibble(bs_results$t)
  bs_t <- mutate(bs_t, V5 = circular(V5, units = "degrees", rotation = "counter"))
  smart_quantile <- function(data, probs){
    if (is.circular(data)) {
      q <- circular::quantile.circular(data, probs = probs) %% 360
    } else {
      q <- quantile(data, probs = probs)
    }
    return(q)
  }
  # Create output including 95\% confidence intervals
  results <- tibble(
    parameter = c("Elevation", "X-Value", "Y-Value", 
      "Amplitude", "Displacement", "Model Fit"),
    estimate = ssm,
    lower_ci = map_dbl(bs_t, smart_quantile, probs = .025),
    upper_ci = map_dbl(bs_t, smart_quantile, probs = .975)
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
#' @param group The column name in \code{data} that specifies each observation's
#'   group. Be sure to enter the name as is (i.e., not as a string or number).
#'   This variable can be a factor with two levels or a numeric or character
#'   variable with two unique values.
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

ssm_profile2 <- function(data, groups, scales, angles, bs_number = 2000) {
  # Split the data by group
  groups <- enquo(groups)
  scales <- enquo(scales)
  data_use <- data %>% select(!!groups, !!scales) %>% mutate(group = factor(!!groups)) %>% select(-!!groups)
  if (nlevels(data_use$group) != 2) {
    return("Error: The number of unique groups must equal 2.")
  }
  data_g1 <- data_use %>% filter(group == levels(group)[[1]]) %>% select(-group) 
  data_g2 <- data_use %>% filter(group == levels(group)[[2]]) %>% select(-group)
  # Get estimates for SSM parameters
  scores_g1 <- colMeans(data_g1)
  scores_g2 <- colMeans(data_g2)
  ssm_g1 <- ssm_parameters(scores_g1, angles)
  ssm_g2 <- ssm_parameters(scores_g2, angles)
  ssm_gd <- ssm_g1 - ssm_g2
  ssm_gd$d <- wd(ssm_g1, ssm_g2)
  # Perform bootstrapping on SSM parameters
  bs_function <- function(data, index, angles) {
    #TODO: Need to change this to filter by group and then computer differences
    resample <- data[index, ]
    scores_rs <- colMeans(resample)
    ssm_rs <- ssm_parameters(scores_rs, angles)
    return(ssm_rs)
  }
  bs_results <- boot(
    data = select(data, scales),
    statistic = bs_function, 
    R = bs_number,
    angles = angles
  )
  # Prepare bootstrap results to calculate quantiles
  bs_t <- as_tibble(bs_results$t)
  bs_t <- mutate(bs_t, V5 = circular(V5, units = "degrees", rotation = "counter"))
  smart_quantile <- function(data, probs){
    if (is.circular(data)) {
      q <- circular::quantile.circular(data, probs = probs) %% 360
    } else {
      q <- quantile(data, probs = probs)
    }
    return(q)
  }
  # Create output including 95\% confidence intervals
  results <- tibble(
    parameter = c("Elevation", "X-Value", "Y-Value", 
      "Amplitude", "Displacement", "Model Fit"),
    estimate = ssm,
    lower_ci = map_dbl(bs_t, smart_quantile, probs = .025),
    upper_ci = map_dbl(bs_t, smart_quantile, probs = .975)
  )
  return(results)
}
