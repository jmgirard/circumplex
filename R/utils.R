#' Calculate difference between angular displacements around circle
#' 
#' @param w1,w2 Angular displacements (numeric).
#' @return Difference between \code{w1} and \code{w2} in circular space.

wd <- function(w1, w2) {
  w <- ((w2 - w1 + 180) %% 360) - 180
  return(w)
}

#' Calculate correct quantiles for circular and non-circular data
#'
#' @param .data A vector or data frame containing numeric data.
#' @param probs A vector of probabilities to get quantiles for.
#' @return A vector of quantiles corresponding to the probabilities in
#'   \code{probs}; these quantiles will be circular if the data are circular and
#'   they wiull be normal quantiles otherwise.

smart_quantile <- function(.data, probs){
  if (circular::is.circular(.data)) {
    q <- circular::quantile.circular(.data, probs = probs) %% 360
  } else {
    q <- stats::quantile(.data, probs = probs)
  }
  return(q)
}

#' Compute difference between two sets of SSM parameters
#'
#' @param p1,p2 Outputs from \code{ssm_parameters()}.
#' @return A numerical vector or tibble (data frame) with \code{p2 - p1} for
#'   each parameter, while accounting for the fact that differences in
#'   displacement need to be handled specially.

param_diff <- function(p1, p2) {
  pd <- p2 - p1
  pd[5] <- wd(p1[5], p2[5])
  return(pd)
}

#' Coerce a variable to the circular data type
#'
#' @param x A vector of numbers.
#' @return The same vector as \code{x} but with the circular data type (in
#'   degree units and counter-clockwise rotation) for the circular package.

make_circular <- function(x) {
  y <- circular::circular(x, units = "degrees", rotation = "counter")
  return(y)
}

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
#' @return A tibble containing SSM parameters (point and interval estimates).

ssm_bootstrap <- function(.data, bs_function, ssm, angles, boots, interval) {

  # Perform bootstrapping ---------------------------------------------------
  bs_results <- boot::boot(
    data = .data,
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
  
  results
}
