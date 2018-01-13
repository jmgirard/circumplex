#' Calculate structural summary parameters
#'
#' @param scores A numeric vector of scores on multiple circumplex scales: can
#'   be either mean scores or correlations.
#' @param angles A numeric vector containing an angular displacement for each
#'   circumplex scale provided in \code{scores} (in degrees).
#' @return A numerical vector containing structural summary method parameters
#'   that describe \code{scores} given \code{angles}. The vector will contain
#'   the following values: elevation, x-axis value, y-axis value, amplitude,
#'   angular displacement (in degrees), and model fit (R-squared).

ssm_parameters <- function(scores, angles) {

  # Check that inputs are valid ---------------------------------------------
  assert_that(not_empty(scores), not_empty(angles))
  assert_that(is.numeric(scores), is.numeric(angles))
  assert_that(are_equal(length(scores), length(angles)))

  # Calculate SSM parameters ------------------------------------------------
  nScales <- length(scores)
  elev <- mean(scores)
  xval <- as.numeric((2 / nScales) * (scores %*% cos(angles * (pi / 180))))
  yval <- as.numeric((2 / nScales) * (scores %*% sin(angles * (pi / 180))))
  ampl <- sqrt(xval ^ 2 + yval ^ 2)
  disp <- (atan2(yval, xval) * (180 / pi)) %% 360
  gfit <- 1 - ((sum((elev + ampl * cos((angles - disp) * (pi / 180)) - 
      scores) ^ 2)) / (stats::var(scores) * (nScales - 1)))
  c(elev, xval, yval, ampl, disp, gfit)
  
}
