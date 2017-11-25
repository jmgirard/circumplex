#' Calculate structural summary parameters
#'
#' @param scores A vector of scores on circumplex scales.
#' @param angles A vector of angles, in degrees, of the circumplex scales.
#' @return A named vector of structural summary method parameters describing
#'   \code{scores} given \code{angles}. The vector will contain the following
#'   values: e (elevation), x (x-axis value), y (y-axis value), 
#'   a (amplitude), d (angular displacement, in degrees), and fit (R-squared).
#' @examples
#' ssm_parameters(c(0.37, -0.57, -0.52, 0.02, 0.69, 1.42, 1.58, 0.68),
#'   c(90, 135, 180, 225, 270, 315, 360, 45))
#' ssm_parameters(c(0.37, -0.57, -0.52, 0.02, 0.69, 1.42, 1.58, 0.68), octants)
#' ssm_parameters(c(0.37, -0.52, 0.69, 1.58), poles)

ssm_parameters <- function(scores, angles) {
  k <- length(scores)
  elev <- mean(scores)
  xval <- as.numeric((2 / k) * (scores %*% cos(angles * (pi / 180))))
  yval <- as.numeric((2 / k) * (scores %*% sin(angles * (pi / 180))))
  ampl <- sqrt(xval ^ 2 + yval ^ 2)
  disp <- (atan2(yval, xval) * (180 / pi)) %% 360
  gfit <- 1 - ((sum((elev + ampl * cos((angles - disp) * (pi / 180)) - 
      scores) ^ 2)) / (var(scores) * (k - 1)))
  ssm <- c(e = elev, x = xval, y = yval, a = ampl, d = disp, fit = gfit)
  return(ssm)
}
