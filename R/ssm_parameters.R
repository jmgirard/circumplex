#' Calculate structural summary parameters
#'
#' @param scores A vector of scores on circumplex scales.
#' @param angles A vector of angles, in degrees, of the circumplex scales.
#' @param tibble A logical determining the type of returned variable. If TRUE,
#'   the returned variable will be a tibble (data frame). If FALSE, the returned
#'   variable will be an unnamed vector. Because the latter is much faster than
#'   the former, use FALSE during bootstrapping and other iterative processes.
#'   (default = TRUE)
#' @return Depending on \code{tibble}, with a tibble (if TRUE) or an unnamed
#'   vector (if FALSE) with structural summary method parameters describing
#'   \code{scores} given \code{angles}. The vector will contain the following
#'   values: e (elevation), x (x-axis value), y (y-axis value), a (amplitude), d
#'   (angular displacement, in degrees), and fit (R-squared).
#' @examples
#' ssm_parameters(c(0.37, -0.57, -0.52, 0.02, 0.69, 1.42, 1.58, 0.68),
#'   c(90, 135, 180, 225, 270, 315, 360, 45))
#' ssm_parameters(c(0.37, -0.57, -0.52, 0.02, 0.69, 1.42, 1.58, 0.68), octants)
#' ssm_parameters(c(0.37, -0.52, 0.69, 1.58), poles)

ssm_parameters <- function(scores, angles, tibble = TRUE) {
  stopifnot(length(scores) == length(angles))
  stopifnot(tibble == TRUE || tibble == FALSE || tibble == 1 || tibble == 0)
  nScales <- length(scores)
  elev <- mean(scores)
  xval <- as.numeric((2 / nScales) * (scores %*% cos(angles * (pi / 180))))
  yval <- as.numeric((2 / nScales) * (scores %*% sin(angles * (pi / 180))))
  ampl <- sqrt(xval ^ 2 + yval ^ 2)
  disp <- (atan2(yval, xval) * (180 / pi)) %% 360
  gfit <- 1 - ((sum((elev + ampl * cos((angles - disp) * (pi / 180)) - 
      scores) ^ 2)) / (var(scores) * (nScales - 1)))
  if (tibble == TRUE) {
    ssm <- tibble::tibble(
      e = elev,
      x = xval,
      y = yval,
      a = ampl,
      d = disp,
      fit = gfit
    )
  } else if (tibble == FALSE) {
    ssm <- c(elev, xval, yval, ampl, disp, gfit)
  }
  return(ssm)
}
