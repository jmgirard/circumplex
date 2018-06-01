#' Pipe operator
#'
#' See \code{magrittr} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Compute differences between two sets of SSM parameters
#'
#' @param p1,p2 Outputs from \code{ssm_parameters()}.
#' @return A numeric vector of differences between the parameters in \code{p1}
#'   and \code{p2}, respecting the circular nature of displacement parameters.
param_diff <- function(p1, p2) {
  assert_that(is.numeric(p1), is.numeric(p2))
  pd <- p1 - p2
  pd[[5]] <- angle_dist(as_radian(p1[[5]]), as_radian(p2[[5]]))
  pd
}

#' Calculate angular distance
#'
#' @param x,y Angles in radian units.
#' @return The shortest distance between \code{x} and \code{y} in radians, where 
#'   positive numbers mean \code{x} is more counterclockwise than \code{y} and
#'   negative numbers mean the opposite. Note that, given how the distance is
#'   calculated, when \code{x} and \code{y} are exactly pi radians apart, the
#'   distance output will be arbitrarily negative.
angle_dist <- function(x, y)  {
  ((x - y + pi) %% (2 * pi)) - pi
}

#' Convert degrees to ggplot's radian format
ggrad <- function(v) {
  (v - 90) * (-pi / 180)
}

#' Determine good max amplitude value for circle plot
pretty_max <- function(v) {
  amax <- max(v, na.rm = TRUE)
  options <- c(
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50, 
    2.00, 2.50, 3.00, 4.00, 5.00)
  match <- options > amax
  if (sum(match) > 1) {
    out <- options[match][[2]]
  } else if (sum(match) == 1) {
    out <- options[match][[1]]
  } else {
    out <- ceiling(amax * 1.50)
  }
  out
}
