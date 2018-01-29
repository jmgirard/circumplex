#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' rwd
#' 
#' @param wd Description
#' @return Description

rwd <- function(wd) {
  ((wd + 180) %% 360) - 180
}

#' Compute difference between two sets of SSM parameters
#'
#' @param p1,p2 Outputs from \code{ssm_parameters()}.
#' @return A numeric vector of differences between the parameters in \code{p1}
#'   and \code{p2}, respecting the circular nature of displacement parameters.
param_diff <- function(p1, p2) {
  assert_that(is.numeric(p1), is.numeric(p2))
  pd <- p1 - p2
  pd[[5]] <- angle_dist(p1[[5]], p2[[5]])
  pd
}

#' Coerce a variable to the circular data type
#'
#' @param x A vector of numbers.
#' @return The same vector as \code{x} but with the circular data type (in
#'   degree units and counter-clockwise rotation) for the circular package.

as_angle <- function(x) {
  assert_that(is.numeric(x))
  class(x) <- "angle"
  x
}

#' ggrad
#' 
#' @param v Description
#' @return Description

ggrad <- function(v) {
  (v - 90) * (-pi / 180)
}

#' pretty_max
#' 
#' @param v Description
#' @return Description

pretty_max <- function(v) {
    amax <- max(v, na.rm = TRUE)
  options <- c(
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50, 
    2.00, 2.50, 3.00, 4.00, 5.00)
  match <- options > amax
  if (sum(match) > 1) {
    options[match][[2]]
  } else if (sum(match) == 1) {
    options[match][[1]]
  } else {
    ceiling(amax * 1.50)
  }
}
