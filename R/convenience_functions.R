#' Angular displacements for octant circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for eight equally
#' spaced circumplex scales corresponding to the circumplex octants. Can be
#' passed to the \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with eight elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   PA, BC, DE, FG, HI, JK, LM, NO.
#' @export
#' @usage octants()
#' @examples
#' octants()
octants <- function() {
  as_degree(c(90, 135, 180, 225, 270, 315, 360, 45))
}

#' Angular displacements for pole circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for four equally spaced
#' circumplex scales corresponding to the circumplex poles. Can be passed to the
#' \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with four elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   PA, DE, HI, LM.
#' @export
#' @usage poles()
#' @examples
#' poles()
poles <- function() {
  as_degree(c(90, 180, 270, 360))
}

#' Angular displacements for quadrant circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for four equally spaced
#' circumplex scales corresponding to the circumplex quadrants. Can be passed to
#' the \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with eight elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   BC, FG, JK, NO.
#' @export
#' @usage quadrants()
#' @examples
#' quadrants()
quadrants <- function() {
  as_degree(c(135, 225, 315, 45))
}
