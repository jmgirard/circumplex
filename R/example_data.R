#' Standardized octant scores on hypothetical circumplex scales
#'
#' A small example dataset containing standardized scores on eight hypothetical
#' circumplex scales. Taken from Wright, Pincus, Conroy, & Hilsenroth (2009).
#' 
#' @format A data frame with 5 rows and 8 variables:
#' \describe{
#'   \item{PA}{circumplex scale at displacement 90}
#'   \item{BC}{circumplex scale at displacement 135}
#'   \item{DE}{circumplex scale at displacement 180}
#'   \item{FG}{circumplex scale at displacement 225}
#'   \item{HI}{circumplex scale at displacement 270}
#'   \item{JK}{circumplex scale at displacement 315}
#'   \item{LM}{circumplex scale at displacement 360}
#'   \item{NO}{circumplex scale at displacement 45}
#' }
#' @source \url{http://doi.org/10.1080/00223890902935696}
"wright2009"

#' Angular displacements for octant circumplex scales
#'
#' A vector of angular displacements, in degrees, for the most common ordering
#' of eight equally spaced (octant) circumplex scales. Can be passed to the
#' angles parameter of other functions in this package for convenience.
#'
#' @format A vector with 8 elements: PA, BC, DE, FG, HI, JK, LM, NO
#'   
"octants"

#' Angular displacements for quadrant circumplex scales
#'
#' A vector of angular displacements, in degrees, for the most common ordering
#' of four blended (quadrant) circumplex scales. Can be passed to the angles
#' parameter of other functions in this package for convenience.
#'
#' @format A vector with 4 elements: BC, FG, JK, NO
#'   
"quadrants"

#' Angular displacements for pole circumplex scales
#'
#' A vector of angular displacements, in degrees, for the most common ordering
#' of four pure (pole) circumplex scales. Can be passed to the angles parameter
#' of other functions in this package for convenience.
#'
#' @format A vector with 4 elements: PA, DE, HI, LM
#'   
"poles"
