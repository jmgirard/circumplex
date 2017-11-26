#' Standardized octant scores on hypothetical circumplex scales
#'
#' A small example dataset containing standardized scores on eight hypothetical
#' circumplex scales. Taken from Wright, Pincus, Conroy, & Hilsenroth (2009).
#' 
#' @format A data frame with 5 observations and 8 variables:
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

#' Standardized octant scores on IIP-C with covariates
#'
#' A large example dataset containing standardized scores on the Inventory of
#' Interpersonal Problems Circumplex Scales and categorical and continuous
#' covariates. Taken from Girard et al. (2017).
#'
#' @format A data frame with 825 observations and 10 variables:
#' \describe{
#'   \item{ZPA}{Domineering (standardized score)}
#'   \item{ZBC}{Vindictive (standardized score)}
#'   \item{ZDE}{Cold (standardized score)}
#'   \item{ZFG}{Socially Avoidant (standardized score)}
#'   \item{ZHI}{Nonassertive (standardized score)}
#'   \item{ZJK}{Easily Exploited (standardized score)}
#'   \item{ZLM}{Overly Nurturant (standardized score)}
#'   \item{ZNO}{Intrusive (standardized score)}
#'   \item{bordl}{Borderline Personality Disorder (0 = FALSE, 1 = TRUE)}
#'   \item{detach}{Detachment Factor Score}
#' }
#' @source \url{http://doi.org/10.1016/j.comppsych.2017.06.014}
"girard2017"

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
