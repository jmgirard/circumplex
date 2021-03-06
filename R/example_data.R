#' Standardized octant scores on hypothetical circumplex scales
#'
#' A small example dataset containing standardized scores on eight hypothetical
#' circumplex scales. Taken from Wright, Pincus, Conroy, & Hilsenroth (2009).
#'
#' @format A data frame with 5 observations and 8 variables:
#' \describe{
#'   \item{PA}{circumplex scale at 90 degrees}
#'   \item{BC}{circumplex scale at 135 degrees}
#'   \item{DE}{circumplex scale at 180 degrees}
#'   \item{FG}{circumplex scale at 225 degrees}
#'   \item{HI}{circumplex scale at 270 degrees}
#'   \item{JK}{circumplex scale at 315 degrees}
#'   \item{LM}{circumplex scale at 360 degrees}
#'   \item{NO}{circumplex scale at 45 degrees}
#' }
#' @source \doi{10.1080/00223890902935696}
"aw2009"

#' Raw octant scores on real circumplex scales with covariates
#'
#' A large example dataset containing gender, raw mean scores on the Inventory
#' of Interpersonal Problems - Short Circumplex (IIP-SC), and raw sum scores on
#' the Personality Diagnostic Questionnaire - 4th Edition Plus (PDQ-4+).
#'
#' @format A data frame with 1166 observations and 19 variables:
#' \describe{
#'   \item{Gender}{Self-reported Gender}
#'   \item{PA}{Domineering Problems (IIP-SC) 90 degrees}
#'   \item{BC}{Vindictive Problems (IIP-SC) 135 degrees}
#'   \item{DE}{Cold Problems (IIP-SC) 180 degrees}
#'   \item{FG}{Socially Avoidant Problems (IIP-SC) 225 degrees}
#'   \item{HI}{Nonassertive Problems(IIP-SC) 270 degrees}
#'   \item{JK}{Easily Exploited Problems (IIP-SC) 315 degrees}
#'   \item{LM}{Overly Nurturant Problems (IIP-SC) 360 degrees}
#'   \item{NO}{Intrusive Problems (IIP-SC) 45 degrees}
#'   \item{PARPD}{Paranoid PD Symptoms (PDQ-4+)}
#'   \item{SCZPD}{Schizoid PD Symptoms (PDQ-4+)}
#'   \item{SZTPD}{Schizotypal PD Symptoms (PDQ-4+)}
#'   \item{ASPD}{Antisocial PD Symptoms (PDQ-4+)}
#'   \item{BORPD}{Borderline PD Symptoms (PDQ-4+)}
#'   \item{HISPD}{Histrionic PD Symptoms (PDQ-4+)}
#'   \item{NARPD}{Narcissistic PD Symptoms (PDQ-4+)}
#'   \item{AVPD}{Avoidant PD Symptoms (PDQ-4+)}
#'   \item{DPNPD}{Dependent PD Symptoms (PDQ-4+)}
#'   \item{OCPD}{Obsessive-Compulsive PD Symptoms (PDQ-4+)}
#' }
#' @source \doi{10.1177/1073191115621795}
"jz2017"

#' Raw item responses on real circumplex scales
#'
#' A small example dataset containing raw item responses on the Inventory of
#' Interpersonal Problems, Short Circumplex (IIP-SC). This data set is useful
#' for testing functions that operate on item-level data.
#'
#' @format A data frame with 10 observations and 32 variables.
"raw_iipsc"
