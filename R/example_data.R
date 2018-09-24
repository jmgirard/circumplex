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
#' @source \url{https://doi.org/10.1080/00223890902935696}
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
#' @source \url{https://doi.org/10.1177/1073191115621795}
"jz2017"

#' Instrument: CSIE
#'
#' Information about the Circumplex Scales of Interpersonal Efficacy.
#'
#' @source \url{https://www.webpages.uidaho.edu/klocke/csie.htm}
#' @examples
#' data(csie)
#' summary(csie)
"csie"

#' Instrument: CSIG
#'
#' Information about the Circumplex Scales of Intergroup Goals.
#'
#' @source \url{https://www.webpages.uidaho.edu/klocke/csig.htm}
#' @examples
#' data(csig)
#' summary(csig)
"csig"

#' Instrument: CSIP
#'
#' Information about the Circumplex Scales of Interpersonal Problems.
#'
#' @source \url{https://doi.org/10.1037/pas0000505}
#' @examples
#' data(csip)
#' summary(csip)
"csip"

#' Instrument: CSIV
#'
#' Information about the Circumplex Scales of Interpersonal Values.
#'
#' @source \url{https://www.webpages.uidaho.edu/klocke/csiv.htm}
#' @examples
#' data(csiv)
#' summary(csiv)
"csiv"

#' Instrument: IIP-32
#'
#' Information about the Inventory of Interpersonal Problems Brief Version.
#' Note that, although we have permission to provide some information about the
#' IIP-32, Mind Garden Inc. has exclusive rights to distribute it in full.
#'
#' @source \url{https://www.mindgarden.com/113-inventory-of-interpersonal-problems}
#' @examples
#' data(iip32)
#' summary(iip32)
"iip32"

#' Instrument: IIP-64
#'
#' Information about the Inventory of Interpersonal Problems. Note that,
#' although we have permission to provide some information about the IIP-64,
#' Mind Garden Inc. has exclusive rights to distribute it in full.
#'
#' @source \url{https://www.mindgarden.com/113-inventory-of-interpersonal-problems}
#' @examples
#' data(iip64)
#' summary(iip64)
"iip64"

#' Instrument: IIP-SC
#'
#' Information about the Inventory of Interpersonal Problems Short Circumplex.
#'
#' @source \url{https://doi.org/10.1080/00223890802388665}
#' @examples
#' data(iipsc)
#' summary(iipsc)
"iipsc"

#' Instrument: IIS-64
#'
#' Information about the Inventory of Interpersonal Strengths.
#'
#' @source \url{https://doi.org/10.1037/a0017269}
#' @examples
#' data(iis64)
#' summary(iis64)
"iis64"

#' Instrument: IPIP-IPC
#'
#' Information about the IPIP Interpersonal Circumplex.
#'
#' @source \url{https://doi.org/10.1177/1073191109340382}
#' @examples
#' data(ipipipc)
#' summary(ipipipc)
"ipipipc"

#' Instrument: ISC
#'
#' Information about the Interpersonal Sensitivities Circumplex.
#'
#' @source \url{https://doi.org/10.1111/j.1467-6494.2011.00696.x}
#' @examples
#' data(isc)
#' summary(isc)
"isc"
