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

#' Normative data for CSIE
#' 
#' Means and standard deviations for the octant scales of the Circumplex Scales
#' of Interpersonal Efficacy (CSIE). Norms correspond to data from 367 American
#' college students. Note that these norms assume that mean scale scores are
#' used rather than sum scale scores. Scores range from 0 to 10.
#' 
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://www.webpages.uidaho.edu/klocke/csie.htm}
"csie"

#' Normative data for CSIP
#' 
#' Means and standard deviations for the octant scales of the Circumplex Scales
#' of Interpersonal Problems (CSIP). Norms correspond to data from 712 American
#' college students. Note that these norms assume that mean scale scores are
#' used rather than sum scale scores. Scores range from 0 to 3.
#' 
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://doi.org/10.1037/pas0000505}
"csip"

#' Normative data for CSIV
#' 
#' Means and standard deviations for the octant scales of the Circumplex Scales
#' of Interpersonal Values (CSIV). Norms correspond to data from 1200 American
#' college students. Note that these norms assume that mean scale scores are
#' used rather than sum scale scores. Scores range from 0 to 4.
#' 
#' @format A data frame with 8 observations and 5 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://www.webpages.uidaho.edu/klocke/csiv.htm}
"csiv"

#' Normative data for IIP-SC
#'
#' Means and standard deviations for the octant scales of the Inventory of
#' Interpersonal Problems - Short Circumplex (IIP-SC). Norms correspond to data
#' from 872 American college students. Note that these norms assume that mean
#' scale scores are used rather than sum scale scores. Scores range from 0 to 4.
#'
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://doi.org/10.1080/00223890802388665}
"iipsc"

#' Normative data for IIS-64
#'
#' Means and standard deviations for the octant scales of the Inventory of
#' Interpersonal Strengths, 64-item version (IIS-64). Norms correspond to data
#' from 684 American college students. Note that these norms assume that mean
#' scale scores are used rather than sum scale scores. Scores range from 1 to 6.
#'
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://doi.org/10.1037/a0017269}
"iis64"

#' Normative data for IPIP-IPC
#' 
#' Means and standard deviations for the octant scales of the IPIP Interpersonal
#' Circumplex (IPIP-IPC). Norms correspond to data from 274 American college
#' students. Note that these norms assume that mean scale scores are
#' used rather than sum scale scores. Scores range from 1 to 5.
#' 
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://doi.org/10.1177/1073191109340382}
"ipipipc"

#' Normative data for ISC
#' 
#' Means and standard deviations for the octant scales of the Interpersonal
#' Sensitivities Circumplex (ISC). Norms correspond to data from 649 American
#' college students. Note that these norms assume that mean scale scores are
#' used rather than sum scale scores. Scores range from 1 to 8.
#' 
#' @format A data frame with 8 observations and 6 variables:
#' \describe{
#'   \item{Scale}{Text description (name) of the scale}
#'   \item{Abbrev}{Text abbreviation of the scale name}
#'   \item{Angle}{Angle of the scale (in degrees)}
#'   \item{M}{Normative mean for the scale}
#'   \item{SD}{Normative standard deviation for the scale}
#'   \item{Items}{A comma-separated list of item numbers}
#' }
#' @source \url{https://doi.org/10.1111/j.1467-6494.2011.00696.x}
"isc"
