% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssm_analysis.R
\name{ssm_score}
\alias{ssm_score}
\title{Calculate SSM parameters by row and add results as new columns}
\usage{
ssm_score(data, scales, angles = octants(), append = TRUE, ...)
}
\arguments{
\item{data}{Required. A data frame containing at least circumplex scales.}

\item{scales}{Required. The variable names or column numbers for the
variables in \code{.data} that contain circumplex scales to be analyzed.}

\item{angles}{Required. A numeric vector containing the angular displacement
of each circumplex scale included in \code{scales} (in degrees).}

\item{append}{Optional. A logical indicating whether to append the output to
\code{data} or simply return the output (default = "TRUE").}

\item{...}{Optional. Additional parameters to pass to
\code{\link{ssm_parameters}()}, such as \code{prefix} and \code{suffix}.}
}
\value{
A data frame containing \code{.data} plus six additional columns
containing the SSM parameters (calculated rowwise).
}
\description{
Calculate the SSM parameters for each row of a data frame and add the results
as additional columns. This can be useful when the SSM is being used for the
description or visualization of individual data points rather than for
statistical inference on groups of data points.
}
\examples{
data("aw2009")
ssm_score(
  aw2009,
  scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
)

}
\seealso{
Other ssm functions: 
\code{\link{ssm_analyze}()},
\code{\link{ssm_parameters}()},
\code{\link{ssm_table}()}

Other analysis functions: 
\code{\link{ssm_analyze}()},
\code{\link{ssm_parameters}()}
}
\concept{analysis functions}
\concept{ssm functions}
