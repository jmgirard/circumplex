% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidying_functions.R
\name{score}
\alias{score}
\title{Score circumplex scales from item responses}
\usage{
score(
  data,
  items,
  instrument,
  na.rm = TRUE,
  prefix = "",
  suffix = "",
  append = TRUE
)
}
\arguments{
\item{data}{Required. A data frame containing at least circumplex scales.}

\item{items}{Required. The variable names or column numbers for the variables
in \code{.data} that contain all the circumplex items from a single
circumplex measure, in ascending order from item 1 to item N.}

\item{instrument}{Required. An instrument object from the package. To see the
available circumplex instruments, use \code{instruments()}.}

\item{na.rm}{Optional. A logical that determines if missing values should be
omitted from the calculation of scores (default = TRUE). When set to TRUE,
scales with missing data are essentially calculated with mean imputation.}

\item{prefix}{Optional. A string to include at the beginning of the newly
calculated scale variables' names, before \code{Abbrev} from \code{key} and
\code{suffix} (default = "").}

\item{suffix}{Optional. A string to include at the end of the newly
calculated scale variables' names, after \code{Abbrev} from \code{key} and
\code{prefix} (default = "").}

\item{append}{Optional. A logical that determines whether the calculated
score variables will be appended to \code{data} or returned on their own
(default = TRUE).}
}
\value{
A data frame that matches \code{.data} except that new variables are
appended that contain mean scores on each variable included in \code{key}.
}
\description{
Calculate mean scores on circumplex scales from item responses by using a set
of scoring instructions, which may be loaded from the package or created as a
custom data frame.
}
\examples{
data("raw_iipsc")
score(raw_iipsc, items = 1:32, instrument = iipsc, prefix = "IIPSC_")
}
\seealso{
Other tidying functions: 
\code{\link{ipsatize}()},
\code{\link{norm_standardize}()},
\code{\link{self_standardize}()}
}
\concept{tidying functions}
