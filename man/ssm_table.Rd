% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssm_table.R
\name{ssm_table}
\alias{ssm_table}
\title{Create HTML table from SSM results or contrasts}
\usage{
ssm_table(ssm_object, caption = NULL, drop_xy = FALSE, render = TRUE)
}
\arguments{
\item{ssm_object}{Required. The results output of \code{ssm_analyze()}.}

\item{caption}{A string to be displayed above the table (default = NULL).}

\item{drop_xy}{A logical indicating whether the x-value and y-value parameters
should be omitted from the output (default = FALSE).}

\item{render}{A logical indicating whether the table should be displayed in
the RStudio viewer or web browser (default = TRUE).}
}
\value{
A data frame containing the information for the HTML table. As a
side-effect, may also output the HTML table to the web viewer.
}
\description{
Take in the results of an SSM analysis and return an HTML table with the
desired formatting.
}
\examples{
\donttest{
# Load example data
data("jz2017")

# Create table of profile results
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD")
)
ssm_table(res)

# Create table of contrast results
res <- ssm_analyze(
  jz2017,
  scales = 2:9,
  measures = c("NARPD", "ASPD"), 
  contrast = TRUE
)
ssm_table(res)
}

}
\seealso{
Other ssm functions: 
\code{\link{ssm_analyze}()},
\code{\link{ssm_parameters}()},
\code{\link{ssm_score}()}

Other table functions: 
\code{\link{html_render}()}
}
\concept{ssm functions}
\concept{table functions}
