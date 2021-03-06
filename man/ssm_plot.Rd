% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssm_visualization.R
\name{ssm_plot}
\alias{ssm_plot}
\title{Create a figure from SSM results}
\usage{
ssm_plot(.ssm_object, fontsize = 12, ...)
}
\arguments{
\item{.ssm_object}{Required. The results output of \code{\link{ssm_analyze}}.}

\item{fontsize}{Optional. A single positive number indicating the font size
of text in the figure, in points (default = 12).}

\item{...}{Additional arguments to pass on to the plotting function.}
}
\value{
A ggplot2 object representing the figure
}
\description{
Take in the results of an SSM analysis function and create figure from it.
}
\examples{
\donttest{
# Load example data
data("jz2017")

# Plot profile results
res <- ssm_analyze(jz2017,
  scales = PA:NO, angles = octants(),
  measures = c(NARPD, ASPD)
)
p <- ssm_plot(res)

# Plot contrast results
res <- ssm_analyze(jz2017,
  scales = PA:NO, angles = octants(),
  measures = c(NARPD, ASPD), contrast = "test"
)
p <- ssm_plot(res)
}

}
\seealso{
ggsave Function for saving plots to image files.

Other ssm functions: 
\code{\link{ssm_analyze}()},
\code{\link{ssm_append}()},
\code{\link{ssm_parameters}()},
\code{\link{ssm_score}()},
\code{\link{ssm_table}()}
}
\concept{ssm functions}
\concept{visualization functions}
