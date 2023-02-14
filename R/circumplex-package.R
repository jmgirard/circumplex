#' Analysis and visualization of circumplex data
#'
#' circumplex provides functions for analyzing and visualizing circumplex data.
#'
#' Its goal is to unify, modernize, and extend existing methods of working with
#' circumplex data. Its functions share an underlying design philosophy and
#' grammar. The three guiding principles it aspires to are:
#'
#' \itemize{
#' \item \strong{Accessibility:} zero cost, open source, libre; works on many
#'   platforms; easy to use across skill levels
#' \item \strong{Flexibility:} customizable by the user; extendable for other uses;
#'   plays nicely with other packages
#' \item \strong{Consistency:} unit-tested quality control; firm naming conventions;
#'   data flows between functions
#' }
#'
#' To learn more about circumplex, start with the vignettes:
#' `browseVignettes(package = "circumplex")`
#'
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib circumplex, .registration = TRUE
## usethis namespace: end
#' @importFrom assertthat assert_that is.flag is.number is.count is.string
#' @importFrom stats cor quantile sd var
#' @keywords internal
"_PACKAGE"
