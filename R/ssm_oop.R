# S3 vector constructors -------------------------------------------------------

# Create a new S3 class from a numeric
new_s3_num <- function(x, ..., class) {
  stopifnot(is.numeric(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

# Create a new S3 class from a list
new_s3_lst <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

# Create a new S3 class from a scalar
new_s3_scalar <- function(..., class) {
  new_s3_lst(list(...), class = class)
}

# Class degree -----------------------------------------------------------------

# S3 Constructor
new_degree <- function(x) {
  new_s3_num(x, class = c("circumplex_degree", "numeric"))
}

# S3 Generic
as_degree <- function(x, ...) {
  UseMethod("as_degree")
}

# S3 Method
#' @method as_degree default
#' @export
as_degree.default <- function(x, ...) {
  new_degree(x)
}

# S3 Method
#' @method as_degree circumplex_degree
#' @export
as_degree.circumplex_degree <- function(x, ...) {
  x
}

# S3 Method
#' @method as_degree circumplex_radian
#' @export
as_degree.circumplex_radian <- function(x, ...) {
  new_degree(x * (180 / pi))
}

# Class radian -----------------------------------------------------------------

# S3 Constructor
new_radian <- function(x) {
  new_s3_num(x, class = c("circumplex_radian", "numeric"))
}

# S3 Generic
as_radian <- function(x, ...) {
  UseMethod("as_radian")
}

# S3 Method
#' @method as_radian default
#' @export
as_radian.default <- function(x, ...) {
  new_radian(x)
}

# S3 Method 
#' @method as_radian circumplex_radian
#' @export
as_radian.circumplex_radian <- function(x, ...) {
  x
}

# S3 Method
#' @method as_radian circumplex_degree
#' @export
as_radian.circumplex_degree <- function(x, ...) {
  new_radian(x * (pi / 180))
}

# S3 Method
#' @method print circumplex_degree
#' @export
print.circumplex_degree <- function(x, digits = 3, ...) {
  cat(round(x, digits = digits), "\nDegrees\n")
}

# S3 Method
#' @method print circumplex_radian
#' @export
print.circumplex_radian <- function(x, digits = 3, ...) {
  cat(round(x, digits = digits), "\nRadians\n")
}

# Class ssm --------------------------------------------------------------------

# S3 Constructor
new_ssm <- function(results, details, call, ...) {
  new_s3_scalar(
    results = results,
    details = details,
    call = call,
    ...,
    class = "circumplex_ssm"
  )
}

#  Print method for objects of ssm class
#' @method print circumplex_ssm
#' @export
print.circumplex_ssm <- function(x, digits = 3, ...) {
  # Print each result as a block
  for (i in 1:nrow(x$results)) {
    dat <- x$results[i, ]
    v <- c(
      dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit_est,
      dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
      dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA
    )
    m <- round(matrix(v, nrow = 6, ncol = 3), digits)
    prefix <- ifelse(
      test = x$details$contrast && i == nrow(x$results),
      yes = "\u0394 ",
      no = ""
    )
    rownames(m) <- paste0(prefix, c(
      "Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit"
    ))
    colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
    results_type <- ifelse(
      test = x$details$contrast && i == nrow(x$results), 
      yes = "Contrast", 
      no = "Profile"
    )
    cat("\n# ", results_type, " [", dat$Label, "]:\n\n",
      sep = ""
    )
    print.default(m, print.gap = 3L, na.print = "")

    # Interpretation guardrails (profiles only; a contrast's fit/amplitude are
    # differences, not prototypicality measures, so these notes do not apply)
    is_contrast_row <- x$details$contrast && i == nrow(x$results)
    if (!is_contrast_row) {
      if (is.na(dat$fit_est) || dat$fit_est < 0.70) {
        cat(
          "  Note: model fit is inadequate (R\u00b2 < .70); ",
          "interpret only the elevation parameter.\n",
          sep = ""
        )
      }
      if (is.na(dat$a_lci) || round(dat$a_lci, digits) <= 0) {
        cat(
          "  Note: the amplitude CI includes zero; ",
          "the displacement is not interpretable.\n",
          sep = ""
        )
      }
    }
    cat("\n")
  }
}

# Summary method for objects of ssm class
#' @method summary circumplex_ssm
#' @export
summary.circumplex_ssm <- function(object, digits = 3, ...) {
  # Print analysis details
  cat(
    "\nStatistical Basis:\t", object$details$score_type, "Scores",
    "\nBootstrap Resamples:\t", object$details$boots,
    "\nConfidence Level:\t", object$details$interval,
    "\nListwise Deletion:\t", object$details$listwise,
    "\nScale Displacements:\t", as.numeric(object$details$angles),
    "\n\n"
  )
  print(object)
}
