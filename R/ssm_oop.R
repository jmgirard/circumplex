# S3 vector constructors -------------------------------------------------------

# Create a new S3 class from a double
new_s3_dbl <- function(x, ..., class) {
  stopifnot(is.double(x))
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

# Set numeric object to class 'degree'
new_degree <- function(x) {
  new_s3_dbl(x, class = c("degree", "numeric"))
}

# S3 generic for class 'degree'
as_degree <- function(x, ...) {
  UseMethod("as_degree")
}

# Set numeric object to class 'degree'
as_degree.default <- function(x, ...) {
  new_degree(x)
}

# Return object if already class 'degree'
as_degree.degree <- function(x, ...) {
  x
}

# Convert from class 'radian' to class 'degree'
as_degree.radian <- function(x, ...) {
  new_degree(x * (180 / pi))
}

# Class radian -----------------------------------------------------------------

# Set numeric object to class 'radian'
new_radian <- function(x) {
  new_s3_dbl(x, class = c("radian", "numeric"))
}

# S3 generic for class 'radian'
as_radian <- function(x, ...) {
  UseMethod("as_radian")
}

# Set numeric object to class 'radian'
as_radian.default <- function(x, ...) {
  new_radian(x)
}

# Return object if already class 'radian'
as_radian.radian <- function(x, ...) {
  x
}

# Convert from class 'degree' to class 'radian'
as_radian.degree <- function(x, ...) {
  new_radian(x * (pi / 180))
}

# Class ssm --------------------------------------------------------------------

# Constructor function
new_ssm <- function(results, details, call, ...) {
  new_s3_scalar(
    results = results,
    details = details,
    call = call,
    ...,
    class = "ssm"
  )
}

#  Print method for objects of ssm class
#' @export
print.ssm <- function(x, digits = 3, ...) {
  # Print function call
  cat("Call:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n",
    sep = ""
  )
  # Print each result as a block
  for (i in 1:nrow(x$results)) {
    dat <- x$results[i, ]
    v <- c(
      dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit,
      dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
      dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA
    )
    m <- round(matrix(v, nrow = 6, ncol = 3), digits)
    # TODO: Add delta symbols if parameter contrast
    rownames(m) <- c(
      "Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit"
    )
    colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
    cat("\n", x$details$results_type, " [", dat$label, "]:\n",
      sep = ""
    )
    print.default(m, print.gap = 3L, na.print = "")
  }
  cat("\n")
}

# Summary method for objects of ssm class
#' @export
summary.ssm <- function(object, digits = 3, ...) {
  # Print function call
  cat("Call:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n",
    sep = ""
  )
  # Print analysis details
  cat(
    "\nStatistical Basis:\t", object$details$score_type, "Scores",
    "\nBootstrap Resamples:\t", object$details$boots,
    "\nConfidence Level:\t", object$details$interval,
    "\nListwise Deletion:\t", object$details$listwise,
    "\nScale Displacements:\t", as.numeric(object$details$angles),
    "\n"
  )
  # Print each result as a block
  for (i in 1:nrow(object$results)) {
    dat <- object$results[i, ]
    v <- c(
      dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit,
      dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
      dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA
    )
    m <- round(matrix(v, nrow = 6, ncol = 3), digits)
    # TODO: Add delta symbols if parameter contrast
    rownames(m) <- c(
      "Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit"
    )
    colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
    cat("\n", object$details$results_type, " [", dat$label, "]:\n",
      sep = ""
    )
    print.default(m, print.gap = 3L, na.print = "")
  }
  cat("\n")
}
