# S3 vector constructors -------------------------------------------------------

new_s3_dbl <- function(x, ..., class) {
  stopifnot(is.double(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

new_s3_lst <- function(x, ..., class) {
  stopifnot(is.list(x))
  stopifnot(is.character(class))
  structure(x, ..., class = class)
}

new_s3_scalar <- function(..., class) {
  new_s3_lst(list(...), class = class)
}

# Class degree -----------------------------------------------------------------


# Set numeric object to class 'degree'
new_degree <- function(x) {
  new_s3_dbl(x, class = "degree")
}

# S3 generic for class 'degree'
as_degree <- function(x) {
  UseMethod("as_degree")
}

# Set numeric object to class 'degree'
as_degree.default <- function(x) {
  new_degree(x)
}

# Return object if already class 'degree'
as_degree.degree <- function(x) {
  x
}

# Convert from class 'radian' to class 'degree'
as_degree.radian <- function(x) {
  new_degree(x * (180 / pi))
}


# Class radian -----------------------------------------------------------------


# Set numeric object to class 'radian'
new_radian <- function(x) {
  new_s3_dbl(x, class = "radian")
}

# S3 generic for class 'radian'
as_radian <- function(x) {
  UseMethod("as_radian")
}

# Set numeric object to class 'radian'
as_radian.default <- function(x) {
  new_radian(x)
}

# Return object if already class 'radian'
as_radian.radian <- function(x) {
  x
}

# Convert from class 'degree' to class 'radian'
as_radian.degree <- function(x) {
  new_radian(x * (pi / 180))
}


# Class ssm --------------------------------------------------------------------

#
ssm <- function(x, ...) {
  UseMethod("ssm")
}

# Constructor function
new_ssm <- function(results, contrasts, details, call, ...) {
  new_s3_scalar(
    results = results,
    contrasts = contrasts,
    details = details,
    call = call,
    ...,
    class = "ssm")
}

#' @export
print.ssm <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n", sep = "")
  for (i in 1:nrow(x$results)) {
    dat <- x$results[i, ]
    v <- c(dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit,
           dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
           dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA)
    m <- round(matrix(v, nrow = 6, ncol = 3), 3)
    rownames(m) <- c("Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit")
    colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
    cat("\n", x$type, " [", dat$label, "]:\n", sep = "")
    print.default(m, print.gap = 3L, na.print = "")
  }
  if (is.null(x$contrasts)) {
    cat("\n")
    return()
  }
  for (i in 1:nrow(x$contrasts)) {
    dat <- x$contrasts[i, ]
    v <- c(dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit,
      dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
      dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA)
    m <- round(matrix(v, nrow = 6, ncol = 3), 3)
    rownames(m) <- c("Elevation", "X-Value", "Y-Value",
      "Amplitude", "Displacement", "Model Fit")
    colnames(m) <- c("Estimate", "Lower CI", "Upper CI")
    cat("\nContrast [", dat$label, "]:\n", sep = "")
    print.default(m, print.gap = 3L, na.print = "")
  }
  cat("\n")
}
