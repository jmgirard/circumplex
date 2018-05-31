# Class degree ------------------------------------------------------------


#' Set numeric object to class 'degree'
new_degree <- function(x) {
  sloop::new_s3_dbl(x, class = "degree")
}

#' S3 generic for class 'degree'
as_degree <- function(x) {
  UseMethod("as_degree")
}

#' Set numeric object to class 'degree'
as_degree.default <- function(x) {
  new_degree(x)
}

#' Return object if already class 'degree'
as_degree.degree <- function(x) {
  x
}

#' Convert from class 'radian' to class 'degree'
as_degree.radian <- function(x) {
  new_degree(x * (180 / pi))
}


# Class radian ------------------------------------------------------------


#' Set numeric object to class 'radian'
new_radian <- function(x) {
  sloop::new_s3_dbl(x, class = "radian")
}

#' S3 generic for class 'radian'
as_radian <- function(x) {
  UseMethod("as_radian")
}

#' Set numeric object to class 'radian'
as_radian.default <- function(x) {
  new_radian(x)
}

#' Return object if already class 'radian'
as_radian.radian <- function(x) {
  x
}

#' Convert from class 'degree' to class 'radian'
as_radian.degree <- function(x) {
  new_radian(x * (pi / 180))
}


# Class ssm ---------------------------------------------------------------


ssm <- function(x, y, ...) {
  UseMethod("ssm")
}

print.ssm <- function(x) {
  message("success")
}

new_ssm <- function(x, ...) {
  sloop::new_s3_scalar(x, ..., class = "ssm")
}

as_ssm.ssm <- function(x, ...) {
  x
}