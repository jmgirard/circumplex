# Class fit --------------------------------------------------------------------

# Constructor function
new_fit <- function(stat, details, call, ...) {
  new_s3_scalar(
    stat = stat,
    details = details,
    call = call,
    ...,
    class = "fit"
  )
}

#  Print method for objects of fit class
#' @export
print.fit <- function(x, digits = 3, ...) {
  print.default(round(x$stat, digits))
}

#  Summary method for objects of gap class
#' @export
summary.fit <- function(object, digits = 3, ...) {
  # Print function call
  cat("Call:\n",
    paste(deparse(object$call), sep = "\n", collapse = "\n"),
    "\n", sep = "")
  cat("\nSample Size:\t", object$details$n)
  cat("\nRidge Constant:\t", object$details$ridge)
  cat("\nFactor Method:\t", object$details$fm)
  if (object$details$type == "gap") {
    cat("\n\nAngles:\n")
    angles <- round(object$angles, digits)
    print.default(angles, print.gap = 3L)
    cat("\nGaps:\n")
    gaps <- round(object$gaps, digits)
    print.default(gaps, print.gap = 3L)
  } else if (object$details$type == "fisher") {
    cat("\n\nRadii:\n")
    radius <- round(object$radius, digits)
    print.default(radius, print.gap = 3L)
  }
  cat("\nTest Statistic:\n")
  cat(round(object$stat, digits))
  
}