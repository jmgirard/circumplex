# SUPERSEDED (2026-07-07, M4.5): do not port statistics or thresholds from
# this draft. The shared infrastructure and the Fisher/Gap/VT/RT statistics
# now live in R/fit_structure.R, implemented against the adjudicated readings
# in devel/ar2004-transcription.md; this file retains every bug catalogued in
# devel/fit-drafts-method-review.md (ridge applied to the data matrix, no
# wrap-around gap, sign*acos angles, the x[0] indexing no-op, quarter-period
# rotation grids, thresholds attached to the wrong Fisher scale and to the
# wrong nv). It stays only as the reference for T6 (fit_randall's
# correspondence-index machinery) and as the historical draft record.

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
    "\n",
    sep = ""
  )
  cat("\nSample Size:\t", object$details$n)
  if (object$details$ridge != 0) {
    cat("\nRidge Constant:\t", object$details$ridge)
  }
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
  } else if (object$details$type == "vt") {
    cat("\n\nCriteria:\n")
    criteria <- round(object$criteria, digits)
    print.default(criteria, print.gap = 3L)
  } else if (object$details$type == "rt") {
    cat("\n\nCriteria:\n")
    criteria <- round(object$criteria, digits)
    print.default(criteria, print.gap = 3L)
  }
  cat("\nTest Statistic:\n")
  cat(round(object$stat, digits))
}
