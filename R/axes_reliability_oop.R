# S3 class and constructor for circumplex axes-reliability results (Strack,
# Jacobs & Grosse Holtforth, 2013). The constructor mirrors new_structure()
# (R/fit_structure_oop.R): a scalar S3 list with named components and a class
# tag. axes_reliability() (R/axes_reliability.R) is the only caller.
# print()/summary() methods are added by T10.

new_axes_reliability <- function(results, components, fit, details, call) {
  stopifnot(is.data.frame(results))
  stopifnot(is.data.frame(components))
  stopifnot(is.list(fit))
  stopifnot(is.list(details))
  new_s3_lst(
    list(
      results = results,
      components = components,
      fit = fit,
      details = details,
      call = call
    ),
    class = "circumplex_axes_reliability"
  )
}

is_axes_reliability <- function(x) {
  is.list(x) && inherits(x, "circumplex_axes_reliability")
}

# ---- shared formatting ------------------------------------------------------

# Round-or-dash: format a numeric vector to `digits`, showing NA as "--".
axes_fmt <- function(x, digits = 3) {
  out <- formatC(round(x, digits), format = "f", digits = digits)
  out[is.na(x)] <- "--"
  out
}

# The standard-error caveat shared by print()/summary() and the roxygen: the
# model is fit to the item correlation matrix as if it were a covariance matrix
# (the paper's own practice), so the point estimates are correct but the
# standard errors and the global chi-square are approximate (Cudeck, 1989).
axes_se_caveat <- paste0(
  "  Note: the model is fit to the item correlation matrix, so the point\n",
  "  estimates are exact but the standard errors and global fit are\n",
  "  approximate (Cudeck, 1989)."
)

# ---- methods ----------------------------------------------------------------

#' Print circumplex axes-reliability results
#'
#' Compact display of an [axes_reliability()] object: the per-axis reliability,
#' SEm, and Nunnally-Bernstein comparison, with the correlation-as-covariance
#' standard-error caveat.
#'
#' @param x A `circumplex_axes_reliability` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `x`, invisibly.
#' @method print circumplex_axes_reliability
#' @export
print.circumplex_axes_reliability <- function(x, digits = 3, ...) {
  d <- x$details
  cat(
    "\nCircumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)",
    "\nItems:        ", d$n_items, " (", d$n_scales, " scales)",
    "\nComplete N:   ", d$n,
    "\nSEm scale:    ", if (is.character(d$sd)) d$sd else "custom",
    "\n\n# Per-axis reliability\n\n",
    sep = ""
  )
  disp <- data.frame(
    Axis = x$results$Axis,
    item_n = x$results$item_n,
    Reliability = axes_fmt(x$results$reliability, digits),
    SEm = axes_fmt(x$results$sem, digits),
    NB_Reliability = axes_fmt(x$results$nb_reliability, digits),
    stringsAsFactors = FALSE
  )
  print(disp, row.names = FALSE, right = FALSE)
  if (isTRUE(d$boundary)) {
    cat(
      "\n  Note: a boundary solution (non-positive axes variance) was reached;",
      "\n  reliability and SEm are NA rather than a clipped value.\n",
      sep = ""
    )
  }
  cat("\n", axes_se_caveat, "\n", sep = "")
  invisible(x)
}

#' Summarize circumplex axes-reliability results
#'
#' Fuller display of an [axes_reliability()] object: everything [print()] shows
#' plus the estimated variance components (with standard errors) and the global
#' fit indices.
#'
#' @param object A `circumplex_axes_reliability` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `object`, invisibly.
#' @method summary circumplex_axes_reliability
#' @export
summary.circumplex_axes_reliability <- function(object, digits = 3, ...) {
  x <- object
  print(x, digits = digits)
  cat("\n# Variance components\n\n")
  comp <- data.frame(
    Component = x$components$Component,
    Estimate = axes_fmt(x$components$Estimate, digits),
    SE = axes_fmt(x$components$SE, digits),
    stringsAsFactors = FALSE
  )
  print(comp, row.names = FALSE, right = FALSE)

  cat("\n# Global fit\n\n")
  cat(
    "  chi-square(", axes_fmt(x$fit$df, 0), ") = ", axes_fmt(x$fit$chisq, 2),
    ",  RMSEA = ", axes_fmt(x$fit$rmsea, 3),
    ",  CFI = ", axes_fmt(x$fit$cfi, 3),
    "\n", sep = ""
  )
  invisible(x)
}
