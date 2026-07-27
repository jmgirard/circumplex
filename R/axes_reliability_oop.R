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
# The SE half of this caveat is GONE as of M66: the component standard errors
# are now corrected for the correlation-as-covariance metric (RR13 BC1, D-035),
# so the text no longer warns about them. What survives is the global-fit half,
# which the correction does not touch -- the chi-square carries the same
# approximation in the OTHER direction and keeps its sentence verbatim.
#
# Why the SE warning could not simply have been sharpened instead: the ratio it
# described runs [0.81, 1.97] across the accepted input space and dips below 1
# for weak-axes/strong-general instruments, so it is sign-unstable and no static
# sentence states it honestly (RR13 section 2). That is what overturned
# "document, don't fix".
axes_se_caveat <- paste0(
  "  Note: the model is fit to the item correlation matrix as if it were a\n",
  "  covariance matrix, so the global fit statistics are approximate\n",
  "  (Cudeck, 1989). Global fit is flattered by roughly 4%. The component\n",
  "  standard errors are corrected for this and are calibrated; they are\n",
  "  typically smaller than the values printed by Strack et al. (2013),\n",
  "  whose LISREL standard errors carry the uncorrected approximation."
)

# The extra sentence the FIML path owes on top of the caveat above. Its SEs
# carry an approximation the correction does NOT remove: they are computed on
# the standardized metric holding the standardization constants fixed, so they
# do not propagate the uncertainty in the FIML means and SDs that produced that
# metric. M66 corrected the correlation-as-covariance metric error on this path
# as on every other -- composed multiplicatively, so the observed-information
# pricing of the missing information survives -- and RR13 section 4 measured
# what is left over afterwards at 0.1 / 0.8 / 1.8% at 2 / 5 / 10% MCAR, an
# order of magnitude below the effect that was corrected. Unchanged at M66
# (its wording never mentioned the metric error), and stated where the SEs are
# shown rather than only in the help page, because that is where a reader is
# about to use them.
axes_fiml_se_caveat <- paste0(
  "  Note: under missing = \"fiml\" the standard errors are\n",
  "  observed-information SEs on the standardized metric, conditional on the\n",
  "  FIML standardization constants; FIML assumes the data are missing at\n",
  "  random and multivariate normal."
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
  from_cormat <- identical(d$input, "cormat")
  # identical() on the read-back field, so an object built before M65 (no
  # `missing` field at all) reads as FALSE rather than erroring.
  is_fiml <- identical(d$missing, "fiml")
  cat(
    "\nCircumplex Axes Reliability (Strack, Jacobs & Grosse Holtforth, 2013)",
    "\nInput:        ", if (from_cormat) "correlation matrix" else "item data",
    "\nItems:        ", d$n_items, " (", d$n_scales, " scales)",
    # Three different N's, three different labels, because they are three
    # different quantities and a shared label would hide that. "Complete N"
    # names the listwise complete-case count; "Sample N" the size a supplied
    # correlation matrix was computed from; "Total N" every respondent in the
    # data, with the complete-case count beside it so the reader can see how
    # much of it listwise would have discarded (M65).
    #
    # "Total N" prints d$n_total, NOT d$n. The two differ whenever a row with no
    # observed item at all was dropped, and printing N_used under a label
    # reading "Total" named a number that was not the total -- caught at review
    # against AC8, which requires the total N. Where they differ the used count
    # is shown beside it rather than replaced by it, because N_used is what the
    # fit consumed and the reader needs both; where they agree it is left out,
    # so the common case gains no noise.
    if (from_cormat) {
      "\nSample N:     "
    } else if (is_fiml) {
      "\nTotal N:      "
    } else {
      "\nComplete N:   "
    },
    if (is_fiml) d$n_total else d$n,
    if (is_fiml) {
      paste0(
        " (",
        if (!identical(d$n_total, d$n)) paste0(d$n, " used, "),
        d$n_complete, " complete)"
      )
    },
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
      # Names the CLASS of solution, not one disjunct: the flag is raised by an
      # axes variance outside (0, 1) OR any negative variance component, and
      # naming only the first told a user the wrong cause for the others
      # (M62 review, F2). The components table below already shows which.
      "\n  Note: a boundary solution was reached (an axes variance outside",
      "\n  (0, 1), or a negative variance component); reliability and SEm are",
      "\n  NA rather than a clipped value. See the components table above.\n",
      sep = ""
    )
  } else if (isTRUE(all.equal(
    x$results$reliability[[1]], x$results$reliability[[2]]
  ))) {
    # The two axes share one (equal-constrained) axes-variance estimate, so a
    # balanced instrument's rows match by construction (RR09) -- flag it so a
    # user does not read the identical rows as a bug.
    cat(
      "\n  Note: the two axes share one axes-variance estimate and, with equal",
      "\n  items per axis, carry the same reliability -- expected, not an error.\n",
      sep = ""
    )
  }
  # RR09 sec. 7.4 and M61-D1: NA-with-reason, never silently dropped. `d$nb_reason`
  # is NULL when the comparison is available. The `from_cormat` fallback keeps the
  # note on objects built before M61 added the field.
  # `nb_reason` carries EVERY reason that applies, so both notes print when a
  # correlation matrix also has single-item scales -- membership, not identity.
  nb_reason <- d$nb_reason
  if (is.null(nb_reason) && from_cormat) nb_reason <- "cormat"
  if ("cormat" %in% nb_reason) {
    cat(
      "\n  Note: the Nunnally-Bernstein comparison needs the raw item scores",
      "\n  (scale alphas and the axis-composite variance), so it is NA on the",
      "\n  correlation-matrix path.\n",
      sep = ""
    )
  }
  if ("fiml" %in% nb_reason) {
    cat(
      "\n  Note: the Nunnally-Bernstein comparison needs each scale's alpha and",
      "\n  the axis-composite variance, both of which need items observed by",
      "\n  every respondent, so it is NA under missing = \"fiml\" rather than",
      "\n  computed from whatever cells happened to be answered.\n",
      sep = ""
    )
  }
  if ("single_item" %in% nb_reason) {
    cat(
      "\n  Note: the Nunnally-Bernstein comparison needs each scale's alpha,",
      "\n  which is undefined for a scale carrying only one item, so it is NA",
      "\n  here. Strack et al. (2013) likewise leave it blank for such",
      "\n  instruments.\n",
      sep = ""
    )
  }
  cat("\n", axes_se_caveat, "\n", sep = "")
  if (is_fiml) cat("\n", axes_fiml_se_caveat, "\n", sep = "")
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
