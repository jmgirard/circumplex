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

# The metric note shared by print()/summary() and the roxygen: the model is fit
# to the item correlation matrix as if it were a covariance matrix (the paper's
# own practice), so the point estimates are correct but normal-theory maximum
# likelihood misprices everything downstream of the sampling variability
# (Cudeck, 1989).
#
# This text used to WARN. Both halves of the warning are now gone, in two
# milestones: M66 corrected the component standard errors (RR13 BC1, D-035),
# and M68 scaled the global test statistic (satorra1994 p. 407, D-036). What is
# left is not a caveat but an orientation note -- it says what was corrected,
# because the reported numbers differ from the ones a reader reproduces in
# LISREL or by calling lavaan directly, and an unexplained difference reads as
# a bug.
#
# Why neither warning could simply have been sharpened instead: the SE ratio
# runs [0.81, 1.97] across the accepted input space and dips below 1 for
# weak-axes/strong-general instruments (RR13 section 2), and the fit distortion
# is likewise a per-fit quantity that the retired "roughly 4%" reported as a
# constant. A caveat that quantifies one population is not a caveat that can be
# stated honestly; that is what overturned "document, don't fix" on both sides.
# Split into three pieces because the two corrections can fail INDEPENDENTLY,
# and a note that asserts a correction which did not happen is worse than no
# note. print() below prints the shared opening beside whichever halves are
# live; a failed half gets its own NA-with-reason note instead.
axes_metric_note <- paste0(
  "  Note: the model is fit to the item correlation matrix as if it were a\n",
  "  covariance matrix (Cudeck, 1989), and both sides of that mismatch are\n",
  "  corrected -- so these numbers differ from LISREL's, and from lavaan's\n",
  "  own, by design."
)
axes_se_corrected_note <- paste0(
  "  The component standard errors are adjusted to the correlation metric\n",
  "  and are calibrated; they are typically smaller than the values printed\n",
  "  by Strack et al. (2013), whose LISREL output carries no correction."
)
axes_fit_scaled_note <- paste0(
  "  The global fit statistics chisq, pvalue, rmsea and cfi are scaled to\n",
  "  that metric (Satorra & Bentler, 1994), which removes a distortion that\n",
  "  flatters fit; df and srmr are unchanged."
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
  # The correction-failure state gets its own note, like every other
  # NA-with-reason state above it (boundary, cormat, fiml, single_item) --
  # RR09 sec. 7.4's NA-with-reason doctrine, which the new state was otherwise
  # the only one to skip. Without it the SE column reads as all-NA with the
  # explanation reachable only from `details`, and the call-time warning() is
  # routinely muffled (this package's own harnesses wrap the call in
  # suppressWarnings()).
  #
  # It also SUPPRESSES the corrected-SE half of the caveat rather than printing
  # it beside the gap: that text asserts the standard errors "are corrected ...
  # and are calibrated", which is a claim about numbers that are not there. The
  # surviving half is still printed so nothing true is lost with it (M66 review,
  # F3) -- and M68 makes that half conditional in turn, because the scaling can
  # fail on its own. The two failures usually arrive together (both routes solve
  # the same sigma-hat) but need not, and neither note may speak for the other.
  se_failed <- x$details$se_correction_failed
  fit_failed <- x$details$fit_scaling_failed
  live <- c(
    if (is.null(se_failed)) axes_se_corrected_note,
    if (is.null(fit_failed)) axes_fit_scaled_note
  )
  if (!is.null(se_failed)) {
    cat(
      "\n  Note: the component standard errors could not be computed (",
      se_failed, ") and are\n  NA. The point estimates, reliability, and SEm ",
      "are unaffected.\n",
      sep = ""
    )
  }
  if (!is.null(fit_failed)) {
    cat(
      "\n  Note: the global fit statistics could not be scaled to the ",
      "correlation\n  metric (", fit_failed, "), so chisq, pvalue, rmsea and ",
      "cfi are NA. What\n  lavaan reported unscaled is in ",
      "details$fit_uncorrected; df and srmr are\n  unaffected.\n",
      sep = ""
    )
  }
  if (length(live) > 0) {
    cat("\n", paste(c(axes_metric_note, live), collapse = "\n"), "\n", sep = "")
    if (is.null(se_failed) && is_fiml) {
      cat("\n", axes_fiml_se_caveat, "\n", sep = "")
    }
  }
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
