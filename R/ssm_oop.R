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

# S3 Generic. Deliberately internal: the generic is NOT exported (only its
# methods are S3-registered), so `as_degree`/`as_radian` are boundary-conversion
# helpers, not public API. Keep-internal was chosen over promoting them to a
# documented converter API (M13, D-006 follow-up): minimal-API doctrine, no
# API-maintenance commitment, reversible. Reopen only if a public deg<->rad
# converter is genuinely wanted.
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

# S3 Constructor for the contrast variant (a signed radian difference whose
# circular quantiles are allowed to stay negative; see
# quantile.circumplex_contrast_radian). Single-sources the class tag applied
# to contrast displacement columns in ssm_bootstrap.R and ssm_ci_accuracy.R.
new_contrast_radian <- function(x) {
  new_s3_num(x, class = c("circumplex_contrast_radian", "numeric"))
}

# S3 Generic. Deliberately internal (see as_degree above): generic unexported,
# methods registered; boundary-conversion helper, not public API (M13).
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

# The displacement-interpretability guardrail: a profile's displacement is
# certified as interpretable when the amplitude CI's lower bound sits at least
# k = 0.35 CI-widths above zero -- r = a_lci / (a_uci - a_lci) >= k. Scale-free
# (numerator and denominator carry the same scale factor, so the verdict is
# invariant to the score metric) and print-independent (no display rounding).
# k is a pinned constant calibrated to the 95% default interval: approximately
# the 97.5% point of r's asymptotically-pivotal zero-amplitude (Rayleigh) null,
# giving false-certification ~ alpha/2 where the superseded
# round(a_lci, digits) > 0 rule sat at 1.000 (D-007, RR03;
# spec devel/m4-ci-accuracy-spec.md sec. 3.4/12.5). Equivalent to
# a_lci >= (k / (1 + k)) * a_uci = 0.259 * a_uci; do not "simplify" into that
# form thinking it a different rule. THE single definition of the rule --
# print.circumplex_ssm() applies it and ssm_ci_accuracy() measures its
# operating characteristics; both move together. Pure function of the amplitude
# CI pair (a_est is never consulted); an NA lower bound and degenerate
# zero-width CIs fail closed via the is.finite() guard. Vectorized. Contrast
# rows are never certification-gated (M15-D1).
ssm_certified <- function(a_lci, a_uci, k = 0.35) {
  ratio <- a_lci / (a_uci - a_lci)
  is.finite(ratio) & ratio >= k
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
      if (!ssm_certified(dat$a_lci, dat$a_uci)) {
        cat(
          "  Note: the amplitude CI lower bound is under 0.35 CI-widths ",
          "above zero; the displacement is not interpretable.\n",
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
  # Print analysis details (objects predating the method option are bootstrap)
  replicate_label <- if (identical(object$details$method, "montecarlo")) {
    "\nMonte Carlo Draws:\t"
  } else {
    "\nBootstrap Resamples:\t"
  }
  cat(
    "\nStatistical Basis:\t", object$details$score_type, "Scores",
    replicate_label, object$details$boots,
    "\nConfidence Level:\t", object$details$interval,
    "\nListwise Deletion:\t", object$details$listwise,
    "\nScale Displacements:\t", as.numeric(object$details$angles),
    # Occasions metadata (conditional; occasions analyses only). The inline
    # `if` yields NULL otherwise, which cat() drops without a separator, so
    # non-occasions output stays byte-identical.
    if (!is.null(object$details$occasions)) {
      c("\nOccasions:\t\t", object$details$occasions)
    },
    "\n\n"
  )
  print(object)
}
