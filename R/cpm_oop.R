# S3 class and methods for the circular process model fit (design sec. 5.4).
# The constructor mirrors new_ssm() (R/ssm_oop.R): a scalar S3 list with named
# components and a class tag. cpm_fit() (R/cpm_fit.R) is the only caller.

# ---- constructor ------------------------------------------------------------

new_cpm <- function(results, betas, fit, corfun, matrices, details, call) {
  stopifnot(is.data.frame(results))
  stopifnot(is.data.frame(betas))
  stopifnot(is.list(fit))
  stopifnot(is.function(corfun))
  stopifnot(is.list(matrices))
  stopifnot(is.list(details))
  new_s3_lst(
    list(
      results = results,
      betas = betas,
      fit = fit,
      corfun = corfun,
      matrices = matrices,
      details = details,
      call = call
    ),
    class = "circumplex_cpm"
  )
}

# ---- shared formatting helpers ----------------------------------------------

# One-line fit summary: chi-square(df), RMSEA [90% CI], SRMR, CFI (design sec. 5.4).
cpm_fit_line <- function(fit, digits = 3) {
  if (is.na(fit$df) || fit$df < 1) {
    return(paste0(
      "Fit: saturated model (df = ", fit$df, "); fit indices undefined.\n"
    ))
  }
  paste0(
    "Fit: \u03c7\u00b2(", fit$df, ") = ", round(fit$chisq, digits),
    ", p = ", format.pval(fit$pvalue, digits = digits, eps = 1e-4),
    "; RMSEA = ", round(fit$rmsea, digits),
    " [", round(fit$rmsea_ci[1], digits), ", ",
    round(fit$rmsea_ci[2], digits), "]",
    "; SRMR = ", round(fit$srmr, digits),
    "; CFI = ", round(fit$cfi, digits), "\n"
  )
}

# Diagnostic/boundary lines, gathered so print() and summary() agree (design
# sec. 2.5 / sec. 3.5). Returns a character vector (possibly empty).
cpm_diagnostic_lines <- function(details) {
  msg <- character(0)
  if (!isTRUE(details$accepted)) {
    msg <- c(msg, paste0(
      "  Note: the fit did not meet the convergence acceptance criterion ",
      "(gradient norm ", format(details$gradient_norm, digits = 2),
      "); interpret with caution.\n"
    ))
  }
  if (isTRUE(details$heywood)) {
    msg <- c(msg, paste0(
      "  Note: a communality index reached its upper boundary ",
      "(\u03b6 > 0.995, a Heywood-type solution).\n"
    ))
  }
  if (length(details$removed_harmonics) > 0) {
    msg <- c(msg, paste0(
      "  Note: harmonic(s) ",
      paste(details$removed_harmonics, collapse = ", "),
      " were on the zero boundary and removed (df adjusted).\n"
    ))
  }
  if (isTRUE(details$multimodal)) {
    msg <- c(msg, paste0(
      "  Note: competing near-tied optima were found; the solution may be ",
      "weakly identified.\n"
    ))
  }
  # Bootstrap replicate accounting (design sec. 5.2): surface exclusions.
  if (identical(details$ci_method, "bootstrap") &&
      isTRUE(details$boots_used < details$boots)) {
    n_bad <- details$boots - details$boots_used
    msg <- c(msg, paste0(
      "  Note: ", n_bad, " of ", details$boots, " bootstrap resamples were ",
      "excluded (", details$boots_degenerate, " degenerate, ",
      details$boots_nonconvergent, " non-convergent); the intervals are ",
      "based on ", details$boots_used, " replicates and are conditional on ",
      "estimability.\n"
    ))
  }
  msg
}

# Round the numeric columns of the results/betas data frames for display.
cpm_round_df <- function(df, digits) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits = digits)
  df
}

# ---- print ------------------------------------------------------------------

#' Print a circular process model fit
#'
#' Compact display of a [cpm_fit()] object: the estimated angles and communality
#' indices with confidence intervals, a one-line fit summary, and any
#' boundary/convergence notes.
#'
#' @param x A `circumplex_cpm` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `x`, invisibly.
#' @method print circumplex_cpm
#' @export
print.circumplex_cpm <- function(x, digits = 3, ...) {
  d <- x$details
  cat(
    "\nCircular Process Model (Browne, 1992)",
    "\nModel:            ", d$model,
    "\nHarmonics (m):    ", d$m,
    "\nSample size (N):  ", d$N,
    "\nReference scale:  ", d$scales[[d$reference]],
    "\n\n"
  )
  print(cpm_round_df(x$results, digits), row.names = FALSE)
  cat("\n", cpm_fit_line(x$fit, digits), sep = "")
  for (line in cpm_diagnostic_lines(d)) cat(line)
  invisible(x)
}

# ---- summary ----------------------------------------------------------------

#' Summarize a circular process model fit
#'
#' Fuller display of a [cpm_fit()] object: adds the correlation-function weights,
#' the full set of fit indices, a residual summary (the largest absolute
#' residual and the pair it belongs to), and all boundary/identification
#' diagnostics in plain language. When the confidence intervals are analytic and
#' the sample size is modest, prints a caution that Wald intervals may materially
#' mis-cover (see [cpm_fit()]).
#'
#' @param object A `circumplex_cpm` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `object`, invisibly.
#' @method summary circumplex_cpm
#' @export
summary.circumplex_cpm <- function(object, digits = 3, ...) {
  d <- object$details
  fit <- object$fit
  cat(
    "\nCircular Process Model (Browne, 1992)",
    "\nModel:            ", d$model,
    "\nHarmonics (m):    ", d$m,
    "\nSample size (N):  ", d$N,
    "\nReference scale:  ", d$scales[[d$reference]],
    "\nCI method:        ", d$ci_method,
    "\nConfidence level: ", d$interval,
    "\n\n# Estimated angles and communality indices\n\n"
  )
  print(cpm_round_df(object$results, digits), row.names = FALSE)

  cat("\n# Correlation-function weights\n\n")
  print(cpm_round_df(object$betas, digits), row.names = FALSE)

  cat("\n# Fit indices\n\n")
  if (is.na(fit$df) || fit$df < 1) {
    cat("  Saturated model (df =", fit$df, "); fit indices undefined.\n")
  } else {
    cat(
      "  \u03c7\u00b2(", fit$df, ") = ", round(fit$chisq, digits),
      ", p = ", format.pval(fit$pvalue, digits = digits, eps = 1e-4), "\n",
      "  RMSEA = ", round(fit$rmsea, digits),
      " [", round(fit$rmsea_ci[1], digits), ", ",
      round(fit$rmsea_ci[2], digits), "] (90% CI)\n",
      "  SRMR  = ", round(fit$srmr, digits), "\n",
      "  CFI   = ", round(fit$cfi, digits),
      "    TLI = ", round(fit$tli, digits), "\n",
      "  AIC   = ", round(fit$aic, digits),
      "    BIC = ", round(fit$bic, digits), "\n",
      sep = ""
    )
  }

  # Residual summary: largest |off-diagonal residual| and its pair.
  resid <- object$matrices$residuals
  ut <- upper.tri(resid)
  rvals <- resid[ut]
  idx <- which(ut, arr.ind = TRUE)
  worst <- which.max(abs(rvals))
  i <- idx[worst, 1]
  j <- idx[worst, 2]
  cat(
    "\n# Residuals\n\n",
    "  Largest absolute residual: ", round(abs(rvals[worst]), digits),
    " (", d$scales[[i]], " \u2013 ", d$scales[[j]], ")\n",
    sep = ""
  )

  diag_lines <- cpm_diagnostic_lines(d)
  if (length(diag_lines) > 0) {
    cat("\n# Diagnostics\n\n")
    for (line in diag_lines) cat(line)
  }

  # N-conditional analytic-CI caution (design sec. 5.2).
  if (identical(d$ci_method, "analytic") && d$N < cpm_analytic_ci_n_caution) {
    cat(
      "\n  Note: analytic (Wald) confidence intervals may materially mis-cover ",
      "at this sample size\n  (N < ", cpm_analytic_ci_n_caution,
      "); prefer the bootstrap on the raw-data path when available.\n",
      sep = ""
    )
  }
  cat("\n")
  invisible(object)
}
