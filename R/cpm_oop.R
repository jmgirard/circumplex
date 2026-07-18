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
  if (isTRUE(details$sigma_pathology)) {
    msg <- c(msg, paste0(
      "  Note: a fitted variance ratio (\u03c3\u00b2) departs materially from 1 ",
      "(outside [0.5, 2]);\n  the scaling or model may be misspecified.\n"
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
#' diagnostics in plain language. When the confidence intervals are analytic,
#' prints a coverage caution calibrated by simulation: unconditionally when the
#' sample size is modest (N < 2000, where Wald intervals mis-covered for every
#' configuration studied), and up to N = 50000 when the fitted solution shows a
#' boundary or weak-identification marker (Heywood communality, removed
#' harmonic, small correlation-function weight, ill-conditioning, or competing
#' near-tied optima), the regime where they mis-covered even at large N (see
#' [cpm_fit()]).
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
  # Symmetric misfit patterns can tie several pairs at the maximum to within
  # floating-point noise (~1e-16); which.max() would then break the tie on
  # sub-ULP differences that vary by BLAS/platform, making the reported pair
  # non-deterministic across machines. Take the first (fixed column-major
  # order) of the pairs within a tolerance of the maximum instead.
  aresid <- abs(rvals)
  worst <- which(aresid >= max(aresid) - 1e-9)[1]
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

  # N-conditional analytic-CI caution (design sec. 5.2), calibrated by the B6
  # coverage oracle: unconditional below cpm_analytic_ci_n_caution;
  # boundary-marker-conditional below cpm_analytic_ci_n_boundary_caution
  # (see the constants in R/cpm_fit.R for the measured coverage behind both).
  # The SAME thresholds apply to the free-scaling family: the M19 coverage
  # oracle measured its theta/zeta/beta coverage regime to be the diag family's
  # (sigma-hat ~= 1 at correlation truths), so the diag thresholds are
  # coverage-validated for the free family too, not silently reused (D-010,
  # superseding M18-D3's placeholder unconditional caution). The free family's
  # variance ratios carry no interval, so that note is appended for it below.
  if (identical(d$ci_method, "analytic")) {
    if (d$N < cpm_analytic_ci_n_caution) {
      cat(
        "\n  Note: analytic (Wald) confidence intervals may materially mis-cover ",
        "at this sample size\n  (N < ", cpm_analytic_ci_n_caution,
        "); prefer the bootstrap on the raw-data path when available.\n",
        sep = ""
      )
    } else if (d$N < cpm_analytic_ci_n_boundary_caution) {
      markers <- cpm_boundary_markers(object)
      if (length(markers) > 0) {
        cat(
          "\n  Note: this solution is near a parameter boundary or weakly ",
          "identified\n  (", paste(markers, collapse = "; "), ");\n  ",
          "analytic (Wald) confidence intervals mis-covered for such fits ",
          "in validation\n  even at N in the tens of thousands. Interpret ",
          "them with caution and prefer\n  the bootstrap on the raw-data ",
          "path when available.\n",
          sep = ""
        )
      }
    }
    if (identical(d$scaling, "free")) {
      # The free family reports sigma^2 as an uncertainty-free variance-ratio
      # diagnostic (D-009); no analytic interval is offered for it, ever. (Its
      # bordered information is also singular below N ~ 2000, so those CIs are
      # often NA -- an independent reason the N < ...n_caution note above holds.)
      cat(
        "\n  Note: the free-scaling variance ratios (\u03c3\u00b2) carry no ",
        "confidence interval.\n",
        sep = ""
      )
    }
  }
  cat("\n")
  invisible(object)
}

# ---- plot -------------------------------------------------------------------

#' Plot a circular process model fit
#'
#' Draw the estimated item configuration of a [cpm_fit()] object on the circular
#' canvas from [ggcircumplex()]. Each scale is placed at its *estimated* angle
#' (`θ`), at a radius given by its communality (`ζ²`, the share of
#' its variance explained by the common circumplex factors), so items that the
#' model explains well sit near the outer ring and items it explains poorly sit
#' near the centre. The canvas spokes mark the *theoretical* angles supplied to
#' [cpm_fit()], so the gap between a point and its spoke shows how far the
#' estimated angle departed from the hypothesised one. Where the confidence
#' intervals are estimable, a wedge spans each item's angle CI (angularly) and
#' communality CI (radially).
#'
#' @param x A `circumplex_cpm` object from [cpm_fit()].
#' @param amax A single positive number giving the communality represented by
#'   the canvas's outer ring (default = 1, the maximum possible communality).
#' @param angle_labels Either `NULL` or a character vector of spoke labels, one
#'   per scale in the fitted order. `NULL` (default) labels the spokes with the
#'   scale names.
#' @param legend A logical: draw a legend keying the colours to the scale names
#'   (default = `TRUE`).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A \pkg{ggplot2} object.
#' @seealso [cpm_fit()], [ggcircumplex()]
#' @method plot circumplex_cpm
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' set.seed(12345)
#' fit <- cpm_fit(jz2017, scales = scales, boots = 100)
#' plot(fit)
#' }
plot.circumplex_cpm <- function(x, amax = 1, angle_labels = NULL,
                                legend = TRUE, ...) {
  chkDots(...)
  df <- x$results
  # Canvas spokes mark the theoretical angles the user supplied to cpm_fit().
  angles <- df$Angle_theory

  stopifnot(is_num(amax, n = 1) && amax > 0)
  stopifnot(is_flag(legend))
  stopifnot(is_null_or_char(angle_labels, n = nrow(df)))

  # Radial axis = communality (zeta^2, in [0, 1]). The communality CI comes from
  # squaring the zeta bounds; squaring is monotone on [0, 1] so the order is
  # preserved, and the zeta bounds are first clamped to [0, 1] because an
  # analytic (Wald) interval can overshoot the boundary.
  df$comm_est <- df$Communality
  df$comm_lci <- pmin(pmax(df$Zeta_lci, 0), 1)^2
  df$comm_uci <- pmin(pmax(df$Zeta_uci, 0), 1)^2

  # A scale is drawn as a point whenever it has a location (cpm angles are
  # always estimated, so this holds unless a future path yields NA) and as a
  # wedge only when its CI region is estimable AND names a proper arc (< 360
  # deg span). A Heywood/weakly-identified fit can leave the CI NA (no region)
  # or produce a near-full-circle angle CI; such scales render as a point with
  # no wedge, and we name them rather than let the wedge vanish silently.
  df$Scale <- factor(df$Scale, levels = unique(as.character(df$Scale)))
  span <- ssm_arc_span(df$Angle_lci, df$Angle_uci)
  drawable <- ssm_has_region(df$comm_lci, df$comm_uci,
                             df$Angle_lci, df$Angle_uci) &
    is.finite(span) & span >= 0 & span < 360
  pointable <- ssm_has_location(df$comm_est, df$Angle)
  no_wedge <- pointable & !drawable
  if (any(no_wedge)) {
    warning(
      "Confidence wedge omitted for scale(s) with an inestimable or ",
      "full-circle interval: ",
      paste(as.character(df$Scale)[no_wedge], collapse = ", "),
      "; drawn as a point only.",
      call. = FALSE
    )
  }

  labels <- if (is.null(angle_labels)) as.character(df$Scale) else angle_labels

  # A single fill aesthetic keys the colour so points and wedges share one
  # legend (a colour aesthetic on the wedges would split it into two guides).
  # Pin the fill order to the scale levels: the reference scale's wedge has zero
  # angular width and so drops out of the arc layer's computed data, which would
  # otherwise let scale training append it last (and colour it grey).
  p <- ggcircumplex(angles = angles, labels = labels, amax = amax) +
    ggplot2::scale_fill_brewer(palette = "Set2", limits = levels(df$Scale)) +
    ggplot2::theme(
      legend.position = if (legend) "right" else "none"
    )

  if (any(drawable)) {
    p <- p +
      geom_ssm_arc(
        data = df[drawable, ],
        mapping = ggplot2::aes(
          amplitude_min = .data$comm_lci,
          amplitude_max = .data$comm_uci,
          displacement_min = .data$Angle_lci,
          displacement_max = .data$Angle_uci,
          fill = .data$Scale
        ),
        alpha = 0.4,
        color = "grey40",
        linewidth = 0.5
      )
  }

  p +
    geom_ssm_point(
      data = df[pointable, ],
      mapping = ggplot2::aes(
        amplitude = .data$comm_est,
        displacement = .data$Angle,
        fill = .data$Scale
      ),
      shape = 21,
      size = 3,
      color = "black"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend("Scale"))
}
