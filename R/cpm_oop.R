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
  # Each entry is wrapped here rather than by the caller, so the returned
  # character vector stays the finished text the callers cat() unchanged.
  note <- function(...) {
    paste0(
      paste(wrap_prose(paste0(...), prefix = "  "), collapse = "\n"),
      "\n"
    )
  }
  msg <- character(0)
  if (!isTRUE(details$accepted)) {
    msg <- c(msg, note(
      "Note: the fit did not meet the convergence acceptance criterion ",
      "(gradient norm ", format(details$gradient_norm, digits = 2),
      "); interpret with caution."
    ))
  }
  if (isTRUE(details$heywood)) {
    msg <- c(msg, note(
      "Note: a communality index reached its upper boundary ",
      "(\u03b6 > 0.995, a Heywood-type solution)."
    ))
  }
  if (isTRUE(details$sigma_pathology)) {
    msg <- c(msg, note(
      "Note: a fitted variance ratio (\u03c3\u00b2) departs materially from 1 ",
      "(outside [0.5, 2]); the scaling or model may be misspecified."
    ))
  }
  if (length(details$removed_harmonics) > 0) {
    msg <- c(msg, note(
      "Note: harmonic(s) ",
      paste(details$removed_harmonics, collapse = ", "),
      " were on the zero boundary and removed (df adjusted)."
    ))
  }
  if (isTRUE(details$multimodal)) {
    msg <- c(msg, note(
      "Note: competing near-tied optima were found; the solution may be ",
      "weakly identified."
    ))
  }
  # Bootstrap replicate accounting (design sec. 5.2): surface exclusions.
  if (identical(details$ci_method, "bootstrap") &&
      isTRUE(details$boots_used < details$boots)) {
    n_bad <- details$boots - details$boots_used
    msg <- c(msg, note(
      "Note: ", n_bad, " of ", details$boots, " bootstrap resamples were ",
      "excluded (", details$boots_degenerate, " degenerate, ",
      details$boots_nonconvergent, " non-convergent); the intervals are ",
      "based on ", details$boots_used, " replicates and are conditional on ",
      "estimability."
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
#' [cpm_fit()]). The vignette section *When a fit sits at a boundary*
#' (`vignette("evaluating-circumplex-structure")`) glosses each marker and
#' gives the interpretation and next steps when one fires. When the
#' confidence intervals are bootstrap, any fired markers are instead listed
#' in a descriptive note at every sample size; the note also states that
#' what has been measured about the markers covers analytic intervals only
#' (and not every marker was measured), so they are not validated as
#' predictors of the bootstrap intervals.
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

  # Bootstrap-path fired-marker note (M94): without it the marker vocabulary
  # the vignette teaches prints only inside the analytic caution below, which
  # a bootstrap fit never reaches. Descriptive, at every N: the marker study
  # measured analytic intervals only (and not every marker), so the note
  # states that limit rather than claiming an interval consequence. The note
  # belongs to the Diagnostics section, so it also opens the section when no
  # diagnostic line fires.
  boot_markers <- if (identical(d$ci_method, "bootstrap")) {
    cpm_boundary_markers(object)
  } else {
    character(0)
  }

  diag_lines <- cpm_diagnostic_lines(d)
  if (length(diag_lines) > 0 || length(boot_markers) > 0) {
    cat("\n# Diagnostics\n\n")
    for (line in diag_lines) cat(line)
  }

  if (length(boot_markers) > 0) {
    # Wrap the label sentence at whole-label boundaries only: a marker label
    # is taught as a unit, so it must never break across lines. The opening
    # clause and each label are handed to wrap_prose() as atomic units, so a
    # break can only ever fall between them.
    items <- paste0(boot_markers, ";")
    items[length(items)] <- sub(";$", ".", items[length(items)])
    # The blank separator line is owed only after printed diagnostic lines;
    # when the note opens the section itself, the header's own "\n\n"
    # already provides the single blank line every other render shows.
    if (length(diag_lines) > 0) cat("\n")
    cat_prose(
      c("Note: boundary/weak-identification markers fired:", items),
      prefix = "  ",
      atomic = TRUE
    )
    # The trailing caveat is ordinary prose and wraps at word boundaries.
    cat_prose(
      paste0(
        "What has been measured about these markers covers analytic ",
        "intervals only, and not every marker was measured; they are not ",
        "validated as predictors of the bootstrap intervals shown here ",
        "(see the vignette section 'When a fit sits at a boundary')."
      ),
      prefix = "  "
    )
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
      cat("\n")
      cat_prose(
        paste0(
          "Note: analytic (Wald) confidence intervals may materially ",
          "mis-cover at this sample size (N < ", cpm_analytic_ci_n_caution,
          "); prefer the bootstrap on the raw-data path when available."
        ),
        prefix = "  "
      )
    } else if (d$N < cpm_analytic_ci_n_boundary_caution) {
      markers <- cpm_boundary_markers(object)
      if (length(markers) > 0) {
        # A marker label is taught as a unit here too, so it must not break
        # across lines: the surrounding prose is split into words and each
        # label (with the punctuation that joins it to its neighbours) is
        # handed to wrap_prose() whole, as atomic units.
        labels <- paste0(markers, ";")
        labels[[length(labels)]] <- paste0(markers[[length(markers)]], ");")
        labels[[1]] <- paste0("(", labels[[1]])
        words <- function(s) unlist(strsplit(s, " ", fixed = TRUE))
        cat("\n")
        cat_prose(
          c(
            words(paste0(
              "Note: this solution is near a parameter boundary or weakly ",
              "identified"
            )),
            labels,
            words(paste0(
              "analytic (Wald) confidence intervals mis-covered for such fits ",
              "in validation even at N in the tens of thousands. Interpret ",
              "them with caution and prefer the bootstrap on the raw-data ",
              "path when available."
            ))
          ),
          prefix = "  ",
          atomic = TRUE
        )
      }
    }
    if (identical(d$scaling, "free")) {
      # The free family reports sigma^2 as an uncertainty-free variance-ratio
      # diagnostic (D-009); no analytic interval is offered for it, ever. (Its
      # bordered information is also singular below N ~ 2000, so those CIs are
      # often NA -- an independent reason the N < ...n_caution note above holds.)
      cat("\n")
      cat_prose(
        paste0(
          "Note: the free-scaling variance ratios (\u03c3\u00b2) carry no ",
          "confidence interval."
        ),
        prefix = "  "
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
#' (\eqn{\theta}{theta}), at a radius given by its communality
#' (\eqn{\zeta^2}{zeta^2}, the share of
#' its variance explained by the common circumplex factors), so items that the
#' model explains well sit near the outer ring and items it explains poorly sit
#' near the centre. The canvas spokes mark the *theoretical* angles supplied to
#' [cpm_fit()], so the gap between a point and its spoke shows how far the
#' estimated angle departed from the hypothesised one. Where the confidence
#' intervals are estimable, a wedge spans each item's angle CI (angularly) and
#' communality CI (radially). An interval of zero width has no area, so it is
#' drawn as a line instead: along the radius for a zero-width angle CI, along
#' the arc for a zero-width communality CI, and as a short cap across the
#' interval when both are zero. The cap has the same drawn length at every
#' radius except near the centre, where it spans at most a quarter turn and so
#' is shorter. A scale whose communality CI is zero at both ends has no
#' visible interval at the centre and is drawn as a point, with a warning.
#'
#' The communality axis and its labels are drawn along the midpoint of the
#' widest gap between spokes that holds no estimated angle (ties go to the
#' smallest midpoint; a point on a spoke counts as in both gaps next to it).
#' When every gap holds a point, the axis goes in the widest gap, as
#' [coord_circumplex()] places it by default.
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
  # always estimated, so this holds unless a future path yields NA) and gets an
  # interval mark only when its CI region is estimable, names a proper arc
  # (< 360 deg span), and reaches past the centre. A Heywood/weakly-identified
  # fit can leave the CI NA (no region), produce a near-full-circle angle CI, or
  # clamp both zeta bounds to 0; at communality 0 every angle maps to the one
  # centre point, so no mark there has any length. Such scales render as a point
  # only, and we name each with its reason rather than let the mark vanish
  # silently.
  df$Scale <- factor(df$Scale, levels = unique(as.character(df$Scale)))
  span <- ssm_arc_span(df$Angle_lci, df$Angle_uci)
  has_region <- ssm_has_region(df$comm_lci, df$comm_uci,
                               df$Angle_lci, df$Angle_uci)
  proper_arc <- is.finite(span) & span >= 0 & span < 360
  off_centre <- has_region & df$comm_uci > 0
  drawable <- has_region & proper_arc & off_centre
  pointable <- ssm_has_location(df$comm_est, df$Angle)
  no_wedge <- pointable & !drawable
  if (any(no_wedge)) {
    reason <- ifelse(
      !has_region, "inestimable interval",
      ifelse(!proper_arc, "full-circle angle interval",
             "communality interval at 0")
    )
    warning(
      "Confidence wedge omitted for scale(s): ",
      paste(
        paste0(as.character(df$Scale), " (", reason, ")")[no_wedge],
        collapse = ", "
      ),
      "; drawn as a point only.",
      call. = FALSE
    )
  }

  # A zero-width interval names no wedge (geom_ssm_arc() drops a zero angle
  # span, and a zero communality span has no area), so it is drawn as a line
  # instead: along the radius for a zero-width angle interval, along the arc for
  # a zero-width communality interval, and as a short cap across the interval's
  # own location when both widths are zero. The cap's angular extent is chosen
  # so its drawn length is the same at every radius: `cap_length` is a fraction
  # of the rim radius, limited to a quarter turn near the centre.
  angle_zero <- drawable & span == 0
  comm_zero <- drawable & df$comm_lci == df$comm_uci
  wedge <- drawable & !angle_zero & !comm_zero
  marked <- angle_zero | comm_zero
  marks <- NULL
  if (any(marked)) {
    cap_length <- 0.12
    m <- df[marked, ]
    m_span <- span[marked]
    both <- angle_zero[marked] & comm_zero[marked]
    cap_half <- pmin(
      cap_length / 2 / (m$comm_lci / amax) * 180 / pi, 45
    )
    marks <- data.frame(
      Scale = m$Scale,
      x = ifelse(both, m$Angle_lci - cap_half, m$Angle_lci),
      xend = ifelse(both, m$Angle_lci + cap_half, m$Angle_lci + m_span),
      y = m$comm_lci,
      yend = m$comm_uci
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
  # Keep the amplitude axis labels off the plotted points. The canvas coord is
  # built fresh by ggcircumplex() above, so setting its field changes no other
  # plot.
  p$coordinates$r_axis_angle <- ssm_r_axis_angle_clear(
    angles, df$Angle[pointable]
  )

  if (any(wedge)) {
    p <- p +
      geom_ssm_arc(
        data = df[wedge, ],
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

  if (!is.null(marks)) {
    # geom_segment() under the circumplex coord is munched, so an angular
    # segment follows the arc. The stroke takes the scale's colour so it
    # matches the fill of the point drawn on top of it.
    p <- p +
      ggplot2::geom_segment(
        data = marks,
        mapping = ggplot2::aes(
          x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
          colour = .data$Scale
        ),
        linewidth = 2.5,
        lineend = "round",
        show.legend = FALSE,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_colour_brewer(
        palette = "Set2", limits = levels(df$Scale), guide = "none"
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
