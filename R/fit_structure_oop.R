# S3 class and methods for the circumplex-structure tests (Acton & Revelle,
# 2004). The constructor mirrors new_cpm() (R/cpm_oop.R): a scalar S3 list with
# named components and a class tag. fit_structure() (R/fit_structure.R) is the
# only caller.

# ---- constructor ------------------------------------------------------------

new_structure <- function(results, randall, loadings, details, call) {
  stopifnot(is.data.frame(results))
  stopifnot(is.list(randall))
  stopifnot(is.matrix(loadings))
  stopifnot(is.list(details))
  new_s3_lst(
    list(
      results = results,
      randall = randall,
      loadings = loadings,
      details = details,
      call = call
    ),
    class = "circumplex_structure"
  )
}

# ---- shared formatting helpers ----------------------------------------------

# Map an interpretation category to Acton & Revelle's likelihood phrasing,
# combined with the hypothesis the test evaluates ("equal axes", "equal
# spacing", "interstitiality"). An undefined category (NA, from an uncalibrated
# scale count) prints a dash so print()/summary() render a stable column.
structure_phrase <- function(hypothesis, category) {
  if (is.na(category)) {
    return("-")
  }
  # The cutoffs are likelihood ratios of the target vs the competing structure
  # (a CDF ratio F_target/F_other), not posterior probabilities, so the phrasing
  # is "as likely as the alternative", never "more likely than not" (which would
  # read as a >50% posterior or a significance claim -- the caveat and the
  # CLAUDE.md vignette-precision rule both forbid that reading).
  claim <- switch(category,
    almost = "almost certain",
    thrice = "at least 3x as likely as the alternative",
    twice  = "at least 2x as likely as the alternative",
    weak   = "not clearly supported"
  )
  paste0(hypothesis, ": ", claim)
}

# Format a numeric vector for display, rendering NA as a dash. print.data.frame's
# `na.print` argument does not apply to numeric columns (it is silently swallowed
# by `...`), so any NA -- an undefined statistic (a degenerate scale) or an
# uncalibrated cutoff (no cutoffs at this nv) -- is pre-formatted here to "-",
# matching the Verdict/Interpretation columns instead of printing a bare "NA".
structure_dash <- function(x, digits) {
  out <- format(round(x, digits))
  out[is.na(x)] <- "-"
  out
}

# Terse verdict for summary()'s cutoff table, which is already wide with the
# three cutoff columns. An undefined category prints a dash.
structure_verdict <- function(category) {
  if (is.na(category)) {
    return("-")
  }
  switch(category,
    almost = "almost certain",
    thrice = "3x+ likely",
    twice  = "2x+ likely",
    weak   = "unsupported"
  )
}

# One RANDALL summary line: index, p-value, and how the null was built.
structure_randall_line <- function(randall, digits = 3) {
  if (is.na(randall$statistic)) {
    return("Correspondence index: undefined (incomplete correlations).\n")
  }
  paste0(
    "Correspondence index = ", round(randall$statistic, digits),
    ", p = ", format.pval(randall$p_value, digits = digits, eps = 1e-4),
    " (", randall$method,
    ", ", randall$n_perm, " relabelings)\n"
  )
}

# The heuristic-cutoff caveat, printed once wherever interpretations appear so
# the vignette-precision rule (CLAUDE.md) is never silently dropped: these are
# likelihood classifications read off simulated distributions, not tests.
structure_caveat <- paste0(
  "  Interpretations are heuristic likelihood classifications from ",
  "simulation, not\n  significance tests (Acton & Revelle, 2004). RANDALL's ",
  "p-value is exact.\n"
)

# ---- print ------------------------------------------------------------------

#' Print circumplex-structure test results
#'
#' Compact display of a [fit_structure()] object: the four factor-analytic
#' criteria with their statistics and interpretive classifications, and the
#' RANDALL order test with its randomization p-value.
#'
#' @param x A `circumplex_structure` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `x`, invisibly.
#' @method print circumplex_structure
#' @export
print.circumplex_structure <- function(x, digits = 3, ...) {
  d <- x$details
  cat(
    "\nCircumplex Structure Tests (Acton & Revelle, 2004)",
    "\nScales (nv):  ", d$nv,
    "\nScoring:      ", d$scoring,
    if (identical(d$scoring, "deviation")) " (row-mean centered)" else "",
    "\n\n# Exploratory criteria\n\n",
    sep = ""
  )

  disp <- data.frame(
    Test = x$results$Test,
    Statistic = structure_dash(x$results$Statistic, digits),
    Interpretation = mapply(
      structure_phrase, x$results$Hypothesis, x$results$Category
    ),
    stringsAsFactors = FALSE
  )
  print(disp, row.names = FALSE, right = FALSE)

  cat("\n# Order hypothesis (RANDALL)\n\n  ",
    structure_randall_line(x$randall, digits),
    sep = ""
  )

  if (!d$calibrated) {
    cat(
      "\n  Note: no interpretive cutoffs are calibrated for ", d$nv,
      " scales; only\n  eight (octant) scales are. The criterion statistics ",
      "are reported without\n  an interpretation.\n",
      sep = ""
    )
  } else {
    cat("\n", structure_caveat, sep = "")
  }
  invisible(x)
}

# ---- summary ----------------------------------------------------------------

#' Summarize circumplex-structure test results
#'
#' Fuller display of a [fit_structure()] object: adds the interpretive cutoffs
#' behind each classification, the estimated angle and communality of each
#' scale, and the analysis settings.
#'
#' @param object A `circumplex_structure` object.
#' @param digits The number of decimal places to display (default = 3).
#' @param ... Not used.
#' @return `object`, invisibly.
#' @method summary circumplex_structure
#' @export
summary.circumplex_structure <- function(object, digits = 3, ...) {
  d <- object$details
  cat(
    "\nCircumplex Structure Tests (Acton & Revelle, 2004)",
    "\nScales (nv):  ", d$nv,
    "\nScoring:      ", d$scoring,
    if (identical(d$scoring, "deviation")) " (row-mean centered)" else "",
    "\nRidge:        ", d$ridge,
    "\n\n# Exploratory criteria\n\n",
    sep = ""
  )

  res <- object$results
  tab <- data.frame(
    Test = res$Test,
    Statistic = structure_dash(res$Statistic, digits),
    Almost = structure_dash(res$Almost, digits),
    Thrice = structure_dash(res$Thrice, digits),
    Twice = structure_dash(res$Twice, digits),
    Verdict = vapply(res$Category, structure_verdict, character(1)),
    stringsAsFactors = FALSE
  )
  print(tab, row.names = FALSE, right = FALSE)

  # Estimated per-scale geometry: angle (degrees, [0, 360)) and communality.
  L <- object$loadings
  h2 <- rowSums(L^2)
  angle <- (atan2(L[, 2], L[, 1]) * 180 / pi) %% 360
  geom <- data.frame(
    Scale = rownames(L),
    Angle = round(angle, digits),
    Communality = round(h2, digits),
    stringsAsFactors = FALSE
  )
  cat("\n# Estimated scale geometry\n\n")
  print(geom, row.names = FALSE, right = FALSE)

  cat("\n# Order hypothesis (RANDALL)\n\n  ",
    structure_randall_line(object$randall, digits),
    sep = ""
  )

  if (!d$calibrated) {
    cat(
      "\n  Note: no interpretive cutoffs are calibrated for ", d$nv,
      " scales; only\n  eight (octant) scales are. The criterion statistics ",
      "are reported without\n  an interpretation.\n",
      sep = ""
    )
  } else {
    cat("\n", structure_caveat, sep = "")
  }
  cat("\n")
  invisible(object)
}

# ---- plot -------------------------------------------------------------------

#' Plot a circumplex-structure configuration
#'
#' Draw the two-factor loading configuration of a [fit_structure()] object on
#' the circular canvas from [ggcircumplex()]. Each scale is placed at its
#' estimated angle (`atan2` of its two principal-axis loadings) and at a radius
#' given by its communality (the share of its variance on the first two
#' factors), so a clean circumplex shows the scales spread evenly around a ring
#' of similar radius, unequal axes show scales at differing radii (what the
#' Fisher Test measures), and simple structure shows scales bunched near a few
#' angles (what the Gap and interstitiality tests measure). The canvas spokes
#' mark the same estimated angles, labelled by scale.
#'
#' @param x A `circumplex_structure` object from [fit_structure()].
#' @param amax A single positive number giving the communality represented by
#'   the canvas's outer ring (default = 1). Principal-axis communalities can
#'   exceed 1 in a Heywood case; when any scale's communality exceeds `amax`
#'   the ring is expanded to contain it, so no point is ever drawn outside the
#'   canvas.
#' @param legend A logical: draw a legend keying the colours to the scale names
#'   (default = `TRUE`).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A \pkg{ggplot2} object.
#' @seealso [fit_structure()], [ggcircumplex()]
#' @method plot circumplex_structure
#' @export
#' @examples
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' plot(fit_structure(jz2017, scales = scales))
plot.circumplex_structure <- function(x, amax = 1, legend = TRUE, ...) {
  chkDots(...)
  stopifnot(is_num(amax, n = 1) && amax > 0)
  stopifnot(is_flag(legend))

  L <- x$loadings
  scales <- rownames(L)
  h2 <- rowSums(L^2)
  # Un-rescaled PAF communalities can exceed 1 (a Heywood case), which would put
  # a point past the outer ring; expand the ring to contain the largest so no
  # point is clipped (never shrink a user-supplied amax; ignore NA communalities
  # from a degenerate solution).
  amax <- max(amax, h2, na.rm = TRUE)
  angle <- (atan2(L[, 2], L[, 1]) * 180 / pi) %% 360
  df <- data.frame(
    Scale = factor(scales, levels = scales),
    angle = angle,
    comm = h2
  )

  # Canvas spokes at the estimated angles, in angular order, labelled by scale.
  ord <- order(df$angle)
  # Set2 is the package's categorical palette (matching plot.circumplex_cpm) but
  # tops out at eight colours; instruments with more scales fall back to
  # evenly-spaced hues so every scale still gets a distinct fill.
  fill_scale <- if (nlevels(df$Scale) <= 8) {
    ggplot2::scale_fill_brewer(palette = "Set2", limits = levels(df$Scale))
  } else {
    ggplot2::scale_fill_hue(limits = levels(df$Scale))
  }
  p <- ggcircumplex(
    angles = df$angle[ord], labels = as.character(df$Scale)[ord], amax = amax
  ) +
    fill_scale +
    ggplot2::theme(legend.position = if (legend) "right" else "none")

  p +
    geom_ssm_point(
      data = df,
      mapping = ggplot2::aes(
        amplitude = .data$comm,
        displacement = .data$angle,
        fill = .data$Scale
      ),
      amax = amax,
      shape = 21,
      size = 3,
      color = "black"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend("Scale"))
}
