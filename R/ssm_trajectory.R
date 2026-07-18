# Trajectory plotting for occasions objects (M33) ------------------------------
# A Cartesian companion to the circular canvas: SSM parameters plotted against
# occasion, one facet per parameter. Everything statistically load-bearing
# happens here in the reshape, not in the ggplot call -- the occasion ordering
# and the displacement branch are wrong-answer channels that render without
# error, so they are pinned at the data level and tested there.

# Panel titles, in canonical parameter order. Doubles as the panel factor's
# level set, so a dropped parameter cannot silently reorder the facets.
ssm_trajectory_panels <- function() {
  c(
    e = "Elevation",
    x = "X-value",
    y = "Y-value",
    a = "Amplitude",
    d = "Displacement"
  )
}

# Unwrap a temporally ordered displacement series onto a continuous branch,
# bridging occasions whose displacement is undefined.
#
# angle_unwrap() propagates NA from a missing wave *onward* (cumsum), which is
# the right policy for a series whose later values are genuinely
# branch-ambiguous. For a plot it is the wrong one: a single flat occasion
# would blank the rest of the trajectory rather than leaving a gap at the flat
# occasion. So unwrap the defined occasions as a sequence and reinsert NA at
# the gaps. The assumption this widens is angle_unwrap()'s own -- that the
# profile rotates less than a half-turn between consecutive *defined*
# occasions -- applied across the gap instead of across one step; it is
# documented on ssm_plot_trajectory() because no data can verify it.
ssm_unwrap_gapped <- function(x) {
  ok <- !is.na(x)
  out <- rep(NA_real_, length(x))
  if (any(ok)) out[ok] <- angle_unwrap(as.numeric(x[ok]))
  out
}

# Place a CI interval on its estimate's unwrapped branch.
#
# Non-contrast displacement bounds are each independently wrapped into
# [0, 360] by quantile.circumplex_radian() (R/ssm_bootstrap.R), so a
# seam-straddling interval is stored with lower > upper. Never place a bound at
# the estimate's branch offset -- that throws a straddling bound a full turn off
# and inverts the ribbon (LESSONS M27).
#
# The lower bound goes at its counterclockwise distance *below* the estimate,
# and the upper bound is then derived from the interval's stored arc span rather
# than placed independently. Placing each bound by its own signed distance
# (LESSONS M27's expression, which is correct for the contrast rows and coverage
# checks it was written for) silently clamps every bound into (-180, 180] of its
# estimate, so it cannot represent an interval whose arc exceeds a half-turn --
# exactly the near-zero-amplitude case D-007 certification exists to flag, where
# it rendered a 337-degree "displacement unknown" interval as a 23-degree
# INVERTED band that read as the most precise occasion in the series. Reading
# the pair as the counterclockwise arc from lower to upper is the package's
# standing convention (ssm_arc_span(), R/geom_ssm.R). Caught by review, M33.
ssm_interval_on_branch <- function(lci, uci, est, branch) {
  lo <- branch - ((as.numeric(est) - as.numeric(lci)) %% 360)
  list(lo = lo, hi = lo + ssm_arc_span(as.numeric(lci), as.numeric(uci)))
}

# Reshape a wide per-time-point table into the long per-panel frame the
# trajectory plot draws: one row per (Group, <time_col>, Parameter).
#
# THE single definition of the displacement unwrap, the certification carry, and
# the melt, shared by both entry points (an occasions object and a user-supplied
# trajectory table). The two paths differ only in how they assemble `dat` and in
# whether the time axis is discrete; everything statistically load-bearing
# happens here exactly once.
#
# `dat` arrives with a `Group` factor, a time column named `time_col` (already
# ordered -- a factor on the occasions path, numeric on the table path), a
# logical `Certified` column (all-NA when the caller supplied no verdict), and
# `<p>_est`/`<p>_lci`/`<p>_uci` triples for some subset of the canonical
# parameters. Which panels appear is read off the triples present, so a table
# carrying only amplitude and displacement yields only those two panels.
ssm_trajectory_long <- function(dat, time_col, drop_xy = FALSE) {
  # A profile has a defined displacement iff it has a location; the shared
  # predicate keeps this agreeing with the circular geoms rather than rolling a
  # second is.na() criterion. A flat (zero-amplitude) profile fails it.
  located <- ssm_has_location(dat$a_est, dat$d_est)
  dat$d_est[!located] <- NA_real_

  # Displacement onto a continuous branch, per group series, in time order.
  # Done before the melt so the unwrap sees the temporally ordered sequence.
  dat <- dat[order(dat$Group, dat[[time_col]]), , drop = FALSE]
  by_group <- split(seq_len(nrow(dat)), dat$Group, drop = TRUE)
  d_branch <- rep(NA_real_, nrow(dat))
  d_low <- rep(NA_real_, nrow(dat))
  d_high <- rep(NA_real_, nrow(dat))
  for (idx in by_group) {
    est <- as.numeric(dat$d_est[idx])
    branch <- ssm_unwrap_gapped(est)
    interval <- ssm_interval_on_branch(
      dat$d_lci[idx], dat$d_uci[idx], est, branch
    )
    d_branch[idx] <- branch
    d_low[idx] <- interval$lo
    d_high[idx] <- interval$hi
  }
  dat$d_est <- d_branch
  dat$d_lci <- d_low
  dat$d_uci <- d_high

  params <- names(ssm_trajectory_panels())
  params <- params[vapply(
    params, function(p) all(paste0(p, c("_est", "_lci", "_uci")) %in% names(dat)),
    logical(1)
  )]
  if (drop_xy) params <- setdiff(params, c("x", "y"))

  out <- do.call(rbind, lapply(params, function(p) {
    df <- data.frame(
      Group = dat$Group,
      Parameter = p,
      est = as.numeric(dat[[paste0(p, "_est")]]),
      lci = as.numeric(dat[[paste0(p, "_lci")]]),
      uci = as.numeric(dat[[paste0(p, "_uci")]]),
      Certified = dat$Certified,
      stringsAsFactors = FALSE
    )
    # Inserted by name rather than in the data.frame() call so the time column
    # keeps the caller's own name ("Occasion", "wave") without a rename step.
    df[[time_col]] <- dat[[time_col]]
    df[c("Group", time_col, "Parameter", "est", "lci", "uci", "Certified")]
  }))

  panels <- ssm_trajectory_panels()
  out$Panel <- factor(panels[out$Parameter], levels = panels[params])
  out
}

# Reshape an occasions object into the long per-panel frame the trajectory plot
# draws: one row per (Group, Occasion, Parameter).
ssm_trajectory_frame <- function(ssm_object, drop_xy = FALSE) {
  results <- ssm_object$results
  details <- ssm_object$details

  # Drop the contrast row. It is the last row when details$contrast (the
  # positional detector print.circumplex_ssm() uses -- there is no boolean
  # column), it is not a time point, and its displacement rides the opposite
  # branch convention (circumplex_contrast_radian: already contiguous, may be
  # negative or exceed 360). ssm_plot_circle()'s df[1:2, ] slice is not reusable
  # here -- it truncates k > 2 and grouped objects.
  if (isTRUE(details$contrast)) {
    results <- results[-nrow(results), , drop = FALSE]
  }

  # details$occasions is the canonical order (the occasions list order, or the
  # long path's factor levels / first-appearance order). results$Occasion is
  # character, so mapping it to a discrete scale without this factoring lets
  # ggplot2 re-sort it alphabetically -- which flips a T10/T2 pair and silently
  # reverses the trajectory's time axis.
  results$Occasion <- factor(results$Occasion, levels = details$occasions)
  results$Group <- factor(results$Group, levels = unique(results$Group))

  # D-007 displacement-interpretability guardrail, per profile row: a pure
  # function of the amplitude CI pair. Carried on every row so the plot can
  # mark it where it applies (the displacement panel) without a second join.
  results$Certified <- ssm_certified(results$a_lci, results$a_uci)

  ssm_trajectory_long(results, time_col = "Occasion", drop_xy = drop_xy)
}

#' Create a Trajectory Plot of SSM Results Across Occasions
#'
#' Plot each Structural Summary Method parameter against occasion, one facet per
#' parameter, with its confidence interval as a band. This is a Cartesian
#' diagnostic plot, not a circumplex figure: the horizontal axis is time, not
#' angle.
#'
#' The displacement panel is drawn on an *unwrapped* branch, so a profile whose
#' displacement crosses the 0/360 boundary renders as one continuous path rather
#' than jumping a full turn. Values on that panel may therefore fall outside
#' \[0, 360); each confidence bound is placed at its signed angular distance
#' from its own estimate. Unwrapping assumes the profile rotates less than a
#' half-turn between consecutive occasions at which its displacement is defined
#' -- no data can verify this, so occasions that are far apart in time, or a
#' series with a gap, should be read with that in mind.
#'
#' Occasions appear in the order they were supplied to [ssm_analyze()] (or in
#' the occasion factor's level order for [ssm_analyze_long()]), never in
#' alphabetical order.
#'
#' On the displacement panel, an occasion whose amplitude confidence interval is
#' too close to zero for its displacement to be interpretable is drawn as a
#' hollow point; see [ssm_analyze()] for the certification rule. A profile with
#' no defined displacement at all (a flat profile) leaves a gap in that panel.
#'
#' A contrast row is never plotted as an occasion -- it is a difference, not a
#' time point. Use [ssm_plot_contrast()] for it.
#'
#' @param ssm_object An SSM results object produced by [ssm_analyze()] with the
#'   `occasions` argument, or by [ssm_analyze_long()].
#' @param drop_xy A logical determining whether the X-value and Y-value panels
#'   should be omitted (default = `FALSE`), leaving elevation, amplitude, and
#'   displacement.
#' @param base_size A positive number determining the base font size of the plot
#'   (default = 11).
#' @param na.rm A logical determining whether occasions that cannot be plotted
#'   (no defined displacement) are dropped silently (default = `TRUE`) or with a
#'   warning naming how many were removed (`FALSE`).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A ggplot object depicting each SSM parameter's trajectory across
#'   occasions, with confidence bands.
#' @family visualization functions
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' t1 <- jz2017[, scales]
#' t1$id <- seq_len(nrow(t1))
#' t1$occasion <- "T1"
#' t2 <- t1
#' t2$occasion <- "T2"
#' res <- ssm_analyze_long(rbind(t1, t2),
#'   scales = scales, id = "id", occasion = "occasion"
#' )
#' ssm_plot_trajectory(res)
#' ssm_plot_trajectory(res, drop_xy = TRUE)
#' }
ssm_plot_trajectory <- function(ssm_object,
                                drop_xy = FALSE,
                                base_size = 11,
                                na.rm = TRUE,
                                ...) {
  stopifnot(inherits(ssm_object, "circumplex_ssm"))
  if (is.null(ssm_object$details$occasions)) {
    stop(
      "This SSM object contains no occasions to plot a trajectory across; ",
      "produce one with `ssm_analyze(occasions = )` or `ssm_analyze_long()`. ",
      "For a single-occasion profile see `ssm_plot_circle()`.",
      call. = FALSE
    )
  }
  chkDots(...)

  stopifnot(is_flag(drop_xy), !is.na(drop_xy))
  stopifnot(is_flag(na.rm), !is.na(na.rm))
  # !is.finite() catches NA, NaN, and +/-Inf alike: an infinite base_size slips
  # past an is.na() guard and only surfaces as a cryptic error during render,
  # never naming this argument (M32).
  stopifnot(is_num(base_size, n = 1))
  if (!is.finite(base_size) || base_size <= 0) {
    stop("`base_size` must be a single positive finite number.", call. = FALSE)
  }

  df <- ssm_trajectory_frame(ssm_object, drop_xy = drop_xy)

  # Occasions that cannot be placed (a flat profile has no displacement). The
  # geoms would drop these anyway; routing the count through the shared warn
  # helper makes the drop speak when the caller opts out of silent removal,
  # matching the circular layers' na.rm convention. Counted on the displacement
  # panel so the number is one per *profile* -- counting melted rows would
  # report the same flat occasion once per affected parameter.
  unplottable <- is.na(df$est[df$Parameter == "d"])
  ssm_warn_dropped(
    sum(unplottable), na.rm, "ssm_plot_trajectory", "no defined displacement"
  )

  grouped <- nlevels(df$Group) > 1L
  d_rows <- df[df$Parameter == "d" & !is.na(df$est), , drop = FALSE]
  other_rows <- df[df$Parameter != "d" & !is.na(df$est), , drop = FALSE]

  p <-
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data$Occasion,
        y = .data$est,
        color = .data$Group,
        fill = .data$Group,
        group = .data$Group
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lci, ymax = .data$uci),
      alpha = 0.2,
      color = NA,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(na.rm = TRUE) +
    # Points are split by panel so certification -- a displacement-only
    # guardrail -- marks only where it applies, instead of implying every
    # parameter carries an interpretability verdict.
    ggplot2::geom_point(data = other_rows, size = 2, na.rm = TRUE) +
    ggplot2::geom_point(
      data = d_rows,
      mapping = ggplot2::aes(shape = .data$Certified),
      size = 2,
      na.rm = TRUE
    ) +
    ggplot2::scale_shape_manual(
      name = "Displacement interpretable",
      values = c("TRUE" = 16, "FALSE" = 1),
      limits = c("TRUE", "FALSE"),
      drop = FALSE
    ) +
    # drop = FALSE keeps a requested parameter's panel visible even when every
    # one of its occasions was dropped, rather than letting it vanish silently.
    ggplot2::facet_wrap(~Panel, scales = "free_y", drop = FALSE) +
    ggplot2::labs(
      x = "Occasion",
      y = NULL,
      caption = paste(
        "Displacement is shown on an unwrapped branch and may fall",
        "outside [0, 360)."
      )
    ) +
    # The shape keys carry no group identity, so pin them to black; inheriting
    # the colour aesthetic leaves the hollow key effectively invisible once a
    # grouping supplies pale series colours.
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(color = "black", linetype = 0)
      )
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (!grouped) {
    # A single ungrouped series carries no information in a colour legend, and
    # a hue that encodes nothing invites reading one into it -- draw it black,
    # as the package's other Cartesian plots do.
    p <- p +
      ggplot2::scale_color_manual(values = "black") +
      ggplot2::scale_fill_manual(values = "black") +
      ggplot2::guides(color = "none", fill = "none")
  }

  p
}
