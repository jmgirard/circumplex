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

# Parameter triples a supplied trajectory table must carry. Amplitude and
# displacement are what the unwrap, the D-007 certification gate, and the
# displacement panel are computed from; elevation and the coordinates are
# optional extra panels.
ssm_trajectory_required <- c("a", "d")

# Column names a time column may not take. Two groups, both of which would be
# silently overwritten rather than honoured: the names ssm_trajectory_long()
# puts in its output, and the input's own parameter/verdict columns -- the time
# column is placed into `dat` BEFORE the parameter loop, so a time column named
# `a_est` is clobbered by the amplitude values and the figure draws a
# meaningless diagonal with no error. Refuse the collision rather than perform
# it (caught by review, M35).
ssm_trajectory_reserved <- function() {
  suffixes <- c("_est", "_lci", "_uci")
  c(
    "Group", "Parameter", "est", "lci", "uci", "Certified", "Panel",
    as.vector(t(outer(names(ssm_trajectory_panels()), suffixes, paste0))),
    "certified"
  )
}

# Validate a user-supplied per-time-point trajectory table -- the shape a
# model-based workflow assembles from ssm_draws() evaluated at each time point
# -- and hand it to ssm_trajectory_long() in the same `dat` shape the occasions
# path uses. Validation is deliberately loud and specific: every failure here is
# one that would otherwise render a wrong figure without erroring.
ssm_trajectory_table_frame <- function(x, time, drop_xy = FALSE) {
  suffixes <- c("_est", "_lci", "_uci")

  if (!is_char(time, n = 1) || is.na(time)) {
    stop("`time` must be a single string naming the time column.", call. = FALSE)
  }
  # Duplicated names are a wrong-answer channel, not an error: `[.data.frame`
  # resolves a duplicated name to the FIRST match, so a second `d_est` column
  # would be silently ignored rather than flagged.
  if (anyDuplicated(names(x))) {
    stop(
      sprintf(
        "The trajectory table has duplicated column name(s): %s.",
        paste(unique(names(x)[duplicated(names(x))]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!time %in% names(x)) {
    stop(
      sprintf(
        "`time` column \"%s\" was not found. Available columns: %s.",
        time, paste(names(x), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (time %in% ssm_trajectory_reserved()) {
    stop(
      sprintf(
        paste(
          "`time` column \"%s\" collides with a name the trajectory table or",
          "the plot data already uses; rename the time column."
        ),
        time
      ),
      call. = FALSE
    )
  }

  # Which parameters the table can fill a panel for. A parameter needs all
  # three of its columns: a half-supplied triple is a mistake, not a request
  # for a partial panel.
  params <- names(ssm_trajectory_panels())
  n_have <- vapply(
    params, function(p) sum(paste0(p, suffixes) %in% names(x)), integer(1)
  )
  present <- params[n_have == 3L]

  missing_req <- setdiff(ssm_trajectory_required, present)
  if (length(missing_req) > 0) {
    want <- as.vector(t(outer(missing_req, suffixes, paste0)))
    stop(
      sprintf(
        paste(
          "A trajectory table must carry amplitude and displacement estimates",
          "with their bounds. Missing column(s): %s."
        ),
        paste(setdiff(want, names(x)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  partial <- setdiff(params[n_have > 0L & n_have < 3L], ssm_trajectory_required)
  if (length(partial) > 0) {
    want <- as.vector(t(outer(partial, suffixes, paste0)))
    stop(
      sprintf(
        paste(
          "Parameter column(s) %s are incomplete: each parameter needs all of",
          "_est, _lci, and _uci. Missing: %s."
        ),
        paste(partial, collapse = ", "),
        paste(setdiff(want, names(x)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  tv <- x[[time]]
  if (!is.numeric(tv)) {
    stop(
      sprintf(
        paste(
          "`time` column \"%s\" must be numeric, not %s. A trajectory table is",
          "plotted on a continuous time axis; for ordered occasions pass the",
          "SSM object from `ssm_analyze(occasions = )` instead."
        ),
        time, class(tv)[[1]]
      ),
      call. = FALSE
    )
  }
  tv <- as.numeric(tv)
  if (!all(is.finite(tv))) {
    stop(
      sprintf("`time` column \"%s\" must be finite (no NA, NaN, or Inf).", time),
      call. = FALSE
    )
  }
  if (length(unique(tv)) < 2L) {
    stop(
      sprintf(
        "`time` column \"%s\" needs at least two distinct time points; found %d.",
        time, length(unique(tv))
      ),
      call. = FALSE
    )
  }
  if (anyDuplicated(tv)) {
    stop(
      sprintf(
        paste(
          "`time` column \"%s\" has repeated value(s): %s. A trajectory table",
          "carries one row per time point."
        ),
        time, paste(unique(tv[duplicated(tv)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dat <- data.frame(Group = factor(rep("1", nrow(x))))
  dat[[time]] <- tv
  for (p in present) {
    for (s in suffixes) {
      col <- paste0(p, s)
      v <- x[[col]]
      if (!is.numeric(v)) {
        stop(
          sprintf("Column `%s` must be numeric, not %s.", col, class(v)[[1]]),
          call. = FALSE
        )
      }
      dat[[col]] <- as.numeric(v)
    }
    # An estimate is either a real number or missing -- never infinite.
    # is.na(Inf) is FALSE, so an infinite estimate slips past the NA-based
    # ssm_has_location() predicate, reaches `Inf %% 360` -> NaN in the unwrap,
    # and cumsum() then propagates that NaN over every LATER time point,
    # silently blanking the rest of a perfectly good series. Guard with
    # !is.finite(), never is.na() (LESSONS M32; caught by review, M35). NaN is
    # left to read as missing, since is.na(NaN) is TRUE.
    est <- dat[[paste0(p, "_est")]]
    bad_est <- !is.na(est) & !is.finite(est)
    if (any(bad_est)) {
      stop(
        sprintf(
          "Column `%s_est` is not finite at %d row(s); an estimate must be a number or NA.",
          p, sum(bad_est)
        ),
        call. = FALSE
      )
    }

    # A bound is allowed to be missing only where its estimate is: an
    # undefined time point leaves a gap, but a defined one with a broken
    # interval would draw a ribbon that silently loses that row.
    est_ok <- is.finite(est)
    for (s in c("_lci", "_uci")) {
      bad <- est_ok & !is.finite(dat[[paste0(p, s)]])
      if (any(bad)) {
        stop(
          sprintf(
            "Column `%s%s` is not finite at %d row(s) where `%s_est` is defined.",
            p, s, sum(bad), p
          ),
          call. = FALSE
        )
      }
    }
  }

  # The D-007 verdict is the caller's to supply -- it is a property of how the
  # draws were made, not something recoverable from the table. Absent, the plot
  # makes no interpretability claim at all rather than asserting one the data
  # never carried.
  cert <- rep(NA, nrow(x))
  if ("certified" %in% names(x)) {
    if (!is.logical(x[["certified"]])) {
      stop(
        "Column `certified` must be logical (TRUE/FALSE/NA).",
        call. = FALSE
      )
    }
    cert <- x[["certified"]]
  }
  dat$Certified <- as.logical(cert)

  ssm_trajectory_long(dat, time_col = time, drop_xy = drop_xy)
}

#' Create a Trajectory Plot of SSM Results Over Time
#'
#' Plot each Structural Summary Method parameter against time, one facet per
#' parameter, with its confidence interval as a band. This is a Cartesian
#' diagnostic plot, not a circumplex figure: the horizontal axis is time, not
#' angle.
#'
#' Two kinds of input are accepted, and they differ only in their time axis:
#'
#' * An **SSM results object** with occasions, from [ssm_analyze()] with the
#'   `occasions` argument or from [ssm_analyze_long()]. Occasions are discrete
#'   and ordered, so the axis is discrete.
#' * A **trajectory table**: a data frame with one row per time point, a numeric
#'   time column named by `time`, and the columns `a_est`, `a_lci`, `a_uci`,
#'   `d_est`, `d_lci`, and `d_uci` (optionally the `e_*`, `x_*`, and `y_*`
#'   triples, and a logical `certified` column). This is the shape a
#'   model-based workflow assembles by evaluating a fitted growth model at each
#'   time point and passing the draws through [ssm_draws()]; see
#'   `vignette("growth-ssm-analysis")`. The axis is continuous, so unequally
#'   spaced time points are drawn at their actual spacing.
#'
#' Both paths share one implementation of the displacement unwrap and the
#' certification marking described below.
#'
#' The displacement panel is drawn on an *unwrapped* branch, so a profile whose
#' displacement crosses the 0/360 boundary renders as one continuous path rather
#' than jumping a full turn. Values on that panel may therefore fall outside
#' \[0, 360); each confidence bound is placed at its signed angular distance
#' from its own estimate. Unwrapping assumes the profile rotates less than a
#' half-turn between consecutive time points at which its displacement is
#' defined -- no data can verify this, so time points that are far apart, or a
#' series with a gap, should be read with that in mind.
#'
#' Occasions appear in the order they were supplied to [ssm_analyze()] (or in
#' the occasion factor's level order for [ssm_analyze_long()]), never in
#' alphabetical order.
#'
#' On the displacement panel, a time point whose amplitude confidence interval
#' is too close to zero for its displacement to be interpretable is drawn as a
#' hollow point; see [ssm_analyze()] for the certification rule. For an SSM
#' object the verdict is computed from the amplitude interval; for a trajectory
#' table it is read from the optional `certified` column, and when that column
#' is absent no interpretability claim is made or shown. A profile with no
#' defined displacement at all (a flat profile) leaves a gap in that panel.
#'
#' A contrast row is never plotted as a time point -- it is a difference, not a
#' time point. Use [ssm_plot_contrast()] for it.
#'
#' @param x An SSM results object produced by [ssm_analyze()] with the
#'   `occasions` argument or by [ssm_analyze_long()], or a trajectory table
#'   (a data frame) as described above.
#' @param time A string naming the numeric time column of a trajectory table.
#'   Required for the data frame method; unused for SSM objects.
#' @param drop_xy A logical determining whether the X-value and Y-value panels
#'   should be omitted (default = `FALSE`), leaving elevation, amplitude, and
#'   displacement.
#' @param base_size A positive number determining the base font size of the plot
#'   (default = 11).
#' @param na.rm A logical determining whether time points that cannot be plotted
#'   (no defined displacement) are dropped silently (default = `TRUE`) or with a
#'   warning naming how many were removed (`FALSE`).
#' @param ... Not used. Supplying an unrecognized argument produces a warning.
#' @return A ggplot object depicting each SSM parameter's trajectory over time,
#'   with confidence bands.
#' @family visualization functions
#' @seealso [geom_ssm_path()] and `ssm_plot_circle(path = TRUE)`, which draw the
#'   same change across occasions as movement on the circumplex canvas rather
#'   than as parameter-by-time panels.
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
#'
#' # A model-based trajectory table, plotted on a continuous time axis
#' trajectory <- data.frame(
#'   wave = 0:4,
#'   a_est = c(0.60, 0.55, 0.52, 0.58, 0.63),
#'   a_lci = c(0.48, 0.43, 0.40, 0.46, 0.51),
#'   a_uci = c(0.72, 0.67, 0.64, 0.70, 0.75),
#'   d_est = c(350, 355, 2, 8, 12),
#'   d_lci = c(340, 345, 352, 358, 2),
#'   d_uci = c(0, 5, 12, 18, 22),
#'   certified = c(TRUE, TRUE, FALSE, TRUE, TRUE)
#' )
#' ssm_plot_trajectory(trajectory, time = "wave")
ssm_plot_trajectory <- function(x, ...) {
  UseMethod("ssm_plot_trajectory")
}

#' @rdname ssm_plot_trajectory
#' @export
ssm_plot_trajectory.default <- function(x, ...) {
  stop(
    "`ssm_plot_trajectory()` needs an SSM results object with occasions or a ",
    "trajectory table (a data frame), not ", class(x)[[1]], ". ",
    "See `?ssm_plot_trajectory` for the trajectory table's columns.",
    call. = FALSE
  )
}

# Shared argument checks for both methods. base_size uses !is.finite() rather
# than is.na(): an infinite base_size slips past an is.na() guard and only
# surfaces as a cryptic error during render, never naming this argument (M32).
ssm_trajectory_check_args <- function(drop_xy, base_size, na.rm) {
  stopifnot(is_flag(drop_xy), !is.na(drop_xy))
  stopifnot(is_flag(na.rm), !is.na(na.rm))
  stopifnot(is_num(base_size, n = 1))
  if (!is.finite(base_size) || base_size <= 0) {
    stop("`base_size` must be a single positive finite number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @rdname ssm_plot_trajectory
#' @export
ssm_plot_trajectory.circumplex_ssm <- function(x,
                                               drop_xy = FALSE,
                                               base_size = 11,
                                               na.rm = TRUE,
                                               ...) {
  if (is.null(x$details$occasions)) {
    stop(
      "This SSM object contains no occasions to plot a trajectory across; ",
      "produce one with `ssm_analyze(occasions = )` or `ssm_analyze_long()`. ",
      "For a single-occasion profile see `ssm_plot_circle()`.",
      call. = FALSE
    )
  }
  chkDots(...)
  ssm_trajectory_check_args(drop_xy, base_size, na.rm)

  df <- ssm_trajectory_frame(x, drop_xy = drop_xy)
  ssm_trajectory_ggplot(df, "Occasion", "Occasion", base_size, na.rm)
}

#' @rdname ssm_plot_trajectory
#' @export
ssm_plot_trajectory.data.frame <- function(x,
                                           time,
                                           drop_xy = FALSE,
                                           base_size = 11,
                                           na.rm = TRUE,
                                           ...) {
  if (missing(time)) {
    stop(
      "`time` must name the trajectory table's numeric time column, e.g. ",
      "`ssm_plot_trajectory(trajectory, time = \"wave\")`.",
      call. = FALSE
    )
  }
  chkDots(...)
  ssm_trajectory_check_args(drop_xy, base_size, na.rm)

  df <- ssm_trajectory_table_frame(x, time = time, drop_xy = drop_xy)
  ssm_trajectory_ggplot(df, time, time, base_size, na.rm)
}

# Draw the long per-panel frame. Shared by both methods: the time column's class
# is what makes the axis discrete (occasions) or continuous (a table), so the
# two paths need no branch here.
ssm_trajectory_ggplot <- function(df, time_col, xlab, base_size, na.rm) {
  # Time points that cannot be placed (a flat profile has no displacement). The
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
  # A trajectory table may carry no certification verdict at all. Marking every
  # point solid would assert an interpretability claim the data never made, so
  # the shape aesthetic and its legend are simply absent instead.
  show_cert <- any(!is.na(df$Certified))
  drawn <- df[!is.na(df$est), , drop = FALSE]
  d_rows <- drawn[drawn$Parameter == "d", , drop = FALSE]
  other_rows <- drawn[drawn$Parameter != "d", , drop = FALSE]

  p <-
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data[[time_col]],
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
    # drop = FALSE keeps a requested parameter's panel visible even when every
    # one of its time points was dropped, rather than letting it vanish silently.
    ggplot2::facet_wrap(~Panel, scales = "free_y", drop = FALSE) +
    ggplot2::labs(
      x = xlab,
      y = NULL,
      caption = paste(
        "Displacement is shown on an unwrapped branch and may fall",
        "outside [0, 360)."
      )
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      # scales = "free_y" gives each panel its own interior y-axis, so the
      # Amplitude and Displacement panels' tick labels sit in the gutter to
      # their left and crowd the neighbouring panel at vignette width. Widen
      # the horizontal panel gap so the labels clear it (M50).
      panel.spacing.x = grid::unit(1.2, "lines")
    )

  if (show_cert) {
    # Points are split by panel so certification -- a displacement-only
    # guardrail -- marks only where it applies, instead of implying every
    # parameter carries an interpretability verdict.
    p <- p +
      ggplot2::geom_point(data = other_rows, size = 2, na.rm = TRUE) +
      ggplot2::geom_point(
        data = d_rows,
        mapping = ggplot2::aes(shape = .data$Certified),
        size = 2,
        na.rm = TRUE,
        # Under the default (NA), ggplot2 drops a key's GLYPH for any break the
        # layer's own data does not contain: a trajectory with nothing
        # uncertified rendered the FALSE key as a label with no symbol beside
        # it -- a legend naming an encoding it never showed. show.legend = TRUE
        # makes the layer claim every break the scale defines, so the hollow
        # key is drawn whether or not an uncertified occasion happens to exist.
        # Keeping the break alive with drop = FALSE is necessary but not
        # sufficient, and an `override.aes` shape vector does not reach the key.
        show.legend = TRUE
      ) +
      ggplot2::scale_shape_manual(
        name = "Displacement interpretable",
        values = c("TRUE" = 16, "FALSE" = 1),
        limits = c("TRUE", "FALSE"),
        drop = FALSE
      ) +
      # The shape keys carry no group identity, so pin them to black; inheriting
      # the colour aesthetic leaves the hollow key effectively invisible once a
      # grouping supplies pale series colours.
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          override.aes = list(color = "black", linetype = 0)
        )
      )
  } else {
    p <- p + ggplot2::geom_point(data = drawn, size = 2, na.rm = TRUE)
  }

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
