# Polar-native ggplot2 layers for circumplex space ----------------------------
# These geoms accept Structural Summary Method parameters (amplitude and
# displacement) as aesthetics and hand them to coord_circumplex(), which owns
# the amplitude->radius scaling and the displacement->angle polar transform.
# geom_ssm_point() emits its amplitude/displacement as the coord's y/x; the
# coord bends them onto the circular canvas. geom_ssm_arc() emits one rectangle
# in (displacement, amplitude) space per profile, which the polar coord bends
# into an annular wedge -- a displacement interval that straddles the 0/360 seam
# is unwrapped by extension (xmax = xmin + span, possibly > 360) so the coord's
# periodic transform carries it the short way across the pole. geom_ssm_path()
# connects one profile's successive occasions, unwrapping the whole series onto
# a continuous branch for the same reason; the coord's non-linear munching is
# what curves each segment along the polar geodesic.

# Plottability predicates for the circular canvas (ROADMAP viz-robustness
# track). A profile has a *location* -- a point can be drawn -- iff its
# amplitude and displacement are both defined. It has a *region* -- a CI wedge
# can be drawn -- iff all four interval bounds are defined too. GeomSsmPoint,
# GeomSsmArc, and the plot-level callers (ssm_plot_circle(),
# plot.circumplex_cpm()) all classify rows through these two helpers so their
# NA handling agrees, instead of each rolling a slightly different criterion
# (which used to render a point with a defined estimate but an undefined CI as
# a point with no wedge and no message).
ssm_has_location <- function(amplitude, displacement) {
  !is.na(amplitude) & !is.na(displacement)
}
ssm_has_region <- function(amplitude_min, amplitude_max,
                           displacement_min, displacement_max) {
  !is.na(amplitude_min) & !is.na(amplitude_max) &
    !is.na(displacement_min) & !is.na(displacement_max)
}

# Counterclockwise angular span (degrees) an arc covers from displacement_min
# to displacement_max, unwrapping a min > max pair across the 0/360 seam (the
# package's CI convention stores a straddling interval that way; DESIGN.md).
# A proper interval has a span in [0, 360); anything else (bounds outside
# [0, 360] -- 360 is the pole's LM = 360 label (M20) -- or reversed the long
# way) does not name a unique arc. Shared by GeomSsmArc's geometry/validation
# and plot.circumplex_cpm()'s pre-filter.
ssm_arc_span <- function(displacement_min, displacement_max) {
  upper <- ifelse(
    displacement_max < displacement_min,
    displacement_max + 360, displacement_max
  )
  upper - displacement_min
}

# Opt-in ggplot2 na.rm warn-parity. The geoms always drop rows that cannot be
# placed (no location / no region); this makes that drop *speak* when the caller
# opts out of silent removal. na.rm = TRUE (the geom default) stays silent -- no
# new warnings in existing plots, and ssm_plot_circle() keeps naming dropped
# profiles itself; na.rm = FALSE warns once with the dropped-row count before the
# drop, matching the ggplot2 convention. Scoped to missing/incomplete (NA) rows,
# the ggplot2 sense of "missing values"; a complete-but-zero-width arc is a
# separate geometry rule and stays silent.
ssm_warn_dropped <- function(n_dropped, na.rm, fn, what) {
  if (n_dropped > 0L && isFALSE(na.rm)) {
    warning(
      sprintf(
        "Removed %d row%s with %s (`%s()`). Use `na.rm = TRUE` to silence this.",
        n_dropped, if (n_dropped == 1L) "" else "s", what, fn
      ),
      call. = FALSE
    )
  }
}

# One-time soft-deprecation note for the retired per-layer `amax`/`n` geom
# arguments (M31): amplitude scaling and arc smoothness are now owned by
# coord_circumplex(). Unconditional (fires whenever the argument is supplied,
# once per session) and never an error -- an error would break the package's
# own documented examples (RR08 R-10).
ssm_deprecate_geom_arg <- function(value, arg, fn) {
  if (is.null(value)) return(invisible())
  detail <- if (arg == "amax") {
    "Amplitude scaling is now owned by `coord_circumplex()`; set `amax` there (or via `ggcircumplex(amax = )`)."
  } else {
    "Arc smoothness is now owned by `coord_circumplex()`, which curves the wedge automatically."
  }
  rlang::inform(
    c(
      "!" = sprintf("The `%s` argument of `%s()` is deprecated and ignored.", arg, fn),
      "i" = detail
    ),
    .frequency = "once",
    .frequency_id = sprintf("circumplex_geom_%s_%s", fn, arg)
  )
}

#' Draw SSM profile points in circumplex space
#'
#' A \pkg{ggplot2} layer that places a point for each profile at its amplitude
#' and displacement on a circumplex canvas built with [coord_circumplex()] (for
#' example the canvas from [ggcircumplex()]). The amplitude and displacement are
#' supplied directly in SSM units (amplitude in the score metric, displacement
#' in degrees); the coordinate system performs the polar transform.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,... Standard
#'   \pkg{ggplot2} layer arguments. `mapping` must supply the `amplitude` and
#'   `displacement` aesthetics.
#' @param amax (Deprecated) The amplitude represented by
#'   the outer ring is now owned by [coord_circumplex()]; a value supplied here
#'   is ignored with a one-time note.
#' @param na.rm If `FALSE`, warn (with the dropped-row count) before removing
#'   profiles with a missing displacement or amplitude, since they have no
#'   location on the circle; if `TRUE` (the default) remove them silently.
#' @return A \pkg{ggplot2} layer.
#' @family circumplex layers
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' ggcircumplex(octants(), amax = 0.5) +
#'   geom_ssm_point(
#'     data = res$results,
#'     mapping = ggplot2::aes(amplitude = a_est, displacement = d_est)
#'   )
geom_ssm_point <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., amax = NULL,
                           na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE) {
  ssm_deprecate_geom_arg(amax, "amax", "geom_ssm_point")
  ggplot2::layer(
    geom = GeomSsmPoint, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Circumplex ggproto classes
#'
#' These are the \pkg{ggplot2} [ggplot2::ggproto()] classes that back the
#' circumplex layers and coordinate system: `GeomSsmPoint` (the profile-point
#' geom), `GeomSsmArc` (the confidence-region arc geom), `GeomSsmPath` (the
#' movement-path geom), and `CoordCircumplex` (the coordinate system). They are
#' exported so that downstream packages can subclass them to build custom
#' circumplex layers; most users should use the [geom_ssm_point()],
#' [geom_ssm_arc()], [geom_ssm_path()], and [coord_circumplex()] constructors
#' instead.
#'
#' @seealso [geom_ssm_point()], [geom_ssm_arc()], [geom_ssm_path()],
#'   [coord_circumplex()]
#' @name circumplex-ggproto
#' @keywords internal
NULL

#' @rdname circumplex-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSsmPoint <- ggplot2::ggproto(
  "GeomSsmPoint", ggplot2::GeomPoint,
  required_aes = c("amplitude", "displacement"),
  default_aes = utils::modifyList(
    ggplot2::GeomPoint$default_aes,
    ggplot2::aes(shape = 21, size = 3, colour = "black", fill = "grey50")
  ),
  setup_data = function(data, params) {
    # Drop profiles with no location, then hand amplitude/displacement to the
    # coord as y/x (the coord owns the polar transform; no cartesian math here).
    keep <- ssm_has_location(data$amplitude, data$displacement)
    ssm_warn_dropped(
      sum(!keep), params$na.rm, "geom_ssm_point",
      "a missing amplitude or displacement"
    )
    data <- data[keep, ]
    data$x <- data$displacement
    data$y <- data$amplitude
    data
  }
)

#' Draw a profile's movement across occasions in circumplex space
#'
#' A \pkg{ggplot2} layer that connects a profile's successive positions on a
#' circumplex canvas built with [coord_circumplex()] (for example the canvas
#' from [ggcircumplex()]), so change in amplitude and displacement reads as
#' movement through circumplex space. Each segment is curved along the polar
#' geodesic by the coordinate system, which owns the transform; the layer owns
#' the ordering, the 0/360 seam handling, and the optional arrowheads.
#'
#' Points are connected in the order the rows appear in the data, exactly as
#' [ggplot2::geom_path()] does, and the `group` aesthetic separates one series
#' from another. Supplying the optional `order` aesthetic sorts the rows within
#' each group before drawing, which is the safer choice when the data are
#' assembled by hand: an occasion label sorted as text puts `T10` before `T2`
#' and silently reverses time.
#'
#' Consecutive occasions are joined the **short** way around the circle. The
#' displacements of each group are unwrapped onto a continuous branch before the
#' coordinate system sees them, so a step from `350` to `10` degrees is drawn as
#' the 20 degree arc across the pole rather than a 340 degree sweep the long way
#' round. Unwrapped values may therefore fall outside `[0, 360)`. This assumes
#' the profile rotates less than a half-turn between consecutive occasions at
#' which its displacement is defined; no data can verify that, so widely spaced
#' occasions should be read with it in mind.
#'
#' An occasion with no defined location -- a flat or zero-amplitude profile,
#' whose displacement is undefined -- **breaks** the path rather than being
#' interpolated through, and the segment after the gap is still drawn on the
#' correct branch. Non-finite amplitudes and displacements are treated the same
#' way, since an infinite angle names no position on the circle.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,... Standard
#'   \pkg{ggplot2} layer arguments. `mapping` must supply the `amplitude` and
#'   `displacement` aesthetics, and may supply `order`.
#' @param arrow An arrow specification produced by [ggplot2::arrow()], or `NULL`
#'   (the default) for a path drawn without arrowheads. Arrowheads mark the
#'   direction of time along the path.
#' @param na.rm If `FALSE`, warn when occasions with no location are removed
#'   from the ends of a path; if `TRUE` (the default) remove them silently.
#'   Occasions with no location in the *middle* of a path break it either way.
#' @return A \pkg{ggplot2} layer.
#' @family circumplex layers
#' @export
#' @examples
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
#' ggcircumplex(octants(), amax = 0.5) +
#'   geom_ssm_path(
#'     data = res$results,
#'     mapping = ggplot2::aes(amplitude = a_est, displacement = d_est),
#'     arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))
#'   )
geom_ssm_path <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., arrow = NULL,
                          na.rm = TRUE, show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomSsmPath, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(arrow = arrow, na.rm = na.rm, ...)
  )
}

#' @rdname circumplex-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSsmPath <- ggplot2::ggproto(
  "GeomSsmPath", ggplot2::GeomPath,
  required_aes = c("amplitude", "displacement"),
  optional_aes = "order",
  setup_data = function(data, params) {
    # Non-finite guard, BEFORE the unwrap. ssm_has_location() is an is.na()
    # test, and is.na(Inf) is FALSE -- an infinite displacement would sail
    # through it into angle_unwrap()'s cumsum() and NaN out every later
    # occasion in the series, blanking the path from that point on with no
    # error (the recurring M32/M35 trap). An infinite amplitude or displacement
    # names no position on the circle, so it is demoted to NA here and then
    # handled by exactly the same gap machinery as a flat profile.
    unplottable <- !is.finite(data$amplitude) | !is.finite(data$displacement)
    data$amplitude[unplottable] <- NA_real_
    data$displacement[unplottable] <- NA_real_

    if (is.null(data$group)) data$group <- -1L

    # The `order` aesthetic, when supplied, sorts within each series. order()
    # is stable, so rows tied on `order` keep their data order, and sorting by
    # group first keeps each series contiguous (GeomPath sorts by group again
    # at draw time, also stably, so within-series order survives).
    if (!is.null(data$order)) {
      data <- data[order(data$group, data$order), , drop = FALSE]
    }

    # Unwrap each series onto a continuous branch, in row order, so the coord's
    # periodic transform carries each step the short way across the 0/360 seam.
    # ssm_unwrap_gapped() (R/ssm_trajectory.R) bridges an undefined occasion
    # instead of propagating NA onward: the gap stays a gap, and the occasions
    # after it are unwrapped relative to the last defined one rather than
    # being blanked. Per group via split(seq_len()) so row order is untouched.
    for (i in split(seq_len(nrow(data)), data$group)) {
      data$displacement[i] <- ssm_unwrap_gapped(data$displacement[i])
    }

    # Hand amplitude/displacement to the coord as y/x. The NA rows are left in
    # place: GeomPath$handle_na() trims them from the ends of each series and
    # keeps the interior ones, which is what breaks the line at a gap. That is
    # also where `na.rm` speaks, so this geom does not warn a second time.
    data$x <- data$displacement
    data$y <- data$amplitude
    data
  }
)

#' Draw SSM confidence-region arcs in circumplex space
#'
#' A \pkg{ggplot2} layer that draws, for each profile, the wedge spanning its
#' amplitude confidence interval (radially) and its displacement confidence
#' interval (angularly) on a circumplex canvas built with [coord_circumplex()]
#' (for example the canvas from [ggcircumplex()]). The bounds are supplied
#' directly in SSM units; the coordinate system bends the (displacement,
#' amplitude) rectangle into an annular wedge.
#'
#' Each arc spans **counterclockwise** from `displacement_min` to
#' `displacement_max` (both in degrees). Supply them in `[0, 360]` (a bound of
#' exactly 360 is the 0/360 pole under the package's LM = 360 labeling). A
#' `displacement_min` greater than `displacement_max` is read as an interval
#' that crosses the 0/360 seam and is drawn the short way across it (e.g.
#' `350 -> 10` is a 20 degree arc, matching how the package stores a
#' displacement CI that straddles the boundary). The interval must describe
#' less than a full circle; bounds that imply a span of 360 degrees or more
#' (for example, values outside `[0, 360]`) are rejected, since they do not
#' name a unique arc.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,... Standard
#'   \pkg{ggplot2} layer arguments. `mapping` must supply the `amplitude_min`,
#'   `amplitude_max`, `displacement_min`, and `displacement_max` aesthetics.
#' @param amax (Deprecated) The amplitude represented by
#'   the outer ring is now owned by [coord_circumplex()]; a value supplied here
#'   is ignored with a one-time note.
#' @param n (Deprecated) Arc smoothness is now owned by the
#'   coordinate system, which curves the wedge automatically; a value supplied
#'   here is ignored with a one-time note.
#' @param na.rm If `FALSE`, warn (with the dropped-row count) before removing
#'   profiles with an incomplete confidence region (a missing amplitude or
#'   displacement bound); if `TRUE` (the default) remove them silently.
#' @return A \pkg{ggplot2} layer.
#' @family circumplex layers
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' ggcircumplex(octants(), amax = 0.5) +
#'   geom_ssm_arc(
#'     data = res$results,
#'     mapping = ggplot2::aes(
#'       amplitude_min = a_lci, amplitude_max = a_uci,
#'       displacement_min = d_lci, displacement_max = d_uci
#'     ),
#'     alpha = 0.4
#'   )
geom_ssm_arc <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", ..., amax = NULL, n = NULL,
                         na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE) {
  ssm_deprecate_geom_arg(amax, "amax", "geom_ssm_arc")
  ssm_deprecate_geom_arg(n, "n", "geom_ssm_arc")
  ggplot2::layer(
    geom = GeomSsmArc, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname circumplex-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSsmArc <- ggplot2::ggproto(
  "GeomSsmArc", ggplot2::GeomRect,
  required_aes = c(
    "amplitude_min", "amplitude_max", "displacement_min", "displacement_max"
  ),
  setup_data = function(data, params) {
    # Drop rows without a complete CI region (one predicate, shared with the
    # point geom and the plot-level callers).
    keep_region <- ssm_has_region(
      data$amplitude_min, data$amplitude_max,
      data$displacement_min, data$displacement_max
    )
    ssm_warn_dropped(
      sum(!keep_region), params$na.rm, "geom_ssm_arc",
      "an incomplete confidence region"
    )
    data <- data[keep_region, ]
    # Unwrap a displacement interval that crosses the 0/360 seam by *extension*:
    # xmax = xmin + span may exceed 360, and the polar coord's periodic
    # transform carries it the short way across the pole. Range must stay
    # coord-side (thetalim) -- an x-scale limit would censor xmax > 360 to NA.
    span <- ssm_arc_span(data$displacement_min, data$displacement_max)
    if (any(!is.finite(span) | span < 0 | span >= 360)) {
      stop(
        "geom_ssm_arc(): each displacement interval must span less than a ",
        "full circle. Supply displacement_min and displacement_max in ",
        "[0, 360]; a min greater than max is read as an interval crossing ",
        "the 0/360 seam.",
        call. = FALSE
      )
    }
    # A zero-width interval (span == 0) names no wedge; drop it so the coord
    # does not draw a degenerate radial line (matches the old arc-bar behavior;
    # plot.circumplex_cpm()'s reference scale relies on this).
    keep <- span > 0
    data <- data[keep, ]
    span <- span[keep]
    data$xmin <- data$displacement_min
    data$xmax <- data$displacement_min + span
    data$ymin <- data$amplitude_min
    data$ymax <- data$amplitude_max
    data
  }
)
