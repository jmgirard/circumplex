# Polar-native ggplot2 layers for circumplex space ----------------------------
# These geoms accept Structural Summary Method parameters (amplitude and
# displacement) as aesthetics and hand them to coord_circumplex(), which owns
# the amplitude->radius scaling and the displacement->angle polar transform.
# geom_ssm_point() emits its amplitude/displacement as the coord's y/x; the
# coord bends them onto the circular canvas. geom_ssm_arc() emits one rectangle
# in (displacement, amplitude) space per profile, which the polar coord bends
# into an annular wedge -- a displacement interval that straddles the 0/360 seam
# is unwrapped by extension (xmax = xmin + span, possibly > 360) so the coord's
# periodic transform carries it the short way across the pole.

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
#' @param na.rm Ignored; profiles with a missing displacement or amplitude
#'   (degenerate profiles) are always dropped, since they have no location.
#' @return A \pkg{ggplot2} layer.
#' @seealso [coord_circumplex()], [ggcircumplex()], [geom_ssm_arc()]
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
    data <- data[ssm_has_location(data$amplitude, data$displacement), ]
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
#' @param na.rm Ignored; profiles with a missing displacement or amplitude
#'   bound (degenerate profiles) are always dropped.
#' @return A \pkg{ggplot2} layer.
#' @seealso [coord_circumplex()], [ggcircumplex()], [geom_ssm_point()]
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

GeomSsmArc <- ggplot2::ggproto(
  "GeomSsmArc", ggplot2::GeomRect,
  required_aes = c(
    "amplitude_min", "amplitude_max", "displacement_min", "displacement_max"
  ),
  setup_data = function(data, params) {
    # Drop rows without a complete CI region (one predicate, shared with the
    # point geom and the plot-level callers).
    data <- data[ssm_has_region(
      data$amplitude_min, data$amplitude_max,
      data$displacement_min, data$displacement_max
    ), ]
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
