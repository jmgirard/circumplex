# Polar-native ggplot2 layers for circumplex space ----------------------------
# These geoms accept Structural Summary Method parameters (amplitude and
# displacement) as aesthetics and internalize the transform from that native
# SSM space onto the circular canvas drawn by ggcircumplex(): amplitude is
# rescaled so the canvas's outer ring (radius 5) corresponds to amax, and
# displacement (degrees, counterclockwise from the right) is converted to the
# ggforce arc convention via ggrad(). The same transform previously lived
# inline in ssm_plot_circle().

# Rescale an amplitude to canvas radius (outer ring at radius 5 = amax)
ssm_radius <- function(amplitude, amax) amplitude * 5 / amax

# Convert SSM parameters (amplitude, and displacement in degrees) to canvas
# x/y coordinates. Shared by GeomSsmPoint$setup_data() and ssm_plot_circle()'s
# repel branch so the polar->canvas transform lives in exactly one place.
ssm_to_cartesian <- function(amplitude, displacement, amax) {
  r <- ssm_radius(amplitude, amax)
  rad <- displacement * pi / 180
  list(x = r * cos(rad), y = r * sin(rad))
}

# Plottability predicates for the circular canvas (ROADMAP viz-robustness
# track). A profile has a *location* -- a point can be drawn -- iff its
# amplitude and displacement are both defined. It has a *region* -- a CI wedge
# can be drawn -- iff all four interval bounds are defined too. GeomSsmPoint,
# StatSsmArc, and the plot-level callers (ssm_plot_circle(),
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
# [0, 360), or reversed the long way) does not name a unique arc. Shared by
# StatSsmArc's geometry/validation and plot.circumplex_cpm()'s pre-filter.
ssm_arc_span <- function(displacement_min, displacement_max) {
  upper <- ifelse(
    displacement_max < displacement_min,
    displacement_max + 360, displacement_max
  )
  upper - displacement_min
}

#' Draw SSM profile points in circumplex space
#'
#' A \pkg{ggplot2} layer that places a point for each profile at its
#' amplitude and displacement, on the canvas produced by [ggcircumplex()].
#' The amplitude/displacement-to-canvas transform is handled internally, so
#' the `amplitude` and `displacement` aesthetics are supplied directly in SSM
#' units (amplitude in the score metric, displacement in degrees).
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,... Standard
#'   \pkg{ggplot2} layer arguments. `mapping` must supply the `amplitude` and
#'   `displacement` aesthetics.
#' @param amax A single positive number giving the amplitude represented by the
#'   canvas's outer ring; must match the `amax` used for [ggcircumplex()] so the
#'   points align with the amplitude gridlines (default = 0.5).
#' @param na.rm Ignored; profiles with a missing displacement or amplitude
#'   (degenerate profiles) are always dropped, since they have no location.
#' @return A \pkg{ggplot2} layer.
#' @seealso [ggcircumplex()], [geom_ssm_arc()]
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' amax <- 0.5
#' ggcircumplex(octants(), amax = amax) +
#'   geom_ssm_point(
#'     data = res$results,
#'     mapping = ggplot2::aes(amplitude = a_est, displacement = d_est),
#'     amax = amax
#'   )
geom_ssm_point <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., amax = 0.5,
                           na.rm = TRUE, show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomSsmPoint, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(amax = amax, na.rm = na.rm, ...)
  )
}

GeomSsmPoint <- ggplot2::ggproto(
  "GeomSsmPoint", ggplot2::GeomPoint,
  required_aes = c("amplitude", "displacement"),
  extra_params = c("na.rm", "amax"),
  default_aes = utils::modifyList(
    ggplot2::GeomPoint$default_aes,
    ggplot2::aes(shape = 21, size = 3, colour = "black", fill = "grey50")
  ),
  setup_data = function(data, params) {
    data <- data[ssm_has_location(data$amplitude, data$displacement), ]
    amax <- if (is.null(params$amax)) 0.5 else params$amax
    xy <- ssm_to_cartesian(data$amplitude, data$displacement, amax)
    data$x <- xy$x
    data$y <- xy$y
    data
  }
)

#' Draw SSM confidence-region arcs in circumplex space
#'
#' A \pkg{ggplot2} layer that draws, for each profile, the wedge spanning its
#' amplitude confidence interval (radially) and its displacement confidence
#' interval (angularly), on the canvas produced by [ggcircumplex()]. The
#' amplitude/displacement-to-canvas transform -- including the wrap-around when
#' a displacement interval crosses the 0/360 degree boundary -- is handled
#' internally, so the bounds are supplied directly in SSM units.
#'
#' Each arc spans **counterclockwise** from `displacement_min` to
#' `displacement_max` (both in degrees). Supply them in `[0, 360)`. A
#' `displacement_min` greater than `displacement_max` is read as an interval
#' that crosses the 0/360 seam and is drawn the short way across it (e.g.
#' `350 -> 10` is a 20 degree arc, matching how the package stores a
#' displacement CI that straddles the boundary). The interval must describe
#' less than a full circle; bounds that imply a span of 360 degrees or more
#' (for example, values outside `[0, 360)`) are rejected, since they do not
#' name a unique arc.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes,... Standard
#'   \pkg{ggplot2} layer arguments. `mapping` must supply the `amplitude_min`,
#'   `amplitude_max`, `displacement_min`, and `displacement_max` aesthetics.
#' @param amax A single positive number giving the amplitude represented by the
#'   canvas's outer ring; must match the `amax` used for [ggcircumplex()]
#'   (default = 0.5).
#' @param n The number of points used to draw each arc's curved edges (default
#'   = 360).
#' @param na.rm Ignored; profiles with a missing displacement or amplitude
#'   bound (degenerate profiles) are always dropped.
#' @return A \pkg{ggplot2} layer.
#' @seealso [ggcircumplex()], [geom_ssm_point()]
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' amax <- 0.5
#' ggcircumplex(octants(), amax = amax) +
#'   geom_ssm_arc(
#'     data = res$results,
#'     mapping = ggplot2::aes(
#'       amplitude_min = a_lci, amplitude_max = a_uci,
#'       displacement_min = d_lci, displacement_max = d_uci
#'     ),
#'     amax = amax, alpha = 0.4
#'   )
geom_ssm_arc <- function(mapping = NULL, data = NULL, stat = StatSsmArc,
                         position = "identity", ..., amax = 0.5, n = 360,
                         na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    geom = ggforce::GeomArcBar, stat = stat, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(amax = amax, n = n, na.rm = na.rm, ...)
  )
}

StatSsmArc <- ggplot2::ggproto(
  "StatSsmArc", ggforce::StatArcBar,
  required_aes = c(
    "amplitude_min", "amplitude_max", "displacement_min", "displacement_max"
  ),
  compute_panel = function(self, data, scales, n = 360, amax = 0.5) {
    # Drop rows without a complete CI region (one predicate, shared with the
    # point geom and the plot-level callers).
    data <- data[ssm_has_region(
      data$amplitude_min, data$amplitude_max,
      data$displacement_min, data$displacement_max
    ), ]
    # rep_len (not a scalar) so the assignments are valid when every row was
    # dropped; the empty frame is then routed through the parent like any other,
    # returning its normal (0-row) structure rather than the raw input columns.
    nr <- nrow(data)
    data$x0 <- rep_len(0, nr)
    data$y0 <- rep_len(0, nr)
    data$r0 <- ssm_radius(data$amplitude_min, amax)
    data$r <- ssm_radius(data$amplitude_max, amax)
    # Unwrap a displacement interval that crosses the 0/360 boundary so the arc
    # spans the short way across the seam (the same fix ssm_plot_circle()
    # applied inline). See geom_ssm_arc()'s docs for the min > max convention.
    span <- ssm_arc_span(data$displacement_min, data$displacement_max)
    if (any(!is.finite(span) | span < 0 | span >= 360)) {
      stop(
        "geom_ssm_arc(): each displacement interval must span less than a ",
        "full circle. Supply displacement_min and displacement_max in ",
        "[0, 360); a min greater than max is read as an interval crossing ",
        "the 0/360 seam.",
        call. = FALSE
      )
    }
    data$start <- ggrad(data$displacement_min)
    data$end <- ggrad(data$displacement_min + span)
    ggplot2::ggproto_parent(ggforce::StatArcBar, self)$compute_panel(
      data, scales, n = n
    )
  }
)
