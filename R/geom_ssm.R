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
    data <- data[!is.na(data$amplitude) & !is.na(data$displacement), ]
    amax <- if (is.null(params$amax)) 0.5 else params$amax
    r <- ssm_radius(data$amplitude, amax)
    data$x <- r * cos(data$displacement * pi / 180)
    data$y <- r * sin(data$displacement * pi / 180)
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
    data <- data[stats::complete.cases(
      data[c("amplitude_min", "amplitude_max",
             "displacement_min", "displacement_max")]
    ), ]
    if (nrow(data) == 0) return(data)
    data$x0 <- 0
    data$y0 <- 0
    data$r0 <- ssm_radius(data$amplitude_min, amax)
    data$r <- ssm_radius(data$amplitude_max, amax)
    # Unwrap a displacement interval that crosses the 0/360 boundary so the arc
    # spans the short way (the same fix ssm_plot_circle() applied inline)
    upper <- ifelse(
      data$displacement_max < data$displacement_min,
      data$displacement_max + 360,
      data$displacement_max
    )
    data$start <- ggrad(data$displacement_min)
    data$end <- ggrad(upper)
    ggplot2::ggproto_parent(ggforce::StatArcBar, self)$compute_panel(
      data, scales, n = n
    )
  }
)
