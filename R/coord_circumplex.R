# Circumplex coordinate system -------------------------------------------------
# coord_circumplex() is the single owner of the amplitude->radius scaling
# (`amax`) and the displacement->angle polar transform that together place SSM
# results on the circular canvas. It subclasses ggplot2's CoordRadial so that
# `amax` and the amplitude center become r-scale limits trained in one place --
# the per-layer `amax` shared-state defect (DESIGN.md) becomes structurally
# impossible -- and the rings/spokes/labels are themed panel furniture rather
# than frozen drawn geometry. See cairn milestone M31 and devel/m30-coord-spec.md.
#
# The angle invariants (CLAUDE.md; ip-touching) are carried by the pinned
# convention below: displacement 0 is at +x and increases counterclockwise
# (start = pi/2, reverse = "theta"), the theta range is hard-pinned to [0, 360]
# with expansion off so a profile at the 0/360 pole draws identically for either
# float label (I3) and a seam-straddling arc rectangle whose xmax has been
# unwrapped past 360 bends the short way across the pole (I2).

#' Circumplex coordinate system
#'
#' A \pkg{ggplot2} coordinate system that maps Structural Summary Method
#' parameters onto the circular circumplex canvas: the `displacement` aesthetic
#' (degrees, counterclockwise from the right, with the 0/360 pole labelled 360)
#' becomes the angle and the `amplitude` aesthetic becomes the radius. It owns
#' the amplitude-to-radius scaling, so [geom_ssm_point()] and [geom_ssm_arc()]
#' no longer take an `amax`, and the canvas and data layers can never disagree.
#'
#' `coord_circumplex()` subclasses [ggplot2::coord_radial()] and hard-pins the
#' angular convention (displacement 0 at the right, increasing counterclockwise,
#' the 0/360 range with no expansion) so the circumplex angle invariants survive
#' the transform. The amplitude at the circle's center and at its outer ring are
#' the radial limits, set once here.
#'
#' @param amax Optional. A single positive number giving the amplitude
#'   represented by the outer ring. `NULL` (the default) trains it from the data
#'   (as [ssm_plot_circle()] does).
#' @param center Optional. A single number giving the amplitude at the center of
#'   the circle (default = 0). Ring labels and the amplitude-to-radius mapping
#'   are guaranteed to agree.
#' @param ... Reserved for future extensions; currently unused.
#' @return A \pkg{ggplot2} coordinate system that can be added to a plot with
#'   `+`.
#' @seealso [ggcircumplex()], [geom_ssm_point()], [geom_ssm_arc()]
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' ggplot2::ggplot(res$results) +
#'   coord_circumplex(amax = 0.5) +
#'   geom_ssm_point(ggplot2::aes(amplitude = a_est, displacement = d_est))
coord_circumplex <- function(amax = NULL, center = 0, ...) {
  stopifnot(is_null_or_num(amax, n = 1))
  if (!is.null(amax) && !(amax > center)) {
    stop("`amax` must be greater than `center`.", call. = FALSE)
  }
  stopifnot(is_num(center, n = 1))

  # Build a stock CoordRadial with the pinned circumplex convention, then adopt
  # its computed fields under the CoordCircumplex parent so our overrides
  # dispatch. Copying the fields (rather than recomputing arc/inner_radius here)
  # keeps us robust to CoordRadial's internal setup changing across ggplot2
  # versions. rlim's upper bound is trained in setup_panel_params when amax is
  # NULL; a placeholder keeps coord_radial() happy meanwhile.
  rlim <- c(center, if (is.null(amax)) center + 1 else amax)
  base <- ggplot2::coord_radial(
    theta = "x", start = pi / 2, thetalim = c(0, 360), rlim = rlim,
    expand = FALSE, reverse = "theta", inner.radius = 0,
    r.axis.inside = 0, clip = "off"
  )

  ggplot2::ggproto(
    NULL, CoordCircumplex,
    # Fields copied verbatim from the stock CoordRadial build.
    limits = base$limits, theta = base$theta, r = base$r, arc = base$arc,
    expand = base$expand, reverse = base$reverse,
    r_axis_inside = base$r_axis_inside, rotate_angle = base$rotate_angle,
    inner_radius = base$inner_radius, clip = base$clip,
    # Circumplex-specific state.
    amax = amax, center = center
  )
}

CoordCircumplex <- ggplot2::ggproto(
  "CoordCircumplex", ggplot2::CoordRadial,

  # Train the outer amplitude limit from the data when amax was not supplied,
  # while always pinning the inner limit to `center`. The theta range is already
  # hard-pinned to [0, 360] by the `limits$theta` field, independent of the
  # data's displacement range (the guard the seam and pole mechanisms hang on).
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    if (is.null(self$amax)) {
      r_scale <- if (self$theta == "x") scale_y else scale_x
      data_max <- suppressWarnings(max(r_scale$get_limits(), na.rm = TRUE))
      if (!is.finite(data_max) || data_max <= self$center) {
        data_max <- self$center + 1
      }
      self$limits$r <- c(self$center, data_max)
    }
    ggplot2::ggproto_parent(ggplot2::CoordRadial, self)$setup_panel_params(
      scale_x, scale_y, params
    )
  }
)
