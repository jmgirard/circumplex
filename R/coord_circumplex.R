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
#' @param r_axis_angle Optional. A single number giving the displacement (in
#'   degrees) along which the amplitude (radial) axis and its labels are drawn.
#'   `NULL` (the default) places it automatically in the widest gap between the
#'   displacement spokes, so the amplitude labels never collide with a spoke
#'   label.
#' @param ... Reserved for future extensions; currently unused.
#' @return A \pkg{ggplot2} coordinate system that can be added to a plot with
#'   `+`.
#' @family circumplex layers
#' @export
#' @examples
#' data("jz2017")
#' res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
#' ggplot2::ggplot(res$results) +
#'   coord_circumplex(amax = 0.5) +
#'   geom_ssm_point(ggplot2::aes(amplitude = a_est, displacement = d_est))
coord_circumplex <- function(amax = NULL, center = 0, r_axis_angle = NULL, ...) {
  # Every numeric argument here is guarded with !is.finite() rather than
  # is.na(): is.na(Inf) is FALSE, so an infinite limit slips past an is.na()
  # guard and surfaces only as a cryptic error deep in the render, never naming
  # the argument that caused it. `center` is validated before it is used in the
  # `amax`/`center` comparison, so a bad `center` is named as the culprit (not
  # `amax`) and the comparison never returns NA.
  stopifnot(is_num(center, n = 1))
  if (!is.finite(center)) {
    stop("`center` must be a single finite number.", call. = FALSE)
  }
  stopifnot(is_null_or_num(amax, n = 1))
  stopifnot(is_null_or_num(r_axis_angle, n = 1))
  if (!is.null(r_axis_angle) && !is.finite(r_axis_angle)) {
    stop("`r_axis_angle` must be a single finite number (or NULL).",
         call. = FALSE)
  }
  if (!is.null(amax) && !is.finite(amax)) {
    stop("`amax` must be a single finite number (or NULL).", call. = FALSE)
  }
  if (!is.null(amax) && amax <= center) {
    stop("`amax` must be a single number greater than `center`.", call. = FALSE)
  }

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
    amax = amax, center = center, r_axis_angle = r_axis_angle
  )
}

# Upper bound to hand ggplot2 for the radial range, a few ULPs above `amax`.
# ggplot2 censors the r breaks against that range with an exact comparison,
# while the break generator overshoots: seq(0, 0.3, by = 0.1) ends at
# 0.30000000000000004. Without the headroom a break sitting *on* the rim is
# censored to NA, the outer ring vanishes, and data plots outside the outermost
# visible ring even though `amax` is documented as the amplitude that ring
# represents (amax = 0.5 and 0.8 escaped only because their top break is exact).
# The headroom is relative to the radial span and scaled well above the
# generator's accumulated drift (a few ULPs) while staying far below anything
# that could shift a ring by a visible amount.
rim_limit <- function(center, amax) {
  amax + (amax - center) * 64 * .Machine$double.eps
}

# Guarantee a ring at the rim. The break algorithm places one at `amax` only by
# coincidence -- over [0, 1.75] it proposes 2, genuinely outside the panel and
# correctly censored -- so the outermost ring sat below the rim and the circle
# was drawn open, with data able to plot beyond the last visible ring. Append
# the rim to the radial view scale's breaks; `guide_grid()` draws the rings from
# `r$mapped_breaks()`, so the ring and the axis guide both follow from the one
# patched break set.
#
# The appended break carries a blank label (M38-D1). Crowding near the rim is
# governed by rendered label width rather than break spacing, so labelling every
# rim collides with its neighbour, and suppressing that neighbour to make room
# deletes a ring the break algorithm chose. Where `amax` is already a generated
# break there is nothing to append and it keeps its own label.
rim_view_scale <- function(view, rim) {
  breaks <- view$get_breaks()
  tol <- abs(rim) * 1e-9
  if (any(abs(breaks - rim) <= tol, na.rm = TRUE)) {
    return(view)
  }
  ggplot2::ggproto(
    NULL, view,
    breaks = c(breaks, rim),
    get_labels = function(self, breaks = self$get_breaks()) {
      # Label the breaks the scale knows about, then blank the appended rim.
      # The rim is asked for separately rather than blanked afterwards because
      # a scale carrying explicit `labels` pairs them positionally with its own
      # breaks and aborts on a length mismatch -- handing it the appended break
      # would error out of the build. NULL labels (a caller suppressing the
      # amplitude labels) stay NULL: assigning into NULL by index would
      # fabricate a vector of literal NA labels.
      is_rim <- !is.na(breaks) & abs(breaks - rim) <= tol
      labels <- self$scale$get_labels(breaks[!is_rim])
      if (is.null(labels)) {
        return(labels)
      }
      out <- rep(if (is.list(labels)) list("") else "", length(breaks))
      out[!is_rim] <- labels
      out
    }
  )
}

# Displacement (degrees) at which to draw the amplitude (radial) axis: the
# midpoint of the widest angular gap between consecutive displacement spokes, so
# the amplitude tick labels never collide with a spoke label (the due-East
# `0.5`/`LM` overlap resolved in M32). Equally spaced spokes tie, so the tie
# breaks to the smallest such midpoint for a deterministic placement. Degenerate
# break sets fall back to a sensible fixed angle.
ssm_r_axis_angle <- function(breaks) {
  b <- sort(unique(breaks[is.finite(breaks)] %% 360))
  if (length(b) < 1L) return(90)                 # no spokes: straight up
  if (length(b) < 2L) return((b + 180) %% 360)   # one spoke: opposite it
  gaps <- diff(c(b, b[[1]] + 360))               # includes the wrap gap
  mids <- (b + gaps / 2) %% 360
  min(mids[gaps > max(gaps) - 1e-9])             # widest gap, smallest midpoint
}

#' @rdname circumplex-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCircumplex <- ggplot2::ggproto(
  "CoordCircumplex", ggplot2::CoordRadial,

  # Train the outer amplitude limit from the data when amax was not supplied,
  # while always pinning the inner limit to `center`. The theta range is already
  # hard-pinned to [0, 360] by the `limits$theta` field, independent of the
  # data's displacement range (the guard the seam and pole mechanisms hang on).
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    r_max <- self$amax
    if (is.null(r_max)) {
      r_scale <- if (self$theta == "x") scale_y else scale_x
      data_max <- suppressWarnings(max(r_scale$get_limits(), na.rm = TRUE))
      if (!is.finite(data_max) || data_max <= self$center) {
        data_max <- self$center + 1
      }
      r_max <- data_max
    }
    # Recomputed from `amax`/`center` on every call rather than nudged in place,
    # so repeated builds of the same coord stay idempotent.
    self$limits$r <- c(self$center, rim_limit(self$center, r_max))
    # Place the amplitude (radial) axis in the widest spoke gap unless the caller
    # pinned an angle, so its labels clear the spoke labels. The theta scale's
    # breaks are the spokes; set r_axis_inside before delegating so the parent
    # positions the radial axis guide there.
    theta_scale <- if (self$theta == "x") scale_x else scale_y
    self$r_axis_inside <- if (is.null(self$r_axis_angle)) {
      ssm_r_axis_angle(theta_scale$get_breaks())
    } else {
      self$r_axis_angle %% 360
    }
    params <- ggplot2::ggproto_parent(
      ggplot2::CoordRadial, self
    )$setup_panel_params(scale_x, scale_y, params)
    # Add the rim ring after the parent has built the view scales, then keep the
    # precomputed major positions consistent with the break set they came from.
    params$r <- rim_view_scale(params$r, r_max)
    params$r.major <- params$r$map(params$r$get_breaks())
    params
  }
)
