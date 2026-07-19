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

# Semi-opaque plates drawn behind the amplitude tick labels (M39).
#
# The radial axis is a FOREGROUND guide: `CoordRadial$render_fg` emits it after
# the geom layers, so its labels are already painted on top of the data. The
# defect this fixes is therefore contrast, not draw order -- grey label text over
# a dark arrowhead or a dense scatter stays on top and still cannot be read. The
# plate is deliberately semi-transparent rather than opaque so it restores
# contrast without erasing the data it covers.
#
# It is built FROM the label text grob rather than from computed positions.
# `CoordRadial` places the radial axis through an unexported `rotate_r_axis()`,
# so reproducing the placement would mean either reaching into ggplot2 internals
# or re-deriving arithmetic that drifts silently and leaves the plate beside the
# label instead of behind it. Sizing with `stringWidth()`/`stringHeight()` at the
# text's own x/y/just hands the geometry to grid at draw time, making the plate
# exact by construction (M39-D1).
label_backdrop <- function(txt, pad = 1) {
  # Keep the label in its ORIGINAL form, not as.character(): a plotmath
  # expression deparses to its source text, and measuring that string sizes the
  # plate to `"beta[max]"` rather than to the rendered glyph. Only the
  # blank/NA test below needs the character form.
  raw <- txt$label
  labels <- as.character(raw)
  # A blank label (M38 appends the rim break with one) would otherwise get a
  # stray floating plate with nothing on it.
  keep <- !is.na(labels) & labels != ""
  if (!any(keep)) {
    return(NULL)
  }
  n <- length(labels)
  # x/just may arrive scalar for a vector of labels; recycle before subsetting
  # so every plate is anchored exactly like the label it sits behind.
  recycle <- function(u) if (length(u) == 1L && n > 1L) rep(u, n) else u
  idx <- which(keep)
  x <- recycle(txt$x)[keep]
  y <- recycle(txt$y)[keep]
  # `%||%` is not used here: it only reached base R in 4.4 and this package
  # declares R (>= 4.1), so it is not guaranteed available (D-021).
  or_else <- function(value, default) if (is.null(value)) default else value
  hj <- rep_len(or_else(txt$hjust, 0.5), n)[keep]
  vj <- rep_len(or_else(txt$vjust, 0.5), n)[keep]
  # The labels are ROTATED -- the radial axis sits at an angle, and each label
  # is turned about its own anchor to stay readable. `rectGrob()` has no
  # rotation, so a plate sharing the text's x/y is still drawn axis-aligned and
  # slides off the label it is meant to back (caught by rendering, not by any
  # structural check -- see the M39 work log). Each plate therefore gets its own
  # viewport centred on that label's anchor: a viewport rotates about its own
  # centre, so centring it on the anchor reproduces exactly the rotate-about-
  # the-justification-point behaviour `textGrob()` applies to the label.
  rot <- rep_len(or_else(txt$rot, 0), n)[keep]
  # Inherit the label's font so the measurement below sizes the text as it will
  # actually be drawn rather than at the device default.
  font <- list(
    fontsize = txt$gp$fontsize, fontfamily = txt$gp$fontfamily,
    fontface = txt$gp$font, cex = txt$gp$cex,
    # White at 75% alpha, written as a literal rather than built with
    # grDevices::adjustcolor() so the package gains no second dependency for a
    # constant (the minimal-deps doctrine, D-006/D-014). BF = round(0.75 * 255).
    fill = "#FFFFFFBF", col = NA
  )
  gp <- do.call(grid::gpar, font[!vapply(font, is.null, logical(1))])
  plates <- lapply(seq_along(idx), function(i) {
    # Measure the label as a GROB, not as a string. `stringWidth()` takes a
    # character vector, so a plotmath label would be measured as its deparsed
    # source (`"gamma^2"`, seven characters) instead of the one glyph actually
    # drawn -- a plate several times too wide, and vertically offset because the
    # superscript changes the rendered height. `grobWidth()`/`grobHeight()` on a
    # text grob built from the label itself measures what the device will draw,
    # for plain strings and expressions alike.
    one <- grid::textGrob(raw[[idx[[i]]]], gp = gp)
    # Padding widens the plate symmetrically, which would move it off the label
    # unless the justification is centred: a plate `2 * pad` wider but pinned at
    # the same edge sits off to one side. Shifting by `pad * (2 * just - 1)`
    # re-centres it for any justification.
    grid::rectGrob(
      x = grid::unit(0.5, "npc") + grid::unit(pad * (2 * hj[[i]] - 1), "pt"),
      y = grid::unit(0.5, "npc") + grid::unit(pad * (2 * vj[[i]] - 1), "pt"),
      width = grid::grobWidth(one) + grid::unit(2 * pad, "pt"),
      height = grid::grobHeight(one) + grid::unit(2 * pad, "pt"),
      hjust = hj[[i]], vjust = vj[[i]],
      gp = gp,
      vp = grid::viewport(x = x[i], y = y[i], angle = rot[[i]])
    )
  })
  do.call(grid::grobTree, c(plates, list(name = "circumplex-label-backdrop")))
}

# Walk a rendered foreground tree and put a backdrop behind the amplitude labels.
#
# Two conditions must BOTH hold before a text grob is plated, because either one
# alone is unsafe. The grob must sit inside the radial axis's own gtable (named
# "axis"), and its labels must match the radial view scale's. Position alone is
# not enough -- M32 found these grobs nested in unnamed gTrees, so an index path
# into them is not stable across ggplot2 versions. Labels alone are not enough
# either: the theta (spoke) labels, which M39 deliberately leaves alone, live in
# a sibling subtree that is traversed FIRST, so a caller whose spoke labels
# happen to read like amplitudes (`scale_x_continuous(labels = c("0.0", ...))`)
# would otherwise have them silently plated too. Requiring the axis subtree
# scopes the walk to the guide M39 owns; requiring the labels keeps it from
# plating anything else that guide might contain. A match failure draws no plate
# at all, which is visible and fenced, rather than styling the wrong text.
add_label_backdrop <- function(grob, labels, in_axis = FALSE) {
  # The view scale can carry a break the guide never draws -- an amax past the
  # last generated break leaves an out-of-range break whose label is NA, dropped
  # on the way to a grob -- so compare against the labels that can actually be
  # rendered, not the raw break set.
  labels <- as.character(labels)
  labels <- labels[!is.na(labels)]
  if (inherits(grob, "text")) {
    if (in_axis && identical(as.character(grob$label), labels)) {
      backdrop <- label_backdrop(grob)
      if (!is.null(backdrop)) {
        # Keep the wrapper's name so the parent's childrenOrder still resolves.
        return(grid::grobTree(backdrop, grob, name = grob$name))
      }
    }
    return(grob)
  }
  # gtables hold their children in `grobs`, gTrees in `children`; assign back by
  # index in both cases so the existing names (and childrenOrder) survive. The
  # gtable branch must come first: a gtable IS a gTree, but carries no
  # `children`, so testing gTree first would silently descend into nothing.
  # Entering the guide's "axis" gtable is what arms plating for its subtree.
  if (inherits(grob, "gtable")) {
    in_axis <- in_axis || identical(grob$name, "axis")
    for (i in seq_along(grob$grobs)) {
      grob$grobs[[i]] <- add_label_backdrop(grob$grobs[[i]], labels, in_axis)
    }
    return(grob)
  }
  for (i in seq_along(grob$children)) {
    grob$children[[i]] <- add_label_backdrop(grob$children[[i]], labels, in_axis)
  }
  grob
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
  },

  # Let the parent draw the foreground exactly as it does -- the radial axis is
  # already painted above the geom layers there -- then slip a backdrop behind
  # the amplitude labels so data underneath cannot swallow them (M39).
  render_fg = function(self, panel_params, theme) {
    fg <- ggplot2::ggproto_parent(
      ggplot2::CoordRadial, self
    )$render_fg(panel_params, theme)
    add_label_backdrop(fg, panel_params$r$get_labels())
  }
)
