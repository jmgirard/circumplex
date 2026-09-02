# Unit tests for the circumplex coordinate system (M31). The geom- and
# plot-level boundary battery (seam-straddle, zero-width wedge, back-compat) is
# in test-geom_ssm.R / the plot snapshot tests; here we pin the coord's own
# contract: construction, the amplitude limits it owns, the hard-pinned theta
# range, and the LM=360 counterclockwise-from-right angular convention.

# Transform known (displacement, amplitude) rows through a built plot's coord,
# returning the npc panel positions. Center of the panel is (0.5, 0.5).
transform_points <- function(coord, data, amax = 0.5, center = 0) {
  p <- ggplot2::ggplot(data.frame(x = c(0, 360), y = c(center, amax))) +
    ggplot2::geom_blank(ggplot2::aes(x = .data$x, y = .data$y)) +
    coord +
    ggplot2::scale_x_continuous(limits = c(0, 360))
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  b$layout$coord$transform(data, pp)
}

test_that("coord_circumplex() constructs a CoordRadial subclass", {
  co <- coord_circumplex(amax = 0.5)
  expect_s3_class(co, "CoordCircumplex")
  expect_s3_class(co, "CoordRadial")
  expect_s3_class(co, "Coord")
})

test_that("coord_circumplex() validates amax and center", {
  skip_on_cran()
  expect_error(coord_circumplex(amax = "a"), "amax")
  expect_error(coord_circumplex(amax = c(1, 2)), "amax")
  # amax must exceed the center (else the radial axis is empty/inverted).
  expect_error(coord_circumplex(amax = 0.5, center = 0.5), "greater than")
  expect_error(coord_circumplex(amax = 0.2, center = 0.5), "greater than")
  expect_error(coord_circumplex(center = "x"), "center")
  # A bad `center` is named even when `amax` is also supplied (validation order:
  # the comparison must not fire its message before `center` is type-checked).
  expect_error(coord_circumplex(amax = 1, center = "x"), "center")
  # Non-finite amax/center yield a clean message naming the argument, not a
  # cryptic "missing value" error and not a render-time NaN. is.na(Inf) is
  # FALSE, so an is.na() guard passes +/-Inf straight through to the transform
  # (LESSONS 2026-07-18, M32); these must all be caught at call time.
  for (bad in list(NA_real_, NaN, Inf, -Inf)) {
    expect_error(coord_circumplex(amax = bad), "`amax`.*finite")
    expect_error(coord_circumplex(center = bad), "`center`.*finite")
  }
  # A finite amax below a finite center still reports the comparison, not
  # finiteness -- the two guards stay distinguishable.
  expect_error(coord_circumplex(amax = 0.2, center = 0.5), "greater than")
})

test_that("amax and center are the radial limits, trained in one place", {
  # Explicit amax/center become the r-scale range directly.
  p <- ggplot2::ggplot(data.frame(x = 45, y = 0.3)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = 1, center = 0.2)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(pp$r.range, c(0.2, 1))
})

test_that("a break landing on the rim survives the radial censor", {
  skip_on_cran()
  # `amax` is documented as the amplitude the outer ring represents, but ggplot2
  # censors the radial breaks against the panel range with an exact comparison
  # and the break generator drifts a few ULPs wide of it: seq(0, 0.3, by = 0.1)
  # ends at 0.30000000000000004. The rim break was dropped to NA and the circle
  # rendered with no outer ring, leaving the data outside the outermost visible
  # ring; amax = 0.5 and 0.8 escaped only because their top break is exact.
  for (amax in c(0.3, 0.6, 1.2)) {
    p <- ggplot2::ggplot(data.frame(x = 45, y = amax * 0.7)) +
      ggplot2::geom_point(ggplot2::aes(x, y)) +
      coord_circumplex(amax = amax)
    breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$r$get_breaks()
    expect_false(anyNA(breaks))
    expect_equal(max(breaks), amax)
  }
  # Same at a nonzero center, where the rim break is generated off that origin.
  p <- ggplot2::ggplot(data.frame(x = 45, y = 0.25)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = 0.3, center = 0.1)
  breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$r$get_breaks()
  expect_false(anyNA(breaks))
  expect_equal(max(breaks), 0.3)
})

# Radial breaks and their labels as the panel actually renders them.
rim_furniture <- function(amax, center = 0, data_at = 0.7, trained_max = 1) {
  y <- if (is.null(amax)) trained_max else center + (amax - center) * data_at
  p <- ggplot2::ggplot(data.frame(x = 45, y = y)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = amax, center = center)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  breaks <- pp$r$get_breaks()
  keep <- is.finite(breaks)
  list(breaks = breaks[keep], labels = as.character(pp$r$get_labels())[keep])
}

test_that("the canvas always draws a ring at the rim", {
  skip_on_cran()
  # The break algorithm often proposes no break at `amax` at all -- over
  # [0, 1.75] it proposes 2, which is genuinely outside the panel and correctly
  # censored -- so the outermost ring sat below the rim and the circle was drawn
  # open. The coord appends the rim itself (M38).
  for (case in list(
    list(amax = 0.7, center = 0),
    list(amax = 1.1, center = 0),
    list(amax = 1.75, center = 0),
    list(amax = 2.4, center = 0),
    list(amax = 0.28, center = 0.15)
  )) {
    f <- rim_furniture(case$amax, case$center)
    expect_equal(max(f$breaks), case$amax)
    expect_equal(min(f$breaks), case$center)
  }
  # A trained (amax = NULL) canvas gets the same guarantee off its trained rim.
  # The datum is deliberately not a round number: at a trained rim of exactly 1
  # the break generator already emits one there, so the assertion would pass
  # with the rim append removed entirely and prove nothing.
  f <- rim_furniture(NULL, center = 0, trained_max = 0.73)
  expect_equal(max(f$breaks), 0.73)
  expect_equal(f$labels[[length(f$labels)]], "")
})

test_that("the rim ring is labeled only when amax is itself a generated break", {
  skip_on_cran()
  # M38-D1: the rim adds a ring and nothing else. Crowding is governed by
  # rendered label width, not break spacing, so labelling every rim collides
  # (amax = 1.1 printed 1.00/1.10 as "1.0010") and suppressing the neighbour to
  # make room deletes a ring the break algorithm chose. The generated ladder is
  # therefore left exactly as it is and the appended rim carries a blank label.
  f <- rim_furniture(1.75)
  expect_equal(f$breaks, c(0, 0.5, 1.0, 1.5, 1.75))
  # The generated breaks are labeled exactly as the scale labels them on its own
  # -- the rim is not handed to the labeller, so it cannot drag extra decimal
  # places onto the visible labels either.
  expect_equal(f$labels, c("0.0", "0.5", "1.0", "1.5", ""))
  # The crowded case that motivated the abandoned suppression rule: 0.275 keeps
  # its label and its ring, and the rim is silent rather than printing over it.
  f <- rim_furniture(0.28, center = 0.15)
  expect_equal(max(f$breaks), 0.28)
  expect_true(0.275 %in% f$breaks)
  expect_equal(f$labels[[length(f$labels)]], "")
  # Where amax is already a generated break there is nothing to append, so it
  # keeps the label the break algorithm gave it.
  for (amax in c(0.3, 0.5, 0.8, 1.2)) {
    f <- rim_furniture(amax)
    expect_equal(max(f$breaks), amax)
    expect_false(f$labels[[length(f$labels)]] == "")
    # Exactly one ring at the rim -- the generated break is not duplicated by an
    # appended one. Compared with tolerance: the generated break drifts a few
    # ULPs wide of amax (0.30000000000000004), which is what the radial range's
    # headroom exists to accommodate.
    expect_equal(sum(abs(f$breaks - amax) <= abs(amax) * 1e-9), 1L)
  }
})

test_that("appending the rim respects what the amplitude scale says about labels", {
  skip_on_cran()
  # A scale carrying explicit `labels` pairs them positionally with its own
  # breaks and aborts on a length mismatch, so the appended rim must never be
  # handed to it: doing so errored out of the build entirely.
  p <- ggplot2::ggplot(data.frame(x = 45, y = 1.2)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = 1.75) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.5, 1, 1.5), labels = c("a", "b", "c", "d")
    )
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(max(pp$r$get_breaks()), 1.75)
  expect_equal(as.character(pp$r$get_labels()), c("a", "b", "c", "d", ""))
  # A caller suppressing the amplitude labels keeps no labels at all -- blanking
  # the rim by index into NULL would fabricate a vector of literal NA labels.
  p <- ggplot2::ggplot(data.frame(x = 45, y = 1.2)) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = 1.75) +
    ggplot2::scale_y_continuous(labels = NULL)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  # na.rm: the generated set carries a censored NA break (the algorithm's
  # out-of-range proposal), which is what made the rim missing in the first place.
  expect_equal(max(pp$r$get_breaks(), na.rm = TRUE), 1.75)
  expect_null(pp$r$get_labels())
})

test_that("amax = NULL trains the outer limit from the data, inner pinned to center", {
  p <- ggplot2::ggplot(data.frame(x = c(10, 200), y = c(0.1, 0.42))) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = NULL, center = 0)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(pp$r.range[[1]], 0)          # inner pinned to center
  expect_gte(pp$r.range[[2]], 0.42)         # outer covers the data
})

test_that("theta range is hard-pinned to [0, 360] regardless of the data range (T-i1b)", {
  # Even when every displacement sits in a narrow band, the angular canvas is
  # the full circle -- the guard the seam and pole mechanisms depend on.
  p <- ggplot2::ggplot(data.frame(x = c(80, 100), y = c(0.2, 0.3))) +
    ggplot2::geom_point(ggplot2::aes(x, y)) +
    coord_circumplex(amax = 0.5)
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(pp$theta.range, c(0, 360))
})

test_that("LM=360 convention: displacement 0 at the right, increasing counterclockwise", {
  co <- coord_circumplex(amax = 0.5)
  tr <- transform_points(co, data.frame(x = c(0, 90, 180, 270), y = 0.5))
  # 0 deg -> East (right of center), 90 -> North (up), 180 -> West, 270 -> South.
  expect_gt(tr$x[[1]], 0.5); expect_equal(tr$y[[1]], 0.5, tolerance = 1e-9)  # E
  expect_gt(tr$y[[2]], 0.5); expect_equal(tr$x[[2]], 0.5, tolerance = 1e-9)  # N
  expect_lt(tr$x[[3]], 0.5); expect_equal(tr$y[[3]], 0.5, tolerance = 1e-9)  # W
  expect_lt(tr$y[[4]], 0.5); expect_equal(tr$x[[4]], 0.5, tolerance = 1e-9)  # S
})

test_that("the 0/360 pole draws at one position for either float label (I3)", {
  co <- coord_circumplex(amax = 0.5)
  tr <- transform_points(co, data.frame(x = c(0, 360), y = 0.5))
  expect_equal(tr$x[[1]], tr$x[[2]], tolerance = 1e-12)
  expect_equal(tr$y[[1]], tr$y[[2]], tolerance = 1e-12)
})

# --- T3: the amplitude axis is placed off the spokes (no 0.5/LM overlap) -------

test_that("ssm_r_axis_angle() picks the widest-gap midpoint, off every spoke (T3)", {
  # Equally spaced spokes tie; the tie breaks to the smallest midpoint.
  expect_equal(ssm_r_axis_angle(octants()), 22.5)          # 8 spokes, 45deg gaps
  expect_equal(ssm_r_axis_angle(c(0, 90, 180, 270)), 45)   # 4 poles, 90deg gaps
  expect_equal(ssm_r_axis_angle(seq(0, 330, 30)), 15)      # 12 spokes, 30deg gaps
  # It never lands on a spoke, for each instrument shape.
  for (br in list(octants(), c(0, 90, 180, 270), seq(0, 330, 30))) {
    expect_false(ssm_r_axis_angle(br) %in% (br %% 360))
  }
  # An uneven layout puts the axis in the genuinely widest gap.
  expect_equal(ssm_r_axis_angle(c(0, 45, 90)), 225)        # widest gap 90->360
  # Degenerate break sets fall back sensibly.
  expect_equal(ssm_r_axis_angle(numeric(0)), 90)           # no spokes
  expect_equal(ssm_r_axis_angle(0), 180)                   # one spoke -> opposite
})

test_that("the built canvas draws its amplitude axis off the due-East spoke (T3)", {
  p <- ggcircumplex(octants(), amax = 0.5)
  co <- ggplot2::ggplot_build(p)$layout$coord
  # Radial axis moved off theta = 0 (the old due-East 0.5/LM collision) into the
  # widest spoke gap, and does not coincide with any spoke.
  expect_equal(co$r_axis_inside, 22.5)
  expect_false(co$r_axis_inside %in% (octants() %% 360))
})

test_that("r_axis_angle overrides the automatic placement (T3)", {
  p <- ggplot2::ggplot() +
    coord_circumplex(amax = 0.5, r_axis_angle = 200) +
    ggplot2::geom_blank(
      data = data.frame(.x = c(0, 360), .y = c(0, 0.5)),
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y), inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = octants())
  co <- ggplot2::ggplot_build(p)$layout$coord
  expect_equal(co$r_axis_inside, 200)
  expect_error(coord_circumplex(r_axis_angle = NA_real_), "finite")
  expect_error(coord_circumplex(r_axis_angle = Inf), "finite")
  expect_error(coord_circumplex(r_axis_angle = c(1, 2)), "r_axis_angle")
})

test_that("the rim ring renders on an unround amax", {
  skip_if_not_installed("vdiffr")
  # The rim ring is purely visual, so it gets the one plot guard the repo's
  # test doctrine allows (vdiffr where the plot is the product). No existing
  # baseline covers it: every vdiffr canvas in the suite uses amax 0.5, 0.6 or
  # 1.0, all of which the break algorithm already places a break at, so none of
  # them moved when the rim ring landed.
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "rim ring at an unround amax",
    ggcircumplex(octants(), labels = PANO(), amax = 1.75)
  )
})

# --- M39: the amplitude labels carry a backdrop so data cannot swallow them ---

# Pull the panel's foreground grob (the last panel child; the radial axis is a
# FOREGROUND guide, which is why the labels were already on top of the data and
# the defect was contrast rather than draw order) and return the amplitude-axis
# text grob alongside the backdrop drawn behind it. Located by name and by
# label-set rather than by index: M32 established that these grobs are nested in
# unnamed gTrees, so a positional path into them is not stable, and the theta
# (spoke) labels sit in a sibling subtree that M39 deliberately leaves alone.
axis_label_parts <- function(p) {
  panel <- ggplot2::ggplotGrob(p)
  panel <- panel$grobs[[which(panel$layout$name == "panel")]]
  found <- list(text = NULL, backdrop = NULL)
  walk <- function(g) {
    if (inherits(g, "text") && !any(grepl("°", as.character(g$label)))) {
      found$text <<- g
    }
    if (identical(g$name, "circumplex-label-backdrop")) found$backdrop <<- g
    kids <- if (inherits(g, "gtable")) g$grobs else g$children
    for (k in kids) walk(k)
  }
  walk(panel)
  found
}

test_that("the amplitude labels are drawn over a backdrop (M39 T2)", {
  skip_on_cran()
  parts <- axis_label_parts(ggcircumplex(octants(), amax = 0.8))

  # The labels themselves are unchanged: still one vectorized text grob.
  expect_false(is.null(parts$text))
  expect_equal(as.character(parts$text$label), c("0.0", "0.2", "0.4", "0.6", "0.8"))

  # A backdrop exists, carrying one plate per label rather than one for all.
  expect_false(is.null(parts$backdrop))
  expect_length(parts$backdrop$children, length(parts$text$label))
  for (plate in parts$backdrop$children) expect_s3_class(plate, "rect")

  # Semi-transparent fill and no border, so the data underneath stays visible.
  for (plate in parts$backdrop$children) {
    expect_identical(plate$gp$col, NA)
    expect_match(plate$gp$fill, "^#FFFFFF", ignore.case = TRUE)
    expect_false(identical(toupper(plate$gp$fill), "#FFFFFFFF")) # not opaque
  }
})

test_that("each plate is rotated onto its own label (M39 T2)", {
  skip_on_cran()
  # The regression this exists for: the radial axis sits at an angle and every
  # label is turned about its own anchor to stay readable, but rectGrob has no
  # rotation. A first implementation shared the labels' x/y and still drew the
  # plates axis-aligned, so they slid off the text -- and every structural
  # assertion above still passed. Only rendering exposed it. Fence the rotation
  # and the per-label anchor directly so it cannot come back silently.
  parts <- axis_label_parts(ggcircumplex(octants(), amax = 0.8))
  rot <- parts$text$rot
  expect_true(rot != 0) # precondition: the labels really are rotated here

  anchors_x <- character(0)
  for (plate in parts$backdrop$children) {
    expect_false(is.null(plate$vp))
    expect_equal(plate$vp$angle, rot)
    anchors_x <- c(anchors_x, format(plate$vp$y))
  }
  # Each plate is anchored at its own label's position, not all at one point.
  expect_equal(length(unique(anchors_x)), length(parts$text$label))

  # The plates inherit the label font, so their size measures the text as drawn
  # rather than at the device default.
  for (plate in parts$backdrop$children) {
    expect_equal(plate$gp$fontsize, parts$text$gp$fontsize)
  }
})

test_that("the backdrop tracks a relocated axis (M39 T2)", {
  skip_on_cran()
  # r_axis_angle (M32) moves the axis, which changes the label rotation; the
  # plates must follow it rather than keep the default placement's angle.
  p <- ggplot2::ggplot() +
    coord_circumplex(amax = 0.8, r_axis_angle = 200) +
    ggplot2::geom_blank(
      data = data.frame(.x = c(0, 360), .y = c(0, 0.8)),
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y), inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = octants())
  moved <- axis_label_parts(p)
  expect_false(is.null(moved$backdrop))
  expect_length(moved$backdrop$children, length(moved$text$label))
  for (plate in moved$backdrop$children) {
    expect_equal(plate$vp$angle, moved$text$rot)
  }
  # And that really is a different angle from the default placement.
  default_rot <- axis_label_parts(ggcircumplex(octants(), amax = 0.8))$text$rot
  expect_false(isTRUE(all.equal(moved$text$rot, default_rot)))
})

test_that("the rim's blank label gets no plate (M39 T2)", {
  skip_on_cran()
  # M38 appends the rim break with a blank label. A plate behind an empty string
  # would be a stray floating rectangle, so the count follows the non-empty
  # labels, not the break count.
  parts <- axis_label_parts(ggcircumplex(octants(), amax = 1.75))
  labels <- as.character(parts$text$label)
  # amax 1.75 is not a generated break, so M38 appends the rim with a blank
  # label -- the precondition this test depends on, asserted rather than assumed.
  expect_true(any(labels == ""))
  expect_length(parts$backdrop$children, sum(labels != ""))
})

test_that("an amplitude label over a dark mark stays legible (M39 T4)", {
  skip_if_not_installed("vdiffr")
  # The defect M39 fixes is a CONTRAST failure, and no existing canvas baseline
  # could see it: every one of them draws the labels over empty panel. This
  # baseline puts a large dark marker and a heavy arrowhead exactly where the
  # amplitude labels fall -- the `advanced-visualization.Rmd` situation reduced
  # to a fixture -- so the plates are the only thing keeping the labels
  # readable, and removing them moves this image.
  skip_on_ci()
  # amax 0.8 puts labels at 0.2/0.4/0.6; the path runs straight through them.
  marks <- data.frame(a = c(0.2, 0.4, 0.6, 0.8), d = rep(22.5, 4))
  vdiffr::expect_doppelganger(
    "amplitude labels over dark marks",
    ggcircumplex(octants(), amax = 0.8) +
      geom_ssm_point(
        data = marks,
        mapping = ggplot2::aes(amplitude = .data$a, displacement = .data$d),
        fill = "grey10", size = 9
      ) +
      geom_ssm_path(
        data = marks,
        mapping = ggplot2::aes(amplitude = .data$a, displacement = .data$d),
        arrow = grid::arrow(length = grid::unit(0.3, "inches"), type = "closed"),
        linewidth = 1.4
      )
  )
})

test_that("plate extent and padding offset are fenced structurally (M39 F4)", {
  skip_on_cran()
  # Review found the structural fence covered rotation and anchor but NOT size
  # or offset: a plate hardcoded to 30x30pt, or one with the padding re-centring
  # dropped, passed everything here and failed only the two vdiffr baselines --
  # both `skip_on_ci()`, so on CI those regressions went green. Fence the extent
  # and the offset directly so the guard does not depend on a skipped snapshot.
  parts <- axis_label_parts(ggcircumplex(octants(), amax = 0.8))
  for (plate in parts$backdrop$children) {
    # Size derives from the measured label, not a constant: the unit is a sum
    # carrying a grob-measurement term, so a fixed-size plate cannot satisfy it.
    for (dim in list(plate$width, plate$height)) {
      expect_true(inherits(dim, "unit"))
      expect_match(
        paste(as.character(dim), collapse = " "), "grobwidth|grobheight",
        ignore.case = TRUE
      )
    }
    # The anchor carries the padding correction that re-centres the wider plate
    # on its label. hjust/vjust are 0/1 here, so the offsets are -1pt and +1pt;
    # dropping the correction leaves a bare 0.5npc with no "pt" term.
    expect_match(paste(as.character(plate$x), collapse = " "), "pt|points")
    expect_match(paste(as.character(plate$y), collapse = " "), "pt|points")
  }
})

test_that("spoke labels are never plated, even when they read like amplitudes (M39 F2)", {
  skip_on_cran()
  # Review reproduction: the walk matched on label text alone, so a caller whose
  # THETA labels happen to equal the amplitude labels got plates behind both --
  # and the theta guide is traversed first. The spoke labels are explicitly Out
  # of this milestone's scope, so this must plate the amplitude axis only.
  p <- ggplot2::ggplot() +
    coord_circumplex(amax = 0.8) +
    ggplot2::geom_blank(
      data = data.frame(.x = c(0, 360), .y = c(0, 0.8)),
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y), inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(72, 144, 216, 288, 360),
      labels = c("0.0", "0.2", "0.4", "0.6", "0.8")
    )
  panel <- ggplot2::ggplotGrob(p)
  panel <- panel$grobs[[which(panel$layout$name == "panel")]]
  n <- 0L
  walk <- function(g) {
    if (identical(g$name, "circumplex-label-backdrop")) n <<- n + 1L
    kids <- if (inherits(g, "gtable")) g$grobs else g$children
    for (k in kids) walk(k)
  }
  walk(panel)
  # Exactly one backdrop group: the amplitude axis. Two would mean the spoke
  # labels were plated too.
  expect_equal(n, 1L)
})

test_that("a plotmath label is measured as drawn, not as its source text (M39 F3)", {
  skip_on_cran()
  # Review found `as.character()` deparsed an expression, so the plate was sized
  # to the string "gamma^2" rather than to the single rendered glyph -- several
  # times too wide, and vertically offset by the superscript. The plate must be
  # measured from the label itself.
  p <- ggplot2::ggplot() +
    coord_circumplex(amax = 0.8) +
    ggplot2::geom_blank(
      data = data.frame(.x = c(0, 360), .y = c(0, 0.8)),
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y), inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(breaks = octants()) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8),
      labels = parse(text = c("alpha", "beta[max]", "gamma^2", "delta", "epsilon"))
    )
  parts <- axis_label_parts(p)
  expect_false(is.null(parts$backdrop))
  # Measurement is grob-based, so plotmath is measured rendered rather than
  # deparsed; a stringWidth() implementation would carry "strwidth" instead.
  for (plate in parts$backdrop$children) {
    w <- paste(as.character(plate$width), collapse = " ")
    expect_match(w, "grobwidth", ignore.case = TRUE)
    expect_false(grepl("strwidth", w, ignore.case = TRUE))
  }
})
