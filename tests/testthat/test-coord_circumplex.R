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
