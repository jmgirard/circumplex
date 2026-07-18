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
