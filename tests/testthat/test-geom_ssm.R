# Boundary/regression battery for the circumplex geoms under coord_circumplex()
# (M31 AC4 / spec devel/m30-coord-spec.md §6, §11). The geoms hand amplitude and
# displacement to the coord, which owns the polar transform; these tests fence
# the angle invariants (CLAUDE.md; ip-touching) at the data level (post
# setup_data / ggplot_build) AND the grob level (post-coord render), because
# devtools::check() runs clean on a visually wrong figure (M13/M27 lesson).

# --- helpers ------------------------------------------------------------------

# Built data for the layer drawn by a given geom class (locate by geom, never by
# a hardcoded index -- ggcircumplex()'s geom_blank shifts the numbering).
layer_data_for <- function(p, geom_class) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, geom_class),
                      logical(1)))
  b$data[[idx[[1]]]]
}
arc_layer_index <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomSsmArc"),
               logical(1)))[[1]]
}

# Angular coverage (degrees, [0, 360)) of a drawn arc grob's polygon vertices,
# measured about the panel center (0.5, 0.5) in npc space -- the post-coord
# rendered wedge, not the pre-transform rectangle.
arc_grob_angles <- function(p) {
  gr <- ggplot2::layer_grob(p, arc_layer_index(p))[[1]]
  xs <- as.numeric(gr$x); ys <- as.numeric(gr$y)
  ok <- is.finite(xs) & is.finite(ys)
  (atan2(ys[ok] - 0.5, xs[ok] - 0.5) * 180 / pi) %% 360
}
point_grob_xy <- function(p) {
  gr <- ggplot2::layer_grob(p, which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomSsmPoint"), logical(1)
  ))[[1]])[[1]]
  list(x = as.numeric(gr$x), y = as.numeric(gr$y))
}

arc_plot <- function(df, amax = 0.5, angles = octants()) {
  ggcircumplex(angles, amax = amax) +
    geom_ssm_arc(
      data = df,
      mapping = ggplot2::aes(
        amplitude_min = .data$a_lci, amplitude_max = .data$a_uci,
        displacement_min = .data$d_lci, displacement_max = .data$d_uci
      )
    )
}
point_plot <- function(df, amax = 0.5) {
  ggcircumplex(octants(), amax = amax) +
    geom_ssm_point(
      data = df,
      mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est)
    )
}

# --- T-i1b: theta range hard-pinned regardless of the data --------------------

test_that("theta range stays [0, 360] however narrow the data's displacement (T-i1b)", {
  p <- point_plot(data.frame(a_est = c(0.2, 0.3), d_est = c(80, 100)))
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_equal(pp$theta.range, c(0, 360))
})

# --- R1: the coord owns amax; there is no per-layer amax ----------------------

test_that("amplitude radius is owned by the coord, not the layer (R1)", {
  df <- data.frame(a_est = 0.25, d_est = 0)
  r_of <- function(amax) {
    xy <- point_grob_xy(point_plot(df, amax = amax))
    sqrt((xy$x - 0.5)^2 + (xy$y - 0.5)^2)
  }
  # a = 0.25 is half the radius at amax = 0.5 but a quarter at amax = 1.0: the
  # single coord scales it, so the same amplitude lands farther out at amax=0.5.
  expect_gt(r_of(0.5), r_of(1.0))
  # A point at the center amplitude sits at the panel center.
  xy0 <- point_grob_xy(point_plot(data.frame(a_est = 0, d_est = 0), amax = 0.5))
  expect_equal(xy0$x[[1]], 0.5, tolerance = 1e-9)
  expect_equal(xy0$y[[1]], 0.5, tolerance = 1e-9)
})

# --- T-i2: seam-straddle unwrap by extension, short way across the pole --------

test_that("a seam-straddling arc unwraps by extension and draws the short way (T-i2)", {
  p <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 350, d_uci = 10))
  # Data level: xmax extended past 360 (350 + 20-degree span).
  ad <- layer_data_for(p, "GeomSsmArc")
  expect_equal(ad$xmin, 350)
  expect_equal(ad$xmax, 370)
  # Grob level: covers (330, 360] and (0, 30] but NOT the 340-degree complement.
  ang <- arc_grob_angles(p)
  expect_true(any(ang > 330 & ang <= 360))
  expect_true(any(ang > 0 & ang <= 30))
  expect_false(any(ang > 30 & ang < 330))
})

test_that("a seam-adjacent non-straddling arc touches the pole without wrapping (T-i2b)", {
  # [350, 360] and [0, 10] each span 10 degrees and touch the pole from one side.
  below <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 350, d_uci = 360))
  above <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 0, d_uci = 10))
  expect_equal(layer_data_for(below, "GeomSsmArc")$xmax, 360)
  expect_equal(layer_data_for(above, "GeomSsmArc")$xmax, 10)
  # Neither spans more than its 10 degrees.
  expect_false(any(arc_grob_angles(below) > 20 & arc_grob_angles(below) < 340))
  expect_false(any(arc_grob_angles(above) > 20 & arc_grob_angles(above) < 340))
})

test_that("an interval spanning a full circle is rejected from setup_data (T-i2c)", {
  bad <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = -100, d_uci = 300))
  expect_error(ggplot2::ggplot_build(bad), "full circle")
  # A genuine seam-crossing interval (min > max, short way) is still accepted.
  ok <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 350, d_uci = 10))
  expect_no_error(ggplot2::ggplot_build(ok))
})

# --- T-i3: the 0/360 pole draws at one place for either float label -----------

test_that("points at displacement 0 and 360 draw at the identical position (T-i3)", {
  p <- point_plot(data.frame(a_est = c(0.3, 0.3), d_est = c(0, 360)))
  xy <- point_grob_xy(p)
  expect_equal(xy$x[[1]], xy$x[[2]], tolerance = 1e-12)
  expect_equal(xy$y[[1]], xy$y[[2]], tolerance = 1e-12)
})

# --- T-arc0: zero-width wedge drops ------------------------------------------

test_that("a zero-width interval draws no wedge, not a degenerate line (T-arc0)", {
  # displacement_min == displacement_max -> span 0 -> dropped in setup_data.
  p <- arc_plot(data.frame(a_lci = 0.2, a_uci = 0.3, d_lci = 90, d_uci = 90))
  expect_equal(nrow(layer_data_for(p, "GeomSsmArc")), 0)
})

# --- I4: flat / undefined rows are dropped ------------------------------------

test_that("a flat profile (NA displacement) is dropped, not mis-drawn (I4)", {
  df <- data.frame(a_est = c(0.3, NA), d_est = c(90, NA))
  p <- point_plot(df)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(nrow(layer_data_for(p, "GeomSsmPoint")), 1)
})

test_that("a defined estimate with an undefined CI draws a point but no wedge", {
  df <- data.frame(
    a_est = 0.3, d_est = 90,
    a_lci = NA_real_, a_uci = NA_real_, d_lci = NA_real_, d_uci = NA_real_
  )
  p <- ggcircumplex(octants(), amax = 0.5) +
    geom_ssm_arc(data = df, mapping = ggplot2::aes(
      amplitude_min = a_lci, amplitude_max = a_uci,
      displacement_min = d_lci, displacement_max = d_uci)) +
    geom_ssm_point(data = df, mapping = ggplot2::aes(
      amplitude = a_est, displacement = d_est))
  expect_equal(nrow(layer_data_for(p, "GeomSsmArc")), 0)   # no wedge
  expect_equal(nrow(layer_data_for(p, "GeomSsmPoint")), 1) # one point
})

# --- amax / n soft-deprecation (unconditional note, never an error) -----------

test_that("supplying the retired amax/n geom arguments notes once and never errors", {
  rlang::reset_message_verbosity("circumplex_geom_geom_ssm_point_amax")
  rlang::reset_message_verbosity("circumplex_geom_geom_ssm_arc_n")
  expect_message(geom_ssm_point(amax = 0.5), "deprecated")
  expect_message(geom_ssm_arc(n = 100), "deprecated")
  # Not supplying them is silent.
  expect_no_message(geom_ssm_point())
  expect_no_message(geom_ssm_arc())
  # And a plot that passes a stray amax still renders correctly (self-heals).
  p <- suppressMessages(
    ggcircumplex(octants(), amax = 0.5) +
      geom_ssm_point(data = data.frame(a_est = 0.3, d_est = 45),
                     mapping = ggplot2::aes(amplitude = a_est, displacement = d_est),
                     amax = 0.9)
  )
  expect_no_error(ggplot2::ggplot_build(p))
})

# --- T2: na.rm opt-in warn-parity ---------------------------------------------
# Default na.rm = TRUE drops degenerate rows silently (unchanged); na.rm = FALSE
# warns with the dropped-row count before dropping (ggplot2 convention).

test_that("geom_ssm_point warns by count under na.rm = FALSE, silent under TRUE (T2)", {
  df <- data.frame(a_est = c(0.3, NA, 0.2), d_est = c(90, NA, 45)) # one degenerate
  build <- function(na.rm) {
    ggplot2::ggplot_build(
      ggcircumplex(octants(), amax = 0.5) +
        geom_ssm_point(
          data = df, na.rm = na.rm,
          mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est)
        )
    )
  }
  expect_warning(build(FALSE), "[Rr]emoved 1 row")
  expect_no_warning(build(TRUE))
})

test_that("geom_ssm_arc warns by count under na.rm = FALSE, silent under TRUE (T2)", {
  # Two incomplete-CI rows (missing bounds) among three.
  df <- data.frame(
    a_lci = c(0.2, NA, 0.1), a_uci = c(0.3, 0.4, 0.2),
    d_lci = c(40, 50, NA), d_uci = c(60, 70, 30)
  )
  build <- function(na.rm) {
    ggplot2::ggplot_build(
      ggcircumplex(octants(), amax = 0.5) +
        geom_ssm_arc(
          data = df, na.rm = na.rm,
          mapping = ggplot2::aes(
            amplitude_min = .data$a_lci, amplitude_max = .data$a_uci,
            displacement_min = .data$d_lci, displacement_max = .data$d_uci
          )
        )
    )
  }
  expect_warning(build(FALSE), "[Rr]emoved 2 row")
  expect_no_warning(build(TRUE))
})

test_that("na.rm = FALSE with no degenerate rows does not warn (T2)", {
  df <- data.frame(a_est = c(0.3, 0.2), d_est = c(90, 45))
  p <- ggcircumplex(octants(), amax = 0.5) +
    geom_ssm_point(
      data = df, na.rm = FALSE,
      mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est)
    )
  expect_no_warning(ggplot2::ggplot_build(p))
})

# --- visual regression --------------------------------------------------------

test_that("a canvas-plus-geoms plot renders (visual regression)", {
  skip_on_ci() # bootstrap CI positions are BLAS-sensitive (legacy lesson)
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  p <- ggcircumplex(octants(), amax = 0.5) +
    geom_ssm_arc(data = res$results, mapping = ggplot2::aes(
      amplitude_min = a_lci, amplitude_max = a_uci,
      displacement_min = d_lci, displacement_max = d_uci, group = Label)) +
    geom_ssm_point(data = res$results, mapping = ggplot2::aes(
      amplitude = a_est, displacement = d_est, group = Label))
  vdiffr::expect_doppelganger("ggcircumplex with ssm geoms", p)
})
