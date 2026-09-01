# Model-based trajectory plotting from a per-time-point table (M35).
#
# Fixture provenance: every fixture below is written literally in-file with
# KNOWN displacement values -- no simulation, no seed, no committed data. The
# table shape is the one a model-based workflow assembles from ssm_draws()
# evaluated at each time point (see vignettes/growth-ssm-analysis.Rmd), so the
# fixtures pin the contract that vignette relies on.
#
# The seam fixture deliberately crosses 0/360 and the wide-arc fixture
# deliberately exceeds a half-turn: both are regimes where a plausible-looking
# unwrap renders a wrong figure without erroring (LESSONS M27, M33), so they
# are asserted at the data level rather than by eye or by snapshot.

# A five-wave trajectory whose displacement drifts across the 0/360 seam.
traj_table <- function(certified = c(TRUE, TRUE, FALSE, TRUE, TRUE)) {
  out <- data.frame(
    wave = c(0, 1, 2, 3, 4),
    a_est = c(0.60, 0.55, 0.52, 0.58, 0.63),
    a_lci = c(0.48, 0.43, 0.40, 0.46, 0.51),
    a_uci = c(0.72, 0.67, 0.64, 0.70, 0.75),
    d_est = c(350, 355, 2, 8, 12),
    d_lci = c(340, 345, 352, 358, 2),
    d_uci = c(0, 5, 12, 18, 22)
  )
  if (!is.null(certified)) out$certified <- certified
  out
}

has_shape_layer <- function(p) {
  any(vapply(p$layers, function(l) "shape" %in% names(l$mapping), logical(1)))
}

x_scale <- function(p) ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]

# Happy path and the continuous axis (AC1) ------------------------------------

test_that("a trajectory table plots on a continuous time axis", {
  p <- ssm_plot_trajectory(traj_table(), time = "wave")

  expect_s3_class(p, "ggplot")
  # The substantive difference from the occasions path: time is numeric, so
  # unequally spaced waves would be drawn at their actual spacing.
  expect_true(inherits(x_scale(p), "ScaleContinuous"))
  expect_true("wave" %in% names(p$data))
  expect_type(p$data$wave, "double")
  expect_equal(sort(unique(p$data$wave)), c(0, 1, 2, 3, 4))
})

test_that("the time column keeps the caller's own name and labels the axis", {
  tbl <- traj_table()
  names(tbl)[names(tbl) == "wave"] <- "month"
  p <- ssm_plot_trajectory(tbl, time = "month")

  expect_true("month" %in% names(p$data))
  expect_false("wave" %in% names(p$data))
  expect_equal(p$labels$x, "month")
})

test_that("only the panels the table can fill are drawn", {
  # Amplitude and displacement alone: the minimum legal table.
  p <- ssm_plot_trajectory(traj_table(), time = "wave")
  expect_equal(levels(p$data$Panel), c("Amplitude", "Displacement"))

  # Adding an elevation triple adds exactly its panel, in canonical order.
  tbl <- traj_table()
  tbl$e_est <- c(2.0, 2.1, 2.2, 2.1, 2.0)
  tbl$e_lci <- tbl$e_est - 0.2
  tbl$e_uci <- tbl$e_est + 0.2
  p2 <- ssm_plot_trajectory(tbl, time = "wave")
  expect_equal(levels(p2$data$Panel), c("Elevation", "Amplitude", "Displacement"))

  # A full table yields all five, and drop_xy removes the coordinate panels.
  tbl$x_est <- tbl$e_est
  tbl$x_lci <- tbl$e_lci
  tbl$x_uci <- tbl$e_uci
  tbl$y_est <- tbl$e_est
  tbl$y_lci <- tbl$e_lci
  tbl$y_uci <- tbl$e_uci
  expect_equal(
    levels(ssm_plot_trajectory(tbl, time = "wave")$data$Panel),
    c("Elevation", "X-value", "Y-value", "Amplitude", "Displacement")
  )
  expect_equal(
    levels(ssm_plot_trajectory(tbl, time = "wave", drop_xy = TRUE)$data$Panel),
    c("Elevation", "Amplitude", "Displacement")
  )
})

# The seam and the interval span are M33's machinery, shared (AC2) ------------

test_that("a seam-straddling model trajectory renders as one continuous path", {
  tbl <- traj_table()
  d <- ssm_plot_trajectory(tbl, time = "wave")$data
  d <- d[d$Parameter == "d", ]
  d <- d[order(d$wave), ]

  # Unwrapped: the 355 -> 2 step is +7 degrees, never -353.
  expect_equal(diff(d$est), c(5, 7, 6, 4))
  # The series leaves [0, 360) rather than jumping back to it.
  expect_true(any(d$est > 360))
  expect_true(all(d$est == cummax(d$est)))
})

test_that("each interval's drawn width equals its stored arc span", {
  # The invariant at EVERY width. Asserting the width merely falls under 180
  # is guaranteed by the very (-180, 180] clamping bug it would be meant to
  # catch, so it has no teeth (LESSONS M33).
  tbl <- traj_table()
  d <- ssm_plot_trajectory(tbl, time = "wave")$data
  d <- d[d$Parameter == "d", ]
  d <- d[order(d$wave), ]

  expect_equal(d$uci - d$lci, ssm_arc_span(tbl$d_lci, tbl$d_uci))
  # Each estimate sits inside its own interval -- the property the inverted
  # ribbon violated.
  expect_true(all(d$lci <= d$est & d$est <= d$uci))
})

test_that("an interval wider than a half-turn is not inverted", {
  # A near-origin wave: ssm_draws() reports a "displacement unknown" interval
  # covering most of the circle. Placing each bound by its own signed distance
  # would clamp this into (-180, 180] and render it as the most precise wave in
  # the series (LESSONS M33).
  #
  # The estimate is deliberately OFF-CENTRE within the arc (110 degrees above
  # the lower bound, 230 below the upper). A wide arc centred on its estimate
  # does not discriminate -- the clamping expression reproduces the correct
  # width there by symmetry, so a centred fixture would have no teeth. Diffuse
  # near-origin draws give an asymmetric interval anyway: the circular median
  # need not sit midway between the quantiles.
  tbl <- traj_table(certified = NULL)[1:3, ]
  tbl$certified <- c(TRUE, FALSE, TRUE)
  tbl$wave <- c(0, 1, 2)
  tbl$a_est <- c(0.60, 0.02, 0.55)
  tbl$a_lci <- c(0.48, 0.00, 0.43)
  tbl$a_uci <- c(0.72, 0.30, 0.67)
  tbl$d_est <- c(350, 300, 10)
  tbl$d_lci <- c(340, 190, 2)
  tbl$d_uci <- c(0, 170, 18)

  d <- ssm_plot_trajectory(tbl, time = "wave")$data
  d <- d[d$Parameter == "d", ]
  d <- d[order(d$wave), ]

  expect_equal(d$uci - d$lci, ssm_arc_span(tbl$d_lci, tbl$d_uci))
  expect_equal(d$uci[[2]] - d$lci[[2]], 340)
  expect_true(all(d$lci <= d$est & d$est <= d$uci))
  # The wide wave is the LEAST precise, not the most.
  expect_equal(which.max(d$uci - d$lci), 2L)
})

test_that("a time point with no defined displacement leaves a gap", {
  tbl <- traj_table()
  tbl$d_est[[3]] <- NA
  tbl$d_lci[[3]] <- NA
  tbl$d_uci[[3]] <- NA

  p <- ssm_plot_trajectory(tbl, time = "wave")
  d <- p$data[p$data$Parameter == "d", ]
  expect_true(is.na(d$est[d$wave == 2]))
  expect_equal(sum(is.na(d$est)), 1L)
  # The surrounding waves still unwrap across the gap rather than blanking.
  expect_equal(sum(!is.na(d$est)), 4L)

  expect_warning(
    ssm_plot_trajectory(tbl, time = "wave", na.rm = FALSE),
    "Removed 1 row with no defined displacement"
  )
  expect_silent(ssm_plot_trajectory(tbl, time = "wave"))
})

# Certification marking (AC1, AC2) -------------------------------------------

test_that("the certified column drives hollow marking on the displacement panel", {
  p <- ssm_plot_trajectory(traj_table(), time = "wave")
  d <- p$data[p$data$Parameter == "d", ]
  d <- d[order(d$wave), ]

  expect_type(d$Certified, "logical")
  expect_equal(d$Certified, c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_true(has_shape_layer(p))
  # Certification is a displacement-only verdict, but it is carried on every
  # row so the plot needs no second join; the marking is what is panel-scoped.
  shape_layers <- Filter(function(l) "shape" %in% names(l$mapping), p$layers)
  expect_equal(unique(as.character(shape_layers[[1]]$data$Parameter)), "d")
})

test_that("the certification legend draws both keys on the table path too", {
  # Same defect, second entry point: an all-TRUE certified column is exactly
  # the case a model-based ssm_draws() workflow produces most often.
  all_true <- legend_key_glyphs(
    ssm_plot_trajectory(traj_table(certified = rep(TRUE, 5)), time = "wave"),
    "Displacement interpretable"
  )
  expect_length(all_true, 2)
  expect_equal(unname(lengths(all_true)), c(1L, 1L)) # one glyph per key, never overdrawn
  expect_equal(sort(unname(unlist(all_true))), c(1, 16))

  # The mixed case already drew both keys; assert it is left alone.
  mixed <- legend_key_glyphs(
    ssm_plot_trajectory(traj_table(), time = "wave"),
    "Displacement interpretable"
  )
  expect_equal(unname(lengths(mixed)), c(1L, 1L))
  expect_equal(sort(unname(unlist(mixed))), c(1, 16))
})

test_that("a table with no certified column makes no interpretability claim", {
  p <- ssm_plot_trajectory(traj_table(certified = NULL), time = "wave")

  expect_true(all(is.na(p$data$Certified)))
  expect_false(has_shape_layer(p))
  # No shape scale means no "Displacement interpretable" legend at all.
  expect_false(
    any(vapply(p$scales$scales, function(s) "shape" %in% s$aesthetics, logical(1)))
  )
  # The points are still drawn -- only the verdict is withheld.
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})

# Malformed input errors specifically (AC3) -----------------------------------

test_that("missing required columns are named", {
  expect_error(
    ssm_plot_trajectory(traj_table()[, c("wave", "a_est", "d_est")], time = "wave"),
    "Missing column\\(s\\): a_lci, a_uci, d_lci, d_uci"
  )
  expect_error(
    ssm_plot_trajectory(traj_table()[, c("wave", "d_est", "d_lci", "d_uci")], time = "wave"),
    "Missing column\\(s\\): a_est, a_lci, a_uci"
  )
})

test_that("an incomplete optional triple is refused rather than half-drawn", {
  tbl <- traj_table()
  tbl$e_est <- 1
  expect_error(
    ssm_plot_trajectory(tbl, time = "wave"),
    "column\\(s\\) e are incomplete.*Missing: e_lci, e_uci"
  )
})

test_that("the time column must be numeric and non-degenerate", {
  tbl <- traj_table()

  expect_error(
    ssm_plot_trajectory(transform(tbl, wave = letters[1:5]), time = "wave"),
    "must be numeric, not character"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, wave = factor(1:5)), time = "wave"),
    "must be numeric, not factor"
  )
  expect_error(
    ssm_plot_trajectory(tbl[1, ], time = "wave"),
    "at least two distinct time points; found 1"
  )
  expect_error(
    ssm_plot_trajectory(tbl[0, ], time = "wave"),
    "at least two distinct time points; found 0"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, wave = c(0, 0, 1, 2, 3)), time = "wave"),
    "repeated value\\(s\\): 0"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, wave = c(0, 1, 2, 3, NA)), time = "wave"),
    "must be finite"
  )
})

test_that("the time argument itself is validated", {
  tbl <- traj_table()

  expect_error(ssm_plot_trajectory(tbl), "`time` must name")
  expect_error(ssm_plot_trajectory(tbl, time = "tick"), "was not found")
  expect_error(ssm_plot_trajectory(tbl, time = 1), "single string")
  expect_error(ssm_plot_trajectory(tbl, time = c("wave", "wave")), "single string")
  expect_error(ssm_plot_trajectory(tbl, time = NA_character_), "single string")

  names(tbl)[names(tbl) == "wave"] <- "Group"
  expect_error(ssm_plot_trajectory(tbl, time = "Group"), "collides with a name")
})

test_that("a non-data-frame input is refused by name", {
  expect_error(ssm_plot_trajectory(1:5), "needs an SSM results object")
  expect_error(ssm_plot_trajectory("wave"), "not character")
  expect_error(ssm_plot_trajectory(list(a_est = 1)), "not list")
})

test_that("broken bounds and columns are refused rather than silently dropped", {
  tbl <- traj_table()

  expect_error(
    ssm_plot_trajectory(transform(tbl, a_lci = c(NA, .4, .4, .4, .4)), time = "wave"),
    "`a_lci` is not finite at 1 row\\(s\\) where `a_est` is defined"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, d_uci = c(Inf, 5, 12, 18, 22)), time = "wave"),
    "`d_uci` is not finite at 1 row\\(s\\)"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, a_est = as.character(a_est)), time = "wave"),
    "Column `a_est` must be numeric, not character"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, certified = c("y", "y", "n", "y", "y")), time = "wave"),
    "`certified` must be logical"
  )

  dup <- cbind(tbl, tbl["d_est"])
  expect_error(
    ssm_plot_trajectory(dup, time = "wave"),
    "duplicated column name\\(s\\): d_est"
  )
})

test_that("the shared argument checks fire on the data frame method too", {
  tbl <- traj_table()

  expect_error(ssm_plot_trajectory(tbl, time = "wave", base_size = Inf), "positive finite")
  expect_error(ssm_plot_trajectory(tbl, time = "wave", base_size = 0), "positive finite")
  expect_error(ssm_plot_trajectory(tbl, time = "wave", base_size = NA_real_), "positive finite")
  expect_error(ssm_plot_trajectory(tbl, time = "wave", drop_xy = NA), "is.na\\(drop_xy\\)")
  expect_error(ssm_plot_trajectory(tbl, time = "wave", na.rm = "yes"), "is_flag\\(na.rm\\)")
  expect_warning(ssm_plot_trajectory(tbl, time = "wave", nonsense = 1), "nonsense")
})

# Rendered appearance ---------------------------------------------------------

test_that("the trajectory-table plot renders as expected", {
  skip_if_not_installed("vdiffr")
  # Secondary to the data-level assertions above: a passing baseline is a
  # rendering guard against unintended drift, never the fence for a behavioral
  # criterion (LESSONS M31). Unlike the occasions baselines these fixtures are
  # literal, not bootstrapped, so they carry no BLAS sensitivity.
  vdiffr::expect_doppelganger(
    "trajectory table",
    ssm_plot_trajectory(traj_table(), time = "wave")
  )
  vdiffr::expect_doppelganger(
    "trajectory table uncertified",
    ssm_plot_trajectory(
      transform(traj_table(), certified = c(TRUE, FALSE, FALSE, TRUE, TRUE)),
      time = "wave"
    )
  )
  vdiffr::expect_doppelganger(
    "trajectory table no certification",
    ssm_plot_trajectory(traj_table(certified = NULL), time = "wave")
  )
})

# Review findings, M35 (both confirmed wrong-answer channels) -----------------

test_that("a non-finite estimate is refused rather than blanking the series", {
  # is.na(Inf) is FALSE, so an infinite estimate slips past the NA-based
  # located predicate, reaches `Inf %% 360` -> NaN in the unwrap, and cumsum
  # propagates that NaN over every LATER time point -- four good waves vanish
  # with no error and no warning. Same family as the M32 lesson: guard
  # user-facing numerics with !is.finite(), never is.na().
  tbl <- traj_table()
  expect_error(
    ssm_plot_trajectory(transform(tbl, d_est = c(350, Inf, 2, 8, 12)), time = "wave"),
    "`d_est` is not finite at 1 row"
  )
  expect_error(
    ssm_plot_trajectory(transform(tbl, a_est = c(.6, .55, -Inf, .58, .63)), time = "wave"),
    "`a_est` is not finite at 1 row"
  )
  # NaN still reads as missing (is.na(NaN) is TRUE), leaving a gap, not an error.
  gap <- transform(tbl, d_est = c(350, NaN, 2, 8, 12))
  gap$d_lci[[2]] <- NA
  gap$d_uci[[2]] <- NA
  p <- ssm_plot_trajectory(gap, time = "wave")
  d <- p$data[p$data$Parameter == "d", ]
  expect_equal(sum(is.na(d$est)), 1L)
  expect_equal(sum(!is.na(d$est)), 4L)
})

test_that("a time column naming a parameter column is refused, not clobbered", {
  # `dat[[time]] <- tv` runs before the parameter loop, so a time column named
  # after a parameter is silently overwritten by that parameter's values: the
  # real time variable disappears and the figure draws a meaningless diagonal
  # with no error. The reserved-name guard exists to refuse exactly this.
  tbl <- traj_table()
  for (nm in c("a_est", "a_lci", "d_uci", "certified")) {
    expect_error(
      ssm_plot_trajectory(tbl, time = nm),
      "collides with a name",
      info = nm
    )
  }
})
