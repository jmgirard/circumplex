# Boundary/regression battery for geom_ssm_path(), the on-circle movement path
# across occasions (M37). The layer hands amplitude/displacement to the coord as
# y/x and lets coord_circumplex()'s non-linear munching curve each segment; what
# the layer owns is ordering, seam handling, gap breaking, and arrows. These
# tests fence those at the data level (post setup_data / ggplot_build) AND at the
# grob level, because devtools::check() runs clean on a visually wrong figure
# (M13/M27 lesson).

# --- helpers ------------------------------------------------------------------

# Built data for the path layer (located by geom class, never by a hardcoded
# data[[i]] index -- ggcircumplex()'s geom_blank shifts the numbering; M31).
path_layer_data <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSsmPath"),
                      logical(1)))
  b$data[[idx[[1]]]]
}
path_layer_index <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomSsmPath"),
               logical(1)))[[1]]
}
path_plot <- function(df, amax = 0.5, ...) {
  ggcircumplex(octants(), amax = amax) +
    geom_ssm_path(
      data = df,
      mapping = ggplot2::aes(
        amplitude = .data$a_est, displacement = .data$d_est
      ),
      ...
    )
}

# --- T1/AC2: the 0/360 seam is crossed the short way --------------------------

test_that("a path across the 0/360 seam travels the short arc (AC2)", {
  # 350 -> 10 is a 20 degree step counterclockwise across the pole. Fed to the
  # coord as raw [0, 360) values it would sweep 340 degrees the wrong way round;
  # the layer must unwrap first so the coord's periodic transform carries it the
  # short way (the same extension convention geom_ssm_arc() uses).
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, 0.3), d_est = c(350, 10))
  ))
  expect_equal(diff(range(d$x)), 20)
  expect_false(isTRUE(all.equal(diff(range(d$x)), 340)))
})

test_that("seam unwrapping accumulates across several occasions (AC2)", {
  # Four steps of +20 each, crossing the seam twice in the same direction: the
  # branch must keep extending (350, 370, 390, 410), not fold back into [0, 360).
  d <- path_layer_data(path_plot(
    data.frame(a_est = rep(0.3, 4), d_est = c(350, 10, 30, 50))
  ))
  expect_equal(diff(range(d$x)), 60)
  expect_equal(diff(d$x[order(d$x)]), c(20, 20, 20))
})

test_that("a clockwise seam crossing unwraps downward (AC2)", {
  # 10 -> 350 is -20, not +340: the unwrap must be signed.
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, 0.3), d_est = c(10, 350))
  ))
  expect_equal(diff(range(d$x)), 20)
  expect_true(min(d$x) < 0)
})

# --- T3/AC3: degenerate occasions break the path, they do not corrupt it ------

test_that("an undefined occasion breaks the path and the tail is bridged (AC3)", {
  # A flat (zero-amplitude) occasion has no displacement. It must leave a gap,
  # and the occasions after it must be unwrapped relative to the last *defined*
  # occasion -- 350 -> (gap) -> 30 continues to 390, not back to 30. A naive
  # implementation that propagated NA onward (angle_unwrap()'s own cumsum
  # policy) would blank the whole tail instead of gapping one occasion.
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, 0, 0.3), d_est = c(350, NA, 30))
  ))
  expect_true(is.na(d$x[[2]]))
  expect_equal(d$x, c(350, NA, 390))
})

test_that("a non-finite displacement is guarded before the unwrap (AC3)", {
  # is.na(Inf) is FALSE, so an infinite displacement sails through an is.na()
  # location test straight into angle_unwrap()'s cumsum() and NaNs out every
  # later occasion (the recurring M32/M35 trap). The guard must demote it to a
  # gap, leaving the occasion AFTER it intact.
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, 0.3, 0.3), d_est = c(10, Inf, 30))
  ))
  expect_true(is.na(d$x[[2]]))
  expect_false(is.nan(d$x[[3]]))
  expect_equal(d$x[[3]], 30)
})

test_that("a non-finite amplitude is guarded too (AC3)", {
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, Inf, 0.3), d_est = c(10, 20, 30))
  ))
  expect_true(is.na(d$y[[2]]))
  expect_true(is.na(d$x[[2]]))
  # The surviving occasions keep their own values.
  expect_equal(d$y[c(1, 3)], c(0.3, 0.3))
})

test_that("an all-undefined series yields no drawable path (AC3)", {
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(NA_real_, NA_real_), d_est = c(NA_real_, NA_real_))
  ))
  expect_true(all(is.na(d$x)))
})

# --- T3/AC1: ordering and series separation -----------------------------------

test_that("the order aesthetic sorts within a series (AC1)", {
  # Occasions supplied out of time order, with `order` naming the true order.
  # T10 sorted as text lands before T2; the order aesthetic is the defence.
  df <- data.frame(
    a_est = c(0.3, 0.4, 0.5),
    d_est = c(30, 10, 20),
    step = c(3, 1, 2)
  )
  p <- ggcircumplex(octants(), amax = 0.6) +
    geom_ssm_path(
      data = df,
      mapping = ggplot2::aes(
        amplitude = .data$a_est, displacement = .data$d_est,
        order = .data$step
      )
    )
  d <- path_layer_data(p)
  expect_equal(d$x, c(10, 20, 30))
  expect_equal(d$y, c(0.4, 0.5, 0.3))
})

test_that("without an order aesthetic the data row order is honoured (AC1)", {
  d <- path_layer_data(path_plot(
    data.frame(a_est = c(0.3, 0.4, 0.5), d_est = c(30, 10, 20))
  ))
  expect_equal(d$x, c(30, 10, 20))
})

test_that("group separates series and each unwraps independently (AC1)", {
  # Two series crossing the seam in opposite directions. Unwrapping them as one
  # pooled sequence would contaminate each with the other's branch.
  df <- data.frame(
    a_est = rep(0.3, 4),
    d_est = c(350, 10, 10, 350),
    series = c("a", "a", "b", "b")
  )
  p <- ggcircumplex(octants(), amax = 0.5) +
    geom_ssm_path(
      data = df,
      mapping = ggplot2::aes(
        amplitude = .data$a_est, displacement = .data$d_est,
        group = .data$series
      )
    )
  d <- path_layer_data(p)
  a <- d$x[d$group == d$group[[1]]]
  b <- d$x[d$group != d$group[[1]]]
  expect_equal(sort(a), c(350, 370))
  expect_equal(sort(b), c(-10, 10))
})

# --- AC1: the coord curves the segments, the layer does not ---------------------

test_that("segments are munched along the polar geodesic (AC1)", {
  # coord_circumplex() reports is_linear() == FALSE, so ggplot2 munches each
  # segment into many small pieces that follow the circle. If the path were
  # drawn as straight cartesian chords, the grob would carry exactly one point
  # per occasion. This is what makes the layer "supply ordering, not drawing".
  df <- data.frame(a_est = c(0.3, 0.3), d_est = c(0, 180))
  gr <- ggplot2::layer_grob(path_plot(df), path_layer_index(path_plot(df)))[[1]]
  expect_gt(length(as.numeric(gr$x)), nrow(df))
  # A constant-amplitude half-turn must stay at a constant radius: every munched
  # vertex sits on the same circle, which a straight chord would cut across.
  xs <- as.numeric(gr$x); ys <- as.numeric(gr$y)
  r <- sqrt((xs - 0.5)^2 + (ys - 0.5)^2)
  expect_equal(max(r) - min(r), 0, tolerance = 1e-6)
})

# --- T4/AC4: arrowheads --------------------------------------------------------

test_that("arrowheads are absent by default and render when requested (AC4)", {
  df <- data.frame(a_est = c(0.2, 0.3, 0.4), d_est = c(10, 60, 120))
  # Verified at grob level, not by baseline alone: an arrow parameter that is
  # accepted but dropped on the way to the grob renders an identical-looking
  # figure at low resolution.
  grob_of <- function(p) ggplot2::layer_grob(p, path_layer_index(p))[[1]]

  bare <- grob_of(path_plot(df))
  expect_null(bare$arrow)

  arrowed <- grob_of(path_plot(
    df,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))
  ))
  expect_s3_class(arrowed$arrow, "arrow")
})

# --- T5/AC5: the ssm_plot_circle(path = TRUE) convenience surface -------------

# Three occasions whose displacements differ, built by circularly shifting the
# scale columns: shifting the octant scores by one position rotates the fitted
# profile by 45 degrees, so each occasion lands somewhere different on the
# circle and a mis-ordered path is visible in the built layer's x sequence.
occasions_fixture <- function(labels) {
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  base <- jz2017[, scales]
  parts <- lapply(seq_along(labels), function(k) {
    # Circular shift by (k - 1) positions; k = 1 is the unshifted profile.
    idx <- ((seq_along(scales) - 1 + (k - 1)) %% length(scales)) + 1
    d <- base[, scales[idx]]
    names(d) <- scales
    d$id <- seq_len(nrow(d))
    d$occasion <- labels[[k]]
    d
  })
  do.call(rbind, parts)
}

path_layer_of <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomSsmPath"),
                      logical(1)))
  expect_length(idx, 1)
  b$data[[idx[[1]]]]
}

test_that("path = TRUE follows details$occasions order, not alphabetical (AC5)", {
  data("jz2017")
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  # T1, T2, T10 -- alphabetical sorting puts T10 second and reverses the middle
  # of the series, the M33 lesson this criterion exists to fence.
  res <- ssm_analyze_long(
    occasions_fixture(c("T1", "T2", "T10")),
    scales = scales, id = "id", occasion = "occasion"
  )
  expect_equal(res$details$occasions, c("T1", "T2", "T10"))

  d <- path_layer_of(ssm_plot_circle(res, path = TRUE))
  # The path's rows must be in supplied-occasion order. Compare against the
  # results table read in that same order, so this asserts the ordering rather
  # than restating the fixture's numbers.
  want <- res$results[match(c("T1", "T2", "T10"), res$results$Occasion), ]
  expect_equal(d$y, want$a_est, tolerance = 1e-8)
  # Displacements are compared modulo the unwrap the layer applies.
  expect_equal(d$x %% 360, want$d_est %% 360, tolerance = 1e-8)
  # And the fixture actually moves, so the ordering assertion has teeth.
  expect_gt(diff(range(d$x)), 1)
})

test_that("path = TRUE is refused for an object with no occasions (AC5)", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "NARPD")
  expect_error(ssm_plot_circle(res, path = TRUE), "needs an SSM object with occasions")
})

test_that("path = FALSE adds no path layer (AC5)", {
  data("jz2017")
  scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  res <- ssm_analyze_long(
    occasions_fixture(c("T1", "T2")),
    scales = scales, id = "id", occasion = "occasion"
  )
  p <- ssm_plot_circle(res)
  expect_false(any(vapply(
    p$layers, function(l) inherits(l$geom, "GeomSsmPath"), logical(1)
  )))
})

# --- AC6: rendered appearance -------------------------------------------------

test_that("the movement path renders as expected", {
  # Secondary to the data-level assertions above: this is a rendering guard, not
  # the fence for any acceptance criterion. The M37 render-and-inspect pass is
  # what caught the defect a baseline cannot -- an arrowhead drawn underneath
  # the terminal occasion's marker, which hides the direction of time while
  # every data-level fence and the baseline itself still pass.
  skip_on_ci()
  df <- data.frame(
    a_est = c(0.35, 0.45, 0.40),
    d_est = c(330, 355, 20)
  )
  vdiffr::expect_doppelganger(
    "ssm path across the seam",
    ggcircumplex(octants(), amax = 0.6) +
      geom_ssm_point(
        data = df,
        mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est),
        size = 2
      ) +
      geom_ssm_path(
        data = df,
        mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est),
        arrow = ggplot2::arrow(
          length = ggplot2::unit(0.18, "inches"), type = "closed"
        ),
        linewidth = 0.7
      )
  )
  vdiffr::expect_doppelganger(
    "ssm path with a gap",
    ggcircumplex(octants(), amax = 0.6) +
      geom_ssm_path(
        data = data.frame(
          a_est = c(0.35, 0, 0.40, 0.45),
          d_est = c(330, NA, 20, 60)
        ),
        mapping = ggplot2::aes(amplitude = .data$a_est, displacement = .data$d_est)
      )
  )
})
