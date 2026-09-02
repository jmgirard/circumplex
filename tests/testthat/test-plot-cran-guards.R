# CRAN-visible guards for the plot functions (M120 AC4 i).
#
# Every existing block for these four is a vdiffr snapshot, and vdiffr skips on
# CRAN by design -- so on CRAN nothing exercised them at all, before this
# milestone or after it. These blocks assert the BUILT layer data instead of an
# image, so they run wherever the package is checked: a plot that errors during
# construction, drops its data, or maps an amplitude to a non-finite radius
# fails here rather than only in CI.
#
# They assert the NUMBERS the layers carry -- each profile's displacement and
# amplitude, which the built data holds directly -- not the picture. Appearance
# stays vdiffr's job off CRAN; nothing here is a substitute for it.

plot_guard_result <- function() {
  # An exact first-harmonic profile per measure, so the built coordinates are
  # determined by the profile rather than by a bootstrap draw.
  theta <- as.numeric(octants()) * pi / 180
  dat <- as.data.frame(do.call(rbind, lapply(c(30, 350), function(d) {
    2 + 0.5 * cos(theta - d * pi / 180)
  })))
  names(dat) <- PANO()
  # Repeat each profile with a small deterministic perturbation: identical rows
  # give a zero-variance block and a cor() warning, and the group mean stays the
  # profile, so the plotted point is still the closed form of it.
  dat <- dat[rep(1:2, each = 3), ]
  dat[] <- dat + outer(seq(-1, 1, length.out = 6), rep(1e-6, ncol(dat)))
  dat$Group <- rep(c("a", "b"), each = 3)
  ssm_analyze(dat, scales = PANO(), angles = octants(),
              grouping = "Group", boots = 10)
}

built_layers <- function(p) {
  expect_true(ggplot2::is_ggplot(p))
  b <- ggplot2::ggplot_build(p)
  expect_gt(length(b$data), 0L)
  b$data
}

# The layer whose row count matches the profiles and which carries both plotted
# coordinates. Erroring rather than returning NULL matters: a plot that silently
# dropped its data layer would otherwise make the assertions below vacuous.
profile_layer <- function(p, n_prof) {
  expect_true(ggplot2::is_ggplot(p))
  layers <- ggplot2::ggplot_build(p)$data
  # Selected on the SSM aesthetics, not on the row count: the amplitude axis
  # layer also has one row per profile by coincidence and would otherwise be
  # picked, and its x values are axis breaks rather than displacements.
  hit <- Filter(function(d) {
    nrow(d) == n_prof && all(c("x", "y", "amplitude", "displacement") %in% names(d))
  }, layers)
  expect_length(hit, 1L)
  hit[[1]]
}

test_that("ssm_plot_circle places each profile at its own displacement and amplitude", {
  res <- plot_guard_result()
  d <- profile_layer(ssm_plot_circle(res), nrow(res$results))
  # x is the displacement in degrees and y the amplitude, so a profile at 350
  # must not be drawn at -10 and an amplitude must not arrive rescaled.
  expect_equal(as.numeric(d$x), as.numeric(res$results$d_est), tolerance = 1e-8)
  expect_equal(as.numeric(d$y), as.numeric(res$results$a_est), tolerance = 1e-8)
  expect_equal(as.numeric(d$x), c(30, 350), tolerance = 1e-6)
})

test_that("ssm_plot_curve draws each profile's cosine peaking at its displacement", {
  res <- plot_guard_result()
  layers <- ggplot2::ggplot_build(ssm_plot_curve(res))$data
  curves <- Filter(function(d) all(c("x", "y", "group") %in% names(d)) && nrow(d) > 20, layers)
  expect_gt(length(curves), 0L)
  d <- curves[[1]]
  expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
  groups <- split(d, d$group)
  expect_length(groups, nrow(res$results))
  # The curve is evaluated on a grid, so its maximum lands on a grid point, and
  # the grid spans only the plotted angular window -- a profile whose
  # displacement falls outside that window peaks at the window's edge, which is
  # a property of the drawn range and not of the estimate. So the expectation is
  # the grid point nearest the displacement, measured LINEARLY: a displacement
  # left on atan2's branch (-10 for 350) is nearest the low edge, where the
  # correct curve peaks near 350, so this still fails under that defect. The
  # bound is half a grid step, absolute in degrees -- testthat edition 3's
  # `tolerance` is relative, under which a 10x error passes at `tolerance = 5`.
  step <- stats::median(diff(sort(unique(d$x))))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    dd <- as.numeric(res$results$d_est[[i]])
    expected <- g$x[which.min(abs(g$x - dd))]
    expect_lt(abs(g$x[which.max(g$y)] - expected), step / 2)
  }
})

test_that("ssm_plot_contrast plots the contrast row the object reports", {
  theta <- as.numeric(octants()) * pi / 180
  dat <- as.data.frame(do.call(rbind, lapply(c(30, 350), function(d) {
    2 + 0.5 * cos(theta - d * pi / 180)
  })))
  names(dat) <- PANO()
  dat <- dat[rep(1:2, each = 3), ]
  dat[] <- dat + outer(seq(-1, 1, length.out = 6), rep(1e-6, ncol(dat)))
  dat$Group <- rep(c("a", "b"), each = 3)
  res <- ssm_analyze(dat, scales = PANO(), angles = octants(),
                     grouping = "Group", contrast = TRUE, boots = 10)
  p <- ssm_plot_contrast(res)
  expect_true(ggplot2::is_ggplot(p))
  # The contrast row is the LAST results row (second minus first level). What
  # the plot carries must be that row, not a profile row: plotting profile 1
  # while labelling it a contrast is the confusion this pins.
  contrast_row <- res$results[nrow(res$results), ]
  expect_setequal(names(p$data), c("Parameter", "Difference", "lci", "uci", "sig"))
  # One plotted difference per SSM parameter, each equal to the contrast row's
  # own estimate: plotting a profile row while labelling it a contrast, or
  # dropping a parameter, both fail here.
  expect_equal(
    as.numeric(p$data$Difference),
    as.numeric(unlist(contrast_row[, c("e_est", "x_est", "y_est",
                                       "a_est", "d_est")])),
    tolerance = 1e-8
  )
  # -40 is the second group minus the first across the 0/360 pole (350 - 30),
  # reported on (-180, 180]; 320 would be the same rotation on the wrong branch.
  expect_equal(as.numeric(contrast_row$d_est), -40, tolerance = 1e-6)
  layers <- ggplot2::ggplot_build(p)$data
  expect_true(any(vapply(layers, function(d) nrow(d) > 0L, logical(1))))
})

test_that("geom_ssm_path draws its vertices on the short arc across the pole", {
  res <- plot_guard_result()
  p <- ggcircumplex(octants(), labels = PANO()) +
    geom_ssm_path(
      ggplot2::aes(amplitude = a_est, displacement = d_est),
      data = res$results
    )
  expect_true(ggplot2::is_ggplot(p))
  layers <- ggplot2::ggplot_build(p)$data
  hit <- Filter(function(d) "displacement" %in% names(d), layers)
  expect_length(hit, 1L)
  d <- hit[[1]]
  # A path from 30 to 350 crosses the pole, so the second vertex is placed at
  # -10, the short way round -- not left at 350, which would draw the long arc
  # the wrong way about the circle.
  expect_equal(sort(as.numeric(d$displacement)), c(-10, 30), tolerance = 1e-8)
  expect_equal(as.numeric(d$amplitude), c(0.5, 0.5), tolerance = 1e-8)
  expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
})
