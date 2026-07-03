test_that("scale_x_circumplex() places breaks at the scale angles", {
  s <- scale_x_circumplex(octants())
  expect_true(inherits(s, "Scale"))
  expect_equal(s$breaks, octants())
})

test_that("default labels match the curve plot's degree formatting", {
  # The canvas and the axis share circumplex_degree_labels(); for the integer
  # octant angles this is the same text the curve plot has always shown.
  s <- scale_x_circumplex(octants())
  expect_equal(
    s$get_labels(octants()),
    paste0(octants(), "\U00B0")
  )
})

test_that("a character labels vector flows through", {
  s <- scale_x_circumplex(octants(), labels = PANO())
  expect_equal(s$get_labels(octants()), PANO())
})

test_that("an instrument supplies both angles and abbreviation labels", {
  data("csip")
  s <- scale_x_circumplex(instrument = csip)
  expect_equal(s$breaks, csip$Scales$Angle)
  expect_equal(s$get_labels(csip$Scales$Angle), csip$Scales$Abbrev)

  # An explicit labels argument still overrides the instrument's abbreviations
  s2 <- scale_x_circumplex(instrument = csip, labels = LETTERS[1:8])
  expect_equal(s2$get_labels(csip$Scales$Angle), LETTERS[1:8])
})

test_that("scale_x_circumplex() reproduces the curve plot's axis labels", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  angles <- res$details$angles

  # Default (numeric degree) axis
  p_curve <- ssm_plot_curve(res)
  b_curve <- ggplot2::ggplot_build(p_curve)
  x_curve <- b_curve$layout$panel_params[[1]]$x
  # Same breaks and labels produced by the standalone scale helper
  s <- scale_x_circumplex(angles)
  keep <- x_curve$breaks %in% angles
  expect_equal(
    x_curve$get_labels()[keep],
    s$get_labels(x_curve$breaks[keep])
  )
})

test_that("scale_x_circumplex() and ggcircumplex() resolve labels identically", {
  data("csip")
  # The shared resolver must give the axis the same labels the canvas draws
  s <- scale_x_circumplex(instrument = csip)
  # ggcircumplex draws the abbreviations as the displacement labels (layer 5)
  p <- ggcircumplex(instrument = csip)
  canvas_labels <- ggplot2::ggplot_build(p)$data[[5]]$label
  expect_setequal(s$get_labels(csip$Scales$Angle), canvas_labels)
})

test_that("scale_x_circumplex() validates its arguments", {
  data("csip")
  expect_error(scale_x_circumplex(octants(), labels = c("A", "B")))
  expect_error(scale_x_circumplex(instrument = mtcars))
})

test_that("scale_x_circumplex() labels fractional angles without rounding (R6)", {
  # sprintf("%.0f") rounded 22.5 -> "22" and 67.5 -> "68"; a 16-scale or custom
  # instrument with half-degree spacing must label its true angles.
  ang <- seq(22.5, 337.5, by = 45)
  labs <- scale_x_circumplex(ang)$get_labels(ang)
  expect_true("22.5\U00B0" %in% labs)
  expect_false("22\U00B0" %in% labs)
  expect_false("68\U00B0" %in% labs)
})

test_that("ggcircumplex() draws fractional angles exactly, not rounded (R5)", {
  ang <- seq(22.5, 337.5, by = 45)
  b <- ggplot2::ggplot_build(ggcircumplex(ang))
  # Default degree labels (displacement-label layer 5) show the true angle
  expect_true("22.5\U00B0" %in% b$data[[5]]$label)
  expect_false("22\U00B0" %in% b$data[[5]]$label)
  # Spokes (segment layer 2) end at the exact angle, not the rounded one
  seg <- b$data[[2]]
  expect_equal(
    min(abs(seg$xend - 5 * cos(22.5 * pi / 180))), 0,
    tolerance = 1e-9
  )
})
