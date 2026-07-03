test_that("scale_x_circumplex() places breaks at the scale angles", {
  s <- scale_x_circumplex(octants())
  expect_true(inherits(s, "Scale"))
  expect_equal(s$breaks, octants())
})

test_that("default labels match the curve plot's degree formatting", {
  # ssm_plot_curve() labels its angle axis with sprintf("%.0f<degree>", x);
  # the scale helper's default must reproduce that exactly
  s <- scale_x_circumplex(octants())
  expect_equal(
    s$get_labels(octants()),
    sprintf("%.0f\U00B0", octants())
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
