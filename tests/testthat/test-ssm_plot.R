test_that("Single-group mean-based SSM plot is correct", {
  data("aw2009")
  set.seed(12345)
  res <- ssm_analyze(aw2009, scales = 1:8)
  p <- ssm_plot_circle(res)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("single group mean ssm", p)

  p2 <- ssm_plot_circle(res, palette = NULL)
  vdiffr::expect_doppelganger("single group mean ssm no palette", p2)
  
  p3 <- ssm_plot_curve(res, angle_labels = PANO())
  vdiffr::expect_doppelganger("single group mean ssm with labels", p3)
})

test_that("Single-group correlation-based SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  p <- ssm_plot_circle(res)
  
  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("single group correlation ssm", p)
})

test_that("Measure-contrast SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("ASPD", "NARPD"),
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res, drop_xy = TRUE)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("measure-contrast ssm", p)
  
  p2 <- ssm_plot_circle(res)
  
  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("measure-contrast-circle ssm", p2)
  
  p3 <- ssm_plot_curve(res, drop_lowfit = TRUE)
  vdiffr::expect_doppelganger("measure-contrast-curve ssm", p3)
})

test_that("Group-contrast correlation-based SSM plot is correct", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9, 
    measures = "NARPD",
    grouping = "Gender",
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res)

  # Test the output object
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("group-constrast correlation ssm", p)
})

test_that("Removing plots with low fit works as expected", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "OCPD")
  expect_error(ssm_plot_circle(res, drop_lowfit = TRUE))
})

test_that("many plots works as expected", {
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = 10:13)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("many_circle-plots", p)
  p2 <- ssm_plot_curve(res)
  vdiffr::expect_doppelganger("many_curve-plots", p2)
  p3 <- ssm_plot_circle(res, repel = TRUE)
  vdiffr::expect_doppelganger("many_circle repel", p3)
})

test_that("things are working at 0/360", {
  data("jz2017")
  set.seed(12345)
  dat <- jz2017[sample(1:nrow(jz2017), size = 100), ]
  res <- ssm_analyze(dat, 2:9, measures = 19)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("cross-zero circle", p)
})

test_that("ggcircumplex() builds a public circular canvas", {
  p <- ggcircumplex(octants())
  expect_true(ggplot2::is_ggplot(p))
  vdiffr::expect_doppelganger("ggcircumplex octant canvas", p)
})

test_that("ggcircumplex() derives angles and labels from an instrument", {
  data("csip")

  # Instrument input must resolve to the same canvas as passing that
  # instrument's angles and abbreviations explicitly (proves instrument-aware
  # labeling, including the LM = 360 scale). Compare the built plot data rather
  # than rendering twice, so the equivalence is exact and device-independent.
  p_inst <- ggcircumplex(instrument = csip)
  p_expl <- ggcircumplex(
    angles = csip$Scales$Angle,
    labels = csip$Scales$Abbrev
  )
  expect_true(ggplot2::is_ggplot(p_inst))
  expect_equal(
    ggplot2::ggplot_build(p_inst)$data,
    ggplot2::ggplot_build(p_expl)$data
  )
  vdiffr::expect_doppelganger("ggcircumplex instrument canvas", p_inst)

  # An explicit labels argument still overrides the instrument's abbreviations
  p_override <- ggcircumplex(instrument = csip, labels = LETTERS[1:8])
  expect_true(ggplot2::is_ggplot(p_override))
})

test_that("ggcircumplex() validates its arguments", {
  data("csip")
  # labels must match the number of angles
  expect_error(ggcircumplex(octants(), labels = c("A", "B")))
  # instrument must be an actual instrument object
  expect_error(ggcircumplex(instrument = mtcars))
  # scalar numeric requirements
  expect_error(ggcircumplex(octants(), amax = c(0.5, 1)))
  expect_error(ggcircumplex(octants(), font_size = "big"))
})

test_that("plot functions warn about unrecognized arguments", {
  data("aw2009")
  set.seed(1)
  res <- ssm_analyze(aw2009, scales = 1:8, boots = 50)

  # A typo'd argument lands in ... and is flagged rather than silently ignored
  expect_warning(ssm_plot_circle(res, angle_lables = PANO()), "disregarded")
  expect_warning(ssm_plot_curve(res, angle_lables = PANO()), "disregarded")

  data("jz2017")
  set.seed(1)
  cres <- ssm_analyze(
    jz2017, scales = 2:9, grouping = "Gender", contrast = TRUE, boots = 50
  )
  expect_warning(ssm_plot_contrast(cres, nonsense_arg = 1), "disregarded")

  # A clean call emits no "disregarded" warning (partial matches are fine)
  w <- capture_warnings(ssm_plot_circle(res, angle_labels = PANO()))
  expect_false(any(grepl("disregarded", w)))
})
