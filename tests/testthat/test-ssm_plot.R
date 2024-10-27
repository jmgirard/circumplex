test_that("Single-group mean-based SSM plot is correct", {
  data("aw2009")
  res <- ssm_analyze(aw2009, scales = 1:8)
  p <- ssm_plot_circle(res)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("single group mean ssm", p)

  p2 <- ssm_plot_circle(res, palette = NULL)
  vdiffr::expect_doppelganger("single group mean ssm no palette", p2)
  
  p3 <- ssm_plot_curve(res, angle_labels = PANO())
  vdiffr::expect_doppelganger("single group mean ssm with labels", p3)
})

test_that("Single-group correlation-based SSM plot is correct", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")
  p <- ssm_plot_circle(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("single group correlation ssm", p)
})

test_that("Measure-contrast SSM plot is correct", {
  data("jz2017")
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("ASPD", "NARPD"),
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res, drop_xy = TRUE)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("measure-contrast ssm", p)
  
  p2 <- ssm_plot_circle(res, drop_xy = TRUE)
  
  # Test the output object
  expect_type(p2, "list")
  expect_s3_class(p2, "ggplot")
  vdiffr::expect_doppelganger("measure-contrast-circle ssm", p2)
  
  p3 <- ssm_plot_curve(res, drop_lowfit = TRUE)
  vdiffr::expect_doppelganger("measure-contrast-curve ssm", p3)
})

test_that("Group-contrast correlation-based SSM plot is correct", {
  data("jz2017")
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9, 
    measures = "NARPD",
    grouping = "Gender",
    contrast = TRUE
  )
  p <- ssm_plot_contrast(res)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("group-constrast correlation ssm", p)
})

test_that("Removing plots with low fit works as expected", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "OCPD")
  expect_error(ssm_plot_circle(res, drop_lowfit = TRUE))
})

test_that("many plots works as expected", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, measures = 10:13)
  p <- ssm_plot_circle(res)
  vdiffr::expect_doppelganger("many_circle-plots", p)
  p2 <- ssm_plot_curve(res)
  vdiffr::expect_doppelganger("many_curve-plots", p2)
  p3 <- ssm_plot_circle(res, repel = TRUE)
  vdiffr::expect_doppelganger("many_circle repel", p3)
})
