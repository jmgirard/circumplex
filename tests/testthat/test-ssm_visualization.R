context("test-ssm_visualization.R")

test_that("Single-group mean-based SSM plot is correct", {
  
  data("aw2009")
  res <- ssm_analyze(aw2009, PA:NO, octants())
  p <- ssm_plot(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  
  # TODO: Add tests of transformed data and legend
  
})

test_that("Single-group correlation-based SSM plot is correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = PARPD)
  p <- ssm_plot(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  
})

test_that("Measure-contrast SSM plot is correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = c(ASPD, NARPD),
    contrast = "test")
  p <- ssm_plot(res, xy = FALSE)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")

})

test_that("Group-contrast correlation-based SSM plot is correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = NARPD,
    grouping = Gender, contrast = "test")
  p <- ssm_plot(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  
})

test_that("Removing plots with low fit works as expected", {
  
  data("jz2017")
  
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = c(NARPD, OCPD))
  expect_message(ssm_plot(res), "One or more profiles were not plotted*")
  
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = OCPD)
  expect_error(ssm_plot(res), "After removing profiles, *")
  
})