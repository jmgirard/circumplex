context("test-ssm_visualization.R")

test_that("Single-group mean-based SSM plot is correct", {
  
  data("aw2009")
  res <- ssm_analyze(aw2009, PA:NO, octants())
  p <- ssm_plot(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  
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
  p <- ssm_plot(res)
  
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