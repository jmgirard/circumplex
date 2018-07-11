context("RcppExport.R")

test_that("SSM parameters are correct", {
  data("aw2009")
  scores <- colMeans(aw2009)
  res <- ssm_parameters(scores, octants())
  expect_equal(round(res[5, 1], 3), 1.038)
})
