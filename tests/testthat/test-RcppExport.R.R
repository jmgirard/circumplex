context("RcppExport.R")

test_that("SSM parameters are correct", {
  data("aw2009")
  scores <- colMeans(aw2009)
  res <- ssm_parameters(scores, as_radian(octants()))
  
  d_est_radian <- res[5, 1]
  expect_equal(round(d_est_radian, 3), 6.010)
  
  d_est_degree <- as_degree(as_radian(d_est_radian))
  expect_equal(as.vector(round(d_est_degree, 1)), 344.4)
})
