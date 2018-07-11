context("RcppExport.R")

test_that("SSM parameters are correct", {
  data("aw2009")
  scores <- colMeans(aw2009)
  res <- ssm_parameters(scores, as_radian(octants()))
  
  expect_equal(round(res[[1]], 3), 0.423)
  expect_equal(round(res[[2]], 3), 0.945)
  expect_equal(round(res[[3]], 3), -0.264)
  expect_equal(round(res[[4]], 3), 0.981)
  expect_equal(round(res[[5]], 3), 6.010)
  expect_equal(round(res[[6]], 3), 0.954)
  
})

test_that("remainder is functioning properly", {
  ans <- remainder(-0.2730117, 2 * pi)
  expect_equal(round(ans, 3), 6.010)
})