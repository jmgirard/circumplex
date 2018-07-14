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

test_that("Column means are correct", {
  mat <- matrix(runif(200), ncol = 2)
  idx <- sample(1:200, 50, replace = FALSE)
  mat[idx] <- NA
  rcm <- colMeans(mat, na.rm = TRUE)
  ccm <- col_means(mat)
  expect_equal(rcm[1], ccm[[1]])
  expect_equal(rcm[2], ccm[[2]])
})

test_that("Angular mean is correct", {
  am <- angle_mean(c(0, 0, pi / 2))
  expect_equal(round(am, 3), 0.464)
})

test_that("Angular median is correct", {
  # In the case of a single median candidate
  amdn <- angle_median(c(0, pi / 2, pi))
  expect_equal(amdn, pi / 2)
  # In the case of multiple median candidates
  amdn <- angle_median(c(0, pi / 2, pi, pi))
  expect_equal(round(amdn, 3), 2.678)
})

test_that("Angluar deviation from mean is correct", {
  angles <- c(0, pi / 2, pi, pi)
  ad <- angle_dev(angles, angle_mean(angles))
  expect_equal(round(ad, 3), 1.178)
})