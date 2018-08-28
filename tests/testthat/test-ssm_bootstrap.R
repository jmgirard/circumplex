context("test-ssm_bootstrap.R")

test_that("Quantile for circular radians works", {
  a <- as_degree(as.double(0:180))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_type(qb, "double")
  expect_s3_class(qb, "radian")
  expect_equivalent(qb, as_radian(as_degree(c(0, 45, 90, 135, 180))))

  a <- as_degree(as.double(180:360))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_type(qb, "double")
  expect_s3_class(qb, "radian")
  expect_equivalent(qb, as_radian(as_degree(c(180, 225, 270, 315, 0))))

  a <- as_degree(as.double(c(270:360, 1:90)))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_type(qb, "double")
  expect_s3_class(qb, "radian")
  expect_equivalent(qb, as_radian(as_degree(c(270, 315, 0, 45, 90))))
})
