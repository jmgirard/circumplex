context("test-ssm_bootstrap.R")

test_that("Quantile for circular radians works", {
  a <- as_degree(0:180)
  b <- as_radian(a)
  qb <- quantile(b)
  expect_s3_class(qb, "radian")
  expect_equivalent(qb, as_radian(as_degree(c(0, 45, 90, 135, 180))))

  a <- as_degree(180:360)
  b <- as_radian(a)
  qb <- quantile(b)
  expect_s3_class(qb, "radian")
  if (getRversion() >= "3.7.0") {
    expect_equivalent(qb, as_radian(as_degree(c(180, 225, 270, 315, 360))))
  } else {
    expect_equivalent(qb, as_radian(as_degree(c(180, 225, 270, 315, 0))))
  }

  a <- as_degree(c(270:360, 1:90))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_s3_class(qb, "radian")
  expect_equivalent(qb, as_radian(as_degree(c(270, 315, 0, 45, 90))))

  a <- as_degree(c(NA_real_, NA_real_, NA_real_))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_true(is.na(qb))

  a <- as_degree(c(0, 0, 30, 90, NA_real_))
  b <- as_radian(a)
  c <- as_degree(c(0, 0, 30, 90))
  d <- as_radian(c)
  expect_equal(quantile(b), quantile(d))
})
