context("test-ssm_oop.R")

test_that("S3 degree functions work as expected", {
  x <- c(0, 90, 180, 360)
  y <- c(0, pi / 2, pi, pi * 2)
  
  x1 <- as_degree(x)
  expect_s3_class(x1, "degree")
  expect_equal(as.numeric(x1), x)
  
  x2 <- as_degree(as_degree(x))
  expect_s3_class(x2, "degree")
  expect_equal(as.numeric(x2), x)
  
  x3 <- as_radian(as_degree(x))
  expect_s3_class(x3, "radian")
  expect_equal(as.numeric(x3), y)
  
  y1 <- as_radian(y)
  expect_s3_class(y1, "radian")
  expect_equal(as.numeric(y1), y)
  
  y2 <- as_radian(as_radian(y))
  expect_s3_class(y2, "radian")
  expect_equal(as.numeric(y2), y)
  
  y3 <- as_degree(as_radian(y))
  expect_s3_class(y3, "degree")
  expect_equal(as.numeric(y3), x)
})
