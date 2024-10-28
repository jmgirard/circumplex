test_that("Quantile for circular radians works", {
  a <- as_degree(0:180)
  b <- as_radian(a)
  qb <- stats::quantile(b)
  expect_s3_class(qb, "circumplex_radian")
  expect_equal(qb, as_radian(as_degree(c(0, 45, 90, 135, 180))), 
               ignore_attr = TRUE)

  a <- as_degree(180:360)
  b <- as_radian(a)
  qb <- quantile(b)
  expect_s3_class(qb, "circumplex_radian")
  expect_equal(qb, as_radian(as_degree(c(180, 225, 270, 315, 360))),
               ignore_attr = TRUE)
  
  a <- as_degree(c(270:360, 1:90))
  b <- as_radian(a)
  qb <- quantile(b)
  expect_equal(qb, as_radian(as_degree(c(270, 315, 360, 45, 90))),
               ignore_attr = TRUE)
  
  a <- as_degree(c(NA_real_, NA_real_, NA_real_))
  b <- as_radian(a)
  qb <- stats::quantile(b)
  expect_true(is.na(qb))

  a <- as_degree(c(0, 0, 30, 90, NA_real_))
  b <- as_radian(a)
  c <- as_degree(c(0, 0, 30, 90))
  d <- as_radian(c)
  expect_equal(stats::quantile(b), stats::quantile(d))
})
