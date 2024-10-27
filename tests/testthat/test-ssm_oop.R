test_that("S3 degree functions work as expected", {
  x <- c(0, 90, 180, 360)
  y <- c(0, pi / 2, pi, pi * 2)

  x1 <- as_degree(x)
  expect_s3_class(x1, "circumplex_degree")
  expect_equal(as.numeric(x1), x)

  x2 <- as_degree(as_degree(x))
  expect_s3_class(x2, "circumplex_degree")
  expect_equal(as.numeric(x2), x)

  x3 <- as_radian(as_degree(x))
  expect_s3_class(x3, "circumplex_radian")
  expect_equal(as.numeric(x3), y)

  y1 <- as_radian(y)
  expect_s3_class(y1, "circumplex_radian")
  expect_equal(as.numeric(y1), y)

  y2 <- as_radian(as_radian(y))
  expect_s3_class(y2, "circumplex_radian")
  expect_equal(as.numeric(y2), y)

  y3 <- as_degree(as_radian(y))
  expect_s3_class(y3, "circumplex_degree")
  expect_equal(as.numeric(y3), x)
})

test_that("The ssm display methods is working", {
  skip_on_cran()

  data("aw2009")
  res <- ssm_analyze(aw2009, scales = 1:8)
  expect_output(print(res), "# Profile \\[All\\]:")
  expect_output(summary(res), "Statistical Basis:\\t Mean Scores")
  expect_output(summary(res), "Bootstrap Resamples:\\t 2000")
  expect_output(summary(res), "Confidence Level:\\t 0\\.95")
  expect_output(summary(res), "Listwise Deletion:\\t TRUE")
  expect_output(summary(res), "Scale Displacements:\\t 90 135 180 225 270 315 360 45")

  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9, grouping = "Gender")
  expect_output(print(res), "# Profile \\[Female\\]:")
  expect_output(print(res), "# Profile \\[Male\\]:")

  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    grouping = "Gender",
    contrast = TRUE
  )
  expect_output(print(res), "# Profile \\[Female\\]:")
  expect_output(print(res), "# Profile \\[Male\\]:")
  expect_output(print(res), "# Contrast \\[Male - Female\\]:")
  expect_output(print(res), "\u0394 Elevation")

  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "PARPD",
    grouping = "Gender", 
    contrast = TRUE
  )
  expect_output(print(res), "# Contrast \\[PARPD: Male - Female\\]:")
  expect_output(summary(res), "Statistical Basis:\\t Correlation Scores")
})

test_that("unit classes are working", {
  expect_snapshot(octants())
  expect_snapshot(as_radian(octants()))
})
