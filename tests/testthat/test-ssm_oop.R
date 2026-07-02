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

test_that("print notes when a profile is not interpretable", {
  skip_on_cran()
  data("jz2017")

  # Low-fit profile: note advising to interpret only elevation
  set.seed(1)
  low <- suppressWarnings(
    ssm_analyze(jz2017, scales = 2:9, measures = "OCPD", boots = 200)
  )
  out_low <- capture.output(print(low))
  expect_true(any(grepl("only the elevation", out_low, ignore.case = TRUE)))

  # Healthy profile (good fit, amplitude well above zero): no note
  data("aw2009")
  set.seed(1)
  good <- ssm_analyze(aw2009, scales = 1:8, boots = 200)
  out_good <- capture.output(print(good))
  expect_false(any(grepl("not interpretable|only the elevation", out_good,
                         ignore.case = TRUE)))

  # Flat (degenerate) profile: amplitude CI includes zero -> displacement note
  flat <- as.data.frame(matrix(1, nrow = 20, ncol = 8))
  colnames(flat) <- PANO()
  set.seed(1)
  deg <- suppressWarnings(ssm_analyze(flat, scales = 1:8, boots = 50))
  out_deg <- capture.output(print(deg))
  expect_true(any(grepl("displacement is not interpretable", out_deg,
                        ignore.case = TRUE)))

  # summary() inherits the note (it delegates to print)
  out_sum <- capture.output(summary(low))
  expect_true(any(grepl("only the elevation", out_sum, ignore.case = TRUE)))
})

test_that("interpretation notes are not applied to the contrast row", {
  skip_on_cran()
  data("jz2017")
  set.seed(1)
  res <- suppressWarnings(ssm_analyze(
    jz2017, scales = 2:9, measures = c("OCPD", "NARPD"), contrast = TRUE,
    boots = 200
  ))
  out <- capture.output(print(res))
  # Find the contrast block and confirm no interpretation note appears in it
  contrast_start <- grep("# Contrast", out)
  contrast_block <- out[contrast_start:length(out)]
  expect_false(any(grepl("not interpretable|only the elevation", contrast_block,
                         ignore.case = TRUE)))
})

test_that("unit classes are working", {
  expect_snapshot(octants())
  expect_snapshot(as_radian(octants()))
})
