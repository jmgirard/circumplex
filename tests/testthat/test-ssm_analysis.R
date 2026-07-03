test_that("Single-group mean-based SSM results are correct", {
  skip_on_cran()
  
  data("aw2009")
  set.seed(12345)
  res <- ssm_analyze(aw2009, scales = 1:8)

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.423)
  expect_equal(round(res$results$x_est, 3), 0.945)
  expect_equal(round(res$results$y_est, 3), -0.264)
  expect_equal(round(res$results$a_est, 3), 0.981)
  expect_equal(round(res$results$d_est, 1), as_degree(344.4))
  expect_equal(round(res$results$fit_est, 3), 0.954)
  expect_equal(res$results$Label, "All")
  expect_equal(round(res$results$e_lci, 3), 0.129)
  expect_equal(round(res$results$e_uci, 3), 0.708)
  expect_equal(round(res$results$x_lci, 3), 0.654)
  expect_equal(round(res$results$x_uci, 3), 1.251)
  expect_equal(round(res$results$y_lci, 3), -0.946)
  expect_equal(round(res$results$y_uci, 3), 0.300)
  expect_equal(round(res$results$a_lci, 3), 0.662)
  expect_equal(round(res$results$a_uci, 3), 1.403)
  expect_equal(round(res$results$d_lci, 3), as_degree(316.480))
  expect_equal(round(res$results$d_uci, 3), as_degree(17.191))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), 0.374)
  expect_equal(round(res$scores$BC, 3), -0.572)
  expect_equal(round(res$scores$DE, 3), -0.520)
  expect_equal(round(res$scores$FG, 3), 0.016)
  expect_equal(round(res$scores$HI, 3), 0.688)
  expect_equal(round(res$scores$JK, 3), 1.142)
  expect_equal(round(res$scores$LM, 3), 1.578)
  expect_equal(round(res$scores$NO, 3), 0.678)
  expect_equal(res$scores$Label, "All")

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_false(res$details$contrast)
  expect_equal(res$details$angles, octants(), ignore_attr = TRUE)
  expect_equal(res$details$score_type, "Mean")
})

test_that("Multiple-group mean-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, grouping = "Gender")

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), c(0.946, 0.884))
  expect_equal(round(res$results$x_est, 3), c(0.459, 0.227))
  expect_equal(round(res$results$y_est, 3), c(-0.310, -0.186))
  expect_equal(round(res$results$a_est, 3), c(0.554, 0.294))
  expect_equal(round(res$results$d_est, 3), as_degree(c(325.963, 320.685)))
  expect_equal(round(res$results$fit_est, 3), c(0.889, 0.824))
  expect_equal(res$results$Label, c("Female", "Male"))
  expect_equal(round(res$results$e_lci, 3), c(0.907, 0.839))
  expect_equal(round(res$results$e_uci, 3), c(0.984, 0.928))
  expect_equal(round(res$results$x_lci, 3), c(0.422, 0.191))
  expect_equal(round(res$results$x_uci, 3), c(0.498, 0.262))
  expect_equal(round(res$results$y_lci, 3), c(-0.357, -0.225))
  expect_equal(round(res$results$y_uci, 3), c(-0.266, -0.147))
  expect_equal(round(res$results$a_lci, 3), c(0.511, 0.256))
  expect_equal(round(res$results$a_uci, 3), c(0.600, 0.330))
  expect_equal(round(res$results$d_lci, 3), as_degree(c(321.834, 313.386)))
  expect_equal(round(res$results$d_uci, 3), as_degree(c(329.805, 327.985)))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.519, 0.585))
  expect_equal(round(res$scores$BC, 3), c(0.504, 0.674))
  expect_equal(round(res$scores$DE, 3), c(0.589, 0.664))
  expect_equal(round(res$scores$FG, 3), c(0.685, 0.856))
  expect_equal(round(res$scores$HI, 3), c(1.330, 1.075))
  expect_equal(round(res$scores$JK, 3), c(1.361, 1.047))
  expect_equal(round(res$scores$LM, 3), c(1.645, 1.300))
  expect_equal(round(res$scores$NO, 3), c(0.933, 0.868))
  expect_equal(res$scores$Label, c("Female", "Male"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants(), ignore_attr = TRUE)
  expect_false(res$details$contrast)
  expect_equal(res$details$score_type, "Mean")
})

test_that("Multiple-group mean-based SSM contrast is correct", {
  skip_on_cran()

  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    grouping = "Gender",
    contrast = TRUE
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), c(0.946, 0.884, -0.062))
  expect_equal(round(res$results$x_est, 3), c(0.459, 0.227, -0.232))
  expect_equal(round(res$results$y_est, 3), c(-0.310, -0.186, 0.124))
  expect_equal(round(res$results$a_est, 3), c(0.554, 0.294, -0.261))
  expect_equal(round(res$results$d_est, 3), as_degree(c(325.963, 320.685, -5.278)))
  expect_equal(round(res$results$fit_est, 3), c(0.889, 0.824, -0.066))
  expect_equal(res$results$Label, c("Female", "Male", "Male - Female"))
  expect_equal(round(res$results$e_lci, 3), c(0.907, 0.839, -0.122))
  expect_equal(round(res$results$e_uci, 3), c(0.984, 0.928, -0.002))
  expect_equal(round(res$results$x_lci, 3), c(0.422, 0.191, -0.285))
  expect_equal(round(res$results$x_uci, 3), c(0.498, 0.262, -0.180))
  expect_equal(round(res$results$y_lci, 3), c(-0.357, -0.225, 0.067))
  expect_equal(round(res$results$y_uci, 3), c(-0.266, -0.147, 0.183))
  expect_equal(round(res$results$a_lci, 3), c(0.511, 0.256, -0.318))
  expect_equal(round(res$results$a_uci, 3), c(0.600, 0.330, -0.205))
  expect_equal(round(res$results$d_lci, 3), as_degree(c(321.834, 313.386, -13.521)))
  expect_equal(round(res$results$d_uci, 3), as_degree(c(329.805, 327.985, 3.029)))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants(), ignore_attr = TRUE)
  expect_true(res$details$contrast)
  expect_equal(res$details$score_type, "Mean")
})

test_that("Requesting impossible contrasts throws error", {
  data("jz2017")
  jz2017$Three <- sample(c("a", "b", "c"), size = nrow(jz2017), replace = TRUE)
  # One group and no measures
  expect_error(ssm_analyze(jz2017, scales = 2:9, contrast = TRUE))
  # Three groups and no measures
  expect_error(ssm_analyze(
    jz2017, 
    scales = 2:9, 
    grouping = "Three",
    contrast = TRUE
  ))
  # Two groups but two measures
  expect_error(ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("PARPD", "NARPD"),
    grouping = "Gender",
    contrast = TRUE
  ))
  # One group and one measure
  expect_error(ssm_analyze(
    jz2017, 
    scales = 2:9, 
    measures = "PARPD", 
    contrast = TRUE
  ))
  
})

test_that("Single-group correlation-based SSM results are correct", {
  skip_on_cran()
  
  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(jz2017, scales = 2:9, measures = "PARPD")

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.250)
  expect_equal(round(res$results$x_est, 3), -0.094)
  expect_equal(round(res$results$y_est, 3), 0.117)
  expect_equal(round(res$results$a_est, 3), 0.150)
  expect_equal(round(res$results$d_est, 1), as_degree(128.9))
  expect_equal(round(res$results$fit_est, 3), 0.802)
  expect_equal(res$scores$Group, "All")
  expect_equal(res$scores$Measure, "PARPD")
  expect_equal(res$scores$Label, "PARPD")
  expect_equal(round(res$results$e_lci, 3), 0.218)
  expect_equal(round(res$results$e_uci, 3), 0.282)
  expect_equal(round(res$results$x_lci, 3), -0.128)
  expect_equal(round(res$results$x_uci, 3), -0.062)
  expect_equal(round(res$results$y_lci, 3), 0.081)
  expect_equal(round(res$results$y_uci, 3), 0.153)
  expect_equal(round(res$results$a_lci, 3), 0.113)
  expect_equal(round(res$results$a_uci, 3), 0.189)
  expect_equal(round(res$results$d_lci, 3), as_degree(117.261))
  expect_equal(round(res$results$d_uci, 3), as_degree(141.596))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), 0.329)
  expect_equal(round(res$scores$BC, 3), 0.494)
  expect_equal(round(res$scores$DE, 3), 0.329)
  expect_equal(round(res$scores$FG, 3), 0.203)
  expect_equal(round(res$scores$HI, 3), 0.102)
  expect_equal(round(res$scores$JK, 3), 0.143)
  expect_equal(round(res$scores$LM, 3), 0.207)
  expect_equal(round(res$scores$NO, 3), 0.193)
  expect_equal(res$scores$Group, "All")
  expect_equal(res$scores$Measure, "PARPD")
  expect_equal(res$scores$Label, "PARPD")

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_false(res$details$contrast)
  expect_match(res$details$score_type, "Correlation")
})

test_that("Pairwise and listwise scores are the same with no missingness", {
  skip_on_cran()

  # Single-group mean
  data("jz2017")
  res_lw <- ssm_analyze(jz2017, scales = 2:9, listwise = TRUE)
  res_pw <- ssm_analyze(jz2017, scales = 2:9, listwise = FALSE)
  expect_equal(res_lw$scores, res_pw$scores)

  # Single-group correlation
  res_lw <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "PARPD",
    listwise = TRUE
  )
  res_pw <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "PARPD",
    listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)

  # Multiple-group mean
  res_lw <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    grouping = "Gender",
    listwise = TRUE
  )
  res_pw <- ssm_analyze(
    jz2017, 
    scales = 2:9, 
    grouping = "Gender",
    listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)

  # Multiple-group correlation
  res_lw <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "PARPD",
    grouping = "Gender", 
    listwise = TRUE
  )
  res_pw <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = "PARPD",
    grouping = "Gender", 
    listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)
})

test_that("Measure-contrast correlation-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = c("ASPD", "NARPD"),
    contrast = TRUE
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), c(0.124, 0.202, 0.079))
  expect_equal(round(res$results$x_est, 3), c(-0.099, -0.062, 0.037))
  expect_equal(round(res$results$y_est, 3), c(0.203, 0.179, -0.024))
  expect_equal(round(res$results$a_est, 3), c(0.226, 0.189, -0.037))
  expect_equal(round(res$results$d_est, 1), as_degree(c(115.9, 109, -7.0)))
  expect_equal(round(res$results$fit_est, 3), c(0.964, 0.957, -0.007))
  expect_equal(res$results$Label, c("ASPD", "NARPD", "NARPD - ASPD"))
  expect_equal(round(res$results$e_lci, 3), c(0.087, 0.169, 0.042))
  expect_equal(round(res$results$e_uci, 3), c(0.158, 0.238, 0.117))
  expect_equal(round(res$results$x_lci, 3), c(-0.133, -0.094, -0.001))
  expect_equal(round(res$results$x_uci, 3), c(-0.064, -0.029, 0.075))
  expect_equal(round(res$results$y_lci, 3), c(0.170, 0.145, -0.063))
  expect_equal(round(res$results$y_uci, 3), c(0.239, 0.213, 0.014))
  expect_equal(round(res$results$a_lci, 3), c(0.191, 0.154, -0.077))
  expect_equal(round(res$results$a_uci, 3), c(0.264, 0.227, 0.003))
  expect_equal(round(res$results$d_lci, 3), as_degree(c(107.327, 98.633, -17.384)))
  expect_equal(round(res$results$d_uci, 3), as_degree(c(124.188, 118.537, 3.245)))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.368, 0.400, 0.031))
  expect_equal(round(res$scores$BC, 3), c(0.354, 0.385, 0.032))
  expect_equal(round(res$scores$DE, 3), c(0.187, 0.234, 0.047))
  expect_equal(round(res$scores$FG, 3), c(0.045, 0.108, 0.063))
  expect_equal(round(res$scores$HI, 3), c(-0.073, 0.051, 0.124))
  expect_equal(round(res$scores$JK, 3), c(-0.045, 0.058, 0.103))
  expect_equal(round(res$scores$LM, 3), c(-0.018, 0.084, 0.101))
  expect_equal(round(res$scores$NO, 3), c(0.173, 0.300, 0.127))
  expect_equal(res$scores$Group, c("All", "All", "All"))
  expect_equal(res$scores$Measure, c("ASPD", "NARPD", "NARPD - ASPD"))
  expect_equal(res$scores$Label, c("ASPD", "NARPD", "NARPD - ASPD"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_true(res$details$contrast)
  expect_equal(res$details$score_type, "Correlation")
})

test_that("Group-contrast correlation-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  set.seed(12345)
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9,
    measures = "NARPD",
    grouping = "Gender", 
    contrast = TRUE
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), c(0.172, 0.244, 0.072))
  expect_equal(round(res$results$x_est, 3), c(-0.080, -0.029, 0.051))
  expect_equal(round(res$results$y_est, 3), c(0.202, 0.146, -0.056))
  expect_equal(round(res$results$a_est, 3), c(0.217, 0.149, -0.068))
  expect_equal(round(res$results$d_est, 1), as_degree(c(111.7, 101.2, -10.4)))
  expect_equal(round(res$results$fit_est, 3), c(0.972, 0.902, -0.071))
  expect_equal(res$results$Label, c("NARPD: Female", "NARPD: Male", "NARPD: Male - Female"))
  expect_equal(round(res$results$e_lci, 3), c(0.126, 0.194, 0.005))
  expect_equal(round(res$results$e_uci, 3), c(0.217, 0.295, 0.142))
  expect_equal(round(res$results$x_lci, 3), c(-0.123, -0.076, -0.015))
  expect_equal(round(res$results$x_uci, 3), c(-0.035, 0.017, 0.111))
  expect_equal(round(res$results$y_lci, 3), c(0.157, 0.101, -0.120))
  expect_equal(round(res$results$y_uci, 3), c(0.247, 0.190, 0.006))
  expect_equal(round(res$results$a_lci, 3), c(0.170, 0.105, -0.133))
  expect_equal(round(res$results$a_uci, 3), c(0.265, 0.195, -0.003))
  expect_equal(round(res$results$d_lci, 3), as_degree(c(100.043, 83.117, -30.168)))
  expect_equal(round(res$results$d_uci, 3), as_degree(c(122.481, 119.726, 12.302)))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.385, 0.415, 0.029))
  expect_equal(round(res$scores$BC, 3), c(0.377, 0.397, 0.020))
  expect_equal(round(res$scores$DE, 3), c(0.227, 0.240, 0.013))
  expect_equal(round(res$scores$FG, 3), c(0.083, 0.129, 0.045))
  expect_equal(round(res$scores$HI, 3), c(-0.010, 0.138, 0.148))
  expect_equal(round(res$scores$JK, 3), c(-0.007, 0.155, 0.162))
  expect_equal(round(res$scores$LM, 3), c(0.036, 0.158, 0.122))
  expect_equal(round(res$scores$NO, 3), c(0.283, 0.322, 0.039))
  expect_equal(res$scores$Group, c("Female", "Male", "Male - Female"))
  expect_equal(res$scores$Measure, c("NARPD", "NARPD", "NARPD"))
  expect_equal(res$scores$Label, c("NARPD: Female", "NARPD: Male", "NARPD: Male - Female"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_true(res$details$contrast)
  expect_equal(res$details$score_type, "Correlation")
})


test_that("ssm_parameters works", {
  data("aw2009")
  scores <- unlist(aw2009[1, ])
  expect_error(ssm_parameters(scores = PANO()))
  expect_error(ssm_parameters(scores = scores, angles = PANO()))
  expect_error(ssm_parameters(scores = scores, angles = quadrants()))
  expect_equal(
    round(ssm_parameters(scores), 2), 
    data.frame(
      Elev = 0.43,
      Xval = 1.25, 
      Yval = -1.31, 
      Ampl = 1.81, 
      Disp = 313.71, 
      Fit = 0.97
    )
  )
})

test_that("ssm_score accepts matrix input and numeric scales", {
  data("aw2009")
  ref <- ssm_score(aw2009, scales = PANO(), append = FALSE)

  # Matrix input (advertised in the docs) must work and match the data frame
  m <- as.matrix(aw2009)
  out_mat <- ssm_score(m, scales = PANO(), append = FALSE)
  expect_equal(out_mat, ref)

  # Numeric column indexes must work (roxygen promises "column numbers")
  out_num <- ssm_score(aw2009, scales = 1:8, append = FALSE)
  expect_equal(out_num, ref)
})

test_that("ssm_analyze accepts matrix input", {
  skip_on_cran()
  data("aw2009")
  set.seed(12345)
  ref <- ssm_analyze(aw2009, scales = 1:8, boots = 50)
  set.seed(12345)
  out <- ssm_analyze(as.matrix(aw2009), scales = 1:8, boots = 50)
  expect_equal(out$results, ref$results)
})

test_that("degenerate profiles return NA with one warning", {
  # Flat profile with an exactly representable value
  expect_warning(out <- ssm_parameters(rep(1, 8)), "flat|amplitude|undefined")
  expect_equal(out$Elev, 1)
  expect_true(is.na(out$Disp))
  expect_true(is.na(out$Fit))
  expect_lt(abs(out$Ampl), 1e-12)

  # Flat profile with a non-representable value (var is ~2e-34, not exactly 0)
  expect_warning(out2 <- ssm_parameters(rep(0.1, 8)), "flat|amplitude|undefined")
  expect_true(is.na(out2$Disp))
  expect_true(is.na(out2$Fit))

  # Pure second harmonic: real variance but zero first-harmonic amplitude, so
  # displacement is undefined while fit is exactly 0 (model reduces to mean)
  rad <- as.numeric(as_radian(octants()))
  s2 <- cos(2 * rad)
  expect_warning(out3 <- ssm_parameters(s2), "flat|amplitude|undefined")
  expect_true(is.na(out3$Disp))
  expect_equal(out3$Fit, 0)

  # A small but real amplitude must NOT be treated as degenerate
  s_small <- 1 + 0.001 * cos(rad - pi / 4)
  expect_no_warning(out4 <- ssm_parameters(s_small))
  expect_equal(out4$Disp, 45, tolerance = 1e-6)
  expect_equal(out4$Fit, 1, tolerance = 1e-6)

  # Missing scores propagate as NA without noise angles
  expect_warning(out5 <- ssm_parameters(c(NA, rnorm(7))), "flat|amplitude|undefined|missing")
  expect_true(is.na(out5$Disp))
})

test_that("ssm_score works", {
  data("aw2009")
  out <- ssm_score(aw2009, scales = PANO(), append = TRUE)
  expect_equal(
    round(out[1:2, 9:14], 2),
    data.frame(
      Elev = c(0.43, 0.23),
      Xval = c(1.25, 1.42),
      Yval = c(-1.31, 0.51),
      Ampl = c(1.81, 1.51),
      Disp = c(313.71, 19.67),
      Fit = c(0.97, 0.92)
    )
  )
})

test_that("ssm_score forwards the angles argument", {
  data("aw2009")

  # Same-length custom angles must change the results (regression: these were
  # silently ignored and octants() used instead)
  rotated <- c(0, 45, 90, 135, 180, 225, 270, 315)
  out_rot <- ssm_score(aw2009, scales = PANO(), angles = rotated, append = FALSE)
  out_oct <- ssm_score(aw2009, scales = PANO(), append = FALSE)
  expect_false(isTRUE(all.equal(out_rot$Disp, out_oct$Disp)))

  # Row-wise results must match ssm_parameters() given the same angles
  expect_equal(
    unlist(out_rot[1, ]),
    unlist(ssm_parameters(unlist(aw2009[1, PANO()]), angles = rotated)),
    ignore_attr = TRUE
  )

  # Four scales with poles() must work (regression: errored on length mismatch)
  pano4 <- c("PA", "DE", "HI", "LM")
  out_poles <- ssm_score(aw2009, scales = pano4, angles = poles(), append = FALSE)
  expect_equal(
    unlist(out_poles[2, ]),
    unlist(ssm_parameters(unlist(aw2009[2, pano4]), angles = poles())),
    ignore_attr = TRUE
  )

  # Boundary: profile peaking exactly at the 0/360 degree crossover
  bdat <- as.data.frame(rbind(cos(rotated * pi / 180)))
  colnames(bdat) <- PANO()
  out_bound <- ssm_score(bdat, scales = PANO(), angles = rotated, append = FALSE)
  expect_equal(out_bound$Ampl, 1)
  expect_equal(out_bound$Fit, 1)
  expect_true(
    abs(out_bound$Disp - 360) < 1e-8 || abs(out_bound$Disp - 0) < 1e-8
  )
})

test_that("ssm_score forwards prefix, suffix, and label arguments", {
  data("aw2009")

  out <- ssm_score(aw2009, scales = PANO(), append = FALSE, prefix = "IIP_")
  expect_identical(
    colnames(out),
    c("IIP_Elev", "IIP_Xval", "IIP_Yval", "IIP_Ampl", "IIP_Disp", "IIP_Fit")
  )

  out2 <- ssm_score(
    aw2009, scales = PANO(), append = FALSE,
    x_label = "LOV", y_label = "DOM"
  )
  expect_identical(colnames(out2), c("Elev", "LOV", "DOM", "Ampl", "Disp", "Fit"))

  # Values must match ssm_parameters() with the same labels, row for row
  expect_equal(
    unlist(out2[1, ]),
    unlist(ssm_parameters(
      unlist(aw2009[1, PANO()]), x_label = "LOV", y_label = "DOM"
    )),
    ignore_attr = TRUE
  )
})

test_that("ssm_score warns once (with a count) for degenerate rows", {
  dat <- as.data.frame(matrix(rnorm(3 * 8), ncol = 8))
  colnames(dat) <- PANO()
  dat[2, ] <- 1 # flat row: undefined displacement and fit

  expect_warning(
    out <- ssm_score(dat, scales = PANO(), append = FALSE),
    "1 of 3"
  )
  expect_true(is.na(out$Disp[2]))
  expect_true(is.na(out$Fit[2]))
  expect_false(is.na(out$Disp[1]))
  expect_false(is.na(out$Disp[3]))
})

test_that("ssm_score errors on an unrecognized ... argument", {
  # Regression: forwarding ... via apply(FUN = ssm_parameters, ...) used to
  # raise "unused argument" for typos (ssm_parameters() has no ...); a typo
  # must still be caught, not silently ignored.
  data("aw2009")
  expect_error(
    ssm_score(aw2009, scales = PANO(), append = FALSE, bogus_arg = "x"),
    "unused argument"
  )
})

test_that("NA grouping values are dropped with a message in both modes", {
  data("jz2017")
  jz <- jz2017
  jz$Gender[c(1, 5, 10)] <- NA
  manual <- jz[!is.na(jz$Gender), ]

  # Pairwise deletion previously crashed (unique(): detected NaN)
  set.seed(1)
  expect_message(
    res_pw <- ssm_analyze(
      jz, scales = 2:9, grouping = "Gender", listwise = FALSE, boots = 20
    ),
    "3 observation"
  )
  set.seed(1)
  res_pw_manual <- ssm_analyze(
    manual, scales = 2:9, grouping = "Gender", listwise = FALSE, boots = 20
  )
  expect_equal(res_pw$results, res_pw_manual$results)
  expect_equal(res_pw$scores, res_pw_manual$scores)

  # Listwise deletion also reports the dropped NA-group count
  set.seed(1)
  expect_message(
    res_lw <- ssm_analyze(
      jz, scales = 2:9, grouping = "Gender", listwise = TRUE, boots = 20
    ),
    "3 observation"
  )
  set.seed(1)
  res_lw_manual <- ssm_analyze(
    manual, scales = 2:9, grouping = "Gender", listwise = TRUE, boots = 20
  )
  expect_equal(res_lw$results, res_lw_manual$results)

  # A contrast over two real levels plus NA still works and matches
  set.seed(1)
  res_c <- suppressMessages(ssm_analyze(
    jz, scales = 2:9, grouping = "Gender", contrast = TRUE,
    listwise = FALSE, boots = 20
  ))
  set.seed(1)
  res_c_manual <- ssm_analyze(
    manual, scales = 2:9, grouping = "Gender", contrast = TRUE,
    listwise = FALSE, boots = 20
  )
  expect_equal(res_c$results, res_c_manual$results)

  # A scale literally named "Group" must not be mistaken for the grouping
  # column. Here Gender has no NA, so no grouping rows should be dropped and
  # no message should fire, even though the "Group" scale has an NA.
  jz_collide <- jz2017
  jz_collide$Group <- jz_collide$PA
  jz_collide$Group[2] <- NA
  expect_no_message(
    ssm_analyze(
      jz_collide,
      scales = c("Group", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
      grouping = "Gender", listwise = FALSE, boots = 5
    )
  )

  # If every grouping value is missing, error clearly rather than crash
  jz_allna <- jz2017
  jz_allna$Gender <- NA
  expect_error(
    suppressMessages(
      ssm_analyze(jz_allna, scales = 2:9, grouping = "Gender", boots = 5)
    ),
    "No observations remain"
  )
})

test_that("measures_labels length is validated", {
  data("jz2017")

  # Wrong number of labels must error (regression: was silently accepted)
  expect_error(
    ssm_analyze(
      jz2017,
      scales = 2:9,
      measures = c("NARPD", "ASPD"),
      measures_labels = "Narcissistic",
      boots = 1
    ),
    "measures_labels"
  )

  # Labels without measures must error rather than be silently ignored
  expect_error(
    ssm_analyze(jz2017, scales = 2:9, measures_labels = "Mean", boots = 1),
    "measures_labels"
  )

  # Correct number of labels still works and is used in the output
  set.seed(12345)
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("NARPD", "ASPD"),
    measures_labels = c("Narcissistic", "Antisocial"),
    boots = 1
  )
  expect_equal(res$results$Label, c("Narcissistic", "Antisocial"))

  # NULL remains the default and works
  set.seed(12345)
  res_null <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = c("NARPD", "ASPD"),
    boots = 1
  )
  expect_equal(res_null$results$Label, c("NARPD", "ASPD"))
})

