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
