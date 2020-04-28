context("test-ssm_analysis.R")

test_that("Single-group mean-based SSM results are correct", {
  data("aw2009")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(aw2009, PA:NO, octants())

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
  expect_equal(res$results$label, "All")
  expect_equal(round(res$results$e_lci, 3), 0.131)
  expect_equal(round(res$results$e_uci, 3), 0.702)
  expect_equal(round(res$results$x_lci, 3), 0.654)
  expect_equal(round(res$results$x_uci, 3), 1.235)
  expect_equal(round(res$results$y_lci, 3), -0.829)
  expect_equal(round(res$results$y_uci, 3), 0.300)
  expect_equal(round(res$results$a_lci, 3), 0.662)
  expect_equal(round(res$results$a_uci, 3), 1.370)
  expect_equal(round(res$results$d_lci, 3), as_degree(316.480))
  expect_equal(round(res$results$d_uci, 3), as_degree(17.194))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), 0.374)
  expect_equal(round(res$scores$BC, 3), -0.572)
  expect_equal(round(res$scores$DE, 3), -0.520)
  expect_equal(round(res$scores$FG, 3), 0.016)
  expect_equal(round(res$scores$HI, 3), 0.688)
  expect_equal(round(res$scores$JK, 3), 1.142)
  expect_equal(round(res$scores$LM, 3), 1.578)
  expect_equal(round(res$scores$NO, 3), 0.678)
  expect_equal(res$scores$label, "All")

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equivalent(res$details$angles, octants())
  expect_equal(res$details$score_type, "Mean")
  expect_equal(res$details$results_type, "Profile")
})

test_that("Multiple-group mean-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(jz2017, PA:NO, octants(), grouping = Gender)

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
  expect_equal(res$results$label, c("Female", "Male"))
  expect_equal(round(res$results$e_lci, 3), c(0.907, 0.843))
  expect_equal(round(res$results$e_uci, 3), c(0.982, 0.925))
  expect_equal(round(res$results$x_lci, 3), c(0.421, 0.192))
  expect_equal(round(res$results$x_uci, 3), c(0.498, 0.261))
  expect_equal(round(res$results$y_lci, 3), c(-0.350, -0.224))
  expect_equal(round(res$results$y_uci, 3), c(-0.268, -0.149))
  expect_equal(round(res$results$a_lci, 3), c(0.511, 0.258))
  expect_equal(round(res$results$a_uci, 3), c(0.596, 0.331))
  expect_equal(round(res$results$d_lci, 3), as_degree(c(322.305, 313.424)))
  expect_equal(round(res$results$d_uci, 3), as_degree(c(330.027, 327.915)))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.519, 0.585))
  expect_equal(round(res$scores$BC, 3), c(0.504, 0.674))
  expect_equal(round(res$scores$DE, 3), c(0.589, 0.664))
  expect_equal(round(res$scores$FG, 3), c(0.685, 0.856))
  expect_equal(round(res$scores$HI, 3), c(1.330, 1.075))
  expect_equal(round(res$scores$JK, 3), c(1.361, 1.047))
  expect_equal(round(res$scores$LM, 3), c(1.645, 1.300))
  expect_equal(round(res$scores$NO, 3), c(0.933, 0.868))
  expect_equal(res$scores$label, c("Female", "Male"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equivalent(res$details$angles, octants())
  expect_equal(res$details$contrast, "none")
  expect_equal(res$details$score_type, "Mean")
  expect_equal(res$details$results_type, "Profile")
})

test_that("Multiple-group mean-based SSM contrast is correct", {
  skip_on_cran()

  data("jz2017")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(jz2017, PA:NO, octants(),
    grouping = Gender,
    contrast = "model"
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), -0.062)
  expect_equal(round(res$results$x_est, 3), -0.232)
  expect_equal(round(res$results$y_est, 3), 0.124)
  expect_equal(round(res$results$a_est, 3), 0.263)
  expect_equal(round(res$results$d_est, 3), as_degree(151.858))
  expect_equal(round(res$results$fit_est, 3), 0.855)
  expect_equal(res$results$label, "Male - Female")
  expect_equal(round(res$results$e_lci, 3), -0.116)
  expect_equal(round(res$results$e_uci, 3), -0.004)
  expect_equal(round(res$results$x_lci, 3), -0.286)
  expect_equal(round(res$results$x_uci, 3), -0.183)
  expect_equal(round(res$results$y_lci, 3), 0.065)
  expect_equal(round(res$results$y_uci, 3), 0.181)
  expect_equal(round(res$results$a_lci, 3), 0.211)
  expect_equal(round(res$results$a_uci, 3), 0.320)
  expect_equal(round(res$results$d_lci, 3), as_degree(141.379))
  expect_equal(round(res$results$d_uci, 3), as_degree(164.198))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equivalent(res$details$angles, octants())
  expect_equal(res$details$contrast, "model")
  expect_equal(res$details$score_type, "Mean")
  expect_equal(res$details$results_type, "Contrast")
})

test_that("Providing one group throws error", {
  data("aw2009")
  expect_error(
    ssm_analyze(aw2009, PA:NO, octants(), contrast = "model"),
    "Without specifying measures or grouping, *"
  )

  data("jz2017")
  expect_error(ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    contrast = "test"
  ), "No valid contrasts were possible*")
})

test_that("Providing more than two groups throws error", {
  data("jz2017")
  expect_error(ssm_analyze(jz2017, PA:NO, octants(),
    grouping = PARPD,
    contrast = "test"
  ), "Only two groups can be contrasted at a time.*")
})

test_that("Single-group correlation-based SSM results are correct", {
  data("jz2017")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = PARPD)

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
  expect_equal(res$scores$Group, factor("All"))
  expect_equal(res$scores$Measure, "PARPD")
  expect_equal(res$scores$label, "PARPD")
  expect_equal(round(res$results$e_lci, 3), 0.218)
  expect_equal(round(res$results$e_uci, 3), 0.280)
  expect_equal(round(res$results$x_lci, 3), -0.129)
  expect_equal(round(res$results$x_uci, 3), -0.060)
  expect_equal(round(res$results$y_lci, 3), 0.081)
  expect_equal(round(res$results$y_uci, 3), 0.153)
  expect_equal(round(res$results$a_lci, 3), 0.115)
  expect_equal(round(res$results$a_uci, 3), 0.189)
  expect_equal(round(res$results$d_lci, 3), as_degree(115.894))
  expect_equal(round(res$results$d_uci, 3), as_degree(142.094))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), 0.329)
  expect_equal(round(res$scores$BC, 3), 0.494)
  expect_equal(round(res$scores$DE, 3), 0.329)
  expect_equal(round(res$scores$FG, 3), 0.203)
  expect_equal(round(res$scores$HI, 3), 0.102)
  expect_equal(round(res$scores$JK, 3), 0.143)
  expect_equal(round(res$scores$LM, 3), 0.207)
  expect_equal(round(res$scores$NO, 3), 0.193)
  expect_equal(res$scores$Group, factor("All"))
  expect_equal(res$scores$Measure, "PARPD")
  expect_equal(res$scores$label, "PARPD")

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_match(res$details$score_type, "Correlation")
  expect_match(res$details$results_type, "Profile")
})

test_that("Pairwise and listwise scores are the same with no missingness", {
  skip_on_cran()

  # Single-group mean
  data("jz2017")
  res_lw <- ssm_analyze(jz2017, PA:NO, octants(), listwise = TRUE)
  res_pw <- ssm_analyze(jz2017, PA:NO, octants(), listwise = FALSE)
  expect_equal(res_lw$scores, res_pw$scores)

  # Single-group correlation
  res_lw <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    listwise = TRUE
  )
  res_pw <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)

  # Multiple-group mean
  res_lw <- ssm_analyze(jz2017, PA:NO, octants(),
    grouping = Gender,
    listwise = TRUE
  )
  res_pw <- ssm_analyze(jz2017, PA:NO, octants(),
    grouping = Gender,
    listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)

  # Multiple-group correlation
  res_lw <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    grouping = Gender, listwise = TRUE
  )
  res_pw <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    grouping = Gender, listwise = FALSE
  )
  expect_equal(res_lw$scores, res_pw$scores)
})

test_that("Measure-contrast correlation-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = c(ASPD, NARPD),
    contrast = "test"
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.079)
  expect_equal(round(res$results$x_est, 3), 0.037)
  expect_equal(round(res$results$y_est, 3), -0.024)
  expect_equal(round(res$results$a_est, 3), -0.037)
  expect_equal(round(res$results$d_est, 1), as_degree(-7.0))
  expect_equal(round(res$results$fit_est, 3), -0.007)
  expect_equal(res$results$label, "NARPD - ASPD")
  expect_equal(round(res$results$e_lci, 3), 0.039)
  expect_equal(round(res$results$e_uci, 3), 0.116)
  expect_equal(round(res$results$x_lci, 3), -0.003)
  expect_equal(round(res$results$x_uci, 3), 0.075)
  expect_equal(round(res$results$y_lci, 3), -0.060)
  expect_equal(round(res$results$y_uci, 3), 0.012)
  expect_equal(round(res$results$a_lci, 3), -0.075)
  expect_equal(round(res$results$a_uci, 3), 0.001)
  expect_equal(round(res$results$d_lci, 3), as_degree(-17.322))
  expect_equal(round(res$results$d_uci, 3), as_degree(3.662))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.368, 0.400))
  expect_equal(round(res$scores$BC, 3), c(0.354, 0.385))
  expect_equal(round(res$scores$DE, 3), c(0.187, 0.234))
  expect_equal(round(res$scores$FG, 3), c(0.045, 0.108))
  expect_equal(round(res$scores$HI, 3), c(-0.073, 0.051))
  expect_equal(round(res$scores$JK, 3), c(-0.045, 0.058))
  expect_equal(round(res$scores$LM, 3), c(-0.018, 0.084))
  expect_equal(round(res$scores$NO, 3), c(0.173, 0.300))
  expect_equal(res$scores$Group, factor(c("All", "All")))
  expect_equal(res$scores$Measure, c("ASPD", "NARPD"))
  expect_equal(res$scores$label, c("ASPD", "NARPD"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_equal(res$details$contrast, "test")
  expect_equal(res$details$score_type, "Correlation")
  expect_equal(res$details$results_type, "Contrast")
})

test_that("Group-contrast correlation-based SSM results are correct", {
  skip_on_cran()

  data("jz2017")
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(12345)
  res <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = NARPD,
    grouping = Gender, contrast = "test"
  )

  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "circumplex_ssm")

  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.072)
  expect_equal(round(res$results$x_est, 3), 0.051)
  expect_equal(round(res$results$y_est, 3), -0.056)
  expect_equal(round(res$results$a_est, 3), -0.068)
  expect_equal(round(res$results$d_est, 1), as_degree(-10.4))
  expect_equal(round(res$results$fit_est, 3), -0.071)
  expect_equal(res$results$label, "NARPD: Male - Female")
  expect_equal(round(res$results$e_lci, 3), 0.004)
  expect_equal(round(res$results$e_uci, 3), 0.140)
  expect_equal(round(res$results$x_lci, 3), -0.009)
  expect_equal(round(res$results$x_uci, 3), 0.113)
  expect_equal(round(res$results$y_lci, 3), -0.122)
  expect_equal(round(res$results$y_uci, 3), 0.011)
  expect_equal(round(res$results$a_lci, 3), -0.134)
  expect_equal(round(res$results$a_uci, 3), 0.003)
  expect_equal(round(res$results$d_lci, 3), as_degree(-31.700))
  expect_equal(round(res$results$d_uci, 3), as_degree(9.985))

  # Test the scores subobject
  expect_equal(round(res$scores$PA, 3), c(0.385, 0.415))
  expect_equal(round(res$scores$BC, 3), c(0.377, 0.397))
  expect_equal(round(res$scores$DE, 3), c(0.227, 0.240))
  expect_equal(round(res$scores$FG, 3), c(0.083, 0.129))
  expect_equal(round(res$scores$HI, 3), c(-0.010, 0.138))
  expect_equal(round(res$scores$JK, 3), c(-0.007, 0.155))
  expect_equal(round(res$scores$LM, 3), c(0.036, 0.158))
  expect_equal(round(res$scores$NO, 3), c(0.283, 0.322))
  expect_equal(res$scores$Group, factor(c("Female", "Male")))
  expect_equal(res$scores$Measure, c("NARPD", "NARPD"))
  expect_equal(res$scores$label, c("Female_NARPD", "Male_NARPD"))

  # Test the details subobject
  expect_equal(res$details$boots, 2000)
  expect_equal(res$details$interval, 0.95)
  expect_true(res$details$listwise)
  expect_equal(res$details$angles, octants())
  expect_equal(res$details$contrast, "test")
  expect_equal(res$details$score_type, "Correlation")
  expect_equal(res$details$results_type, "Contrast")
})
