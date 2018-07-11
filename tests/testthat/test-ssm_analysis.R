context("test-ssm_analysis.R")

test_that("Single-group mean-based SSM results are correct", {
  
  data("aw2009")
  res1 <- ssm_analyze(aw2009, PA:NO, octants())
  
  # Test the output object
  expect_type(res1, "list")
  expect_s3_class(res1, "ssm")
  
  # Test the results subobject
  expect_equal(round(res1$results$e_est, 3), 0.423)
  expect_equal(round(res1$results$x_est, 3), 0.945)
  expect_equal(round(res1$results$y_est, 3), -0.264)
  expect_equal(round(res1$results$a_est, 3), 0.981)
  expect_equal(round(res1$results$d_est, 1), as_degree(344.4))
  expect_equal(round(res1$results$fit, 3), 0.954)
  expect_equal(res1$results$label, "All")
  # TODO: Figure out a way to test bootstrap interval estimates
  
  # Test the scores subobject
  expect_equal(round(res1$scores$PA, 3), 0.374)
  expect_equal(round(res1$scores$BC, 3), -0.572)
  expect_equal(round(res1$scores$DE, 3), -0.520)
  expect_equal(round(res1$scores$FG, 3), 0.016)
  expect_equal(round(res1$scores$HI, 3), 0.688)
  expect_equal(round(res1$scores$JK, 3), 1.142)
  expect_equal(round(res1$scores$LM, 3), 1.578)
  expect_equal(round(res1$scores$NO, 3), 0.678) 
  expect_equal(res1$scores$label, "All")
  
  # Test the details subobject
  expect_equal(res1$details$boots, 2000)
  expect_equal(res1$details$interval, 0.95)
  expect_true(res1$details$listwise)
  expect_equal(res1$details$angles, as_degree(octants()))
  expect_equal(res1$details$score_type, "Mean")
  expect_equal(res1$details$results_type, "Profile")
  
})

test_that("Single-group correlation-based SSM results are correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = PARPD)
  
  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "ssm")
  
  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.250)
  expect_equal(round(res$results$x_est, 3), -0.094)
  expect_equal(round(res$results$y_est, 3), 0.117)
  expect_equal(round(res$results$a_est, 3), 0.150)
  expect_equal(round(res$results$d_est, 1), as_degree(128.9))
  expect_equal(round(res$results$fit, 3), 0.802)
  expect_equal(res$scores$Group, factor("All"))
  expect_equal(res$scores$Measure, "PARPD")
  expect_equal(res$scores$label, "PARPD")
  # TODO: Figure out a way to test bootstrap interval estimates
  
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
  expect_equal(res$details$angles, as_degree(octants()))
  expect_match(res$details$score_type, "Correlation")
  expect_match(res$details$results_type, "Profile")

})

test_that("Measure-contrast correlation-based SSM results are correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = c(ASPD, NARPD),
    contrast = "test")
  
  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "ssm")
  
  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.079)
  expect_equal(round(res$results$x_est, 3), 0.037)
  expect_equal(round(res$results$y_est, 3), -0.024)
  expect_equal(round(res$results$a_est, 3), -0.037)
  expect_equal(round(res$results$d_est, 1), -7.0) # TODO: check sign
  expect_equal(round(res$results$fit, 3), -0.007)
  expect_equal(res$results$label, "NARPD - ASPD")
  # TODO: Figure out a way to test bootstrap interval estimates
  
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
  expect_equal(res$details$angles, as_degree(octants()))
  expect_equal(res$details$score_type, "Correlation")
  expect_equal(res$details$results_type, "Contrast")
})

test_that("Group-contrast correlation-based SSM results are correct", {
  
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = NARPD,
    grouping = Gender, contrast = "test")
  
  # Test the output object
  expect_type(res, "list")
  expect_s3_class(res, "ssm")
  
  # Test the results subobject
  expect_equal(round(res$results$e_est, 3), 0.072)
  expect_equal(round(res$results$x_est, 3), 0.051)
  expect_equal(round(res$results$y_est, 3), -0.056)
  expect_equal(round(res$results$a_est, 3), -0.068)
  expect_equal(round(res$results$d_est, 1), -10.4) # TODO: check sign
  expect_equal(round(res$results$fit, 3), -0.071)
  expect_equal(res$results$label, "NARPD: Male - Female")
  # TODO: Figure out a way to test bootstrap interval estimates
  
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
  expect_equal(res$details$angles, as_degree(octants()))
  expect_equal(res$details$score_type, "Correlation")
  expect_equal(res$details$results_type, "Contrast")
})