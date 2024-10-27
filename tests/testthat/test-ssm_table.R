test_that("SSM Table captions are correct", {
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9)
  expect_equal(
    dcaption(res),
    "Mean-based Structural Summary Statistics with 95% CIs"
  )
  
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    grouping = "Gender",
    contrast = TRUE
  )
  expect_equal(
    dcaption(res),
    "Mean-based Structural Summary Statistic Contrasts with 95% CIs"
  )
  
  res <- ssm_analyze(
    jz2017, 
    scales = 2:9, 
    measures = "PARPD"
  )
  expect_equal(
    dcaption(res),
    "Correlation-based Structural Summary Statistics with 95% CIs"
  )
  
  res <- ssm_analyze(
    jz2017,
    scales = 2:9,
    measures = "PARPD",
    grouping = "Gender", 
    contrast = TRUE
  )
  expect_equal(
    dcaption(res),
    "Correlation-based Structural Summary Statistic Contrasts with 95% CIs"
  )
})


test_that("ssm_table is correct", {
  set.seed(12345)
  data("jz2017")
  res <- ssm_analyze(jz2017, scales = 2:9)
  expect_snapshot(ssm_table(res, render = FALSE))
  expect_snapshot(ssm_table(res, drop_xy = TRUE, render = FALSE))
  expect_snapshot(ssm_table(res, render = TRUE))
})
