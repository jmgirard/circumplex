test_that("Single-group mean-based SSM plot is correct", {
  #skip_if(getRversion() > "4.0.0")
  data("aw2009")
  res <- ssm_analyze(aw2009, PA:NO, octants())
  p <- ssm_plot(res)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("single group mean ssm", p)

  # TODO: Add tests of transformed data and legend
})

test_that("Single-group correlation-based SSM plot is correct", {
  #skip_if(getRversion() > "4.0.0")
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = PARPD)
  p <- ssm_plot(res)
  
  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("single group correlation ssm", p)
})

test_that("Measure-contrast SSM plot is correct", {
  #skip_if(getRversion() > "4.0.0")
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = c(ASPD, NARPD),
    contrast = "test"
  )
  p <- ssm_plot(res, xy = FALSE)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("measure-contrast ssm", p)
})

test_that("Group-contrast correlation-based SSM plot is correct", {
  #skip_if(getRversion() > "4.0.0")
  data("jz2017")
  res <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = NARPD,
    grouping = Gender, contrast = "test"
  )
  p <- ssm_plot(res)

  # Test the output object
  expect_type(p, "list")
  expect_s3_class(p, "ggplot")
  vdiffr::expect_doppelganger("group-constrast correlation ssm", p)
})

test_that("Removing plots with low fit works as expected", {
  data("jz2017")

  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = OCPD)
  expect_error(ssm_plot(res, lowfit = FALSE), "After removing profiles, *")
})

test_that("SSM Table captions are correct", {
  data("jz2017")

  res <- ssm_analyze(jz2017, PA:NO, octants())
  expect_equal(
    dcaption(res),
    "Mean-based Structural Summary Statistics with 95% CIs"
  )

  res <- ssm_analyze(jz2017, PA:NO, octants(),
    grouping = Gender,
    contrast = "model"
  )
  expect_equal(
    dcaption(res),
    "Mean-based Structural Summary Statistic Contrasts with 95% CIs"
  )

  res <- ssm_analyze(jz2017, PA:NO, octants(), measures = PARPD)
  expect_equal(
    dcaption(res),
    "Correlation-based Structural Summary Statistics with 95% CIs"
  )

  res <- ssm_analyze(jz2017, PA:NO, octants(),
    measures = PARPD,
    grouping = Gender, contrast = "test"
  )
  expect_equal(
    dcaption(res),
    "Correlation-based Structural Summary Statistic Contrasts with 95% CIs"
  )
})
