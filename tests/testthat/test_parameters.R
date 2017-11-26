library(ssm)
context("SSM parameters")

test_that("ssm_parameters of wright2009 are correct", {
  expect_equal(ssm_parameters(colMeans(wright2009), octants, FALSE),
    c(0.4230000, 0.9445214, -0.2644691, 0.9808489, 344.3575801, 0.9540601))
})
