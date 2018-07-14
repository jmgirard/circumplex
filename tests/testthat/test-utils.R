context("test-utils.R")

test_that("Percentages are converted to strings properly", {
  expect_equal(str_percent(0.95, 2), "95%")
  expect_equal(str_percent(0.999, 2), "99.9%")
  expect_equal(str_percent(0.9999, 2), "99.99%")
  expect_equal(str_percent(0.99999, 2), "99.99%")
})
