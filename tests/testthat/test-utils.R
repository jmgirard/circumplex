context("test-utils.R")

test_that("Percentages are converted to strings properly", {
  expect_equal(str_percent(0.95, 2), "95%")
  expect_equal(str_percent(0.999, 2), "99.9%")
  expect_equal(str_percent(0.9999, 2), "99.99%")
  expect_equal(str_percent(0.99999, 2), "99.99%")
})

test_that("pretty max outputs the correct values", {
  expect_warning(pretty_max(NA))
  expect_equal(pretty_max(0), 0.05)
  expect_equal(pretty_max(1), 1.25)
  expect_equal(pretty_max(2.4), 2.5)
  expect_equal(pretty_max(5), 8)
})

test_that("angle convenience functions work", {
  expect_equal(octants(), as_degree(c(90, 135, 180, 225, 270, 315, 360, 45)))
  expect_equal(poles(), as_degree(c(90, 180, 270, 360)))
  expect_equal(quadrants(), as_degree(c(135, 225, 315, 45)))
})
