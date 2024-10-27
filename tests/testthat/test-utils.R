test_that("Percentages are converted to strings properly", {
  expect_equal(str_percent(0.95, 2), "95%")
  expect_equal(str_percent(0.999, 2), "99.9%")
  expect_equal(str_percent(0.9999, 2), "99.99%")
  expect_equal(str_percent(0.99999, 2), "99.99%")
})

test_that("pretty max outputs the correct values", {
  expect_warning(pretty_max(NA))
  expect_equal(pretty_max(0), 0.05)
  expect_equal(pretty_max(1), 2)
  expect_equal(pretty_max(2.4), 4)
  expect_equal(pretty_max(5), 7.5)
})

test_that("angle convenience functions work", {
  expect_equal(octants(), as_degree(c(90, 135, 180, 225, 270, 315, 360, 45)))
  expect_equal(poles(), as_degree(c(90, 180, 270, 360)))
  expect_equal(quadrants(), as_degree(c(135, 225, 315, 45)))
})

test_that("scales shortcut functions work", {
  expect_equal(PANO(), c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"))
})

test_that("assertions work", {
  expect_true(is_num(1))
  expect_false(is_num("A"))
  expect_false(is_count(-1))
})

test_that("rescale works", {
  expect_equal(rescale(1:3), c(0.0, 0.5, 1.0))
})

test_that("prettifying works", {
  expect_equal(pretty_min(0.5), 0.2)
  expect_equal(pretty_min(-0.5), -1.0)
  expect_equal(pretty_min(-10), -15)
})
