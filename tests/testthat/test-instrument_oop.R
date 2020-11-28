test_that("S3 class constructors for instrument class work as expected", {
  i <- new_instrument(list(), list(), list(), list(), list())
  expect_s3_class(i, "circumplex_instrument")
  expect_equal(is_instrument(i), TRUE)
})


test_that("The instrument function works with string and NSE input", {
  isc <- instrument(isc)
  isc2 <- instrument("isc")
  expect_equal(isc, isc2)
})


test_that("The instrument function produces the same output as the data function", {
  by_instrument <- instrument("isc")
  data("isc")
  expect_equal(by_instrument, isc)
})


test_that("The print method for the S3 instrument class produces the right output", {
  isc <- instrument("isc")
  expect_snapshot_output(print(isc))
})


test_that("The summary method for the S3 instrument class produces the right output", {
  isc <- instrument("isc")
  expect_snapshot_output(summary(isc, scales = FALSE, anchors = FALSE, items = FALSE, norms = FALSE))
  expect_snapshot_output(summary(isc, scales = TRUE, anchors = FALSE, items = FALSE, norms = FALSE))
  expect_snapshot_output(summary(isc, scales = FALSE, anchors = TRUE, items = FALSE, norms = FALSE))
  expect_snapshot_output(summary(isc, scales = FALSE, anchors = FALSE, items = TRUE, norms = FALSE))
  expect_snapshot_output(summary(isc, scales = FALSE, anchors = FALSE, items = FALSE, norms = TRUE))
})


test_that("The sub-summary functions produce the expected output", {
  isc <- instrument("isc")
  expect_snapshot_output(scales(isc))
  expect_snapshot_output(scales(isc, items = TRUE))
  expect_snapshot_output(items(isc))
  expect_snapshot_output(anchors(isc))
  expect_snapshot_output(norms(isc))
})


test_that("The norms function detects when no norms are available", {
  isc_drop <- instrument("isc")
  isc_drop$Norms[[2]] <- tibble::new_tibble(list(), nrow = 0)
  expect_snapshot_output(norms(isc_drop))
})


test_that("The instruments function produces the expected output", {
  expect_snapshot_output(instruments())
})
