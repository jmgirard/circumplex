context("test-instrument_oop")

test_that("S3 class constructors for instrument class work as expected", {
  i <- new_instrument(list(), list(), list(), list(), list())
  expect_s3_class(i, "instrument")
  expect_equal(is_instrument(i), TRUE)
})

test_that("S3 methods for instrument class work as expected", {
  isc <- instrument(isc)
  isc2 <- instrument("isc")
  expect_equal(isc, isc2)

  expect_output(
    print(isc),
    "ISC: Interpersonal Sensitivities Circumplex\\n64 items, 8 scales, 1 normative data sets\\nHopwood et al\\. \\(2011\\)\\n<https://doi\\.org/10\\.1111/j\\.1467-6494\\.2011\\.00696\\.x>"
  )

  expect_output(
    scales(isc),
    "The ISC contains 8 circumplex scales\\.\\\nPA \\(090 deg\\): Sensitive to Control*"
  )

  expect_output(
    items(isc),
    "The ISC contains 64 items\\.\\\n1\\. Thinks they are my boss*"
  )

  expect_output(
    norms(isc),
    "The ISC currently has 1 normative data set\\(s\\): \\\n1\\. 649 American college students \\\n   Hopwood et al\\. \\(2011\\)*"
  )
})
