# The corroboration helper's own probing behaviour (helper-lavaan-cfi.R).
# Tested directly rather than indirectly: what it does when a spelling is
# unavailable is independent logic, and it is exactly what stopped working
# silently the last time lavaan renamed the arguments (M107).

test_that("M107 AC5: the helper falls back to the older argument spelling", {
  # A stand-in accepting ONLY the older names, which is what an older lavaan
  # is from the helper's point of view. Delete the fallback arm and the first
  # probe's `unused arguments` error is the only outcome left, so the helper
  # returns NULL and this expectation reddens.
  old_only <- function(X2, df, X2.null, df.null) {
    1 - (X2 - df) / (X2.null - df.null)
  }
  expect_equal(lav_cfi_ref(200, 100, 400, 150, f = old_only),
               1 - (200 - 100) / (400 - 150))
})

test_that("M107 AC5: the current spelling is preferred over the older one", {
  # Both spellings accepted, returning distinguishable values, so which arm
  # ran is readable off the result. Without this the fallback test above would
  # also pass a helper that tried the old spelling first.
  both <- function(x2 = NULL, df = NULL, x2_null = NULL, df_null = NULL,
                   X2 = NULL, X2.null = NULL, df.null = NULL) {
    if (!is.null(x2)) 0.5 else 0.25
  }
  expect_equal(lav_cfi_ref(200, 100, 400, 150, f = both), 0.5)
})

test_that("M107 AC5: an unusable function yields NULL, so the caller skips", {
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) stop("nope")))
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) NA_real_))
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) c(1, 2)))
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = NULL))
})

test_that("M107 review: an unusable RETURN TYPE yields NULL rather than erroring", {
  # is.finite() has no method for a list, so with the shape checks outside the
  # tryCatch these returned an error to the caller instead of NULL -- turning
  # "lavaan is unavailable, skip" into "the suite is red". Asserted as NULL,
  # not merely as not-erroring, so the skip path is what is pinned.
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) list(0.9)))
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) "0.9"))
  expect_null(lav_cfi_ref(200, 100, 400, 150, f = function(...) NULL))
})

test_that("M107 review: a warning does not discard an otherwise usable value", {
  # A deprecation warning on a function still returning the right number must
  # not read as unavailability -- treating it that way would skip both
  # comparisons forever with the numbers available the whole time.
  warns_but_works <- function(x2 = NULL, df = NULL, x2_null = NULL,
                              df_null = NULL) {
    warning("lav_fit_cfi() is deprecated")
    1 - (x2 - df) / (x2_null - df_null)
  }
  expect_equal(lav_cfi_ref(200, 100, 400, 150, f = warns_but_works),
               1 - (200 - 100) / (400 - 150))
  # ...and the warning is muffled rather than propagated to the caller.
  expect_silent(lav_cfi_ref(200, 100, 400, 150, f = warns_but_works))
})

test_that("M107 review: a warning still does not rescue an unusable value", {
  # The control for the test above: demoting warnings must not weaken the
  # shape check, or "warns and returns nonsense" would start being trusted.
  expect_null(lav_cfi_ref(200, 100, 400, 150,
                          f = function(...) { warning("w"); NA_real_ }))
})

test_that("M168: the corroboration reaches the installed lavaan", {
  # The CFI comparisons skip when lav_cfi_ref() returns NULL, so a renamed or
  # removed lav_fit_cfi() leaves them skipping with the suite green; this test
  # fails instead. It skips on CRAN, so a lavaan release cannot fail the
  # package's check there. Local devtools::test() and devtools::check() set
  # NOT_CRAN and run it, and so does every CI job, because
  # r-lib/actions/setup-r exports NOT_CRAN=true.
  skip_on_cran()
  skip_if_not_installed("lavaan")
  expect_equal(lav_cfi_ref(200, 100, 400, 150),
               1 - (200 - 100) / (400 - 150))
})
