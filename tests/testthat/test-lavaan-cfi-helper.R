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
