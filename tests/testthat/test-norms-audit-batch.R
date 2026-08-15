# The norms audit validates its batch's comparison parameters (M80).
#
# `divisor` is the documented unit deviation between a source's printed value
# and the shipped one: shipped = source / divisor. It is applied to every M and
# SD of the sample before the comparison, so an unusable divisor does not stop
# the audit -- it makes the audit compare wrong numbers while every count it
# prints stays clean, or reports a page of mismatches against a source that is
# in fact correct. `validate_batch()` refused a duplicated (instrument, sample)
# pair and a wrong `scales` count from M72 on, and left `divisor` unchecked.
#
# One test per refused shape, each asserting that shape's own message: a bare
# expect_error() is satisfied by any of the five, and by the batch being broken
# in some way none of them names.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed, so these skip against the
# installed package, as the sibling audit test files do.

batch_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

# A batch that passes every other check, so each case below differs from a
# valid batch in exactly the cell it is about.
ok_batch <- function() {
  data.frame(instrument = c("fx", "fy"), sample = c(1, 1),
             citekey = c("k1", "k2"), divisor = c(1, 8),
             scales = c(TRUE, TRUE), stringsAsFactors = FALSE)
}

with_divisor <- function(d) {
  b <- ok_batch()
  b$divisor <- d
  b
}

test_that("a valid batch's divisor column passes (M80)", {
  env <- batch_defs()
  # The control. Without it every assertion below is satisfied by a
  # validate_batch() that refuses everything, and by the real AUDIT_BATCH
  # already failing one of the new guards.
  expect_true(env$validate_batch(ok_batch()))
  expect_true(env$validate_batch(env$AUDIT_BATCH))
})

test_that("a missing divisor column is refused (M80)", {
  env <- batch_defs()
  b <- ok_batch()
  b$divisor <- NULL
  # The required-names condition, which has covered this shape since M72; the
  # message is R's deparse of the condition, matched as a stem.
  expect_audit_abort(env$validate_batch(b),
    paste0('all(c("instrument", "sample", "citekey", "divisor", ',
           '"scales") %in% names(batch))'))
})

test_that("a non-numeric divisor is refused (M80)", {
  env <- batch_defs()
  # Character divisors are what a hand-edited batch produces: one quoted cell
  # makes the whole column character, `as.numeric(source) / "1"` errors, and
  # before this guard the audit died with R's "non-numeric argument to binary
  # operator" from inside values_agree() -- a message that names neither the
  # batch nor the column.
  expect_audit_abort(env$validate_batch(with_divisor(c("1", "8"))),
    "AUDIT_BATCH$divisor must be numeric, not {}")
  expect_error(env$validate_batch(with_divisor(c("1", "8"))), "character")
})

test_that("an NA divisor is refused, and named (M80)", {
  env <- batch_defs()
  expect_audit_abort(env$validate_batch(with_divisor(c(1, NA_real_))),
    "AUDIT_BATCH$divisor is missing for: {}")
  # The offending row is named, not merely counted: source/NA is NA, so every
  # M and SD of that sample compares FALSE and the ledger fills with mismatches
  # that name the instrument's values rather than the batch cell behind them.
  expect_error(env$validate_batch(with_divisor(c(1, NA_real_))),
               "fy sample 1", fixed = TRUE)
  expect_error(env$validate_batch(with_divisor(c(NA_real_, 8))),
               "fx sample 1", fixed = TRUE)
})

test_that("a non-finite divisor is refused (M80)", {
  env <- batch_defs()
  # Distinct from the NA case, and deliberately so: source/Inf is 0, which
  # compares FALSE against every shipped value, while source/NA is NA. Both
  # arrive as mismatches; only the message says which cell caused them.
  expect_audit_abort(env$validate_batch(with_divisor(c(1, Inf))),
    "AUDIT_BATCH$divisor is not finite for: {}")
  expect_audit_abort(env$validate_batch(with_divisor(c(1, -Inf))),
    "AUDIT_BATCH$divisor is not finite for: {}")
  expect_error(env$validate_batch(with_divisor(c(1, Inf))), "fy sample 1",
               fixed = TRUE)
})

test_that("a zero or negative divisor is refused (M80)", {
  env <- batch_defs()
  # The one shape that produces no NA and no error at all: source/0 is Inf and
  # source/-8 is a finite negative, so the comparison runs to completion and
  # reports numbers. This is the case a "did it error?" check cannot see.
  expect_audit_abort(env$validate_batch(with_divisor(c(1, 0))),
    "AUDIT_BATCH$divisor must be strictly positive; wrong for: {}")
  expect_audit_abort(env$validate_batch(with_divisor(c(1, -8))),
    "AUDIT_BATCH$divisor must be strictly positive; wrong for: {}")
  expect_error(env$validate_batch(with_divisor(c(0, -8))),
               "fx sample 1, fy sample 1", fixed = TRUE)
})
