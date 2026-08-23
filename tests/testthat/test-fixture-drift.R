# Fixture drift fences.
#
# These tests fence a committed test fixture against the repo-side record it
# was taken from. They are DELIBERATELY source-tree-only: the thing on the
# other side of the comparison lives under the repo's cairn/ tracking
# directory, which .Rbuildignore keeps out of the built package, so under
# R CMD check there is nothing to compare against and each one skips. That is
# the correct behaviour here and not the false coverage the M7 lesson warns
# about -- what these fence is the tracking record, not shipped behaviour, and
# the shipped behaviour they used to be entangled with is now asserted against
# the packaged copy in test-axes-scaled-fit.R, which does run under check
# (M107).

test_that("M107 T2: the packaged exemplar B is byte-identical to the repo's record", {
  # rb18-counterexample-b.rds has no seed and no generator (provenance at its
  # first read site in test-axes-scaled-fit.R): the bytes ARE the artifact, so
  # a copy is the only way to ship it and byte-identity is the only check that
  # means anything. Compared as raw bytes rather than as deserialized objects,
  # because it is the bytes the assertions downstream depend on -- the case
  # flips to NULL under a value-preserving round trip that loses the last bits.
  packaged <- test_path("fixtures", "rb18-counterexample-b.rds")
  expect_true(file.exists(packaged))

  root <- test_path("..", "..")
  record <- file.path(root, "cairn", "reviews", "rb18-counterexample-b.rds")
  skip_if_not(file.exists(record),
              "repo tracking record absent (running against a built package)")

  read_bytes <- function(f) readBin(f, "raw", n = file.size(f))
  expect_identical(read_bytes(packaged), read_bytes(record))
})
