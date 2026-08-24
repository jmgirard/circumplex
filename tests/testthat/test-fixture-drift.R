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
  # first read site in test-axes-scaled-fit.R), so a copy is how it ships and
  # byte-identity is what can be fenced. Compared as raw bytes rather than as
  # deserialized objects because byte-identity is the whole claim: a
  # value-preserving round trip that perturbs the last bits would pass an
  # all.equal() comparison while leaving the two copies different files.
  packaged <- test_path("fixtures", "rb18-counterexample-b.rds")
  expect_true(file.exists(packaged))

  root <- test_path("..", "..")
  record <- file.path(root, "cairn", "reviews", "rb18-counterexample-b.rds")
  # Gate on the tracking DIRECTORY, not on the record file. Skipping on the
  # file's absence cannot tell "there is no cairn/ here, we are in a tarball"
  # from "someone deleted the record" -- and the second is one of the drifts
  # this test exists to catch, so it must redden, not skip (M107 review).
  skip_if_not(dir.exists(file.path(root, "cairn")),
              "no cairn/ tracking dir (running against a built package)")
  if (!file.exists(record)) {
    fail(paste("the cairn/ tracking record is gone but cairn/ is present:",
               record))
    return(invisible(NULL))
  }

  read_bytes <- function(f) readBin(f, "raw", n = file.size(f))
  expect_identical(read_bytes(packaged), read_bytes(record))
})
