# Reference-kind field (M78).
#
# Every shipped normative sample carries a machine-readable classification of
# what kind of reference distribution it is. The expected assignment below is
# transcribed from cairn/references/norms-audit.md's "Reference kind" table --
# the audit record -- and NOT recomputed from the shipped column, which would
# make the comparison two readings of one origin and unable to fail. That file
# is not installed with the package, so it cannot be read at test time; the
# transcription is the pin, and data-raw/derive-norms-kind.R is what re-derives
# the same partition from the record itself.

KIND_TOKENS <- c("standardization", "published", "unsourced")

# instrument, sample, expected kind.
expected_kinds <- data.frame(
  instrument = c(
    "cais", "cais", "csie", "csig", "csip", "csiv", "iei", "iei",
    "igicr", "igicr", "igicr", "iip32", "iip32", "iip32",
    "iip64", "iip64", "iip64", "iipsc", "iipsc", "iis32", "iis64",
    "ipipipc", "isc", "iitc"
  ),
  sample = c(
    1, 2, 1, 1, 1, 1, 1, 2,
    1, 2, 3, 1, 2, 3,
    1, 2, 3, 1, 2, 1, 1,
    1, 1, 1
  ),
  kind = c(
    "published", "published", "published", "published", "published",
    "published", "published", "published",
    "published", "published", "published",
    "standardization", "standardization", "standardization",
    "standardization", "standardization", "standardization",
    "published", "published", "unsourced", "published",
    "unsourced", "published", "published"
  ),
  stringsAsFactors = FALSE
)

pair_key <- function(instrument, sample) paste0(instrument, ":", sample)

# Every instrument-sample pair actually shipped, read from the data rather than
# hand-listed, so a new instrument or sample cannot slip past the map below.
shipped_pairs <- function() {
  do.call(rbind, lapply(shipped_instruments(), function(nm) {
    info <- shipped_instrument(nm)$Norms[[2]]
    data.frame(
      instrument = nm, sample = info$Sample, stringsAsFactors = FALSE
    )
  }))
}

test_that("every shipped sample carries a Kind from the controlled vocabulary", {
  for (nm in shipped_instruments()) {
    info <- shipped_instrument(nm)$Norms[[2]]
    expect_true("Kind" %in% names(info), info = nm)
    expect_true(all(info$Kind %in% KIND_TOKENS), info = nm)
    # A missing value would pass the %in% check above (NA %in% x is FALSE, but
    # all(logical(0)) is TRUE on an empty frame), so pin the arity too.
    expect_identical(nrow(info), length(info$Kind), info = nm)
  }
})

test_that("the shipped kinds are the ones the audit record assigns", {
  pairs <- shipped_pairs()
  # Exhaustiveness first: a loop over the expectation map alone would pass
  # while silently skipping a shipped pair the map forgot.
  expect_setequal(
    pair_key(pairs$instrument, pairs$sample),
    pair_key(expected_kinds$instrument, expected_kinds$sample)
  )

  for (i in seq_len(nrow(expected_kinds))) {
    inst <- expected_kinds$instrument[[i]]
    smp <- expected_kinds$sample[[i]]
    info <- shipped_instrument(inst)$Norms[[2]]
    got <- info$Kind[info$Sample == smp]
    expect_identical(
      got, expected_kinds$kind[[i]],
      info = paste0(inst, " sample ", smp)
    )
  }
})

test_that("the shipped roster partitions 6 / 16 / 2 across the three kinds", {
  # RR16 BC3's counts, asserted as literals rather than as a re-tally of the
  # expectation map -- the map and the shipped column already agree by the test
  # above, so tallying either one here would restate that agreement instead of
  # pinning the partition the review bound.
  kinds <- unlist(lapply(shipped_instruments(), function(nm) {
    shipped_instrument(nm)$Norms[[2]]$Kind
  }), use.names = FALSE)

  expect_identical(length(kinds), 24L)
  expect_identical(sum(kinds == "standardization"), 6L)
  expect_identical(sum(kinds == "published"), 16L)
  expect_identical(sum(kinds == "unsourced"), 2L)
})

test_that("the six standardization samples are the IIP forms", {
  # The partition counts above would survive six standardization labels landing
  # on the wrong instruments; this names which six.
  standardizing <- unlist(lapply(shipped_instruments(), function(nm) {
    info <- shipped_instrument(nm)$Norms[[2]]
    rep(nm, sum(info$Kind == "standardization"))
  }), use.names = FALSE)

  expect_setequal(standardizing, c(rep("iip32", 3), rep("iip64", 3)))
})
