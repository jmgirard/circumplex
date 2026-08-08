# Anchor-range invariant for the shipped normative data.
#
# No shipped norm sample's octant mean may fall outside the response range its
# own instrument declares in $Anchors. A mean outside that range is proof the
# sample's moments are not on the metric the user's raw scores are on, so
# norm_standardize() against it returns z-scores in an undefined unit -- wrong
# numbers, with nothing on the surface to say so. The norms provenance audit
# (M72-M75) could not catch this: it compares shipped against source, and here
# the two agree.
#
# The one known violation is cais sample 2, whose source prints three CAIS
# octant means above the instrument's own 1-5 maximum. The evidence that
# sodano2006's Table 4 has its M and SD rows transposed with the IAS block,
# what was ruled out, and the author query outstanding on it are recorded in
# cairn/references/sodano2006.md ("On the adult sample's M and SD").
#
# It is pinned as an exact expected set rather than skipped, so the test fails
# in both directions that matter: a NEW violation appears, or this one
# disappears (which would mean the norms were corrected and this pin, and the
# refusal in norm_standardize(), should both be revisited).

# Every shipped sample of every shipped instrument, swept by the same
# enumeration the provenance pins use -- not a hand-list, which would only
# cover what its author remembered to name.
anchor_range_violations <- function() {
  out <- character(0)
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    anchors <- obj$Anchors
    values <- obj$Norms[[1]]
    if (is.null(anchors) || is.null(values) || nrow(values) == 0) next
    lo <- min(anchors$Value)
    hi <- max(anchors$Value)
    for (s in unique(values$Sample)) {
      m <- values$M[values$Sample == s]
      if (any(m < lo | m > hi, na.rm = TRUE)) {
        out <- c(out, paste0(nm, " sample ", s))
      }
    }
  }
  sort(out)
}

test_that("no shipped norm sample's mean falls outside its instrument's anchors", {
  expect_identical(anchor_range_violations(), "cais sample 2")
})

test_that("the cais anchor-range violation is the three octants on record", {
  values <- cais$Norms[[1]]
  s2 <- values[values$Sample == 2, ]
  hi <- max(cais$Anchors$Value)
  expect_identical(sort(s2$Scale[s2$M > hi]), c("LM", "NO", "PA"))
})

# norm_standardize() must refuse the sample rather than return z-scores whose
# unit is undefined. Refusal, not a warning: there is no metric under which the
# returned numbers are correct, so this blocks no defensible analysis (GP2,
# fail closed on the undecidable rather than guessing).

test_that("norm_standardize() refuses a norm sample outside the anchor range", {
  data("jz2017")
  expect_error(
    norm_standardize(jz2017, scales = 2:9, instrument = cais, sample = 2),
    "outside .* response range",
    fixed = FALSE
  )
})

test_that("norm_standardize()'s refusal names the sample and points somewhere useful", {
  data("jz2017")
  msg <- tryCatch(
    norm_standardize(jz2017, scales = 2:9, instrument = cais, sample = 2),
    error = conditionMessage
  )
  expect_match(msg, "sample 2")
  expect_match(msg, "CAIS")
  expect_match(msg, "norms\\(\\)")
})

test_that("norm_standardize() still standardizes an in-range sample of the same instrument", {
  data("jz2017")
  out <- norm_standardize(
    jz2017,
    scales = 2:9, instrument = cais, sample = 1, append = FALSE, quiet = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_identical(ncol(out), 8L)
  expect_true(all(vapply(out, is.numeric, logical(1))))
})

test_that("the refusal does not disturb instruments with no violation", {
  data("jz2017")
  # quiet = TRUE because every successful call now discloses its sample; what
  # this case asserts is that no *refusal* disturbs a non-violating
  # instrument, and the disclosure message would mask that with noise of its
  # own. The message itself is fenced in test-norms-disclosure.R.
  expect_silent(
    norm_standardize(
      jz2017, scales = 2:9, instrument = iipsc, sample = 2, quiet = TRUE
    )
  )
  out <- norm_standardize(
    jz2017,
    scales = 2:9, instrument = iipsc, sample = 2, append = FALSE, quiet = TRUE
  )
  expect_identical(ncol(out), 8L)
})
