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
# From M112 the shipped roster exhibits no violation. Its one violator, the
# cais adult sample, was withdrawn rather than corrected: its source prints
# three CAIS octant means above the instrument's own 1-5 maximum, and 21 days
# after the author query neither of the two dispositions D-040 named was
# available, so the sample shipped as data no call could use (D-052). The
# transcription, the evidence that sodano2006's Table 4 has its M and SD rows
# transposed with the IAS block, and what a reply would reopen are recorded in
# cairn/references/sodano2006.md.
#
# So the expected violation set is empty, and the two things that could make
# that emptiness meaningless are fenced separately: the sweep's domain is
# asserted non-empty here, and the refusal itself is exercised below on
# constructed off-metric objects rather than on a shipped sample.

# Every shipped sample of every shipped instrument, swept by the same
# enumeration the provenance pins use -- not a hand-list, which would only
# cover what its author remembered to name.
anchor_range_pairs <- function() {
  out <- character(0)
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    values <- obj$Norms[[1]]
    if (is.null(obj$Anchors) || is.null(values) || nrow(values) == 0) next
    out <- c(out, paste0(nm, " sample ", unique(values$Sample)))
  }
  sort(out)
}

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

# A shipped instrument with exactly one of its means pushed out of its own
# declared range -- the off-metric case the roster no longer carries. Built
# from a shipped object so the refusal meets a real Norms/Scales/Anchors
# shape rather than a hand-written stub that could diverge from one.
off_metric_instrument <- function(nm, sample = 1, which_row = 1) {
  obj <- shipped_instrument(nm)
  values <- obj$Norms[[1]]
  rows <- which(values$Sample == sample)
  values$M[[rows[[which_row]]]] <- max(obj$Anchors$Value) + 1
  obj$Norms[[1]] <- values
  obj
}

test_that("no shipped norm sample's mean falls outside its instrument's anchors", {
  # Domain first: an empty sweep would satisfy the emptiness below without
  # having looked at anything (M108).
  expect_gt(length(anchor_range_pairs()), 0L)
  expect_identical(anchor_range_violations(), character(0))
})

# The withdrawn sample is gone from the roster, not merely refused. Asking for
# it must report the argument at fault and what the instrument does carry --
# the unmatched-sample message, not the anchor-range one, which would mean the
# rows were still shipped.

test_that("the withdrawn cais adult sample is absent, not present-and-refused", {
  data("jz2017")
  msg <- tryCatch(
    norm_standardize(jz2017, scales = 2:9, instrument = cais, sample = 2),
    error = conditionMessage
  )
  expect_match(msg, "No normative data for sample 2", fixed = TRUE)
  expect_match(msg, "CAIS carries sample 1", fixed = TRUE)
  expect_false(grepl("response range", msg, fixed = TRUE))
})

# norm_standardize() must refuse an off-metric sample rather than return
# z-scores whose unit is undefined. Refusal, not a warning: there is no metric
# under which the returned numbers are correct, so this blocks no defensible
# analysis (GP2, fail closed on the undecidable rather than guessing). No
# shipped sample exercises this path any more, so the case is constructed.

test_that("norm_standardize() refuses a norm sample outside the anchor range", {
  data("jz2017")
  obj <- off_metric_instrument("cais")
  expect_error(
    norm_standardize(
      jz2017, scales = 2:9, angles = obj$Scales$Angle, instrument = obj,
      sample = 1
    ),
    "outside .* response range",
    fixed = FALSE
  )
})

test_that("norm_standardize()'s refusal names the sample and points somewhere useful", {
  data("jz2017")
  obj <- off_metric_instrument("cais")
  msg <- tryCatch(
    norm_standardize(
      jz2017, scales = 2:9, angles = obj$Scales$Angle, instrument = obj,
      sample = 1
    ),
    error = conditionMessage
  )
  expect_match(msg, "sample 1")
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

# The refusal names the offending scales by reading the second column of
# Norms[[1]], which is labelled `Scale` on 8 shipped instruments and `Abbrev`
# on 7. Both cases are constructed: no shipped sample is off-metric, so
# neither label can be exercised by the roster.

test_that("the refusal names the offending scales on an Abbrev-labelled instrument", {
  data("jz2017")
  obj <- shipped_instrument("iitc")
  expect_true("Abbrev" %in% names(obj$Norms[[1]]))
  values <- obj$Norms[[1]]
  hi <- max(obj$Anchors$Value)
  # Push exactly one octant out of range, and remember which.
  offender <- as.character(values$Abbrev[[3]])
  values$M[[3]] <- hi + 1
  obj$Norms[[1]] <- values
  msg <- tryCatch(
    norm_standardize(
      jz2017, scales = 2:9, angles = obj$Scales$Angle, instrument = obj,
      sample = 1
    ),
    error = conditionMessage
  )
  expect_match(msg, "response range")
  expect_match(msg, offender, fixed = TRUE)
  # Verified against the pre-fix expression: `key$Scale` is NULL on an
  # Abbrev-labelled instrument, so the offending-scale list came out empty --
  # the refusal named no scale at all rather than naming a wrong one. This
  # pins that shape, so the assertion above cannot pass on a message that
  # merely happens to contain the abbreviation somewhere else.
  expect_false(grepl("mean score for  falls", msg, fixed = TRUE))
})

test_that("the refusal names the offending scales on a Scale-labelled instrument", {
  data("jz2017")
  obj <- shipped_instrument("cais")
  expect_true("Scale" %in% names(obj$Norms[[1]]))
  offender <- as.character(obj$Norms[[1]]$Scale[[3]])
  obj <- off_metric_instrument("cais", which_row = 3)
  msg <- tryCatch(
    norm_standardize(
      jz2017, scales = 2:9, angles = obj$Scales$Angle, instrument = obj,
      sample = 1
    ),
    error = conditionMessage
  )
  expect_match(msg, "response range")
  expect_match(msg, offender, fixed = TRUE)
  expect_false(grepl("mean score for  falls", msg, fixed = TRUE))
})
