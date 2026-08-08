# What norm_standardize() tells the user about the sample it standardized
# against.
#
# The reference *choice* moves scores 0.44 SD on average and 0.78 at the
# extreme (M74's measurement), which is far larger than the sampling error of
# any one sample's moments. So the sample used is a result-determining input,
# and a call that names it only in an argument the caller may have defaulted
# leaves no record of which distribution the z-scores are relative to. These
# tests fence the disclosure: a message at the call site, and an attribute on
# the returned frame for scripts that never see the console.

# The probe is deliberately shaped from the instrument itself rather than
# hand-written, so the sweeps below cover every shipped instrument whatever its
# scale count or naming.
disclosure_probe <- function(obj) {
  probe <- as.data.frame(matrix(2, nrow = 2, ncol = nrow(obj$Scales)))
  names(probe) <- obj$Scales$Abbrev
  probe
}

# Samples whose means leave the instrument's response range are refused before
# any message is emitted (D-040), so the disclosure sweeps skip them rather
# than hand-listing the one shipped violation.
disclosure_usable <- function(obj, s) {
  key <- obj$Norms[[1]]
  m <- key$M[key$Sample == s]
  all(
    m >= min(obj$Anchors$Value) & m <= max(obj$Anchors$Value),
    na.rm = TRUE
  )
}

standardize_probe <- function(obj, s, ...) {
  probe <- disclosure_probe(obj)
  norm_standardize(
    probe,
    scales = names(probe), angles = obj$Scales$Angle,
    instrument = obj, sample = s, append = FALSE, ...
  )
}

# AC1 -------------------------------------------------------------------

test_that("the two message forms partition the shipped instruments", {
  # The forms are asserted below over their own memberships; if the two did not
  # exhaust the roster, an instrument could exhibit neither and go unswept.
  multi <- single <- character(0)
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    if (nrow(obj$Norms[[2]]) > 1) multi <- c(multi, nm) else single <- c(single, nm)
  }
  expect_setequal(c(multi, single), shipped_instruments())
  expect_identical(intersect(multi, single), character(0))
  expect_gt(length(multi), 0)
  expect_gt(length(single), 0)
})

test_that("a single-sample instrument's message names the sample, size and description", {
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    if (nrow(obj$Norms[[2]]) > 1) next
    row <- obj$Norms[[2]]
    msg <- capture_messages(standardize_probe(obj, row$Sample[[1]]))
    expect_length(msg, 1)
    expect_match(msg[[1]], paste0("sample ", row$Sample[[1]]), fixed = TRUE)
    expect_match(msg[[1]], paste0("N = ", row$Size[[1]]), fixed = TRUE)
    expect_match(msg[[1]], obj$Details$Abbrev, fixed = TRUE)
    # A one-sample instrument has no alternatives, so it must not offer any.
    expect_false(grepl("other sample", msg[[1]], fixed = TRUE))
  }
})

test_that("a multi-sample instrument's message says how many other samples exist", {
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    n <- nrow(obj$Norms[[2]])
    if (n < 2) next
    for (s in obj$Norms[[2]]$Sample) {
      if (!disclosure_usable(obj, s)) next
      msg <- capture_messages(standardize_probe(obj, s))
      expect_length(msg, 1)
      expect_match(msg[[1]], paste0("sample ", s), fixed = TRUE)
      expect_match(
        msg[[1]], paste0(n - 1, " other sample"),
        fixed = TRUE,
        info = paste(nm, "sample", s)
      )
      expect_match(msg[[1]], "norms()", fixed = TRUE)
    }
  }
})

test_that("quiet = TRUE emits nothing", {
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    for (s in obj$Norms[[2]]$Sample) {
      if (!disclosure_usable(obj, s)) next
      expect_silent(standardize_probe(obj, s, quiet = TRUE))
    }
  }
})

test_that("the message reads Size and description by Sample, not by row position", {
  # No shipped instrument stores its Norms[[2]] rows out of Sample order, so a
  # positional read passes the whole roster. This fixture is the only thing
  # that separates the two.
  obj <- shipped_instrument("iipsc")
  obj$Norms[[2]] <- obj$Norms[[2]][c(2, 1), ]
  msg <- capture_messages(standardize_probe(obj, 1))
  expect_match(msg[[1]], "N = 872", fixed = TRUE)
  expect_match(msg[[1]], "American college students", fixed = TRUE)
  expect_false(grepl("N = 106", msg[[1]], fixed = TRUE))
})

# AC8 (RR16 BC2) --------------------------------------------------------

test_that("every shipped sample's message carries its Population value verbatim", {
  # Quantified over all 24 (instrument, sample) pairs rather than over the 15
  # instruments: the value varies per sample, and `sample` defaults to 1, so an
  # instrument-level sweep would never emit a message for 9 of the 24.
  seen <- 0L
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    for (s in obj$Norms[[2]]$Sample) {
      if (!disclosure_usable(obj, s)) next
      pop <- obj$Norms[[2]]$Population[obj$Norms[[2]]$Sample == s]
      msg <- capture_messages(standardize_probe(obj, s))
      expect_match(msg[[1]], pop, fixed = TRUE, info = paste(nm, "sample", s))
      seen <- seen + 1L
    }
  }
  # 24 shipped samples less the one refused for leaving its anchor range.
  expect_identical(seen, 23L)
})

test_that("the message's fixed text frames the sample as a description, not a population", {
  # The stored value is data and may say anything; what this pins is the
  # package's own words around it. A sentinel Population carrying both banned
  # tokens is the only way to tell the fixed text from the data: strip the
  # value out of the emitted message, and what remains is the fixed text.
  sentinel <- "a representative population of nobody"
  obj <- shipped_instrument("iipsc")
  obj$Norms[[2]]$Population[obj$Norms[[2]]$Sample == 1] <- sentinel
  msg <- capture_messages(standardize_probe(obj, 1))
  expect_match(msg[[1]], sentinel, fixed = TRUE)
  fixed_text <- sub(sentinel, "", msg[[1]], fixed = TRUE)
  expect_false(grepl("population", fixed_text, ignore.case = TRUE))
  expect_false(grepl("representative", fixed_text, ignore.case = TRUE))
})

# AC2 -------------------------------------------------------------------

test_that("both return paths carry the norm_sample attribute", {
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    for (s in obj$Norms[[2]]$Sample) {
      if (!disclosure_usable(obj, s)) next
      row <- obj$Norms[[2]][obj$Norms[[2]]$Sample == s, ]
      for (app in c(TRUE, FALSE)) {
        probe <- disclosure_probe(obj)
        out <- norm_standardize(
          probe,
          scales = names(probe), angles = obj$Scales$Angle,
          instrument = obj, sample = s, append = app, quiet = TRUE
        )
        got <- attr(out, "norm_sample")
        expect_type(got, "list")
        expect_identical(got$Instrument, obj$Details$Abbrev,
                         info = paste(nm, "sample", s, "append", app))
        expect_identical(got$Sample, s)
        expect_identical(got$Size, row$Size[[1]])
        expect_identical(got$Population, row$Population[[1]])
      }
    }
  }
})

test_that("the attribute is present whether or not the message was emitted", {
  obj <- shipped_instrument("iipsc")
  loud <- suppressMessages(standardize_probe(obj, 1))
  hushed <- standardize_probe(obj, 1, quiet = TRUE)
  expect_identical(attr(loud, "norm_sample"), attr(hushed, "norm_sample"))
})

# AC9 (RR16 BC1) --------------------------------------------------------

test_that("the norm-standardizing surfaces keep the names the review settled on", {
  # RR16/D-041 decided against renaming these. The decision is only as durable
  # as something that fails when it is undone, so this pin is that something.
  exports <- getNamespaceExports("circumplex")
  expect_true("norm_standardize" %in% exports)
  expect_true("norms" %in% exports)
  for (nm in shipped_instruments()) {
    obj <- shipped_instrument(nm)
    expect_true("Norms" %in% names(obj), info = nm)
    expect_true("Population" %in% names(obj$Norms[[2]]), info = nm)
  }
})
