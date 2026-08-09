# AUDIT_BATCH is bound to the shipped roster (M79).
#
# The batch is a hand-written table, and until M79 nothing tied it to `data/`.
# The audit walks the batch and the notes the batch names, so an instrument or
# sample the batch omits was never reached: measured 2026-08-08 at cef9d36f,
# dropping `isc` cost all 17 of its audited values while the ledger fell from
# 194 rows to 177, the coverage report from 15 to 13, and the gap count stayed
# at 0 with no row anywhere naming the instrument or its note.
#
# DEVELOPMENT-ONLY: data-raw/ and cairn/ are not installed, so these skip
# against the installed package, as the sibling audit test files do.

roster_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

roster_notes <- function() {
  dir <- testthat::test_path("..", "..", "cairn", "references")
  skip_if_not(dir.exists(dir), "cairn/ not present (installed package)")
  dir
}

test_that("dropping any batch row is visible in the run (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH

  # Abort-or-gap, not gap alone. Six of the 24 rows are the `scales = TRUE`
  # entry of a multi-sample instrument, and removing one leaves that
  # instrument with no `scales` entry, which validate_batch() refuses before
  # any coverage count exists -- so demanding a gap from all 24 would be
  # demanding one from a run that never returns. What must hold of every row
  # is that its removal is NOTICED, by either route.
  #
  # Measured 2026-08-08. Before the roster sweep: 6 abort, 8 report a gap, and
  # 10 -- the 9 single-sample rows and iipsc sample 1 -- are silent. After:
  # 6 abort, 18 report a gap, 0 silent. The silent 10 are what this fences.
  silent <- character(0)
  for (i in seq_len(nrow(batch))) {
    res <- tryCatch(env$audit_norms(batch[-i, , drop = FALSE], dir = dir),
                    error = function(e) NULL)
    if (is.null(res)) next
    if (sum(!res$coverage$exempt) == 0L) {
      silent <- c(silent, paste(batch$instrument[[i]], batch$sample[[i]]))
    }
  }
  expect_identical(silent, character(0))
})

test_that("a dropped instrument is named, not just counted (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  batch <- env$AUDIT_BATCH
  # isc is single-sample and its note is read by nothing else, so dropping it
  # removes the instrument from the run entirely -- the shape no note-side
  # sweep can see. Assert the row names the instrument and the sample, not
  # merely that the count moved: a gap count alone cannot say what is missing.
  res <- env$audit_norms(batch[batch$instrument != "isc", , drop = FALSE],
                         dir = dir)
  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  hit <- gaps[gaps$side == "shipped-sample-not-audited", , drop = FALSE]
  expect_identical(hit$instrument, "isc")
  expect_identical(hit$scale, "1")
})

test_that("the batch covers the shipped roster exactly (M79)", {
  env <- roster_defs()
  # The check a gap count cannot make. `sum(!exempt) == 0` in
  # test-norms-provenance.R says nothing is uncovered; it cannot say the batch
  # and the roster are the same set, and it names nothing when they are not.
  # Shipping a new instrument should fail HERE, by name.
  roster <- env$shipped_roster()
  batch <- env$AUDIT_BATCH
  expect_setequal(
    paste(roster$instrument, roster$sample),
    paste(batch$instrument, batch$sample)
  )
  # And that the comparison ranged over something: 15 shipped instruments,
  # 24 (instrument, sample) pairs, measured 2026-08-08 at cef9d36f. A roster
  # that came back empty would satisfy expect_setequal against an empty batch.
  expect_identical(nrow(roster), 24L)
  expect_identical(length(unique(roster$instrument)), 15L)
})

test_that("an instrument shipping no norms is not a roster gap (M79)", {
  env <- roster_defs()
  # Every shipped instrument carries norms today, so this is future-proofing:
  # an instrument with nothing to audit must not be reported as unaudited, or
  # the gap count stops meaning what the roster sweep exists to make it mean.
  # NULL[[1]] is NULL rather than an error in R, which is what makes the
  # one-line guard sufficient -- pinned here so a rewrite cannot quietly rely
  # on the other behaviour.
  none <- list(Norms = NULL, Scales = data.frame(Abbrev = "PA", Angle = 90))
  expect_identical(nrow(env$shipped_roster(list(fz = none))), 0L)

  empty <- list(Norms = list(
    data.frame(Sample = numeric(0), Scale = character(0), M = numeric(0))
  ))
  expect_identical(nrow(env$shipped_roster(list(fz = empty))), 0L)
})

test_that("the roster is the package's own enumeration, not a copy (M79)", {
  # Comparing shipped_roster()'s instruments against shipped_instruments() is
  # a tautology -- both bottom out in circumplex:::instrument_names(), so the
  # assertion holds even if that function returns nothing (M79 review, F11).
  # The claim is about the SOURCE, so the source is what is checked: the audit
  # script must reach the package's enumeration and must not carry one of its
  # own. A third copy written into data-raw/ reddens the second assertion.
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  src <- readLines(script, warn = FALSE)

  expect_true(any(grepl("instrument_names", src, fixed = TRUE)))
  # The sweep is `utils::data(package = "circumplex")` plus a class filter.
  # data-raw/ may not run one: that is the second copy this test exists to stop.
  expect_false(any(grepl("data(package", src, fixed = TRUE)))

  env <- roster_defs()
  # Non-vacuity, so an empty enumeration cannot satisfy the above by silence.
  roster <- env$shipped_roster()
  expect_gt(length(unique(roster$instrument)), 1L)
  expect_true(all(vapply(
    unique(roster$instrument),
    function(nm) inherits(shipped_instrument(nm), "circumplex_instrument"),
    logical(1)
  )))
})

test_that("injecting one object does not shrink the audited world (M79)", {
  env <- roster_defs()
  dir <- roster_notes()
  # The return-2 hole: `objects` overrides one instrument's VALUES, and the
  # roster used to be derived from it, so a value override silently narrowed
  # the world. Auditing a one-instrument slice of the real batch reported a
  # CLEAN run over every other shipped sample -- the exact "clean run over
  # data it never read" the Goal forbids. The two are now separate arguments.
  batch <- env$AUDIT_BATCH
  inst <- batch$instrument[[1L]]
  slice <- batch[batch$instrument == inst, , drop = FALSE]

  gaps <- function(res) {
    g <- res$coverage[!res$coverage$exempt, , drop = FALSE]
    g[g$side == "shipped-sample-not-audited", , drop = FALSE]
  }
  bare <- gaps(env$audit_norms(slice, dir = dir))
  # Injecting that instrument's own real object changes no value and so must
  # change no gap: same count, same pairs. Before the fix this was 0 rows.
  injected <- gaps(env$audit_norms(
    slice, dir = dir,
    objects = stats::setNames(list(shipped_instrument(inst)), inst)
  ))
  expect_gt(nrow(bare), 0L)
  expect_identical(nrow(injected), nrow(bare))
  expect_setequal(paste(injected$instrument, injected$scale),
                  paste(bare$instrument, bare$scale))
  # And the omitted instruments are named, not merely counted.
  expect_false(inst %in% injected$instrument)
  expect_gt(length(unique(injected$instrument)), 1L)
})
