# The norms audit's coverage report is keyed, not string-pasted (M80).
#
# The report is the audit's statement of what it did NOT compare, and it is
# read by machine -- the sibling test files assert over its columns and a
# reader relates it to the batch and to `data/` through them. Six emitters used
# to paste their payload into whichever column was free: `field` held
# "M (sample 1)" and `instrument` held "horowitz2003 (iip32)", so no row could
# be joined to anything and two columns did not hold what they are named for.
#
# These tests pin the schema, pin what each side fills, and pin the two shapes
# the schema alone cannot state: a note-only row belongs to a block rather than
# to each pass over it, and a block whose instrument-level rows no pass reads
# is reported rather than dropped.
#
# DEVELOPMENT-ONLY: data-raw/ and cairn/ are not installed, so these skip
# against the installed package, as the sibling audit test files do.

coverage_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

coverage_notes <- function() {
  dir <- testthat::test_path("..", "..", "cairn", "references")
  skip_if_not(dir.exists(dir), "cairn/ not present (installed package)")
  dir
}

# Two samples over two scales, so a fixture can drive a per-sample pass and an
# instrument-level pass separately.
cov_object <- function() {
  list(
    Norms = list(
      data.frame(Sample = c(1, 1, 2, 2), Scale = c("PA", "NO", "PA", "NO"),
                 Angle = c(90, 45, 90, 45), M = c(1.11, 1.22, 2.11, 2.22),
                 SD = c(0.11, 0.22, 0.31, 0.32), stringsAsFactors = FALSE),
      data.frame(Sample = c(1, 2), Size = c(100, 200),
                 Population = c("p1", "p2"), Reference = c("r1", "r2"),
                 URL = c("u1", "u2"), stringsAsFactors = FALSE)
    ),
    Scales = data.frame(Abbrev = c("PA", "NO"), Angle = c(90, 45),
                        Items = c("1, 3", "2, 4"), stringsAsFactors = FALSE)
  )
}

cov_head <- c("| field | sample | scale | value | anchor |",
              "|---|---|---|---|---|")

# One sample's rows, matching cov_object() so the ledger stays clean and the
# assertions below are about coverage rather than about a contrived mismatch.
cov_sample_rows <- function(s) {
  m <- if (s == 1) c("1.11", "1.22") else c("2.11", "2.22")
  sd <- if (s == 1) c("0.11", "0.22") else c("0.31", "0.32")
  rec <- if (s == 1) c("100", "p1", "r1", "u1") else c("200", "p2", "r2", "u2")
  c(paste0("| M | ", s, " | PA | ", m[[1]], " | T |"),
    paste0("| M | ", s, " | NO | ", m[[2]], " | T |"),
    paste0("| SD | ", s, " | PA | ", sd[[1]], " | T |"),
    paste0("| SD | ", s, " | NO | ", sd[[2]], " | T |"),
    paste0("| Size | ", s, " | — | ", rec[[1]], " | T |"),
    paste0("| Population | ", s, " | — | ", rec[[2]], " | T |"),
    paste0("| Reference | ", s, " | — | ", rec[[3]], " | T |"),
    paste0("| URL | ", s, " | — | ", rec[[4]], " | T |"))
}

# The instrument-level rows, read by the one pass carrying scales = TRUE.
cov_instrument_rows <- c(
  "| Angle | — | PA | 90 | convention |",
  "| Angle | — | NO | 45 | convention |",
  "| Items | — | PA | 1, 3 | Appendix |",
  "| Items | — | NO | 2, 4 | Appendix |"
)

cov_note_dir <- function(notes) {
  dir <- tempfile("m80-coverage-")
  dir.create(dir)
  for (nm in names(notes)) {
    writeLines(c("<!-- audit-values-begin -->", cov_head, notes[[nm]],
                 "<!-- audit-values-end -->"),
               file.path(dir, paste0(nm, ".md")))
  }
  dir
}

cov_batch <- function(citekeys, scales) {
  data.frame(instrument = "fx", sample = c(1, 2), citekey = citekeys,
             divisor = c(1, 1), scales = scales, stringsAsFactors = FALSE)
}

test_that("the coverage report's schema is the ten declared columns (M80)", {
  env <- coverage_defs()
  dir <- coverage_notes()
  res <- env$audit_norms(env$AUDIT_BATCH, dir)

  # The constant and the frame bind in both directions: a column added to one
  # and not the other fails here, which is what makes the header comment's
  # per-side table a description of the frame rather than a claim beside it.
  expect_identical(names(res$coverage), env$COVERAGE_COLUMNS)
  expect_identical(names(env$empty_coverage()), env$COVERAGE_COLUMNS)
  expect_type(res$coverage$exempt, "logical")
})

test_that("no coverage cell is a string-pasted payload (M80)", {
  env <- coverage_defs()
  dir <- coverage_notes()
  cov <- env$audit_norms(env$AUDIT_BATCH, dir)$coverage
  expect_gt(nrow(cov), 0L)

  # The three pastes this milestone removed, asserted as absences over the real
  # run. Absence assertions are weak on their own -- an empty report satisfies
  # every one -- so the row count above and the positive per-side assertions
  # below are what keep them from being vacuous.
  expect_false(any(grepl("(sample ", cov$field, fixed = TRUE)))
  expect_false(any(grepl("(", cov$instrument, fixed = TRUE)))

  # `field` holds a bare field name, from the fixed set the audit compares.
  fields <- unique(stats::na.omit(cov$field))
  expect_true(all(fields %in% c("M", "SD", "Size", "Population", "Reference",
                                "URL", "Angle", "Items")))

  # `instrument` holds a shipped instrument name, or NA where no batch row
  # names one (AC2). It never holds a citekey: every citekey in the batch would
  # fail this, which is what the pre-M80 `instrument` column contained.
  insts <- unique(stats::na.omit(cov$instrument))
  expect_true(all(insts %in% shipped_instruments()))
  expect_false(any(env$AUDIT_BATCH$citekey %in% insts))
})

test_that("a note-only row is reported once per block and payload (M80)", {
  env <- coverage_defs()
  dir <- coverage_notes()
  cov <- env$audit_norms(env$AUDIT_BATCH, dir)$coverage
  only <- cov[cov$side == "note-only-sample", , drop = FALSE]

  # 14, measured over the committed notes. The number is the point: every
  # note-only row in the repo carries the NO_SAMPLE token in its sample cell,
  # so a dedupe key of (citekey, block, sample) -- the obvious one -- collapses
  # these 14 to the 9 blocks that carry them, and the report would silently
  # lose 5 statements about what the sources publish. The payload is therefore
  # part of the key. Both figures measured 2026-08-13 over cairn/references/.
  expect_identical(nrow(only), 14L)
  expect_identical(length(unique(paste(only$citekey, only$tag))), 9L)
  # And each carries its payload in the free-text columns, not in a key column.
  # `!is.na()` before `nzchar()`, because nzchar(NA_character_) is TRUE and an
  # emitter that filled neither column would satisfy nzchar() alone -- which is
  # the regression this line exists to catch (M80 review, F10).
  expect_true(all(!is.na(only$label) & nzchar(only$label)))
  expect_true(all(!is.na(only$detail) & nzchar(only$detail)))
  expect_true(all(is.na(only$field)) && all(is.na(only$scale)))
})

test_that("two passes over one note do not duplicate its note-only row (M80)", {
  env <- coverage_defs()
  # The live shape the repo has no instance of: an instrument whose two samples
  # are audited against ONE note. Both passes parse the block and both used to
  # emit its note-only rows, so the report double-counted material the source
  # publishes -- once per pass rather than once per fact.
  objects <- list(fx = cov_object())
  dir <- cov_note_dir(list(one = c(
    cov_sample_rows(1), cov_sample_rows(2), cov_instrument_rows,
    "| note-only | — | a further sample | n = 42, not shipped | Table 9 |"
  )))
  res <- env$audit_norms(cov_batch(c("one", "one"), c(TRUE, FALSE)), dir = dir,
                         objects = objects,
                         roster = env$shipped_roster(objects))
  only <- res$coverage[res$coverage$side == "note-only-sample", , drop = FALSE]
  expect_identical(nrow(only), 1L)
  expect_identical(only$label, "a further sample")
  expect_identical(only$instrument, "fx")
  # Two passes really did run over that block, so the deduplication is what
  # produced the 1 above rather than the fixture only being read once.
  expect_identical(nrow(res$ledger), 0L)
  expect_identical(sum(!res$coverage$exempt), 0L)
})

test_that("two note-only rows differing only in anchor stay two (M80)", {
  env <- coverage_defs()
  # The dedupe key is the NOTE ROW -- scale, value and anchor -- not the two
  # cells the report carries. Keyed on the report's `label` and `detail` alone,
  # these two rows collapse to one and the second citation vanishes with no row
  # anywhere recording it (M80 review, F1): one source tabling one sample at two
  # places is an ordinary shape, and which table a value came from is the
  # provenance this audit exists to keep. No committed note has it, hence a
  # fixture.
  objects <- list(fx = cov_object())
  dir <- cov_note_dir(list(one = c(
    cov_sample_rows(1), cov_sample_rows(2), cov_instrument_rows,
    "| note-only | — | replication sample | n = 50 | Table 9 |",
    "| note-only | — | replication sample | n = 50 | Table 10 |"
  )))
  res <- env$audit_norms(cov_batch(c("one", "one"), c(TRUE, FALSE)), dir = dir,
                         objects = objects,
                         roster = env$shipped_roster(objects))
  only <- res$coverage[res$coverage$side == "note-only-sample", , drop = FALSE]
  # Two rows, and both from one pass over the block rather than one row per
  # pass: the count alone cannot tell those apart, so assert the payloads.
  expect_identical(nrow(only), 2L)
  expect_identical(only$label, rep("replication sample", 2L))
  expect_identical(only$detail, rep("n = 50", 2L))
  expect_identical(sum(!res$coverage$exempt), 0L)
})

test_that("a value missing from either side names its field and sample (M80)", {
  env <- coverage_defs()
  # AC1's central case, and the one the committed run cannot exercise: it has
  # no non-exempt row at all, so the two value-level emitters emit nothing and
  # an assertion over the real report says nothing about them (M80 review, F8).
  # The fixture's note omits the object's NO scale and tables a ZZ scale the
  # object does not ship, so one row of each side is produced.
  objects <- list(fx = cov_object())
  rows <- cov_sample_rows(1)
  rows <- rows[!grepl("^\\| M \\| 1 \\| NO ", rows)]
  dir <- cov_note_dir(list(k = c(
    rows, cov_instrument_rows, "| M | 1 | ZZ | 9.99 | T |"
  )))
  bat <- data.frame(instrument = "fx", sample = 1, citekey = "k", divisor = 1,
                    scales = TRUE, stringsAsFactors = FALSE)
  cov <- env$audit_norms(bat, dir = dir, objects = objects,
                         roster = env$shipped_roster(objects))$coverage

  miss_note <- cov[cov$side == "shipped-value-not-in-note", , drop = FALSE]
  miss_ship <- cov[cov$side == "note-value-not-shipped", , drop = FALSE]
  # `field` is the bare field name and the sample rides in `sample`. Before
  # M80 `field` held "M (sample 1)" and `sample` did not exist, so these four
  # assertions are what would redden if that paste came back.
  expect_identical(miss_note$field, "M")
  expect_identical(miss_note$sample, "1")
  expect_identical(miss_note$scale, "NO")
  expect_identical(miss_ship$field, "M")
  expect_identical(miss_ship$sample, "1")
  expect_identical(miss_ship$scale, "ZZ")
  # Both sides carry the block they came from, and neither is exempt.
  expect_true(all(c(miss_note$citekey, miss_ship$citekey) == "k"))
  expect_true(all(c(miss_note$instrument, miss_ship$instrument) == "fx"))
  expect_false(any(c(miss_note$exempt, miss_ship$exempt)))
})

test_that("an unaudited note sample names the claiming instrument (M80)", {
  env <- coverage_defs()
  dir <- coverage_notes()
  # AC2's second clause. trucco2013 tables igicr's three samples; dropping the
  # third from the batch leaves the block claimed by the surviving two, so the
  # sweep reports the unclaimed sample. The instrument comes from the block's
  # claiming batch row -- before M80 this column held "citekey (tag)" instead,
  # and the only assertion anywhere on this side checked the side NAME, which
  # passes either way (M80 review, F9).
  b <- env$AUDIT_BATCH
  res <- env$audit_norms(b[!(b$instrument == "igicr" & b$sample == 3), ], dir)
  hit <- res$coverage[res$coverage$side == "note-sample-not-audited", ,
                      drop = FALSE]
  expect_identical(hit$instrument, "igicr")
  expect_identical(hit$citekey, "trucco2013")
  expect_identical(hit$sample, "3")
  expect_true(is.na(hit$tag))
})

test_that("a block whose instrument-level rows no pass reads is reported (M80)", {
  env <- coverage_defs()
  # Exactly one pass per instrument carries scales = TRUE, and every other pass
  # DROPS the note's instrument-level rows. When that one pass reads a
  # different note -- iipsc's does today -- the dropped block's Angle and Items
  # rows are compared by nothing and reported by nothing.
  objects <- list(fx = cov_object())
  dir <- cov_note_dir(list(
    a = c(cov_sample_rows(1), cov_instrument_rows),
    b = c(cov_sample_rows(2), cov_instrument_rows)
  ))
  res <- env$audit_norms(cov_batch(c("a", "b"), c(FALSE, TRUE)), dir = dir,
                         objects = objects,
                         roster = env$shipped_roster(objects))
  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  hit <- gaps[gaps$side == "note-instrument-row-not-audited", , drop = FALSE]

  # Note `a`'s four instrument-level rows, named by field and scale rather than
  # counted: a bare count is satisfied by any four gaps of any kind.
  expect_identical(nrow(hit), 4L)
  expect_true(all(hit$citekey == "a"))
  expect_true(all(hit$instrument == "fx"))
  expect_setequal(paste(hit$field, hit$scale),
                  c("Angle PA", "Angle NO", "Items PA", "Items NO"))
  # Note `b`'s identical rows ARE read, by the scales = TRUE pass, so they
  # produce no row. Without this the criterion would be met by reporting every
  # block's instrument rows, covered and uncovered alike.
  expect_false(any(hit$citekey == "b"))

  # And the whole run is otherwise clean, so the four rows above are the only
  # thing the fixture is saying.
  expect_identical(nrow(gaps), 4L)
})

test_that("the committed coverage report is the one this code emits (M80)", {
  env <- coverage_defs()
  dir <- coverage_notes()
  path <- testthat::test_path("..", "..", "data-raw",
                              "norms-audit-coverage.csv")
  skip_if_not(file.exists(path), "coverage report not present")

  # The CSV is a committed artifact of a run, so it can drift from the code
  # that writes it -- a schema change with no regeneration leaves a file whose
  # columns no longer exist. Compared column by column rather than as text, so
  # that write.csv()'s quoting is not what is under test; row order IS pinned,
  # the comparison being element-wise.
  on_disk <- utils::read.csv(path, stringsAsFactors = FALSE,
                             colClasses = "character")
  fresh <- env$audit_norms(env$AUDIT_BATCH, dir)$coverage
  expect_identical(names(on_disk), env$COVERAGE_COLUMNS)
  expect_identical(nrow(on_disk), nrow(fresh))
  for (col in setdiff(env$COVERAGE_COLUMNS, "exempt")) {
    expect_identical(on_disk[[col]], as.character(fresh[[col]]), info = col)
  }
  expect_identical(on_disk$exempt, as.character(fresh$exempt))
})
