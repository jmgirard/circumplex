# The norms audit joins shipped values to source values WITHIN a sample (M74).
#
# The defect this fences, found at M72: shipped_values() keyed M and SD by
# scale alone, so a two-sample instrument emitted two rows both keyed "M PA"
# and match() resolved both to the first. Sample 2's shipped mean was compared
# against sample 1's source value, and sample 1's against itself -- so a wrong
# sample-2 value could never produce a ledger row, and every count in the run
# summary still read clean. The same collision hit Size, Population, Reference
# and URL, whose scale cell is "—" for every sample.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed and, unlike Rd or vignettes, has
# no installed counterpart to read instead, so a skip is the legitimate case
# the M70 lesson leaves open. Kept in its own file so no runtime pin skips with
# it.

audit_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

# A two-sample instrument whose two samples differ in EVERY per-sample field.
# Sample-distinguishing values are the whole point: a fixture repeating one
# sample's numbers cannot tell a sample-keyed join from an unkeyed one, which
# is the "both sides share an origin" trap of the M72 lesson.
fixture_object <- function(swap = FALSE) {
  m1 <- c(1.11, 1.22)
  m2 <- c(2.11, 2.22)
  if (swap) {
    tmp <- m1
    m1 <- m2
    m2 <- tmp
  }
  list(
    Norms = list(
      data.frame(
        Sample = c(1, 1, 2, 2),
        Scale = c("PA", "NO", "PA", "NO"),
        Angle = c(90, 45, 90, 45),
        M = c(m1, m2),
        SD = c(0.11, 0.22, 0.31, 0.42),
        stringsAsFactors = FALSE
      ),
      data.frame(
        Sample = c(1, 2),
        Size = c(100, 200),
        Population = c("first population", "second population"),
        Reference = c("Author (2001)", "Author (2002)"),
        URL = c("https://example.org/one", "https://example.org/two"),
        stringsAsFactors = FALSE
      )
    ),
    Scales = data.frame(
      Abbrev = c("PA", "NO"),
      Angle = c(90, 45),
      Items = c("1, 3", "2, 4"),
      Label = c("first", "second"),
      stringsAsFactors = FALSE
    )
  )
}

# The source note the fixture object should agree with, written in the
# five-column audit-values schema (field | sample | scale | value | anchor).
# withr is not in Suggests and a test file is not the place to acquire a
# dependency (the M73 precedent), so the directory is a plain tempfile() left
# to the session tempdir rather than a withr scope.
fixture_note_dir <- function() {
  dir <- tempfile("m74-notes-")
  dir.create(dir)
  rows <- c(
    "<!-- audit-values-begin -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "| M | 1 | NO | 1.22 | Table 1 |",
    "| M | 2 | PA | 2.11 | Table 2 |",
    "| M | 2 | NO | 2.22 | Table 2 |",
    "| SD | 1 | PA | 0.11 | Table 1 |",
    "| SD | 1 | NO | 0.22 | Table 1 |",
    "| SD | 2 | PA | 0.31 | Table 2 |",
    "| SD | 2 | NO | 0.42 | Table 2 |",
    "| Angle | — | PA | not-published-in-source | package convention |",
    "| Angle | — | NO | not-published-in-source | package convention |",
    "| Items | — | PA | 1, 3 | Appendix |",
    "| Items | — | NO | 2, 4 | Appendix |",
    "| Size | 1 | — | 100 | Table 1 |",
    "| Size | 2 | — | 200 | Table 2 |",
    "| Population | 1 | — | first population | Table 1 |",
    "| Population | 2 | — | second population | Table 2 |",
    "| Reference | 1 | — | Author (2001) | p. 1 |",
    "| Reference | 2 | — | Author (2002) | p. 2 |",
    "| URL | 1 | — | https://example.org/one | p. 1 |",
    "| URL | 2 | — | https://example.org/two | p. 2 |",
    "<!-- audit-values-end -->"
  )
  writeLines(rows, file.path(dir, "fixture.md"))
  dir
}

fixture_batch <- function() {
  data.frame(
    instrument = c("fx", "fx"),
    sample = c(1, 2),
    citekey = c("fixture", "fixture"),
    divisor = c(1, 1),
    scales = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("a two-sample instrument agreeing with its source audits clean (M74)", {
  env <- audit_defs()
  dir <- fixture_note_dir()
  res <- env$audit_norms(fixture_batch(), dir = dir,
                         objects = list(fx = fixture_object()),
                         roster = env$roster_from_objects(list(fx = fixture_object())))

  # No DISAGREEMENT, rather than no ledger rows: the two Angle rows are marked
  # not-published-in-source, as in every real note, and land in the ledger by
  # design. Asserting an empty ledger would have fenced the wrong thing.
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
  expect_setequal(res$ledger$field, "Angle")
  expect_identical(nrow(res$coverage[!res$coverage$exempt, , drop = FALSE]), 0L)

  # And that the pass actually compared something: a run that silently audited
  # zero values would also show no mismatch (the M72 "no count could show it"
  # shape). 2 scales x (M, SD) + 4 source-record fields + 2 scales x
  # (Angle, Items) = 12 for the scales-bearing pass, 8 without.
  expect_identical(nrow(env$shipped_values("fx", 1, TRUE, fixture_object())),
                   12L)
  expect_identical(nrow(env$shipped_values("fx", 2, FALSE, fixture_object())),
                   8L)
})

test_that("swapping the two samples' means reddens the audit (M74)", {
  env <- audit_defs()
  dir <- fixture_note_dir()
  res <- env$audit_norms(fixture_batch(), dir = dir,
                         objects = list(fx = fixture_object(swap = TRUE)),
                         roster = env$roster_from_objects(list(fx = fixture_object())))

  # Under the M72 key every one of these four rows compared against sample 1's
  # source value, so the swap produced ZERO ledger rows. Assert the count and
  # the identity of the rows -- "some mismatch occurred" would pass on a
  # fixture broken in any other way.
  mism <- res$ledger[res$ledger$kind == "mismatch", , drop = FALSE]
  expect_identical(nrow(mism), 4L)
  expect_setequal(mism$field, "M")
  expect_setequal(paste(mism$sample, mism$scale),
                  c("1 PA", "1 NO", "2 PA", "2 NO"))
  expect_setequal(mism$shipped, c("2.11", "2.22", "1.11", "1.22"))
})

test_that("shipped_values() refuses a sample the object does not carry (M74)", {
  env <- audit_defs()
  # A batch entry naming a missing sample must abort rather than audit nothing:
  # an empty comparison leaves every count clean, which is the failure mode the
  # whole sample rekey exists to remove.
  expect_error(
    env$shipped_values("fx", 3, FALSE, fixture_object()),
    "sample 3"
  )
})

test_that("a note sample no batch entry claims is reported as a gap (M74)", {
  env <- audit_defs()
  dir <- fixture_note_dir()
  # Audit only sample 1; the note's sample-2 rows are then unaudited, and that
  # must surface as a non-exempt coverage row rather than vanishing.
  batch <- fixture_batch()[1, , drop = FALSE]
  res <- env$audit_norms(batch, dir = dir,
                         objects = list(fx = fixture_object()),
                         roster = env$roster_from_objects(list(fx = fixture_object())))
  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  expect_true(any(gaps$side == "note-sample-not-audited"))
})
