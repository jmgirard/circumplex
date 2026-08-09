# Block-marker recognition in the norms audit's source-note parser (M79).
#
# The parser locates a note's machine-readable block with two independent
# greps -- one in parse_source_note(), one in source_note_block_tags() --
# neither anchored at the end of the line nor aware of fenced code blocks, and
# both handing whatever follows the prefix to the tag extractor. Three shapes
# get through:
#
#   (a) A note that SHOWS the audit-values format inside a fence has its
#       example parsed as a real block. With a matching fenced end marker the
#       begin/end counts balance, so the well-formedness test passes and the
#       example's values become a block of the note -- and the instrument whose
#       real block is untagged can then no longer be found at all, because a
#       two-block note is selected by tag.
#   (b) The same fenced example reaches the unclaimed-block sweep through
#       source_note_block_tags(), which emits a phantom `note-block-not-audited`
#       coverage gap for a block that does not exist.
#   (c) `<!-- audit-values-beginning -->` is accepted as a marker carrying the
#       tag "ning", because tag extraction strips the prefix and the trailing
#       `-->` and returns whatever is left.
#
# No note in cairn/references/ carries a fence today (only browne1982.md does,
# and it has no markers), so all three are latent rather than live.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed, so these skip against the
# installed package, as the sibling audit test files do.

marker_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

# One real untagged block, followed by a fenced example of the same format.
# The example carries BOTH markers so the begin/end counts balance -- an
# unbalanced fence would merely abort, which is a false alarm rather than the
# silent mis-parse this fences.
fenced_note_dir <- function() {
  dir <- tempfile("m79-fenced-")
  dir.create(dir)
  writeLines(c(
    "# A source note that documents its own format",
    "",
    "<!-- audit-values-begin -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "| M | 1 | NO | 1.22 | Table 1 |",
    "<!-- audit-values-end -->",
    "",
    "The block above is written in this format:",
    "",
    "```",
    "<!-- audit-values-begin: example -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 9.99 | not a real anchor |",
    "<!-- audit-values-end -->",
    "```"
  ), file.path(dir, "fenced.md"))
  dir
}

test_that("a fenced example is not parsed as a block (M79)", {
  env <- marker_defs()
  dir <- fenced_note_dir()

  # Two real rows, and the 9.99 from the fenced example is not among them.
  got <- env$parse_source_note("fenced", dir, instrument = "fx")
  expect_identical(nrow(got), 2L)
  expect_false("9.99" %in% got$value)
  expect_identical(attr(got, "tag"), "")
})

test_that("a fenced example contributes no block tag (M79)", {
  env <- marker_defs()
  dir <- fenced_note_dir()

  # source_note_block_tags() feeds the unclaimed-block sweep, so a phantom tag
  # here becomes a phantom `note-block-not-audited` coverage gap.
  expect_identical(env$source_note_block_tags("fenced", dir), "")
})

test_that("a fenced example does not raise a phantom block gap (M79)", {
  env <- marker_defs()
  dir <- fenced_note_dir()
  batch <- data.frame(
    instrument = "fx", sample = 1, citekey = "fenced",
    divisor = 1, scales = TRUE, stringsAsFactors = FALSE
  )
  obj <- list(
    Norms = list(
      data.frame(Sample = c(1, 1), Scale = c("PA", "NO"), Angle = c(90, 45),
                 M = c(1.11, 1.22), SD = c(0.11, 0.22),
                 stringsAsFactors = FALSE),
      data.frame(Sample = 1, Size = 100, Population = "p",
                 Reference = "r", URL = "u", stringsAsFactors = FALSE)
    ),
    Scales = data.frame(Abbrev = c("PA", "NO"), Angle = c(90, 45),
                        Items = c("1, 3", "2, 4"), stringsAsFactors = FALSE)
  )
  res <- env$audit_norms(batch, dir = dir, objects = list(fx = obj))
  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  expect_false("note-block-not-audited" %in% gaps$side)
})

# --- one note, two instruments (M79) -----------------------------------------
#
# An untagged block is handed whole to whoever asks, so two instruments reading
# one are each audited against rows that may be the other's -- and their rows
# are indistinguishable inside the block, both keying on (field, sample, scale)
# over the same octant names and sample numbers. The M75 review found this as a
# `claimed`-key collision; it is refused here instead, because tidy coverage
# counts over a mis-comparison are worse than an abort.

two_scale_object <- function(m = c(1.11, 1.22)) {
  list(
    Norms = list(
      data.frame(Sample = c(1, 1), Scale = c("PA", "NO"), Angle = c(90, 45),
                 M = m, SD = c(0.11, 0.22), stringsAsFactors = FALSE),
      data.frame(Sample = 1, Size = 100, Population = "p", Reference = "r",
                 URL = "u", stringsAsFactors = FALSE)
    ),
    Scales = data.frame(Abbrev = c("PA", "NO"), Angle = c(90, 45),
                        Items = c("1, 3", "2, 4"), stringsAsFactors = FALSE)
  )
}

block_rows <- function(tag = NULL) {
  c(
    if (is.null(tag)) "<!-- audit-values-begin -->"
    else paste0("<!-- audit-values-begin: ", tag, " -->"),
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "| M | 1 | NO | 1.22 | Table 1 |",
    "| SD | 1 | PA | 0.11 | Table 1 |",
    "| SD | 1 | NO | 0.22 | Table 1 |",
    "| Size | 1 | — | 100 | Table 1 |",
    "| Population | 1 | — | p | Table 1 |",
    "| Reference | 1 | — | r | Table 1 |",
    "| URL | 1 | — | u | Table 1 |",
    "| Angle | — | PA | not-published-in-source | convention |",
    "| Angle | — | NO | not-published-in-source | convention |",
    "| Items | — | PA | 1, 3 | Appendix |",
    "| Items | — | NO | 2, 4 | Appendix |",
    "<!-- audit-values-end -->"
  )
}

shared_batch <- function() {
  data.frame(
    instrument = c("fx", "fy"), sample = c(1, 1),
    citekey = c("shared", "shared"), divisor = c(1, 1),
    scales = c(TRUE, TRUE), stringsAsFactors = FALSE
  )
}

shared_objects <- function() {
  list(fx = two_scale_object(), fy = two_scale_object())
}

test_that("an untagged note read by two instruments is refused (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-shared-")
  dir.create(dir)
  writeLines(block_rows(), file.path(dir, "shared.md"))

  # The message names both instruments and the citekey: "some error" would pass
  # against a batch rejected for any of validate_batch()'s other reasons.
  expect_error(
    env$audit_norms(shared_batch(), dir = dir, objects = shared_objects()),
    "shared carries an untagged audit-values block but is read by 2 .*fx, fy"
  )
})

test_that("a note read by one instrument still parses (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-solo-")
  dir.create(dir)
  writeLines(block_rows(), file.path(dir, "shared.md"))

  res <- env$audit_norms(shared_batch()[1, , drop = FALSE], dir = dir,
                         objects = list(fx = two_scale_object()))
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
})

test_that("a tagged note read by two instruments still parses (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-tagged-")
  dir.create(dir)
  writeLines(c(block_rows("fx"), "", block_rows("fy")),
             file.path(dir, "shared.md"))

  res <- env$audit_norms(shared_batch(), dir = dir, objects = shared_objects())
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
  # And the pass compared something: 2 scales x (M, SD) + 4 record fields +
  # 2 scales x (Angle, Items) = 12 rows per instrument, both scales-bearing.
  expect_identical(
    nrow(env$shipped_values("fx", 1, TRUE, two_scale_object())), 12L
  )
})

test_that("source_note_tags() reads the two well-formed marker shapes (M79)", {
  env <- marker_defs()
  expect_identical(env$source_note_tags("<!-- audit-values-begin -->"), "")
  expect_identical(
    env$source_note_tags("<!-- audit-values-begin: iip64 -->"), "iip64"
  )
  # Leading whitespace and a colon with no space are both real shapes in the
  # committed notes' vicinity; neither may change the tag.
  expect_identical(
    env$source_note_tags("   <!-- audit-values-begin:iip32 -->"), "iip32"
  )
})

test_that("a malformed marker aborts instead of inventing a tag (M79)", {
  env <- marker_defs()
  # Not `expect_error()` alone: the pre-fix defect is that this returns the
  # string "ning", so a test that merely demanded SOME failure would pass
  # against a parser that failed for an unrelated reason. Assert the message.
  expect_error(
    env$source_note_tags("<!-- audit-values-beginning -->"),
    "malformed audit-values marker"
  )
  expect_error(
    env$source_note_tags("<!-- audit-values-begin: iip64"),
    "malformed audit-values marker"
  )
})

test_that("a note whose marker is malformed aborts rather than parsing (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-typo-")
  dir.create(dir)
  writeLines(c(
    "<!-- audit-values-beginning -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "<!-- audit-values-end -->"
  ), file.path(dir, "typo.md"))

  expect_error(
    env$parse_source_note("typo", dir, instrument = "fx"),
    "malformed audit-values marker"
  )
  expect_error(
    env$source_note_block_tags("typo", dir),
    "malformed audit-values marker"
  )
})
