# Block-marker recognition in the norms audit's source-note parser (M79).
#
# The parser located a note's machine-readable block with two independent
# greps -- one in parse_source_note(), one in source_note_block_tags() --
# neither anchored at the end of the line, and both handing whatever followed
# the prefix to the tag extractor, so `<!-- audit-values-beginning -->` was a
# marker carrying the tag "ning".
#
# The first fix ignored any marker lying inside a fenced code block, so a note
# could display its own format. That needed a markdown fence tracker, and the
# M79 review found four defects in it: an indented code block is not a fence,
# a `~~~` line closed a backtick fence, a line opening with an inline code
# span flipped fence parity for the rest of the note, and an unclosed fence
# hid every later block from every reader in silence -- a data-loss path the
# fix itself introduced.
#
# So the audit stopped inferring. A line carrying the marker prefix is either
# one of the three exact accepted shapes or an abort; markdown fences are not
# parsed at all, and an indented, fenced, inline or misspelled marker is
# refused rather than ignored. These tests pin the accepted shapes, and pin
# that each refused shape aborts by name.
#
# No note in cairn/references/ carries a stray `<!-- audit-values-` occurrence
# today, and the one note with a fence (browne1982.md) has no marker in it, so
# nothing committed is refused by this.
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

# One well-formed untagged block, then whatever the case under test appends.
note_with <- function(trailing) {
  dir <- tempfile("m79-marker-")
  dir.create(dir)
  writeLines(c(
    "# A source note",
    "",
    "<!-- audit-values-begin -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "| M | 1 | NO | 1.22 | Table 1 |",
    "<!-- audit-values-end -->",
    "",
    trailing
  ), file.path(dir, "note.md"))
  dir
}

# Every shape AC4 refuses, named by how it goes wrong. Each is fixtured alone:
# the first abort ends the read, so a note carrying all of them would only ever
# prove the first.
REFUSED_MARKERS <- list(
  # Indentation is how a note displays a marker without writing one, so it is
  # refused rather than trimmed -- including the 4-space code block that the
  # retired fence tracker never saw as a fence at all (F1).
  "an indented marker" = "    <!-- audit-values-begin: example -->",
  "an indented fenced marker" =
    c("  ```", "  <!-- audit-values-end -->", "  ```"),
  # The tag shapes: junk riding in after the tag, and a colon that promises a
  # tag it does not deliver.
  "junk after the tag" = "<!-- audit-values-begin: iip64 --> and more -->",
  "a doubled colon" = "<!-- audit-values-begin:: fx -->",
  "a colon with no tag" = "<!-- audit-values-begin: -->",
  # The prefix itself, misspelt or buried in prose.
  "a misspelt prefix" = "<!-- audit-values-beginning -->",
  # Neither begin nor end. Its guard RELOCATES rather than being load-bearing:
  # no-oped, "middle" falls through to the colon check and aborts there with
  # the same message (measured 2026-08-08). Kept because relying on that
  # fall-through is an accident of the word lengths.
  "a prefix that is neither begin nor end" = "<!-- audit-values-middle -->",
  "an end marker with a tail" = "<!-- audit-values-end: fx -->",
  "junk after an end marker" = "<!-- audit-values-end --> and more -->",
  "a marker inline in prose" = "See <!-- audit-values-begin --> for the shape.",
  # The return-2 four: `substring()` on an exhausted string returns "", so the
  # procedural recognizer's space before the terminator was silently optional.
  "no space before the terminator (begin)" = "<!-- audit-values-begin-->",
  "no space before the terminator (end)" = "<!-- audit-values-end-->",
  "padding before the terminator" = "<!-- audit-values-begin      -->",
  "a padded tag" = "<!-- audit-values-begin:   iip32   -->",
  # Two tolerances the return-1 tests deliberately pinned as accepted, both
  # outside AC4's "and nothing else" and used by no committed note: a colon
  # without its following space, and trailing whitespace.
  "a colon with no space after it" = "<!-- audit-values-begin:iip32 -->",
  "trailing whitespace" = "<!-- audit-values-begin -->  "
)

# The accepted set, stated extensionally: the two untagged shapes byte-exact,
# and the tagged form over instrument-name tags. Everything the recognizer
# accepts must be here, and everything here must be accepted -- the partition
# test below asserts both directions over this vector plus every single-line
# refused shape above, so the accepted set cannot silently grow a member the
# way the procedural recognizer's did (M79 return 2).
ACCEPTED_MARKERS <- c(
  "<!-- audit-values-end -->",
  "<!-- audit-values-begin -->",
  "<!-- audit-values-begin: iip64 -->",
  "<!-- audit-values-begin: a.b-c_9 -->"
)

test_that("the accepted marker set is exactly AC4's three shapes (M79)", {
  env <- marker_defs()
  reads <- function(line) {
    !inherits(tryCatch(env$source_note_marker(line), error = identity),
              "error")
  }
  # Single lines only: the multi-line REFUSED_MARKERS fixtures carry their
  # marker on one line; classify each line that carries the prefix.
  refused <- unlist(REFUSED_MARKERS, use.names = FALSE)
  refused <- refused[grepl("<!-- audit-values-", refused, fixed = TRUE)]
  boundary <- c(ACCEPTED_MARKERS, refused)
  got <- vapply(boundary, reads, logical(1), USE.NAMES = TRUE)
  expect_identical(names(got)[got], ACCEPTED_MARKERS)
})

test_that("an ambiguous marker line is refused, not ignored (M79)", {
  skip_on_cran()
  env <- marker_defs()
  for (why in names(REFUSED_MARKERS)) {
    dir <- note_with(REFUSED_MARKERS[[why]])
    # The specific message, not bare failure: a note broken in some unrelated
    # way would also raise, and would prove nothing about marker recognition.
    # Both readers, because they scanned independently before M79 and a shape
    # refused for one must be refused for the other.
    expect_error(
      env$parse_source_note("note", dir, instrument = "fx"),
      "malformed audit-values marker", info = why
    )
    expect_error(
      env$source_note_block_tags("note", dir),
      "malformed audit-values marker", info = why
    )
  }
})

test_that("a column-zero marker in a fence is read as real (M79)", {
  env <- marker_defs()
  # The honest limit of not parsing fences: this line is indistinguishable from
  # a real marker, so it becomes one. Pinned because the alternative -- guessing
  # it is only an example -- is what the retired fence tracker did, and what
  # made a block disappear. Not silently ignored is the property; refused is
  # not claimed.
  dir <- note_with(c(
    "```",
    "<!-- audit-values-begin: example -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 9.99 | not a real anchor |",
    "<!-- audit-values-end -->",
    "```"
  ))
  # Both blocks are reported: the real untagged one and the displayed one.
  expect_identical(env$source_note_block_tags("note", dir), c("", "example"))
  # And the note is now a two-block note, so asking it for an instrument no
  # block names aborts rather than handing back the first block.
  expect_error(
    env$parse_source_note("note", dir, instrument = "fx"),
    "has no audit-values block for"
  )
})

test_that("an unclosed fence cannot hide a later block (M79)", {
  env <- marker_defs()
  # The regression test for the silent-loss path the fence tracker introduced:
  # an unclosed fence put every later line "inside" it, so source_note_block_tags()
  # reported only the first block, the unclaimed-block sweep saw nothing, and
  # the shared-untagged refusal could not see the hidden block either. With no
  # fence tracking, the fence is just text and both blocks are reported.
  dir <- tempfile("m79-unclosed-")
  dir.create(dir)
  writeLines(c(
    "<!-- audit-values-begin: fx -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 1.11 | Table 1 |",
    "<!-- audit-values-end -->",
    "",
    "```",                      # opened and never closed
    "some example output",
    "",
    "<!-- audit-values-begin: fy -->",
    "| field | sample | scale | value | anchor |",
    "|---|---|---|---|---|",
    "| M | 1 | PA | 2.22 | Table 1 |",
    "<!-- audit-values-end -->"
  ), file.path(dir, "hidden.md"))

  expect_identical(env$source_note_block_tags("hidden", dir), c("fx", "fy"))
  expect_identical(
    env$parse_source_note("hidden", dir, instrument = "fy")$value, "2.22"
  )
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
    env$audit_norms(shared_batch(), dir = dir, objects = shared_objects(),
                    fixture_world = TRUE,
                    roster = env$roster_from_objects(shared_objects())),
    "shared carries an untagged audit-values block but is read by 2 .*fx, fy"
  )
})

test_that("a note read by one instrument still parses (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-solo-")
  dir.create(dir)
  writeLines(block_rows(), file.path(dir, "shared.md"))

  res <- env$audit_norms(shared_batch()[1, , drop = FALSE], dir = dir,
                         objects = list(fx = two_scale_object()),
                         fixture_world = TRUE,
                         roster = env$roster_from_objects(list(fx = two_scale_object())))
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
})

test_that("a tagged note read by two instruments still parses (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-tagged-")
  dir.create(dir)
  writeLines(c(block_rows("fx"), "", block_rows("fy")),
             file.path(dir, "shared.md"))

  res <- env$audit_norms(shared_batch(), dir = dir, objects = shared_objects(),
                         fixture_world = TRUE,
                         roster = env$roster_from_objects(shared_objects()))
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
  # And the pass compared something: 2 scales x (M, SD) + 4 record fields +
  # 2 scales x (Angle, Items) = 12 rows per instrument, both scales-bearing.
  expect_identical(
    nrow(env$shipped_values("fx", 1, TRUE, two_scale_object())), 12L
  )
})

test_that("source_note_tags() reads the two well-formed begin shapes (M79)", {
  env <- marker_defs()
  expect_identical(env$source_note_tags("<!-- audit-values-begin -->"), "")
  expect_identical(
    env$source_note_tags("<!-- audit-values-begin: iip64 -->"), "iip64"
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
