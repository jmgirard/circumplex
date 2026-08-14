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
                    roster = env$shipped_roster(shared_objects())),
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
                         roster = env$shipped_roster(list(fx = two_scale_object())))
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
})

test_that("a tagged note read by two instruments still parses (M79)", {
  env <- marker_defs()
  dir <- tempfile("m79-tagged-")
  dir.create(dir)
  writeLines(c(block_rows("fx"), "", block_rows("fy")),
             file.path(dir, "shared.md"))

  res <- env$audit_norms(shared_batch(), dir = dir, objects = shared_objects(),
                         roster = env$shipped_roster(shared_objects()))
  expect_identical(sum(res$ledger$kind == "mismatch"), 0L)
  # And the pass compared something: 2 scales x (M, SD) + 4 record fields +
  # 2 scales x (Angle, Items) = 12 rows per instrument, both scales-bearing.
  expect_identical(
    nrow(env$shipped_values("fx", 1, TRUE, two_scale_object())), 12L
  )
})

# --- the script's stop()/stopifnot() sites (M79 registry, M81 enumeration) ----
#
# One registry entry per ABORT SITE, each carrying the fixture that provokes
# that site. Two tests bind it: one asserts every fixture raises its own site's
# message, the other asserts the registry and the script's parse tree hold the
# same sites, in both directions.
#
# The domain is exactly the `stop()` and `stopifnot()` calls the parse walk
# collects, run block included -- NOT every way this script can fail. Outside
# it: other abort spellings (rlang::abort, cli::cli_abort, do.call-dispatched
# and aliased heads), calls built at runtime, process exits, warnings promoted
# under options(warn = 2), and failures that are not calls at all (a subscript
# error, a coercion, match.arg). Nothing here enumerates those, so nothing here
# claims them; what the walk cannot key it refuses rather than skips.
#
# The domain is the parse tree, not a sourced environment and not the file's
# text (helper-norms-audit-script.R says why both fail). The predecessor
# counted `stop(` occurrences in the deparsed bodies of the functions a
# defs-only sys.source() leaves behind: measured 2026-08-09 at 2ab626f6 it
# counted 12 while the parse walk saw 13 calls, the trailing run block being
# invisible to it, so an abort landing there registered by not being seen.
#
# Each parse_source_note() guard was no-oped in turn and the surviving
# behaviour recorded (measured 2026-08-08). Three are load-bearing in the
# strong sense -- no-oping them leaves the parser RETURNING ROWS rather than
# raising anything. Three relocate: some later expression, or another guard,
# fails instead, so the test reddens on the message without that proving this
# guard is what stopped the bad state. Recorded per case below rather than
# papered over, because a test that reddens for the wrong reason is the false
# coverage the guard rule warns about.

note_dir <- function(name, lines) {
  dir <- tempfile("m79-abort-")
  dir.create(dir)
  writeLines(lines, file.path(dir, paste0(name, ".md")))
  dir
}

table_head <- c("| field | sample | scale | value | anchor |", "|---|---|---|---|---|")

# One registry entry. The identity is (kind, binding, key, ordinal): `binding`
# is the top-level binding the site sits under, and `ordinal` is DECLARED here
# rather than derived, so registering one entry twice collides at build time
# instead of quietly becoming a second, matchless identity (M82).
site <- function(kind, binding, key, fixture, ordinal = 1L) {
  list(kind = kind, binding = binding, key = key, fixture = fixture,
       ordinal = ordinal)
}

SCRIPT_ABORTS <- norms_audit_build_registry(list(
  # validate_batch()'s stopifnot(), one entry per CONDITION: each fails on its
  # own and each needs a fixture that reaches it rather than its sibling.
  #
  # The non-data-frame fixture carries all five required names, so the sibling
  # condition below is TRUE and cannot abort in this one's place -- without
  # that, deleting `is.data.frame(batch)` would still leave the case red.
  site("stopifnot", "validate_batch", "is.data.frame(batch)", function(env) {
    env$validate_batch(list(instrument = "fx", sample = 1, citekey = "k",
                            divisor = 1, scales = TRUE))
  }),
  site("stopifnot", "validate_batch",
       paste0('all(c("instrument", "sample", "citekey", "divisor", ',
              '"scales") %in% names(batch))'),
       function(env) {
         # A real data frame, so the sibling condition is TRUE: only the
         # missing `scales` column can be what aborts here.
         env$validate_batch(data.frame(instrument = "fx", sample = 1,
                                       citekey = "k", divisor = 1,
                                       stringsAsFactors = FALSE))
       }),
  site("stop", "validate_batch", "AUDIT_BATCH names the same (instrument, sample) twice: {}",
       function(env) env$validate_batch(rbind(shared_batch(), shared_batch()[1, ]))),
  site("stop", "validate_batch",
       paste0("AUDIT_BATCH must mark exactly one `scales` entry per ",
              "instrument; wrong for: {}"),
       function(env) {
         noscales <- shared_batch()
         noscales$scales <- FALSE
         env$validate_batch(noscales)
       }),
  # validate_batch()'s divisor guards (M80). Each fixture is clean in every
  # other respect, so only the shape it names can be what aborts: the NA case
  # carries no Inf, the Inf case no NA, and the non-positive case neither.
  site("stop", "validate_batch", "AUDIT_BATCH$divisor must be numeric, not {}", function(env) {
    b <- shared_batch()
    b$divisor <- c("1", "1")
    env$validate_batch(b)
  }),
  site("stop", "validate_batch", "AUDIT_BATCH$divisor is missing for: {}", function(env) {
    b <- shared_batch()
    b$divisor <- c(1, NA_real_)
    env$validate_batch(b)
  }),
  site("stop", "validate_batch", "AUDIT_BATCH$divisor is not finite for: {}", function(env) {
    b <- shared_batch()
    b$divisor <- c(1, Inf)
    env$validate_batch(b)
  }),
  site("stop", "validate_batch", "AUDIT_BATCH$divisor must be strictly positive; wrong for: {}",
       function(env) {
         b <- shared_batch()
         b$divisor <- c(1, 0)
         env$validate_batch(b)
       }),
  # normalise_items() (M80): an unparseable item key aborts rather than
  # coercing to the string "NA", which two unparseable cells shared.
  site("stop", "normalise_items", "item key is not a comma-separated list of integers: {}",
       function(env) env$normalise_items("not an item key")),
  site("stop", "source_note_marker", "malformed audit-values marker: {}",
       function(env) env$source_note_tags("<!-- audit-values-beginning -->")),
  # The two "source note not found" sites are different functions, and the
  # registry carries both: this key is the one place a site count above 1 is
  # intended, so the set equality below is what keeps it honest.
  #
  # RELOCATES: no-oped, readLines() fails next -- "cannot open the connection".
  site("stop", "parse_source_note", "source note not found: {}", function(env) {
    env$parse_source_note("absent", note_dir("other", "x"), instrument = "fx")
  }),
  site("stop", "source_note_block_tags", "source note not found: {}", function(env) {
    env$source_note_block_tags("absent", note_dir("other", "x"))
  }),
  # RELOCATES, into the duplicate-tag guard: no-oped, the two untagged blocks
  # both carry the tag "", so that guard catches it. Removing BOTH would return
  # a row range spanning someone else's block, which is why neither is
  # redundant.
  site("stop", "parse_source_note", "source note {} has no well-formed audit-values block(s)",
       function(env) {
         env$parse_source_note("bad", note_dir("bad", c(
           "<!-- audit-values-begin -->", "<!-- audit-values-begin -->",
           table_head, "| M | 1 | PA | 1.1 | t |", "<!-- audit-values-end -->"
         )), instrument = "fx")
       }),
  # LOAD-BEARING: no-oped, it RETURNS the first of the two blocks tagged alike
  # (measured: 1 row, value 1.1, the second block's 2.2 never seen), so the
  # second instrument is audited against the first's rows and no comparison can
  # fail. Nothing else raises.
  site("stop", "parse_source_note", "source note {} tags two audit-values blocks alike: {}",
       function(env) {
         env$parse_source_note("dup", note_dir("dup", c(
           "<!-- audit-values-begin: fx -->", table_head,
           "| M | 1 | PA | 1.1 | t |", "<!-- audit-values-end -->",
           "<!-- audit-values-begin: fx -->", table_head,
           "| M | 1 | PA | 2.2 | t |", "<!-- audit-values-end -->"
         )), instrument = "fx")
       }),
  # RELOCATES: no-oped, k stays NA and b[[NA]] raises "subscript out of
  # bounds" -- a redder message for the same state, not a survival.
  site("stop", "parse_source_note", "source note {} has no audit-values block for {}; it tags: {}",
       function(env) {
         env$parse_source_note("tagged", note_dir("tagged", c(
           "<!-- audit-values-begin: fx -->", table_head,
           "| M | 1 | PA | 1.1 | t |", "<!-- audit-values-end -->",
           "<!-- audit-values-begin: fy -->", table_head,
           "| M | 1 | PA | 2.2 | t |", "<!-- audit-values-end -->"
         )), instrument = "zz")
       }),
  # LOAD-BEARING: no-oped, it RETURNS the row with anchor NA (measured: cell 5
  # of a four-cell row), so the value joins the note with its provenance anchor
  # silently gone. An anchor containing a literal "|" is how a real note gets
  # here.
  site("stop", "parse_source_note", "source note {} has {} malformed audit row(s); first: {}",
       function(env) {
         env$parse_source_note("short", note_dir("short", c(
           "<!-- audit-values-begin -->", table_head,
           "| M | 1 | PA | 1.1 |", "<!-- audit-values-end -->"
         )), instrument = "fx")
       }),
  # LOAD-BEARING: no-oped, it RETURNS the row with an empty value (measured),
  # which downstream compares as a mismatch -- a wrong ledger row rather than a
  # refusal, so the note's defect is reported as the package's.
  site("stop", "parse_source_note", "source note {} has {} audit row(s) with an empty value; first: {}",
       function(env) {
         env$parse_source_note("empty", note_dir("empty", c(
           "<!-- audit-values-begin -->", table_head,
           "| M | 1 | PA |  | t |", "<!-- audit-values-end -->"
         )), instrument = "fx")
       }),
  # shipped_values(): a batch row naming a sample the object does not carry
  # must abort, not audit nothing.
  site("stop", "shipped_values",
       paste0("{} has no single norms record for sample {} ",
              "({} norm rows, {} source rows)"),
       function(env) env$shipped_values("fx", 2, TRUE, two_scale_object())),
  site("stop", "refuse_shared_untagged_blocks",
       paste0("source note {} carries an untagged audit-values block but is ",
              "read by {} instruments ({}); tag each block with the ",
              "instrument it backs"),
       function(env) {
         dir <- tempfile("m81-untagged-")
         dir.create(dir)
         writeLines(block_rows(), file.path(dir, "shared.md"))
         env$refuse_shared_untagged_blocks(shared_batch(), dir)
       })
))

test_that("every registered abort site raises its own message (M79, M81)", {
  env <- marker_defs()
  for (s in SCRIPT_ABORTS) {
    # The site's own message, never bare failure: most of these fixtures would
    # also raise if the script were broken in an unrelated way, and several
    # reach a function with more than one guard in it.
    expect_abort_at_site(function() s$fixture(env), s$matcher)
  }
})

test_that("no stop()/stopifnot() site the walk collects is unregistered (M81, M82)", {
  norms_audit_script_path()  # skips against the installed package
  # Both directions at once: sorted (kind, binding, key, ordinal) ids compared
  # for identity, so an unregistered site and a registered non-site each fail.
  # The parse tree covers the trailing run block, which the sourced-environment
  # count this replaces could not see at all.
  #
  # The identity is the full four-part one since M82. M81 compared kind and key,
  # which the two `source note not found` sites satisfy in EITHER pairing: each
  # could be registered against the other's function and nothing here noticed.
  expect_identical(
    norms_audit_site_ids(SCRIPT_ABORTS),
    norms_audit_site_ids(norms_audit_abort_sites())
  )
})

# The identity's four components, probed one at a time (M82). Asserting only
# that the two sides agree today would leave every component unexercised: the
# sides agree because they were written to, and a comparison neither side can
# fail is the false coverage this whole file exists to remove.

test_that("registering one site twice is refused at build time (M82)", {
  one <- SCRIPT_ABORTS[[1L]]
  expect_error(
    norms_audit_build_registry(list(one, one)),
    "declares the same abort site twice"
  )
  # The refusal is about the DECLARED identity, not about being a copy: the
  # same site at a different declared ordinal is a different entry and builds.
  twin <- one
  twin$ordinal <- 2L
  expect_length(norms_audit_build_registry(list(one, twin)), 2L)
})

test_that("a wrong entry in any identity component reddens the comparison (M82)", {
  norms_audit_script_path()
  sites <- norms_audit_abort_sites()
  # The control: unmutated, the two sides agree. Every assertion below is a
  # departure from THIS, so a disagreement cannot come from somewhere else.
  expect_identical(norms_audit_site_ids(SCRIPT_ABORTS),
                   norms_audit_site_ids(sites))

  reddens <- function(mutate, info) {
    reg <- SCRIPT_ABORTS
    reg[[1L]] <- mutate(reg[[1L]])
    expect_false(
      identical(norms_audit_site_ids(reg), norms_audit_site_ids(sites)),
      info = info
    )
  }
  reddens(function(e) { e$binding <- "no_such_function"; e }, "binding")
  reddens(function(e) { e$kind <- "stop"; e }, "kind")
  reddens(function(e) { e$key <- paste0(e$key, " (corrupted)"); e }, "key")
  reddens(function(e) { e$ordinal <- 7L; e }, "ordinal")
})

test_that("the comparison cannot see a shared-key binding SWAP (M82)", {
  norms_audit_script_path()
  # Measured, and recorded as a bound rather than fixed: `norms_audit_site_ids()`
  # compares sorted identity multisets, and the `source note not found` pair is
  # identical in kind, key and ordinal, so exchanging the two entries' bindings
  # maps the multiset onto itself. Both entries stay individually correct-looking
  # while each names the other's function.
  #
  # This is exactly why RR17 makes BC7 and BC8 inseparable, and it is AC3's
  # stack assertion -- not this comparison -- that discriminates the pair. The
  # test exists so the blindness is stated where a reader meets the comparison,
  # instead of being rediscovered as a hole.
  swapped <- SCRIPT_ABORTS
  idx <- which(vapply(swapped, function(s) s$key, character(1)) ==
                 "source note not found: {}")
  expect_length(idx, 2L)
  b <- vapply(swapped[idx], function(s) s$binding, character(1))
  expect_identical(sort(b), c("parse_source_note", "source_note_block_tags"))
  swapped[[idx[[1L]]]]$binding <- b[[2L]]
  swapped[[idx[[2L]]]]$binding <- b[[1L]]
  expect_identical(norms_audit_site_ids(swapped),
                   norms_audit_site_ids(norms_audit_abort_sites()))
})

test_that("two identical guards in one function stay separable (M82)", {
  # AC1's set equality and the duplicate-identity refusal have to be jointly
  # satisfiable: a function carrying the same guard twice is a legal script, and
  # the ordinal is what keeps its two sites distinct instead of collapsing them
  # into one identity the registry can only half-satisfy.
  # The key clears AC4's 15-literal-character floor, since the registry built
  # below runs the matcher constructor over it.
  planted_key <- "a planted guard that clears the floor"
  sites <- norms_audit_abort_sites(parse(
    text = sprintf('f <- function(x) { stop("%s"); stop("%s") }',
                   planted_key, planted_key),
    keep.source = FALSE
  ))
  expect_identical(
    vapply(sites, function(s) s$ordinal, integer(1)), c(1L, 2L)
  )
  expect_identical(vapply(sites, function(s) s$binding, character(1)),
                   c("f", "f"))
  # Registering both, each at its own declared ordinal, builds and matches.
  reg <- norms_audit_build_registry(list(
    site("stop", "f", planted_key, function(env) NULL, ordinal = 1L),
    site("stop", "f", planted_key, function(env) NULL, ordinal = 2L)
  ))
  expect_identical(norms_audit_site_ids(reg), norms_audit_site_ids(sites))
})

test_that("the identity carries no source reference (M82)", {
  # A STANDING GUARD, not a verification: the shipped enumerator parses with
  # keep.source = FALSE, so it has no line numbers to key on and this passes
  # against it trivially. It exists so that a later change to srcref-based
  # identity -- which would make every identity move whenever a comment above it
  # moved -- fails here rather than in a confusing diff months later.
  base <- 'f <- function(x) stop("boom")'
  shifted <- c("# a comment inserted above", "", base)
  expect_identical(
    norms_audit_site_ids(norms_audit_abort_sites(
      parse(text = base, keep.source = FALSE))),
    norms_audit_site_ids(norms_audit_abort_sites(
      norms_audit_parse_text(shifted)))
  )
})

# The matcher floors (M82, RR17 BC9). Each floor gets its own probe, because a
# shortened key can never fall below a floor that tracks it -- the `stopifnot`
# floor is `min(nchar(key), 40)`, so mutating the key moves the floor with it
# and proves nothing. The two are checked at different times for the same
# reason: a `stop` key's literal content is known when the registry is built,
# while a stem exists only once a message has been raised.

test_that("a stop() key under the literal floor stops the build (M82)", {
  short <- "too short"  # 9 literal characters, under the floor of 15
  expect_lt(nchar(short), NORMS_AUDIT_STOP_KEY_FLOOR)
  expect_error(
    norms_audit_matcher("stop", short),
    "literal characters, under the floor"
  )
  # `{}` is an interpolated argument, matching anything, so it is not
  # discrimination and does not count: a key that clears the floor only by
  # counting its placeholders is refused too.
  expect_error(
    norms_audit_matcher("stop", paste0(short, "{}{}{}{}{}{}{}{}")),
    "literal characters, under the floor"
  )
  # Every shipped stop key clears it, with the headroom RR17 states.
  shipped <- vapply(
    Filter(function(s) s$kind == "stop", norms_audit_abort_sites()),
    function(s) nchar(norms_audit_key_literals(s$key)), integer(1)
  )
  expect_gte(min(shipped), 23L)
})

test_that("a stopifnot matcher rejects a degenerate stem (M82)", {
  # The defect this floor closes: before it, the shipped check accepted any
  # non-empty prefix, so a one-character stem satisfied a 20-character key and
  # any `stopifnot()` in the script could stand in for any other.
  key <- "is.data.frame(batch)"
  m <- norms_audit_matcher("stopifnot", key)
  expect_true(m("is.data.frame(batch) is not TRUE"))
  expect_false(m("i is not TRUE"))
  # ... and the old rule would have accepted it, so the assertion above is
  # about the floor rather than about the prefix test.
  expect_true(startsWith(squish(key), norms_audit_stopifnot_stem("i is not TRUE")))
  # The floor is the key's own length where the key is shorter than 40, so a
  # truncated-but-honest stem from a LONG condition still matches.
  long <- paste(rep("condition_fragment", 5L), collapse = " + ")
  expect_gt(nchar(long), NORMS_AUDIT_STEM_FLOOR)
  ml <- norms_audit_matcher("stopifnot", long)
  expect_true(ml(paste(substr(long, 1L, 66L), "....")))
  expect_false(ml("condition ...."))
})

test_that("no matcher accepts a message from another site (M82)", {
  env <- marker_defs()
  # Each fixture raised ONCE, its message captured under the shared locale pin.
  # Once, because provoking a site per cell would make the matrix quadratic in
  # fixture runs for no gain -- the message is a property of the site.
  msgs <- vapply(SCRIPT_ABORTS, function(s) {
    norms_audit_with_c_messages(
      tryCatch({
        s$fixture(env)
        NA_character_
      }, error = conditionMessage)
    )
  }, character(1))
  # A fixture that raised nothing would contribute a vacuous row and column.
  expect_false(anyNA(msgs))

  n <- length(SCRIPT_ABORTS)
  accepts <- matrix(FALSE, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) accepts[i, j] <- SCRIPT_ABORTS[[i]]$matcher(msgs[[j]])
  }

  # Every site matches its own message. Without this the equality below could
  # be satisfied by a matcher that accepts nothing at all.
  expect_true(all(diag(accepts)))

  # And the off-diagonal accepting set EQUALS the declared shared-key pairs --
  # derived from the registry, not listed here, so a new shared pair changes
  # both sides of this comparison at once and a leaky matcher changes only one.
  key <- vapply(SCRIPT_ABORTS, function(s) paste(s$kind, s$key, sep = "\t"),
                character(1))
  offdiag <- !diag(TRUE, n)
  expect_identical(accepts & offdiag, outer(key, key, "==") & offdiag)

  # Today that set is exactly the two `source note not found` cells. Pinned, so
  # the equality above cannot quietly become true of a larger set.
  expect_identical(sum(accepts & offdiag), 2L)
})

# Shared-key sites: the pair one message cannot tell apart (M82, RR17 BC8).
#
# `source note not found: {}` is raised by two functions. Their messages are
# identical by construction, so no matcher discriminates them and the identity
# comparison cannot either (see the SWAP test above). What is left is WHERE the
# abort happened, and that is what these assert.

test_that("every shared-key site is stack-bound to its own function (M82)", {
  env <- marker_defs()
  shared <- norms_audit_shared_key_sites(SCRIPT_ABORTS)

  # The roster is DERIVED from the registry, so a shared pair added later joins
  # this loop by existing. Pinned all the same, so that a new pair is a visible
  # change here rather than a silent extra iteration.
  expect_identical(
    sort(vapply(shared, function(s) s$binding, character(1))),
    c("parse_source_note", "source_note_block_tags")
  )

  for (s in shared) {
    frames <- norms_audit_capture_abort_frames(function() s$fixture(env))
    # Non-empty first: a capture that caught nothing would make every binding
    # assertion below vacuous, and vacuous is the failure mode this file is
    # about. NA_character_ from the search would satisfy nothing either way,
    # but an empty capture should say so in its own words.
    expect_true(length(frames) > 0L, info = paste("no frames captured:", s$binding))
    expect_identical(
      norms_audit_innermost_script_binding(frames, env), s$binding,
      info = s$binding
    )
  }
})

test_that("a shared-key fixture pointed at its twin's trigger reddens (M82)", {
  env <- marker_defs()
  shared <- norms_audit_shared_key_sites(SCRIPT_ABORTS)
  expect_length(shared, 2L)

  # The mutation the identity comparison cannot make: keep each entry's binding
  # and give it the OTHER site's fixture. Both still raise, with the same
  # message, and every other assertion in this file stays green.
  crossed <- shared
  crossed[[1L]]$fixture <- shared[[2L]]$fixture
  crossed[[2L]]$fixture <- shared[[1L]]$fixture

  twin <- rev(vapply(shared, function(s) s$binding, character(1)))
  for (i in seq_along(crossed)) {
    s <- crossed[[i]]
    frames <- norms_audit_capture_abort_frames(function() s$fixture(env))
    # The assertion fails ...
    expect_length(
      norms_audit_expectation_failures(
        expect_identical(norms_audit_innermost_script_binding(frames, env),
                         s$binding)
      ),
      1L
    )
    # ... and fails because the stack names the TWIN, not because the capture
    # came back empty. An NA would redden the line above just as well, which
    # would make this mutation evidence for nothing (measured 2026-08-14: 12
    # frames captured, resolving to the twin in both directions).
    expect_identical(
      norms_audit_innermost_script_binding(frames, env), twin[[i]]
    )
  }
})

test_that("a named-form site is matched by exact equality, not a stem (M81)", {
  # A synthetic site, raised through the real mechanism: a stopifnot() call
  # whose condition is named, built as a call so the message is R's own rather
  # than one this test writes.
  raise <- function(msg) {
    cl <- as.call(list(quote(stopifnot), FALSE))
    names(cl) <- c("", msg)
    function() eval(cl)
  }
  # What expect_abort_at_site() is asked to accept is the question, so its
  # FAILURES are what this test reads -- running it directly would only report
  # whether this file passes, which is the opposite of what is under test.
  failures <- function(expr) {
    out <- character()
    withCallingHandlers(expr, expectation_failure = function(cnd) {
      out <<- c(out, conditionMessage(cnd))
      invokeRestart("continue_test")
    })
    out
  }

  key <- "divisor must be numeric"
  expect_identical(tryCatch(raise(key)(), error = conditionMessage), key)

  m <- norms_audit_matcher("stopifnot_named", key)
  expect_length(failures(expect_abort_at_site(raise(key), m)), 0L)
  # Both directions fail: a strict superstring and a strict substring.
  expect_length(
    failures(expect_abort_at_site(raise(paste0(key, " and finite")), m)), 1L
  )
  truncated <- substr(key, 1L, nchar(key) - 1L)
  expect_length(failures(expect_abort_at_site(raise(truncated), m)), 1L)
  # An unrecognised kind is refused rather than dispatched to the loosest
  # matcher. Named with a typo, as the real incident was. Since M82 the refusal
  # is the CONSTRUCTOR's, so it fires at registry build rather than at the
  # first assertion that happens to use the site.
  expect_error(
    norms_audit_matcher("stopifnot_nmaed", key),
    "unknown abort site kind"
  )
  # Every kind the walk can emit is one the matcher accepts, so the registry
  # and the matcher cannot drift apart in the other direction either.
  expect_true(all(
    vapply(norms_audit_abort_sites(), function(s) s$kind, character(1)) %in%
      SITE_KINDS
  ))

  # And the substring case is the one that needed equality: the stem matcher
  # the POSITIONAL form uses accepts it, so a named site keyed through that
  # matcher would report a truncated message as its own.
  stem <- norms_audit_stopifnot_stem(truncated)
  expect_true(nzchar(stem) && startsWith(squish(key), stem))
})

test_that("the walk keys named conditions and refuses the rest (M81)", {
  sites <- function(txt) norms_audit_abort_sites(parse(text = txt, keep.source = FALSE))

  # A named condition IS a condition, keyed on its name -- the runtime message.
  # Reading positional arguments only is what returned this milestone: such a
  # guard fires and contributes no key, so nothing ever reports it missing.
  # Each site carries its full identity since M82: the fixture text binds
  # nothing, so both sit under `"<run>"` at ordinal 1.
  expect_identical(
    sites('stopifnot("must be numeric" = is.numeric(x), is.data.frame(y))'),
    list(list(kind = "stopifnot_named", key = "must be numeric",
              binding = "<run>", ordinal = 1L),
         list(kind = "stopifnot", key = "is.data.frame(y)",
              binding = "<run>", ordinal = 1L))
  )

  # `stop()`'s own formals carry no message, so they leave the key alone.
  expect_identical(
    sites('stop("boom", call. = FALSE, domain = NA)'),
    list(list(kind = "stop", key = "boom", binding = "<run>", ordinal = 1L))
  )

  # Refused, not skipped. Each of these is a real abort site whose message the
  # keying rules cannot predict, and a site the walk passes over silently is
  # the false coverage the whole registry exists to remove: `exprs = ` holds
  # its conditions in an expression object this walk does not descend, and a
  # `stop()` argument named anything else is concatenated into the message
  # while the template drops it (`stop("boom ", tail = "T")` raises "boom T").
  expect_error(sites("stopifnot(exprs = { TRUE })"),
               "cannot enumerate .*stopifnot\\(\\) formal exprs")
  expect_error(sites("stopifnot(local = TRUE)"),
               "cannot enumerate .*stopifnot\\(\\) formal local")
  expect_error(sites('stop("boom ", tail = "TAIL")'),
               "cannot enumerate .*stop\\(\\) argument named tail")

  # The reserved set is read from formals(), not written out: this R spells
  # the third formal `exprObject` where RR17 spelled it `exprs.env`, and a
  # literal list would have stopped covering whichever name R renamed.
  expect_true(all(c("exprs", "local") %in% STOPIFNOT_RESERVED))
  expect_false("..." %in% STOPIFNOT_RESERVED)
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
