# Norms provenance audit (M72)
#
# Compares every shipped audited-field value against the value recorded in the
# instrument's committed source note, and writes a ledger of mismatches.
#
# The source-side values are PARSED from the notes rather than retyped here on
# purpose: a hand-typed table in this script would be a second transcription by
# the same reader, so a green ledger would prove only that one reader copied
# consistently, not that the package matches the source. The note is the single
# source of truth; this script is the comparison.
#
# Usage (from the package root, with the package loaded):
#   Rscript -e 'devtools::load_all(); source("data-raw/audit-norms.R")'
# Writes: data-raw/norms-audit-ledger.csv   (or -prefix.csv, see LEDGER_PATH)
#         data-raw/norms-audit-coverage.csv (the coverage report, see AC3)

# One row per (instrument, sample), because an instrument's norm samples can
# come from different papers -- iipsc's college sample and its outpatient
# sample do -- and because the shipped side has to be enumerated one sample at
# a time (see shipped_values()).
#
# `divisor` is the documented unit deviation for that sample: shipped value =
# source value / divisor. csip's source prints octant SUM scores (0-24) and
# data-raw/csip.R divides by 8 to express them as item means on the
# instrument's own 0-3 anchor range. It rides in the batch rather than in a
# second lookup because it is a property of the (instrument, sample) pairing,
# not of the instrument.
#
# `scales` marks the one entry per instrument whose note is the source for the
# instrument-level fields, Angle and Items. Those are not per-sample facts, so
# auditing them once per sample would demand the same item map from every
# sample's paper; exactly one entry per instrument carries TRUE, checked by
# validate_batch().
AUDIT_BATCH <- data.frame(
  instrument = c("csie", "csig", "csip", "csiv", "iitc",
                 "iis32", "iis64", "ipipipc", "isc",
                 "cais", "cais", "iei", "iei",
                 "igicr", "igicr", "igicr", "iipsc", "iipsc",
                 "iip32", "iip32", "iip32", "iip64", "iip64", "iip64"),
  sample     = c(rep(1, 9),
                 1, 2, 1, 2,
                 1, 2, 3, 1, 2,
                 1, 2, 3, 1, 2, 3),
  citekey    = c("locke2007", "locke2014", "boudreaux2018", "locke2000",
                 "bliton2019", "hatcher2012", "hatcher2009", "markey2009",
                 "hopwood2011",
                 "sodano2006", "sodano2006", "horner2024", "horner2024",
                 "trucco2013", "trucco2013", "trucco2013",
                 "hopwood2008", "soldz1995",
                 "horowitz2003", "horowitz2003", "horowitz2003",
                 "horowitz2003", "horowitz2003", "horowitz2003"),
  # iipsc sample 1 is the one deviation among the batch-3 instruments:
  # hopwood2008 Table 1 prints octant SUMS over four 0-4 items and
  # data-raw/iipsc.R divides by 4 to express them as item means. Its sample 2
  # needs no divisor -- soldz1995 already prints item means.
  #
  # Both IIP instruments deviate the same way: horowitz2003 prints raw scale
  # SUMS over a common 0-4 item anchor range, and the package ships item means,
  # so the divisor is the scale length -- 4 for the IIP-32's four-item scales
  # (Table F.5, p. 91) and 8 for the IIP-64's eight-item scales (Table 4.4,
  # pp. 27-29). One source note, one citekey, two instruments: the note tags its
  # two audit-values blocks and parse_source_note() picks by instrument.
  divisor    = c(1, 1, 8, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1,
                 1, 1, 1, 4, 1,
                 4, 4, 4, 8, 8, 8),
  # The `scales` entry per multi-sample instrument is the pass whose note
  # actually carries the instrument-level rows. For iipsc that is SAMPLE 2:
  # soldz1995 prints the item-to-octant grouping and hopwood2008 does not.
  scales     = c(rep(TRUE, 9),
                 TRUE, FALSE, TRUE, FALSE,
                 TRUE, FALSE, FALSE, FALSE, TRUE,
                 TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# A batch that names one (instrument, sample) twice would audit it twice and
# report both passes' coverage; one that marks no `scales` entry for an
# instrument would silently stop auditing its Angle and Items rows, and one
# that marks two would demand the item map from both papers. Neither shows up
# in any count, so both are refused here rather than discovered in a ledger.
validate_batch <- function(batch) {
  stopifnot(is.data.frame(batch),
            all(c("instrument", "sample", "citekey", "divisor", "scales") %in%
                  names(batch)))
  dup <- duplicated(paste(batch$instrument, batch$sample))
  if (any(dup)) {
    stop("AUDIT_BATCH names the same (instrument, sample) twice: ",
         paste(unique(paste(batch$instrument, batch$sample)[dup]),
               collapse = ", "), call. = FALSE)
  }
  # `divisor` divides every source-side M and SD before the comparison, so an
  # unusable one either compares the wrong numbers while every count reads
  # clean, or dies far from its cause. A non-numeric column reaches
  # `values_agree()`'s division and raises R's "non-numeric argument to binary
  # operator" from inside the ledger loop, naming neither the batch nor the
  # column. NA and Inf fail the other way, silently: x/NA is NA and x/Inf is 0,
  # so every value compares FALSE or 0 against the source; 0 or a negative
  # divisor turns a real value into a wrong one.
  # None of the four is distinguishable downstream from a package error, which
  # is why each is refused here and named.
  #
  # A `divisor` column absent altogether is refused by the stopifnot() above.
  where <- function(i) paste0(batch$instrument[i], " sample ", batch$sample[i])
  if (!is.numeric(batch$divisor)) {
    stop("AUDIT_BATCH$divisor must be numeric, not ",
         class(batch$divisor)[[1L]], call. = FALSE)
  }
  bad <- which(is.na(batch$divisor))
  if (length(bad)) {
    stop("AUDIT_BATCH$divisor is missing for: ",
         paste(where(bad), collapse = ", "), call. = FALSE)
  }
  bad <- which(!is.finite(batch$divisor))
  if (length(bad)) {
    stop("AUDIT_BATCH$divisor is not finite for: ",
         paste(where(bad), collapse = ", "), call. = FALSE)
  }
  bad <- which(batch$divisor <= 0)
  if (length(bad)) {
    stop("AUDIT_BATCH$divisor must be strictly positive; wrong for: ",
         paste(where(bad), collapse = ", "), call. = FALSE)
  }

  n_scales <- tapply(batch$scales, batch$instrument, sum)
  bad <- names(n_scales)[n_scales != 1L]
  if (length(bad)) {
    stop("AUDIT_BATCH must mark exactly one `scales` entry per instrument; ",
         "wrong for: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# `roster` states which shipped (instrument, sample) pairs the run must cover,
# and the sweep at the end of audit_norms() reports as a gap every pair the
# roster holds that the batch does not name. So the roster is the only thing
# standing between an unaudited sample and a clean count: one that is empty, or
# whose columns are spelt `Instrument`/`Sample`, reports NO gaps and the run
# reads clean over data nothing read -- the silent shape the sweep exists to
# remove, reintroduced through the argument that lets a fixture state its own
# roster. Measured 2026-08-14 before this guard: a one-instrument slice of
# AUDIT_BATCH with a capitalised-column roster reported 0 gaps, against 23 with
# the shipped one.
#
# audit_norms() resolves its NULL default before calling this, so the default
# sweep is what gets checked and a caller who passes nothing is never refused
# for passing nothing.
#
# The two key columns are checked one at a time, and each refusal names the
# column it is about. One condition over both (`all(c("instrument", "sample")
# %in% names(roster))`, through M85) raised a message naming BOTH columns for
# either omission, so a roster misspelling one read exactly like a roster
# misspelling the other -- and the assertion covering it survived weakening the
# condition to a single column, since the deparsed condition text is what the
# message carries either way (M86).
#
# Written out one column at a time rather than looped, because a loop is one
# `stop()` call carrying the column as an argument: its registry key would be
# "`roster` has no `{}` column", whose matcher accepts BOTH messages, and the
# cross-discrimination matrix would then certify as distinguishable two
# refusals it cannot tell apart. Two calls, two keys, two fixtures.
validate_roster <- function(roster) {
  stopifnot(is.data.frame(roster))
  if (!"instrument" %in% names(roster)) {
    stop("`roster` has no `instrument` column; it has: ",
         paste(names(roster), collapse = ", "), call. = FALSE)
  }
  if (!"sample" %in% names(roster)) {
    stop("`roster` has no `sample` column; it has: ",
         paste(names(roster), collapse = ", "), call. = FALSE)
  }
  if (!nrow(roster)) {
    stop("`roster` names no (instrument, sample) pair to cover, so every ",
         "unaudited shipped sample would be reported as covered",
         call. = FALSE)
  }
  invisible(TRUE)
}

NOT_PUBLISHED <- "not-published-in-source"

# A source note may also table a norm sample the source publishes and the
# package does not ship. Those rows carry field = "note-only" and are exempt
# from the coverage report: they have no shipped counterpart by construction.
NOTE_ONLY <- "note-only"

# A Reference row's source-side value is the author-year credit for the norm
# SAMPLE, and the audit is only as strong as where that credit came from. Where
# the source itself prints the credit, the anchor names the page and the two
# sides of the comparison have independent origins. Where no page prints it --
# locke2000's undated CSIV norms table is the known case -- the note author
# CONSTRUCTED the credit, so comparing it against the shipped string compares
# two descendants of one origin and can never fail (the M72 lesson). Those rows
# are not removed, because the shipped credit still has to be recorded
# somewhere; they are LABELLED with this token in the anchor cell and listed as
# exempt coverage rows, so a reader of the coverage report can tell a quoted
# credit from an authored one instead of finding them indistinguishable.
CONSTRUCTED_CREDIT <- "constructed-credit"

# --- source side -------------------------------------------------------------

MARKER_PREFIX <- "<!-- audit-values-"
MARKER_END <- "<!-- audit-values-end -->"
MARKER_BEGIN <- "<!-- audit-values-begin -->"
MARKER_TAGGED <- "^<!-- audit-values-begin: ([A-Za-z0-9._-]+) -->$"

# Read one marker line, or refuse it.
#
# The audit does not INFER which lines carrying the marker prefix are real
# markers; it refuses the ones it cannot read unambiguously. Its predecessor
# inferred, ignoring any marker inside a fenced code block so that a note
# could display its own format -- which meant carrying a markdown fence
# tracker the audit needs for nothing else, and getting it wrong four
# independent ways (M79 review): an indented code block is not a fence, a
# `~~~` line closed a backtick fence, a line opening with an inline code span
# flipped fence parity for the rest of the note, and -- the one that loses
# data -- an unclosed fence hid every later block from every reader, in
# silence. Refusing is the doctrine this file already follows elsewhere: a
# marker one character wrong is a block someone meant to write, not prose.
#
# The accepted set IS the three definitions above: the two literals, compared
# byte-for-byte, and the tagged form, matched whole-line. Nothing is trimmed
# and no substring arithmetic runs, because the set an accepted-shape
# procedure admits is emergent -- the M79 return-2 review found four shapes
# admitted by `substring()` returning "" on an exhausted string, which made
# the space before the terminator optional. A definitional recognizer has no
# next member to find: what the constants say is what is accepted, trailing
# whitespace and a colon without its following space included in the refusals.
# Every other line CONTAINING the prefix aborts, so an indented, inline, or
# misspelled marker stops the audit rather than joining a block or hiding one.
# Prose may still discuss audit-values blocks; what it may not do is write the
# comment opener.
#
# What this does NOT claim: a column-zero marker inside a fence is
# indistinguishable from a real one and is read as real. That is the fail-closed
# side of the trade -- such a line is never silently DROPPED, which is the
# property the fence tracker broke. A note may not display a marker at column
# zero; indent it, and it aborts by name.
source_note_marker <- function(one) {
  if (identical(one, MARKER_END)) return(list(kind = "end", tag = ""))
  if (identical(one, MARKER_BEGIN)) return(list(kind = "begin", tag = ""))
  hit <- regmatches(one, regexec(MARKER_TAGGED, one))[[1L]]
  if (length(hit) == 2L) return(list(kind = "begin", tag = hit[[2L]]))
  stop("malformed audit-values marker: ", one, call. = FALSE)
}

# The tag carried by each begin marker; "" for an untagged block. A named
# helper because the accepted and refused marker shapes are tested directly.
source_note_tags <- function(begin_lines) {
  vapply(begin_lines, function(one) source_note_marker(one)$tag,
         character(1), USE.NAMES = FALSE)
}

# Locate a note's block markers: begin lines, end lines, and each begin's tag.
# The single scanner both readers below share, so a shape refused for one is
# refused for the other -- they previously ran independent greps and drifted.
source_note_markers <- function(lines) {
  hit <- which(grepl(MARKER_PREFIX, lines, fixed = TRUE))
  marks <- lapply(lines[hit], source_note_marker)
  kind <- vapply(marks, function(m) m$kind, character(1))
  list(begin = hit[kind == "begin"], end = hit[kind == "end"],
       tags = vapply(marks[kind == "begin"], function(m) m$tag, character(1)))
}

# Every block tag a note carries, in file order. Read by the unclaimed-block
# sweep, which must see blocks NO batch row selected -- parse_source_note()
# by construction only ever returns one.
source_note_block_tags <- function(citekey,
                                   dir = file.path("cairn", "references")) {
  path <- file.path(dir, paste0(citekey, ".md"))
  if (!file.exists(path)) {
    stop("source note not found: ", path, call. = FALSE)
  }
  source_note_markers(readLines(path, warn = FALSE))$tags
}

# Parse the machine-readable block a source note carries between its
# audit-values markers. Returns a data.frame(field, sample, scale, value,
# anchor), carrying the block's tag in attr(, "tag").
#
# A note may carry SEVERAL blocks, each tagged with the instrument it backs:
#
#   <!-- audit-values-begin: iip64 -->
#
# One source can be the published source for more than one instrument -- the
# IIP manual (M75) norms both the IIP-64 and the IIP-32 -- and their rows
# collide, because the comparison keys on (field, sample, scale) and both
# instruments have samples 1-3 over the same eight octant names. Two notes for
# one source would duplicate its provenance and citation, so the note stays
# one page and the blocks carry the instrument.
#
# `instrument` selects among tagged blocks. A note with a single UNTAGGED block
# is the batch-1..3 shape and is returned whatever is asked for -- exactly one
# instrument names such a note, so there is nothing to disambiguate. What is
# refused is the ambiguous middle: asking a multi-block note for an instrument
# none of its blocks names aborts rather than falling back to the first block,
# which would audit one instrument against another's values and could not fail.
parse_source_note <- function(citekey,
                              dir = file.path("cairn", "references"),
                              instrument = NULL) {
  path <- file.path(dir, paste0(citekey, ".md"))
  if (!file.exists(path)) {
    stop("source note not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  marks <- source_note_markers(lines)
  b <- marks$begin
  e <- marks$end
  # Blocks must nest as begin/end/begin/end: interleaved or unclosed markers
  # would silently hand back a row range spanning someone else's block.
  ok <- length(b) && length(e) == length(b) && all(e > b) &&
    (length(b) == 1L || all(b[-1L] > e[-length(e)]))
  if (!ok) {
    stop("source note ", citekey, " has no well-formed audit-values block(s)",
         call. = FALSE)
  }
  tags <- marks$tags
  if (anyDuplicated(tags)) {
    stop("source note ", citekey, " tags two audit-values blocks alike: ",
         paste(tags[duplicated(tags)], collapse = ", "), call. = FALSE)
  }
  if (length(b) == 1L && !nzchar(tags)) {
    k <- 1L
  } else {
    k <- match(instrument %||% "", tags)
    if (is.na(k)) {
      stop("source note ", citekey, " has no audit-values block for ",
           instrument %||% "<no instrument given>", "; it tags: ",
           paste(tags, collapse = ", "), call. = FALSE)
    }
  }
  rows <- lines[(b[[k]] + 1L):(e[[k]] - 1L)]
  rows <- rows[grepl("^\\|", rows)]
  # drop the header and the |---|---| separator
  rows <- rows[-(1:2)]
  cells <- lapply(strsplit(sub("^\\|", "", sub("\\|$", "", rows)), "|",
                           fixed = TRUE), trimws)
  # A row that does not split into exactly five cells is a malformed note, not
  # a row to skip: silently dropping it would remove a value from the audit
  # while every count still read clean (an anchor containing a literal "|" is
  # the way this happens).
  #
  # Five, not four: M74 added the `sample` column so a note backing several of
  # an instrument's norm samples can say which row belongs to which. There is
  # deliberately no four-column fallback -- a default sample would make a
  # mis-migrated note read as sample 1 and audit clean against the wrong rows,
  # which is the very collision the column exists to remove.
  bad <- which(vapply(cells, length, integer(1)) != 5L)
  if (length(bad)) {
    stop("source note ", citekey, " has ", length(bad),
         " malformed audit row(s); first: ", rows[[bad[[1]]]], call. = FALSE)
  }
  empty <- which(vapply(cells, function(x) !nzchar(x[[4]]), logical(1)))
  if (length(empty)) {
    stop("source note ", citekey, " has ", length(empty),
         " audit row(s) with an empty value; first: ", rows[[empty[[1]]]],
         call. = FALSE)
  }
  out <- data.frame(
    field  = vapply(cells, `[`, character(1), 1L),
    sample = vapply(cells, `[`, character(1), 2L),
    scale  = vapply(cells, `[`, character(1), 3L),
    value  = vapply(cells, `[`, character(1), 4L),
    anchor = vapply(cells, `[`, character(1), 5L),
    stringsAsFactors = FALSE
  )
  attr(out, "tag") <- tags[[k]]
  out
}

# Angle and Items are instrument-level facts with no sample of their own; both
# sides of the comparison mark them with this token so they key alike.
NO_SAMPLE <- "—"

# --- shipped side ------------------------------------------------------------

# Enumerate ONE sample's shipped audited-field values from the package object.
# Norms[[1]]'s scale-name column is `Scale` for some instruments and `Abbrev`
# for others, so the key is normalised here rather than assumed.
#
# One sample at a time, not all of them: M72 enumerated the whole object and
# keyed each row by (field, scale), which for a two-sample instrument produced
# two rows keyed "M PA" that match() both resolved to the first. Sample 2's
# shipped mean was then compared against sample 1's source value and could
# never disagree. Filtering here means the join below cannot collide, and the
# emitted `sample` column keeps the key honest at the comparison too.
#
# `scales` selects whether this pass also emits the instrument-level Angle and
# Items rows; exactly one of an instrument's passes does (see AUDIT_BATCH).
# `obj` is injectable so a test can drive a synthetic two-sample instrument.
shipped_values <- function(inst, sample, scales = TRUE, obj = NULL) {
  if (is.null(obj)) obj <- get(inst, envir = asNamespace("circumplex"))
  norms <- obj$Norms[[1]]
  src <- obj$Norms[[2]]
  scl <- obj$Scales
  key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"

  nrows <- norms[norms$Sample == sample, , drop = FALSE]
  srows <- src[src$Sample == sample, , drop = FALSE]
  # A batch entry naming a sample the object does not carry must abort, not
  # audit nothing: an empty comparison leaves the ledger, the coverage report
  # and every printed count clean, which is indistinguishable from a pass.
  if (nrow(nrows) == 0L || nrow(srows) != 1L) {
    stop(inst, " has no single norms record for sample ", sample,
         " (", nrow(nrows), " norm rows, ", nrow(srows), " source rows)",
         call. = FALSE)
  }

  smp <- as.character(sample)
  out <- rbind(
    data.frame(field = "M", sample = smp, scale = nrows[[key]],
               value = as.character(nrows$M), stringsAsFactors = FALSE),
    data.frame(field = "SD", sample = smp, scale = nrows[[key]],
               value = as.character(nrows$SD), stringsAsFactors = FALSE),
    data.frame(field = "Size", sample = smp, scale = NO_SAMPLE,
               value = as.character(srows$Size), stringsAsFactors = FALSE),
    data.frame(field = "Population", sample = smp, scale = NO_SAMPLE,
               value = as.character(srows$Population), stringsAsFactors = FALSE),
    data.frame(field = "Reference", sample = smp, scale = NO_SAMPLE,
               value = as.character(srows$Reference), stringsAsFactors = FALSE),
    data.frame(field = "URL", sample = smp, scale = NO_SAMPLE,
               value = as.character(srows$URL), stringsAsFactors = FALSE)
  )
  if (isTRUE(scales)) {
    out <- rbind(
      out,
      data.frame(field = "Angle", sample = NO_SAMPLE, scale = scl$Abbrev,
                 value = as.character(scl$Angle), stringsAsFactors = FALSE),
      data.frame(field = "Items", sample = NO_SAMPLE, scale = scl$Abbrev,
                 value = normalise_items(scl$Items), stringsAsFactors = FALSE)
    )
  }
  out$instrument <- inst
  out
}

# "1,  9, 17" and "1, 9, 17" are the same assignment; compare on the numbers.
#
# A cell that is not a comma-separated list of integers aborts rather than
# coercing. `as.integer("x")` is NA with a warning, and the paste turned that
# into the STRING "NA" -- so two unparseable cells normalised to "NA" and
# compared equal, which is a comparison that cannot fail. That is the same
# shape this file refuses everywhere else: a value the audit cannot read is a
# note or an object someone got wrong, not a value to agree with.
#
# The refusal composes TWO independent tests, because each admits what the
# other refuses and this milestone shipped each one alone once:
#
#   * shape -- `grepl("^[0-9]+$", p)`. An item number is a plain unsigned digit
#     string. `as.integer()` alone reads a decimal, a scientific literal, a hex
#     literal and a signed integer without ever returning NA, so it REWRITES
#     such a cell into a different key instead of refusing it: "1.5, 9" became
#     "1, 9" and agreed with a shipped "1, 9" (M80 review round 2, G1).
#   * coercion -- `as.integer()`'s own verdict. The shape test alone admits any
#     digit string, and `as.integer("99999999999")` is NA for being out of
#     integer range, so two overflowing cells both normalised to the string
#     "NA" and compared equal (M80 review round 1, F3).
#
# Neither is a stand-in for the other: shape refuses what R can read but this
# audit must not, coercion refuses what R cannot read at all.
#
# `strsplit()` drops a trailing empty field, so the fields are counted against
# the separators rather than read off the split: "1, 9," is a malformed cell
# and not a two-item key (M80 review, F4).
normalise_items <- function(x) {
  vapply(x, function(one) {
    one <- as.character(one)
    p <- trimws(strsplit(one, ",", fixed = TRUE)[[1L]])
    n <- suppressWarnings(as.integer(p))
    bad <- is.na(one) || !length(p) || anyNA(n) || !all(grepl("^[0-9]+$", p)) ||
      length(p) != lengths(regmatches(one, gregexpr(",", one, fixed = TRUE))) + 1L
    if (bad) {
      stop("item key is not a comma-separated list of integers: ", one,
           call. = FALSE)
    }
    paste(n, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

# --- comparison --------------------------------------------------------------

# Angles are compared modulo 360 so the package's LM = 360 matches a source
# printing 0 (DESIGN.md IP2).
values_agree <- function(field, shipped, source, divisor) {
  if (identical(source, NOT_PUBLISHED)) return(NA)
  if (field %in% c("M", "SD")) {
    s <- suppressWarnings(as.numeric(source)) / divisor
    p <- suppressWarnings(as.numeric(shipped))
    if (is.na(s) || is.na(p)) return(FALSE)
    # compare at the precision the source prints, after the documented divisor
    return(isTRUE(all.equal(p, s, tolerance = 1e-8)))
  }
  if (identical(field, "Angle")) {
    s <- suppressWarnings(as.numeric(source)) %% 360
    p <- suppressWarnings(as.numeric(shipped)) %% 360
    if (is.na(s) || is.na(p)) return(FALSE)
    return(isTRUE(all.equal(p, s, tolerance = 1e-8)))
  }
  # The note records the author-year credit the source itself supports for the
  # norm sample; the shipped Reference must credit it, and may carry further
  # context around it (csiv cites the instrument's article alongside the
  # website that publishes the norms). Containment, not equality, is therefore
  # the comparison -- but a shipped string that drops the credit still fails,
  # which is exactly the pre-fix csiv defect.
  if (identical(field, "Reference")) {
    return(grepl(trimws(source), trimws(shipped), fixed = TRUE))
  }
  # Both sides, not one. The shipped side was normalised in shipped_values()
  # and the source side was not, so a note writing an item key with the column
  # padding the package uses ("1,  9, 17, 25") disagreed with the shipped key
  # it was transcribed from -- a mismatch reported against the package for a
  # difference in whitespace.
  if (identical(field, "Items")) {
    return(identical(normalise_items(shipped), normalise_items(source)))
  }
  identical(trimws(shipped), trimws(source))
}

# Every shipped (instrument, sample) pair there is. The batch is a hand-written
# table and nothing has ever bound it to `data/`: measured 2026-08-08, dropping
# `isc` from AUDIT_BATCH lost all 17 of its audited values while the ledger fell
# from 194 rows to 177, the coverage report from 15 to 13, and the gap count
# stayed at 0 with no row anywhere naming the instrument or its note. The
# note-side sweeps below cannot see it: they walk the notes the batch NAMES, so
# an instrument the batch omits is never reached at all.
#
# The instrument enumeration is the package's own (circumplex:::instrument_names),
# not a copy, so the roster cannot drift from `data/`.
#
# shipped_roster() takes NO arguments (M84). It used to accept an `objects`
# list that overrode the enumeration, which is how `roster = shipped_roster(
# objects)` came to be spellable: that call re-fused the roster to the object
# list the caller was ALSO passing as a value override, reproducing the very
# narrowing M79 split the two arguments to end. The derivation lives in
# roster_from_objects() below, which fixtures call by its own name; the fusion
# is no longer a thing anyone can write by accident.
shipped_roster <- function() {
  ns <- asNamespace("circumplex")
  nms <- get("instrument_names", envir = ns)()
  objects <- lapply(nms, function(nm) get(nm, envir = ns))
  names(objects) <- nms
  roster_from_objects(objects)
}

# The roster over an explicit, NAMED list of instrument objects.
#
# Every refusal here is a norms table the builder cannot roster, and each was a
# silent or unnamed failure before M84: the roster is what says which shipped
# (instrument, sample) pairs the audit must cover, so a pair this loop drops is
# a pair nothing will ever report. Measured 2026-08-14 against the pre-M84
# builder, all four:
#
#   - a non-data-frame `Norms[[1]]` raised R's "invalid argument type" from the
#     `!nrow(norms)` guard, naming neither the instrument nor the fault;
#   - a row-carrying table with no `Sample` column raised "arguments imply
#     differing number of rows: 1, 0" from data.frame(), same;
#   - `Sample = c(1, NA)` returned ONE row -- sort() drops NA, so the sample was
#     neither rostered nor reported, the silent shape;
#   - `Sample = c(NA, NA)` raised the differing-number-of-rows message, which is
#     the SECOND shape's message: without a named guard the two are
#     indistinguishable to whoever reads the failure;
#   - an unnamed `objects` list returned a zero-row roster, which covers nothing
#     and reports every unaudited sample as covered.
#
# A zero-row `Norms[[1]]` is NOT one of them and is skipped as it always was:
# an instrument shipping no norms has nothing to audit and is not a gap.
roster_from_objects <- function(objects) {
  nms <- names(objects)
  # `nzchar(NA_character_)` is TRUE, so an NA name clears a bare nzchar() test
  # and then `objects[[NA_character_]]` returns NULL rather than raising -- the
  # loop skips it as a no-norms instrument and the builder returns the very
  # zero-row roster this guard exists to refuse (measured 2026-08-14, M84
  # review F1). `setNames(list(obj), lookup)` with a lookup that missed is how
  # a caller reaches it.
  if (length(objects) && (is.null(nms) || !all(!is.na(nms) & nzchar(nms)))) {
    stop("every entry of `objects` must be named for the instrument it ",
         "carries; an unnamed list rosters nothing at all", call. = FALSE)
  }
  # The loop below walks NAMES and resolves each with `objects[[nm]]`, which
  # returns the first entry carrying that name -- so a repeated name rosters
  # the first entry's samples once per repetition and the second entry's not at
  # all. Measured 2026-08-15: `list(fx = <Sample 1>, fx = <Sample 2>)` returned
  # two rows both reading `fx 1`. `validate_batch()` has refused the same shape
  # for the batch since M72; this closes the sibling asymmetry (M86).
  if (anyDuplicated(nms)) {
    stop("`objects` carries the name ",
         paste(unique(nms[duplicated(nms)]), collapse = ", "),
         " more than once, and only the first entry of a repeated name is ",
         "ever read", call. = FALSE)
  }
  out <- list()
  for (nm in nms) {
    # `objects[[nm]]$Norms[[1]]` used to be reached before anything about the
    # entry was known, so R's own message was the whole report: a non-list
    # entry raised "$ operator is invalid for atomic vectors" and an empty
    # `Norms` list raised "subscript out of bounds", naming neither the
    # instrument nor the fault (both measured 2026-08-15, M86).
    obj <- objects[[nm]]
    if (!is.list(obj)) {
      stop("instrument object for ", nm, " is not a list but a ",
           class(obj)[[1L]], call. = FALSE)
    }
    # `Norms = NULL` is an instrument with nothing to audit, not a malformed
    # one, and it is skipped -- tested BEFORE the emptiness guard below, since
    # NULL and list() are both length 0 and only the second is a defect (M79).
    if (is.null(obj$Norms)) next
    if (!is.list(obj$Norms) || !length(obj$Norms)) {
      stop("`Norms` for ", nm, " must be a non-empty list to hold a norms ",
           "table; it is a ", class(obj$Norms)[[1L]], " of length ",
           length(obj$Norms), call. = FALSE)
    }
    norms <- obj$Norms[[1]]
    # NULL[[1]] is NULL rather than an error in R, which is what lets the
    # no-norms case be tested before the table's shape is (M79).
    if (is.null(norms)) next
    if (!is.data.frame(norms)) {
      stop("norms table for ", nm, " is not a data frame but a ",
           class(norms)[[1L]], call. = FALSE)
    }
    if (!nrow(norms)) next
    if (!"Sample" %in% names(norms)) {
      stop("norms table for ", nm, " has no `Sample` column; it has: ",
           paste(names(norms), collapse = ", "), call. = FALSE)
    }
    if (anyNA(norms$Sample)) {
      stop("norms table for ", nm, " leaves `Sample` missing in ",
           sum(is.na(norms$Sample)), " of ", nrow(norms), " rows, and a ",
           "missing sample is dropped from the roster rather than covered",
           call. = FALSE)
    }
    out[[length(out) + 1L]] <- data.frame(
      instrument = nm, sample = as.character(sort(unique(norms$Sample))),
      stringsAsFactors = FALSE
    )
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), sample = character(0),
               stringsAsFactors = FALSE)
}

# One source can be the published source for more than one instrument, and the
# blocks then have to be tagged: an UNTAGGED block is handed whole to whoever
# asks, so two instruments reading one would each be audited against rows that
# may be the other's. Their rows are indistinguishable inside the block --
# both key on (field, sample, scale), over the same octant names and the same
# sample numbers -- so there is no join that separates them and no comparison
# that could fail. That is the same "audit one instrument against another's
# values and could not fail" case parse_source_note() already refuses when a
# multi-block note is asked for an instrument none of its blocks names; this
# closes it one shape over, where the block carries no tag at all.
#
# Refused up front rather than repaired downstream: the alternative was to key
# each pass's claims per instrument, which leaves the mis-comparison in place
# and only makes the coverage counts tidy about it.
refuse_shared_untagged_blocks <- function(batch, dir) {
  for (citekey in unique(batch$citekey)) {
    insts <- unique(batch$instrument[batch$citekey == citekey])
    if (length(insts) < 2L) next
    tags <- source_note_block_tags(citekey, dir)
    if (any(!nzchar(tags))) {
      stop("source note ", citekey, " carries an untagged audit-values block ",
           "but is read by ", length(insts), " instruments (",
           paste(sort(insts), collapse = ", "),
           "); tag each block with the instrument it backs", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# --- the coverage report ------------------------------------------------------

# The coverage report says what the audit did NOT compare -- one row per thing
# left out -- and it is read by machine: the test files assert over its columns,
# and a reader relates it to the ledger through the names the ledger uses. So
# every cell holds one fact, and no cell holds a sentence assembled out of two.
# Before M80 the emitters below put their payload wherever a column was free:
# five pasted two facts into one cell (`field` became "M (sample 1)",
# `instrument` became "horowitz2003 (iip32)") and the other two pasted nothing
# but filled a mis-named column, a note-only row putting its scale in `field`
# and its value in `scale`. Both shapes left the report unjoinable and, worse,
# unreadable by its own column names: `instrument` did not hold an instrument.
#
# Ten columns. Seven are the KEY -- instrument, citekey, tag, side, field,
# sample, scale -- and each is NA on a row that has no such fact, rather than
# carrying a stand-in for one. `label` and `detail` are free text and are never
# keys. `exempt` marks a row that records a fact about the sources rather than
# a gap in the audit; the run block's gap count is `sum(!exempt)`.
#
# Coverage rows and ledger rows are disjoint by construction -- a coverage row
# exists precisely because there was nothing to compare -- so what the key
# provides is joinABILITY (to the batch, to the notes, to `data/`), not a join.
#
# Which cells each side fills:
#
#   note-only-sample (exempt)     instrument citekey tag sample; label = the
#     note's own name for material the source publishes and the package does
#     not ship, detail = the note's description of it. One row per block and
#     payload, not per batch pass over the block. `sample` is the note row's
#     own -- the NO_SAMPLE token on every committed row, a sample label where
#     the note names one -- because the dedupe key at the emitter distinguishes
#     two such rows by it and a report that did not carry it would emit them
#     identically (M85).
#   constructed-credit-reference (exempt)  instrument citekey tag field sample;
#     label = the credit the note's author constructed rather than quoted.
#   shipped-value-not-in-note     instrument citekey tag field sample scale
#   note-value-not-shipped        instrument citekey tag field sample scale
#   note-instrument-row-not-audited  instrument citekey tag field sample scale;
#     sample is the NO_SAMPLE token, these rows being instrument-level.
#   note-sample-not-audited       instrument citekey tag sample; sample is the
#     one the note tables and no batch row claimed.
#   note-block-not-audited        citekey tag; instrument is NA because no batch
#     row names one -- that absence is exactly why the block is a gap.
#   shipped-sample-not-audited    instrument sample; citekey and tag are NA for
#     the mirror-image reason, no batch row names a note.
#
# `tag` is NA for an untagged block rather than "": an empty string reads as a
# tag whose text went missing, not as a block that carries none.
COVERAGE_COLUMNS <- c("instrument", "citekey", "tag", "side", "field",
                      "sample", "scale", "label", "detail", "exempt")

# Every emitter goes through this, so a side cannot invent a column order or
# omit a column, and an unfilled cell is NA by default rather than by each
# emitter remembering to say so.
coverage_rows <- function(side, exempt,
                          instrument = NA_character_, citekey = NA_character_,
                          tag = NA_character_, field = NA_character_,
                          sample = NA_character_, scale = NA_character_,
                          label = NA_character_, detail = NA_character_) {
  data.frame(instrument = as.character(instrument),
             citekey = as.character(citekey),
             tag = as.character(tag),
             side = as.character(side),
             field = as.character(field),
             sample = as.character(sample),
             scale = as.character(scale),
             label = as.character(label),
             detail = as.character(detail),
             exempt = exempt,
             stringsAsFactors = FALSE)
}

empty_coverage <- function() {
  data.frame(instrument = character(0), citekey = character(0),
             tag = character(0), side = character(0), field = character(0),
             sample = character(0), scale = character(0),
             label = character(0), detail = character(0), exempt = logical(0),
             stringsAsFactors = FALSE)
}

# "" is how the parser spells "this block carries no tag"; the report spells it
# NA, so that `tag` is either a tag or nothing.
tag_or_na <- function(tag) if (nzchar(tag)) tag else NA_character_

# The same rule over a vector of note-row cells. parse_source_note() validates
# the VALUE cell for emptiness and not the sample cell, so a blank sample
# reaches an emitter as "" -- and an empty string in one of the seven KEY
# columns reads as a label whose text went missing, not as a row that carries
# no such fact, which is what the schema above declares NA to mean.
blank_to_na <- function(x) {
  x[!nzchar(x)] <- NA_character_
  x
}

# `roster` is the world the batch is audited against, and it is deliberately
# NOT derived from `objects`. The two answer different questions: `objects`
# overrides one instrument's VALUES, `roster` states which shipped
# (instrument, sample) pairs must be covered. Deriving one from the other made
# a value override silently shrink the world -- auditing a one-instrument slice
# of AUDIT_BATCH with that instrument's real object injected reported 0 gaps
# where the same slice reports 23 without it, which is exactly the clean run
# over unread data this milestone exists to prevent (M79 return-2 review).
# A fixture pass states its own roster; the default is the full shipped sweep.
audit_norms <- function(batch = AUDIT_BATCH,
                        dir = file.path("cairn", "references"),
                        objects = NULL,
                        roster = NULL) {
  if (is.null(roster)) roster <- shipped_roster()
  # After validate_batch(), not before: a caller who got both arguments wrong
  # should meet the batch's message, which names the column it is missing.
  validate_batch(batch)
  validate_roster(roster)
  refuse_shared_untagged_blocks(batch, dir)
  ledger <- list()
  coverage <- list()
  # Which of each note BLOCK's sample labels some batch entry actually claimed.
  # A note tabling three samples while the batch names two would otherwise drop
  # the third with no row anywhere -- the same silent-loss shape the malformed
  # -row abort above refuses. Keyed per block, not per citekey: a two-block note
  # keyed by citekey alone would let one instrument's claims cover the other
  # instrument's unaudited samples, which is the whole failure this sweep exists
  # to catch.
  claimed <- list()
  blocks <- list()
  # Which of each block's note-only rows have already been reported; see the
  # emitter below for why the key is the note row rather than the report row.
  note_only_seen <- list()

  for (i in seq_len(nrow(batch))) {
    inst <- batch$instrument[[i]]
    smp <- batch$sample[[i]]
    citekey <- batch$citekey[[i]]
    divisor <- batch$divisor[[i]]
    ship <- shipped_values(inst, smp, batch$scales[[i]], obj = objects[[inst]])

    note_all <- parse_source_note(citekey, dir, inst)
    note_only <- note_all[note_all$field == NOTE_ONLY, , drop = FALSE]
    note_real <- note_all[note_all$field != NOTE_ONLY, , drop = FALSE]
    btag <- attr(note_all, "tag")
    bkey <- paste0(citekey, "\r", btag)
    # `scales` accumulates by OR: whether a block's instrument-level rows were
    # ever read is a property of the BLOCK across every pass over it, so the
    # last pass must not overwrite an earlier pass's TRUE (AC6).
    blocks[[bkey]] <- list(
      citekey = citekey, instrument = inst, tag = btag,
      scales = isTRUE(blocks[[bkey]]$scales) || isTRUE(batch$scales[[i]])
    )
    claimed[[bkey]] <- union(
      claimed[[bkey]] %||% character(0), as.character(smp)
    )
    # Restrict to the sample this pass audits, plus the instrument-level rows.
    # Other samples' rows belong to their own pass, not to this one's coverage.
    note <- note_real[note_real$sample %in% c(as.character(smp), NO_SAMPLE),
                      , drop = FALSE]
    if (!isTRUE(batch$scales[[i]])) {
      note <- note[note$sample != NO_SAMPLE, , drop = FALSE]
    }

    # A note-only row is a fact about the BLOCK, so it is emitted once per
    # block and payload rather than once per batch pass that reads the block:
    # iipsc's two passes read two different notes today, but an instrument
    # whose samples share one note would have reported each of its note-only
    # rows twice.
    #
    # The payload is part of the key, not decoration, and the payload is the
    # NOTE ROW -- sample, scale, value and anchor -- not the two cells the
    # report happens to carry. Keying on (citekey, block, sample) alone would
    # collapse the 14 rows the repo emits to the 9 blocks that carry them,
    # every note-only row carrying the NO_SAMPLE token; keying without the
    # anchor collapses two rows citing one sample to two different tables into
    # one; keying without the sample collapses an instrument-level note and a
    # per-sample note that happen to read alike. All three are the silent row
    # loss every other sweep in this file refuses. `sample` earns its place on
    # the same ground as `anchor` and not on the committed run, where every
    # note-only row carries the NO_SAMPLE token and it changes nothing.
    #
    # Keyed here rather than by deduplicating the assembled frame: the anchor
    # is the audit's provenance cell and the report does not carry it, so the
    # key exists only at this point. Filtering here also cannot reach across
    # sides, which a `duplicated()` over the whole frame could.
    if (nrow(note_only)) {
      key <- paste(note_only$sample, note_only$scale, note_only$value,
                   note_only$anchor, sep = "\r")
      # Within this pass's rows as well as against earlier passes. Filtering
      # only against earlier passes leaves the key inert for a block read once
      # -- every row of a single pass is unseen whatever it says -- so the
      # anchor could be dropped from the key with no test able to tell (caught
      # by mutating this line during the M80 return-1 fixes).
      fresh <- !duplicated(key) &
        !(key %in% (note_only_seen[[bkey]] %||% character(0)))
      note_only_seen[[bkey]] <- c(note_only_seen[[bkey]] %||% character(0),
                                  key[fresh])
      if (any(fresh)) {
        coverage[[length(coverage) + 1L]] <- coverage_rows(
          "note-only-sample", TRUE, instrument = inst, citekey = citekey,
          tag = tag_or_na(btag), sample = blank_to_na(note_only$sample[fresh]),
          label = note_only$scale[fresh], detail = note_only$value[fresh]
        )
      }
    }

    constructed <- note[note$field == "Reference" &
                          grepl(CONSTRUCTED_CREDIT, note$anchor, fixed = TRUE),
                        , drop = FALSE]
    if (nrow(constructed)) {
      coverage[[length(coverage) + 1L]] <- coverage_rows(
        "constructed-credit-reference", TRUE, instrument = inst,
        citekey = citekey, tag = tag_or_na(btag), field = "Reference",
        sample = constructed$sample, label = constructed$value
      )
    }

    # every shipped value must have a source-side entry, and vice versa
    ship_key <- paste(ship$field, ship$sample, ship$scale)
    note_key <- paste(note$field, note$sample, note$scale)
    miss_source <- ship[!(ship_key %in% note_key), , drop = FALSE]
    miss_ship <- note[!(note_key %in% ship_key), , drop = FALSE]
    if (nrow(miss_source)) {
      coverage[[length(coverage) + 1L]] <- coverage_rows(
        "shipped-value-not-in-note", FALSE, instrument = inst,
        citekey = citekey, tag = tag_or_na(btag), field = miss_source$field,
        sample = miss_source$sample, scale = miss_source$scale
      )
    }
    if (nrow(miss_ship)) {
      coverage[[length(coverage) + 1L]] <- coverage_rows(
        "note-value-not-shipped", FALSE, instrument = inst,
        citekey = citekey, tag = tag_or_na(btag), field = miss_ship$field,
        sample = miss_ship$sample, scale = miss_ship$scale
      )
    }

    for (k in seq_len(nrow(ship))) {
      j <- match(ship_key[[k]], note_key)
      if (is.na(j)) next
      agree <- values_agree(ship$field[[k]], ship$value[[k]],
                            note$value[[j]], divisor)
      if (isTRUE(agree)) next
      ledger[[length(ledger) + 1L]] <- data.frame(
        instrument = inst,
        field = ship$field[[k]],
        sample = ship$sample[[k]],
        scale = ship$scale[[k]],
        shipped = ship$value[[k]],
        source = note$value[[j]],
        anchor = note$anchor[[j]],
        kind = if (is.na(agree)) "not-published" else "mismatch",
        stringsAsFactors = FALSE
      )
    }
  }

  for (bkey in names(claimed)) {
    blk <- blocks[[bkey]]
    tabled <- parse_source_note(blk$citekey, dir, blk$instrument)
    tabled <- tabled[tabled$field != NOTE_ONLY, , drop = FALSE]
    unclaimed <- setdiff(unique(tabled$sample),
                         c(claimed[[bkey]], NO_SAMPLE))
    if (length(unclaimed)) {
      coverage[[length(coverage) + 1L]] <- coverage_rows(
        "note-sample-not-audited", FALSE, instrument = blk$instrument,
        citekey = blk$citekey, tag = tag_or_na(blk$tag), sample = unclaimed
      )
    }

    # The instrument-level rows -- Angle and Items, keyed with NO_SAMPLE -- are
    # read by the ONE pass per instrument that carries `scales = TRUE`, and
    # dropped by every other pass (see the restriction above). When that pass
    # reads a DIFFERENT block from this one, nothing ever compares this block's
    # instrument-level rows and nothing reports them: they are dropped in
    # silence, the same shape as the sweeps above one level in. iipsc is the
    # live near-miss -- its `scales` pass is sample 2 (soldz1995), so
    # hopwood2008's block would lose any instrument-level row it carried -- and
    # no committed note has one today, which is why this needs a fixture.
    if (!isTRUE(blk$scales)) {
      inst_rows <- tabled[tabled$sample == NO_SAMPLE, , drop = FALSE]
      if (nrow(inst_rows)) {
        coverage[[length(coverage) + 1L]] <- coverage_rows(
          "note-instrument-row-not-audited", FALSE,
          instrument = blk$instrument, citekey = blk$citekey,
          tag = tag_or_na(blk$tag), field = inst_rows$field,
          sample = inst_rows$sample, scale = inst_rows$scale
        )
      }
    }
  }

  # ...and the block level. The sweep above walks blocks the batch CLAIMED, so
  # it can only ever report a sample missing from a block someone audited. A
  # whole block no batch row selects is never parsed, never keyed, and never
  # counted -- measured at the M75 review: dropping the three iip32 rows from
  # AUDIT_BATCH made all 48 of that instrument's tabled values vanish with the
  # ledger, the coverage report and every printed count reading clean. That is
  # the same silent-loss shape the malformed-row and missing-sample aborts
  # above refuse, one level up, so it is refused here too.
  for (citekey in unique(batch$citekey)) {
    tags <- source_note_block_tags(citekey, dir)
    seen <- vapply(names(claimed), function(k) blocks[[k]]$tag, character(1),
                   USE.NAMES = FALSE)
    unaudited <- setdiff(tags, seen[
      vapply(names(claimed), function(k) blocks[[k]]$citekey, character(1),
             USE.NAMES = FALSE) == citekey
    ])
    if (length(unaudited)) {
      coverage[[length(coverage) + 1L]] <- coverage_rows(
        "note-block-not-audited", FALSE, citekey = citekey,
        tag = vapply(unaudited, tag_or_na, character(1), USE.NAMES = FALSE)
      )
    }
  }

  # ...and the shipped side, the one direction neither sweep above covers.
  # Both of those walk the notes the batch names, so they can only ever report
  # something missing from a note SOMEONE audited; a shipped sample the batch
  # never mentions is not reached by either. Reported rather than refused, as
  # its two siblings are: an abort would stop the audit exactly when a new
  # instrument lands before its source note does, which is when it is wanted.
  unaudited <- roster
  unaudited <- unaudited[
    !(paste(unaudited$instrument, unaudited$sample) %in%
        paste(batch$instrument, batch$sample)), , drop = FALSE
  ]
  if (nrow(unaudited)) {
    coverage[[length(coverage) + 1L]] <- coverage_rows(
      "shipped-sample-not-audited", FALSE,
      instrument = unaudited$instrument, sample = unaudited$sample
    )
  }

  cov <- if (length(coverage)) do.call(rbind, coverage) else empty_coverage()
  row.names(cov) <- NULL

  list(
    ledger = if (length(ledger)) do.call(rbind, ledger) else empty_ledger(),
    coverage = cov
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

empty_ledger <- function() {
  data.frame(instrument = character(0), field = character(0),
             sample = character(0), scale = character(0),
             shipped = character(0), source = character(0),
             anchor = character(0), kind = character(0),
             stringsAsFactors = FALSE)
}

# --- cross-check the two shipped angle copies --------------------------------

# Scales$Angle and Norms[[1]]$Angle are two independent shipped copies of the
# same fact; nothing else in the package makes them agree.
angle_copies_agree <- function(batch = AUDIT_BATCH, objects = NULL) {
  out <- list()
  add <- function(inst, scale, na, sa, why) {
    out[[length(out) + 1L]] <<- data.frame(
      instrument = inst, scale = scale, norms_angle = na, scales_angle = sa,
      problem = why, stringsAsFactors = FALSE
    )
  }
  # Per instrument, not per batch row: both shipped angle copies are
  # instrument-level, so a multi-sample instrument would otherwise be reported
  # once per sample and inflate the split count.
  for (inst in unique(batch$instrument)) {
    obj <- objects[[inst]] %||% get(inst, envir = asNamespace("circumplex"))
    norms <- obj$Norms[[1]]
    scales <- obj$Scales
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    j <- match(norms[[key]], scales$Abbrev)

    # An unmatched scale name is a split, not a row to skip: which(NA) drops
    # the row, so the unjoined case has to be caught before the comparison.
    unjoined <- which(is.na(j))
    if (length(unjoined)) {
      add(inst, norms[[key]][unjoined], norms$Angle[unjoined], NA_real_,
          "scale name not found in Scales$Abbrev")
    }
    # An NA in either copy compares FALSE against everything and would
    # otherwise be invisible to the != test below.
    nas <- which(is.na(norms$Angle) | (!is.na(j) & is.na(scales$Angle[j])))
    if (length(nas)) {
      add(inst, norms[[key]][nas], norms$Angle[nas], scales$Angle[j][nas],
          "NA angle in a shipped copy")
    }
    ok <- !is.na(j) & !is.na(norms$Angle) & !is.na(scales$Angle[j])
    bad <- which(ok & (norms$Angle %% 360) != (scales$Angle[j] %% 360))
    if (length(bad)) {
      add(inst, norms[[key]][bad], norms$Angle[bad], scales$Angle[j][bad],
          "copies disagree modulo 360")
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), scale = character(0),
               norms_angle = numeric(0), scales_angle = numeric(0),
               problem = character(0), stringsAsFactors = FALSE)
}

# --- the IP2 convention, which the modulo comparison cannot see --------------

# Angles are degrees in [0, 360) in the user API with LM = 360, never 0
# (DESIGN.md IP2). The audit compares against the source modulo 360, so a
# shipped 0 where the convention wants 360 agrees with the source and passes
# silently. This check is on the shipped side only, where the convention lives.
ip2_convention_holds <- function(batch = AUDIT_BATCH, objects = NULL) {
  out <- list()
  for (inst in unique(batch$instrument)) {
    obj <- objects[[inst]] %||% get(inst, envir = asNamespace("circumplex"))
    norms <- obj$Norms[[1]]
    key <- if ("Scale" %in% names(norms)) "Scale" else "Abbrev"
    copies <- list(
      list(where = "Norms[[1]]$Angle", scale = as.character(norms[[key]]),
           angle = norms$Angle),
      list(where = "Scales$Angle", scale = as.character(obj$Scales$Abbrev),
           angle = obj$Scales$Angle)
    )
    for (cp in copies) {
      bad <- which(is.na(cp$angle) | cp$angle <= 0 | cp$angle > 360)
      if (length(bad)) {
        out[[length(out) + 1L]] <- data.frame(
          instrument = inst, copy = cp$where, scale = cp$scale[bad],
          angle = cp$angle[bad], stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out)) do.call(rbind, out) else
    data.frame(instrument = character(0), copy = character(0),
               scale = character(0), angle = numeric(0),
               stringsAsFactors = FALSE)
}

# --- run ---------------------------------------------------------------------

# Two stamps, not one. A run's own HEAD is necessarily the PARENT of the
# commit that lands the ledger, so a single "commit" column can never name the
# commit containing the file. Recording the script and the package data
# separately also lets a pre-fix snapshot be rebuilt honestly: today's script
# against an earlier tree's data/, each named.
#
# Every column is filled with `rep(x, nrow(ledger))` rather than the scalar x.
# `df$col <- x` recycles a length-1 value to any POSITIVE number of rows and
# errors on zero ("replacement has 1 row, data has 0"), so the scalar form
# worked on every ledger M72 happened to produce and would have crashed the run
# on the first fully-clean one -- the run this audit exists to reach.
stamp_ledger <- function(ledger,
                         generated = format(Sys.Date()),
                         script_commit = NA_character_,
                         data_commit = NA_character_) {
  n <- nrow(ledger)
  ledger$generated <- rep(as.character(generated), n)
  ledger$script_commit <- rep(as.character(script_commit), n)
  ledger$data_commit <- rep(as.character(data_commit), n)
  ledger
}

if (!isTRUE(getOption("norms_audit_defs_only", FALSE))) {
  LEDGER_PATH <- Sys.getenv("NORMS_AUDIT_LEDGER",
                            "data-raw/norms-audit-ledger.csv")
  COVERAGE_PATH <- Sys.getenv("NORMS_AUDIT_COVERAGE",
                              "data-raw/norms-audit-coverage.csv")
  res <- audit_norms()
  angle_check <- angle_copies_agree()
  ip2_check <- ip2_convention_holds()

  disp_path <- "data-raw/norms-audit-dispositions.csv"
  ledger <- res$ledger
  if (file.exists(disp_path) && nrow(ledger)) {
    disp <- utils::read.csv(disp_path, stringsAsFactors = FALSE)
    k <- paste(ledger$instrument, ledger$field, ledger$sample, ledger$scale)
    dk <- paste(disp$instrument, disp$field, disp$sample, disp$scale)
    ledger$disposition <- disp$disposition[match(k, dk)]
    ledger$disposition[is.na(ledger$disposition)] <- "UNDISPOSITIONED"
  } else if (nrow(ledger)) {
    ledger$disposition <- "UNDISPOSITIONED"
  } else {
    ledger$disposition <- character(0)
  }

  git_head <- function() {
    tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
             error = function(e) NA_character_)
  }
  or_head <- function(v) if (nzchar(v)) v else git_head()

  ledger <- stamp_ledger(
    ledger,
    script_commit = or_head(Sys.getenv("NORMS_AUDIT_SCRIPT_COMMIT")),
    data_commit = or_head(Sys.getenv("NORMS_AUDIT_DATA_COMMIT"))
  )

  utils::write.csv(ledger, LEDGER_PATH, row.names = FALSE)
  utils::write.csv(res$coverage, COVERAGE_PATH, row.names = FALSE)

  gaps <- res$coverage[!res$coverage$exempt, , drop = FALSE]
  cat("norms audit\n")
  cat("  ledger rows:      ", nrow(ledger), " -> ", LEDGER_PATH, "\n", sep = "")
  cat("  coverage rows:    ", nrow(res$coverage), " -> ", COVERAGE_PATH, "\n",
      sep = "")
  cat("  coverage gaps:    ", nrow(gaps), "\n", sep = "")
  # Counted by side, not by `exempt`: constructed-credit rows are exempt too,
  # so one `sum(exempt)` counter would report them as note-only rows -- hiding
  # the exact quantity the constructed-credit label exists to make visible.
  cat("  note-only rows:   ",
      sum(res$coverage$side == "note-only-sample"), "\n", sep = "")
  cat("  constructed creds:",
      sum(res$coverage$side == "constructed-credit-reference"), "\n", sep = "")
  cat("  angle-copy splits:", nrow(angle_check), "\n", sep = "")
  cat("  IP2 breaches:     ", nrow(ip2_check), "\n", sep = "")
  if (nrow(res$coverage)) print(res$coverage)
  if (nrow(angle_check)) print(angle_check)
  if (nrow(ip2_check)) print(ip2_check)
  if (nrow(ledger)) {
    print(ledger[, c("instrument", "field", "sample", "scale", "kind",
                     "disposition")])
  }
}
