# The abort-site manifest for `data-raw/audit-norms.R` (M87).
#
# One row per `stop()` call and per `stopifnot()` CONDITION, keyed
# (kind, binding, key, ordinal) exactly as `norms_audit_abort_sites()` collects
# them. `test-norms-audit-manifest.R` asserts set equality between this table
# and a fresh walk of the script, so a guard added to the script with no entry
# here reddens the suite -- the one property the retired abort-site registry
# existed for (M81-M83), kept at a fraction of its mass.
#
# GENERATED, never hand-edited: re-derive with a walk of the script rather than
# typing a row, so the table cannot drift from what the script actually raises.
# A `{}` stands for an interpolated argument and matches anything, so it counts
# as no discrimination and never toward the literal floor.

NORMS_AUDIT_MANIFEST <- data.frame(
  kind = c(
    "stopifnot",
    "stopifnot",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stopifnot",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop",
    "stop"
  ),
  binding = c(
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_batch",
    "validate_roster",
    "validate_roster",
    "validate_roster",
    "validate_roster",
    "validate_roster",
    "validate_roster",
    "source_note_marker",
    "source_note_block_tags",
    "parse_source_note",
    "parse_source_note",
    "parse_source_note",
    "parse_source_note",
    "parse_source_note",
    "parse_source_note",
    "shipped_values",
    "normalise_items",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "roster_from_objects",
    "refuse_shared_untagged_blocks"
  ),
  ordinal = c(
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L
  ),
  key = c(
    "is.data.frame(batch)",
    "all(c(\"instrument\", \"sample\", \"citekey\", \"divisor\", \"scales\") %in% names(batch))",
    "AUDIT_BATCH names the same (instrument, sample) twice: {}",
    "AUDIT_BATCH$divisor must be numeric, not {}",
    "AUDIT_BATCH$divisor is missing for: {}",
    "AUDIT_BATCH$divisor is not finite for: {}",
    "AUDIT_BATCH$divisor must be strictly positive; wrong for: {}",
    "AUDIT_BATCH must mark exactly one `scales` entry per instrument; wrong for: {}",
    "is.data.frame(roster)",
    "`roster` has no `instrument` column; it has: {}",
    "`roster` has no `sample` column; it has: {}",
    "`roster` names no (instrument, sample) pair to cover, so every unaudited shipped sample would be reported as covered",
    "`roster` cannot be checked for completeness: the shipped roster could not be built, so the fault is not in the roster passed here -- {}",
    "`roster` omits {} shipped (instrument, sample) pair(s), which would be reported as covered; pass `fixture_world = TRUE` if this roster is not about `data/`: {}",
    "malformed audit-values marker: {}",
    "source note not found: {}",
    "source note not found: {}",
    "source note {} has no well-formed audit-values block(s)",
    "source note {} tags two audit-values blocks alike: {}",
    "source note {} has no audit-values block for {}; it tags: {}",
    "source note {} has {} malformed audit row(s); first: {}",
    "source note {} has {} audit row(s) with an empty value; first: {}",
    "{} has no single norms record for sample {} ({} norm rows, {} source rows)",
    "item key is not a comma-separated list of integers: {}",
    "every entry of `objects` must be named for the instrument it carries; an unnamed list rosters nothing at all",
    "`objects` carries the name {} more than once, and only the first entry of a repeated name is ever read",
    "`objects` names {} but carries NULL for it, so the instrument would be rostered with no samples at all",
    "instrument object for {} is not a list but a {}",
    "`Norms` for {} must be a non-empty list to hold a norms table; it is a {} of length {}",
    "norms table for {} is not a data frame but a {}",
    "norms table for {} has no `Sample` column; it has: {}",
    "norms table for {} leaves `Sample` missing in {} of {} rows, and a missing sample is dropped from the roster rather than covered",
    "source note {} carries an untagged audit-values block but is read by {} instruments ({}); tag each block with the instrument it backs"
  ),
  stringsAsFactors = FALSE
)

# The one key this script raises from two bindings, so the only key a manifest
# lookup cannot resolve to a single site: `source note not found: {}` at
# data-raw/audit-norms.R:321 (source_note_block_tags) and :352
# (parse_source_note). A site assertion on it must declare which binding it
# expects; every other key selects exactly one site on its own.
NORMS_AUDIT_AMBIGUOUS_KEYS <- "source note not found: {}"

# Does `msg` match `key` under `kind`? The three kinds fail differently, so they
# match differently, and dispatching on a kind read from the manifest is what
# keeps a `stopifnot` message from being judged by the loosest branch.
#
# `stop`: the key's literal fragments in order, `{}` standing for an
# interpolated argument. `stopifnot_named`: the name IS the message, so
# equality. Positional `stopifnot`: R deparses the condition into the message,
# so the key is a prefix -- floored at min(nchar(key), 40) where R did not
# truncate, and unfloored where it did, the line break being R's choice and not
# the guard's (M83).
audit_key_matches <- function(kind, key, msg) {
  if (identical(kind, "stop")) {
    return(grepl(norms_audit_key_regex(key), msg))
  }
  if (identical(kind, "stopifnot_named")) {
    return(identical(msg, key))
  }
  # Fail closed on an unrecognised kind rather than falling through. The
  # positional-`stopifnot` branch below is the loosest of the three, so a kind
  # this dispatch does not know would silently get the weakest check -- which
  # is not hypothetical: a stale dispatch did exactly that during M81 and
  # accepted a key's own superstring. The retired matcher refused by name here
  # and this keeps that refusal.
  if (!identical(kind, "stopifnot")) {
    stop("unknown abort site kind: ", paste(deparse(kind), collapse = ""),
         " (expected one of stop, stopifnot, stopifnot_named)", call. = FALSE)
  }
  got <- norms_audit_stopifnot_stem(msg)
  floor <- min(nchar(squish(key)), NORMS_AUDIT_STEM_FLOOR)
  nzchar(got$stem) && startsWith(squish(key), got$stem) &&
    (got$truncated || nchar(got$stem) >= floor)
}

# Assert that `expr` aborts AT the manifest site named by `key` -- never that
# some error occurred.
#
# Three conditions, and the third is the one a per-test regexp cannot express.
# A regexp is checked only against the message its own fixture raised, so
# nothing stops it also accepting a sibling site's message: six sites of this
# script open "source note ", and six tests asserting that fragment would all
# pass while discriminating nothing. Requiring the raised message to be matched
# by exactly ONE manifest key folds the retired acceptance matrix's cross-site
# property into the per-call check.
#
# The key is looked up rather than trusted: a key absent from the manifest is a
# stale call site, and it fails by name here instead of quietly matching
# nothing.
expect_audit_abort <- function(expr, key, binding = NULL) {
  rows <- NORMS_AUDIT_MANIFEST[NORMS_AUDIT_MANIFEST$key == key, , drop = FALSE]
  if (!nrow(rows)) {
    stop("`key` names no manifest site: ", key, call. = FALSE)
  }
  if (nrow(rows) > 1L) {
    if (is.null(binding)) {
      stop("`key` resolves to ", nrow(rows), " sites (", key,
           "); name the one you expect with `binding`", call. = FALSE)
    }
    rows <- rows[rows$binding == binding, , drop = FALSE]
    if (nrow(rows) != 1L) {
      stop("`binding` ", binding, " names ", nrow(rows), " sites for: ", key,
           call. = FALSE)
    }
  }

  norms_audit_with_c_messages({
    err <- tryCatch({
      expr
      NULL
    }, error = identity)
    expect_true(inherits(err, "error"), info = paste("no error raised:", key))
    if (!inherits(err, "error")) return(invisible(NULL))
    msg <- conditionMessage(err)

    expect_true(audit_key_matches(rows$kind[[1L]], key, msg),
                info = paste0(key, " -- got: ", msg))

    distinct <- unique(NORMS_AUDIT_MANIFEST[c("kind", "key")])
    hits <- vapply(seq_len(nrow(distinct)), function(i) {
      audit_key_matches(distinct$kind[[i]], distinct$key[[i]], msg)
    }, logical(1))
    expect_true(sum(hits) == 1L,
                info = paste0(key, " -- message matched ", sum(hits),
                              " manifest keys: ",
                              paste(distinct$key[hits], collapse = " | ")))
  })
  invisible(NULL)
}

