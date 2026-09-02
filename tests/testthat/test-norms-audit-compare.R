# The norms audit's item-key comparison normalises both sides (M80).
#
# `Items` is the shipped item-to-octant key, and the package writes it with
# column padding ("1,  9, 17, 25") that carries no meaning. shipped_values()
# normalised the shipped side through normalise_items() and values_agree() then
# compared it against the note's raw cell, so a note transcribing the key
# faithfully -- padding and all -- disagreed with the package, and the ledger
# reported a mismatch about whitespace.
#
# The other half is the coercion: as.integer("x") is NA with a warning and the
# paste turned that into the STRING "NA", so any two unparseable cells
# normalised alike and compared EQUAL. Normalising both sides without also
# refusing an unparseable cell would have made that reachable from the source
# side too, which is why the abort and the second normalisation ship together.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed, so these skip against the
# installed package, as the sibling audit test files do.

compare_defs <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  env <- new.env()
  old <- options(norms_audit_defs_only = TRUE)
  on.exit(options(old), add = TRUE)
  sys.source(script, env)
  env
}

test_that("an item key compares equal across padding differences (M80)", {
  env <- compare_defs()
  # The shipped spelling against the note's, with the padding on either side
  # and on neither. All three are the same assignment.
  expect_true(env$values_agree("Items", "1,  9, 17, 25", "1, 9, 17, 25", 1))
  expect_true(env$values_agree("Items", "1, 9, 17, 25", "1,  9, 17, 25", 1))
  expect_true(env$values_agree("Items", " 1, 9,17 , 25", "1, 9, 17, 25", 1))
  # And a genuinely different key still disagrees -- the normalisation must not
  # have made every key equal to every other.
  expect_false(env$values_agree("Items", "1, 9, 17, 25", "1, 9, 17, 26", 1))
  expect_false(env$values_agree("Items", "1, 9, 17", "1, 9, 17, 25", 1))
})

test_that("two unparseable item cells do not compare equal (M80)", {
  skip_on_cran()
  env <- compare_defs()
  # The defect the abort exists for. Without it both sides normalise to the
  # string "NA" and this comparison returns TRUE: two cells nobody can read,
  # reported as agreeing. Assert the abort by its own message, since a
  # comparison of two junk strings would also raise if the audit were broken in
  # an unrelated way.
  expect_audit_abort(env$values_agree("Items", "x", "y", 1),
    "item key is not a comma-separated list of integers: {}")
  expect_audit_abort(env$normalise_items(NA_character_),
    "item key is not a comma-separated list of integers: {}")
  expect_audit_abort(env$normalise_items("1, 9, seventeen"),
    "item key is not a comma-separated list of integers: {}")
  # The shape that survived the first fix: a digit string is not an integer if
  # it is out of R's integer range, and `as.integer()` returns NA for it with a
  # warning. Two such cells both normalised to the string "NA" and compared
  # EQUAL -- the same defect one shape over, inside the guard that claimed to
  # close it (M80 review, F3). Asserted as a pair, since a single abort proves
  # only that one cell is refused.
  expect_audit_abort(env$normalise_items("99999999999"),
    "item key is not a comma-separated list of integers: {}")
  expect_audit_abort(env$values_agree("Items", "99999999999", "88888888888", 1),
    "item key is not a comma-separated list of integers: {}")
  # `strsplit()` drops a trailing empty field, so "1, 9," parsed as a two-item
  # key while ",1" aborted -- one malformed shape refused and its mirror image
  # normalised away (M80 review, F4).
  expect_audit_abort(env$normalise_items("1, 9,"),
    "item key is not a comma-separated list of integers: {}")
  expect_audit_abort(env$normalise_items("1,,9"),
    "item key is not a comma-separated list of integers: {}")
  # An empty cell is unreadable too. It cannot arrive from a note --
  # parse_source_note() refuses an empty value cell before the comparison sees
  # it -- so this fences the shipped side, which has no such guard of its own.
  expect_audit_abort(env$normalise_items(""),
    "item key is not a comma-separated list of integers: {}")
  # The shape the SECOND fix opened by dropping the shape test rather than
  # composing with it: `as.integer()` reads a decimal, a scientific literal, a
  # hex literal and a signed integer without ever returning NA, so each was
  # silently rewritten into a different key -- "1.5, 9" normalised to "1, 9"
  # and agreed with a shipped "1, 9" (M80 review round 2, G1). An item number
  # is a plain unsigned digit string; none of these is one.
  for (cell in c("1.5, 9", "0x10, 9", "1e2, 9", "+1, 9", "-9, 1")) {
    expect_audit_abort(env$normalise_items(cell),
      "item key is not a comma-separated list of integers: {}")
  }
  # The comparison that made it a defect rather than a cosmetic rewrite: a note
  # transcribing an item as 1.4 agreed with a shipped 1.
  expect_audit_abort(env$values_agree("Items", "1, 9", "1.4, 9", 1),
    "item key is not a comma-separated list of integers: {}")
  # The control: a real key still normalises, so the guard has not simply
  # refused everything.
  expect_identical(env$normalise_items(c("1,  9, 17", "2, 10, 18")),
                   c("1, 9, 17", "2, 10, 18"))
})

test_that("a not-published item key is still exempt, not an abort (M80)", {
  env <- compare_defs()
  # Four shipped instruments publish no item map, and their notes say so with
  # the NOT_PUBLISHED token. That token is not an item key and must never reach
  # normalise_items(): values_agree() returns NA for it before any parsing, and
  # the ledger records it as not-published rather than as a mismatch.
  expect_true(is.na(env$values_agree("Items", "1, 9, 17, 25",
                                     env$NOT_PUBLISHED, 1)))
})
