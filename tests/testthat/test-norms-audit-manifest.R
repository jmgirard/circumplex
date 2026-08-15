# The norms audit's abort sites are enumerated, and the enumeration is checked
# against the script rather than against the tests (M87).
#
# M81-M83 built a registry, per-site matchers with build-time floors, a
# cross-discrimination matrix and a denylist sweep to hold this property --
# roughly 1500 lines guarding a `data-raw/` script that ships to nobody. M87
# retires all of it and keeps the one thing per-test assertions structurally
# cannot do: per-test regexps are quantified over the TESTS, so a guard added to
# the script with no test at all is invisible to them. The manifest is
# quantified over the SCRIPT.
#
# What that buys, concretely: `helper-norms-audit-manifest.R` is set-equal to a
# fresh walk, so adding a `stop()` to the audit script without an entry reddens
# this file. What it does not buy is stated in the milestone's decision entry --
# non-`stop()` abort spellings are no longer swept for at all.
#
# DEVELOPMENT-ONLY: data-raw/ is not installed, so these skip against the
# installed package, as the sibling audit test files do.

manifest_sites <- function() {
  script <- testthat::test_path("..", "..", "data-raw", "audit-norms.R")
  skip_if_not(file.exists(script), "data-raw/ not present (installed package)")
  norms_audit_abort_sites(parse(script, keep.source = FALSE))
}

# The manifest's own rows as the same three-part identity the walk produces, so
# the two sides are compared as sets of identities rather than by row order.
manifest_ids <- function() {
  paste(NORMS_AUDIT_MANIFEST$kind, NORMS_AUDIT_MANIFEST$binding,
        NORMS_AUDIT_MANIFEST$key, sep = "\t")
}

walked_ids <- function(sites) {
  vapply(sites, function(s) {
    paste(s$kind, s$binding, s$key, sep = "\t")
  }, character(1))
}

test_that("the manifest is set-equal to a fresh walk of the audit script (M87)", {
  sites <- manifest_sites()
  got <- walked_ids(sites)
  want <- manifest_ids()

  # Both directions, and named: a site the script raises and the manifest omits
  # is the failure this file exists for, while a manifest row the script no
  # longer raises is a stale entry that would keep a deleted guard looking
  # covered.
  expect_setequal(got, want)
  expect_identical(setdiff(got, want), character(0))
  expect_identical(setdiff(want, got), character(0))

  # Both sides are sets, and BOTH are asserted duplicate-free -- the property
  # the ordinal used to carry, kept as a refusal instead of a discriminator
  # (M88, D-043).
  #
  # `expect_setequal()` compares membership, so it is blind to a repeat on
  # either side: two guards identical in (kind, binding, key) would walk to two
  # equal ids, collapse onto the manifest's one row, and leave the comparison
  # green with a site nothing asserts. The manifest side needs the same check
  # for the mirror reason -- one duplicated row would stand in for two sites.
  #
  # A twin is not forbidden, only silent adoption of one. Adding a second guard
  # identical in all three parts now reddens here, and the decision is then a
  # deliberate one: reword one guard, or restore an ordinal.
  expect_identical(anyDuplicated(want), 0L)
  expect_identical(anyDuplicated(got), 0L)

  # The identity's field SET, asserted directly rather than by searching for a
  # spelling. A fourth field is how any restoration of the ordinal -- or of
  # anything else keyed per site -- would have to arrive, whatever it is named
  # and however it is written: dollar or double-bracket field assignment, a
  # field renamed to something the word search cannot know, a setNames() or
  # cbind() column. Each of those escapes a search for the word and none of
  # them escapes this. The grep AC4 also names is a spot check on three
  # spellings, not what carries the claim (M88).
  expect_identical(names(NORMS_AUDIT_MANIFEST), c("kind", "binding", "key"))
  for (s in sites) {
    expect_setequal(names(s), c("kind", "key", "binding"))
  }
})

test_that("every manifest key carries enough literal text to discriminate (M87)", {
  skip_if_not(nrow(NORMS_AUDIT_MANIFEST) > 0L)

  # The floor the retired matcher enforced at registry-build time, kept because
  # uniqueness does not subsume it: an all-placeholder key renders the regex
  # ".", which accepts ANY message while remaining perfectly unique. The
  # helper's own comments record two incidents of that failing open.
  # The shipped helper, not a re-implementation of it: a test that retypes the
  # expression under test agrees with it by construction and diverges only
  # where it matters (M78).
  literals <- vapply(NORMS_AUDIT_MANIFEST$key, norms_audit_key_literals,
                     character(1), USE.NAMES = FALSE)
  is_stop <- NORMS_AUDIT_MANIFEST$kind == "stop"

  expect_true(all(nchar(literals[is_stop]) >= NORMS_AUDIT_STOP_KEY_FLOOR))
  expect_true(all(nchar(literals) > 0L))

  # Headroom is asserted here rather than written into the constant, where an
  # edit to the script would strand it.
  expect_gt(min(nchar(literals[is_stop])), NORMS_AUDIT_STOP_KEY_FLOOR)
})

test_that("only the declared key resolves to more than one site (M87)", {
  sites <- manifest_sites()
  keyed <- vapply(sites, function(s) paste(s$kind, s$key, sep = "\t"), character(1))
  dup <- unique(keyed[duplicated(keyed)])

  # `source note not found: {}` is raised from two bindings by design
  # (source_note_block_tags and parse_source_note). Every other key selects one
  # site on its own, which is what lets a site assertion name a key and mean a
  # site.
  expect_identical(dup, paste("stop", NORMS_AUDIT_AMBIGUOUS_KEYS, sep = "\t"))

  bindings <- vapply(sites[keyed %in% dup], function(s) s$binding, character(1))
  expect_setequal(bindings, c("source_note_block_tags", "parse_source_note"))
})
