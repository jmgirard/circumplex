# M107: Make the exemplar-B guards run in the gate that ships a release

**Status:** done (2026-08-24, PR #136 https://github.com/jmgirard/circumplex/pull/136)

**Goal:** Relocate the exemplar-B fixture so the four assertions that skipped under
`R CMD check` execute there, and restore the lavaan cross-check that had stopped comparing.

**Outcome:** `rb18-counterexample-b.rds` (246 bytes) ships at `tests/testthat/fixtures/`; the
four sites reading it via `test_path("..", "..", "cairn", ...)` behind `skip_if_not(file.exists(...))`
now read the packaged copy unguarded, execute under a tarball check, and were each mutation-proved
to redden alone. `test-fixture-drift.R` fences those bytes against the `cairn/reviews/` record,
gating on `dir.exists(cairn)` so a deleted record reddens rather than skips. `lav_cfi_ref()`
(`helper-lavaan-cfi.R`) probes `lav_fit_cfi()` by call — current `x2/df/x2_null/df_null`, then
older `X2/df/X2.null/df.null` — muffling warnings while keeping their value, NULL only when
neither yields a single finite number; the three CFI comparisons compare again. Exported
behaviour unchanged.

**Decisions:** none cross-cutting. Milestone-local: revive the lavaan cross-check rather than delete it; defer the ~50 other source-tree reads.

**Review:** three fresh-context reviewers; two found nothing, the diff-bug lens ten, five
re-verified here. Fixed at the gate: two false comments (the matrix *can* be written as code —
`hexNumeric` dput round-trips bit-identically — and a plain round trip does not flip the case to
NULL); the helper erroring rather than skipping on an odd return type; a warning discarding a
usable value; the drift guard skipping on a deleted record. Record corrected: two call sites,
three comparisons. Provenance, rename detection and the devel oracle's second copy → the M107-remainder candidate row.
