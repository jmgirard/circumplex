# M85: Carry the sample through the audit's note-only coverage rows

- **Status:** planned
- **Priority:** low
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

A note-only coverage row carries the sample its dedupe key already
distinguishes it by, so two rows differing only in sample stop emitting
identically and surviving a `unique()` as one.

## Scope

**In:** the `note-only-sample` emitter at `data-raw/audit-norms.R:713-719`,
which passes no `sample` and so inherits `coverage_rows()`'s
`sample = NA_character_` default at `:592`; the resulting regeneration of the
committed `data-raw/norms-audit-coverage.csv`; the coverage tests that assert
over it.

**Out:** roster validation → M84; the test machinery → M83. No change to the
dedupe key at `:702-703`, which already includes `sample` correctly (D-M80-1).

## Acceptance criteria

- [ ] AC1. The `note-only-sample` emitter passes the note's own `sample`, so
      two note-only rows differing only in that field emit as two distinct
      rows. Evidence: the M80 fixture's `sample` axis asserts
      `nrow(unique(only)) == nrow(only)`; measured today as 1 against 2.
- [ ] AC2. `test-norms-audit-coverage.R`'s four-axis note-only test gains a
      `sample`-column assertion, so it can distinguish the fixed emitter from
      the broken one on that axis. Today the four axes assert only `label` and
      `detail` and cannot.
- [ ] AC3. `data-raw/norms-audit-coverage.csv` is regenerated from a run-block
      execution and committed, and the "the committed coverage report is the
      one this code emits" test passes against it column by column. The 14
      committed `note-only-sample` rows carry the `NO_SAMPLE` token from
      `data-raw/audit-norms.R:327` rather than `NA`.
- [ ] AC4. No shipped audit verdict changes: the non-exempt gap count and the
      set of instruments named in the report are identical before and after,
      compared against the pre-change CSV.
- [ ] AC5. `devtools::test()` and `devtools::check(args = "--no-manual")` clean,
      with `document()` warning-free per the profile's consistency gate.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4

## Tasks

- [ ] T1. Pass `sample = note_only$sample[fresh]` from the emitter
      (`data-raw/audit-norms.R:713-719`).
- [ ] T2. Add the `sample`-axis assertions to the note-only test
      (`test-norms-audit-coverage.R:180-245`) — write them first and watch them
      redden against the current emitter.
- [ ] T3. Regenerate `data-raw/norms-audit-coverage.csv`; diff it against the
      committed version and confirm only the 14 note-only `sample` cells move.
- [ ] T4. Full check.

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose a separate milestone over folding this into M84, because its lineage is M80's report schema rather than M79's roster and it changes a committed data artifact, which deserves its own review surface; falsified by the regeneration turning out to be inseparable from a roster change.

## Decisions

## Review
