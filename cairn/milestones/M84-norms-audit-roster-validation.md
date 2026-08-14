# M84: Validate the norms-audit roster at its boundary

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M83
- **Driving RR:** —
- **Principles touched:** GP2
- **Branch/PR:** `m84-norms-audit-roster-validation`

## Goal

`audit_norms()` refuses a roster it cannot audit against and `shipped_roster()`
refuses a degenerate norms table by name, so no audit reports a clean run over
data it never read.

## Scope

**In:** `data-raw/audit-norms.R` — the roster boundary in `audit_norms()`
(`:627-857`, roster resolved at `:631`, consumed at `:838-848`) and the builder
`shipped_roster()` (`:485-507`); the `SCRIPT_ABORTS` registrations each new
refusal requires; the roster tests.

**Out:** the coverage report's blank `sample` column → M85. The test machinery
itself → M83, on which this depends. `parse_source_note()` returning a note's
single untagged block when the caller names no instrument stays **declined on
the merits**, as at the M79 plan gate: the design note at
`data-raw/audit-norms.R:138-143` makes it deliberate and it is unreachable
through `audit_norms()`. The M75 re-capture of the same defect in the ROADMAP
row is a duplicate of that rejection, not a live finding.

## Acceptance criteria

- [ ] AC1. `audit_norms()` validates the roster it will audit against — after
      the `NULL` default is resolved by `shipped_roster()` at `:631`, so the
      default is never refused — by the same shape as the sibling
      `validate_batch()` (`:82-132`): a `validate_roster()` refuses a
      non-data-frame, a missing `instrument` or `sample` column, and a zero-row
      roster, each with its own message naming the fault. Evidence: one test
      per refusal shape asserting that shape's message.
- [ ] AC2. `shipped_roster()` refuses by name a norms table it cannot roster: a
      `Norms[[1]]` that is not a data frame, and one with no `Sample` column.
      Evidence: a test per shape asserting the message names the offending
      instrument. Today these surface as R's own `"invalid argument type"` and
      `"arguments imply differing number of rows: 1, 0"`, naming neither.
- [ ] AC3. A norms row whose `Sample` is `NA` is refused rather than silently
      dropped by `sort()` at `:500`. Evidence: a fixture object with
      `Sample = c(1, NA)` raises a named refusal; today it returns one row.
- [ ] AC4. `shipped_roster()` takes no arguments, so `shipped_roster(objects)`
      is not spellable and cannot re-fuse the roster to the object list.
      Evidence: `expect_length(formals(env$shipped_roster), 0L)`. Fixtures
      needing a roster over an explicit object list call a separately named
      constructor; the 13 argument-taking call sites in tracked files migrate
      to it. `grep -rn 'shipped_roster(' tests/ data-raw/ R/ vignettes/` as
      hygiene.
- [ ] AC5. Every abort site this milestone adds is declared in `SCRIPT_ABORTS`
      and provoked by a fixture, settled in three parts by tests cited by name,
      not line: spelling stays closed to `stop()`/`stopifnot()` by the
      denylist sweep in `test-norms-audit-denylist.R`; declaration is settled
      by the registry set-equality test in `test-norms-audit-markers.R`; and
      fixture provocation is settled by that file's cross-discrimination test,
      whose non-NA-message and diagonal assertions fail on an entry whose
      fixture raises nothing. Each new key clears the 15-literal-character
      floor and is mutually discriminating against the existing keys.
- [ ] AC6. The M79 regression gap closes: `audit_norms()` over a real
      single-instrument batch reports the same non-exempt gap count with and
      without an explicitly passed roster. Measured today: 23 gaps both ways,
      but 0 when passed `roster = shipped_roster(objects)`.
- [ ] AC7. `devtools::test()` and `devtools::check(args = "--no-manual")` clean,
      with `document()` warning-free per the profile's consistency gate.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T5
- AC3 → T2, T5
- AC4 → T3
- AC5 → T4
- AC6 → T3, T5
- AC7 → T6

## Tasks

- [x] T1. Add `validate_roster()` beside `validate_batch()`
      (`data-raw/audit-norms.R:82-132`); call it in `audit_norms()` after the
      default resolves at `:631`, sequenced so `validate_batch()`'s clearer
      message is not masked.
- [ ] T2. Add `shipped_roster()`'s named refusals at `:496-502` —
      non-data-frame `Norms[[1]]`, absent `Sample` column, `NA` sample —
      preserving the `NULL[[1]]` behaviour pinned at
      `test-norms-audit-roster.R:97-101`.
- [ ] T3. Cut the `objects` parameter (`:485-493`); add the fixture
      constructor; migrate the 13 argument-taking call sites (markers `:268`,
      `:281`, `:293`; coverage `:169`, `:220`, `:241`, `:263`, `:314`;
      sample-key `:121`, `:145`, `:178`; roster `:101`, `:106`).
- [ ] T4. Register each new abort site in `SCRIPT_ABORTS`
      (`test-norms-audit-markers.R:351`) with its provoking fixture. The
      registrations travel with the task that adds the site — the set-equality
      test reddens the moment an unregistered site exists, so a code task
      cannot leave the suite clean without them — and this task is the sweep
      that confirms the registry is complete once every site is in.
- [ ] T5. Add the roster-refusal tests to `test-norms-audit-roster.R:92-107`
      and the AC6 real-instrument regression.
- [ ] T6. Full check.

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose validating the roster argument in place over withdrawing it for a caller-passed predicate, because the consumer at `:843-848` must *enumerate* unaudited pairs to name them and a predicate can only test membership — withdrawing it would regress M79's goal; falsified by a caller needing a roster `shipped_roster()` cannot produce (none of the 13 current sites does).
- 2026-08-14: plan gate chose a `validate_roster()` checker over a validating constructor, following the local precedent of `validate_batch()`, which validates the argument it is handed rather than building it; falsified by fixtures needing a roster the shipped builder cannot construct.
- 2026-08-14: implement started on `m84-norms-audit-roster-validation`.
- 2026-08-14: the ROADMAP's claim that M79's AC1 sanctioned an unvalidated roster does not survive quoting — AC1 sanctions an *explicit* roster and says nothing about validation, and its stated companion premise "`batch` is unvalidated on the same footing" was already false at M79, `validate_batch()` having existed since M72.
- 2026-08-14: T1 — `validate_roster()` refuses a non-data-frame, a roster missing `instrument`/`sample`, and a zero-row roster; called after `validate_batch()` so the batch's message is not masked. Measured with the guard bound to a no-op: the csie slice reports 0 non-exempt gaps against a capitalised-column roster and 0 against an empty one, where the shipped roster reports 23.
- 2026-08-14: minor amendment — T4's registrations travel with the task that adds each site, since the registry set-equality test reddens on any unregistered site and no code task could otherwise leave the suite clean; T4 becomes the completeness sweep. T1's three sites are registered in its own commit.

## Decisions

## Review
