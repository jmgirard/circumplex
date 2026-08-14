# M84: Validate the norms-audit roster at its boundary

- **Status:** review
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
- [ ] AC2. The roster builder `roster_from_objects()` refuses by name two of
      the norms tables it cannot roster: a `Norms[[1]]` that is not a data
      frame, and one carrying rows but no `Sample` column. A zero-row
      `Norms[[1]]` is not one of them and stays skipped, as an instrument
      shipping no norms always was. Evidence: a test per shape asserting the
      message names the offending instrument, and a test asserting
      `shipped_roster()`'s body calls `roster_from_objects()`. Today the two
      shapes surface as R's own `"invalid argument type"` and `"arguments
      imply differing number of rows: 1, 0"`, naming neither.
- [ ] AC3. A norms row whose `Sample` is `NA` is refused by
      `roster_from_objects()` rather than silently dropped by the `sort()` the
      builder derives its sample labels with. Evidence: an object-list fixture
      whose `Norms[[1]]` has `Sample = c(1, NA)` raises a refusal naming the
      instrument, and one with `Sample = c(NA, NA)` raises the same refusal.
      Measured 2026-08-14 before the guard: the first returns one row, and the
      second raises `"arguments imply differing number of rows: 1, 0"` — the
      message AC2's second shape also raises, so today the two are
      indistinguishable to a reader of the failure.
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
- [x] T2. Add `roster_from_objects()`'s named refusals — non-data-frame
      `Norms[[1]]`, absent `Sample` column, `NA` sample, and the unnamed
      `objects` list added at the gate — preserving the `NULL[[1]]` behaviour
      pinned by "an instrument shipping no norms is not a roster gap (M79)".
- [x] T3. Cut the `objects` parameter; extract the derivation into
      `roster_from_objects()` with `shipped_roster()` as its no-argument
      wrapper; migrate the 13 argument-taking call sites (3 in markers, 5 in
      coverage, 3 in sample-key, 2 in roster), found by
      `grep -rn 'shipped_roster(' tests/ data-raw/ R/ vignettes/`.
- [x] T4. Register each new abort site in `SCRIPT_ABORTS`
      (`test-norms-audit-markers.R:351`) with its provoking fixture. The
      registrations travel with the task that adds the site — the set-equality
      test reddens the moment an unregistered site exists, so a code task
      cannot leave the suite clean without them — and this task is the sweep
      that confirms the registry is complete once every site is in.
- [x] T5. Add the roster-refusal tests to `test-norms-audit-roster.R:92-107`
      and the AC6 real-instrument regression.
- [x] T6. Full check.

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose validating the roster argument in place over withdrawing it for a caller-passed predicate, because the consumer at `:843-848` must *enumerate* unaudited pairs to name them and a predicate can only test membership — withdrawing it would regress M79's goal; falsified by a caller needing a roster `shipped_roster()` cannot produce (none of the 13 current sites does).
- 2026-08-14: plan gate chose a `validate_roster()` checker over a validating constructor, following the local precedent of `validate_batch()`, which validates the argument it is handed rather than building it; falsified by fixtures needing a roster the shipped builder cannot construct.
- 2026-08-14: implement started on `m84-norms-audit-roster-validation`.
- 2026-08-14: the ROADMAP's claim that M79's AC1 sanctioned an unvalidated roster does not survive quoting — AC1 sanctions an *explicit* roster and says nothing about validation, and its stated companion premise "`batch` is unvalidated on the same footing" was already false at M79, `validate_batch()` having existed since M72.
- 2026-08-14: T1 — `validate_roster()` refuses a non-data-frame, a roster missing `instrument`/`sample`, and a zero-row roster; called after `validate_batch()` so the batch's message is not masked. Measured with the guard bound to a no-op: the csie slice reports 0 non-exempt gaps against a capitalised-column roster and 0 against an empty one, where the shipped roster reports 23.
- 2026-08-14: minor amendment — T4's registrations travel with the task that adds each site, since the registry set-equality test reddens on any unregistered site and no code task could otherwise leave the suite clean; T4 becomes the completeness sweep. T1's three sites are registered in its own commit.
- 2026-08-14: amendment gate — AC2 and AC3 amended to name `roster_from_objects()`, the builder the refusals move into, because AC4 removes the argument that made them reachable through `shipped_roster()`. Both wordings cleared a fresh-context [O] criteria audit; its findings fixed before writing were AC2's exhaustive "the two" (AC3 names a third shape), AC3's false today-claim (an all-NA `Sample` errors today with AC2's second message, verified), the zero-row corner AC2 must not refuse, and AC2's unbacked "fixturable only through an object list" clause, dropped. Declined: editing the Goal, which is plan-owned and stays true through the wrapper; naming the builder in AC4 and AC6, which are satisfiable as written.
- 2026-08-14: amendment gate added a fourth refusal not in the plan — an unnamed `objects` list, which returned a zero-row roster covering nothing. Registered and fixture-provoked under AC5 rather than given its own criterion.
- 2026-08-14: T2, T3, T5 — `roster_from_objects()` holds the derivation and all four refusals; `shipped_roster()` is its no-argument wrapper over the package's own enumeration. 13 argument-taking call sites migrated. Four new abort sites registered. Line anchors in Scope, AC1 and T1 predate `validate_roster()` and are ~25 lines short of the current file; the builder now sits at `data-raw/audit-norms.R:508-576` and the roster resolves at `:705`.
- 2026-08-14: T4 — registry sweep: 7 sites added (3 in `validate_roster()`, 4 in `roster_from_objects()`), all present in `SCRIPT_ABORTS`, 26 sites with 26 distinct identities, every new `stop` key 43–122 literal characters against the floor of 15, and the cross-discrimination off-diagonal still pinned at the 2 `source note not found` cells. `document()` emits no unresolved-link warning and no `man/`/`NAMESPACE` diff; the roxygen 8.0.0→8.1.0 `DESCRIPTION` line it wrote is a local toolchain artifact, reverted.
- 2026-08-14: T6 — `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7216; `devtools::check(args = "--no-manual")` Status: OK (0 errors, 0 warnings, 0 notes); `cairn_validate` all checks passed. No NEWS entry: `data-raw/` is not installed, so nothing here is user-visible.
- 2026-08-14: status → review.

## Decisions

## Review
