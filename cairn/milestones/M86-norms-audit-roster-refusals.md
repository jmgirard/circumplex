# M86: Name every roster shape the norms-audit builder cannot honestly audit

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** m86-norms-audit-roster-refusals

## Goal

Every roster and object list `audit_norms()` cannot honestly audit against is
refused by a message naming the instrument, column, or pair at fault.

## Scope

**In:** the roster path of `data-raw/audit-norms.R` — `validate_roster()`,
`roster_from_objects()`, and the order in which `audit_norms()` validates its
two arguments — plus the roster tests in
`tests/testthat/test-norms-audit-roster.R` and the abort-site registration
those refusals require. Absorbs the six M84 review findings scored 20–65
(F2–F7) carried by the norms-audit roster-builder candidate row.

**Out:** the abort machinery's own matcher and sweep in
`tests/testthat/helper-norms-audit-script.R` — the same-binding-twin defect and
the denylist's field-access-as-value hole stay on their own candidate rows,
which name that helper rather than this script. No shipped `data/` object, no
`R/` surface, and no user-facing behavior changes; this is developer machinery
serving IP5.

## Acceptance criteria

- [ ] AC1: `validate_roster()` refuses a roster missing `instrument` with a
      message naming `instrument`, and one missing `sample` with a message
      naming `sample`; the two messages differ. The single
      `%in% names(roster)` condition at `data-raw/audit-norms.R:150`, the
      assertion at `tests/testthat/test-norms-audit-roster.R:206-211`, and the
      registry entry at `tests/testthat/test-norms-audit-markers.R:414-419` are
      replaced by their two-site successors, not left standing. A test asserts
      each message against a roster missing only that column.
- [ ] AC2: `roster_from_objects()` refuses an `objects` list carrying one name
      twice, naming the repeated name, rather than rostering the first entry
      twice — measured 2026-08-15, `list(fx = <Sample 1>, fx = <Sample 2>)`
      returns two rows both reading `fx 1`. A test asserts message and refusal.
- [ ] AC3: `roster_from_objects()` evaluates `objects[[nm]]$Norms[[1]]` only
      after asserting the entry is a list and its `Norms` a non-empty list,
      each refusal naming the instrument; a `NULL` `Norms` is still skipped and
      `tests/testthat/test-norms-audit-roster.R:100-101` still passes. Measured
      2026-08-15 before the guard: a non-list entry raises `$ operator is
      invalid for atomic vectors`, `Norms = list()` raises `subscript out of
      bounds`, naming neither instrument nor fault. A test asserts each message.
- [ ] AC4: `audit_norms()` calls `validate_batch(batch)` before resolving the
      `NULL` roster default. A test binds `shipped_roster` in the sourced
      script environment to a function that aborts distinctively, calls
      `audit_norms()` with a malformed batch and a defaulted roster, and
      asserts the batch's own message surfaces — an assertion that fails
      against the order at `data-raw/audit-norms.R:725-728`. M84's "the default
      roster is resolved before it is validated" test still passes.
- [ ] AC5: `tests/testthat/test-norms-audit-roster.R` asserts (a)
      `roster_from_objects()` over the shipped objects `expect_identical()`-equals
      a 24-row (instrument, sample) literal authored as character in the test
      file, the builder's frame compared uncoerced so a type change reddens;
      and (b) M79's gap-equivalence regression, auditing the batch slice with
      that literal as `roster` against the defaulted run. Verified by mutating
      the builder's returned frame 26 times — one per pair dropped, one
      spurious pair added, one `sample` returned numeric — each reddening (a).
      The self-comparing assertion at `:334-335` is replaced by both.
- [ ] AC6: `validate_roster()` refuses a roster naming at least one instrument
      in `circumplex:::instrument_names()` unless its pairs are a superset of
      `shipped_roster()`'s, naming the omitted pairs; a roster naming no such
      instrument is not consulted against `data/`. Measured 2026-08-15:
      `data.frame(instrument = "csie", sample = "1")` audits the csie batch
      slice with 0 non-exempt shipped-sample gaps against 23 for the shipped
      roster, and is refused after the guard.
- [ ] AC7: every `stop()`/`stopifnot()` site this milestone adds to or removes
      from `data-raw/audit-norms.R` is reflected in the abort-site registry —
      the registry/walk set-equality assertion in
      `tests/testthat/test-norms-audit-markers.R` passes, each new site carries
      a fixture that raises at it, and the cross-discrimination matrix shows
      each new message distinguishable from every other registered site's. The
      denylist sweep in `tests/testthat/test-norms-audit-denylist.R` covers the
      aliased and namespaced abort spellings the parse-tree walk cannot see.
- [ ] AC8: `devtools::test()` clean and `devtools::check(args = "--no-manual")`
      at 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T7
- AC2 → T2, T7
- AC3 → T3, T7
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7
- AC7 → T7
- AC8 → T8

## Tasks

- [x] T1: split `validate_roster()`'s `%in% names(roster)` condition
      (`data-raw/audit-norms.R:149-150`) into two named refusals; migrate the
      superseded assertion and add the per-column tests.
- [x] T2: refuse a duplicate-named `objects` entry in `roster_from_objects()`
      (`data-raw/audit-norms.R:546-557`); test.
- [ ] T3: guard the `$Norms[[1]]` access — `is.list(entry)`, then the `NULL`
      skip, then non-empty `Norms` (`data-raw/audit-norms.R:559-563`); test.
- [ ] T4: move `validate_batch(batch)` ahead of the default-roster resolution
      (`data-raw/audit-norms.R:725-728`); add the stubbed-`shipped_roster`
      ordering test.
- [ ] T5: author the 24-pair literal, replace the self-comparing assertion at
      `tests/testthat/test-norms-audit-roster.R:334-335` with the equality and
      gap-equivalence pair, and run the 26 mutations.
- [ ] T6: add the shipped-superset refusal to `validate_roster()`; test the
      measured csie shape and confirm the fake-instrument fixtures are untouched.
- [ ] T7: register every added and removed abort site; re-run the registry
      set-equality assertion and the cross-discrimination matrix.
- [ ] T8: run the profile's verify slot and the full check.

## Work log

- 2026-08-15: created by /milestone-plan.
- 2026-08-15: plan-gate criteria audit ([O], fresh context) ran twice — round 1 returned findings on AC1, AC3, AC4, AC5, AC6 plus three cross-criterion conflicts (AC4's probe could not discriminate the two orders; AC5's mutation was blind at the very pair its test was built around); round 2 over the revised set returned AC5's coercion ambiguity, AC5's dropped gap-equivalence regression, and AC7's universal outrunning its procedure. All fixed before writing; AC6's ambiguity went to the user.
- 2026-08-15: plan gate chose refusing any roster that names a shipped instrument without covering every shipped pair, over per-instrument completeness, because csie ships one sample so the measured 0-gap roster is already per-instrument complete; falsified by a legitimate use for a narrow audit over real data.
- 2026-08-15: plan gate chose a hand-authored 24-pair literal over asserting counts and instrument names, because only the literal reddens on a mistyped sample or a swapped pair; falsified by the literal's maintenance cost exceeding the losses it catches as `data/` grows.
- 2026-08-15: plan gate chose taking the argument-ordering fix with a stubbed-`shipped_roster` test over leaving it out, the shape being unreachable from shipped data today; falsified by the stub proving unbindable in the sourced script environment.

- 2026-08-15: T1 done. The two column guards are written out rather than looped: a loop is one `stop()` call carrying the column as an argument, keying `"`roster` has no `{}` column"`, whose matcher accepts both messages — the matrix would then certify as distinguishable two refusals it cannot tell apart. Deleting the `sample` guard reddens 1 assertion in test-norms-audit-roster.R and 8 in test-norms-audit-markers.R; restore verified by blob hash.

- 2026-08-15: T2 done. `anyDuplicated(nms)` refuses a repeated name, reporting each repeated name once however many times it recurs; measured before the guard, `list(fx = <Sample 1>, fx = <Sample 2>)` returned two rows both reading `fx 1`. Guard sits after the naming check, so an unnamed list still reports as unnamed.

## Decisions

## Review
