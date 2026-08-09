# M81: Enumerate the norms-audit abort registry from the script's parse tree

- **Status:** blocked
- **Priority:** normal
- **Depends on:** M79
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m81-norms-audit-abort-registry` / https://github.com/jmgirard/circumplex/pull/108

## Goal

Make the norms-audit suite's abort registry count over a domain the script
itself produces, so an abort cannot pass by landing where the guard never
looked.

## Scope

**In:** `tests/testthat/test-norms-audit-markers.R`'s `SCRIPT_ABORTS` registry
and the procedure that enumerates its domain; a message test for
`validate_batch()`'s `stopifnot()` at `data-raw/audit-norms.R:83-85`; and the
single-sourcing assertion at `tests/testthat/test-norms-audit-roster.R:119`,
green today over deletion of the call it stands behind because a text grep
matches the doc comment at `data-raw/audit-norms.R:404` and the string literal
at `:414`. One mechanism serves all three: read the script's parse tree, never
its text and never a sourced environment. M79 removed its AC5 to here after it
failed twice, each time by the counted enumeration falling one scope short of
the file.

**Out:** adding, moving, or widening any guard in `data-raw/audit-norms.R` —
this milestone changes tests and their enumeration, not the script's abort
sites. The coverage report's column schema → M80. Any change to `data/` → not
here. A promise about errors the script raises by mechanisms other than a
`stop()`/`stopifnot()` call — a subscript failure, a coercion, `match.arg()` —
is declined at this gate rather than deferred: no procedure available here
enumerates that domain, and AC2 states the bound instead of claiming it.

## Acceptance criteria

- [ ] AC1 The registry's domain is produced by parsing the script, not by
      sourcing it. A helper walks the full expression tree of every top-level
      expression of `parse(file = "data-raw/audit-norms.R")` — including the
      trailing run block that `norms_audit_defs_only = TRUE` skips — and
      collects every call whose deparsed head is one of `stop`, `stopifnot`,
      `base::stop`, `base::stopifnot`. Each collected `stop()` site keys on its
      **message template** — every literal fragment of the call in order, with
      each non-literal argument rendered `{}` — and not on its first fragment
      alone, which is `"source note "` at six distinct sites (measured
      2026-08-09) and would let a fixture for one of the six satisfy AC2's
      assertion for another. A `stopifnot()` site, which carries
      no message argument, contributes one key per condition, each keyed on
      that condition's deparsed text. A test asserts the
      collected site set equals the registry's, in both directions, by key and
      count. Mutation-verified with the return-3 mutant: an unregistered
      `stop()` planted inside the run block reddens the test, where the
      pre-milestone guard stayed at FAIL 0 / PASS 69 and its own count stayed
      at 12 while the parse walk saw 13.
- [ ] AC2 Every **site** AC1's walk collects — not every registry entry; the
      `"source note not found: {}"` key covers two sites, `:193` and `:224` —
      carries a fixture that provokes that site, and a test asserts the fixture
      raises an error matching that site's key rather than bare failure. With
      AC1's set equality the message-tested set and the parsed set are
      identical. The promise is bounded by AC1's walk: it quantifies over the
      calls that walk returns and claims nothing about errors the script raises
      by other mechanisms, which no procedure here enumerates.
- [ ] AC3 `validate_batch()`'s `stopifnot()` at `data-raw/audit-norms.R:83-85`
      is registered and message-tested under AC1 and AC2 for each of its two
      conditions — a batch that is not a data frame, and one missing a required
      column — each asserted by the failing condition's own deparsed text
      rather than by any error. Each is mutation-checked, and the
      not-a-data-frame fixture carries all five required names so that dropping
      `is.data.frame(batch)` cannot abort through the sibling condition
      instead.
- [ ] AC4 The single-sourcing assertion at
      `tests/testthat/test-norms-audit-roster.R:119` is stated over the
      script's parse tree — the call resolving `instrument_names` from the
      package namespace, today `get("instrument_names", envir = ns)()` at
      `data-raw/audit-norms.R:414` — so neither the doc comment at `:404` nor
      any string literal satisfies it. Mutation-verified: deleting the call at
      `:414` reddens it while the comment at `:404` stands, which the text grep
      it replaces does not do.
- [ ] AC5 `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
      re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
      `data-raw/norms-audit-coverage.csv` unchanged but for their three stamp
      columns (`generated`, `script_commit`, `data_commit`,
      `data-raw/audit-norms.R:750-758`), compared column by column.

## Coverage

- AC1 → T1, T4
- AC2 → T2
- AC3 → T3
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1 The parse-tree collector: walk every top-level expression of
      `data-raw/audit-norms.R`, collect the four call heads, key each site per
      AC1. Replace `marker_defs()`'s sourced enumeration in the count test.
- [x] T2 Re-key `SCRIPT_ABORTS` to one entry per site, each carrying its
      fixture; one test asserts each fixture raises its own key, another
      asserts set equality with T1's collection in both directions.
- [x] T3 The two `validate_batch()` `stopifnot()` cases, the five-name
      non-data-frame fixture, and both mutation checks.
- [x] T4 Mutation-verify T1's collector with the return-3 mutant (an
      unregistered `stop()` in the run block); record the measured before/after
      and restore.
- [x] T5 Re-anchor the single-sourcing assertion on the parse tree; both AC4
      mutations measured.
- [x] T6 Re-run the audit (stamp columns only), `devtools::test()`, full
      `check(args = "--no-manual")`, run with nothing else in the session
      touching the R library.

## Work log

- 2026-08-09: created by /milestone-plan, from M79's return-3 thrash routing.
- 2026-08-09: plan gate chose enumerating the abort domain by parsing the script file over sourcing it and over a text sweep; a sourced environment is what failed twice (the domain is whatever the test loads), and a text sweep matches comments and string literals, which is the defect AC4 exists to fix. Falsified by an abort site the parse walk cannot see that a reader can.
- 2026-08-09: plan gate chose settling the twice-failed criterion here over a /milestone-brief escalation, offered per M79's thrash trigger (b): the repair is named by the bounded-promise rule (narrow the promise until a stated procedure settles it), not by a judgment the session lacks. Falsified by AC1 or AC2 failing again by a third mechanism of the same shape.
- 2026-08-09: plan gate chose folding the single-sourcing guard in over leaving it a candidate row; it is the same false-coverage shape and the same repair, and it stands behind a clause M79 is shipping. Falsified by the parse-tree re-anchoring proving unrelated to the registry work at implement time.
- 2026-08-09: criteria audit ([O], fresh context, authored none of the criteria) returned findings on 4 of the 5 drafted criteria plus M79's amended one; all adopted. The load-bearing ones: AC4 was unsatisfiable as drafted (no `circumplex:::instrument_names` call exists; `:414` is a `get()` call, whose string literal also satisfies today's grep); AC1 left its matching and keying rules to the reader, "the AC5 failure mode in miniature"; AC2 promised one fixture per registry entry where `"source note not found: "` covers two sites; and AC3's mutation clause was defeatable by a fixture missing the five required names. The auditor verified the run-block invisibility (12 sites counted vs 13 parsed on a planted mutant) and the FAIL 0 / PASS 69 baseline.

- 2026-08-09: implement gate amended AC1's keying rule and AC2's example key — sites key on the message template (all literal fragments, `{}` for the rest), not the first fragment alone, which is `"source note "` at six distinct sites and would let one site's fixture satisfy another's AC2 assertion. Measured on the parse walk over `data-raw/audit-norms.R` at `2ab626f6`: 13 abort calls (12 `stop()`, 1 `stopifnot()`) against the shipped text guard's 12.
- 2026-08-09: implement gate chose a shared `tests/testthat/helper-norms-audit-script.R` for the parse walk over a copy in each of the two test files, matching the suite's five existing helper files.
- 2026-08-09: T1+T2 landed in one checkpoint — replacing the count test's sourced enumeration (T1) has no meaning without the per-site registry it compares against (T2). New `tests/testthat/helper-norms-audit-script.R`; the walk returns 14 site entries (12 `stop()`, 2 `stopifnot()` conditions), every key unique but the intended `source note not found: {}` pair. `test-norms-audit-markers.R`: FAIL 0 | PASS 76, up from PASS 69.
- 2026-08-09: T3 mutations measured on `data-raw/audit-norms.R`, script restored byte-clean after each. Deleting `is.data.frame(batch)`: FAIL 2 | PASS 73, the message test reporting "no error raised" — the five-name fixture reaches no sibling guard, so the case cannot pass through one. Deleting the required-columns condition: FAIL 2 | PASS 74, its fixture likewise raising nothing. Both also redden the set-equality test, which drops the deleted condition's key.
- 2026-08-09: T4 return-3 mutant measured — an unregistered `stop()` planted in the run block beside `res <- audit_norms()`. New guard: FAIL 1 | PASS 75, the set-equality test reporting the site as unregistered. Retired text-count guard run against the same mutated file: still 12, i.e. it would have stayed green. Script restored byte-clean.
- 2026-08-09: T5 re-anchored the single-sourcing assertion on the parse tree via `norms_audit_resolves_name()`; the absence half stays a text assertion, deliberately, a second enumeration being a defect whether live or commented out. Two mutations measured, script restored byte-clean after each. Deleting the resolving call and replacing it with a hard-coded copy of the 15-instrument list — behaviour identical, so the mutation isolates: FAIL 1 | PASS 16, the only failure being the assertion, while the retired text grep stays TRUE off the doc comment at `:404`. Control: switching the call to `circumplex:::instrument_names()` keeps FAIL 0 | PASS 17, and green for the claim's reason — the `:::` shape is what matched, verified by listing the matching call.
- 2026-08-09: the AC4 assertion accepts `pkg:::nm`, `pkg::nm` and a literal-naming `get()`/`getExportedValue()`, not the one shape at `:414` alone — a test that reddens under a behaviour-preserving accessor switch is a defect in the test.
- 2026-08-09: T6 — audit re-run at `53908411`: ledger 194 rows, coverage 15 rows, 0 gaps, 14 note-only, 1 constructed credit, 0 angle-copy splits, 0 IP2 breaches. Compared column by column against the committed CSVs: coverage identical in all 5 columns; ledger identical in all 12 but `script_commit` and `data_commit`, `generated` included. The stamp-only churn was reverted rather than committed — M81 changes no audited value, so the committed stamps stay a true record of the run that produced them. `devtools::test()`: FAIL 0 | WARN 4 | SKIP 0 | PASS 6883, the 4 warnings outside this milestone's files (both ran WARN 0 on their own). `devtools::check(args = "--no-manual")`: Status OK, 0 errors / 0 warnings / 0 notes, 16m 46s.
- 2026-08-09: review returned M81 to `in-progress` on AC1 (defect return 1 on this milestone). What failed: AC1 says a collected `stopifnot()` site "contributes one key per condition", but `call_positional_args()` drops named arguments, so a condition written `stopifnot("msg" = cond)` contributes none. Verified at the gate — a real added guard fires (`divisor must be numeric`) while the walk returns 14 sites against a baseline of 14 and the suite stays FAIL 0 | PASS 76; the `stopifnot(exprs = {...})` form returns 12 against the same baseline. Thrash trigger (b) fires: this is the third mechanism of the same shape against this criterion's lineage, after M79's AC5 failed twice, and it is exactly the falsifier the 2026-08-09 plan gate wrote down when it declined a `/milestone-brief` escalation. Trigger (a) composes — a re-plan has already been spent on this criterion (M81 is that re-cut) — so a bare retry is not the disposition.
- 2026-08-09: blocked on RB17 — whether a syntactic enumeration of abort sites over open-ended R source can be complete at all, and what shape the promise should take if not. Raised at the user's choice at the review gate, on the thrash trigger the plan gate had recorded in advance. Brief committed on the milestone branch rather than the default branch: the milestone file's status mirror travels with it and lands at squash-merge, so splitting the two across branches would conflict.
- 2026-08-09: RR17 binding-criteria ingest audit ([O], fresh context, authored none of them) returned must-fix defects on 2 of 9 and should-fix on 2 more, so nothing was ingested. BC8 is unsatisfiable as written (BC5 makes the two `source note not found` sites distinct identities sharing one key, so a message-only matrix cannot be diagonal). BC7's closing sentence reintroduces the unbounded universal the brief existed to remove, over a domain no named procedure enumerates, and is false today at 79 bare `expect_error()` calls across 19 files (re-measured here by parsing every test file; the audit's 137/19 figure was a line-based overcount) — all outside Scope In, so it is also the one criterion that would need a Scope amendment. BC2's mutation baseline is wrong (appending `stopifnot(exprs = {TRUE})` leaves the count at 14, verified here; the 12 comes from rewriting the two existing conditions) and it places one numeric floor in three different homes. BC5 drops RR17 s4's ordinal escape hatch, leaving AC1 and BC5 latently jointly unsatisfiable. The audit also found AC1 needs three edits, not the one RR17 names.
- 2026-08-09: gate chose splitting M81 over compressing it to fit RR17's nine criteria — 36 lines of headroom against ~74 lines of criteria, and 14 acceptance criteria against a split tripwire of ~7. M81 keeps what is built plus the minimal repair closing the demonstrated named-`stopifnot` hole; a successor milestone takes the larger machinery (denylist sweep, composite site identity, stack-based fixture binding, discrimination matrix). Falsified if the reissued criteria prove inseparable without becoming jointly unsatisfiable across the cut.
- 2026-08-09: gate chose returning the four defective criteria to RR17's author over settling them here, per the rule that the implementing session never authors the durable verdict on the review constraining it; the reviewer was also asked to choose the split partition, its criteria's dependency structure being what decides a safe cut.

## Decisions

## Review

**2026-08-09 — returned to `in-progress` on AC1. PR #108 (draft, not merged).**

Evidence gathered before the return:

- AC1 (structure, PASS): 27 top-level expressions parsed; the last is the `if`
  guarded by `norms_audit_defs_only`, so the run block is inside the walked
  domain. 14 sites (12 `stop`, 2 `stopifnot` conditions), 13 distinct keys,
  the one duplicate being the intended `source note not found: {}` pair. Set
  equality is bidirectional (sorted multiset `identical()`).
- AC1 (promise, **FAIL**): see the return below.
- AC2: `SCRIPT_ABORTS` holds 14 entries, all carrying `(kind, key, fixture)`.
  The diff-bug reviewer's cross-contamination matrix — every fixture's actual
  message against every key's matcher — found no message satisfying a key
  other than its own, bar the intended pair.
- AC3: reviewer independently reproduced the discrimination; the five-name
  non-data-frame fixture leaves the sibling condition TRUE.
- AC4: met against the two stated mutations; F6 names a third it does not
  survive.
- AC5: not re-verified fresh this session — the return came first.
- Consistency gate: `cairn_validate` exit 0, every CHECK PASS. The 47
  `work-log format` advisories are all pre-existing hard-wrapped lines in M7.
- Touched test files, fresh: FAIL 0 | PASS 76 and FAIL 0 | PASS 17.

Fresh-context review: three lenses. Blame-history — zero findings (F15's
occurrence-counting superseded by AST node collection, all four assertions of
the removed non-parser abort test present, F11's fix tightened not reverted,
no D-entry contradicted). Prior-review — zero findings; the
`gh api pulls/comments` probe returned empty, so the thread walk was skipped.
Diff-bug — 11 findings, scored by a fresh [S] scorer.

**Actioned (>= 80): F1, scored 82 — AC1's walk misses a `stopifnot()`
condition written in named-message form.** `call_positional_args()` drops
every named argument, which is right for `stop()`'s `call. = FALSE` and wrong
for `stopifnot("msg" = cond)`, where the name IS the message and the argument
is a real condition. Verified independently at the gate: adding
`"divisor must be numeric" = is.numeric(batch$divisor)` to `validate_batch()`
gives a guard that genuinely fires (`validate_batch()` on a character
`divisor` raises `divisor must be numeric`), while the walk still returns 14
sites against a baseline of 14 and the suite stays FAIL 0 | WARN 0 | PASS 76.
The reviewer separately measured the `stopifnot(exprs = {...})` form at 12
sites against the same baseline. Additions hide; removals show.

Below threshold, logged not actioned (10): F3 (78) an all-`{}` key renders
regex `"."`, so a `sprintf`-form rewrite of any `stop()` would make that
site's AC2 assertion pass on any error — measured. F6 (72) the AC4 assertion
checks neither the package operand of `:::` nor that `get()`'s `envir` is a
namespace, so a local hard-coded copy read via `get("instrument_names",
envir = environment())` satisfies it. F4 (65) `stopifnot` stem matching has
no length floor and cannot see past R's truncation width — measured. F2 (55)
nothing ties a registry entry to a site, so two same-key entries could share
one fixture. F9 (55) key mutual-non-matching is a comment, not a test.
F5 (45) `stopifnot` keys redden under a pure formal rename. F10 (42) a
run-block abort is collectable but not fixturable, so AC1 and AC2 would be
jointly unsatisfiable for one. F8 (40) `norms_audit_site_ids()` is
name-sensitive. F7 (32) the absence-half text grep is evadable. F11 (15)
generic helper names in the shared test environment.
