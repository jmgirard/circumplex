# M81: Enumerate the norms-audit abort registry from the script's parse tree

- **Status:** planned
- **Priority:** normal
- **Depends on:** M79
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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
      `base::stop`, `base::stopifnot`. Each collected site keys on its own
      first literal message fragment, and a `stopifnot()` site, which carries
      no message argument, keys on its deparsed condition. A test asserts the
      collected site set equals the registry's, in both directions, by key and
      count. Mutation-verified with the return-3 mutant: an unregistered
      `stop()` planted inside the run block reddens the test, where the
      pre-milestone guard stayed at FAIL 0 / PASS 69 and its own count stayed
      at 12 while the parse walk saw 13.
- [ ] AC2 Every **site** AC1's walk collects — not every registry entry; the
      `"source note not found: "` key covers two sites, `:193` and `:224` —
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

- [ ] T1 The parse-tree collector: walk every top-level expression of
      `data-raw/audit-norms.R`, collect the four call heads, key each site per
      AC1. Replace `marker_defs()`'s sourced enumeration in the count test.
- [ ] T2 Re-key `SCRIPT_ABORTS` to one entry per site, each carrying its
      fixture; one test asserts each fixture raises its own key, another
      asserts set equality with T1's collection in both directions.
- [ ] T3 The two `validate_batch()` `stopifnot()` cases, the five-name
      non-data-frame fixture, and both mutation checks.
- [ ] T4 Mutation-verify T1's collector with the return-3 mutant (an
      unregistered `stop()` in the run block); record the measured before/after
      and restore.
- [ ] T5 Re-anchor the single-sourcing assertion on the parse tree; both AC4
      mutations measured.
- [ ] T6 Re-run the audit (stamp columns only), `devtools::test()`, full
      `check(args = "--no-manual")`, run with nothing else in the session
      touching the R library.

## Work log

- 2026-08-09: created by /milestone-plan, from M79's return-3 thrash routing.
- 2026-08-09: plan gate chose enumerating the abort domain by parsing the script file over sourcing it and over a text sweep; a sourced environment is what failed twice (the domain is whatever the test loads), and a text sweep matches comments and string literals, which is the defect AC4 exists to fix. Falsified by an abort site the parse walk cannot see that a reader can.
- 2026-08-09: plan gate chose settling the twice-failed criterion here over a /milestone-brief escalation, offered per M79's thrash trigger (b): the repair is named by the bounded-promise rule (narrow the promise until a stated procedure settles it), not by a judgment the session lacks. Falsified by AC1 or AC2 failing again by a third mechanism of the same shape.
- 2026-08-09: plan gate chose folding the single-sourcing guard in over leaving it a candidate row; it is the same false-coverage shape and the same repair, and it stands behind a clause M79 is shipping. Falsified by the parse-tree re-anchoring proving unrelated to the registry work at implement time.
- 2026-08-09: criteria audit ([O], fresh context, authored none of the criteria) returned findings on 4 of the 5 drafted criteria plus M79's amended one; all adopted. The load-bearing ones: AC4 was unsatisfiable as drafted (no `circumplex:::instrument_names` call exists; `:414` is a `get()` call, whose string literal also satisfies today's grep); AC1 left its matching and keying rules to the reader, "the AC5 failure mode in miniature"; AC2 promised one fixture per registry entry where `"source note not found: "` covers two sites; and AC3's mutation clause was defeatable by a fixture missing the five required names. The auditor verified the run-block invisibility (12 sites counted vs 13 parsed on a planted mutant) and the FAIL 0 / PASS 69 baseline.

## Decisions

## Review
