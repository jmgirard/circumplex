# M81: Enumerate the norms-audit abort registry from the script's parse tree

- **Status:** review
- **Priority:** normal
- **Depends on:** M79
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m81-norms-audit-abort-registry` / https://github.com/jmgirard/circumplex/pull/108

## Goal

Make the norms-audit registry count over the `stop()`/`stopifnot()` calls in
the script's parse tree, run block included — never other abort spellings or
non-call failures — so no such abort escapes by landing where no guard looks.

## Scope

**In:** `tests/testthat/test-norms-audit-markers.R`'s `SCRIPT_ABORTS` registry
and the procedure that enumerates its domain; a message test for
`validate_batch()`'s `stopifnot()` at `data-raw/audit-norms.R:83-85`; and the
single-sourcing assertion in `tests/testthat/test-norms-audit-roster.R`. One
mechanism serves all three: read the script's parse tree, never its text and
never a sourced environment. The enumeration's prior failures, and RR17's
diagnosis of them as one family, are in the work log and in RR17.

**Out:** adding, moving, or widening any guard in `data-raw/audit-norms.R` —
this milestone changes tests and their enumeration, not the script's abort
sites; transient mutation restored byte-clean is not such a change. The
coverage report's column schema → M80. Any change to `data/` → not here. The
denylist sweep, composite site identity, stack-bound fixtures, matcher floors
and the discrimination matrix (RR17 rev 2 BC6–BC11) → the successor milestone.

## Acceptance criteria

- [ ] AC1 The registry's domain is produced by parsing the script, not by
      sourcing it. A helper walks the full expression tree of every top-level
      expression of `parse(file = "data-raw/audit-norms.R")` — including the
      trailing run block that `norms_audit_defs_only = TRUE` skips — and
      collects every call whose deparsed head is one of `stop`, `stopifnot`,
      `base::stop`, `base::stopifnot`. Each collected `stop()` site keys on its
      **message template** — every literal fragment in order, each non-literal
      argument rendered `{}` — not its first fragment alone, which is
      `"source note "` at six distinct sites. A `stopifnot()` site contributes
      one key per condition: a positional condition keyed on its deparsed text,
      a named one on its name, which is the runtime message (AC6). A test
      asserts the collected site set equals the registry's, in both directions,
      by key and count. Mutation-verified with the return-3 mutant (work log,
      T4): a `stop()` planted in the run block reddens the test, where the
      retired guard stayed green at a count of 12 against the walk's 13.
      The enumerated domain is stated, and what lies outside it named, in
      exactly these five texts: this criterion; M81's Goal sentence; the
      section comment at `tests/testthat/test-norms-audit-markers.R:302`; the
      test name at `:464`, which must merely stop claiming "anywhere"; and the
      helper header at `tests/testthat/helper-norms-audit-script.R:12-14`.
      Outside the domain: alternative abort spellings (`rlang::abort`,
      `cli::cli_abort`, `do.call`-dispatched and aliased heads), dynamically
      constructed or runtime-resolved calls, process exits, `warning` promotion
      under `options(warn = 2)`, and the non-call failure mechanisms AC2 already
      bounds. No claim of enumerating "all aborts" is made in any of the five.
- [ ] AC2 Every **site** AC1's walk collects — not every registry entry; the
      `"source note not found: {}"` key covers two sites, `:193` and `:224` —
      carries a fixture that provokes that site, and a test asserts the fixture
      raises an error matching that site's key rather than bare failure. With
      AC1's set equality the message-tested and parsed sets are identical. The
      promise is bounded by AC1's walk and claims nothing about errors raised
      by other mechanisms, which no procedure here enumerates.
- [ ] AC3 `validate_batch()`'s `stopifnot()` at `data-raw/audit-norms.R:83-85`
      is registered and message-tested under AC1 and AC2 for each of its two
      conditions — a non-data-frame batch, and one missing a required column —
      each asserted by the failing condition's own deparsed text rather than by
      any error, and each mutation-checked. The non-data-frame fixture carries
      all five required names, so dropping `is.data.frame(batch)` cannot abort
      through the sibling condition instead.
- [ ] AC4 The single-sourcing assertion in
      `tests/testthat/test-norms-audit-roster.R` is stated over the script's
      parse tree — a call resolving `instrument_names` from the package
      namespace — so neither the doc comment at `data-raw/audit-norms.R:404`
      nor any string literal satisfies it. Mutation-verified (work log, T5):
      replacing the call with a hard-coded roster copy reddens it while the
      comment stands, which the retired text grep does not; the
      `circumplex:::` accessor switch stays green, for the `:::` shape.
- [ ] AC5 (RR17 BC5) On the finished branch each of these reproduces at the
      recorded FAIL count (tolerance 0; PASS counts non-decreasing against the
      recorded 76 and 17, measured per test file in isolation), each mutation
      restored byte-clean: the two AC3 mutations (FAIL 2 each), the AC4
      mutation (FAIL 1) and its control (FAIL 0), and the T4 run-block mutant
      (FAIL 1). `devtools::test()` FAIL 0 suite-wide with this milestone's two
      test files WARN 0 in isolation; `devtools::check(args = "--no-manual")`
      0/0/0; re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
      `data-raw/norms-audit-coverage.csv` unchanged but for their stamp columns
      (`generated`, `script_commit`, `data_commit`), compared column by column.
- [ ] AC6 (RR17 BC1+BC2) The enumerator treats every named argument of a
      collected `stopifnot()` call as one condition keyed on its name, except
      the names in `setdiff(names(formals(stopifnot)), "...")`, on which it
      raises an error naming the call deparsed; likewise on any `stop()`
      argument named other than `call.`/`domain`, which R concatenates into the
      runtime message while the template drops it. A named condition carries a
      kind distinct from a positional one, which AC7's matcher keys on.
      Mutation-verified against the baselines RR17 rev 2 BC1/BC2 records,
      script restored byte-clean after each: the planted `divisor` guard moves
      the collected count 14 → 15 and reddens set-equality; both `exprs` forms
      redden.
- [ ] AC7 (RR17 BC3) A named-form condition site is matched by full string
      equality of `conditionMessage()` with the site's key — no stem, no regex
      — verified by a unit test driving `expect_abort_at_site()` with a
      synthetic site where the exact message passes and any strict superstring
      or substring fails. Every test asserting a `stopifnot` message pins the C
      locale (`LANGUAGE=C`, `LC_MESSAGES=C`); RR17 rev 2 BC9 records the
      measured French failure.

**Deviations from RR17 (rev 2).** BC4 folds into AC1, being an amendment to
its text, and its closing universal over "M81 text" is replaced by the
five-text enumeration above — the defect the rev-2 re-audit found. BC5 folds
into AC5, which it restates and extends. BC1+BC2 merge into AC6, which they
declare inseparable; AC6 restores the `stop()` named-argument clause rev 2
dropped from both partitions, and BC2's three literal names become
`formals(stopifnot)`. BC3 becomes AC7, reworded to the same effect. BC9's
locale pin rides along in AC7. BC6-BC11 are deviated out of M81 entirely, to
the successor milestone RR17 itself partitions them into (ROADMAP candidate).
Measured baselines cross-reference RR17 rather than being restated, for the cap.

## Coverage

- AC1 → T1, T4, T7
- AC2 → T2
- AC3 → T3
- AC4 → T5
- AC5 → T6, T9
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 The parse-tree collector, replacing the sourced enumeration.
- [x] T2 `SCRIPT_ABORTS` re-keyed one entry per site, each with its fixture;
      per-site message test and bidirectional set equality.
- [x] T3 The two `validate_batch()` `stopifnot()` cases, the five-name
      non-data-frame fixture, and both mutation checks.
- [x] T4 The return-3 run-block mutant measured against both guards.
- [x] T5 The single-sourcing assertion re-anchored; both AC4 mutations.
- [x] T6 Audit re-run, `devtools::test()`, full `check(args = "--no-manual")`.
- [x] T7 AC6: named-`stopifnot` conditions collected and keyed on their name;
      fail-closed refusal of the `formals(stopifnot)` names and of `stop()`
      named arguments; distinct kind for named conditions. All four mutations
      measured and restored.
- [x] T8 AC7: the equality matcher for named-form sites, its synthetic-site
      unit test, and the C-locale pin across the `stopifnot` message tests.
- [x] T9 AC1's five promise texts; re-run the AC5 regression floor entire.

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
- 2026-08-09: RR17 revision-2 re-audit ([O], fresh context, neither author nor prior auditor), scoped to the five `[M81]` criteria and the partition. Every stated number reproduces (14->14 with FAIL 0 | PASS 76 pre-repair; 14->15 repaired; exprs append 14 unchanged; exprs rewrite 12 of 14; AC3 FAIL 2 each; AC4 FAIL 1 with control FAIL 0; T4 FAIL 1; check 0/0/0), and the partition is separable in both directions. Two must-fixes remain. BC4's closing sentence quantifies over "M81 text", a domain nothing enumerates -- the third occurrence of this defect in the lineage, and wider than revision 1's correctly-bounded "acceptance criterion of M81"; verified false today at `test-norms-audit-markers.R:302` and at M81's own Goal, and its helper citation `:11-14` is off by one (line 11 is a bare `#`). Revision 2 also dropped revision 1's `stop()` named-argument clause from both partitions: verified here, `stop(msg = "x")` keys `""` whose regex is `.` and matches an unrelated error, and `stop("boom ", tail = "TAIL")` keys `"boom "` while raising `"boom TAIL"` -- the same `call_positional_args()` mechanism that returned this milestone.
- 2026-08-09: re-audit line budget: BC1-BC5 verbatim measure 49 lines plus 5 Coverage lines against 37 headroom, and would leave 10 acceptance criteria against a ~7 tripwire, so the split as drawn does not by itself achieve what the split gate was for.
- 2026-08-09: ingested RR17 rev 2 into M81 at the maintainer's gate ruling, which settled both re-audit must-fixes: BC4's universal over "M81 text" replaced by a five-text enumeration, and the `stop()` named-argument clause restored into AC6. Four should-fixes applied (distinct kind for named conditions, PASS non-decreasing per file, `formals(stopifnot)` in place of three literal names, C-locale pin brought forward). Folds and departures are in the Deviations block under the criteria. Plan-owned body 148/149 after compressing the acceptance criteria in one pass and collapsing the six completed tasks to one line each. Status back to in-progress; BC6-BC11 go to a successor milestone, captured as a ROADMAP candidate row.
- 2026-08-09: `Driving RR` left at `—` rather than RR17, and the reason is a defect in how this session ran the escalation, not in RR17. Two authoring errors: the reviewer was told to APPEND a superseding `## Binding criteria` section, so RR17 carries two and the check cannot tell which binds; and it was never given the item shape the check parses (`- BC<n>: ...`), so neither revision is parseable. Substantively, every one of the 11 criteria was reworded, folded, merged or partitioned by the maintainer's gate ruling, so none binds verbatim in any case and a deviations table naming all 11 is equivalent to `—`. The departures and their reasons are recorded in the Deviations block and above. An RR18 reissuing the settled criteria as one parseable section is what would restore mechanical enforcement; the fork is open with the maintainer. RR17 was briefly edited here to satisfy the parser and has been restored byte-identical — the check's own contract is that the RR file is history and never edited.
- 2026-08-13: resumed on a new laptop with no R user library, so nothing could be verified until it was rebuilt: the 2 hard deps (`htmlTable`, `RcppArmadillo`) and 7 Suggests (`brms`, `ggrepel`, `glmmTMB`, `kableExtra`, `OpenMx`, `psych`, `vdiffr`) were installed at the user's approval. Linking then failed on a missing gfortran toolchain (`ld: library 'emutls_w' not found`) even though `src/` is C++ only, R's default `FLIBS` naming `/opt/gfortran/lib` unconditionally; worked around for this session with a scratch `FLIBS=` via `R_MAKEVARS_USER`, which is not committed. CI is unaffected and stayed green throughout. Candidate for a LESSONS line at review.
- 2026-08-13: T7 — the walk now reads named `stopifnot()` conditions (kind `stopifnot_named`, keyed on the name, which is the runtime message) and fails closed on the shapes it cannot key: any `stopifnot()` formal from `setdiff(names(formals(stopifnot)), "...")`, and any `stop()` argument named other than `call.`/`domain`. `formals()` rather than a literal list matters here — this R spells the third formal `exprObject` where RR17 rev 2 spelled it `exprs.env`. Four mutations of `data-raw/audit-norms.R` measured, each restored byte-clean (`git diff --quiet` after): the planted named `divisor` guard moves the walk 14 → 15 and reddens set-equality (FAIL 1 | PASS 75); `stopifnot(exprs = {...})` appended, and the same form rewriting `validate_batch()`'s conditions, are both now refused by name rather than silently miscounted (each FAIL 1) — the append form is the one that previously stayed at 14 with FAIL 0; a planted `stop("boom ", tail = "TAIL")` is likewise refused (FAIL 1). The rules also carry a committed test over synthetic parse trees rather than only these mutations. `test-norms-audit-markers.R`: FAIL 0 | WARN 0 | PASS 83, up from 76.
- 2026-08-13: T8 — named-form sites are matched by `identical(conditionMessage(err), key)`, no stem and no regex; the synthetic-site test drives `expect_abort_at_site()` and reads its FAILURES (via the `expectation_failure` condition and testthat's `continue_test` restart), so it asserts what the matcher accepts rather than only that this file passes: exact 0 failures, strict superstring 1, strict substring 1. The substring case is the one equality buys — the same test measures that the positional form's stem matcher accepts it, so a named site keyed through that matcher would report a truncated message as its own. The C-locale pin is in `expect_abort_at_site()` itself, where the message is both raised and read. **Its mutation returns a negative and is recorded as one:** neutralizing the pin and running under `LANGUAGE=fr` leaves the file at FAIL 0 | PASS 92, because testthat 3e already sets `LANGUAGE=C` inside every `test_that()` block (measured directly — `Sys.getenv("LANGUAGE")` is `fr` outside the block and `C` inside it, with the French verdict appearing only outside). So the pin satisfies AC7 as written and covers `expect_abort_at_site()` called outside a 3e block, but under this suite's own runner it cannot redden and no mutation can show it load-bearing; RR17 rev 2 BC9's French failure is presumably measured outside a test block. Helper restored byte-clean after the mutation. Both files: FAIL 0 | WARN 0 | PASS 92 and 17.
- 2026-08-13: **correction to the T8 entry above, which claimed work that was not in its commit.** The locale-pin inversion restored the helper with `git checkout --` while T8 was still uncommitted, so that revert silently took the matcher and the pin with it, and `df6c3ef4` landed the AC7 test against a helper still on the pre-T8 dispatch. The test did not then redden the way it should have: with `kind` falling through to the `stop()` regex branch, the exact and SUPERSTRING cases both passed (a key is a substring of its own superstring, so the regex matches) and only the substring case failed — one failure, which the next run reported and which was very nearly attributed to T9. Re-applied here and re-measured: FAIL 0 | WARN 0 | PASS 92 and 17. The earlier locale measurements stand, having been taken while the pin was present; what does not stand is the T8 entry's implication that the matcher was committed. Cause: a `git checkout --` used to restore a mutation, over a file also carrying uncommitted work — the mutation-restore step needs to be scoped to the mutated file only, and the mutated file here was the helper itself.
- 2026-08-13: T9 — AC1's five texts now state the `stop()`/`stopifnot()` domain and name what falls outside it: the Goal (amended at the implement gate, three lines in and three out, so the plan-owned body stays 149/149), the registry section comment, the set-equality test name (`no stop()/stopifnot() site the walk collects is unregistered`, dropping "anywhere"), the helper header, and AC1 itself, which already carried it. `norms_audit_abort_sites()`'s own comment lost its "every abort the script contains" wording too — not a sixth statement of the domain, which AC1 caps at five, but a stray claim contradicting them. AC5 floor re-run entire against a correct tree: baseline FAIL 0 (PASS 92 and 17, both WARN 0 in isolation, against the recorded 76/17); AC3 mutations FAIL 2 each; T4 run-block FAIL 1; AC4 FAIL 1 with control FAIL 0; every mutation restored byte-clean and the tree clean after the set. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 6889, the 6 warnings all outside this milestone's files. `devtools::check(args = "--no-manual")`: Status OK, 0/0/0, 7m 28s. Audit re-run via `NORMS_AUDIT_LEDGER`/`NORMS_AUDIT_COVERAGE` into scratch paths, so the committed CSVs were never written: ledger 194 rows x 12 cols and coverage 15 x 5, every column identical bar `generated`, `script_commit`, `data_commit`.
- 2026-08-13: two environment findings that are not this milestone's, recorded for the review gate. The suite's first run showed FAIL 2, both ERRORS in `test-ssm_analysis.R`'s `parallel = "snow"` tests: a socket cluster's workers are fresh R sessions and cannot see a `load_all()`ed package, and `circumplex` was not installed on the new laptop. `devtools::install()` fixed it and both now run and pass. Separately, `test-axes-scaled-fit.R` skips 3 lavaan-internal corroborations here that ran on the old machine, each naming its own reason (`lavaan::lav_fit_cfi is not callable with these arguments`; `fixture was generated under a different R or lavaan version`) — designed environment guards firing on a newer lavaan, so real coverage is lower on this machine than the committed record assumes. Also noted: a bare `testthat::test_file()` does not set `NOT_CRAN`, so `skip_on_cran()` tests silently skip in per-file runs; M81's two files carry none, so their figures are unaffected.
- 2026-08-13: all tasks checked, local checks clean, status in-progress -> review. Acceptance-criterion boxes deliberately left unticked: review ticks them against its own fresh evidence.

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
