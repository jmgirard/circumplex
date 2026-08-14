# M82: Make the norms-audit abort registry discriminating

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M81
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m82-norms-audit-abort-discrimination`

## Goal

Identify and match every registered norms-audit abort site so no fixture
satisfies another's assertion, and sweep for the spellings M81 does not collect.

## Scope

**In:** `tests/testthat/helper-norms-audit-script.R` and the `SCRIPT_ABORTS`
registry in `tests/testthat/test-norms-audit-markers.R` — RR17 rev 2's
successor partition (BC6–BC11): denylist sweep, composite site identity with
ordinal, stack-bound fixtures for shared keys, matcher floors with the C-locale
pin, cross-discrimination matrix — RR17 flags BC7+BC8+BC10 inseparable.

**Out:** adding, moving or widening any guard in `data-raw/audit-norms.R` —
the script stays byte-unchanged (transient mutation restored byte-clean is not
a change). The five `shipped_roster()`/`roster` holes and M80's note-only `NA`
`sample` cell → the standing `audit-norms.R` robustness candidate row, whose
promotion condition (a milestone opening `audit_norms()`) this one does not
meet. Suite-wide bare `expect_error()` hygiene (79 calls, 19 files) → unbound
per BC10. Runtime-resolved abort names, and `data/` → not here.

## Acceptance criteria

- [ ] AC1 (BC6) **Denylist sweep.** One test walks every call in
      `data-raw/audit-norms.R`'s parse tree and fails, naming the deparsed
      call, on (i) any call whose paren-normalised head deparses to
      `rlang::abort`, `abort`, `cli::cli_abort` or `cli_abort`; (ii) any
      `do.call`/`base::do.call` whose first positional argument, or argument
      named `what`, is the string or symbol of a denied head; (iii) any bare
      denied-head symbol in a non-head position of any call — one rule covering
      `fail <- stop`, `fail <<- stop`, `assign("fail", stop)` and
      `lapply(msgs, stop)` alike. Denied heads for (ii)/(iii): `ABORT_HEADS`
      (`helper-norms-audit-script.R:64`) **plus the four spellings of (i)**,
      widening BC6 (work log). The set is closed and stated as string literals
      in the test source; it enumerates that set, never "all aborts". Its
      boundary is tested as an enumerated partition of **source-text** fixtures
      parsed through the sweep's own parse path — denied shapes and near-misses
      (a comment naming `rlang::abort`; `"stop"` as an argument of a call that
      is not `do.call`; a denied head in head position) — asserted both ways.
      Mutation-verified on the real script for at least
      `do.call("stop", list("x"))` and `fail <- stop`, restored byte-clean —
      both measured invisible to the walk (RR17, 2026-08-09, at a collected
      count of 14; 18 calls / 19 sites today, re-measured 2026-08-14).
- [ ] AC2 (BC7) **Composite site identity with ordinal.** Every registry entry
      and collected site carries `(kind, enclosing top-level binding —
      `"<run>"` for run-block sites, key, ordinal)`. A collected site's ordinal
      is assigned in source order; a registry entry **declares** its ordinal,
      and registry construction errors on two entries with identical declared
      identity. The ordinal distinguishes only entries otherwise identical. The
      `:500` set-equality test compares the full identity both ways, superseding
      M81's "by key and count". Mutation-verified on three axes: double
      registration reddens the build; swapping the `source note not found`
      pair's declared bindings (`parse_source_note`, `source_note_block_tags`)
      reddens the identity comparison; two identical `stop()` calls planted in
      one function of a scratch copy stay jointly satisfiable, their identities
      differing in ordinal alone. Identity holds no line or column number; the
      comment-insertion invariance check is a standing guard against future
      srcref keying, not verification — it passes against the shipped
      enumerator too (`helper-norms-audit-script.R:36`).
- [ ] AC3 (BC8) **Stack-bound fixtures for shared keys.** The declared
      shared-key pair set and the stack-fixture roster derive from one structure
      and are asserted set-equal, so no future pair can be declared without a
      fixture. For every site in that set — today exactly the `source note not
      found` pair — the per-site test captures the abort's frame stack via a
      calling handler established around the thunk with no exiting handler
      between it and the abort, asserts the capture is non-empty (a vacuous
      capture fails, never silently passes), and asserts the **innermost**
      captured frame whose function is a binding of the sourced script
      environment is `identical()` to the binding the site's identity names.
      Mutation-verified: pointing one pair fixture at the other's trigger
      reddens its binding assertion.
- [ ] AC4 (BC9) **Matcher floors, one home, locale pinned.** Discriminating-
      power checks live in one procedure — matcher construction at
      registry-build time; `expect_abort_at_site()` consumes prebuilt matchers
      and adds no floor. Build and match time are separated because a
      `stopifnot` stem exists only at match time: **at build time** the
      constructor errors on a `stop`-kind key under 15 literal characters; **at
      match time** a constructed `stopifnot` matcher rejects a stem shorter than
      `min(nchar(squish(key)), 40)`. Both sit inside RR17's bands ([10, 20],
      [20, 45]) and keep its headroom, re-measured 2026-08-14 (minimum shipped
      `stop` key 23 literal characters; 66 left of the longest condition after
      R's truncation). Each floor gets its own probe, a shortened key never
      falling below a floor that tracks it: a key shortened past 15 characters
      reddens the build; a matcher fed a 1-character stem fails, where today it
      passes (`helper-norms-audit-script.R:291-296`). Every assertion through
      `expect_abort_at_site()` or a registry-built matcher, AC5's capture
      included, runs under one shared C-locale pin.
- [ ] AC5 (BC10) **Cross-discrimination matrix.** One test captures each
      fixture's `conditionMessage()` once (pinned per AC4), evaluates every
      registry-built matcher against every captured message, and asserts the
      set of accepting off-diagonal cells *equals* the declared shared-key pair
      set — today the two `source note not found` cells, both directions, and
      nothing else, those cells discriminated by AC3's stack assertion rather
      than exempted by comment.
- [ ] AC6 (BC11, amended at the gate) **Gate floor.** At review each of M81's
      five recorded mutations — two AC3, two AC4 (control FAIL 0), the T4
      run-block mutant — reddens with the M81 test it was recorded against
      **named among the failures**, at a FAIL count no lower than the recorded
      one, each restored byte-clean; BC11's tolerance 0 on the counts is
      dropped as unsatisfiable alongside AC5 (work log). `devtools::test()`
      FAIL 0 suite-wide with the touched files WARN 0 in isolation;
      `devtools::check(args = "--no-manual")` 0/0/0; an audit re-run leaves
      `norms-audit-ledger.csv` unchanged but for its three stamp columns and
      `norms-audit-coverage.csv` **byte-identical**, that frame being written
      unstamped (`data-raw/audit-norms.R:1011-1012`).

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4, T5
- AC3 → T6
- AC4 → T7
- AC5 → T8
- AC6 → T9

## Tasks

- [ ] T1 Denylist sweep over the parse tree (rules i–iii over `ABORT_HEADS` +
      the four denied spellings), with the accepted/near-miss partition built
      from source-text fixtures parsed through the sweep's own path.
- [ ] T2 Mutation-verify the sweep on the real script (`do.call("stop", …)`,
      `fail <- stop`), restoring byte-clean and recording the measurements.
- [ ] T3 Extend the enumerator to composite identity: top-level binding name
      (`"<run>"` for the run block), source-order ordinals, `site_ids()` retuple.
- [ ] T4 Rebuild `SCRIPT_ABORTS` with declared binding + ordinal; add the
      duplicate-identity refusal at registry build; widen the `:500` test to
      the full identity both ways.
- [ ] T5 The three AC2 probes (double registration, swapped pair bindings, the
      two-identical-`stop()` scratch copy) and the comment-insertion guard.
- [ ] T6 Derive the pair set and fixture roster from one structure, assert
      set-equality, add the calling-handler capture with its non-empty and
      innermost-binding assertions, and cross-point one fixture to verify.
- [ ] T7 Move matcher construction to registry-build time with the 15-character
      `stop` floor; add the match-time stem floor; route the locale pin through
      one helper; probe each floor separately.
- [ ] T8 Cross-discrimination matrix: captured messages × registry matchers.
- [ ] T9 Gate floor: M81's five mutations with named tests among the failures,
      full `test()` and `check()`, audit re-run compared (ledger stamp-only,
      coverage byte-identical).

## Work log

- 2026-08-14: created by /milestone-plan. Absorbs the ROADMAP candidate row "Norms-audit abort enumeration, successor to M81" (RR17 rev 2 BC6–BC11), whose promotion condition — M81 merged — was met 2026-08-13 (PR #108).
- 2026-08-14: criteria audit ([O], fresh context) ran and returned 10 findings. Seven fixed before the gate: AC1's stale count (14 → 18 calls / 19 sites), AC1's vacuous comment near-miss, AC2's derived-vs-declared ordinal ambiguity, AC2's missing binding-axis probe, AC3's author-declared pair roster, AC4's constructor-time stem floor and its missing probe, AC4's unenumerated "every test" locale universal. Three went to the gate. AC5 passed all three questions. The two gate-changed criteria (AC1, AC6) were re-asked the audit's three questions before writing: both name the procedure enumerating their own domain (a closed literal set; five enumerated mutations) and neither is blocked by an IP or D-entry.
- 2026-08-14: plan gate chose the named-test AC6 floor over re-recording FAIL counts at the gate, because those counts would not exist until implementation and so could not be checked when written; falsified by a mutation that reddens only tests this milestone adds while the named M81 test stays green.
- 2026-08-14: plan gate chose to widen BC6's rules (ii)/(iii) to the four denied spellings over keeping RR17's `ABORT_HEADS`-only wording, because the narrower form leaves `fail <- abort` and `do.call("cli_abort", …)` undetected for one vector's cost; falsified by the wider rule flagging a legitimate shape in this script or a successor.
- 2026-08-14: plan gate chose test-side-only scope over folding in the `shipped_roster()` robustness row, because that row's promotion condition is unmet and folding it would break the byte-unchanged-script Scope Out; falsified by a shipped instrument reaching one of the five silent paths.
- 2026-08-14: `Driving RR` left `—` for M81's reason, re-verified today — RR17's `- BC<n> \`[tag]\` **Label.**` bullets do not match cairn_validate's `_BC_HEAD`, so all three of its Binding criteria sections parse to zero items and naming RR17 in the slot would fail the check LOUD. BC6–BC11 are ingested by substance with their ids cited on each criterion instead.

- 2026-08-14: in-progress on `m82-norms-audit-abort-discrimination`, cut from master at 23f469f9.
- 2026-08-14: T1 written (denylist sweep + partition). Checkpoint only, T1 NOT checked off: the full `devtools::test()` verify run is still in flight, so nothing here claims a clean suite. The partition caught one defect in its own rule (iii), which reached the `stopifnot` symbol inside `base::stopifnot(x)`'s own head and reported a shipped call as an alias (measured 2026-08-14, "(iii) base::stopifnot"); `::`/`:::` calls are now exempt from (iii) and `fail <- base::stop` is unaffected. `test_file()` on the new file alone: FAIL 0 | WARN 0 | PASS 43.

## Decisions

## Review
