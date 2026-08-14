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
meet. Bare `expect_error()` hygiene suite-wide → unbound per BC10.
Runtime-resolved abort names, and `data/` → not here.

## Acceptance criteria

- [ ] AC1 (BC6) **Denylist sweep.** One test walks every call in
      `data-raw/audit-norms.R`'s parse tree and fails, naming the deparsed call,
      on (i) any call whose paren-normalised head deparses to `rlang::abort`,
      `abort`, `cli::cli_abort` or `cli_abort`; (ii) any `do.call` whose first
      positional or `what` argument is the string or symbol of a denied head;
      (iii) any bare denied-head symbol in a non-head position of any call — one
      rule covering `fail <- stop`, `fail <<- stop`, `assign("fail", stop)` and
      `lapply(msgs, stop)` alike. Denied heads for (ii)/(iii): `ABORT_HEADS`
      (`helper-norms-audit-script.R:64`) **plus the four spellings of (i)**,
      widening BC6 (work log). The set is closed, stated as string literals in
      the test source, and enumerates that set alone, never "all aborts". Its
      boundary is an enumerated partition of **source-text** fixtures parsed
      through the sweep's own parse path — denied shapes and near-misses (a
      comment naming `rlang::abort`; `"stop"` outside `do.call`; a denied head
      in head position) — asserted both ways. Mutation-verified on the real
      script for at least `do.call("stop", list("x"))` and `fail <- stop`,
      restored byte-clean and both measured invisible to the walk (RR17,
      2026-08-09, at 14 sites; 19 today, re-measured 2026-08-14).
- [ ] AC2 (BC7) **Composite site identity with ordinal.** Every registry entry
      and collected site carries `(kind, enclosing top-level binding —
      `"<run>"` for run-block sites, key, ordinal)`. A collected site's ordinal
      is assigned in source order; a registry entry **declares** its ordinal,
      and registry construction errors on two entries with identical declared
      identity. The ordinal distinguishes only entries otherwise identical. The
      `:500` set-equality test compares the full identity both ways, superseding
      M81's "by key and count". Mutation-verified by one probe at build time and
      four on the comparison path: double registration reddens the build; and,
      each reddening the identity comparison, a registry entry declaring a
      binding no collected site carries anywhere, one declaring the other kind,
      one whose key is corrupted, and one declaring an ordinal no collected site
      carries. Two identical `stop()` calls planted in one function of a scratch
      copy stay jointly satisfiable, their identities differing in ordinal
      alone. Recorded measurement, not a required behaviour (2026-08-14):
      swapping the `source note not found` pair's declared bindings does **not**
      redden — identical in kind, key and ordinal, they map the sorted multiset
      onto itself — so pairwise binding-to-site association is outside a set
      comparison and is AC3's domain. Identity holds no line or column number; the
      comment-insertion invariance check is a standing guard against future
      srcref keying, not verification — it passes against the shipped
      enumerator too (`helper-norms-audit-script.R:36`).
- [ ] AC3 (BC8) **Stack-bound fixtures for shared keys.** The declared
      shared-key pair set and the stack-fixture roster derive from one structure
      and are asserted set-equal, so no future pair can be declared without a
      fixture. For every site in that set — today exactly the `source note not
      found` pair — the per-site test captures the abort's frame stack via a
      calling handler around the thunk with no exiting handler between it and
      the abort, asserts the capture is non-empty (a vacuous capture fails,
      never silently passes), and asserts the **innermost** captured frame whose
      function is a binding of the sourced script environment is `identical()`
      to the binding the site's identity names. Mutation-verified: pointing one
      pair fixture at the other's trigger reddens its binding assertion.
- [ ] AC4 (BC9) **Matcher floors, one home, locale pinned.** Discriminating-
      power checks live in one procedure — matcher construction at
      registry-build time; `expect_abort_at_site()` consumes prebuilt matchers
      and adds no floor. Build and match time are separated because a
      `stopifnot` stem exists only at match time: **at build time** the
      constructor errors on a `stop`-kind key under 15 literal characters; **at
      match time** a constructed `stopifnot` matcher rejects a stem shorter than
      `min(nchar(squish(key)), 40)`. Both sit inside RR17's bands ([10, 20],
      [20, 45]) and keep its headroom, re-measured 2026-08-14 (minimum shipped
      `stop` key 23 characters; 66 left of the longest condition after R's
      truncation). Each floor gets its own probe, a shortened key never falling
      below a floor that tracks it: a key shortened past 15 characters reddens
      the build; a matcher fed a 1-character stem fails, where today it passes.
      Every assertion through `expect_abort_at_site()` or a registry-built
      matcher, AC5's capture included, runs under one shared C-locale pin.
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
      one, each restored byte-clean; BC11's tolerance 0 on the counts is dropped
      as unsatisfiable alongside AC5 (work log). `devtools::test()` FAIL 0
      suite-wide with the touched files WARN 0 in isolation;
      `devtools::check(args = "--no-manual")` 0/0/0; an audit re-run leaves
      `norms-audit-ledger.csv` unchanged but for its three stamp columns and
      `norms-audit-coverage.csv` **byte-identical**, that frame being written
      unstamped.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4, T5
- AC3 → T6
- AC4 → T7
- AC5 → T8
- AC6 → T9

## Tasks

- [x] T1 Denylist sweep over the parse tree (rules i–iii), with the
      accepted/near-miss partition of source-text fixtures.
- [x] T2 Mutation-verify the sweep on the real script (`do.call("stop", …)`,
      `fail <- stop`), restoring byte-clean and recording the measurements.
- [x] T3 Extend the enumerator to composite identity: top-level binding name
      (`"<run>"` for the run block), source-order ordinals, `site_ids()` retuple.
- [x] T4 Rebuild `SCRIPT_ABORTS` with declared binding + ordinal; add the
      duplicate-identity refusal; widen the `:500` test to the full identity.
- [x] T5 AC2's five probes (double registration; wrong binding, kind, key and
      ordinal) plus the comment-insertion guard and the swap measurement.
- [x] T6 Derive the pair set and fixture roster from one structure, assert
      set-equality, add the calling-handler capture with its non-empty and
      innermost-binding assertions, cross-point one fixture to verify.
- [ ] T7 Matcher construction at registry-build time with the 15-character
      `stop` floor; the match-time stem floor; one shared locale pin; a probe
      per floor.
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

- 2026-08-14: T1 done. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7069 (the 6 warnings are pre-existing lavaan warnings in `test-ssm_sem.R` and siblings, none in a file this milestone touches); `test-norms-audit-denylist.R` in isolation FAIL 0 | WARN 0 | PASS 43.
- 2026-08-14: T2 done, both mutants measured against `data-raw/audit-norms.R` at blob 3a9be94c and restored byte-clean (blob re-hashed identical after each). `unused_helper <- function() do.call("stop", list("x"))` -> M81 walk 19 sites (unchanged), `test-norms-audit-markers.R` FAIL 0 | PASS 104, denylist sweep reports `(ii) do.call("stop", list("x"))` and its file goes FAIL 1. `unused_alias <- function() { fail <- stop; fail("x") }` -> walk 19 sites, markers FAIL 0 | PASS 104, sweep reports `(iii) fail <- stop`, FAIL 1. So both are invisible to M81 end to end, not merely at the walk, and both are caught here.
- 2026-08-14: measurement note, and a trap for any later mutation of this script. The first attempt planted each mutant as a BARE TOP-LEVEL expression appended to the file, and `do.call("stop", list("x"))` then reddened `test-norms-audit-markers.R` -- not because the registry noticed the site but because `marker_defs()` loads the script with `sys.source()`, `norms_audit_defs_only = TRUE` skips only the run-block `if`, and a top-level abort therefore executes during the load and fails every test in the file (backtrace named `base::do.call("stop", list("x"))`, max-fails exceeded). Read as coverage that would have been the "reddens for the wrong reason" failure LESSONS already carries. Both mutants were re-planted inside functions that are never called, which is also the realistic shape, and the invisibility claim above is measured on those.

- 2026-08-14: T3-T5 written; NOT checked off in this commit, the full `devtools::test()` verify run being still in flight. `test-norms-audit-markers.R` in isolation FAIL 0 | WARN 0 | PASS 118. Two M81 assertions at `:581`/`:588` pinned the enumerator's exact output shape and were updated to the four-part identity, which AC2 supersedes; the `:507` set-equality test's comment now says what M81's (kind, key) ids could not tell apart.
- 2026-08-14: AMENDMENT, AC2, substantive (wording), user-approved at a mini gate. The clause "swapping the `source note not found` pair's declared bindings reddens the identity comparison" was measured FALSE at T5: `norms_audit_site_ids()` compares sorted identity multisets and the pair is identical in kind, key and ordinal, so the swap is an automorphism (measured 2026-08-14: swap -> identical TRUE; a binding no site carries -> identical FALSE). Amended to record the blindness as a bound owned by AC3 and to require four comparison-path probes (binding, kind, key, ordinal) plus the build-time duplicate refusal. A fresh-context [O] reader asked the audit's three questions of the amended wording first and returned four defects, all fixed before writing: "a binding no collected site carries" was ambiguous between global and local absence (now "anywhere"); the stated structural reason was itself an over-general universal, false for a swap between entries differing in key (narrowed to the true one); the RR17 attribution was wrong, RR17's inseparability rationale being matcher degeneracy over a shared message key rather than identity permutation (dropped); and "three axes" named three mutations against a four-part identity, `kind` and `key` having no comparison-path probe under either wording (now four probes).
- 2026-08-14: plan gate's alternative reconsidered and declined at the amendment gate: regrouping ordinals by `(kind, key)` would make the swap redden and the original wording true, but it departs from RR17 BC7's "distinguishing only entries otherwise identical" and makes ordinals cross-binding, so inserting a site in one function renumbers a site in another; falsified by a case where cross-binding source order is what a reader needs from an ordinal.
- 2026-08-14: the amendment grew the plan-owned body past the cap; the Acceptance criteria section was compressed in one pass, 156 -> 149 lines against the <150 cap.

- 2026-08-14: T3-T5 done. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7083 (7069 before this milestone's tests; the 6 warnings are the same pre-existing lavaan ones, none in a touched file). Markers file in isolation FAIL 0 | WARN 0 | PASS 118.

- 2026-08-14: T6 written, NOT checked off pending the full verify run; markers file in isolation FAIL 0 | WARN 0 | PASS 128. The shared-key roster is derived from the registry by `norms_audit_shared_key_sites()` rather than declared beside it, so a later pair joins the stack loop by existing. Capture measured live: 12 frames per abort, resolving to `parse_source_note` and `source_note_block_tags`; crossing the two fixtures resolves each to its TWIN rather than to NA, so the mutation reddens on the binding and not on an empty capture -- pinned as its own assertion, an NA having reddened the first assertion just as well.

- 2026-08-14: T6 done, `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7093.
- 2026-08-14: T7-T8 written, NOT checked off pending the verify run that covers both (one run, both tasks -- recorded rather than claimed per task). Markers file in isolation FAIL 0 | WARN 0 | PASS 142. T7 moves every discrimination check into `norms_audit_matcher()`, called from the registry builder: the 15-literal-character `stop` floor fails the BUILD (`{}` placeholders excluded, matching anything and so not discrimination), the stem floor is checked at match time because a stem exists only once a message is raised, `expect_abort_at_site()` consumes a prebuilt matcher, the locale pin moved to one shared `norms_audit_with_c_messages()` the matrix also uses, and the unknown-kind refusal moved to the constructor. The degenerate-stem probe also asserts the OLD rule accepted `"i"` against `is.data.frame(batch)`, so it is evidence about the floor and not about the prefix test. T8's matrix asserts the off-diagonal accepting set equals the registry-derived shared-key pairs, with a full-TRUE diagonal (without which a matcher accepting nothing would satisfy the equality) and the count pinned at 2.

## Decisions

## Review
