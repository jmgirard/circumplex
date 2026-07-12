<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M10: Package-wide scalar-count validator

- **Status:** review
- **Priority:** low
- **Depends on:** —
- **Branch/PR:** m10-scalar-count-validator / #34

## Goal

Add one scalar-count predicate to `R/utils.R` and adopt it uniformly across the
`ssm_analyze()` / `cpm_fit()` / `ssm_sem()` families, resolving the two
divergent readings of the CLAUDE.md `is_*()` rule with a recorded decision.

## Scope

**In:** The M5 close-review's cross-cutting validator item (legacy ROADMAP
"Milestone 5" item f):
- Add an `is_scalar_count()`-style helper (or extend `is_count()`,
  `R/utils.R:163-169`, which today checks integer-ness and `>= 0` but **not**
  length-1) with direct tests.
- Replace the hand-bolted `length(x) == 1` scalar-count checks at
  `R/ssm_ci_accuracy.R:184,194,196,198`; `R/cpm_fit.R:1347,1551`;
  `R/ssm_sem.R:1395`; and reconcile the third spelling
  (`is_num(n_groups, n=1L), is_count(n_groups)`) at `R/ssm_sem_syntax.R:254-256`.
- Record the canonical `is_*()` interpretation as a `cairn/DECISIONS.md` D-entry
  (the two families read the CLAUDE.md `is_*()` rule two different ways today).

**Out:** The `is_flag()` length-1-logical sibling (`R/instrument_oop.R:68`) — a
different predicate; leave to a candidate row. SEM DRY → M8; numeric → M9.

## Acceptance criteria

- [x] `R/utils.R` defines a scalar-count predicate checking integer-ness, the
      appropriate `>= 0` / `>= 1` floor, **and** length-1; unit-tested directly
      (rejects length-2, `NA`, non-integer, negative; accepts a valid scalar).
- [x] All identified duplicated scalar-count sites use the helper; each retains
      an equivalent abort on bad input (a test fires each family's validation:
      `ssm_ci_accuracy()`, `cpm_fit()`, `ssm_sem()`).
- [x] A `cairn/DECISIONS.md` D-entry records the canonical `is_*()`
      interpretation and which reading was superseded.
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T3
- AC3 → T2
- AC4 → T4

## Tasks

- [x] **T1** — Add the scalar-count predicate to `R/utils.R` + direct tests.
- [x] **T2** — Resolve the two `is_*()` readings (question-gate at implement);
      author the D-entry recording the canonical interpretation (D-005).
- [x] **T3** — Adopt the helper across the sites in `R/ssm_ci_accuracy.R`,
      `R/cpm_fit.R`, `R/ssm_sem.R`; reconcile `R/ssm_sem_syntax.R:254-256`.
      Assert each validation still aborts on bad input. 18 call sites converted;
      length>1 regression tests added for all three families.
- [x] **T4** — `devtools::check()` clean (0 errors, 0 warnings, 0 notes). No roxygen touched
      (`is_scalar_count()` is internal, plain-comment documented), so
      `devtools::document()` was not required.

## Work log

- 2026-07-12: created by /milestone-plan from the legacy M5 close-review
  follow-up (item f), grounded on the duplicated scalar-count sites verified
  this session. Planned free-standing (no `Depends on: M7`) per user's
  sequencing choice; behaviour is validation-message-only, low freeze risk.
  Carries a convention decision (canonical `is_*()` reading) deferred to a
  question-gate at implement, not pre-decided here.
- 2026-07-12: implemented. `is_scalar_count(x, min=1L)` added (T1), D-005
  recorded (T2), 18 call sites converted across `ssm_ci_accuracy`/`cpm_fit`/
  `ssm_sem`/`ssm_sem_syntax` + length>1 regression tests for all three families
  (T3), `devtools::check()` clean (0 errors, 0 warnings, 0 notes) (T4). Plan line
  numbers had drifted post-M8/M9; actual sites resolved by grep.
  (Earlier commit messages said "15 sites"; the true count is 18 — the amended
  cpm_fit sites were undercounted. No code impact.) Status → review.
- 2026-07-12: review scope-completion amendment. Both independent reviewers
  flagged that the Goal names the `ssm_analyze()` family but the enumerated
  Scope/Tasks omitted `ssm_analyze()`'s own count validators
  (`R/ssm_analysis.R:212` boots, `:217` ncpus — the identical old inline
  pattern), so the NEWS entry naming `ssm_analyze()` overclaimed. Converted both
  to `is_scalar_count()` + added length>1 regression tests (test-ssm_analysis.R).
  Total now 20 call sites; NEWS is accurate. This completes the plan-owned Goal
  rather than expanding beyond it (Scope Out never excluded `ssm_analyze()`).

## Decisions

- 2026-07-12: question gate — (1) predicate shape: new `is_scalar_count(x, min=1L)`
  helper, leaving `is_count()` unchanged as the vectorized `n=` guard; (2) adoption
  scope amended (minor) to also cover the `is_count()`-only scalar sites in
  `R/cpm_fit.R` (107,108,109,479,1321,1322,1325 — p, m, reference, boots) that lack
  a length-1 guard, for cpm_fit internal consistency. Behaviour change is strictly
  stricter (rejects length>1 args that today partially slip through).

## Review

**Evidence (2026-07-12, branch `m10-scalar-count-validator` @ PR #34):**

- **AC1** ✓ — `is_scalar_count(x, min=1L)` defined at `R/utils.R:178`. Direct unit
  test (`test-utils.R`, block "is_scalar_count validates…"): accepts valid scalar
  (`1`, `3L`, `1000`), rejects length-2, `integer(0)`, `NA`/`NA_real_`/`NA_integer_`
  (returns `FALSE` not `NA`), non-integer (`1.5`), negative (`-1`), non-numeric
  (`"1"`, `TRUE`); `min=0L` accepts `0`. `devtools::test()` via `check()`: all pass.
- **AC2** ✓ — 20 call sites use the helper (`grep is_scalar_count( R/` = 20 excl.
  definition: ssm_ci_accuracy 4, cpm_fit 9, ssm_sem 4, ssm_sem_syntax 1,
  ssm_analysis 2). Each family's validation fires on bad input via passing tests:
  `ssm_ci_accuracy()` (`test-ci_accuracy.R`: `reps=0`, `digits=-1`, `reps=c(5,10)`,
  `digits=c(1,2)`); `cpm_fit()`/`cpm_simulate()` (`test-cpm_api.R`:
  `reference/m/boots=c(.,.)`, `n=c(10,20)`); `ssm_sem()`/`ssm_sem_syntax()`
  (`boots/ncpus=c(.,.)`, `n_groups=0/2.5/c(2,3)`); `ssm_analyze()`
  (`test-ssm_analysis.R`: `boots/ncpus=c(.,.)`).
- **AC3** ✓ — `cairn/DECISIONS.md:58` D-005 records the canonical `is_*()` reading
  (length in the predicate name/argument; `is_count()` retained only as the
  internal `n=` guard) and the superseded reading.
- **AC4** ✓ — `devtools::check(args="--no-manual")` on the final tree: 0 errors,
  0 warnings, 0 notes (5m34s).

**Consistency gate:** `cairn_validate.py` exit 0 (all checks pass);
Coverage complete (AC1→T1, AC2→T3, AC3→T2, AC4→T4, all tasks present);
no DESIGN principle changed (impact report N/A); `devtools::document()` no diff;
README.md already newer than README.Rmd, change is internal (no rebuild);
`is_scalar_count()` internal (no export → no `_pkgdown.yml`/NAMESPACE row);
NEWS.md bullet added (user-visible stricter count-arg validation, no milestone #);
no new top-level files.

**Independent review (two lenses + scorer):**
- **[O] diff-bug (Opus)** and **[S] blame-history (Sonnet)** independently
  converged on ONE finding (score ~95, CONFIRMED): the NEWS bullet named
  `ssm_analyze()` but its own count validators (`R/ssm_analysis.R:212,217`) were
  left on the old inline pattern — an overclaim, and a gap against the plan-owned
  Goal (which names the `ssm_analyze()` family). **Fixed now:** converted both to
  `is_scalar_count()` + length>1 regression tests; NEWS now accurate.
- Both reviewers verified the other 18 conversions preserve semantics: all floors
  correct (`digits` → `min=0L`; positive counts → default `min=1L`), all domain
  bounds retained (`p>=3`, `reference<=p`, `n>p`), `is_scalar_count()` short-circuit
  order safe (no NA/length leak). Blame lens: no site ever deliberately accepted a
  vector; the strictening is intended; no D-entry/CLAUDE.md contradiction. (Noted,
  <80, not actioned: pre-existing `Inf` acceptance in `is_count`/`is_scalar_count`,
  untouched by this diff.)
- Findings below 80 excluded from action: 1 (the `Inf` note above).
