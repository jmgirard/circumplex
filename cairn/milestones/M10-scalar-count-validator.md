<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M10: Package-wide scalar-count validator

- **Status:** in-progress
- **Priority:** low
- **Depends on:** —
- **Branch/PR:** m10-scalar-count-validator

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

- [ ] `R/utils.R` defines a scalar-count predicate checking integer-ness, the
      appropriate `>= 0` / `>= 1` floor, **and** length-1; unit-tested directly
      (rejects length-2, `NA`, non-integer, negative; accepts a valid scalar).
- [ ] All identified duplicated scalar-count sites use the helper; each retains
      an equivalent abort on bad input (a test fires each family's validation:
      `ssm_ci_accuracy()`, `cpm_fit()`, `ssm_sem()`).
- [ ] A `cairn/DECISIONS.md` D-entry records the canonical `is_*()`
      interpretation and which reading was superseded.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T3
- AC3 → T2
- AC4 → T4

## Tasks

- [x] **T1** — Add the scalar-count predicate to `R/utils.R` + direct tests.
- [x] **T2** — Resolve the two `is_*()` readings (question-gate at implement);
      author the D-entry recording the canonical interpretation (D-005).
- [ ] **T3** — Adopt the helper across the sites in `R/ssm_ci_accuracy.R`,
      `R/cpm_fit.R`, `R/ssm_sem.R`; reconcile `R/ssm_sem_syntax.R:254-256`.
      Assert each validation still aborts on bad input.
- [ ] **T4** — `devtools::document()` (if roxygen touched) + `devtools::check()`.

## Work log

- 2026-07-12: created by /milestone-plan from the legacy M5 close-review
  follow-up (item f), grounded on the duplicated scalar-count sites verified
  this session. Planned free-standing (no `Depends on: M7`) per user's
  sequencing choice; behaviour is validation-message-only, low freeze risk.
  Carries a convention decision (canonical `is_*()` reading) deferred to a
  question-gate at implement, not pre-decided here.

## Decisions

- 2026-07-12: question gate — (1) predicate shape: new `is_scalar_count(x, min=1L)`
  helper, leaving `is_count()` unchanged as the vectorized `n=` guard; (2) adoption
  scope amended (minor) to also cover the `is_count()`-only scalar sites in
  `R/cpm_fit.R` (107,108,109,479,1321,1322,1325 — p, m, reference, boots) that lack
  a length-1 guard, for cpm_fit internal consistency. Behaviour change is strictly
  stricter (rejects length>1 args that today partially slip through).

## Review
