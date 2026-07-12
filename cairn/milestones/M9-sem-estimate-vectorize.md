<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M9: sem_estimate() vectorization + oracle single-sourcing

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** —

## Goal

Replace `sem_estimate()`'s per-draw `apply()` with one vectorized matrix pass
(spec §9's form) and single-source the two-group coverage oracle onto
`sem_pop()`, re-verifying every seeded numeric pin.

## Scope

**In:** The M5 close-review's numeric-churn pair (legacy ROADMAP "Milestone 5"
items a, b):
- (a) vectorize the `t(apply(pk, 1, sem_ssm_transform, ...))` per-draw loop at
  `R/ssm_sem.R:553-559` into a single matrix pass per spec §9; the two call
  sites (`R/ssm_sem.R:1226,1428`) keep identical behaviour. **Floating-point
  operations reorder under vectorization, so every seeded pin is re-verified in
  the same change.**
- (b) refactor `make_pop_2g()` (`devel/m5-coverage-oracle.R:159-189`) to compose
  the shared `sem_pop()` (per its own header claim, line 65-69) instead of
  hand-building two-group populations; re-record the affected two-group
  coverage cells (`devel/m5-coverage-oracle-results.rds`).

**Out:** DRY refactors (c/d/e/g) → M8. Scalar-count validator (f) → M10.

## Acceptance criteria

- [ ] `sem_estimate()` uses one vectorized matrix pass — no per-draw `apply()`
      remains at `R/ssm_sem.R:553-559`; both call sites behave identically.
- [ ] Every seeded numeric pin in the SEM test suite is re-verified in the same
      change: either bit-for-bit unchanged, or — where FP reordering legitimately
      shifts low-order bits — re-pinned with the shift recorded in the work-log
      and the pin still traced to its oracle. `devtools::test()` green.
      *(Numeric statistics churn; oracle exists — spec §9 form + `m5-coverage-oracle`.)*
- [ ] `make_pop_2g()` composes `sem_pop()`; the re-recorded two-group cells match
      the pre-refactor oracle within its documented tolerance (devel artifact;
      not part of installed-package `check()`).
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [ ] **T1** — Vectorize `sem_estimate()` (`R/ssm_sem.R:553-559`) to spec §9's
      single matrix pass; confirm both call sites unchanged.
- [ ] **T2** — Re-verify every seeded pin in the SEM suite in the same change;
      document any FP-driven re-pins in the work-log with their oracle trace.
- [ ] **T3** — Refactor `make_pop_2g()` → compose `sem_pop()`; re-record the
      two-group cells in `devel/m5-coverage-oracle-results.rds`.
- [ ] **T4** — `devtools::check()`; recommend **Fable-tier** `/milestone-review`
      (estimator-touching numeric churn, per CLAUDE.md model tiers).

## Work log

- 2026-07-12: created by /milestone-plan from the legacy M5 close-review
  follow-ups (items a/b), grounded on file:line locations verified this session
  (`make_pop_2g` confirmed at `devel/m5-coverage-oracle.R:159`, not in the
  package tree). Planned free-standing (no `Depends on: M7`) per user's
  sequencing choice — **caution for implement:** the M5 close-review deferred
  this pair "post-v2.0.0, not pre-freeze because they churn validated code";
  running it before the v2.0.0 freeze (~2026-07-26) risks destabilizing the
  release, so confirm timing before starting. Review tier: Fable + hard re-pin
  AC (user gate, 2026-07-12); no RB — the target form is fixed by spec §9.

## Decisions

## Review
