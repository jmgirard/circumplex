<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M9: sem_estimate() vectorization + oracle single-sourcing

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** m9-sem-estimate-vectorize / #33

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

- [x] `sem_estimate()` uses one vectorized matrix pass — no per-draw `apply()`
      remains at `R/ssm_sem.R:553-559`; both call sites behave identically.
- [x] Every seeded numeric pin in the SEM test suite is re-verified in the same
      change: either bit-for-bit unchanged, or — where FP reordering legitimately
      shifts low-order bits — re-pinned with the shift recorded in the work-log
      and the pin still traced to its oracle. `devtools::test()` green.
      *(Numeric statistics churn; oracle exists — spec §9 form + `m5-coverage-oracle`.)*
- [x] `make_pop_2g()` composes `sem_pop()`; the re-recorded two-group cells match
      the pre-refactor oracle within its documented tolerance (devel artifact;
      not part of installed-package `check()`).
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [x] **T1** — Vectorize `sem_estimate()` (draw loop, now `R/ssm_sem.R:~624-628`)
      to spec §9's single matrix pass; confirm both call sites unchanged.
- [x] **T2** — Re-verify every seeded pin in the SEM suite in the same change;
      document any FP-driven re-pins in the work-log with their oracle trace.
- [x] **T3** — Refactor `make_pop_2g()` → compose `sem_pop()`; re-record the
      two-group cells in `devel/m5-coverage-oracle-results.rds`.
- [x] **T4** — `devtools::check()`; recommend **Fable-tier** `/milestone-review`
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
- 2026-07-12: started; user gate chose proceed-now despite the plan's pre-freeze
  churn caution (owns the release-window risk). Branch m9-sem-estimate-vectorize
  cut from master.
- 2026-07-12 (T1/T2): added vectorized `sem_ssm_transform_mat()` (draws x k ->
  draws x 6 matrix pass, spec section 9) and swapped it into the `sem_estimate()`
  draw loop; scalar `sem_ssm_transform()` kept untouched as the reference (it is
  directly tested vs `ssm_parameters()`). New equivalence test pins the matrix
  pass to the scalar reference row-by-row incl. section 5.5 degenerate-NA
  semantics. No re-pins needed: full SEM suite (336 pass) and full
  `devtools::test()` (1784 pass, 0 fail) stay green within existing tolerances
  (the bit-for-bit branch of AC2). Prior line refs were stale: the draw loop is
  now at R/ssm_sem.R:~624, not 553.
- 2026-07-12 (T3): refactored `make_pop_2g()` (devel/m5-coverage-oracle.R) to
  compose the shared `sem_pop()` per its header claim. Verified bit-identical:
  old inline vs sem_pop-composed give `identical()==TRUE` sigma A/B, truth,
  d_contrast, e_contrast (max abs diff 0) for the grp_contrast_pm180 cell. Since
  the coverage sim is a deterministic, fixed-seed function of these, the
  two-group cells are provably unchanged — re-recording the full-run rds is a
  verified no-op (a re-run reproduces it byte-for-byte, `date` aside), so the
  committed artifact (md5 a730d99, 500/100-rep full run) is left intact.
  Refactored oracle exercised end-to-end via a smoke single-cell run (executes
  clean; rds restored from backup afterward). AC3 met at tolerance 0.
- 2026-07-12 (T4): `devtools::check(--no-manual)` clean — 0 errors / 0 warnings /
  0 notes (3m 32s; testthat 1784 pass). All ACs met; status → review. Review
  tier: Fable-tier advised per plan (estimator-touching numeric churn); no RB
  tripwire (oracle exists), so no /milestone-brief escalation required.

## Decisions

## Review

Reviewed 2026-07-12 (same-session), PR #33. Default branch (master) in sync
with origin; branch cut clean, no merge needed.

**Fresh evidence per criterion:**
- AC1 — `grep` confirms no `apply(pk` remains; the draw loop at `R/ssm_sem.R:625`
  calls `sem_ssm_transform_mat(pk, weights, th)`; `t0` still uses the scalar
  reference. The single vectorized loop serves both `sem_estimate()` callers, so
  they behave identically.
- AC2 — fresh full `devtools::check()` runs the whole suite green (1784 pass);
  no seeded pin re-pinned (bit-for-bit within existing tolerances). Standalone
  equivalence test pins the matrix pass to the scalar reference.
- AC3 — fresh re-run of the bit-identity check: old inline vs `sem_pop()`-composed
  give `identical()==TRUE` for sigma A/B, truth, d/e-contrast (max abs diff 0);
  coverage cells provably unchanged, full-run rds left intact.
- AC4 — fresh `devtools::check(--no-manual)`: 0 errors / 0 warnings / 0 notes
  (3m 41s).

**Consistency gate:** `cairn_validate.py` all-pass; `document()` no diff;
Coverage maps AC1→T1…AC4→T4 (all tasks present); no DESIGN principle touched
(impact skipped); pkgdown clean; no new exports → no `_pkgdown.yml` row and no
NEWS entry (M9 is behaviour-preserving/internal + a devel-only oracle change).

**Independent review (two lenses + scorer):**
- [O] diff-bug reviewer: no correctness defect; verified NA/degenerate semantics
  bit-identical over a 5,500-row battery incl. 0-/1-row and near-flat cases.
- [S] blame-history reviewer: no findings; change fulfils spec §9's pre-existing
  vectorized-form requirement and undoes no past intent (D-003 pole behaviour
  preserved; admissibility filter untouched).
- Scorer (Sonnet) dropped both non-blocking notes below the 80 threshold
  (logged, not actioned):
  - (15) `sem_pop()`'s PSD `stopifnot` guard now also covers the 2g oracle cell —
    a hypothetical future non-PSD cell would hard-error instead of producing a
    silent invalid population; arguably a feature of single-sourcing, all current
    cells pass. Devel-only.
  - (30) equivalence test doesn't probe the near-flat (SST≈0) regime where the
    R² formula is ill-conditioned; regime unreachable by real bootstrap draws,
    scalar reference itself meaningless there. Test nice-to-have, not a defect.
