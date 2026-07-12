# M9: sem_estimate() vectorization + oracle single-sourcing (done)

- **Shipped:** 2026-07-12 · PR #33 (squash `7019fcc`) · merged to master.
- **Goal:** replace `sem_estimate()`'s per-draw `apply()` with one vectorized
  matrix pass (spec §9) and single-source the two-group coverage oracle onto
  `sem_pop()`, re-verifying every seeded pin.

## Outcome
Behaviour-preserving. `sem_ssm_transform_mat()` (draws×k → draws×6) swapped
into the `sem_estimate()` draw loop, replacing
`t(apply(pk, 1, sem_ssm_transform, ...))`; scalar `sem_ssm_transform()` kept as
the validated reference (tested vs `ssm_parameters()`), with a new equivalence
test pinning the matrix pass to it row-by-row incl. §5.5 degenerate-NA
semantics. `make_pop_2g()` (coverage oracle) now composes shared `sem_pop()`.
- **No re-pins:** suite 1784 pass; `check()` clean (FP reorder within tolerance).
- **AC3 at tolerance 0:** old inline vs `sem_pop()`-composed populations
  `identical()` → cells provably unchanged; full-run results.rds left intact.
- Two-lens review clean; two notes scored 15/30 (logged, not actioned).

## Decisions
- Landed on master pre-freeze (user timing gate, 2026-07-12): estimator numeric
  churn accepted into the v2.0.0 train, fenced by pins + the coverage oracle.
- Scalar `sem_ssm_transform()` NOT folded into the matrix helper — it is
  directly tested and is the reference; two forms, one pinned to the other.
