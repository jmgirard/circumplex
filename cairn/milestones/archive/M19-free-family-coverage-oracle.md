# M19: CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration (done 2026-07-13)

**Goal:** measure the free-scaling family's analytic (Wald) CI coverage and
replace M18-D3's placeholder unconditional caution with the validated result.

**Outcome:** extended `devel/m4-coverage-oracle.R` (stage 3, `CPM_COV_FREE_ONLY=1`)
with 500-rep free-family analytic-CI coverage on circumplex-correlation truths
(σ_pop=1). Coverage tracks the diag family (σ̂≈1): interior in-band at N=2000
(angle .915), boundary only near N=50000 (.914) — so the diag N-thresholds
(2000/50000) are the correct, now-coverage-validated free thresholds. The free
`summary()` branch now shares the diag N-conditional caution + a σ²-no-interval
note (a clean free fit at N≥2000 prints no mis-coverage caution). New finding:
the bordered σ-Hessian is singular (NA SE) in ~52–55% of N=250 fits (~0% at
N≥2000), reinforcing the small-N caution. σ² carries no interval (D-009).
Validated by 2 oracle types (simulation-coverage O-M19-cov + live
parametric-bootstrap SE cross-check O-M19-se); closed an M18 gap (free caution
shipped untested); T_free collected for the deferred comparison (candidate 3).

**Key decisions:** D-010 (diag thresholds reused, coverage-validated; supersedes
M18-D3); M19-D1 (parametric-bootstrap, not OpenMx, for the live SE cross-check).

**Review:** 3-lens fan-out; 1 finding (scored 85) — oracle seed collision
(`1e3*N` coupled two cells) — fixed on-branch + re-run; conclusion unchanged.
check 0/0/0; suite 422, 0 fail. **PR:** #43. **Deferred (grouped candidate):**
bootstrap θ/ζ/β + σ CIs, T_diag-vs-T_free decision, covariance-matrix input.
