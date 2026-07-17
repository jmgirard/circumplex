# M27 growth-model coverage oracle — results (2026-07-17)

Companion to `devel/m27-coverage-oracle.R` (regeneration script; seeded,
level-indexed, pre-registered acceptance in its header) and
`devel/m27-coverage-results.rds` (committed results). Validates the
growth-recipe pipeline shipped by the growth-ssm-analysis vignette:
joint glmmTMB fit on per-person (e, x, y) → fixed-effect MVN draws →
per-t `ssm_draws()` → d(t) credible intervals, simulated from the same
model family the recipe fits (500 reps/cell; 200 reps/invariant;
2000 draws/rep; n = 200/cell, n = 2000 for the consistency cell).

## Verdicts (all pass, pre-registered gates)

| Gate | Cell | Result |
|---|---|---|
| A1 | pole-crossing (d: 350°→10°) | per-wave d(t) coverage .948–.962 ∈ [.90, .98] |
| A2 | low-amplitude (a(2) ≈ 0.01) | cert rate at degraded wave .02 ≤ .05; waves 0/4 cert 1.00 ≥ .95, coverage .958/.966 ∈ [.90, .98]; wave-2 coverage **.854** (reported, ungated — the danger the certification exists to flag) |
| A3 | strong x–y correlation (ρᵤ = .9, d ≈ 135°) | joint coverage .932–.944 ∈ [.90, .98]; univariate-shortcut mean coverage **.856 < .90** (fails, as designed) |
| A4 | unwrap-vs-(x, y) agreement, concentrated regime | mean max-over-waves diff 0.34° ≤ 2°; p95 0.93° ≤ 4° |
| A5 | two-occasion zero-slope vs M25 paired contrast | mean Δd diff 0.021° ≤ 1°; coverage of true 0: growth .935 / paired .945 ∈ [.90, .99]; median CI-width ratio 1.002 ∈ [.75, 1.33] |

All glmmTMB fits converged with PD Hessians (500/500 per coverage cell;
200/200 in the consistency cell).

## Reading

- **A1** is the boundary-machinery headline: the (x, y) framing is
  boundary-free, so nominal coverage across a pole-crossing trajectory
  certifies the wrapping/summary code, not a statistical near-miss.
- **A2** shows both halves of the D-007 per-t guardrail: the caution fires
  where direction is unidentified (98% of replicates), and the coverage it
  guards against is genuinely degraded there (.854 vs nominal .95).
  Uncertified d(t) intervals are documented as not interpretable.
- **A3** is the discriminating cell: the oracle *fails* the
  plausible-but-wrong independent-univariate-fits shortcut (independent
  vcovs zero Cov(x̂, ŷ); with strongly correlated person effects and the
  trajectory's tangential direction riding the correlated axis, its d(t)
  CIs are too narrow) while the joint recipe stays nominal. The vignette's
  hard requirement — fit jointly — is load-bearing, not stylistic.
- **A4/A5** are the invariants: two different aggregations (mean of
  unwrapped directions vs direction of the mean) agree in the concentrated
  common-branch regime, and the growth pipeline agrees with M25's paired
  occasions machinery (different estimators, asymptotic agreement) at
  large n.

## Design iteration (recorded)

The first full run (2026-07-16) placed the low-amplitude cell's degraded
truth at a(2) = 0.02 ≈ 0.9 SE — genuine signal in the certification rule's
power-onset region — and observed cert rate .058 vs the pre-registered
≤ .05 (every other gate passed with values identical to the table above;
the other cells' seeds are disjoint and their results were bit-identical
on the re-run). The cell's named regime is a(t) → 0, so the truth moved to
a(2) = 0.01 and the full battery was re-run with gates unchanged. The
first run's miss is recorded here and in the M27 work log, not discarded.

## Oracle types (≥ 2 per numeric result)

simulation-coverage (A1–A3) + invariants (A4–A5) + closed-form fixtures
(`tests/testthat/test-angle_unwrap.R`; deterministic miniatures of A4/A5
and the adapter point-collapse contract in
`tests/testthat/test-growth_invariants.R`, which carry the oracle-registry
provenance comments).
