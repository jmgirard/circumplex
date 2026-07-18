# M29 AC3 — occasions ci_accuracy discrimination oracle (2026-07-17)

Run: `devel/m29-ci-accuracy-occasions-discrimination.R` (reps = 1000,
boots = 400; level-indexed seeds). Results committed as
`devel/m29-ci-accuracy-occasions-discrimination-results.rds` (`smoke = FALSE`);
every gate below was **pre-registered in the script header before the full
run**, and is pinned by the testthat test "committed AC3 discrimination oracle
satisfies the registered gates" (`tests/testthat/test-ssm_occasions.R`).

## Why width, not coverage

Coverage alone is provably blind to a dependence-dropping population: both
replayed procedures cover the paired-contrast truth at nominal even when
simulated from a wrongly-independent population (RR07). The discriminating
observable is interval **width**. Together with the AC2 simulation-coverage
oracle this meets the ≥2-independent-oracle-types bar (simulation-coverage +
invariant + closed-form).

## Arms

For each cell, one dependent occasions object (ρ = 0.5, n = 150, octants) is
built; `Σ̂` is its stacked covariance and `Σ̂₀` its block-diagonal (cross-blocks
zeroed).

- **A (dependent)** — `ssm_ci_accuracy()` on the object.
- **B (zeroed)** — the same object with `Σ̂₀` substituted; the paired contrast
  becomes an independent-difference.
- **C (two-group reference)** — a genuine two-group classic mean contrast
  (occasion 1 → group A, occasion 2 → group B, **fresh** independent units,
  same marginals/means), `structure = "observed"` — the M4-validated
  independent-groups diagnostic.

## Gates (pre-registered) and result

**(A) invariant — B reproduces C** (base cell contrast row). Coverage
`|cov_B − cov_C|` within the 4-SE binomial band; `Median_width` ratio B/C in
`[0.90, 1.11]`, for e/a/d.
Result: cov 0.941/0.951/0.950 (B) vs 0.940/0.946/0.948 (C); width ratio
1.016/1.000/0.991. ✓

**(B) closed-form elevation width** (both cells). The dependent/zeroed
elevation-contrast `Median_width` ratio matches
`√(w′Σ̂w / w′Σ̂₀w)`, `w = (1/p)[−1_p ; +1_p]`, within ±8%.
Result: base 0.701 vs target 0.697 (obs/target 1.005); reversal 0.725 vs 0.727
(0.998). ✓ (the elevation identity is exact; observed error < 1%.)

**(C) displacement reversal sign.** The dependent/zeroed displacement-contrast
`Median_width` ratio is < 1 at Δd = 40 (paired narrower) and > 1 at Δd = 135
(paired **wider** — the D-013/RR06 sign reversal for cos Δd < 0).
Result: 0.691 (base) and 1.126 (reversal). ✓

Runtime 0.6 min.
