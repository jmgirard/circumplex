# M29 AC2 — occasions ci_accuracy simulation-coverage oracle (2026-07-17)

Run: `devel/m29-ci-accuracy-occasions-oracle.R` (R1 = 1000 diagnostic reps,
R2 = 800 empirical reps, boots = 300; seeds derived from a fixed base with
distinct cell×engine and rep terms that cannot alias, LESSONS 2026-07-13).
Results committed as `devel/m29-ci-accuracy-occasions-oracle-results.rds`
(`smoke = FALSE`); every acceptance below was **pre-registered in the script
header before the full run**. The registered band is pinned by the testthat
test "committed AC2 simulation-coverage oracle satisfies the registered band"
(`tests/testthat/test-ssm_occasions.R`), which reads the committed rds.

## What is validated

`ssm_ci_accuracy()` on an occasions object reports the coverage its interval
procedure would attain in a population like the fitted estimates. The oracle
checks that this **reported** coverage matches the **direct empirical**
coverage of the object's *own* procedure at the *same* plug-in population,
computed independently: fresh datasets are drawn from the object's stacked
`(μ̂, Σ̂)` and the shipped `ssm_analyze()` occasions procedure is run on each
(`boot::boot` for bootstrap; `ssm_montecarlo` for MC), tallying coverage of the
fixed plug-in truths. The diagnostic replays the procedure internally (shared-W
weighted occasion-block means for bootstrap; `ssm_mc_replicates(occ_k=)` for
MC); the direct loop replays it via the shipped analysis path. Agreement within
Monte Carlo error confirms the M29 replay machinery reproduces the procedure it
assesses. The plug-in truths are the object's own point estimates `e/a/d_est`
(the SSM parameters of `μ̂`), so both sides score the identical fixed targets —
isolating the replay machinery from plug-in estimation error (the diagnostic's
documented limitation, not what AC2 tests).

## Cells

Both cells: k = 2, single group, n = 120, ρ = 0.5 (cross-occasion), octant
angles, paired contrast `TRUE`.

- **interior** — occasion 1 at d = 135°, Δd = 40°, amplitudes 1.2/1.4 (both
  well away from zero and the pole). Run on **both** engines.
- **pole** — occasion 1 peaks **on** the 0/360 pole (d = 0°), Δd = 70°,
  exercising the angular displacement-coverage machinery at the seam. Run on
  **bootstrap** (the genuinely-independent engine check; MC shares
  `ssm_mc_replicates()` and is covered by the interior cell).

Every cell×engine exercises the paired-contrast row (AC2).

## Acceptance (pre-registered)

For every cell × engine × profile-row (T1, T2, T2−T1) × parameter (e, a, d),
with reported coverage `r` (R1 reps) and empirical coverage `e` (R2 reps):

    |r − e| ≤ 4·√( r(1−r)/R1 + e(1−e)/R2 ) + 0.010

The 4-SE band (two independent binomial estimates of the same probability) plus
a 0.010 slack absorbs the bootstrap engine's genuine implementation difference
(multinomial weights vs `boot::boot`'s index draw — same law, different
realization). The MC engine shares `ssm_mc_replicates()` on both sides, so its
agreement is tighter.

## Result

All 27 checks (3 cell×engine × 3 rows × 3 params) pass. Largest observed
|r − e| = 0.032 (interior.bootstrap, T1 amplitude) against a band of 0.058;
most differences are below 0.015. Reported and empirical coverage both sit near
the nominal .95 across rows and engines. Runtime 0.7 min on
`detectCores() − 1`.
