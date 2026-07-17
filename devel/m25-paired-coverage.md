# M25 paired-occasion coverage oracle — results & acceptance (2026-07-16)

Run: `devel/m25-paired-coverage.R` (reps = 500, boots = 600, seeds
level-indexed `1e6*cell + rep`, re-paired arms `+5e5`); results committed as
`devel/m25-paired-coverage-results.rds`; every acceptance below was
**pre-registered in the script header before the full run**. The registered
gates are additionally pinned by the testthat test "committed
coverage-oracle results satisfy the registered bands"
(`tests/testthat/test-ssm_occasions.R`), which reads the committed rds.

## Population

Persons draw stacked two-occasion score vectors MVN(μ, Σ): μ_j a cosine
profile (e_j, a_j, d_j) over the 8 octants (equal spacing ⇒ closed-form SSM
truths exact); Σ isotropic within/across occasions with within-person
correlation ρ = 0.6. Truths: e = 2/2.4, a = 1.5/1.8, d₁ per cell, Δd per
cell. Re-paired arms draw occasion 2 from **fresh persons** (an
independent-groups design through the occasions code path) — see the
oracle-bug note below.

## Coverage (nominal .95; registered band [.91, .98]; small-n bootstrap [.89, .98])

| cell | engine | Δe | Δa | Δd |
|---|---|---|---|---|
| base (Δd=30, n=100) | boot / MC | .930 / .936 | .942 / .944 | .928 / .924 |
| dd_near0 (Δd=2) | boot / MC | .944 / .946 | .938 / .944 | .944 / .948 |
| dd_178 (Δd=178) | boot / MC | .932 / .936 | .948 / .948 | .952 / .948 |
| pole (d₁=355, Δd=20) | boot / MC | .942 / .946 | .946 / .940 | .940 / .942 |
| small_n (n=30) | boot | .962 | .956 | .922 |
| small_n (n=30) | MC (measured, ungated) | .970 | .954 | .928 |
| reversal (Δd=135) | boot / MC | .938 / .942 | .974 / .972 | .946 / .952 |
| base_repaired | boot / MC | .940 / .946 | .940 / .938 | .954 / .946 |
| reversal_repaired | boot / MC | .948 / .956 | .966 / .962 | .952 / .952 |
| k3 profile d (MC) | — | .948 / .950 / .946 (T1/T2/T3) | | |

**All gated cells inside their registered bands.** The small-n MC arm,
measured not gated, shows no material anticonservatism at this population
(.928 worst) — the docs still steer small samples to the bootstrap.

## Conditional efficiency (RR06 R1) — the discriminating result

Var(Δd̂) and Var(Δâ), paired / independent (fresh-persons) baseline,
registered band [0.70, 1.30] × theory, theory = 1 − ρ·cos Δd:

| cell | theory | Δd̂ ratio | Δâ ratio |
|---|---|---|---|
| base (Δd = 30°) | 0.480 | **0.526** | **0.440** |
| reversal (Δd = 135°) | 1.424 | **1.365** | **1.294** |

Paired is **narrower** at Δd = 30° and **wider** at Δd = 135° despite
ρ = 0.6 — the reversal RR06 derived, observed. The unconditional "paired is
narrower" claim fails here by design; the docs print only the conditional
statement (`?ssm_analyze`, Occasions section).

Exact paired-elevation identity Var(Δê) = 2σ²(1−ρ)/(p·n): empirical/theory
ratios 1.032 (base), 1.044 (reversal); registered band [0.80, 1.25].

## Oracle types for the paired contrast (≥ 2 independent bar)

1. **simulation-coverage** — this run (both engines, all cells above);
2. **closed-form** — the paired-Δe textbook interval agreement test
   (in testthat, seeded, pre-registered tolerance 0.15·SE at B = 5000);
3. **invariant** — fresh-persons independence baseline covering at nominal
   through the same code path + boot-vs-MC endpoint agreement at a
   pre-registered 0.30·SE tolerance (testthat; honestly noted as
   non-independent for the shared quantile path — coverage carries that
   weight).

## Oracle-bug note (first full run, discarded)

The first full run implemented the independence baseline as a
**within-sample row permutation** of occasion 2. Group means are
permutation-invariant, so the "re-paired" arm kept the *paired* estimator
distribution while its CIs (correctly) reflected independence — visible as
base_repaired overcoverage (.99) and reversal_repaired undercoverage (.87),
and efficiency ratios ≈ 1 with no dependence signal. The registered
reversal-expecting design flagged it immediately (LESSONS 2026-07-16, M23:
give the oracle a cell that expects the reversal). Fixed by drawing occasion
2 from fresh persons; the paired arms' seeds and results were unaffected.
