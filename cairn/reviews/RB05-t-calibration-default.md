# RB05: T_diag-vs-T_free inference-default decision (M21)

- **Date:** 2026-07-16
- **Output required:** write findings to `cairn/reviews/RR05-t-calibration-default.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

circumplex is an R package (CRAN) for circumplex data analysis. Its CPM
module fits Browne's circular stochastic process model: `cpm_fit()` /
`cpm_engine()` estimate a circumplex correlation structure P(γ) with
parameters θ (angles), ζ (communality), β (harmonic weights), tested by
`T = (N−1)·F̂ ~ χ²_df`. M18 added a second covariance family, CIRCUM
free-scaling (`Σ = D_σ P(γ) D_σ`, `scaling = "free"`), admitted (D-008/D-009)
solely for exact reproduction of published CIRCUM/CircE output. Both families
share one df formula (free adds p σ-parameters AND p diagonal moments;
`R/cpm_fit.R:149-177`), so their T statistics target the same χ²_df.

D-009 deferred one question: whether a T_diag-vs-T_free calibration
comparison "could make the free family the preferable inference default"
(for the model test — the fit χ²/p-value users act on). M21 measured that
comparison (T1, evidence below) and must now decide the default, superseding
D-009's deferral with a new D-entry. The decision touches the exported
inference surface a v2.0.0 CRAN release ships, hence this independent
review (RB tripwire: irreversible-api).

The shipped default today: `cpm_fit()` fits `scaling = "unit"` (the
correlation-structure family); the free family is opt-in. Analytic-CI
trust for both families is governed by a coverage-validated N-conditional
caution ladder (D-010); the free family's σ̂² never carries an interval
(D-009), and its bordered information matrix is singular (NA SE) in ~52–55%
of N = 250 fits, ~13–14% at N = 1000, ~0% at N ≥ 2000 (D-010).

## Materials

- `devel/m21-t-calibration.md` — the T1 analysis summary (read first).
- `devel/m21-t-calibration.R` — its generator: paired design, each replicate
  fits BOTH engines to the same `R = cor(X)` at the two stage-1 circumplex
  correlation truths (boundary/interior), N ∈ {250, 1000, 2000, 5000, 20000,
  50000}, 500 reps/cell, deterministic disjoint seeds.
- `devel/m21-t-calibration-results.rds` — full results incl. per-replicate
  paired T vectors (`results[[cell]]$T_unit`, `$T_free`), calibration
  summaries (`$unit`, `$free`: mean/df, var-ratio, rejection at α = .05 with
  Wilson CI, KS vs χ²_df), paired stats (`$paired`), and the committed
  one-family cross-references (`$committed`).
- `R/cpm_fit.R:117-177` — spec/df construction for both families (df
  identical); `:123-128` the `scaling` argument and default.
- Rerun if desired: `Rscript devel/m21-t-calibration.R` (~4 min, 7 cores;
  reproduced bit-identically twice on 2026-07-16). Smoke:
  `CPM_T_SMOKE=1 Rscript devel/m21-t-calibration.R`.
- Context decisions: `cairn/DECISIONS.md` D-008, D-009, D-010.

Headline T1 numbers (verify against the rds): paired mean `T_free − T_unit`
∈ [−0.044, −0.011] at df = 10 (≤ 0.5% of df), paired correlation ≥ .998
every cell; identical rejection rates and KS regime cell-by-cell (both
families mildly conservative at small/mid N, both nominal by
boundary N = 50000 / interior N = 2000); free-nests-unit violated in 3/5751
replicates by optimizer tail (max +5.5).

## Questions

1. **Decision:** given the T1 evidence, should the unit family remain the
   CPM model-test inference default, with the free family staying the
   opt-in reproduction feature? If you instead find a basis for preferring
   the free family (or for a conditional default), state the evidence and
   the exact rule.
2. **Sufficiency:** is the paired design at 500 reps × 12 cells, restricted
   to both-accepted/both-unpolished/equal-df replicates, sufficient to
   support that decision at the v2.0.0 ship bar — or does the decision need
   more (more reps, other truths within the correlation-input contract,
   polished-replicate stratification, variant B–D spot checks)? Name any
   run you require before the D-entry is written.
3. **Scope caveat:** T1 notes the comparison is only well-posed at
   correlation truths with σ_pop = 1 (no covariance-input path exists;
   `cor()` discards variances). Is the proposed re-trigger — revisit the
   default decision if/when covariance-matrix input ships (D-009 item 4) —
   the right condition, and correctly stated?
4. **User-facing wording:** M21 will apply the decision as documentation
   (`cpm_fit()` roxygen, the CPM vignette wording on which family to use for
   inference). Are there statistical-precision traps to avoid in phrasing
   "the families are calibration-equivalent; use unit for inference, free
   for reproducing published CIRCUM/CircE output" (e.g., overclaiming
   equivalence beyond the measured truths/N range)?

## Constraints

- D-008 (CIRCUM in v2.0.0; date yields to statistics) and D-010 (shared
  N-conditional caution ladder, coverage-validated) are fixed.
- D-009's deferral of THIS decision is the thing being discharged; its other
  holdings (no analytic σ CIs ever; bootstrap default; deferred items 2/4)
  are not up for relitigation.
- The correlation-input contract (`cpm_fit` fits `cor(X)`; no covariance
  path) is a fact of the current package, not a choice this review can
  change.
- Statistical correctness outranks all other concerns (repo doctrine).
  Flag disagreement with any constraint explicitly rather than silently
  working around it.

## Output format

In `RR05-t-calibration-default.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason.
