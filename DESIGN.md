# circumplex — design notes

Stable architecture and decision rationale. Update when architecture or
conventions change (rare); day-to-day status lives in MILESTONES.md.

## Data flow

```
User API                     Internal                        C++ (src/)
--------                     --------                        ----------
ssm_analyze()          ->    ssm_analyze_means()       ->    mean_scores()
  (dispatch on               ssm_analyze_corrs()       ->    corr_scores(), pairwise_r()
   measures=NULL)                 |
                             ssm_bootstrap()  [boot::boot, stratified by Group]
                                  |  statistic = bs_function
                                  v
                             ssm_by_group()            ->    group_parameters()
                                  |                           -> ssm_parameters_cpp()
                             param_diff() [if contrast]
                                  |
                             quantile.circumplex_radian()          [profile d CIs]
                             quantile.circumplex_contrast_radian() [contrast d CIs]
                                  |
                             new_ssm()  ->  circumplex_ssm object

ssm_parameters() / ssm_score()  ->  ssm_parameters_cpp()   [no bootstrap, descriptive]
score(), ipsatize(), norm_standardize(), self_standardize()  [tidying, pre-analysis]
ssm_table(), ssm_plot_circle(), ssm_plot_curve(), ssm_plot_contrast()  [output]
```

## Class system (S3, constructors in R/ssm_oop.R and R/instrument_oop.R)

- `circumplex_degree`, `circumplex_radian` — numeric vectors tagging angular
  units; conversion via `as_degree()`/`as_radian()` generics. All user-facing
  angles are degrees; all trig is radians.
- `circumplex_contrast_radian` — bootstrap replicates of a displacement
  *difference*; its `quantile` method returns values on a continuous branch
  (may be negative or exceed 360°) so CIs straddling 0° stay contiguous.
- `circumplex_ssm` — list: `results` (data frame, one row per profile plus
  optional contrast row last), `scores`, `details` (boots, interval, listwise,
  angles, contrast, score_type), `call`.
- `circumplex_instrument` — list: Scales, Anchors, Items, Norms, Details.
  Instrument data objects live in data/ and are built from data-raw/.

## Statistical conventions and their rationale

| Convention | Rationale |
|---|---|
| LM at 360°, not 0° | Matches published SSM tradition (Wright et al. 2009 tables); keeps `octants()` monotone-free but consistent with norms tables, which store 360. Mixing 0 and 360 breaks `norm_standardize()` matching. |
| Displacement in [0, 360) for profiles | Standard compass-style reporting in the SSM literature. |
| Contrast displacement in (-180°, 180°] | A signed angular difference is the shortest rotation; sign carries direction (positive = counterclockwise of comparison). Computed by `angle_dist()`. |
| Contrast = second minus first level | Mirrors the "treatment minus reference" default; direction is printed in the Label ("Male - Female"). |
| Percentile bootstrap CIs, stratified by group | Zimmermann & Wright (2017) generalization; stratification preserves group n's. BCa is a planned option (ROADMAP M2) but percentile stays default for backward reproducibility. |
| Circular CI method | Bootstrap displacement replicates are centered on their circular mean, unwrapped to (-π, π], quantiled linearly, re-wrapped. Valid when replicates are concentrated (amplitude reliably > 0); meaningless for flat profiles — hence the interpretation guardrails (fit ≥ .70, amplitude CI excluding 0). |
| Fit = 1 − SSE/SST (R²) | Gurtman's prototypicality; denominator `var(scores) * (n-1)`. Undefined for zero-variance profiles. |
| Closed-form estimator (2/n Σ s·cos, 2/n Σ s·sin) | Equals OLS iff angles are equally spaced around the circle (orthogonal design). For unequal spacing it is the conventional Gurtman estimator, not least-squares; documented, with an OLS option under consideration. |

## Key references

- Gurtman (1992) JPSP — SSM foundations; Gurtman & Pincus (2003) — methods.
- Wright, Pincus, Conroy, & Hilsenroth (2009) JPA — group comparison.
- Zimmermann & Wright (2017) Assessment — bootstrapped SSM, interpretation
  benchmarks (fit ≥ .70/.80; |e|, a ≥ .15 "marked" for correlation SSM).
- Browne (1992) Psychometrika — stochastic process model (ROADMAP M4).
- Grassi, Luccio, & Di Blas (2010) Behav Res Methods — CircE, the archived R
  implementation of Browne's model that ROADMAP M4 replaces.

## Dependency policy

Imports kept minimal (boot, ggplot2, ggforce, htmlTable, Rcpp, rlang, stats).
Heavier or optional functionality goes to Suggests with graceful degradation
(ggrepel, kableExtra; lavaan planned for M4). No tidyverse in package code.

## Testing strategy

- Numerical regression tests pin published/known values (test-ssm_analysis.R
  uses seeded bootstraps — changing RNG flow breaks them intentionally).
- vdiffr snapshots for plots; snapshot tests for print methods.
- C++ helpers are tested against base-R equivalents (colMeans, cor pairwise).
- Boundary suite (0°/360°, ±180° contrasts, flat profiles) — being expanded
  under ROADMAP M1; required for any estimation change.
