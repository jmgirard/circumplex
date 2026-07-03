# circumplex — design notes

Stable architecture and decision rationale. Update when architecture or
conventions change (rare); day-to-day status lives in MILESTONES.md.

## Data flow

```
User API                     Internal                        C++ (src/)
--------                     --------                        ----------
ssm_analyze()          ->    ssm_analyze_means()       ->    mean_scores()
  (dispatch on               ssm_analyze_corrs()       ->    corr_scores(), pairwise_r()
   measures=NULL;                  |
   method chooses         [method = "bootstrap", default]
   the engine below)            ssm_bootstrap()  [boot::boot, stratified by Group;
                                       |            parallel/ncpus passed straight through --
                                       |            master process draws all resample indices
                                       |            before any dispatch, so results are seed-
                                       |            reproducible regardless of parallelism]
                                       |  statistic = bs_function -> ssm_by_group()
                                       v
                            [method = "montecarlo"]
                               ssm_montecarlo()  [asymptotic MVN draws per group: mean-vector
                                       |            covariance (means) or empirical influence-
                                       |            function covariance on the Fisher z scale
                                       |            (correlations, joint across measures);
                                       |            mvn_draws() is PSD-safe (eigendecomposition,
                                       |            not Cholesky), consumes rnorm() once per
                                       |            group in group_ids order]
                                       v
                             ssm_by_group()            ->    group_parameters()  [vectorized]
                                  |                           -> ssm_parameters_cpp()
                             param_diff() [if contrast]
                                  |
                             ssm_replicate_intervals()  [shared by both engines: names replicate
                                  |                        columns via ssm_param_names(), degenerate-
                                  |                        replicate warning, radian class tagging]
                             quantile.circumplex_radian()          [profile d CIs]
                             quantile.circumplex_contrast_radian() [contrast d CIs]
                                  |
                             new_ssm()  ->  circumplex_ssm object

ssm_parameters() / ssm_score()  ->  group_parameters() -> ssm_parameters_cpp()  [descriptive,
                                                            no resampling; deterministic]
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
| Displacement in [0, 360) for profiles | Standard compass-style reporting in the SSM literature. The estimator is `modu(atan2(y, x), 2π)`, whose range is exactly [0°, 360°). **Boundary (G2 decision, 2026-07):** a profile peaking exactly at the 0°/360° pole is reported as ≈360° (deterministically: `y` computes to a tiny *negative* value ~−3e-17, so `atan2` returns a small negative angle that wraps to just under 2π; ≈359.9999°, displayed 360.0). Equivalent to ≈0° — the same direction, the LM pole under octant labeling. We do **not** canonicalize this: it is a measure-zero float artifact for real data, any snap is an arbitrary 0-vs-360 tie-break, and ≈360 matches the package's LM=360 convention. Tests at the boundary accept either ~0 or ~360. |
| Contrast displacement in (-180°, 180°] | A signed angular difference is the shortest rotation; sign carries direction (positive = counterclockwise of comparison). Computed by `angle_dist()`. The contrast's CI is reported on the estimate's branch: near ±180° the circular-mean-centered interval can land on the opposite branch from the `angle_dist` estimate, so both endpoints are shifted by the same multiple of 360° (width and contiguity preserved; identity away from the boundary) so the estimate lies numerically inside an interval it is geometrically inside. Endpoints may therefore exceed ±180°. |
| Contrast = second minus first level | Mirrors the "treatment minus reference" default; direction is printed in the Label ("Male - Female"). |
| Percentile bootstrap CIs, stratified by group | Zimmermann & Wright (2017) generalization; stratification preserves group n's. BCa was considered and dropped (ROADMAP M2, 2026-07): undefined for circular displacement, so it would be permanently mixed-method per parameter. A Monte Carlo alternative (asymptotic MVN sampling distribution, propagated through the SSM transformation) ships instead as an opt-in `method`; percentile bootstrap stays default for reproducibility and field convention. |
| Circular CI method | Bootstrap displacement replicates are centered on their circular mean, unwrapped to (-π, π], quantiled linearly, re-wrapped. Valid when replicates are concentrated (amplitude reliably > 0); meaningless for flat profiles — hence the interpretation guardrails (fit ≥ .70, amplitude CI excluding 0). |
| Fit = 1 − SSE/SST (R²) | Gurtman's prototypicality; denominator `var(scores) * (n-1)`. Undefined for zero-variance profiles. |
| Closed-form estimator (2/n Σ s·cos, 2/n Σ s·sin) | Equals OLS iff angles are equally spaced around the circle (orthogonal design). For unequal spacing it is the conventional Gurtman estimator, not least-squares; documented, with an OLS option under consideration. |
| Degenerate profiles → NA at machine-noise tolerance | Flat profile (sd ≤ 8·ε·n·max\|s\|): displacement and fit are NA. Zero amplitude with real variance (pure higher harmonic): displacement NA, fit exactly 0. The tolerance is float-cancellation scale only (~13 orders below real variation) — small real amplitudes are never NA'd; their unreliability is the CI's job (plus G1 guardrails). C++ returns NAs silently; R warns once (and once with a count for degenerate bootstrap resamples, whose exclusion makes CIs conditional on estimability). Cannot test `var == 0` exactly: a constant vector of a non-representable value (e.g., 0.1) has var ≈ 2e-34. |

## Reproducibility

`ssm_analyze()` is the package's only entry point that consumes R's global
RNG stream (`ssm_score()`/`ssm_parameters()` and the tidying functions are
deterministic). Call `set.seed()` immediately before `ssm_analyze()` to get
reproducible confidence intervals. What that reproducibility covers, per
engine:

| Engine | Seed guarantee | RNG consumption |
|---|---|---|
| Bootstrap, serial (`parallel = "no"`, the default) | Same seed -> byte-identical `results`. | `boot::boot()` draws all `R` resample index vectors from the master stream up front, then evaluates `bs_function` (deterministic) on each. |
| Bootstrap, parallel (`parallel = "snow"`/`"multicore"`) | Same seed -> byte-identical `results`, **regardless of `ncpus`**. Not a general property of parallel bootstrapping — it holds *because* index generation happens in the master process before any work is dispatched to workers (see table above); the workers only evaluate the deterministic statistic. | Same as serial: one master-stream draw of the full index array. Workers consume no RNG state the results depend on. |
| Monte Carlo (`method = "montecarlo"`) | Same seed -> byte-identical `results`. | One `rnorm()` block per group, consumed via `mvn_draws()` in `group_ids` sort order — see the data-flow diagram. For correlation-based analyses, one group's block covers *all* of its measures jointly (a single draw from the stacked measure-scale correlation vector, then sliced per measure), not one block per measure. Adding a group, a measure, or reordering `scales`/`measures` changes the draw sequence, so results are seed-reproducible for a *fixed* call but not stable across such structural edits (expected: the RNG stream is being asked a different question, not violated). |

What reproducibility does **not** mean:

- **Cross-engine agreement.** Bootstrap and Monte Carlo draw from the RNG
  stream in unrelated ways (resample indices vs. MVN deviates); the same seed
  gives each engine internally-reproducible but mutually unrelated draws.
  Statistical agreement *between* engines is a separate, validated property
  (see the Monte Carlo log entry in MILESTONES.md) — not an RNG-identity one.
- **Stability across `boots`.** Increasing `boots` (draw/resample count)
  changes the draw sequence and therefore the exact CI, by design — more
  draws should tighten the Monte Carlo error, not reproduce the smaller run's
  numbers. Only the *distribution* the CIs approximate is invariant.
- **Stability across R/platform versions.** Ordinary caveat: `set.seed()`
  reproducibility depends on `RNGkind()` defaults, which R occasionally
  changes across major versions.

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
