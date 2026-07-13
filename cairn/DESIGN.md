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
| Displacement in [0, 360) for profiles | Standard compass-style reporting in the SSM literature. The estimator is `modu(atan2(y, x), 2π)`, whose range is exactly [0°, 360°). **Boundary (G2 decision, 2026-07):** a profile peaking exactly at the 0°/360° pole is reported as exactly 360.0° (deterministically: `y` computes to a tiny *negative* value ~−3e-17, so `atan2` returns a small negative angle that `modu(·, 2π)` rounds up to exactly 2π — a classic fmod-at-the-edge artifact, not an underestimate). Equivalent to 0° — the same direction, the LM pole under octant labeling. We do **not** canonicalize this: it is a measure-zero float artifact for real data, any snap is an arbitrary 0-vs-360 tie-break, and exactly 360 matches the package's LM=360 convention. Tests at the boundary accept either ~0 or ~360. |
| Contrast displacement in (-180°, 180°] | A signed angular difference is the shortest rotation; sign carries direction (positive = counterclockwise of comparison). Computed by `angle_dist()`. The contrast's CI is reported on the estimate's branch: near ±180° the circular-mean-centered interval can land on the opposite branch from the `angle_dist` estimate, so both endpoints are shifted by the same multiple of 360° (width and contiguity preserved; identity away from the boundary) so the estimate lies numerically inside an interval it is geometrically inside. Endpoints may therefore exceed ±180°. |
| Contrast = second minus first level | Mirrors the "treatment minus reference" default; direction is printed in the Label ("Male - Female"). |
| Percentile bootstrap CIs, stratified by group | Zimmermann & Wright (2017) generalization; stratification preserves group n's. BCa was considered and dropped (ROADMAP M2, 2026-07): undefined for circular displacement, so it would be permanently mixed-method per parameter. A Monte Carlo alternative (asymptotic MVN sampling distribution, propagated through the SSM transformation) ships instead as an opt-in `method`; percentile bootstrap stays default for reproducibility and field convention. |
| Circular CI method | Bootstrap displacement replicates are centered on their circular mean, unwrapped to (-π, π], quantiled linearly, re-wrapped. Valid when replicates are concentrated (amplitude reliably > 0); meaningless for flat profiles — hence the interpretation guardrails (fit ≥ .70, amplitude CI excluding 0). |
| Fit = 1 − SSE/SST (R²) | Gurtman's prototypicality; denominator `var(scores) * (n-1)`. Undefined for zero-variance profiles. |
| Closed-form estimator (2/n Σ s·cos, 2/n Σ s·sin) | Equals OLS exactly when the angle set satisfies first- and second-harmonic balance (Σcos = Σsin = Σcos2θ = Σsin2θ = 0); equal spacing (p ≥ 3) implies the condition but is not necessary — structured unequal sets can satisfy it (sharpened 2026-07-07 per the M5 spec §2.1; the safe sufficient direction "equally spaced ⇒ identical" that all existing uses rely on is unchanged). Off the balance condition it is the conventional Gurtman estimator, not least-squares; documented. The SEM-based SSM layer (`ssm_sem()`) instead always uses the OLS projection `(BᵀB)⁻¹Bᵀ` — the two functionals coincide for all shipped (equally spaced) instruments and genuinely differ off balance. |
| Degenerate profiles → NA at machine-noise tolerance | Flat profile (sd ≤ 8·ε·n·max\|s\|): displacement and fit are NA. Zero amplitude with real variance (pure higher harmonic): displacement NA, fit exactly 0. The tolerance is float-cancellation scale only (~13 orders below real variation) — small real amplitudes are never NA'd; their unreliability is the CI's job (plus G1 guardrails). C++ returns NAs silently; R warns once (and once with a count for degenerate bootstrap resamples). Exclusion from the confidence intervals is **per parameter, not per replicate**: `ssm_replicate_intervals()` quantiles each parameter column with `na.rm = TRUE`, so a degenerate replicate's undefined displacement (and fit, if flat) is dropped only from that parameter's CI, which is therefore conditional on estimability; its other, well-defined parameters (elevation/x/y/amplitude) still enter their own CIs undisturbed — dropping the whole replicate row would instead bias those CIs (e.g., pull a near-zero amplitude CI away from 0). Cannot test `var == 0` exactly: a constant vector of a non-representable value (e.g., 0.1) has var ≈ 2e-34. |

### CPM confidence intervals: measured coverage (M4/B6 coverage oracle)

Recorded 2026-07-07 from `devel/m4-coverage-oracle.R` (seeded; results in
`devel/m4-coverage-oracle-results.rds` / `-analytic.rds`): 500 replications
per cell, nominal-95% CIs, p = 8 octant truths with ζ = .75 and two β
configurations — "boundary" (.45/.35/.15/**.05**, small trailing harmonic)
and "interior" (.35/.30/.20/**.15**). Bootstrap cells used 1000 resamples.

| Cell | Boot angle | Boot ζ | Boot β | Analytic angle | Analytic ζ | Analytic β |
|---|---|---|---|---|---|---|
| boundary N=250 | .900 | .820 | .792 | .800 | .851 | .875 |
| boundary N=500 | .906 | .884 | .767 | .801 | .904 | .849 |
| boundary N=1000 | .931 | .920 | .771 | .764 | .909 | .876 |
| interior N=250 | .934 | .758 | .894 | .821 | .782 | .922 |
| interior N=500 | .940 | .870 | .882 | .869 | .902 | .922 |
| interior N=1000 | .949 | .936 | .881 | .876 | .948 | .944 |

Analytic-only ladder: interior truths reach the [.90, .98] band at
**N = 2000** and stay; boundary truths stay badly outside it through
**N = 20000** (angle .70–.81) and recover only by **N = 50000** (.934) —
matching the A-review F1 finding. What this record decides:

- **Bootstrap default affirmed, with a documented failure.** The bootstrap
  dominates the analytic method for angles everywhere (.90–.95 vs .76–.88;
  analytic angle coverage does not improve 250→1000 at boundary truths) and
  for ζ at N ≥ 500. But the design's acceptance band ([.90, .98] for the
  default at every N/parameter) **fails**: ζ under-covers at N ≤ 500 (down
  to .758, misses one-sided above truth — ζ̂'s boundary bias, which
  percentile intervals inherit), and β under-covers at boundary truths at
  every N (~.77, flat in N — the classic percentile-bootstrap failure for a
  parameter near its boundary, structural rather than small-sample). The
  BCa/alternative-interval follow-up is recorded in ROADMAP (post-M4 note).
- **`summary()` caution calibrated** (constants in R/cpm_fit.R):
  unconditional below N = 2000; between 2000 and 50000 conditional on the
  fit's own boundary/weak-identification markers (Heywood, removed
  harmonics, min β̂ < .10, Hessian condition > 1e8 — the markers that
  separated the two regimes above — plus the multimodality flag, not
  separately measured but the same regime), with the fired markers named in
  the caution text.
- **T = n·F̂ is not χ²_df at these truths at field N**: seeded KS rejects in
  5 of 6 cells (only interior N=1000 passes, p = .255), tracking the Heywood
  rates (.21–.91 across cells; boundary pile-up makes T a mixture). At a
  well-identified non-octant truth at N = 2000 the KS check passes
  (test-cpm_oracles.R) — this is a boundary-regime effect, not a broken
  statistic, but χ²/RMSEA on octant instruments at field N should be read
  accordingly (W1 vignette material).
- **Heywood solutions are the norm, not the exception, at field N** for
  octant-like truths at ζ = .75: 59–91% of fits at N ≤ 500 contain at least
  one ζ̂ > .995 (driven by the weakly identified alternating/Nyquist mode of
  equally spaced grids interacting with β₀).
- Replicate exclusions were small (used 411–494 of 500 per cell; acceptance
  criterion; zero worker errors) and are counted in the RDS.
- Measurement caveats, checked: the recorded run scored circular-CI
  membership with an estimate-anchored rule that could in principle
  mis-score intervals whose endpoints sit > 180° from the point estimate;
  re-running the most-affected cell (boundary N=250, Heywood rate .79) under
  the anchor-free span rule reproduced every coverage number to all printed
  decimals, so the table is insensitive to the rule. The boundary-cell β
  coverage is not a polish artifact: fit-level polish rates were ≤ .04
  (boundary N=250) and 0.00 elsewhere, so the flat ~.77 reflects percentile
  intervals at a near-boundary β truth, not degenerate zero-width intervals
  from removed harmonics.

### CPM free-scaling analytic CIs: measured coverage (M19 coverage oracle)

Recorded 2026-07-13 from `devel/m4-coverage-oracle.R` stage 3 (`CPM_COV_FREE_ONLY=1`;
seeded; results in `devel/m19-free-coverage-results.rds`): 500 replications per
cell, nominal-95% **analytic (Wald)** CIs for θ/ζ/β under `scaling = "free"`, the
same p = 8 octant correlation truths (ζ = .75, boundary and interior β) as the
diag record above. The correlation-input contract forces σ_pop = 1 — the free
family fits σ as free **nuisance** parameters absorbing finite-N correlation
misfit (median max variance-ratio ≈ 1.00 across every cell confirms σ̂ ≈ 1), so
these truths are the diag truths and the only change is the p bordered σ
parameters. Coverage is conditional on acceptance **and** on a non-singular
information matrix (`cpm_analytic_se` returns NA otherwise); both conditioning
events are counted.

| Cell | used/500 | SE-fail | Angle | ζ | β | KS(T,df) |
|---|---|---|---|---|---|---|
| boundary N=250 | 175 | 288 | .829 | .864 | .874 | .000 |
| boundary N=1000 | 403 | 91 | .733 | .906 | .858 | .000 |
| interior N=250 | 127 | 277 | .841 | .767 | .911 | .034 |
| interior N=1000 | 431 | 61 | .889 | .957 | .947 | .001 |

Analytic-only ladder (angle / ζ / β): **boundary** .70/.88/.85 (N=2000) →
.71/.86/.86 (5000) → .83/.89/.91 (20000) → .91/.92/.94 (50000); **interior**
.93/.95/.95 (N=2000) → .94/.95/.95 (5000) → .95/.95/.94 (20000) →
.95/.95/.95 (50000). What this record decides:

- **The free family's coverage regime is the diag family's.** Interior truths
  reach the [.90, .98] band at **N = 2000** (angle .928) and stay; boundary
  truths stay badly outside it through **N = 20000** (angle .70–.83) and recover
  only by **N = 50000** (.912) — the same two-regime pattern the diag B6 record
  found, as expected once σ̂ ≈ 1. So the diag `summary()` caution constants
  (`cpm_analytic_ci_n_caution = 2000`, `cpm_analytic_ci_n_boundary_caution =
  50000`, boundary markers) are **the correct free-family thresholds — now
  coverage-validated for the free family, not silently reused** (spec §4;
  M18-D3's placeholder unconditional free caution is retired, D-010).
- **The free bordered information matrix is fragile below N ≈ 2000.** The p extra
  σ nuisance parameters make `cpm_analytic_se`'s Hessian singular (NA SE) in
  **55–58% of N = 250 fits** and 12–18% at N = 1000, but ~0% at N ≥ 2000. A free
  analytic CI is therefore often simply undefined at small N — an *independent*
  reason to distrust free analytic CIs below the N = 2000 threshold, reinforcing
  (not weakening) the unconditional caution there. σ̂² itself carries no interval
  ever (D-009).
- **T = n·F̂ is χ²_df only for well-identified interior truths at N ≥ 5000**
  (KS p .84/.76/.88), rejecting at every boundary cell and interior N ≤ 2000 —
  the same boundary-regime effect as the diag family, read the same way.
- **≥2 oracle types (registry).** Two independent oracle types back these CIs,
  recorded here (this repo records oracles by distributed test headers, not a
  central file):
  - **O-M19-cov** — *simulation-coverage*. The full run above
    (`devel/m19-free-coverage-results.rds`, frozen) + a fast live reproduction
    asserting `test-cpm_oracles.R` "free-scaling coverage smoke: interior N=2000
    analytic CIs cover in-band". Source: injected circumplex-correlation truths.
  - **O-M19-se** — *live*. An independent parametric-bootstrap SE (refit the free
    model on data drawn from the fitted Σ̂) cross-checks the FD-Hessian analytic
    SE, asserting `test-cpm_oracles.R` "free-scaling SE cross-check: analytic
    Wald SE agrees with parametric bootstrap". Source: parametric bootstrap.
  The M18 point-estimate ≥2-types bar (frozen Grassi App. A + live OpenMx) is
  separate and already met.

## Reproducibility

**RNG contract (an invariant, not an inventory):** a function consumes R's
global RNG stream **iff its statistical output is stochastic** (resampling
or simulation). Every such entry point documents that fact and follows the
`set.seed()`-immediately-before convention; everything else — including
internal conveniences such as optimizer multi-starts, jitter, or
tie-breaking — must be deterministic and leave `.Random.seed` untouched.
(Restated 2026-07-03 from the earlier "`ssm_analyze()` is the only entry
point that consumes the RNG stream," which froze the then-true inventory
instead of stating the rule that produced it.)

RNG-consuming entry points, now six (`ssm_score()`, `ssm_parameters()`, and
the tidying functions are deterministic): `ssm_analyze()`,
`cpm_fit(ci_method = "bootstrap")`, `cpm_simulate()`,
`ssm_ci_accuracy()` (the latter three landed in M4 — see
devel/m4-browne-design.md §3.5/§8 and devel/m4-ci-accuracy-spec.md §3), and
`ssm_sem()` / `ssm_sem_parameters()` (M5 T3): both `ci_method` settings
consume the global stream — `"mvn"` through `mvn_draws()` (one `rnorm`
block over the free-parameter vector), `"boot"` through a lavaan bootstrap
seed drawn from the global stream — and the lavaan model *fit* itself is
deterministic.

- **`ssm_analyze()`** — call `set.seed()` immediately before it to get
  reproducible confidence intervals. What that reproducibility covers, per
  engine:

| Engine | Seed guarantee | RNG consumption |
|---|---|---|
| Bootstrap, serial (`parallel = "no"`, the default) | Same seed -> byte-identical `results`. | `boot::boot()` draws all `R` resample index vectors from the master stream up front, then evaluates `bs_function` (deterministic) on each. |
| Bootstrap, parallel (`parallel = "snow"`/`"multicore"`) | Same seed -> byte-identical `results`, **regardless of `ncpus`**. Not a general property of parallel bootstrapping — it holds *because* index generation happens in the master process before any work is dispatched to workers (see table above); the workers only evaluate the deterministic statistic. | Same as serial: one master-stream draw of the full index array. Workers consume no RNG state the results depend on. |
| Monte Carlo (`method = "montecarlo"`) | Same seed -> byte-identical `results`. | One `rnorm()` block per group, consumed via `mvn_draws()` in `group_ids` sort order — see the data-flow diagram. For correlation-based analyses, one group's block covers *all* of its measures jointly (a single draw from the stacked measure-scale correlation vector, then sliced per measure), not one block per measure. Adding a group, a measure, or reordering `scales`/`measures` changes the draw sequence, so results are seed-reproducible for a *fixed* call but not stable across such structural edits (expected: the RNG stream is being asked a different question, not violated). |

- **`cpm_fit(ci_method = "bootstrap")`** — call `set.seed()` immediately
  before it for reproducible bootstrap intervals. All point estimates, fit
  indices, and the correlation-matrix path's analytic (Wald) intervals are
  deterministic and never touch the stream; only the raw-data path's
  bootstrap does. As with `ssm_analyze()`'s bootstrap engine, all resample
  indices are drawn from the master stream in one block before any
  refitting, so a given seed yields the same intervals regardless of how
  many replicates are later excluded as degenerate or non-convergent.
- **`cpm_simulate()`** — consumes the stream directly (not via `boot::boot()`):
  draws the common-factor scores, then the unique deviates, in that fixed
  order, so a given seed reproduces the draw exactly. The fit it simulates
  from is itself deterministic.
- **`ssm_ci_accuracy()`** — call `set.seed()` immediately before it. It draws
  one `sample.int()` value from the caller's stream to seed an internal
  L'Ecuyer-CMRG generator, gives every simulated (condition × replicate)
  dataset its own deterministic substream via `nextRNGStream()`, and
  restores the caller's `.Random.seed` and RNG kind on exit — so results for
  a given seed are identical regardless of `parallel`/`ncpus`, and the
  caller's stream is left advanced by exactly that one `sample.int()` draw.

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

## Visualization extension

The public ggplot2 extension (M3) turns the former inline plotting code into
composable pieces: `ggcircumplex()` (canvas), `geom_ssm_point()` /
`geom_ssm_arc()` (polar-native layers), and `scale_x_circumplex()` (angle
axis). `ssm_plot_circle()`/`_curve()` are built on them; `ssm_plot_contrast()`
is a Cartesian difference plot and stays independent.

**Architecture.**

- **Canvas** (`ggcircumplex()` → internal `circle_base()`): the rings, spokes,
  and labels are *drawn geometry* (`ggforce::geom_circle`, `geom_segment`,
  `geom_label`) on a `theme_void()` base with hidden continuous x/y scales in a
  radius space of roughly `[-5, 5]`. It is a plot constructor, not a coord.
- **Point geom** (`GeomSsmPoint` ⊂ `GeomPoint`): the amplitude/displacement →
  Cartesian transform runs in `setup_data()`, which executes *before* position
  scale training, so the computed `x`/`y` train the panel range correctly.
- **Arc stat** (`StatSsmArc` ⊂ `ggforce::StatArcBar`): `compute_panel()` injects
  `x0/y0/r0/r/start/end` from the SSM bounds (with the 0/360 unwrap) and then
  delegates to the parent via `ggproto_parent()` for the polygon tessellation.
  This is the idiomatic split (a Stat computes positions; the Geom draws).
- **Label resolution**: a shared `resolve_circumplex_labels()` backs both the
  canvas and the axis scale, so identical `angles`/`labels`/`instrument` inputs
  label both contexts consistently.

**Best-practices review (V6 verdict, 2026-07).**

- **`after_stat()`/`after_scale()`**: not used, and correctly so. The arc's Stat
  feeds `GeomArcBar`'s required aesthetics directly (as ggforce itself does),
  and no aesthetic needs post-scale remapping. Their absence is right, not a
  gap.
- **`ggforce` dependency: KEEP** (the acceptance's "keep iff it simplifies
  arcs"). It supplies (a) the annular-sector polygon tessellation
  (`StatArcBar`/`arcPaths`), which `StatSsmArc` reuses by inheritance rather
  than reimplementing — hand-rolling a wrap-aware annular-wedge tessellator is
  exactly the fiddly geometry worth *not* owning — and (b) `geom_circle` for the
  canvas rings. It is already a mature hard dependency (Imports, ≥ 0.3.0);
  dropping it would add risk for no benefit. See also Dependency policy above.

**Known limitations / accepted trade-offs (deliberate for M3, candidates for a
later milestone; do not "fix" casually — each risks the V4 snapshot stability
that byte-identical output depended on).**

- **`amax` is a per-layer parameter, not shared state.** The amplitude→radius
  scale factor lives on both `ggcircumplex()` and each geom, and the caller must
  keep them equal; a mismatch silently misaligns points from the rings. ggplot2
  has no first-class way for a geom to read a plot-level constant. The idiomatic
  fix is a `CoordCircumplex` (or a carrier scale) that owns `amax` and the polar
  transform — a substantial rewrite of the drawn-geometry canvas, deferred.
  A configurable amplitude *center* belongs in that same future scale/coord (as
  its `limits`), not on the constructor: an early exported `ggcircumplex(amin=)`
  relabelled the rings on an `amin..amax` scale while the geoms mapped amplitude
  as `a*5/amax` (center = 0), silently mislabelling the axis. `amin` was removed
  from the public constructor (2026-07-03, R3); `circle_base()` keeps an internal
  `amin = 0` default only.
- **The canvas does not respond to themes.** Because rings/spokes/labels are
  drawn geoms under `theme_void()`, `+ theme_bw()` etc. do not restyle them.
  Themed panel furniture would again require the coord/scale approach above. The
  data layers themselves theme and scale normally.
- **`na.rm` is effectively always TRUE.** The geoms silently drop rows with a
  missing amplitude/displacement (degenerate profiles have no location),
  regardless of the flag — a minor deviation from the ggplot2 convention where
  `na.rm = FALSE` warns. As of R2 (2026-07-03) the higher-level
  `ssm_plot_circle()` compensates by detecting undefined-displacement profiles
  itself and warning by name before the geoms drop them; the raw geoms remain
  silent (they have no profile labels to name).
- **The `GeomSsmPoint`/`StatSsmArc` ggproto generators are not exported** (only
  the layer constructors are). Fine for use; exporting them (with
  `@format NULL`) would let others subclass — a cheap future addition.

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
(ggrepel, kableExtra). OpenMx is in Suggests as a **test oracle only**
(cross-implementation checks in test-cpm_oracles.R, skipped when not
installed). lavaan is in Suggests both as a test oracle and — since M5 — as
the **runtime engine of the SEM-based SSM feature family** (`ssm_sem()`,
`ssm_sem_parameters()`): those entry points gate on `requireNamespace()`
with a clear install-hint error, the package loads and all non-SEM
functionality runs without lavaan, and it is never load-required (amended
2026-07-07 per the M5 spec §7.4). No tidyverse in package code.

## Testing strategy

- Numerical regression tests pin published/known values (test-ssm_analysis.R
  uses seeded bootstraps — changing RNG flow breaks them intentionally).
- vdiffr snapshots for plots; snapshot tests for print methods.
- C++ helpers are tested against base-R equivalents (colMeans, cor pairwise).
- Boundary suite (0°/360°, ±180° contrasts, flat profiles) — being expanded
  under ROADMAP M1; required for any estimation change.
- CPM validation battery (M4/B6, test-cpm_oracles.R + helper-cpm-oracles.R):
  published CIRCUM/CircE oracles transcribed from Grassi et al. (2010) with
  the model-difference triage documented in devel/m4-browne-design.md §11;
  OpenMx/lavaan cross-implementation oracles (Suggests, skip-if-absent);
  sampling-consistency and T-calibration simulation checks (skip-on-CRAN);
  the heavy coverage oracle lives in devel/m4-coverage-oracle.R (stage 3 adds
  the free-scaling analytic-CI coverage, M19; `CPM_COV_FREE_ONLY=1` runs it
  standalone) and is run out-of-band, never by R CMD check; a fast in-suite
  smoke reproduction + the parametric-bootstrap SE cross-check assert in
  test-cpm_oracles.R.
