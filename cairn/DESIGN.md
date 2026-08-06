# circumplex — design notes

Stable architecture and decision rationale. Update when architecture or
conventions change (rare); day-to-day status lives in ROADMAP.md.

## Purpose & scope

_Elicited at the 2026-08-04 design interview (Jeff); principles formalization
follows in the Design Principles section._

**Purpose.** circumplex gives applied researchers — clinical, personality,
and vocational psychologists analyzing their own data — validated, guarded
tools for instrument-based circumplex analysis: scoring and standardization,
the Structural Summary Method with resampled inference, circumplex measurement
models (Browne's CPM, SEM projection, axes reliability), and publication-ready
tables and circular figures.

**Audience.** Applied researchers first. The statistical guardrails
(the D-007 certification rule, `summary()` coverage cautions, degenerate-profile
NAs) exist to protect non-methodologists from over-reading results and are
design commitments, not conveniences. The expert tier (opt-in engines,
composable geoms, diagnostic outputs) is served through the same surfaces,
two-tiered: simple front doors, expert back doors.

**Contract boundary.** The job is circumplex-construct analysis and ends
there: no general circular statistics (no von Mises fitting, no Watson tests —
that is the `circular` package's territory). The degree/radian classes are
internal boundary tags, not a public circular-data toolkit. The DESCRIPTION
title ("circular data") claims more than the intended surface; the exports are
the contract.

**Capability bar.** A new capability needs (a) a peer-reviewed methodological
source and (b) a feasible independent validation oracle (the ≥2-oracle-types
bar) before it ships. The maintainer's own research program is a legitimate
secondary driver of what enters, but it enters on the same bar.

**Guardrail stance.** Compute anything statistically well-defined and caution
loudly; refuse only ill-defined or wrong-object inputs (refuse-don't-coerce).
Guardrails label honestly — NA with a warning, cautions naming their trigger —
and never block a defensible analysis.

**Structure-assessment surfaces.** `fit_structure()` (descriptive RANDALL),
`cpm_fit()` (formal model test), and `axes_reliability()` (reliability
decomposition) are complementary by design — separate exports answering
different questions, integrated by cross-referencing docs, never a unified
front door.

**Docs mission.** Teaching the field correct SSM practice is part of the
package's mission: vignettes and the pkgdown site are pedagogy, worth their own
milestones, and their prose stays statistically precise because readers absorb
interpretation habits from it.

**API stability.** Post-2.0, exported signatures, return structures, and
printed output are commitments: a break requires statistical cause (a wrong
number) or a gated irreversible-api decision, and ships with a deprecation
cycle.

**Platforms.** The commitment is exactly CRAN's check matrix (three OSes,
r-devel/r-release/r-oldrel, win-builder before submission); anything CRAN
flags blocks a release. Other platforms are fixed on report, best-effort.

**Instrument roster.** A new instrument enters `data/` when its psychometrics
and norms are published, transcribable with provenance, and clear of
permission questions. The bar is data quality, not demand.

**Maintainership.** Single maintainer (Jeff Girard); external PRs are welcome
through the intake path and held to the same oracle and review bars. Design
records exist for the maintainer and the AI workflow, not to onboard
co-maintainers.

**Known fragilities** (confirmed 2026-08-04; each carries a ROADMAP candidate
row): boundary-regime CI coverage failure at field N (documented, cautioned,
but users live in that regime); shipped instrument norms have never been
re-verified against their published sources; CPM fits at field N sit in a
59–91% Heywood regime the cautions describe but users may not absorb. (A
fourth — the `repel = TRUE` branch hand-computing canvas coordinates, D-019's
flag — was stale at capture: M31's coord-aware rewrite had already fixed it;
corrected 2026-08-06.)

## Design Principles

_Adopted at the 2026-08-04 design interview (D-038). IP = inviolable (never
violated in implementation; changing one requires an explicit user decision
recorded as a D-entry). GP = guiding (default stance, tradeable with stated
justification). Numbers are never reused or renumbered._

### Inviolable principles

- IP1: **Statistical correctness outranks all other concerns.** Release
  timing, API stability, convenience, and performance all yield to it.
- IP2: **The angle conventions are fixed.** Degrees [0, 360) in the user
  API with LM = 360 (never 0); contrasts = second minus first, reported in
  (−180°, 180°]; radians internal only. Breaking any of these silently
  corrupts norms matching and published-results comparability.
- IP3: **Every shipped numeric result is validated against ≥2 independent
  oracle types**, and estimation changes revalidate against them (the
  validation-doctrine bar; oracles recorded at the asserting tests).
- IP4: **The RNG contract.** A function consumes the global RNG stream iff
  its statistical output is stochastic; everything else is deterministic and
  leaves `.Random.seed` untouched; a given seed reproduces results regardless
  of parallelism (see Reproducibility below).
- IP5: **Shipped instrument data carries published-source provenance.**
  Norms, scoring keys, and anchors enter or change `data/` only with their
  published source recorded in `data-raw/`. Binds forward from 2026-08-04;
  the pre-existing roster's unverified transcriptions are the norms-audit
  candidate row's debt, not a standing violation.
- IP6: **Estimation changes ship with boundary tests.** Any change touching
  displacement, contrasts, or `src/` includes tests at profiles peaking at
  0°/360°, CIs straddling 0°/360°, contrasts near ±180°, and flat
  (zero-variance) profiles.

### Guiding principles

- GP1: **Circumplex constructs only.** The package does instrument-based
  circumplex analysis, never general circular statistics (the `circular`
  package's territory); the degree/radian classes stay internal boundary tags.
- GP2: **Compute anything well-defined; caution loudly; fail closed.**
  Refusal is reserved for statistically ill-defined or wrong-object inputs
  (refuse-don't-coerce); guardrails label honestly (NA + warning, cautions
  naming their trigger) and never block a defensible analysis; undecidable
  edge cases fail closed (not certified, not computed) rather than guessing.
- GP3: **Minimal dependencies; standard-evaluation API.** Few Imports;
  heavier or optional functionality goes to Suggests with graceful
  degradation; no tidyverse in package code; the user API is standard
  evaluation (character names / numeric indices), per D-014's evidence-based
  rejection of NSE.
- GP4: **Post-2.0 API stability.** Exported signatures, return structures,
  and printed output are commitments; a break requires statistical cause (a
  wrong number) or a gated irreversible-api decision, and ships with a
  deprecation cycle.
- GP5: **Teach the field, precisely.** Pedagogy for applied researchers is
  part of the mission and worth its own milestones; its prose is statistically
  precise (never describe an angular CI excluding 0° as a significance test).
  The venue is tradeable — vignettes/pkgdown today, possibly an ebook later
  (candidate row) — the precision is not.
- GP6: **Scope is the variable.** When something must give, scope yields —
  never the statistics, and never at the demand of a date.
- GP7: **Evidence reopens decisions.** A closed design decision records the
  class of evidence that would reopen it and is revisited only on that
  evidence; modernization or style advocacy alone never re-triggers.

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
ssm_parameters_id()  ->  group_parameters()  [per-person layer: within-person means
      |                                        by id, then per-person transform;
      v                                        deterministic; NA semantics + na_rate]
summary.circumplex_ssm_id()  [circular mean + resultant length of d_i, NA-d strip]
ssm_draws()  ->  [shape A: (e,x,y) per-draw transform (modu-parity wrap);
      |           shape B: group_parameters() per draw]
      v
ssm_replicate_intervals()  [same interval machinery; replicate_label
                            "posterior draws", credible-interval wording,
                            t0 = the adapter's own point summaries]
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
- `circumplex_ssm_id` — a lightly classed data frame (per-person parameter
  table from `ssm_parameters_id()`); the class exists only to carry the
  circular-statistics `summary()` method, so data-frame semantics survive.
- `circumplex_ssm_draws` — standalone list (draws, results, details, call)
  from `ssm_draws()`; deliberately NOT a `circumplex_ssm` subclass so no
  inherited method can mislabel posterior draws as bootstrap output
  (M26 gate decision; subclassing later would be additive, not breaking).
- `circumplex_instrument` — list: Scales, Anchors, Items, Norms, Details.
  Instrument data objects live in data/ and are built from data-raw/.

## Statistical conventions and their rationale

| Convention | Rationale |
|---|---|
| LM at 360°, not 0° | Matches published SSM tradition (Wright et al. 2009 tables); keeps `octants()` monotone-free but consistent with norms tables, which store 360. Mixing 0 and 360 breaks `norm_standardize()` matching. |
| Displacement in [0, 360) for profiles | Standard compass-style reporting in the SSM literature. The estimator is `modu(atan2(y, x), 2π)`, whose range is exactly [0°, 360°). **Boundary (G2 decision, 2026-07):** a profile peaking exactly at the 0°/360° pole is reported as exactly 360.0° (deterministically: `y` computes to a tiny *negative* value ~−3e-17, so `atan2` returns a small negative angle that `modu(·, 2π)` rounds up to exactly 2π — a classic fmod-at-the-edge artifact, not an underestimate). Equivalent to 0° — the same direction, the LM pole under octant labeling. We do **not** canonicalize this: it is a measure-zero float artifact for real data, any snap is an arbitrary 0-vs-360 tie-break, and exactly 360 matches the package's LM=360 convention. Tests at the boundary accept either ~0 or ~360. **CI endpoints and the CPM reported angle (M20, 2026-07-16):** unlike the SSM estimate, a displacement-CI endpoint (`quantile.circumplex_radian`) or CPM reported angle (`theta_deg`) that denotes the pole *is* value-level snapped to 360 (never 0) — closing D-003's parked cosmetic follow-up; CPM's computational radians are untouched (M20-D1). |
| Contrast displacement in (-180°, 180°] | A signed angular difference is the shortest rotation; sign carries direction (positive = counterclockwise of comparison). Computed by `angle_dist()`. The contrast's CI is reported on the estimate's branch: near ±180° the circular-mean-centered interval can land on the opposite branch from the `angle_dist` estimate, so both endpoints are shifted by the same multiple of 360° (width and contiguity preserved; identity away from the boundary) so the estimate lies numerically inside an interval it is geometrically inside. Endpoints may therefore exceed ±180°. |
| Contrast = second minus first level | Mirrors the "treatment minus reference" default; direction is printed in the Label ("Male - Female"). **Occasion contrasts (M25):** second *listed* minus first listed — `names(occasions)` list order (temporal, as supplied), never factor/alphabetical sorting (a `T10`/`T2` pair would silently flip under sorting; regression-tested). Occasions results are group-major/occasion-minor with a conditional-presence `Occasion` column. |
| Oracle records | Numeric-result oracles are recorded as provenance comments at the asserting tests (e.g., `tests/testthat/test-cpm_oracles.R`, `test-ssm_occasions.R`) plus, for simulation oracles, a committed `devel/m<NN>-*-results.rds` with its seeded regeneration script and pre-registered acceptance in the script header (M19/M21/M25 pattern). |
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
| boundary N=250 | 176 | 277 | .831 | .863 | .851 | .000 |
| boundary N=1000 | 422 | 72 | .760 | .914 | .875 | .000 |
| interior N=250 | 146 | 259 | .804 | .754 | .897 | .194 |
| interior N=1000 | 434 | 63 | .884 | .946 | .929 | .042 |

Analytic-only ladder (angle / ζ / β): **boundary** .72/.89/.84 (N=2000) →
.69/.86/.84 (5000) → .80/.87/.89 (20000) → .91/.93/.93 (50000); **interior**
.92/.95/.94 (N=2000) → .94/.96/.95 (5000) → .94/.95/.95 (20000) →
.95/.95/.96 (50000). What this record decides:

- **The free family's coverage regime is the diag family's.** Interior truths
  reach the [.90, .98] band at **N = 2000** (angle .915) and stay; boundary
  truths stay badly outside it through **N = 20000** (angle .69–.80) and recover
  only by **N = 50000** (.914) — the same two-regime pattern the diag B6 record
  found, as expected once σ̂ ≈ 1. So the diag `summary()` caution constants
  (`cpm_analytic_ci_n_caution = 2000`, `cpm_analytic_ci_n_boundary_caution =
  50000`, boundary markers) are **the correct free-family thresholds — now
  coverage-validated for the free family, not silently reused** (spec §4;
  M18-D3's placeholder unconditional free caution is retired, D-010).
- **The free bordered information matrix is fragile below N ≈ 2000.** The p extra
  σ nuisance parameters make `cpm_analytic_se`'s Hessian singular (NA SE) in
  **52–55% of N = 250 fits** and 13–14% at N = 1000, but ~0% at N ≥ 2000. A free
  analytic CI is therefore often simply undefined at small N — an *independent*
  reason to distrust free analytic CIs below the N = 2000 threshold, reinforcing
  (not weakening) the unconditional caution there. σ̂² itself carries no interval
  ever (D-009).
- **T = n·F̂ tracks χ²_df for interior truths at N ≥ 2000** (KS p .36/.65/.16/.46
  at N = 2000/5000/20000/50000), rejecting at every boundary cell below
  N = 50000 and at interior N ≤ 1000 — the same boundary-regime effect as the
  diag family, read the same way.
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

RNG-consuming entry points, now six (`ssm_score()`, `ssm_parameters()`,
`ssm_parameters_id()`, `ssm_draws()` (deterministic transform of draws the
*user* supplies), and the tidying functions are deterministic):
`ssm_analyze()`,
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
| Occasions analyses (`occasions=`, M25) | Same seed -> byte-identical `results`, both engines. | Bootstrap: unchanged — the wide person-row is the resampling unit, so the same single master-stream index draw covers every occasion. Monte Carlo: one stacked (k·p)-dimensional `rnorm()` block per group, sliced per occasion after the draw — adding an occasion (like adding a measure) changes the draw sequence, per the fixed-call rule above. |

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

**Architecture (M31 rewrite; D-019 coord + D-020 ggforce removal).**

- **Coordinate system** (`coord_circumplex()` ⊂ `ggplot2::CoordRadial`): the
  single owner of `amax` (the amplitude→radius scaling) and the
  displacement→angle polar transform. It hard-pins the circumplex convention
  internally — `thetalim = c(0, 360)`, `expand = FALSE`, `start = pi/2`,
  `reverse = "theta"` — so LM=360, the identical 0/360 pole (I3), and the
  seam-straddle short-way wrap (I2) survive the transform; `amax` and the
  configurable amplitude center are the radial limits (`rlim = c(center, amax)`),
  trained once. Rings/spokes/labels are the coord's themed panel grid, so
  `+ theme_*()` restyles them. The coord also owns the **rim ring**: the break
  algorithm places a break at `amax` only by coincidence, so the coord appends
  the rim to the radial breaks itself (M38) and every canvas closes. That
  appended ring carries a blank label — crowding near the rim is governed by
  rendered label width rather than break spacing, and suppressing the crowded
  neighbour would delete a ring the break algorithm chose (M38-D1). Where `amax`
  is already a generated break it keeps its own label and nothing is appended.
- **Canvas** (`ggcircumplex()`): a thin constructor returning
  `ggplot() + coord_circumplex() + <breaks/labels + theme>` (a `geom_blank`
  establishes the extent). It no longer draws geometry — the former
  `circle_base()` is gone.
- **Point geom** (`GeomSsmPoint` ⊂ `GeomPoint`): `setup_data()` drops rows with
  no location and maps amplitude/displacement to the coord's `y`/`x` (no
  cartesian math; the coord owns the transform).
- **Arc geom** (`GeomSsmArc` ⊂ `GeomRect`): `setup_data()` drops incomplete and
  zero-width regions, unwraps a seam-straddling interval by extension
  (`xmax = xmin + span`, may exceed 360) and validates the span; the polar coord
  bends the rectangle into an annular wedge.
- **Label resolution**: a shared `resolve_circumplex_labels()` backs both the
  canvas theta axis and `scale_x_circumplex()`, so identical
  `angles`/`labels`/`instrument` inputs label both contexts consistently.

The three former known limitations — `amax` as un-shared per-layer state, no
configurable center, a theme-frozen canvas — are **resolved** by this rewrite:
each is structurally impossible to reintroduce once the coord owns the transform
(M30 design; D-019). `ggforce` is no longer a dependency (D-020) — the arc is a
coord-bent `GeomRect` and the rings are the coord's r-gridlines; the dead
cartesian helpers `ggrad()`/`ssm_to_cartesian()`/`ssm_radius()` went with it.
The `GeomSsmPoint` / `GeomSsmArc` / `CoordCircumplex` ggproto generators are
exported for downstream subclassing (M32; documented under `circumplex-ggproto`
with `@keywords internal`), alongside the layer/coord constructors that most
users call. `na.rm` follows the ggplot2 convention as an opt-in (M32): the geom
default `na.rm = TRUE` drops degenerate rows silently (and `ssm_plot_circle()`
still names dropped profiles itself), while `na.rm = FALSE` warns with the
dropped-row count before dropping missing/incomplete rows.

## Key references

- Gurtman (1992) JPSP — SSM foundations; Gurtman & Pincus (2003) — methods.
- Wright, Pincus, Conroy, & Hilsenroth (2009) JPA — group comparison.
- Zimmermann & Wright (2017) Assessment — bootstrapped SSM, interpretation
  benchmarks (fit ≥ .70/.80; |e|, a ≥ .15 "marked" for correlation SSM).
- Browne (1992) Psychometrika — stochastic process model (ROADMAP M4).
- Grassi, Luccio, & Di Blas (2010) Behav Res Methods — CircE, the archived R
  implementation of Browne's model that ROADMAP M4 replaces.

## Dependency policy

Imports kept minimal (boot, ggplot2, htmlTable, Rcpp, rlang, stats; `ggforce`
removed in M31/D-020 once `coord_circumplex()` re-owned the arc/canvas geometry).
Heavier or optional functionality goes to Suggests with graceful degradation
(ggrepel, kableExtra). OpenMx is in Suggests as a **test oracle only**
(cross-implementation checks in test-cpm_oracles.R, skipped when not
installed). lavaan is in Suggests both as a test oracle and — since M5 — as
the **runtime engine of the SEM-based SSM feature family** (`ssm_sem()`,
`ssm_sem_parameters()`): those entry points gate on `requireNamespace()`
with a clear install-hint error, the package loads and all non-SEM
functionality runs without lavaan, and it is never load-required (amended
2026-07-07 per the M5 spec §7.4). No tidyverse in package code. The user
API is standard evaluation by design — character names / numeric indices,
never tidy-eval NSE (v1.0.0 removal re-affirmed with evidence: D-014,
`devel/m24-nse-evaluation.md`).

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
