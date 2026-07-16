# Longitudinal & intraindividual SSM — build-ready design spec (M23)

**Status:** draft for adversarial Fable review (RB06). Successor to Brief E
(`devel/m5-m6-design-questions.md` §M6), whose recommended directions this
spec turns into build-ready decisions. Brief E's `[blocked on M4]` markers
are discharged: `cpm_fit()`, `cpm_simulate()`, and `ssm_ci_accuracy()`
shipped in the v2.0.0 dev line.

**Binding conventions** (CLAUDE.md / DESIGN.md): angles in degrees [0, 360)
in the API with LM = 360, radians internally; contrasts second minus first
in (−180°, 180°] via `angle_dist()`; displacement CIs via circular quantiles
(center on circular mean, unwrap, quantile, re-wrap; pole endpoints report
360 per D-003/M20); closed-form SSM estimator = OLS only under first+second
harmonic balance; minimal dependencies, no tidyverse in package code; no
per-call seed arguments (global RNG contract).

**Plan-gate decisions held fixed** (M23, 2026-07-16): one unified spec;
Bayesian scope = thin draws adapter + brms vignette only (Stan companion
out, criteria in §5.4); builds not merge-gated behind M7 (D-012); this spec
gets an independent Fable review before build milestones are planned.

---

## 1. Repeated-measures API

### 1.1 Decision: wide-format `occasions` blocks on `ssm_analyze()`

`ssm_analyze()` gains one argument:

```r
ssm_analyze(data, scales, angles = octants(), ...,
            occasions = NULL, ...)
```

- `occasions`: `NULL` (current behavior) or a **named list of character/
  numeric vectors**, each selecting the *same* circumplex scales measured at
  one occasion, in the same scale order, all of length `length(angles)`:
  `occasions = list(T1 = c("PA_1", ..., "NO_1"), T2 = c("PA_2", ..., "NO_2"))`.
  When `occasions` is supplied, `scales` must be absent (they are mutually
  exclusive spellings of "which columns are the circumplex scores").
- **Data shape: wide, one row per person.** This is the load-bearing choice:
  the row is already the resampling unit everywhere in the package
  (`boot::boot` over rows, stratified by `Group`), so with persons as rows
  the person-level case (cluster) bootstrap of Brief E Q6.2 **is** the
  existing resampler — each drawn row carries that person's entire set of
  occasion scores, and within-person dependence is preserved
  nonparametrically with zero new resampling machinery. A long-format
  `id`-variable API was rejected for the build: it requires an internal
  reshape-to-wide pass anyway (the estimator consumes per-occasion score
  matrices), adds an id-uniqueness/duplicate-wave validation surface, and
  its one advantage (tidy input) is a user-side one-liner
  (`stats::reshape()` / pivot). A `ssm_analyze_long()` convenience wrapper
  can be a later candidate; it is sugar, not design.
- Each occasion block is scored per group exactly as `scales` is today; the
  result table gains an `Occasion` column (labels = `names(occasions)`,
  defaulting to `T1..Tk`), composed into `Label` the way `Group`/`Measure`
  already are (`build_result_labels()` gains the occasion dimension).

### 1.2 Composition matrix (validated, not emergent)

| Combination | v2.x build | Semantics |
|---|---|---|
| occasions × mean-based, 1 group | **in** | per-occasion profiles, one row each |
| occasions × mean-based × grouping | **in** | occasions within groups; persons stay the resampling unit inside their group stratum |
| occasions × `contrast = TRUE` (2 occasions, 1 group) | **in** | paired occasion contrast, second minus first (§2) |
| occasions × measures (correlation path) | **out** (candidate) | stacked occasion×measure correlation blocks explode the draw core's key space; defer until a concrete use case exists |
| occasions × contrast × grouping | **out** (candidate) | "did the T2−T1 change differ by group" is a difference-of-differences — a new estimand needing its own design pass |

Contrast validation extends the existing rule: `contrast = TRUE` requires
exactly two of {groups, measures, occasions} coexisting as today — the new
legal triple is (1 group, 0 measures, 2 occasions). More than 2 occasions
with `contrast = TRUE` is an error (matching groups/measures).

### 1.3 Missing waves

Default `listwise = TRUE` keeps its natural extension: `na.omit()` on the
wide row — a person missing *any* occasion is dropped from *all* occasions
(complete-cases-across-waves). `listwise = FALSE` (pairwise) means a person
contributes each occasion block they completed; under `contrast`, the paired
CI is then computed over resamples in which each person contributes to both
occasions or neither **per replicate** — but the *point* contrast uses all
available data per occasion. That asymmetry is confusing and easy to get
subtly wrong, so the build ships **listwise-only for occasions** (pairwise +
occasions errors with a clear message), and pairwise support is recorded as
a candidate. Deliberate narrowing, stated in docs; not an accidental
semantics.

### 1.4 What `occasions` is *not*

Not a growth-model interface (no time metric, no trajectories — §4), and
not intraindividual scoring (§3). It is the k-occasion generalization of the
package's existing "several profiles from one sample" analyses, with the
person as sampling unit.

---

## 2. Paired occasion contrasts (dependent resampling)

### 2.1 Bootstrap: the case bootstrap via row resampling

With wide person-rows (§1.1), `boot::boot(data = bs_input, strata = Group)`
already implements the person-level case bootstrap: nothing about the
resampler changes. Per replicate, each occasion's score matrix is computed
from the *same* resampled persons, `ssm_by_group()` produces per-occasion
parameter vectors, and the contrast block is `param_diff(occ2, occ1)` —
the identical second-minus-first convention and `angle_dist()` displacement
difference the group/measure contrasts use. Downstream, the contrast
displacement replicates flow through `quantile.circumplex_contrast_radian()`
and the branch-alignment step in `ssm_replicate_intervals()`
(`R/ssm_bootstrap.R:136-144`) completely unchanged. The correlation-path
measure contrast (2 measures, 1 group) is the existing in-package precedent:
it already computes a dependent contrast from jointly-resampled rows.

### 2.2 Monte Carlo analogue: stack the occasions

The MC engine generalizes by the same move it already uses for measures
(`R/ssm_montecarlo.R:119-149`, joint draws across a group's measures):
stack the k occasion mean vectors into one length-`k·p` vector per group,
estimate its asymptotic covariance as the sample covariance of the stacked
person-level score vectors divided by n (the within-person cross-occasion
covariance enters through the off-diagonal p×p blocks), draw MVN jointly,
transform each occasion block through the closed-form SSM map, contrast via
`param_diff()`. No Fisher-z step on the mean path (as now). The
occasions × measures cell is out (§1.2), so the correlation-path stacking
question does not arise in this build.

### 2.3 Oracle strategy (≥2 independent types per numeric result)

1. **simulation-coverage** (primary; a CI method's oracle is coverage):
   simulate wide two-occasion samples from known bivariate populations with
   controlled cross-occasion covariance (Σ_within specified between occasion
   blocks; population SSM truths set per occasion so the true contrast
   Δd, Δa, Δe is known — including Δd near ±180° and near 0°, and truths
   straddling the 0/360 pole). Paired contrast CIs must cover the known
   contrast at nominal rate; the run also measures the efficiency claim
   (paired narrower than independent-groups at positive within-person
   correlation).
2. **invariant**: bootstrap vs Monte Carlo agreement in expectation on the
   same data (the package's standing two-engine invariant, extended to the
   occasions cell); plus the degenerate-dependence invariant — occasions
   blocks made *independent* (persons randomly re-paired) must reproduce
   the independent-groups contrast distribution within Monte Carlo error.
3. **closed-form** (elevation only): the paired Δe contrast is a linear
   statistic; its MC interval must match the textbook paired-difference
   normal interval computed with deliberately dumb code on the same data.
4. Boundary regression tests per CLAUDE.md: contrast near ±180°, CIs
   straddling 0°/360°, flat/zero-variance occasion, one occasion degenerate.

---

## 3. Intraindividual SSM (descriptive layer)

### 3.1 Decision: per-person scoring + case-bootstrap inference; pooling is the Bayesian upgrade

In: a per-person SSM scoring path — score each person's own profile (their
p scale scores at one occasion, or their within-person means across
occasions of intensive data) through the closed-form transform, returning a
per-person parameter table (e_i, x_i, y_i, a_i, d_i with the standard
degenerate-profile NA semantics, which at intensive-data T this *will* hit;
NA rates are a reported column, not a silent drop). Group-level summaries of
per-person parameters use circular statistics for d (circular mean +
resultant length; never arithmetic means of angles) and the documented
aggregation caveat (mean resultant ≤ mean amplitude) from Brief E.
Inference on summaries: person-level case bootstrap (§2.1 machinery).

Out (Stan-companion criteria, §5.4): per-person *pooled/shrunken* estimates
from short noisy series — that is hierarchical-model territory; flat
per-person estimates plus honest NA/noise reporting are the in-package
product, and the draws adapter (§5) is the bridge for users who fit the
hierarchical model themselves.

### 3.2 API sketch

`ssm_score()` already vectorizes profile scoring; the build adds an exported
`ssm_parameters_id(data, scales, angles, id = NULL)`-shaped wrapper (name
final at build) returning the per-person parameter data frame. It is
deterministic (no RNG), so its oracle needs are closed-form fixtures +
invariants only.

### 3.3 Oracles

1. **closed-form**: hand-computed fixtures (arithmetic in comments) for 2–3
   synthetic persons, including one exactly-flat and one pure-second-
   harmonic person (NA semantics).
2. **invariant**: for a sample of persons with identical profiles, the
   per-person path must reproduce the group mean-based path exactly; the
   circular mean of per-person d must match `angle_mean()` on the d vector.

---

## 4. Growth models on displacement

### 4.1 Decision: bivariate (x, y) framing; the package owns the transform, not the mixed-model fit

Per Brief E Q6.1: the primary framing is the bivariate growth model on the
Cartesian coordinates x(t), y(t) (elevation rides along as a third ordinary
outcome). It is boundary-free and lives in the coordinates the estimator
already uses. The **package does not fit mixed models**: no lme4/nlme/brms
Import (minimal-deps doctrine; a growth-fitting wrapper would also freeze
one modeling framework into the API). Instead the package ships the two
halves it is uniquely positioned to get right:

1. **Input side**: the per-person(-per-occasion) coordinate table (§3.2
   output includes x_i(t), y_i(t)) — tidy input for any LMM/LGM the user
   picks (nlme ships with R; lme4/lavaan/brms per taste, `Suggests` only,
   through the dependency gate at build time).
2. **Output side**: the draws adapter (§5) — fitted-model draws of the mean
   (x(t), y(t)) trajectory (posterior draws, parametric-bootstrap draws, or
   MVN draws from the fixed-effect vcov, which is the MC engine's own
   asymptotic move) map to (a(t), d(t)) draws with circular-correct
   summaries and CIs at each t.

The two Brief E caveats are vignette-documented, not "fixed": derived d(t)
is the direction of the mean trajectory, not the mean of directions; derived
a(t) shrinks toward 0 under directional dispersion (the standard SSM
aggregation fact).

Unwrap-then-LMM ships as a **documented recipe** in the vignette with its
failure modes stated (branch ambiguity near 180° jumps; no common branch
under heterogeneous locations), supported by one exported helper
`angle_unwrap(x)` (cumulative `angle_dist()` between successive timepoints;
deterministic, trivially fixture-testable). Projected-normal regression
(bpnreg) is referenced in the vignette as the model-based upgrade; not
wrapped.

### 4.2 Oracles

1. **simulation-coverage**: simulate person-level (x, y) trajectories from
   known linear growth truths — including a trajectory whose true d(t)
   crosses the 0/360 pole (e.g., drifting 350°→10°) — fit the vignette's
   reference nlme model, push draws through the adapter, and check d(t)
   pointwise CIs cover the true direction at nominal rate at each t. The
   pole-crossing cell is the reason this whole design exists; it is the
   headline test, not an edge case.
2. **invariant**: in the concentrated common-branch regime (all persons and
   times well away from the cut, high amplitude), unwrap-then-LMM and the
   (x, y)-framing trajectory d(t) must agree within tolerance; and a
   zero-slope truth must reproduce the §2 paired-contrast machinery's answer
   for the two-occasion special case.
3. **closed-form** for `angle_unwrap()`: hand-computed fixtures including a
   350°→10°→30° sequence (unwraps to 350, 370, 390) and an exact-180° step
   (documented convention: `angle_dist` reports +180, so unwrap ascends).

---

## 5. Bayesian draws adapter

### 5.1 Decision: one adapter, two accepted shapes

One exported function (working name `ssm_draws()`):

```r
ssm_draws(draws, angles = NULL, interval = 0.95)
```

- **Shape A — parameter draws**: a numeric matrix (or 3-col data frame)
  with columns interpretable as (e, x, y) — e.g., brms fixed-effect draws
  `b_Intercept, b_cos, b_sin`. `angles` must be `NULL`. Each row maps to
  (e, a, d) by the closed-form transform (a = √(x²+y²), d = atan2(y, x)
  wrapped to [0, 360), flat/zero-amplitude rows → NA per the standing
  semantics).
- **Shape B — profile draws**: a numeric matrix with p ≥ 3 columns of scale
  scores and `angles` supplied (length p). Each row goes through
  `group_parameters()` exactly as a bootstrap replicate would.
- Shape is disambiguated by `is.null(angles)` (never by ncol sniffing:
  a p = 3 instrument is legal). Output: an object holding the SSM parameter
  draws plus a summary table built by the **existing**
  `ssm_replicate_intervals()` path — d draws classed `circumplex_radian` so
  the circular quantile/pole/branch machinery applies verbatim; point
  summaries are medians (a is right-skewed; documented), d point summary is
  the circular mean. This single adapter serves brms posteriors, growth-
  model draws (§4), and any external Bayesian workflow — they all inherit
  the package's boundary correctness from one code path.

### 5.2 Documented statistical footnotes (from Brief E, spec-binding)

Independent priors on (x, y) induce a non-uniform prior on (a, d) — roughly
Rayleigh-shaped on a, mass pushed away from a = 0 — documented as a modeling
choice in the vignette and the adapter's docs; posterior a summaries are
medians/quantile intervals, never mean ± SD.

### 5.3 Vignette

`bayesian_ssm.Rmd` finished properly: the brms cos/sin regression recipe →
`ssm_draws()` → circular-correct summaries, fixing the sketch's line-114
`#TODO: Account for 360 boundary` by construction. brms cannot run on CRAN
builders: the vignette is **precomputed** (chunk outputs cached/static;
exact mechanism per the r-package profile at build time) with brms in
`Suggests` — the dependency addition goes through the dependency gate +
D-entry at build time, per tracking rules.

### 5.4 Stan companion package: stay-out criteria

Revisit (as its own design) only if ≥2 of: (a) users demonstrably need
pooled per-person estimates the adapter route can't give them; (b) the
projected-normal growth model becomes a recommended default rather than a
reference; (c) a maintained upstream (bpnreg or successor) can't be
recommended instead. Recorded here so the question isn't re-litigated ad
hoc.

### 5.5 Oracles

1. **invariant** (the decisive one): feeding the adapter the *bootstrap
   replicate matrix* of an existing `ssm_analyze()` run must reproduce that
   run's intervals exactly (same quantile path); feeding shape-B profile
   draws equal to a repeated observed profile must reproduce the point
   estimates.
2. **closed-form**: hand-computed fixtures for a 4-row draws matrix
   (arithmetic in comments), including a draw pair straddling the 0/360
   pole whose naive linear quantiles would invert, and an all-flat draws
   matrix (all-NA return contract).
3. Boundary regressions: pole-straddling d draws (CI must wrap), exact-pole
   summaries report 360 (D-003/M20 convention).

---

## 6. Validation & test plan (cross-cutting)

- Every stochastic claim above names simulation-coverage plus at least one
  deterministic type (invariant / closed-form) — the ≥2-independent-types
  bar holds per component, not merely in aggregate.
- Coverage runs follow the M19/M21 pattern: seeded, cell-indexed by level
  (LESSONS 2026-07-13: index the level, never the raw value), smoke-first,
  results committed as `devel/m6-*-results.rds` with regeneration scripts
  (reproducibility hard stop).
- **Gap flagged for the build milestones:** DESIGN.md's Conventions section
  does not yet declare the oracle-registry pointer required by the
  validation doctrine ("Oracle records: …" line). The build milestone that
  first adds a longitudinal oracle should add the declaration (existing
  practice: provenance comments at the asserting tests, e.g.
  `test-cpm_oracles.R`) rather than this docs-only milestone inventing a
  registry shape ahead of the tests.
- New `Suggests` (nlme and/or lme4 for the growth vignette; brms for the
  Bayesian vignette) are dependency changes: question gate + D-entry at
  build time, never unilateral (tracking rules).

## 7. Build-milestone cut (recommendation to /milestone-plan, not binding)

1. **Build A — occasions core**: §1 API + §2 paired contrasts, both engines,
   full oracle battery. The v2.x headline.
2. **Build B — per-person layer + adapter**: §3 + §5 (they share the
   transform-the-draws machinery), incl. the Bayesian vignette.
3. **Build C — growth-model support**: §4 helpers + vignette + coverage
   oracle (depends on B for the adapter).

Each is one PR-sized vertical slice; none merge-gated behind M7 (D-012).

## 8. Open questions for the Fable review (RB06)

1. §1.3's listwise-only narrowing: is refusing pairwise + occasions the
   right call, or is there a coherent pairwise semantics worth speccing now?
2. §2.2: is the stacked-occasions MC covariance (sample covariance of
   stacked person vectors / n) the correct asymptotic object under
   unbalanced group sizes with `grouping` present?
3. §4.1: does the draws adapter applied to *frequentist* vcov MVN draws of
   fixed effects give pointwise d(t) intervals with defensible coverage, or
   does the delta-scale nonlinearity at low amplitude demand a caution
   parallel to the CPM analytic-CI ladder (D-010)?
4. §5.1: is shape dispatch on `is.null(angles)` sound, or does it invite a
   silent misuse (3-scale profile draws passed without angles being
   transformed as (e, x, y))?
5. The efficiency claim in §2.3 (paired beats independent at ρ > 0): state
   it in docs as measured-by-the-oracle, or derive and cite it?
