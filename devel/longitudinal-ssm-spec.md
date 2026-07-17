# Longitudinal & intraindividual SSM — build-ready design spec (M23)

**Status:** revised against the independent Fable review (RB06 → RR06,
2026-07-16; verdict "needs change (targeted)", architecture confirmed).
§9 is the revision log. Successor to Brief E
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
  **`Occasion` is conditional-presence** (appears only for occasions
  analyses): always-present-NA (the `Measure` precedent) would soft-break
  every existing consumer of `results` in a minor release, while in-package
  consumers (table/plot/print) must branch for occasions objects regardless
  (§7 Build A acceptance). Stated consequence: downstream code tests for the
  column, and the schema difference is NEWS-documented. [RR06 R3c]
- **Signature consequence (stated so the build is deliberate):** `scales`
  becomes optional (`NULL` default) since `occasions` is its alternative
  spelling; `stopifnot(is_var(scales))` (`R/ssm_analysis.R:208`) becomes
  conditional on `is.null(occasions)`. Backward-compatible for positional
  callers. [RR06 R10]
- **Cross-occasion alignment validation (closes the spec's largest silent-
  corruption channel):** the "same scales, same order" contract is not
  checkable by literal column names (`PA_1` vs `PA_2`), and a rotated
  occasion block silently rotates displacement. The build validates by
  **stem matching**: strip the longest common per-block prefix/suffix from
  each occasion's column names; if all blocks yield stem vectors, require
  identical stem *order* across occasions (mismatch → error naming the
  offending block; same stems in different order is exactly the rotation
  bug). If no stem structure is detectable, emit a one-time message naming
  the assumed positional alignment ("PA_1 ~ NO_2 aligned by position 1").
  Fixture-tested both ways. [RR06 R2]
- **Grouping is time-invariant by construction** (one `Group` cell per
  person-row) — documented so time-varying grouping isn't shoehorned in as
  occasion-specific columns. `details` gains occasions metadata
  (`names(occasions)`, k) known to `print.circumplex_ssm` and snapshots.
  [RR06 Q10-3/4]

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

**Pinned ordering conventions [RR06 R3]:**

- **Occasion contrast order is `names(occasions)` list order (temporal, as
  supplied), never factor/alphabetical sorting** — `param_diff(occ2, occ1)`
  = second listed minus first listed. This deliberately differs from the
  group contrast's factor-level order; a `T10`/`T2`-style name pair is a
  required regression test (alphabetical sorting would silently flip the
  sign).
- **Profile-row order is occasion-major within group** (all occasions of
  group 1, then group 2 …), paralleling the measure path's group-major/
  measure-minor layout; the same order governs the score matrix, replicate
  columns, and result table. `ssm_by_group()`'s positional contrast slice
  (`R/ssm_bootstrap.R:162`) is only reached in the 2-occasion/1-group cell,
  where the order is unambiguous.
- If occasions × contrast × grouping ever lands, the unbalanced-strata
  estimand question reopens with it (recorded in that cell's candidate
  note). [RR06 Q3]

### 1.3 Missing waves

Default `listwise = TRUE` keeps its natural extension: `na.omit()` on the
wide row — a person missing *any* occasion is dropped from *all* occasions
(complete-cases-across-waves). The build ships **listwise-only for
occasions** (pairwise + occasions errors with a message stating *why*:
within-person contrast semantics), with pairwise support recorded as a
candidate. The reason is the **estimand** [RR06 R6, correcting this spec's
earlier draft]: under pairwise the existing plumbing is internally coherent
(each replicate's statistic mirrors the point statistic — `bs_function`
applies the same `listwise` flag, `R/ssm_analysis.R:374-380` →
`src/parameters.cpp:104`), but the "paired contrast" stops being a
within-person contrast at all — it compares partially overlapping
subpopulations (T1-completers vs T2-completers), an estimand nobody asked
for, biased under outcome-related missingness.

Two further obligations [RR06 R6]: (a) occasions listwise deletion emits an
informative message reporting how many persons were dropped (the current
`na.omit(bs_input)` is silent, and with k·p columns the deletion rate grows
with k); (b) the docs carry a selection-bias caution — complete-cases-
across-waves estimates *completers'* change, which differs from population
change when dropout relates to the outcome.

### 1.4 What `occasions` is *not*

Not a growth-model interface (no time metric, no trajectories — §4), and
not intraindividual scoring (§3). It is the k-occasion generalization of the
package's existing "several profiles from one sample" analyses, with the
person as sampling unit.

**`ssm_ci_accuracy()` contract [RR06 R7]:** an occasions object's
`details$suff_stats` would describe the flattened k·p columns with the
wrong dependence structure — the diagnostic would plausibly *run* and
silently simulate from the wrong population. Build A makes
`ssm_ci_accuracy()` **error informatively** on occasions objects; the
occasions extension (natural via the stacked-MC machinery, §2.2) is a
recorded candidate, its own design.

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
question does not arise in this build. Docs note that the MC engine wants
n_g comfortably above k·p (the existing `min(tabulate(grp)) < 2` guard is
formally sufficient but weak in kp dimensions); small-n behavior is
measured, not redesigned — the percentile bootstrap stays the small-n
answer [RR06 R9/R15].

Paired-interpretability sentence for the docs [RR06 Q3]: a paired Δd CI is
interpretable only when *both* occasions' amplitudes are reliably nonzero —
a user may certify only one occasion's profile and must not read the
contrast as directional change.

### 2.3 Oracle strategy (≥2 independent types per numeric result)

1. **simulation-coverage** (primary; a CI method's oracle is coverage):
   simulate wide two-occasion samples from known bivariate populations with
   controlled cross-occasion covariance (Σ_within specified between occasion
   blocks; population SSM truths set per occasion so the true contrast
   Δd, Δa, Δe is known — including Δd near ±180° and near 0°, truths
   straddling the 0/360 pole, and a **small-n cell, n ≈ 25–50**, to measure
   the stacked-MC arm's known-Σ̂ anticonservatism; a k = 3 cell if k > 2
   ships in Build A [RR06 R9]). Paired contrast CIs must cover the known
   contrast at nominal rate. The run also **measures the corrected
   efficiency statement [RR06 R1]**, including a ρ > 0, Δd ≈ 135° cell
   *expecting the reversal* — turning this oracle from a confirmation into
   a discrimination:
   - **Δe (exact, finite-sample):** Var(ē₂ − ē₁) = (σ₁² + σ₂² − 2ρ_e σ₁σ₂)/n
     vs (σ₁² + σ₂²)/n independent — paired narrower **iff ρ_e > 0**, where
     ρ_e is the within-person correlation of profile *elevations*
     (textbook paired-design identity).
   - **Δa, Δd (asymptotic, conditional):** paired variance =
     Var₁ + Var₂ − 2∇g₂ᵀC∇g₁ (C = cross-occasion covariance of the
     estimated (x̂, ŷ) blocks; ∇g_j the parameter gradient at occasion j) —
     narrower **iff the gradient-projected cross-covariance ∇g₂ᵀC∇g₁ > 0**,
     not iff within-person correlation is positive. Under isotropic
     C = cI₂ the cross term for both Δa and Δd is ∝ c·cos(Δd): paired is
     narrower for |Δd| < 90° and **asymptotically wider for |Δd| > 90°**
     despite strongly positive within-person correlation (RR06 numerics:
     paired/independent Var(Δd̂) ratio ≈ 0.49 at Δd = 30°, ≈ 1.41 at 135°,
     ρ = 0.6, matching 1 − ρcos Δd). Docs state only this conditional
     claim; the unconditional "paired is narrower at ρ > 0" must never be
     printed.
2. **invariant**: bootstrap vs Monte Carlo agreement on the same data with
   a **pre-registered SE-based tolerance** (never a build-time judgment
   call) — noting honestly that this invariant is not independent for the
   shared downstream code (both engines flow through `param_diff()` and the
   same quantile path), so the coverage oracle carries the branch-handling
   weight [RR06 Q8]; plus the degenerate-dependence invariant — occasions
   blocks made *independent* (persons randomly re-paired) must reproduce
   the independent-groups contrast distribution within Monte Carlo error
   (genuinely discriminating for the dependence handling).
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
2. **invariant** (strengthened per RR06 R8 — the identical-profiles check
   only tests row plumbing, since both paths share the C++ kernel):
   - **linearity (exact, heterogeneous)**: e, x, y are linear in scores, so
     the mean over persons of per-person (e_i, x_i, y_i) must equal the
     group path's (e, x, y) from the mean profile, exactly, on arbitrary
     heterogeneous data — catches aggregation-order and row-misalignment
     bugs the identical-profiles case cannot;
   - **Jensen inequality**: group amplitude ≤ mean per-person amplitude,
     strictly when directions disperse (the aggregation caveat as an
     assertion);
   - identical-profiles reproduction and the circular-mean check retained,
     with the circular mean **recomputed by hand in the test** (atan2 of
     summed sines/cosines), never via `angle_mean()` (tautology otherwise).

Documentation obligations [RR06 Q5]: the circular mean of per-person d_i
(equal weight per direction) is a *different quantity* from the
displacement of the group mean profile (amplitude-weighted) — documented,
with a fixture asserting they **differ** on a heterogeneous sample (an
anti-confusion regression). The summary layer strips NA d_i (reporting the
count) before any circular aggregation — `angle_mean()`
(`src/circular.cpp`) has no `na.rm`.

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

**Hard requirement the recipes must state [RR06 R4]: the LMM is fit
*jointly* on (x, y)** (the stacked-outcome dummy-coded multivariate
formulation). *[M27 amendment, 2026-07-16, gated: the vignette's reference
engine is glmmTMB (`us(0 + dv | person)` gives the correlated cross-outcome
random effects directly), with nlme named as the base-R alternative. The
RR06-reviewed holding — joint fitting — is engine-agnostic and unchanged;
D-016.]* d(t) depends on the joint distribution of (x̂(t), ŷ(t)); the
plausible-but-wrong shortcut — two univariate LMMs with independent vcovs —
zeroes Cov(x̂(t), ŷ(t)) and produces wrong d(t) intervals. The §4.2 oracle
grid includes a cell that makes this shortcut *fail* (below).

**Amplitude-conditioned caution (answering the §8 question affirmatively)
[RR06 R4]:** MVN-vcov propagation is defensible only in the concentrated
regime; the danger is **low amplitude at some t** (origin-proximal
trajectories: direction reversals, crossovers, extrapolated t), where the
d(t) draw distribution is diffuse/bimodal and circular quantiles are
meaningless. At each t the summary applies the shipped scale-free
certification rule to the a(t) draws (`a_lci/(a_uci − a_lci) ≥ 0.35`, the
D-007 rule) and flags uncertified t; the vignette states d(t) intervals at
uncertified t are not interpretable. Vignette-level (user-side, not
adapter-fixable): REML fixed-effect vcov ignores variance-component
uncertainty — anticonservative at small N; Kenward–Roger/t-scale named as
the user-side remedy.

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
   known linear growth truths, **from the same model family the reference
   recipe fits** (so a coverage failure indicts the adapter, not
   misspecification), fit the vignette's reference joint model (glmmTMB per
   the M27 amendment above), push
   draws through the adapter, and check d(t) pointwise CIs cover the true
   direction at nominal rate at each t. Three named cells [RR06 R4/Q8]:
   - **pole-crossing** (true d(t) drifting 350°→10°) — the *boundary-
     machinery acceptance headline* (nearly guaranteed to pass in the
     boundary-free framing; it exercises the wrapping/summary code);
   - **low-amplitude/origin-proximal** (a(t) dipping toward 0 at an
     interior t) — the *statistical* danger cell, where coverage actually
     degrades and the §4.1 caution must demonstrably fire;
   - **strong x–y fixed-effect correlation** — chosen so an
     independent-fits shortcut (§4.1) *fails* coverage, making the oracle
     discriminating against that exact error.
2. **invariant**: in the concentrated common-branch regime (all persons and
   times well away from the cut, high amplitude), unwrap-then-LMM and the
   (x, y)-framing trajectory d(t) must agree within tolerance. The
   two-occasion zero-slope comparison against the §2 paired-contrast
   machinery is reframed [RR06 Q6]: the two are *different estimators*
   (model-based LMM draws vs nonparametric case bootstrap) agreeing only
   asymptotically under correct specification — it runs as a consistency
   check with a pre-registered tolerance at one large-n well-specified
   cell, never as an exact invariant.
3. **closed-form** for `angle_unwrap()`: hand-computed fixtures including a
   350°→10°→30° sequence (unwraps to 350, 370, 390) and an exact-180° step
   (documented convention: `angle_dist` reports +180, so unwrap ascends).
   Build details pinned [RR06 Q6]: input is degrees, wrapped to [0, 360)
   first (any reals accepted); NA policy is **propagate NA from the missing
   wave onward** (every subsequent value is branch-ambiguous), documented
   and fixtured.

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
- **Shape dispatch [RR06 R5, closing the §8 ambiguity]:** `is.null(angles)`
  alone is unsound at ncol = 3 (a p = 3 instrument's profile draws passed
  without angles would be silently transformed as (e, x, y), and vice
  versa). Rules: `angles` supplied → shape B, requiring
  `ncol(draws) == length(angles)`; `angles = NULL` and `ncol(draws) != 3` →
  error explaining both shapes; `angles = NULL` and `ncol(draws) == 3` →
  an explicit `type = "parameters"` argument is **required** (error naming
  the ambiguity otherwise). Shape A's assumed column order (e, x, y) is
  documented loudly; when `colnames(draws)` are present but not
  recognizably (intercept, cos, sin)-like, the adapter messages the assumed
  mapping. (Two exported functions instead of dispatch: rejected — doubles
  the irreversible exported surface for no gain; RR06 R14.)
- Output: an object holding the SSM parameter
  draws plus a summary table built by the **existing**
  `ssm_replicate_intervals()` path — d draws classed `circumplex_radian` so
  the circular quantile/pole/branch machinery applies verbatim (percentile
  quantiles of posterior draws = equal-tailed credible interval;
  statistically honest reuse per RR06 Q7). Four managed leaks [RR06 Q7]:
  `replicate_label = "posterior draws"` (warnings must not say "bootstrap
  resamples"); the per-parameter NA exclusion reads as the same
  "conditional on estimability" semantics (measure-zero for continuous
  shape-A posteriors; can bind for shape B — documented); `t0` is the
  adapter's own point summaries (stated explicitly — there is no observed
  estimate); shape A synthesizes the 6-column `ssm_param_names()` layout
  with `fit = NA` (a 5-column matrix would misalign every parameter).
  Point summaries are medians (a is right-skewed) and the circular mean for
  d, with the documented coherence caveat [RR06 Q7]: marginal summaries are
  not jointly coherent — median(a) ≠ √(median(x)² + median(y)²) and the
  reported d is not the direction of the reported (x, y); `angle_mean()`'s
  NA at zero resultant is the correct diffuse-posterior outcome, documented
  rather than "fixed". This single adapter serves brms posteriors, growth-
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
`#TODO: Account for 360 boundary` by construction. **The sketch is treated
as untrusted** [RR06 Q10-6]: its line-43 derivation comment has the atan2
arguments swapped relative to its own correct code (line 47) — a live
instance of the shape-A column-order hazard. The vignette derives the
mapping fresh (x = cos coefficient, y = sin coefficient, d = atan2(y, x)),
and the §5.5 fixtures pin a known-direction case. brms cannot run on CRAN
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
3. **invariant** (shape consistency [RR06 R11]): for any profile-draws
   matrix, shape B must equal shape A applied to the per-row (e, x, y)
   computed from those profiles — exact by construction, and the only
   oracle exercising the dispatch/column-mapping channels.
4. Boundary regressions: pole-straddling d draws (CI must wrap), exact-pole
   summaries report 360 (D-003/M20 convention).
5. The §5.2 induced-prior statement is shown-not-asserted: the vignette
   includes a ~10-line prior-predictive simulation exhibiting the
   Rayleigh-shaped prior on a [RR06 Q8].

---

## 6. Validation & test plan (cross-cutting)

- Every stochastic claim above names simulation-coverage plus at least one
  deterministic type (invariant / closed-form) — the ≥2-independent-types
  bar holds per component, not merely in aggregate.
- Coverage runs follow the M19/M21 pattern: seeded, cell-indexed by level
  (LESSONS 2026-07-13: index the level, never the raw value), smoke-first,
  results committed as `devel/m<NN>-*-results.rds` named per the *producing
  build milestone's* own ID — never the legacy "m6" prefix (IDs ≤ M6 are
  reserved legacy citations) — with regeneration scripts (reproducibility
  hard stop).
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
   full oracle battery. The v2.x headline. Acceptance additionally covers
   the output surface [RR06 R12]: `ssm_table()`/plot functions either
   support or cleanly reject occasions objects; `print.circumplex_ssm` +
   snapshots updated for the occasions `details` fields; `ssm_ci_accuracy()`
   errors informatively (§1.4); DESIGN.md's reproducibility table gains the
   occasions RNG row [RR06 Q10-5].
2. **Build B — per-person layer + adapter**: §3 + §5 (they share the
   transform-the-draws machinery), incl. the Bayesian vignette.
3. **Build C — growth-model support**: §4 helpers + vignette + coverage
   oracle (depends on B for the adapter).

Each is one PR-sized vertical slice; none merge-gated behind M7 (D-012).

## 8. Open questions — resolved by RR06 (2026-07-16)

All five draft questions were answered by the independent review and folded
into the sections above: (1) listwise-only stands on corrected estimand
grounds (§1.3); (2) the stacked MC covariance is verified correct,
including unbalanced groups and no-Fisher-z (§2.2); (3) yes — the
amplitude-conditioned per-t caution is required (§4.1); (4) dispatch on
`is.null(angles)` alone is unsound at ncol = 3 — explicit type required in
the ambiguous cell (§5.1); (5) derive-and-state — the draft's unconditional
claim was **false** for Δa/Δd and is replaced by the conditional statement
with the |Δd| > 90° reversal (§2.3).

## 9. Revision log (vs RR06)

Every RR06 recommendation, disposition → resolution:

- **R1 (efficiency claim) — applied**: §2.3 rewritten with the exact Δe
  identity, the ∇g₂ᵀC∇g₁ condition, the cos(Δd) special case, the reversal
  cell in the oracle grid, and a docs prohibition on the unconditional
  claim. The draft's claim was wrong; the review's derivation and numerics
  were independently checked and adopted.
- **R2 (alignment validation) — applied**: §1.1 stem-matching rule + no-stem
  positional message + fixtures.
- **R3 (ordering conventions) — applied**: §1.2 pins list-order contrasts
  (with T10-style regression test), occasion-major row order, and resolves
  the `Occasion` column as conditional-presence (author's call where RR06
  offered either: always-present-NA soft-breaks every existing `results`
  consumer in a minor release; in-package consumers must branch anyway).
- **R4 (joint LMM + oracle cells + caution) — applied**: §4.1 joint-fit
  requirement; §4.2 low-amplitude and strong-correlation cells added,
  pole-crossing reframed as boundary headline; per-t D-007 certification
  caution.
- **R5 (shape dispatch) — applied**: §5.1 explicit-type-when-ambiguous
  rules + column-order documentation/messaging.
- **R6 (pairwise justification) — applied**: §1.3 rationale replaced with
  the estimand argument (the draft's asymmetry claim was incorrect against
  the actual plumbing); dropped-n message + selection-bias caution added.
- **R7 (`ssm_ci_accuracy()` contract) — applied**: §1.4 informative error;
  extension recorded as a candidate at ingestion.
- **R8 (per-person invariants) — applied**: §3.3 linearity + Jensen
  invariants, hand-recomputed circular mean, differ-fixture, NA stripping.
- **R9 (small-n / k = 3 cells, n_g ≫ kp note) — applied** (adopted from
  consider): §2.2/§2.3.
- **R10 (signature note) — applied** (adopted from consider): §1.1.
- **R11 (shape-A/B invariant) — applied** (adopted from consider): §5.5.
- **R12 (output-surface acceptance) — applied** (adopted from consider):
  §7 Build A.
- **R13 (long/id API) — rejection accepted**: wide-only stands; R2 addresses
  the real risk.
- **R14 (two exported adapter functions) — rejection accepted**: §5.1
  explicit-type rule instead.
- **R15 (Wishart/t correction for stacked MC) — rejection accepted**:
  measure (R9), don't fragment the engine's semantics.
- Beyond-the-brief items folded in: Q10-1 → §1.4; Q10-2/3 → §7/§1.1;
  Q10-4 → §1.1; Q10-5 → §7; Q10-6 → §5.3 untrusted-sketch note.
