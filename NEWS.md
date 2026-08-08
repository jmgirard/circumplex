# circumplex 2.0.0

This is a major release. Its flagship addition is `cpm_fit()`, a native
reimplementation of Browne's (1992) circular stochastic process model for the
correlational structure of circumplex scales — filling the gap left by the
archived CircE package, the previous R implementation. Alongside it come four
other new analysis families: latent-variable SSM analysis with `ssm_sem()`,
repeated-measures (longitudinal) SSM analysis, `fit_structure()` for
exploratory circumplex-structure tests, `axes_reliability()` for the
reliability of the circumplex axes (Strack et al., 2013), and
`ssm_ci_accuracy()`, a diagnostic for whether an `ssm_analyze()` result's
confidence intervals can be trusted at your sample size and profile
(Zimmermann & Wright, 2017). The
plotting layer has been rebuilt on a real ggplot2 coordinate system.

## Breaking changes and changed behavior

* The component standard errors reported by `axes_reliability()` are now
  calibrated. Previously they were computed as if the item correlation matrix
  were a covariance matrix — the source paper's own practice, documented as
  approximate — which for strong-axes instruments overstated the standard
  error of the axes variance by 25–45%, and for weak-axes, strong-general
  instruments could understate it slightly. Because the error changed sign
  across the range of instruments the function accepts, no fixed caveat could
  state it honestly. Point estimates, reliabilities, SEm, degrees of freedom,
  and SRMR are all unchanged; the remaining fit statistics are corrected
  separately, below. Corrected standard errors are
  typically *smaller* than those printed in Strack et al. (2013), whose LISREL
  values carry the same uncorrected approximation. The uncorrected values
  remain available in `details$se_uncorrected`.

* The global fit statistics reported by `axes_reliability()` are now
  calibrated to the correlation metric. `chisq`, `pvalue`, `rmsea` and `cfi`
  are Satorra-Bentler-type *scaled* values, computed by dividing the
  chi-square by a factor evaluated at the fitted matrix (with `cfi` also
  scaling its baseline model); `df` and `srmr` are unchanged. Previously these
  carried the same correlation-as-covariance mismatch as the standard errors
  did, running the other way: sample correlations vary less than covariances,
  so the test statistic came out too small and fit was flattered — by roughly
  4% at one reference population, which the documentation had reported as
  though it were a constant. It is not a constant, and the scaling factor is
  now recomputed for every fit. All three input paths scale, including
  `missing = "fiml"`. Expect slightly larger chi-squares, smaller p-values,
  slightly higher RMSEA, and slightly lower CFI than previous versions
  reported on the same data. The unscaled values remain available in
  `details$fit_uncorrected`, with the factors in `details$scaling_factor`. If
  the factor cannot be computed, the four are `NA` with the reason in
  `details$fit_scaling_failed` rather than falling back to the unscaled
  values. The correction is calibrated in mean and its test is asymptotically
  exact, but at small samples relative to the item count it over-rejects: see
  `?axes_reliability` for the measured rates and the sample sizes they were
  measured at. Note the direction — the scaled test over-flags misfit, where
  the uncorrected one flattered it. These follow the definitions lavaan calls
  `chisq.scaled`, `pvalue.scaled`, `rmsea.scaled` and `cfi.scaled`, not its
  `*.robust` forms. Since the fit itself is estimated with plain ML,
  `fitMeasures()` on an equivalent fit reports the unscaled values under the
  bare names and no `*.scaled` or `*.robust` measure at all, so a cross-check
  against lavaan's bare `cfi` will differ and a request for `cfi.robust` will
  come back empty.

* `axes_reliability()` objects now report `details$n_moments`, the number of
  distinct analyzed moments p\* = p(p+1)/2, and `details$baseline`, the
  independence model's unscaled chi-square and degrees of freedom.
  `details$n` is documented now as the sample size the fit was priced at, as
  distinct from `n_total` and `n_complete`. Together `n_moments` and `n` let
  you locate a fit on the calibration table in `vignette("axes-reliability")`;
  `baseline`, with `fit$chisq`, `fit$df` and the `baseline` element of
  `details$scaling_factor`, lets you reproduce the reported `cfi`.

* The displacement-interpretability guardrail in `print()` and `summary()`
  now uses a scale-free rule: a profile's displacement is certified as
  interpretable only when the amplitude confidence interval's lower bound sits
  at least 0.35 interval-widths above zero. This replaces the rule introduced
  in 1.2.0, which certified whenever the lower bound rounded above zero at the
  display precision — a threshold that moved with the print `digits` and meant
  different things on different score metrics, and that (as the new
  `ssm_ci_accuracy()` diagnostic makes visible) certified a genuinely zero
  amplitude almost every time. The new rule holds false-certification near the
  interval's one-sided error rate regardless of scale or display settings. As a
  result, some near-zero-amplitude profiles that were previously certified are
  now flagged uninterpretable. The threshold is calibrated for the default 95%
  confidence interval.

* Displacement and angle confidence-interval endpoints that land exactly on
  the 0/360 pole are now reported as 360, never 0, matching how the package
  labels that pole everywhere else (LM = 360): `ssm_analyze()` bootstrap
  displacement CIs and `cpm_fit()` bootstrap angle CIs both use the shared
  circular-quantile machinery that now applies this labeling. `cpm_fit()`'s
  reported `Angle` column likewise labels the pole 360 — a reference scale
  with a theory angle of 360 previously printed `Angle = 0` with a degenerate
  CI of `[0, 0]`, and now prints 360 throughout. An exact-pole endpoint is a
  measure-zero floating-point corner for real data, so numeric results are
  otherwise unchanged.

* `norm_standardize()` now refuses a normative sample whose mean scores fall
  outside the instrument's own response range, instead of returning z-scores
  computed from it. Such a sample cannot be on the same metric as the scores
  being standardized, so the values it produced were wrong in an undefined
  unit, with nothing in the output to indicate it. One shipped sample is
  affected: the CAIS adult sample (`sample = 2`), three of whose octant means
  exceed the CAIS's 1–5 response range. The CAIS child sample (`sample = 1`)
  and every other instrument are unaffected. See `?cais`; the source of those
  values is under query with its authors, and the sample will be corrected or
  withdrawn once that is settled.

* The package now requires ggplot2 (>= 4.0.0), and `ggforce` is no longer a
  dependency. The declared R requirement moves to R (>= 4.1) to match the floor
  ggplot2 already imposes; no installation that worked before is affected.

* `ssm_score()`'s extra arguments passed through `...` must now be named
  (e.g. `prefix = "IIP_"`) and must be single strings; an unnamed or
  non-scalar argument is now an error rather than being silently ignored
  (previously it could yield unlabeled or garbled output columns). Rows whose
  profile has undefined displacement now produce a single warning reporting how
  many such rows there are, rather than one warning per row.

* Count-valued arguments (e.g. `boots`, `reps`, `ncpus`, `digits`, and the
  sample size `n`) across `ssm_analyze()`, `ssm_ci_accuracy()`, `cpm_fit()`,
  `cpm_simulate()`, and `ssm_sem()` are now uniformly validated as a single
  non-negative whole number. A few of these previously accepted a
  length-greater-than-one vector without complaint; such input now raises a
  clear error.

* `ssm_plot_circle()` now warns and names any profile it cannot place on the
  circle because its displacement is undefined (a flat or zero-amplitude
  profile), instead of dropping it from the figure without notice.

## Circumplex structure and model fitting

* New `cpm_fit()` function estimates Browne's (1992) circular stochastic
  process model for the correlational structure of circumplex scales or items,
  a native replacement for the archived CircE package. It accepts either raw
  data or a correlation matrix, estimates item angles and communality indices
  (with four model variants), and reports the usual covariance-structure fit
  indices (chi-square, RMSEA with a 90% confidence interval, SRMR, CFI, TLI,
  AIC, BIC). The returned `circumplex_cpm` object has `print()` and `summary()`
  methods. On the raw-data path, confidence intervals are estimated by a
  nonparametric bootstrap by default (resampling rows and refitting the model,
  with percentile intervals; angle intervals use the package's circular
  quantile machinery, so an interval straddling the 0/360 degree boundary is
  reported wrapped, as with displacement intervals). Resamples that are
  degenerate or fail the convergence criterion are excluded with a warning and
  counted in the output. Only the bootstrap consumes R's random number stream:
  call `set.seed()` immediately before `cpm_fit()` for reproducible intervals
  (point estimates are deterministic). On the correlation-matrix path,
  intervals are analytic (Wald) — there is no raw data to resample — and
  `summary()` cautions when the sample size is small enough that these may
  mis-cover. A `scaling` argument selects the covariance-scaling family:
  `"unit"` (the default) fits the correlation structure, while `"free"` fits
  Browne's covariance structure with `p` free variance scales — the
  parameterization CIRCUM and CircE use — so `cpm_fit()` can reproduce their
  published output exactly. Free scaling adds `p` parameters without changing
  the degrees of freedom, and reports the fitted variance ratios in a
  `VarRatio` column (without confidence intervals). With correlation input the
  two families' model-test statistics are calibration-indistinguishable
  (paired simulation at sample sizes 250–50,000), so the default remains the
  recommended family for routine inference; use `scaling = "free"` when the
  goal is reproducing published CIRCUM/CircE output. `cpm_fit(scaling =
  "free")` also starts its optimizer from the unit-scaling solution, so the
  free family's fit statistic can never exceed the default family's on the same
  input beyond numerical tolerance (the free family mathematically nests the
  default).

* The `cpm_fit()` estimator has been validated against the published
  CIRCUM/CircE literature (Grassi, Luccio, & Di Blas, 2010, reanalyzing
  Browne's 1992 vocational-interest example) and against independent
  OpenMx and lavaan implementations of the same model (both now in Suggests
  as test oracles only). CIRCUM and CircE fit Browne's covariance
  parameterization with free variance scalings; `cpm_fit(scaling = "free")`
  fits that same family and reproduces their published estimates, chi-square,
  and fit indices to printed precision, while the default correlation-structure
  fit differs from them slightly in finite samples (same degrees of freedom,
  asymptotically equivalent); see the package's design notes for details. A
  large seeded simulation study measured the coverage of both interval methods:
  based on its results, `summary()` now also cautions about analytic intervals
  at any sample size below 50,000 when the fitted solution is near a parameter
  boundary or weakly identified (Heywood case, removed harmonic, very small
  correlation-function weight, ill-conditioning, or competing near-tied
  optima — the caution names which), the regime where they measurably
  mis-covered. Percentile bootstrap intervals were confirmed as the better
  default but are themselves conservative-liberal in spots (notably for
  near-boundary correlation-function weights); improving them is planned
  follow-up work.

* New `fit_structure()` function evaluates whether a set of scales forms a
  circumplex using the exploratory criteria of Acton & Revelle (2004). Four
  criteria are computed from the first two unrotated principal-axis factors of
  the scales' correlations — the Fisher Test of equal axes, the Gap Test of
  equal spacing, and the Variance (VT2) and Rotation tests of interstitiality —
  and a fifth, the RANDALL correspondence index (Hubert & Arabie, 1987; Tracey,
  1997), tests the hypothesized circular *order* of the scales with a
  randomization test that yields an exact p-value. The factor-analytic
  statistics are classified against interpretive cutoffs that were re-derived by
  simulation under Acton & Revelle's own generating model for eight (octant)
  scales — their published cutoffs were calibrated on far more variables and do
  not transfer — and that are keyed to the scoring, since these criteria work
  best with a general factor removed. `fit_structure()` deviation-scores
  (ipsatizes) by default for that reason, with a raw opt-out. Missing values
  are handled by listwise deletion by default (a `listwise` argument, matching
  `ssm_analyze()`), so all five tests share one complete-case correlation
  matrix — the metric the cutoffs were calibrated on. The returned
  `circumplex_structure` object has `print()`, `summary()`, and `plot()`
  methods; interpretations are presented as the heuristic likelihood
  classifications they are, never as significance tests.

* New `axes_reliability()` function estimates the reliability (and standard
  error of measurement) of the two circumplex axes with the item-level
  restricted tau-equivalent CFA of Strack, Jacobs, and Grosse Holtforth (2013).
  The model decomposes each item's variance into a general factor, the two
  circumplex axes, scale specificity, and item specificity, and reads the axes'
  reliability off the isolated axes-variance component with the Spearman-Brown
  formula — a confirmatory, item-level complement to `fit_structure()`'s
  exploratory scale-level criteria. The Nunnally-Bernstein axis reliability is
  reported alongside for comparison (it overestimates when scale specificity is
  large). Items are supplied through a `circumplex_instrument` or an explicit
  angle-and-item map. Any equally spaced set of scale angles is accepted, at
  any rotation and any count from four scales up — the canonical octants, an
  interstitial set rotated off the axes, or a six- or twelve-scale circumplex.
  Unequally spaced (quasi-circumplex) angles are refused rather than
  approximated, and three scales are refused because the variance components
  are not separately identified at that count. Scales may carry a single item
  each, as Strack's single-item circumplex types do: with one item at every
  position no two items share a scale, so the scale-specificity component is
  not identified and is dropped from the model rather than estimated, leaving
  a three-row components table. A mixed instrument carrying at least one
  multi-item scale still estimates it. Because coefficient alpha is undefined
  for a one-item scale, the Nunnally-Bernstein comparison is reported as `NA`
  with a stated reason whenever any scale has fewer than two items — as Strack
  et al. themselves do, leaving it blank for such instruments. Blockwise
  instruments — those administering items in blocks cutting across the scales —
  are supported through a `blocks` argument taking a list of item columns, one
  element per block, which adds Strack's block-specificity component to the
  model and a `zeta2` row to the components table. Blocks that carry no
  information the model lacks (blocks that coincide with the scales, one block
  for everything, or one block per item) leave the component unidentified, and
  it is dropped with `details$zeta2_fitted` recording that, as scale
  specificity is on a single-item instrument. Whether ignoring real blocks
  matters depends on their geometry: the general factor is inflated under most
  layouts and never deflated, while the axes variance — and so the reliability
  — moves only when block membership carries information about the angular
  distance between items. When each block draws exactly one item from every
  scale it carries none, and the reliability is unaffected; other layouts bias
  it in either direction, and being evenly spread around the circle is not
  sufficient for safety. Estimation works either from raw item data or, through
  `cormat` and `n`, from a published item correlation matrix alone, for
  reanalyzing a matrix whose raw data is not available; on that path the
  Nunnally-Bernstein comparison is reported as `NA` and `sd = "raw"` is refused,
  since both need the respondents' own item scores. Missing data are handled by
  listwise deletion by default, and a `missing = "fiml"` setting estimates from
  every respondent who answered at least one item by full-information maximum
  likelihood instead. FIML assumes the data are missing at random and
  multivariate normal, both stronger assumptions than listwise deletion needs:
  under MCAR listwise deletion is already consistent and merely inefficient, so
  the gain there is precision rather than correctness, while under MAR listwise
  deletion is biased and FIML is not. The items are standardized by the
  saturated model's own FIML moments rather than by whichever cells happen to be
  observed, and those columns feed a single FIML fit; the reported standard
  errors are observed-information standard errors on that standardized metric,
  conditional on the standardization constants. Under `missing = "fiml"` the
  Nunnally-Bernstein comparison is `NA` and `sd = "raw"` is refused, both
  needing items observed by every respondent. Pairwise-deletion correlations are
  never used on either setting. Strack et al. report no missing-data analyses,
  so the FIML variant is certified against the package's own synthetic oracle
  rather than against their results. A boundary
  fit returns `NA` reliability rather than a clipped value; and the returned
  `circumplex_axes_reliability` object has `print()` and `summary()` methods. A
  bundled simulated dataset, `simulated_items`, is included for the examples.

* New `cpm_simulate()` function draws standardized observations from a fitted
  `cpm_fit()` model's implied correlation matrix, using the model's exact
  positive-semidefinite factor representation. It returns a numeric matrix with
  one column per scale (in fitted order, named), whose population correlation
  matrix is the fitted `Phat`. Call `set.seed()` immediately before it for
  reproducible draws.

## Latent-variable (SEM) analysis

* New SEM-based (latent-variable) SSM analysis: `ssm_sem()` estimates the
  Structural Summary Method profile of one or more external measures against
  the *latent* circumplex content of the scales — the disattenuated analog of
  the correlation-based `ssm_analyze()` — from a fixed-theoretical-angle
  measurement model fitted with lavaan (now a runtime Suggests dependency for
  this feature family; everything else works without it). Confidence
  intervals for all parameters are built in-package by propagating draws of
  the model's free parameters (multivariate-normal by default, or a full
  lavaan bootstrap via `ci_method = "boot"`) through the profile and SSM
  transforms and the same circular-quantile machinery as `ssm_analyze()` —
  never lavaan's delta-method or percentile intervals, which ignore the
  angular branch cut. The default draws propagate lavaan's robust
  (sandwich) covariance, which the package's coverage validation found
  necessary to keep the intervals calibrated when the fixed-angle model
  only approximates the data; the default estimator is MLR, so the global
  fit indices `print()` reports are likewise the robust/scaled versions
  (circumplex scale scores are typically skewed). Includes `ssm_sem_syntax()`
  (an inspectable lavaan model-syntax generator that works without lavaan
  installed) and `ssm_sem_parameters()` (estimate from a lavaan fit you have
  modified or fitted yourself). Results are `circumplex_ssm` objects, so
  `ssm_table()` and the `ssm_plot_*` functions work on them unchanged. With a
  `grouping` variable, `ssm_sem()` fits the measurement model across groups and
  gates a latent group contrast on measurement invariance: it tests a
  configural-metric-scalar ladder using lavaan's own nested-model test (the
  scaled difference test under robust estimators) at the `invariance` rung
  (defaulting to each path's required level) and the `invariance_alpha` level,
  and computes the disattenuated contrast only when the required rung is
  retained. When invariance is rejected it reports an honest non-comparison —
  the verdict plus each group's separate configural profile — rather than a
  contrast that would confound structural difference with measurement
  non-invariance. Supplying `grouping` without `measures` analyzes the latent
  mean path (each group's model-implied latent mean profile). The
  observed-score group contrast in `ssm_analyze()` remains the right tool when
  invariance cannot be assumed; it answers its own, different question.

* The invariance ladder that `print()` reports now also carries `dcfi`, the
  change in CFI from the previous fitted rung, as a labeled *secondary*
  criterion: Cheung and Rensvold's (2002) general rule rejects an invariance
  step when CFI falls by more than .01. (That direction is taken from the
  article's own Table 5, whose critical values are the 1% *lower* tails of the
  simulated null distributions; the sentence stating the rule on its p. 251
  reads backwards relative to that table, and the package follows the
  simulation.) It is reported and never gates — the
  nested test alone decides comparability, the verdict, and which fit the
  estimates come from — and the two criteria can legitimately disagree, since a
  change in CFI is insensitive to sample size where the nested test is not. The
  retain/reject label prints only inside the envelope their simulation covers,
  which is narrow: they simulated **two groups, ML estimation, and
  multivariate normal data, and examined Type I error only** — not power — and
  robust CFI variants were not part of their study. So the label appears only
  for a two-group fit estimated by ML whose CFI is the plain, non-robust one.
  `estimator = "ML"` is necessary but not sufficient: `missing = "fiml"` also
  makes lavaan report a robust CFI, as do the `"MLR"` default and `"MLM"`.
  Under a robust CFI from any of those routes, a non-ML estimator such as
  `"GLS"`, or more than two groups, the `dcfi` value still prints, marked as
  not validated for that configuration, with a note naming which condition
  applies and carrying no verdict — extending the cutoff there would require
  simulation that has not been done.

## Repeated-measures and longitudinal analysis

* New repeated-measures (longitudinal) SSM analyses: `ssm_analyze()` gains an
  `occasions` argument taking a named list of column blocks, one per
  occasion, each selecting the same circumplex scales measured at that
  occasion (wide data, one row per person). Every occasion yields its own
  profile row, occasions cross with `grouping`, and `contrast = TRUE` with
  exactly two occasions (single group) estimates the paired within-person
  contrast — second listed occasion minus first — through both engines: the
  bootstrap resamples persons (preserving within-person dependence
  nonparametrically) and the Monte Carlo engine draws the stacked occasion
  mean vectors jointly. Cross-occasion column alignment is validated by stem
  matching (a reordered occasion block errors instead of silently rotating
  displacement). Occasions analyses are listwise-only across waves, with the
  dropped-person count messaged and a selection caution documented. Results
  from occasions analyses carry a new `Occasion` column that is present only
  for such analyses — downstream code should test for the column by name.
  Coverage of the paired contrasts was validated by simulation at nominal
  rate across boundary cells (displacement changes near 0 and 180 degrees,
  CIs straddling the 0/360 pole, small samples, three occasions); note that
  paired contrasts are not unconditionally more efficient than
  independent-groups designs (see the new Occasions section in
  `?ssm_analyze`).

* New `ssm_analyze_long()` provides a long-format (one row per person per
  occasion) interface to the repeated-measures occasions analysis. It reshapes
  the data to the wide layout `ssm_analyze()` expects and delegates to it, so
  the estimation, paired within-person contrasts, and listwise missing-wave
  handling are unchanged. Occasion order is taken from the factor levels (or
  first appearance) of the `occasion` column and is never sorted
  alphabetically, so a `T10`/`T2` pair keeps its temporal order.

* New per-person (intraindividual) SSM scoring: `ssm_parameters_id()` scores
  each person's own circumplex profile through the closed-form SSM transform
  and returns a per-person parameter table — one row per person, with an
  `id` argument that first averages a person's rows (e.g., occasions of
  intensive longitudinal data) within person before scoring. Degenerate
  profiles keep their row with `NA` parameters (never a silent drop), and an
  `na_rate` column exposes each person's share of missing scale cells. A
  `summary()` method aggregates the table at the group level using circular
  statistics for displacement (circular mean and mean resultant length,
  never arithmetic means of angles), reporting how many undefined
  displacements were excluded. Two documented caveats: the circular mean of
  per-person displacements (equal weight per person) is a different quantity
  from the displacement of the group mean profile (amplitude-weighted), and
  by the triangle inequality the group profile's amplitude is at most the
  mean per-person amplitude, strictly smaller when directions disperse.

* New Bayesian draws adapter: `ssm_draws()` converts posterior draws from a
  user-fitted Bayesian model (e.g., a brms cosine regression) into SSM
  parameter draws and summarizes them with the package's circular-statistics
  machinery — circular quantiles for displacement (credible intervals that
  straddle 0/360 wrap instead of inverting), posterior medians for the
  linear parameters (the amplitude posterior is right-skewed), and the
  circular mean for displacement, with the marginal-coherence caveat
  documented. Two draw shapes are accepted and never guessed: (e, x, y)
  parameter draws (`type = "parameters"`, required because a 3-column
  matrix is ambiguous) and profile draws (one column per scale, with
  `angles`). Draws with undefined displacement are excluded from the
  displacement summaries only, with an honest warning that says "posterior
  draws" and "credible interval". `ssm_draws()` objects also apply the
  package's displacement-certification rule to the amplitude credible
  interval: when the interval's lower bound sits under 0.35 interval-widths
  above zero, printing notes that the displacement is not interpretable, and
  the verdict is stored in `$details$certified`.

* New growth-model support for repeated-measures SSM analysis, documented in a
  new vignette ("Growth Models on SSM Parameters"): fit a *joint* mixed model
  to the per-person Cartesian coordinates from `ssm_parameters_id()` (the
  reference recipe uses glmmTMB, now in Suggests; fitting the coordinates with
  separate univariate models silently zeroes their cross-covariance and
  produces wrong displacement intervals), then convert fixed-effect draws to
  amplitude/displacement trajectories with `ssm_draws()`. The recipe was
  validated by simulation: pointwise displacement coverage is nominal in a
  pole-crossing design, and the univariate shortcut demonstrably fails coverage
  under correlated person effects.

* New `angle_unwrap()` helper unwraps a temporally ordered sequence of
  angles onto a continuous branch (350, 10, 30 becomes 350, 370, 390),
  supporting the vignette's alternative unwrap-then-model recipe. Inputs
  are wrapped to [0, 360) first; an exact 180-degree step ascends (the
  package's half-turn convention); `NA` makes later waves branch-ambiguous
  and so propagates onward.

## Interval methods and diagnostics

* New `ssm_ci_accuracy()` function assesses, by simulation, whether the
  confidence intervals of an `ssm_analyze()` result would cover the true SSM
  parameters at their nominal rate if the population looked like the fitted
  estimates, at the observed sample size(s) — the CI-trustworthiness
  diagnostic of Zimmermann & Wright (2017), generalized to the user's own
  configuration (grouping, contrasts, measures, engine, resample count, and
  interval level). The population's scale structure is characterized by a
  `cpm_fit()` model (or, optionally, the observed correlations); each
  simulated dataset replays the object's own interval procedure. Coverage is
  reported per profile row, parameter, and amplitude condition — a ladder of
  populations with the amplitude scaled toward zero, where percentile
  amplitude intervals are theoretically weakest — along with one-sided miss
  rates, interval widths, the certification rate of the printed
  displacement-interpretability guardrail, and displacement coverage
  conditional on certification. For a contrast row — a signed difference that
  `print.circumplex_ssm()` never certification-gates — the displacement
  verdict and printed coverage are reported unconditionally, matching that
  profiles-only stance (its conditional coverage is retained in the object as
  a descriptive). Coverage at the as-estimated condition is
  classified against Bradley's (1978) liberal robustness band using 95%
  Wilson score intervals, and `print()`/`summary()` translate the
  classifications into a plain-language verdict, including a line reporting
  how often the guardrail would certify displacement if the true amplitude
  were zero (the scale-free rule holds this near the interval's one-sided
  error rate, and a caution is raised only if it materially exceeds that).
  `summary()` also annotates the realism of the simulated population
  (structural-model convergence and fit, against conventional RMSEA/SRMR
  benchmarks with citations) and, when an amplitude estimate is itself below
  half its CI width, notes that the analysis already sits in the near-zero
  regime and adds a ladder rung at the certification margin. A `plot()` method
  draws coverage across the amplitude ladder with the Bradley band shaded.
  Simulation replicates can be parallelized (`parallel`/`ncpus`) with
  seed-identical results, and the caller's random-number state is restored
  on exit. To support the diagnostic, `ssm_analyze()` now stores per-group
  sufficient statistics (sizes, scale SDs, and correlation matrices) in its
  output; objects created by earlier versions can be assessed by re-supplying
  the original data via `ssm_ci_accuracy(..., data = )`, which is checked
  for consistency against the stored profiles.

* `ssm_ci_accuracy()` also assesses repeated-measures occasions analyses. Its
  plug-in population is a multivariate normal with the observed stacked
  cross-occasion covariance, so the within-person dependence across occasions
  is carried into the simulation (rather than ignored); it reports CI
  trustworthiness per occasion and for the paired within-person contrast. A
  flat occasion is refused by name, a rank-deficient stacked covariance is
  flagged (the fit-statistic pass rate becomes descriptive), and because the
  occasions population is the observed covariance the `structure`/`cpm`
  arguments are not accepted on that path.

* `ssm_analyze()` gains a `method` argument offering a Monte Carlo alternative
  to the bootstrap (`method = "montecarlo"`): SSM parameter replicates are
  drawn from the asymptotic sampling distribution of the group mean vector or
  measure-scale correlation vector (a multivariate normal with empirically
  estimated covariance; correlations are drawn jointly across measures on the
  Fisher z scale) and propagated through the SSM transformation. It produces
  intervals closely matching the bootstrap on large samples while running in a
  fraction of the time, but relies on asymptotic normality and requires
  listwise-complete data, so the bootstrap remains the default and the
  recommended choice for small samples. `summary()` reports which method
  produced the intervals.

* `ssm_analyze()` gains `parallel` and `ncpus` arguments (passed to
  `boot::boot()`) to distribute the bootstrap computation across multiple CPU
  cores. Because the resample indices are drawn in the main R process before
  any work is distributed, results for a given `set.seed()` are identical
  regardless of these settings, so parallelizing never changes your estimates
  or confidence intervals.

* The Monte Carlo interval engine (`ssm_analyze(method = "montecarlo")`) is
  faster on correlation-based analyses: the influence-function covariance is
  built in one vectorized pass and all profile rows are propagated through
  the SSM transformation in a single compiled call. Results are unchanged
  (byte-identical for a fixed seed).

* `ssm_score()` is now vectorized internally (one compiled call instead of a
  row-wise loop), making it much faster on large data sets. Results are
  unchanged.

## Visualization

* Circumplex figures are now built on a real ggplot2 coordinate system. The new
  `coord_circumplex()` owns the amplitude-to-radius scaling and the
  displacement-to-angle transform in one place, so a canvas and its data layers
  can no longer disagree about the outer-ring amplitude. It adds a configurable
  amplitude *center* (the rings relabel and the amplitudes remap together) and a
  theme-responsive canvas: the rings, spokes, and labels drawn by
  `ggcircumplex()` now restyle through `+ theme_*()`. It always draws an
  amplitude ring at `amax`, so every circumplex canvas closes at its rim and no
  point is drawn past the last visible ring; that rim ring is unlabeled unless
  `amax` is itself one of the axis breaks. A non-finite `amax` or `center` is
  rejected with a message naming the argument. `ggcircumplex()`,
  `geom_ssm_point()`, `geom_ssm_arc()`, and `ssm_plot_circle()` keep their
  signatures and correct output. The per-layer `amax` argument (and
  `geom_ssm_arc()`'s `n`) are no longer needed and are ignored with a one-time
  note.

* New `ggcircumplex()` function builds an empty circumplex plotting canvas
  (amplitude rings, displacement spokes, and scale labels) as a ggplot2
  object that you can add layers to with `+`. It accepts a set of scale
  `angles` and `labels`, or a `circumplex_instrument` object to derive both
  automatically. The package's own `ssm_plot_circle()` draws on the same canvas.
  `ggcircumplex()` and `scale_x_circumplex()` label and place circumplex scales
  at their exact angles, including non-integer angles (for example, the
  22.5-degree spacing of a 16-scale instrument), instead of rounding them to
  whole degrees.

* New `geom_ssm_point()` and `geom_ssm_arc()` layers draw SSM profile points
  and their confidence-region arcs directly in circumplex space on a
  `ggcircumplex()` canvas, taking amplitude and displacement as aesthetics and
  handling the polar transform (including wrap-around at the 0/360 degree
  boundary) internally. These make it possible to build custom circumplex
  figures by composing ggplot2 layers.

* New `scale_x_circumplex()` provides an angle-labeled x-axis scale for linear
  circumplex plots (such as the score-by-angle curve). It labels axis breaks
  with their angle in degrees by default, or with custom labels or a
  `circumplex_instrument`'s scale abbreviations, using the same conventions as
  `ggcircumplex()`.

* The circumplex ggplot2 layers are extensible and ergonomic. The
  `GeomSsmPoint`, `GeomSsmArc`, and `CoordCircumplex` ggproto generators are
  exported so downstream packages can subclass them. The amplitude (radial) axis
  and its labels are drawn in the widest gap between the displacement spokes,
  so they no longer overlap a spoke label; `coord_circumplex()` gains an
  `r_axis_angle` argument to place it manually. The canvas theme is exported as
  `theme_circumplex()`. `geom_ssm_point()` and `geom_ssm_arc()` follow the
  ggplot2 `na.rm` convention: with `na.rm = FALSE` they warn (with the count)
  before dropping profiles that cannot be placed, while the default
  `na.rm = TRUE` drops them silently. `ssm_plot_circle(repel = TRUE)` now gives
  a clear error when the suggested `ggrepel` package is not installed.

* The new `geom_ssm_path()` layer draws a profile's movement across occasions as
  a path on the circumplex canvas, so change in amplitude and displacement reads
  as motion in circumplex space rather than only as separate parameter panels.
  Each segment is curved along the circle by `coord_circumplex()`. Consecutive
  occasions are joined the short way around the 0/360 boundary, so a step from
  350 to 10 degrees is drawn as a 20 degree arc across the pole rather than a
  340 degree sweep the long way round. Occasions are connected in data order,
  with `group` separating one series from another and an optional `order`
  aesthetic to sort within a series; an optional `arrow` marks the direction of
  time. An occasion with no defined displacement (a flat or zero-amplitude
  profile) breaks the path rather than being interpolated through, and the
  segment after the gap is still drawn on the correct branch.

* `ssm_plot_circle()` gains a `path` argument that adds this movement path to
  its usual points and confidence wedges, for results from
  `ssm_analyze(occasions = )` and `ssm_analyze_long()`. Occasions are connected
  in the order they were supplied, never alphabetically.

* The new `ssm_plot_trajectory()` plots how each SSM parameter changes across
  occasions, one panel per parameter with its confidence interval as a band, for
  results from `ssm_analyze(occasions = )` and `ssm_analyze_long()`. The
  displacement panel is drawn on an unwrapped branch, so a profile whose
  displacement crosses the 0/360 boundary is shown as one continuous path
  instead of jumping a full turn, and each confidence bound is placed on its own
  estimate's branch. Occasions appear in the order they were supplied, never
  alphabetically. An occasion whose amplitude is too close to zero for its
  displacement to be interpretable is marked with a hollow point, and a profile
  with no defined displacement leaves a gap rather than a spurious segment.

* `ssm_plot_trajectory()` also accepts a **trajectory table**: a data frame with
  one row per time point, a numeric time column named by the new `time`
  argument, and `a_est`/`a_lci`/`a_uci` and `d_est`/`d_lci`/`d_uci` columns
  (optionally the `e_*`, `x_*`, and `y_*` triples and a logical `certified`
  column). This is the shape a model-based workflow assembles by evaluating a
  fitted growth model at each time point and passing the draws through
  `ssm_draws()`, and it is plotted on a continuous time axis, so unequally
  spaced time points are drawn at their actual spacing. Only the panels the
  table can fill are drawn. The displacement unwrap, the interval placement, and
  the hollow marking of uninterpretable time points are shared with the
  occasions path; when no `certified` column is supplied, the figure makes no
  interpretability claim rather than asserting one.

* New `plot()` method for `circumplex_cpm` objects draws the estimated item
  configuration on the `ggcircumplex()` canvas: each scale appears at its
  estimated angle and at a radius given by its communality, with a wedge
  spanning its angle and communality confidence intervals where these are
  estimable (scales with an inestimable interval are drawn as a point only and
  named).

* The amplitude axis labels are now drawn over a translucent backdrop, so they
  stay readable where a data layer falls behind them. The amplitude axis is
  drawn on top of the plotted data, which kept the labels visible but not
  legible: a label crossing a dark marker, an arrowhead, or a dense scatter had
  too little contrast against it to read. The backdrop is deliberately
  translucent rather than opaque, so it restores contrast without hiding the
  data it covers.

## Instrument data

* The normative data shipped with `csie`, `csig`, `csip`, `csiv` and `iitc` has
  been re-verified against its published sources, value by value. Every mean,
  standard deviation and sample size was confirmed correct, as was every
  item-to-scale assignment its source publishes; no norm value changed.
* The source recorded for two of those instruments did change. `csiv` now
  reports its norms as unpublished data from the instrument's author rather
  than attributing them to Locke (2000), whose article reports a different
  sample and publishes no octant statistics. The `URL` recorded for `csie` and
  `csiv` now points at the author's current norms tables; the previous
  addresses had been redirected to a site homepage. Use `norms()` to see the
  provenance recorded for any instrument.
* `?norms` now states that the population shown for a normative sample is a
  short standardized label chosen by this package, deliberately broader than
  the description the original source gives.
* `norm_standardize()` now works with `iei`. The sample column of the IEI's
  normative data had been built so that its two samples were interleaved rather
  than stacked, which left each sample holding four octants twice and four not
  at all; standardizing against either IEI sample failed with an error about
  duplicate angles. No normative value was wrong, and no other instrument was
  affected.
* The reference recorded for the `iei` norms misspelled the second author's
  name and now reads Horner, Locke, & Hulsey (2024).
* The normative data shipped with `iis32`, `iis64`, `ipipipc` and `isc` has now
  been re-verified the same way. For `iis64` and `isc`, every mean, standard
  deviation and sample size was confirmed correct against the published source,
  as was every item-to-scale assignment the source publishes.
* For `iis32` and `ipipipc`, it could not be. Neither instrument's cited article
  publishes the octant means and standard deviations the package ships:
  Hatcher and Rogers (2012) reports no descriptive statistics at all, and
  Markey and Markey (2009) reports them only for a sample other than the one
  the package names. No other source for them has been identified. The values
  ship unchanged, since nothing establishes they are wrong either, but the
  `Reference` recorded for each now says the norms source is unconfirmed
  instead of crediting an article that does not carry them, and `?iis32` and
  `?ipipipc` say the same. Standardized scores from these two instruments should
  be treated as resting on unverified norms.
* Four item texts were wrong against their sources and are corrected. In
  `iis64`, item 5 had been truncated to "I realize " and now reads "I realize
  that I don't have to be friends with everyone", and item 7 read "not
  agreeable with others" where the source prints "not agreeable to others". In
  `iis32`, item 28 read "I'm ok with not being included in all activities"
  where its own source prints "okay" (the wording differs between the two IIS
  articles). In `ipipipc`, item 16 read "Don't fall for sob-stories" where the
  source prints "sob stories".
* The sources cited for `iitc` and `iei` were recorded as "in press" and now
  give the published citations.
* **`cais` scores change.** The CAIS item-to-scale key was wrong and is
  corrected. The instrument's 37 items are not distributed evenly across the
  eight octants — its source assigns five items each to PA, BC, DE, HI, LM and
  NO, four to FG and three to JK — but the key shipped four items per octant,
  which put one Warm-Agreeable item into Unassuming-Ingenuous, one
  Gregarious-Extraverted item into Warm-Agreeable and one Assured-Dominant item
  into Gregarious-Extraverted, and left the last five items scored into no scale
  at all. `score()` and anything downstream of it therefore returned different
  values than the instrument's authors defined, and `norm_standardize()`
  compared those values against norms computed the correct way. **Seven of the
  eight octants change under the correction:** PA gains the item that had been
  scored as Gregarious-Extraverted, BC, DE and HI each gain one of the
  previously unscored items, JK loses an item, and LM and NO each lose one item
  and gain two. Only FG (items 4, 12, 20 and 28) is unchanged. Re-run any CAIS analysis. The item text and its ordering were
  correct throughout.
* The normative data shipped with `cais`, `iei`, `igicr` and `iipsc` has now
  been re-verified against its published sources the same way as the other nine.
  Every mean and standard deviation of all nine normative samples was confirmed
  correct, as was every scale angle the sources publish. Four shipped values did
  not match their source and are corrected: the `cais` item-to-scale key above,
  and the three provenance records below.
* Three provenance records changed with that verification. The `cais` sample
  size for the child sample now reports 204, the sample size printed on the
  table its means and standard deviations come from, rather than the 213 given
  for the child sample elsewhere in the same article. The `iipsc`
  college-student norms were credited to a 2011 publication and now name
  Hopwood, Pincus, DeMoor, & Koonce (2008), the article that publishes them,
  which is also the DOI the instrument already recorded; `?iipsc` now cites both
  of its normative sources rather than only one. The `iei` `URL` pointed at the
  study's data repository, which publishes neither of its normative tables, and
  now gives one address per sample: the author's IEI norms page for the
  undergraduate sample and the article for the community sample.
* Every bundled instrument's item-to-scale key is now checked to cover each of
  the instrument's items exactly once, so a key that silently drops or
  double-counts an item — the `cais` failure above — cannot ship again.
* The normative data shipped with `iip32` and `iip64` has now been re-verified
  against the IIP professional manual, which completes the sweep: every one of
  the fifteen bundled instruments has now been checked against a published
  source, though for `iis32` and `ipipipc` that check is what established that
  no source publishes their values. All 96 means and standard deviations of the
  six IIP normative samples were confirmed correct, as were all 96 item-to-scale
  assignments and the three IIP-64 sample sizes. No value changed. The manual
  prints no sample sizes for the IIP-32 specifically, so its 800/400/400 are
  carried over from the same standardization sample the manual describes for the
  longer form, which is what the shorter form was scored from.
* `?iip32` and `?iip64` now cite the third edition of the manual (Horowitz,
  Alden, Wiggins, & Pincus, 2003, Mind Garden), the edition the shipped values
  were verified against, rather than the earlier edition from a different
  publisher; both help pages also carry the credit line the publisher's
  reproduction permission requires for the normative statistics.
* The `Population` recorded for both IIP instruments' normative samples now
  describes them as a national standardization sample rather than as community
  adults, which is what the manual reports: 800 adults sampled to be
  representative of the U.S. adult population, with separate norms for women
  and men.

## Documentation

* New vignette, "Evaluating Circumplex Structure": how to test whether an
  instrument fits a circumplex in your sample with `cpm_fit()` (reading and
  benchmarking the fit indices, comparing the constrained model variants,
  and the boundary-solution/chi-square cautions from the package's
  validation simulations), and how to check whether SSM confidence
  intervals can be trusted at your sample size and profile with
  `ssm_ci_accuracy()`. Summarizes Zimmermann & Wright's (2017) simulation
  findings as cited context (transcribed from the published article),
  reproduces their Study 5 analyses on the bundled `jz2017` data, and adds
  guidance on when to trust each SSM parameter and on what ipsatizing
  octant scores costs an SSM analysis. The diagnostic itself was validated
  against the article: configured to transcribed Zimmermann & Wright
  simulation conditions, it reproduces their published accuracy
  classifications (validation scripts and results are recorded in the
  package's development repository).

* New vignette, "SEM-Based SSM Analysis," teaching the latent SSM: the
  disattenuated estimand and how it differs from the observed profile, why
  amplitude and displacement intervals are built in-package rather than by
  lavaan, the two group-difference estimands (observed vs. invariance-gated
  latent) side by side, and the model-conditional assumptions that make the
  latent parameters interpretable.

* New precomputed vignette, "Bayesian SSM Analysis," derives the
  cosine-regression mapping (pinning the atan2 argument order with an
  executable known-direction check), walks a brms random-intercept example
  whose posterior draws ship with the package, and exhibits the Rayleigh-shaped
  prior that independent (x, y) priors induce on amplitude (brms is a new
  optional `Suggests` dependency used only by that vignette's frozen
  model-fitting chunk).

* New vignette, "Advanced Circumplex Visualization," teaches the plotting API:
  `coord_circumplex()` as the owner of the amplitude-to-radius mapping, the
  configurable circle center and amplitude-axis placement, restyling the canvas
  through `theme_circumplex()` and ordinary `theme()` calls, subclassing the
  exported `GeomSsmPoint`/`GeomSsmArc` objects to build reusable layers, and
  plotting a trajectory across occasions.

* The reference index now groups the plotting API into "Complete Plots" and
  "Building Blocks". The `ssm_plot_*` functions cross-link to each other, so
  `ssm_plot_trajectory()` is reachable from its siblings' help pages, and the
  composable layers (`ggcircumplex()`, `coord_circumplex()`, the `geom_ssm_*()`
  layers, `scale_x_circumplex()`, and `theme_circumplex()`) likewise cross-link
  to each other.

* Clarified in the documentation of `ssm_parameters()`, `ssm_score()`, and
  `ssm_analyze()` that the reported model fit is a bounded R-squared in
  `[0, 1]` for equally spaced `angles` (more generally, for any angle set
  satisfying first- and second-harmonic balance); for angle sets violating
  that balance the closed-form estimator is not a least-squares fit and the
  reported fit can fall below 0.

## Bug fixes

* Fixed a bug where a bootstrap resample under pairwise deletion
  (`listwise = FALSE`) could crash `ssm_analyze()` with `mean(): object has no
  elements` when the resample happened to draw only missing values for one
  scale. Such a scale now yields an `NA` mean (matching the correlation path),
  and the affected resample is excluded from the confidence intervals as a
  degenerate profile, consistent with the existing degeneracy handling.

* Fixed a bug where the displacement of a group contrast between two exactly
  opposed profiles (a half-turn apart) was reported as `-180` degrees instead
  of `+180`, inconsistent with the documented `(-180, 180]` convention for
  contrasts. Such a contrast is now reported as `+180`.

## Other

* `instruments()` now derives its listing from the bundled instrument data
  rather than a hardcoded table, so it always reflects the instruments
  actually shipped. As part of this, the listed name for the IIP-SC now
  reads "Inventory of Interpersonal Problems Short Circumplex" (matching its
  stored metadata).

# circumplex 1.2.0

* The SSM plotting functions (`ssm_plot_circle()`, `ssm_plot_curve()`,
  `ssm_plot_contrast()`) now warn when given an unrecognized argument (e.g., a
  misspelled parameter name) instead of silently ignoring it.
* Matrix input now works wherever it is documented. `ssm_analyze()`,
  `ssm_score()`, `ipsatize()`, `score()`, `norm_standardize()`, and
  `self_standardize()` previously errored when given a matrix despite
  advertising matrix support; they now coerce it to a data frame internally.
* `ssm_score()` now accepts numeric column indexes for `scales` (e.g.,
  `scales = 1:8`), consistent with its documentation and with `ssm_analyze()`;
  it previously required character names.
* Printing an SSM object (via `print()` or `summary()`) now adds a note under
  any profile whose model fit is inadequate (R-squared < .70; interpret only
  elevation) or whose amplitude confidence interval includes zero (the
  displacement is not interpretable). The notes apply to profiles only, not to
  contrast rows.
* Contrast displacement estimates and their confidence intervals are now
  always reported on the same angular branch. Previously, for contrasts near
  ±180 degrees, the estimate (reported in (-180, 180]) could fall numerically
  outside a confidence interval it was geometrically inside, because the
  interval was centered on the bootstrap circular mean's own branch. The
  interval is now shifted by a full circle when needed (its width and meaning
  are unchanged; results away from the boundary are identical), so interval
  endpoints may exceed ±180 degrees when the contrast straddles the boundary.
* `norm_standardize()` now matches each scale to its normative data by angular
  position rather than exact numeric equality, so 0 and 360 degrees are treated
  as the same angle (previously passing 0 for a scale stored at 360 failed with
  a cryptic error). An angle with no matching normative row, or with more than
  one, now produces an informative error naming the available angles.
* Degenerate profiles are now handled explicitly instead of returning
  numerical noise. A flat (zero-variance) profile returns `NA` displacement
  and fit with a warning (previously an arbitrary angle and `-Inf`); a profile
  with real variance but zero amplitude returns `NA` displacement and a fit
  of 0. Bootstrap resamples that produce degenerate profiles (e.g., a
  resampled measure with zero variance) no longer crash `ssm_analyze()`; they
  are excluded from the confidence intervals with a warning reporting the
  count. Genuinely small amplitudes are unaffected — the degeneracy test
  operates at machine-noise scale only.
* Fixed a bug where a missing (`NA`) value in the `grouping` variable of
  `ssm_analyze()` crashed with a cryptic error under pairwise deletion
  (`listwise = FALSE`). Such observations are now dropped before analysis with
  a message reporting how many were removed, in both deletion modes; if no
  observations remain, a clear error is given.
* Fixed a bug where length requirements on character arguments were never
  enforced (`is_null_or_char()` dropped its `n` argument). `ssm_analyze()` now
  errors if `measures_labels` does not match the number of `measures` (or is
  given without `measures`), `ssm_plot_circle()`/`ssm_plot_curve()` now error
  if `angle_labels` does not match the number of angles (previously mismatched
  labels could be silently recycled onto the wrong scales), and
  `ssm_table()`/`html_render()` now require `caption` to be a single string.
* Fixed a bug where `ssm_score()` silently ignored its `angles` argument and
  always used `octants()`: custom angle sets of the same length produced
  incorrect results without warning, and angle sets of a different length
  (e.g., `poles()` with four scales) errored. Results from `ssm_score()` with
  the default `angles = octants()` are unaffected. (found in 2026-07 audit)

# circumplex 1.1.0

## Minor improvements and fixes

* Improve handling of radian distributions crossing the 0/2pi boundary

* Add unit tests regarding the above cases

* Optimize pairwise correlation C++ code

* Fix bug with angular median calculation retaining rejected candidates

# circumplex 1.0.2

## Minor improvements and fixes

* Update RcppArmadillo dependency

* Fix some deprecated ggplot args

# circumplex 1.0.1

## New features

* Add the `self_standardize()` function for standardizing variables using sample means and SDs

## Minor improvements and fixes

* Fix some typos in documentation

* Change plot tests to accommodate changes to ggplot2

# circumplex 1.0.0

## Breaking changes

* Nearly all code rewritten/refactored to streamline and reduce dependencies.

* Removed support for non-standard evaluation

* The `contrast` argument to `ssm_analyze()` is now TRUE or FALSE instead of "none", "model", or "test". Model contrasts were removed and TRUE yields test contrasts.

* Many arguments renamed (e.g., `.data` to `data`, `.ssm_object` to `ssm_object`, `xy` to `drop_xy`)

* Removed `ssm_plot()` function in favor of `ssm_plot_circle()`, `ssm_plot_curve()`, and `ssm_plot_contrast()`.

* Renamed `standardize()` function to `norm_standardize()`

## New features

* Added `ssm_plot_curve()`

* Added CAIS and IEI instrument data

* Added profile scores, results, and plotting to models with contrasts

* Added `PANO()` function for conveniently creating scale names

* All internal and external data are now data frames instead of tibbles

* Rewrote all vignettes to use the updated functions, arguments, etc.

## Minor improvements and fixes

* Harmonized the `results` and `scores` fields in the output of `ssm_analyze()`

* Added many unit tests, increasing the package to 100% code coverage

* Added many assertions to check for invalid input arguments

* Harmonized the tidying function arguments (e.g., `prefix`, `suffix`, `append`)

* Added print methods for degree and radian classes

* Replace internal non-standard evaluation with `.data` references

* Minor visual improvements to print and summary methods for ssm_objects

# circumplex 0.3.10

## Minor improvements and fixes

* Fix a bug when comparing R versions

* Update \{vdiffr\} tests

* Update GitHub Actions

----

# circumplex 0.3.9

## Minor improvements and fixes

* Fixed a bug related to `NaN` values and `dplyr::na_if()`

* Updated package website using new version of \{pkgdown\}

----

# circumplex 0.3.8

## Minor improvements and fixes

* Fix testing error on Solaris systems

* Update package description paragraph

* Add cpp11 plugin for Rcpp

* Exclude devel folder from linguist statistics

----

# circumplex 0.3.7

## New features

* Add `angle_labels` argument to `ssm_plot()` to allow users to customize the angle labels around a circular plot

* Add `palette` argument to `ssm_plot()` to allow users to customize the color palette (from \{RColorBrewer\}) of a circular plot

* Replaced the `font_size` argument to `ssm_plot()` with the `legend_font_size` and `scale_font_size` arguments to allow users to customize the font size of different elements of a circular plot

## Minor improvements and fixes

* Update `ggsave()` documentation for future compatibility

* Update \{Rcpp\} code for future compatibility

* Added a black border to the points in a circular plot to greater distinguish them visually

* Change CI notation from [] to () to play nice with pandoc

* Update to \{testthat\} 3E and add `ssm_plot()` tests using \{vdiffr\}

* Recompile vignettes with new version of \{roxygen2\}

* Replace TravisCI with GitHub Actions

----

# circumplex 0.3.6

## Minor improvements and fixes

* Update dependency versions and require R >= 3.4.0

* Fix issues related to how R 4.0.0 handles S3 methods

* Modernize ssm_plot() function to use new tidyr syntax

* Update travis CI configuration to be more explicit

----

# circumplex 0.3.5

## Minor improvements and fixes

* Remove several unit tests that were causing problems for CRAN checks

----

# circumplex 0.3.4

## Minor improvements and fixes

* Adjust the test of `quantile.radian()` to account for changes to `%%` starting in R 3.6.1 Patched

* Add the name of the package to the S3 class names (e.g., `circumplex_radian` instead of `radian`) to minimize the risk of overlapping classes between packages

* Add some supplementary files to the R build ignore list to avoid notes during CRAN check

----

# circumplex 0.3.3

## Minor improvements and fixes

* Add APA-style citations to instrument documentation in addition to DOI links.

* Add "Instruments" menu to package website for viewing documentation pages.

* Adjust the test of `quantile.radian()` to account for changes to `%%` starting in R 4.0.0

----

# circumplex 0.3.2

## New features

* New `iitc` provides instrument information for the Inventory of Influence
  Tactics Circumplex.

## Minor improvements and fixes

* Fix CRAN warnings by setting `LazyData: true`.

* Fix CRAN note by replacing relative URLs with absolute URLs.

* Nonstandard evaluation is now handled using `{{}}` notation.

* Updated the formatting on this NEWS changelog to match tidyverse style.

----

# circumplex 0.3.1

## Minor improvements and fixes

* Avoid a bug with dplyr 0.8.1 and S3 methods on Linux systems.

* Update the web address for Johannes in the README document.

----

# circumplex 0.3.0

## New features

* New `ssm_parameters()` calculates SSM parameters (without
  confidence intervals) from a vector of scores.

* New `ssm_score()` calculates SSM parameters by row.

* Added support for older versions of R (3.3.x).

## Minor improvements and fixes

* Updated the "Introduction to SSM" vignette's figures.

* Replaced use of `dplyr::funs()` as this function is being deprecated.

* Fixed a bug in the normative data for `ipipipc` that prevented standardization.

* Fixed a bug caused by changes in how random numbers are generated in R 3.6.x.

* Fixed several broken links by running package through new version of `usethis`.

* Fixed warnings related to documentation inherited from other packages.

----

# circumplex 0.2.1

## New features

* `iis32` now has normative data.

* Added open-access (i.e., full item text) to the `iis32` and `iis64`.

## Minor improvements and fixes

* `iis32` item ordering and scoring now match the author's version.

* `iis32` response anchors now range from 1 to 6 and match norms.

* Changed use of `tibble` functions to avoid problems when new version releases.

* Removed dependency on `MASS` package (until it is used by exported functions).

----

# circumplex 0.2.0

## New features

* Added functions and documentation for numerous circumplex instruments.

* Added functions for ipsatizing and scoring item-level data.

* Added function for standardizing scale-level data using normative data.

## Minor improvements and fixes

* Changed OpenMP flags in Makevars to fix a compile problem on Debian machines.

* Fixed a bug related to calculating angular medians in the presence of NAs.

* Changed the default to plot profiles with low fit (but with dashed borders).

* Import and export functions from rlang tidy evaluation.

* Added unit testing of various functions to increase code coverage.

* Redesigned package website to be more attractive and clear.

* Updated the SSM vignette to use the `standardize()` function.

----

# circumplex 0.1.2

## New features

* `ssm_plot()` now uses dashed borders to indicate that a profile has low prototypicality/fit.

## Minor improvements and fixes

* Fixed bug that prevented compilation on Solaris systems.

* Fixed bug that prevented CRAN checks on old R versions.

* Improved the formatting of vignette source code.

----

# circumplex 0.1.1

## New features

* [Package website](https://circumplex.jmgirard.com) added using [pkgdown](https://pkgdown.r-lib.org/).

## Minor improvements and fixes

* Fixed documentation to meet CRAN standards.

----

# circumplex 0.1.0

* Package submitted to CRAN.
