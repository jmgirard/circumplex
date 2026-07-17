# Changelog

## circumplex (development version)

- New growth-model support for repeated-measures SSM analysis. A new
  vignette (“Growth Models on SSM Parameters”) documents the recommended
  recipe for modeling change in SSM parameters over time: fit a *joint*
  mixed model to the per-person Cartesian coordinates from
  [`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md)
  (the reference recipe uses glmmTMB, now in Suggests; fitting the
  coordinates with separate univariate models silently zeroes their
  cross-covariance and produces wrong displacement intervals), then
  convert fixed-effect draws to amplitude/displacement trajectories with
  [`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md).
  The recipe was validated by simulation: pointwise displacement
  coverage is nominal in a pole-crossing design, and the univariate
  shortcut demonstrably fails coverage under correlated person effects.

- New
  [`angle_unwrap()`](http://circumplex.jmgirard.com/dev/reference/angle_unwrap.md)
  helper unwraps a temporally ordered sequence of angles onto a
  continuous branch (350, 10, 30 becomes 350, 370, 390), supporting the
  vignette’s alternative unwrap-then-model recipe. Inputs are wrapped to
  \[0, 360) first; an exact 180-degree step ascends (the package’s
  half-turn convention); `NA` makes later waves branch-ambiguous and so
  propagates onward.

- [`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md)
  objects now apply the package’s displacement-certification rule to the
  amplitude credible interval: when the interval’s lower bound sits
  under 0.35 interval-widths above zero, printing notes that the
  displacement is not interpretable, and the verdict is stored in
  `$details$certified` (used per-timepoint by the growth vignette).

- New per-person (intraindividual) SSM scoring:
  [`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md)
  scores each person’s own circumplex profile through the closed-form
  SSM transform and returns a per-person parameter table – one row per
  person, with an `id` argument that first averages a person’s rows
  (e.g., occasions of intensive longitudinal data) within person before
  scoring. Degenerate profiles keep their row with `NA` parameters
  (never a silent drop), and an `na_rate` column exposes each person’s
  share of missing scale cells. A
  [`summary()`](https://rdrr.io/r/base/summary.html) method aggregates
  the table at the group level using circular statistics for
  displacement (circular mean and mean resultant length, never
  arithmetic means of angles), reporting how many undefined
  displacements were excluded. Two documented caveats: the circular mean
  of per-person displacements (equal weight per person) is a different
  quantity from the displacement of the group mean profile
  (amplitude-weighted), and by the triangle inequality the group
  profile’s amplitude is at most the mean per-person amplitude, strictly
  smaller when directions disperse.

- New Bayesian draws adapter:
  [`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md)
  converts posterior draws from a user-fitted Bayesian model (e.g., a
  brms cosine regression) into SSM parameter draws and summarizes them
  with the package’s circular-statistics machinery – circular quantiles
  for displacement (credible intervals that straddle 0/360 wrap instead
  of inverting), posterior medians for the linear parameters (the
  amplitude posterior is right-skewed), and the circular mean for
  displacement, with the marginal-coherence caveat documented. Two draw
  shapes are accepted and never guessed: (e, x, y) parameter draws
  (`type = "parameters"`, required because a 3-column matrix is
  ambiguous) and profile draws (one column per scale, with `angles`).
  Draws with undefined displacement are excluded from the displacement
  summaries only, with an honest warning that says “posterior draws” and
  “credible interval”. A new precomputed vignette, *Bayesian SSM
  Analysis*, derives the cosine-regression mapping (pinning the atan2
  argument order with an executable known-direction check), walks a brms
  random-intercept example whose posterior draws ship with the package,
  and exhibits the Rayleigh-shaped prior that independent (x, y) priors
  induce on amplitude (brms is a new optional `Suggests` dependency used
  only by that vignette’s frozen model-fitting chunk).

- New repeated-measures (longitudinal) SSM analyses:
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  gains an `occasions` argument taking a named list of column blocks,
  one per occasion, each selecting the same circumplex scales measured
  at that occasion (wide data, one row per person). Every occasion
  yields its own profile row, occasions cross with `grouping`, and
  `contrast = TRUE` with exactly two occasions (single group) estimates
  the paired within-person contrast – second listed occasion minus first
  – through both engines: the bootstrap resamples persons (preserving
  within-person dependence nonparametrically) and the Monte Carlo engine
  draws the stacked occasion mean vectors jointly. Cross-occasion column
  alignment is validated by stem matching (a reordered occasion block
  errors instead of silently rotating displacement). Occasions analyses
  are listwise-only across waves, with the dropped-person count messaged
  and a selection caution documented. Results from occasions analyses
  carry a new `Occasion` column that is present only for such analyses –
  downstream code should test for the column by name. Coverage of the
  paired contrasts was validated by simulation at nominal rate across
  boundary cells (displacement changes near 0 and 180 degrees, CIs
  straddling the 0/360 pole, small samples, three occasions); note that
  paired contrasts are not unconditionally more efficient than
  independent-groups designs (see the new Occasions section in
  [`?ssm_analyze`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)).
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  errors informatively on occasions objects for now.

- `cpm_fit(scaling = "free")` now also starts its optimizer from the
  unit-scaling solution, so the free family’s fit statistic can never
  exceed the default family’s on the same input beyond numerical
  tolerance (the free family mathematically nests the default;
  previously a rare multi-start tail — about 1 in 2,000 fits in
  simulation — could land the free optimizer on a slightly worse
  optimum). Results change only in those rare cases, and only for the
  better; the default family is unaffected. The convergence-acceptance
  criterion is unchanged and still requires the multi-start battery
  itself to reproduce the reported optimum, so a fit rescued by the new
  start may now carry the (accurate) acceptance warning where it
  previously reported a slightly worse optimum silently.

- Displacement and angle confidence-interval endpoints that land exactly
  on the 0/360 pole are now reported as 360, never 0, matching how the
  package labels that pole everywhere else (LM = 360):
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  bootstrap displacement CIs and
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  bootstrap angle CIs both use the shared circular-quantile machinery
  that now applies this labeling.
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)’s
  reported `Angle` column likewise labels the pole 360 — a reference
  scale with a theory angle of 360 previously printed `Angle = 0` with a
  degenerate CI of `[0, 0]`, and now prints 360 throughout. An
  exact-pole endpoint is a measure-zero floating-point corner for real
  data, so numeric results are otherwise unchanged.

- New SEM-based (latent-variable) SSM analysis:
  [`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)
  estimates the Structural Summary Method profile of one or more
  external measures against the *latent* circumplex content of the
  scales — the disattenuated analog of the correlation-based
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  — from a fixed-theoretical-angle measurement model fitted with lavaan
  (now a runtime Suggests dependency for this feature family; everything
  else works without it). Confidence intervals for all parameters are
  built in-package by propagating draws of the model’s free parameters
  (multivariate-normal by default, or a full lavaan bootstrap via
  `ci_method = "boot"`) through the profile and SSM transforms and the
  same circular-quantile machinery as
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  — never lavaan’s delta-method or percentile intervals, which ignore
  the angular branch cut. The default draws propagate lavaan’s robust
  (sandwich) covariance, which the package’s coverage validation found
  necessary to keep the intervals calibrated when the fixed-angle model
  only approximates the data; the default estimator is MLR, so the
  global fit indices [`print()`](https://rdrr.io/r/base/print.html)
  reports are likewise the robust/scaled versions (circumplex scale
  scores are typically skewed). Includes
  [`ssm_sem_syntax()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_syntax.md)
  (an inspectable lavaan model-syntax generator that works without
  lavaan installed) and
  [`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)
  (estimate from a lavaan fit you have modified or fitted yourself).
  Results are `circumplex_ssm` objects, so
  [`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md)
  and the `ssm_plot_*` functions work on them unchanged. With a
  `grouping` variable,
  [`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)
  fits the measurement model across groups and gates a latent group
  contrast on measurement invariance: it tests a
  configural-metric-scalar ladder using lavaan’s own nested-model test
  (the scaled difference test under robust estimators) at the
  `invariance` rung (defaulting to each path’s required level) and the
  `invariance_alpha` level, and computes the disattenuated contrast only
  when the required rung is retained. When invariance is rejected it
  reports an honest non-comparison — the verdict plus each group’s
  separate configural profile — rather than a contrast that would
  confound structural difference with measurement non-invariance.
  Supplying `grouping` without `measures` analyzes the latent mean path
  (each group’s model-implied latent mean profile). The observed-score
  group contrast in
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  remains the right tool when invariance cannot be assumed; it answers
  its own, different question.

- New vignette, “SEM-Based SSM Analysis,” teaching the latent SSM: the
  disattenuated estimand and how it differs from the observed profile,
  why amplitude and displacement intervals are built in-package rather
  than by lavaan, the two group-difference estimands (observed
  vs. invariance-gated latent) side by side, and the model-conditional
  assumptions that make the latent parameters interpretable.

This version’s flagship addition is
[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
a native reimplementation of Browne’s (1992) circular stochastic process
model for the correlational structure of circumplex scales — filling the
gap left by the archived CircE package, the previous R implementation.
It ships alongside
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
a new diagnostic for whether an
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
result’s confidence intervals can be trusted at your sample size and
profile (Zimmermann & Wright, 2017). See the new vignette, “Evaluating
Circumplex Structure”, for a worked introduction to both.

- The displacement-interpretability guardrail in
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) now uses a
  scale-free rule: a profile’s displacement is certified as
  interpretable only when the amplitude confidence interval’s lower
  bound sits at least 0.35 interval-widths above zero. This replaces the
  rule introduced in 1.2.0, which certified whenever the lower bound
  rounded above zero at the display precision — a threshold that moved
  with the print `digits` and meant different things on different score
  metrics, and that (as the new
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  diagnostic makes visible) certified a genuinely zero amplitude almost
  every time. The new rule holds false-certification near the interval’s
  one-sided error rate regardless of scale or display settings. As a
  result, some near-zero-amplitude profiles that were previously
  certified are now flagged uninterpretable. The threshold is calibrated
  for the default 95% confidence interval.

- New vignette, “Evaluating Circumplex Structure”: how to test whether
  an instrument fits a circumplex in your sample with
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  (reading and benchmarking the fit indices, comparing the constrained
  model variants, and the boundary-solution/chi-square cautions from the
  package’s validation simulations), and how to check whether SSM
  confidence intervals can be trusted at your sample size and profile
  with
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md).
  Summarizes Zimmermann & Wright’s (2017) simulation findings as cited
  context (transcribed from the published article), reproduces their
  Study 5 analyses on the bundled `jz2017` data, and adds guidance on
  when to trust each SSM parameter and on what ipsatizing octant scores
  costs an SSM analysis. The diagnostic itself was validated against the
  article: configured to transcribed Zimmermann & Wright simulation
  conditions, it reproduces their published accuracy classifications
  (validation scripts and results are recorded in the package’s
  development repository).

- New
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  function assesses, by simulation, whether the confidence intervals of
  an
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  result would cover the true SSM parameters at their nominal rate if
  the population looked like the fitted estimates, at the observed
  sample size(s) — the CI-trustworthiness diagnostic of Zimmermann &
  Wright (2017), generalized to the user’s own configuration (grouping,
  contrasts, measures, engine, resample count, and interval level). The
  population’s scale structure is characterized by a
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  model (or, optionally, the observed correlations); each simulated
  dataset replays the object’s own interval procedure. Coverage is
  reported per profile row, parameter, and amplitude condition — a
  ladder of populations with the amplitude scaled toward zero, where
  percentile amplitude intervals are theoretically weakest — along with
  one-sided miss rates, interval widths, the certification rate of the
  printed displacement-interpretability guardrail, and displacement
  coverage conditional on certification. For a contrast row — a signed
  difference that `print.circumplex_ssm()` never certification-gates —
  the displacement verdict and printed coverage are reported
  unconditionally, matching that profiles-only stance (its conditional
  coverage is retained in the object as a descriptive). Coverage at the
  as-estimated condition is classified against Bradley’s (1978) liberal
  robustness band using 95% Wilson score intervals, and
  [`print()`](https://rdrr.io/r/base/print.html)/[`summary()`](https://rdrr.io/r/base/summary.html)
  translate the classifications into a plain-language verdict, including
  a line reporting how often the guardrail would certify displacement if
  the true amplitude were zero (the scale-free rule holds this near the
  interval’s one-sided error rate, and a caution is raised only if it
  materially exceeds that).
  [`summary()`](https://rdrr.io/r/base/summary.html) also annotates the
  realism of the simulated population (structural-model convergence and
  fit, against conventional RMSEA/SRMR benchmarks with citations) and,
  when an amplitude estimate is itself below half its CI width, notes
  that the analysis already sits in the near-zero regime and adds a
  ladder rung at the certification margin. A
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method draws
  coverage across the amplitude ladder with the Bradley band shaded.
  Simulation replicates can be parallelized (`parallel`/`ncpus`) with
  seed-identical results, and the caller’s random-number state is
  restored on exit. To support the diagnostic,
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  now stores per-group sufficient statistics (sizes, scale SDs, and
  correlation matrices) in its output; objects created by earlier
  versions can be assessed by re-supplying the original data via
  `ssm_ci_accuracy(..., data = )`, which is checked for consistency
  against the stored profiles.

- New
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  function evaluates whether a set of scales forms a circumplex using
  the exploratory criteria of Acton & Revelle (2004). Four criteria are
  computed from the first two unrotated principal-axis factors of the
  scales’ correlations — the Fisher Test of equal axes, the Gap Test of
  equal spacing, and the Variance (VT2) and Rotation tests of
  interstitiality — and a fifth, the RANDALL correspondence index
  (Hubert & Arabie, 1987; Tracey, 1997), tests the hypothesized circular
  *order* of the scales with a randomization test that yields an exact
  p-value. The factor-analytic statistics are classified against
  interpretive cutoffs that were re-derived by simulation under Acton &
  Revelle’s own generating model for eight (octant) scales — their
  published cutoffs were calibrated on far more variables and do not
  transfer — and that are keyed to the scoring, since these criteria
  work best with a general factor removed.
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  deviation-scores (ipsatizes) by default for that reason, with a raw
  opt-out. Missing values are handled by listwise deletion by default (a
  `listwise` argument, matching
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)),
  so all five tests share one complete-case correlation matrix — the
  metric the cutoffs were calibrated on. The returned
  `circumplex_structure` object has
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods;
  interpretations are presented as the heuristic likelihood
  classifications they are, never as significance tests. See the
  “Evaluating Circumplex Structure” vignette.

- The Monte Carlo interval engine (`ssm_analyze(method = "montecarlo")`)
  is faster on correlation-based analyses: the influence-function
  covariance is built in one vectorized pass and all profile rows are
  propagated through the SSM transformation in a single compiled call.
  Results are unchanged (byte-identical for a fixed seed).

- New
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  function estimates Browne’s (1992) circular stochastic process model
  for the correlational structure of circumplex scales or items, a
  native replacement for the archived CircE package. It accepts either
  raw data or a correlation matrix, estimates item angles and
  communality indices (with four model variants), and reports the usual
  covariance-structure fit indices (chi-square, RMSEA with a 90%
  confidence interval, SRMR, CFI, TLI, AIC, BIC). The returned
  `circumplex_cpm` object has
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods. On the
  raw-data path, confidence intervals are estimated by a nonparametric
  bootstrap by default (resampling rows and refitting the model, with
  percentile intervals; angle intervals use the package’s circular
  quantile machinery, so an interval straddling the 0/360 degree
  boundary is reported wrapped, as with displacement intervals).
  Resamples that are degenerate or fail the convergence criterion are
  excluded with a warning and counted in the output. Only the bootstrap
  consumes R’s random number stream: call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) immediately before
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  for reproducible intervals (point estimates are deterministic). On the
  correlation-matrix path, intervals are analytic (Wald) — there is no
  raw data to resample — and
  [`summary()`](https://rdrr.io/r/base/summary.html) cautions when the
  sample size is small enough that these may mis-cover. A `scaling`
  argument selects the covariance-scaling family: `"unit"` (the default)
  fits the correlation structure, while `"free"` fits Browne’s
  covariance structure with `p` free variance scales — the
  parameterization CIRCUM and CircE use — so
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  can reproduce their published output exactly. Free scaling adds `p`
  parameters without changing the degrees of freedom, and reports the
  fitted variance ratios in a `VarRatio` column (without confidence
  intervals). With correlation input the two families’ model-test
  statistics are calibration-indistinguishable (paired simulation at
  sample sizes 250–50,000), so the default remains the recommended
  family for routine inference; use `scaling = "free"` when the goal is
  reproducing published CIRCUM/CircE output.

- The
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  estimator has been validated against the published CIRCUM/CircE
  literature (Grassi, Luccio, & Di Blas, 2010, reanalyzing Browne’s 1992
  vocational-interest example) and against independent OpenMx and lavaan
  implementations of the same model (both now in Suggests as test
  oracles only). CIRCUM and CircE fit Browne’s covariance
  parameterization with free variance scalings;
  `cpm_fit(scaling = "free")` fits that same family and reproduces their
  published estimates, chi-square, and fit indices to printed precision,
  while the default correlation-structure fit differs from them slightly
  in finite samples (same degrees of freedom, asymptotically
  equivalent); see the package’s design notes for details. A large
  seeded simulation study measured the coverage of both interval
  methods: based on its results,
  [`summary()`](https://rdrr.io/r/base/summary.html) now also cautions
  about analytic intervals at any sample size below 50,000 when the
  fitted solution is near a parameter boundary or weakly identified
  (Heywood case, removed harmonic, very small correlation-function
  weight, ill-conditioning, or competing near-tied optima — the caution
  names which), the regime where they measurably mis-covered. Percentile
  bootstrap intervals were confirmed as the better default but are
  themselves conservative-liberal in spots (notably for near-boundary
  correlation-function weights); improving them is planned follow-up
  work.

- New
  [`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md)
  function draws standardized observations from a fitted
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  model’s implied correlation matrix, using the model’s exact
  positive-semidefinite factor representation. It returns a numeric
  matrix with one column per scale (in fitted order, named), whose
  population correlation matrix is the fitted `Phat`. Call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) immediately before
  it for reproducible draws.

- New [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  for `circumplex_cpm` objects draws the estimated item configuration on
  the
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  canvas: each scale appears at its estimated angle and at a radius
  given by its communality, with a wedge spanning its angle and
  communality confidence intervals where these are estimable (scales
  with an inestimable interval are drawn as a point only and named).

- New
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  function builds an empty circumplex plotting canvas (amplitude rings,
  displacement spokes, and scale labels) as a ggplot2 object that you
  can add layers to with `+`. It accepts a set of scale `angles` and
  `labels`, or a `circumplex_instrument` object to derive both
  automatically. This is the first piece of a public circumplex
  visualization layer; the package’s own
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)
  draws on the same canvas.

- New
  [`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md)
  and
  [`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)
  layers draw SSM profile points and their confidence-region arcs
  directly in circumplex space on a
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  canvas, taking amplitude and displacement as aesthetics and handling
  the polar transform (including wrap-around at the 0/360 degree
  boundary) internally. These make it possible to build custom
  circumplex figures by composing ggplot2 layers.

- New
  [`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md)
  provides an angle-labeled x-axis scale for linear circumplex plots
  (such as the score-by-angle curve). It labels axis breaks with their
  angle in degrees by default, or with custom labels or a
  `circumplex_instrument`’s scale abbreviations, using the same
  conventions as
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md).

- New vignette “Advanced Circumplex Visualization” shows how to build
  custom circumplex figures by composing
  [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md),
  [`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md),
  [`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md),
  and
  [`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md)
  with other ggplot2 components.

- [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  and
  [`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md)
  now label and place circumplex scales at their exact angles, including
  non-integer angles (for example, the 22.5-degree spacing of a 16-scale
  instrument), instead of rounding them to whole degrees.

- [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)
  now warns and names any profile it cannot place on the circle because
  its displacement is undefined (a flat or zero-amplitude profile),
  instead of dropping it from the figure without notice.

- [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  gains a `method` argument offering a Monte Carlo alternative to the
  bootstrap (`method = "montecarlo"`): SSM parameter replicates are
  drawn from the asymptotic sampling distribution of the group mean
  vector or measure-scale correlation vector (a multivariate normal with
  empirically estimated covariance; correlations are drawn jointly
  across measures on the Fisher z scale) and propagated through the SSM
  transformation. It produces intervals closely matching the bootstrap
  on large samples while running in a fraction of the time, but relies
  on asymptotic normality and requires listwise-complete data, so the
  bootstrap remains the default and the recommended choice for small
  samples. [`summary()`](https://rdrr.io/r/base/summary.html) reports
  which method produced the intervals.

- [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  gains `parallel` and `ncpus` arguments (passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html)) to
  distribute the bootstrap computation across multiple CPU cores.
  Because the resample indices are drawn in the main R process before
  any work is distributed, results for a given
  [`set.seed()`](https://rdrr.io/r/base/Random.html) are identical
  regardless of these settings, so parallelizing never changes your
  estimates or confidence intervals.

- [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  is now vectorized internally (one compiled call instead of a row-wise
  loop), making it much faster on large data sets. Results are
  unchanged. Rows whose profile has undefined displacement now produce a
  single warning reporting how many such rows there are, rather than one
  warning per row. Extra arguments passed through `...` must now be
  named (e.g. `prefix = "IIP_"`) and must be single strings; an unnamed
  or non-scalar argument is now an error rather than being silently
  ignored (previously it could yield unlabeled or garbled output
  columns).

- Fixed a bug where a bootstrap resample under pairwise deletion
  (`listwise = FALSE`) could crash
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  with `mean(): object has no elements` when the resample happened to
  draw only missing values for one scale. Such a scale now yields an
  `NA` mean (matching the correlation path), and the affected resample
  is excluded from the confidence intervals as a degenerate profile,
  consistent with the existing degeneracy handling.

- Clarified in the documentation of
  [`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md),
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
  and
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  that the reported model fit is a bounded R-squared in `[0, 1]` for
  equally spaced `angles` (more generally, for any angle set satisfying
  first- and second-harmonic balance); for angle sets violating that
  balance the closed-form estimator is not a least-squares fit and the
  reported fit can fall below 0.

- Fixed a bug where the displacement of a group contrast between two
  exactly opposed profiles (a half-turn apart) was reported as `-180`
  degrees instead of `+180`, inconsistent with the documented
  `(-180, 180]` convention for contrasts. Such a contrast is now
  reported as `+180`.

- Count-valued arguments (e.g. `boots`, `reps`, `ncpus`, `digits`, and
  the sample size `n`) across
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md),
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md),
  [`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
  and
  [`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)
  are now uniformly validated as a single non-negative whole number. A
  few of these previously accepted a length-greater-than-one vector
  without complaint; such input now raises a clear error.

- [`instruments()`](http://circumplex.jmgirard.com/dev/reference/instruments.md)
  now derives its listing from the bundled instrument data rather than a
  hardcoded table, so it always reflects the instruments actually
  shipped. As part of this, the listed name for the IIP-SC now reads
  “Inventory of Interpersonal Problems Short Circumplex” (matching its
  stored metadata).

## circumplex 1.2.0

CRAN release: 2026-07-02

- The SSM plotting functions
  ([`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md),
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md),
  [`ssm_plot_contrast()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_contrast.md))
  now warn when given an unrecognized argument (e.g., a misspelled
  parameter name) instead of silently ignoring it.
- Matrix input now works wherever it is documented.
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md),
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
  [`ipsatize()`](http://circumplex.jmgirard.com/dev/reference/ipsatize.md),
  [`score()`](http://circumplex.jmgirard.com/dev/reference/score.md),
  [`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md),
  and
  [`self_standardize()`](http://circumplex.jmgirard.com/dev/reference/self_standardize.md)
  previously errored when given a matrix despite advertising matrix
  support; they now coerce it to a data frame internally.
- [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  now accepts numeric column indexes for `scales` (e.g.,
  `scales = 1:8`), consistent with its documentation and with
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md);
  it previously required character names.
- Printing an SSM object (via
  [`print()`](https://rdrr.io/r/base/print.html) or
  [`summary()`](https://rdrr.io/r/base/summary.html)) now adds a note
  under any profile whose model fit is inadequate (R-squared \< .70;
  interpret only elevation) or whose amplitude confidence interval
  includes zero (the displacement is not interpretable). The notes apply
  to profiles only, not to contrast rows.
- Contrast displacement estimates and their confidence intervals are now
  always reported on the same angular branch. Previously, for contrasts
  near ±180 degrees, the estimate (reported in (-180, 180\]) could fall
  numerically outside a confidence interval it was geometrically inside,
  because the interval was centered on the bootstrap circular mean’s own
  branch. The interval is now shifted by a full circle when needed (its
  width and meaning are unchanged; results away from the boundary are
  identical), so interval endpoints may exceed ±180 degrees when the
  contrast straddles the boundary.
- [`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md)
  now matches each scale to its normative data by angular position
  rather than exact numeric equality, so 0 and 360 degrees are treated
  as the same angle (previously passing 0 for a scale stored at 360
  failed with a cryptic error). An angle with no matching normative row,
  or with more than one, now produces an informative error naming the
  available angles.
- Degenerate profiles are now handled explicitly instead of returning
  numerical noise. A flat (zero-variance) profile returns `NA`
  displacement and fit with a warning (previously an arbitrary angle and
  `-Inf`); a profile with real variance but zero amplitude returns `NA`
  displacement and a fit of 0. Bootstrap resamples that produce
  degenerate profiles (e.g., a resampled measure with zero variance) no
  longer crash
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md);
  they are excluded from the confidence intervals with a warning
  reporting the count. Genuinely small amplitudes are unaffected — the
  degeneracy test operates at machine-noise scale only.
- Fixed a bug where a missing (`NA`) value in the `grouping` variable of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  crashed with a cryptic error under pairwise deletion
  (`listwise = FALSE`). Such observations are now dropped before
  analysis with a message reporting how many were removed, in both
  deletion modes; if no observations remain, a clear error is given.
- Fixed a bug where length requirements on character arguments were
  never enforced (`is_null_or_char()` dropped its `n` argument).
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  now errors if `measures_labels` does not match the number of
  `measures` (or is given without `measures`),
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)/[`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md)
  now error if `angle_labels` does not match the number of angles
  (previously mismatched labels could be silently recycled onto the
  wrong scales), and
  [`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md)/[`html_render()`](http://circumplex.jmgirard.com/dev/reference/html_render.md)
  now require `caption` to be a single string.
- Fixed a bug where
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  silently ignored its `angles` argument and always used
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md):
  custom angle sets of the same length produced incorrect results
  without warning, and angle sets of a different length (e.g.,
  [`poles()`](http://circumplex.jmgirard.com/dev/reference/poles.md)
  with four scales) errored. Results from
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  with the default `angles = octants()` are unaffected. (found in
  2026-07 audit)

## circumplex 1.1.0

CRAN release: 2026-05-24

### Minor improvements and fixes

- Improve handling of radian distributions crossing the 0/2pi boundary

- Add unit tests regarding the above cases

- Optimize pairwise correlation C++ code

- Fix bug with angular median calculation retaining rejected candidates

## circumplex 1.0.2

CRAN release: 2025-09-23

### Minor improvements and fixes

- Update RcppArmadillo dependency

- Fix some deprecated ggplot args

## circumplex 1.0.1

CRAN release: 2025-07-28

### New features

- Add the
  [`self_standardize()`](http://circumplex.jmgirard.com/dev/reference/self_standardize.md)
  function for standardizing variables using sample means and SDs

### Minor improvements and fixes

- Fix some typos in documentation

- Change plot tests to accommodate changes to ggplot2

## circumplex 1.0.0

CRAN release: 2024-10-28

### Breaking changes

- Nearly all code rewritten/refactored to streamline and reduce
  dependencies.

- Removed support for non-standard evaluation

- The `contrast` argument to
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  is now TRUE or FALSE instead of “none”, “model”, or “test”. Model
  contrasts were removed and TRUE yields test contrasts.

- Many arguments renamed (e.g., `.data` to `data`, `.ssm_object` to
  `ssm_object`, `xy` to `drop_xy`)

- Removed `ssm_plot()` function in favor of
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md),
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md),
  and
  [`ssm_plot_contrast()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_contrast.md).

- Renamed `standardize()` function to
  [`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md)

### New features

- Added
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md)

- Added CAIS and IEI instrument data

- Added profile scores, results, and plotting to models with contrasts

- Added [`PANO()`](http://circumplex.jmgirard.com/dev/reference/PANO.md)
  function for conveniently creating scale names

- All internal and external data are now data frames instead of tibbles

- Rewrote all vignettes to use the updated functions, arguments, etc.

### Minor improvements and fixes

- Harmonized the `results` and `scores` fields in the output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)

- Added many unit tests, increasing the package to 100% code coverage

- Added many assertions to check for invalid input arguments

- Harmonized the tidying function arguments (e.g., `prefix`, `suffix`,
  `append`)

- Added print methods for degree and radian classes

- Replace internal non-standard evaluation with `.data` references

- Minor visual improvements to print and summary methods for ssm_objects

## circumplex 0.3.10

CRAN release: 2023-08-22

### Minor improvements and fixes

- Fix a bug when comparing R versions

- Update {vdiffr} tests

- Update GitHub Actions

------------------------------------------------------------------------

## circumplex 0.3.9

CRAN release: 2023-02-14

### Minor improvements and fixes

- Fixed a bug related to `NaN` values and
  [`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)

- Updated package website using new version of {pkgdown}

------------------------------------------------------------------------

## circumplex 0.3.8

CRAN release: 2021-05-28

### Minor improvements and fixes

- Fix testing error on Solaris systems

- Update package description paragraph

- Add cpp11 plugin for Rcpp

- Exclude devel folder from linguist statistics

------------------------------------------------------------------------

## circumplex 0.3.7

CRAN release: 2021-05-17

### New features

- Add `angle_labels` argument to `ssm_plot()` to allow users to
  customize the angle labels around a circular plot

- Add `palette` argument to `ssm_plot()` to allow users to customize the
  color palette (from {RColorBrewer}) of a circular plot

- Replaced the `font_size` argument to `ssm_plot()` with the
  `legend_font_size` and `scale_font_size` arguments to allow users to
  customize the font size of different elements of a circular plot

### Minor improvements and fixes

- Update
  [`ggsave()`](http://circumplex.jmgirard.com/dev/reference/ggsave.md)
  documentation for future compatibility

- Update {Rcpp} code for future compatibility

- Added a black border to the points in a circular plot to greater
  distinguish them visually

- Change CI notation from \[\] to () to play nice with pandoc

- Update to {testthat} 3E and add `ssm_plot()` tests using {vdiffr}

- Recompile vignettes with new version of {roxygen2}

- Replace TravisCI with GitHub Actions

------------------------------------------------------------------------

## circumplex 0.3.6

CRAN release: 2020-04-29

### Minor improvements and fixes

- Update dependency versions and require R \>= 3.4.0

- Fix issues related to how R 4.0.0 handles S3 methods

- Modernize ssm_plot() function to use new tidyr syntax

- Update travis CI configuration to be more explicit

------------------------------------------------------------------------

## circumplex 0.3.5

CRAN release: 2020-01-10

### Minor improvements and fixes

- Remove several unit tests that were causing problems for CRAN checks

------------------------------------------------------------------------

## circumplex 0.3.4

CRAN release: 2019-12-05

### Minor improvements and fixes

- Adjust the test of `quantile.radian()` to account for changes to `%%`
  starting in R 3.6.1 Patched

- Add the name of the package to the S3 class names (e.g.,
  `circumplex_radian` instead of `radian`) to minimize the risk of
  overlapping classes between packages

- Add some supplementary files to the R build ignore list to avoid notes
  during CRAN check

------------------------------------------------------------------------

## circumplex 0.3.3

CRAN release: 2019-09-26

### Minor improvements and fixes

- Add APA-style citations to instrument documentation in addition to DOI
  links.

- Add “Instruments” menu to package website for viewing documentation
  pages.

- Adjust the test of `quantile.radian()` to account for changes to `%%`
  starting in R 4.0.0

------------------------------------------------------------------------

## circumplex 0.3.2

CRAN release: 2019-08-21

### New features

- New `iitc` provides instrument information for the Inventory of
  Influence Tactics Circumplex.

### Minor improvements and fixes

- Fix CRAN warnings by setting `LazyData: true`.

- Fix CRAN note by replacing relative URLs with absolute URLs.

- Nonstandard evaluation is now handled using `{{}}` notation.

- Updated the formatting on this NEWS changelog to match tidyverse
  style.

------------------------------------------------------------------------

## circumplex 0.3.1

CRAN release: 2019-05-15

### Minor improvements and fixes

- Avoid a bug with dplyr 0.8.1 and S3 methods on Linux systems.

- Update the web address for Johannes in the README document.

------------------------------------------------------------------------

## circumplex 0.3.0

CRAN release: 2019-04-26

### New features

- New
  [`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md)
  calculates SSM parameters (without confidence intervals) from a vector
  of scores.

- New
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  calculates SSM parameters by row.

- Added support for older versions of R (3.3.x).

### Minor improvements and fixes

- Updated the “Introduction to SSM” vignette’s figures.

- Replaced use of
  [`dplyr::funs()`](https://dplyr.tidyverse.org/reference/funs.html) as
  this function is being deprecated.

- Fixed a bug in the normative data for `ipipipc` that prevented
  standardization.

- Fixed a bug caused by changes in how random numbers are generated in R
  3.6.x.

- Fixed several broken links by running package through new version of
  `usethis`.

- Fixed warnings related to documentation inherited from other packages.

------------------------------------------------------------------------

## circumplex 0.2.1

CRAN release: 2018-11-29

### New features

- `iis32` now has normative data.

- Added open-access (i.e., full item text) to the `iis32` and `iis64`.

### Minor improvements and fixes

- `iis32` item ordering and scoring now match the author’s version.

- `iis32` response anchors now range from 1 to 6 and match norms.

- Changed use of `tibble` functions to avoid problems when new version
  releases.

- Removed dependency on `MASS` package (until it is used by exported
  functions).

------------------------------------------------------------------------

## circumplex 0.2.0

CRAN release: 2018-10-26

### New features

- Added functions and documentation for numerous circumplex instruments.

- Added functions for ipsatizing and scoring item-level data.

- Added function for standardizing scale-level data using normative
  data.

### Minor improvements and fixes

- Changed OpenMP flags in Makevars to fix a compile problem on Debian
  machines.

- Fixed a bug related to calculating angular medians in the presence of
  NAs.

- Changed the default to plot profiles with low fit (but with dashed
  borders).

- Import and export functions from rlang tidy evaluation.

- Added unit testing of various functions to increase code coverage.

- Redesigned package website to be more attractive and clear.

- Updated the SSM vignette to use the `standardize()` function.

------------------------------------------------------------------------

## circumplex 0.1.2

CRAN release: 2018-08-06

### New features

- `ssm_plot()` now uses dashed borders to indicate that a profile has
  low prototypicality/fit.

### Minor improvements and fixes

- Fixed bug that prevented compilation on Solaris systems.

- Fixed bug that prevented CRAN checks on old R versions.

- Improved the formatting of vignette source code.

------------------------------------------------------------------------

## circumplex 0.1.1

CRAN release: 2018-07-31

### New features

- [Package website](https://circumplex.jmgirard.com) added using
  [pkgdown](https://pkgdown.r-lib.org/).

### Minor improvements and fixes

- Fixed documentation to meet CRAN standards.

------------------------------------------------------------------------

## circumplex 0.1.0

- Package submitted to CRAN.
