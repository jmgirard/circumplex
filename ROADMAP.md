# circumplex Roadmap

> **Status: PROPOSAL** — drafted 2026-07-02 from a full audit of the package
> (v1.1.0.9000); revised same day to add the CI-trustworthiness diagnostic,
> the ggplot2 extension milestone, and refactor targets. Sequencing and scope
> are open to revision. Each milestone is intended to be a releasable unit:
> correctness first, then inference quality, then new capabilities in order
> of increasing scope.

## Guiding principles

1. **Correctness before capability.** Known bugs and silent-failure modes get
   fixed and regression-tested before any new features land.
2. **Every statistical routine gets an independent numerical cross-check.**
   New estimators are validated against a reference implementation (e.g., `lm()`
   for OLS-equivalent fits, published worked examples, or simulation recovery).
3. **Angles are the danger zone.** Any change touching displacement, contrasts,
   or the 0°/360° boundary requires tests at the boundary (profiles peaking at
   0°/360°, CIs straddling the boundary, contrasts near ±180°).
4. **One convention, stated everywhere.** Degrees in the user API ([0, 360),
   with LM at 360 by convention), radians internally, contrasts reported as
   *second minus first* level in (-180°, 180°].

**Design verdict from the audit:** the architecture (thin R dispatch →
C++ estimation core → `boot` resampling; S3 classes) is sound and does *not*
warrant a rewrite. Needed refactors are localized and listed in the
continuous track below; new capabilities should be added as new functions
rather than more arguments on `ssm_analyze()`.

---

## CRAN release strategy

Milestones are **GitHub** units of work; they are *not* one-to-one with CRAN
submissions. CRAN asks maintainers not to submit more than roughly once every
1–2 months and pushes back on churn, so **accumulate finished milestones on
GitHub and submit to CRAN only when there is a coherent user-facing story a
CRAN-only user needs** (they will not `install_github`). Decoupling the two
lets us keep shipping to GitHub continuously while spacing CRAN submissions.

**Triage of the roadmap into CRAN submissions:**

- **Tier 1 — submit on its own, promptly. v1.2.0 (M1).** Correctness fixes
  (silently-wrong results) justify interrupting cadence; ship alone, do not
  wait to bundle features.
- **Tier 2 — flagship, its own slot. M4** (fit statistics + Browne's model /
  CircE replacement). Highest new-user value and strongest standalone story:
  CircE is archived on CRAN, so no R package currently estimates Browne's
  model. Also the riskiest statistically, so it benefits from not sharing a
  release.
- **Tier 3 — bundle, don't spend a slot each. M2 + M3 → one release
  (~v1.3.0).** M3 (ggplot2 extension) is mostly infrastructure whose payoff is
  realized by later milestones — weak as a solo CRAN submission; M2 (inference
  quality) is incremental. Together they make a substantial "faster, more
  flexible, composable plots + new visualization vignette" release. Caveat:
  both touch fragile internals (do the named-column results-assembly refactor
  before M2), so the bundle has a larger check/review surface — budget a
  `/code-review ultra` pass accordingly.
- **Tier 4 — naturally their own releases; cadence is moot. M5** (SEM) and
  **M6** (longitudinal, v2.0.0) are large and far enough out that no bundling
  decision is needed now.
- **Never a CRAN submission on its own:** the continuous/infra track below
  (refactors, test renames, CI upkeep, coverage). Ships to GitHub whenever
  convenient, folded into whichever milestone touches that code.

**Suggested submission train:** (1) now — v1.2.0 (M1); (2) next slot —
v1.3.0 (M2 + M3 bundled) once the viz extension is stable; (3) headline slot —
M4 (CircE replacement); (4) M5, then M6/v2.0.0 as they land.

Note: a quick **patch** (e.g. v1.2.1) shortly after a release is acceptable to
CRAN when it fixes a real bug — bug-fixes are the accepted exception to the
cadence rule. It is *feature* releases that must be spaced out.

---

## Milestone 1 — Correctness & robustness patch (target: v1.2.0)

Fixes for issues found in the 2026-07 audit. Small, high-value, low-risk.
Task-level breakdown with acceptance criteria lives in MILESTONES.md.

### Bugs (each fix ships with a regression test)

- [ ] **`ssm_score()` ignores its `angles` argument.** `apply(..., FUN =
      ssm_parameters, ...)` never forwards `angles`, so custom angles of
      matching length produce silently wrong results (verified: rotated angles
      return octant-based estimates); mismatched lengths error confusingly.
      (`R/ssm_analysis.R:508-532`)
- [ ] **`is_null_or_char()` discards its `n` argument** (passes `n = NULL`
      through), so `measures_labels` length is never validated in
      `ssm_analyze()`. (`R/utils.R:145-147`)
- [ ] **NA in `grouping` with `listwise = FALSE`** crashes with cryptic
      Armadillo error `unique(): detected NaN`. Drop NA-group rows with an
      informative message in both deletion modes. (`R/ssm_analysis.R`,
      `src/parameters.cpp`)
- [ ] **Degenerate (flat) profiles**: zero-variance scores return `Fit = -Inf`
      and a numerical-noise displacement with no warning; near-zero amplitude
      likewise yields an uninterpretable displacement. Return `NA` for
      displacement/fit with a warning. (`src/parameters.cpp:23-34`)
- [ ] **`norm_standardize()` angle matching** uses exact float equality against
      the norm table's `Angle` column; passing 0° where norms store 360° fails
      with `replacement has length zero`. Match scales by name/position or
      normalize angles mod 360 before matching, and error informatively.
      (`R/tidying_functions.R:181-186`)
- [ ] **Contrast displacement near ±180°**: point estimate (signed angular
      distance in (-180°, 180°]) and CI (circular-mean-centered quantiles) can
      disagree in convention at the boundary. Harmonize by re-centering the
      reported estimate to the CI's branch (or vice versa) and add tests at
      ±180°.

### Guardrails & UX

- [ ] Warn (in `print`/`summary`) when displacement/amplitude are being
      interpreted for a profile with inadequate fit (< .70) or an amplitude CI
      that includes 0, per Zimmermann & Wright (2017) guidance.
- [ ] Decide and document the boundary display convention: displacement of a
      profile peaking at 0°/360° currently prints as 360.0. Either is
      defensible; state it in `?ssm_analyze` and the intro vignette.
- [ ] Replace `stopifnot(class(x) == "circumplex_ssm")` with `inherits()`
      throughout; validate or drop the advertised-but-broken `matrix` input
      support (`data[scales]` fails for matrices).
- [ ] Consider warning on unused `...` arguments in plotting functions (a typo
      like `angle_lables=` is currently swallowed silently).

### Documentation corrections

- [ ] Remove references to the deleted `ssm_plot()`: intermediate vignette §5
      code block, introduction vignette §4 prose.
- [ ] Fix `ssm_plot_curve()` example typo `angle_lables` → `angle_labels`.
- [ ] `instruments()` says "14 instruments" but lists 15.
- [ ] Document contrast direction (second minus first factor level, levels
      sorted alphabetically unless the variable is a factor) in `?ssm_analyze`,
      not just the vignette.
- [ ] Document that the closed-form SSM estimator equals OLS only for equally
      spaced angles; with unequal spacing it remains the conventional Gurtman
      estimator but is not least-squares optimal (verified numerically:
      disp 49.5° vs OLS 44.7° in a test case).
- [ ] Intro vignette: rephrase "displacement significantly different from
      zero" — a displacement CI excluding 0° is not a meaningful hypothesis
      test for an angle.
- [ ] Delete stale `CRAN-SUBMISSION` file (`.Rbuildignore` already updated).
- [ ] NEWS.md cleanup: remove duplicated `# circumplex 1.1.0` heading; skim
      for similar artifacts.

---

## Milestone 2 — Inference quality (target: v1.3.0)

Upgrades to the existing bootstrap machinery; no new statistical scope.

- [ ] **Parallel bootstrapping** via `boot`'s built-in `parallel`/`ncpus`
      arguments, exposed through `ssm_analyze()`.
- ~~**BCa confidence intervals**~~ **DROPPED 2026-07-02** (decision with
      Jeff): BCa cannot apply to circular displacement (its bias-correction
      and acceleration terms are order-statistic concepts that need a line,
      not a circle), so the option would be permanently mixed-method
      per-parameter — a standing communication burden in every table, plot,
      and user methods section, against field convention (Z&W 2017 =
      percentile), with real implementation risk (boot.ci handles neither our
      circular quantiles nor NA-filtered degenerate resamples). The one
      genuine beneficiary (amplitude: nonnegative, upward-biased, skewed near
      zero) is instead folded into M4's CI-trustworthiness diagnostic, and the
      Monte Carlo task below already provides an independent cross-check on
      percentile CIs. Full rationale in MILESTONES.md log.
- [ ] **Monte Carlo alternative to bootstrapping**: sample SSM parameters from
      the asymptotic sampling distribution of the mean vector / correlation
      vector (multivariate normal with estimated covariance), propagate through
      the parameter transformation. Much faster for large n, enables analytic
      sensitivity checks. Validate against bootstrap results on `jz2017`.
- [ ] **Vectorize `ssm_score()`** (currently row-wise `apply` + `rbind` of
      data frames): elevation/x/y are single matrix products; amplitude,
      displacement, and fit follow element-wise. Orders-of-magnitude faster on
      large data.
- [ ] Seed/reproducibility documentation for all resampling paths.

## Milestone 3 — Visualization layer: ggplot2 circumplex extension (target: v1.4.0)

Turn the internal, single-purpose plotting code into a public ggplot2
extension so users (and later milestones) can compose arbitrary layers in
circumplex space instead of rebuilding the circular canvas from scratch.
Deliberately sequenced *before* the fit-statistics/SEM/longitudinal
milestones, whose new visualizations should be built on it.

- [ ] **Public circular canvas**: promote `circle_base()` to an exported,
      documented API — e.g., `ggcircumplex()` constructor and/or
      `annotation_circumplex()` (rings, spokes, scale labels, amplitude
      gridlines), with instrument-aware labeling from
      `circumplex_instrument` objects.
- [ ] **Polar-native geoms/stats** (ggproto): `geom_ssm_point()` /
      `geom_ssm_arc()` (or a `stat_ssm()`) that accept amplitude/displacement
      aesthetics directly and handle the degree→canvas transform, wrap-around
      arcs, and amplitude rescaling that `ssm_plot_circle()` currently does
      inline.
- [ ] **Scales**: `scale_*_circumplex()` helpers for angle-labeled axes and
      amplitude gridlines; sensible defaults matching current appearance.
- [ ] **Refactor existing plots onto the extension** (`ssm_plot_circle()`,
      `ssm_plot_curve()`, `ssm_plot_contrast()` unchanged in behavior —
      vdiffr snapshots must stay stable or changes justified).
- [ ] **Vignette: "Advanced Circumplex Visualization"** — the long-promised
      third vignette (the intermediate vignette already announces it);
      demonstrates composing raw data, SSM results, and annotations.
- [ ] Design review against ggplot2 extension best practices (ggproto
      lifecycle, `after_stat`, theme integration); keep ggforce dependency if
      it simplifies arcs.

## Milestone 4 — Circumplex fit & structure statistics (target: v1.5.0)

Revive and modernize the drafts in `devel/fit_analysis.R` / `devel/fit_oop.R`
(currently written in superseded tidyverse-heavy style with a `psych`
dependency decision pending).

- [ ] Fisher test of equal axes (draft exists).
- [ ] Gap test of equal spacing (draft exists).
- [ ] Variance test of equal communalities / interstitiality indices.
- [ ] **Browne's (1992) stochastic process model — native reimplementation
      (CircE replacement).** CircE (Grassi, Luccio, & Di Blas, 2010) is
      archived on CRAN, leaving R users without an estimator for Browne's
      model. Implement estimation of the circular stochastic process model
      (free/constrained item angles, communality index, Fourier correlation
      function; point estimates, CIs, fit indices such as RMSEA/CFI from the
      discrepancy function). Decide backend: native optimization vs. OpenMx/
      lavaan. Validate against published CircE/CIRCUM output. This is the
      anchor feature of the milestone.
- [ ] **SSM CI trustworthiness diagnostic (Zimmermann & Wright, 2017).**
      Z&W's simulation studies show bootstrap SSM CI accuracy depends on
      sample size and the population circumplex structure; they used Browne-
      model estimates to characterize that structure. Reimplement as a
      user-facing diagnostic: fit Browne's model to the user's data (item- or
      scale-level), then either (a) simulate from the fitted model to estimate
      empirical CI coverage for the user's n, or (b) map the estimated
      parameters onto the accuracy results of Z&W Studies 1–5. *Spec from the
      paper + supplemental materials before implementation; depends on the
      CircE replacement above.* Surface as something like
      `ssm_ci_accuracy(ssm_object)` with a plain-language verdict in
      `summary()`. *Absorbed from the dropped M2 BCa task (2026-07-02): the
      diagnostic should specifically assess percentile-CI coverage for
      amplitude near zero (nonnegative, upward-biased, skewed — the one SSM
      parameter where percentile intervals are theoretically weakest), since
      the amplitude CI drives the "displacement not interpretable" guardrail.*
- [ ] `ssm_fit()`-style API returning a typed object with `print`/`summary`/
      `plot` methods, consistent with `circumplex_ssm` (plots built on the M3
      extension).
- [ ] New vignette: "Evaluating Circumplex Structure" (fit statistics, CI
      trustworthiness, when to trust SSM parameters, ipsatization guidance).

## Milestone 5 — SEM-based SSM (target: v1.6.0)

Builds on the lavaan explorations in `devel/lavaan_ssm.Rmd` and
`devel/circum_lavaan.Rmd`.

- [ ] Latent-variable SSM: estimate SSM parameters from a lavaan measurement
      model (disattenuated correlations), with delta-method or bootstrap CIs.
- [ ] Multi-group SEM contrasts (invariance-constrained comparisons as a more
      principled alternative to bootstrap group contrasts).
- [ ] Tooling to generate lavaan syntax for circumplex measurement models from
      `circumplex_instrument` objects.
- [ ] `lavaan` moves to `Suggests`; features degrade gracefully without it.
- [ ] Vignette: "SEM-based SSM Analysis" (adapt `devel/lavaan_ssm.Rmd`).

## Milestone 6 — Longitudinal & intraindividual SSM (target: v2.0.0)

The largest extension; benefits from Milestones 2–5 (fast estimation, the
visualization layer, fit diagnostics, SEM infrastructure).

- [ ] Repeated-measures SSM: parameter trajectories over time (growth models
      on e/a/d, with circular handling for d).
- [ ] Intraindividual SSM: per-person parameters from intensive longitudinal
      data (builds on vectorized `ssm_score()`), with multilevel summaries.
- [ ] Contrasts across timepoints (paired/dependent resampling — the current
      bootstrap assumes independent groups).
- [ ] Optional Bayesian estimation (revisit `devel/bayesian_ssm.Rmd`; likely
      a separate companion package if it drags in Stan).

## Continuous / infrastructure track (any release)

Targeted refactors — the 2026-07 audit's verdict is that these are worthwhile
but none block feature work; fold each into whichever milestone first touches
the relevant code:

- **Named, long-format internal results assembly.** `ssm_bootstrap()`
  identifies displacement columns by positional arithmetic
  (`d_vars <- 1:(ncol/6)*6 - 1`) and `reshape_params()` assumes a fixed
  6-parameter block; fragile the moment a parameter is added. Replace with
  named columns / one-row-per-parameter internal format. (Do before M2's
  interval work, which touches exactly this code.)
- **Deduplicate Group/Measure/Label construction** — the same block is built
  twice each in `ssm_analyze_means()`/`ssm_analyze_corrs()`; extract one
  helper. (Do with M1 or M2.)
- **Move degree/radian/contrast classes onto `vctrs`** (or S7) so arithmetic,
  printing, and quantile behavior are centralized and harder to misuse.
  (Nice-to-have; natural companion to M2.)
- **Rewrite the `devel/` fit drafts in current package style** (base R,
  no dplyr/rlang quasiquotation) when M4 begins — they predate the package's
  tidyverse-ectomy.
- Rename `tests/testthat/test-RcppExport.R.R` (double extension).
- Boundary-condition test suite: displacement at 0°/360°, CIs straddling the
  boundary, contrasts near ±180°, flat profiles, single-scale edge cases.
- Keep GitHub Actions workflows current; add R-devel to the check matrix.
- Track code coverage on the statistical core (`ssm_*`, `src/`) specifically.

Explicitly **not** planned: a ground-up rewrite. The R-dispatch → C++ core →
`boot` architecture, the S3 class design, and the minimal dependency policy
all hold up; inefficiencies found in the audit are local (see M2 vectorization
and the items above).
