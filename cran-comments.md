## Test environments

* local macOS (Darwin 25.5.0), R 4.6.1, via `devtools::check(manual = TRUE)`
  (including the PDF manual)
* win-builder (R-devel), via `devtools::check_win_devel()`
* GitHub Actions CI matrix (macOS-latest/release, windows-latest/release,
  ubuntu-latest/devel, ubuntu-latest/release, ubuntu-latest/oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

No reverse dependencies on CRAN (checked via
`tools::package_dependencies("circumplex", reverse = TRUE)`).

## Notes on the URL check

`urlchecker::url_check()` reports a 403 (Forbidden) for the DOI
https://doi.org/10.1177/01466216261440511 cited in the new "SEM-Based SSM
Analysis" vignette. The DOI resolves correctly (a HEAD request to doi.org
returns a 302 redirect); the 403 comes from the publisher (SAGE) blocking
automated requests, not from a broken link.

## Summary of changes

This is a major release accumulating several feature families developed since
1.2.0 — see NEWS.md for the full list. Highlights:

* `cpm_fit()`: a native reimplementation of Browne's (1992) circular stochastic
  process model for the correlational structure of circumplex scales, filling
  the gap left by the archived CircE package (no other CRAN package estimates
  this model). Validated against the published CIRCUM/CircE literature and
  against independent OpenMx and lavaan implementations (both Suggests-only,
  used as test oracles).
* `ssm_ci_accuracy()`: a simulation-based diagnostic for whether an
  `ssm_analyze()` result's confidence intervals can be trusted at the user's
  sample size and profile.
* `fit_structure()`: exploratory circumplex-structure tests (Acton & Revelle,
  2004) with an exact-p randomization test of circular order.
* A SEM-based (latent-variable) SSM family — `ssm_sem()`, `ssm_sem_syntax()`,
  `ssm_sem_parameters()` — estimating the disattenuated SSM profile of a
  measure from a fixed-angle lavaan measurement model, with invariance-gated
  multi-group latent contrasts. `lavaan` is Suggests; the feature degrades
  gracefully when it is absent.
* A repeated-measures (longitudinal) family: `ssm_analyze(occasions = )` and
  `ssm_analyze_long()` for within-person profiles and paired contrasts,
  `ssm_parameters_id()` for per-person scoring, and `ssm_draws()` for
  converting posterior or model draws into SSM parameter summaries.
* A rebuilt plotting layer on a real ggplot2 coordinate system
  (`coord_circumplex()`), with `ggcircumplex()`, the `geom_ssm_*()` layers,
  `scale_x_circumplex()`, and `theme_circumplex()` as the composable API and
  `ssm_plot_*()` as convenience wrappers.
* A Monte Carlo interval engine (`ssm_analyze(method = "montecarlo")`) and
  parallel bootstrapping (`parallel`/`ncpus`). Five new vignettes.
* Bug fixes: a pairwise-deletion bootstrap crash on an all-missing resampled
  scale, and a contrast-displacement sign error at the exact +/-180 degree
  boundary.

Three user-visible behavior changes motivate the major-version bump, all
documented in NEWS.md:

1. The displacement-interpretability guardrail in `print()`/`summary()` now
   uses a scale-free rule (amplitude CI lower bound at least 0.35
   interval-widths above zero) rather than the print-precision-dependent rule
   introduced in 1.2.0. Some near-zero-amplitude profiles previously certified
   as interpretable are now flagged uninterpretable. The replacement was
   calibrated by simulation; the old rule certified a genuinely zero amplitude
   almost every time.
2. Confidence-interval endpoints landing exactly on the 0/360 degree pole are
   now labeled 360 rather than 0, matching the package's convention elsewhere.
   This is a measure-zero floating-point corner for real data; numeric results
   are otherwise unchanged.
3. `ssm_score()`'s extra `...` arguments must now be named and scalar, so a
   previously-silently-mishandled unnamed or non-scalar argument now errors
   rather than yielding garbled output columns. All documented call forms are
   unaffected. Count-valued arguments across several functions are likewise
   now validated as a single non-negative whole number.

## Notes on dependencies

The `ggplot2` requirement moves from `>= 3.3.0` to `>= 4.0.0`: the new
coordinate system is a `CoordRadial` subclass and uses parameters introduced in
ggplot2 4.0.0. `ggforce` is dropped from Imports (the new coordinate system
subsumes what it was used for) and base R's `parallel` is added (for the new
`parallel`/`ncpus` bootstrapping arguments), so the Imports count is unchanged
at seven and no new third-party dependency is introduced.

The declared `Depends: R` moves from `>= 3.4` to `>= 4.1`. This corrects an
understated declaration rather than adding a restriction: ggplot2 (>= 4.0.0)
and htmlTable both declare `Depends: R (>= 4.1)`, so 4.1 has been the effective
install floor since the ggplot2 re-pin. It also subsumes the R (>= 3.5.0)
requirement that `R CMD build` derives from the serialized vignette fixture.

New Suggests are `lavaan`, `OpenMx`, `psych`, `brms`, and `glmmTMB`. They are
used in tests, as test oracles, and optionally at runtime by the SEM feature
family and by two vignettes; the package loads and all other functionality runs
without them. The `brms` vignette is precomputed (its model-fitting chunk is
`eval = FALSE` and its posterior draws ship as a committed fixture), so no Stan
toolchain is required on the check machines.
