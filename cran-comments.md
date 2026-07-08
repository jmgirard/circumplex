## Test environments

* local macOS (Darwin 25.5.0), R 4.6.1, via `devtools::check(args = "--no-manual")`
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
* A public ggplot2 extension for circumplex figures (`ggcircumplex()`,
  `geom_ssm_point()`, `geom_ssm_arc()`, `scale_x_circumplex()`), a Monte Carlo
  interval engine (`ssm_analyze(method = "montecarlo")`), and parallel
  bootstrapping (`parallel`/`ncpus`). Four new vignettes.
* Bug fixes: a pairwise-deletion bootstrap crash on an all-missing resampled
  scale, and a contrast-displacement sign error at the exact +/-180 degree
  boundary.

The one user-visible API tightening is in `ssm_score()`: extra `...` arguments
must now be named and scalar, so a previously-silently-mishandled unnamed or
non-scalar argument now errors rather than yielding garbled output. All
documented call forms are unaffected. This, with the accumulated feature
families, motivates the major-version bump.

New Suggests (`lavaan`, `OpenMx`, `psych`) are used only in tests and,
optionally, at runtime by the SEM feature family; the package loads and all
other functionality runs without them.
