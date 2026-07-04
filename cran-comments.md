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

## Summary of changes

Inference-quality and visualization release — see NEWS.md for the full list.
Highlights: a Monte Carlo alternative to the bootstrap for confidence intervals
(`method = "montecarlo"`, faster on large samples), parallel bootstrapping
(`parallel`/`ncpus`), a vectorized `ssm_score()`, and a new public ggplot2
extension for circumplex visualization (`ggcircumplex()`, `geom_ssm_point()`,
`geom_ssm_arc()`, `scale_x_circumplex()`) with an accompanying vignette. Also
includes three bug fixes from a pre-release audit: a pairwise-deletion
bootstrap crash on an all-missing resampled scale, a documentation gap where
the reported model fit can fall outside [0, 1] for unequally spaced angles, and
a contrast-displacement sign error at the exact +/-180 degree boundary. No
breaking changes to the public API (a tightened validation path in
`ssm_score()` now errors on previously-silently-mishandled unnamed/non-scalar
arguments; the documented call forms are unaffected).
