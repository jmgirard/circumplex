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

Correctness and robustness patch — see NEWS.md for the full list. Highlights:
several estimation-adjacent bugs fixed (`ssm_score()` angle forwarding, NA
grouping values, degenerate/zero-variance profiles, normative-data angle
matching, contrast CI branch selection near +/-180 degrees), input validation
tightened, matrix input support restored, and print/summary output now flags
statistically uninterpretable profiles. No breaking changes to the public API.
