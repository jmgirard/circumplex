## Test environments

* local macOS 26.6.2, R 4.6.1 — `devtools::check(manual = TRUE)` and
  `R CMD check --as-cran` on the built tarball
* GitHub Actions — macOS/release, windows/release, ubuntu/devel,
  ubuntu/release, ubuntu/oldrel-1

## R CMD check results

0 errors | 0 warnings | 1 note

The note is CRAN incoming's "Days since last update", explained below.

## Downstream dependencies

None on CRAN.

## Notes

This is a resubmission. An earlier 2.0.1 tarball was rejected by the incoming
pre-test on 2026-09-04 for an ERROR on the linux-arm64 additional check. That
ERROR is fixed here, and this resubmission also keeps the fix the earlier
tarball carried.

This patch exists to clear the two test failures that 2.0.0 shows on CRAN's
check farm. Neither is reachable on the platforms available to me; both are
last-place floating-point differences that a test asserted away.

* r-release-macos-x86_64 and r-oldrel-macos-x86_64: four assertions in
  `test-ssm_sem_syntax.R` compared `ssm_sem_syntax()`'s printed output against
  a stored copy at 17 significant digits, and those platforms' math library
  rounds `cos(225°)` one unit in the last place differently.
* linux-arm64: two assertions in `test-ssm_draws.R` compared amplitudes
  computed in the package's C++ code against the same amplitudes recomputed in
  R and required them to agree bit for bit. They do on every other platform;
  on linux-arm64 two of ten differ by one unit in the last place.

In both cases the comparison now ignores differences beyond 12 significant
digits and nothing else. No exported behavior changed, and no test was removed
or skipped. The remaining change in this release raises the resolution of the
pre-rendered vignette figures.

Those failures are why this release follows 2.0.0 so closely rather than
waiting.

On the maintainer's macOS machine `R CMD check --as-cran` on this tarball
takes 2 minutes 27 seconds in total, of which the tests are 29 seconds,
installation 16 seconds, and the examples 14 and 20 seconds. The vignettes
ship pre-computed, so re-building them runs no model fits.
