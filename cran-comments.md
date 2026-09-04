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

This patch exists to clear the ERROR that 2.0.0 shows on
r-release-macos-x86_64 and r-oldrel-macos-x86_64. Four assertions in
`test-ssm_sem_syntax.R` compared `ssm_sem_syntax()`'s printed output against a
stored copy at 17 significant digits, and those platforms' math library rounds
`cos(225°)` one unit in the last place differently, so the stored text did not
match. The comparison now ignores differences beyond 12 significant digits and
nothing else; `ssm_sem_syntax()`'s output is unchanged, and no test was removed
or skipped. The remaining change in this release raises the resolution of the
pre-rendered vignette figures.

That ERROR is why the release follows 2.0.0 by a day rather than waiting.

Check times are unchanged from the 2.0.0 submission. On the maintainer's macOS
machine `R CMD check --as-cran` on this tarball takes 1 minute 53 seconds in
total, of which the tests are 23 seconds, installation 11 seconds, and the
examples 10 and 14 seconds. The vignettes ship pre-computed, so re-building
them runs no model fits.
