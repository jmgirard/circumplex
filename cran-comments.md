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

This patch clears the ERROR that circumplex shows on the linux-arm64
additional check. Two assertions in `test-ssm_draws.R` compared amplitudes
computed by the package's C++ code against the same amplitudes recomputed in
R, and required the two to agree bit for bit. They do on every other platform;
on linux-arm64 they can differ by one unit in the last place. The comparison
now requires agreement to 12 significant digits and nothing else. No result
reported by `ssm_draws()` has changed, and no test was removed or skipped.

This release follows 2.0.1 closely for the same reason 2.0.1 followed 2.0.0:
each clears a platform-specific test failure that only CRAN's check farm can
see. 2.0.1 fixed the macOS x86_64 ERROR; this one fixes the remaining arm64
ERROR.

On the maintainer's macOS machine `R CMD check --as-cran` on this tarball
takes 1 minute 51 seconds in total, of which the tests are 24 seconds,
installation 11 seconds, and the examples 10 and 14 seconds. The vignettes
ship pre-computed, so re-building them runs no model fits.
