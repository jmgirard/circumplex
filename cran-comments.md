## Test environments

* local macOS 26.6.2, R 4.6.1 — `devtools::check(manual = TRUE)` and
  `R CMD check --as-cran` on the built tarball
* local reproduction of the linux-arm64 additional check, 2026-09-05 — Docker
  on Apple Silicon, `aarch64-unknown-linux-gnu`, R 4.6.1, OpenBLAS 0.3.33
  (`libopenblasp-r0.3.33.so`); `Status: OK` on the tarball submitted here
* GitHub Actions — macOS/release, windows/release, ubuntu/devel,
  ubuntu/release, ubuntu/oldrel-1

## R CMD check results

0 errors | 0 warnings | 1 note

The note is CRAN incoming's "Days since last update", explained below.

## Downstream dependencies

None on CRAN.

## Notes

This is a resubmission. Two earlier 2.0.1 tarballs were rejected by the
incoming pre-test on 2026-09-04, each for an ERROR on the linux-arm64
additional check. Both causes are fixed here, and this tarball carries every
fix the earlier ones carried. Because that check is not one I could previously
run, I built a local reproduction of it — the container described above — and
this tarball reports `Status: OK` there.

This patch exists to clear the test failures that 2.0.0 shows on CRAN's check
farm. None of them is reachable on the platforms otherwise available to me;
all are last-place floating-point differences that a test asserted away.

* r-release-macos-x86_64 and r-oldrel-macos-x86_64: four assertions in
  `test-ssm_sem_syntax.R` compared `ssm_sem_syntax()`'s printed output against
  a stored copy at 17 significant digits, and those platforms' math library
  rounds `cos(225°)` one unit in the last place differently.
* linux-arm64: two assertions in `test-ssm_draws.R` compared amplitudes
  computed in the package's C++ code against the same amplitudes recomputed in
  R and required them to agree bit for bit. They do on every other platform;
  on linux-arm64 two of ten differ by one unit in the last place.
* linux-arm64: one assertion in `test-axes-certificate.R` priced a
  deliberately ill-conditioned matrix and required `axes_reliability()`'s
  internal accuracy check to report a number for it. Whether a number can be
  computed at that matrix depends on the platform's linear algebra library: on
  linux-arm64 the inversion gives up, which the assertion treated as a defect.
  It now accepts either outcome and requires what is true on both — that the
  fit is refused, and that the accuracy check says so.

In the first two cases the comparison now ignores differences beyond 12
significant digits and nothing else. `axes_reliability()` refuses that matrix
on every platform, as it did before. No exported behavior changed, and no test
was removed or skipped. The remaining change in this release raises the
resolution of the pre-rendered vignette figures.

Those failures are why this release follows 2.0.0 so closely rather than
waiting.

On the maintainer's macOS machine `R CMD check --as-cran` on this tarball
takes 3 minutes 11 seconds in total, of which the tests are 18 seconds,
installation 14 seconds, and the examples 14 and 14 seconds. The vignettes
ship pre-computed, so re-building them runs no model fits.
