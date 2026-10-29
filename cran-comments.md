## Test environments

* local macOS 27.0.1, R 4.6.1: `devtools::check(manual = TRUE)` and
  `R CMD check --as-cran` on the built tarball
* local reproduction of the linux-arm64 additional check, 2026-10-28: Docker
  on Apple Silicon, `aarch64-unknown-linux-gnu`, R 4.6.1, OpenBLAS 0.3.33
  (`libopenblasp-r0.3.33.so`). `Status: OK` on the tarball submitted here.
* win-builder, R-devel on Windows, 2026-10-29: no notes other than the
  maintainer-change note that its alternate results address caused
* GitHub Actions: macOS/release, windows/release, ubuntu/devel,
  ubuntu/release, ubuntu/oldrel-1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None on CRAN.

## Notes

This is a minor release with new features and fixes. The CRAN checks of
2.0.1 are OK on every flavor.

Some functions now refuse column selections that gave a wrong result or an
unclear error before: a column selected twice, a number below 1, and a name
that occurs twice in the data. No package on CRAN depends on circumplex.

On the maintainer's macOS machine, `R CMD check --as-cran` on this tarball
takes 2 minutes 10 seconds in total. Of that, the tests take 47 seconds,
installation 11 seconds, and the examples 13 and 17 seconds. The vignettes
ship pre-computed, so re-building them runs no model fits.
