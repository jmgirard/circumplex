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

0 errors | 0 warnings | 0 notes locally.

The tarball is under the 10000000 bytes above which the incoming check of
R 4.6.1 reports its size by default.

## Downstream dependencies

None on CRAN.

## Notes

This is a resubmission. The incoming pretest of the first 2.1.0 tarball, on
2026-10-29, listed three NOTEs. Each is answered here.

* The incoming-feasibility NOTE, which reported `Size of tarball: 10970779
  bytes`. The pre-computed vignette figures are now SVG files instead of PNG
  images, and the tarball is now under 5250000 bytes.
* `cpm_fit` at 11.09 s elapsed on r-devel-windows-x86_64, and at 5.2 s CPU
  on r-devel-linux-x86_64-debian-gcc. The `cpm_fit()` example with 100
  bootstrap resamples is now inside `\donttest{}`. The matrix-input example
  still runs. Locally, without the `\donttest{}` code, the `cpm_fit`
  examples take under 1 s.

This is a minor release with new features and fixes. The CRAN checks of
2.0.1 are OK on every flavor.

Some functions now refuse column selections that gave a wrong result or an
unclear error before: a column selected twice, a number below 1, and a name
that occurs twice in the data. No package on CRAN depends on circumplex.

The times in this paragraph come from `R CMD check --as-cran` on this
tarball, on the maintainer's macOS machine, without the `\donttest{}` code.
The tests take under 90 seconds, installation under 15 seconds, and the
examples under 15 seconds. Eleven of the fourteen vignettes ship
pre-computed. The other three fit no SEM, CPM or bootstrap model, and
re-building all fourteen takes under 15 seconds.
