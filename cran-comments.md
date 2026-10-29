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

CRAN's incoming check will still give one NOTE, for the size of the
tarball. It is explained below.

## Downstream dependencies

None on CRAN.

## Notes

This is a resubmission. The incoming pretest of the first 2.1.0 tarball, on
2026-10-29, gave three NOTEs. Each is answered here.

* Size of tarball (10970779 bytes). The pre-computed vignette figures are now
  SVG files instead of PNG images, and the tarball is now 5134444 bytes. It
  is still over 5 MB because of three animated GIFs in the introduction
  vignette. They ship twice: as files (1023015 bytes in all) and embedded in
  the built `introduction-to-ssm-analysis.html`. 2.0.1 shipped at 9840575
  bytes.
* Examples with CPU or elapsed time over 10 s on r-devel-windows-x86_64
  (`cpm_fit`, 11.09 s elapsed), and over 5 s on
  r-devel-linux-x86_64-debian-gcc (`cpm_fit`, 5.2 s CPU). The `cpm_fit()`
  example with 100 bootstrap resamples is now inside `\donttest{}`. The
  matrix-input example still runs. Locally, without the `\donttest{}` code,
  the `cpm_fit` examples take 0.82 s.

This is a minor release with new features and fixes. The CRAN checks of
2.0.1 are OK on every flavor.

Some functions now refuse column selections that gave a wrong result or an
unclear error before: a column selected twice, a number below 1, and a name
that occurs twice in the data. No package on CRAN depends on circumplex.

The times below come from `R CMD check --as-cran` on this tarball, on the
maintainer's macOS machine, without the `\donttest{}` code. The tests take 47
seconds, installation 11 seconds, and the examples 12 seconds. Eleven of the
fourteen vignettes ship pre-computed. The other three fit no SEM, CPM or
bootstrap model, and re-building all fourteen takes 13 seconds.
