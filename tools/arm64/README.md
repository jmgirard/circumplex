# linux-arm64 check harness

A local reproduction of CRAN's `r-devel-linux-x86_64` **arm64** additional
check — the flavor that rejected the 2.0.1 tarball twice. One command, on
Apple Silicon, natively (no emulation):

```
tools/arm64/check.sh /path/to/circumplex_X.Y.Z.tar.gz
```

`testfile.sh` is the fast loop once `check.sh` has found something: it
installs the working tree and runs a single test file.

## What the image is

| | |
|---|---|
| Base image | `r-base:latest`, pinned by digest `sha256:41d5564375009abf74a63987fd7fb9b44c90b1580b310be10ef973abe92496c3` (the tag resolved to that digest on 2026-09-05) |
| R | 4.6.1 (2026-06-24) |
| Platform | `aarch64-unknown-linux-gnu` |
| LAPACK | `/usr/lib/aarch64-linux-gnu/openblas-pthread/libopenblasp-r0.3.33.so` |
| OpenBLAS | Debian `libopenblas0-pthread` 0.3.33+ds-3 |

The R, platform, LAPACK and OpenBLAS rows were read from inside the container
on 2026-09-05. `check.sh` re-reads R, platform, LAPACK and BLAS on every run,
in the same container as the check, and writes them to `arm64-platform.txt`
beside the tarball, stamped with the tarball name and the UTC time.
`00check.log` names the platform but no LAPACK or BLAS path, so the log alone
cannot say which linear algebra the check ran against — which on this package
is the whole question.

Build it with:

```
docker build --platform linux/arm64 -t circumplex-arm64check:latest tools/arm64/
```

Set `CIRCUMPLEX_ARM64_IMAGE` to use a different tag.

## What it does not cover

**Four heavyweight Suggests are deliberately absent**: `brms`, `OpenMx` and
`glmmTMB` take hours to build; `vdiffr` wants system font headers. The tests
that use `OpenMx`, `glmmTMB` and `vdiffr` self-skip when the package is
missing. `brms` appears in no test at all — it is a dependency of
`vignettes/bayesian-ssm-analysis.Rmd`, and `check.sh` passes `--no-vignettes`.
A failure confined to those four packages is invisible here.

**The assertion counts, and the gap between them.** This harness ran **2399**
passing assertions on a plain `R CMD build` tarball of commit `ecb06de7`
(2.0.1); CRAN's own arm64 log for that commit recorded **2410** (transcribed at
`cairn/reviews/archive/RB22-certificate-platform-refusal.md:49`). Build flags
move this figure — the same source built with `--no-build-vignettes` gives 2365
— so the count belongs to the recipe as much as to the commit. **What accounts
for the 11-assertion difference is not known**, and it is credited to no cause
here. In particular it is not established that the four absent packages explain
it: removing them moves the suite from 69 skips to 540, and CRAN's log reports
540 skips too.

**CRAN's macOS x86_64 flavor is not covered.** That is the other of the two
platform-exact rejection sources this package has hit, and reproducing it
would need a different image on different hardware. A green run here is
evidence about arm64 only.

`check.sh` passes `--no-manual --no-vignettes`, so the PDF manual and vignette
re-building steps are CRAN's to run, not this harness's.

## Refreshing the pin

The pin lags CRAN deliberately — a harness that fails to build on the day of a
resubmission is worse than one a month behind. Refresh when CRAN's arm64
checks move to an R version this image no longer matches, or when a CRAN arm64
failure will not reproduce here:

1. `docker pull r-base:latest`
2. `docker inspect r-base:latest --format '{{index .RepoDigests 0}}'`
3. Put that digest in the `FROM` line of `Dockerfile`, and the tag-resolution
   date in the table above.
4. Rebuild (command above).
5. Re-run `check.sh` on a tarball whose result you already know, and update
   the R / LAPACK / OpenBLAS rows and the assertion count from the new run.
