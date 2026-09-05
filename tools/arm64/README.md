# linux-arm64 check harness

A local reproduction of CRAN's `linux-arm64` special check — the incoming-
pretest flavor served under `specialChecks/linux-arm64`, which rejected the
2.0.1 tarball twice. Runs on Apple Silicon natively (no emulation).

Build the image once:

```
docker build --platform linux/arm64 -t circumplex-arm64check:latest tools/arm64/
```

then, per tarball:

```
tools/arm64/check.sh /path/to/circumplex_X.Y.Z.tar.gz
```

`check.sh` exits 0 only when `00check.log` reports `Status: OK`; it exits 2
when the harness itself could not run (no image, a non-aarch64 container, a
dead platform probe, no verdict in the log).

`testfile.sh` is the fast loop once `check.sh` has found something: it copies
the working tree into the container (the repo is mounted read-only, so the
compile leaves nothing behind) and runs a single test file.

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

Set `CIRCUMPLEX_ARM64_IMAGE` to use a different tag. Whatever the tag,
`check.sh` refuses to run a container that does not report an `aarch64`
platform.

## What it does not cover

**Four heavyweight Suggests are deliberately absent**: `brms`, `OpenMx` and
`glmmTMB` take hours to build; `vdiffr` wants system font headers. The tests
that use `OpenMx`, `glmmTMB` and `vdiffr` self-skip when the package is
missing. `brms` appears in no test at all, and in
`vignettes/bayesian-ssm-analysis.Rmd` only in prose and in one `eval = FALSE`
chunk — nothing builds against it. A failure confined to those four packages
is invisible here.

**The image is pinned at its base layer only.** `Dockerfile`'s `FROM` is a
digest, but `apt-get update` runs against Debian unstable and the two
`install.packages()` layers fetch whatever CRAN serves that day, so a rebuild
months from now is not the same image. What each run actually got is in
`arm64-platform.txt`; the version table above is a record of one build, not a
guarantee about the next.

**The assertion counts, and the gap between them.** This harness ran **2399**
passing assertions on a plain `R CMD build` tarball of commit `ecb06de7`
(2.0.1); CRAN's own arm64 log for that commit recorded **2410** (transcribed at
`cairn/reviews/archive/RB22-certificate-platform-refusal.md:49`). Build flags
move this figure — the same source built with `--no-build-vignettes` gives 2365
— so the count belongs to the recipe as much as to the commit. **What accounts
for the 11-assertion difference is not known**, and it is credited to no cause
here. In particular it is not established that the four absent packages explain
it: this container and CRAN's arm64 machine both report `SKIP 540`, and CRAN's
log never names the four packages, so CRAN appears to lack them too. (The
suite's skip total is dominated by its 469 `skip_on_cran()` calls, which fire
under `R CMD check` and not under `devtools::test()`; only 27 blocks are
guarded on these four packages. A host `devtools::test()` total is therefore
not comparable with either figure above.)

**CRAN's macOS x86_64 flavor is not covered.** That is the other of the two
platform-exact rejection sources this package has hit, and reproducing it
would need a different image on different hardware. A green run here is
evidence about arm64 only.

`check.sh` passes `--no-manual --no-vignettes`, so the PDF manual step is
skipped and the vignettes are neither re-built nor **run** — and the vignettes
exercise the estimators, so numeric divergence reachable only from vignette
code would not surface here. It also runs plain `R CMD check`, not
`--as-cran`, so any CRAN-only incoming check is outside what a green here
says.

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
