# M121: A local reproduction of CRAN's linux-arm64 check flavor

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — dev tooling; no consumer of the package relies on it
- **Branch/PR:** `m121-arm64-check-harness`

## Goal

Give the repo a one-command local check that reproduces CRAN's linux-arm64
additional-check flavor, so a platform-specific failure is found before
submission rather than by rejection.

## Scope

**In:** a pinned `linux/arm64` Docker image and its runner scripts under
`tools/arm64/`; a header recording what the image is and what it does not
cover; the release-walk step that requires a green arm64 log before
submission; the command in `CLAUDE.md`.

**Out:** the counterexample-B test fix → M122. Installing `brms`, `OpenMx`,
`glmmTMB` or `vdiffr` in the image → candidate row (build cost; their tests
self-skip). An `ubuntu-24.04-arm` CI job → candidate row (RR22 rec 10).
Covering CRAN's macOS x86_64 flavor → candidate row; this harness reproduces
one of the two platform-exact rejection sources, not both.

## Acceptance criteria

- [ ] AC1: `tools/arm64/check.sh` run on a source tarball built from commit
      `ecb06de7` reports `Status: 1 ERROR`, and the test-failure block of the
      resulting `00check.log` names `test-axes-certificate.R:544:3` and
      contains `the shipped pricing REFUSES at case 'cxb'`.
- [ ] AC2: The same run records, from inside the container and not from
      `00check.log` (which prints no such line), `R.version$platform` equal to
      `aarch64-unknown-linux-gnu` and a `La_library()` path containing
      `openblas`.
- [ ] AC3: The same script run on a tarball identical to AC1's except that the
      `test_that()` block opening at `test-axes-certificate.R:527` is deleted
      reports no ERROR arising from `test-axes-certificate.R` — showing the
      harness distinguishes the failing tarball from a passing one rather than
      reporting ERROR unconditionally.
- [ ] AC4: The image is pinned by digest, and `tools/arm64/`'s header records
      the tag the digest was taken from, the R and OpenBLAS versions, the four
      omitted Suggests with the count of assertions they cost (11 against
      CRAN's 2410), and that CRAN's macOS x86_64 flavor is not covered.
- [ ] AC5: `cairn/PROFILE.md`'s release-walk slot requires a dated green arm64
      check log in `cran-comments.md`'s test-environments list before the
      submission step; `CLAUDE.md`'s Commands section carries the command; and
      `tools/arm64/` carries a digest-refresh recipe.
- [ ] AC6: `Rscript -e 'devtools::check(manual = TRUE)'` reports 0 errors and
      0 warnings, and `tar -tzf` on the built tarball lists no path under
      `tools/`.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T1
- AC5 → T4
- AC6 → T5

## Tasks

- [x] T1: Move the Dockerfile and the two runner scripts from
      `~/.cache/circumplex-arm64/` into `tools/arm64/`; pin the base image by
      digest; write the header AC4 requires; have the runner record platform
      and LAPACK from inside the container.
- [ ] T2: Build the `ecb06de7` tarball, run the harness on it, record the log
      excerpt as evidence.
- [ ] T3: Build the block-deleted tarball, run the harness, record the result.
- [ ] T4: Add the release-walk requirement to `cairn/PROFILE.md`, the command
      to `CLAUDE.md`, and the refresh recipe to `tools/arm64/`.
- [ ] T5: Run `devtools::check(manual = TRUE)` and `tar -tzf` on the built
      tarball.

## Work log

- 2026-09-05: planned. Criteria audit (full mode): returned six findings on
  this milestone's drafts — AC2 named a LAPACK line `00check.log` never
  prints, AC3 was off by one on the block's opening line and quantified over
  the whole check rather than the site under test, AC5 named a tarball
  manifest `devtools::check()` does not produce and a `.Rbuildignore` entry
  already present, and the header claimed IP1 for dev tooling. All six
  repaired before writing; cleared AC1.
- 2026-09-05: approach — the harness is a container rather than a CI job
  because CI cannot pin CRAN's image and GitHub's macOS runners are arm64
  already; rejected tracking a rolling base tag (RR22 Q6: a harness that
  fails to build on the day of a resubmission is worse than one lagging CRAN
  by a month, and the first build attempt did fail that way). Falsified by:
  a CRAN arm64 failure the pinned image cannot reproduce.
- 2026-09-05: T1 — harness committed to `tools/arm64/` (Dockerfile, `check.sh`,
  `testfile.sh`, `README.md`). Base pinned `FROM r-base@sha256:41d55643…`, the
  digest `r-base:latest` resolved to on 2026-09-05; building the committed file
  produced image `b61a7aa01c56`, byte-identical to the one the evidence runs on,
  so the pin cost no rebuild. `check.sh` now writes `arm64-platform.txt` from
  inside the container and sets `_R_CHECK_TESTS_NLINES_=0` — at the default 13
  lines the failing-test block in `00check.log` filled with vdiffr snapshot
  notices and never reached the actual failure.
