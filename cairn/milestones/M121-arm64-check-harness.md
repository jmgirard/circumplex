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
- [ ] AC2: The same run writes `arm64-platform.txt` beside the tarball, from
      inside the container that runs the check, recording `R.version$platform`
      equal to `aarch64-unknown-linux-gnu` and both a `La_library()` and an
      `extSoftVersion()[["BLAS"]]` path containing `openblas`.
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
- [x] T2: Build the `ecb06de7` tarball, run the harness on it, record the log
      excerpt as evidence.
- [x] T3: Build the block-deleted tarball, run the harness, record the result.
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
- 2026-09-05: T2 — `R CMD build` on a `git archive ecb06de7` export produced
  `circumplex_2.0.1.tar.gz`; `check.sh` on it exits 1 with `Status: 1 ERROR`,
  and `00check.log` carries `[ FAIL 1 | WARN 4 | SKIP 540 | PASS 2399 ]` with
  the failure block at lines 350-351 naming `test-axes-certificate.R:544:3` and
  `the shipped pricing REFUSES at case 'cxb'`. `arm64-platform.txt` records
  platform `aarch64-unknown-linux-gnu` and LAPACK
  `.../openblas-pthread/libopenblasp-r0.3.33.so`. Run took 40 s.
- 2026-09-05: T2 — the first run exposed a defect in `check.sh` itself: a
  tarball outside Docker Desktop's shared paths mounts as an EMPTY directory,
  so `R CMD check` warned "neither a file nor directory", skipped it and exited
  0 — a green harness that ran nothing. `check.sh` now proves the tarball
  visible inside the container before checking and requires a `Status:` line in
  `00check.log` after; both guards were seen to fire red on the empty mount
  before the passing run.
- 2026-09-05: amendment (substantive, AC2) — the drafted parenthetical
  "`00check.log` (which prints no such line)" is false: the log's line 3 reads
  `* using platform: aarch64-unknown-linux-gnu`. Only the LAPACK and BLAS paths
  are absent. Amended at a mini gate, then twice more on the re-audits below;
  AC2 now names the artifact it binds (`arm64-platform.txt`), drops every claim
  about what `R CMD check` prints, and requires both linear-algebra paths.
- 2026-09-05: re-audit: AC2 (full) — returned five findings on the first
  amended draft: an unbounded "never prints" claim about R CMD check's output;
  that clause binding a third-party instrument rather than the deliverable; no
  artifact named for "records"; "from inside the container" overstating a probe
  that ran in a separate `docker run` from the check; and a stale
  `arm64-platform.txt` admissible as evidence. All five repaired — the clause
  dropped, `arm64-platform.txt` named, the probe moved into the checking
  container, the file removed before the run.
- 2026-09-05: re-audit: AC2 (full) — returned four findings on the second
  draft: the criterion binds the probe process rather than the checking process
  (answered by AC2's own "from inside the container" wording, which the reader
  called honest); `check.sh`'s comment overclaimed past that (corrected to
  container identity, not process identity); freshness unobservable from the
  finished file (the record is now stamped with tarball and UTC time); and the
  criterion under-binding evidence the run already produces. The fourth went to
  the user, who chose to require the BLAS path too. AC2's re-entry is spent.
- 2026-09-05: T2 re-run on the revised `check.sh` — `Status: 1 ERROR`, same
  failure at `00check.log:350-351`, same `[ FAIL 1 | WARN 4 | SKIP 540 | PASS
  2399 ]`. `arm64-platform.txt` now reads `tarball: circumplex_2.0.1.tar.gz` /
  `date: 2026-09-05T17:40:04Z` / platform `aarch64-unknown-linux-gnu` / LAPACK
  `.../openblas-pthread/libopenblasp-r0.3.33.so` / BLAS
  `.../openblas-pthread/libblas.so.3`. 39 s.
- 2026-09-05: T3 — same `ecb06de7` export with `test-axes-certificate.R` lines
  527-567 removed (the `test_that()` opening at 527 through its matching `})`;
  the file drops from 14 blocks to 13). `check.sh` on that tarball exits 0 with
  `Status: OK` and `* checking tests ... OK`; `test-axes-certificate.R` appears
  zero times in `00check.log`. So the harness's ERROR tracks the block under
  test rather than firing unconditionally — the two runs differ only in those
  41 lines.
