# M121: A local reproduction of CRAN's linux-arm64 check flavor

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — dev tooling; no consumer of the package relies on it
- **Branch/PR:** `m121-arm64-check-harness` / https://github.com/jmgirard/circumplex/pull/154

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

- [x] AC1: `tools/arm64/check.sh` run on a source tarball built from commit
      `ecb06de7` reports `Status: 1 ERROR`, and the test-failure block of the
      resulting `00check.log` names `test-axes-certificate.R:544:3` and
      contains `the shipped pricing REFUSES at case 'cxb'`.
- [x] AC2: The same run writes `arm64-platform.txt` beside the tarball, from
      inside the container that runs the check, recording `R.version$platform`
      equal to `aarch64-unknown-linux-gnu` and both a `La_library()` and an
      `extSoftVersion()[["BLAS"]]` path containing `openblas`.
- [x] AC3: The same script run on a tarball identical to AC1's except that the
      `test_that()` block opening at `test-axes-certificate.R:527` is deleted
      reports no ERROR arising from `test-axes-certificate.R` — showing the
      harness distinguishes the failing tarball from a passing one rather than
      reporting ERROR unconditionally.
- [ ] AC4: The image is pinned by digest, and `tools/arm64/`'s header records
      the tag the digest was taken from, the R and OpenBLAS versions, the four
      omitted Suggests with the count of assertions they cost (11 against
      CRAN's 2410), and that CRAN's macOS x86_64 flavor is not covered.
- [x] AC5: `cairn/PROFILE.md`'s release-walk slot requires a dated green arm64
      check log in `cran-comments.md`'s test-environments list before the
      submission step; `CLAUDE.md`'s Commands section carries the command; and
      `tools/arm64/` carries a digest-refresh recipe.
- [x] AC6: `Rscript -e 'devtools::check(manual = TRUE)'` reports 0 errors and
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
- [x] T4: Add the release-walk requirement to `cairn/PROFILE.md`, the command
      to `CLAUDE.md`, and the refresh recipe to `tools/arm64/`.
- [x] T5: Run `devtools::check(manual = TRUE)` and `tar -tzf` on the built
      tarball.

## Review

Fresh evidence, 2026-09-05, on branch `m121-arm64-check-harness` at the head
pushed to PR #154. The tarballs were rebuilt from `git archive ecb06de7`; the
image is `circumplex-arm64check:latest`, id `b61a7aa01c56` — the same id T1
recorded, so the pin held.

- **AC1 — pass.** `R CMD build` on a fresh `git archive ecb06de7` export
  produced `circumplex_2.0.1.tar.gz` (9,790,877 bytes). `tools/arm64/check.sh`
  on it exits 1; the last line of `00check.log` (378) is `Status: 1 ERROR`, and
  the failure block at lines 350-351 reads
  `── Failure ('test-axes-certificate.R:544:3'): AC2/AC3: at counterexample B
  the estimate brackets a 3.4%-wrong SE ──` / `the shipped pricing REFUSES at
  case 'cxb' (unidentified, unidentified) -- an admitted geometry, so this is a
  regression, not a platform difference`. The testthat tally is
  `[ FAIL 1 | WARN 4 | SKIP 540 | PASS 2399 ]`, identical to T2's.
  (A first attempt built with `--no-build-vignettes`; that tarball reported
  `Status: 1 ERROR, 2 WARNINGs`, both warnings about the absent vignette
  sources, and the tally shifted to `SKIP 546 | PASS 2365`. It is not the
  tarball AC1 names and was discarded; the criterion was verified on the plain
  `R CMD build` tarball above.)
- **AC2 — pass.** The same run wrote `arm64-platform.txt` beside the tarball.
  It is stamped `tarball: circumplex_2.0.1.tar.gz` / `date:
  2026-09-05T18:02:04Z` — the run's own time, and the file was removed by
  `check.sh` before the run, so no earlier answer could survive. Contents:
  `platform: aarch64-unknown-linux-gnu`; `LAPACK:
  /usr/lib/aarch64-linux-gnu/openblas-pthread/libopenblasp-r0.3.33.so`; `BLAS:
  /usr/lib/aarch64-linux-gnu/openblas-pthread/libblas.so.3`. Both linear-algebra
  paths contain `openblas`. The probe runs in the same `docker run` as
  `R CMD check`, so this is the container the check ran in.
- **AC3 — pass.** A second `git archive ecb06de7` export with
  `tests/testthat/test-axes-certificate.R` lines 527-567 deleted — the
  `test_that("AC2/AC3: at counterexample B …")` block opening at 527 through its
  matching `})` at 567; `diff` against the committed file reports exactly those
  41 lines removed and nothing else, and the file drops from 14 `test_that(`
  blocks to 13. `check.sh` on the resulting tarball exits 0, `00check.log:83`
  reads `Status: OK`, line 76 reads `* checking tests ... OK`, and
  `test-axes-certificate.R` appears zero times in the log. So the harness's
  ERROR tracks the block under test rather than firing unconditionally.
- **AC4 — pass.** `tools/arm64/Dockerfile`'s `FROM` is
  `r-base@sha256:41d5564375009abf74a63987fd7fb9b44c90b1580b310be10ef973abe92496c3`
  — a digest, not a tag. `tools/arm64/README.md`'s "What the image is" table
  records the tag the digest came from (`r-base:latest`, resolved 2026-09-05,
  same digest string), R 4.6.1 (2026-06-24), and OpenBLAS Debian
  `libopenblas0-pthread` 0.3.33+ds-3. "What it does not cover" names all four
  omitted Suggests (`brms`, `OpenMx`, `glmmTMB`, `vdiffr`) and their cost as 11
  assertions, 2399 here against CRAN's 2410 — the 2399 is the figure AC1's run
  reproduced, and the 2410 is transcribed at
  `cairn/reviews/archive/RB22-certificate-platform-refusal.md:49`, which the
  README cites. The same section states that CRAN's macOS x86_64 flavor is not
  covered and that a green run here is evidence about arm64 only.
- **AC5 — pass.** `cairn/PROFILE.md`'s `release-walk` slot carries "linux-arm64
  check, before the handoff", requiring `Status: OK` on the tarball about to be
  submitted and its dated green log in `cran-comments.md`'s test-environments
  list, with "Without it the submission step is not reached" — placed above the
  handoff-checklist bullet. `CLAUDE.md`'s Commands section carries `arm64 CRAN
  flavor: tools/arm64/check.sh <tarball>`. `tools/arm64/README.md` carries the
  five-step "Refreshing the pin" recipe (`docker pull` → `docker inspect
  --format '{{index .RepoDigests 0}}'` → edit the `FROM` line and the
  tag-resolution date → rebuild → re-run on a known tarball and update the
  recorded versions).
- **AC6 — pass.** `Rscript -e 'devtools::check(manual = TRUE)'` on the branch:
  `Status: OK`, `0 errors | 0 warnings | 0 notes`, duration 24m 14.4s, with
  `* checking PDF version of manual ... OK` — the step `--no-manual` skips. On
  the tarball that check built, `tar -tzf` lists 357 paths, 0 of them under
  `circumplex/tools/` and 0 matching `arm64` or `Dockerfile` (case-insensitive);
  `.Rbuildignore`'s pre-existing `^tools$` entry covers the new directory.

### Consistency gate — pass

- `cairn_validate.py`: exit 0, `all checks passed`, 90 advisory warnings (the
  `work-log format` advisory on multi-line entries in M121 and M122 — a
  pre-existing style in this repo, not a gate failure).
- `cairn_impact.py`: skipped — the milestone's `Principles touched:` slot is
  `—` and the diff changes no DESIGN principle.
- `devtools::document()` (with `cli.width = 500`): no working-tree diff, 0
  lines matching `resolve link`.
- Generated files: covered by the no-diff `document()` run.
- README.Rmd: not touched by the diff; `document()` left it in sync.
- `pkgdown::check_pkgdown()`: `No problems found`.
- NEWS.md: no entry owed — M121 ships developer tooling that never enters the
  built package (AC6's `tar -tzf` confirms), so there is no user-visible change.
- `.Rbuildignore`: `^tools$` already present, confirmed by AC6's manifest.
- `devtools::check()`: AC6's run, 0 errors / 0 warnings / 0 notes.
- Master watches: `R-CMD-check.yaml` and `test-coverage.yaml` both `success` on
  the newest push run of `master` (2026-09-04, "record the 2.0.1 resubmission
  and the platform-exactness sweep").
- `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` (5/5
  synthetic payloads ok) and `tools/check-branch-protection.R` all exit clean.
- Budgets: `cairn/PROFILE.md` 119 lines / 8,109 bytes (cap 120 lines);
  `cairn/LESSONS.md` 49 lines / 19,992 bytes (cap 50 / 20,000 — 8 bytes of
  headroom, so any lesson added at hygiene needs a retirement first);
  `cairn/test-craft.md` 29 lines / 8,924 bytes against its header's < 35 / <
  9,000.

### Independent review — three lenses

Full three-lens fan-out: the diff adds executable surface (`check.sh`,
`testfile.sh`, `Dockerfile`), so the internal-tier docs-only shortcut does not
apply. The [S] prior-review lens reported **no prior-review evidence** — no
archived `## Review` section touches these files, the probe
`gh api repos/jmgirard/circumplex/pulls/comments?per_page=1` returned `[]`, and
`LESSONS.md` has no docker/arm64 line — so it contributed zero findings, as
designed.

Findings from the [O] diff-bug and [S] blame-history lenses, verified against
the implementation rather than against the reporter's account of it. The gate
was not reached (see the amendment return below), so these carry **proposed**
dispositions and are triaged at the re-review.

- **F1 (confirmed by measurement) — `check.sh` exits 0 on a WARNING-only
  check.** `R CMD check` exits nonzero only on ERROR, and `check.sh:80`
  propagates that status verbatim, while `PROFILE.md`'s new release-walk slot
  says the run "must report `Status: OK`". Reproduced: the same block-deleted
  export built with `--no-build-vignettes` gives `Status: 2 WARNINGs` and
  `check.sh` exits **0**. CRAN rejects on a WARNING, so the harness's success
  signal disagrees with the gate that consumes it. Proposed: fix now (assert
  `^Status: OK$`, or exit nonzero on any non-OK status).
- **F2 — the platform probe's failure is swallowed.** Inside the `bash -c` at
  `check.sh:63-65` there is no `set -e` and no `pipefail`;
  `Rscript -e "$PROBE" | tee -a arm64-platform.txt` takes `tee`'s status, so a
  dead probe leaves a header-only `arm64-platform.txt` and the run continues.
  `check.sh` then prints `platform record: …` unconditionally. AC2's guarantee
  is verified by hand today, not by the script. Proposed: fix now.
- **F3 — `circumplex.Rcheck` is hardcoded and never proven fresh.**
  `check.sh:69` reads `$DIR/circumplex.Rcheck/00check.log`. `R CMD check`
  unlinks only the directory its own tarball names, so a tarball unpacking to
  another package name leaves a previous run's log in place and the `Status:`
  guard passes on it. The re-audit fixed exactly this class for
  `arm64-platform.txt` (stamped, removed before the run) and left the log,
  the more load-bearing artifact, untreated. Proposed: fix now (remove the
  check directory before the run).
- **F4 — nothing ties the running image to the pinned digest.**
  `check.sh:22` only asks whether the tag exists; a stale build, or a
  `CIRCUMPLEX_ARM64_IMAGE` override, yields a green the README's version table
  does not describe. `arm64-platform.txt` records what it got but nothing
  compares it to the recorded values. Proposed: candidate row.
- **F5 — the README's flavor label is wrong, and two of the lens's three
  sub-claims are refuted.** `README.md:3-4` calls this "CRAN's
  `r-devel-linux-x86_64` **arm64** additional check"; CRAN serves that log
  under `specialChecks/linux-arm64`, and `r-devel-linux-x86_64` is a different
  (x86_64) flavor. **Refuted:** the lens also claimed the image is release R
  where CRAN runs R-devel, and that CRAN's pretest uses `--as-cran`. CRAN's own
  2.0.1 arm64 log reads `R version 4.6.1 (2026-06-24)` and records its options
  as `'--no-manual --no-vignettes'` — the harness matches CRAN's R version and
  its flags exactly. Proposed: fix now (the label only).
- **F6 — the digest pin covers the base layer only.** `Dockerfile:15` runs
  `apt-get update` against Debian unstable, whose index the file's own comment
  says drifts out of sync with the image, and lines 21/27 fetch current CRAN
  versions. T1's "byte-identical image" is a cache hit, not a rebuild, so the
  stated rationale ("a harness that fails to build on the day of a
  resubmission…") is only partly delivered. Proposed: candidate row plus a line
  under "What it does not cover".
- **F7 (confirmed by measurement) — the README's "11 assertions" attribution is
  false, and AC4 pre-committed to it.** See the amendment return below.
- **F8 — `testfile.sh` interpolates unsafely and compiles into the host tree.**
  `testfile.sh:17-20` puts `$FILE` inside a double-quoted `bash -c` string —
  the pattern `check.sh` deliberately avoids with `bash -c '…' _ "$BASE"` — and
  `R CMD INSTALL … .` compiles in place on a read-write mount of the repo,
  leaving aarch64-Linux `src/*.o` and `src/*.so` in a macOS working tree, with
  install diagnostics sent to `/dev/null`. Proposed: fix now.
- **F9 — "One command" overstates.** `README.md:5` promises one command;
  `check.sh:22-25` exits 2 until `docker build` has been run, and `CLAUDE.md`'s
  new entry does not mention the prerequisite. Proposed: fix now.
- **F10 — `check.sh`'s file header overclaims.** Line 8 says
  `arm64-platform.txt   the platform and LAPACK the check ran on`: it omits
  BLAS (which AC2 now requires) and reasserts the process-identity claim the
  body comment at lines 38-41 was rewritten to disavow. Proposed: fix now.
- **F11 — the `greenfield-openers` rewrite is outside Scope and lost a
  distinction.** Scope "In" names the release-walk step and the `CLAUDE.md`
  command, not this section. The original scoped the actual menu to three
  options (**pure R**, Rcpp, RcppArmadillo), deliberately narrower than the
  five compiled-code types the question named; the rewrite merges them into
  "Rcpp / RcppArmadillo / C / C++ / Fortran, or pure R?", which reads as five
  selectable options. Verified: the section runs 13 lines on `master` and 9
  here. Proposed: fix now (restore the options-vs-types distinction).
- **F12 — the T4 work-log line misreports the compression.** It says the slot
  went "15 lines to 9"; the section is 13 lines on `master` and 9 here (12→8
  excluding the header). The resulting 119 lines / 8,109 bytes are correct.
  Proposed: fix now (correct the number).
- **F13 — `.gitignore` has no `*.Rcheck` or `arm64-platform.txt` entry**, so
  running `check.sh` on a tarball in the repo root leaves untracked strays.
  Proposed: fix now.
- **Rejected — "AC6 is unticked while the milestone is at `review`."** That was
  the pre-review state the reviewers read; AC6 is ticked above against its own
  fresh evidence. The blame lens independently confirmed the unticked box was
  correct under AC fencing.
- **Rejected — the `greenfield-openers` intro rewording** ("distribution
  ambition, rendered here as **CRAN intent**" → "CRAN intent"). The operative
  meaning is unchanged; the reporting lens said as much.
- **Noted, no action — Dockerfile hardcodes `-j8` / `Ncpus = 8L`, and the
  second install layer carries no version assertion.** The install verification
  does catch a failed install, which is what it was written for.

### Amendment return — AC4

**F7, measured.** `README.md` states that the four omitted Suggests cost 11
assertions, "2399 [here] against CRAN's 2410". Both tallies are real, but the
attribution is false, and three measurements show it:

| run | Suggests present | tally |
|---|---|---|
| host macOS (AC6) | all four | `SKIP 69 \| PASS 8689` |
| arm64 container | none | `SKIP 540 \| PASS 2399` |
| CRAN arm64, 2.0.1 `ecb06de7` | — | `SKIP 540 \| PASS 2410` |

Removing the four packages moves `SKIP` from 69 to 540. CRAN's arm64 log
reports `SKIP 540` — the container's figure exactly — and never mentions
`vdiffr`, `brms`, `OpenMx` or `glmmTMB`. So CRAN's arm64 machine does not have
them either, and their absence costs this harness **nothing** relative to
CRAN. The 11-assertion gap is real but has an uncharacterized cause (most
likely expectations guarded by `requireNamespace()` rather than
`skip_if_not_installed()`, which add passes without adding skips).

AC4 does not merely permit this claim, it requires it: it binds the header to
record "the four omitted Suggests **with the count of assertions they cost**
(11 against CRAN's 2410)". The header records exactly what AC4 demands, so the
implementation is faithful; the criterion is what is wrong. Under the
never-reinterpret rule this is evidence about the promise rather than about the
work, so it takes the amendment return rather than a defect return: AC4's
checkbox is unticked, status goes to `in-progress` for that amendment alone,
and review stops here. The amendment is the only work convened; F1-F3 and
F5-F13 keep their proposed dispositions and are triaged at the re-review gate.

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
- 2026-09-05: T4 — release-walk slot now requires a `Status: OK` arm64 run on
  the submission tarball and its dated log in `cran-comments.md`'s
  test-environments list before the handoff step; `CLAUDE.md`'s Commands
  section carries `tools/arm64/check.sh <tarball>`; the refresh recipe is
  `tools/arm64/README.md`'s "Refreshing the pin". `PROFILE.md` was at 119 of
  its 120-line cap, so the `greenfield-openers` slot was compressed 15 lines to
  9 (no content dropped) to make room: 119 lines, 8,109 bytes after.
- 2026-09-05: T5 — `devtools::check(manual = TRUE)` on the branch: `Status: OK`,
  0 errors / 0 warnings / 0 notes, 12m 8.9s, the PDF-manual step included. On
  that same built tarball `tar -tzf` lists 357 paths, 0 of them under
  `circumplex/tools/` and none matching `arm64` or `Dockerfile` — the existing
  `^tools$` `.Rbuildignore` entry covers the new directory. The suite also
  passes on macOS arm64 (`checking tests ... OK`), where the `cxb` case prices,
  which is the platform dependence M122 takes up.
- 2026-09-05: all tasks done, status → review.
- 2026-09-05: review — PR #154 opened as a draft. All six criteria run with
  fresh evidence; AC1, AC2, AC3, AC5 and AC6 pass (Review section). Consistency
  gate green: `cairn_validate` exit 0 (advisories only), `document()` no-diff
  and no unresolved links, `pkgdown::check_pkgdown()` clean, `devtools::check()`
  0/0/0, both master watches `success`, the three `tools/` audit scripts clean.
  Three-lens fan-out run (executable surface in the diff); the prior-review lens
  reported no prior-review evidence and contributed nothing. Thirteen findings
  recorded, two rejected, one noted.
- 2026-09-05: amendment return: AC4 — "the four omitted Suggests, the harness's
  own passing-assertion count beside CRAN's for the same tarball (2399 against
  2410), and that the cause of that gap is uncharacterised and is not the
  omitted Suggests". Measured: removing the four Suggests moves the suite from
  `SKIP 69 | PASS 8689` (host, all four present) to `SKIP 540 | PASS 2399`
  (container, none present), and CRAN's own arm64 log for `ecb06de7` reports
  `SKIP 540 | PASS 2410` and never names the four packages — so CRAN's machine
  lacks them too and their absence costs this harness nothing relative to CRAN.
  AC4 as written requires the header to record 11 as "the count of assertions
  they cost", which is false; the implementation records exactly what AC4
  demands, so the criterion is the defect. First amendment return on M121;
  defect-return count unchanged at 0.

