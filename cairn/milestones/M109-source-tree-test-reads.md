# M109: Repair the test guards that skip on the surface that ships

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m109-source-tree-test-reads` / PR #141 (https://github.com/jmgirard/circumplex/pull/141)

## Goal

Make the tests that assert shipped behaviour through a source-tree path run
under `R CMD check`, and record the disposition of every source-tree read the
suite contains.

## Scope

Surface tier: **internal** — the deliverable is test-suite code, which no
external consumer of the package relies on.

Running the enumerating grep over `tests/testthat/`, before these repairs,
returned 50 lines in 24 files. Eleven already resolve under check through a dual-source fallback; four
are the guards this milestone repairs and two more are the read AC3 fences; 22
read artifacts no check-time test can reach (19 excluded from the build, 3
reading `R/` sources absent from the installed package); 11 sit in
`_problems/`, which testthat never collects. Exactly two assert shipped
behaviour and always skip.

**In:**
- The two vignette guards that always skip under check.
- The one dual-source Rd read missing its vacuity fence.
- The two `norms-audit.md` guards that gate on the file rather than the directory.
- A committed classification of every line the enumerating grep returns.

**Out:**
- Relocating or deleting the developer-only norms-audit checkers → stays with
  the existing candidate row about that script's argument-guard surface.
- The three `devel/` oracle-result pins, which fence shipped CI-coverage
  behaviour through an unshipped evidence artifact and would need a shipped
  artifact to change → candidate row.

## Acceptance criteria

- [x] AC1: The vignette test at `tests/testthat/test-axes-corrected-se.R:733`
      and the one at `tests/testthat/test-axes-scaled-fit.R:964` each obtain the
      vignette through `system.file("doc", "axes-reliability.Rmd", package =
      "circumplex")` when the source-tree path is absent, and each executes
      rather than skips in a `devtools::check()` run. A residual skip remains
      for the build that installs no vignettes, which `covr` does.
- [x] AC2: Both tests are proved able to fail inside a `devtools::check()` run
      by a probe deleting a sentence each requires present. The `AC7: the
      vignette's caveats match the corrected contract` guard is additionally
      proved by a probe inserting a sentence it requires absent; the `AC11: the
      vignette carries the same four claims` guard is not, because a deletion
      probe cannot exercise an `expect_no_match` assertion and, of the two
      guards' bodies, only the former carries any.
- [x] AC3: The dual-source Rd read in `tests/testthat/test-axes-scaled-fit.R`'s
      "AC12: the Rd fences the scaling against the robustness misreading" test
      carries a vacuity fence that fails both when its source yields empty text
      and when it yields a truncated read, proved by both plants.
- [x] AC4: The two guards at `tests/testthat/test-norms-provenance.R:581` and
      `:703` gate on `dir.exists()` of the tracking directory rather than on
      the status file, so a deleted `norms-audit.md` in a source checkout fails
      those tests rather than skipping them; proved by deleting it in a scratch
      copy. This is the shape M107 established at `test-fixture-drift.R`.
- [x] AC5: Running `grep -rn 'test_path("\.\."' tests/testthat/` classifies
      every line it returns into exactly one of: runs under `R CMD check`
      today; repaired by this milestone; development-only by construction,
      because the artifact it reads is absent from the tree a check-time test
      can reach -- its path is excluded by `.Rbuildignore`, or it is a file
      under `R/`, which an installed package does not carry; or never executed,
      because testthat does not collect it. The classification
      is committed in this file's `## Decisions` section, one row per returned
      line.
- [x] AC6: `Rscript -e 'devtools::test()'` and
      `Rscript -e 'devtools::check(args = "--no-manual")'` clean.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T1
- AC6 → T6

## Tasks

- [x] T1: Run the enumerating grep, classify every returned line into the four
      classes, and commit the table to this file's `## Decisions` section.
- [x] T2: Repair the two vignette guards, following the `vignette_path()`
      helper at `tests/testthat/test-cpm_boundary_vignette.R:7-25` — candidate
      vector, `nzchar() & file.exists()` filter, `skip_if` on the empty case.
- [x] T3: Probe both repaired guards inside a check run, deletion and insertion
      arms, and record which assertion each reddens.
- [x] T4: Add the vacuity fence to `test-axes-scaled-fit.R`'s AC12 test,
      matching the `expect_gt(nchar(rd), 1000L)` the other five dual-source
      reads of `axes_reliability.Rd` carry; plant an empty and a truncated read.
- [x] T5: Repair the two `norms-provenance.R` guards to `dir.exists()`; prove
      by scratch-copy deletion that a missing record reddens.
- [x] T6: Run the profile verify slot and the check.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose leaving the 38 no-counterpart reads in place over relocating the norms-audit checkers to a developer script, because they cannot run under a package check by construction and most already say so in a comment; falsified by evidence a maintainer path reaches them expecting check coverage.
- 2026-08-24: criteria audit ran in FULL mode ([O], fresh context, authored none of them) over M109 and M110 together — M110's user-facing tier mandates full, and M109 was audited at the same bar though its internal tier and tripwire-free criteria would have allowed the reduced mode. Fixed here before writing: the classification trichotomy could not classify the 11 never-collected `_problems/` lines nor the dev-only builder-script reads, so a fourth class was added and the runtime-half clause dropped; and the mutation probe was deletion-only, which provably cannot exercise the five `expect_no_match` assertions in the AC7 test, so an insertion arm was added.
- 2026-08-24: M108 deleted `tests/testthat/test-fixture-drift.R` with the second copy of the exemplar-B fixture; AC4 cites it as the shape to follow, and AC4 states that shape in full itself (gate on `dir.exists()` of the tracking directory, not on the status file). The worked example now lives in the M107 archive and in git.
- 2026-08-24: implement gate chose a shared `helper-vignette.R` lookup over per-file copies (the boundary-vignette test keeps its own, being out of scope), and `dir.exists("cairn/references")` over the tracking root for the norms guards.
- 2026-08-24: amendment (mini gate, all three recommended): Scope's tally corrected to the measured 50 lines / 24 files and its four classes; AC3's "one of the four dual-source Rd reads" census clause deleted and its `:1049` cite replaced by the test's own title; AC5's third class restated as `.Rbuildignore`-excluded or under `R/`, so the three `R/` reads it could not classify now sort.
- 2026-08-24: amended AC3 and AC5 went to a fresh-context [O] reduced criteria audit (internal tier) that authored neither; it returned six findings. Its two narrowing repairs are the amended wording above. Its two instrument findings -- AC3's "proved by both plants", AC5's committed-ledger sentence -- were declined: the deliverable here is a check whose ability to fail is the shipped property, and the ledger is that classification's evidence.
- 2026-08-24: T4 reworded to name the AC12 test rather than a line number, matching the amended AC3.
- 2026-08-24: T2 done -- `tests/testthat/helper-vignette.R` adds `vignette_source(file)`, trying `vignettes/` then `system.file("doc", ...)`, and both repaired guards call it and `skip_if(!nzchar(vig))`. Suite clean: FAIL 0 | WARN 5 | SKIP 1 | PASS 8627.
- 2026-08-24: T4 done -- the AC12 test's dual-source Rd read now carries `expect_gt(nchar(rd), 1000L)`. Planted in a scratch copy of the repo: an emptied `man/axes_reliability.Rd` reddens it at nchar 0, a 400-byte truncation at nchar 382; the unmutated control passes with the fence exercised.
- 2026-08-24: T5 done -- both `norms-audit.md` guards gate on `dir.exists("cairn/references")` and then assert `file.exists()` of the record itself, so a deleted record fails as itself rather than as readLines()'s connection error. Planted by deleting the record in the scratch copy: both tests failed, neither skipped. Suite clean: FAIL 0 | WARN 5 | SKIP 1 | PASS 8630.
- 2026-08-24: T1 done -- all 49 lines the grep now returns are classified in the Decisions table above (11 run under check, 5 repaired here, 22 development-only, 11 never collected). Scope's tally is marked as the pre-repair state at the mini gate's recommended option; the table carries the current one.
- 2026-08-24: T3 done -- three `devtools::check()` runs. Baseline on the branch: Status OK, 0 errors / 0 warnings / 0 notes, 8m43s. Deletion arm (one required sentence removed per guard, scratch copy): both reddened -- `test-axes-corrected-se.R:757` on `expect_match("The component standard errors are **corrected** for it")` and `test-axes-scaled-fit.R:974` on `expect_match("| .092 | .079 | .062 | .054 |")`, which is also the proof both execute under check rather than skipping. Insertion arm ("Treat them as order-of-magnitude guidance." added): `test-axes-corrected-se.R:750` reddened on `expect_no_match("order-of-magnitude")`; the AC11 guard did not, carrying no such assertion.
- 2026-08-24: amendment (mini gate, recommended option): AC2 narrowed to what the probes can prove -- both guards by the deletion arm, the AC7 guard alone by the insertion arm, because the AC11 guard's five assertions are all positives. A fresh-context [O] reduced audit of the amended wording returned one finding, applied here: the asymmetry now names the two `test_that` blocks rather than their files, since both files carry `expect_no_match` assertions elsewhere.
- 2026-08-24: T6 done -- `devtools::test()` FAIL 0 | WARN 5 | SKIP 1 | PASS 8630 and `devtools::check(args = "--no-manual")` 0/0/0 on the branch; no code changed after either run.
- 2026-08-24: the two guards AC1 names are the `AC7: the vignette's caveats match the corrected contract` test in `test-axes-corrected-se.R` and the `AC11: the vignette carries the same four claims` test in `test-axes-scaled-fit.R`; the line numbers AC1 cites move as this branch edits those files.

## Decisions

### Classification of every source-tree read (T1, AC5)

`grep -rn 'test_path("\.\."' tests/testthat/` returns **49 lines in 25 files**
after this milestone's repairs (50 in 24 before them: the two vignette guards'
own `test_path()` calls collapsed into the single call in the new
`helper-vignette.R`). Every returned line sits in exactly one class below --
11 run under `R CMD check` today, 5 are repaired here, 22 are development-only
by construction, and 11 are never collected at all. The never-collected class
is not a reading of the `.Rbuildignore` entry alone: no `_problems/` file
appears anywhere in a `devtools::test()` run's output.

| line | reads | class | why |
|---|---|---|---|
| `_problems/test-norms-audit-batch-118.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-batch-122.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-batch-124.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-320.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-320.R:14` | `cairn/references` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-324.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-324.R:14` | `cairn/references` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-332.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-coverage-332.R:14` | `cairn/references` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-markers-496.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `_problems/test-norms-audit-markers-509.R:5` | `data-raw/audit-norms.R` | never collected | testthat collects `test-*.R` in `tests/testthat/` only, never a subdirectory; `_problems/` is `.Rbuildignore`d as well |
| `helper-norms-audit-script.R:38` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `helper-vignette.R:15` | `vignettes` | repaired here | the shared lookup's first candidate; it falls back to `system.file("doc", ...)`, which is what an installed package carries |
| `test-axes-corrected-se.R:581` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-corrected-se.R:1192` | `R/axes_corrected_se.R` | dev-only | `R/` sources ship in the tarball but an installed package carries none, and this read has no installed counterpart |
| `test-axes-reliability.R:2870` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-reliability.R:3025` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-reliability.R:3062` | `vignettes/axes-reliability.Rmd` | runs under check | dual source: `vignettes/` in the source tree, `system.file("doc", ...)` once installed |
| `test-axes-reliability.R:3242` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-scaled-fit.R:934` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-scaled-fit.R:935` | `man/axes_reliability.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-axes-scaled-fit.R:1054` | `man/axes_reliability.Rd` | repaired here | dual-source Rd read; ran under check already, and now carries the vacuity fence its five siblings carry |
| `test-axes-scaled-fit.R:1055` | `man/axes_reliability.Rd` | repaired here | dual-source Rd read; ran under check already, and now carries the vacuity fence its five siblings carry |
| `test-axes-scaled-fit.R:1147` | `R/axes_scaled_fit.R` | dev-only | `R/` sources ship in the tarball but an installed package carries none, and this read has no installed counterpart |
| `test-axes-scaled-fit.R:1148` | `R/axes_corrected_se.R` | dev-only | `R/` sources ship in the tarball but an installed package carries none, and this read has no installed counterpart |
| `test-cpm_boundary_vignette.R:11` | `vignettes/evaluating-circumplex-structure.Rmd` | runs under check | dual source: `vignettes/` in the source tree, `system.file("doc", ...)` once installed |
| `test-cpm_boundary_vignette.R:214` | `man/summary.circumplex_cpm.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-fit_structure.R:255` | `data-raw/structure-test-cutoffs.rds` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-batch.R:19` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-compare.R:20` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-coverage.R:19` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-coverage.R:29` | `cairn/references` | dev-only | `cairn/` is `.Rbuildignore`d |
| `test-norms-audit-coverage.R:390` | `data-raw` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-manifest.R:21` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-markers.R:31` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-roster.R:14` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-audit-roster.R:24` | `cairn/references` | dev-only | `cairn/` is `.Rbuildignore`d |
| `test-norms-audit-sample-key.R:17` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:464` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:473` | `cairn/references` | dev-only | `cairn/` is `.Rbuildignore`d |
| `test-norms-provenance.R:527` | `man/.Rd` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-norms-provenance.R:583` | `cairn/references` | repaired here | now gates on `dir.exists()` of `cairn/references`, so a deleted `norms-audit.md` fails rather than skips; `cairn/` is `.Rbuildignore`d, so the read itself stays development-only and the repair buys the developer run, not the check run |
| `test-norms-provenance.R:616` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:651` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:711` | `cairn/references` | repaired here | now gates on `dir.exists()` of `cairn/references`, so a deleted `norms-audit.md` fails rather than skips; `cairn/` is `.Rbuildignore`d, so the read itself stays development-only and the repair buys the developer run, not the check run |
| `test-rd-latex-safe.R:36` | `man` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-ssm_occasions.R:446` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |
| `test-ssm_occasions.R:849` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |
| `test-ssm_occasions.R:881` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |

## Review

Reviewed 2026-08-24 on branch `m109-source-tree-test-reads`, PR #141. `master`
sat at fe8cf0ad, unmoved since the branch was cut, so no merge into the branch
was needed. Every figure below comes from a run made during this review.

### Acceptance-criteria evidence

- **AC2** — `devtools::check(args = "--no-manual")` on a scratch copy with one
  required sentence deleted per guard (the `**corrected** for it` sentence, and
  the `| rejection rate | .092 | .079 | .062 | .054 |` table row): FAIL 2, both
  guards red inside the check — `test-axes-corrected-se.R:757` on its
  `expect_match` of the corrected-SE sentence, `test-axes-scaled-fit.R:974` on
  its `expect_match` of the table row, each failure message printing the
  vignette text it read. Insertion arm (`Treat them as order-of-magnitude
  guidance.` added, same scratch shape): FAIL 1 — `test-axes-corrected-se.R:750`
  red on `expect_no_match("order-of-magnitude")`; the AC11 guard stayed green,
  its four assertions all positives.

- **AC3** — three `devtools::test(filter = "axes-scaled-fit")` runs on a scratch
  copy. Unmutated control: FAIL 0 | SKIP 1 | PASS 1871. Emptied
  `man/axes_reliability.Rd`: the fence at `test-axes-scaled-fit.R:1066` red,
  `nchar(rd)` 0. Truncated to 400 bytes: same fence red, `nchar(rd)` 382. Both
  plants fail it; the control passes with it exercised.

- **AC4** — `cairn/references/norms-audit.md` deleted in a scratch copy,
  `devtools::test(filter = "norms-provenance")`: FAIL 4 | SKIP 0. Both guards
  red on their own named assertion — `test-norms-provenance.R:588` and `:716`,
  `expect_true(file.exists(status))`, each carrying the
  `cairn/references/norms-audit.md is missing` info string — and each followed
  by `readLines()`'s connection error on the next statement. Neither skipped.

- **AC5** — the enumerating grep returns 49 lines in 25 files at this commit.
  The 49 table keys in the `## Decisions` classification, sorted, `diff` clean
  against the grep's own sorted output: every returned line appears exactly
  once, and the table names no line the grep does not return. Class tallies
  11 + 5 + 22 + 11 = 49. The third class checks out against the tree:
  `.Rbuildignore` carries `^devel$`, `^data-raw$` and `^cairn$`, covering 19 of
  the 22, and the remaining three read files under `R/`. The never-collected
  class checks out against a run: no `_problems/` path appears anywhere in the
  `devtools::test()` output.

- **AC6** — `devtools::test()` on the branch: FAIL 0 | WARN 5 | SKIP 1 |
  PASS 8630, the one skip at `test-axes-scaled-fit.R:918` and pre-existing.
  `devtools::check(args = "--no-manual")` on the branch: Status OK, 0 errors /
  0 warnings / 0 notes, 10m5s.

- **AC1** — both guards execute under `devtools::check(args = "--no-manual")`:
  in the AC2 deletion arm each reddened inside the check run, its failure
  message printing the vignette prose it had read, which a skipped test cannot
  do. The read came through the fallback, not the source tree: a check runs the
  tests from `<pkg>.Rcheck/tests`, whose `../../vignettes` does not exist, so
  `system.file("doc", "axes-reliability.Rmd", package = "circumplex")` is the
  candidate that resolved. Discrimination control — the same deleted sentences
  with `master`'s pre-repair guards restored and `helper-vignette.R` removed:
  Status OK, 0 errors / 0 warnings / 0 notes, 11m19s. The repair is what makes
  them run. Residual skip confirmed rather than assumed: reproducing `covr`'s
  own path (`R CMD INSTALL --install-tests` of the source directory, then
  `tools::testInstalledPackage(types = "tests")`) skips exactly these two —
  `test-axes-corrected-se.R:744`, `test-axes-scaled-fit.R:969`, reason
  `vignette source unavailable (build installed without vignettes)`.

### Consistency gate

`cairn_validate.py` exit 0, all checks passed; 47 advisory warnings, every one
M7's multi-line work-log entries, none from this milestone. No `DESIGN.md`
principle changed, so `cairn_impact.py` did not run. Toolchain slot
(`r-package`): `document()` produced no diff and zero `resolve link` lines at
`cli.width = 500`; no generated file hand-edited; `README.md` newer than
`README.Rmd`; `pkgdown::check_pkgdown()` clean; no NEWS entry owed, the diff
changing test-suite code only; no new top-level file, so no `.Rbuildignore`
entry owed; full check clean (AC6). Master watches: the newest push run on
`master` reaching a verdict is e1f405e1, `success` on both `R-CMD-check.yaml`
and `test-coverage.yaml` (fe8cf0ad is `cairn/`-only and ran neither).
`check-master-red-alert.R`, `master-red-alert-dryrun.R` (5/5 synthetic
payloads) and `check-branch-protection.R` all exit clean.

### Review findings and triage

Three fresh-context reviewers, none having authored the work: [O] on the diff,
[S] on `git blame`/`log` history, [S] on the prior-review record. The history
lens returned no findings, confirming the cited precedents hold: the
`vignette_source()` shape copies `test-cpm_boundary_vignette.R:7-25`, and the
`dir.exists()` gating is M107's shape at `test-fixture-drift.R` (deleted by M108
for an unrelated reason). The prior-review lens found no regression of a past
lesson; its `gh api .../pulls/comments` probe returned `[]`, so the PR-thread
walk was skipped. [O] returned nine ranked findings.

Fixed on the branch:

- **F1** (ranked 1) — the fence comment at `test-axes-scaled-fit.R:1066` claimed
  "every expect_no_match below is satisfied by an empty read", but that block
  holds four `expect_match` assertions and no `expect_no_match`, so both plants
  would have reddened it without the fence. Comment rewritten to what the fence
  actually buys here. Checking the finding's own premise turned up a further
  correction: none of the five sibling reads carries an `expect_no_match`
  either, so the rewritten comment does not claim they do.
- **F2** (ranked 2) — the comment at `test-norms-provenance.R:587`/`:716` claimed
  a deleted record "fails as itself rather than as `readLines()`'s connection
  error"; `expect_true()` does not halt the block, so both are emitted, as the
  AC4 run shows. Comment corrected; the assertion stays, since it is what names
  the missing file. The prior-review lens raised the same point independently.
- **F4** (ranked 4) — the two `test-norms-provenance.R` ledger rows read
  `cairn/`, which is `.Rbuildignore`d, so "repaired here" alone could read as
  "now runs under check". Both rows' `why` cells now say the read stays
  development-only and what the repair bought.

Rejected, with reason:

- **F3** (ranked 3) — that AC1's residual-skip clause is wrong about `covr`.
  Refuted against the implementation, not the account of it: `covr` installs the
  source directory and runs `tools::testInstalledPackage()`, whose working
  directory is the installed tree, and the repo carries no `inst/doc`.
  Reproducing that path skips exactly the two guards, reason `vignette source
  unavailable (build installed without vignettes)` (AC1 evidence above).
- **F5** — a third inline copy of the lookup at `test-axes-reliability.R:3062`.
  Pre-existing, outside the diff, and already classified `runs under check`; it
  is not defective, only unshared.
- **F6** — Scope's "the one dual-source Rd read missing its vacuity fence" does
  not count `test-cpm_boundary_vignette.R:214`, a sixth unfenced read. Scope is
  plan-owned and was gated; the read is positives-only, so nothing is at risk.
- **F7** — the census rests on one literal grep pattern with no validator. AC5
  defines the census by that grep; the reviewer searched the alternate spellings
  and found none at this commit.
- **F8** — four added lines exceed 80 columns. Formatter class.
- **F9** — the fence sits after the whitespace collapse where the siblings put
  it before. Numerically immaterial.

No finding demonstrated an acceptance criterion failing, and none is a defect in
what the package does for its users; status stays `review`. `devtools::test()`
re-run over both touched files after the three fixes: FAIL 0 | SKIP 1 |
PASS 2073.
