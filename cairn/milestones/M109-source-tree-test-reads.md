# M109: Repair the test guards that skip on the surface that ships

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m109-source-tree-test-reads`

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

- [ ] AC1: The vignette test at `tests/testthat/test-axes-corrected-se.R:733`
      and the one at `tests/testthat/test-axes-scaled-fit.R:964` each obtain the
      vignette through `system.file("doc", "axes-reliability.Rmd", package =
      "circumplex")` when the source-tree path is absent, and each executes
      rather than skips in a `devtools::check()` run. A residual skip remains
      for the build that installs no vignettes, which `covr` does.
- [ ] AC2: Each of those two tests is proved able to fail inside a
      `devtools::check()` run by two probes of differing form — one deleting a
      sentence the test requires present, one inserting a sentence the test
      requires absent — because a deletion probe cannot exercise the
      `expect_no_match` half at all.
- [ ] AC3: The dual-source Rd read in `tests/testthat/test-axes-scaled-fit.R`'s
      "AC12: the Rd fences the scaling against the robustness misreading" test
      carries a vacuity fence that fails both when its source yields empty text
      and when it yields a truncated read, proved by both plants.
- [ ] AC4: The two guards at `tests/testthat/test-norms-provenance.R:581` and
      `:703` gate on `dir.exists()` of the tracking directory rather than on
      the status file, so a deleted `norms-audit.md` in a source checkout fails
      those tests rather than skipping them; proved by deleting it in a scratch
      copy. This is the shape M107 established at `test-fixture-drift.R`.
- [ ] AC5: Running `grep -rn 'test_path("\.\."' tests/testthat/` classifies
      every line it returns into exactly one of: runs under `R CMD check`
      today; repaired by this milestone; development-only by construction,
      because the artifact it reads is absent from the tree a check-time test
      can reach -- its path is excluded by `.Rbuildignore`, or it is a file
      under `R/`, which an installed package does not carry; or never executed,
      because testthat does not collect it. The classification
      is committed in this file's `## Decisions` section, one row per returned
      line.
- [ ] AC6: `Rscript -e 'devtools::test()'` and
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
- [ ] T3: Probe both repaired guards inside a check run, deletion and insertion
      arms, and record which assertion each reddens.
- [x] T4: Add the vacuity fence to `test-axes-scaled-fit.R`'s AC12 test,
      matching the `expect_gt(nchar(rd), 1000L)` the other five dual-source
      reads of `axes_reliability.Rd` carry; plant an empty and a truncated read.
- [x] T5: Repair the two `norms-provenance.R` guards to `dir.exists()`; prove
      by scratch-copy deletion that a missing record reddens.
- [ ] T6: Run the profile verify slot and the check.

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
| `test-norms-provenance.R:583` | `cairn/references` | repaired here | now gates on `dir.exists()` of `cairn/references`, so a deleted `norms-audit.md` fails rather than skips |
| `test-norms-provenance.R:616` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:651` | `data-raw/audit-norms.R` | dev-only | `data-raw/` is `.Rbuildignore`d |
| `test-norms-provenance.R:711` | `cairn/references` | repaired here | now gates on `dir.exists()` of `cairn/references`, so a deleted `norms-audit.md` fails rather than skips |
| `test-rd-latex-safe.R:36` | `man` | runs under check | dual source: `man/` in the source tree, `tools::Rd_db("circumplex")` once installed |
| `test-ssm_occasions.R:446` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |
| `test-ssm_occasions.R:849` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |
| `test-ssm_occasions.R:881` | `devel` | dev-only | `devel/` is `.Rbuildignore`d |

## Review
