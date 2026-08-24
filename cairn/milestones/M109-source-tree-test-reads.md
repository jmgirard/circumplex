# M109: Repair the test guards that skip on the surface that ships

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Make the tests that assert shipped behaviour through a source-tree path run
under `R CMD check`, and record the disposition of every source-tree read the
suite contains.

## Scope

Surface tier: **internal** — the deliverable is test-suite code, which no
external consumer of the package relies on.

Running the enumerating grep over `tests/testthat/` returns 50 lines in 25
files. Twelve already resolve under check through a dual-source fallback;
38 read artifacts `.Rbuildignore` excludes, so no installed counterpart exists
for them by construction. Exactly two assert shipped behaviour and always skip.

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
- [ ] AC3: The dual-source Rd read at
      `tests/testthat/test-axes-scaled-fit.R:1049` carries a vacuity fence that
      fails both when its source yields empty text and when it yields a
      truncated read, proved by both plants. It is the only one of the four
      dual-source Rd reads without such a fence.
- [ ] AC4: The two guards at `tests/testthat/test-norms-provenance.R:581` and
      `:703` gate on `dir.exists()` of the tracking directory rather than on
      the status file, so a deleted `norms-audit.md` in a source checkout fails
      those tests rather than skipping them; proved by deleting it in a scratch
      copy. This is the shape M107 established at `test-fixture-drift.R`.
- [ ] AC5: Running `grep -rn 'test_path("\.\."' tests/testthat/` classifies
      every line it returns into exactly one of: runs under `R CMD check`
      today; repaired by this milestone; development-only by construction,
      because the artifact it reads is excluded from the built package; or
      never executed, because testthat does not collect it. The classification
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

- [ ] T1: Run the enumerating grep, classify every returned line into the four
      classes, and commit the table to this file's `## Decisions` section.
- [ ] T2: Repair the two vignette guards, following the `vignette_path()`
      helper at `tests/testthat/test-cpm_boundary_vignette.R:7-25` — candidate
      vector, `nzchar() & file.exists()` filter, `skip_if` on the empty case.
- [ ] T3: Probe both repaired guards inside a check run, deletion and insertion
      arms, and record which assertion each reddens.
- [ ] T4: Add the vacuity fence at `test-axes-scaled-fit.R:1049`, matching the
      `expect_gt(nchar(rd), 1000L)` its three siblings carry; plant an empty
      and a truncated read.
- [ ] T5: Repair the two `norms-provenance.R` guards to `dir.exists()`; prove
      by scratch-copy deletion that a missing record reddens.
- [ ] T6: Run the profile verify slot and the check.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose leaving the 38 no-counterpart reads in place over relocating the norms-audit checkers to a developer script, because they cannot run under a package check by construction and most already say so in a comment; falsified by evidence a maintainer path reaches them expecting check coverage.
- 2026-08-24: criteria audit ran in FULL mode ([O], fresh context, authored none of them) over M109 and M110 together — M110's user-facing tier mandates full, and M109 was audited at the same bar though its internal tier and tripwire-free criteria would have allowed the reduced mode. Fixed here before writing: the classification trichotomy could not classify the 11 never-collected `_problems/` lines nor the dev-only builder-script reads, so a fourth class was added and the runtime-half clause dropped; and the mutation probe was deletion-only, which provably cannot exercise the five `expect_no_match` assertions in the AC7 test, so an insertion arm was added.

## Decisions

## Review
