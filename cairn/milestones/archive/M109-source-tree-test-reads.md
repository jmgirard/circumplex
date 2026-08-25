# M109: Repair the test guards that skip on the surface that ships

**Status:** done (2026-08-24, PR #141 https://github.com/jmgirard/circumplex/pull/141)

**Goal:** Make the tests that assert shipped behaviour through a source-tree path run under `R CMD check`, and record the disposition of every source-tree read the suite contains.

**Outcome:** `tests/testthat/helper-vignette.R` adds `vignette_source(file)` — `vignettes/` first, then `system.file("doc", ...)`. The two guards that always skipped under check (`AC7: the vignette's caveats match the corrected contract`, `AC11: the vignette carries the same four claims`) call it with `skip_if(!nzchar(vig))`; a build installing no vignettes (covr's) still skips. The AC12 dual-source Rd read in `test-axes-scaled-fit.R` gained `expect_gt(nchar(rd), 1000L)`, matching its five siblings. Both `norms-audit.md` guards in `test-norms-provenance.R` gate on `dir.exists("cairn/references")` then assert `file.exists()` of the record, so a deleted record reddens instead of skipping (M107's shape). The ledger in the milestone file's `## Decisions` classifies all 49 lines the enumerating grep returns, in 25 files: 11 run under check, 5 repaired here, 22 development-only by `.Rbuildignore` or `R/`, 11 never collected.

**Decisions:** none cross-cutting. Milestone-local: the ledger, and the plan-gate choice to leave the no-counterpart reads in place.

**Review:** three fresh-context lenses ([O] diff, [S] blame-history, [S] prior-review); both [S] returned nothing. [O] returned nine — three fixed on the branch (a false fence comment, a half-true norms-guard comment, two ledger rows reading as "now runs under check"), six rejected with reason, including a covr claim refuted by reproducing covr's install-and-test path. No criterion failed.
