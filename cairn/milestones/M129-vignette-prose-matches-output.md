# M129: Vignette prose matches the package's code and printed output

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M127, M128
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — corrects prose and table layout in shipped vignettes
- **Branch/PR:** `m129-vignette-prose-matches-output`

## Goal

Four known wrong vignette claims are corrected, the SEM results tables fit on one line per estimate, and each sentence that quotes printed output matches the output after M127's print changes.

## Scope

**In:** the SEM latent results table layout, doc claims (ii) to (v) from the former doc-bug candidate row, the prose that reads M127's new accuracy summary and invariance ladder print, and the search for dashes in printed output. These came from the M128 plan before its re-cut. Every precomputed vignette is re-knitted.

**Out:** hiding simulation and table-building code, and the simulated datasets → M128. Print layouts themselves → M127. kable to gt → its candidate row. The three older vignettes' plain-English pass → its candidate row.

## Acceptance criteria

- [ ] AC1: In the article from `pkgdown::build_article("sem-based-ssm-analysis")`, viewed 1280 pixels wide, each SSM results table has each estimate and its interval on one line.
- [ ] AC2: Take each vignette whose `#>` output lines differ between 73218afa, before M127 and M128, and the branch head: the precomputed `.Rmd` files by their `#>` lines, and `bayesian-ssm-analysis` and `using-instruments` by the `#>` lines of their `pkgdown::build_article()` HTML built at both commits. In its `tools/prose-sweep.R --prose` output, each sentence that holds a number, a code span or a quoted label and names a value, column, row, line or label of a chunk's printed output names one that the re-knitted output shows.
- [ ] AC3: Four vignette claims are checked against the code and corrected where wrong. (ii) `sem-based-ssm-analysis.Rmd.orig` says the plane factors are "fixed isotropic and orthogonal", but `R/ssm_sem_syntax.R` fixes them only under the scaled tier. (iii) `evaluating-circumplex-structure.Rmd.orig` says the output "withholds" the OCPD displacement and its interval, but `print.circumplex_ssm` (`R/ssm_oop.R`) prints both with a not-interpretable note. (iv) It names a "weak" classification, but `R/fit_structure_oop.R` prints "not clearly supported" or "unsupported". (v) It says `cpm_fit()` "commits to" the theoretical angles, but its default quasi-circumplex model (`R/cpm_fit.R`) estimates them. Each line that `grep -n "withholds\|isotropic\|commits to\|weak"` returns over those two files either is corrected or states what the code does.
- [ ] AC4: The search `grep -nE -- '^#>.*(-- |--$|—)' vignettes/*.Rmd` returns only lines that also match `^#>.*[0-9.]\s+--\s*$`, the missing-value cells of the axes-reliability tables. The same search over the `#>` lines of the text extracted, with entities decoded, from the `bayesian-ssm-analysis` and `using-instruments` HTML built by `pkgdown::build_article()` returns no line.
- [ ] AC5: `tools/prose-sweep.R` exits 0 on each touched source. `tools/check-vignette-staleness.R` passes after `tools/precompute-vignettes.R`, and the `vignette-precompute` CI job passes on the PR. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T1, T4
- AC3 → T3
- AC4 → T4
- AC5 → T1, T5

## Tasks

- [x] T1: Cut the branch after M127 and M128 merge. Build the `bayesian-ssm-analysis` and `using-instruments` articles with pkgdown at 73218afa as AC2's baseline. Run `devtools::install()` so the knit reads the new package (lesson M21 family).
- [x] T2: Make the SEM latent table text smaller with non-breaking spaces or an inline style in its hidden `kable()` chunk, and add `drop_xy = TRUE` if that fits the prose. Build the article with pkgdown and view it at 1280 px.
- [x] T3: Fix doc claims (ii) to (v), checking each against its `R/` file, and re-read each corrected sentence against the code (plain-vignettes rule 9). Keep the phrases that `tests/testthat/test-cpm_boundary_vignette.R` matches.
- [x] T4: Re-knit every precomputed vignette and build the two knit-at-build articles. List each sentence AC2 selects with the output line it names, in the work log. Update the prose that no longer matches, and run AC4's search.
- [ ] T5: Run `tools/prose-sweep.R`, `tools/check-vignette-staleness.R`, `devtools::test()` and `devtools::check(args = "--no-manual")`. Add a NEWS.md documentation entry.

## Work log

- 2026-09-14: created by /milestone-plan as the remainder of the M128 re-cut: the SEM table layout, doc claims (ii) to (v), output-dash search and M127 prose from the earlier M128 plan. The earlier gate's choices carry over: kable with a hidden render chunk over gt, and the doc claims fixed here over /hotfix.
- 2026-09-14: criteria audit (full mode, fresh [O] reader, shared with the M128 re-cut) found 4 items on this draft, all fixed: phrase anchors in place of line numbers (AC3), numbers and a recorded branch-cut SHA (AC2), knit-at-build HTML and an exact exemption pattern (AC4), and "SSM results table" with a narrower goal (AC1).
- 2026-09-14: second fresh [O] audit found 4 items on this file, all fixed. AC2's baseline is 73218afa, so that vignettes M127 re-knits stay in scope, and knit-at-build HTML is built at both commits (T1). AC3 settles each grep hit. AC4 reads decoded HTML text.
- 2026-09-15: implement started, branch cut from master at 714d5c78. T1: pkgdown built the baseline `bayesian-ssm-analysis` and `using-instruments` in a 73218afa worktree against a 73218afa scratch-library install, saved outside the repo. The branch package is installed with `R CMD INSTALL`.
- 2026-09-15: question gate: the SEM latent table uses `drop_xy = TRUE` with non-breaking spaces (maintainer choice over keeping X/Y with sideways scroll or smaller text).
- 2026-09-15: T3 fixed four claims against the code. (ii) The plane constraint is scoped to the scaled tier, and the strict tier frees the factor covariance (`R/ssm_sem_syntax.R`). (iii) Three sentences say print shows the displacement with a not-interpretable note (`R/ssm_oop.R`). (iv) "weak" became "not clearly supported" (print) and "unsupported" (summary), per `R/fit_structure_oop.R`. (v) The `cpm_fit()` default estimates angles, and constrained-angles fixes them (`R/cpm_fit.R`). Grep hits kept as true: sem 448 (`sem_dcfi_flag()` gives no verdict out of scope), evaluating 180 (`R/cpm_fit.R` Hessian marker text), 238 and 405 (simulation and literature findings), 573 (amplitude bias). prose-sweep exits 0 on both sources. The T2 chunk edit rides in this commit, not yet verified.
- 2026-09-15: T2: the SEM latent table uses `drop_xy = TRUE` and non-breaking spaces, and the shown call matches. The pkgdown article, served locally at 1280 px with site CSS, gives 1 line per cell (table 776 px in an 800 px column). A control with the old 7-column spaced markup in the same column wrapped 5 cells to 2 lines.
- 2026-09-15: T4 and T5 in progress. `tools/check-vignette-staleness.R` exits 0 on all 7 pre-computed vignettes. AC4 `.Rmd` search returns only the 3 axes-reliability missing-value lines, and the HTML search over both knit-at-build articles (35 and 275 `#>` lines) returns none. Neither knit-at-build article's `#>` lines differ from 73218afa, so AC2 covers the 5 changed pre-computed vignettes. A NEWS.md documentation entry is drafted, and `ssm_sem()` defaults to `model = "scaled"` (`R/ssm_sem.R`).
- 2026-09-15: T4 ledger, delegated to a fresh [O] reader and checked by the session against output and code. 101 sentences were selected (advanced-visualization 6, axes-reliability 15, evaluating 52, growth 13, sem 15). 7 did not match and were fixed, 6 in evaluating and 1 in axes-reliability. They were the removed guardrail table (now the `cert` column and `Guardrail` lines), `VT2` (prints `Variance`), `inadequate` (prints `INADEQUATE`), the amplitude miss direction, "about the benchmark rate" (1.0% vs 2.5%), the verdict block labels, and axes `NA` (prints `--`). 3 doubts needed no change: the short ladder (prose names the argument), `block_specificity` symbol (returned object, no chunk), Wilson interval (named in the printed header).
- 2026-09-15: T5 tests. `devtools::test()` gives FAIL 0, WARN 9, SKIP 1, PASS 9512. The warnings come from `test-ci_accuracy.R`, `test-pole-values.R` and `test-ssm_sem.R`, which read no vignette, and the branch leaves `R/` and `tests/` identical to master. `test-cpm_boundary_vignette.R` with `NOT_CRAN=true` passes 43 expectations after the T4 edits. The staleness check exits 0 on all 7 vignettes after the T4 commit.

## Decisions
