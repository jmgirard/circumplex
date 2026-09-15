# M129: Vignette prose matches the package's code and printed output

- **Status:** planned
- **Priority:** normal
- **Depends on:** M127, M128
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — corrects prose and table layout in shipped vignettes
- **Branch/PR:** —

## Goal

Four known wrong vignette claims are corrected, the SEM results tables fit on one line per estimate, and each sentence that quotes printed output matches the output after M127's print changes.

## Scope

**In:** the SEM latent results table layout, doc claims (ii) to (v) from the former doc-bug candidate row, the prose that reads M127's new accuracy summary and invariance ladder print, and the search for dashes in printed output. These came from the M128 plan before its re-cut. Every precomputed vignette is re-knitted.

**Out:** hiding simulation and table-building code, and the simulated datasets → M128. Print layouts themselves → M127. kable to gt → its candidate row. The three older vignettes' plain-English pass → its candidate row.

## Acceptance criteria

- [ ] AC1: In the article from `pkgdown::build_article("sem-based-ssm-analysis")`, viewed 1280 pixels wide, each SSM results table has each estimate and its interval on one line.
- [ ] AC2: Take each vignette whose rendered output differs from master at the branch cut (SHA recorded in T1): the precomputed `.Rmd` files by `git diff --stat <branch-cut> -- vignettes/*.Rmd`, and `bayesian-ssm-analysis` and `using-instruments` by their `pkgdown::build_article()` HTML. In its `tools/prose-sweep.R --prose` output, each sentence that holds a number, a code span or a quoted label and names a value, column, row, line or label of a chunk's printed output names one that the re-knitted output shows.
- [ ] AC3: Four vignette claims are checked against the code and corrected where wrong. (ii) `sem-based-ssm-analysis.Rmd.orig` says the plane factors are "fixed isotropic and orthogonal", but `R/ssm_sem_syntax.R` fixes them only under the scaled tier. (iii) `evaluating-circumplex-structure.Rmd.orig` says the output "withholds" the OCPD displacement and its interval, but `print.circumplex_ssm` (`R/ssm_oop.R`) prints both with a not-interpretable note. (iv) It names a "weak" classification, but `R/fit_structure_oop.R` prints "not clearly supported" or "unsupported". (v) It says `cpm_fit()` "commits to" the theoretical angles, but its default quasi-circumplex model (`R/cpm_fit.R`) estimates them. Every occurrence found by `grep -n "withholds\|isotropic\|commits to\|weak"` over those two files is checked the same way.
- [ ] AC4: The search `grep -nE -- '^#>.*(-- |--$|—)' vignettes/*.Rmd` returns only lines that also match `^#>.*[0-9.]\s+--\s*$`, the missing-value cells of the axes-reliability tables. The same search over the `#>` lines of the `bayesian-ssm-analysis` and `using-instruments` HTML from `pkgdown::build_article()` returns no line.
- [ ] AC5: `tools/prose-sweep.R` exits 0 on each touched source. `tools/check-vignette-staleness.R` passes after `tools/precompute-vignettes.R`, and the `vignette-precompute` CI job passes on the PR. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T4
- AC3 → T3
- AC4 → T4
- AC5 → T1, T5

## Tasks

- [ ] T1: Cut the branch after M127 and M128 merge, and record the branch-cut SHA in the work log. Run `devtools::install()` so the knit reads the new package (lesson M21 family).
- [ ] T2: Make the SEM latent table text smaller with non-breaking spaces or an inline style in its hidden `kable()` chunk, and add `drop_xy = TRUE` if that fits the prose. Build the article with pkgdown and view it at 1280 px.
- [ ] T3: Fix doc claims (ii) to (v), checking each against its `R/` file, and re-read each corrected sentence against the code (plain-vignettes rule 9). Keep the phrases that `tests/testthat/test-cpm_boundary_vignette.R` matches.
- [ ] T4: Re-knit every precomputed vignette and build the two knit-at-build articles. List each sentence AC2 selects with the output line it names, in the work log. Update the prose that no longer matches, and run AC4's search.
- [ ] T5: Run `tools/prose-sweep.R`, `tools/check-vignette-staleness.R`, `devtools::test()` and `devtools::check(args = "--no-manual")`. Add a NEWS.md documentation entry.

## Work log

- 2026-09-14: created by /milestone-plan as the remainder of the M128 re-cut: the SEM table layout, doc claims (ii) to (v), output-dash search and M127 prose from the earlier M128 plan. The earlier gate's choices carry over: kable with a hidden render chunk over gt, and the doc claims fixed here over /hotfix.
- 2026-09-14: criteria audit (full mode, fresh [O] reader, shared with the M128 re-cut) found 4 items on this draft, all fixed: phrase anchors in place of line numbers (AC3), numbers and a recorded branch-cut SHA (AC2), knit-at-build HTML and an exact exemption pattern (AC4), and "SSM results table" with a narrower goal (AC1).

## Decisions
