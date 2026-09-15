# M128: Vignette code is short and reads on one pass

- **Status:** planned
- **Priority:** normal
- **Depends on:** M126, M127
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — rewrites code and prose in five shipped vignettes
- **Branch/PR:** —

## Goal

The code chunks the maintainer named in the structure, SEM, Bayesian and growth vignettes are short enough to follow on one read, and each vignette's prose matches its re-knitted output.

## Scope

**In:** evaluating-circumplex-structure: the `variants` chunk and the prose that reads the new M127 accuracy summary. sem-based-ssm-analysis: the `latent-table` chunk becomes a shown `ssm_table(latent)` call (`eval = FALSE`) plus a hidden kable chunk. Its table text is smaller, and no break falls inside an estimate and its interval. Also the prose that reads the new ladder print. bayesian-ssm-analysis Sections 2 and 3. growth-ssm-analysis Sections 2 to 5: the simulation moves to a hidden chunk that the prose describes, and one short visible helper replaces the two copied draws-to-table blocks. Doc bugs (ii) to (v) come from the former doc-bug candidate row. Every precomputed vignette whose output M126 or M127 changed is re-knitted.

**Out:** figures and axis placement → M126. Print layouts → M127. A switch from kable to gt stays on its candidate row, with no new dependency. An exported draws-to-trajectory helper → new candidate row. The three older vignettes (introduction, intermediate, using-instruments) stay on the plain-English follow-on row.

## Acceptance criteria

- [ ] AC1: The `variants` chunk in `evaluating-circumplex-structure.Rmd.orig` is at most 10 lines with no `vapply()`. It prints the model name, df, RMSEA, SRMR, CFI and TLI of the three variants, each index rounded to 3 decimals.
- [ ] AC2: In sem-based-ssm-analysis, the reader sees `ssm_table(latent)` as unevaluated code and does not see a `knitr::kable()` call. In the article from `pkgdown::build_article("sem-based-ssm-analysis")`, viewed 1280 pixels wide, each table's estimate and its interval sit on one line.
- [ ] AC3: Counted from each file's source at `845fb5e7` (`bayesian-ssm-analysis.Rmd`, `growth-ssm-analysis.Rmd.orig`), the echoed code lines in bayesian Sections 2 and 3 and in growth Sections 2 to 5 are at most half as many as before. Each figure and each printed estimate table those sections showed is still shown.
- [ ] AC4: In each vignette whose rendered `.Rmd` output differs from `845fb5e7` (enumerated by `git diff --stat 845fb5e7 -- vignettes/*.Rmd`), every sentence that names a column, row, line or label of a chunk's printed output names one that appears in that chunk's re-knitted output.
- [ ] AC5: The vignette claims of doc bugs (ii) to (v) are corrected against the code: `sem-based-ssm-analysis.Rmd.orig:428` against `R/ssm_sem_syntax.R`, `evaluating-circumplex-structure.Rmd.orig:510` and `:564` against `R/ssm_oop.R`, `:707` against `R/fit_structure_oop.R`, and `:715` against `R/cpm_fit.R`. Every other occurrence found by `grep -n "withholds\|isotropic\|commits to"` over those two files is corrected too.
- [ ] AC6: The search `grep -nE -- '^#>.*(-- |—)' vignettes/*.Rmd` returns no hit other than the axes-reliability missing-value placeholder.
- [ ] AC7: `tools/prose-sweep.R` exits 0 on each touched `.Rmd.orig` and on `bayesian-ssm-analysis.Rmd`. `tools/check-vignette-staleness.R` passes after `tools/precompute-vignettes.R`, and the `vignette-precompute` CI job passes on the PR. `devtools::check(args = "--no-manual")` reports no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4, T5
- AC4 → T6, T7
- AC5 → T6
- AC6 → T7
- AC7 → T1, T8

## Tasks

- [ ] T1: Cut the branch after M126 and M127 merge. Run `devtools::install()` so the knit reads the new package (lesson M21 family).
- [ ] T2: Rewrite `variants` (`evaluating-circumplex-structure.Rmd.orig:307`) with a `models` vector, one `lapply()` over `cpm_fit()`, and a table built from `f$fit[c("df", "rmsea", "srmr", "cfi", "tli")]`. Keep the volatile-numbers comment around it (lesson M120).
- [ ] T3: Split `latent-table` (`sem-based-ssm-analysis.Rmd.orig:186`) into a shown `eval = FALSE` chunk and a hidden `echo = FALSE` chunk. Make the table smaller with non-breaking spaces or an inline style, and add `drop_xy = TRUE` if that fits the prose. Build the article with pkgdown and view it at 1280 px.
- [ ] T4: Bayesian Sections 2 and 3: cut the `stopifnot()` checks from `known-direction` and build `dat` in `data-prep` in fewer lines. Keep the lines the prose explains.
- [ ] T5: Growth Sections 2 to 5: move `simulate`, and the `lowamp` simulation, into hidden chunks with a prose sentence each. Every name a later echoed chunk uses must be described (lesson M50). Write one visible helper that turns coefficient draws at given waves into a trajectory table, and use it for both trajectories. Count echoed lines before and after.
- [ ] T6: Fix doc bugs (ii) to (v), checking each claim against its `R/` file, and give each corrected sentence a fresh re-read against the code (plain-vignettes rule 9). Keep the phrases that `tests/testthat/test-cpm_boundary_vignette.R` matches.
- [ ] T7: Re-knit every precomputed vignette. List each sentence that names printed output in the vignettes AC4 enumerates, with the output line it names, in the work log. Update the prose that no longer matches, and run AC6's search.
- [ ] T8: Run `tools/prose-sweep.R` on the touched files, then `tools/check-vignette-staleness.R` and `devtools::check(args = "--no-manual")`. Add a NEWS.md documentation entry.

## Work log

- 2026-09-14: created by /milestone-plan. The plan gate kept kable, with a hidden render chunk and no-break cells, over switching to gt now, because gt needs a dependency gate. Falsified by a no-break kable table that still wraps or looks wrong in the pkgdown article.
- 2026-09-14: the plan gate simplified the growth vignette without new API over adding an exported draws-to-trajectory helper, because a helper is an irreversible API decision. Falsified by a rewrite that cannot reach AC3's half-length bar without one.
- 2026-09-14: the plan gate absorbed doc bugs (ii) to (v) of the doc-bug candidate row instead of routing them to /hotfix, because this milestone rewrites both pages.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 7 items on this file's draft. All were fixed before writing: AC1 bounds the whole chunk, AC2 names a pkgdown build and viewport, AC3 and AC7 name the Bayesian `.Rmd`, AC4 is enumerated by git diff, AC5 lists sites and code referents, and AC6 exempts the placeholder and depends on M127.

## Decisions
