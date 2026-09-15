# M128: Vignettes show the package calls they teach, not the plumbing around them

- **Status:** planned
- **Priority:** normal
- **Depends on:** M126
- **Driving RR:** —
- **Principles touched:** GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — changes the code shown in nine shipped vignettes and adds three exported datasets
- **Branch/PR:** —

## Goal

A vignette reader sees the circumplex calls and model-fitting calls being taught, while the code that simulates data, assembles or formats display tables, or checks internals is hidden, and simulated data is loaded from the package by name.

## Scope

**In:** the nine vignette sources: every `vignettes/*.Rmd.orig`, plus `bayesian-ssm-analysis.Rmd` and `using-instruments.Rmd`, which have no `.orig`. The chunks AC1's search flags at 73218afa: structure `variants` and `ipsatize`; growth `simulate`, `stack`, `draws`, `lowamp` and `lowamp-plot`; SEM `latent-table`; Bayesian `known-direction`, `data-prep`, `load-draws` and `induced-prior`; the using-instruments norm-sample count chunk; advanced-visualization `coord-built`, `occasions-data`, `individuals` and `curve-axis`. A flagged step is hidden, with no exceptions, and the prose says what it does. Three datasets from `data-raw/` under D-058: `simulated_occasions` from one script, and `simulated_growth` and `simulated_growth_origin` from one script that shares their person effects. A table made from `ssm_table()` shows the call as code and hides the `kable()` formatting. The growth vignette shows one `ssm_draws()` call for a single wave, hides the loop that builds the trajectory table, and shows the printed table and the plots. NEWS.md.

**Out:** SEM table width, doc claims (ii) to (v), the search for dashes in output, and the prose that reads M127's new prints → M129. An exported draws-to-trajectory helper → its candidate row. kable to gt → its candidate row. `vignettes/bayesian_ssm_draws.rds` stays a file, because it holds model output rather than simulated data. Only the chunk that reads it is hidden.

## Acceptance criteria

- [ ] AC1: An echoed chunk is a fenced code block in a source whose header has no `echo = FALSE`, `echo = F`, `include = FALSE` or `#| echo: false`, in any spacing. `eval = FALSE` chunks and plain `r` fences count as echoed. The sources are every `vignettes/*.Rmd.orig` and every `vignettes/*.Rmd` with no `.Rmd.orig` of the same name. `tools/vignette-echo-sweep.R` prints each line of an echoed chunk that matches `lapply\(|sapply\(|vapply\(|mapply\(|Map\(|do\.call\(|Filter\(|Reduce\(|stopifnot\(|kable|(^|[^A-Za-z0-9_.])r(norm|unif|binom|pois|exp|gamma|beta|chisq|t)\(|sample\(|mvrnorm|function\(|\\\(|for \(|rbind\(|reshape\(|subset\(|data\.frame\(|readRDS\(`. On the branch head it prints no line and exits 0.
- [ ] AC2: Take each name of a function exported by circumplex (`getNamespaceExports("circumplex")`), and each `glmmTMB::` or `brms::` function name, that is called in the echoed chunks of a source at the branch-cut commit. Each such name is also called in the echoed chunks of the same source on the branch head.
- [ ] AC3: `data/` holds `simulated_occasions`, `simulated_growth` and `simulated_growth_origin`. `data-raw/simulated_occasions.R` writes the first. `data-raw/simulated_growth.R` writes the other two from one set of person effects. Each script sets its seed, and running it again writes objects `identical()` to the shipped ones. `simulated_occasions` has 200 persons at waves T1, T2 and T3, with `wave` a factor in that level order and group displacements 330, 355 and 20 degrees. Both growth datasets have 150 persons at waves 0 to 4 as person-by-wave octant scores. In `simulated_growth` the group displacement moves from 350 to 10 degrees at amplitude 0.6. In `simulated_growth_origin` the group x moves from 0.5 to -0.5 with y at 0.02. Each dataset has a help page that says the data are simulated, states these parameters and links its script on GitHub. Each has a `_pkgdown.yml` reference row, and NEWS.md announces the three datasets.
- [ ] AC4: `advanced-visualization.Rmd.orig` and `growth-ssm-analysis.Rmd.orig` load their simulated data with `data()` on AC3's datasets. Each call found by `grep -nE 'ssm_analyze_long\(|ssm_parameters_id\(|glmmTMB::glmmTMB\('` over the two files takes data built from those datasets. Each `ssm_parameters_id()` call on growth data returns one row per person-wave, as many rows as its input dataset has. `grep -nE '(^|[^A-Za-z0-9_.])r(norm|unif|binom)\('` over the two files returns lines only inside the one hidden growth chunk that draws coefficient samples from a fitted model.
- [ ] AC5: Take each chunk with `echo = FALSE` in which `kable()` formats `ssm_table(X, ...)`. A code block placed before that chunk shows `ssm_table(X)`, either as an `eval = FALSE` chunk or as a plain `r` fence.
- [ ] AC6: Each rendered precomputed `.Rmd` has as many `<img src="figures/` lines as at the branch cut. For `bayesian-ssm-analysis` and `using-instruments`, the HTML from `pkgdown::build_article()` has as many `<img` tags as at the branch cut. These printed outputs are still shown, with the same column or row labels: the structure `variants` fit table and `ipsatize` comparison table, the SEM latent table, the occasions results table, the growth trajectory table, the growth `mid` print, and the using-instruments norm-sample counts. In the re-knitted growth output, every wave of the first trajectory is certified, and wave 2 of the origin trajectory is not.
- [ ] AC7: In the `tools/prose-sweep.R --prose` output of `advanced-visualization.Rmd.orig` and `growth-ssm-analysis.Rmd.orig`, each sentence about the data, a printed output or a figure is true of the re-knitted output. For each other source whose rendered output differs from the branch cut, each sentence that holds a number, a code span or a quoted label and names a printed value, column or label names one the re-knitted output shows. Each name that a hidden chunk assigns or attaches with `library()`, and that a later echoed chunk uses (`all.names()` of its parsed code), is named in the prose before that echoed chunk.
- [ ] AC8: `tools/prose-sweep.R` exits 0 on each touched source. `tools/check-vignette-staleness.R` passes after `tools/precompute-vignettes.R`, and the `vignette-precompute` CI job passes on the PR. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2, T4, T5, T6
- AC2 → T2, T4, T5, T6
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6
- AC6 → T7
- AC7 → T2, T7
- AC8 → T1, T8

## Tasks

- [ ] T1: Cut the branch from master after M126 merges, and record the branch-cut SHA in the work log. Run `devtools::install()` so the knit reads the branch's package (lesson M21 family).
- [ ] T2: Write `tools/vignette-echo-sweep.R` with AC1's search, a mode that lists AC2's call names per source, and a mode that lists AC7's hidden-chunk names used by echoed chunks. Give it fixture sources under `tools/` with one chunk per hidden-marker form and per echoed form (plain `r` fence, tilde fence, `eval = FALSE`), and one line per search alternative. Assert its exact output on them. Run it on the branch-cut sources, and save the AC2 call list for review.
- [ ] T3: Write the two `data-raw/` scripts from the current simulation code, the help pages beside `simulated_items` in `R/example_data.R`, the `_pkgdown.yml` rows and the NEWS.md entry. Rerun each script and compare with `identical()`. Run `devtools::document()` and revert any `Config/roxygen2/version` change (lesson M85).
- [ ] T4: advanced-visualization: load `simulated_occasions`, and hide the `subset()` table code, the `individuals` filter and the `curve-axis` data frame behind prose. Move `library(ggplot2)` into an echoed chunk if echoed code uses it.
- [ ] T5: growth Sections 2 to 5: load both datasets, show `ssm_parameters_id()` on the octant scores, hide the reshape and the draws loop, and show one `ssm_draws()` call for one wave. Describe each hidden step and every name it defines (lesson M50).
- [ ] T6: structure `variants` and `ipsatize`, SEM `latent-table`, the four Bayesian chunks and the using-instruments count chunk: keep the package calls shown, hide table assembly, checks and data building, and add the `ssm_table()` code blocks that AC5 needs.
- [ ] T7: Re-knit every precomputed vignette with `tools/precompute-vignettes.R`, and build the two knit-at-build articles with pkgdown. Count figures, and check the named outputs and the certification verdicts. Read the prose of the sources AC7 names against the new output, fix what no longer matches, and record one work-log line per vignette.
- [ ] T8: Run `tools/prose-sweep.R`, `tools/check-vignette-staleness.R`, `devtools::test()` and `devtools::check(args = "--no-manual")`. Add a NEWS.md documentation entry for the vignette changes.

## Work log

- 2026-09-14: created by /milestone-plan. The plan gate kept kable, with a hidden render chunk and no-break cells, over switching to gt now, because gt needs a dependency gate. Falsified by a no-break kable table that still wraps or looks wrong in the pkgdown article.
- 2026-09-14: the plan gate simplified the growth vignette without new API over adding an exported draws-to-trajectory helper, because a helper is an irreversible API decision. Falsified by a rewrite that cannot reach AC3's half-length bar without one.
- 2026-09-14: the plan gate absorbed doc bugs (ii) to (v) of the doc-bug candidate row instead of routing them to /hotfix, because this milestone rewrites both pages.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 7 items on this file's draft. All were fixed before writing: AC1 bounds the whole chunk, AC2 names a pkgdown build and viewport, AC3 and AC7 name the Bayesian `.Rmd`, AC4 is enumerated by git diff, AC5 lists sites and code referents, and AC6 exempts the placeholder and depends on M127.
- 2026-09-14: second fresh [O] audit of the written criteria found 5 items, all fixed. AC3 was unreachable for bayesian alone (at least 22 of 31 lines stay), so it now counts both vignettes together, gives bayesian a per-chunk bar and defines an echoed line. AC4 names the `--prose` output as its list. AC5 states the four claims, adds "weak" to its grep and corrects only wrong uses. AC6 matches a dash at a wrapped line end.
- 2026-09-14: re-cut by /milestone-plan on maintainer feedback. Vignettes must hide data simulation, table assembly and check code, and load simulated data by name. The earlier criteria, which kept that code shown but shorter, are superseded. Their doc claims, SEM table width, output dash search and M127 prose move to M129. Re-cut count on this milestone: 1, with no review returns.
- 2026-09-14: plan gate chose package datasets over `.rds` files in `vignettes/`, because readers can then load the data by name (D-058). Falsified by a CRAN size note from `data/` or a request to remove the datasets.
- 2026-09-14: plan gate chose to hide every flagged chunk, the Bayesian prior-predictive simulation included, over keeping named teaching exceptions. Falsified by a reader who cannot follow a hidden step from the prose.
- 2026-09-14: plan gate chose one shown `ssm_draws()` call with the loop hidden over hiding the whole step or exporting a helper. Falsified by a reader who cannot rebuild the trajectory table from the shown call and the prose.
- 2026-09-14: the plan showed the `ssm_table()` call with `kable()` hidden, over hiding the whole table code, without posing it (question cap), because the earlier gate chose that form for the SEM table. Falsified by a request to hide the calls too.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 17 items across the M128 and M129 drafts. Twelve were fixed before writing: the call-kept check (AC2), `ssm_parameters_id()` averaging across waves (AC4), shared growth person effects and certification outcomes (AC3, AC6), hidden-marker forms moved to T2 fixtures, the `ssm_table()` wording (AC5), branch-cut baselines, named outputs with no review escape (AC6), a procedure for hidden names (AC7), phrase anchors, number coverage and exact exemptions in M129, and small items (GitHub link, factor waves, grep patterns). Four became gate questions, and the fifth was settled by the earlier SEM choice.
- 2026-09-14: checkpoint: criteria written, and a second fresh [O] audit of the written wording is running. The plan is not final until its findings are disposed of.

## Decisions
