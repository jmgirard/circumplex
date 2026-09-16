# M135: Vignettes are labeled by level and framed like the older ones

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes and the pkgdown site
- **Branch/PR:** m135-vignette-levels-and-frame

## Goal

Every vignette states its level and opens and closes the way the three older vignettes do, and the website groups the vignettes by level in reading order.

## Scope

**In:** The frame the older vignettes use: a Level line under the setup chunk, a numbered `## 1. Overview` that names the sections and functions the page covers, numbered sections after it, a `## Wrap-up` that says what the reader learned and names the next page, then References. All nine vignettes get the frame, with their chunks and output unchanged. `_pkgdown.yml` gets an `articles:` index grouped by level and a navbar menu in the same order, with `axes-reliability` added (it is missing today). NEWS.md gets one entry.

The level map. Introductory: Using Circumplex Instruments (start), Introduction to SSM Analysis. Intermediate: Intermediate SSM Analysis, Evaluating Circumplex Structure. Advanced: Advanced Circumplex Visualization, SEM-Based SSM Analysis, Axes Reliability, Bayesian SSM Analysis, Growth Models on SSM Parameters. Reading order, as three chains: Instruments → Introduction → Intermediate → Evaluating Structure → Visualization. Evaluating Structure → SEM → Axes Reliability. Intermediate → Bayesian → Growth. Terminal pages (no page follows): Visualization, Axes Reliability, Growth. M136 and M137 place their new pages inside this map.

**Out:** Splitting the structure vignette → M136. Splitting the axes and SEM vignettes → M137. The plain-English pass over the three older vignettes → the candidate row "Plain-English follow-ons (i)". Moving teaching content to an ebook → the candidate row "Didactic vignettes → ebook". Any change to a chunk, its options or its output.

## Acceptance criteria

- [x] AC1: In each of the nine vignette source files (the `.Rmd.orig` where one exists, else the `.Rmd`), the first prose line after the setup chunk has the form `**Level:** <Introductory|Intermediate|Advanced>. <sentence>`. The sentence names each page to read first by its title, or says that none is needed. Procedure: a script greps `^\*\*Level:\*\*` in the nine files, requires one hit per file, checks the level word against the map in Scope, and checks each quoted title against the `VignetteIndexEntry` titles in `vignettes/`.
- [x] AC2: In each of the nine source files, `grep '^## '` prints, in order, `## 1. Overview`, then `## 2.` to `## N.` with consecutive numbers, then `## Wrap-up`, then `## References` where the page has one. Procedure: the same script checks the heading sequence and checks that the text of every heading after Overview appears in the Overview section's body.
- [x] AC3: Each Wrap-up section names at least one page to read next by its title, and each named title is a `VignetteIndexEntry` title. Each page in the reading order in Scope is named by the Wrap-up of the page before it. Procedure: the script reads the Wrap-up sections and checks both directions against the reading order.
- [x] AC4: `_pkgdown.yml` has an `articles:` section with one group per level, every vignette in `vignettes/` listed exactly once across the groups in the map's order, and a navbar Vignettes menu with the same entries in the same order. Procedure: a script reads the two lists from the YAML and the vignette titles from `vignettes/` and diffs them. `pkgdown::check_pkgdown()` runs clean as a second check.
- [x] AC5: For each of the seven pre-computed vignettes, the `tools/prose-sweep.R --chunks` output at the branch head is identical to the output at the merge base, and `tools/check-vignette-staleness.R` passes at the branch head.
- [x] AC6: For each of the nine source files, the finding list of `tools/prose-sweep.R` at the branch head is a subset of its finding list at the merge base. The SEM page and the three older pages have findings at master, so the frame adds none and removes any it touches.
- [x] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note that the merge base does not report.

## Coverage

- AC1 → T2, T3, T4, T6
- AC2 → T2, T3, T4, T6
- AC3 → T2, T3, T4, T6
- AC4 → T1, T6
- AC5 → T5, T6
- AC6 → T2, T3, T4, T6
- AC7 → T7

## Tasks

- [x] T1: Add the `articles:` index and regroup the navbar menu in `_pkgdown.yml` by the level map, adding `axes-reliability`. Write the AC4 diff script under `tools/` or as a testthat test that skips where `vignettes/` is absent (see `tests/testthat/test-cpm_boundary_vignette.R` for the installed-tree fallback).
- [x] T2: Frame the three older vignettes: `using-instruments.Rmd`, `introduction-to-ssm-analysis.Rmd.orig`, `intermediate-ssm-analysis.Rmd.orig`. Their sections are already numbered. Add the Level line, rename the first section to `## 1. Overview` where the content fits or add one, and name the next page in each Wrap-up.
- [x] T3: Frame `evaluating-circumplex-structure`, `sem-based-ssm-analysis`, `growth-ssm-analysis`, `axes-reliability` (numbered already, no Overview). Add the Level line and the Overview, renumber, and add the Wrap-up where missing (growth has none).
- [x] T4: Frame `advanced-visualization.Rmd.orig` (unnumbered headings) and `bayesian-ssm-analysis.Rmd` (its section 6 "Where to go next" becomes the Wrap-up). Write the AC1 to AC3 check script in the same place as T1's and run it over all nine.
- [x] T5: Re-render the seven pre-computed pages with `tools/precompute-vignettes.R` (the package must be installed first), and run the staleness and width guards.
- [x] T6: Run the AC1 to AC4 scripts, the `--chunks` comparison and the sweep-subset comparison against the merge base, and record the results in the work log. Add the NEWS.md entry.
- [x] T7: Run tests and the check, and compare the notes with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose three levels with a Level line, Overview and Wrap-up on every page over labels in the site index only, because the older vignettes carry their frame in the page and the maintainer asked to emulate them; falsified by readers who report the frame as noise.
- 2026-09-16: criteria audit ran in full mode on a fresh Opus reader; 16 findings. Fixed at the gate: the sweep already fails on the SEM page (AC6 now a subset rule), `check_pkgdown()` validates no navbar (AC4 names a diff script), the Level, Overview and Wrap-up checks are scripted (AC1 to AC3), the reading map moved from a Decisions list into Scope, and "0 notes" became "no new notes".
- 2026-09-16: implement started on branch m135-vignette-levels-and-frame. Question gate chose a testthat test for the Level, heading and Wrap-up checks and a tools/ script for the pkgdown index diff, because the diff needs the yaml package and the package does not list it. Baseline: the seven pre-computed pages re-render up to date on this machine before any edit.
- 2026-09-16: T1 done. `_pkgdown.yml` gains the three-level `articles:` index and the navbar menu follows the same order with `axes-reliability` added. `tools/check-pkgdown-vignettes.R` diffs both lists against `vignettes/` and passes, and `pkgdown::check_pkgdown()` finds no problems.
- 2026-09-16: T2 done. `tests/testthat/test-vignette-frame.R` checks AC1 to AC3 over the nine pages and failed on all nine before any page changed. A planted bug in its own fence scan was caught by that first run. The three older pages pass it now. The AC6 comparison strips line numbers from the sweep findings, and the three pages add none.
- 2026-09-16: T3 done. The four numbered pages gain the Level line, an Overview as section 1, and a Wrap-up (new on the SEM and growth pages). Every in-text "Section N" reference was renumbered, and the axes page's reference to its own Wrap-up now says so. The sweep adds no finding on any of the four. The frame test now collapses whitespace so a wrapped heading matches.
- 2026-09-16: T4 done. The visualization page's first section became the Overview and its headings are numbered 2 to 9. The Bayesian page's "Where to go next" became the Wrap-up. Three planted defects each failed the frame test: a wrong level, a skipped section number, a misspelled next-page title. The pkgdown script failed on a missing navbar entry, a swapped index order and an unlisted tenth page. The test reads a quoted string as a title only if it starts with a capital letter.
- 2026-09-16: T5 done. The seven pages re-rendered from the framed sources. The diff to the committed renders is prose plus one elapsed-time line inside a masked volatile region. No figure changed, and the width guard passes on all seven.
- 2026-09-16: T6 done. AC1 to AC3: the frame test passes on all nine sources. AC4: the index script and `pkgdown::check_pkgdown()` both pass. AC5: the `--chunks` output of each `.Rmd` and `.Rmd.orig` is byte-identical to the merge base, and the staleness guard passes at the head. AC6: with line numbers stripped, no source gains a sweep finding, and two findings went away (a Wrap-up sentence on each of the instruments and intermediate pages). NEWS.md has the entry.
- 2026-09-16: claim audit: 71 claims read, 3 corrected — vignettes/sem-based-ssm-analysis.Rmd.orig, vignettes/advanced-visualization.Rmd.orig, NEWS.md. The SEM Overview no longer credits Section 4 with `ssm_sem_parameters()`, the visualization Overview says Section 3 builds a figure from scratch, and the NEWS entry says a terminal page names a related page. The two pages re-rendered with no output change, and the guards pass again.
- 2026-09-16: T7 done. `devtools::test()` at the head: 0 failures, 11217 passes, 1 skip. `devtools::check(args = "--no-manual")`: Status OK with 0 errors, 0 warnings and 0 notes, so no note is new. Status set to review.

## Decisions

## Review

Reviewed 2026-09-16 on branch m135-vignette-levels-and-frame at 9eb175e0. Master did not move after the branch was cut (merge base 8d87bb61).

- AC1: `tests/testthat/test-vignette-frame.R` ran fresh with 183 passes and 0 failures. Its Level checks cover the nine sources against the Scope map and the index titles.
- AC2: the same run passed the heading-sequence and Overview-mentions checks on all nine.
- AC3: the same run passed the Wrap-up next-page checks in both directions of the reading order.
- AC4: `tools/check-pkgdown-vignettes.R` reports that the articles index, the navbar menu and `vignettes/` agree on 9 pages. `pkgdown::check_pkgdown()` finds no problems.
- AC5: `tools/prose-sweep.R --chunks` output is byte-identical to the merge base for all 16 touched `.Rmd` and `.Rmd.orig` files. `tools/check-vignette-staleness.R` reports all 7 pre-computed vignettes up to date.
- AC6: with line numbers stripped, no source gains a sweep finding. The instruments and intermediate pages each lose one finding, and the other seven are unchanged (SEM keeps its 2, the rest 0).
- AC7: `devtools::test()` reports 0 failures, 11217 passes, 1 skip and 11 warnings, all in test files the branch does not touch. `devtools::check(args = "--no-manual")` reports Status OK with 0 errors, 0 warnings and 0 notes, so no note is new.

Consistency gate: `cairn_validate.py` passes every check. `document()` makes no diff and prints no link warning. README.md is in sync. NEWS.md has the entry. `tools/` is in `.Rbuildignore`. The newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master are both green (4ac54be8). The master-red-alert audits and the branch-protection check exit clean. No principle changed, so `cairn_impact.py` was skipped.

Independent review, three lenses ([O] diff-bug, [S] blame-history, [S] prior-review). Findings merged across lenses, ranked, with disposition:

- F1 `vignettes/growth-ssm-analysis.Rmd.orig:500` (all three lenses): the Wrap-up says the next page to read is "Advanced Circumplex Visualization", but Scope lists Growth as terminal and the NEWS entry says a terminal page names a related page. Disposition: pending gate.
- F2 `vignettes/advanced-visualization.Rmd.orig:29` (two lenses): the Level line names "Intermediate SSM Analysis" as the page to read first, but the reading map puts "Evaluating Circumplex Structure" before Visualization. The AC1 procedure only checks that the title exists, so the criterion passes as written. Disposition: pending gate.
- F3 `tests/testthat/test-vignette-frame.R:118`: the Level check never compares the named page with the reading map, which is why F2 passes. Disposition: pending gate.
- F4 `vignettes/sem-based-ssm-analysis.Rmd.orig:508` and `vignettes/evaluating-circumplex-structure.Rmd.orig:878`: "corrects a measure's profile for the unequal reliability of the scales" names only heterogeneity, but the latent SSM also corrects the amplitude for average unreliability. Disposition: pending gate.
- F5 `vignettes/bayesian-ssm-analysis.Rmd:263`: "summarizes the fit with `ssm_draws()`" collides with the SSM fit statistic. The growth page summarizes fixed-effect draws. Disposition: pending gate.
- F6 `vignettes/evaluating-circumplex-structure.Rmd.orig:25`: "the two assumptions that an SSM analysis rests on" undercounts the page and calls interpretation preconditions assumptions. Disposition: pending gate.
- F7 `vignettes/evaluating-circumplex-structure.Rmd:236`: the rendered elapsed time changed from 6.2s to 5.3s. It sits inside a declared volatile region and the `--chunks` output is unchanged. Disposition: rejected, the line is masked volatile output.
- F8 to F15, test and script robustness: the Overview check ignores the section numbers (F8). A wrapped Level line finds no title (F9). Only straight quotes with a capital match (F10). The level map is duplicated in the test and the script (F11). Nothing runs `tools/check-pkgdown-vignettes.R` (F12). The script skips a group with an unknown title (F13). A missing yaml package exits 2 (F14). References is only recognized last (F15). Disposition: pending gate.
- F16 `vignettes/axes-reliability.Rmd.orig:335`: "(the Wrap-up)" is a vaguer pointer than the numbered one it replaced. Disposition: pending gate.
- F17 `vignettes/advanced-visualization.Rmd.orig:65`: a hard line break in the Overview paragraph, cosmetic. Disposition: rejected, renders identically.
- The blame lens noted the navbar reorder as deliberate and found no undone past fix or contradicted decision. The prior-review lens found no PR review threads and one archived precedent (M77) for F1.
