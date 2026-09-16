# M135: Vignettes are labeled by level and framed like the older ones

- **Status:** in-progress
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

- [ ] AC1: In each of the nine vignette source files (the `.Rmd.orig` where one exists, else the `.Rmd`), the first prose line after the setup chunk has the form `**Level:** <Introductory|Intermediate|Advanced>. <sentence>`. The sentence names each page to read first by its title, or says that none is needed. Procedure: a script greps `^\*\*Level:\*\*` in the nine files, requires one hit per file, checks the level word against the map in Scope, and checks each quoted title against the `VignetteIndexEntry` titles in `vignettes/`.
- [ ] AC2: In each of the nine source files, `grep '^## '` prints, in order, `## 1. Overview`, then `## 2.` to `## N.` with consecutive numbers, then `## Wrap-up`, then `## References` where the page has one. Procedure: the same script checks the heading sequence and checks that the text of every heading after Overview appears in the Overview section's body.
- [ ] AC3: Each Wrap-up section names at least one page to read next by its title, and each named title is a `VignetteIndexEntry` title. Each page in the reading order in Scope is named by the Wrap-up of the page before it. Procedure: the script reads the Wrap-up sections and checks both directions against the reading order.
- [ ] AC4: `_pkgdown.yml` has an `articles:` section with one group per level, every vignette in `vignettes/` listed exactly once across the groups in the map's order, and a navbar Vignettes menu with the same entries in the same order. Procedure: a script reads the two lists from the YAML and the vignette titles from `vignettes/` and diffs them. `pkgdown::check_pkgdown()` runs clean as a second check.
- [ ] AC5: For each of the seven pre-computed vignettes, the `tools/prose-sweep.R --chunks` output at the branch head is identical to the output at the merge base, and `tools/check-vignette-staleness.R` passes at the branch head.
- [ ] AC6: For each of the nine source files, the finding list of `tools/prose-sweep.R` at the branch head is a subset of its finding list at the merge base. The SEM page and the three older pages have findings at master, so the frame adds none and removes any it touches.
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note that the merge base does not report.

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
- [ ] T5: Re-render the seven pre-computed pages with `tools/precompute-vignettes.R` (the package must be installed first), and run the staleness and width guards.
- [ ] T6: Run the AC1 to AC4 scripts, the `--chunks` comparison and the sweep-subset comparison against the merge base, and record the results in the work log. Add the NEWS.md entry.
- [ ] T7: Run tests and the check, and compare the notes with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose three levels with a Level line, Overview and Wrap-up on every page over labels in the site index only, because the older vignettes carry their frame in the page and the maintainer asked to emulate them; falsified by readers who report the frame as noise.
- 2026-09-16: criteria audit ran in full mode on a fresh Opus reader; 16 findings. Fixed at the gate: the sweep already fails on the SEM page (AC6 now a subset rule), `check_pkgdown()` validates no navbar (AC4 names a diff script), the Level, Overview and Wrap-up checks are scripted (AC1 to AC3), the reading map moved from a Decisions list into Scope, and "0 notes" became "no new notes".
- 2026-09-16: implement started on branch m135-vignette-levels-and-frame. Question gate chose a testthat test for the Level, heading and Wrap-up checks and a tools/ script for the pkgdown index diff, because the diff needs the yaml package and the package does not list it. Baseline: the seven pre-computed pages re-render up to date on this machine before any edit.
- 2026-09-16: T1 done. `_pkgdown.yml` gains the three-level `articles:` index and the navbar menu follows the same order with `axes-reliability` added. `tools/check-pkgdown-vignettes.R` diffs both lists against `vignettes/` and passes, and `pkgdown::check_pkgdown()` finds no problems.
- 2026-09-16: T2 done. `tests/testthat/test-vignette-frame.R` checks AC1 to AC3 over the nine pages and failed on all nine before any page changed. A planted bug in its own fence scan was caught by that first run. The three older pages pass it now. The AC6 comparison strips line numbers from the sweep findings, and the three pages add none.
- 2026-09-16: T3 done. The four numbered pages gain the Level line, an Overview as section 1, and a Wrap-up (new on the SEM and growth pages). Every in-text "Section N" reference was renumbered, and the axes page's reference to its own Wrap-up now says so. The sweep adds no finding on any of the four. The frame test now collapses whitespace so a wrapped heading matches.
- 2026-09-16: T4 done. The visualization page's first section became the Overview and its headings are numbered 2 to 9. The Bayesian page's "Where to go next" became the Wrap-up. Three planted defects each failed the frame test: a wrong level, a skipped section number, a misspelled next-page title. The pkgdown script failed on a missing navbar entry, a swapped index order and an unlisted tenth page. The test reads a quoted string as a title only if it starts with a capital letter.

## Decisions

## Review
