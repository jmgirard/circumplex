# M136: The structure vignette becomes four shorter vignettes

- **Status:** planned
- **Priority:** high
- **Depends on:** M135
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — vignettes, tests that read them, and the pkgdown site
- **Branch/PR:** —

## Goal

`evaluating-circumplex-structure` becomes four vignettes, each no longer than the older vignettes, with every sentence and chunk kept.

## Scope

**In:** Four new `.Rmd.orig` sources in place of `vignettes/evaluating-circumplex-structure.Rmd.orig`, cut at the page's own section boundaries: (1) fitting the circular process model and reading its indices (sections 1, 2.1, 2.2, 2.4 today), Intermediate; (2) a fit at a boundary (section 2.3, 1372 words today), Advanced; (3) confidence-interval accuracy with `ssm_ci_accuracy()` (section 3), Intermediate; (4) the structure tests with `fit_structure()` and ipsatization (sections 4 and 5), Intermediate. Page 1 keeps the filename `evaluating-circumplex-structure` so existing links land on the first page. Each page gets its own setup chunk, `options(width = 77)`, the M135 frame, and a place in the M135 reading order (1 → 3 → 4, with 2 following 1 as an Advanced branch). Retargeting of everything that names the old page: `VIGNETTES` in `tools/precompute-vignettes.R`, `_pkgdown.yml`, `tests/testthat/test-cpm_boundary_vignette.R` (both of its file reads), the `vignette()` links in `R/cpm_oop.R` and `R/fit_structure.R`, the other vignettes, `README.Rmd`, `.github/workflows/`, and the line-anchored citations in `cairn/references/`. The three `precompute:volatile-numbers` regions move with their chunks. NEWS.md names the four pages.

**Out:** Dropping or shortening content (the plan gate chose to move everything). The axes and SEM splits → M137. Old NEWS.md entries that name the old page stay as history.

## Acceptance criteria

- [ ] AC1: `vignettes/` holds the four sources named in Scope and no `evaluating-circumplex-structure` content outside page 1. The set of `.Rmd.orig` basenames in `vignettes/` equals `VIGNETTES` in `tools/precompute-vignettes.R`. Each page is in the `_pkgdown.yml` articles groups and navbar menu, and the M135 check script passes over every source file.
- [ ] AC2: The multiset of non-heading sentences that `tools/prose-sweep.R --prose` prints for the merge base's source equals the union of the multisets printed for the four new sources, apart from at most 20 sentences that the milestone's Decisions section lists as reworded, each with its old and new text. No sentence is dropped. Procedure: a script sorts both sentence lists, drops heading units, diffs them and matches the residual to the listed pairs.
- [ ] AC3: `tools/prose-sweep.R --prose <source> | wc -w` is at most 2600 for each of the four new sources.
- [ ] AC4: Every chunk that `tools/prose-sweep.R --chunks` prints for the merge base's source appears once across the four new sources with the same code and options, apart from the chunk labelled `setup`, which each new source carries. Each `precompute:volatile-numbers` start and end marker of the merge base is in the new source that holds its chunk. Procedure: a diff of the `--chunks` outputs and a grep of the marker pairs.
- [ ] AC5: `tests/testthat/test-cpm_boundary_vignette.R` reads page 2 at both of its file reads (the shipped `.Rmd` and the `.Rmd.orig`) and passes. `grep -rn 'evaluating-circumplex-structure' R/ vignettes/ README.Rmd tests/ tools/ .github/ _pkgdown.yml cairn/references/` returns hits only at page 1's own files and at links whose content page 1 now holds.
- [ ] AC6: `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass at the branch head (the staleness guard knits each page in its own process, so a pass shows each page knits on its own and its figures carry its own prefix), and `tools/prose-sweep.R` exits 0 on each of the four new sources.
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note that the merge base does not report. NEWS.md names the four pages and the page they replace.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T5
- AC3 → T2, T5
- AC4 → T2, T5
- AC5 → T3, T5
- AC6 → T4
- AC7 → T6

## Tasks

- [ ] T1: Write the AC2 and AC4 comparison script under `tools/` (it reads two `--prose` and `--chunks` outputs and reports the residual), and prove it reddens on a planted dropped sentence and a planted changed chunk option before the split.
- [ ] T2: Cut the source into the four pages at the section boundaries in Scope. Each page repeats the setup chunk and `options(width = 77)`, carries the M135 frame, and takes its volatile-number markers. Record every reworded sentence in the Decisions section as it happens.
- [ ] T3: Retarget every site in Scope's list, the test's two reads first. Update the `cairn/references/` citations by line to the new page and line.
- [ ] T4: Add the four names to `VIGNETTES`, and the pages to `_pkgdown.yml` and the M135 reading order. Re-render, and run the staleness, width and M135 check scripts.
- [ ] T5: Run the T1 script against the merge base, the sweep on each page, and the word counts, and record the results in the work log.
- [ ] T6: NEWS.md entry, tests and check, notes compared with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose four pages (the boundary section as its own Advanced page) over three pages with a trimmed boundary section, because a three-page cut leaves the fit page near 2600 words and trimming drops claims; falsified by readers who find four pages harder to navigate than one. Plan gate chose "move everything, cut nothing" over listed cuts, because a sentence diff can then bound what the split changes; falsified by pages that stay too long after the split.
- 2026-09-16: criteria audit (full mode, fresh Opus reader, shared with M135 and M137). Fixed here: sentence comparison excludes headings and uses multisets (three sentences already repeat at master), setup chunk defined by its label, volatile markers get their own grep, the test's two reads are both named, the rename grep covers `.github/` and `cairn/references/`, `VIGNETTES` must equal the `.Rmd.orig` set, and the rewording allowance is capped at 20 listed sentences.

## Decisions

## Review
