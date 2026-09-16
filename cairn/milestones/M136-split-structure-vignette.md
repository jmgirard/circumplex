# M136: The structure vignette becomes four shorter vignettes

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M135
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — vignettes, tests that read them, and the pkgdown site
- **Branch/PR:** m136-split-structure-vignette

## Goal

`evaluating-circumplex-structure` becomes four vignettes, each no longer than the older vignettes, with every sentence and chunk kept.

## Scope

**In:** Four new `.Rmd.orig` sources in place of `vignettes/evaluating-circumplex-structure.Rmd.orig`, cut at the page's own section boundaries: (1) fitting the circular process model and reading its indices (sections 1, 2.1, 2.2, 2.4 today), Intermediate; (2) a fit at a boundary (section 2.3, 1372 words today), Advanced; (3) confidence-interval accuracy with `ssm_ci_accuracy()` (section 3), Intermediate; (4) the structure tests with `fit_structure()` and ipsatization (sections 4 and 5), Intermediate. Page 1 keeps the filename `evaluating-circumplex-structure` so existing links land on the first page. Each page gets its own setup chunk, `options(width = 77)`, the M135 frame, and a place in the M135 reading order (1 → 3 → 4, with 2 following 1 as an Advanced branch). Retargeting of everything that names the old page: `VIGNETTES` in `tools/precompute-vignettes.R`, `_pkgdown.yml`, `tests/testthat/test-cpm_boundary_vignette.R` (both of its file reads), the `vignette()` links in `R/cpm_oop.R` and `R/fit_structure.R`, the other vignettes, `README.Rmd`, `.github/workflows/`, and the line-anchored citations in `cairn/references/`. The three `precompute:volatile-numbers` regions move with their chunks. NEWS.md names the four pages.

**Out:** Dropping or shortening content (the plan gate chose to move everything). The axes and SEM splits → M137. Old NEWS.md entries that name the old page stay as history.

## Acceptance criteria

- [ ] AC1: `vignettes/` holds the four sources named in Scope and no `evaluating-circumplex-structure` content outside page 1. The set of `.Rmd.orig` basenames in `vignettes/` equals `VIGNETTES` in `tools/precompute-vignettes.R`. Each page is in the `_pkgdown.yml` articles groups and navbar menu, and the M135 check script passes over every source file.
- [ ] AC2: The multiset sum of the non-heading sentences that `tools/prose-sweep.R --prose` prints for the four new sources equals the multiset printed for the merge base's source, with two exceptions. First, the Level line and the body of `## 1. Overview` of every page are left out on both sides, because each page composes its own; `tests/testthat/test-vignette-frame.R` checks that the Level line names the level and the page to read first and that the Overview names every section after it, and nothing checks the other Overview sentences. Second, at most 35 sentences that the milestone's Decisions section lists as reworded or added, each with its old text and its new text (an added sentence has no old text, and a text may hold more than one sentence). No other sentence is dropped. Procedure: `tools/check-vignette-split.R` blanks the heading lines, the Level line and the Overview body of each source, reads the remaining sentences the way `--prose` does, diffs the two multisets and matches the residual to the listed lines.
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

- [x] T1: Write the AC2 and AC4 comparison script under `tools/` (it reads two `--prose` and `--chunks` outputs and reports the residual), and prove it reddens on a planted dropped sentence, an unlisted added sentence, a sentence duplicated across pages, a listed line count over the cap, a changed chunk option, a chunk in two pages, a marked region left open or without its start, and a chunk label repeated in one page, and stays green on a sentence moved between pages, a heading edit, an edit inside the Level line or the Overview body, and a listed one-to-two split, before the split.
- [ ] T2: Cut the source into the four pages at the section boundaries in Scope. Each page repeats the setup chunk and `options(width = 77)`, carries the M135 frame, and takes its volatile-number markers. Record every reworded sentence in the Decisions section as it happens.
- [ ] T3: Retarget every site in Scope's list, the test's two reads first. Update the `cairn/references/` citations by line to the new page and line.
- [ ] T4: Add the four names to `VIGNETTES`, and the pages to `_pkgdown.yml` and the M135 reading order. Re-render, and run the staleness, width and M135 check scripts.
- [ ] T5: Run the T1 script against the merge base, the sweep on each page, and the word counts, and record the results in the work log.
- [ ] T6: NEWS.md entry, tests and check, notes compared with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose four pages (the boundary section as its own Advanced page) over three pages with a trimmed boundary section, because a three-page cut leaves the fit page near 2600 words and trimming drops claims; falsified by readers who find four pages harder to navigate than one. Plan gate chose "move everything, cut nothing" over listed cuts, because a sentence diff can then bound what the split changes; falsified by pages that stay too long after the split.
- 2026-09-16: criteria audit (full mode, fresh Opus reader, shared with M135 and M137). Fixed here: sentence comparison excludes headings and uses multisets (three sentences already repeat at master), setup chunk defined by its label, volatile markers get their own grep, the test's two reads are both named, the rename grep covers `.github/` and `cairn/references/`, `VIGNETTES` must equal the `.Rmd.orig` set, and the rewording allowance is capped at 20 listed sentences.

- 2026-09-16: /milestone-implement started. Branch m136-split-structure-vignette. Question gate chose: AC2 amended (frame sentences excluded, added sentences allowed); page 2 refits the model under a new chunk label `cpm_refit` with its output shown inside a new marked region; page names `cpm-boundary-fits` ("CPM Fits at a Boundary"), `ci-accuracy` ("Confidence Interval Accuracy"), `structure-tests` ("Structure Tests and Ipsatization"); page 4 becomes the page the two Advanced pages name as read first.
- 2026-09-16: re-audit: AC2 (full) — three findings: whole-Overview/Wrap-up exclusion leaves frame prose unchecked by any instrument; `--prose` output alone cannot mark heading or frame units; T1's plant varies only the dropped-sentence axis. Fixed by keeping the Wrap-up in the comparison, raising the cap to 35, and widening T1's plants (minor task edit).
- 2026-09-16: re-audit: AC2 (full) — returned four: "union of multisets" undefined (sum vs max, three known repeats); deletions and splits inexpressible; excluded Overview bodies unchecked by the cited test; T1 needs four redden and three stay-green axes. Fixed by saying "multiset sum", stating what the frame test checks, noting that a listed text may hold more than one sentence, and adding the stay-green plants. Second line for AC2, so no further reader; the final wording went to the user.
- 2026-09-16: Substantive amendment, accepted at the gate: AC2 replaced by the wording above (the old text: multiset equality apart from at most 20 listed reworded sentences, no frame exclusion). Coverage unchanged. The Overview's own sentences are compared by nothing; the plan gate's "cut nothing" promise rests on the Overview being a section list, as M135 defined it.
- 2026-09-16: T1 done. `tools/check-vignette-split.R` sources the sweep for its sentence and chunk definitions, reads the base as `<ref>:<path>`, and reads the listed lines from the milestone file (`- Reworded: old => new`, `- Added: new`). Ten defect plants reddened and four stay-green cases stayed green on a two-page cut of the current source. AC4's preamble exemption widened at the gate to both preamble chunks; the wording goes to a fresh reader before it is written.

## Decisions

## Review
