# M136: The structure vignette becomes four shorter vignettes

- **Status:** review
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

- [x] AC1: `vignettes/` holds the four sources named in Scope and no `evaluating-circumplex-structure` content outside page 1. The set of `.Rmd.orig` basenames in `vignettes/` equals `VIGNETTES` in `tools/precompute-vignettes.R`. Each page is in the `_pkgdown.yml` articles groups and navbar menu, and the M135 check script passes over every source file.
- [x] AC2: The multiset sum of the non-heading sentences that `tools/prose-sweep.R --prose` prints for the four new sources equals the multiset printed for the merge base's source, with two exceptions. First, the Level line and the body of `## 1. Overview` of every page are left out on both sides, because each page composes its own; `tests/testthat/test-vignette-frame.R` checks that the Level line names the level and the page to read first and that the Overview names every section after it, and nothing checks the other Overview sentences. Second, at most 35 sentences that the milestone's Decisions section lists as reworded or added, each with its old text and its new text (an added sentence has no old text, and a text may hold more than one sentence). No other sentence is dropped. Procedure: `tools/check-vignette-split.R` blanks the heading lines, the Level line and the Overview body of each source, reads the remaining sentences the way `--prose` does, diffs the two multisets and matches the residual to the listed lines.
- [x] AC3: `tools/prose-sweep.R --prose <source> | wc -w` is at most 2600 for each of the four new sources.
- [x] AC4: Every chunk that `tools/prose-sweep.R --chunks` prints for the merge base's source appears once across the four new sources, byte for byte with its header line, apart from the two preamble chunks, the first unlabelled chunk (the `include = FALSE` options chunk) and the first chunk labelled `setup`, which each new source carries. Any chunk a new source adds is listed by its label in the milestone's Decisions section, and a new source adds no marked region except one that holds added chunks only. Each `precompute:volatile-numbers` region of the merge base is in the new source that holds its chunks, with the same chunk labels inside it and no other. Procedure: `tools/check-vignette-split.R` compares the fenced blocks byte for byte, fails a source that lacks either preamble chunk, holds an unlisted added chunk or adds a region over a base chunk, and compares the marked regions by the chunk labels they hold.
- [x] AC5: `tests/testthat/test-cpm_boundary_vignette.R` reads page 2 at both of its file reads (the shipped `.Rmd` and the `.Rmd.orig`) and passes. `grep -rn 'evaluating-circumplex-structure' R/ vignettes/ README.Rmd tests/ tools/ .github/ _pkgdown.yml cairn/references/` returns hits only at page 1's own files and at links whose content page 1 now holds.
- [x] AC6: `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass at the branch head (the staleness guard knits each page in its own process, so a pass shows each page knits on its own and its figures carry its own prefix), and `tools/prose-sweep.R` exits 0 on each of the four new sources.
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

- [x] T1: Write the AC2 and AC4 comparison script under `tools/` (it reads two `--prose` and `--chunks` outputs and reports the residual), and prove it reddens on a planted dropped sentence, an unlisted added sentence, a sentence duplicated across pages, a listed line count over the cap, a changed chunk option, a chunk in two pages, a marked region left open or without its start, a region whose chunk set changed or that is split across pages, a region added over a base chunk, a page without either preamble chunk, an unlisted added chunk, a listed chunk that no page holds, a chunk label repeated in one page, and a listing outside the Decisions section, and stays green on an added chunk listed by label, a region added around it, a sentence moved between pages, a heading edit, an edit inside the Level line or the Overview body, and a listed one-to-two split, before the split.
- [x] T2: Cut the source into the four pages at the section boundaries in Scope. Each page repeats the setup chunk and `options(width = 77)`, carries the M135 frame, and takes its volatile-number markers. Record every reworded sentence in the Decisions section as it happens.
- [x] T3: Retarget every site in Scope's list, the test's two reads first. Update the `cairn/references/` citations by line to the new page and line.
- [x] T4: Add the four names to `VIGNETTES`, and the pages to `_pkgdown.yml` and the M135 reading order. Re-render, and run the staleness, width and M135 check scripts.
- [x] T5: Run the T1 script against the merge base, the sweep on each page, and the word counts, and record the results in the work log.
- [x] T6: NEWS.md entry, tests and check, notes compared with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose four pages (the boundary section as its own Advanced page) over three pages with a trimmed boundary section, because a three-page cut leaves the fit page near 2600 words and trimming drops claims; falsified by readers who find four pages harder to navigate than one. Plan gate chose "move everything, cut nothing" over listed cuts, because a sentence diff can then bound what the split changes; falsified by pages that stay too long after the split.
- 2026-09-16: criteria audit (full mode, fresh Opus reader, shared with M135 and M137). Fixed here: sentence comparison excludes headings and uses multisets (three sentences already repeat at master), setup chunk defined by its label, volatile markers get their own grep, the test's two reads are both named, the rename grep covers `.github/` and `cairn/references/`, `VIGNETTES` must equal the `.Rmd.orig` set, and the rewording allowance is capped at 20 listed sentences.

- 2026-09-16: /milestone-implement started. Branch m136-split-structure-vignette. Question gate chose: AC2 amended (frame sentences excluded, added sentences allowed); page 2 refits the model under a new chunk label `cpm_refit` with its output shown inside a new marked region; page names `cpm-boundary-fits` ("CPM Fits at a Boundary"), `ci-accuracy` ("Confidence Interval Accuracy"), `structure-tests` ("Structure Tests and Ipsatization"); page 4 becomes the page the two Advanced pages name as read first.
- 2026-09-16: re-audit: AC2 (full) — three findings: whole-Overview/Wrap-up exclusion leaves frame prose unchecked by any instrument; `--prose` output alone cannot mark heading or frame units; T1's plant varies only the dropped-sentence axis. Fixed by keeping the Wrap-up in the comparison, raising the cap to 35, and widening T1's plants (minor task edit).
- 2026-09-16: re-audit: AC2 (full) — returned four: "union of multisets" undefined (sum vs max, three known repeats); deletions and splits inexpressible; excluded Overview bodies unchecked by the cited test; T1 needs four redden and three stay-green axes. Fixed by saying "multiset sum", stating what the frame test checks, noting that a listed text may hold more than one sentence, and adding the stay-green plants. Second line for AC2, so no further reader; the final wording went to the user.
- 2026-09-16: Substantive amendment, accepted at the gate: AC2 replaced by the wording above (the old text: multiset equality apart from at most 20 listed reworded sentences, no frame exclusion). Coverage unchanged. The Overview's own sentences are compared by nothing; the plan gate's "cut nothing" promise rests on the Overview being a section list, as M135 defined it.
- 2026-09-16: T1 done. `tools/check-vignette-split.R` sources the sweep for its sentence and chunk definitions, reads the base as `<ref>:<path>`, and reads the listed lines from the milestone file (`- Reworded: old => new`, `- Added: new`). Ten defect plants reddened and four stay-green cases stayed green on a two-page cut of the current source. AC4's preamble exemption widened at the gate to both preamble chunks; the wording goes to a fresh reader before it is written.
- 2026-09-16: re-audit: AC4 (full) — added chunks unbounded and stricter-than-stated byte match; "each source carries setup" unchecked; missing probes for a region's chunk set changed or split; Scope says three regions, base has four. Fixed by listing added chunks by label, saying "byte for byte", making the script fail a page without either preamble chunk, and adding the two plants. The Scope count is wrong as written (four regions); the intent holds and the text stays.
- 2026-09-16: re-audit: AC4 (full) — three: added marked regions unbounded (staleness blind spot); "listed in the Decisions section" checked only as a line anywhere in the pairs file; T1's plant list stale versus the new clauses. Fixed by binding new regions to added chunks, reading listed lines from the Decisions section only, and rewriting T1's plant list. Second line for AC4, so no further reader; the final wording went to the user.
- 2026-09-16: Substantive amendment, accepted at the gate: AC4 replaced by the wording above (the old text exempted the `setup` chunk only and compared markers by grep). Coverage unchanged.
- 2026-09-16: T2 done. Four sources cut at the section boundaries; page 3's subsections became numbered sections and page 4 kept the two old sections; page 2 holds the one boundary section with the `cpm_refit` chunk in its own marked region. The guard passes against the master source with the 22 lines in Decisions. Prose words: page 1 1635, page 2 1520, page 3 1971, page 4 1790. The sweep exits 0 on each page.
- 2026-09-16: T3 done. The boundary test reads `cpm-boundary-fits` at both reads, runs `cpm_refit`, and strips the section number before reading the help page (43 pass). The frame map gained the three pages and the reading order 1 → 2, 1 → 3 → 4 → Advanced (249 pass). Roxygen links in `R/cpm_oop.R` and `R/fit_structure.R` retargeted and `man/` regenerated (the installed roxygen2 is 8.0.0, the package records 8.1.0; only the two link lines changed). The Level lines of the visualization and SEM pages name page 4, and the intermediate Wrap-up no longer says page 1 covers interval trust. Reference pages: five line-anchored citations re-read against the rendered pages and marked `re-read at M136`; the unanchored ones and the INDEX rows renamed. The AC5 grep now hits only page 1's own sites. README.Rmd and `.github/` never named the page.
- 2026-09-16: T4 done. `VIGNETTES`, the pkgdown level map, the articles index and the navbar menu carry the four pages (the Advanced group's description gained "Boundary fits"). The seven changed pages re-rendered in their own processes after an install; two figures moved to the pages that draw them and the old copies were removed. Width guard: all 10 pre-computed pages fit. Index script and `check_pkgdown()` clean. The staleness guard runs after the checkpoint, because it compares against the committed copy.
- 2026-09-16: T5 done. Staleness guard at the T3/T4 checkpoint: all 10 pages up to date (page 1 two masked regions, page 2 one, page 3 two). `tools/check-vignette-split.R --pairs <milestone file> --max-pairs 35` against `master:vignettes/evaluating-circumplex-structure.Rmd.orig`: 486 base sentences outside the frame, 499 across the pages, 22 listed lines, 14 base chunks each in one page, one added chunk `cpm_refit`, four marked regions placed; exit 0. Sweep exit 0 on each page. Prose words 1635, 1520, 1971, 1790 (cap 2600). The `.Rmd.orig` basename set equals `VIGNETTES`.
- 2026-09-16: claim audit: 21 claims read, 0 corrected — NEWS.md, R/cpm_oop.R, R/fit_structure.R, tests/testthat/test-cpm_boundary_vignette.R, tools/check-vignette-split.R, tools/check-vignette-staleness.R, the four new vignette sources. The fresh Opus reader ran the split guard and its failure modes itself and read the refit's rendered warnings against page 1's.
- 2026-09-16: T6 done. NEWS.md Documentation entry names the four pages and the page they replace. `devtools::test()`: 0 failures, 11 warnings, 1 skip, 11283 pass; the warnings are the ill-conditioned Hessian and lavaan marker warnings from test-ci_accuracy.R, test-print-width.R and test-ssm_sem.R, files this branch does not touch. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes, the same as the M135 review recorded for master. Status to review.

## Decisions

- M136-D1 (2026-09-16): the sentences the split changed, in the shape `tools/check-vignette-split.R` reads. Seventeen cross-references reworded to name a page instead of a section, one refit introduction added, four Wrap-up passages added, one chunk added.
- Reworded: This vignette shows how to answer both questions. => This page and the two after it show how to answer both questions.
- Reworded: The section *When a fit sits at a boundary*, below, explains the first. => The page "CPM Fits at a Boundary" explains the first.
- Reworded: The SSM accuracy thresholds in Section 4 span roughly $n = 50$ to $200$. => The SSM accuracy thresholds in "Confidence Interval Accuracy" span roughly $n = 50$ to $200$.
- Reworded: The next subsection, *When a fit sits at a boundary*, says what that means and what to do about it. => The page "CPM Fits at a Boundary" says what that means and what to do about it.
- Reworded: When a boundary marker (defined in the next subsection) is present, the analytic caution prints up to very large $N$. => When a boundary marker (defined on that page) is present, the analytic caution prints up to very large $N$.
- Reworded: The first of these refits (`quasi-circumplex`) prints the same ill-conditioned Hessian warning that *When a fit sits at a boundary* explains. => The first of these refits (`quasi-circumplex`) prints the same ill-conditioned Hessian warning that "CPM Fits at a Boundary" explains.
- Reworded: The fit above is one of these fits, and it says so. => The fit above sits at a boundary, and it says so.
- Reworded: The default `"quasi-circumplex"` model is the least constrained of the variants in the next subsection. => The default `"quasi-circumplex"` model is the least constrained of the variants that "Evaluating Circumplex Structure" compares.
- Reworded: Read them with the caution given earlier in this section, which is about field sample sizes, not about markers. => Read them with the caution that "Evaluating Circumplex Structure" gives under *Reading the fit indices*, which is about field sample sizes, not about markers.
- Reworded: The CPM fit inside the diagnostic prints the same ill-conditioned Hessian warning that *When a fit sits at a boundary* explains. => The CPM fit inside the diagnostic prints the same ill-conditioned Hessian warning that "CPM Fits at a Boundary" explains.
- Reworded: Second, the embedded CPM fit is returned as `acc$cpm`, for inspection with the tools from Section 3. => Second, the embedded CPM fit is returned as `acc$cpm`, for inspection with the tools from "Evaluating Circumplex Structure".
- Reworded: Putting Sections 3 and 4 together into a checklist: => Putting "Evaluating Circumplex Structure" and this page together into a checklist:
- Reworded: See Section 6.) => See "Structure Tests and Ipsatization".)
- Reworded: Section 3's confirmatory model (`cpm_fit()`) fits one theory-driven circular model and asks how well it fits. => The confirmatory model of "Evaluating Circumplex Structure" (`cpm_fit()`) fits one theory-driven circular model and asks how well it fits.
- Reworded: For the IIP-SC octants, the picture agrees with Section 3's CPM fit. => For the IIP-SC octants, the picture agrees with the CPM fit in "Evaluating Circumplex Structure".
- Reworded: The caution is to inspect the loading configuration (the plot above) and Section 3's CPM fit together. => The caution is to inspect the loading configuration (the plot above) and the CPM fit in "Evaluating Circumplex Structure" together.
- Reworded: `cpm_fit()` (Section 3) and `fit_structure()` ask related but different questions. => `cpm_fit()` (in "Evaluating Circumplex Structure") and `fit_structure()` ask related but different questions.
- Added: This page starts from the fit of the `jz2017` octants that "Evaluating Circumplex Structure" makes. The same call refits it here, with the same seed and the same two warnings.
- Added: "CPM Fits at a Boundary" explains the boundary note that the fit above printed and what to do when a marker fires. "Confidence Interval Accuracy" asks whether the intervals can be trusted at your sample size and profile.
- Added: A boundary solution is the estimator meeting real data, and a fired marker is a finding to locate, refit, report and keep in view. No page follows this one. "Evaluating Circumplex Structure" compares the model variants that a refit chooses among. "Confidence Interval Accuracy" asks whether the intervals of an SSM analysis can be trusted at your sample size.
- Added: Interval accuracy depends on the sample size, the instrument and how differentiated the profile really is, and `ssm_ci_accuracy()` measures it at your own configuration. The next page is "Structure Tests and Ipsatization". It asks the exploratory version of the structure question and shows what ipsatizing removes from a profile.
- Added: `fit_structure()` asks whether the scales show circumplex structure at all, without committing to the theoretical angles, and `ipsatize()` removes elevation before any profile is computed. Two pages follow this one.
- Added chunk: cpm_refit

## Review

Reviewed 2026-09-16 at branch head 0a3631cd against master 75757fb9 (master unmoved since the cut; no PR open).

- AC1: `vignettes/` holds the four `.Rmd.orig` sources; the sorted basename set of all ten `.Rmd.orig` files equals `VIGNETTES` (diff empty). No `.Rmd.orig` names the old page. `_pkgdown.yml` lists the four pages in the articles groups (lines 121-127) and the navbar menu (181-187). `tools/check-pkgdown-vignettes.R`: 12 pages agree, exit 0. Evidence recorded; ticked.
- AC2: `tools/check-vignette-split.R --pairs <milestone> --max-pairs 35 master:…orig` on the four pages: 486 base sentences outside the frame, 499 across the pages, 22 listed pairs, every base sentence in exactly one page, exit 0. Ticked.
- AC3: prose words 1635, 1520, 1971, 1790 (cap 2600). Ticked.
- AC4: same guard run: 14 base chunks outside the preamble each in one page, one added chunk `cpm_refit` listed, 4 marked regions placed, exit 0. Ticked.
- AC5: `test-cpm_boundary_vignette.R` reads `cpm-boundary-fits.Rmd` (line 12-13) and `.Rmd.orig` (line 33). 15 pass, 0 fail, 3 CRAN skips. The grep hits only page 1's own sites. Those are the frame map and reading order in `test-vignette-frame.R`, `VIGNETTES`, the pkgdown index script, `_pkgdown.yml`, and the reference-page citations that page 1 still holds. The gitignored `tests/testthat/_problems/` scratch files also hit and are not tracked. Ticked.
- AC6: staleness guard: all 10 pages up to date (page 1 two masked regions, page 2 one, page 3 two), exit 0. Width guard: all 10 pages within 80 columns, exit 0. Sweep exit 0 on each of the four pages. Ticked.

Consistency gate: `cairn_validate.py` all checks passed. No DESIGN.md principle changed, so no impact report. `document()` produced no diff and no resolve-link line. README.md is not older than README.Rmd and neither is in the diff. `pkgdown::check_pkgdown()` found no problems. NEWS.md Documentation entry names the four pages and the page they replace. `tools/` is in `.Rbuildignore`. Master watches: the newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master (56d8a147, an ancestor of master's head) both concluded success. Master-red alert audit, its dry run and the branch-protection check all exit clean.

Independent review, three fresh-context lenses. The blame-history lens found nothing. The prior-review lens found no inline PR comments and three findings, each an item of the ROADMAP row "Harden the vignette frame guards", whose promotion condition (a page added) M136 meets.
- P1: the level map and reading order are kept by hand in `test-vignette-frame.R`, `tools/check-pkgdown-vignettes.R` and `_pkgdown.yml`, and the diff extends all three.
- P2: `tools/check-pkgdown-vignettes.R` runs in no workflow or test.
- P3: title cross-references match straight double quotes only.
The diff-bug lens, ranked most severe first:
- F1: page 1 line 167 "(defined on that page)" names no page in its paragraph. The page is named eleven lines earlier in a bullet.
- F2: page 4's added Wrap-up says `fit_structure()` does not commit to the theoretical angles, while the body says RANDALL takes an order hypothesis.
- F3: the two Advanced pages now say to read "Structure Tests and Ipsatization" first, a reading order stated as a prerequisite.
- F4: page 1's Wrap-up keeps the `ssm_ci_accuracy()` trust claim on a page that no longer runs it.
- F5: page 1 line 55 "the two after it" is ambiguous against the navbar order.
- F6: the split guard prints the word count but never fails on it.
- F7: the split guard accepts an added region that holds no chunk.
- F8: `read_pairs` splits a listed text on its first and on its last " => " differently.
- F9: `strip_frame` also blanks `#` comment lines inside chunks.
- F10: the scoping comment of `boundary_section_text()` overstates what it buys on a page that is one section.
- F11: the row body at `cairn/references/plain-vignettes.md:170` says "this page" where four pages now exist.
- F12: NEWS.md "every sentence and chunk kept" overstates. 17 sentences were reworded and 5 passages added.
