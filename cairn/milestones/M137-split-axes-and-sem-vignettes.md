# M137: The axes-reliability and SEM vignettes each split into a core page and an advanced page

- **Status:** planned
- **Priority:** normal
- **Depends on:** M135
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — vignettes, tests that read them, the width guard and the pkgdown site
- **Branch/PR:** —

## Goal

`axes-reliability` and `sem-based-ssm-analysis` each become a core page and an advanced page, each no longer than the older vignettes, with every sentence and chunk kept.

## Scope

**In:** Axes reliability: the core page keeps the filename and sections 1 to 4 (what axis reliability is, the worked example, reading the components, the correlation-matrix input); the advanced page takes section 5, "Caveats to keep in mind" (2001 words today), and the Wrap-up content that belongs to it. SEM: the core page keeps the filename and sections 1 to 5 (why a latent SSM, the measurement model, estimating a latent profile, where the intervals come from, what the parameters mean); the advanced page takes sections 6 to 9 (group differences, invariance-gated contrasts, limitations, relation to the literature). All four are Advanced in the M135 map, each core page followed by its advanced page. Each page gets its own setup chunk with the lavaan gate, `options(width = 77)` and the M135 frame. Retargeting: `VIGNETTES`, `_pkgdown.yml`, the three tests that read `axes-reliability.Rmd` (their pinned phrases all sit in section 5, so they move to the advanced page; `test-axes-reliability.R:3117` hardcodes the path), the `EXEMPT` entries in `tools/check-vignette-width.R` (the `cx =~` and `cy =~` lines stay wherever the syntax chunk lands), the `vignette()` links in `R/axes_reliability.R`, the other vignettes, `README.Rmd`, `.github/workflows/pkgdown.yaml`, `tools/m131-line-identity.R`, `tools/check-ci-deps.R`, and the line-anchored citations in `cairn/references/`. NEWS.md names the four pages.

**Out:** Dropping or shortening content (the plan gate chose to move everything). Trimming the growth (2433 words) and visualization (2422 words) pages, which stay whole. Old NEWS.md entries stay as history.

## Acceptance criteria

- [ ] AC1: `vignettes/` holds the four sources named in Scope. The set of `.Rmd.orig` basenames in `vignettes/` equals `VIGNETTES` in `tools/precompute-vignettes.R`. Each page is in the `_pkgdown.yml` articles groups and navbar menu, and the M135 check script passes over every source file.
- [ ] AC2: For each of the two merge-base sources, the multiset of non-heading sentences that `tools/prose-sweep.R --prose` prints equals the union of the multisets printed for its two new sources, apart from at most 20 sentences per split that the Decisions section lists as reworded, each with its old and new text. No sentence is dropped. Procedure: the M136 comparison script.
- [ ] AC3: `tools/prose-sweep.R --prose <source> | wc -w` is at most 2600 for each of the four new sources.
- [ ] AC4: For each of the two merge-base sources, every chunk that `tools/prose-sweep.R --chunks` prints appears once across its two new sources with the same code and options, apart from the chunk labelled `setup`, which each new source carries. Procedure: the M136 comparison script.
- [ ] AC5: The three tests that read the axes vignette (`test-axes-scaled-fit.R`, `test-axes-corrected-se.R`, `test-axes-reliability.R`) read the advanced axes page and pass. The `EXEMPT` entries in `tools/check-vignette-width.R` name the page that holds the `cx =~` and `cy =~` lines. `grep -rn -e 'axes-reliability' -e 'sem-based-ssm-analysis' R/ vignettes/ README.Rmd tests/ tools/ .github/ _pkgdown.yml cairn/references/` returns hits only at the core pages' own files, at the `EXEMPT` entries, and at links whose content the core page now holds.
- [ ] AC6: `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass at the branch head, and `tools/prose-sweep.R` exits 0 on each of the four new sources (the SEM page has two long sentences at master, at lines 379 and 455 of the source, which the split fixes).
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note that the merge base does not report. NEWS.md names the four pages and the two they replace.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T4
- AC3 → T1, T4
- AC4 → T1, T4
- AC5 → T2, T4
- AC6 → T3
- AC7 → T5

## Tasks

- [ ] T1: Cut each source into its two pages at the section boundaries in Scope. Each page repeats the setup chunk with the lavaan gate and `options(width = 77)`, and carries the M135 frame. Record every reworded sentence in the Decisions section as it happens, the two long SEM sentences among them.
- [ ] T2: Retarget every site in Scope's list, the three axes tests and the width guard's `EXEMPT` entries first. Update the `cairn/references/` citations by line.
- [ ] T3: Add the names to `VIGNETTES`, and the pages to `_pkgdown.yml` and the M135 reading order. Re-render, and run the staleness, width and M135 check scripts.
- [ ] T4: Run the M136 comparison script against the merge base for both splits, the sweep on each page, and the word counts, and record the results in the work log.
- [ ] T5: NEWS.md entry, tests and check, notes compared with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose to split axes and SEM (3253 and 3265 prose words) over leaving them whole under an Advanced label, because both exceed the longest older vignette (2290) by about 1000 words and each has a section boundary that separates a worked example from its caveats; falsified by readers who need the caveats beside the example. The 2600-word cap applies to the four new pages only; the plan did not weigh trimming growth and visualization.
- 2026-09-16: criteria audit (full mode, fresh Opus reader, shared with M135 and M136). Fixed here: the pinned test phrases were verified to sit in section 5, the hardcoded test path is named, the rename grep covers `.github/`, `tools/` and `cairn/references/`, the "every vignette under 2600 words" clause was dropped as reaching pages this milestone does not touch, and `VIGNETTES` must equal the `.Rmd.orig` set.

## Decisions

## Review
