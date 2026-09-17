# M137: The axes-reliability and SEM vignettes each split into a core page and an advanced page

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M135
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — vignettes, tests that read them, the width guard and the pkgdown site
- **Branch/PR:** `m137-split-axes-and-sem-vignettes`

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
- [ ] AC5: The three tests that read the axes vignette (`test-axes-scaled-fit.R`, `test-axes-corrected-se.R`, `test-axes-reliability.R`) read `vignettes/axes-reliability-caveats.Rmd` (through `vignette_source()` or `test_path()`) and pass. The `EXEMPT` entries in `tools/check-vignette-width.R` name the page that holds the `cx =~` and `cy =~` lines. At the branch head, every `vignette("<page>")` call, every `articles/<page>.html` link, and every `vignettes/<page>.Rmd` mention in `cairn/references/`, with or without `:<line>`, among the lines that `git grep -n -e 'axes-reliability' -e 'sem-based-ssm-analysis' -e 'sem-latent-contrasts' -- R/ vignettes/ README.Rmd tests/ tools/ .github/ _pkgdown.yml cairn/references/` returns, names a page that exists and holds the content that line attributes to it. For a `:<line>` mention, that content is on that line. Page-list entries, which AC1 covers, and figure `src` paths are excluded.
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

- [x] T1: Cut each source into its two pages at the section boundaries in Scope. Each page repeats the setup chunk with the lavaan gate and `options(width = 77)`, and carries the M135 frame. Record every reworded sentence in the Decisions section as it happens, the two long SEM sentences among them.
- [x] T2: Retarget every site in Scope's list, the three axes tests and the width guard's `EXEMPT` entries first. Update the `cairn/references/` citations by line.
- [x] T3: Add the names of the pre-rendered pages to `VIGNETTES` (the caveats page evaluates no output and ships as a live `.Rmd`), and the pages to `_pkgdown.yml` and the M135 reading order. Re-render, and run the staleness, width and M135 check scripts.
- [x] T4: Run the M136 comparison script against the merge base for both splits, the sweep on each page, and the word counts, and record the results in the work log.
- [ ] T5: NEWS.md entry, tests and check, notes compared with the merge base.

## Work log

- 2026-09-16: created by /milestone-plan. Plan gate chose to split axes and SEM (3253 and 3265 prose words) over leaving them whole under an Advanced label, because both exceed the longest older vignette (2290) by about 1000 words and each has a section boundary that separates a worked example from its caveats; falsified by readers who need the caveats beside the example. The 2600-word cap applies to the four new pages only; the plan did not weigh trimming growth and visualization.
- 2026-09-16: criteria audit (full mode, fresh Opus reader, shared with M135 and M136). Fixed here: the pinned test phrases were verified to sit in section 5, the hardcoded test path is named, the rename grep covers `.github/`, `tools/` and `cairn/references/`, the "every vignette under 2600 words" clause was dropped as reaching pages this milestone does not touch, and `VIGNETTES` must equal the `.Rmd.orig` set.
- 2026-09-16: implement gate (M137-D1): page names `axes-reliability-caveats` and `sem-latent-contrasts`, one reading chain, and go on before the frame-guards row is planned (user choice over the row's "plan before M137" note).
- 2026-09-16: T1 cut. Split guard exit 0 on both splits with the M137-D2 pairs (axes 4, SEM 6 plus one added chunk); sweep exit 0 on all four pages.
- 2026-09-16: AC5 amended (substantive, user-approved at the mini gate). The planned grep could not pass: `axes-reliability` matches the new page name, test file names and the method name. New wording scopes the check to `vignette()` calls, article links and `cairn/references/` page mentions.
- re-audit: AC5 (full) — draft 1 reachable, but six hits ambiguous (workflow comment, roxygen comment, historical heading, figure path, reading map, test-file mention); narrowed to named reference forms.
- re-audit: AC5 (full) — draft 2 reachable; flagged stale `browne1992`/`wendt2019` line citations, bare-path mentions in `cheung2002` and `wendt2019`, and "advanced axes page" as ambiguous; adopted its wording naming the caveats file and covering bare mentions.
- 2026-09-16: T3 minor amendment. `axes-reliability-caveats` ships as a live `.Rmd`, not pre-rendered: it prints no output, and the width guard fails a pre-rendered page with no output line. AC1's `.Rmd.orig` set still equals `VIGNETTES`.
- 2026-09-16: T2 done. The three axes tests read the caveats page, the two roxygen pointers to the calibration table name it, the pkgdown workflow comment names the contrasts page, and the `wendt2019`, `browne1992`, `cheung2002` and `INDEX` citations were re-read against the rendered pages. `EXEMPT` unchanged: the syntax chunk stays on the SEM core page.
- 2026-09-16: T3 done. Three pages re-rendered. Their `#>` output, concatenated with the core page, is identical to the merge base for both splits, and the contrast figure is byte-identical under its new name. Width guard and pkgdown index check exit 0. `devtools::test()` 0 failures (11319 pass, one environment skip). T1 to T3 land in one checkpoint commit, because the tests only pass once all three are in.
- 2026-09-16: T4 at a731f083. Split guard exit 0 on both splits (axes: 232 base sentences, 4 pairs, 4 chunks; SEM: 227 base sentences, 6 pairs, 11 chunks, 1 added). Prose words: axes core 1047, caveats 2539, SEM core 2014, contrasts 1660. Sweep exit 0 on all four. Staleness guard exit 0 after the commit. AC5 grep: 21 reference lines, each names the page holding its content.

## Decisions

- M137-D1 (2026-09-16, implement gate): the advanced pages are `axes-reliability-caveats` ("Axes Reliability Caveats") and `sem-latent-contrasts` ("Latent Group Contrasts"). The reading order is one chain: SEM core, Latent Group Contrasts, Axes Reliability, Axes Reliability Caveats (terminal). The split went ahead before the frame-guards candidate was planned.
- M137-D2: the sentences each split changed, in the shape `tools/check-vignette-split.R` reads. Each subsection is passed to the guard as its own pairs file.

### Axes split

- Reworded: For an instrument administered in blocks, it adds a block-specificity component (Section 6 explains blocks). => For an instrument administered in blocks, it adds a block-specificity component ("Axes Reliability Caveats" explains blocks).
- Reworded: The examples above all use the canonical eight octant scales, but nothing in the model requires them. => The examples in "Axes Reliability" all use the canonical eight octant scales, but nothing in the model requires them.
- Added: The next page to read is "Axes Reliability Caveats", which states those caveats and the limits of the model.
- Added: Sections 2 to 5 take them in turn.

### SEM split

- Reworded: `ssm_sem()` therefore prints the `dcfi` and `cr` columns and a short ΔCFI note only for a two-group fit estimated by ML whose CFI is the plain, non-robust one. => `ssm_sem()` therefore prints the `dcfi` and `cr` columns and a short ΔCFI note only inside that envelope. That is a two-group fit estimated by ML whose CFI is the plain, non-robust one.
- Reworded: The secondary `dcfi` criterion gates nothing, and it prints beside the nested test only for a two-group fit estimated by ML with a plain CFI, and only when a rung has a `dcfi` value. => The secondary `dcfi` criterion gates nothing. It prints beside the nested test only for a two-group fit estimated by ML with a plain CFI. Even then it prints only when a rung has a `dcfi` value.
- Reworded: **Displacement and fit have the disattenuated meanings of Section 6**, not the naive "angle in latent space" and "cosine-ness" readings. => **Displacement and fit have the disattenuated meanings of Section 6 of "SEM-Based SSM Analysis"**, not the naive "angle in latent space" and "cosine-ness" readings.
- Added: The next page to read is "Latent Group Contrasts". It covers group differences, the invariance gate, the limitations of the method and its relation to the literature.
- Added: `ssm_sem()` computes a latent group contrast only when the invariance ladder supports it. When the ladder does not, it returns each group's separate profile with a verdict.
- Added: A contrast of two measures within one group needs no invariance gate.
- Added chunk: lavaan-note-contrasts

## Review
