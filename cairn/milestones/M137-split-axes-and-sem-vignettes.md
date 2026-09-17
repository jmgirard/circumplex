# M137: The axes-reliability and SEM vignettes each split into a core page and an advanced page

- **Status:** review
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

- [x] AC1: `vignettes/` holds the four sources named in Scope. The set of `.Rmd.orig` basenames in `vignettes/` equals `VIGNETTES` in `tools/precompute-vignettes.R`. Each page is in the `_pkgdown.yml` articles groups and navbar menu, and the M135 check script passes over every source file.
- [x] AC2: For each of the two merge-base sources, the multiset of non-heading sentences that `tools/prose-sweep.R --prose` prints equals the union of the multisets printed for its two new sources, apart from at most 20 sentences per split that the Decisions section lists as reworded, each with its old and new text. No sentence is dropped. Procedure: the M136 comparison script.
- [x] AC3: `tools/prose-sweep.R --prose <source> | wc -w` is at most 2600 for each of the four new sources.
- [x] AC4: For each of the two merge-base sources, every chunk that `tools/prose-sweep.R --chunks` prints appears once across its two new sources with the same code and options, apart from the chunk labelled `setup`, which each new source carries. Procedure: the M136 comparison script.
- [x] AC5: The three tests that read the axes vignette (`test-axes-scaled-fit.R`, `test-axes-corrected-se.R`, `test-axes-reliability.R`) read `vignettes/axes-reliability-caveats.Rmd` (through `vignette_source()` or `test_path()`) and pass. The `EXEMPT` entries in `tools/check-vignette-width.R` name the page that holds the `cx =~` and `cy =~` lines. At the branch head, every `vignette("<page>")` call, every `articles/<page>.html` link, and every `vignettes/<page>.Rmd` mention in `cairn/references/`, with or without `:<line>`, among the lines that `git grep -n -e 'axes-reliability' -e 'sem-based-ssm-analysis' -e 'sem-latent-contrasts' -- R/ vignettes/ README.Rmd tests/ tools/ .github/ _pkgdown.yml cairn/references/` returns, names a page that exists and holds the content that line attributes to it. For a `:<line>` mention, that content is on that line. Page-list entries, which AC1 covers, and figure `src` paths are excluded.
- [x] AC6: `tools/check-vignette-staleness.R` and `tools/check-vignette-width.R` pass at the branch head, and `tools/prose-sweep.R` exits 0 on each of the four new sources (the SEM page has two long sentences at master, at lines 379 and 455 of the source, which the split fixes).
- [x] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and no note that the merge base does not report. NEWS.md names the four pages and the two they replace.

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
- [x] T5: NEWS.md entry, tests and check, notes compared with the merge base.

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
- claim audit: 25 claims read, 4 corrected — vignettes/sem-latent-contrasts.Rmd.orig, vignettes/axes-reliability-caveats.Rmd
- 2026-09-16: claim-audit fixes: contrasts options comment, two contrasts Overview sentences, and the caveats provenance sentence (a fifth axes pair in M137-D2). Same reader re-read all four: hold. Contrasts page re-rendered; axes split guard and sweep exit 0.
- 2026-09-16: T5 done. NEWS.md entry names the four pages. `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes (built from 4dda5de4, before the prose-only claim-audit fixes); merge base reported 0 notes at M136. Frame test 285 pass and staleness guard exit 0 after the fixes. Status review.

## Decisions

- M137-D1 (2026-09-16, implement gate): the advanced pages are `axes-reliability-caveats` ("Axes Reliability Caveats") and `sem-latent-contrasts` ("Latent Group Contrasts"). The reading order is one chain: SEM core, Latent Group Contrasts, Axes Reliability, Axes Reliability Caveats (terminal). The split went ahead before the frame-guards candidate was planned.
- M137-D2: the sentences each split changed, in the shape `tools/check-vignette-split.R` reads. Each subsection is passed to the guard as its own pairs file.

### Axes split

- Reworded: For an instrument administered in blocks, it adds a block-specificity component (Section 6 explains blocks). => For an instrument administered in blocks, it adds a block-specificity component ("Axes Reliability Caveats" explains blocks).
- Reworded: The examples above all use the canonical eight octant scales, but nothing in the model requires them. => The examples in "Axes Reliability" all use the canonical eight octant scales, but nothing in the model requires them.
- Added: The next page to read is "Axes Reliability Caveats", which states those caveats and the limits of the model.
- Added: Sections 2 to 5 take them in turn.
- Reworded: Note the provenance, because it differs from the rest of this vignette. => Note the provenance of the FIML path.

### SEM split

- Reworded: `ssm_sem()` therefore prints the `dcfi` and `cr` columns and a short ΔCFI note only for a two-group fit estimated by ML whose CFI is the plain, non-robust one. => `ssm_sem()` therefore prints the `dcfi` and `cr` columns and a short ΔCFI note only inside that envelope. That is a two-group fit estimated by ML whose CFI is the plain, non-robust one.
- Reworded: The secondary `dcfi` criterion gates nothing, and it prints beside the nested test only for a two-group fit estimated by ML with a plain CFI, and only when a rung has a `dcfi` value. => The secondary `dcfi` criterion gates nothing. It prints beside the nested test only for a two-group fit estimated by ML with a plain CFI. Even then it prints only when a rung has a `dcfi` value.
- Reworded: **Displacement and fit have the disattenuated meanings of Section 6**, not the naive "angle in latent space" and "cosine-ness" readings. => **Displacement and fit have the disattenuated meanings of Section 6 of "SEM-Based SSM Analysis"**, not the naive "angle in latent space" and "cosine-ness" readings.
- Added: The next page to read is "Latent Group Contrasts". It covers group differences, the invariance gate, the limitations of the method and its relation to the literature.
- Added: `ssm_sem()` computes a latent group contrast only when the invariance ladder supports it. When the ladder does not, it returns each group's separate profile with a verdict.
- Added: A contrast of two measures within one group needs no invariance gate.
- Added chunk: lavaan-note-contrasts

## Review

- AC1 (2026-09-16, at 8eeb80fa): `vignettes/` holds `axes-reliability.Rmd.orig`, `axes-reliability-caveats.Rmd`, `sem-based-ssm-analysis.Rmd.orig` and `sem-latent-contrasts.Rmd.orig`. The 11 `.Rmd.orig` basenames equal the 11 names in `VIGNETTES`. All four pages are in the `_pkgdown.yml` articles list (lines 129-132) and navbar (193-199). `tools/check-pkgdown-vignettes.R` exit 0 (14 pages agree); `test-vignette-frame.R` 285 pass, 0 fail. Pass.
- AC2: `tools/check-vignette-split.R --pairs <subsection> --max-pairs 20` against merge base 045828f9, exit 0 on both. Axes: 232 base sentences, 234 across pages, 5 listed pairs. SEM: 227 base, 235 across pages, 6 listed pairs. Both report every sentence in exactly one page. Pass.
- AC3: `prose-sweep.R --prose | wc -w`: axes core 1047, caveats 2534, SEM core 2014, contrasts 1665. All at most 2600. Pass.
- AC4: same split-guard runs. Axes: 4 base chunks outside the preamble, 0 added. SEM: 11 base chunks, 1 added (`lavaan-note-contrasts`, listed as an added chunk in M137-D2). Every base chunk in exactly one page byte for byte. Pass.
- AC6: `check-vignette-staleness.R` exit 0, `check-vignette-width.R` exit 0 (11 pre-computed pages within 80 columns), `prose-sweep.R` exit 0 on each of the four sources. Pass.
- AC5: the three axes tests read `axes-reliability-caveats.Rmd` (`test-axes-scaled-fit.R:980` and `test-axes-corrected-se.R:752` through `vignette_source()`, `test-axes-reliability.R:3117` through `test_path()`). `devtools::test()` 0 failures, 11319 pass; its one skip (`test-axes-scaled-fit.R:930`, fixture environment gate) does not read the vignette. `EXEMPT` names `sem-based-ssm-analysis`, whose rendered `.Rmd:107-108` holds the `cx =~` and `cy =~` lines. The AC5 grep, filtered to `vignette()` calls, `articles/` links and `vignettes/<page>.Rmd` mentions, returns 19 lines. Each was read against the page: the two roxygen pointers name the page with the calibration table, the four navbar links exist, and every `:<line>` in `INDEX`, `browne1992` and `wendt2019` Traces holds its content on that line of the rendered `.Rmd`. Pass.
- AC7: `devtools::test()` 0 failures (11319 pass, 1 environment skip, 11 CPM Hessian warnings from `test-ci_accuracy.R`). `devtools::check(args = "--no-manual")` at 7e023c82 (code identical to 8eeb80fa): 0 errors, 0 warnings, 0 notes, vignette re-build OK; the merge base reported 0 notes at M136. NEWS.md Documentation entry names "SEM-Based SSM Analysis", "Latent Group Contrasts", "Axes Reliability" and "Axes Reliability Caveats", and says the first and third are each now two pages. Pass.
- Consistency gate: `cairn_validate.py` exit 0. `document()` with `cli.width = 500`: 0 `resolve link` lines, no diff. README.Rmd untouched. `check_pkgdown()` no problems. Master watches: newest R-CMD-check push run with a verdict (56d8a147) success, newest test-coverage run success. `check-master-red-alert.R`, `master-red-alert-dryrun.R` and `check-branch-protection.R` exit 0. No new top-level files. GP5 touched, not changed, so no impact report.
- Independent review (three lenses, user-facing tier). Blame-history [S]: no findings. Prior-review [S]: 1 finding. Diff-bug [O]: 13 findings. Dispositions below are proposed, and the step-7 gate settles them.
- P1 [S prior-review] stale `wendt2019`/`browne1992` line citations for the contrasts page: proposed reject. The reviewer read the 282-line `.Rmd.orig`; the citations name the 344-line rendered `.Rmd`, where each cited line holds its content (re-read in this review).
- O1 `wendt2019.md` claims table (`:44`, `:114`, `:368`, `:394-397`) and Open questions (`:394`, `:396-397`) still carry master's line numbers, which matched master's Traces: proposed fix now.
- O2 `INDEX.md:8` says `browne1992` traces to "both vignettes", now three: proposed fix now.
- O3 the accepted-instrument material (equal spacing, quasi-circumplex and three-scale refusals, single-item scales, SYMLOG) sits only in the caveats Wrap-up: proposed follow-up.
- O4 caveats page "Four properties ... Sections 2 to 5 take them in turn" now sits inside Section 2 and leaves out Section 6: proposed follow-up (changing it needs new split pairs).
- O5 caveats headings 3 to 5 are each followed by a bold lead-in that restates them: proposed follow-up with O4.
- O6 caveats setup comment says chunks that fit the model are gated on lavaan, but the page has no such chunks: proposed fix now (setup chunk is outside the split comparison, live `.Rmd`, no re-render).
- O7 axes core Level line names "Latent Group Contrasts" as prerequisite though no content depends on it: proposed reject, the M137-D1 reading chain.
- O8 axes core pointer could name Section 5 of the caveats page: proposed reject, style.
- O9 NEWS "every code chunk kept" while the contrasts page adds one chunk: proposed reject, every chunk is kept and the statement is true.
- O10 contrasts "elevation or amplitude difference whose interval excludes zero is a difference": proposed reject, moved unchanged, and it concerns linear parameters, not an angular CI.
- O11 reworded source lines past 80 columns: proposed reject, formatting nit no guard reads.
- O12 Scope section numbers ignore the Overview as Section 1: proposed reject, plan-owned record with no effect on pages.
- O13 D-033/D-034 cite the old axes path: proposed reject, IP4 history.
