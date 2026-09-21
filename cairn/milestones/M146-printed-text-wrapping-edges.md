<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M146: Printed text wraps at its edge cases, and the CPM table drops Communality

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP4   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — changes what print(), summary(), items() and scales() show in the console   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m146-printed-text-wrapping-edges   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Close the print-side items (ii)-(viii) of the code-box-width candidate row, so that the package's printed text stays inside the reader's console width at the edge cases those items name.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** item text in `items()` and `scales(items = TRUE)`, plus the `scales()` crash on licensed instruments; the three `strwrap()` sites in `R/ssm_sem.R`; the scaled-fit note in `summary()` of `axes_reliability()`; the Heywood note and the bootstrap fired-marker note of `cpm_fit()`; the Communality column of the printed CPM results table; the `wrap_prose()` and `ssm_ci_cat_line()` edge cases. These are layout changes under D-056 and D-057, with the column change under D-057.

**Out:** the width-guard gaps (i) and (ix): they stay in the code-box-width candidate row. The spliced-failure-reason half of (iv) has no defect, because every reason is one token (`singular` … `infinite_diagonal`), so no break falls inside it. The work log records it as dropped, not deferred. `fit_structure()` keeps its own printed Communality column, because its table is not the CPM table.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: `items()` and `scales(items = TRUE)` wrap each item line, and `items()` its Prefix and Suffix lines, to `getOption("width")`, with continuation lines indented under the first character of the text after the label. `scales(items = TRUE)` on an instrument whose `$Items` holds only a notice row (`iip32`, `iip64`) prints its scale lines, then the notice once, instead of stopping with "subscript out of bounds", and a regression test asserts that output. A test loops over every instrument `instrument_names()` returns, calling both functions at widths 40, 60 and 80, and finds no item, Prefix or Suffix line wider than the width except a line holding one word wider than the room left after its indent.
- [ ] AC2: `R/ssm_sem.R` holds no `strwrap()` call (`grep -n strwrap R/ssm_sem.R` prints nothing). The ΔCFI note, the `Verdict:` line and labeled facts of `sem_format_verdict()`, the invariance-ladder notes and the stored-verdict fallback in `sem_print_invariance()` all wrap through `wrap_prose()`. A test calls each of those four formatters directly with text containing double-width characters at every width from 30 to 120 and finds every printed line within the width in display columns, except a line holding one word wider than the room after its indent.
- [ ] AC3: In `summary()` of an `axes_reliability()` fit whose fit statistics were scaled, the sentence beginning "They follow lavaan's" starts a printed line at every width from 30 to 120, and a test sweeps those widths.
- [ ] AC4: In `print()` and `summary()` of a `cpm_fit()` result, the Heywood note never breaks inside "(ζ > 0.995," at any width from 30 to 120. In `summary()`, the bootstrap note opening "Note: boundary/weak-identification markers fired:" breaks its opening clause between words, and at every width from 30 to 120 it prints within the width except on a line holding a single marker label wider than the room after its prefix. Tests sweep those widths over the Heywood and bootstrap-marker fixtures of `test-cpm_summary_markers.R`, including one that fires `small_beta`, the longest label.
- [ ] AC5: `print()` and `summary()` of a `cpm_fit()` result no longer print the Communality column, and `fit$results` still holds it. A new fixture with eight 16-character scale names, free scaling and analytic intervals joins the M133 one-block test, and on that fixture the results table prints in one block at width 77. The M133 one-block test passes over all its fixtures, with Communality left out of the expected header.
- [ ] AC6: `wrap_prose()` refuses `width = Inf` and a `prefix` or `continuation` containing a tab, each test asserting that the error names the refused argument. `ssm_ci_cat_line()` given empty text prints its label on a line of its own.
- [ ] AC7: NEWS.md carries entries naming the print methods whose output changed, the dropped Communality column and the `scales()` fix. The frozen CPM results tables in `evaluating-circumplex-structure.Rmd` and `cpm-boundary-fits.Rmd` are regenerated from a live run, and no frozen output line in those two files shows Communality (`grep -n '^#>.*Communality' vignettes/evaluating-circumplex-structure.Rmd vignettes/cpm-boundary-fits.Rmd` prints nothing). The verify and check commands in `cairn/PROFILE.md` are clean.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T6
- AC2 → T5
- AC3 → T2
- AC4 → T3
- AC5 → T4
- AC6 → T1
- AC7 → T4, T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. -->

- [x] T1: Tests first in `test-wrap-prose.R` and the `ssm_ci_cat_line()` class of `helper-caution-fixtures.R`, then the fix. `wrap_prose()` (R/utils.R:267) refuses a non-finite `width` and a tab in `prefix` or `continuation`. `ssm_ci_cat_line()` (R/ssm_ci_oop.R:32) prints its leader alone when `text` is empty.
- [x] T2: Split `axes_fit_scaled_note` (R/axes_reliability_oop.R:113) into two elements so the lavaan sentence is its own paragraph. Add a width-sweep test (30-120) on a scaled fit asserting a line starts with "They follow lavaan's".
- [x] T3: In R/cpm_oop.R, hand the Heywood note to `wrap_prose(atomic = TRUE)` as words, with "(ζ > 0.995," as one unit. Hand the bootstrap marker note's opening clause over as words, not one 49-column unit. Add width sweeps (30-120) in `test-cpm_summary_markers.R`, including a `small_beta` fixture.
- [x] T4: Remove Communality from `cpm_display_results()` (R/cpm_oop.R:117) and from `expect_cpm_table_one_block()`'s expected header (helper-cpm-table.R). Add the eight-name, 16-character, free-scaling, analytic-interval fixture to the M133 one-block test. Grep R/ roxygen, `man/` sources and vignette prose for text that describes the printed CPM table's columns, and update each hit. Note that "communality index" names ζ and stays.
- [x] T5: Replace the `strwrap()` calls at R/ssm_sem.R:803, :814 and :1894 with `wrap_prose()`, and route the `Verdict:` line and the stored-verdict fallback through it. Add a test that calls each formatter directly with double-width text across widths 30-120.
- [x] T6: Regression test first for `scales(iip32, items = TRUE)`. Then, in R/instrument_oop.R (`scales()` :66, `items()` :100), print a notice-only instrument's notice once after the scale lines, and wrap item, Prefix and Suffix lines with `wrap_prose()` using a hanging indent. Add the `instrument_names()` loop test at widths 40, 60 and 80.
- [x] T7: Re-capture the changed snapshots and read each diff. Line breaks and the Communality column must be the only changes (D-056, D-057). Regenerate the precomputed vignettes' frozen output through the repo's precompute script. Add NEWS.md entries. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-21: created by /milestone-plan from the code-box-width candidate row (items (ii)-(viii)), promoted by the 2026-09-21 triage pass.
- 2026-09-21: criteria audit (full mode, [O] reader, two passes). Pass 1 found that `scales(items = TRUE)` crashes on iip32 and iip64, that the spliced-reason clause had no defect, that the M133 helper rejects a dropped column, and that nothing can satisfy the snapshot identity check. Pass 2 found that the AC2 probe skips three paths, that AC4 named the marker note ambiguously, that AC5 had no 16-character fixture, and that the AC7 grep was bound to one line shape. All were fixed in the wording above. It also noted the PDF manual check, which is left to release.
- 2026-09-21: plan gate chose to drop Communality from the printed CPM table for all fits over dropping it for free-scaling fits only (fixed-scaling tables still reach 78 columns) and over keeping the column (item vii becomes an accepted limitation); falsified by a user report that needs Communality printed, or a 16-character table still splitting at width 77.
- 2026-09-21: plan gate folded the `scales()` notice-row crash into this milestone over a separate /hotfix, because T6 rewrites the same loop; falsified by a user report of the crash before this milestone merges.
- 2026-09-21: plan chose protected units in `wrap_prose(atomic = TRUE)` for "(ζ > 0.995," over rewording the Heywood note, because rewording changes a caution's text for a layout defect; falsified by the unit leaving a line past the width at a swept width.
- 2026-09-21: item (iv)'s spliced-failure-reason half was dropped as no defect: every reason `axes_reliability()` splices is a single token, so `wrap_prose()` cannot break inside it; falsified by a multi-word reason reaching those notes.
- 2026-09-21: T1 done. `wrap_prose()` refuses a non-finite width and a tabbed prefix or continuation (named `stopifnot` messages), and `ssm_ci_cat_line()` prints its label alone on empty text. Tests went red first, then green. The empty-label test sits in `test-wrap-prose.R`, not the caution-fixture ledger, because it is a direct unit test. Full suite: 0 failures.
- 2026-09-21: T2 done. `axes_fit_scaled_note` is two elements, so the lavaan sentence starts a line. The 30-120 width sweep in `test-axes-scaled-fit.R` failed first and now passes. The full suite ran once over T2-T4 together (0 failures), because an earlier T2-only run was stopped when it began reading T3/T4 test edits.
- 2026-09-21: T3 done. The Heywood note keeps "(ζ > 0.995," as one atomic unit through a `keep` argument on `cpm_diagnostic_lines()`'s note helper, and the bootstrap marker note hands its opening clause over as words. Both 30-120 sweeps in `test-cpm_summary_markers.R` failed first and now pass. Width-80 snapshots are unchanged.
- 2026-09-21: T4 done. `cpm_display_results()` drops Communality, and `expect_cpm_table_one_block()` asserts that the object keeps it and leaves it out of the expected header. The new `free_long` fixture (eight 16-character names, free scaling, analytic intervals) prints in one 74-column block; with Communality it measured about 86. Re-captured `cpm_api.md` and `cpm_summary_markers.md`: a script confirmed every changed line equals the old line minus one token. The doc sweep (`grep` over R/ roxygen, man/ and vignette prose) found no text describing the printed columns; "communality index" names ζ and stays. Committed with T3 because both edit `R/cpm_oop.R`.
- 2026-09-21: item (iii)'s premise is false. `strwrap()` measures words with `nchar(type = "w")` (read from its deparsed source, and a 30-word double-width string wrapped at width 40 gave a 39-column maximum), so it already counted display columns. The real defects in `R/ssm_sem.R` were the unwrapped `Verdict:` line, the 20-column floor on labeled values, and strwrap's break one column short of the width. AC2 still holds as written. The candidate row's wording of (iii) is corrected when the row is rewritten at post-merge hygiene.
- 2026-09-21: T5 done. The three `strwrap()` sites and the `Verdict:` line wrap through `wrap_prose()`, and `grep -n strwrap R/ssm_sem.R` prints nothing. The new `test-ssm_sem_print_wrap.R` drives `sem_dcfi_note()`, `sem_format_verdict()` and `sem_print_invariance()` (rung notes and the stored-verdict fallback) over widths 30-120. The Verdict test failed first. The notes and fallback test passed before the fix too, which matches the refuted premise above. The Delta-CFI note has no caller text, so its test guards the width only. Full suite (T5 and T6 together): 0 failures, and no snapshot moved.
- 2026-09-21: T6 done. `scales(items = TRUE)` prints a notice-only instrument's notice once after the scale lines. The regression test failed first on the reported `subscriptOutOfBoundsError`. Item, Prefix and Suffix lines wrap through a new internal `cat_item()` with a hanging indent. The new `test-instrument_wrap.R` loops over all 15 `instrument_names()` at widths 40, 60 and 80. No shipped item text is empty, so no item line is dropped. The tests sit in a new file so the full-suite run in progress did not read them.
- 2026-09-21: T7 done. NEWS.md has four entries under "Minor improvements and fixes". All 11 precomputed vignettes were re-rendered from an installed build. Kept: the three CPM tables without Communality, in `evaluating-circumplex-structure` and `cpm-boundary-fits`, and the lavaan sentence on its own line in `axes-reliability`. Restored: an elapsed-time line in `ci-accuracy` and four figure PNGs that changed from render noise only. The AC7 grep prints nothing. `tools/check-vignette-width.R` passes for all 11. `devtools::document()` makes no diff. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes.
- 2026-09-21: claim audit: 31 claims read, 3 corrected — R/cpm_oop.R, tests/testthat/test-cpm_summary_markers.R, tests/testthat/test-ssm_sem_print_wrap.R
- 2026-09-21: the audit measured master's `free_long` table at 84 columns, not the 87 in the plan gate and in two comments, nor the "about 86" in the T4 line above. Both comments now say 84. The one-block result stands: 72 columns without Communality.
- 2026-09-21: review return 1 (defect): AC4 fails as written. Its width sweeps cover only `m94_boot_jz()`, not the file's other Heywood fixture (analytic `hey`) or its other marker-firing bootstrap fixture (`m94_boot_big()`). Every other criterion passed fresh evidence, and the suite and check are clean. Twelve reviewer findings are logged untriaged in the Review section.
- 2026-09-21: return 1 fixed. The Heywood sweep in `test-cpm_summary_markers.R` now runs over both Heywood fixtures (analytic `hey` and `m94_boot_jz()`), and the marker sweep over both marker-firing bootstrap fixtures (`m94_boot_jz()` and `m94_boot_big()`), each asserting its fixture fires what the comment says. 366 and 592 expectations, 0 failures. No red run was possible: the behavior already held (pass-1 probe), so this closes a coverage gap. Full suite: 0 failures. The twelve findings stay for the review gate.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive -->

### Pass 1 (2026-09-21): returned to in-progress, AC4 unmet as written

No box is ticked on this pass. The next pass re-verifies every criterion fresh.

Evidence gathered before the return:

- AC1: an independent probe over all 15 instruments at widths 40, 60 and 80 read 4,260 item, Prefix and Suffix lines: 0 over the width, 528 first continuation lines, 0 not under the text's first character. `scales(iip32/iip64, items = TRUE)` prints 10 lines, the notice last. `test-instrument_wrap.R`: 3 tests, 0 failures.
- AC2: `grep -n strwrap R/ssm_sem.R` prints nothing. `test-ssm_sem_print_wrap.R`: 3 tests, 0 failures.
- AC3: the M146 test in `test-axes-scaled-fit.R` ran 91 expectations, 0 failures, not skipped.
- AC4: FAILS as written. The criterion asks for sweeps "over the Heywood and bootstrap-marker fixtures of `test-cpm_summary_markers.R`, including one that fires `small_beta`". Both sweeps use `m94_boot_jz()` only. The file's other Heywood fixture (the analytic `cpm_oracle_voc()` fit, `hey`) and its other marker-firing bootstrap fixture (`m94_boot_big()`, fires `small_beta`) are not swept. A probe found the behavior holds on `hey` (182 print/summary sweeps with the Heywood note, 0 broken inside the ζ clause). The gap is test coverage.
- AC5: the one-block test ran 231 expectations over all fixtures, `free_long` included, 0 failures.
- AC6: the probe gave "is.finite(width) is not TRUE", "`prefix` must not contain a tab", "`continuation` must not contain a tab", and `ssm_ci_cat_line("Label", "")` printed `    Label`. `test-wrap-prose.R`: 22 tests, 0 failures.
- AC7: the Communality grep prints nothing. NEWS.md carries the entries.
- Full suite: 0 failures. `devtools::check()`: 0 errors, 0 warnings, 0 notes.
- Gate: `cairn_validate` passes. `document()` makes no diff and 0 `resolve link` lines. `check_pkgdown()` finds no problems. README is in sync. The master-red-alert audits and the branch-protection check exit clean. Master watch: the newest R-CMD-check push run (cd5d2881) is red because Pandoc failed to download (HTTP 504) on macOS and ubuntu release. Its failed jobs were rerun and were still in progress at the return. test-coverage is green.

Reviewer findings (reported, not yet triaged; carried to the next gate):

- [O]1 `test-instrument_wrap.R:52-73`: the hanging-indent test runs one assertion, because every check sits in an `if` that the chosen lines skip.
- [O]2 `test-instrument_wrap.R:31-50`: the width loop passes if item text stops printing, because it checks only lines that are too wide.
- [O]3 `R/ssm_sem.R:816-820`: `labeled()` now drops a label whose value is `""` (latent, since no caller builds an empty fact today).
- [O]4 `R/utils.R:290`: the `is.finite(width)` refusal has no named message, unlike the tab refusals.
- [O]5 `R/utils.R:275-282`: the lead check refuses tabs but not `\n`, `\r` or other zero-width characters.
- [O]6 `test-cpm_summary_markers.R:360-411`: the AC4 sweeps use one fixture (the AC4 failure above).
- [O]7 `test-ssm_sem_print_wrap.R:26-31`: the Delta-CFI sweep also passed on master (the work log says so).
- [O]8 NEWS.md CPM entry: "a table with long scale names fits in 77 columns" is broader than the tested 16-character case. Names of 20 characters pass 77.
- [O]9 `R/utils.R:258-262`: the `wrap_prose()` comment names only the marker note as an atomic-mode user.
- [O]10 the CPM `Fit:` line is not wrapped (90 columns at width 77). It is outside the diff.
- [S-prior]1 the dropped Communality column contradicts M133's choice of shorter headers. It is an intentional plan-gate change.
- [S-prior]2 wrapping the instrument printers contradicts M131's scope, which left them out. It is an intentional scope choice.
- [S-blame]: no findings.
