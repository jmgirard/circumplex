<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M146: Printed text wraps at its edge cases, and the CPM table drops Communality

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
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
- [ ] T3: In R/cpm_oop.R, hand the Heywood note to `wrap_prose(atomic = TRUE)` as words, with "(ζ > 0.995," as one unit. Hand the bootstrap marker note's opening clause over as words, not one 49-column unit. Add width sweeps (30-120) in `test-cpm_summary_markers.R`, including a `small_beta` fixture.
- [ ] T4: Remove Communality from `cpm_display_results()` (R/cpm_oop.R:117) and from `expect_cpm_table_one_block()`'s expected header (helper-cpm-table.R). Add the eight-name, 16-character, free-scaling, analytic-interval fixture to the M133 one-block test. Grep R/ roxygen, `man/` sources and vignette prose for text that describes the printed CPM table's columns, and update each hit. Note that "communality index" names ζ and stays.
- [ ] T5: Replace the `strwrap()` calls at R/ssm_sem.R:803, :814 and :1894 with `wrap_prose()`, and route the `Verdict:` line and the stored-verdict fallback through it. Add a test that calls each formatter directly with double-width text across widths 30-120.
- [ ] T6: Regression test first for `scales(iip32, items = TRUE)`. Then, in R/instrument_oop.R (`scales()` :66, `items()` :100), print a notice-only instrument's notice once after the scale lines, and wrap item, Prefix and Suffix lines with `wrap_prose()` using a hanging indent. Add the `instrument_names()` loop test at widths 40, 60 and 80.
- [ ] T7: Re-capture the changed snapshots and read each diff. Line breaks and the Communality column must be the only changes (D-056, D-057). Regenerate the precomputed vignettes' frozen output through the repo's precompute script. Add NEWS.md entries. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

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

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive -->
