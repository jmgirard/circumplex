# M133: Fit vignette output reads cleanly

- **Status:** review
- **Priority:** normal
- **Depends on:** M132
- **Driving RR:** —
- **Principles touched:** GP4
- **Resolves:** —
- **Surface tier:** user-facing — changes `print()`/`summary()` output of `cpm_fit()` and `ssm_ci_accuracy()` objects and a vignette
- **Branch/PR:** m133-fit-vignette-output

## Goal

In the evaluating-circumplex-structure vignette, readers see the CPM table in one block, a notice before each Hessian warning, and the short accuracy report first.

## Scope

**In:** The CPM results table in `print.circumplex_cpm()` and `summary.circumplex_cpm()` (R/cpm_oop.R:128, :173) gets shorter printed headers, so it fits in 77 columns. The returned `results` data frame keeps its names. The header line of `print.circumplex_ci_accuracy()` (R/ssm_ci_oop.R:384) wraps. The vignette source `vignettes/evaluating-circumplex-structure.Rmd.orig` gets one sentence before each chunk that emits the ill-conditioned-Hessian warning. Its accuracy chunk shows `print(acc)`, and `summary(acc)` moves to a later chunk that the text introduces as the full report. The milestone also re-renders, updates snapshots and adds a NEWS.md entry. D-056 and D-057 cover all layout changes.

**Out:** The cause of the ill-conditioned jz2017 Hessian. The gate chose to announce the warning. If the implementer finds evidence of an estimator defect, it goes to `/hotfix` or a candidate row. Shorter `print.circumplex_ci_accuracy()` output beyond its header, and hiding untrustworthy values, were declined at the gate, with no row. A smaller site code font was declined at the gate, with no row. Width-guard hardening stays in its own candidate row.

## Acceptance criteria

- [x] AC1: `names(cpm_fit(...)$results)` is unchanged from master, shown by an existing or new test asserting the name vector.
- [x] AC2: At `options(width = 77)`, `print()` and `summary()` of each CPM fixture in `test-cpm_api.R` and `test-cpm_summary_markers.R` show the estimated-angles table as one header row plus one row per scale. A test asserts this. The tests titled "byte-identical to merge-base" are renamed to say what they now pin.
- [x] AC3: A test shows that the values in the printed CPM table equal `cpm_round_df(results)` column by column, in the same order. The test states the one-to-one map from old to new header names (for example `Angle_theory` to `Theory`).
- [x] AC4: At `options(width = 77)`, every line of `print()` for `cache$ci`, `ci_guard`, `ci_cpm` and `ci_contrast` (from `helper-caution-fixtures.R`) is at most 77 display columns. A test measures this with `nchar(type = "width")`. The vignette's rendered `print(acc)` output passes `tools/check-vignette-width.R`.
- [x] AC5: The committed rendered `.Rmd` has three chunks whose output contains "CPM Hessian is ill-conditioned" (the jz2017 fit, the model variants and `acc`). The prose before each such chunk has a sentence that says the warning will appear and names the section *When a fit sits at a boundary*.
- [x] AC6: In the rendered vignette, the first chunk that prints `acc` calls `print(acc)`, and a later chunk calls `summary(acc)`. The bullet list that explains the coverage table, the `Condition` ladder, the `` `cert` `` column and `Verdicts (c = 1` comes after the `summary(acc)` chunk. A grep for `` `cert` ``, `Condition = `, `coverage table`, `Structure note` and `Verdicts (c = 1` finds no line before that chunk.
- [x] AC7: `devtools::test()` reports 0 failures. `devtools::check(args = "--no-manual")` reports 0 errors and 0 warnings. `tools/check-vignette-width.R` and `tools/check-vignette-staleness.R` pass. NEWS.md has an entry for both layout changes.

## Coverage

- AC1 → T1
- AC2 → T1, T4, T5
- AC3 → T1
- AC4 → T2, T4
- AC5 → T3, T4
- AC6 → T3, T4
- AC7 → T4, T6

## Tasks

- [x] T1: Write tests first that pin `names(results)` and the one-block CPM table. In R/cpm_oop.R, print a display copy of `results` with shorter headers (for example `Theory`, `lci`, `uci`) in both methods. Rename the "byte-identical to merge-base" tests in `test-cpm_summary_markers.R` and update the two snapshot files. Add the value-equality test for AC3. Sweep `git grep` hits for the old header names in docs and prose.
- [x] T2: Write a width test first for `print.circumplex_ci_accuracy()` over the four named fixtures. Wrap its header line with `wrap_prose()` (R/utils.R:267). Update snapshots.
- [x] T3: Edit `vignettes/evaluating-circumplex-structure.Rmd.orig`. Add the warning sentences before the three chunks that emit the Hessian warning (jz2017 fit, model variants, accuracy). Use `print(acc)` in the main chunk. Add a `summary(acc)` chunk after the verdict discussion, and move the text about the ladder, the `cert` column and the table after it.
- [x] T4: Re-render with `tools/precompute-vignettes.R`. Run the width guard over the rendered `print(acc)`, then run both vignette guards, the tests and the check. Add the NEWS.md entry.
- [x] T5: In `test-cpm_summary_markers.R`, add one test that runs `expect_cpm_table_one_block()` with both `print()` and `summary()` on all seven fixtures of the file (clean, hey, small, free, m94_boot_jz, m94_boot_big, m94_boot_clean).
- [x] T6: Reword the NEWS.md entry for the accuracy heading so that it does not contradict the first entry, which says headings stay unwrapped.

## Work log

- 2026-09-16: created by /milestone-plan.
- 2026-09-16: plan gate chose announcing the Hessian warning over investigating its cause or changing the example, because the boundary section already teaches it as a signal. Falsified by evidence the condition number comes from an estimator defect rather than the Heywood NO scale.
- 2026-09-16: plan gate chose shorter printed headers over dropping the Communality column or a smaller site font, because it keeps every number and the 80-column basis of M132. Falsified by a header set that cannot fit 77 columns for realistic scale names.
- 2026-09-16: plan gate chose showing `print(acc)` first over shortening `print()` or hiding untrustworthy values, because `print()` already omits the table and settings without dropping any verdict. Falsified by readers still finding the verdict blocks too long.
- 2026-09-16: criteria audit (full mode, fresh reader) returned five findings: AC2 bound to snapshots with no width and silently dropped byte-identical tests, AC3 checked by reading a diff, AC4 open fixture set, AC5 missed the model-variants chunk, AC6 grep matched verdict text. All five fixed by rewording AC2 to AC6 and T1 to T4.
- 2026-09-16: implement gate chose headers Scale, Theory, Angle, lci, uci, Zeta, lci, uci, Communality (66 columns on jz2017) over keeping Angle_lci/Zeta_lci with Comm (76 columns), because it leaves room for longer scale names and VarRatio. Falsified by readers confusing the two lci/uci pairs.
- 2026-09-16: T1 done. `cpm_display_results()` in R/cpm_oop.R, `expect_cpm_table_one_block()` helper added to both snapshot test files (7 fixtures), snapshots re-captured, M94 byte-identical tests renamed. devtools::test() 0 failures.
- 2026-09-16: T2 done. `print.circumplex_ci_accuracy()` wraps its header with `wrap_prose()`. New test-ci_accuracy_print_width.R covers the four fixtures. Its now-unprinted header entry left the M131 width ledger in test-print-width.R. devtools::test() 0 failures.
- 2026-09-16: T3 done. Warning notices before the cpm, variants and accuracy_run chunks. accuracy_run prints `print(acc)` with a short reading guide. New `#### The full report` subsection holds `summary(acc)`, the table/ladder/cert guide and the plot. Wrap-up checklist item 3 now points at `summary()` or `plot()` for the ladder.
- 2026-09-16: T4 done. Rendered after `R CMD INSTALL` (the first render read the old installed package and was discarded). Width guard: all 7 vignettes within 80 columns. Staleness guard: up to date. devtools::test() 0 failures. devtools::check(--no-manual) 0 errors, 0 warnings, 0 notes. NEWS.md has three entries.
- 2026-09-16: claim audit: 30 claims read, 2 corrected — vignettes/evaluating-circumplex-structure.Rmd.orig, vignettes/evaluating-circumplex-structure.Rmd (only fit_quasi of the variants warns; the cpm chunk prints two warnings). Prose fixed identically in source and render.
- 2026-09-16: review return 1 (defect): AC2 failed. No test asserts the one-block table for print() of the four analytic fits in test-cpm_summary_markers.R, for summary() of m94_boot_jz, or for m94_boot_big and m94_boot_clean with either method. Status back to in-progress. Reviewer findings [O] 2-11 await triage at the next review gate.
- 2026-09-16: minor amendment after review return 1: added T5 (AC2 assertions for all seven fixtures in test-cpm_summary_markers.R) and T6 (review finding [O] 3, NEWS contradiction). Coverage now maps AC2 to T5 and AC7 to T6.
- 2026-09-16: T5 done. New test runs the one-block check on 7 fixtures with print() and summary() (198 expectations, skip_on_cran only). Planted defect (cpm_display_results() with the old headers) turned it red.
- 2026-09-16: T6 done. The NEWS entry now says the accuracy heading wraps because it has no columns under it, and points at the first entry.
- 2026-09-16: claim audit: 7 claims read, 0 corrected — NEWS.md, tests/testthat/test-cpm_summary_markers.R (one NEWS line rewrapped to 80 columns).
- 2026-09-16: implement complete after return 1. devtools::test(): 0 failed, 0 errors, 1 skipped, 11034 passed. Status review.

## Decisions

## Review

Pass 1 (2026-09-16), branch in sync with origin/master (0 behind, 0 ahead).

- AC1: pass. The existing name-vector assertion at tests/testthat/test-cpm_api.R:70-72 is unchanged from master, and devtools::test() passed.
- AC2: FAIL. The behavior holds: an ad-hoc probe ran `expect_cpm_table_one_block()` for print() and summary() on m94_boot_jz, m94_boot_big, m94_boot_clean, the Heywood VOC fit and the cpm_api misfit fit, and all 10 passed. The test does not assert it for every fixture. test-cpm_summary_markers.R checks clean, hey, small and free with summary() only, jz with print() only, and never checks m94_boot_big or m94_boot_clean. The rename of the byte-identical tests is done.
- AC3: pass. helper-cpm-table.R states `cpm_table_header_map` as its own constant and compares every printed column with `cpm_round_df(results)` in order, with tolerance 0. It runs on all 7 asserted fixture/method pairs.
- AC4: pass. test-ci_accuracy_print_width.R checks every print() line of ci, ci_guard, ci_cpm and ci_contrast at width 77 with `nchar(type = "width")` and passed. tools/check-vignette-width.R: evaluating-circumplex-structure, 295 output lines, all fit.
- AC5: pass. The rendered .Rmd has the warning in three chunk outputs (lines 77, 432, 630: cpm, fit_quasi, acc). The prose before each names *When a fit sits at a boundary* (lines 63-65, 427-429, 616-618).
- AC6: pass. The first chunk that prints acc calls `print(acc)` (line 632), `summary(acc)` follows at line 724, and the bullet list is at lines 792-810. The five grep terms match only lines 731, 743, 792, 793, 808 and 810, all after line 724.
- AC7: pass. devtools::test(): 0 failed, 0 errors, 1 skipped, 10836 passed. devtools::check(args = "--no-manual"): 0 errors, 0 warnings, 0 notes. Width and staleness guards: all 7 vignettes pass. NEWS.md has entries for the CPM headers and the accuracy header.

Consistency gate: cairn_validate exit 0. document() made no diff and 0 `resolve link` lines. check_pkgdown() found no problems. README not touched. No new top-level files. Master R-CMD-check and test-coverage push runs: newest verdict success (7127001b). Master-red-alert audit and dry run exit 0. Branch protection matches.

Independent review (three reviewers). Dispositions are pending, because the AC2 failure returned the milestone before the approval gate:
- [O] 1: AC2 only partly asserted (same as the AC2 evidence above). Returned.
- [O] 2: most one-block checks carry skip_on_ci(). On CI, only the analytic cpm_api fit runs, which has no VarRatio column.
- [O] 3: NEWS.md contradicts itself: the wrap entry says headings stay unwrapped "because wrapping them would destroy their columns", and the new entry wraps the accuracy print() heading. Confirmed by reading NEWS.md:14-21.
- [O] 4: the new "How to read this output" paragraph says the lines classify coverage at the estimated amplitude, but the Guardrail line reports the 0 rung.
- [O] 5: the "The full report" intro omits the Near-zero regime note and the structural-zero note that summary() prints.
- [O] 6: the one-block property is tested only at digits = 3; larger digits can widen a free-scaling table past 77 columns.
- [O] 7: no help page says which lci/uci pair belongs to Angle and which to Zeta.
- [O] 8: helper-cpm-table.R splits on whitespace. A scale name with a space makes the helper fail when the table is correct.
- [O] 9: test-ci_accuracy_print_width.R reaches its builders through `environment(fx[[1]]$build)`.
- [O] 10: "This chunk prints two warnings" depends on bootstrap exclusions that can vary across BLAS on a re-render.
- [O] 11: nits. NEWS says "first line", but print() starts with a blank line. A code comment cites "(M133)".
- [S] blame-history: no findings. One nit: two sentences are joined on one source line at .Rmd.orig:513.
- [S] prior-review: no reintroduction. The wrap_prose() use on a heading is a planned extension of M131's scope. The change resolves ROADMAP candidate item (vii), the ledgered accuracy heading.

Pass 2 (2026-09-16), branch in sync with origin/master (merge-base 26fc9288, 0 behind). Resume route (d): no PR exists.

- AC1: pass. test-cpm_api.R:70-72 asserts the nine `results` names, the file has no diff in that block, and devtools::test() passed.
- AC2: pass. test-cpm_summary_markers.R:315-340 runs `expect_cpm_table_one_block()` with print() and summary() on all seven fixtures. The fixtures are clean, hey, small, free, boot_jz, boot_big and boot_clean. At width 77 the helper requires one `Scale` header row, one row per scale and a blank line after. It carries skip_on_cran() only. test-cpm_api.R:552-553 and :661-662 cover both methods for the analytic and bootstrap fits there. T5's work-log line records the planted defect turning it red. The tests formerly titled "byte-identical to merge-base" now read "matches its snapshot" (:75, :301).
- AC3: pass. helper-cpm-table.R:7-11 states the header map as its own constant: Angle_theory to Theory, the two `_lci` names to lci, the two `_uci` names to uci. The helper compares each printed column with `cpm_round_df(results)` in order, tolerance 0.
- AC4: pass. test-ci_accuracy_print_width.R passed in the suite. tools/check-vignette-width.R: evaluating-circumplex-structure 295 output lines, all fit. All 7 vignettes fit in 80 columns.
- AC5: pass. Rendered .Rmd warning lines 77, 432, 630. Prose naming *When a fit sits at a boundary* at lines 65 and 617, and the variants notice before line 432 (pass 1 line 427-429, file unchanged since).
- AC6: pass. `print(acc)` at line 632, `summary(acc)` at line 724. The five grep terms match only lines 731, 743, 792, 793, 808, 810.
- AC7: pass. devtools::test(): FAIL 0, WARN 11, SKIP 1, PASS 11034. devtools::check(args = "--no-manual"): 0 errors, 0 warnings, 0 notes. Width guard and staleness guard: all 7 vignettes pass. NEWS.md has the CPM-header entry and the accuracy-heading entry.

Consistency gate: cairn_validate exit 0 (all checks passed). document() made no diff, 0 `resolve link` lines. check_pkgdown(): no problems. README not touched. New files are both under tests/testthat. Master push runs, newest verdict: R-CMD-check success and test-coverage success (7127001b). check-master-red-alert.R, master-red-alert-dryrun.R and check-branch-protection.R exit 0.

Independent review, pass 2 (three fresh reviewers). Pass-1 findings [O] 1 and [O] 3 are fixed by T5 and T6. Candidate findings, ranked:
- [O] a: NEWS.md:21 (M131 entry) says column headers are unchanged, and the new entry changes the CPM table's column headers.
- [O] b: the one-block claim has little headroom. A probe at width 77 used `scaling = "free"` and IIP-length names (longest 16 characters). VarRatio printed in a second block. The same names with unit scaling fit (75 columns), and free scaling with 7-character names fits (75 columns). No fixture uses long names.
- [O] c / [S] prior-review 1: the one-block property is tested only at digits = 3 (pass-1 [O] 6, still open).
- [O] d / [S] prior-review 3: the vignette says `summary()` "adds the settings", but it prints a different settings paragraph and a Near-zero regime note that the text never mentions (pass-1 [O] 5).
- [S] prior-review 2: pass-1 [O] 4 (Guardrail line reports the 0 rung) has no recorded fix. The [O] reviewer checked the prose against the rendered output and found it consistent.
- [O] e: test-cpm_summary_markers.R:7 says the helper checks that values are "unchanged", but it compares with the same object, not with master.
- [O] f: test-ci_accuracy_print_width.R:7 reaches builders through `environment(fx[[1]]$build)` (pass-1 [O] 9).
- [O] g: .Rmd.orig:513 is a 112-character prose line (pass-1 [S] nit).
- [O] h: "This chunk prints two warnings" rests on a bootstrap-exclusion count that can change across BLAS builds (pass-1 [O] 10).
- [O] i: AC3's "one-to-one" wording is loose, because two old names map to `lci` and two to `uci`.
- [O] j: the skip_on_ci() blocks at test-cpm_summary_markers.R:91-108, :309 repeat checks the new CI block already runs.
- [O] k: the vignette does not say why only `fit_quasi` of the three variants warns (outside scope).
- Carried from pass 1, not re-raised: [O] 7 (no help page says which lci/uci pair belongs to which estimate), [O] 8 (helper splits on whitespace), [O] 11 (NEWS "first line" nit, "(M133)" code comment).
- [S] blame-history: no finding contradicts a past change or decision. D-056 and D-057 cover the header change. Removing the accuracy heading from the width ledger is correct.

No finding shows an acceptance criterion failing. Dispositions are recorded at the approval gate.
