# M133: Fit vignette output reads cleanly

- **Status:** in-progress
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

- [ ] AC1: `names(cpm_fit(...)$results)` is unchanged from master, shown by an existing or new test asserting the name vector.
- [ ] AC2: At `options(width = 77)`, `print()` and `summary()` of each CPM fixture in `test-cpm_api.R` and `test-cpm_summary_markers.R` show the estimated-angles table as one header row plus one row per scale. A test asserts this. The tests titled "byte-identical to merge-base" are renamed to say what they now pin.
- [ ] AC3: A test shows that the values in the printed CPM table equal `cpm_round_df(results)` column by column, in the same order. The test states the one-to-one map from old to new header names (for example `Angle_theory` to `Theory`).
- [ ] AC4: At `options(width = 77)`, every line of `print()` for `cache$ci`, `ci_guard`, `ci_cpm` and `ci_contrast` (from `helper-caution-fixtures.R`) is at most 77 display columns. A test measures this with `nchar(type = "width")`. The vignette's rendered `print(acc)` output passes `tools/check-vignette-width.R`.
- [ ] AC5: The committed rendered `.Rmd` has three chunks whose output contains "CPM Hessian is ill-conditioned" (the jz2017 fit, the model variants and `acc`). The prose before each such chunk has a sentence that says the warning will appear and names the section *When a fit sits at a boundary*.
- [ ] AC6: In the rendered vignette, the first chunk that prints `acc` calls `print(acc)`, and a later chunk calls `summary(acc)`. The bullet list that explains the coverage table, the `Condition` ladder, the `` `cert` `` column and `Verdicts (c = 1` comes after the `summary(acc)` chunk. A grep for `` `cert` ``, `Condition = `, `coverage table`, `Structure note` and `Verdicts (c = 1` finds no line before that chunk.
- [ ] AC7: `devtools::test()` reports 0 failures. `devtools::check(args = "--no-manual")` reports 0 errors and 0 warnings. `tools/check-vignette-width.R` and `tools/check-vignette-staleness.R` pass. NEWS.md has an entry for both layout changes.

## Coverage

- AC1 → T1
- AC2 → T1, T4
- AC3 → T1
- AC4 → T2, T4
- AC5 → T3, T4
- AC6 → T3, T4
- AC7 → T4

## Tasks

- [x] T1: Write tests first that pin `names(results)` and the one-block CPM table. In R/cpm_oop.R, print a display copy of `results` with shorter headers (for example `Theory`, `lci`, `uci`) in both methods. Rename the "byte-identical to merge-base" tests in `test-cpm_summary_markers.R` and update the two snapshot files. Add the value-equality test for AC3. Sweep `git grep` hits for the old header names in docs and prose.
- [x] T2: Write a width test first for `print.circumplex_ci_accuracy()` over the four named fixtures. Wrap its header line with `wrap_prose()` (R/utils.R:267). Update snapshots.
- [ ] T3: Edit `vignettes/evaluating-circumplex-structure.Rmd.orig`. Add the warning sentences before the three chunks that emit the Hessian warning (jz2017 fit, model variants, accuracy). Use `print(acc)` in the main chunk. Add a `summary(acc)` chunk after the verdict discussion, and move the text about the ladder, the `cert` column and the table after it.
- [ ] T4: Re-render with `tools/precompute-vignettes.R`. Run the width guard over the rendered `print(acc)`, then run both vignette guards, the tests and the check. Add the NEWS.md entry.

## Work log

- 2026-09-16: created by /milestone-plan.
- 2026-09-16: plan gate chose announcing the Hessian warning over investigating its cause or changing the example, because the boundary section already teaches it as a signal. Falsified by evidence the condition number comes from an estimator defect rather than the Heywood NO scale.
- 2026-09-16: plan gate chose shorter printed headers over dropping the Communality column or a smaller site font, because it keeps every number and the 80-column basis of M132. Falsified by a header set that cannot fit 77 columns for realistic scale names.
- 2026-09-16: plan gate chose showing `print(acc)` first over shortening `print()` or hiding untrustworthy values, because `print()` already omits the table and settings without dropping any verdict. Falsified by readers still finding the verdict blocks too long.
- 2026-09-16: criteria audit (full mode, fresh reader) returned five findings: AC2 bound to snapshots with no width and silently dropped byte-identical tests, AC3 checked by reading a diff, AC4 open fixture set, AC5 missed the model-variants chunk, AC6 grep matched verdict text. All five fixed by rewording AC2 to AC6 and T1 to T4.
- 2026-09-16: implement gate chose headers Scale, Theory, Angle, lci, uci, Zeta, lci, uci, Communality (66 columns on jz2017) over keeping Angle_lci/Zeta_lci with Comm (76 columns), because it leaves room for longer scale names and VarRatio. Falsified by readers confusing the two lci/uci pairs.
- 2026-09-16: T1 done. `cpm_display_results()` in R/cpm_oop.R, `expect_cpm_table_one_block()` helper added to both snapshot test files (7 fixtures), snapshots re-captured, M94 byte-identical tests renamed. devtools::test() 0 failures.
- 2026-09-16: T2 done. `print.circumplex_ci_accuracy()` wraps its header with `wrap_prose()`. New test-ci_accuracy_print_width.R covers the four fixtures. Its now-unprinted header entry left the M131 width ledger in test-print-width.R. devtools::test() 0 failures.

## Decisions

## Review
