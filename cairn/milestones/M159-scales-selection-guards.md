# M159: Selection guards for the scales argument

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — exported functions of the released package refuse inputs they accept today
- **Branch/PR:** m159-scales-selection-guards

## Goal

The exported functions that take `scales` refuse a selection that reaches one column twice, holds a position below 1, or names a column whose name occurs twice in the data, with an error that names the cause.

## Scope

**In:** One internal helper in `R/utils.R` that judges a `scales` selection, called by each export that `formals()` over `getNamespaceExports("circumplex")` lists with a `scales` argument, except `self_standardize()`. At 447a74dd that is `cpm_fit()`, `fit_structure()`, `norm_standardize()`, `ssm_analyze()`, `ssm_analyze_long()`, `ssm_growth_data()`, `ssm_parameters_id()`, `ssm_score()`, `ssm_sem()`, `ssm_sem_parameters()` and `ssm_sem_syntax()`. The M158 repeated-scale guard in `ssm_growth_data()` moves onto the helper. `ssm_analyze()` also refuses a character `occasions` block that names a duplicated column. `ssm_growth_data()` refuses a list-column `id`. Help pages and NEWS change to match. The refusals are errors at once, with no deprecation cycle (question set).

**Out:**
- `self_standardize()`: it takes no angles, and a repeated column gives a defined result (`PA_z`, `PA.1_z`). It goes to the new candidate row "Selection guards without angles".
- The `items` argument of `axes_reliability()`, `score()` and `ipsatize()`: the same candidate row.
- Repeated and out-of-range `occasions` columns: `ssm_analyze()` already refuses them ("overlapping columns", "out of range"), and this milestone keeps those messages.
- Non-whole positions of 1 or more: they stay accepted, because R truncates them and M158 accepted that.
- An instrument object whose own scale abbreviations repeat (the `instrument` path of `ssm_sem_syntax()`): no bundled instrument has one.

## Acceptance criteria

- [ ] AC1: Each export named in Scope In refuses a `scales` vector that selects one entry twice, with an error that names the repeated column or label. Where an export selects columns of `data`, the refused forms are two equal names and two numbers that reach one column after R's truncation (`3` and `3.5`), for data-frame and matrix `data` where the export accepts a matrix. Where `scales` only labels (`ssm_sem_parameters()`, `ssm_sem_syntax()` with `scales` given, and the `cormat` path of `cpm_fit()`), two equal entries are refused, including labels that `cpm_fit()` takes from duplicated `colnames(cormat)`.
- [ ] AC2: Each export in Scope In that selects columns of `data` refuses a numeric `scales` entry that truncates below 1 (`0`, `0.5`, `-0.5`, `-1`), with an error that names the value. Today such an entry gives a matrix-dimension error or a silent selection by exclusion.
- [ ] AC3: Each export in Scope In that selects columns of `data` refuses a `scales` name that occurs more than once among the column names it indexes, with an error that names the column. `ssm_analyze()` refuses a character `occasions` block that names such a column. A number that selects a column whose name repeats stays accepted (the bound-waves case in `tests/testthat/test-ssm_occasions.R`).
- [ ] AC4: For each export in Scope In that selects columns of `data`, four probes give the same result or error as at 447a74dd: a non-whole number of 1 or more, an `NA` entry, an absent name, and a number past the last column. Pre-existing test expectations stay unchanged, except `ssm_growth_data()` repeated-scale messages that the helper rewords, each still naming the column.
- [ ] AC5: `ssm_growth_data()` refuses an `id` that names a list column. The error names `id` and says that it is a list column.
- [ ] AC6: For each export in Scope In, the `@param scales` text (and the `@param occasions` text of `ssm_analyze()`) names the refusals that apply to it, and `devtools::document()` regenerates `man/`. The NEWS development section states the new refusals, and a test from AC1 to AC5 backs each NEWS claim.
- [ ] AC7: `Rscript -e 'devtools::test()'` passes, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2, T3, T4, T5
- AC2 → T1, T2, T3, T4
- AC3 → T1, T2, T3, T4
- AC4 → T1, T2, T3, T4
- AC5 → T4
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Write the helper and its tests first. Before any code change, record in a test file the result or error of the four AC4 probes for each data-selecting export at 447a74dd, asserting each error's message. Then write the helper in `R/utils.R` (names judged after a matrix becomes a data frame, positions by `seq_along(data)[scales]`, `NA` and unmatched entries passed through to the caller's own error). Unit-test it on names, numbers, `3`/`3.5`, `0`/`0.5`/`-0.5`/`-1`, duplicated data names, matrix and data-frame input.
- [x] T2: Call the helper from `ssm_analyze()` (`scales`, and character `occasions` blocks for AC3 only), `ssm_analyze_long()`, `ssm_score()` and `ssm_parameters_id()`. Per export, a test fires each AC1 to AC3 form and asserts its message.
- [x] T3: Call the helper from `ssm_sem()`, `cpm_fit()` (data path, and the label check on the `cormat` path), `fit_structure()` (judging names against the matrix it indexes at `R/fit_structure.R:809`) and `norm_standardize()`. Add the label check to `ssm_sem_parameters()` and `ssm_sem_syntax()`. Tests as in T2, with `skip_if_not_installed("lavaan")` where a fit is needed.
- [x] T4: Move the repeated-scale guard of `ssm_growth_data()` (`R/ssm_growth_data.R:133-146`) onto the helper, and update the reworded expectations in `tests/testthat/test-ssm_growth_data.R`. Add the list-column `id` refusal with its test.
- [x] T5: Add a test that lists the exports with a `scales` argument by `formals()` over `getNamespaceExports("circumplex")`, and fails when that list differs from the guarded set plus `self_standardize()`.
- [x] T6: Update each `@param scales` (and `@param occasions` in `ssm_analyze()`), run `devtools::document()`, and add the NEWS entry. Sweep the vignettes and help pages for text that says a selection is accepted which is now refused.
- [ ] T7: Run `Rscript -e 'devtools::test()'` and `Rscript -e 'devtools::check(args = "--no-manual")'`.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: question set: which work to plan — scales selection guards (over a plain-English vignette pass and help-page review notes).
- 2026-10-18: question set: errors at once or warnings for one release first — errors at once, no deprecation cycle; IP1 outranks GP4 because these inputs give wrong numbers or wrong columns today.
- 2026-10-18: plan gate chose refusal with an error over a warning-first deprecation because the inputs give wrong results today; falsified by a user with a defensible analysis that uses a repeated column or a selection by exclusion.
- 2026-10-18: plan gate chose one shared helper over per-function guards because eleven exports share the rule and M158's guard already holds it; falsified by an export whose selection rule differs so that the helper needs per-caller branches.
- 2026-10-18: collision sweep: absorbs the candidate rows "Growth id and scales edge inputs" and "Growth scales selection edge inputs" in full, merged into one row for the ROADMAP line cap; extends M158 (done); D-005 read, the helper is a check that stops, not an `is_*()` predicate. Inbox: 1 open issue (#180, stale master-red alert), no overlap; 0 open PRs.
- 2026-10-18: split: `self_standardize()` and the `items` argument go to the new candidate row "Selection guards without angles"; repeated and out-of-range `occasions` columns are out because they are already refused.
- 2026-10-18: criteria audit (full mode, fresh Opus reader): 12 findings, all fixed toward the narrower promise — label-only paths split from data paths (1); `self_standardize()` moved out on GP2 (2); probe forms named per export (3); the drift test moved from AC1 to T5 (4); occasions dropped from AC2, it already refuses (5); `0.5`/`-0.5` and matrix `fit_structure()` added (6); names judged against what each export indexes (7); character `occasions` blocks added to AC3 (8); AC4 narrowed to four listed probes (9) and its allowed changes named in the criterion (10); AC5 wording (11); Goal narrowed to the three refusals (12).
- 2026-10-18: lessons harvested: M157 (judge names after matrix conversion), M158 (positions by `seq_along`), M56 (sweep help pages and vignettes both ways), M146 (assert that a guarded case occurs).
- 2026-10-18: model tier: Opus for implementation (guard and validation code, no estimator change, no oracle needed because no number changes).
- 2026-10-18: implement started on branch m159-scales-selection-guards; baseline probes for AC4 recorded in tests/testthat/test-scales-selection.R and passing on unchanged code (only the five helper tests failed before the helper existed).
- 2026-10-18: implementation choice: refusals use stop(call. = FALSE) as the surrounding code and M157/M158 do, not cli::cli_abort() as the profile's test-doctrine says, because cli is not a dependency and adding it is a dependency change the plan did not grant.
- 2026-10-18: implementation choice: three internal helpers in R/utils.R: check_scales_selection(), check_scales_positions() (its position half, which ssm_growth_data() runs before resolving columns so its id/time messages keep their order) and check_scales_labels() for the label-only paths; the messages reuse M158's wording, so no growth expectation changed.
- 2026-10-18: checkpoint (work in progress, tasks unchecked): helper and calls in all eleven exports, list-column id refusal, roxygen text and NEWS written; full suite running; refusal tests drafted, man/ not yet regenerated. Three call-site edits went through a script instead of the Edit tool (ssm_analyze_long, ssm_score, ssm_parameters_id).
- 2026-10-18: T1 verified: full suite clean at the T1 state (0 failures; 1 pre-existing skip, 11 pre-existing warnings).
- 2026-10-18: refusal tests for AC1 to AC3 and AC5 and the export-enumeration test (T5) added to test-scales-selection.R; man/ regenerated (11 Rd files). Plants: guard removed from ssm_score() reddened its AC1 forms; label check removed from the cpm_fit() cormat path reddened the label test; ssm_sem_syntax dropped from the guarded list reddened the enumeration test; all restored. Checkpoint before the full-suite run.
- 2026-10-18: T2 to T6 verified: full suite clean with all changes (0 failures; the same 1 skip and 11 warnings as before). No pre-existing expectation was edited: the helper reuses M158's growth messages. T6 sweep: no vignette or help text claimed a now-refused selection was accepted.
