# M160: Selection guards for items and the remaining column arguments

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — exported functions of the released package refuse inputs they accept today
- **Branch/PR:** m160-items-selection-guards

## Goal

The column-selection arguments that M159 left unguarded refuse the selections that give a wrong result or a misleading error, with an error that names the cause.

## Scope

**In:**
- The M159 helper in `R/utils.R` (`check_scales_selection()`, `check_scales_positions()`) names the argument it judges, so that it serves `items` as well as `scales`. It is called by `score()`, `ipsatize()`, `axes_reliability()` and `self_standardize()`.
- `axes_reliability()` judges both of its forms: the flat `items` vector with `instrument`, and the per-scale list without it, on the data path and the `cormat` path. Because it turns numbers into column names (`axes_colnames()`, `R/axes_reliability.R:505`), it also refuses a number that reaches a column whose name repeats. A probe on 2026-10-18 showed that such a number silently used the first column with that name.
- `ssm_analyze_long()` refuses a numeric `scales` selection that reaches two columns with one name, before its wide reshape (`R/ssm_analyze_long.R:171-187`).
- `check_scales_labels()` refuses `NA` listed more than once.
- `ssm_sem()` and `ssm_sem_syntax()` refuse a `measures` entry that is also one of the scales. Today the lavaan data then holds two columns with one name, and the syntax gives that one variable two roles, indicator and outside measure.
- The help pages AC6 lists and NEWS change to match. The refusals are errors at once, with no deprecation cycle (question set).

**Out:**
- A `score()` or `axes_reliability()` `items` vector longer than the instrument's item count (an extra leading column shifts every scale), a single `NA` label in `ssm_sem_syntax()` and `ssm_sem_parameters()`, positions below 1 in the `blocks` argument of `axes_reliability()`, and a `measures` entry named like a scale in `ssm_sem_parameters()` (it reads a fit the user built): the new candidate row "Selection guard remainders".
- A `measures` entry that is also a scale in `ssm_analyze()`: the correlation is defined, so it is not refused.
- Non-whole positions of 1 or more, `NA` entries, absent names and numbers past the last column: they pass through to each function's existing result or error, as in M159.

## Acceptance criteria

- [ ] AC1: `score()`, `ipsatize()` and `axes_reliability()` refuse an `items` selection, and `self_standardize()` a `scales` selection, in each of three forms, with an error that names the argument and the value, column or position at fault: a number that truncates below 1 (`0`, `0.5`, `-1`); a name that occurs more than once among the column names the function indexes (for `axes_reliability()`, the columns of `data`, or the column names of `cormat` on the matrix path); and two entries that reach one column (two equal names, or `3` and `3.5`). For `axes_reliability()` without `instrument`, the third form also covers one column listed under two scales. `axes_reliability()` also refuses a number that reaches a column whose name occurs more than once in `data`, or in `colnames(cormat)` on the matrix path. A test fires each form for each function, with data-frame and matrix `data`, and for `axes_reliability()` on the flat form with `instrument` and the list form, each on the data path and the `cormat` path.
- [ ] AC2: In `score()`, `ipsatize()` and `self_standardize()`, a number that selects a column whose name repeats in `data` stays accepted. In each function of AC1 (for `axes_reliability()`, the list form on the data path), four probes give the same result or error as at 71d44034, each replacing one entry of a valid selection: `1.5` in place of `1`, `NA`, an absent name, and a number past the last column. Test expectations that exist at 71d44034 stay unchanged, except the export-listing test that AC3 extends.
- [ ] AC3: Each export whose formals include `items` or `scales`, as `formals()` over `getNamespaceExports("circumplex")` lists them, refuses the AC1 forms (for `ssm_sem_parameters()` and `ssm_sem_syntax()`, whose `scales` hold labels, a repeated label), except `scales()`, whose `items` argument is a logical flag.
- [ ] AC4: `ssm_analyze_long()` refuses a numeric `scales` selection that reaches two columns with one name, with an error that names that name. The error comes before the reshape, so the "`occasions` blocks select overlapping columns" message does not appear, and the test asserts both facts.
- [ ] AC5: `ssm_sem_syntax()` and `ssm_sem_parameters()` with `scales` given, and the `cormat` path of `cpm_fit()`, refuse labels that hold `NA` more than once. `ssm_sem()` refuses a `measures` entry that names or selects a column that `scales` also reaches, and `ssm_sem_syntax()` a `measures` entry equal to one of the scale labels, whether they come from `scales` or from `instrument`. Each error names the entry at fault.
- [ ] AC6: The `@param items` text of `score()`, `ipsatize()` and `axes_reliability()`; the `@param scales` text of `self_standardize()`, `ssm_analyze_long()`, `cpm_fit()`, `ssm_sem_syntax()` and `ssm_sem_parameters()`; and the `@param measures` text of `ssm_sem()` and `ssm_sem_syntax()` name the new refusals that apply. `man/` matches `devtools::document()` output, with no diff after a rerun. The NEWS "Breaking changes" section states the new refusals.
- [ ] AC7: `Rscript -e 'devtools::test()'` passes, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3, T4, T5
- AC4 → T5
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Before any code change, record in a test file the result or error of the four AC2 probes for each AC1 function at 71d44034, asserting each error's message, and a test that reproduces the `score()` shift (`items = c(1, 1:32)` on `raw_iipsc`) and the `axes_reliability()` first-name pick. Then give the helper an argument for the name it reports, keeping every M159 message the same, and unit-test it on `items`.
- [x] T2: Call the helper from `score()`, `ipsatize()` and `self_standardize()` (`R/tidying_functions.R`), after a matrix becomes a data frame. Per function, a test fires each AC1 form and asserts its message, and a test shows the AC2 repeated-name number accepted.
- [x] T3: Guard `axes_reliability()` in `axes_resolve_map()` (`R/axes_reliability.R:515`): the flat vector, each per-scale vector, the union across scales, and numbers that reach a repeated name, on the data and `cormat` paths. Tests as in T2, with `skip_if_not_installed("lavaan")`, since `axes_reliability()` needs lavaan.
- [ ] T4: Extend the export-listing test in `tests/testthat/test-scales-selection.R:388` to `items`: it fires a refusal through each export found by `formals()` and asserts the found set equals the guarded set plus the one exemption, `scales()`. Remove its `self_standardize()` exemption.
- [ ] T5: The three M159 review loose ends: the `ssm_analyze_long()` refusal before the reshape, the repeated-`NA` refusal in `check_scales_labels()` (`R/utils.R:289`), and the `measures` check in `ssm_sem()` and `ssm_sem_syntax()`, each with a test that asserts its message, with `skip_if_not_installed("lavaan")` where a fit is needed.
- [ ] T6: Update the help text named in AC6, run `devtools::document()` twice to confirm no diff, add the NEWS entry, check that a test from T1 to T5 backs each NEWS claim, and sweep the vignettes and help pages for text that says a now-refused selection is accepted.
- [ ] T7: Run `Rscript -e 'devtools::test()'` and `Rscript -e 'devtools::check(args = "--no-manual")'`.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: question set: which work to plan — item selection guards (over a plain-English vignette pass, the CRAN-dark boundary tests, and the ladder print test gaps).
- 2026-10-18: question set: fold in the three M159 review loose ends — yes, one milestone covers both candidate rows.
- 2026-10-18: question set: errors at once or warnings for one release first — errors at once, no deprecation cycle; IP1 outranks GP4 where `score()`, `ipsatize()` and `axes_reliability()` give wrong numbers on these inputs today; see the audit line for the forms that do not.
- 2026-10-18: plan gate chose refusal over a warning-first deprecation because the inputs give wrong results today; falsified by a user with a defensible analysis that repeats an item.
- 2026-10-18: plan gate chose refusing the `ssm_analyze_long()` case over keying the reshape by position because results label scales by name, so two scales with one name stay ambiguous downstream; falsified by a use case for two scales that share a name.
- 2026-10-18: plan gate chose to refuse a number reaching a repeated name in `axes_reliability()` only, over refusing it everywhere, because the other three functions index by position and give the right column; falsified by a probe showing a wrong column from one of them.
- 2026-10-18: `self_standardize()` joins at the user's choice; M159 left it out under GP2 because a repeated column gives a defined result (`PA_z`, `PA.1_z`).
- 2026-10-18: collision sweep: absorbs the candidate rows "Selection guards without angles" and "Selection guard follow-ons" in full; extends M159 (done). No D-entry overlaps. Inbox: 1 open issue (#180, a master-red alert from 2026-09-21; master green since), no overlap; 0 open PRs.
- 2026-10-18: split: the `items` length check, the single `NA` label and `blocks` positions go to the new candidate row "Selection guard remainders"; each widens what is refused beyond the two absorbed rows.
- 2026-10-18: criteria audit (full mode, fresh Opus reader): 17 findings, 2 with none. Kept as asked (1): some refused forms give a defined result today (`score()` with `c(0, 1:32)`, `ipsatize()` with `-1`, `self_standardize()` with a repeated column), so for them the break rests on the user's gate answer and GP2's wrong-object reading, not on IP1; the user picked these forms by name, so narrowing is not the agent's call; the plan summary corrects the question-set text that called all of them wrong scores, and the merge question restates it. Fixed: `cormat` repeated-name numbers in AC1 (2); message names the column or position (3); `axes_reliability()` forms and paths (4); AC2 exception for the listing test (5) and named probe vectors (6); AC3 reworded to refusal, labels for the two SEM label exports (7, 8); stated cause for the `measures` refusal in Scope (10); `ssm_sem_syntax()` labels from `instrument` (11); `ssm_sem_parameters()` `measures` to Out (12); lavaan skips moved to T3 and T5 (13); AC6 page list and no-diff end state, Scope narrowed to it (14); NEWS-to-test check moved to T6 (15); Out wording "existing result or error" (17).
- 2026-10-18: ROADMAP: pruned M157's terminal row so the line cap holds, and added the candidate row "Selection guard remainders".
- 2026-10-18: lessons harvested: M157 (judge names after matrix conversion), M159 (probe data-frame and matrix shapes; `format(digits = 15)` in messages), M56 (sweep help pages and vignettes both ways), M146 (assert that a guarded case occurs).
- 2026-10-18: model tier: Opus for implementation (guard code, no estimator change, no oracle needed because no number changes).
- 2026-10-18: implement started on branch m160-items-selection-guards, cut from 9a8e71cd.
- 2026-10-18: T1 done: `check_scales_selection()` and `check_scales_positions()` take `arg` (default "scales") and `where` (default "`data`"), M159 messages unchanged (test-scales-selection.R 0 failures); `tests/testthat/test-items-selection.R` records the four probes at 71d44034 (22 passes under NOT_CRAN). The two regression tests (`score()` shift, `axes_reliability()` first-name pick) fail on unfixed code, shown in a scratch run; they land with T2 and T3.
- 2026-10-18: T2 and T3 done in one commit (they share `test-items-selection.R`): `score()`, `ipsatize()` and `self_standardize()` call the helper after matrix conversion; `axes_reliability()` checks items through a new `axes_check_items()` in `axes_resolve_map()` (flat and list forms, `where` naming `data` or `cormat`), plus a cross-scale repeat check on resolved names. A plant of the T1-commit `axes_resolve_map()` turns all four axes tests red. Full suite: 0 failures, 14796 passes.
- 2026-10-18: implement chose to refuse a number reaching column 1 too when its name repeats later, over accepting the first such column, because the fit reads the name and cannot tell which column was meant.
