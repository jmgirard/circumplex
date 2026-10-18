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

- AC1 → T1, T2, T3, T8, T9
- AC2 → T1, T2, T3
- AC3 → T2, T3, T4, T5, T8
- AC4 → T5
- AC5 → T5, T8, T9
- AC6 → T6, T8, T9
- AC7 → T7, T8, T9

## Tasks

- [x] T1: Before any code change, record in a test file the result or error of the four AC2 probes for each AC1 function at 71d44034, asserting each error's message, and a test that reproduces the `score()` shift (`items = c(1, 1:32)` on `raw_iipsc`) and the `axes_reliability()` first-name pick. Then give the helper an argument for the name it reports, keeping every M159 message the same, and unit-test it on `items`.
- [x] T2: Call the helper from `score()`, `ipsatize()` and `self_standardize()` (`R/tidying_functions.R`), after a matrix becomes a data frame. Per function, a test fires each AC1 form and asserts its message, and a test shows the AC2 repeated-name number accepted.
- [x] T3: Guard `axes_reliability()` in `axes_resolve_map()` (`R/axes_reliability.R:515`): the flat vector, each per-scale vector, the union across scales, and numbers that reach a repeated name, on the data and `cormat` paths. Tests as in T2, with `skip_if_not_installed("lavaan")`, since `axes_reliability()` needs lavaan.
- [x] T4: Extend the export-listing test in `tests/testthat/test-scales-selection.R:388` to `items`: it fires a refusal through each export found by `formals()` and asserts the found set equals the guarded set plus the one exemption, `scales()`. Remove its `self_standardize()` exemption.
- [x] T5: The three M159 review loose ends: the `ssm_analyze_long()` refusal before the reshape, the repeated-`NA` refusal in `check_scales_labels()` (`R/utils.R:289`), and the `measures` check in `ssm_sem()` and `ssm_sem_syntax()`, each with a test that asserts its message, with `skip_if_not_installed("lavaan")` where a fit is needed.
- [x] T6: Update the help text named in AC6, run `devtools::document()` twice to confirm no diff, add the NEWS entry, check that a test from T1 to T5 backs each NEWS claim, and sweep the vignettes and help pages for text that says a now-refused selection is accepted.
- [x] T7: Run `Rscript -e 'devtools::test()'` and `Rscript -e 'devtools::check(args = "--no-manual")'`.
- [x] T8: Review return 1: add `axes_reliability()` matrix-`data` cases for every AC1 form and flat-form `cormat` cases for the repeated-name and number-to-repeated-name refusals; build `ssm_sem()`'s scale names from `scales` when they are names, and from `names(data)[scales]` when they are numbers; make the lavaan-less export-listing branch check both directions; fix the NEWS `score()` sentence and bullet spacing; rerun T7's commands.
- [ ] T9: Review return 2: a matrix-`data` test for the cross-scale refusal; every AC1 form and value on data-frame and matrix `data` for the three tidying functions; factor list elements in `axes_reliability()` judged as names; the measures message names a shared name; NEWS `score()` and `axes_reliability()` wording and the `score()` help reason; pass-1 follow-ups into "Selection guard remainders"; rerun T7's commands.

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
- 2026-10-18: T4 done: the case tables moved to `tests/testthat/helper-selection.R` (minor amendment: needed so one test sees both); the export-listing test fires a repeated selection through each export with `scales` or `items` and asserts the found set equals the fired set plus `scales()`. Both selection files: 0 failures (203 and 97 passes, NOT_CRAN).
- 2026-10-18: T5 done: `ssm_analyze_long()` refuses numbers reaching two same-named columns before the reshape; `check_scales_labels()` refuses NA listed twice; new `check_measures_not_scales()` in `R/utils.R`, called by `ssm_sem()` (names reached by `scales` and `measures`) and `ssm_sem_syntax()` (after `instrument` resolves the labels). Stashing the T5 code turns all four new tests red. Full suite: 0 failures, 14827 passes (T4 is covered by this run too).
- 2026-10-18: implement chose to compare names in the `ssm_sem()` measures check, over comparing column positions, because lavaan reaches variables by name, so a measure column that only shares a scale's name collides as well.
- 2026-10-18: T6 done: the nine help params of AC6 updated, `devtools::document()` rewrote nine `man/` pages and a second run left no diff; two NEWS "Breaking changes" bullets added, each claim backed by a test in `test-items-selection.R` or `test-scales-selection.R`. Sweep (grep of vignettes, R/ and README for below-1 or repeated selections and "twice"/"repeated item" wording): no stale text. The simple-english hook reports 432 hits over the whole NEWS file; the new bullets were split to its sentence limit, and the older entries are left as they are.
- 2026-10-18: claim audit: 41 claims read, 3 corrected — tests/testthat/test-items-selection.R (direction of the `score()` shift), R/axes_reliability.R (the old cross-scale outcome was a positive-definiteness error, not two loadings), tests/testthat/helper-selection.R (`self_standardize()` sits in `it_cases()`); the same reader re-read all three: hold.
- 2026-10-18: T7 done at d7924dc7: `devtools::test()` 0 failures, 14827 passes (11 lavaan warnings, as before the branch); `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes. Status set to review.
- 2026-10-18: review return 1: AC1 fails as written. It requires matrix `data` for every form and both paths for each form, but `test-items-selection.R` tests matrix `data` for `axes_reliability()` only in the repeated-name form, and has no flat-form `cormat` case for the repeated-name and number-to-repeated-name refusals (diff-bug #2). Fix-now items ride the same return: `ssm_sem()`'s measures check reads all-NA scale names for character `scales` (diff-bug #1, prior-review #1, blame-history #3); the lavaan-less branch of the export-listing test checks one direction only (blame-history #2); NEWS says each scale was scored from the wrong items, which holds only for scales after the repeat (diff-bug #3); NEWS bullets lack a blank line (diff-bug #7).
- 2026-10-18: T8 in progress (checkpoint): T8 added for review return 1, Coverage lines for AC1, AC3, AC5, AC6, AC7 extended to it (minor amendment). Code and tests written: `axes_reliability()` refusals now run over six ways in (flat and list, by data frame, matrix and `cormat`); `ssm_sem()` takes scale names from `scales` itself when they are names, with a test that fails before the fix (no grouping-drop message before the refusal); the lavaan-less listing branch names the three lavaan exports and asserts set equality; NEWS `score()` sentence corrected (checked against master: a repeat at the end changes nothing, at item 32 only NO) and the bullets spaced. Both selection files: 0 failures (114 and 223 passes, NOT_CRAN). Full suite and check running.
- 2026-10-18: T8 done at 12ec3db6: `devtools::test()` 0 failures, 14848 passes; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes. Status set to review.
- 2026-10-18: review return 2: AC1 fails as written: the cross-scale refusal (part of AC1's third form for the list form) has no matrix-`data` test (diff-bug pass 2 #2). Fix-now items ride the return: factor list elements, the two pass-1 follow-ups into "Selection guard remainders", NEWS `score()` and `axes_reliability()` wording, the measures message, full matrix cases for the tidying functions, T8's task text, the `score()` help reason (pass 2 diff-bug #1, #3 to #9).
- 2026-10-18: T9 in progress (checkpoint): T9 added and T8's task text corrected (minor amendments); Coverage for AC1, AC5, AC6, AC7 extended to T9. Matrix cross-scale test; the tidying refusal tests loop every form and value over data-frame and matrix `data`; `axes_check_items()` judges a factor as its labels (the new test fails without it); measures message reworded to "which is the name of one of the scales; each measure must have a name of its own"; NEWS and the `score()` help reworded; follow-ups (v) and (vi) added to "Selection guard remainders". Both selection files: 0 failures (131 and 223 passes, NOT_CRAN). Full suite and check running.

## Decisions

## Review

Pass 1 (2026-10-18, branch head 4866c2ce). spawned: diff-bug, blame-history, prior-review. Consistency gate before the return: `cairn_validate.py` all checks passed; `pkgdown::check_pkgdown()` no problems; master watches: newest `R-CMD-check.yaml` and `test-coverage.yaml` push runs on master `success` at df998375; `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`, `tools/check-branch-protection.R` exit 0; README.Rmd untouched. The test and check run was stopped for the return, so no criterion is ticked in this pass.

- diff-bug #1: `ssm_sem()` measures check is dead for character `scales` (the refusal still comes from `ssm_sem_syntax()`) — fix now.
- diff-bug #2: AC1 test gaps for `axes_reliability()` matrix data and flat-form `cormat` — fix now via review return 1 (AC1 fails as written).
- diff-bug #3: NEWS overstates the old `score()` failure for a late repeat — fix now.
- diff-bug #4: a negative `measures` entry gets a message naming a column the user did not type — follow-up, row "Selection guard remainders" (below-1 `measures` positions are out of scope).
- diff-bug #5: a repeated `measures` entry still reaches lavaan's positive-definiteness error — follow-up, row "Selection guard remainders".
- diff-bug #6: the AC2 `1.5` probe compares with the branch's own whole-number result — reject (false as a defect): no number changed on the branch, and pass 2 records a direct comparison with master as AC2 evidence.
- diff-bug #7: no blank line between the two new NEWS bullets — fix now (rides the NEWS edit).
- blame-history #1: refusing forms with a defined result (`score()` with `0`, `ipsatize()` with `-1`, `self_standardize()` with a repeat) without a deprecation cycle or a decision entry — reject (planned change): the user chose these forms and errors at once at the question set; the merge question restates it.
- blame-history #2: without lavaan the export-listing test checks one direction only — fix now.
- blame-history #3: same as diff-bug #1 — fix now.
- blame-history #4: one NA label passes while two are refused — reject (planned change; single NA is in Out).
- blame-history #5: a number reaching a repeated name is accepted in three functions and refused in `axes_reliability()` — reject (planned change, recorded at the plan gate).
- prior-review #1: same as diff-bug #1 — fix now. The lens found no past review point reintroduced; the PR-comment probe returned no comments.
- Pass 1 fix-now items fixed 12ec3db6 (diff-bug #1, #2 in part, #3, #7; blame-history #2, #3; prior-review #1).

Pass 2 (2026-10-18, branch head 11306c63). spawned: diff-bug, blame-history, prior-review. The evidence run was stopped for return 2, so no criterion is ticked in this pass.

- diff-bug #1: a factor element in `axes_reliability()`'s list form is judged by its integer codes, so the new message can name the wrong column, and a factor reaching a repeated name passes — fix now (judge factors as names, as `axes_colnames()` already resolves them).
- diff-bug #2: the cross-scale refusal has no matrix-`data` test, which AC1's third form requires — fix now via review return 2 (AC1 fails as written).
- diff-bug #3: pass-1 follow-ups #4 and #5 not yet in the row "Selection guard remainders" — fix now (written on the branch).
- diff-bug #4: the NEWS `score()` sentence describes an inserted repeat only, not one that replaces an entry — fix now.
- diff-bug #5: the NEWS reason for the `axes_reliability()` number refusal does not cover a number reaching the first same-named column — fix now.
- diff-bug #6: the measures message says the entry "is also one of the scales" when it is a different column with a scale's name — fix now.
- diff-bug #7: matrix cases for the three tidying functions fire one value per form — fix now (every form and value on both shapes).
- diff-bug #8: T8's task line names `names(data[scales])`, which the code does not use — fix now (task text).
- diff-bug #9: the `score()` help gives a by-position reason for all three refusals — fix now.
- blame-history #1: GP4 binds after v2.0.0, and past GP4 readings (D-039, D-040, D-056) each have a decision entry; M160 refuses forms with a defined result with no entry and no warning period — put to the user at the merge question (record a decision entry, or warn first for those forms).
- blame-history #2: M160 reverses M159's GP2 exclusion of `self_standardize()` without answering its reason — same disposition as blame-history #1 (one decision covers both).
- blame-history #3: the plan's falsifier (a defensible analysis that repeats an item) is not checked for `ipsatize()` — reject (false): a repeat changes every row mean and gives two output columns with one name, so no analysis relies on it; the merge question names this.
- prior-review: nothing reintroduced (probe returned no PR comments; M59, M119, M142, M148, M157, M25 points checked intact). Weak analog on check order in `ssm_analyze_long()` — reject (false): the order is inherited from master's `check_scales_selection()` call.
