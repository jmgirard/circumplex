# M157: Input guards for ssm_growth_data()

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2
- **Resolves:** —
- **Surface tier:** user-facing — new refusals in an exported function
- **Branch/PR:** m157-growth-data-guards

## Goal

`ssm_growth_data()` refuses the four kinds of input that today give a wrong long table with no error: duplicated referenced column names, `scales` selecting the id or time column, infinite time values, and repeated person-by-time rows.

## Scope

**In:** the guards in `R/ssm_growth_data.R`, placed before the unchanged computation; their tests in `tests/testthat/test-ssm_growth_data.R`; the roxygen text (`@param data`, `@param scales`, `@param id`, `@param time`) and the regenerated `man/ssm_growth_data.Rd`; the NEWS.md development-version bullet's refusal list. The repeated-pair refusal rests on GP2's wrong-object clause: the `data` argument is documented as one row per person per time point, and the fixed joint model would fit replicate rows without error, so the guard enforces the documented contract, not a statistical impossibility.

**Out:** distinct double ids that `factor()` merges into one person at different times, and a name repeated inside `scales` itself → the "Growth id and scales edge inputs" candidate row (added by this plan). Guards in `ssm_growth_formula()` and `ssm_trajectory()` → not needed by this scope; no row.

## Acceptance criteria

- [ ] AC1: `ssm_growth_data()` stops with an error naming the argument and the column name when the `id` name, the `time` name, or, when `scales` gives names, a `scales` name occurs more than once among the column names of `data`. A matrix `data` with such duplicated column names is refused the same way. A duplicated column name that none of the three arguments references gives no error, and numeric `scales` that select a duplicated name give no error unless that name is also `id` or `time`. Tests fire the `id`, `time` and `scales` cases, the matrix case, and both no-error cases.
- [ ] AC2: `ssm_growth_data()` stops with an error naming `scales` and the column when `scales` selects the `id` or the `time` column, whether `scales` gives names or column numbers (integer or double, including non-integer doubles, which R truncates). Tests fire the id and time cases by name, by integer number, and by a non-integer double; a test shows `scales = 3:10` on `simulated_growth` returns the same table as `scales = PANO()`.
- [ ] AC3: `ssm_growth_data()` stops with an error naming `time` when the time column holds `Inf` or `-Inf`. Tests fire both values.
- [ ] AC4: `ssm_growth_data()` stops with an error naming the id value and the time value of the first repeated pair when two rows of `data` give the same id and the same time as the output table holds them (the id after `factor()`, the time after `as.numeric()`). Tests fire it for character, factor, integer and double ids, including double ids `0.1 + 0.2` and `0.3` at one time; a test shows one id at two times and two ids at one time give no error.
- [ ] AC5: For any input that meets none of the AC1 to AC4 conditions, `ssm_growth_data()` returns what the branch-point version returns: the guards are `stop()` calls placed before the unchanged computation, verified by reading the diff. Evidence: the branch-point tests pass with no edits, and new tests on `simulated_growth` and `simulated_growth_origin` match each `value` to `ssm_parameters_id()`.
- [ ] AC6: The `ssm_growth_data()` help page (its roxygen source and the regenerated `man/ssm_growth_data.Rd`) and the NEWS.md development-version bullet on `ssm_growth_data()` each name every refusal that AC1 to AC4 add; the `scales` parameter text states that `scales` may not select the `id` or `time` column.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T1, T2
- AC6 → T3

## Tasks

- [x] T1: Write the failing tests in `tests/testthat/test-ssm_growth_data.R` for AC1 to AC4, each asserting the message pattern that names its argument and the offending column or value; add the no-error cases, the `scales = 3:10` equivalence test, and the two bundled-dataset value tests (AC5). Run them and confirm the refusal tests fail for the missing guard, not another error.
- [x] T2: Add the guards in `R/ssm_growth_data.R` before scoring (current lines 60-94): duplicate-name check on the column names of `data` before matrix conversion; numeric `scales` resolved as `seq_along(data)[scales]` for the overlap check; `!is.finite()` on time after the NA check; repeated pairs via `duplicated()` on `factor(id_col)` and `as.numeric(time_col)`. `devtools::test()` clean.
- [x] T3: Update the roxygen text and NEWS.md bullet (AC6); `devtools::document()`, reverting any `Config/roxygen2/version` rewrite in DESCRIPTION; `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-10-17: created by /milestone-plan.
- 2026-10-17: tier check: not run — work named at the question set.
- 2026-10-17: question set: which work to plan — Growth data guards; repeated person-by-time rows refuse or warn — refuse.
- 2026-10-17: collision sweep: candidate row "Growth data input guards" absorbed in full (started token); M151 archive shipped the function; D-064 read whole, no conflict; inbox: one open issue (#180, a stale master-red alert, master green 2026-10-17), no overlap; zero PRs after filtering.
- 2026-10-17: lessons harvested: M32/M35 (`!is.finite()`, not `is.na()`), M56 family (the NEWS refusal list is an enumeration to update), M21 family (`document()` rewrites `Config/roxygen2/version`).
- 2026-10-17: plan gate chose refusing repeated person-by-time pairs over warning because `data` is documented as one row per person per time point (GP2 wrong-object input); falsified by a user design with replicate measures at one time point that the coverage oracle validates.
- 2026-10-17: plan chose refusing only duplicated names that `id`, `time` or named `scales` reference over refusing any duplicated name, because an unreferenced duplicate leaves the table well-defined (GP2: never block a defensible analysis); falsified by an unreferenced duplicate that changes the output.
- 2026-10-17: criteria audit (full mode, fresh Opus reader) returned 8 findings, all taken: AC4 equality now as the output holds it, with id types varied (F1); GP2 basis written into Scope (F2); AC2 adds double and truncated numbers and a positive by-number test (F3); AC1 matrix premise corrected, numeric-scales case stated (F4, F5); AC5 restated as a code property with tests as evidence (F6); AC3 no finding (F7); AC6 names the `scales` param text (F8). Double-id merging and names repeated inside `scales` went to a new candidate row.
- 2026-10-17: ROADMAP line cap: the M154 terminal row was pruned (archive file kept) so the M157 row and the new candidate row fit under 60 lines.
- 2026-10-17: no oracle needed: the guards change no computed value; AC5's value tests derive from `ssm_parameters_id()`. No issue in Resolves, so no assignment option.
- 2026-10-17: implement started on branch m157-growth-data-guards, cut from master at 02f052d5.
- 2026-10-17: T1 done: 9 test blocks appended; before the guards, 17 expectations fail, each as "expected to throw an error" with no error raised (missing guard, not another failure); the no-error, by-number and dataset tests pass. Message wording chosen here: "more than once", "selects the `id`/`time` column", "infinite", "more than one row ... `id` ... `time`".
- 2026-10-17: T2 done: guards added before scoring; the output-building lines are byte-unchanged (the repeated-pair check builds its own `factor()`/`as.numeric()` copies). Growth tests 144 pass; full `devtools::test()` FAIL 0, WARN 11 (lavaan), SKIP 1, PASS 14489. Plants (in memory, four): raw-id pair check, untruncated scale numbers, make-unique names, no Inf check; each stops refusing its test's input.
- 2026-10-17: T3 done: roxygen `@param data`/`scales`/`time` (committed with T2) and the NEWS bullet's refusal list name the four new refusals; `document()` changed only `man/ssm_growth_data.Rd` (DESCRIPTION untouched); `devtools::check(args = "--no-manual")` Status OK, 0/0/0. The simple-english lint hook reports 429 hits over the whole of NEWS.md, unchanged by this edit; older entries left as they are.
- 2026-10-17: claim audit pass: 25 claims read, 3 corrected (ids merge by `as.character()` at 15 digits, not by printing; matrix-names comment; test header "where there is one"); the ROADMAP candidate row's "print alike" corrected in place; growth tests 144 pass after `document()`; same reader's one re-read pending.

## Decisions

## Review
