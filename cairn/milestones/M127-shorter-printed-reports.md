# M127: Printed reports are shorter and easier to scan

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Resolves:** —
- **Surface tier:** user-facing — changes the printed output of exported print and summary methods
- **Branch/PR:** m127-shorter-printed-reports

## Goal

The printed accuracy summary and the printed invariance ladder show their verdicts and cautions in a shorter layout, and no printed message uses a double hyphen as a dash.

## Scope

**In:** `summary.circumplex_ci_accuracy()` (`R/ssm_ci_oop.R:424`) gets a shorter layout. `print.circumplex_ssm_sem()` (`R/ssm_sem.R:1711`) wraps its ladder notes and verdict prose to the console width and trims them. Printed strings in `R/*.R` that use `--` as a dash get rewritten. D-056 records why these layout changes need no deprecation cycle under GP4. Snapshots and NEWS.md are updated. The precomputed vignettes whose printed output changes are re-knitted, so that the vignette staleness check passes on the PR.

**Out:** the vignette prose that reads this output → M129. Dashes in roxygen help text are out, because no one reported them. The `--` placeholder for a missing value in the axes-reliability component table (`R/axes_reliability_oop.R:33`) stays, because it is a table cell and not a dash. Removing a verdict or a caution from printed output is out. Removing a number is also out, except a table column that stays in the returned object (D-056 as corrected by D-057).

## Acceptance criteria

- [ ] AC1: At `options(width = 80)`, `summary()` of the seeded object in `tests/testthat/test-ci_accuracy.R`'s "print and summary snapshots" test prints at most half as many lines as at commit `845fb5e7`. Each line that `ssm_ci_verdict_blocks()`, `ssm_ci_structure_note()` and `ssm_ci_cat_para()` wrote in the old output still appears in the new output, with changes only to dashes and line wrapping. Every table column that the summary no longer prints stays in the returned object. The same line rule holds on objects that fire the CAUTION branch (`R/ssm_ci_oop.R:335`), the near-zero regime with its margin rung, a contrast block and an occasions block.
- [ ] AC2: At `options(width = 80)`, every line of the invariance-ladder block in `print()` output of a multi-group `ssm_sem()` result is at most 80 characters by `nchar(type = "width")`. The block covers the ladder heading, the table, the rung notes, the ΔCFI note and the verdict lines. When lavaan is installed, tests run a fit whose invariance holds and fits whose invariance is rejected with and without a requested contrast. They also run a fit with a rung note and fits with the ΔCFI note in and out of scope.
- [ ] AC3: The search `grep -nE -- '-- |--"|"--|--\\n|\\u2014|—' R/*.R` lists every candidate. On lines that are not comments, no hit sits in a string that the package prints or signals as a message, warning or error, except the table placeholder named in Out.
- [ ] AC4: NEWS.md has an entry for each changed printed report. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T1, T5

## Tasks

- [x] T1: Confirm that D-056 and D-057 are in `cairn/DECISIONS.md`. They are the gate record for this milestone.
- [x] T2: Record the line count of the old summary at width 80 from commit `845fb5e7`. Write tests for AC1's five objects first: the seeded snapshot, CAUTION, near-zero regime, contrast and occasions. Then redesign the summary. Candidates are verdict blocks first and a compact coverage table with one row per profile and condition, with the MC_se, miss and width columns left to `acc$coverage`. The phrase tests at `test-ci_accuracy.R:743`, `:827` and `:863` pass with changes only to dashes and line wrapping.
- [x] T3: Wrap the prose in the ladder section with `strwrap()` at `getOption("width")`, and shorten the ΔCFI note (`sem_dcfi_note()`) and the verdict text without dropping a condition they name. Add the lavaan-gated tests from AC2. Skip them with `skip_if_not_installed("lavaan")` (lesson M65 family).
- [x] T4: Run AC3's search, list each hit in the work log with its disposition, and rewrite each printed dash as a period, a comma or a connecting word. Then update the phrase tests that match the old text (lesson M56 family: sweep both directions).
- [x] T5: Regenerate the changed `expect_snapshot()` files, review each diff line by line against AC1 and AC2, and add the NEWS.md entries. After `devtools::install()`, re-knit the precomputed vignettes with `tools/precompute-vignettes.R` and run `tools/check-vignette-staleness.R`. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-14: created by /milestone-plan. The plan gate chose to change the package's printed layouts under a gated decision (D-056) over vignette-only workarounds, because the dashes and the long ladder lines come from the print methods themselves. Falsified by a user workflow that parses the old printed text.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 5 items on this file's draft. All were fixed before writing: the missing GP4 gate became D-056, AC1 got width 80 and the CAUTION, contrast and occasions probes, AC2 was scoped to package prose with three branch probes, AC3's pattern gained `) -- ` and U+2014 with the placeholder exempt, and AC4's snapshot promise moved to T5.
- 2026-09-14: second fresh [O] audit of the written criteria found 5 items, all fixed. D-056 required every number to stay printed but allowed dropped columns, so D-057 corrects it. AC1 names its line sources and adds a near-zero probe. AC2 is scoped to the ladder block with more branch probes. AC3's pattern gains `--\n` and `—` and covers signaled conditions.
- 2026-09-14: the M128 re-cut moved the vignette prose that reads this output to M129. The re-knit stays here, because the `vignette-precompute` job fails on a PR whose print changes leave a precomputed vignette stale.
- 2026-09-14: implement started on branch `m127-shorter-printed-reports`. The old seeded summary prints 80 lines at width 80, so AC1's limit is 40. The AC3 search found 15 non-comment hits: 14 printed dashes and the exempt placeholder.
- 2026-09-14: T1 done. D-056 and D-057 are in `cairn/DECISIONS.md`. The question gate chose one merged table, a three-line header and kept the x and y columns.
- 2026-09-14: T2 done. The probes are seeded, non-converged CAUTION, near-zero, contrast and occasions. Their summary line counts at width 80 were 80, 79, 81, 146 and 134 at `845fb5e7`. They are now 35, 34, 36, 69 and 57.
- 2026-09-14: T3 done. The ladder block moved to the internal `sem_print_invariance()`, which wraps its prose to the console width. Before the change, ladder lines reached 148 and 230 characters. The new test covers four fits and passes. With a planted width of 1000, its width check failed on all four. It also fixes a doubled period after an untestable-rung verdict. `devtools::test()` is clean.
- 2026-09-14: T4 done. AC3 hits and dispositions follow. `axes_corrected_se.R:1013,1027` (collinearity message) became a colon. `axes_reliability_oop.R:75,79,83` became a comma, `:114` a colon, and `:227` a period with "This is". `ssm_ci_oop.R:110` became a colon, `:131` a comma, `:286` a period with each sentence capitalized, `:335` ", because", and `:488` a period with "This is". `ssm_sem.R:820,1018` were rewritten in T3. `axes_reliability_oop.R:33` is the exempt placeholder. No phrase test matched the old dashes. The accepted snapshot diff changes only dashes and wrapping. `devtools::test()` is clean.
- 2026-09-14: T5 done. Three NEWS entries were added, and a unit test pins the single closing period. That test failed with the fix removed. The re-knit changed only output lines in three vignettes, and `tools/check-vignette-staleness.R` reports all 7 up to date. `devtools::test()` is clean. `devtools::check(args = "--no-manual")` gives 0 errors, 0 warnings and 0 notes.
- 2026-09-14: claim audit: 41 claims read, 6 corrected — NEWS.md, R/ssm_ci_oop.R, man/summary.circumplex_ci_accuracy.Rd, R/ssm_sem.R, tests/testthat/test-ssm_sem_groups.R
- 2026-09-14: the claim reader re-read the six once. Five were fixed, and the NEWS wording on verdicts got one more fix. Status set to review. A diff of old and new output showed only header and table lines changed. The new tests failed on length before the change. `devtools::test()` is clean.

## Decisions

- 2026-09-14 (question gate): `summary.circumplex_ci_accuracy()` prints one merged table with one row per profile and condition. Its columns are the coverage of e, x, y, a and d, the conditional displacement coverage, the certification rate and the Structural flag. All other coverage and guardrail columns stay only in the returned object (D-057).
- 2026-09-14 (question gate): the nine tab-aligned setting lines become three prose lines that keep every number.
