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

- [x] AC1: At `options(width = 80)`, `summary()` of the seeded object in `tests/testthat/test-ci_accuracy.R`'s "print and summary snapshots" test prints at most half as many lines as at commit `845fb5e7`. Each line that `ssm_ci_verdict_blocks()`, `ssm_ci_structure_note()` and `ssm_ci_cat_para()` wrote in the old output still appears in the new output, with changes only to dashes and line wrapping. Every table column that the summary no longer prints stays in the returned object. The same line rule holds on objects that fire the CAUTION branch (`R/ssm_ci_oop.R:335`), the near-zero regime with its margin rung, a contrast block and an occasions block.
- [x] AC2: At `options(width = 80)`, every line of the invariance-ladder block in `print()` output of a multi-group `ssm_sem()` result is at most 80 characters by `nchar(type = "width")`. The block covers the ladder heading, the table, the rung notes, the ΔCFI note and the verdict lines. When lavaan is installed, tests run a fit whose invariance holds and fits whose invariance is rejected with and without a requested contrast. They also run a fit with a rung note and fits with the ΔCFI note in and out of scope.
- [x] AC3: The search `grep -nE -- '-- |--"|"--|--\\n|\\u2014|—' R/*.R` lists every candidate. On lines that are not comments, no hit sits in a string that the package prints or signals as a message, warning or error, except the table placeholder named in Out.
- [x] AC4: NEWS.md has an entry for each changed printed report. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

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
- 2026-09-15: review found all four criteria passing, and the gate chose fixes for O1 to O4 and the O10 header comment. They are committed on the branch, and the suite passes.
- 2026-09-15: step-7 approval: m127-shorter-printed-reports approved for merge

## Decisions

- 2026-09-14 (question gate): `summary.circumplex_ci_accuracy()` prints one merged table with one row per profile and condition. Its columns are the coverage of e, x, y, a and d, the conditional displacement coverage, the certification rate and the Structural flag. All other coverage and guardrail columns stay only in the returned object (D-057).
- 2026-09-14 (question gate): the nine tab-aligned setting lines become three prose lines that keep every number.

## Review

Sync: the branch contains `origin/master` (29271ef7). No merge was needed.

- AC1 evidence (2026-09-14, a review script outside the test suite). The five probe objects were built once with the branch code. `summary()` was captured at width 80 from the branch and from a `git archive` of `845fb5e7`. Line counts, old to new: seeded 80 to 35, CAUTION 79 to 34, near-zero 81 to 36, contrast 146 to 68, occasions 134 to 55. Each new count is at most half. The script captured 130 old lines from the three named functions. It squashed whitespace, ignored case, and let `--` become a colon, comma, period, ", because" or ". This is". After that, 0 old lines were missing from the new output. The new test helper asserts every coverage and guardrail column name in the returned object, and the suite passes.
- AC2 evidence. With lavaan installed, a `NOT_CRAN=true` run of the ladder width test gave 25 expectations and 0 failures. It covers a fit whose invariance holds with the ΔCFI note out of scope. It covers rejected fits with and without a requested contrast, with the note in scope. It covers a strict-tier fit with a rung note. The full `devtools::test()` run also ran it. The work log records the planted width-1000 failure.
- AC3 evidence. AC3's search over `R/*.R`, with comment lines removed, gives 1 hit on the branch and 15 on `master`. The branch hit is the exempt placeholder at `R/axes_reliability_oop.R:33`. A wider search for any `--` on lines that are not comments finds only that placeholder.
- AC4 evidence. NEWS.md has three new entries: the accuracy summary, the ladder print, and the dash rewrite. Branch `devtools::test()`: FAIL 0, WARN 9, SKIP 1, PASS 9501. A `git archive` of `master` gave FAIL 0, WARN 9, SKIP 1, PASS 9435. The 9 warnings come from the same tests on both, so none is new. Branch `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes.

Consistency gate (2026-09-14). `cairn_validate.py` exits 0, with one advisory on M128 sizing. No DESIGN.md principle changed, so `cairn_impact` was skipped. `devtools::document()` leaves no diff and prints 0 `resolve link` lines. README.Rmd is unchanged on the branch. `pkgdown::check_pkgdown()` finds no problems. `devtools::build_manual()` builds the PDF. On master, the newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` (6dfc4dc6) are `success`. The two alert audits and the branch-protection check each exit 0.

Independent review: [O] diff-bug, [S] blame-history and [S] prior-review lenses, each in a fresh context. The prior-review lens found no prior-review evidence that the diff contradicts. The PR-comment probe returned no comments. Findings, most severe first, with dispositions set at the merge gate:

- O1. `R/ssm_ci_oop.R:314`: the occasions rank-deficiency CAUTION says the "coverage and width remain valid" and the "fit-statistic pass rate is descriptive only". `summary()` no longer prints the width or the pass rate.
- O2. `R/ssm_sem.R:1018`: the verdict string stored in `$invariance$verdict` lost "not required for this contrast" for rejected rungs above the required one. No test runs that branch, and NEWS does not name it.
- O3. `R/ssm_ci_oop.R:516`: the verdict heading dropped "liberal" from "Bradley's (1978) liberal band".
- O4. `R/ssm_ci_oop.R:397`: the table keys rows on `details$conditions`. A hand-edited object whose conditions differ from `coverage$Condition` loses a rung without a message.
- O5. `tests/testthat/helper-ci-accuracy-summary.R:17`: the text check compares output to the current helper functions, so wording drift cannot fail it. The seeded and contrast length checks skip on CI.
- O6. `vignettes/evaluating-circumplex-structure.Rmd:713` still names "the guardrail table", which is no longer printed.
- O7. `strwrap` splits "alpha =" from its value in the ladder verdict, and "elapsed" from its value in the summary. If "elapsed" splits from its value, the snapshot's per-line mask misses it.
- O8. `R/ssm_ci_oop.R:404`: with `digits = 0`, Condition values round together, and the `e` column prints with one decimal beside three. The old table did the same.
- O9. The table prints `d_cert` without `N_conditional`, so a rate from 3 certified reps looks precise.
- O10. The file header comment at `R/ssm_ci_oop.R:1` still says `summary()` shows the full tables. The certification rule lost "(scale-free, print-independent)". `R/axes_reliability_oop.R:227` is now 77 characters.
- S1. The new table prints the contrast row's `d_cert` and `cert`. M15-D1 calls these a selection-effect quantity that no display uses. The review checked the old summary. Its raw tables already printed both numbers for the contrast row.
- S2. Same as the second part of O10.

Dispositions (merge gate, 2026-09-15):

- Fix now: O1, O2, O3, O4 and the header comment in O10. The caution now names the coverage and guardrail elements, and the heading says "liberal band". The stored verdict says "reported only and not required for this contrast". The table keys rows on `coverage$Condition`. Two tests were extended: the rank-deficiency caution on the occasions test and the full verdict clause on the above-required-rung test. The snapshot changed one heading line, and `evaluating-circumplex-structure` was re-knitted. After that, `devtools::test()` gave FAIL 0, WARN 9, SKIP 1, PASS 9510, and the staleness check passed.
- Follow-up: O5 is a candidate row in `cairn/ROADMAP.md`.
- Rejected: O6, because M129 owns that vignette prose. O7, because the maintainer did not select it and the snapshot mask works on the current output. O8, because the old table behaved the same. O9, the dropped qualifier and the 77-character line, because D-056 and D-057 allow them. S1, because the old summary already printed the contrast row's conditional coverage and certification rate, and the help page says what "certified" means on a contrast row.
