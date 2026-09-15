# M130: The invariance-ladder verdict prints as labeled lines

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — it changes the printed output of an exported function and a vignette
- **Branch/PR:** m130-ladder-verdict-labeled-lines

## Goal

A grouped `ssm_sem()` print shows the invariance verdict as short labeled lines, and the block fits the website's code box.

## Scope

**In:** Rewrite `sem_print_invariance()` and `sem_dcfi_note()` in `R/ssm_sem.R` to the layout below (mockup C, chosen at the plan gate). Add an internal helper that builds the verdict's facts. `sem_fit_ladder()` uses it to build its stored `verdict` string, which stays byte-identical, and the print method uses it too. Stop printing the `dcfi` column and the ΔCFI note outside the ΔCFI scope (D-059). Set `options(width = 77)` in the setup chunk of `vignettes/sem-based-ssm-analysis.Rmd.orig` and re-knit. Update the vignette prose, the `ssm_sem()` roxygen and NEWS.md that describe this output.

**Out:** Long `#>` lines in other places go to the candidate row "Vignette output lines past the website's code-box width". These are warnings and notes in four other vignettes, and lavaan syntax in this vignette. The tab-aligned `Global fit` header lines stay as they are. The gate left `inv$verdict`, the `ssm_sem()` warning and the `ssm_plot_contrast()` error unchanged, so no work on them is planned.

Target layout. The implementer picks the wording within AC1 to AC3.

```
Verdict:  metric invariance rejected
  Test:     Δχ²(14) = 54.78, p < 0.0001, alpha = 0.05
  Result:   groups cannot be compared on this instrument's latent metric
  Contrast: not computed; rows below are separate configural profiles
  Instead:  ssm_analyze() observed-score contrast (a different question)
```

## Acceptance criteria

- [ ] AC1: `print()` of a grouped `ssm_sem()` result prints the verdict as a `Verdict:` line that names the decision. Indented labeled lines follow it. They give the nested test or tests, or the reason no test ran. They say what the decision means for comparing the groups. They name any rung rejected above the required rung, marked as reported only and not required for the contrast. When the groups are not comparable, they say that the rows below are each group's separate (configural) latent profile. With a requested contrast, they also say that the contrast was not computed and that the observed-score contrast from `ssm_analyze()` answers a different question. With no requested contrast, they say that a latent contrast is not computable. A value too long for its line continues on an indented line under its label. Tests in `tests/testthat/test-ssm_sem_groups.R` assert these lines for each arm of the verdict `if`/`else` chain in `sem_fit_ladder()` and for its above-rung clause. The cases are the configural gate, the strict tier's vacuous metric rung with and without an above-rung rejection, one retained rung, two retained rungs, a retained rung with an above-rung rejection, and a rejection with and without a requested contrast. The last case is a nested test that lavaan cannot compute. It runs through `ssm_sem()` with `lavaan::lavTestLRT` mocked (`.package = "lavaan"`) to return a two-row table whose difference columns are NA.
- [ ] AC2: The `dcfi` and `cr` columns and a ΔCFI note print only when `res$invariance$dcfi_scope$in_scope` is TRUE and at least one `dcfi` value is not NA. The note is at most two lines at widths 77 and 80. It names the rule and its source: Cheung & Rensvold (2002), from a two-group ML simulation, reject a step when CFI drops by more than .01, at alpha = .01. It also says that the verdict does not use the rule. In every other case, no `dcfi` column and no ΔCFI text print. In every case, `res$invariance$table$dcfi` and `res$invariance$dcfi_scope` hold the values they hold at master. Tests cover the in-scope case. They also cover each of the three out-of-scope reasons (robust CFI, a non-ML estimator, more than two groups) and one combination of reasons.
- [ ] AC3: Apart from what AC2 removes, every number, verdict and caution that the ladder block printed at master `0f8f3289` still prints with the same meaning, for each AC1 and AC2 case. `res$invariance`, the `ssm_sem()` warning for a non-comparable contrast and the `ssm_plot_contrast()` error are identical to master's on the AC1 and AC2 fixtures, run at `0f8f3289` and at the branch head.
- [ ] AC4: The ladder block runs from the `Invariance ladder` line to the line before the first `# Profile` line. Every line in it is at most `getOption("width")` characters at widths 77 and 80, counted with `nchar(type = "width")`, for each AC1 and AC2 case.
- [ ] AC5: In the re-knitted `vignettes/sem-based-ssm-analysis.Rmd`, every `#>` line from each `Invariance ladder` line to the next `# Profile` line is at most 80 characters, counted with `nchar(type = "width")`. `tools/check-vignette-staleness.R` passes. Two prose areas describe the new output correctly, including that out-of-scope ΔCFI values appear only in the returned table. The first area is `vignettes/sem-based-ssm-analysis.Rmd.orig`, from the paragraph before the `by_gender` chunk to the end of the ΔCFI paragraphs after it, plus the "Invariance gating is a modeling decision" bullet. The second is the `@param invariance`, `@details` and `@return` roxygen of `ssm_sem()` in `R/ssm_sem.R`. In both areas, every sentence about the printed ladder, the verdict or ΔCFI matches the new output.
- [ ] AC6: NEWS.md describes the change. Its existing bullet on the invariance-ladder print says that the ΔCFI note is shorter, and it is revised so that no sentence in it is false. `devtools::test()` has 0 failures. `devtools::check(args = "--no-manual")` has 0 errors, warnings and notes.

## Coverage

- AC1 → T2, T3, T4, T8, T9, T13
- AC2 → T2, T4
- AC3 → T1, T3, T5, T8, T11, T14
- AC4 → T2, T4
- AC5 → T6, T10
- AC6 → T6, T7, T10, T14

## Tasks

- [x] T1: Write the AC1 and AC2 fixtures as one scratchpad script. Run it in a `git worktree` of `0f8f3289`. For each case, save the printed ladder block at width 80, `res$invariance`, and any warning and `ssm_plot_contrast()` error as RDS. Use the same mock at both refs for the mocked case.
- [x] T2: Write the tests first in `tests/testthat/test-ssm_sem_groups.R`. Cover the labeled lines for each AC1 arm, the ΔCFI show or hide for each AC2 case, and the AC4 width limit at 77 and 80. Skip a test in a locale where Δ does not print as one character. Rewrite the M127 width and wording tests (about lines 935 to 1075) for the new layout. Make sure that each new test fails on master for the reason it claims.
- [x] T3: Add the internal verdict-facts helper next to `sem_fit_ladder()` (`R/ssm_sem.R`, about lines 955 to 1040). `sem_fit_ladder()` builds its unchanged `verdict` string from the helper. The print method reads the facts from fields that `res$invariance` already holds.
- [x] T4: Rewrite `sem_print_invariance()` (about line 1748) and `sem_dcfi_note()` (about line 795) to the layout in Scope. Hide `dcfi`, `cr` and the note outside the ΔCFI scope. Run the T2 tests until they pass.
- [x] T5: Run the T1 script at the branch head and compare. `res$invariance`, the warnings and the errors must be identical. For the printed text, write a ledger per case in `## Decisions`. The ledger maps each number, verdict and caution in master's block to the new line that carries it, or to the AC2 removal.
- [x] T6: Set `options(width = 77)` in the vignette setup chunk and update the AC5 prose. Run `tools/precompute-vignettes.R` for this vignette, the AC5 width count and `tools/check-vignette-staleness.R`. Update the roxygen, run `devtools::document()` and edit NEWS.md.
- [x] T7: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
- [x] T8: (review return, finding O2) For a non-comparable fit with no requested contrast whose nested test could not be computed, print the meaning master printed: a latent contrast is not computable on this instrument's latent metric. The stored verdict stays unchanged. Assert it in the `untestable_plain` test.
- [x] T9: (finding O1) When no contrast is requested and the required rung is lowered to the gate (for example the latent-mean path with `invariance = "metric"`), the `Result:` line makes no claim that latent means can be compared. Name the rung retained instead. Add that case as a fixture and a test.
- [x] T10: (findings O3 to O5) Correct the "Invariance gating is a modeling decision" vignette bullet (the condition is two groups, ML and a plain CFI, not multivariate normality). In NEWS.md and the `ssm_sem()` roxygen, say that nothing prints when every `dcfi` is NA. In NEWS.md, say that `dcfi_scope` records the fields. Rewrap the roxygen line over 80 characters. Re-knit, re-document, and run the AC5 width count and the staleness check.
- [x] T11: (finding O6) Base the vacuous-tier arm on `sem_strict_metric_vacuous()` again, or add a test that fails when a non-vacuous note on the metric row changes the verdict or `comparable`.
- [x] T12: (finding O7) `print()` of an `invariance` element that lacks the fields the facts helper reads falls back to printing `inv$verdict`, with a test.
- [x] T13: (finding O11) Start the `Verdict:` value in the same column as the labeled values, and stop repeating the rung name on a `Test:` line when only one rung was tested. Keep AC4 widths.
- [ ] T14: Re-run the T5 comparison over all cases (identical `res$invariance`, warnings, plot errors, stored verdicts), `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-15: Created by /milestone-plan. This extends M127, which wrapped the block to 80 columns but kept it as paragraphs. No open issue or PR overlaps.
- 2026-09-15: Criteria audit in full mode by two fresh [O] readers. The first pass found 11 issues. The plan fixed 6 and sent 3 to the gate. The second pass, after the gate, found 11 issues, and the plan fixed all of them in the criteria and moved the ledger to T5.
- 2026-09-15: The plan gate chose verdict-first labeled lines (mockup C) over a heading for each block and over flat labels, by maintainer choice after mockups. Falsified by readers who miss a ΔCFI or verdict fact in the new block.
- 2026-09-15: The plan gate chose to rebuild the verdict facts in print over changing the stored `verdict` string, because the `ssm_sem()` warning and the `ssm_plot_contrast()` error paste that string. Falsified by a helper that must parse the stored string to get the facts.
- 2026-09-15: The plan gate chose to hide out-of-scope ΔCFI (D-059) over a short note in every case, because the default MLR estimator never gets a label. Falsified by users who need the out-of-scope `dcfi` value in printed output.
- 2026-09-15: The plan chose width 77 in this vignette only over all vignettes, because the other long lines come from warning text, data frames and lavaan syntax, which the width option does not wrap. Falsified by a knit that shows the option wraps them.
- 2026-09-15: T1 done. `tests/testthat/helper-ssm-sem-ladder.R` fits 13 cached cases, and at master `0f8f3289` each one reaches its intended verdict arm and ΔCFI scope branch (the mocked `lavTestLRT` gives the untestable arm). The baseline is in the scratchpad as `m130-master.rds`.
- 2026-09-15: T2 done. Three M130 tests replace the two M127 print tests, and three older tests now assert that out-of-scope ΔCFI does not print. The M127 double-period test is gone, because the verdict no longer prints as a sentence. On the unchanged code the new tests fail as claimed: no labeled lines after `Verdict:`, an in-scope note of 3 to 4 lines without "the verdict does not use it", and a `dcfi` column out of scope.
- 2026-09-15: T3 and T4 done. `sem_verdict_facts()` builds the stored verdict and the printed facts, and `sem_format_verdict()` lays the facts out. `sem_dcfi_note()` lost its out-of-scope branch. The three M130 tests pass (310 expectations, 0 failures). A first run failed only because the test helper read the blank line before `# Profile` as part of the block, and the helper now trims it.
- 2026-09-15: T6 done, pending the staleness re-render. `options(width = 77)` sits in the vignette's hidden setup chunk, and a separate chunk would add blank lines to the render. The re-knitted ladder block spans lines 408 to 421, with a maximum width of 75 counted by `nchar(type = "width")`. The rest of the output is unchanged. The prose, the roxygen and NEWS.md are updated, and the M127 NEWS bullet is replaced.
- 2026-09-15: claim audit: 31 claims read, 5 corrected — NEWS.md, R/ssm_sem.R, vignettes/sem-based-ssm-analysis.Rmd.orig (width claims held only at 77 and 80, the note is two lines only at 80, `Also:` prints only when comparable, and `dcfi_scope` records fields rather than one reason).
- 2026-09-15: T7 tests: `devtools::test()` at `08cf7b8a` code gave FAIL 0, WARN 9, SKIP 1, PASS 9788. The 9 warnings match the count M129 recorded on master. Later commits changed only a comment, roxygen and prose.
- 2026-09-15: T7 first check at `c86ff63d`: 0 errors, 2 warnings, 0 notes, tests OK. Both warnings came from this branch. `sem_verdict_facts()` held a literal `Δχ²` where master had `\u` escapes, because the Edit tool wrote the characters, so a byte-level replace restored the escapes. The test helper called `withr::`, which is not declared, and base `options()` replaces it.
- 2026-09-15: T7 done. The second check at `c0596ba2` gave Status OK: 0 errors, 0 warnings, 0 notes, and the tests passed. The staleness check passed after the last re-knit. Status is set to review.
- 2026-09-15: T5 done. The branch head gives `identical()` invariance, warnings and plot errors in all 13 cases, and the printed-text ledger is in `## Decisions`.
- 2026-09-15: review return 1 (defect): AC1 and AC3 fail. For `untestable_plain`, the print says the latent contrast is not computed, where master and AC1 say a latent contrast is not computable. Maintainer triage added T8 to T13 and a candidate row. Status back to in-progress.
- 2026-09-15: return question gate: T8 says why a latent contrast is not computable ("because comparability cannot be established") rather than master's "on this instrument's latent metric", T9 appends the rung a contrast needs rather than dropping the comparability claim, and T11 passes the vacuous flag from the model tier rather than only adding a test guard, all by maintainer choice.
- 2026-09-15: T8, T9, T11, T12 and T13 done, tests first. On the unchanged code the new expectations failed as claimed: `Verdict:` value at column 11, the rung repeated on single-rung `Test:` lines, no contrast-rung clause on `configural` and the new `means_metric` fixture, the old untestable `Contrast:` text, `vacuous_metric` an unused argument, and an error printing a ladder without `required`. After the change `test-ssm_sem_groups.R` passes with no failures. `sem_fit_ladder()` passes `vacuous_metric` from `sem_strict_metric_vacuous()`, and the gating filter reads that flag instead of the note. `print()` reads the path from `details$score_type`.
- 2026-09-15: T10 done. Roxygen, NEWS.md and the vignette bullet corrected, `document()` gives no link warnings, and the re-knitted ladder block (lines 408 to 421) has a maximum width of 75. The re-knit changed only the `Verdict:` column, the `Test:` line and the bullet.
- 2026-09-15: T14 in progress (checkpoint): the 14-case comparison at `b854388d` gives `identical()` invariance, warnings, plot errors and stored verdicts, and every block line fits at 77 and 80. `devtools::test()`, `devtools::check()` and the claim audit are still running.
- 2026-09-15: claim audit: 68 claims read, 7 corrected — NEWS.md, R/ssm_sem.R, man/ssm_sem.Rd, tests/testthat/helper-ssm-sem-ladder.R, tests/testthat/test-ssm_sem_groups.R (`Instead:` prints only with a requested contrast, `dcfi_scope$estimator` is lavaan's name and reads ML for MLR, no released object reaches the print fallback, the fallback verdict wraps, fixture and test comments name `sem_verdict_facts()` and what the note test calls). The same reader's re-read of the 7 is pending.
- 2026-09-15: claim-audit re-read: all 7 corrected claims hold. A NEWS.md line over 80 characters in the source is rewrapped.

## Decisions

- 2026-09-15 (T5 ledger): master `0f8f3289` against the branch head, over the 13 cases of `helper-ssm-sem-ladder.R`. `res$invariance`, the warnings and the `ssm_plot_contrast()` errors are `identical()` in all 13. The printed block maps as follows. The table's rung, chisq, df, cfi, rmsea, dchisq, ddf and p columns and the `note [rung]` lines print unchanged. In scope with a dcfi value (vacuous_above, rejected, untestable), the dcfi and cr columns print unchanged. Note: "Cheung & Rensvold (2002)", "alpha = .01", "two-group ML simulation" and "ΔCFI < -0.01 rejects that step" become the note's "Cheung & Rensvold (2002), two-group ML simulation, alpha = .01; cr = reject when ΔCFI < -0.01". "It is secondary and reported only: the verdict below gates on the nested chi-square difference test alone" becomes "the verdict does not use it", because the verdict block states its own nested test. Out of scope (retained_one, retained_above, gls, groups3_ml, groups3_mlr), the dcfi column, "NOT validated here (<reason>)" and "descriptive only, with no binary verdict" are removed under D-059. In scope with every dcfi NA (configural, vacuous), master printed an all-blank dcfi column and no note, and the column no longer prints. Verdict: each retained, rejected or untestable test string (Δχ², df, p, alpha) moves whole to a `Test:` line. "these groups cannot be compared on this instrument's latent metric", "no cross-group constraints required", "(not testable)", "strict tier's fixed loadings", "holds by construction", "comparability cannot be established", "Inspect the ladder fits directly" and the above-rung sentence move to the decision, `Test:`, `Result:` or `Also:`. "so the latent contrast is not computed" and "The requested latent contrast was therefore not computed" become `Contrast:`. "A latent contrast is not computable on this instrument's latent metric" becomes `Contrast:` for the rejected case and "the latent contrast is not computed" for the untestable case without a requested contrast, as the stored verdict says. "The rows below are each group's separate (configural) latent profile" becomes `Profiles:`. "The observed-score contrast from ssm_analyze() answers a different question and remains available" becomes `Instead:`. The new `Result:` lines "the groups can be compared at the <required> level" state `comparable = TRUE` in words.
- 2026-09-15 (T14 ledger, review return): master `0f8f3289` against `b854388d`, over the 14 cases of `helper-ssm-sem-ladder.R` (the 13 above plus `means_metric`, the latent-mean path with `invariance = "metric"` and no contrast). `res$invariance`, the warnings, the `ssm_plot_contrast()` errors and the stored verdict strings are `identical()` in all 14. Changes to the printed block since the T5 ledger: the `Verdict:` value starts in column 13 with the labeled values. A single tested rung's `Test:` line drops the `<rung>: ` prefix, because the `Verdict:` line names the rung, and two tested rungs keep it. For `untestable_plain`, master's "A latent contrast is not computable on this instrument's latent metric" becomes `Contrast:` "a latent contrast is not computable, because comparability cannot be established", which keeps "not computable" and does not imply a rejection (maintainer choice). When no contrast is requested and the required rung is below the rung the path's contrast needs (`configural`, `means_metric`), `Result:` adds "; a latent <measure-profile|mean> contrast needs <metric|scalar> invariance, which was not tested". This adds a caution and removes none.

## Review

First pass, 2026-09-15, at `609ca47e`. The default branch had not moved, so no merge was needed.

Evidence:
- AC1: FAILS. At the branch head, `untestable_plain` prints `Contrast: the latent contrast is not computed`. The criterion requires that, with no requested contrast, the lines say a latent contrast is not computable. Master's print of the same case said "A latent contrast is not computable on this instrument's latent metric". The other cases print the required lines (verdict blocks read at width 80 for all 13 cases). Not ticked.
- AC2: `dcfi` and the note print in exactly the 5 cases where `dcfi_scope$in_scope` is TRUE and a `dcfi` value is not NA, and in none of the other 8. The note is 2 lines at width 77 (read for `vacuous_above` and `untestable_plain`). `res$invariance` is `identical()` to master in all 13 cases. Not ticked, because the milestone returns and the evidence is re-run at re-review.
- AC3: FAILS on meaning for `untestable_plain` (the AC1 line above). `res$invariance`, warnings, `ssm_plot_contrast()` errors and stored verdict strings are `identical()` at `0f8f3289` and the branch head in all 13 cases (`m130-baseline.R` re-run in a scratch worktree of `0f8f3289`). Not ticked.
- AC4: maximum block width over the 13 cases is 75 at width 77 and 79 at width 80, with `nchar(type = "width")`. Not ticked (return).
- AC5: the re-knitted ladder block (lines 408 to 421) has 14 `#>` lines, maximum width 75, none over 80. `tools/check-vignette-staleness.R` passes. The prose has the defect in finding O3. Not ticked.
- AC6: `devtools::test()` FAIL 0, WARN 9, SKIP 1, PASS 9788. `devtools::check(args = "--no-manual")` Status OK, 0 errors, 0 warnings, 0 notes. NEWS.md has the defect in finding O4. Not ticked.

Consistency gate: `cairn_validate.py` passes. `document()` leaves no diff and prints no `resolve link` line. `pkgdown::check_pkgdown()` finds no problems. README.Rmd is unchanged. The newest verdict push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master (`af167d0e`) are success. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit clean. No DESIGN principle changed.

Reviewers: [S] blame-history found nothing. [S] prior-review found no prior-review evidence that the diff regresses (M57, M127, M129 archives read; no PR review comments). [O] diff-bug reported 12 findings, dispositions by the maintainer at the gate:
- O2 (untestable arm without a contrast changes meaning): AC1 and AC3 failure, defect return, T8.
- O1 (`Result:` claims comparability at a lowered required rung on the latent-mean path): fix in this return, T9.
- O3 to O5 (vignette bullet cites multivariate normality; NEWS and roxygen omit the all-NA case; NEWS says `dcfi_scope` records why; long roxygen line): fix in this return, T10.
- O6 (vacuous arm inferred from a non-empty metric-row note): fix in this return, T11.
- O7 (`print()` errors on an `invariance` element lacking `required` or `alpha`): fix in this return, T12.
- O8 (`test_stat()` rebuilds the expected string with the code's own formatting) and O9 (no test requires a line to wrap): follow-up, candidate row.
- O10 (literal non-ASCII characters in test code): rejected, because tests are read as UTF-8 and check does not flag them.
- O11 (`Verdict:` value column differs from labeled values; `Test:` repeats the rung): alignment and repeat fixed in this return, T13.
- O12 (released 2.0.0 NEWS entry describes the old print): rejected, because released history stays as written.
