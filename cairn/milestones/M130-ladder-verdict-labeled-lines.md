# M130: The invariance-ladder verdict prints as labeled lines

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — it changes the printed output of an exported function and a vignette
- **Branch/PR:** —

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

- AC1 → T2, T3, T4
- AC2 → T2, T4
- AC3 → T1, T3, T5
- AC4 → T2, T4
- AC5 → T6
- AC6 → T6, T7

## Tasks

- [ ] T1: Write the AC1 and AC2 fixtures as one scratchpad script. Run it in a `git worktree` of `0f8f3289`. For each case, save the printed ladder block at width 80, `res$invariance`, and any warning and `ssm_plot_contrast()` error as RDS. Use the same mock at both refs for the mocked case.
- [ ] T2: Write the tests first in `tests/testthat/test-ssm_sem_groups.R`. Cover the labeled lines for each AC1 arm, the ΔCFI show or hide for each AC2 case, and the AC4 width limit at 77 and 80. Skip a test in a locale where Δ does not print as one character. Rewrite the M127 width and wording tests (about lines 935 to 1075) for the new layout. Make sure that each new test fails on master for the reason it claims.
- [ ] T3: Add the internal verdict-facts helper next to `sem_fit_ladder()` (`R/ssm_sem.R`, about lines 955 to 1040). `sem_fit_ladder()` builds its unchanged `verdict` string from the helper. The print method reads the facts from fields that `res$invariance` already holds.
- [ ] T4: Rewrite `sem_print_invariance()` (about line 1748) and `sem_dcfi_note()` (about line 795) to the layout in Scope. Hide `dcfi`, `cr` and the note outside the ΔCFI scope. Run the T2 tests until they pass.
- [ ] T5: Run the T1 script at the branch head and compare. `res$invariance`, the warnings and the errors must be identical. For the printed text, write a ledger per case in `## Decisions`. The ledger maps each number, verdict and caution in master's block to the new line that carries it, or to the AC2 removal.
- [ ] T6: Set `options(width = 77)` in the vignette setup chunk and update the AC5 prose. Run `tools/precompute-vignettes.R` for this vignette, the AC5 width count and `tools/check-vignette-staleness.R`. Update the roxygen, run `devtools::document()` and edit NEWS.md.
- [ ] T7: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-15: Created by /milestone-plan. This extends M127, which wrapped the block to 80 columns but kept it as paragraphs. No open issue or PR overlaps.
- 2026-09-15: Criteria audit in full mode by two fresh [O] readers. The first pass found 11 issues. The plan fixed 6 and sent 3 to the gate. The second pass, after the gate, found 11 issues, and the plan fixed all of them in the criteria and moved the ledger to T5.
- 2026-09-15: The plan gate chose verdict-first labeled lines (mockup C) over a heading for each block and over flat labels, by maintainer choice after mockups. Falsified by readers who miss a ΔCFI or verdict fact in the new block.
- 2026-09-15: The plan gate chose to rebuild the verdict facts in print over changing the stored `verdict` string, because the `ssm_sem()` warning and the `ssm_plot_contrast()` error paste that string. Falsified by a helper that must parse the stored string to get the facts.
- 2026-09-15: The plan gate chose to hide out-of-scope ΔCFI (D-059) over a short note in every case, because the default MLR estimator never gets a label. Falsified by users who need the out-of-scope `dcfi` value in printed output.
- 2026-09-15: The plan chose width 77 in this vignette only over all vignettes, because the other long lines come from warning text, data frames and lavaan syntax, which the width option does not wrap. Falsified by a knit that shows the option wraps them.

## Decisions

## Review
