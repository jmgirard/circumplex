# M132: Rendered vignette output fits the website's code box

- **Status:** review
- **Priority:** normal
- **Depends on:** M131
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes are the package's teaching material on the website
- **Branch/PR:** m132-vignette-output-fits-code-box

## Goal

No output line in a pre-rendered vignette makes the website's code box scroll
sideways.

## Scope

The pkgdown code box was measured on the live site on 2026-09-15. It is
774 px wide at every desktop viewport. At 1440 px and above the monospace
font is 15.75 px, so the box fits 81 characters. Its `overflow-x` is `auto`,
so a longer line scrolls sideways rather than being cut. knitr prefixes each
output line with `#> `, so the package's own line fits in 77 characters.

**In:** the setup chunk of each of the seven pre-rendered vignette sources
gets `options(width = 77)`. The two live vignettes get the same setting. The
introduction vignette stops passing the deprecated ggplot2 argument
`label.size` at its five sites. All seven vignettes are re-rendered. A new
`tools/check-vignette-width.R` guards the rendered files, and
`.github/workflows/vignette-precompute.yaml` runs it after the re-render.
The accuracy report's verdict heading at `R/ssm_ci_oop.R:522` is reworded to
`Verdicts (c = 1, as estimated), Bradley (1978) liberal band, 95% Wilson CIs:`,
which is 76 columns. The vignette prose that quotes the heading, its snapshot,
and its width-ledger entry in `tests/testthat/test-print-width.R` follow.

**Out:** the two live vignettes are outside the guard's reach (plan gate).
`bayesian-ssm-analysis.Rmd` and `using-instruments.Rmd` are not pre-rendered,
so their shipped file holds no output line for a guard to read. They get the
width setting and a candidate row, not a criterion. The generated lavaan
loading lines (`cx =~`, `cy =~`) in `sem-based-ssm-analysis.Rmd` stay long
behind a declared exemption. Their length follows from the scale names, so no
width setting reaches them, and their emitter is pinned by stored snapshots. Wrapping the
package's own cautions is M131.

## Acceptance criteria

- [ ] AC1: `Rscript tools/check-vignette-width.R` exits 0. Every line in the
      seven pre-rendered `vignettes/*.Rmd` files that begins `#> ` and sits
      outside a `vignette-width:exempt` region is 80 display columns or
      fewer, as the guard measures them.
- [ ] AC2: The guard goes red against each of four planted defect forms,
      applied one file at a time across all seven. Form one is an
      81-column ASCII line. Form two is an 81-column line built from `ζ`
      and `²`. Form three is a long line just outside an exemption marker.
      Form four is a long line that arrives from a re-render rather than
      from an edit. A long line just inside an exemption stays green.
- [ ] AC3: The guard's domain is not empty. It reports, for each file, how
      many `#> ` lines it read and how many exemptions it honored. Both
      counts appear in the milestone's review evidence, and the line count
      is above zero for all seven files.
- [ ] AC4: Re-running `Rscript tools/precompute-vignettes.R` and then
      `Rscript tools/check-vignette-staleness.R` reports no difference
      against the committed render.
- [ ] AC5: The rendered introduction vignette contains no line matching
      `deprecated`, and `grep -n "label.size" vignettes/` returns no hit.
- [ ] AC6: Exactly one `vignette-width:exempt` region exists in the seven
      pre-rendered vignettes. The guard's AC3 count is 1 for
      `sem-based-ssm-analysis.Rmd` and 0 for the other six. The region holds
      the `cx =~` and `cy =~` loading lines. Its marker records that the line
      length follows from the scale names.
- [ ] AC7: `Rscript -e 'devtools::test()'` is clean. Running
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports no error,
      warning or note that master does not also report. The workflow run on
      the branch is green.

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T5
- AC3 → T4, T5
- AC4 → T4
- AC5 → T2
- AC6 → T1, T5
- AC7 → T6, T7

## Tasks

- [x] T1: Write `tools/check-vignette-width.R`. Read every `#> ` line, count
      display columns, honor an exemption marker, and report per-file line
      and exemption counts. Add the exemption marker to
      `sem-based-ssm-analysis.Rmd.orig`.
- [x] T2: Add `options(width = 77)` to all nine setup chunks. Replace
      `label.size = NA` in `introduction-to-ssm-analysis.Rmd.orig` at lines
      64, 84, 302, 315 and 336 with the current ggplot2 argument.
- [x] T3: Reword the verdict heading at `R/ssm_ci_oop.R:522`. Update the
      vignette prose that quotes it, its snapshot, and its width-ledger entry.
- [x] T4: Re-render the seven vignettes, commit the render, then run the
      width guard and the staleness guard.
- [x] T5: Run the four planted defect forms of AC2 and the inside-exemption
      control. Record each result.
- [x] T6: Wire the guard into `.github/workflows/vignette-precompute.yaml`
      after the re-render step. Add `tools/check-vignette-width.R` to
      `.Rbuildignore`.
- [x] T7: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
      Sweep vignette prose for any claim about the old output width.

## Work log

- 2026-09-15: created by /milestone-plan.
- 2026-09-15: plan gate chose an exemption marker for the generated lavaan lines over wrapping them or raising the guard's limit to 86. Wrapping changes code readers copy, and a global limit of 86 stops the guard catching real regressions. Falsified by a second exemption becoming necessary.
- 2026-09-15: plan gate chose the width setting without a guard for the two live vignettes, over adding them to the pre-render set. One of them needs brms, which makes the render job slow and fragile. Falsified by an over-long line reaching the site from either vignette.
- 2026-09-15: criteria audit ran in full mode. Its most serious finding concerned the guard's domain. The guard reads committed file text. That text is disjoint from the rendered output of the two live vignettes. A promise over all nine files therefore passes without checking two of them. The scope now names seven. The audit also replaced a single plant form with four, split an unsatisfiable CI promise, and dropped a mandated marker wording from a criterion.
- 2026-09-16: implement gate chose a start/end region marker for the exemption (fails when it covers no over-wide line) and a committed tools/m132-planted-defects.R for the AC2 plants. The region's markers add rendered lines, so the exempted lines move from :93-94.
- 2026-09-16: T1 done. tools/check-vignette-width.R reads `#>` lines of the seven renders, measures display columns against 80, honors vignette-width:exempt regions, and errors on an empty domain. Before the re-render it reports 41 over-wide lines; the plan's 23 was measured at 26bd64ac, before M131's wrapped cautions reached 81-83 columns.
- 2026-09-16: T2 done. `options(width = 77)` added to eight setup chunks (sem-based already had it). `label.size = NA` became `linewidth = NA` at the five sites, the replacement ggplot2 4.0.3's deprecation warning names.
- 2026-09-16: amendment (Scope): the first re-render left one 81-column line, the fixed heading `Verdicts at c = 1 (as estimated), Bradley (1978) liberal band, 95% Wilson CIs:` from `R/ssm_ci_oop.R:522`. Mini gate chose rewording it over raising the limit to 81 or a second exemption. Scope In gained the reworded heading. Criteria unchanged, so no re-audit is owed.
- 2026-09-16: minor amendment: new T3 (reword the heading) inserted, render moved before the plants (plants read a green render). Old T3-T6 are now T5, T4, T6, T7. Coverage renumbered.
- 2026-09-16: T3 done. Heading reworded in `R/ssm_ci_oop.R`, `_snaps/ci_accuracy.md`, the `test-print-width.R` ledger and the vignette source prose. The ci_accuracy and print-width tests pass (876).
- 2026-09-16: T4 render committed. Width guard exits 0: output lines per file 284, 147, 9, 255, 65, 41, 94, one exemption (sem-based lines 94-95). Output words match the previous render apart from the heading, a timing, the removed deprecation warning, and table columns that moved when tables wrapped at 77.
- 2026-09-16: T6 done. The workflow runs the width guard after the staleness step, with `if: !cancelled()` so both report. No `.Rbuildignore` edit was needed, because `^tools$` already excludes the guard.
- 2026-09-16: AC4 run: a second full re-render then check-vignette-staleness.R reported all 7 up to date, and the width guard stayed green.
- 2026-09-16: T5 done. tools/m132-planted-defects.R: 42 of 42 plants as expected. Forms ascii (81 col), unicode (81 col, 159 bytes), outside (89), rerender (93) went red naming the planted line in each of the 7 files. Controls unicode80 (80 col, 157 bytes) and inside stayed green in each.
- 2026-09-16: T7 in progress: NEWS.md gained entries for the reworded heading and the vignette width. Full `devtools::test()` passed (FAIL 0, WARN 11, SKIP 1, PASS 10743). Prose sweep of the nine vignette sources, README.Rmd and NEWS.md found no claim about the old output width.
- 2026-09-16: claim audit: 31 claims read, 4 corrected — tools/m132-planted-defects.R, .github/workflows/vignette-precompute.yaml, the eight vignette setup comments, NEWS.md. Re-read once by the same reader, all four correct.
- 2026-09-16: T7 done. `devtools::check(args = "--no-manual")` at 756d7167: 0 errors, 0 warnings, 0 notes. The branch workflow run of AC7 happens when review opens the PR. Status set to review.
- 2026-09-16: amendment return: AC1 — "The 23 lines over 81 columns that this plan measured at `26bd64ac` are gone." 2 of the 23 are the exempted sem-based loading lines, still present by design.
- 2026-09-16: amendment return: AC6 — "Exactly one exemption exists, at `sem-based-ssm-analysis.Rmd:93-94`." The exempted lines are the same content, now at 94-95 after the start marker. Status back to in-progress for these two amendments only. This is not a defect return.
- 2026-09-16: re-audit: AC1 (full) — the "23 lines are gone" sentence restated a search result the all-lines check already implies, and "declared exemption" named no marker. Both fixed before the gate.
- 2026-09-16: re-audit: AC6 (full) — "exemption count of AC3 is 1" was ambiguous against per-file counts, and Scope Out still cited :93-94. Both fixed before the gate. It also found the region spans the whole 37-line chunk output.
- 2026-09-16: amendment (AC1, AC6, Scope Out), mini gate approved: AC1 drops the 23-line clause and names the `vignette-width:exempt` region. AC6 names the loading lines by content and states per-file counts. Scope Out drops the :93-94 line numbers. The region-breadth gap goes to the code-box-width candidate row at archive.
- 2026-09-16: re-audit: AC1 (full) — nothing.
- 2026-09-16: re-audit: AC6 (full) — nothing. It noted that the Goal ("no output line") does not qualify the two exempted lines, which the plan already exempted. Goal is plan-owned, so this goes to the review gate.
- 2026-09-16: T4 box ticked. The T4 work-log lines above record the render and both guards green.

## Decisions

## Review

Review pass 1, 2026-09-16, at da8139cd. The branch contains origin/master. No PR exists.

- AC1 (not ticked, criterion wrong as written): `Rscript tools/check-vignette-width.R` exits 0 and every output line outside the exemption is 80 columns or fewer. But re-measuring at 26bd64ac finds the 23 lines over 81 columns (evaluating 10, growth 5, intermediate 5, introduction 1, sem-based 2). The 2 sem-based lines, the `cx =~` and `cy =~` loading lines, are still in the render verbatim, inside the exemption that Scope and AC6 require. "The 23 lines ... are gone" therefore cannot hold alongside AC6. 21 of the 23 are gone.
- AC3 (partial evidence, not ticked): the guard's per-file counts are 284, 147, 9, 255, 65, 41, 94 output lines, and exemptions 0, 1, 0, 0, 0, 0, 0. Not ticked because the review stopped at the AC1/AC6 return.
- AC5 (evidence, not ticked): no `deprecated` line in the rendered introduction vignette, and `grep -rn "label.size" vignettes/` exits 1 with no hit.
- AC6 (not ticked, criterion wrong as written): exactly one exemption exists, and its marker states that the line length follows from the scale names. The exempted lines are the same two loading lines that were `sem-based-ssm-analysis.Rmd:93-94`, but they now sit at lines 94-95 because the start marker adds a line. The criterion names :93-94.
- AC7 (partial): `devtools::test()` FAIL 0, WARN 11, SKIP 1, PASS 10743. Check and the workflow run not rerun at this pass.
- AC2, AC4: not run at this pass.
- Consistency gate: `cairn_validate.py` all checks passed. Toolchain checks not run at this pass.
- Observation for the amendment round, not a triaged finding: the exempt region spans the whole `syntax` chunk (rendered lines 80-129, 37 output lines), so a new over-wide line anywhere in that output would pass unseen. T4's task box is unticked although the work log records T4 done.
- Outcome: review stopped before the reviewer fan-out. AC1 and AC6 go back for a gated criterion amendment.
