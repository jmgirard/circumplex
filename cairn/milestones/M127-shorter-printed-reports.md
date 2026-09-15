# M127: Printed reports are shorter and easier to scan

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Resolves:** —
- **Surface tier:** user-facing — changes the printed output of exported print and summary methods
- **Branch/PR:** —

## Goal

The printed accuracy summary and the printed invariance ladder show their verdicts and cautions in a shorter layout, and no printed message uses a double hyphen as a dash.

## Scope

**In:** `summary.circumplex_ci_accuracy()` (`R/ssm_ci_oop.R:424`) gets a shorter layout. `print.circumplex_ssm_sem()` (`R/ssm_sem.R:1711`) wraps its ladder notes and verdict prose to the console width and trims them. Printed strings in `R/*.R` that use `--` as a dash get rewritten. D-056 records why these layout changes need no deprecation cycle under GP4. Snapshots and NEWS.md are updated.

**Out:** re-knitting vignettes and the prose that reads this output → M128. Dashes in roxygen help text are out, because no one reported them. The `--` placeholder for a missing value in the axes-reliability component table (`R/axes_reliability_oop.R:33`) stays, because it is a table cell and not a dash. Removing any number, verdict or caution from printed output is out, because D-056 does not cover it.

## Acceptance criteria

- [ ] AC1: At `options(width = 80)`, `summary()` of the seeded object in `tests/testthat/test-ci_accuracy.R`'s "print and summary snapshots" test prints at most half as many lines as at commit `845fb5e7`. Each verdict and caution line of the old output still appears in the new output, with changes only to dashes and line wrapping. Every table column that the summary no longer prints stays in the returned object. On objects that fire the CAUTION branch (`R/ssm_ci_oop.R:335`), a contrast block and an occasions block, the new summary still prints each verdict and caution.
- [ ] AC2: At `options(width = 80)`, every package-written prose line in `print()` output of a multi-group `ssm_sem()` result is at most 80 characters. This covers the ladder heading, the rung notes, the ΔCFI note and the verdict lines. Tests run, when lavaan is installed, a fit whose invariance is rejected, a fit whose invariance holds, and a fit with a rung note.
- [ ] AC3: The search `grep -nE -- '-- |--"|"--|—' R/*.R` lists every candidate. On lines that are not comments, no hit sits in a string that the package prints, except the table placeholder named in Out.
- [ ] AC4: NEWS.md has an entry for each changed printed report. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T1, T5

## Tasks

- [ ] T1: Append D-056 to `cairn/DECISIONS.md` if the plan commit did not land it. It is the gate record for this milestone.
- [ ] T2: Record the line count of the old summary at width 80 from commit `845fb5e7`. Write tests for AC1's four objects first: the seeded snapshot, CAUTION, contrast and occasions. Then redesign the summary. Candidates are verdict blocks first and a compact coverage table with one row per profile and condition, with the MC_se, miss and width columns left to `acc$coverage`. The phrase tests at `test-ci_accuracy.R:743`, `:827` and `:863` pass with changes only to dashes and line wrapping.
- [ ] T3: Wrap the prose in the ladder section with `strwrap()` at `getOption("width")`, and shorten the ΔCFI note (`sem_dcfi_note()`) and the verdict text without dropping a condition they name. Add the three lavaan-gated tests from AC2. Skip them with `skip_if_not_installed("lavaan")` (lesson M65 family).
- [ ] T4: Run AC3's search, list each hit in the work log with its disposition, and rewrite each printed dash as a period, a comma or a connecting word. Then update the phrase tests that match the old text (lesson M56 family: sweep both directions).
- [ ] T5: Regenerate the changed `expect_snapshot()` files, review each diff line by line against AC1 and AC2, and add the NEWS.md entries. Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-14: created by /milestone-plan. The plan gate chose to change the package's printed layouts under a gated decision (D-056) over vignette-only workarounds, because the dashes and the long ladder lines come from the print methods themselves. Falsified by a user workflow that parses the old printed text.
- 2026-09-14: criteria audit (full mode, fresh [O] reader) found 5 items on this file's draft. All were fixed before writing: the missing GP4 gate became D-056, AC1 got width 80 and the CAUTION, contrast and occasions probes, AC2 was scoped to package prose with three branch probes, AC3's pattern gained `) -- ` and U+2014 with the placeholder exempt, and AC4's snapshot promise moved to T5.

## Decisions
