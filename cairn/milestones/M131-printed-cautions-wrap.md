# M131: Printed cautions and notes wrap to the reader's width

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Resolves:** —
- **Surface tier:** user-facing — the cautions are text users read in the console and in the vignettes
- **Branch/PR:** —

## Goal

Every prose caution and note the package prints wraps to `getOption("width")`.

## Scope

**In:** the prose caution and note emitters in `R/cpm_oop.R`, `R/ssm_oop.R`,
`R/ssm_draws.R`, `R/fit_structure_oop.R`, `R/axes_reliability_oop.R` and
`R/ssm_ci_oop.R`. Four wrapping mechanisms are in use there today. Some
cautions are hand-wrapped at a fixed column. Some are wrapped greedily at a
hardcoded 70 (`cpm_oop.R:250`). Some are wrapped at a hardcoded 78
(`ssm_ci_oop.R:29`, `:39`). Some are not wrapped at all. One shared internal
helper replaces all four. Width counts display columns
(`nchar(type = "width")`), because the cautions carry `²`, `ζ` and `ℹ`.

**Out:** tables, column headers, fit lines and section headings. Wrapping
them destroys their columns, and none of them is over-long today (plan gate).
The instrument printers in `R/instrument_oop.R` go to a candidate row. They
emit item text of arbitrary length that comes from the instrument data.
`R/ssm_sem.R` is left alone. It already wraps its prose to
`getOption("width")` at `:796`, `:810` and `:1892`. Warnings and messages
need no change, because knitr and the console already wrap them (T1 census).
The vignette width setting, the re-render and the width guard go to M132.

## Acceptance criteria

- [ ] AC1: No caution or note line printed by `print()` or `summary()`
      exceeds the set width, measured as display columns. This holds at
      `options(width = 60)` and at `options(width = 120)`. It holds for each
      fixture in `tests/testthat/test-print-width.R`. That file holds one
      fixture per caution in the T1 census, including the three cautions
      that the census records as assembled at run time.
- [ ] AC2: Every caution and note that master `26bd64ac` printed still
      prints. The words and their order stay the same after line breaks
      collapse to single spaces. A script compares output from a
      `git archive` of `26bd64ac` against the branch over the AC1 fixtures.
- [ ] AC3: The AC1 test goes red against each of four planted defect forms,
      applied one at a time. Form one is an unwrapped emitter. Form two is
      an emitter wrapped at a hardcoded width. Form three is an emitter
      wrapped at `width + 1`. Form four is an emitter whose indent is not
      counted against the width. Each red run names the emitter.
- [ ] AC4: No table row, column header, fit line or heading changes. Every
      existing snapshot passes with no update. Each of the six classes in
      the T1 census has at least one snapshot or printed-output test that
      runs.
- [ ] AC5: `Rscript -e 'devtools::test()'` is clean. Running
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports no error,
      warning or note that master `26bd64ac` does not also report. NEWS.md
      records the wrapping change.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T4
- AC3 → T3
- AC4 → T2, T5
- AC5 → T5, T6

## Tasks

- [ ] T1: Census the prose caution and note emitters in the six files. For
      each one record the file:line, the current wrapping mechanism, and the
      fixture that fires it. Mark the ones assembled at run time, such as the
      marker-label loop at `cpm_oop.R:243-255`. Write the table into this
      file's `## Decisions` section.
- [ ] T2: Add the shared wrapping helper to `R/utils.R`. It wraps prose to
      `getOption("width")`, counts display columns, and counts any indent or
      leader against the width. Leave tables, headers and fit lines alone.
- [ ] T3: Write `tests/testthat/test-print-width.R` first, one fixture per
      census row, at widths 60 and 120. Run the four planted defect forms of
      AC3 and record each red run.
- [ ] T4: Move each census emitter onto the helper. Then run the
      `git archive` comparison of AC2 and record its result.
- [ ] T5: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
      Compare the note list against master `26bd64ac`.
- [ ] T6: Add the NEWS.md entry. Sweep `?` help pages and vignette prose for
      any claim about the old fixed-width layout.

## Work log

- 2026-09-15: created by /milestone-plan.
- 2026-09-15: plan gate chose prose cautions only over also wrapping the instrument printers and the headers. No measured over-long line comes from either, and wrapping a table destroys its columns. Falsified by a user report of an over-long instrument or header line.
- 2026-09-15: plan gate chose a shared helper over per-site strwrap calls, because the T1 census found four wrapping mechanisms across six files. Falsified by a caution whose layout the shared helper cannot express.
- 2026-09-15: criteria audit ran in full mode and returned ten findings. Five were fixed before writing. A grep clause passed only by changing `fit_est < 0.70` or a preserved caution. An "only wrapping mechanism" claim was already false at `ssm_sem.R:796`. The nchar type was unstated against multibyte cautions. The probe family varied only location. Five clauses bound an instrument. A `0 notes` clause reddens for unrelated reasons, and a width-40 case was unsatisfiable against a 15-column leader.

## Decisions

## Review
