# M36: Visualization polish — certification legend key + non-finite guards

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Close the two shipped-code remainders of the M31–M33 visualization track: the
certification legend's missing `FALSE` key glyph, and `coord_circumplex()`'s
`amax`/`center` acceptance of non-finite values.

## Scope

**In:**
- `ssm_plot_trajectory()`'s "Displacement interpretable" legend draws **both**
  keys (`TRUE` solid, `FALSE` hollow) whenever certification is shown, including
  when the data contain no uncertified point. Affects the occasions path and the
  `ssm_draws()` table path alike (one shared site, `R/ssm_trajectory.R:610-635`).
- `!is.finite()` guards on `coord_circumplex()`'s `amax` and `center`, matching
  the treatment M32 gave `r_axis_angle` (LESSONS 2026-07-18).

**Out:**
- Any change to *which* points are certified (D-007 rule) or to the shape
  encoding itself — the encoding stands, only its legend rendering is fixed.
- On-circle movement paths → M37.
- A sweep of every other numeric API argument for `is.na()`-vs-`is.finite()`
  gaps → stays the standing "continuous / infrastructure refactors" candidate
  row, folded into whichever milestone next touches those functions.

## Acceptance criteria

- [ ] A grob-level test asserts the shape legend draws one key glyph per scale
      break (2) for an **all-certified** trajectory, and fails against the
      pre-fix code. Verified by extracting `pch` from the `guide-box-bottom`
      grob tree: pre-fix yields `16` only; post-fix `16, 1`.
- [ ] The same assertion holds on the `ssm_draws()` table path (a table whose
      `certified` column is all `TRUE`) and on a mixed table (unchanged: `16, 1`).
- [ ] `coord_circumplex(amax = Inf)` and `coord_circumplex(center = -Inf)` each
      error at call time naming the offending argument, matching the message
      style of the existing `r_axis_angle` guard; `NA` and `NaN` keep erroring.
- [ ] A vdiffr baseline for the all-certified trajectory is regenerated and shows
      both legend keys (regenerated per the M31 lesson: delete stale `_snaps`
      SVGs, re-run under `NOT_CRAN=true`).
- [ ] `devtools::test()` clean and `devtools::check()` at 0 errors / 0 warnings /
      0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks

- [ ] T1: Write the failing grob-level legend test (extract `pch` from the
      `guide-box-bottom` grob tree for an all-certified fixture); confirm red
      against current `R/ssm_trajectory.R`. Note for implement: neither
      `override.aes$shape` nor a 2-level `factor(Certified)` with `drop = FALSE`
      restores the key — both probed 2026-07-18 and rejected. ggplot2 draws key
      glyphs only for values present in layer data, so the fix must make the
      absent value present (e.g. a zero-size / `alpha = 0` presence layer keyed
      to the missing break) or build the key manually.
- [ ] T2: Implement the fix at `R/ssm_trajectory.R:610-635`; extend the test to
      the table path and re-assert the mixed-data case is unchanged.
- [ ] T3: Add `!is.finite()` guards for `amax` and `center` in
      `R/coord_circumplex.R`, with error-branch tests for `Inf`/`-Inf`/`NA`/`NaN`.
- [ ] T4: Regenerate the affected vdiffr baseline(s) and confirm unaffected
      plots regenerate byte-identically.
- [ ] T5: `devtools::document()`, full `devtools::test()`, `devtools::check()`;
      NEWS.md entries for both fixes.

## Work log

- 2026-07-18: created by /milestone-plan. Absorbs two items from the ROADMAP
  "continuous / infrastructure refactors" candidate row (legend glyph, M35-found,
  M33-inherited; `amax`/`center` guard, M32 review). Legend behavior chosen at
  the plan gate: both keys always drawn.

## Decisions

## Review
