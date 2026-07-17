# M28: Occasions long-format sugar (`ssm_analyze_long()`) — DONE

- **Status:** done · **PR:** #52 (squash `93578ef`) · **Priority:** normal
- **Depends on:** M25 · **Principles touched:** —

## Goal
Ship `ssm_analyze_long()`, a long-format (one row per person per occasion)
convenience wrapper that reshapes to wide and delegates to
`ssm_analyze(occasions = )` (spec §1.1 "sugar, not design"; no new statistical
surface).

## Outcome
New exported `ssm_analyze_long(data, scales, angles, id, occasion, grouping,
contrast, ...)`. Reshapes long→wide (scores extracted by position; occasion
order = factor levels else first-appearance, never alphabetical) and delegates,
so estimation, paired within-person contrasts, and listwise missing-wave
handling are all inherited unchanged. 14 tests: round-trip equivalence to the
wide `occasions=` call (bootstrap + Monte Carlo + `contrast = TRUE` +
numeric-index), the T10/T2 + factor-level ordering regressions, and the
validation errors. `devtools::check(--no-manual)` 0/0/0.

## Key decisions
- AC3 amended at plan-gate: the wide-format "ragged scale sets" case was
  replaced with the long-format validation surface (duplicate `(id, occasion)`,
  `< 2` occasions, time-varying grouping) — long input shares one score-column
  set, so ragged scales cannot arise.
- Signature `id`/`occasion` (standard evaluation per D-014; `occasion` mirrors
  `ssm_analyze(occasions=)`).

## Review
Three lenses + scorer. **F1** (scored 96, fixed in-review, `a7342cc`): the wide
grouping value was taken from each person's first *physical* row, silently
dropping a person whose group was `NA` at their first-listed occasion but valid
later; fixed to first non-NA per person + regression test. No sub-80 findings.
All four acceptance criteria verified against fresh evidence; consistency gate
green.
