# M28: Occasions long-format sugar (`ssm_analyze_long()`) — DONE

- **Status:** done · **PR:** #52 (squash `93578ef`) · **Priority:** normal · **Depends on:** M25 · **Principles:** —

## Goal
Ship `ssm_analyze_long()`, a long-format (one row per person per occasion) wrapper that reshapes to wide and delegates to `ssm_analyze(occasions=)` (spec §1.1 "sugar, not design"; no new statistical surface).

## Outcome
Exported `ssm_analyze_long(data, scales, angles, id, occasion, grouping, contrast, ...)`: reshapes long→wide (scores by position; occasion order = factor levels else first-appearance, never alphabetical) and delegates, inheriting estimation, paired contrasts, and listwise missing-wave handling. 14 tests (round-trip equivalence to the wide `occasions=` call across bootstrap / Monte Carlo / contrast / numeric-index; T10/T2 + factor-level ordering; validation errors). `check(--no-manual)` 0/0/0.

## Key decisions
- AC3 amended at plan-gate: wide-format "ragged scale sets" → the long-format surface (duplicate `(id, occasion)`, `<2` occasions, time-varying grouping); long input shares one score-column set.
- Signature `id`/`occasion` (standard evaluation per D-014; `occasion` mirrors `occasions=`).

## Review
Three lenses + scorer. **F1** (scored 96, fixed `a7342cc`): the wide grouping value was taken from each person's first *physical* row, silently dropping a person whose group was NA at their first-listed occasion but valid later; fixed to first non-NA per person + regression test. No sub-80 findings; all four ACs verified against fresh evidence; consistency gate green.
