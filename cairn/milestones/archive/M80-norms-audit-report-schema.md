# M80: Give the norms-audit coverage report a machine-readable key

**Status:** done (2026-08-13, PR #109 https://github.com/jmgirard/circumplex/pull/109)

**Goal:** Make every `data-raw/norms-audit-coverage.csv` row keyed rather than
string-pasted, and refuse a batch whose comparison parameters are unusable.

**Outcome:** The coverage frame is ten declared columns (`COVERAGE_COLUMNS`), one
fact per cell, all seven emitters rewritten onto it with a per-side header comment;
sample, citekey and block tag ride in their own columns. Note-only rows dedupe on
the note row's sample, scale, value and anchor, within a pass and across passes.
`validate_batch()` refuses a missing, non-numeric, `NA`, non-finite or non-positive
`divisor`, naming its batch row. `normalise_items()` composes a `^[0-9]+$` shape
test with `as.integer()`'s verdict, `values_agree()` normalises both sides, and
instrument-level note rows no `scales = TRUE` pass reads are reported. `data-raw/`
and `tests/` only: no audited comparison moved.

**Decisions:** D-M80-1 — the note-only dedupe key takes the note row's `sample`
cell as well as its scale, value and anchor, authorizing a second AC3 amendment.

**Review:** Three rounds; 1 and 2 returned on AC5, each time a guard admitting a
cell it should refuse (F3 87, G1 93), closed by composing the two refusals. Round 3
actioned F11 (82) plus three false comments below the bar; 24 logged, highest F1
(72), the note-only emitter leaving the report's `sample` cell NA.
