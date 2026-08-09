# M80: Give the norms-audit coverage report a machine-readable key

- **Status:** planned
- **Priority:** normal
- **Depends on:** M79
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** —

## Goal

Make every `data-raw/norms-audit-coverage.csv` row keyed rather than
string-pasted, and refuse a batch whose comparison parameters are unusable.

## Scope

**In:** the coverage frame's schema and `AUDIT_BATCH` input validation. Six
coverage emitters paste their payload into whichever column is free —
`field` becomes `"M (sample 1)"` at `data-raw/audit-norms.R:406` and `:414`,
`instrument` becomes `"horowitz2003 (iip32)"` at `:448`, and a note-only row
puts its `scale` in `field` and its `value` in `scale` at `:382-383` — so no
row can be joined to anything by machine. Also here: three comparison-side
defects that let a check pass without comparing anything.

**Out:** the shipped-roster sweep, the shared-note key collision, and the
marker/tag parser → M79, which this depends on because both milestones rewrite
the same emitters. Changing any value in `data/` → not here.

## Acceptance criteria

- [ ] AC1 Every coverage row carries a machine-readable key: `field` holds a
      bare field name and any sample label rides in its own `sample` column
      rather than being pasted into `field` (`:406`, `:414`). A header comment
      states the cell contents for each `side`, including the four that carry
      no field or scale of their own — `note-only-sample`,
      `constructed-credit-reference`, `note-sample-not-audited`,
      `note-block-not-audited` — whose payloads ride in mislabelled columns
      today. Coverage rows and ledger rows are disjoint by construction, a
      missing-key coverage row having no ledger counterpart, so what is
      required is a joinable key and not a join.
- [ ] AC2 The coverage report's `instrument` column holds a shipped instrument
      name, and `NA` only where no batch row identifies one
      (`note-block-not-audited`); a `note-sample-not-audited` row carries the
      claiming instrument from `blocks[[bkey]]$instrument`. Citekey and block
      tag ride in their own columns rather than inside `instrument` (`:448`).
- [ ] AC3 A `note-only` coverage row appears once per citekey, block, and the
      row's own payload — not once per batch pass that reads the block
      (`:379-385` sits inside the per-pass loop). A test asserts the full run
      still emits 14 note-only rows: every note-only row in the repo carries
      `sample = "—"`, so a key including `sample` but not the payload
      collapses those 14 to 8.
- [ ] AC4 `validate_batch()` refuses a `divisor` that is missing,
      non-numeric, `NA`, non-finite, or not strictly positive, each shape with
      its own test asserting the specific message.
- [ ] AC5 `values_agree()` compares `Items` after normalising both sides
      through `normalise_items()` — today only the shipped side is normalised
      (`:294` against the fall-through at `:336`) — and `normalise_items()`
      aborts on a cell that is not a comma-separated list of integers rather
      than coercing it to the string `"NA"`. A test asserts two unparseable
      cells do not compare equal, which without the abort they would.
- [ ] AC6 Instrument-level note rows (`sample` = the NO_SAMPLE token) in a
      block that no `scales = TRUE` batch pass reads are reported as
      non-exempt coverage rather than discarded at `:375-377`. Rows in a block
      whose instrument does have a `scales = TRUE` pass are covered by that
      pass and produce no additional row. A fixture supplies the uncovered
      case; no note in the repo has one today.
- [ ] AC7 `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
      `data-raw/norms-audit-coverage.csv` is regenerated and committed with the
      run that produces it, and the ledger CSV is unchanged but for its stamps.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1 Fix the coverage frame's columns — instrument, citekey, tag, side,
      field, sample, scale, exempt — and document each side's cell contents in
      a header comment beside `audit_norms()`.
- [ ] T2 Rewrite the six emitters (`:379-385`, `:387-396`, `:403-418`,
      `:447-453`, `:474-479`) onto that schema; update the printed summary at
      `:646-661` and the coverage assertions in
      `tests/testthat/test-norms-provenance.R`.
- [ ] T3 Dedupe note-only rows on citekey, tag and payload; test the full run
      still emits 14.
- [ ] T4 `divisor` validation in `validate_batch()` (`:82-99`), one test per
      refused shape.
- [ ] T5 Make `normalise_items()` abort on a non-integer-list cell and
      normalise the source side in `values_agree()`; test the two-unparseable
      -cells case.
- [ ] T6 Report instrument-level note rows in a block no `scales = TRUE` pass
      reads; fixture note plus test for both the uncovered and the
      already-covered case.
- [ ] T7 Regenerate the coverage CSV, run `devtools::test()` and
      `devtools::check(args = "--no-manual")`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context, authored none of the criteria) returned findings on all 5 drafted criteria; all adopted. Two were load-bearing: AC1's stated rationale was impossible, coverage rows and ledger rows being disjoint by construction, so the criterion now asks for a joinable key rather than a join; and AC3's drafted dedupe key of (citekey, block, sample) would itself have deleted 6 of the 14 shipped note-only rows, every one of which carries `sample = "—"` with its payload in `scale` — the silent row loss the rest of the script refuses.
- 2026-08-08: plan gate chose two milestones over one, putting this one second because both rewrite the same coverage emitters and a shared-emitter conflict is cheaper to take in sequence than in review; falsified by M79 landing without touching the emitters, which would make the dependency spurious.
- 2026-08-08: AC6 arrives here from M79's draft, where the criteria audit measured that implementing it as drafted would emit 16 duplicate rows for each of 8 passes; the real hole is narrower and has no instance in the repo, which is why it sits in the report milestone rather than the silent-loss one.

## Decisions

## Review
