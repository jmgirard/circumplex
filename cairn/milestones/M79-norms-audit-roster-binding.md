# M79: Bind the norms audit's batch to the shipped roster

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m79-norms-audit-roster-binding`

## Goal

Make `data-raw/audit-norms.R` unable to report a clean run over shipped norm
data it never read.

## Scope

**In:** `data-raw/audit-norms.R` and its tests, plus one unexported
`instrument_names()` in `R/instrument_oop.R` that single-sources the
shipped-instrument sweep the exported `instruments()` and
`tests/testthat/helper-norms.R:8` already write out separately — widened at
the 2026-08-08 implementation gate, AC1 forbidding a third copy while
`helper-norms.R` cannot read `.Rbuildignore`d `data-raw/`. The audit enumerates
`AUDIT_BATCH` and the source notes that batch names, and never the shipped
roster, so `AUDIT_BATCH` is bound to nothing: measured 2026-08-08 at
`cef9d36f`, dropping `isc` from the batch loses 17 audited values while the
ledger falls silently from 194 to 177 rows, the coverage report from 15 to 13,
and the non-exempt gap count stays at 0 with no row anywhere naming `isc` or
`hopwood2011`. This milestone closes that hole and hardens the note-block
parser that decides which source rows a pass sees at all — the sweep is only
as good as the block boundaries it trusts.

**Out:** the coverage report's column schema, `divisor` validation, the
`Items` normalisation asymmetry, and instrument-level note rows no
`scales = TRUE` pass reads → M80. Changing any value in `data/` → not here;
this milestone touches no shipped object. `parse_source_note()` returning a
note's single untagged block when the caller names no instrument → declined
at this gate, not deferred: the design note at `data-raw/audit-norms.R:138-143`
makes it deliberate, and it is unreachable through `audit_norms()`.

## Acceptance criteria

- [ ] AC1 `audit_norms()` emits a `shipped-sample-not-audited` coverage row
      with `exempt = FALSE` for every shipped (instrument, sample) pair no
      `AUDIT_BATCH` row names. The roster is a parameter defaulting to the
      `data()`-plus-`circumplex_instrument`-class sweep crossed with each
      object's `Norms[[1]]$Sample`, and is taken from `objects` when a caller
      injects one, so fixture batches are unaffected and
      `tests/testthat/test-norms-audit-sample-key.R:127` stays green. The
      sweep is single-sourced with `tests/testthat/helper-norms.R:8`, not a
      second copy.
- [ ] AC2 A test iterates `seq_len(nrow(AUDIT_BATCH))`, drops that row, and
      asserts `audit_norms()` either aborts or returns a non-exempt gap count
      above zero. Measured 2026-08-08 at `cef9d36f`: 10 rows (the 9
      single-sample rows and `iipsc` sample 1) return 0 gaps and are the rows
      AC1 fixes; 6 rows (each multi-sample instrument's `scales = TRUE` row)
      abort in `validate_batch()` both before and after AC1; the remaining 8
      already report a gap.
- [ ] AC3 A source note whose block is untagged is refused when more than one
      instrument's `AUDIT_BATCH` row reads it, the abort naming both
      instruments and the citekey. Two instruments' rows key alike on
      (field, sample, scale) — same octant names, same sample numbers — so one
      untagged block cannot audit both, and refusing makes the `claimed`-key
      collision the M75 review found unreachable rather than repairing it
      downstream. Three tests: the refused batch asserts the message; a note
      read by one instrument still parses; a tagged note read by two still
      parses. Measured 2026-08-08: `horowitz2003` is the only note two
      instruments read and it is already tagged, so nothing in the repo is
      refused by this.
- [ ] AC4 A begin or end marker is recognised only on a line whose trimmed
      text starts with that marker's prefix and ends with `-->`, and not when
      the line lies inside a fenced code block; and a tag is accepted only
      where the text between prefix and `-->` is empty or of the form
      `: <tag>`, so `<!-- audit-values-beginning -->` aborts rather than
      yielding the tag `"ning"`. Both hold at both scanning sites,
      `parse_source_note()` and `source_note_block_tags()`, which share one
      helper. A fixture note carries a fenced begin marker, a fenced end
      marker, and the malformed prefix.
- [ ] AC5 Every abort path in `parse_source_note()` has a test asserting its
      specific message, and a test asserts the count of `stop(` occurrences in
      that function's body equals the number of enumerated abort tests, so a
      new `stop()` fails the suite. Where no-oping a `stop()` only relocates
      the error, the test records the mutant's surviving behavior instead of
      claiming the guard is load-bearing.
- [ ] AC6 A test asserts the shipped (instrument, sample) pair set produced by
      AC1's roster sweep equals the `(instrument, sample)` pair set of
      `AUDIT_BATCH`, so shipping a new instrument fails by name rather than as
      an unattributed gap count. The comment on the existing real-roster
      assertion at `tests/testthat/test-norms-provenance.R:462-478` records
      that it becomes a roster check once AC1 lands.
- [ ] AC7 `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
      re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
      `data-raw/norms-audit-coverage.csv` unchanged but for their stamps.

## Coverage

- AC1 → T4
- AC2 → T5
- AC3 → T3
- AC4 → T1, T2
- AC5 → T6
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 Extract one marker-scanning helper from the two independent greps at
      `data-raw/audit-norms.R:160` and `:171-172`: anchored begin/end
      recognition, `-->` terminator, fence-awareness, strict `: <tag>` form
      with an abort on anything else.
- [x] T2 Fixture note carrying a fenced begin marker, a fenced end marker and
      `audit-values-beginning`; tests asserting none parses as a block or tag,
      through both `parse_source_note()` and `source_note_block_tags()`.
- [x] T3 Refuse an untagged block read by more than one instrument, before any
      pass parses it; the `claimed`/`blocks` key at `:365-370` is then
      collision-free by construction. Three tests from AC3.
- [x] T4 Add the `roster` parameter and the `shipped-sample-not-audited`
      emitter to `audit_norms()`; move the `data()`-plus-class sweep to one
      definition shared with `tests/testthat/helper-norms.R:8`.
- [x] T5 The drop-each-row test, with the measured 10/6/8 partition recorded
      in its comment as the reason the assertion is abort-or-gap.
- [x] T6 Abort-path tests for each `stop()` in `parse_source_note()`, plus the
      `stop(`-count test that fails when a new one lands untested.
- [x] T7 The roster-identity test; update the comment at
      `tests/testthat/test-norms-provenance.R:462`.
- [x] T8 Re-run the audit, confirm the two CSVs are unchanged, run
      `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context, authored none of the criteria) returned findings on 7 of this milestone's 8 drafted criteria; all adopted. The load-bearing three: AC2 was unsatisfiable as written, since 6 of the 24 batch rows abort in `validate_batch()` before any coverage count exists; the drafted instrument-level-rows criterion would have emitted 16 duplicate coverage rows per pass and is re-cut into M80 AC6; the drafted marker criteria missed the end marker and `source_note_block_tags()` entirely, leaving the fence protection one-sided.
- 2026-08-08: a second audit pass on the criterion added at the gate found it duplicated the existing assertion at `tests/testthat/test-norms-provenance.R:478`, which already runs the real batch over the real notes; it was re-cut as AC6's roster-identity check, which nothing asserts today.
- 2026-08-08: plan gate chose reporting an unaudited shipped sample as a non-exempt coverage row over aborting, because the two sibling note-side sweeps already report and an abort would stop the audit exactly when a new instrument lands before its source note; falsified by a run where a reported gap is overlooked and unaudited data ships anyway.
- 2026-08-08: plan gate chose two milestones over one 12-fix milestone and over planning M79 alone; falsified by M80's coverage-emitter changes proving inseparable from M79's new emitter at implement time.
- 2026-08-08: Scope amended at the implementation gate to admit an unexported `instrument_names()` in `R/instrument_oop.R`. AC1 forbids a third copy of the shipped-instrument sweep, and investigation found two already exist — `R/instrument_oop.R:237-242` inside the exported `instruments()`, and `tests/testthat/helper-norms.R:8-15` — while `helper-norms.R` cannot read `.Rbuildignore`d `data-raw/` because its callers run against the installed package on CRAN. Jeff chose extraction over a third copy bound by an equality test.
- 2026-08-08: implementation gate chose extracting to `R/` over keeping a third copy bound by a drift test, and over extracting for only two of the three callers; falsified by the extraction changing any observable behaviour of the exported `instruments()`.
- 2026-08-08: T8 done, all eight tasks complete, status to review. `devtools::check(args = "--no-manual")` is Status OK -- 0 errors, 0 warnings, 0 notes, 14 minutes -- and `document()` is clean with no diff in `man/`, `NAMESPACE` or the RcppExports pair. Re-running the audit leaves the ledger and coverage CSVs byte-identical apart from their commit stamps (194 ledger rows, 15 coverage rows, 0 gaps), so no shipped value or audit verdict moves on this branch.
- 2026-08-08: T6 done. All six `stop()` calls in `parse_source_note()` now have a case asserting their own message, and a count test binds the registry to the function body so a seventh abort fails the suite unregistered. Each was no-oped in turn and the surviving behaviour measured rather than assumed, which corrected three of the six labels I first wrote: the nesting guard relocates into the duplicate-tag guard (both untagged blocks carry tag "") rather than into a subscript error; the malformed-row guard returns the row with anchor NA rather than a shifted value; and the empty-value guard returns the row rather than relocating. Three are load-bearing (duplicate tags, malformed row, empty value) and three relocate.
- 2026-08-08: T4, T5, T7 done. `audit_norms()` now sweeps the shipped roster and reports a `shipped-sample-not-audited` gap for any (instrument, sample) the batch omits; the enumeration is the package's own, an unexported `instrument_names()` in `R/instrument_oop.R` that `instruments()` and `tests/testthat/helper-norms.R` now also call instead of each writing it out. Measured on the same probe that opened the milestone: dropping `isc` moves from gaps 0 with no row naming it to gaps 1 naming `isc` sample 1. Across all 24 batch rows, abort-or-gap went from 6/8 with 10 silent to 6/18 with 0 silent. `instruments()` output is byte-identical to the pre-extraction body, and `document()` is clean with no generated-file diff.
- 2026-08-08: T1-T3 done. One fence-aware, `-->`-anchored marker scanner now serves both `parse_source_note()` and `source_note_block_tags()`, which ran independent greps; a malformed marker aborts instead of yielding a tag (`audit-values-beginning` gave `"ning"`); an untagged block read by two instruments is refused. Each guard mutation-checked: removing the refusal, the fence-awareness, and the tag validation reddens 1, 3 and 2 of the new tests respectively and nothing else. The real audit is byte-identical -- 194 ledger rows, 15 coverage rows, 0 gaps -- and only browne1982.md carries a fence, with no markers, so nothing committed changes behaviour.
- 2026-08-08: AC3 and T3 amended at the implementation gate — refuse an untagged block read by two instruments rather than repair the `claimed` key downstream. The planned mechanism left the bad state reachable: both instruments' rows key alike on (field, sample, scale), so each is audited against the other's values while the coverage counts read tidy, and the code comment at `data-raw/audit-norms.R:141-143` already refuses the neighbouring ambiguous case. Measured: `horowitz2003` is the only note two instruments read and it is tagged, so nothing in the repo is refused.
- 2026-08-08: implementation gate chose refusing the untagged shared block over the planned per-instrument `claimed` key, and over doing both; falsified by a legitimate case appearing where two instruments must share one untagged note, which would need the tag mechanism extended rather than the refusal relaxed.
- 2026-08-08: plan gate declined the `parse_source_note(instrument = NULL)` finding (M75 review, scored 55) as intended behaviour per the design note at `data-raw/audit-norms.R:138-143` and unreachable through `audit_norms()`; falsified by a caller outside `audit_norms()` coming to rely on the parser.

## Decisions

## Review
