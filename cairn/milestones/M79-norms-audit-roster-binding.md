# M79: Bind the norms audit's batch to the shipped roster

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m79-norms-audit-roster-binding` · https://github.com/jmgirard/circumplex/pull/107

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

- [x] AC1 `audit_norms()` emits a `shipped-sample-not-audited` coverage row
      with `exempt = FALSE` for every shipped (instrument, sample) pair no
      `AUDIT_BATCH` row names. The roster is a parameter defaulting to the
      `data()`-plus-`circumplex_instrument`-class sweep crossed with each
      object's `Norms[[1]]$Sample`, and is taken from `objects` when a caller
      injects one, so fixture batches are unaffected and
      `tests/testthat/test-norms-audit-sample-key.R:127` stays green. The
      sweep is single-sourced with `tests/testthat/helper-norms.R:8`, not a
      second copy.
- [x] AC2 A test iterates `seq_len(nrow(AUDIT_BATCH))`, drops that row, and
      asserts `audit_norms()` either aborts or returns a non-exempt gap count
      above zero. Measured 2026-08-08 at `cef9d36f`: 10 rows (the 9
      single-sample rows and `iipsc` sample 1) return 0 gaps and are the rows
      AC1 fixes; 6 rows (each multi-sample instrument's `scales = TRUE` row)
      abort in `validate_batch()` both before and after AC1; the remaining 8
      already report a gap.
- [x] AC3 A source note whose block is untagged is refused when more than one
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
- [ ] AC4 The audit parses no markdown fences and infers nothing: a line
      carrying the literal `<!-- audit-values-` is either an exact marker or
      an abort. Accepted, and nothing else, at column zero:
      `<!-- audit-values-end -->`, `<!-- audit-values-begin -->`, and
      `<!-- audit-values-begin: <tag> -->` with `<tag>` an instrument name.
      Every other line carrying the prefix — indented, inline in prose,
      misspelt, or with junk after its tag — aborts naming the line. So no
      fence, indent, or surrounding prose can hide a block from the sweep or
      invent a tag: a marker displayed inside a fence is read as the real
      marker it looks like, never silently dropped. Both hold at both
      scanning sites, `parse_source_note()` and `source_note_block_tags()`,
      which share one helper. A fixture note exercises each refused shape and
      each accepted one, plus the unclosed fence that formerly hid every
      later block.
- [ ] AC5 Every abort path in `data-raw/audit-norms.R` has a test asserting
      its specific message, and a test asserts the count of `stop(` occurrences
      across the bodies of every function the script defines equals the
      registry of enumerated abort cases — file-scoped because a count over one
      function's body is evaded by an abort landing in a helper, which this
      branch did. Where no-oping a `stop()` only relocates the error, the test
      records the mutant's surviving behavior instead of claiming the guard is
      load-bearing.
- [x] AC6 A test asserts the shipped (instrument, sample) pair set produced by
      AC1's roster sweep equals the `(instrument, sample)` pair set of
      `AUDIT_BATCH`, so shipping a new instrument fails by name rather than as
      an unattributed gap count. The comment on the existing real-roster
      assertion at `tests/testthat/test-norms-provenance.R:462-478` records
      that it becomes a roster check once AC1 lands.
- [x] AC7 `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
      re-running the audit leaves `data-raw/norms-audit-ledger.csv` and
      `data-raw/norms-audit-coverage.csv` unchanged but for their stamps.

## Coverage

- AC1 → T4
- AC2 → T5
- AC3 → T3
- AC4 → T1, T2, T10, T14, T15
- AC5 → T6, T11, T16
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1 One marker-scanning helper shared by both readers: strict forms,
      abort on the rest.
- [x] T2 Fixture note: fenced markers and `audit-values-beginning` parse as no
      block or tag, through both readers.
- [x] T3 Refuse an untagged block read by two instruments before any pass
      parses it; three tests from AC3.
- [x] T4 The `roster` sweep and `shipped-sample-not-audited` emitter in
      `audit_norms()`; single-source the shipped-instrument sweep.
- [x] T5 The drop-each-row test, its measured 10/6/8 partition in a comment.
- [x] T6 Abort-path tests for each `stop()` in `parse_source_note()` plus the
      count test binding them.
- [x] T7 The roster-identity test; comment updated at
      `tests/testthat/test-norms-provenance.R:462`.
- [x] T8 Re-run the audit (CSVs stamp-only), `devtools::test()`, full check.

Return 1 (2026-08-08), from the review findings below:

- [x] T9 F14: delete the duplicated marker-constant pair; reunite the doc
      comment with its function.
- [x] T10 AC4 as amended: drop `fenced_lines()`; one strict scanner refusing
      any ambiguous marker line (closes F1-F6), fixture per the criterion.
- [x] T11 F15: the `stop(`-count test counts occurrences, not lines.
- [x] T12 F11: a single-sourcing assertion that reddens on a second sweep.
- [x] T13 Re-run the audit and the full `check()`; CSVs stamp-only.

Return 2 (2026-08-08), from the review findings below:

- [x] T14 The definitional recognizer: two string literals plus one anchored
      whole-line regex, byte-exact, no trimming; the four `substring()` shapes
      and the colon-no-space and trailing-whitespace tolerances become aborts.
- [x] T15 The set-equality boundary test: enumerated accepted lines and
      near-misses asserted as a partition through `source_note_marker()`.
- [x] T16 AC5 as amended: the abort registry and `stop(`-count go file-scoped
      over every script function; message tests for the four that had none.
- [x] T17 Log the exhaustive-claim lesson (extend the derive-from-the-
      requirement family line); re-run the audit and the full check.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context, authored none of the criteria) returned findings on 7 of this milestone's 8 drafted criteria; all adopted. The load-bearing three: AC2 was unsatisfiable as written, since 6 of the 24 batch rows abort in `validate_batch()` before any coverage count exists; the drafted instrument-level-rows criterion would have emitted 16 duplicate coverage rows per pass and is re-cut into M80 AC6; the drafted marker criteria missed the end marker and `source_note_block_tags()` entirely, leaving the fence protection one-sided.
- 2026-08-08: a second audit pass on the criterion added at the gate found it duplicated the existing assertion at `tests/testthat/test-norms-provenance.R:478`, which already runs the real batch over the real notes; it was re-cut as AC6's roster-identity check, which nothing asserts today.
- 2026-08-08: plan gate chose reporting an unaudited shipped sample as a non-exempt coverage row over aborting, because the two sibling note-side sweeps already report and an abort would stop the audit exactly when a new instrument lands before its source note; falsified by a run where a reported gap is overlooked and unaudited data ships anyway.
- 2026-08-08: plan gate chose two milestones over one 12-fix milestone and over planning M79 alone; falsified by M80's coverage-emitter changes proving inseparable from M79's new emitter at implement time.
- 2026-08-08: Scope amended at the implementation gate to admit an unexported `instrument_names()` in `R/instrument_oop.R`. AC1 forbids a third copy of the shipped-instrument sweep, and investigation found two already exist — `R/instrument_oop.R:237-242` inside the exported `instruments()`, and `tests/testthat/helper-norms.R:8-15` — while `helper-norms.R` cannot read `.Rbuildignore`d `data-raw/` because its callers run against the installed package on CRAN. Jeff chose extraction over a third copy bound by an equality test.
- 2026-08-08: implementation gate chose extracting to `R/` over keeping a third copy bound by a drift test, and over extracting for only two of the three callers; falsified by the extraction changing any observable behaviour of the exported `instruments()`.
- 2026-08-09: T17 done, return 2 complete, status to review. `devtools::check(args = "--no-manual")` is Status OK -- 0 errors, 0 warnings, 0 notes, 16m15s -- and `devtools::test()` is FAIL 0 / WARN 4 / PASS 6871. `document()` at `cli.width = 500` emits 0 `resolve link` warnings and leaves the generated files diff-free; `pkgdown::check_pkgdown()` finds no problems; `cairn_validate` exits 0, all checks PASS with 48 pre-existing M7 advisories. Re-running the audit leaves the coverage CSV byte-identical and the ledger identical apart from its stamp columns (194 / 15 / 0 gaps). Verified this session against the recognizer rather than carried from the previous one: the three accepted shapes are accepted and the return-2 four, the colon-no-space and the trailing-whitespace tolerances all abort, as does a marker line carrying an embedded newline -- the anchored regex leaves no gap at `$`. The file-scoped registry pins 12 `stop(` sites across 6 script functions by message fragment, not by total alone.
- 2026-08-08: checkpoint mid-T17, session paused at Jeff's request to resume in the circumplex session: the lesson is logged (extends the derive-from-the-requirement LESSONS line), the audit is re-run at `13b0ccbc` (ledger stamp-only, coverage byte-identical, 194/15/0 gaps), and the Tasks section is compressed for the 150-line cap (`cairn_validate` all-pass, 48 pre-existing M7 advisories). The full `check(args = "--no-manual")` is NOT yet recorded -- T17 stays open on it; the resuming session runs it before completion.
- 2026-08-08: T14-T16 done. `source_note_marker()` is now two `identical()` comparisons against string literals and one anchored whole-line regex — no trimming, no substring arithmetic — so the accepted set is the three constants at `data-raw/audit-norms.R:124-126`. The return-2 four, the colon-no-space tolerance and the trailing-whitespace tolerance all abort by name, and the boundary is asserted as a partition (every ACCEPTED_MARKERS and single-line REFUSED_MARKERS shape classified, both directions). The AC5 registry is file-scoped: SCRIPT_ABORTS registers all 12 `stop(` sites by message fragment across every function the script defines, with new message tests for `validate_batch()`'s two, `shipped_values()`'s single-record abort and `source_note_block_tags()`'s not-found. Both guards mutation-checked: re-tolerating trailing whitespace reddens the partition test plus a refused-shape test, and an unregistered `stop()` reddens the count. `devtools::test()` FAIL 0 / WARN 4 / PASS 6871.
- 2026-08-08: return 2 opened; T14-T17 added for the two actioned findings. The gate chose the definitional recognizer (two string literals plus one anchored full-line regex, byte-exact, no trimming) over escalating via /milestone-brief: both failures share one mechanism — a procedural parser makes the accepted set emergent, so each review buys its next member — and making the set definitional removes the mechanism rather than patching its latest instance. Jeff declined the brief in chat. The colon-no-space and trailing-whitespace tolerances the return-1 tests pinned also fall under AC4's "and nothing else"; measured this session: every committed marker line already matches the three exact shapes, so nothing shipped is refused. Falsified by AC4 failing again with the recognizer definitional.
- 2026-08-08: amendment return: AC5 — "Every abort path in `data-raw/audit-norms.R` has a test asserting its specific message, and a test asserts the count of `stop(` occurrences across the bodies of every function the script defines equals the registry of enumerated abort cases — file-scoped because a count over one function's body is evaded by an abort landing in a helper, which this branch did. Where no-oping a `stop()` only relocates the error, the test records the mutant's surviving behavior instead of claiming the guard is load-bearing."
- 2026-08-08: review returned M79 to in-progress (return 2). AC4 fails inside its own domain again, by a new mechanism: the criterion says "Accepted, and nothing else" for three exact shapes, and four more are accepted because `substring()` on an exhausted string returns "" and the space before the terminator is therefore optional -- `<!-- audit-values-begin-->`, `<!-- audit-values-end-->`, and the padded variants. AC5 routes to a gated amendment instead: its count guard is scoped to `parse_source_note()`'s body, and this diff put a reachable abort in `source_note_marker()`, so the criterion's literal wording holds while its stated purpose does not. AC1, AC2, AC3, AC6, AC7 verified and ticked with fresh evidence. Blame-history and prior-review lenses reported no findings. Thrash trigger (b) fires -- AC4 twice, same shape -- so escalation is offered alongside the fix. Full findings and the 14 below-threshold ones in the Review section.
- 2026-08-08: T13 done, return 1 complete. `devtools::check(args = "--no-manual")` is Status OK -- 0 errors, 0 warnings, 0 notes, 15m44s -- `document()` is clean with no diff and 0 `resolve link` warnings, `pkgdown::check_pkgdown()` finds no problems, and `cairn_validate` exits 0 with all checks PASS (48 `work-log format` advisories, all pre-existing M7 history). Re-running the audit leaves the ledger and coverage CSVs identical apart from the ledger's commit stamps: 194 ledger rows, 15 coverage rows, 0 gaps, unchanged from the pre-return run, so removing the fence tracker moves no audit verdict.
- 2026-08-08: T9-T12 done. `fenced_lines()` is gone: the audit no longer parses markdown, and a line carrying `<!-- audit-values-` is either an exact column-zero marker or an abort naming it. That closes F1, F2, F3, F4 and F6 by removing what they were defects in, and F5 by construction -- the unclosed-fence regression test now sees both blocks where the fence tracker reported one. The duplicate `MARKER_BEGIN`/`MARKER_END` pair is deleted and `parse_source_note()`'s doc comment sits with its function again (F14). Each new guard was no-oped: two of the three I first wrote were dead (the leading-whitespace refusal is subsumed by not trimming, and the interior `-->` check by the tag pattern) and were removed rather than shipped; of the five that remain, four redden and the begin/end check relocates into the colon check, recorded in its fixture comment. The `stop(`-count test counts occurrences, not deparsed lines (F15), and the single-sourcing test now reads the audit script's source instead of comparing one function against itself -- adding a `data(package` sweep to `data-raw/` reddens it, as does renaming `instrument_names()` (F11).
- 2026-08-08: amendment return: AC4 — "The audit parses no markdown fences and infers nothing: a line carrying the literal `<!-- audit-values-` is either an exact marker or an abort. […] Every other line carrying the prefix — indented, inline in prose, misspelt, or with junk after its tag — aborts naming the line. So no fence, indent, or surrounding prose can hide a block from the sweep or invent a tag: a marker displayed inside a fence is read as the real marker it looks like, never silently dropped. […] A fixture note exercises each refused shape and each accepted one, plus the unclosed fence that formerly hid every later block."
- 2026-08-08: the amended AC4 was corrected once before any code was written against it. The first wording claimed a fenced marker is refused; without fence parsing a column-zero marker inside a fence is indistinguishable from a real one and is read as real. What the code can guarantee, and what the criterion now says, is that no such line is silently DROPPED — the property F5 broke.
- 2026-08-08: return-1 gate chose refusing an ambiguous marker line over completing the fence parser, and over escalating the question; F1, F2, F3, F4 and F5 are four independent defects in one hand-rolled markdown fence tracker the audit needs for nothing else, and fail-closed is this file's existing doctrine. Measured before choosing: no committed note carries a stray `<!-- audit-values-` occurrence, and the one note with a fence (`browne1982.md`) has no markers in it, so no committed note is refused by the change. Falsified by a source note legitimately needing to display a marker line it does not mean.
- 2026-08-08: return-1 gate chose fixing F11 (scored 78, below the action threshold) in this pass, because it is the only test standing behind AC1's single-sourcing clause and it passes even when the function it checks returns nothing.
- 2026-08-08: return 1 opened; T9-T13 added for the five actioned findings plus F11 and F15. AC4 amended at the gate above; AC5 unchanged, F15 being a defect in its test rather than in its wording.
- 2026-08-08: review returned M79 to in-progress (return 1). AC4 fails inside its own domain: `fenced_lines()` misses indented code blocks (F1, 87), a `~~~` line closes a backtick fence (F3, 80), and an unclosed fence silently hides every later block (F5, 83) -- the last a silent-loss path this diff INTRODUCED, so it fails the Goal and not only the criterion. AC4's tag clause also fails on F6 (90), junk after the tag being accepted. AC5 fails on F15's literal reading (75, logged). AC1, AC2, AC3, AC6, AC7 verified and ticked. F14 (92) is the duplicate constant pair. Full findings and the fence design question in the Review section.
- 2026-08-08: review in flight (PR #107). AC evidence and the consistency gate are recorded below; criterion checkboxes deliberately NOT yet ticked, pending triage of the three review lenses. Two lenses in, both independently naming one defect: `MARKER_BEGIN`/`MARKER_END` are defined twice, at `data-raw/audit-norms.R:123-124` and `:147-148`. Commit `d9b2ff48`'s message claims it moved them; it added a copy without deleting the original. Inert (identical rebind) but dead duplicate code, and the commit message is a record proven false -- history, so corrected forward here rather than edited.
- 2026-08-08: T8 done, all eight tasks complete, status to review. `devtools::check(args = "--no-manual")` is Status OK -- 0 errors, 0 warnings, 0 notes, 14 minutes -- and `document()` is clean with no diff in `man/`, `NAMESPACE` or the RcppExports pair. Re-running the audit leaves the ledger and coverage CSVs byte-identical apart from their commit stamps (194 ledger rows, 15 coverage rows, 0 gaps), so no shipped value or audit verdict moves on this branch.
- 2026-08-08: T6 done. All six `stop()` calls in `parse_source_note()` now have a case asserting their own message, and a count test binds the registry to the function body so a seventh abort fails the suite unregistered. Each was no-oped in turn and the surviving behaviour measured rather than assumed, which corrected three of the six labels I first wrote: the nesting guard relocates into the duplicate-tag guard (both untagged blocks carry tag "") rather than into a subscript error; the malformed-row guard returns the row with anchor NA rather than a shifted value; and the empty-value guard returns the row rather than relocating. Three are load-bearing (duplicate tags, malformed row, empty value) and three relocate.
- 2026-08-08: T4, T5, T7 done. `audit_norms()` now sweeps the shipped roster and reports a `shipped-sample-not-audited` gap for any (instrument, sample) the batch omits; the enumeration is the package's own, an unexported `instrument_names()` in `R/instrument_oop.R` that `instruments()` and `tests/testthat/helper-norms.R` now also call instead of each writing it out. Measured on the same probe that opened the milestone: dropping `isc` moves from gaps 0 with no row naming it to gaps 1 naming `isc` sample 1. Across all 24 batch rows, abort-or-gap went from 6/8 with 10 silent to 6/18 with 0 silent. `instruments()` output is byte-identical to the pre-extraction body, and `document()` is clean with no generated-file diff.
- 2026-08-08: T1-T3 done. One fence-aware, `-->`-anchored marker scanner now serves both `parse_source_note()` and `source_note_block_tags()`, which ran independent greps; a malformed marker aborts instead of yielding a tag (`audit-values-beginning` gave `"ning"`); an untagged block read by two instruments is refused. Each guard mutation-checked: removing the refusal, the fence-awareness, and the tag validation reddens 1, 3 and 2 of the new tests respectively and nothing else. The real audit is byte-identical -- 194 ledger rows, 15 coverage rows, 0 gaps -- and only browne1982.md carries a fence, with no markers, so nothing committed changes behaviour.
- 2026-08-08: AC3 and T3 amended at the implementation gate — refuse an untagged block read by two instruments rather than repair the `claimed` key downstream. The planned mechanism left the bad state reachable: both instruments' rows key alike on (field, sample, scale), so each is audited against the other's values while the coverage counts read tidy, and the code comment at `data-raw/audit-norms.R:141-143` already refuses the neighbouring ambiguous case. Measured: `horowitz2003` is the only note two instruments read and it is tagged, so nothing in the repo is refused.
- 2026-08-08: implementation gate chose refusing the untagged shared block over the planned per-instrument `claimed` key, and over doing both; falsified by a legitimate case appearing where two instruments must share one untagged note, which would need the tag mechanism extended rather than the refusal relaxed.
- 2026-08-08: plan gate declined the `parse_source_note(instrument = NULL)` finding (M75 review, scored 55) as intended behaviour per the design note at `data-raw/audit-norms.R:138-143` and unreachable through `audit_norms()`; falsified by a caller outside `audit_norms()` coming to rely on the parser.

## Decisions

## Review

### Return 2 — acceptance criteria, fresh evidence (2026-08-08)

Measured on the branch at `d954b0cc`; every figure re-run this pass, none
carried from return 1 or from implementation.

- **AC1** — `shipped_roster()` returns 24 pairs over 15 instruments; dropping
  `isc` from the batch emits exactly one non-exempt row,
  `instrument = "isc"`, `side = "shipped-sample-not-audited"`, `scale = "1"`.
  Ticked on that evidence. Recorded caveat, from finding 2 below (scored 78,
  under the action bar): a caller passing `objects` for one instrument
  replaces the roster wholesale, so a single-instrument injection reports 0
  gaps where the same batch reports 23 without it. AC1's stated behaviour holds
  on the default path; the `objects` path is the logged concern.
- **AC2** — the drop-each-row sweep over all 24 batch rows, re-measured:
  **6 aborts, 18 gap>0, 0 silent**, against the criterion's stated pre-fix
  6/8/10. The 6 aborts are the multi-sample `scales = TRUE` rows the criterion
  predicts.
- **AC3** — an untagged block read by two instruments aborts with
  "source note shared carries an untagged audit-values block but is read by 2
  instruments (fx, fy)" — both instruments and the citekey named, message
  asserted rather than bare failure. `horowitz2003` is confirmed the only note
  two instruments read, and its blocks tag `iip64`, `iip32`, so nothing
  committed is refused.
- **AC4** — **FAILS.** `fenced_lines()` is gone (`exists()` FALSE), one
  `source_note_markers()` serves both readers, and all ten refused shapes abort
  by name. But AC4 says "Accepted, **and nothing else**, at column zero" for
  three exact shapes, and four further shapes are accepted:
  `<!-- audit-values-begin-->` and `<!-- audit-values-end-->` (no space before
  the terminator), `<!-- audit-values-begin      -->` (padded), and
  `<!-- audit-values-begin:   iip32   -->` (padded tag). `substring()` on an
  exhausted string returns `""`, so the space is optional at both sites. The
  in-file comment makes the same "and nothing else" claim and is equally wrong.
  Inside AC4's own domain — which lines count as markers. Not ticked.
- **AC5** — **criterion wrong, not the code.** All 6 `stop(` occurrences in
  `parse_source_note()`'s body have a registered case, the count test now
  counts occurrences rather than deparsed lines, and each abort asserts its own
  message. But the marker abort at `:154` lives in `source_note_marker()`, is
  reachable from `parse_source_note()` on most malformed notes, and is invisible
  to a count scoped to one function body. AC5's literal wording is satisfied;
  its stated purpose — "a new `stop()` fails the suite" — is not. Routes to a
  gated amendment. Not ticked.
- **AC6** — the roster-identity assertion is `TRUE` at review: the batch's
  (instrument, sample) pair set equals the roster's, pinned non-vacuously at 24
  pairs over 15 instruments.
- **AC7** — `devtools::test()` FAIL 0 / WARN 4 / PASS 6845.
  `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings /
  0 notes, 15m44s, on the code at `3e3134e5`; only `cairn/` files and the
  ledger's commit stamps have changed since. Re-running the audit leaves
  `norms-audit-coverage.csv` byte-identical and the ledger identical apart from
  its stamp columns (194 / 15 / 0 gaps).

### Return 2 — consistency gate (2026-08-08)

Universal: `cairn_validate` exit 0, all checks PASS (48 `work-log format`
advisories, all pre-existing M7 history). `cairn_impact` skipped — `DESIGN.md`
is unchanged on this branch.

Profile (`r-package` `consistency-gate`): `document()` at `cli.width = 500`
emits 0 `resolve link` warnings and no diff in `man/`, `NAMESPACE`, `data/` or
the RcppExports pair; `pkgdown::check_pkgdown()` reports no problems; README.md
not stale; no new top-level files, so no `.Rbuildignore` entry is owed; full
`check()` clean as recorded under AC7. **No NEWS entry:** the only shipped-code
change remains the extraction of two unexported helpers, with `instruments()`
byte-identical, so there is no user-visible behaviour an entry could assert.

### Return 2 — review findings (2026-08-08, PR #107)

Three fresh-context lenses (diff-bug [O], blame-history [S], prior-review [S]),
then a [S] scorer that generated none of them. The blame-history and
prior-review lenses each reported **no findings**: the fence tracker was
introduced and removed inside this one milestone so no prior milestone's intent
is undone, the `instrument_names()` extraction continues M14's single-sourced
roster line with `instruments()` byte-identical, and no return-1 finding is
reintroduced. 16 candidates from the diff lens, 2 scored >= 80.

**Actioned (>= 80).** Both return the milestone; neither is fixed review-side.

- (85) AC4's accepted set is wider than the criterion. `<!-- audit-values-begin-->`,
  `<!-- audit-values-end-->`, `<!-- audit-values-begin      -->` and
  `<!-- audit-values-begin:   iip32   -->` are all accepted, against AC4's
  "Accepted, and nothing else". Verified by the reviewer and again by this
  session. **Defect return** — AC4 fails inside its own domain, the second time
  this milestone's marker-recognition predicate has failed by a new mechanism.
- (85) AC5's count guard is evadable by moving an abort into a helper, and this
  diff already did so: the marker `stop()` at `data-raw/audit-norms.R:154` is
  reachable from `parse_source_note()` and invisible to a count over that one
  function's body. AC5's literal wording holds, so the criterion is what is
  wrong. **Amendment return** for AC5.

**Logged below threshold, not actioned (14).** (78) `objects` replaces the
roster wholesale while `audit_norms()` treats it as a per-instrument override
with fallback, so injecting one instrument reports 0 gaps where the same batch
reports 23 -- the F8 shape from return 1, now measured to report *clean* rather
than merely empty. (65) the T12 single-sourcing test is satisfied by a comment
containing `instrument_names`, and a hard-coded roster copy in `data-raw/`
leaves all 12 roster tests green. (60) `shipped_roster()` raises a raw R error
on a norms table with no `Sample` column, and silently drops partly-`NA`
samples. (60) AC1 and T4 both name a `roster` parameter `audit_norms()` does not
have. (55) a tag is pattern-checked but never bound to a real instrument name.
(50) the AC2 drop-each-row test counts any error as "noticed" and asserts no
partition. (50) `source_note_block_tags()` skips the nesting check
`parse_source_note()` performs. (35) `refuse_shared_untagged_blocks()` scans all
of a note's blocks rather than the selected ones. (30) `source_note_tags()` has
no production caller. (30) the roster/batch join key uses a space separator
where the file elsewhere uses `\r`. (25) a near-miss prefix (`<!--audit-values-`,
no space) is invisible -- pre-existing on master, not introduced here. (20)
`shipped_roster()` enumerates via `data()` but fetches via `get()`. (20) two new
fence tests also pass against master. (15) the two new `R/` helpers use `#`
rather than `@noRd`.

**Disposition: returned to `in-progress`. Return 2 of this milestone.** AC1,
AC2, AC3, AC6 and AC7 are verified and ticked. Thrash trigger (b) fires: AC4 has
now failed twice, each time by a new mechanism of the same shape -- which lines
count as markers. The return-1 gate's recorded alternative (completing the fence
parser) does not bear on this mechanism, so escalation via `/milestone-brief` is
offered at the routing point alongside the direct fix.

### Acceptance criteria — fresh evidence (2026-08-08)

Measured on the branch at `d9b2ff48`; every figure below re-run at review time,
not carried from implementation.

- **AC1** — `shipped_roster()` returns 24 pairs over 15 instruments and
  `audit_norms()` emits `shipped-sample-not-audited` (`exempt = FALSE`) for any
  pair the batch omits: dropping `isc` yields one row, `instrument = "isc"`,
  `scale = "1"`. The roster defaults to the `data()`-plus-class sweep crossed
  with `Norms[[1]]$Sample` and takes `objects` when injected, so
  `test-norms-audit-sample-key.R:127` still asserts zero non-exempt rows.
  Single-sourced by code reading, not by runtime comparison:
  `helper-norms.R:12,17` call `circumplex:::instrument_names()` /
  `instrument_object()` directly and `shipped_roster()` reaches the same
  definition via `asNamespace()`, so no second sweep exists in `data-raw/`.
  (The runtime comparison first recorded here was circular -- both sides
  bottom out in one function -- as review finding F11 established.)
- **AC2** — the drop-each-row test over `seq_len(nrow(AUDIT_BATCH))` passes.
  Re-measured at review: 6 aborts, 18 gap>0, **0 silent** — against the
  criterion's stated pre-fix 6/8/10. The 6 aborts are the multi-sample
  instruments' `scales = TRUE` rows, as the criterion predicts.
- **AC3** — an untagged block read by two instruments aborts naming both and
  the citekey; the message is asserted, not bare failure. A note read by one
  instrument and a tagged note read by two both still parse. Confirmed
  `horowitz2003` is the only shared note in the repo and is tagged, so nothing
  shipped is refused.
- **AC4** — one `source_note_markers()` serves both `parse_source_note()` and
  `source_note_block_tags()`. A fenced begin marker, a fenced end marker and
  `audit-values-beginning` are all refused or ignored as specified; the tag
  shapes `""`, `"iip64"` and a colon-without-space all read correctly.
- **AC5** — 6 `stop(` occurrences in `parse_source_note()`'s body, 6 registered
  cases in `PARSE_ABORTS`, and the count test binds them. Each abort asserts
  its own message. Each was no-oped and the surviving behaviour recorded: 3
  load-bearing (the parser returns rows), 3 relocate.
- **AC6** — the roster-identity test asserts the batch and roster pair sets are
  equal (`TRUE` at review) with non-vacuity pins at 24 pairs / 15 instruments,
  and the comment at `test-norms-provenance.R:478` now records that the
  assertion covers the shipped side since this milestone.
- **AC7** — `devtools::check(args = "--no-manual")` Status OK, 0 errors /
  0 warnings / 0 notes, 14m, at `d9b2ff48`; no non-`cairn/` file has changed
  since. Re-running the audit leaves the ledger and coverage CSVs
  byte-identical apart from their commit stamps (194 / 15 / 0 gaps).

### Consistency gate (2026-08-08)

Universal: `cairn_validate` exit 0, all checks PASS (47 `work-log format`
advisories, all pre-existing M7 history). `cairn_impact` skipped — `DESIGN.md`
is unchanged on this branch, so no principle moved.

Profile (`r-package` `consistency-gate` slot): `document()` at
`cli.width = 500` emits 0 `resolve link` warnings and leaves `man/`,
`NAMESPACE`, `data/` and the RcppExports pair diff-free; README.md not stale;
`pkgdown::check_pkgdown()` passes; no new top-level files, so no
`.Rbuildignore` entry is owed; full `check()` clean as recorded under AC7.
**No NEWS entry:** the only shipped-code change is extracting two unexported
helpers, and `instruments()` output is byte-identical to the pre-extraction
body — there is no user-visible behaviour to assert, and an entry asserting one
would have no test that fails without it.

### Review findings (2026-08-08, PR #107)

Three fresh-context lenses (diff-bug [O], blame-history [S], prior-review [S]),
then a [S] scorer that generated none of them. 19 candidate findings, 5 scored
>= 80 and actioned, 12 logged below threshold, 2 cleared as not-defects.

**Actioned (>= 80).** All five return the milestone; none is fixed review-side.

- F14 (92) `MARKER_BEGIN`/`MARKER_END` are defined twice, `data-raw/audit-norms.R:123-124`
  and `:147-148`, both added by this diff. Inert rebind, but dead duplicate code and
  a drift hazard, and it orphans `parse_source_note()`'s doc comment from its function.
  Found independently by two lenses. Commit `d9b2ff48`'s message claims the constants
  were moved; they were copied. That message is history and is corrected forward here.
- F6 (90) `source_note_tags()` accepts arbitrary text after the tag -- only the last
  `-->` is stripped. `"<!-- audit-values-begin: iip64 --> and more -->"` yields the tag
  `"iip64 --> and more"`, and `"...begin:: fx -->"` yields `": fx"`. AC4 requires the
  strict `: <tag>` form, and the code comment at `:188-189` claims a marker is
  recognised "only on a line that is nothing BUT the marker".
- F1 (87) `fenced_lines()` sees only ```` ``` ````/`~~~` fences, never a 4-space indented
  code block, and `source_note_markers()` trims the line so indentation cannot protect
  it. An indented example yields a phantom `"example"` tag and a phantom
  `note-block-not-audited` gap -- failure shapes (a) and (b) from the new test file's
  own header, reproduced in the sibling markdown syntax.
- F5 (83) An unclosed fence after a real block silently hides every later block:
  `source_note_block_tags()` reports only the first, the unclaimed-block sweep reports
  nothing, and `refuse_shared_untagged_blocks()` cannot see the hidden block. **This is
  a silent-loss path this diff introduced** -- before it, fences were ignored entirely
  and no real block could be hidden by one. It fails the milestone's Goal, not only AC4.
- F3 (80) A `~~~` line closes a ```` ``` ```` fence and vice versa, so a marker still inside
  the fence is reported unfenced.

**Logged below threshold, not actioned (12).** F11 (78) the single-sourcing test at
`test-norms-audit-roster.R:108-114` is a tautology -- both sides call one function, and
rebinding it to return nothing leaves the assertion passing; it also invalidated this
review's first AC1 evidence line, corrected above. F4 (76) a line beginning with an
inline code span flips fence parity for the rest of the note. F15 (75) the `stop(`-count
test counts deparsed lines containing the substring, not occurrences, against AC5's
literal wording. F2 (72) nested fences double-toggle parity. F8 (68) a partial `objects`
override silently empties the roster, `shipped_roster()` replacing it wholesale while
`audit_norms()` treats `objects` as a per-instrument override with fallback. F7 (62) a
bare `: -->` degrades to untagged. F18 (50) begin aborts on a malformed tail while end
silently ignores. F9 (45), F10 (45) malformed `objects` shapes raise raw R errors.
F16 (42) the AC6 test re-implements the production join key. F17 (35) the shared-note
refusal is batch-relative. F19 (22) `#` comments where the repo uses `@noRd`.

**Cleared:** `instruments()` is byte-identical post-extraction; no deleted line was
itself a prior fix, and no D-entry is contradicted.

**Disposition: returned to `in-progress`.** AC4 fails inside its own domain on F1-F5
and AC5 on F15's literal reading; AC1, AC2, AC3, AC6 and AC7 are verified and ticked.
Return 1 of this milestone.

**For the next implementation pass**, a design question this review will not settle:
whether to keep parsing markdown fences at all. Getting `fenced_lines()` right means
handling indented blocks, nested fences, mismatched fence characters and unclosed
fences -- a markdown parser the audit does not otherwise need. The alternative, more in
keeping with this file's fail-closed doctrine, is to stop inferring and refuse: abort on
a note whose marker lines are not unambiguous. That may need an AC4 amendment, which is
a gated decision, not a review-side one.
