# M76: Disclose the reference sample at the standardizing call site

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR16
- **Principles touched:** GP2, GP4
- **Branch/PR:** `m76-norms-call-site-disclosure`

## Goal

Make `norm_standardize()` say which normative sample it standardized against.

## Scope

**In:** a `quiet` argument on `norm_standardize()` defaulting to `FALSE`; a
message on every non-quiet call naming the sample used, its size and its
population; an attribute on the returned frame carrying the same facts; and two
fixes to refusal messages in the same function — the `Abbrev`/`Scale` column
assumption at `R/tidying_functions.R:197`, and the unmatched-`sample` case that
today dies in the arity `stopifnot()` at `:182` without naming `sample`. Added
at the RR16 ingest gate: a regression pin on the three identifier surfaces RR16
R1 keeps (AC9), and RR16 B1's "or custom" claim in `norm_standardize()`'s
roxygen, whose help page this milestone already rewrites.

**Out:** what the docs claim these samples *are* → M77 (planned, depends on this
milestone). Renaming the `Population` column or any `norm*` identifier → declined
at this plan gate and closed on the merits by RR16 R1/R5/R6 (D-041). A
`getOption()` global mute beside `quiet` → ROADMAP candidate row; the argument is
the whole ask here. RR16 BC3's per-sample reference-kind column, and the
extension of this milestone's message and attribute to carry it (RR16 B2) → a
new milestone, ROADMAP candidate row; a `data-raw/` schema change and a
`norms()` print change are both outside this scope.

## Acceptance criteria

- [ ] AC1 `norm_standardize()` takes `quiet`, default `FALSE`. On a successful
      non-quiet call it emits a message naming the sample number used and that
      sample's `Size` and `Population`, read from the `Norms[[2]]` row whose
      `Sample` equals the number used rather than by row position; where the
      instrument carries more than one sample the message also says how many
      others exist. `quiet = TRUE` emits nothing. Each message form is tested
      over the shipped instruments that can exhibit it — multi-sample membership
      decided by `nrow(Norms[[2]]) > 1` over the `shipped_instruments()`
      enumeration in `tests/testthat/helper-norms.R` — and the two memberships
      are asserted to partition all 15. The `Sample`-keyed read is fenced by a
      constructed instrument whose `Norms[[2]]` row order differs from its
      `Sample` values, which no shipped instrument does.
- [ ] AC2 Every successful call returns a frame carrying an attribute recording
      the instrument abbreviation, the sample number used, its `Size` and its
      `Population`, on both the `append = TRUE` (`R/tidying_functions.R:242`)
      and `append = FALSE` (`:244`) return paths, and `@return` documents it.
      Tested over the `shipped_instruments()` enumeration at both `append`
      values.
- [ ] AC3 The out-of-anchor-range refusal names the offending scales for an
      instrument whose `Norms[[1]]` labels its second column `Abbrev`, as it
      already does for one labelled `Scale`. Fenced by a constructed
      `Abbrev`-labelled violating instrument, because `cais` is the only shipped
      violator and is `Scale`-labelled — so the shipped roster cannot exhibit
      this. The exact-set pin at `tests/testthat/test-norms-anchor-range.R:46`
      is left standing.
- [ ] AC4 A `sample` matching no `Norms[[1]]` row is refused by its own check,
      distinct from the scales-vs-norms arity `stopifnot()`, which is retained
      and separately tested. The error names the `sample` argument and lists the
      sample numbers the instrument carries; the test asserts those numbers
      appear, not merely the word "sample".
- [ ] AC5 `?norm_standardize` documents `quiet`, the message, the attribute and
      both refusal conditions, and its `@examples` include one call on a
      multi-sample instrument that omits `sample`, so the disclosure appears in
      shipped documentation. Its opening "normative data (from the package or
      custom)" (`R/tidying_functions.R:127`) either drops "or custom" or states
      what it means, the signature admitting only a `circumplex_instrument`
      (`:172`). `Rscript -e 'options(cli.width = 500); devtools::document()'`
      produces no diff and zero lines matching `resolve link`.
- [ ] AC6 `NEWS.md` carries user-facing entries for `quiet`, the message, the
      attribute and the two refusal-message fixes, naming no test file. Each
      entry's asserted behavior has a test that fails without it; the
      entry-to-test mapping is recorded in this milestone's work log, not in
      NEWS.
- [ ] AC7 `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).
- [ ] AC8 (BC2) The disclosure message emitted by `norm_standardize()` (M76 AC1)
      contains the selected sample's `Norms[[2]]$Population` value verbatim, and
      its fixed (non-data) message text contains neither the token "population"
      nor the token "representative", case-insensitively; the verbatim-value
      clause is asserted over every (instrument, sample) pair in the
      `shipped_instruments()` enumeration — all 24, not one sample per
      instrument — and the token-absence clause against the message's fixed
      source text or a message built with a sentinel `Population` carrying both
      tokens, so the procedure asserts what the tolerance states. Tolerance:
      exact string absence in the fixed text; a shipped `Population` *value*
      containing those tokens does not violate this criterion.
- [ ] AC9 (BC1) `norm_standardize` and `norms` both appear in `NAMESPACE` as
      exports under exactly those names, and every instrument in the
      `shipped_instruments()` enumeration has a `Norms` list slot whose second
      element contains a column named `Population`. Asserted by a regression pin
      that fails if any of the three surfaces is renamed or dropped. Tolerance:
      exact; any departure is a deviation.

### Deviations from RR16

| BC | Departure | Why |
|---|---|---|
| BC1 | Ingested at AC9 as a standing regression pin rather than a merge-time assertion, and its `Rscript -e 'utils::data(package="circumplex")'` enumeration replaced by `shipped_instruments()` (`tests/testthat/helper-norms.R`). | The RR's procedure creates no objects to filter — `utils::data(package=)` prints a listing — while `shipped_instruments()` is the same enumeration, working. Verbatim, BC1 passed before any work began and no in-scope work could falsify it (ingest audit, 2026-08-08); the user chose the pin at the ingest gate. |
| BC2 | The verbatim-`Population` clause requantified over all 24 (instrument, sample) pairs; the token-absence clause aimed at the fixed source text or a sentinel fixture rather than the emitted message. | As written it quantifies over the selected sample but names a procedure enumerating 15 instruments with `sample = 1` defaulted, so 9 of 24 samples never emit a message; and its tolerance carves out exactly the case the named whole-message assertion would fail on (ingest audit, 2026-08-08). |
| BC3 | Not ingested here. | It mandates a new column in all 15 `data-raw/` builders plus a `norms()` print change, both outside this milestone's and M77's Scope. RR16 R3 leaves scheduling to the maintainer, who chose a new milestone at the ingest gate; ROADMAP candidate row. |

## Coverage

- AC1 → T2, T3
- AC2 → T4
- AC3 → T5
- AC4 → T6
- AC5 → T7
- AC6 → T8
- AC7 → T1, T8
- AC8 → T2, T3
- AC9 → T9

## Tasks

- [x] T1 Re-fence the existing call sites that will stop being silent:
      `tests/testthat/test-norms-anchor-range.R`'s `expect_silent` case and the
      explicit-`sample` calls in `tests/testthat/test-tidying_functions.R:43,49,191`.
      Decide per site whether it passes `quiet = TRUE` or asserts the message.
- [x] T2 Test-first for AC1 and AC8: both message forms over the partitioned
      roster, the verbatim-`Population` assertion over all 24 (instrument,
      sample) pairs, the fixed-text token absence via a sentinel-`Population`
      fixture, `quiet = TRUE` silence, and the row-order-vs-`Sample` fixture.
- [x] T3 Implement `quiet` and the message in `R/tidying_functions.R`, keying
      the `Size`/`Population` read on `Sample`. Use base `message()`, the
      package's established idiom (`R/ssm_analysis.R:350`); cli is not an Import.
      The fixed text frames the stored value as a description and uses neither
      "population" nor "representative" (AC8).
- [x] T4 Test-first, then implement, the returned attribute on both `append`
      branches.
- [x] T5 Test-first, then fix, the `Abbrev`/`Scale` column assumption at
      `R/tidying_functions.R:197`.
- [x] T6 Test-first, then implement, the unmatched-`sample` refusal, leaving the
      arity check in place with its own test.
- [x] T7 Roxygen for `quiet`, `@return`, the message and both refusals; add the
      default-sample example; resolve the "or custom" claim at
      `R/tidying_functions.R:127`; run `document()`.
- [x] T8 NEWS entries; full `test()` and `check()`.
- [x] T9 The AC9 regression pin on the two exports and the `Population` column.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned 10 findings across M76 and M77 — 2 unsatisfiable, 1 factual error, 1 unbounded promise, 6 false-pass. All 10 had a determinate answer and were fixed before the gate; none became a gate question. Detail in the plan-session chat.
- 2026-08-08: plan gate chose an always-on message with a `quiet` escape over messaging only when the caller omitted `sample`; the narrower form fires exactly where the hazard is but leaves a deliberate chooser with no record, and the user asked for the broader form with a mute. Falsified by any report that the message is noise in loops or knitted documents that `quiet` does not answer.
- 2026-08-08: plan gate chose to leave every `norm*` identifier and the `Population` column alone over renaming them; the identifiers are not themselves claims, and a rename breaks stored data structures and user code for a defect that lives in prose. Falsified by evidence that users read the column name itself as the representativeness claim.

- 2026-08-08: RB16 opened on the norms vocabulary rename. M76 stays planned and workable — its `quiet`/message/attribute scope is independent of what the vocabulary is called — but an RR16 verdict renaming `norm_standardize()` would amend this file through the /milestone-implement step-6 gate.
- 2026-08-08: RR16 ingested. Verdict is no rename, so nothing in this file's existing scope changes; the amendment adds AC8 (BC2, the message's wording), AC9 (BC1 as a regression pin), RR16 B1's "or custom" fix to AC5, and T9. BC3 is routed out to a new milestone. Three departures recorded in the Deviations table above.
- 2026-08-08: ingest audit ([O], fresh context) over the RR16 criteria returned six findings — BC1 vacuous as an AC and naming a procedure that creates no objects to filter, BC2 quantifying over 24 samples while naming a 15-instrument procedure and carving out a tolerance its own procedure would fail, BC3 mandating work both milestones' Scope excludes, the R3-vs-BC3 gap leaving the attribute without the kind, and no milestone owning IP5's `data-raw/` derivation record. All six were determinate; BC3's own falsifier F2 does not fire (2 of 24 hard to place, under its threshold of 4), and every BC3 count verified against the shipped tree. Routing settled at the ingest gate.
- 2026-08-08: T2/T3/T1/T4 done in one checkpoint, T1 following T3 rather than leading it — the re-fencing adds `quiet = TRUE` at call sites, which cannot pass until the argument exists. Tests written red first (10 failures, all "unused argument (quiet = TRUE)" or no message emitted), then green. Two sites beyond T1's named three were re-fenced in the same pass: `test-norms-anchor-range.R`'s two `out <- norm_standardize(...)` calls, the same class as the `expect_silent` case T1 named.
- 2026-08-08: implementation choices settled at the pre-implementation gate — message form "Standardized against IIP-SC normative sample 1: N = 872, American college students." plus, on a multi-sample instrument, "1 other sample is available; see norms()."; and one list attribute `"norm_sample"` over four flat ones. The gate showed `see norms(iipsc)`; the shipped text says `see norms()` because `$Details` carries no lowercase object name to interpolate, and the existing anchor-range refusal already points at bare `norms()`.
- 2026-08-08: this file now carries 9 acceptance criteria, over the ~7 split tripwire. Not split: AC8 constrains AC1's message text and AC9 is a one-file test pin, so both land inside the same reviewable PR rather than forming an independent slice.

- 2026-08-08: T5 and T6 done. The `Abbrev`/`Scale` defect's pre-fix failure was verified rather than assumed: `key$Scale` is NULL on an Abbrev-labelled instrument, so `paste(NULL[outside], collapse = ", ")` yields `""` and the refusal named no scale at all — it did not print "NA", which the test's first draft asserted. The test now pins the empty-list shape it actually produced. Two further loud call sites in `test-tidying_functions.R` (the 0-vs-360 pair) were quieted in the same pass; the suite now emits no disclosure noise.

- 2026-08-08: T7 done — `quiet`, `@return`'s attribute, both refusal conditions, the multi-sample default-`sample` example, and RR16 B1's "or custom" opening, which now names the instrument argument the signature actually requires. `document()` regenerates `man/norm_standardize.Rd` only, with no `resolve link` line and no further diff on a second run.
- 2026-08-08: AC6 entry-to-test mapping. "reports which normative sample it used" + `quiet` → test-norms-disclosure.R "a single-sample instrument's message names the sample, size and description", "a multi-sample instrument's message says how many other samples exist", "quiet = TRUE emits nothing". "norm_sample attribute" → "both return paths carry the norm_sample attribute" and "the attribute is present whether or not the message was emitted". "names the offending scales for every instrument" → test-norms-anchor-range.R "the refusal names the offending scales on an Abbrev-labelled instrument". "a normative sample an instrument does not carry" → test-norms-disclosure.R "a sample the instrument does not carry is refused by name", with the retained arity check fenced by "the scales-vs-norms arity check is retained and still fires".
- 2026-08-08: the NEWS entry's first draft said the reference choice moves scores "half a standard deviation on average"; M74 measured 0.44, so the entry now reads "roughly half ... and by nearly twice that at the extreme" (0.78). Corrected before the entry was committed.
- 2026-08-08: full `devtools::test()` clean — 0 failures, 6537 passing. The 4 warnings are all in `test-ci_accuracy.R`, which this branch does not touch.

- 2026-08-08: T8 and T9 done. `devtools::check(args = "--no-manual")` clean — 0 errors, 0 warnings, 0 notes, vignettes rebuilt (13m 33s). T9's pin was written with the AC1/AC2 tests in the same file rather than as a separate late task; it asserts both exports and the `Population` column over the shipped enumeration. All tasks complete; status → review.

## Decisions

- 2026-08-08 (RR16 R1, R5, R6): `norm_standardize()`, `norms()`, `$Norms` and the `Population` column keep their names. The plan gate's provisional keep is now decided on the merits — the identifiers are the interpersonal-circumplex field's own usage rather than claims, the behavioral hazard runs through silence plus the definite article and is closed by this milestone's disclosure plus M77's prose, and the rename's benefit is near zero at any cost. Promoted to D-041; the ROADMAP's parked rename item is closed rather than deferred.
- 2026-08-08 (RR16 R2, BC2): the disclosure message prints the stored `Population` value as a plain description, never framed by the word "population" or by representativeness wording. Identifiers are not claims but message prose is, and this message is the package's most-visible surface.
- 2026-08-08 (RR16 R3, BC3, B2): the per-sample reference-kind column, and the extension of this milestone's message and attribute to carry it, go to a new milestone rather than into this scope. RR16 leaves scheduling to the maintainer, who chose that route at the ingest gate; the pre-2.0.0 window is what makes it cheap, so it is planned before the release.

## Review
