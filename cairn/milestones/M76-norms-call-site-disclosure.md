# M76: Disclose the reference sample at the standardizing call site

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR16
- **Principles touched:** GP2, GP4
- **Branch/PR:** `m76-norms-call-site-disclosure` / https://github.com/jmgirard/circumplex/pull/104

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

- [x] AC1 `norm_standardize()` takes `quiet`, default `FALSE`. On a successful
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
- [x] AC2 Every successful call returns a frame carrying an attribute recording
      the instrument abbreviation, the sample number used, its `Size` and its
      `Population`, on both the `append = TRUE` (`R/tidying_functions.R:242`)
      and `append = FALSE` (`:244`) return paths, and `@return` documents it.
      Tested over the `shipped_instruments()` enumeration at both `append`
      values.
- [x] AC3 The out-of-anchor-range refusal names the offending scales for an
      instrument whose `Norms[[1]]` labels its second column `Abbrev`, as it
      already does for one labelled `Scale`. Fenced by a constructed
      `Abbrev`-labelled violating instrument, because `cais` is the only shipped
      violator and is `Scale`-labelled — so the shipped roster cannot exhibit
      this. The exact-set pin at `tests/testthat/test-norms-anchor-range.R:46`
      is left standing.
- [x] AC4 A `sample` matching no `Norms[[1]]` row is refused by its own check,
      distinct from the scales-vs-norms arity `stopifnot()`, which is retained
      and separately tested. The error names the `sample` argument and lists the
      sample numbers the instrument carries; the test asserts those numbers
      appear, not merely the word "sample".
- [x] AC5 `?norm_standardize` documents `quiet`, the message, the attribute and
      both refusal conditions, and its `@examples` include one call on a
      multi-sample instrument that omits `sample`, so the disclosure appears in
      shipped documentation. Its opening "normative data (from the package or
      custom)" (`R/tidying_functions.R:127`) either drops "or custom" or states
      what it means, the signature admitting only a `circumplex_instrument`
      (`:172`). `Rscript -e 'options(cli.width = 500); devtools::document()'`
      produces no diff and zero lines matching `resolve link`.
- [x] AC6 `NEWS.md` carries user-facing entries for `quiet`, the message, the
      attribute and the two refusal-message fixes, naming no test file. Each
      entry's asserted behavior has a test that fails without it; the
      entry-to-test mapping is recorded in this milestone's work log, not in
      NEWS.
- [x] AC7 `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).
- [x] AC8 (BC2) The disclosure message emitted by `norm_standardize()` (M76 AC1)
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
- [x] AC9 (BC1) `norm_standardize` and `norms` both appear in `NAMESPACE` as
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

- 2026-08-08: review returned the milestone to in-progress (defect return 1). F1 (90): the "N other samples are available" clause counts anchor-range-refused samples, so cais sample 1's message advertises a sample D-040 refuses. F4 (85): `sample = NA_real_` bypasses AC4's refusal, which is AC4 failing inside its own domain. AC1 and AC4 unticked; the other seven criteria keep their recorded evidence. F2, F10 and F16 are actioned without forcing the return.

- 2026-08-08: all five actioned findings fixed on the branch, tests-first (three new/changed assertions red first, each for its own reason). F1: the other-samples count now counts only samples `norm_standardize()` would accept, via a new internal `norm_sample_usable()` that shares the anchor-range predicate with the refusal rather than restating it — live, cais sample 1 no longer advertises its refused sibling while iipsc sample 1 still offers its usable one. F4: the `Sample` subset uses `which()`, which drops NAs from either side; the first attempt (`!is.na(key$Sample) & key$Sample == sample`) was wrong because `TRUE & NA` is NA, and was corrected before it was run. F2: the multi-sample assertion derives its expected count from the usability predicate instead of mirroring the implementation's `nrow() - 1`. F10: presence is asserted on each path before the two are compared. F16: the roxygen now says "the Value section below" rather than a literal tag name. F9 (78, below threshold) was fixed in the same pass since it sat in the test AC1 names — the roster size is now pinned at 15.
- 2026-08-08: post-fix `devtools::test()`: 0 failures, 6550 passing (up from 6537). The 4 WARNs remain `test-ci_accuracy.R`'s, untouched. AC1 and AC4 re-ticked on the live evidence recorded in the Review section.

## Decisions

- 2026-08-08 (RR16 R1, R5, R6): `norm_standardize()`, `norms()`, `$Norms` and the `Population` column keep their names. The plan gate's provisional keep is now decided on the merits — the identifiers are the interpersonal-circumplex field's own usage rather than claims, the behavioral hazard runs through silence plus the definite article and is closed by this milestone's disclosure plus M77's prose, and the rename's benefit is near zero at any cost. Promoted to D-041; the ROADMAP's parked rename item is closed rather than deferred.
- 2026-08-08 (RR16 R2, BC2): the disclosure message prints the stored `Population` value as a plain description, never framed by the word "population" or by representativeness wording. Identifiers are not claims but message prose is, and this message is the package's most-visible surface.
- 2026-08-08 (RR16 R3, BC3, B2): the per-sample reference-kind column, and the extension of this milestone's message and attribute to carry it, go to a new milestone rather than into this scope. RR16 leaves scheduling to the maintainer, who chose that route at the ingest gate; the pre-2.0.0 window is what makes it cheap, so it is planned before the release.

## Review

Verified 2026-08-08 against branch `m76-norms-call-site-disclosure` at `ac1e9e10`,
PR #104. `devtools::check()` ran at `ee061927`; `git diff ee061927..HEAD -- . ':!cairn'`
is empty, so the check evidence holds for HEAD.

- AC1 — Live: a default (`sample` omitted) call on iipsc emits "Standardized against
  IIP-SC normative sample 1: N = 872, American college students. 1 other sample is
  available; see norms()."; isc (single-sample) emits the form without the
  others clause. Tests: both message forms over their memberships, the two
  memberships asserted to partition all 15, `quiet = TRUE` silence over every
  usable sample, and the row-order-vs-`Sample` fixture (iipsc rows reversed,
  message still reports N = 872 and not N = 106). All green.
- AC2 — Live: `attr(z, "norm_sample")` is a 4-element list (Instrument "IIP-SC",
  Sample 1, Size 872, Population "American college students"); present on the
  `append = FALSE` path too, and identical whether or not `quiet` suppressed the
  message. Tested over the enumeration at both `append` values.
- AC3 — Live: a constructed `Abbrev`-labelled violator (iitc, one octant pushed
  past its anchor maximum) refuses with "Its mean score for DE falls outside the
  instrument's 0 to 5 response range". The exact-set pin at
  `test-norms-anchor-range.R` is untouched and green.
- AC4 — Live: `sample = 7` on iipsc refuses with "No normative data for sample 7.
  The IIP-SC carries samples 1, 2; see norms() for what each one is." — names the
  argument and lists the numbers. The arity `stopifnot()` is retained and has its
  own test.
- AC5 — `Rscript -e 'options(cli.width = 500); devtools::document()'` produces no
  diff and zero `resolve link` lines. The shipped `man/norm_standardize.Rd`
  carries the multi-sample default-`sample` example ending in
  `attr(z, "norm_sample")`. The "or custom" opening is gone (RR16 B1).
- AC6 — 4 new user-facing NEWS entries, naming no test file. The entry-to-test
  mapping is in this milestone's work log (2026-08-08), not in NEWS.
- AC7 — `devtools::test()`: 0 failures, 6537 passing; the 4 WARNs are all in
  `test-ci_accuracy.R`, untouched by this branch. `devtools::check(args =
  "--no-manual")`: 0 errors, 0 warnings, 0 notes, vignettes rebuilt (13m 33s).
- AC8 (BC2) — Measured over the shipped roster: 24 (instrument, sample) pairs, 23
  emitting a message (the CAIS adult sample is refused before any message, D-040),
  and 23 of 23 carrying that sample's `Population` value verbatim. The
  fixed-text clause is asserted via a sentinel `Population`
  ("a representative population of nobody"): stripping the value from the emitted
  message leaves text containing neither "population" nor "representative",
  case-insensitively.
- AC9 (BC1) — `norm_standardize` and `norms` both exported under exactly those
  names; 15 of 15 shipped instruments carry a `Population` column in `Norms[[2]]`.

**Projection vs outcome (RR16).** The one numeric quantity ingested is AC8's
domain: measured 24 (instrument, sample) pairs against RR16's stated 24, and
23 emitting messages — the one gap being the anchor-range refusal RR16 did not
model. RR16's other counts (BC3's 6/16/2 partition) were not ingested here and
are carried by the ROADMAP candidate row.

**Consistency gate.** `cairn_validate` exit 0, every check PASS; the one WARN is
`sizing (split tripwires)` on this file's 9 criteria, logged at the ingest gate
with its justification. No `DESIGN.md` principle changed, so `cairn_impact` was
not run. Toolchain slot: `document()` no-diff and no `resolve link` (above);
generated files regenerated rather than hand-edited; `pkgdown::check_pkgdown()`
reports no problems; NEWS entries present; no new top-level files, so no
`.Rbuildignore` change.

**Independent review (three lenses + scorer), 2026-08-08.** 22 candidate
findings; 5 scored >= 80 and are actioned, 17 below 80 are logged here.

Actioned (>= 80):

- F1 (90) — `R/tidying_functions.R`: the "N other samples are available" clause
  counts every `Norms[[2]]` row, including samples D-040 refuses. Live: cais
  sample 1 prints "1 other sample is available; see norms()." while cais sample
  2 errors. The disclosure points the user at a sample the package refuses.
- F2 (88) — `test-norms-disclosure.R`: the multi-sample assertion computes its
  expected count with the same expression the implementation uses
  (`nrow(Norms[[2]]) - 1`), so no test in the branch can fail on F1.
- F4 (85) — `R/tidying_functions.R`: `sample = NA_real_` passes
  `is_num(sample, n = 1)`, then `key$Sample == NA` yields NAs and `key[NA, ]`
  returns NA-filled rows, so `nrow(key) == 0` is false and the new refusal is
  bypassed — the call dies in the arity `stopifnot()` or in the angle loop.
- F10 (80) — `test-norms-disclosure.R`: "the attribute is present whether or
  not the message was emitted" asserts only identity between two attributes,
  which passes if both are NULL; the AC2 sweep uses `quiet = TRUE` on every
  call, so the non-quiet path's attribute presence is unfenced.
- F16 (88) — `R/tidying_functions.R:137`: "(see `@return`)" renders in the
  shipped help page as the literal text `@return`.

Logged, below threshold (17): F9 (78, the partition assertions are true by
construction and nothing pins the roster at 15) · F3 (68, `Norms[[1]]`/`[[2]]`
sample sets assumed in sync; bare "subscript out of bounds" if not, unreachable
on the shipped roster) · F5 (55, chained `append = TRUE` leaves the attribute
describing only the last call) · F12 (55, bare `expect_error()` on the arity
check) · F14 (45, the single-sample sweep omits the `disclosure_usable()` skip)
· F11 (40) · F13 (40) · F18 (35, NEWS "nearly twice that") · F21 (35) · F6 (30)
· F8 (30) · F17 (30) · F20 (30, the vignette chunk gains a message; file
untouched by the diff and outside Scope) · F22 (30, the `expect_silent`
narrowing, judged deliberate and compensated) · F7 (25) · F19 (20).

Lens coverage: the prior-review lens reported no prior-review evidence
contradicted (GitHub inline-comment probe returned empty; the M72-M75 archives
do not touch this code surface). The blame-history lens confirmed D-040's
refusal intact, PR #99's `iei` sample-key pin preserved, and no contradiction
of D-039/D-040/D-041.

**Post-fix verification, 2026-08-08 (round 2).** All five actioned findings
fixed and verified live: cais sample 1's message now omits the other-samples
clause entirely (its only sibling is anchor-range refused) while iipsc sample 1
still reads "1 other sample is available; see norms()."; `sample = NA_real_`
refuses with "No normative data for sample NA. The IIP-SC carries samples 1, 2"
on a multi-sample instrument and the ISC equivalent on a single-sample one;
`document()` emits zero `resolve link` lines and the rendered `.Rd` carries
"see the Value section below" with no `\verb{@return}`. `devtools::test()`:
0 failures, 6550 passing. AC1 and AC4 re-ticked on this evidence.

**Return floor met — status back to `in-progress`.** F1 scores 90 on a defect
in what the package does for its users, and F4 demonstrates AC4 failing inside
its own domain: a `sample` matching no `Norms[[1]]` row is not refused by its
own check when that sample is `NA`. Defect-return count for this milestone: 1.
