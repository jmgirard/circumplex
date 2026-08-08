# M76: Disclose the reference sample at the standardizing call site

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP4
- **Branch/PR:** —

## Goal

Make `norm_standardize()` say which normative sample it standardized against.

## Scope

**In:** a `quiet` argument on `norm_standardize()` defaulting to `FALSE`; a
message on every non-quiet call naming the sample used, its size and its
population; an attribute on the returned frame carrying the same facts; and two
fixes to refusal messages in the same function — the `Abbrev`/`Scale` column
assumption at `R/tidying_functions.R:197`, and the unmatched-`sample` case that
today dies in the arity `stopifnot()` at `:182` without naming `sample`.

**Out:** what the docs claim these samples *are* → M77 (planned, depends on this
milestone). Renaming the `Population` column or any `norm*` identifier → declined
at this plan gate, ROADMAP candidate row. A `getOption()` global mute beside
`quiet` → ROADMAP candidate row; the argument is the whole ask here.

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
      shipped documentation. `Rscript -e 'options(cli.width = 500);
      devtools::document()'` produces no diff and zero lines matching
      `resolve link`.
- [ ] AC6 `NEWS.md` carries user-facing entries for `quiet`, the message, the
      attribute and the two refusal-message fixes, naming no test file. Each
      entry's asserted behavior has a test that fails without it; the
      entry-to-test mapping is recorded in this milestone's work log, not in
      NEWS.
- [ ] AC7 `Rscript -e 'devtools::test()'` clean and `Rscript -e
      'devtools::check()'` clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T2, T3
- AC2 → T4
- AC3 → T5
- AC4 → T6
- AC5 → T7
- AC6 → T8
- AC7 → T1, T8

## Tasks

- [ ] T1 Re-fence the existing call sites that will stop being silent:
      `tests/testthat/test-norms-anchor-range.R`'s `expect_silent` case and the
      explicit-`sample` calls in `tests/testthat/test-tidying_functions.R:43,49,191`.
      Decide per site whether it passes `quiet = TRUE` or asserts the message.
- [ ] T2 Test-first for AC1: both message forms over the partitioned roster,
      `quiet = TRUE` silence, and the row-order-vs-`Sample` fixture.
- [ ] T3 Implement `quiet` and the message in `R/tidying_functions.R`, keying
      the `Size`/`Population` read on `Sample`. Use base `message()`, the
      package's established idiom (`R/ssm_analysis.R:350`); cli is not an Import.
- [ ] T4 Test-first, then implement, the returned attribute on both `append`
      branches.
- [ ] T5 Test-first, then fix, the `Abbrev`/`Scale` column assumption at
      `R/tidying_functions.R:197`.
- [ ] T6 Test-first, then implement, the unmatched-`sample` refusal, leaving the
      arity check in place with its own test.
- [ ] T7 Roxygen for `quiet`, `@return`, the message and both refusals; add the
      default-sample example; run `document()`.
- [ ] T8 NEWS entries; full `test()` and `check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) returned 10 findings across M76 and M77 — 2 unsatisfiable, 1 factual error, 1 unbounded promise, 6 false-pass. All 10 had a determinate answer and were fixed before the gate; none became a gate question. Detail in the plan-session chat.
- 2026-08-08: plan gate chose an always-on message with a `quiet` escape over messaging only when the caller omitted `sample`; the narrower form fires exactly where the hazard is but leaves a deliberate chooser with no record, and the user asked for the broader form with a mute. Falsified by any report that the message is noise in loops or knitted documents that `quiet` does not answer.
- 2026-08-08: plan gate chose to leave every `norm*` identifier and the `Population` column alone over renaming them; the identifiers are not themselves claims, and a rename breaks stored data structures and user code for a defect that lives in prose. Falsified by evidence that users read the column name itself as the representativeness claim.

## Decisions

## Review
