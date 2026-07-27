# M67: M65 review fold-ins for the `axes_reliability()` FIML path

- **Status:** planned
- **Priority:** normal
- **Depends on:** M66
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Close the seven sub-threshold findings M65's third and fourth review passes
logged against the FIML path, each of which leaves a guard, a comment, or a
documented claim saying something the code does not do.

## Scope

**In:** the EM-stall predicate's dead disjunct; the fit-measure guard's message
and its two false comments, with the six `$fit` names pinned literally; the
thin-overlap warning's small-N misfire; the `@return` text for `n_complete`;
and a discriminative suite assertion that the FIML OLS shadow consumes R̂.

**Out:**
- The corrected component SEs → **M66** (this milestone depends on it; both
  touch `R/axes_reliability.R` around the components table and the caveats).
- Raising `axes_fiml_min_overlap` above 30, or turning the thin-overlap warning
  into a refusal — M65-D2 settled that it warns and never refuses, and RR12 §7
  binds no floor. Only the *wording* and *firing condition* are in scope.
- Any change to the FIML estimator, its metric, or its reported values.

## Acceptance criteria

- [ ] **AC1 (EM-stall predicate fires on both lavaan generations).** A test
  feeds `axes_fiml_em_stalled()` the real wrapped warning text from lavaan
  0.6.21 and 0.7.2 (captured with `dput(conditionMessage(w))`, not retyped) and
  asserts the *first* disjunct matches on both. The `fixed = TRUE` literal
  `"moments using EM"` never fires — lavaan wraps at `getOption("width")`, so
  the phrase straddles a newline — leaving detection resting solely on
  `grepl("em\\.h1", msg)`, whose stem lavaan has already renamed once. Its
  comment claiming the literal matches is corrected, not merely reworded.
- [ ] **AC2 (fit-measure guard is honest on every mismatch).** The guard at
  `R/axes_reliability.R:1574-1590` names the actual problem for a non-drop
  mismatch (today `identical(names(fm), want)` with a `setdiff()` message
  degenerates to "(missing: )" when only the *order* differs); the six `$fit`
  names are pinned literally or the returned length asserted, so a silent
  lavaan drop cannot leave a hole the current assertions pass over; and the
  "silently, no warning" comment is corrected — lavaan 0.7.2 emits
  `unknown fit measure`.
- [ ] **AC3 (thin-overlap warning distinguishes thin from small).** On complete
  data with N < 30 the warning either does not fire or says what is actually
  true — today it reports "Some item pair(s) were jointly observed by as few as
  N respondent(s)" on data with no missing cells at all, because
  `min_coverage` equals N. A regression test covers complete data at N < 30 and
  genuinely thin overlap at N ≥ 30 separately.
- [ ] **AC4 (`n_complete` documented as it behaves).** The `@return` text no
  longer implies `n_complete` is FIML-only; it is set on every path. Verified
  against the built Rd.
- [ ] **AC5 (the FIML OLS shadow consumes R̂).** A suite assertion discriminates
  the FIML correlation matrix from an available-case correlation — discriminative
  only under an M2-style MAR mechanism, where the two differ; a complete-data or
  MCAR fixture cannot tell them apart and must not be used as the fence.
- [ ] **AC6 (gate clean).** `devtools::test()` and
  `devtools::check(args = "--no-manual")` clean, plus a built PDF manual since
  AC4 changes roxygen.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [ ] **T1 — EM-stall predicate.** Capture the real warning text on both lavaan
  generations, then replace the `fixed = TRUE` literal with
  `grepl("moments[[:space:]]+using EM", msg)` at `R/axes_fiml.R:86` and correct
  the comment at `R/axes_fiml.R:80`.
- [ ] **T2 — fit-measure guard.** `all(want %in% names(fm))` then
  `fm <- fm[want]`; pin the six names; correct the "silently, no warning"
  comment at `R/axes_reliability.R:1576`.
- [ ] **T3 — thin-overlap warning.** Decide and implement the small-N behavior
  at `R/axes_reliability.R:1211-1221`, with the two-case regression test.
- [ ] **T4 — `@return` text for `n_complete`.**
- [ ] **T5 — OLS-shadow R̂ assertion**, on an M2-style MAR fixture.
- [ ] **T6 — gate.** Tests, check, PDF manual.

## Work log

- 2026-07-27: created by /milestone-plan, from the seven findings the ROADMAP candidate row carried out of M65's third and fourth review passes (all sub-threshold, scored 36–78, logged in M65's archive with their scores).
- 2026-07-27: plan gate chose to plan these as their own milestone over folding them into M66 and over leaving them as a candidate row, because M66 already sits at eight criteria and mixes a Fable-tier estimator change with Sonnet-tier cleanup; falsified by nothing measured — a scope judgment, reversible by dropping this file back to a candidate row.
- 2026-07-27: `Depends on: M66` is a sequencing choice, not a logical dependency — the two are independent in substance but both edit `R/axes_reliability.R` near the components table and the caveat strings.

## Decisions

## Review
