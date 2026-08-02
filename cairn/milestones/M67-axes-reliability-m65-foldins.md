# M67: M65 review fold-ins for the `axes_reliability()` FIML path

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M66
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m67-axes-m65-foldins`

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

- [x] **T1 — EM-stall predicate.** Capture the real warning text on both lavaan
  generations, then replace the `fixed = TRUE` literal with
  `grepl("moments[[:space:]]+using EM", msg)` at `R/axes_fiml.R:86` and correct
  the comment at `R/axes_fiml.R:80`.
- [x] **T2 — fit-measure guard.** `all(want %in% names(fm))` then
  `fm <- fm[want]`; pin the six names; correct the "silently, no warning"
  comment at `R/axes_reliability.R:1576`.
- [x] **T3 — thin-overlap warning.** Decide and implement the small-N behavior
  at `R/axes_reliability.R:1211-1221`, with the two-case regression test.
- [x] **T4 — `@return` text for `n_complete`.**
- [x] **T5 — OLS-shadow R̂ assertion**, on an M2-style MAR fixture.
- [ ] **T6 — gate.** Tests, check, PDF manual.

## Work log

- 2026-07-27: created by /milestone-plan, from the seven findings the ROADMAP candidate row carried out of M65's third and fourth review passes (all sub-threshold, scored 36–78, logged in M65's archive with their scores).
- 2026-07-27: plan gate chose to plan these as their own milestone over folding them into M66 and over leaving them as a candidate row, because M66 already sits at eight criteria and mixes a Fable-tier estimator change with Sonnet-tier cleanup; falsified by nothing measured — a scope judgment, reversible by dropping this file back to a candidate row.
- 2026-07-27: `Depends on: M66` is a sequencing choice, not a logical dependency — the two are independent in substance but both edit `R/axes_reliability.R` near the components table and the caveat strings.
- 2026-08-02: implement started; branch `m67-axes-m65-foldins` cut from `master` at 58afe1fa. Two plan items found already shipped by M65's own T9 (the third-pass gate): the `@return` `n_complete` claim (T4/AC4) and AC2's literal `$fit` name pinning (`tests/testthat/test-axes-fiml.R:249`). Both become verify-and-record rather than edits; no amendment, since each criterion is satisfiable as written.
- 2026-08-02: T1 done. Warning text captured live with `dput(conditionMessage(w))` on both generations by squeezing the h1 EM cap to 10 (lavaan 0.7-2 installed into a scratch library, the M65 method): 0.6.21 emits from `lav_mvnorm_missing_h1_estimate_moments()` naming `em.h1.iter.max=`, 0.7.2 from `lav_mvn_mi_h1_est_moments()` naming `em.h1.args=` — different function, different remedy option, and both wrapping between `moments` and `using EM` at the default width 80. The test asserts the diagnosis half alone by stripping the remedy sentence at `EM;`, which reddened on both strings before the fix and is what pins detection off the version-specific `em.h1` stem; a boundary-warning negative fences the predicate's deliberate narrowness. Fix is the planned `grepl("moments[[:space:]]+using EM", msg)`, and the false comment claiming the literal matched both generations is replaced, not reworded. Full suite 0 failures / 4384 passing / 4 pre-existing warnings.
- 2026-08-02: T2 done. The guard now keys on membership (`all(want %in% names(fm))`) and imposes the order itself with `fm <- fm[want]`, so the degenerate "(missing: )" message is unreachable: it fired for any mismatch that was not a dropped name, because `identical()` also fails on order and length while `setdiff()` reports neither. Test reddened on exactly that message before the fix. AC2's other two clauses were already met on arrival — the six literal `$fit` names are pinned at `tests/testthat/test-axes-fiml.R:249` (M65 T9) — and the false "silently, no warning" comment is corrected at both sites it appeared, against a measurement rather than a rewording: on a converged fit, requesting one real and one bogus measure returns one element silently on 0.6.21 and with a `simpleWarning` reading `unknown fit measure: 'srmr_bogus_name'` on 0.7.2. The drop is common to both generations; only the silence was version-specific.
- 2026-08-02: T3 done. The warning gains a second clause, `min_coverage < n_used`, which is exactly "missingness thinned a pair": `min_coverage == n_used` holds if and only if every used row is complete. Complete data at N = 25 therefore draws no thin-overlap warning, where it used to report "as few as 25 respondent(s)" on a frame with no missing cell; genuinely thin overlap at N = 200 with one item held to 20 responses still fires and still names 20. The two cases are separate assertions in one regression test, collecting warnings rather than using `expect_no_warning()` so an unrelated boundary warning can neither stand in for this one nor mask its absence. Implement gate chose suppression over rewording: small N alone is not this function's business, and the listwise path does not remark on it either.
- 2026-08-02: T4 done — verification only, no edit. M65's own T9 already corrected this text, and the shipped `man/axes_reliability.Rd:82-86` states both fields are present on every path with `min_coverage` NA outside FIML and both NA on `cormat`. Measured on all three paths at this commit: listwise 200/NA, fiml 144/189, cormat NA/NA. The Rd says what the code does.
- 2026-08-02: T5 done. The live M2 replicate now makes one `axes_reliability()` call and asserts `details$ols_shadow` equals the shadow recomputed from `axes_fiml_moments(mat)$R` at 1e-8, against the available-case shadow measured 6.176e-02 away on ξ1 (0.3363 vs 0.2745) — reproducing RR12's 6.18e-02 and leaving the fence six orders inside the separation. Discriminative by construction: under MCAR the two candidates agree to ~1e-4, so no complete-data or MCAR fixture could carry this claim. Mutation-verified rather than assumed — substituting `stats::cor(mat, use = "pairwise.complete.obs")` for `mom$R` in the FIML branch reddens it (0.2745 against the expected 0.3363), then restored.
- 2026-08-02: T2–T5 share one gate run: full suite 0 failures / 4393 passing / 4 pre-existing warnings.

## Decisions

## Review
