# M67: M65 review fold-ins for the `axes_reliability()` FIML path

- **Status:** review
- **Priority:** normal
- **Depends on:** M66
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m67-axes-m65-foldins` / https://github.com/jmgirard/circumplex/pull/93

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

- [ ] **AC1 (EM-stall predicate fires on both lavaan generations, at every wrap
  position).** A test feeds `axes_fiml_em_stalled()` the real wrapped warning
  text from lavaan 0.6.21 and 0.7.2 (captured with `dput(conditionMessage(w))`,
  not retyped) and asserts the *first* disjunct matches on both. Because lavaan
  re-wraps at `getOption("width")` on emission, the test does not rest on the
  captured width-80 break: it sweeps the break across every inter-word gap of
  each diagnosis sentence — the full domain `lav_msg()` can break in — and
  asserts the disjunct matches at all of them. The `fixed = TRUE` literal
  `"moments using EM"` fails at exactly the two gaps separating its three
  words, which includes the break lavaan takes at the default width 80 on both
  generations, leaving detection there resting solely on
  `grepl("em\\.h1", msg)`, whose stem lavaan has already renamed once. Its
  comment is corrected to that measured behavior, not merely reworded.
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

- AC1 → T1, T7
- AC2 → T2
- AC3 → T3, T8
- AC4 → T4
- AC5 → T5, T9
- AC6 → T6, T10

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
- [x] **T6 — gate.** Tests, check, PDF manual.
- [x] **T7 — F1/F2/F3 (review return): wrap-position-proof EM-stall
  predicate.** Both inter-word gaps whitespace-tolerant; the comment corrected
  to the measured behavior; the AC1 test sweeps every gap instead of pinning
  one width.
- [x] **T8 — F4/F5 (review return): thin-overlap warning under unit
  nonresponse.** Compare against the supplied respondent total, not the
  post-drop `n_used`; third regression case on the row-drop path.
- [x] **T9 — F13 (review return): AC5 recomputation mirrors the package's row
  filter.**
- [x] **T10 — re-gate.** Tests, check, PDF manual.

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
- 2026-08-02: T6 done, status in-progress→review. `devtools::document()` no diff (no roxygen changed this milestone — T4 was verification only). Full suite 0 failures / 4393 passing / 4 pre-existing warnings. `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes in 13m35s. PDF manual builds, 78 pages, with only the pre-existing `Rfn.summary` pdftex cross-reference warning. No NEWS entry is owed: `axes_reliability()` and its FIML path are both unreleased (2.0.0 is the dev version), the existing NEWS text never promised the small-N warning this milestone stops, and every change here is to a guard message, a comment, or a warning's firing condition inside a feature NEWS already describes generically.
- 2026-08-02: **returned by /milestone-review (first return).** Status review→in-progress. Every gate clean — `cairn_validate` 16/16 PASS, `check(args = "--no-manual")` 0/0/0, `document()` no diff, pkgdown clean, PDF manual 78 pages — and the blame-history and prior-review lenses both returned no findings. Three diff-bug findings clear the return floor: F1 (95) the EM-stall regex made only ONE of two inter-word gaps whitespace-tolerant, so it still fails at nine widths in 20–250 while the fully-tolerant pattern fails at none, leaving detection back on the `em.h1` stem AC1 exists to remove; F4 (92) the thin-overlap warning's new `min_coverage < n_used` clause suppresses the warning under heavy unit nonresponse, because `n_used` is counted after all-missing rows are dropped — reproduced at 120 respondents with 95 all-NA, where master warns and this branch is silent; F2 (90) the replacement comment claims the old literal "never fires", re-measured as firing at 209 of 231 widths. F3 (80) actioned with them: the AC1 test pins two width-80 strings and so could not catch either. No criterion ticked — the fixes change the code their evidence would come from.

- 2026-08-02: substantive amendment to AC1, gated and accepted. Two clauses were false or too weak to hold their own fix: "The `fixed = TRUE` literal ... never fires" (F2's finding, inherited into the criterion), and a test bar of "asserts the first disjunct matches on both", which F3 showed is met by a test pinning one width. Amended to state the measured behavior — the literal fails at exactly the two gaps separating its three words, one of which is the width-80 break on both generations — and to require a sweep over every inter-word gap. Shown verbatim in chat before commit.
- 2026-08-02: T7 done (F1, F2, F3). Break position, not width, is the thing the predicate is sensitive to: `lav_msg()` splits the message on whitespace and prefixes the chunk after a break with a newline and three spaces, so the break can land in any inter-word gap and only two of them can break this match. Measured exhaustively over all 12 gaps of each generation's diagnosis sentence: the old literal fails at 2 (gaps 11 and 12), the shipped one-gap pattern at 1 (gap 12, `using`/`EM`) — F1 — and `moments[[:space:]]+using[[:space:]]+EM` at none. That makes the test a 12-position sweep rather than a width sweep: finite, exhaustive over the domain `lav_msg` can break in, and independent of the installed lavaan's wrapping arithmetic. Mutation-verified — reverting to the one-gap pattern reddens the sweep with exactly two failures, one per generation. The comment's "never fires ... flips to TRUE only at width = 300" is replaced by that measurement, not reworded.
- 2026-08-02: T8 done (F4, F5). The second clause now compares `min_coverage` against `n_used + n_dropped`, the respondent count the caller supplied, because `n_used` is counted after `axes_fiml_coverage()` drops all-missing rows: under heavy unit nonresponse every surviving row is complete, so `min_coverage == n_used` and the warning was suppressed where it was true. Reproduced at 120 respondents with 95 all-NA (`n_used` 25, `n_dropped` 95, `min_coverage` 25). The corrected form states the real invariant — equality holds if and only if the input frame had no missing cell at all — and discriminates all four cases: complete N=25 silent, complete N=200 silent, thin overlap at N=200 fires naming 20, unit nonresponse fires naming 25. Two candidates measured wrong and rejected: `nrow(mat)` (mat is row-filtered at R/axes_reliability.R:1162, so it equals `n_used` there) and `anyNA(mat)` (same reason). A third regression case on the row-drop path retires F5, which flagged that path as unexercised; mutation-verified, reverting to `n_used` reddens exactly that case.
- 2026-08-02: T9 done (F13, below threshold, actioned at the user's gate choice). The AC5 shadow is now recomputed from `mat_m2[axes_fiml_coverage(mat_m2)$keep, ]`, mirroring the package's own call. This is robustness, not a bug fix, and does not redden under mutation: `axes_mar_m2()` leaves no all-missing row, so both forms agree on this fixture — which is exactly the accident the fix removes, and the same row-filter accounting that made F4 real.

- 2026-08-02: checkpoint. T7-T9 verified on the one test file they touch (`test-axes-fiml.R`, 0 failures) plus the two mutation reversions; the full `devtools::test()` and the T10 re-gate (check, PDF manual) have NOT yet reported at this commit.
- 2026-08-02: T10 done, status in-progress→review (second time). Full suite 0 failures / 4421 passing / 4 pre-existing warnings — 28 more assertions than the first pass's 4393, which is the 24-position gap sweep plus the third thin-overlap case. `devtools::document()` no diff (this pass changed no roxygen at all; the AC1 amendment is tracking-only). `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes in 13m22s. PDF manual 78 pages, unchanged from the first pass, with no warning outside the pre-existing `Rfn.*` external-link class. NEWS still owes no entry, on the first pass's reasoning: the feature is unreleased, and F1/F4 correct guards that never shipped.

## Decisions

## Review

### First pass — 2026-08-02 (PR #93)

**Outcome: returned to `in-progress`.** Every mechanical gate is clean, but three
findings clear the return floor. No acceptance criterion is ticked: the fixes
change the code each criterion's evidence would come from.

**Gates (all clean, all fresh by command).** `cairn_validate` exit 0, 16 of 16
CHECKs PASS (the 47 `work-log format` advisories are all pre-existing M7 lines,
none from this milestone). No DESIGN principle changed, so `cairn_impact` is
skipped. Toolchain `consistency-gate`: `document()` no diff; `man/`, `NAMESPACE`
and `src/` untouched by the diff; README.Rmd/README.md untouched and unaffected;
`pkgdown::check_pkgdown()` clean; `devtools::check(args = "--no-manual")`
0 errors / 0 warnings / 0 notes in 18m06s; PDF manual builds at 78 pages. NEWS
carries no entry, and none is owed — the whole feature is unreleased and NEWS
never promised the behavior this milestone changes.

### Independent review — three lenses, then a scorer

The **[S] blame-history** lens returned no regressions: each change traces to a
specific M65 finding logged as deferred to the milestone that next touches this
code, M65-D2's warn-never-refuse holding survives, BC7 clause (iii)'s hard
zero-coverage refusal is untouched, and the M65-D3 test lost no assertion when
`est_of()` gave way to a direct call. The **[S] prior-review** lens returned no
findings; the GitHub inline-comment probe came back empty, so archived
`## Review` sections were the evidence base. The **[O] diff-bug** lens returned
15 findings, scored by a fresh **[S]** scorer that did not generate them.

**Actioned (>= 80): four.**

- **F1 (95) — the EM-stall regex is still width-fragile, so AC1's whole point is
  unmet.** Only one of the two inter-word gaps was made whitespace-tolerant:
  `"moments[[:space:]]+using EM"` leaves `using EM` a literal space, and lavaan
  re-wraps at `getOption("width")` at emission time, so the break lands in
  either gap depending on width. Re-verified at review across widths 20-250: the
  shipped pattern fails at nine of them while
  `"moments[[:space:]]+using[[:space:]]+EM"` fails at none. An unmatched warning
  is muffled and `converged` stays `TRUE`, so detection falls back to the
  `em.h1` stem — the single point of failure AC1 exists to remove.
- **F4 (92) — the new second clause suppresses the warning on heavy unit
  nonresponse.** `n_used` is counted AFTER `axes_fiml_coverage()` drops
  all-missing rows, so a frame whose respondents either answered everything or
  answered nothing has every used row complete and `min_coverage == n_used`.
  Reproduced at review: 120 respondents, 95 of them all-NA, 16 items -- `n_used`
  25, `min_coverage` 25, and NO warning fires where master warns. The suppressed
  sentence is true there.
- **F2 (90) — the replacement comment states a measured fact that is false**, in
  a milestone whose Goal is closing comments that say what the code does not do.
  It claims the old literal "never fires ... flips to TRUE only at width = 300";
  re-measured at review, the literal fires at 209 of 231 widths and fails at the
  default 80, which is all the evidence supports. Repeated in the test comment
  ("matched NEITHER generation") and inherited from AC1's own wording.
- **F3 (80) — the AC1 test cannot catch F1 or F2.** It pins two frozen width-80
  strings and calls the predicate directly, so it characterizes one width rather
  than the wrap-sensitivity its own comment names as the subject.

**Below threshold — logged, not actioned (IP3):** F5 (75) the AC3 test never
exercises the row-drop path F4 lives in; F11 (68) the mocked order-only test
asserts names but not that values travelled with them; F13 (62) the AC5
recomputation calls `axes_fiml_moments()` on the unfiltered matrix while the
package calls it on `mat[cvg$keep, ]`, safe only because `axes_mar_m2()` leaves
no all-NA row; F10 (55) the `"missing: )"` assertion is tautological given the
`expect_match` above it; F12 (45) thin-overlap case (a) has no error containment
at N = 25 with p = 16; F14 (45) the AC5 comment mixes a relative tolerance with
an absolute bar; F6 (35) `axes_fiml_min_overlap`'s doc comment is stale but sits
on an unmodified line; F7 (30) AC2 is satisfied by dissolving the non-drop
mismatch rather than naming it, which is the design T2 specified; F8 (30) and
F16 (30) the guard now accepts an over-long or duplicate-named `fm` where
`identical()` refused; F9 (20) the guard checks names but not finiteness,
pre-existing; F15 (8) the reviewer's clean-bill note.

**Return floor.** F1, F4 and F2 each score >= 90 on defects in what the package
does for its users, and F2 additionally falsifies AC1's own "its comment ... is
corrected, not merely reworded" clause. First defect return for this milestone.

