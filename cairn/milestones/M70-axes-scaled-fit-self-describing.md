# M70: Self-describing scaled fit output for `axes_reliability()`

- **Status:** review
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m70-axes-scaled-fit-self-describing` / https://github.com/jmgirard/circumplex/pull/96

## Goal

Let a reader of an `axes_reliability()` object locate its fit statistics on the
documented calibration curve, and against lavaan's own fit measures, without
recomputing anything.

## Scope

**In:** a new `details` field for the count of distinct analyzed moments
`p* = p(p+1)/2`, and `@return` text naming the existing `details$n` as the N the
fit was priced at (the amendment gate found that field already shipped), so the
`p*/N` the vignette's calibration table is indexed by is readable off the
object; whatever further `details` fields the AC4 recomputation needs; the
`@return` documentation and the vignette passage that point at them; and naming
the lavaan variant the scaled `cfi`/`rmsea` correspond to on every user-facing
surface that reports them. Also in, absorbed from its ROADMAP candidate row at
this milestone's amendment gate: the NA-unsafe nonpositive-diagonal guard at
`R/axes_scaled_fit.R:103`, which errors where its own header promises a
named-reason NA — M69 fixed the identical defect in the sibling
`axes_corrected_se()`.

**Out:** any change to the scaled statistics themselves — this milestone reports
and names, and moves no number. The component-SE repricing and its calibration
re-run → M69, which this depends on because both edit the same roxygen block.
The scaled *difference* test and a Swain/Bartlett small-sample correction stay
ROADMAP candidate rows. Reason-code parity between the two surfaces on a zero or
negative diagonal — `axes_scaling_factor()` says `"singular"` where
`axes_corrected_se()` says `"nonpositive_diagonal"` — stands as it is, decided at
the amendment gate: changing it is a user-visible output change this milestone's
first Out clause rules out.

## Acceptance criteria

- [x] **AC1** — `details` gains `n_moments`, the number of distinct analyzed
      moments `p * (p + 1) / 2`, so `p*/N` is readable without arithmetic over
      `n_items`; the N of that ratio is the existing `details$n`, which AC2's
      `@return` text names explicitly as the sample size the fit was priced at,
      distinguishing it from the `n_total` and `n_complete` beside it. Evidence:
      a test asserts `n_moments == p * (p + 1) / 2` for a fit of known p = 24,
      and that `details$n` equals `lavaan::fitMeasures(fit, "ntotal")` on the
      cormat, listwise and FIML paths. The listwise and FIML probes run on data
      where `n`, `n_total` and `n_complete` all differ, so neither assertion can
      pass by coincidence. The cormat path admits no such probe — `n_total` is
      set equal to `n` and `n_complete` is `NA` by construction there — so its
      assertion establishes only that the supplied `n` reaches lavaan
      unchanged, which is all that path has to prove.
- [x] **AC2** — The `@return` text for `details` names both new fields and says
      what they are for, and the vignette's calibration passage tells the reader
      where to read `p*/N` off the object. Evidence: a doc guard reads
      `man/axes_reliability.Rd` and the vignette source and asserts both field
      names plus a verb-carrying phrase of their stated meaning; a second
      assertion pins that the vignette's calibration table and the sentence
      pointing at the object are both present, so removing one without the
      other reddens.
- [x] **AC3** — Every user-facing surface reporting the scaled `cfi`/`rmsea` —
      the roxygen in `R/axes_reliability.R`, the printed output built in
      `R/axes_reliability_oop.R`, `vignettes/axes-reliability.Rmd`, and
      `NEWS.md` — names the lavaan variant the values correspond to
      (`cfi.scaled` / `rmsea.scaled`) and warns that a `fitMeasures()`
      cross-check against either lavaan's unscaled `cfi` or its `cfi.robust`
      will differ. Procedure: those four surfaces are read in full, not grepped,
      and `man/` is regenerated from the roxygen and checked to carry the
      wording; the milestone commits the per-surface list with each classified
      as updated, or as not reporting these values with its reason.
- [x] **AC4** — AC3's correspondence claim is true of the shipped computation: a
      test recomputes `$fit$cfi` from the scaled model chi-square, `df`, the
      scaled baseline chi-square and the baseline df using the `cfi.scaled`
      definition written out arithmetically in the test — no lavaan internal is
      called by name; any corroboration against an unexported lavaan function
      goes through `get()` and skips on failure, per the pattern at
      `test-axes-scaled-fit.R:481-496` — and agrees to 1e-10. On the same probe
      the `cfi.robust` definition applied to those inputs differs by at least
      1e-4, asserted on a fit chosen so both excesses are strictly positive,
      since at perfect fit both definitions truncate to 1 and the check would be
      vacuous. Where `details` does not expose the baseline chi-square and
      baseline df the recomputation needs, this milestone adds them rather than
      inverting lavaan's uncorrected `cfi` to recover them.
- [x] **AC5** — `axes_scaling_factor()`'s nonpositive-diagonal guard
      (`R/axes_scaled_fit.R:103`) returns its documented named-reason NA
      instead of erroring, on the three diagonal inputs the regression test
      below enumerates. Evidence: a regression test, confirmed red against the
      pre-fix predicate, calls `axes_scaling_factor()` directly — needing no
      lavaan fit, so it carries no `skip_if_not_installed()`, unlike the rest
      of its file — once with an `NA_real_` diagonal entry, once with a `NaN`
      one, and once with a `0` one, and asserts each returns `scale` and
      `baseline` both `NA_real_` with `reason == "singular"` and warns "could
      not be computed". The first two cases also raise R's own `cov2cor()`
      non-finite-diagonal warning, which the test expects rather than
      suppresses; the `0` case is the unchanged control showing the fix
      rejects nothing new.
- [x] **AC6** — The profile's `verify` slot is clean: `devtools::document()`
      after roxygen changes and `devtools::test()` passing, plus
      `devtools::check()` before review.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T1, T2, T3, T4, T5

## Tasks

- [x] **T1** — Add `n_moments` and, as one grouped field
      `baseline = c(chisq = ..., df = ...)` matching the existing
      `scaling_factor` idiom, the baseline chi-square and df AC4 needs, to the
      `details` list (`R/axes_reliability.R:1799`); test each on the cormat,
      listwise and FIML paths, on data where `n`, `n_total` and `n_complete`
      differ.
- [x] **T2** — Write the `@return` text for the new fields, add the vignette
      sentence pointing at them beside the calibration table
      (`vignettes/axes-reliability.Rmd:189-202`), and add the paired doc guard.
- [x] **T3** — Read the four user-facing surfaces in full, add the lavaan
      variant naming and the cross-check warning to each that reports the
      scaled values, and commit the per-surface classification ledger.
- [x] **T4** — Add the `cfi.scaled`-versus-`cfi.robust` recomputation test on a
      misfitting probe, with the arithmetic written out and any lavaan internal
      reached only via `get()`.
- [x] **T5** — Change the guard at `R/axes_scaled_fit.R:103` to
      `any(diag(sigma) <= 0, na.rm = TRUE)` and write a load-bearing comment
      for *this* site rather than carrying the sibling's: here the NA/NaN
      matrix does reach `cov2cor()`, and is caught downstream by the
      `tryCatch`/`is.finite(si)` pair, which is the opposite of what the
      sibling's comment says of itself. Write no numbered
      `R/axes_corrected_se.R:<n>-<m>` citation into this file — the
      citation-rot guard at `test-axes-scaled-fit.R:1108` asserts exactly one
      such match. Add AC5's three-case regression test to
      `tests/testthat/test-axes-scaled-fit.R`, callable without lavaan.

## Work log

- 2026-08-03: created by /milestone-plan.
- 2026-08-03: criteria audit ([O], fresh context, authored none of the drafts) returned findings on all four drafted criteria, all fixed in the wording before this file was written: AC1's "reported N" half was already true of shipped code and its formula was mis-typed as `p*(p+1)/2` where `p*` had just been defined as p(p+1)/2; AC2's "absence assertion" was undefined and would have been a no-op; AC3's grep domain omitted `NEWS.md:33-52` and included generated `man/`, which CLAUDE.md forbids hand-editing; and AC4 as drafted was already satisfied by the shipped suite while naming inputs `details` does not expose.
- 2026-08-03: plan gate chose taking up the lavaan-variant naming now over honouring its stated promotion condition — a user reporting the confusion, or the release walk asking for it, neither of which has happened — because the surfaces are open for this milestone anyway and the fix is one sentence each; falsified by evidence that readers do not in fact cross-check these values against `fitMeasures()`.
- 2026-08-04: amended at a /milestone-plan gate (milestone still `planned`, not yet started) to absorb the NA-unsafe `axes_scaling_factor()` guard from its ROADMAP candidate row — new AC5 + T5, old AC5 renumbered AC6, Scope In/Out and Coverage updated. Trigger reading: the row said "whichever milestone next opens `R/axes_scaled_fit.R`" and M70 opens only the caller and the matching test file, so the user waived the strict trigger on adjacency grounds.
- 2026-08-04: criteria audit ([O], fresh context, authored none of the drafts) returned eight findings on the drafted AC5/T5; seven fixed in the wording before this file was written — T5 would have reddened the citation-rot guard at `test-axes-scaled-fit.R:1108` by transcribing a numbered sibling citation; the sibling comment T5 asked to carry is false here (NA/NaN does reach `cov2cor()`); the NA/NaN cases raise a second warning from `cov2cor()` the evidence had not expected; the "carries NA or NaN" universal was unbounded against a two-case test; the test would have been silently lavaan-skipped like the rest of its file; the cited sibling range was off by a paragraph; and no principle conflict exists because `cairn/DESIGN.md` carries no IP/GP block at all.
- 2026-08-04: gate chose leaving the two surfaces' reason codes divergent (`"singular"` vs `"nonpositive_diagonal"` on a zero/negative diagonal) over unifying them, because unification changes documented user-visible output that this milestone's first Out clause excludes; falsified by a user or reviewer reading the two codes as reporting different conditions.

- 2026-08-04: status → in-progress; branch `m70-axes-scaled-fit-self-describing` cut from pushed master.
- 2026-08-04: substantive amendment at the implement question gate — AC1's "gains an explicitly named field for the N" was measurably already shipped: `details$n` is the row count of exactly the matrix handed to lavaan on all three paths (probed: cormat 640/640, FIML 488/500 with 12 all-NA rows dropped, listwise 398/500), so it already equals `fitMeasures(fit, "ntotal")`. Gate chose documenting `n` over adding an `n_fit` alias, because two fields holding one number can later disagree; renaming `n` was rejected as needing a deprecation cycle this milestone did not scope. AC1 amended accordingly, and its evidence tightened to require probe data where `n`, `n_total` and `n_complete` differ — on the package's own example data all three are 500, which would have made the assertion vacuous.
- 2026-08-04: gate chose one grouped `baseline = c(chisq, df)` field over flat `baseline_chisq`/`baseline_df`, matching the existing `scaling_factor = c(model, baseline)` idiom; T1 updated to name the shape (and its stale `:1792` anchor corrected to `:1799`).

- 2026-08-04 (T1): `details` gains `n_moments` and `baseline = c(chisq, df)`; tests capture THE fitted lavaan object through the `axes_converged()` seam rather than refitting, and probe on data where `n`/`n_total`/`n_complete` are pairwise distinct. Full suite FAIL 0, PASS 5749.

- 2026-08-04 (T2): `@return` documents `n_moments`, `baseline`, and which of the three sample sizes `n` is; vignette gains the sentence pointing at both halves of `p*/N`. Doc guard verified by mutation — deleting the Rd clause and deleting the vignette pointer each reddened their own assertion (2 failures, restored).

- 2026-08-04 (T3) per-surface ledger, all four read in full rather than grepped: **roxygen** `R/axes_reliability.R` @details scaling block — UPDATED, new paragraph naming the `*.scaled` correspondence and both failing cross-checks; **printed output** `R/axes_reliability_oop.R` — UPDATED at `axes_fit_scaled_note`, which `axes_cat_fit_note()` is the sole caller of and `print()` emits no fit statistic at all, so the one edit covers the whole printed surface; **vignette** `vignettes/axes-reliability.Rmd` — UPDATED after the scaling passage; **NEWS.md** — UPDATED, scaled-fit bullet gains the variant naming plus a new bullet for `n_moments`/`baseline`/`n`. Checked and classified NOT REPORTING these values: `NEWS.md:115-126` (`cpm_fit()`, different function, unscaled indices) and `NEWS.md:300-319` (`ssm_sem()` `dcfi`, different function and estimator). `man/axes_reliability.Rd` regenerated and verified to carry the wording (`chisq.scaled`/`cfi.robust` present at Rd:190).

- 2026-08-04 (T4): recomputation test rebuilds `$fit$cfi` from `details` alone on a perturbed octant probe (both excesses strictly positive, c = 0.9562 vs c_b = 0.8653). Reported value matches the `cfi.scaled` definition to 1e-10 and the `cfi.robust` definition applied to the same inputs differs by 3.5e-3 — four orders above the tolerance, so the test discriminates the two rather than merely tolerating both. lavaan's unexported `lav_fit_cfi()` corroborates via `get()` and was callable here. Full suite FAIL 0, PASS 5763.

- 2026-08-04 (T5): guard made NA-safe. Test written first and confirmed red with the exact reported error, "missing value where TRUE/FALSE needed". Comment authored for this site rather than carried from the sibling (here the non-finite entry falls THROUGH to `cov2cor()` and is caught by the `solve()`/`is.finite` pair, the opposite of the sibling's route); no numbered sibling citation written, so the citation-rot guard at `test-axes-scaled-fit.R:1108` still counts exactly 1. Both warnings on the NA/NaN route are asserted rather than one left escaping as a test warning. Full suite FAIL 0, PASS 5777.

- 2026-08-04: all five tasks done; `devtools::check(args = "--no-manual")` clean at 0 errors / 0 warnings / 0 notes, full suite FAIL 0 PASS 5777. Status → review.

- 2026-08-04 (review): three-lens fan-out returned 19 findings, 6 actioned at >= 80 and all fixed on the branch; none met the return floor. The AC2 doc guard reddened on my own fix to the sentence it pins, which is the guard working. `devtools::check()` re-run after the fixes: 0/0/0.

- 2026-08-04: amendment return: AC1 — "The listwise and FIML probes run on data where `n`, `n_total` and `n_complete` all differ, so neither assertion can pass by coincidence. The cormat path admits no such probe — `n_total` is set equal to `n` and `n_complete` is `NA` by construction there — so its assertion establishes only that the supplied `n` reaches lavaan unchanged, which is all that path has to prove." Raised as review finding F16 (scored 72); the prior wording required "each" of three paths to be probed on data where the three N's differ, which is unsatisfiable on cormat. First amendment return on this milestone.

## Decisions

## Review

Reviewed 2026-08-04 on PR #96. Evidence gathered fresh by command; no criterion
verified from recall.

- **AC1** — `test-axes-reliability.R` "AC1: details reports p* and the N the fit
  was priced at" passes 16 assertions, 0 skipped: `n_moments == 300` for the
  p = 24 fixture on all three input paths, and `details$n` equals the captured
  fit's `fitMeasures(fit, "ntotal")` on each. The probe's three N's are pairwise
  distinct on the two raw-data probes (listwise n 398, FIML n 488, `n_total`
  500), so those two equalities are discriminating; the cormat probe asserts
  only that the supplied `n = 640` reaches lavaan unchanged, per AC1 as
  amended at the review gate. The paired test "AC1: details exposes the baseline chisq and
  df as one pair" passes 3 assertions: names `c("chisq", "df")`, values equal to
  lavaan's `baseline.chisq`/`baseline.df`, and `df == p(p-1)/2 == 276`.
- **AC2** — "AC2: the Rd names both new fields and what they are for" passes 5
  assertions against the regenerated `man/axes_reliability.Rd`, each field name
  pinned together with a verb-carrying phrase of its meaning. "AC2: the
  vignette's calibration table and its object pointer travel together" passes 3.
  Both guards verified by mutation at implement time: deleting the Rd clause and
  deleting the vignette pointer each reddened their own assertion.
- **AC3** — per-surface ledger committed in the work log (T3 entry), all four
  surfaces read in full rather than grepped. Four classified UPDATED (roxygen,
  the `summary()` note, the vignette, `NEWS.md`); two further `NEWS.md` passages
  classified as reporting other functions' fit indices (`cpm_fit()` at 115-126,
  `ssm_sem()`'s `dcfi` at 300-319). `man/` regenerated from the roxygen and
  verified to carry the wording.
- **AC4** — "AC4: the reported cfi IS the cfi.scaled definition, not
  cfi.robust" passes 6 assertions, 0 skipped, so lavaan's unexported
  `lav_fit_cfi()` corroboration ran rather than skipping. On the perturbed
  octant probe both excesses are strictly positive and the two scaling factors
  differ (c = 0.9562, c_b = 0.8653); the reported `cfi` matches the
  `cfi.scaled` definition to 1e-10 and `cfi.robust` on the same inputs differs
  by 3.5e-3, against a required 1e-4.
- **AC5** — "AC5: a non-finite fitted diagonal refuses cleanly instead of
  erroring" passes 14 assertions, 0 skipped, and needs no lavaan fit. Confirmed
  red before the fix with the exact error the candidate row reported, "missing
  value where TRUE/FALSE needed". `NA_real_` and `NaN` diagonals each return
  `reason == "singular"` with `scale`/`baseline` both `NA_real_`; the `0`
  control is unchanged.
- **AC6** — `devtools::document()` produces no diff (`man/`, `NAMESPACE` clean).
  `pkgdown::check_pkgdown()`: no problems found. `devtools::check(args =
  "--no-manual")` re-run AFTER the review fixes: **0 errors, 0 warnings, 0
  notes** in 16m26s, its embedded suite included.

Consistency gate: `cairn_validate` exit 0, all checks passed (47 advisories, 46
of them the pre-existing `work-log format` warnings on M7's hard-wrapped log).
Profile `consistency-gate` slot: `document()` no-diff PASS, pkgdown PASS,
`NEWS.md` entries present for both user-visible changes, no new top-level files.

### Independent review (three lenses + scorer)

Fan-out on PR #96. **Blame-history [S]:** zero findings — traced the guard change
to the gap M69's own round-2 review logged (A1, scored 48) and graduated
deliberately. **Prior-review-record [S]:** zero regressions; the
`gh api .../pulls/comments` probe returned `[]`, so no PR-thread walk was owed
and the archived `## Review` sections were the whole evidence base.
**Diff-bug [O]:** 19 candidate findings, all passed to a fresh [S] scorer.

**Actioned (>= 80), all fixed on the branch — none met the return floor, since
none scored >= 90 and none demonstrated a criterion failing inside its named
procedure's domain:**

- **F1 (88)** — the new `@return` said `baseline` plus `fit$chisq`/`fit$df`
  reproduce `cfi`. False: the rebuild also needs `scaling_factor[["baseline"]]`.
  Rewritten to state five inputs, and the AC2 doc guard extended to pin that
  clause so the error cannot return silently.
- **F2 (85)** — the same error as a code comment, with a wrong count ("two of
  the three inputs"). Rewritten.
- **F4 (82)** — the `cfi.robust` cross-check advice was misleading: on a plain
  ML fit lavaan returns no `*.robust` or `*.scaled` measure at all, so the
  request comes back empty rather than disagreeing. Corrected on all three
  surfaces.
- **F5 (82)** — the guard comment claimed a general "non-finite" guarantee the
  code does not provide: `+Inf` fails `<= 0`, survives `cov2cor()` (which maps
  it to a zeroed row and a unit diagonal), and `solve()`/`is.finite` accept the
  result, so a factor is computed from a corrupted matrix. Pre-existing, not
  introduced here — the comment was narrowed to NA/NaN and the hole spawned a
  ROADMAP candidate row.
- **F6 (80)** — the comment's causal account of the sibling's differing reason
  string was wrong: the two agree ("singular") on the NA/NaN route and differ
  only on the finite-nonpositive one, where they chose different literals.
  Rewritten.
- **F7 (82)** — the AC2 vignette guard was satisfied to skip ALWAYS under
  `R CMD check`, which runs from `<pkg>.Rcheck/tests/testthat` where
  `../../vignettes` does not exist; it had therefore never run on CI or CRAN.
  Given an `inst/doc` fallback, mirroring the Rd guard beside it.

**Logged, below the 80 threshold, not actioned (13).** F3 (76) NEWS repeated
F1's error — fixed anyway while the surface was open, since shipping known-false
prose is worse than the threshold is worth. F16 (72) AC1's "each probed on data
where those three N's differ" cannot hold on the cormat path, where `n_total`
is set equal to `n` and `n_complete` is NA — surfaced at the merge gate. F12
(68) vignette's "count of rows" false for cormat — fixed. F14 (68) the bare-name
list omitted `pvalue` — fixed. F15 (65) the printed surface has no automated
guard. F9 (62) `details$baseline` overloads the name used by
`scaling_factor[["baseline"]]` — flagged in the `@return` rather than renamed.
F10 (62) grammar embedding the new fields in the `NA` clause — fixed. F11 (55)
`n_moments` wording imprecise on the FIML mean structure. F8 (50) AC4's
fixture-dependent skips — converted to assertions anyway. F18 (48) stale
`:103` anchors after the fix moved the guard to `:116`. F17 (45) the baseline
chisq check is a round-trip, not an oracle. F13 (35) the "three sample sizes"
passage overstated. F19 (12) line length.

