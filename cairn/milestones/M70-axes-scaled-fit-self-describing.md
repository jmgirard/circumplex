# M70: Self-describing scaled fit output for `axes_reliability()`

- **Status:** review
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m70-axes-scaled-fit-self-describing`

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

- [ ] **AC1** — `details` gains `n_moments`, the number of distinct analyzed
      moments `p * (p + 1) / 2`, so `p*/N` is readable without arithmetic over
      `n_items`; the N of that ratio is the existing `details$n`, which AC2's
      `@return` text names explicitly as the sample size the fit was priced at,
      distinguishing it from the `n_total` and `n_complete` beside it. Evidence:
      a test asserts `n_moments == p * (p + 1) / 2` for a fit of known p = 24,
      and that `details$n` equals `lavaan::fitMeasures(fit, "ntotal")` on the
      cormat, listwise and FIML paths — each probed on data where those three
      N's differ, so the assertion cannot pass by coincidence.
- [ ] **AC2** — The `@return` text for `details` names both new fields and says
      what they are for, and the vignette's calibration passage tells the reader
      where to read `p*/N` off the object. Evidence: a doc guard reads
      `man/axes_reliability.Rd` and the vignette source and asserts both field
      names plus a verb-carrying phrase of their stated meaning; a second
      assertion pins that the vignette's calibration table and the sentence
      pointing at the object are both present, so removing one without the
      other reddens.
- [ ] **AC3** — Every user-facing surface reporting the scaled `cfi`/`rmsea` —
      the roxygen in `R/axes_reliability.R`, the printed output built in
      `R/axes_reliability_oop.R`, `vignettes/axes-reliability.Rmd`, and
      `NEWS.md` — names the lavaan variant the values correspond to
      (`cfi.scaled` / `rmsea.scaled`) and warns that a `fitMeasures()`
      cross-check against either lavaan's unscaled `cfi` or its `cfi.robust`
      will differ. Procedure: those four surfaces are read in full, not grepped,
      and `man/` is regenerated from the roxygen and checked to carry the
      wording; the milestone commits the per-surface list with each classified
      as updated, or as not reporting these values with its reason.
- [ ] **AC4** — AC3's correspondence claim is true of the shipped computation: a
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
- [ ] **AC5** — `axes_scaling_factor()`'s nonpositive-diagonal guard
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
- [ ] **AC6** — The profile's `verify` slot is clean: `devtools::document()`
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

## Decisions

## Review
