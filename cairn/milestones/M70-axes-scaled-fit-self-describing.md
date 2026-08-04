# M70: Self-describing scaled fit output for `axes_reliability()`

- **Status:** planned
- **Priority:** normal
- **Depends on:** M69
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Let a reader of an `axes_reliability()` object locate its fit statistics on the
documented calibration curve, and against lavaan's own fit measures, without
recomputing anything.

## Scope

**In:** two new `details` fields — the count of distinct analyzed moments
`p* = p(p+1)/2` and an explicitly named field for the N the fit was priced at —
so the `p*/N` the vignette's calibration table is indexed by is readable off the
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
      moments `p * (p + 1) / 2`, and an explicitly named field for the N the fit
      was priced at, so `p*/N` is readable without arithmetic over `n_items`.
      Evidence: a test asserts `n_moments == p * (p + 1) / 2` for a fit of known
      p = 24, and that the reported N equals `lavaan::fitMeasures(fit,
      "ntotal")` on the cormat, listwise and FIML paths.
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

- [ ] **T1** — Add `n_moments`, the named N field, and the baseline chi-square
      and baseline df AC4 needs to the `details` list
      (`R/axes_reliability.R:1792`); test each on the cormat, listwise and FIML
      paths.
- [ ] **T2** — Write the `@return` text for the new fields, add the vignette
      sentence pointing at them beside the calibration table
      (`vignettes/axes-reliability.Rmd:189-202`), and add the paired doc guard.
- [ ] **T3** — Read the four user-facing surfaces in full, add the lavaan
      variant naming and the cross-check warning to each that reports the
      scaled values, and commit the per-surface classification ledger.
- [ ] **T4** — Add the `cfi.scaled`-versus-`cfi.robust` recomputation test on a
      misfitting probe, with the arithmetic written out and any lavaan internal
      reached only via `get()`.
- [ ] **T5** — Change the guard at `R/axes_scaled_fit.R:103` to
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

## Decisions

## Review
