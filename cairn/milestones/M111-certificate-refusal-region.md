# M111: Shrink the ill-conditioning refusal to what the certificate cannot certify

- **Status:** planned
- **Priority:** normal
- **Depends on:** M108
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** —

## Goal

Replace the a-priori condition-number refusal at both surfaces with M108's
per-fit certificate, so a fit computes whenever its own certificate passes.

## Scope

Surface tier: **user-facing** — it changes what `axes_reliability()` returns
for a class of fitted matrices.

This is the remedy D-048 recorded and D-050 made due rather than conditional.
The contract is fixed by RR19 and RR20 and is mechanism-agnostic: the refusal
becomes computed-with-certificate where the certificate passes, with refusal
retained for indefiniteness, exact singularity, and certificate failure. The
2.0.0 line is unreleased, so this ships as an exported-behaviour change on the
dev line as D-048's own threshold move did, not through a deprecation cycle.

**In:**
- Both consuming surfaces, `R/axes_corrected_se.R:302` and `R/axes_scaled_fit.R:165`.
- A refusal literal for certificate failure, joining the existing vocabulary.
- The `WHY THE LIMB EXISTS AT ALL` section and the exported doc sites.
- `NEWS.md`.

**Out:**
- The certificate itself → M108.
- The `"indefinite"` and `"singular"` limbs, which keep refusing unchanged.
- Moving the constants → refused by D-048 and D-049.
- The calibration-domain sentence → M110.

## Acceptance criteria

- [ ] AC1: `axes_sigma_degenerate()` keeps its condition-number floor only as
      the partition between `"indefinite"` and `"singular"`; neither surface
      refuses on that floor alone.
- [ ] AC2: Each of the five reachable-geometry cases the M106/RR19 family in
      `devel/degeneracy-oracle/exact_oracle.R` enumerates, injected at the
      `axes_fitted_cov()` seam, yields finite corrected component SEs and a
      finite `cval`, with `details$se_correction_failed` and
      `details$fit_scaling_failed` both NULL.
- [ ] AC3: The committed counterexample at
      `tests/testthat/fixtures/rb18-counterexample-b.rds`, injected at the same
      seam, is refused at both surfaces with one shared literal distinct from
      `"indefinite"` and `"singular"`, and both surfaces report that same
      literal for it — the nestedness contract at
      `R/axes_corrected_se.R:363-377` holding across the new literal.
- [ ] AC4: The `"indefinite"` and `"singular"` limbs still refuse with their
      own literals at both surfaces, on both sides of the partition boundary,
      at two values of p and two spectral forms.
- [ ] AC5: The span from the `WHY THE LIMB EXISTS AT ALL` heading to the end of
      the derivation block in `R/axes_corrected_se.R` is rewritten, and no line
      within that span states that the package has no shipped means of
      certifying a computed answer past the floor, or that the reopening
      trigger is only partly met.
- [ ] AC6: The new refusal path is mutation-proved: with the certificate's
      comparison inverted, and separately with its threshold removed, an AC2 or
      AC3 assertion reddens in each case.
- [ ] AC7: `NEWS.md` records the change in what the ill-conditioning refusal
      does; `Rscript -e 'devtools::document()'` produces no diff;
      `Rscript -e 'devtools::test()'` and
      `Rscript -e 'devtools::check(args = "--no-manual")'` clean.

## Coverage

- AC1 → T1
- AC2 → T1, T3
- AC3 → T1, T2, T3
- AC4 → T4
- AC5 → T6
- AC6 → T5
- AC7 → T7

## Tasks

- [ ] T1: Wire M108's certificate into both surfaces, keeping the floor as the
      indefinite/singular partition only.
- [ ] T2: Add the certificate-failure literal to the refusal vocabulary and to
      the enumeration test at `tests/testthat/test-axes-corrected-se.R:1179`.
- [ ] T3: Write the AC2/AC3 tests at the `axes_fitted_cov()` seam, using the
      builders in `tests/testthat/helper-m106-degeneracy.R`.
- [ ] T4: Re-fence the partition (AC4). The pinned tests at
      `test-axes-scaled-fit.R:1906` and `:1944` assert the criterion function's
      own return, which T1 leaves intact; assert the surfaces' behaviour beside
      them rather than editing them.
- [ ] T5: Mutation-prove the new path (AC6), recording which assertion each
      mutant reddens.
- [ ] T6: Rewrite the `WHY THE LIMB EXISTS AT ALL` span. Two claims in it are
      falsified by this milestone and by D-050: "the package has no shipped
      means of certifying the number to delta_star" (`:574`) and "its first
      trigger is partly met on the record already" (`:581`), the latter already
      stale since D-050 recorded that trigger met in full.
- [ ] T7: NEWS entry, document, verify and check.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan chose keeping the criterion's floor as an indefinite/singular partition over removing it outright, because removing it fails the two pinned partition tests and the partition is independent of the accuracy question the certificate answers; falsified by a matrix the partition misclassifies once the floor no longer gates refusal.
- 2026-08-24: plan chose asserting the five reachable cases at the `axes_fitted_cov()` seam over routing them through `axes_reliability()`, because the oracle enumerates matrices while the exported path refits and would price a different matrix than the one enumerated; falsified by a fit whose refitted Sigma-hat lands on the other side of the certificate from the matrix it was built from.
- 2026-08-24: criteria audit findings for this milestone are recorded in M108's work log, which covers the joint audit run.
- 2026-08-24: RR21 (M108's mechanism review) routes four items here: re-key the `"ill_conditioned"` refusal to the certificate and carry the fit's own estimate in the warning (rec 4, the shape D-051's consequences state); surfacing the estimate on computed fits as well as refused ones (rec 5, a design call); emitting the exact oracle's values as hex double pairs so a reference route can be pinned below double resolution (B3); and extending the certificate's worst-component maximum to the FIML ratio vector, which the same pricing call already computes (B4). None is adopted by M108.
- 2026-08-24: M108's review routes its remainder here, headed by the packaged bracket asserting on macOS only (all six cases skip on ubuntu and windows) with nothing failing when that domain empties; thirteen lower-ranked findings ride with it. Text and disposition for each are in the M108 archive's Review; the ROADMAP degeneracy candidate row carries the promotion clause.

## Decisions

## Review
