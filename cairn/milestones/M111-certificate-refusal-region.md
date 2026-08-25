# M111: Shrink the ill-conditioning refusal to what the certificate cannot certify

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M108
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** `m111-certificate-refusal-region`

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
- [ ] AC3: Each of two matrices whose accuracy certificate fails — one on each
      of the certificate's two failure routes — is refused at both surfaces
      under one shared literal distinct from `"indefinite"` and `"singular"`,
      both surfaces reporting that same literal for it (M89's nestedness
      contract, holding across the new literal). The graded route is the
      committed counterexample at
      `tests/testthat/fixtures/rb18-counterexample-b.rds`, priced directly by
      `axes_corrected_se()` and `axes_scaling_factor()` because at p = 3 it
      cannot ride the `axes_fitted_cov()` seam — `axes_reliability()` refuses
      fewer than four scales. The sentinel route is a p = 24
      near-duplicate-item matrix injected at that seam, where
      `axes_reliability()` itself reports the literal in
      `details$se_correction_failed` and `details$fit_scaling_failed`.
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

- [x] T1: Wire M108's certificate into both surfaces, keeping the floor as the
      indefinite/singular partition only.
- [x] T2: Add the certificate-failure literal to the refusal vocabulary and to
      the enumeration test at `tests/testthat/test-axes-corrected-se.R:1179`.
- [x] T3: Write the AC2/AC3 tests at the `axes_fitted_cov()` seam, using the
      builders in `tests/testthat/helper-m106-degeneracy.R`.
- [x] T4: Re-fence the partition (AC4). The pinned tests at
      `test-axes-scaled-fit.R:1906` and `:1944` assert the criterion function's
      own return, which T1 leaves intact; assert the surfaces' behaviour beside
      them rather than editing them.
- [x] T5: Mutation-prove the new path (AC6), recording which assertion each
      mutant reddens.
- [x] T6: Rewrite the `WHY THE LIMB EXISTS AT ALL` span. Two claims in it are
      falsified by this milestone and by D-050: "the package has no shipped
      means of certifying the number to delta_star" (`:574`) and "its first
      trigger is partly met on the record already" (`:581`), the latter already
      stale since D-050 recorded that trigger met in full.
- [ ] T7: NEWS entry, document, verify and check.
- [x] T8: Correct the stale `6.5e-6` corner figure in the accuracy-target
      block (`R/axes_corrected_se.R`), which derives from a = 0.046 while the
      block states a = 0.045 (0.1*0.045/sqrt(5e5) = 6.36e-6). Comment-only;
      routed here by the M110 review, outside its own diff.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan chose keeping the criterion's floor as an indefinite/singular partition over removing it outright, because removing it fails the two pinned partition tests and the partition is independent of the accuracy question the certificate answers; falsified by a matrix the partition misclassifies once the floor no longer gates refusal.
- 2026-08-24: plan chose asserting the five reachable cases at the `axes_fitted_cov()` seam over routing them through `axes_reliability()`, because the oracle enumerates matrices while the exported path refits and would price a different matrix than the one enumerated; falsified by a fit whose refitted Sigma-hat lands on the other side of the certificate from the matrix it was built from.
- 2026-08-24: criteria audit findings for this milestone are recorded in M108's work log, which covers the joint audit run.
- 2026-08-24: RR21 (M108's mechanism review) routes four items here: re-key the `"ill_conditioned"` refusal to the certificate and carry the fit's own estimate in the warning (rec 4, the shape D-051's consequences state); surfacing the estimate on computed fits as well as refused ones (rec 5, a design call); emitting the exact oracle's values as hex double pairs so a reference route can be pinned below double resolution (B3); and extending the certificate's worst-component maximum to the FIML ratio vector, which the same pricing call already computes (B4). None is adopted by M108.
- 2026-08-24: M108's review routes its remainder here, headed by the packaged bracket asserting on macOS only (all six cases skip on ubuntu and windows) with nothing failing when that domain empties; thirteen lower-ranked findings ride with it. Text and disposition for each are in the M108 archive's Review; the ROADMAP degeneracy candidate row carries the promotion clause.
- 2026-08-24: implement started; branch `m111-certificate-refusal-region` cut from master.
- 2026-08-24: gate chose one shared refusal predicate at both surfaces (the worse of the certificate's two estimates against delta_star) over per-surface fields, the literal `"uncertified"`, the certificate's estimate carried in the refusal warning, and folding the M110-review stale-figure correction in as T8.
- 2026-08-24: AC3 amended (mini gate). Its "injected at the same seam" step is unsatisfiable: the p = 3 fixture carries three item names and `axes_reliability()` refuses fewer than four scales, so the seam's realignment errors `subscript out of bounds`. Amended text prices the fixture directly and adds a p = 24 near-duplicate matrix at the seam so the exported surface is pinned.
- 2026-08-24: criteria audit of the amended AC3 ran in full mode (user-facing tier), two fresh-context [O] readers, neither an author of the wording. Findings disposed: the first draft's `naive_reason` clause was unfalsifiable and misattributed (dropped); the second draft's opening clause quantified over all certificate failures while the certificate gates only the ill-conditioned arm (narrowed to two enumerated matrices); the exported surface was named only as coverage prose (rewritten as the promise it carries).
- 2026-08-24: T1 wired the certificate in at both surfaces through one shared decision helper, `axes_degeneracy_refusal()`; the floor now selects which fits are checked, and only a fit whose worse certificate estimate exceeds delta_star refuses, as `"uncertified"`.
- 2026-08-24: T2 added `"uncertified"` to the refusal vocabulary and to the BC5 enumeration test, which now also reads the list-field shape the shared decision helper returns its literal in.
- 2026-08-24: T3 added `tests/testthat/test-axes-certificate-refusal.R` (AC2, AC3). AC2's five geometries are fit on well-conditioned siblings of the same design and injected at the seam: lavaan does not converge on the p = 8 kappa 1e5 anchor itself. Two of the five (a5, b9b) were refused before this milestone and now compute.
- 2026-08-24: T1/T2/T3 moved 28 assertions in four existing test files onto the new behaviour. Two classes: eleven sites where the surfaces still refuse under the new literal, and seventeen where the fit now computes -- the M106 floor-bracket tests (AC4 case 2 at p = 8, case 3 at p = 24, AC5 radius 2) and the two diagnostic-hint controls, whose refusing exemplars moved down to kappa 2.0e8 and 7.2e8 where the certificate's reference route fails.
- 2026-08-24: T4 fenced the two limbs the certificate is never asked about at the SURFACES (AC4), two p and two spectral forms (rotated planted eigenvalue, rank-one projector), both sides of the M90 partition boundary plus the non-finite route.
- 2026-08-24: T5 mutation-proved the new path (AC6). Comparison inverted (`<=` to `>=`): 20 assertions redden, including AC2's `se_correction_failed`/`fit_scaling_failed` NULL checks at a5 and b9b and AC3's `"uncertified"` checks at both routes. Threshold removed (the certified branch taken unconditionally): 10 redden, all in AC3 -- the sentinel route's two failure fields and the graded route's six.
- 2026-08-24: T6 rewrote the `WHY THE LIMB EXISTS AT ALL` span (AC5); T8 corrected the stale `6.5e-6` corner to 6.36e-6 and pinned every figure in that paragraph to its own 0.1*a/sqrt(n) derivation.

## Decisions

## Review
