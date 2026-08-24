# M108: Build and validate a per-fit accuracy certificate

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** m108-per-fit-certificate

## Goal

Build an a-posteriori per-fit accuracy certificate for the corrected component
standard errors and the scaling factor, validated against the exact-rational
oracle, without changing what any exported function returns.

## Scope

Surface tier: **user-facing** — the certificate computes a number that will
govern a user-visible refusal, so it is planned at the stricter tier even
though this milestone changes no exported return.

**In:**
- A Fable escalation settling the certificate's mechanism, and a D-entry
  recording what was chosen and what was rejected.
- An internal certificate estimating, per fit, the relative error carried by
  that fit's corrected component SEs and by its `cval`.
- Validation against `devel/degeneracy-oracle/exact_oracle.R`.
- Repointing that oracle at the packaged fixture copy (the third M107 review
  remainder; its promotion condition is this oracle being run or edited).

**Out:**
- Any change to what `axes_reliability()` returns or refuses → M111.
- Moving `axes_degeneracy_delta_star`, the calibration ceiling, or `tau` →
  refused by D-048 and D-049; a move is its own escalation, never this work.
- The calibration-domain sentence → M110. The stale claims in the
  `WHY THE LIMB EXISTS AT ALL` section → M111.

## Acceptance criteria

- [ ] AC1: An internal function returns a finite, non-negative estimate of the
      relative error carried by a fit's corrected component SEs and of its
      `cval`, for every matrix on which the `"singular"` and `"indefinite"`
      limbs of `axes_sigma_degenerate()` pass, and the quantity it estimates is
      stated beside it. The estimate does not depend on the typed sample size:
      evaluated at two values of `n` on one matrix it returns identical values,
      on the ground D-048 and D-049 refused an n-dependent target.
      (RB tripwire: ip-touching)
- [ ] AC2: On each of the five reachable-geometry cases the M106/RR19 family in
      `devel/degeneracy-oracle/exact_oracle.R` enumerates, the estimate is at
      least the exact-rational oracle's measured relative error for that case
      and at most 1e3 times it — for the SE estimate and the `cval` estimate
      separately. The 1e3 ceiling is pre-registered here, before measurement:
      the a-priori bound this replaces overstates by 5 to 8 decades, so 1e3 is
      at least two decades of improvement and falsifies a certificate that only
      restates the old bound.
- [ ] AC3: The estimate discriminates. On the five cases of AC2 every estimate
      is below 1e-4; on the committed counterexample at
      `tests/testthat/fixtures/rb18-counterexample-b.rds`, whose corrected SEs
      the oracle measures 3.4% wrong, the estimate exceeds 1e-4.
- [ ] AC4: The certificate is mutation-proved by at least three planted
      defects varying in form as well as in location, each recorded with the
      AC2 or AC3 assertion it reddens.
- [ ] AC5: `Rscript devel/degeneracy-oracle/exact_oracle.R` run from the repo
      root exits 0 while `cairn/` is moved aside, reading its fixture from
      `tests/testthat/fixtures/`.
- [ ] AC6: No exported return value changes: the test suite as it stood at this
      branch's base commit passes unmodified against the branch head.
- [ ] AC7: `Rscript -e 'devtools::test()'` and
      `Rscript -e 'devtools::check(args = "--no-manual")'` clean (0 errors,
      0 warnings; any NOTE justified).

## Coverage

- AC1 → T2, T3
- AC2 → T3, T4, T5
- AC3 → T4, T5
- AC4 → T7
- AC5 → T4
- AC6 → T8
- AC7 → T8

## Tasks

- [x] T1: Author the RB for the certificate's mechanism — RR20 names two
      candidates (two independent factorization routes compared; on-demand
      exact-rational recomputation of `v_r` and `cval`) and tiers the choice
      Fable. Escalate via `/milestone-brief`. (RB tripwire: ip-touching)
- [x] T2: Ingest the RR; record the chosen mechanism, the rejected one, and the
      evidence class that would falsify the choice, as a D-entry.
- [x] T3: Implement the certificate as an internal function, n-free by
      construction. `axes_se_pricing()` (`R/axes_corrected_se.R:153-207`) is
      already callable at two different matrices and is the existing
      price-it-twice-and-compare primitive.
- [x] T4: Repoint `exact_oracle.R`'s `FIXTURE` (`:19`) at the packaged copy and
      extend its per-case output to emit the certificate beside the exact
      relative error, for the five reachable cases and counterexample B.
- [x] T5: Write the AC2/AC3 tests at the `axes_fitted_cov()` injection seam,
      using the builders in `tests/testthat/helper-m106-degeneracy.R`.
- [x] T6: Satisfy IP3's two-independent-oracle-types bar for the certificate's
      number, each oracle recorded at its asserting test per DESIGN.md's
      Oracle records convention; the second type is settled by T2's RR.
- [x] T7: Mutation-prove (AC4) — three planted defects varying form and
      location, each verified to redden a named assertion.
- [ ] T8: Run the base-commit test suite against the branch head (AC6); run the
      profile verify slot and the check (AC7).

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose splitting the certificate into mechanism (M108) then rewiring (M111) over one milestone with the design task first, because criteria written before the design review exists took two gated amendment returns in M106; falsified by an RR that changes nothing M111's criteria assume.
- 2026-08-24: plan chose validating against the exact-rational oracle plus a second internal route over runtime exact-rational recomputation as the default shape put to the RB, because the oracle's exact arithmetic is Python and a runtime path would need a new dependency under GP3; falsified by an R rational route cheap enough to run per fit. The RR settles it, not this line.
- 2026-08-24: criteria audit ran in FULL mode ([O], fresh context, authored none of them) over M108 and M111 together; it returned twelve findings. Six with one clear right answer were fixed here before writing: the n-invariance omission that would have rebuilt the yardstick-dependence D-048 refused, the post-hoc "stated factor" ceilings that constrained nothing, the unscoped "finite bound" universal, the diff procedure banning edits to any existing test file, M111's five cases entering through a refit rather than the seam, and the absence of any planted-defect probe in either milestone. Six design calls were settled as planner decisions and recorded as rejected-alternative lines rather than reopened as questions.
- 2026-08-24: implement started; branch m108-per-fit-certificate cut from master at 298747a5.
- 2026-08-24: question gate chose escalating the certificate mechanism via /milestone-brief over settling it in-session, per the plan T1 tripwire tag.
- 2026-08-24: question gate chose deleting cairn/reviews/rb18-counterexample-b.rds once the oracle reads the packaged copy, over keeping both; done at T4.
- 2026-08-24: blocked on RB21 (certificate mechanism; fourth escalation of the ill-conditioning limb, removal listed as question 6).
- 2026-08-24: T1 done — RB21 authored and reviewed; the brief listed removal of the ill-conditioning limb as its question 6, this mechanism's fourth escalation.
- 2026-08-24: T2 done — RR21 ingested. Mechanism promoted to D-051; the estimand, sentinel contract, second oracle type, planted-defect set and cost envelope recorded in this file's Decisions section. RB21/RR21 archived.
- 2026-08-24: RR21 recommendations triaged — 1, 2, 3 apply here (T3-T7); 4 and 5 route to M111 and are logged there; 6 (hex-precision oracle output; extending the estimate to the FIML ratio vector) is not adopted, because both would widen what M108's criteria promise for no gain M108's validation needs, and is logged to M111 with the rest; 7, 8, 9 are the rejections D-051 records.
- 2026-08-24: question gate chose splitting the shipped pre-square-root pricing into internal kernels both the shipped path and the certificate call, over a second copy of that arithmetic inside the certificate, so the certificate prices what ships by construction; AC6 is what checks no returned number moved.
- 2026-08-24: question gate chose a certificate that takes neither `n` nor `df` over one that takes `df`, because `df` is an exact integer divisor that cancels from a relative error exactly as `n` does; the one rounding it adds sits under the 2-epsilon floor.
- 2026-08-24: T3 done — `axes_accuracy_certificate()` in R/axes_certificate.R: vectorized double-double arithmetic (two-sum, Dekker two-product, tree summation, pivoted Gauss-Jordan) replaying the shipped pricing, estimate = 10*max(delta/2, 2*eps), sentinel 1 on any route failure, known-answer self-test of the error-free transforms. The shipped pre-root pricing split into axes_pricing_core()/axes_v_pricing()/axes_u_pricing(); two cross-file line-range citations updated to follow the moved code. devtools::test() 0 failures, 8526 passes (5 warnings pre-existing, in ci_accuracy and ssm_sem).
- 2026-08-24: cost measured at the largest reachable design (p = 24, q = 27): 0.234 s against 14.75 ms for the double pricing — 16x, inside the envelope this file's Decisions section records (up to ~100x, ≤ ~0.5 s).
- 2026-08-24: T4 done — the oracle reads tests/testthat/fixtures/, emits the certificate beside each measured error at all six geometries, and fails if any ratio leaves [1, 1e3]; its transcribed copy of the shipped cval arithmetic replaced by the axes_u_pricing() call. AC5 verified: `Rscript devel/degeneracy-oracle/exact_oracle.R` from the repo root with cairn/ moved aside exits 0, all four flags PASS.
- 2026-08-24: T4 deleted cairn/reviews/rb18-counterexample-b.rds and tests/testthat/test-fixture-drift.R (the byte-identity guard between the two copies) per the recorded gate; the fixture's provenance comment in test-axes-scaled-fit.R and one prose path in R/axes_corrected_se.R updated, and M109's work log notes that its AC4 cites the deleted file for a shape AC4 states in full itself.
- 2026-08-24: T5 done — tests/testthat/test-axes-certificate.R asserts AC2's floor and its 1e3 ceiling at the five reachable geometries and at counterexample B, AC3's discrimination against delta_star, AC1's finiteness across the admitted domain (including machine-singular and roundoff-negative matrices) and its no-n formals, and the sentinel on both routes. Each geometry is fingerprinted by its condition number so a builder edit cannot silently leave the frozen oracle figures describing a different matrix.
- 2026-08-24: T6 done — two independent oracle types recorded at the asserting test: the frozen exact-rational Python oracle (generator named, regeneration command given) and a hand-derived closed-form oracle at a dyadic-rational p = 2 configuration whose exact values (v = 97/128, u = 5/8) the reference route reproduces bit for bit with a zero low word. A planted-perturbation sensitivity invariant ships beside them and is not counted as a type.
- 2026-08-24: T7 done — six planted defects run one at a time; five redden at least one AC2 or AC3 assertion, the sixth reddens nothing and is recorded as a null probe. Per-defect results in this file's Decisions section.

## Decisions

- 2026-08-24 (RR21 §2): the certificate's estimand is fixed as the committed
  relative error of the corrected component SE vector, aggregated by its worst
  component, and of `cval`. It is an estimate of the committed error, not a
  proven upper bound; a stated safety factor of 10 and an additive floor of two
  machine epsilons make it behave as an upper estimate, and that behavior is
  what AC2 and AC3 pin. The estimate is computed from the pre-square-root
  quadratic forms, where the sample size does not appear, and the function
  takes no `n` argument at all — AC1's two-`n` identity is therefore structural.
  Scope stated so it cannot be overclaimed: the certificate prices the
  arithmetic from the priced matrix to the reported numbers; the optimizer's
  error in Sigma-hat and `cov2cor()`'s own rounding are upstream of that matrix
  and out of scope, as they were for the a-priori bound.
- 2026-08-24 (RR21 §5): where either route fails to produce finite values, or a
  quadratic form is nonpositive, or a denominator is zero, the certificate
  returns 1 — "no digits certified". Finite and non-negative by construction,
  and above the accuracy target, so M111's gate fails closed. The meaning
  belongs in the function's header (RR21 B5). Measured along the degradation
  path, the shipped pricing's own `solve()` refuses `"unidentified"` before the
  certificate matters, at and past the point where there is no reported number
  to certify.
- 2026-08-24 (RR21 §4): T6's second independent oracle type is a closed-form
  oracle — one small configuration driven through the pricing seam with the
  matrix and every derivative matrix chosen dyadic-rational, so the two priced
  quantities are exact rationals derived by hand once and committed as literal
  fractions at the asserting test. It shares no code, no library, and no
  pipeline with the exact-rational Python oracle or with the route under test.
  A planted-perturbation sensitivity invariant ships alongside as a supporting
  layer, not as a counted type. The brief's independence hazard — an agreement
  check between the certificate's own two routes — does not arise: neither
  layer is such a check.
- 2026-08-24 (RR21 rec 2): AC4's planted defects, chosen to vary form as well
  as location — truncate the reference route's low-order words before comparing
  (the estimate collapses toward zero, reddening AC2's floor at the
  counterexample); drop the correlation-Jacobian diagonal fold in the reference
  route only (reddens AC2's ceiling on all five reachable cases); drop the
  safety factor (reddens AC2's floor at the two anchors whose raw ratios sit
  just below one); skip an element in the summation (reddens the closed-form
  oracle).
- 2026-08-24 (RR21 §7): the accepted per-fit cost envelope is up to about 100
  times the double-precision pricing and no more than about half a second at
  the largest reachable design. The recommended mechanism was measured inside
  it. The two-word arithmetic is vectorized from the start rather than written
  as scalar loops, which is where the measured difference lies.

- 2026-08-24 (AC4, planted defects): six defects were planted one at a time in
  `R/axes_certificate.R`, the certificate suite run against each, and each
  reverted before the next. **A — the correlation-Jacobian diagonal fold
  dropped in the reference route only**, so the replay prices a different
  quantity than the shipped route: reddens AC2's 1e3 ceiling on the reachable
  geometries, AC3's below-target assertion, and the closed-form oracle.
  **B — the safety factor dropped** (10 to 1): reddens AC2's floor at the
  reachable anchors and at counterexample B, which is what makes the factor
  load-bearing rather than decorative. **C — the tree summation drops its odd
  element** instead of carrying it: reddens AC2's ceiling and AC3. **D — the
  double-double matrix product drops its last rank-one term**: reddens AC1's
  n-invariance check, AC2 at the reachable anchors and at B, AC3, and the
  closed-form oracle. **E — the reference route stops carrying low-order words
  at all**, its renormalization returning a zero low word so the replay
  degrades to double precision: reddens AC2's floor for `cval` at
  counterexample B, the collapse-toward-zero RR21 predicted. **F — a null
  probe, recorded because it found nothing:** zeroing the low word of the
  double-double MULTIPLICATION alone reddens no assertion at these six
  geometries. What the replay's accuracy rests on here is its additions, where
  the cancellation is; a wrong multiplication is caught by the closed-form
  oracle (defect D), not by the low word of a right one.

## Review
