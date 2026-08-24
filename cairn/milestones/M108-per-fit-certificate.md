# M108: Build and validate a per-fit accuracy certificate

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** —

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

- [ ] T1: Author the RB for the certificate's mechanism — RR20 names two
      candidates (two independent factorization routes compared; on-demand
      exact-rational recomputation of `v_r` and `cval`) and tiers the choice
      Fable. Escalate via `/milestone-brief`. (RB tripwire: ip-touching)
- [ ] T2: Ingest the RR; record the chosen mechanism, the rejected one, and the
      evidence class that would falsify the choice, as a D-entry.
- [ ] T3: Implement the certificate as an internal function, n-free by
      construction. `axes_se_pricing()` (`R/axes_corrected_se.R:153-207`) is
      already callable at two different matrices and is the existing
      price-it-twice-and-compare primitive.
- [ ] T4: Repoint `exact_oracle.R`'s `FIXTURE` (`:19`) at the packaged copy and
      extend its per-case output to emit the certificate beside the exact
      relative error, for the five reachable cases and counterexample B.
- [ ] T5: Write the AC2/AC3 tests at the `axes_fitted_cov()` injection seam,
      using the builders in `tests/testthat/helper-m106-degeneracy.R`.
- [ ] T6: Satisfy IP3's two-independent-oracle-types bar for the certificate's
      number, each oracle recorded at its asserting test per DESIGN.md's
      Oracle records convention; the second type is settled by T2's RR.
- [ ] T7: Mutation-prove (AC4) — three planted defects varying form and
      location, each verified to redden a named assertion.
- [ ] T8: Run the base-commit test suite against the branch head (AC6); run the
      profile verify slot and the check (AC7).

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose splitting the certificate into mechanism (M108) then rewiring (M111) over one milestone with the design task first, because criteria written before the design review exists took two gated amendment returns in M106; falsified by an RR that changes nothing M111's criteria assume.
- 2026-08-24: plan chose validating against the exact-rational oracle plus a second internal route over runtime exact-rational recomputation as the default shape put to the RB, because the oracle's exact arithmetic is Python and a runtime path would need a new dependency under GP3; falsified by an R rational route cheap enough to run per fit. The RR settles it, not this line.
- 2026-08-24: criteria audit ran in FULL mode ([O], fresh context, authored none of them) over M108 and M111 together; it returned twelve findings. Six with one clear right answer were fixed here before writing: the n-invariance omission that would have rebuilt the yardstick-dependence D-048 refused, the post-hoc "stated factor" ceilings that constrained nothing, the unscoped "finite bound" universal, the diff procedure banning edits to any existing test file, M111's five cases entering through a refit rather than the seam, and the absence of any planted-defect probe in either milestone. Six design calls were settled as planner decisions and recorded as rejected-alternative lines rather than reopened as questions.

## Decisions

## Review
