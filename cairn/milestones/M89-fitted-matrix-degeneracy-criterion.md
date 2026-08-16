# M89: One degeneracy criterion for the two fitted-matrix consumers

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2, GP4
- **Branch/PR:** —

## Goal

Give `axes_reliability()`'s two consumers of `lavaan::fitted(fit)$cov` one
stated criterion for when that matrix is too degenerate to price, so a user
never receives NA corrected SEs beside silently scaled fit statistics derived
from the same matrix.

## Scope

Surface tier: **user-facing** — `axes_reliability()`'s `$fit`, `$components`
and printed output are exported surfaces a user reads, and the reason literals
are documented and printed verbatim.

**In:** one stated degeneracy criterion on the fitted covariance matrix,
recorded beside its rationale and applied at both consumers
(`R/axes_reliability.R:1715`, `:1824`); relaxing `axes_corrected_se()`'s
emergent `solve()`-based refusals so the stated criterion is what gates;
one shared reason vocabulary across both surfaces, including the `+Inf` case;
what `axes_reliability()` reports when the criterion fires; regression probes
at the boundary; the documented reason enumerations and a NEWS entry.

**Out:** the upstream input-side positive-definiteness gate at
`R/axes_reliability.R:1428`, which prices the user's input R rather than the
fitted matrix → stays unowned, no candidate row (no defect is known in it).
Any change to the scaling arithmetic itself → D-036 stands. A new oracle for
the scaled statistic → M68 already carries one.

## Acceptance criteria

- [ ] **AC1** — A single stated degeneracy criterion on the fitted covariance
      matrix is recorded in code beside its rationale, and is applied at both
      consumers: the `corrected <- axes_corrected_se(...)` expression and the
      `scaling <- axes_scaling_factor(...)` expression in
      `R/axes_reliability.R`. *(RB tripwire: no-oracle — escalation offered and
      declined at the plan gate; see work log.)*
- [ ] **AC2** — The two surfaces agree, in both senses, over the probe grid the
      AC4 test enumerates (both diagonal positions × k = 0..16 × the forms in
      AC4) plus the `+Inf` and `-Inf` cases: they return a non-NULL `reason` at
      exactly the same grid points, and wherever both refuse they name the same
      reason literal. `axes_corrected_se()` is the surface that adopts
      `"infinite_diagonal"` for `+Inf`; `axes_scaling_factor()`'s literal is
      unchanged. Asserted by a test that runs that grid and compares the two
      `reason` fields pairwise.
- [ ] **AC3** — The new criterion is evaluated after the existing `<= 0` and
      `is.infinite()` diagonal guards in `axes_scaling_factor()`
      (`R/axes_scaled_fit.R:147-148`), and the M71 AC1/AC2 block in
      `tests/testthat/test-axes-scaled-fit.R:1258-1300` passes byte-unchanged.
- [ ] **AC4** — Regression probes fail against the pre-milestone code at ≥2
      distinct diagonal positions, and include ≥1 non-inflation form (a
      near-collinear item pair or a near-zero positive diagonal) that drives
      the divergence in the opposite direction — `axes_scaling_factor()`
      refusing while the raw-priced branch survives.
- [ ] **AC5** — The documented reason enumerations (`R/axes_reliability.R`
      roxygen and the regenerated `man/axes_reliability.Rd`) and `NEWS.md`
      name the new literal and the new NA condition.
- [ ] **AC6** — On a constructed fitted matrix that trips the criterion inside
      `axes_reliability()`, the corrected component SEs and the four scaled
      statistics D-036 scales (`chisq`, `pvalue`, `rmsea`, `cfi`) are all NA,
      each surface warning names the shared reason, and `df` and `srmr` are
      unaffected.
- [ ] **AC7** — `devtools::test()` clean and `devtools::check(args =
      "--no-manual")` clean, with a warning-free `devtools::document()` and a
      diff-free `man/`/`NAMESPACE` beyond AC6's intended change.

## Coverage

- AC1 → T3, T4
- AC2 → T1, T4, T5
- AC3 → T5
- AC4 → T1, T2
- AC5 → T7
- AC6 → T6
- AC7 → T8

## Tasks

- [ ] **T1** — Test-first: the AC2 grid as a failing test — both diagonal
      positions × k = 0..16 at the octant probe, comparing the two `reason`
      decisions pairwise. Red against HEAD from k = 7 up.
- [ ] **T2** — Add the non-inflation probe form (near-collinear pair or
      near-zero positive diagonal) and confirm by measurement that it drives
      the divergence in the opposite direction. Red against HEAD.
- [ ] **T3** — Choose the stated criterion and record its rationale in code.
      It must price the raw Σ̂: `cov2cor()` of an inflated matrix stays at
      condition 10.45, so a correlation-metric test cannot see this at all.
- [ ] **T4** — Apply the criterion at both consumers; relax
      `axes_corrected_se()`'s emergent `solve()`-based refusals
      (`R/axes_corrected_se.R:162-163`) so the stated criterion is the gate.
- [ ] **T5** — Unify the reason vocabulary; confirm the M71 block passes
      byte-unchanged.
- [ ] **T6** — Assembly-level test through `axes_reliability()` on a
      constructed fitted matrix.
- [ ] **T7** — Roxygen reason enumeration, `devtools::document()`, NEWS entry.
- [ ] **T8** — Full `devtools::check()`.

## Work log

- 2026-08-15: created by /milestone-plan. Graduates two ROADMAP candidate rows — the finite-degenerate scaling row and the `+Inf` reason-label row, the latter taken in by the plan gate's wide-scope choice.
- 2026-08-15: criteria audit ([O], fresh context) returned seven findings; six fixed before the gate (numbering as shipped): AC1's `grep` procedure selected four lines of which two were comments, replaced with expression-pinned sites; AC3's ordering constraint stated; AC6 restated at the helper-plus-assembly boundary with a constructed matrix, no real fit being known to reach the regime, and narrowed to the four statistics D-036 scales; AC4 widened past one exemplar to two positions plus a non-inflation form; AC5 added for the documented enumerations the user-facing tier obliges. The two judgment calls went to the gate as one scope-width question; the draft's AC2 and AC3 were merged after the gate to hold the criteria count under the split tripwire.
- 2026-08-15: plan gate chose the wide scope — one stated criterion gating both surfaces, emergent `solve()`-based refusals relaxed, one shared reason vocabulary — over the narrow scope that adds the criterion and leaves the existing refusals and labels alone, because the narrow version leaves the `+Inf` case still disagreeing and meets the Goal only half. Falsified by evidence that relaxing the emergent refusals lets a genuinely unpriceable matrix through, which would argue those guards were load-bearing.
- 2026-08-15: plan gate chose to decide the cutoff in the build over escalating it to a written Fable review, because the choice is a numerical-conditioning call rather than a new statistical quantity and the build can justify it in code. Falsified by the build finding the cutoff turns on a statistical property of the estimator rather than on conditioning.

## Decisions

## Review
