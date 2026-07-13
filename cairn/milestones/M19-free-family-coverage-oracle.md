<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M19: CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration

- **Status:** review
- **Priority:** high
- **Depends on:** M18
- **Principles touched:** —
- **Branch/PR:** m19-free-family-coverage-oracle · [PR #43](https://github.com/jmgirard/circumplex/pull/43)

## Goal

Measure the empirical coverage of the free-scaling family's analytic (Wald)
confidence intervals via a simulation-coverage oracle, and replace M18-D3's
unconditional caution with the coverage-validated statement the data support.

## Scope

**In:**
- Extend `devel/m4-coverage-oracle.R` with **free-scaling** cells: circumplex-
  **correlation** truths P(γ) (σ_pop = 1 — the correlation-input contract forces
  it; the free family fits σ as free nuisance parameters absorbing finite-N
  correlation misfit), fit with `scaling = "free"`, measuring **analytic-CI
  coverage for θ/ζ/β** at nominal 95% — boundary + interior β at N ∈ {250, 1000},
  plus an analytic-only N-ladder spot including a Heywood-prone cell (spec §6).
  Seeded, reproducible, committed `.rds`.
- Collect `T_free = n·F̂` per replicate in the same runs and record the summary
  (near-free; enables the deferred T_diag-vs-T_free comparison without a re-run).
- Second oracle type: on one spot cell, cross-check the FD-Hessian analytic SEs
  against an independent **live** source (OpenMx free-model SEs and/or a
  parametric-bootstrap SE), so the CI numbers meet the ≥2-oracle-types bar.
- Record measured free-family coverage in `cairn/DESIGN.md` as a table + what-it-
  decides prose, mirroring the M4/B6 "CPM confidence intervals: measured coverage"
  subsection; update the CPM-CI section (spec §8).
- Re-derive **or** explicitly re-affirm-with-justification the free-family
  `summary()` caution: replace the unconditional M18-D3 caution
  (`R/cpm_oop.R:227-238`) and the "not yet coverage-validated" roxygen
  (`R/cpm_fit.R:1344`) with the coverage-validated statement the measurements
  support (new free-family constants if N-conditional; a justified unconditional
  caution otherwise). Record the outcome as a milestone-local decision promoted
  to a DECISIONS.md entry superseding M18-D3's deferral.
- In-suite asserting smoke test (small seeded reps, in-band assertion,
  `skip_on_cran()`) for the simulation-coverage oracle; snapshot update for the
  changed caution wording; register the oracle (id/type/test:line/source).

**Out:**
- **Bootstrap θ/ζ/β coverage** for the free family (analytic CIs are what the
  caution gates) → stays part of the grouped free-scaling candidate.
- **Bootstrap σ CIs on the raw-data path** → candidate (2), deferred (spec §9).
- **The T_diag-vs-T_free inference-default decision** — the statistics are
  emitted here, but which family is the preferable inference default is decided
  later → candidate (3).
- **Covariance-matrix input** → candidate (4), rejected for now (RR04 rec 12).
- **`m4-browne-design.md` §3.2 rewrite** — already handled at M18 doc time
  (`devel/m4-browne-design.md:260-264`).

## Acceptance criteria

- [ ] `devel/m4-coverage-oracle.R` runs free-scaling cells (circumplex-
      correlation truths, σ_pop = 1; boundary + interior β at N ∈ {250, 1000} +
      ≥1 analytic-ladder spot including a Heywood-prone cell), seeded and
      reproducible, with committed `.rds` results; `T_free = n·F̂` collected per
      replicate. Source: `devel/circum-free-scaling-spec.md` §6.
- [ ] Measured analytic-CI coverage for θ/ζ/β under `scaling = "free"` is
      recorded in `cairn/DESIGN.md` as a coverage table + what-it-decides prose
      (mirroring the M4/B6 subsection). Source: spec §8.
- [ ] The analytic-CI coverage numbers are backed by **≥2 independent oracle
      types** — simulation-coverage (primary) + a live SE cross-check on a spot
      cell (FD-Hessian SE vs. OpenMx/parametric-bootstrap SE) — and the oracle is
      registered (id, type, asserting `test:line`, source). Source: tracking-rules
      "Validation doctrine" (≥2-types bar; a CI method's oracle is coverage).
- [ ] The free-family `summary()` caution is coverage-validated: `R/cpm_oop.R`'s
      unconditional free branch and `R/cpm_fit.R`'s "not yet coverage-validated"
      roxygen are replaced by the statement the measurements support (new
      constants if N-conditional, or a justified unconditional caution), recorded
      as a DECISIONS.md entry superseding M18-D3's deferral; a test pins the
      new caution wording (per the diag-caution convention at `test-cpm_api.R` —
      `expect_match`, not a snapshot; analytic CI endpoints are BLAS-sensitive).
      Source: spec §4 (lines 159-163); M18-D3.
- [ ] An in-suite asserting smoke test (small seeded reps, in-band coverage
      assertion, `skip_on_cran()`) exercises the free-family coverage path;
      `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1, T2
- AC2 → T4
- AC3 → T3, T5
- AC4 → T5, T6
- AC5 → T5, T6

## Tasks

- [x] **T1** — Extend `devel/m4-coverage-oracle.R`: reuse M4's circumplex-
      correlation truth configs (boundary/interior β at octant angles, ζ = .75),
      draw X ~ N(0, P(γ)) via `chol(P0)`, and add an analytic-only free-cell
      coverage pass fitting `cpm_engine(cor(X), scaling = "free")` + scoring θ/ζ/β
      via `cpm_analytic_se()` (already returns the σ-free bordered-Hessian SEs),
      with per-replicate `T_free` collection. Seeded/reproducible; smoke env var.
- [x] **T2** — Run the full seeded free-family oracle; commit the `.rds`
      results (analytic-only ⇒ session-runnable, no per-fit bootstrap).
- [x] **T3** — Interpret the coverage table: decide N-conditional vs. justified-
      unconditional caution for the free family; do **not** silently reuse the
      diag constants (spec §4). Draft the milestone-local decision.
      **Outcome:** N-conditional — the diag thresholds (2000/50000) are the
      correct free thresholds, now coverage-validated (not silently reused);
      small-N SE-fragility reinforces the N<2000 caution. → D-010 (T5).
- [x] **T4** — Record the measured coverage in `cairn/DESIGN.md` (new subsection
      mirroring "CPM confidence intervals: measured coverage"); update the CPM-CI
      section prose.
- [x] **T5** — Update `R/cpm_oop.R` free caution branch + `R/cpm_fit.R` roxygen
      to the validated statement (new free-family constants if N-conditional);
      add the in-suite coverage smoke test + register the oracle; update the
      caution-wording snapshot. Promote the T3 decision to DECISIONS.md
      (supersedes M18-D3's deferral).
      (RB tripwire: no-oracle — the coverage oracle IS the oracle here, so this
      is *not* a tripwire hit; noted to pre-empt a false escalation.)
- [x] **T6** — Live SE cross-check on one spot cell (parametric-bootstrap SE vs.
      FD-Hessian SE — self-contained, no OpenMx parameterization-matching);
      asserts agreement in the suite. Run `devtools::check()`; confirm 0/0/0.

## Work log

- 2026-07-13: created by /milestone-plan. Promotes free-scaling candidate (1); lineage D-009 finding (5) + M18-D3. Gate: analytic-CI coverage only, emit T_free (defer decision), live SE cross-check as 2nd oracle, M19 blocks M7. Machinery pre-exists — runs + calibrates, does not build SEs.
- 2026-07-13: AMENDED (correctness, no gate) — AC1/Scope/T1 "non-unit σ truths" → circumplex-correlation truths (σ_pop=1). Engine only ever fits a unit-diagonal R (`cpm_fit.R:1451-1460`,`:1490`); no covariance-input path, so σ is a free nuisance = 1 only at perfect fit (`m4-browne-design.md:259-266`). Deliverable/acceptance unchanged.
- 2026-07-13: T1 done — stage 3 added to `devel/m4-coverage-oracle.R` (`CPM_COV_FREE_ONLY=1`, no diag re-run); draws X~N(0,P0) via chol, fits `scaling="free"`, scores θ/ζ/β Wald coverage + T_free. Smoke reproduced the boundary under-coverage + small-N SE fragility.
- 2026-07-13: T2 done — full 500-rep run (3.5 min, seeded) → `devel/m19-free-coverage-results.rds`. Coverage tracks the diag family (σ̂≈1); interior in-band at N=2000, boundary near N=50000; NEW: σ-Hessian singular in 55–58% of N=250 fits, ~0% at N≥2000.
- 2026-07-13: T3+T4 done — recorded the coverage subsection in `cairn/DESIGN.md` (table + what-it-decides) + validation-battery cite. Decision: diag N-thresholds are the correct, now-validated free thresholds; retire M18-D3 → D-010.
- 2026-07-13: T5+T6 done — merged free into the shared N-conditional caution in `R/cpm_oop.R` (D-010; M18-D3 placeholder removed) + σ² note; updated `cpm_fit.R` roxygen/constant comment. Tests: free caution-wording (closes an M18 gap) + coverage smoke + parametric-bootstrap SE cross-check; O-M19-cov/O-M19-se registered. D-010 appended. Suite 422, 0 fail/0 error.
- 2026-07-13: AMENDED (gated, minor) — AC4 "a snapshot pins the caution wording" → "a test pins" it, matching the repo's diag-caution convention (`test-cpm_api.R` pins via `expect_match`, not snapshot, because analytic CI endpoints are BLAS-sensitive). Deliverable unchanged (wording is pinned by a test).
- 2026-07-13: all tasks done; `devtools::check(--no-manual)` clean 0/0/0. Status → review.

## Decisions

- **M19-D1 (parametric bootstrap for the SE cross-check):** the 2nd (live)
  oracle type uses a parametric bootstrap (refit free on data drawn from the
  fitted Σ̂), not OpenMx SEs — OpenMx's (θ_raw, w, s) parameterization would need
  a delta-method map through its own Jacobian to reach natural θ/ζ/β
  (error-prone, version-dependent). The bootstrap is independent of the
  FD-Hessian, base-R, and deterministic under seeding. Part of D-010's record.

## Review
