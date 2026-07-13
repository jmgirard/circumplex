<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M18: CIRCUM free-scaling — implementation + oracle validation

- **Status:** review
- **Priority:** high
- **Depends on:** M17
- **Principles touched:** — (no formal IP/GP ids yet; works under DESIGN.md "Statistical conventions" and "CPM confidence intervals: measured coverage")
- **Branch/PR:** m18-circum-free-scaling / —

## Goal

Implement the free-scaling covariance estimation family in `cpm_fit()` per the
M17 spec and validate it against the OpenMx free-scaling oracle and the
published CIRCUM/CircE solution.

## Scope

**In:**
- A `free_scaling = TRUE` code path through `cpm_fit()` per the M17 spec: σ
  parameterization/packing, the covariance discrepancy, the free-family analytic
  gradient (diagonal terms included), identification/canonicalization of σ,
  df/χ²/CI treatment, and the output surface (report σ̂; print/summary).
- Validation against two oracles: the OpenMx free-scaling fit (already
  parameterized in `tests/testthat/test-cpm_oracles.R`) and the published
  Grassi et al. (2010, Appendix A) CircE vocational-interest solution.
- The four boundary invariant classes exercised on the free-scaling path where
  they apply (peak at 0/360; flat/zero-variance), per CLAUDE.md danger-zone
  requirements and `cairn/boundary-coverage.md`.

**Out:**
- Any analytic-CI trustworthiness guarantee the M17 spec does *not* grant
  (bootstrap remains the fallback CI path per `devel/m4-browne-design.md` §3.2,
  §5.2) — scope follows the spec.
- Multi-group free-scaling, OLS/GLS/ADF, polychoric input, correlated
  uniquenesses (documented, not promised; `devel/m4-browne-design.md` §8).
- The RcppArmadillo port (Phase 2, profiling-gated; §8) — R stays the oracle.

## Acceptance criteria

- [ ] `cpm_fit(..., scaling = "free")` fits the covariance family and returns
      σ̂; a regression test in `tests/testthat/test-cpm_fit.R` exercises the path
      end-to-end. (AC1 wording amended 2026-07-13: `scaling = c("unit","free")`
      per spec §7, superseding the plan's `free_scaling = TRUE`.)
- [ ] Reproduces the OpenMx free-scaling oracle to the tolerance fixed by the
      M17 spec, in `tests/testthat/test-cpm_oracles.R` (the free-scaling OpenMx
      transcription already present there is the oracle).
- [ ] Reproduces the published Grassi et al. (2010, Appendix A) CircE solution
      to its printed precision (ζ/β to 4 decimals, angles to ~0.01°, F̂ per the
      §11 nesting/allowance protocol) — test in `test-cpm_oracles.R`.
      Source: Grassi et al. (2010), Appendix A.
- [ ] The free-family analytic gradient agrees with finite differences to the
      spec tolerance at ≥ 50 random feasible points, and the boundary suite
      (peak at 0/360; flat) passes on the free-scaling path — tests in
      `test-cpm_fit.R` / `test-cpm_boundary.R`.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes) and
      `devtools::test()` green (run with `NOT_CRAN=true`, per M16 lesson).

## Coverage

- AC1 → T2, T3
- AC2 → T4
- AC3 → T4
- AC4 → T1, T5
- AC5 → T6

## Tasks

- [x] **T1** — Implement the free-family discrepancy `F(S, Σ)` and its analytic
      gradient (diagonal terms per the M17 spec) as internal functions;
      test-first against finite differences at ≥ 50 random feasible points.
      (RB tripwire: no-oracle for the gradient derivation until the FD check and
      the OpenMx oracle both pass — treat FD agreement as the first gate.)
      Done 2026-07-13: `Ã = D_σ A D_σ` weight + `∂F/∂s = 2(1−(Σ⁻¹R)_ii)`; FD
      err 2.4e-9, σ=1 legacy identity exact 0. Tests pending (T4/below).
- [x] **T2** — Add the σ parameterization/packing and the `scaling = "free"`
      argument plumbing through `cpm_fit()` (spec/pack/unpack/starts), holding
      the existing correlation path bit-identical when `scaling = "unit"`.
      Done 2026-07-13: `[angle][u][s][v]` layout, `n_moments`-driven df, s⁰=0
      starts; full existing suite green (unit path unchanged).
- [x] **T3** — Wire the fit path: canonicalization/identification of σ, df/χ²,
      CI treatment per spec, and the output surface (σ̂ in the returned object;
      print/summary). Regression test the end-to-end fit. Done 2026-07-13:
      `VarRatio` column (free only), free-path analytic caution (M18-D3), σ
      pathology note, σ-aware `cpm_sim_root`. End-to-end free fit reproduces
      published fit indices (χ²/RMSEA/CFI/TLI). Formal regression tests below.
- [x] **T4** — Validation tests: OpenMx free-scaling oracle to spec tolerance;
      Grassi et al. (2010) published CircE targets to printed precision.
      Done 2026-07-13: frozen oracle (App. A angles/ζ/β/σ²/F̂/χ²/RMSEA/RMSEA-CI/
      CFI/TLI/SRMR-converted to printed precision) + live OpenMx cross-check
      (θ/ζ/β/σ² agree; σ² offset = exactly (N−1)/N, OpenMx's ML rescale absorbed
      into σ — confirms equivariance) + Table 2 model-3c fixed-grid row. Plus
      engine invariants in test-cpm_fit.R: FD gradient ≥50 pts, σ=1 legacy
      identity, stationarity, exact recovery σ̂=1, rescale-equivariance, nesting.
- [x] **T5** — Boundary suite on the free-scaling path (peak at 0/360; flat);
      update `cairn/boundary-coverage.md` with the new cells. Done 2026-07-13:
      new `test-cpm_boundary.R` (class A pole recovery incl a genuine σ≠1
      pattern; class D singular/zero-variance/near-flat refusal fail-closed).
      B is the shared circular-quantile engine (σ orthogonal to angles); C is
      not a CPM estimand. boundary-coverage.md row + audit note added.
- [x] **T6** — `devtools::document()` (if the API surface changed), full
      `devtools::check()` + `devtools::test()` (`NOT_CRAN=true`); NEWS entry for
      the new `scaling` argument. Done 2026-07-13: docs regenerated (`scaling`
      param in cpm_fit.Rd); NEWS entry (scaling="free"/VarRatio; CIRCUM/CircE
      now reproducible); design-doc debt cleared (m4-browne-design.md §3.2
      correction + §6.3 SRMR conversion). `check()` clean: 0 errors / 0 warnings
      / 0 notes (`NOT_CRAN=true`); full `test()` green.

## Work log

- 2026-07-12: created by /milestone-plan. The build half of the CIRCUM
  free-scaling split (design gate = M17). Depends on M17's ratified spec and
  go decision — if M17 decides no-go, this milestone is retired unbuilt. In
  v2.0.0 scope per D-008.
- 2026-07-13: status → in-progress; branch m18-circum-free-scaling cut from
  synced master. Question gate settled (see Decisions M18-D1..D3).
- 2026-07-13 (amendment, minor): AC1 wording `free_scaling = TRUE` →
  `scaling = "free"` per spec §7 (M18-D1).
- 2026-07-13: all tasks done; status → review. Free-scaling covariance family
  reproduces published CircE fit to printed precision (F̂/χ²/RMSEA/CFI/TLI/σ²);
  FD gradient err 2.4e-9; check clean 0/0/0 (NOT_CRAN); full test() green.

## Decisions

- **M18-D1 (API name):** the free-scaling flag is `scaling = c("unit","free")`,
  default `"unit"` (bit-identical to today), orthogonal to `model` — spec §7's
  preferred form over the plan's boolean `free_scaling`. Jeff, 2026-07-13.
- **M18-D2 (σ̂² surface):** report σ̂² (reproduced/input variance ratios) as a
  `VarRatio` column in `results`, populated/printed **only** on the free path
  (identically 1 under unit → omitted), no CI. Rationale: unit-mode σ²≡1 is a
  fixed constraint, not an estimate; column presence is the honest "σ estimated"
  signal. Jeff leaned toward always-present for assumption transparency — a
  trivial pre-review flip if preferred. 2026-07-13.
- **M18-D3 (free-path analytic caution):** on `scaling="free"` + analytic CIs,
  `summary()` prints an **unconditional** caution that free-family Wald CIs for
  θ/ζ/β are not yet coverage-validated (the free-family coverage oracle is a
  deferred pre-ship gate, out of M18 scope per spec §4/§6), never reusing the
  diag-calibrated N=2000/50000 thresholds as a validated trust boundary. σ has
  no analytic CI ever (spec §4). Jeff, 2026-07-13.

## Review
