# M17: CIRCUM free-scaling — Fable-reviewed design decision + spec (done)

- **Status:** done · **PR:** #41 (merged 2026-07-13) · design-gate milestone, docs/design only (no package code).
- **Goal:** decide go/no-go and, if go, spec a free-scaling covariance family (`Σ = D_σ P(γ) D_σ`) for `cpm_fit()`, so M18 builds without re-opening design questions.

**Outcome: GO** (D-009). Independent Fable review (RB04→RR04, archived under `cairn/reviews/archive/`) attested the extension is statistically tame:
- σ = e^{s}, all p free, **no identification pin** (map injective, F coercive in each σ_i); σ̂=1 only at perfect fit — finite-N ML preserves `diag(Σ̂⁻¹R)=1`, the precise content of the B6 refutation of the old §3.2 σ̂=1 claim.
- Free-family gradient derived + FD-verified (worst err 3.6e-9): `∂F/∂s_i = 2(1−(Σ⁻¹R)_ii)`; γ blocks = design §3.4 with `A → Ã = D_σ A D_σ` (A from `Σ⁻¹`, not `P⁻¹`).
- **df unchanged** (covariance moment count `p(p+1)/2`); **no analytic σ CIs**; bootstrap stays default; a free-family coverage-oracle extension is a mandatory pre-ship gate.
- σ invariant under rotation+reflection (canonicalization untouched); 5 layout/guard pins (s block before β).

**Deliverables:** build-ready spec `devel/circum-free-scaling-spec.md` (names the OpenMx free-scaling oracle `test-cpm_oracles.R:329` + Grassi et al. 2010 App. A targets with tolerances); D-009; a §11 pointer in `m4-browne-design.md`. Review found + fixed one spec slip (κ(Σ) → Hessian condition number).

**M18 carries:** implement per spec; rewrite design §3.2 (scale-invariance→χ² validity is the *free* family's, not the diag family's); close the pending 2nd human re-read of the Grassi App. A transcription before its tightened tolerances land. Deferred (not v2.0.0-committed): bootstrap σ CIs; a T_diag-vs-T_free calibration → possible future default change.
