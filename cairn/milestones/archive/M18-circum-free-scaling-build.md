# M18: CIRCUM free-scaling — implementation + oracle validation (done 2026-07-13)

**Goal:** build the free-scaling covariance family in `cpm_fit()` per the M17
spec (`devel/circum-free-scaling-spec.md`, GO per D-009/RR04) and validate it.

**Outcome:** `cpm_fit(scaling = c("unit","free"))` fits Browne's covariance
structure Σ = D_σ P D_σ with `p` free variance scales (σ_i = e^{s_i}), so the
package reproduces published CIRCUM/CircE output exactly (Grassi App. A:
F̂/χ²/RMSEA/CFI/TLI and σ̂² to printed precision). Gradient uses the Σ⁻¹-for-P⁻¹
substitution with Ã = D_σ A D_σ on the γ blocks and ∂F/∂s = 2(1−(Σ⁻¹R)_ii); FD
err 2.4e-9, σ=1 legacy identity exact. df unchanged (free fits p(p+1)/2
moments); default `unit` path bit-identical; σ̂² in a `VarRatio` column (no CI).
Validated by 2 oracle types (published-frozen + live OpenMx) + invariants
(stationarity, exact recovery, rescale-equivariance, nesting) + free-path
boundary suite.

**Key decisions:** M18-D1 API name `scaling`; M18-D2 `VarRatio` free-mode only;
M18-D3 free+analytic CIs carry an unconditional not-yet-coverage-validated
caution (free-family coverage oracle deferred, a pre-ship gate — out of scope).

**Review:** 3-lens fan-out found 1 defect (scored 90), fixed on-branch:
free-scaled `Phat` (=Σ) passed to `ssm_ci_accuracy(cpm=)` corrupted the
correlation population → now refused with a guard + test. check 0/0/0, CI green.
**PR:** #42. Deferred (not v2.0.0): free-family coverage oracle, bootstrap σ CIs,
T_diag-vs-T_free calibration, covariance-matrix input.
