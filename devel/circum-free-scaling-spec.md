# CIRCUM free-scaling family — implementation spec (M18)

**Status:** build-ready (M17 output). **Decision:** GO — see `cairn/DECISIONS.md`
D-009. **Derivation source of record:** `cairn/reviews/archive/RR04-circum-free-scaling.md`
(Fable, 2026-07-12; gradient FD-verified, worst mixed error 3.6e-9). This spec
distills RR04 into a build document; where it says "RR04 §n" the full derivation
lives there. Base design: `devel/m4-browne-design.md` (§ references below).
Published targets: Grassi, Luccio & Di Blas (2010), *Behavior Research Methods*
42(1), 55–73, Appendix A / Tables 2–3 (transcribed in
`tests/testthat/helper-cpm-oracles.R`).

The free-scaling family adds `p` variance scale factors so `cpm_fit()` can
**exactly reproduce published CIRCUM/CircE output**, which the shipped
diag-constrained (correlation-structure) family provably cannot (B6 battery,
`m4-browne-design.md` §11). It is a flag **orthogonal** to variants A–D — the
8 combinations `{unit, free} × {A, B, C, D}` are all valid.

---

## 1. The model

Fit the **covariance** structure

    Σ(γ, σ) = D_σ · P(γ) · D_σ,   D_σ = diag(σ_1,…,σ_p),  σ_i > 0,

where `P(γ)` is the existing model-implied **correlation** matrix
(`P_ij = ζ_i ζ_j ρ(θ_i − θ_j)`, `diag P = 1` identically; `m4-browne-design.md`
§1.3). The current family is the special case `σ ≡ 1`, and is strictly nested
in this one. `σ_i² = Σ_ii` is CIRCUM's "ratio of reproduced to input variance"
(Grassi et al. 2010, App. A `var_ratios`; published range .963–1.042).

**Input path:** keep the current **unit-diagonal `cormat`** requirement even in
free mode. The family is closed under diagonal rescaling, so fitting `R` gives
identical inference to fitting any covariance `S = DRD` with `σ̂ → Dσ̂` and
`θ̂, ζ̂, β̂, F̂` invariant (RR04 §4i). Accepting covariance input is **out of
scope** (RR04 rec 12): statistically redundant, widens input validation.

## 2. Parameterization and identification

**Unconstrained map — add one row to the `m4-browne-design.md` §3.3 table:**

| Natural | Constraint | Unconstrained | Map | Jacobian |
|---|---|---|---|---|
| σ_i, i = 1…p | σ_i > 0 | s_i ∈ ℝ | σ_i = e^{s_i} | ∂σ_i/∂s_i = σ_i |

Log map: strictly-feasible iterates (no box constraints, `nlminb`
unconstrained), ≈ identity in the realistic neighborhood (published ratios put
`s` within ±0.021 of 0), symmetric in the multiplicative parameter, cleanest
chained gradient (RR04 §2).

**Starts:** `s⁰ = 0` (σ = 1) for **every** start — mirror and jittered starts
included; **do not jitter s** (preserves the no-RNG determinism contract,
`m4-browne-design.md` §3.5). θ⁰/ζ⁰/β⁰ start logic unchanged. **No boundary
polish analog** for σ (interior optimum by coercivity; no softmax tail stall).

**Identification: all p σ free, no pin** (RR04 §2, rec 11). The decomposition
`Σ = D_σ P D_σ` with `diag P = 1` is unique (`σ_i = √Σ_ii`; the map is
injective and `F` is coercive in each σ_i), so `q → q_corr + p` with **no
offsetting constraint**. A pin would break CIRCUM comparability and the
variance-ratio interpretation. σ̂ = 1 holds *only* at perfect fit; at finite N
the ML projection preserves `diag(Σ̂⁻¹R) = 1`, **not** `diag Σ̂ = 1` — this is
the exact mathematical content of the B6 refutation of the old §3.2 claim.

**Conditioning caveat (not a failure):** σ_i and ζ_i are correlated coordinates,
so the **Hessian** condition number (SE/CI reliability, `avar = (2/n)H⁻¹`)
worsens somewhat vs the diag fit, most at Heywood-adjacent fits (ζ̂_i → 1). The
existing §2.5 Hessian condition-number warning covers it; the coverage-oracle
extension (§5) must include a Heywood-prone cell. (Distinct from κ(Σ), the
model-matrix conditioning that governs `solve(Σ)`/FD accuracy in §6's gradient
test — RR04 §2.)

## 3. Discrepancy and the analytic gradient

**Discrepancy** (unchanged form; `cpm_discrepancy()` already accepts a non-unit
diagonal model matrix — proven by the published-F̂ identity test):

    F(R, Σ) = ln|Σ| − ln|R| + tr(R Σ⁻¹) − p.

**Core differential** (RR04 §3): `dF = tr(A dΣ)` with the **essential
substitution `Σ⁻¹` for `P⁻¹`** —

    A := Σ⁻¹ − Σ⁻¹ R Σ⁻¹   (symmetric).

An implementation that keeps `Pinv` in `A` is wrong in **every** gradient
component. With `σ ≠ 1`, `Σ⁻¹` and `P⁻¹` differ everywhere, not just on the
diagonal.

**σ block (new).** From `Σ_jk = σ_j σ_k P_jk` (RR04 eq 2–3):

    ∂F/∂σ_i = 2 Σ_j A_ij σ_j P_ij   (INCLUDING j = i; diagonal term 2 A_ii σ_i)
    ∂F/∂s_i = σ_i ∂F/∂σ_i = 2 (A Σ)_ii = 2 (1 − (Σ⁻¹ R)_ii).     ← closed form

**γ blocks (corrected §3.4).** Only `P` depends on (θ, ζ, β), so `dΣ = D_σ dP D_σ`
and `dF = tr(Ã dP)` with **Ã := D_σ A D_σ** (`Ã_ij = σ_i σ_j A_ij`). The §3.4
"only off-diagonal ∂P_ij enter" statement is **about dP and remains true**:
`diag P = 1` identically in the (θ,ζ,β) parameterization, so `diag dP = 0`. The
moving diagonal of Σ lives **entirely in the σ block**. The only γ-block change
is the weight matrix `A → Ã`:

    ∂F/∂θ_i = 2 Σ_{j≠i} Ã_ij ζ_i ζ_j ρ'(δ_ij)
    ∂F/∂ζ_i = 2 Σ_{j≠i} Ã_ij ζ_j ρ(δ_ij)
    ∂F/∂β_k =   Σ_{i≠j} Ã_ij ζ_i ζ_j cos(k δ_ij)

with `δ_ij = θ_i − θ_j`, `ρ'(δ) = −Σ_k k β_k sin(kδ)`.

**β-diagonal subtlety — do NOT "fix" it** (RR04 §3): excluding the diagonal from
`∂F/∂β_k` is correct. If one added the diagonal term (constant in k, `Σ_i Ã_ii
ζ_i²`), the softmax Jacobian `J = diag(β) − ββᵀ` annihilates constant vectors,
so the chained `∂F/∂v` is identical. Adding the diagonal to the β block
*without* the simplex-tangent chain would be the actual bug.

**Chain rule (§3.3 Jacobians unchanged; one row added):**

    ∂F/∂θ*_i = ∂F/∂θ_i             (identity, free positions only)
    ∂F/∂u_i  = ζ_i(1 − ζ_i) ∂F/∂ζ_i (logit)
    ∂F/∂v_l  = Σ_k (∂F/∂β_k) β_k(δ_kl − β_l)  (softmax)
    ∂F/∂s_i  = 2(1 − (Σ⁻¹R)_ii)     (log map, already chained)

**Vectorized recipe** (mirrors `cpm_gradient()`; RR04 §3):

    Sigma <- (sigma %o% sigma) * P;  Sinv <- solve(Sigma)
    A  <- Sinv - Sinv %*% R %*% Sinv;  A <- (A + t(A))/2
    At <- (sigma %o% sigma) * A                       # A-tilde
    B  <- At * (zeta %o% zeta); diag(B) <- 0
    dF_dtheta <- 2 * rowSums(B * Rhod)                # from At
    ARho <- At * Rho; diag(ARho) <- 0
    dF_dzeta  <- 2 * as.numeric(ARho %*% zeta)        # from At
    dF_dbeta_k <- sum(B * cos(k * Delta))             # from At
    dF_ds     <- 2 * (1 - rowSums(Sinv * R))          # NEW = 2(1 - diag(Sinv R))

**Two converged-fit invariants** (free tests): at any accepted optimum
`diag(Σ̂⁻¹R) = 1_p` exactly (⇒ `tr(Σ̂⁻¹R) = p`, so `F̂ = ln|Σ̂| − ln|R|`).

## 4. df / χ² / CI treatment

**df UNCHANGED** (RR04 §4i — corrects the RB's "df shrinks" premise). The free
family is a covariance structure fitting `p(p+1)/2` moments with `q_corr + p`
parameters:

    df_free = p(p+1)/2 − (q_corr + p) = p(p−1)/2 − q_corr = df_diag.

Confirmed against the fixture: p = 7, m = 1 → df 7 either way (`app$df = 7`);
`T = 174 × 0.089815 = 15.628 → published 15.63`; `RMSEA = √(.089815/7 − 1/174)
= .0842 → published .084` (Grassi et al. 2010, App. A).

**Implementation trap (flag loudly):** `cpm_spec()` computes `df = p(p−1)/2 − q`.
Naively adding p to `q` there gives `df − p` (wrong). Either switch the moment
count to `p(p+1)/2` when σ is free, **or** exclude the σ block from the `q` used
in that formula while including it in the `q` used for AIC/BIC penalties and the
Hessian dimension. **Unit test:** `df_free == df_diag` on the same
(p, m, variant).

**Confidence intervals:**
- **Bootstrap remains the shipped raw-data default** (machinery is
  family-agnostic: resample → refit).
- **Analytic Wald CIs for θ, ζ, β** offered on the `cormat` path — the
  correlation-structure invariance argument (Cudeck 1989; Browne 1982) holds
  **exactly** for the free family (it *is* Browne's device; θ/ζ/β are scale-free)
  and is in fact *stronger* than for the diag family. **Gated (a real
  pre-ship gate, not a nicety):** extend the coverage oracle with free-family
  cells and re-derive (or explicitly re-affirm with justification) the
  `summary()` caution constants (N = 2000/50000 thresholds, boundary markers),
  which were calibrated on the diag family and **must not be silently reused**.
- **No analytic CIs for σ, ever, on correlation input** (RR04 §4ii, rec 5): σ is
  not scale-free and the unit-diagonal "moments" are degenerate (fixed at 1),
  so the naive avar block describes variation that does not exist. Report `σ̂²`
  as a **variance-ratio diagnostic column, uncertainty-free** (CIRCUM
  convention). Bootstrap σ CIs on the raw-data path are valid but not needed for
  compatibility → deferred (§6).
- Bootstrap is **not mandated** for the free family (the cormat path has no
  resampling alternative; the asymptotic justification is at least as good as
  what already ships).

**Fit indices:**
- **T = n·F̂ (n = N−1), df, RMSEA, RMSEA-CI, CFI/TLI, null model:** all unchanged
  (RR04 §4iii). Null is Σ₀ = D (independence, free variances); on unit-diagonal
  R this gives `F₀ = −ln|R|`, `df₀ = p(p−1)/2` — numerically identical to the
  current convention (fixture `null_chisq 747.663`, `null_df 21`). Reuse
  `cpm_rmsea_ci()` as-is (do not re-implement from §5.3 prose, whose lower-guard
  inequality is stated backwards; RR04 beyond-brief 4).
- **SRMR — pin the convention** (RR04 §4iii, rec 6): diagonal residuals
  `R − Σ̂` are **no longer identically zero** in the free family (`1 − σ̂_i²`),
  so off-diagonal-vs-diagonal-inclusive now changes the statistic beyond a fixed
  factor. **Decision:** keep the package-wide **off-diagonal `p(p−1)/2`**
  convention for both families (documented). Encode the *exact* conversion in
  the oracle (not an allowance):
  `SRMR_CircE² = [ (p(p−1)/2)·SRMR_ours² + Σ_i (1 − σ̂_i²)² ] / (p(p+1)/2)`.
  Update `m4-browne-design.md` §6.3 checklist item 6.
- **AIC/BIC:** penalize with the full `q' = q_corr + p`, but **restrict
  comparisons to within a scaling family** (across families the diagonal moments
  are degenerate on correlation input, so cross-family ΔAIC is not
  interpretable). Document as an extension of the existing "comparable only
  across models on identical data" restriction. **Do not present ΔT as a
  calibrated σ=1 test** (naive χ²_p reference is wrong; report descriptively or
  not at all).

## 5. Canonicalization / identification interaction

**σ is invariant under both existing symmetries; canonicalization is
unaffected** (RR04 §5). Rotation and reflection act on angles only, leaving
every `|δ_ij|`, hence P, hence Σ, unchanged with σ untouched — mirror optima
have identical F̂ **and** σ̂; the §2.3 closest-to-theory rule applies verbatim
on the angle block. No new sign/permutation redundancy (`exp` makes σ > 0
structural; each σ_i is anchored to variable i).

**Mechanical pins the implementer must honor (each a silent-bug site):**

1. **Parameter-vector layout `[angles][u][s][v]`** — s block **between ζ and β**
   so β stays trailing. `cpm_spec_reduce` (boundary polish) rebuilds `i_beta` as
   the last block and shrinks q from the tail; appending s *after* β would shift
   s indices on every polish refit. With s ahead of β, `cpm_spec_reduce` needs
   only its existing `i_beta` rebuild plus a static `i_sigma`.
2. **Reflection in unconstrained coords** (mirror start, `cpm_canonicalize`,
   bootstrap per-replicate mirror guard) touches only `i_angle`; σ/ζ/β pass
   through. s plays no role in mirror detection.
3. **Multimodality detection** must **include the s block** in the
   parameter-distance comparison — two optima differing only in σ is a genuine
   non-identification signature the angle-only comparison would miss.
4. **Starts:** s⁰ = 0 in all starts; jitter applies to angles only.
5. **Flags:** ζ̂ > 0.995 flag unchanged; no σ boundary (interior by
   coercivity). Optional data-pathology note (not a boundary flag) if any
   σ̂² leaves ~[0.5, 2] on unit-diagonal input. 0°/360° handling untouched.

## 6. Validation plan

**≥2 independent oracle types for point estimates (bar met):**

1. **Published program output (frozen)** — Grassi et al. (2010) App. A /
   Tables 2–3, already transcribed in `helper-cpm-oracles.R`. The payoff of the
   free family: compare at **same-model §6.3 tolerances**, retiring the B6
   model-difference allowances for its own comparisons — angles to published
   precision, ζ/β within 0.005, F̂ within optimizer-tail tolerance. Includes the
   Table 2 **model 3c** (fixed grid + free scaling) row as a new-cell oracle.
2. **Independent cross-implementation (live)** — the OpenMx free-scaling oracle
   `tests/testthat/test-cpm_oracles.R:329` (`cpm_mx_model(..., free_scaling =
   TRUE)`), **already green** at publication precision (ζ/β to 4 dp, angles
   ~0.01°). This is the named M18 anchor.

**Required internal oracle additions (no new transcription):**

- **Extended FD gradient test** (invariant/closed-form): ≥20 random feasible
  points, randomize θ (incl. one 0/360-pole config and one near-equal-angles),
  ζ∈(.3,.95) via u, β interior via v~U(−1,1), **s~U(−0.35,0.35)** (σ∈[.70,1.42]),
  random PD R incl. **a few non-unit-diagonal inputs** and **one reduced spec**
  (harmonic polished out). Criterion (A-review F8 mixed): `|g_a − g_fd| ≤ 1e-7 ·
  max(1, |g_fd|)`, central diff, step 1e-6. **Redraw (documented) any point with
  κ(Σ) > ~1e6** (there the FD, not the gradient, degrades).
- **σ = 1 legacy-gradient identity** (invariant): at s = 0, γ-block gradient ==
  legacy `cpm_gradient()` exactly — kills the "used A not Ã" and "used P⁻¹ not
  Σ⁻¹" error classes in one assertion.
- **Stationarity invariant** `diag(Σ̂⁻¹R) = 1` at accepted optima.
- **Exact-recovery / population anchor:** input exactly in the correlation
  family ⇒ F̂ ≈ 0 and **σ̂ = 1 to 1e-6**.
- **Rescale-equivariance (engine level):** fit `D R D` ⇒ `σ̂ → D σ̂`,
  θ̂/ζ̂/β̂/F̂ invariant to ~1e-10 — the sharpest single test of the whole
  construction; no diag-constrained analog.
- **Nesting inequality** `F̂_free ≤ F̂_diag + 1e-8` on every fixture.

**Cautions:** the model-3c fixed-grid fixture has near-tied ζ basins
(`test-cpm_oracles.R:353–355`) — the multimodality flag is expected to fire;
pin β and F̂, not per-variable ζ. **The coverage-oracle extension is a separate,
mandatory gate before any CI-trust statement** (§4) — no published or
cross-implementation oracle can answer coverage (matching CIRCUM's CIs validates
fidelity, not coverage — the F1 lesson); extend `devel/m4-coverage-oracle.R`
with free-family cells (≥ boundary/interior β at N ∈ {250, 1000} plus one
analytic-ladder spot check, incl. a Heywood-prone cell). **Close the pending
second human re-read of the App. A transcription** (`helper-cpm-oracles.R`
header) before the tightened tolerances lean on those digits.

## 7. API surface (Jeff's call; statistical constraints fixed)

- A `scaling = c("unit", "free")` flag (or equivalent), **orthogonal to `model`**
  (variants A–D) → the 8 combinations. Exact name/spelling is Jeff's.
- Keep the unit-diagonal `cormat` input requirement in free mode (§1).
- Report `σ̂²` as a variance-ratio diagnostic column (no CIs).

## 8. Documentation debt (M18 doc time)

- **Rewrite `m4-browne-design.md` §3.2** to the corrected invariance statement:
  the χ²/Wald validity does **not** follow from scale invariance for the *diag*
  family (its true home is the free family); population anchor σ̂ = 1 iff perfect
  fit; finite-N projection preserves `diag(Σ̂⁻¹R) = 1`. So the design doc no
  longer contains a claim its own change log refutes (RR04 beyond-brief 1/3).
- Update §6.3 checklist item 6 (SRMR conversion) and DESIGN.md's CPM-CI section
  once free-family coverage is measured.
- At ship time, DESIGN.md reproducibility contract / `ssm_analyze()` roxygen
  update per `m4-browne-design.md` §8 (unchanged by this spec).

## 9. Deferred / consider (NOT in M18 unless cheap)

- **Bootstrap σ̂ CIs** on the raw-data path — statistically valid, not needed for
  CIRCUM compatibility (RR04 rec 10).
- **T_diag-vs-T_free calibration cells** in the coverage-oracle runs, to write
  the vignette's which-family-for-inference guidance and inform a *future*
  major-version default change. The free family may be the statistically
  preferable inference default — **a question to measure, not decide here**
  (RR04 beyond-brief 1, rec 9).
- **Covariance-matrix input** — rejected for this milestone (RR04 rec 12);
  revisit as a separate candidate only if users ask.
