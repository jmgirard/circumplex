# M4 design: native estimation of Browne's (1992) circular stochastic process model

**Status:** design (Brief A of the 2026-07 Fable window). Not implementation.
**Author:** Fable, 2026-07-03. Backend decision made with Jeff (see §3.6).
**Awaiting:** Brief A-review (fresh-session adversarial review) before implementation.

This is the CircE replacement — the anchor feature of ROADMAP Milestone 4.
CircE (Grassi, Luccio, & Di Blas, 2010) is archived on CRAN; no R package
currently estimates Browne's model. The design below is written to hand
straight to Opus for test-first implementation.

Package conventions that bind everything here (CLAUDE.md / DESIGN.md):
degrees [0, 360) in the user API with LM = 360; radians internally via
`circumplex_degree`/`circumplex_radian`; any change touching angles gets
boundary tests at 0°/360°; statistical correctness outranks everything.

> ⚠️ **Oracle rule (repeated in §6 because it is the most important sentence
> in this document):** every expected value used for validation must be
> transcribed from a *published* CIRCUM/CircE source, independently, at
> implementation time. No local file — explicitly including
> `devel/g2xx1.txt` — may ever serve as a source of expected values. This
> document deliberately contains **no numeric oracle values**; the tables in
> §6 are templates to be filled from the cited papers directly.

---

## 1. The model

### 1.1 Common-score decomposition

Each of `p` manifest variables (scales or items) `x_i` is a point on a circle
at angle `θ_i`, decomposed as

    x_i = ζ_i · c(θ_i) + sqrt(1 − ζ_i²) · e_i ,        i = 1, …, p

where

- `c(θ)` is a zero-mean, unit-variance, stationary stochastic process on the
  circle: `Corr(c(θ_i), c(θ_j)) = ρ(θ_i − θ_j)` depends only on the angular
  separation;
- `ζ_i ∈ (0, 1]` is the **communality index** — the correlation between the
  manifest variable and its common score. `ζ_i²` is the communality (the
  proportion of variance the circle explains). This is the quantity CIRCUM
  and CircE both report as "communality index"; report `ζ_i`, not `ζ_i²`, and
  label it exactly that way to avoid a silent squared/unsquared mismatch when
  validating (§6.5);
- the unique parts `e_i` are mutually uncorrelated and uncorrelated with the
  process.

### 1.2 Fourier correlation function

The process correlation function is a truncated Fourier cosine series:

    ρ(δ) = Σ_{k=0}^{m} β_k · cos(k·δ) ,   with  β_k ≥ 0  and  Σ_{k=0}^{m} β_k = 1.

The two constraints are not conveniences; they are the model:

- `Σ β_k = 1` enforces `ρ(0) = 1` (a correlation function);
- `β_k ≥ 0` is the Herglotz/Bochner condition: a stationary function on the
  circle is a valid (non-negative definite) correlation function **iff** all
  its Fourier coefficients are non-negative. Dropping the constraint can
  produce a `ρ(δ)` that is not any process's correlation function, and a
  non-PSD common part. Any implementation that "relaxes" it is wrong, however
  well it fits.

Note `ρ(δ)` may legitimately be negative (e.g., `m = 1`: `ρ(δ) = β₀ + β₁cos δ`
has minimum `2β₀ − 1 < 0` whenever `β₀ < ½`), so the model accommodates
bipolar circumplexes. `ρ` is even: `ρ(−δ) = ρ(δ)` — the source of the
reflection indeterminacy in §2.3.

### 1.3 Model-implied correlation matrix

    P_ij(γ) = ζ_i ζ_j ρ(θ_i − θ_j)   for i ≠ j;      P_ii = 1.

In matrix form, with `D_ζ = diag(ζ)` and `C_ij = ρ(θ_i − θ_j)`:

    P = D_ζ C D_ζ + (I − D_ζ²).

The diagonal is 1 **by construction** (`C_ii = ρ(0) = 1` cancels), so the
parameterization below never has to constrain the diagonal — this matters for
both the gradient (§3.4) and the correlation-vs-covariance asymptotics (§3.2).

**Low-rank / factor representation** (used for simulation, the SEM oracles,
and intuition). Expanding `cos(kδ_ij) = cos kθ_i cos kθ_j + sin kθ_i sin kθ_j`,
with vectors `c_k = cos(kθ)`, `s_k = sin(kθ)` (and `c₀ = 1`, `s₀ = 0`):

    C = β₀ 11ᵀ + Σ_{k=1}^{m} β_k (c_k c_kᵀ + s_k s_kᵀ)
    P = Λ Λᵀ + (I − D_ζ²),   where
    Λ = D_ζ [ √β₀·1 , √β₁·c₁ , √β₁·s₁ , … , √β_m·c_m , √β_m·s_m ]   (p × (2m+1)).

So the common part has rank ≤ 2m + 1, and the model is exactly a constrained
(2m+1)-factor model — which is why lavaan/OpenMx can express it (awkwardly)
and why they make good *oracles* (§6.4) but a poor engine (§3.6).

### 1.4 Model variants and degrees of freedom

Sample moments fitted: `p(p−1)/2` correlations. Free-parameter counts `q`:

| Variant | Angles | ζ | β | q | df = p(p−1)/2 − q |
|---|---|---|---|---|---|
| A. Quasi-circumplex (default) | p − 1 free (one reference fixed, §2.1) | p free | m free (§2.2) | 2p − 1 + m | e.g. p=8, m=3 → **10** |
| B. Constrained angles | 0 (fixed at theory, e.g. octants) | p free | m free | p + m | p=8, m=3 → 17 |
| C. Equal communality | p − 1 free | 1 | m free | p + m | p=8, m=3 → 17 |
| D. Circulant (B + C) | 0 | 1 | m free | 1 + m | p=8, m=3 → 24 |

The p=8, m=3 quasi-circumplex df of 10 is a standard published configuration
(octant instruments in the CIRCUM literature); recompute it as a unit test of
the df bookkeeping. A and B (and A and C) are nested, so χ² difference tests
of equal spacing / equal communality fall out for free; these nested tests
are the ROADMAP's "free vs constrained item angles" deliverable.

**Feasibility bounds** (validate at input):

- df ≥ 1 required (df = 0 fits perfectly and tests nothing; report but warn).
  For variant A with m = 1 this needs p ≥ 6 (p = 5 gives df = 0; p = 4 is
  over-parameterized). Variants B–D fit smaller p.
- `m ≤ floor((p − 1)/2)`. For equally spaced angles, frequencies k and p − k
  alias exactly (`cos(k·2πj/p)` is the same grid function), so higher m is
  never identified in variants B/D and is ill-conditioned in A. For p = 8
  this cap is 3 — which is also the sensible default `m` (see §7).

### 1.5 Choosing m

`m` is a model-selection choice, not a free parameter. Default `m = 3`
(the CIRCUM-literature convention for octant scales, and the cap for p = 8).
Provide nested-χ²/AIC/BIC comparisons across m = 1, 2, 3 (§5.3) rather than
automating the choice. Higher harmonics whose `β̂_k` hit the 0 boundary are
handled in §3.5.

---

## 2. Identification and the angular danger zone

### 2.1 Rotation

`F` depends on angles only through differences `θ_i − θ_j`, so the solution
is invariant to a common rotation: exactly one angle must be fixed. Fix the
**reference scale's** angle (user-choosable, default the first element of
`scales`) at its theoretical angle (default `octants()` semantics: degrees,
LM = 360). Rotation invariance also means: when validating against published
output whose reference convention differs (CIRCUM fixes its reference
variable's angle too, but possibly at a different value/variable), **compare
angular differences from the reference, never raw angles** (§6.5).

### 2.2 The sum constraint on β

`Σ β_k = 1` removes one β; with the softmax parameterization (§3.3) the free
count is exactly m and the constraint holds identically — never renormalize
after the fact.

### 2.3 Reflection

Because `ρ` is even, the map `θ_i → 2θ_ref − θ_i` (reflection about the
reference) leaves every `cos(k(θ_i − θ_j))` — hence `F` — invariant. Two
mirror-image solutions always exist. Handle it twice:

1. **At optimization:** run both starting orientations (`θ⁰` and its
   reflection) in the multi-start scheme (§3.5); they converge to the two
   mirror optima with identical `F̂`.
2. **At reporting (deterministic canonicalization):** reflect the solution,
   if necessary, so that the second scale in `scales` lies counterclockwise
   of the reference — i.e., `(θ̂₂ − θ̂_ref) mod 360° ∈ (0°, 180°)`. This
   matches the package's counterclockwise-positive convention and makes
   output reproducible. If `θ̂₂ − θ̂_ref` is within numerical tolerance of 0°
   or 180° (reflection undecidable by scale 2), fall through to the first
   subsequent scale that decides it; if none does, warn and report as-is.

### 2.4 Boundary/range behavior of angles (the 0°/360° danger zone)

The objective is **periodic and smooth in every θ_i** — angles enter only
through `cos(k·)` and `sin(k·)` — so optimization has *no* boundary: free
angles live on all of ℝ during optimization and are wrapped only at report
time via the existing degree machinery (`modu(·, 2π)` → `[0, 360)`), same as
the SSM displacement estimator. Consequences:

- Never clamp, wrap, or reflect θ *inside* the optimizer; that would create
  artificial non-smoothness at an arbitrary cut point. Wrap once, on output.
- An estimated angle at the 0/360 pole follows the DESIGN.md G2 convention:
  ≈0 and ≈360 are the same direction; tests at the pole accept either.
- Analytic angle CIs (`θ̂ ± z·SE`, §5.2) are constructed on the unwrapped
  branch and may cross 0/360; wrap the *interval* for display the same way
  displacement CIs are handled (contiguous interval, endpoints may print as
  e.g. `[352°, 8°]` only if rendered through the existing circular-interval
  display; otherwise report the unwrapped branch `[−8°, 8°]` with the
  estimate inside — mirror `quantile.circumplex_radian`'s
  center-unwrap-rewrap convention rather than inventing a new one).
- Bootstrap angle CIs (§5.2) reuse the existing circular quantile machinery
  (`quantile.circumplex_radian`: center on circular mean, unwrap, quantile,
  re-wrap) — do not write a second implementation.

Required boundary tests (§6.6) cover: true angle at the pole, CI straddling
0/360, mirror starts converging to identical `F̂`, and canonicalization
determinism.

### 2.5 Other identification hazards (diagnose, don't hide)

- **ζ_i → 1 (Heywood analog):** the logit parameterization (§3.3) makes 1
  unreachable, so the optimizer diverging in `u_i` is the symptom. Flag any
  `ζ̂_i > 0.995` as a boundary solution in the output object and `summary()`.
- **β_k → 0:** softmax makes 0 unreachable; §3.5 gives the polish step.
- **Empirical non-identification:** clustered angles (all items in one
  quadrant) or a dominant `β₀` (general factor swamping the circle) leave
  some parameters weakly determined. Diagnostic: report the condition number
  of the Hessian at the optimum; warn above ~1e8. Do not "fix" this
  numerically — it is information about the data.
- **Duplicate angles:** two items at the same angle are *allowed* by the
  model (they are parallel measures); no special handling, but the Hessian
  condition warning will fire if their ζ's trade off.

---

## 3. Discrepancy function, parameterization, and optimization

### 3.1 Discrepancy

Minimize the **maximum Wishart likelihood (ML) discrepancy** between the
sample correlation matrix `R` and `P(γ)`:

    F(γ) = ln|P(γ)| − ln|R| + tr(R·P(γ)⁻¹) − p .

`F ≥ 0`, with equality iff `P = R`. This is CIRCUM's estimator (and CircE's
default), which is decisive: the published validation targets in §6 are ML
estimates, so the engine must minimize the same function. OLS/GLS variants
are deliberately out of scope for the first cut (§8).

**Sample-size multiplier:** the test statistic is `T = n·F̂` with `n = N − 1`
(Wishart degrees of freedom). Record this convention prominently: an
`N` vs `N − 1` mismatch against a published χ² shows up as a constant factor
`N/(N−1)` and is the first thing the mismatch-diagnosis checklist (§6.5)
rules out.

### 3.2 Why ML-on-a-correlation-matrix is valid here (and where it isn't)

Fitting a *correlation* structure with the Wishart (covariance) likelihood is
the classic trap (Cudeck, 1989, *Psych Bull*): χ² and SEs are only valid if
the model is scale-invariant. This model is: `diag P(γ) = 1` identically, and
embedding `Σ = D_σ P(γ) D_σ` with free scale factors `σ` reproduces any
rescaling, with `σ̂ = 1` at the optimum when fitted to `R`. All parameters
(θ, ζ, β) are scale-free. So:

- point estimates and `T = n·F̂` are asymptotically valid;
- information-based SEs for θ, ζ, β are justified by the same invariance
  argument, **but this is exactly the kind of claim the validation must
  check** rather than trust: §6.3 gates the analytic CIs on reproducing
  published CIRCUM CIs. If they disagree beyond tolerance, implement the
  correlation-structure asymptotic covariance (Browne, 1984; Browne &
  Shapiro, 1986) instead of shipping the naive one. The bootstrap CI path
  (§5.2) sidesteps the issue entirely and is the fallback.

### 3.3 Unconstrained parameterization

Optimize over `γ* ∈ ℝ^q`, mapped to natural parameters:

| Natural | Constraint | Unconstrained | Map | Jacobian |
|---|---|---|---|---|
| θ_i, i ≠ ref | none (periodic) | θ_i itself | identity | 1 |
| θ_ref | fixed | — | — | — |
| ζ_i | (0, 1) | u_i | ζ_i = 1/(1 + e^(−u_i)) | ζ_i(1 − ζ_i) |
| β_0…β_m | β_k ≥ 0, Σ = 1 | v_1…v_m (v_0 ≡ 0) | β_k = e^(v_k) / Σ_j e^(v_j) | ∂β_k/∂v_l = β_k(δ_kl − β_l) |

Variants B–D drop the corresponding rows (fixed angles; single shared `u`).
All angle handling internal to the estimator is in **radians** via the
existing `as_radian(as_degree())` path; only the API boundary speaks degrees.

Why these maps: smooth, standard, and they keep every iterate strictly
feasible, so `P(γ)` is a proper correlation matrix at every step (given
§1.2's Herglotz condition) and `ln|P|` never sees a non-PD matrix from the
constraint side. (P can still approach singularity if ζ's → 1 with tightly
clustered angles; `nlminb` handles the resulting large-`F` region fine, and
the §2.5 flags report it.)

### 3.4 Analytic gradient

Let `A = P⁻¹ − P⁻¹ R P⁻¹` (symmetric). Then `dF = tr(A · dP)`, and since the
parameterization holds `diag P = 1` fixed, only off-diagonal `∂P_ij` enter.
With `δ_ij = θ_i − θ_j`, `ρ'(δ) = −Σ_k k·β_k·sin(kδ)`:

    ∂F/∂θ_i = 2 · Σ_{j≠i} A_ij · ζ_i ζ_j · ρ'(δ_ij)
    ∂F/∂ζ_i = 2 · Σ_{j≠i} A_ij · ζ_j · ρ(δ_ij)
    ∂F/∂β_k = Σ_{i≠j} A_ij · ζ_i ζ_j · cos(k·δ_ij)

then chain through the §3.3 Jacobians:

    ∂F/∂u_i = ζ_i(1 − ζ_i) · ∂F/∂ζ_i
    ∂F/∂v_l = Σ_{k=0}^{m} (∂F/∂β_k) · β_k(δ_kl − β_l) ,   l = 1…m.

Vectorized R (for the implementer): with `B = A * (ζ ζᵀ)` (elementwise,
diagonal zeroed), `Δ` the p×p matrix of `θ_i − θ_j`:

    dF/dθ  = 2 * rowSums(B * ρ'(Δ))
    dF/dζ  = 2 * (A * ρ(Δ) with diag 0) %*% ζ
    dF/dβ_k = sum(B * cos(k*Δ))

**Mandatory unit test:** analytic gradient vs central finite differences of
`F` at ≥ 20 random feasible points (random θ, ζ, β, random PD `R`), relative
agreement ≤ 1e-7. This single test kills the largest class of
plausible-but-wrong estimation bugs (sign errors in ρ', missing factor of 2,
diagonal leakage).

### 3.5 Optimizer, starts, multi-start, boundary polish

- **Optimizer:** `stats::nlminb(objective = F, gradient = ∇F)` in the
  unconstrained coordinates. No box constraints needed. Tight tolerances
  (`rel.tol ≤ 1e-12`); the problem is ≤ ~35 parameters even at p = 16, m = 3.
- **Starting values:**
  - `θ⁰`: the user's `angles` (theoretical positions; default `octants()`),
    reference fixed.
  - `ζ⁰_i`: `sqrt(max_j≠i |r_ij|)`, clipped to [0.3, 0.95] — cheap, scale-free,
    and inside the feasible region.
  - `β⁰`: least-squares fit of the off-diagonal `r_ij` on
    `{cos(k·δ⁰_ij)}_{k=0..m}` using the starting angles; clip negative
    coefficients to 0.01 and renormalize to sum 1. Degenerate fallback if the
    LS system is singular: `(0.4, 0.3, 0.2, 0.1, …)` truncated to m+1 and
    renormalized.
- **Multi-start:** (a) the start above; (b) its reflection (§2.3);
  (c) 3–5 jittered starts (θ ± U(−30°, 30°) noise, ζ/β at defaults). Keep the
  lowest `F̂`; if the best two distinct optima differ in `F̂` by > 1e-6,
  record a multimodality flag in the output details. Reflection pairs with
  identical `F̂` are expected and are *not* multimodality.
- **β boundary polish:** softmax cannot reach `β_k = 0` exactly; a vanishing
  harmonic shows up as `v_k → −∞` (slow tail convergence). After
  convergence, for any `β̂_k < 1e-4` (k ≥ 1): refit with that harmonic
  removed (drop `v_k`, i.e. fix `β_k = 0`); if `F̂` increases by < 1e-8,
  accept the reduced fit and **report it as the m-reduced model with the
  corresponding df** (a parameter on the boundary is not a free parameter;
  silently keeping its df is anti-conservative). Record the reduction in the
  output details.
- **Convergence reporting:** store `nlminb` convergence code, gradient norm
  at the solution, Hessian condition number, boundary flags (ζ, β),
  multimodality flag. `summary()` surfaces all of them (§5.4). Never return
  estimates from a non-converged fit without a warning.

### 3.6 Backend decision (made with Jeff, 2026-07-03)

**Decision: native optimization. Phase 1 in pure R; Phase 2 ports the
objective+gradient to RcppArmadillo when profiling shows repeated-refit
workloads need it, with the R implementation retained permanently as the
in-package test oracle.** OpenMx and lavaan appear only as Suggests-level
cross-validation oracles in tests (§6.4), never as the engine.

Argued both ways:

- **Native (chosen).** The problem is tiny (q ≤ ~35, one p×p inverse per
  evaluation) and smooth with cheap analytic gradients — sub-second in plain
  R. Zero new hard dependencies, per the package's dependency policy; the
  flagship feature cannot live behind a Suggests "graceful degradation"
  because degrading gracefully would mean the anchor feature doesn't exist.
  Full control of the circular machinery: periodic smooth objective, wrap
  only at reporting, reuse the existing circular-CI code. Validation against
  CIRCUM is direct (same discrepancy function). Cost: we own multi-start,
  boundary polish, and Heywood monitoring — exactly the statistical control
  this model needs anyway.
- **R-first, C++ second (the phasing).** C++ buys nothing for a one-shot
  fit; the only hot paths are bootstrap CIs (~2000 refits) and the Brief-B
  diagnostic's simulate-and-refit loop. Writing R first means the statistics
  are validated in the easiest-to-audit language, and the eventual C++ port
  has a byte-comparable oracle (`F_cpp(γ) == F_R(γ)` to 1e-12 at random
  points) — matching DESIGN.md's existing "C++ tested against base-R
  equivalents" strategy. A day-one C++ core would have neither.
- **OpenMx (rejected as engine).** `mxAlgebra` can express `P(γ)` literally
  and its ML machinery is mature — the most capable delegate. But it is a
  heavy hard dependency (large install, bundled optimizers, occasional CRAN
  friction) for a 35-parameter smooth problem, and the circular issues
  (reflection, wrapping, circular CIs) would still be fought from outside.
- **lavaan (rejected as engine).** Maps only through the §1.3 factor
  representation with nonlinear loading constraints
  (`ζ_i·√β_k·cos(kθ_i)` etc.); m ≥ 2 is contorted, angle parameters inside
  constraint syntax are fragile, and delta-method SEs on circular parameters
  are precisely what we must not delegate. Retained as an m = 1 oracle only.

---

## 4. Inputs

    Raw data path:   data + scales (+ listwise) → Pearson R, N = complete rows
    Matrix path:     cormat + n supplied directly (CircE-style; enables
                     published-matrix validation and, later, user-supplied
                     polychoric matrices)

Validation at entry (house style — `stopifnot()` + `is_*()` helpers):

- exactly one of `data` / `cormat` supplied; `cormat` symmetric, unit
  diagonal, `n` a positive whole number > p;
- `R` PD check: smallest eigenvalue > 1e-10, else error (singular `R` makes
  `ln|R|` undefined and the χ² meaningless — refuse rather than warn;
  pairwise-deleted matrices are a likely source, hence listwise default);
- `angles` length matches `scales`; degrees in; converted once via
  `as_radian(as_degree(angles))`;
- df ≥ 1 for the requested variant/m (§1.4), `m ≤ floor((p−1)/2)`.

Missing data: listwise only in the first cut. No pairwise option (nPD risk);
the `cormat` path is the escape hatch for users with their own missing-data
treatment.

---

## 5. Outputs

### 5.1 Point estimates

- `θ̂_i` — item/scale angles, **degrees in [0, 360)**, canonicalized (§2.3),
  reported alongside the theoretical angles for comparison; radians never
  escape the estimator.
- `ζ̂_i` — communality indices (plus `ζ̂_i²` as a derived column).
- `β̂_0 … β̂_m` — correlation-function weights.
- `ρ̂(δ)` — the estimated correlation function (returned as a function and as
  a 0–180° grid for plotting).
- `P̂ = P(γ̂)`, residuals `R − P̂`.

### 5.2 Confidence intervals

Two methods, both exposed; **analytic is the default** (matches CIRCUM's
output, which is also how it gets validated), bootstrap is the package-native
alternative:

- **Analytic:** `avar(γ̂*) = (2/n)·H⁻¹`, where `H` is the Hessian of `F` in
  the unconstrained coordinates at the optimum, computed by central finite
  differences **of the analytic gradient** (no numDeriv dependency; step
  1e-5, symmetrized). Delta-method back to natural parameters:
  `SE(ζ_i) = ζ_i(1−ζ_i)·SE(u_i)`; β via the softmax Jacobian;
  angles are already natural (Jacobian 1) — CI `θ̂_i ± z·SE`, wrapped for
  display per §2.4. Subject to the §3.2 validation gate.
- **Bootstrap (raw-data path only):** resample rows, recompute `R`, refit
  warm-started from `γ̂` (warm starts make reflection re-handling
  unnecessary); percentile intervals; **angle replicates go through the
  existing `quantile.circumplex_radian` machinery** — center on circular
  mean, unwrap, quantile, re-wrap. Non-converged or singular-R resamples are
  excluded with a count warning (same conditional-on-estimability convention
  as `ssm_analyze`). This path is the Phase-2 C++ trigger if slow (§8).

### 5.3 Fit indices (all defined from the discrepancy)

With `T = n·F̂`, `n = N − 1`, model df from §1.4; null model = independence
(`P₀ = I`), for which `F₀ = −ln|R|` (since `tr(R) = p`), `T₀ = n·F₀`,
`df₀ = p(p−1)/2`:

- **χ² test:** `T`, df, p-value.
- **RMSEA:** `sqrt(max(F̂/df − 1/n, 0))`; 90% CI by inverting the noncentral
  χ²: find `λ_L, λ_U` with `pchisq(T, df, ncp = λ_U) = .05` and
  `pchisq(T, df, ncp = λ_L) = .95` (uniroot; λ_L = 0 when
  `pchisq(T, df) ≥ .95`), bounds `sqrt(λ/(n·df))`.
- **SRMR:** `sqrt( Σ_{i<j} (r_ij − p̂_ij)² / (p(p−1)/2) )`.
- **CFI:** `1 − max(T − df, 0) / max(T₀ − df₀, T − df, 0)`.
- **TLI:** `((T₀/df₀) − (T/df)) / ((T₀/df₀) − 1)`.
- **AIC/BIC** for m-selection across fits to the same `R`:
  `T + 2q`, `T + q·ln N` (comparable only across models on identical data —
  document that restriction).

### 5.4 Object and methods (API sketch — consistent with `circumplex_ssm`)

Proposed constructor name **`cpm_fit()`** ("circular process model"); avoids
overloading "fit", which already means the SSM R² parameter in this package.
(Alternatives considered: `browne_fit()`, `fit_circumplex()`. Final name is
an open decision, §9.)

    cpm_fit(
      data = NULL, scales, angles = octants(),   # theory/start angles, degrees
      cormat = NULL, n = NULL,
      m = 3,
      model = c("quasi-circumplex", "constrained-angles",
                "equal-communality", "circulant"),
      reference = 1,                             # index into scales
      interval = 0.95,
      ci_method = c("analytic", "bootstrap"),
      boots = 2000, listwise = TRUE
    ) -> circumplex_cpm

`circumplex_cpm` (S3 list, constructor in the `new_ssm()` style):

    results   data frame: Scale, Angle_theory, Angle (deg, [0,360)),
              Angle_lci, Angle_uci, Zeta, Zeta_lci, Zeta_uci, Communality
    betas     data frame: k, Beta, Beta_lci, Beta_uci
    fit       list: chisq, df, pvalue, rmsea, rmsea_ci, srmr, cfi, tli,
              aic, bic, F, n, N
    corfun    function(delta_deg) -> rho_hat
    matrices  list: R, Phat, residuals
    details   list: m (as fitted, after any boundary reduction), model,
              reference, ci_method, interval, boots, convergence code,
              gradient norm, hessian condition, boundary flags,
              multimodality flag, call

Methods:

- `print()` — compact: estimates table + one fit line (χ²(df), RMSEA [CI],
  SRMR, CFI), boundary/convergence warnings if any.
- `summary()` — adds β table, residual summary (largest |residual| with the
  offending pair named), all §2.5/§3.5 diagnostics in plain language.
- `plot()` — built on the M3 extension: `ggcircumplex(amax = 1)` canvas;
  each scale drawn at angle `θ̂_i` and **radius `ζ̂_i`** (`geom_ssm_point`
  with amplitude = ζ; the communality index is a natural radius in [0, 1]);
  angle CI drawn as an arc at that radius (`geom_ssm_arc` with the angle CI
  as the angular bounds — reusing the existing 0/360 wrap handling).
- `plot(type = "corfun")` — `ρ̂(δ)` curve over δ ∈ [0°, 180°] with points
  `(δ̂_ij, r_ij / (ζ̂_i ζ̂_j))` overlaid (attenuation-corrected observed
  correlations; the classic CIRCUM-style figure). Cap displayed corrected
  values at ±1 with a note when ζ̂'s are small.
- `cpm_simulate(object, n)` — draw n observations from `P̂` via the factor
  representation `x = Λz + (I − D_ζ²)^{1/2} ε` (exactly PSD by construction;
  consistent with the package's PSD-safe `mvn_draws()` philosophy). **This is
  the dependency contract with Brief B** (`ssm_ci_accuracy()` needs: `γ̂` =
  θ/ζ/β, N, m, and a simulate method — all present above).

---

## 6. Validation strategy

### 6.1 The oracle rule

Every external expected value must be transcribed **from a published
CIRCUM/CircE source, at implementation time, directly from the paper's
tables**. Explicitly banned as oracles: `devel/g2xx1.txt` (11.5k-line R
script of unknown provenance — possibly an un-vetted CircE refactor; see the
memory note and the Brief A-review warning), any other local file, and
**this document's author's memory**. That last one is why the tables below
are templates with blank value columns: half-remembered numbers written here
would be laundered into "the design doc says" authority. Transcription
protocol: one person/session transcribes from the PDF; a second
person/session independently re-reads the table and diffs; both record paper,
table number, and page in the test file comment.

### 6.2 Published oracles (candidate set, in priority order)

For each oracle, the test fixture needs: the input correlation matrix
(published in or reconstructable from the source), N, m, the
reference/constraint configuration, and the published estimates.

| # | Source | What it provides | Notes |
|---|---|---|---|
| O1 | Browne (1992), *Psychometrika* 57, 469–497 — the paper's own numerical illustrations | ML estimates (angles, communality indices, β), χ²/df from the model's author | Primary. Defines the estimator; any disagreement here is our bug until proven otherwise. |
| O2 | Grassi, Luccio & Di Blas (2010), *Behav Res Methods* 42, 55–73 — CircE worked example(s) | Full CircE output: estimates, SEs/CIs, RMSEA and other indices | Primary for CI/fit-index validation; it is the software being replaced. |
| O3 | Fabrigar, Visser & Browne (1997), *PSPR* 1, 184–203 — CIRCUM tutorial example | Estimates with CIs, fit stats, and the reference-fixing convention spelled out | Good for pinning conventions (reference angle, n vs N−1). |
| O4 | Remington, Fabrigar & Visser (2000), *JPSP* 79, 286–300 — CIRCUM reanalyses of published affect matrices | Many fits (multiple matrices): angles + CIs, communality indices, fit indices | Breadth: several p/m configurations from one consistent pipeline. |
| O5 | Zimmermann & Wright (2017), *Assessment* — supplemental CircE fits | Browne-model estimates for IIP-type octant scales | Secondary (supplement, not main tables); doubles as the Brief-B bridge. |

Per-oracle test template (values transcribed at implementation):

    Source/table: ____   Input: R (p = __) from ____, N = __, m = __, model = __
    Expected: θ̂ (deg, as differences from reference): ____
              ζ̂: ____      β̂: ____      χ²(df): ____   RMSEA: ____ [CI ____]

### 6.3 Tolerances and the mismatch-diagnosis checklist

Published tables are rounded; optimizers differ in tail convergence. Targets:

- angles: within **0.5°** of published values rounded to whole degrees
  (0.05° if published to one decimal), compared as differences from the
  reference (§2.1);
- ζ, β: within **0.005** (published to 2 decimals) / 0.0005 (3 decimals);
- χ²: within 0.5 or 0.1% relative, whichever is larger; RMSEA within 0.002;
- analytic CIs: endpoints within 0.5°/0.005 (this is the §3.2 gate — failure
  here means implement the correlation-structure asymptotics, not widen the
  tolerance);
- superiority criterion: our `F̂` evaluated at our optimum must be ≤ `F`
  evaluated at the published estimates (reconstructed into `P`) + 1e-8.
  If we fit *better* but disagree, the published values may be a looser
  optimum — investigate, document, and prefer the mathematical criterion.

When a target misses, diagnose in this order before touching the estimator:
(1) reflection/rotation alignment (§2.1/2.3 — compare reference-relative
differences, try the mirror); (2) `n = N − 1` vs `N` in `T` and RMSEA;
(3) m mismatch or a boundary-reduced m (§3.5); (4) their matrix transcription
(re-diff the input `R` against the paper); (5) ζ vs ζ² labeling; only then
(6) suspect the code.

### 6.4 Internal and cross-implementation oracles (no external numbers needed)

These run in the regular test suite and catch most plausible-but-wrong math
without any transcription:

- **Gradient check** (§3.4): analytic vs finite-difference, ≤ 1e-7.
- **Exact-recovery round trip:** build `P(γ₀)` for known feasible `γ₀`
  (several: generic; angle at the 0/360 pole; near-equal angles; small β
  tail), feed it as the "sample" matrix — the fit must recover `γ₀` to 1e-6
  (angles via angular distance) with `F̂ ≤ 1e-12`. This kills wrong-sign
  harmonics, ζ/ζ² confusion, diagonal leakage, df/rotation errors.
- **Sampling consistency:** simulate large-N data from `P(γ₀)` via
  `cpm_simulate`, refit, estimates → γ₀ as N grows (seeded, loose tolerance).
- **Invariance suite:** permuting scale order, rotating all starting angles,
  and reflecting the data's generating angles must leave `F̂` and
  reference-relative estimates unchanged (canonicalization determinism §2.3).
- **Circulant closed-form check:** for variant D with p equally spaced
  angles and an exactly circulant input matrix, the fitted `β̂` must match
  the (truncated, nonnegative) DFT of the circulant's first row rescaled by
  ζ² — semi-analytic; exact when the input is itself in the model family
  (make it so by construction).
- **Cross-implementation (Suggests, skip-if-not-installed):** an OpenMx
  `mxAlgebra` transcription of §1.3's `P = ΛΛᵀ + I − D_ζ²` fitted by ML on
  the same `R` — independent optimizer and code path, same discrepancy;
  estimates must agree to 1e-4. A lavaan constrained two-factor model as an
  m = 1-only second check. These are *test oracles*, never runtime paths.

### 6.5 Convention traps to encode as tests (not prose)

- Compare angles only as reference-relative differences (rotation).
- ζ (index) vs ζ² (communality) labeled and tested distinctly.
- `n = N − 1` multiplier pinned by a fit-index unit test with hand-computed
  `T` from a fixed small `R`.
- Degrees at the API, radians inside — pinned by a test that `cpm_fit` with
  angles in degrees reproduces an internal radian call exactly.

### 6.6 Boundary suite (CLAUDE.md danger-zone requirements, adapted)

Required tests: generating angle exactly at the 0°/360° pole (estimate
reported ≈0 or ≈360, both accepted per DESIGN G2); analytic and bootstrap
angle CI straddling 0/360 (contiguous, estimate inside on the reported
branch); mirror-start convergence to equal `F̂` and identical canonicalized
output; `β_m → 0` boundary polish (df reduced, `F̂` unchanged);
`ζ → 1` Heywood flag fires; clustered-angle ill-conditioning warning fires;
singular `R` refused with the documented error; df = 0 saturated fit warns.

---

## 7. Defaults (proposed)

| Choice | Default | Rationale |
|---|---|---|
| m | `min(3, floor((p−1)/2))` | CIRCUM-literature convention at p = 8; §1.4 cap |
| model | quasi-circumplex | The scientific question is usually "where are the items actually" |
| reference | first scale, fixed at its theoretical angle | Deterministic, documented, matches user's mental model |
| ci_method | analytic | Matches CIRCUM (validation), instant; bootstrap available |
| interval | 0.95 | Package convention |
| listwise | TRUE (only) | §4; nPD risk |

---

## 8. Phasing and scope

**Phase 1 (this milestone, R only):** model/gradient/optimizer in R;
variants A–D; analytic CIs (with the §3.2 gate); fit indices; bootstrap CIs;
print/summary/plot; `cpm_simulate`; full §6 validation. Files (proposed):
`R/cpm_fit.R` (API + engine), `R/cpm_oop.R` (class/methods),
`tests/testthat/test-cpm_fit.R`, `test-cpm_boundary.R`, `test-cpm_oracles.R`.

**Phase 2 (gated on profiling, not scheduled):** port `F`/`∇F` (and only
those) to RcppArmadillo for the bootstrap and Brief-B simulation loops.
Trigger: default-settings bootstrap (2000 refits) exceeding ~30 s on octant
data, or the Brief-B diagnostic needing ≥ 10⁴ refits. The R implementation
stays as the permanent oracle: `F_cpp == F_R` and `grad_cpp == grad_R` to
1e-12 at ≥ 50 random feasible points.

**Out of scope for the first cut** (documented, not promised): OLS/GLS/ADF
discrepancies; pairwise deletion; polychoric input (the `cormat` path is the
hook); correlated uniquenesses; multi-group CPM; equality constraints beyond
variants B–D.

---

## 9. Open decisions (for Jeff)

1. **Function/class name:** `cpm_fit()` / `circumplex_cpm` (proposed) vs
   `browne_fit()` vs other. Only naming; everything else is name-agnostic.
2. **Default ci_method** analytic (proposed) vs bootstrap-by-default for
   consistency with `ssm_analyze()`'s field-convention argument.
3. Whether variants B–D ship in the first cut (proposed: yes — they are ~free
   given the parameterization, and B enables the equal-spacing test the fit
   milestone wants) or follow the quasi-circumplex.

## 10. Decided

- **Backend:** native optimization; R first, RcppArmadillo port gated on
  profiling with R retained as oracle; OpenMx/lavaan as Suggests test oracles
  only. (Jeff, 2026-07-03 — see §3.6.)

## References

- Browne, M. W. (1992). Circumplex models for correlation matrices.
  *Psychometrika, 57*(4), 469–497.
- Browne, M. W. (1984). Asymptotically distribution-free methods for the
  analysis of covariance structures. *BJMSP, 37*, 62–83.
- Browne, M. W., & Shapiro, A. (1986). The asymptotic covariance matrix of
  sample correlation coefficients under general conditions. *Linear Algebra
  and its Applications, 82*, 169–176.
- Cudeck, R. (1989). Analysis of correlation matrices using covariance
  structure models. *Psychological Bulletin, 105*(2), 317–327.
- Fabrigar, L. R., Visser, P. S., & Browne, M. W. (1997). Conceptual and
  methodological issues in testing the circumplex structure of data in
  personality and social psychology. *PSPR, 1*(3), 184–203.
- Grassi, M., Luccio, R., & Di Blas, L. (2010). CircE: An R implementation of
  Browne's circular stochastic process model. *Behavior Research Methods,
  42*(1), 55–73.
- Remington, N. A., Fabrigar, L. R., & Visser, P. S. (2000). Reexamining the
  circumplex model of affect. *JPSP, 79*(2), 286–300.
- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
  interpersonal construct validation. *Assessment, 24*(1), 3–23.
