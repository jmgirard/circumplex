# RR04: CIRCUM free-scaling covariance family — review findings (M17)

- **Date:** 2026-07-12
- **Brief:** `cairn/reviews/RB04-circum-free-scaling.md`
- **Reviewer role:** independent psychometrician/statistician (SEM, circumplex
  modeling), fresh session, no conversation context.
- **Materials read:** `devel/m4-browne-design.md` (§1, §2, §3.2–3.5, §5.2–5.3,
  §6.3, §11 B6/B4/B2 entries), `tests/testthat/test-cpm_oracles.R` (header,
  `cpm_mx_model`, free-scaling oracle at line 329, published-oracle blocks),
  `tests/testthat/helper-cpm-oracles.R` (the Grassi et al. 2010 transcription),
  `R/cpm_fit.R` (`cpm_discrepancy`, `cpm_spec`, `cpm_gradient`,
  `cpm_polish_beta`, Hessian/analytic-CI machinery, `cpm_rmsea_ci`,
  `cpm_fit_indices`), `cairn/DESIGN.md` "CPM confidence intervals: measured
  coverage". All published values cited below trace to the
  `helper-cpm-oracles.R` transcription of Grassi, Luccio & Di Blas (2010),
  Appendix A / Tables 1–3 (second human re-read still pending per the file
  header — noted again in Q6).
- **Verification note:** the Q3 gradient derived below was checked numerically
  in this session against central finite differences of the discrepancy at 25
  random feasible points (random θ, ζ, β, s ∈ [−0.35, 0.35], random PD
  unit-diagonal R): worst mixed absolute/relative error 3.6e-9, FD-limited.
  All df/fit-index arithmetic quoted below was recomputed from the fixture
  values, not from memory.

**Verdict up front: GO** (details in Q1; one-liner at the end).

---

## 1. Go/no-go

**GO.** Decisive consideration: this package's flagship claim is being the
CIRCUM/CircE replacement, and the B6 battery proved the current family
*provably cannot* reproduce published CIRCUM/CircE output — the published
F̂ = 0.089815 sits strictly below the diag-constrained optimum 0.09596
(ΔT ≈ 1.07 at N = 175, angles off by up to ~1.3°, T/RMSEA/CFI shifted). A
replacement package whose numbers structurally cannot match the literature it
replaces, for every user who re-runs a published analysis, is a standing
credibility cost; "documented allowances" in a test file do not transfer to a
user comparing their output against a published table.

Against the cost side of the ledger, the extension turns out to be
statistically tame — this review found no hidden hazard of the kind that
would justify a no-go:

- **The gradient extends cleanly** (Q3): one new block with a closed form
  `∂F/∂s_i = 2(1 − (Σ⁻¹R)_ii)`, and the existing γ blocks carry over with a
  single substitution (A built from Σ, conjugated by D_σ). Derived and
  numerically verified below.
- **df is unchanged** (Q4i): the p new parameters meet p new (diagonal)
  moments; the published df = 7 for p = 7, m = 1 confirms the convention.
  No new feasibility bounds.
- **Identification is automatic** (Q2): all p σ free, no pin, no new
  redundancy; canonicalization untouched (Q5).
- **The validation anchor already exists and passes**: the OpenMx
  free-scaling oracle (`test-cpm_oracles.R:329`) reproduces the published
  CircE output to its printed precision (ζ/β to 4 decimals, angles ~0.01°),
  so M18 validates against a target that is already green.

There is also a positive statistical argument the brief did not ask for but
that strengthens the case (expanded in Q4ii and "Beyond the brief"): the
free-scaling family — not the diag-constrained one — is the parameterization
for which ML-on-a-correlation-matrix is textbook-valid. Free diagonal
scalings are precisely Browne's device for making correlation-input ML
legitimate (the family is closed under diagonal rescaling, so Cudeck's
invariance condition holds exactly). Adding it gives the package the variant
with the cleanest χ² justification, not just a compatibility mode.

The real costs are: (a) spec/pack/unpack/gradient plumbing for one more
block; (b) a free-family extension of the coverage oracle before any CI
claim; (c) a handful of convention decisions (SRMR diagonal, σ reporting,
AIC/BIC comparability) — all resolved in Q4/Q6 below, none open-ended. The
doubled CI surface is smaller than it sounds: bootstrap machinery is
family-agnostic (resample → refit), and the analytic-CI posture transfers
with the same N-conditional caveat already shipped.

## 2. σ parameterization and identification

### Unconstrained map

Add one row to the §3.3 table:

| Natural | Constraint | Unconstrained | Map | Jacobian |
|---|---|---|---|---|
| σ_i, i = 1…p | σ_i > 0 | s_i ∈ ℝ | σ_i = e^{s_i} | ∂σ_i/∂s_i = σ_i |

The log map is the right choice, and I did not find a better-conditioned
alternative worth arguing for:

- It matches the house style (strictly feasible iterates, no box
  constraints, `nlminb` unconstrained).
- It is perfectly conditioned exactly where all realistic solutions live:
  the published variance ratios are σ̂² ∈ [0.963, 1.042], i.e. s within
  ±0.021 of 0. The exp map is essentially the identity (Jacobian ≈ 1) in
  that whole neighborhood.
- It symmetrizes the multiplicative parameter (ratios .96 and 1.04 are
  equidistant from 1 on the log scale), which is also the scale on which
  CircE itself works for its variance-type parameters (its v CIs are Wald on
  ln v — decoded in `test-cpm_oracles.R:126`).
- The chained gradient has a closed form cleaner than the natural-scale one
  (Q3): `∂F/∂s_i = 2(1 − (Σ⁻¹R)_ii)`.

Start values: `s⁰ = 0` (σ = 1) for **every** start in the multi-start
scheme, including the mirror and jittered starts — do not jitter s. The
correlation-family start logic (θ⁰, ζ⁰, β⁰) is unchanged. No boundary polish
analog is needed for σ: the optimum is interior by coercivity (below), and
softmax-style tail stalling cannot occur.

### Identification: all p σ free, no pin

**The model map is injective, so no constraint is needed.** Given the
existing γ identification (rotation fixed, Σβ = 1, reflection handled at
reporting), the decomposition Σ = D_σ P D_σ with diag P = 1 identically is
unique: σ_i = √(Σ_ii) recovers σ from Σ, then P = D_σ⁻¹ Σ D_σ⁻¹ recovers the
correlation structure, within which (θ, ζ, β) are identified exactly as
today. Equivalently, in factor-model terms with λ_i = σ_i ζ_i and
u_i = σ_i²(1 − ζ_i²): off-diagonals identify λ_i (given ρ), the diagonal
identifies λ_i² + u_i, and (λ_i, u_i) ↔ (σ_i, ζ_i) is a bijection on
σ > 0, ζ ∈ (0, 1). So q → q + p with no offsetting constraint.

**What identifies σ_i when the input has a unit diagonal.** The unit
diagonal of R is *data* as far as the Wishart discrepancy is concerned — the
p diagonal elements enter F through ln|Σ| and tr(RΣ⁻¹) exactly like any
other moments. There is no flat direction in σ-space: perturbing σ_i moves
diag Σ away from the observed 1, which F penalizes. Concretely, each σ_i is
pinned by its own likelihood equation (derived in Q3):

    ∂F/∂s_i = 2(1 − (Σ⁻¹R)_ii) = 0   ⟺   (Σ̂⁻¹R)_ii = 1 ,  i = 1…p.

F is coercive in each σ_i (as σ_i → ∞, ln|Σ| ~ 2 ln σ_i → ∞; as σ_i → 0,
tr(RΣ⁻¹) → ∞ for PD R), so a finite interior optimum in σ always exists.

**Reconciling "σ̂ = 1 is the nesting anchor" with "σ̂ ≠ 1 at finite N".**
Both are correct, and the stationarity condition above is the reconciliation:

- If R lies exactly in the correlation family (population matrices, the
  exact-recovery tests), the free family attains F = 0, which forces Σ̂ = R,
  hence diag Σ̂ = 1, hence σ̂ = 1 — *uniquely*. This is the population
  anchor, and it should be encoded as a test (Q6).
- At finite N, R is not in the family, and the ML projection does **not**
  preserve the diagonal: the free-family optimum satisfies
  diag(Σ̂⁻¹R) = 1, not diag(Σ̂) = diag(R). The two conditions coincide only
  at perfect fit. The original §3.2 claim was, in effect, the assumption
  that the ML projection preserves the diagonal; it preserves
  diag(Σ̂⁻¹R) instead. That is the precise mathematical content of the B6
  refutation, and it makes the finite-N σ̂ ≠ 1 finding a necessity, not a
  curiosity: at the diag-constrained optimum diag(P̂⁻¹R) ≠ 1 generically, so
  the free family's σ gradient is nonzero there and the optimizer must move.

**Is the OpenMx oracle sufficient as the identification answer?** It is
confirmatory, not sufficient. The free-scaling branch of `cpm_mx_model` (all
p of `s` free, `lbound = 0.1` as a numerical guard only) converging from a
cold start to the unique published solution demonstrates empirical
identification *at that dataset*; it cannot establish identification of the
family. The analytic argument above settles it and should go in the design
doc; the oracle then serves as the cross-implementation check it was built
to be. (Note the OpenMx branch correctly omits the N/(N−1) pre-multiplication
that the diag-constrained oracle needs — the free family is closed under
scalar rescaling, which is itself a small empirical confirmation of the
invariance property used throughout this review.)

**Conditioning caveat, not an identification failure:** σ_i and ζ_i are
correlated coordinates (raising σ_i and lowering ζ_i leaves variable i's
off-diagonal row nearly fixed, resisted only by the diagonal misfit
penalty), so the Hessian condition number will generally worsen somewhat
versus the diag-constrained fit, and Heywood-adjacent fits (ζ̂_i → 1) will
show it most. The existing §2.5 condition-number warning covers this;
no new machinery is needed, but the coverage-oracle extension (Q4/Q6)
should include a Heywood-prone cell so the effect is measured, not assumed.

## 3. The free-family analytic gradient (central)

### Setup

The ML discrepancy against input matrix R (unit diagonal on the current API,
but nothing below requires it):

    F(Σ) = ln|Σ| − ln|R| + tr(R Σ⁻¹) − p .

Differentials: d ln|Σ| = tr(Σ⁻¹ dΣ) and d tr(RΣ⁻¹) = −tr(Σ⁻¹ R Σ⁻¹ dΣ), so

    dF = tr(A dΣ) ,   A := Σ⁻¹ − Σ⁻¹ R Σ⁻¹   (symmetric).           (1)

This is §3.4's A with one **essential substitution: Σ⁻¹ replaces P⁻¹**. With
σ ≠ 1 the two matrices differ everywhere, not just on the diagonal — an
implementation that keeps `Pinv` in the A construction is wrong in every
gradient component, and the finite-difference test below will catch it.

The model: Σ = D_σ P D_σ, elementwise Σ_ij = σ_i σ_j P_ij, with
diag P = 1 identically (so Σ_ii = σ_i²), and P depending on (θ, ζ, β)
exactly as in §1.3.

### The σ block (new)

Since Σ_jk = σ_j σ_k P_jk,

    ∂Σ_jk/∂σ_i = (δ_ij σ_k + δ_ik σ_j) P_jk ,

and from (1), using symmetry of A and P,

    ∂F/∂σ_i = Σ_{j,k} A_jk ∂Σ_jk/∂σ_i
            = Σ_k A_ik σ_k P_ik + Σ_j A_ji σ_j P_ji
            = 2 Σ_j A_ij σ_j P_ij ,      **including the j = i term**      (2)

whose diagonal contribution is 2 A_ii σ_i (P_ii = 1). In matrix form
∂F/∂σ = 2 (A ∘ P) σ, where ∘ is the elementwise product with the **full
diagonal kept**. Chaining through the log map (Jacobian σ_i):

    ∂F/∂s_i = σ_i ∂F/∂σ_i = 2 Σ_j (σ_i σ_j A_ij) P_ij = 2 (A ∘ Σ)_i· 1
            = 2 (A Σ)_ii .

And since A Σ = (Σ⁻¹ − Σ⁻¹RΣ⁻¹) Σ = I − Σ⁻¹R, this collapses to the closed
form

    ∂F/∂s_i = 2 (1 − (Σ⁻¹ R)_ii) .                                       (3)

Two immediate corollaries worth encoding as tests:

- **Stationarity identity:** at any free-family optimum,
  diag(Σ̂⁻¹R) = 1_p exactly (each of the p diagonal likelihood equations) —
  a free, sharp, oracle-independent correctness check on converged fits, and
  the free-family analog of the classical "diag of Σ̂⁻¹(R − Σ̂) = 0" property
  of ML factor analysis with free uniquenesses. It also implies
  tr(Σ̂⁻¹R) = p, so F̂ = ln|Σ̂| − ln|R| at the optimum — a second cheap
  invariant.
- **Direction of the B6 effect:** at the diag-constrained optimum (σ = 1,
  Σ = P̂), (3) evaluates to 2(1 − (P̂⁻¹R)_ii) ≠ 0 generically — the
  free-family gradient is nonzero at the nested optimum, which is exactly
  why F̂_free < F̂_diag at finite N.

### The γ blocks (corrected §3.4)

Only P depends on (θ, ζ, β), so dΣ = D_σ dP D_σ and, from (1),

    dF = tr(A D_σ dP D_σ) = tr(D_σ A D_σ dP) = tr(Ã dP) ,
    Ã := D_σ A D_σ ,   i.e.  Ã_ij = σ_i σ_j A_ij .                       (4)

**Where the §3.4 simplification breaks — precisely.** The §3.4 statement
"only off-diagonal ∂P_ij enter" is about dP, and it remains **true** in the
free family: diag P = 1 identically in the (θ, ζ, β) parameterization
(P_ii = ζ_i²ρ(0) + 1 − ζ_i² = 1 with ρ(0) = Σβ = 1 enforced by softmax), so
diag dP = 0 along every feasible γ direction. The moving diagonal of Σ lives
entirely in the σ block, i.e. in (2)–(3), where the diagonal term 2A_ii σ_i
must be kept. What *does* change in the γ blocks is the weight matrix: A is
built from Σ⁻¹ (not P⁻¹) and conjugated by D_σ. So the corrected natural-scale
gradients are §3.4 with A → Ã:

    ∂F/∂θ_i = 2 Σ_{j≠i} Ã_ij ζ_i ζ_j ρ'(δ_ij)                            (5)
    ∂F/∂ζ_i = 2 Σ_{j≠i} Ã_ij ζ_j ρ(δ_ij)                                 (6)
    ∂F/∂β_k = Σ_{i≠j} Ã_ij ζ_i ζ_j cos(k δ_ij)                           (7)

with δ_ij = θ_i − θ_j and ρ'(δ) = −Σ_k k β_k sin(kδ) as before.

**The β-diagonal subtlety (state it so no one "fixes" it).** If one instead
treated β_k as unconstrained in the natural-scale intermediate, the diagonal
would contribute ∂P_ii/∂β_k = ζ_i² cos(0) = ζ_i², i.e. a term
Σ_i Ã_ii ζ_i² to every ∂F/∂β_k, **constant in k**. The softmax Jacobian
J = diag(β) − ββᵀ annihilates constant vectors (J·c1 = c(β − β) = 0), so
including or excluding the diagonal in (7) gives *identical* ∂F/∂v after
chaining. The existing convention (exclude the diagonal, then chain) remains
exactly correct for the free family; it is not a bug to be repaired, and
"repairing" it by adding the diagonal to (7) without the simplex-tangent
chain would be the actual bug.

### Chain rule composition (confirmed)

The §3.3 Jacobians are unchanged and one row is added:

    ∂F/∂θ*_i = ∂F/∂θ_i                      (identity; free positions only)
    ∂F/∂u_i  = ζ_i(1 − ζ_i) ∂F/∂ζ_i          (logit)
    ∂F/∂v_l  = Σ_k (∂F/∂β_k) β_k(δ_kl − β_l) (softmax, over keep_k, drop l = 0)
    ∂F/∂s_i  = 2(1 − (Σ⁻¹R)_ii)              (log map, already chained in (3))

Vectorized recipe for the implementer (mirrors `cpm_gradient()`):

    Sigma <- (sigma %o% sigma) * P            # P as now, diag P = 1
    Sinv  <- solve(Sigma)
    A     <- Sinv - Sinv %*% R %*% Sinv; A <- (A + t(A)) / 2
    At    <- (sigma %o% sigma) * A            # A-tilde
    B     <- At * (zeta %o% zeta); diag(B) <- 0
    dF_dtheta  <- 2 * rowSums(B * Rhod)                    # as now, B from At
    ARho <- At * Rho; diag(ARho) <- 0
    dF_dzeta   <- 2 * as.numeric(ARho %*% zeta)            # as now, from At
    dF_dbeta_k <- sum(B * cos(k * Delta))                  # as now, from At
    dF_ds      <- 2 * (1 - rowSums(Sinv * R))              # NEW; = 2(1 - diag(Sinv %*% R))

**Reduction check:** at σ = 1, Σ = P, Ã = A, and (5)–(7) are verbatim §3.4;
(3) becomes the (generally nonzero) constraint force on the diagonal. The
extended gradient restricted to the γ blocks at s = 0 must equal the legacy
`cpm_gradient()` **exactly** (same floating-point operations up to the
elementwise σσᵀ = 11ᵀ multiply) — encode this as a regression identity test,
because it kills the "used A instead of Ã" and "used P⁻¹ instead of Σ⁻¹"
error classes in one assertion.

**Numerical verification performed for this review:** all four blocks
(5)–(7) and (3), chained through identity/logit/softmax/log, were checked
against central finite differences of F at 25 random feasible points
(θ ~ U(0, 2π), ζ ~ U(.3, .95), interior softmax β with m = 2,
s ~ U(−0.35, 0.35), random PD unit-diagonal R at p = 7). Worst mixed
absolute/relative error 3.6e-9 — at the FD truncation floor, consistent with
the A-review F8 finding that the FD, not the analytic gradient, limits the
agreement.

### Mandatory finite-difference gradient unit test (extended)

Analog of §3.4/§6.4, for the extended parameter vector:

- **Points:** ≥ 20 random feasible points. Randomize θ (uniform on
  [0, 2π), plus at least one configuration with an angle at the 0/360 pole
  and one with near-equal angles), ζ ∈ (0.3, 0.95) via u, β interior via
  v ~ U(−1, 1), **s ~ U(−0.35, 0.35)** (σ ∈ [0.70, 1.42] — generous margin
  around the empirically observed .96–1.04 ratios without manufacturing
  ill-conditioning), random PD R. Include at least a few points with a
  **non-unit-diagonal input matrix** (the math never uses diag R = 1, and
  testing at general S exercises the σ block harder) and at least one point
  on a **reduced spec** (harmonic polished out, `keep_k` ≠ 0:m).
- **Criterion:** the A-review F8 mixed criterion, per component:
  |g_a − g_fd| ≤ 1e-7 · max(1, |g_fd|), central differences, step 1e-6.
  (A pure relative 1e-7 will flake on small components for the same
  FD-truncation reason as before.)
- **Conditioning caveats:** (i) cap the joint draw so κ(Σ) stays moderate —
  high ζ, tightly clustered θ, and |s| near the draw boundary compound;
  redraw (documented) any point with κ(Σ) > ~1e6, since there the FD itself
  degrades and the test would measure the FD, not the gradient. (ii) Keep
  the s draw centered on 0; the optimizer will never visit |s| > 0.5 on
  unit-diagonal input, and extreme s makes ln|Σ| terms dominate the FD
  error. (iii) Add the σ = 1 legacy-identity regression test above as a
  companion — the FD test alone cannot distinguish "correct free-family
  gradient" from "correct but different objective".

## 4. df / χ² / CI treatment

### (i) Degrees of freedom — unchanged (the brief's premise needs correcting)

The brief's framing "q → q + p, so df = p(p−1)/2 − q' shrinks" mixes the two
moment-counting conventions. The free family is a **covariance structure**:
it fits p(p+1)/2 moments with q_corr + p parameters, so

    df_free = p(p+1)/2 − (q_corr + p) = p(p−1)/2 − q_corr = df_diag .

**df is unchanged** — the p new σ meet the p new diagonal moments
one-for-one. Evidence from the transcription: p = 7, m = 1 unconstrained,
CIRCUM/CircE parameterization = 6 angles + 7 z + 7 v + 1 free β = 21
parameters against 28 moments → df 7 (`app$df = 7`), identical to our
diag-constrained 21 − 14 = 7. Also T = 174 × 0.089815 = 15.628 → published
15.63, and RMSEA √(.089815/7 − 1/174) = .0842 → published .084, confirming
the same n = N − 1 and RMSEA conventions carry over unchanged. All variant
df bookkeeping (§1.4 table), feasibility bounds (df ≥ 1), and the m cap are
untouched; the free-scaling flag is orthogonal to variants A–D.

**Implementation trap (flag loudly):** `cpm_spec()` derives
`df = p(p−1)/2 − q`. Naively adding p to q in that formula gives df − p,
which is wrong. The free-family spec must either switch the moment count to
p(p+1)/2 when σ is free or (equivalently) exclude the σ block from the q
used in that formula while including it in the q used for AIC/BIC penalties
and the Hessian dimension. A unit test asserting df_free == df_diag on the
same (p, m, variant) closes this.

**Input path:** it does not matter. The free family is closed under diagonal
rescaling, so fitting D S D for any diagonal D > 0 gives σ̂ → D σ̂ with
θ̂, ζ̂, β̂ and F̂ (hence T, df, RMSEA) exactly invariant. Fitting the
correlation matrix R therefore yields identical inference to fitting the
covariance S that produced it — which is precisely why CIRCUM was built this
way. Consequence for the API: keeping the current unit-diagonal `cormat`
requirement loses nothing statistically (recommend keeping it; see Q6), and
σ̂² is then directly interpretable as CIRCUM's "ratios of reproduced to
input variances" (fixture `var_ratios`).

### (ii) Analytic Wald CIs — the invariance argument survives, and is in fact stronger here

The §3.2 argument as written ("σ̂ = 1 at the optimum") is dead — B6 killed
it. But the *correct* form of the argument is the classical
correlation-structure invariance condition (Cudeck, 1989; Browne, 1982),
and the free family satisfies it **exactly**: the family is closed under
Σ → DΣD, and (θ, ζ, β) are scale-free parameters (invariant under that
rescaling, with σ absorbing D). Under that condition, fitting R with the
Wishart likelihood gives (a) T = n·F̂ asymptotically χ²_df — identical to
the T from the underlying S, by the invariance identity above — and
(b) naive information-based SEs that are asymptotically correct **for the
scale-free parameters** θ, ζ, β. This is not an analogy: free per-variable
scalings are the textbook device (Browne's own, in CIRCUM) for making
correlation-input ML valid, and CircE's published SEs (Appendix A θ SEs, v
SEs) come from exactly this machinery — our existing CI-fidelity comparison
(analytic half-widths within ~2° of 1.96·SE, `test-cpm_oracles.R:161–164`)
already brackets it empirically.

**The exception is σ itself.** σ is not scale-free, and when the input is a
correlation matrix the diagonal "moments" are degenerate (fixed at 1, not
Wishart-distributed), so the naive avar block for s describes sampling
variation that does not exist in the input. **Analytic CIs for σ must not
be reported — ever, on the correlation-input path.** Recommend reporting
σ̂² as "variance ratios" (CIRCUM's own output convention — Appendix A prints
the ratios with no CIs) as a diagnostic column, uncertainty-free. Bootstrap
CIs for σ̂ on the raw-data path would be statistically meaningful
(resampling genuinely re-randomizes R) but are not needed for CIRCUM
compatibility; defer as a decision (Q6).

**Finite-sample position — conditional (analytic-with-caveat), stated as a
decision.** The asymptotic argument is sound, but the measured
diag-constrained coverage record (DESIGN.md: analytic angle .76–.88 at field
N, no improvement 250→1000 at boundary truths; interior truths reach the
band only at N ≈ 2000) is driven by finite-sample mechanisms — Hessian
ill-conditioning at octant-like truths, ζ boundary pile-up, near-boundary β
— that the free family inherits unchanged and, with p extra correlated
coordinates (the σ–ζ ridge, Q2), plausibly slightly worsens. So:

- **Bootstrap remains the shipped default** on the raw-data path (posture
  unchanged; the machinery is family-agnostic).
- **Analytic CIs for θ, ζ, β are offered on the cormat path with the same
  N-conditional `summary()` caution**, but the caution constants
  (N = 2000/50000 thresholds, boundary markers) were calibrated on the
  diag-constrained family and **must not be silently reused**: extend the
  coverage oracle (`devel/m4-coverage-oracle.R`) with free-family cells
  (at minimum boundary/interior β at N ∈ {250, 1000}, plus one analytic-
  ladder spot check) before shipping the free family's analytic CIs. This
  is a gate, not a nicety — it is the same doctrine that caught F1.
- Bootstrap is **not** mandated for the free family: the cormat path has no
  resampling alternative, and the asymptotic justification for the
  scale-free parameters is at least as good as the one already shipped.

### (iii) T / RMSEA / SRMR / baseline conventions

- **T = n·F̂, n = N − 1:** unchanged; F̂ is now evaluated at Σ̂
  (`cpm_discrepancy()` already handles non-unit-diagonal model matrices —
  the published-F̂ identity test proves it). Verified against the fixture:
  174 × 0.089815 = 15.628 ≈ 15.63.
- **df:** unchanged (i above), so p-values, RMSEA, and the RMSEA CI
  inversion (`cpm_rmsea_ci`) carry over verbatim; verified RMSEA .0842 →
  published .084.
- **Null model:** unchanged. The natural free-family null is Σ₀ = D
  (independence with free variances); its ML fit to unit-diagonal R gives
  d̂_i = 1 and F₀ = −ln|R| with df₀ = p(p+1)/2 − p = p(p−1)/2 — numerically
  identical to the current convention, and confirmed by the fixture
  (null_chisq 747.663 = 174·(−ln|R|), null_df 21). CFI/TLI unchanged.
- **SRMR — a real convention decision (pin it in the spec):** in the free
  family the diagonal residuals R − Σ̂ are **no longer identically zero**
  (1 − σ̂_i², up to ±.04 in the published solution), so the off-diagonal-only
  vs diagonal-inclusive choice now changes the statistic beyond a fixed
  √((p−1)/(p+1)) factor. Recommend: keep the package-wide off-diagonal
  p(p−1)/2 convention for both families (internal consistency; documented),
  and encode the *exact* conversion in the oracle test rather than an
  allowance:
  SRMR_CircE² = [ (p(p−1)/2)·SRMR_ours² + Σ_i (1 − σ̂_i²)² ] / (p(p+1)/2)
  (input diagonal is 1, so the standardized diagonal residual is 1 − σ̂_i²).
  Update §6.3 checklist item 6 accordingly.
- **AIC/BIC:** use the full q' = q_corr + p in the penalty (they are real
  fitted parameters). But **restrict AIC/BIC comparisons to within a
  scaling family**: across families the two likelihoods sit on different
  effective moment spaces (the free family's p diagonal moments are
  degenerate on correlation input), so cross-family AIC differences are not
  interpretable. Document as an extension of the existing "comparable only
  across models on identical data" restriction. Cross-family comparison, if
  wanted, is the nesting inequality F̂_free ≤ F̂_diag plus the ΔT diagnostic
  — descriptive, not a calibrated test (the σ = 1 null is on the boundary
  of nothing, but the degenerate-diagonal geometry makes naive χ²_p wrong;
  see Beyond the brief).

## 5. Canonicalization / identification interaction

**σ is invariant under both existing symmetries; canonicalization is
unaffected.**

- **Rotation** (θ_i → θ_i + c) and **reflection** (θ_i → 2θ_ref − θ_i) act
  only on angles: they leave every |δ_ij| — hence C, hence P (given ζ, β),
  hence Σ = D_σ P D_σ — unchanged with σ untouched. Mirror optima therefore
  have identical F̂ **and identical σ̂**, and the §2.3 closest-to-theory
  canonicalization rule applies verbatim, operating on the angle block only.
- **No new discrete redundancy.** Sign: D_{−σ} P D_{−σ} for a single flipped
  σ_i would require compensating sign flips in row/column i of P, which the
  family cannot produce (P_ij = ζ_iζ_jρ with ζ > 0); the exp map makes σ > 0
  structural anyway. Permutation: each σ_i is anchored to variable i; no
  label-switching. Nothing new for canonicalization to resolve.

Interactions the spec must pin (mechanical, but each is a silent-bug site):

1. **Parameter-vector layout:** place the s block **between ζ and β** —
   [angles][u][s][v] — so β remains the trailing block. The boundary-polish
   machinery (`cpm_spec_reduce`) rebuilds `i_beta` as the last block and
   shrinks q from the tail; appending s *after* β would shift the s indices
   on every polish refit (a classic off-by-block bug). With s ahead of β,
   `cpm_spec_reduce` needs only its existing i_beta rebuild plus a static
   `i_sigma`.
2. **Reflection in unconstrained coordinates** (multi-start mirror start,
   `cpm_canonicalize`, and the bootstrap per-replicate mirror guard) must
   touch only the `i_angle` block — σ, ζ, β pass through. The mirror guard's
   angle-distance comparison is unchanged; state explicitly that s plays no
   role in mirror detection.
3. **Multimodality detection** ("distinct parameter points with equal F̂")
   must include the s block in the parameter-distance comparison — two
   optima differing only in σ would be a genuine (and alarming)
   non-identification signature that the angle-only comparison would miss.
4. **Mirror/jitter starts:** s⁰ = 0 in all starts (Q2); jitter patterns
   apply to angles only, preserving the no-RNG determinism contract.
5. **Heywood/boundary flags:** ζ̂ > 0.995 flag unchanged; no σ boundary
   exists (interior by coercivity), but consider a *data-pathology* note
   (not a boundary flag) if any σ̂² leaves, say, [0.5, 2] — on unit-diagonal
   input that indicates something badly wrong with the input matrix. The
   0°/360° danger-zone handling is untouched (σ does not interact with
   angle wrapping).

## 6. Spec adequacy (given go)

With Q2–Q5 above, the remaining derivation gaps that would otherwise force
unreviewed implementer choices are the following — each is resolved here or
explicitly routed to Jeff, so M18 can build without re-deriving anything:

1. **df bookkeeping** — resolved (Q4i): moment count switches to p(p+1)/2;
   df unchanged; unit test df_free == df_diag. Trap flagged.
2. **σ parameterization, starts, identification** — resolved (Q2): exp map,
   all p free, s⁰ = 0, no polish analog.
3. **Gradient** — derived and verified (Q3), including the two error-class
   regression tests (σ = 1 identity; FD extension) and the stationarity
   identity diag(Σ̂⁻¹R) = 1 as a converged-fit invariant.
4. **SRMR convention** — decision needed, recommendation given (Q4iii:
   off-diagonal, exact conversion identity in the oracle). *Route to Jeff
   for sign-off; statistical content settled.*
5. **σ reporting** — recommendation: report σ̂² as "variance ratios" (CIRCUM
   convention), no analytic CIs ever on correlation input; bootstrap σ CIs
   deferred/optional. *Decision, stated as one (Q4ii).*
6. **AIC/BIC cross-family restriction** — resolved (Q4iii): document.
7. **Coverage-oracle extension** — required gate before shipping free-family
   analytic CIs; cells specified (Q4ii). The `summary()` caution constants
   must be re-derived or explicitly shared with justification.
8. **Layout/canonicalization pins** — resolved (Q5, items 1–5).
9. **API surface** — naming and the exact user-facing flag
   (e.g. `scaling = c("unit", "free")`) is Jeff's call; statistically the
   flag must be orthogonal to `model` (variants A–D), giving the 8
   combinations, and the published Table 2 model 3c (fixed grid + free
   scaling) is the oracle for one of the new cells. Recommend **keeping the
   unit-diagonal `cormat` input requirement** even in free-scaling mode
   (Q4i shows nothing is lost; σ̂² keeps its variance-ratio meaning; the
   PD/unit-diagonal validation stays uniform). Accepting covariance input
   is a separable future decision, not part of this milestone.
10. **Convergence acceptance** — no change: the scaled-gradient-norm
    criterion applies to the extended vector as-is; the nlminb code stays
    advisory (do not reopen; noted only for completeness).

**Validation battery sufficiency.** The ≥2-independent-oracle-types bar is
met for point estimates: (a) **published program output** — Grassi et al.
(2010) Appendix A / Tables 2–3, already transcribed, now compared at
*same-model* §6.3 tolerances instead of B6's model-difference allowances
(this is the payoff: the free family retires the allowances for its own
comparisons — angles to published precision, ζ/β within 0.005, F̂ within the
optimizer-tail tolerance, plus the model 3c grid row); and (b) **independent
cross-implementation** — the OpenMx free-scaling oracle, already green at
publication precision. Required additions, all internal (no new
transcription): the extended FD gradient test; the σ = 1 legacy-identity
test; exact-recovery with the population anchor σ̂ = 1 (input exactly in the
correlation family ⇒ F̂ ≈ 0 and σ̂ = 1 to 1e-6); the stationarity identity
diag(Σ̂⁻¹R) = 1 at accepted optima; the **rescale-equivariance test** at
engine level (fit D R D: σ̂ → D σ̂, θ̂/ζ̂/β̂/F̂ invariant to ~1e-10 — the
sharpest single test of the whole construction, and it has no
diag-constrained analog); and the nesting inequality F̂_free ≤ F̂_diag + 1e-8
on every fixture. One caution transfers from the existing suite: the fixed-
grid free-scaling fixture (model 3c) has near-tied ζ basins (noted at
`test-cpm_oracles.R:353–355`), so the engine's multimodality flag is
expected to fire there — pin β and F̂, not per-variable ζ. Separately, the
**coverage oracle extension is mandatory before any CI-trust statement**
(Q4ii) — it is a different question from point-estimate validation and no
published or cross-implementation oracle can answer it (CIRCUM's CIs use
the same asymptotics; matching them validates fidelity, not coverage — the
F1 lesson). Finally, the Appendix A transcription's second independent
human re-read is still pending (helper header); it should be closed before
the free family's *tightened* tolerances lean on those digits.

---

## Beyond the brief

1. **The diag-constrained family's T is not asymptotically χ²_df on
   correlation input — the free family is the one with the clean
   justification.** The invariance condition that validates Wishart-ML on R
   requires the family to be closed under diagonal rescaling. The free
   family is (by construction); the diag-constrained family is **not** —
   the repo already holds direct evidence (`test-cpm_oracles.R` header: the
   (N−1)/N scalar rescale *shifts* the diag-constrained optimum). The
   consequence: T_diag = T_free + ΔT with ΔT ≥ 0, where ΔT = n(F̂_diag −
   F̂_free) is O_p(1) under the true model (σ̂_free − 1 = O_p(n^{-1/2}) ⇒
   ΔF = O_p(1/n)); observed ΔT ≈ 1.07 at N = 175 on df = 7. Since T_free is
   asymptotically χ²_df by invariance, T_diag is asymptotically
   stochastically *larger* than χ²_df by a nondegenerate O_p(1) amount —
   i.e., mildly anti-conservative for model rejection, with a
   truth-dependent magnitude. This is consistent with (though not the sole
   driver of) the DESIGN.md finding that the seeded KS check rejects
   χ²-calibration of T_diag in 5 of 6 field-N cells; the one passing cell
   (interior, N = 1000/2000 regime) may simply have a small ΔT at that
   truth and 500-rep power. I am **not** reopening the settled engine — the
   diag-constrained family remains a coherent estimator of a correlation
   structure and its empirical calibration record stands — but the design
   doc's §3.2 should be rewritten to stop implying its χ² validity follows
   from scale invariance (it doesn't; the invariance argument's true home
   is the free family), and the free family gives the package a direct
   experiment: add T_diag-vs-T_free calibration to the same coverage-oracle
   runs. If the gap is material at field N, the *vignette* guidance on
   which family to use for inference (not just for CIRCUM reproduction)
   should reflect it. This reframes the free family as potentially the
   statistically preferable default in a future major version — a question
   to measure, not decide here.
2. **ΔT is not a calibrated test of σ = 1.** Users will be tempted to read
   the nesting gap as a χ²_p LR test of "does free scaling matter". It is
   not: on correlation input the diagonal moments are degenerate, so the
   naive p-df reference is wrong. Report ΔT descriptively (or not at all);
   document.
3. **§3.2 text debt:** after M18, §3.2's remaining prose ("embedding
   Σ = D_σ P D_σ … reproduces any rescaling, with σ̂ = 1 at the optimum")
   should be replaced by the corrected statement (population anchor
   σ̂ = 1 iff perfect fit; finite-N projection preserves diag(Σ̂⁻¹R) = 1,
   not the diagonal), so the design doc no longer contains a claim its own
   change log refutes.
4. **Minor:** the brief's Background block quotes the B6 change-log entry
   as design doc "§11"; the entry's own text also corrects an RMSEA-CI
   guard inequality (B2 entry) — no bearing on this review, but the
   free-family fit-index code should reuse `cpm_rmsea_ci()` as-is rather
   than re-implement from the §5.3 prose, whose lower-guard inequality is
   stated backwards there.

## Recommendations

1. **Apply** — Implement the free-scaling family for v2.0.0 as a flag
   orthogonal to variants A–D, with the Q3 gradient exactly as derived
   (Ã = D_σAD_σ substitution in the γ blocks; ∂F/∂s = 2(1 − diag(Σ⁻¹R)));
   parameter layout [angles][u][s][v] keeping β trailing.
2. **Apply** — df via the covariance moment count (df unchanged); unit test
   df_free == df_diag; do not add p to q inside the p(p−1)/2 formula.
3. **Apply** — Extended FD gradient test (≥20 points, s ~ U(−0.35, 0.35),
   mixed criterion, conditioning redraw rule) plus the σ = 1
   legacy-gradient identity test plus the diag(Σ̂⁻¹R) = 1 stationarity
   invariant on accepted fits.
4. **Apply** — Internal oracle additions: exact-recovery with σ̂ = 1
   population anchor; engine-level rescale-equivariance (σ̂ → Dσ̂, all else
   invariant); nesting inequality on every fixture. Retire the B6
   model-difference allowances for the free family's own published-oracle
   comparisons (same-model §6.3 tolerances now apply); close the pending
   second human re-read of the Appendix A transcription first.
5. **Apply** — No analytic CIs for σ on correlation input, ever; report σ̂²
   as variance ratios (CIRCUM convention). Analytic CIs for θ/ζ/β allowed
   on the cormat path with the N-conditional caution, **gated on extending
   the coverage oracle with free-family cells** and re-deriving (or
   explicitly re-affirming) the caution constants. Bootstrap remains the
   raw-data default.
6. **Apply** — SRMR stays off-diagonal-only package-wide; encode the exact
   diagonal-residual conversion identity in the CircE oracle test; update
   the §6.3 item-6 checklist text.
7. **Apply** — Document AIC/BIC as within-scaling-family only; do not
   present ΔT as a calibrated σ = 1 test.
8. **Apply** — Rewrite design-doc §3.2 to the corrected invariance
   statement (Beyond-the-brief items 1/3) at M18 documentation time.
9. **Consider** — Add T_diag-vs-T_free calibration cells to the coverage-
   oracle runs; use the result to write the vignette's which-family-for-
   inference guidance, and to inform any future default change.
10. **Consider** — Bootstrap CIs for σ̂ on the raw-data path (statistically
    valid, not needed for CIRCUM compatibility); defer unless cheap.
11. **Reject (with reason)** — Pinning one σ (or a sum/product constraint)
    for identification: unnecessary — the map is injective and F is
    coercive in each σ_i; a pin would break CIRCUM comparability and the
    variance-ratio interpretation.
12. **Reject (with reason)** — Accepting covariance-matrix input as part of
    this milestone: scale invariance makes it statistically redundant
    (identical inference, σ̂ rescaled), and it would widen the input-
    validation surface mid-milestone; revisit as a separate candidate if
    users ask.

---

**GO** — the free-scaling family is the only route to exact reproduction of
published CIRCUM/CircE output, and this review found it statistically tame:
gradient derived and FD-verified, df unchanged, identification automatic,
canonicalization untouched, and the validation anchor already green.
