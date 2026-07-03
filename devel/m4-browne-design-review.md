# Brief A-review: adversarial statistical review of devel/m4-browne-design.md

**Reviewer:** Fable (fresh session, did not author the design), 2026-07-03.
**Scope:** review only; no package code, no edits to the design doc, no commits.
**Method:** the model was re-derived from Browne (1992) before reading the
doc's math, then diffed; every checkable claim was checked by computation
against a scratch implementation of the full estimator (scripts in the
session scratchpad: `check1.R`–`check7.R`; none touch the repo).
`devel/g2xx1.txt` was not used for anything, per the oracle rule.

---

## VERDICT

**Needs changes first — but the changes are confined to the CI-defaults,
convergence-acceptance, canonicalization, and validation layers. The core
model, discrepancy, identification scheme, gradient algebra, df accounting,
and fit-index definitions are verified correct (computationally, not just by
reading), and the backend decision is confirmed. After the revisions in the
prioritized list below, this design is sound to hand to implementation.**

The single most important gap: the proposed **default** (analytic
Hessian-based CIs) demonstrably mis-covers at field-typical sample sizes, and
the design's validation strategy — matching published CIRCUM output — is
structurally incapable of detecting that, because CIRCUM uses the same
asymptotics. A method could pass every test in §6 and still ship CIs with
66–86% actual coverage at N = 500.

---

## Findings, ranked by severity

### F1 — HIGH. Analytic (Wald) CIs, the proposed default, have materially wrong finite-sample coverage at realistic N, and §6 cannot catch it

**The §3.2/§5.2 asymptotics are correct — verified — but converge slowly.**
Simulation with a full scratch implementation (quasi-circumplex, p = 8,
octant-truth angles, ML fit by `nlminb` with the doc's analytic gradient,
SEs = `(2/n)·H⁻¹` + the doc's delta-method back-transformations, evaluated at
the generating truth):

| Config | N | 95% CI coverage observed |
|---|---|---|
| ζ ∈ [.6,.75], β = (.30,.50,.15,.05), m = 3 | 500 | θ: .91–.96; **ζ: .80–1.00 (one config .66–.86 in an earlier run with ζ up to .9); β₃: .74** |
| same | 2000 | most parameters **.85–.93** (SEs ~20% too small) |
| ζ same, β = (.25,.45,.30), m = 2 (all β interior) | 500 | θ: .95–1.00; **ζ: 1.000 (SEs ~30% too large)** |
| either config | 50,000 | **.93–.98 everywhere; empirical SD / analytic SE ≈ 1.00** |

So: asymptotically the design's formulas are exactly right (this also
confirms the §3.2 scale-invariance argument for SEs — see Verified clean),
but at N = 500–2000 the coverage error is large and its **direction flips
with the β configuration** — under-coverage with a small trailing harmonic,
over-coverage with interior β. That is plausible-but-wrong territory: nothing
in the output would look broken. The mechanism is ill-conditioning of the
discrepancy Hessian at octant-like truths (condition number ~2×10³ even for
the interior-β config; smallest eigenvalues ~7×10⁻⁴), which shrinks the
quadratic regime the Wald theory relies on. T = n·F̂ itself was also mildly
miscalibrated at N = 500 when β₃ sat ~2 SEs from the boundary (mean 9.2 vs
df = 10, KS p = 8×10⁻⁴), and clean at N = 2000+.

**Why §6 misses it:** the §6.3 gate validates analytic CIs by reproducing
*published CIRCUM CIs*. CIRCUM's CIs come from the same information-matrix
asymptotics, so agreement validates fidelity to CIRCUM, not coverage. The
§3.2 fallback (Browne 1984 correlation-structure asymptotics) does not fix
this either — the naive and embedded/proper asymptotic SEs agree to 0.5–3%
here (measured); the problem is finite-N, not correlation-vs-covariance.

**Required:**
1. Add a **simulation-based coverage oracle** to §6.4: simulate from
   `P(γ₀)` at N ∈ {250, 500, 1000}, fit, check empirical coverage of both CI
   methods (loose bands, e.g. [.90, .98] at nominal .95, seeded). This is the
   missing test that separates "matches CIRCUM" from "actually covers".
2. Add a T-calibration simulation (T ~ χ²_df under in-family truth) — cheap,
   and it caught the near-boundary miscalibration above.
3. Reconsider `ci_method = "analytic"` as the default (open decision §9.2),
   or at minimum have `summary()` caution on analytic CIs below an
   N-threshold informed by the coverage oracle. Note the irony to document
   either way: the package's own M4 milestone exists partly because Wald-type
   CI trust is exactly what Zimmermann & Wright showed needs checking.
4. Feed this to Brief B: the CPM's own analytic CIs need the same
   trustworthiness treatment `ssm_ci_accuracy()` gives SSM CIs.

### F2 — HIGH (implementation-blocking, easy fix). The convergence-acceptance spec is wrong for `nlminb` at the doc's own tolerances

With `rel.tol ≤ 1e-12` (the §3.5 recommendation), **65–96% of demonstrably
correct fits** in my simulations (gradient norms ≤ 1e-6, median 2×10⁻⁸;
estimates that deliver exact .95 coverage at N = 50k) exit with
`nlminb` message **"singular convergence (7)"**, not convergence code 0.
Under §3.5's "never return estimates from a non-converged fit without a
warning" keyed to the convergence code, the majority of perfectly good fits
would warn; worse, §5.2's bootstrap "non-converged resamples are excluded
with a count warning" would silently discard most replicates or spam
warnings, corrupting bootstrap CIs.

**Required:** define convergence acceptance by a **scaled gradient-norm
criterion at the solution** (plus F̂ agreement across multi-starts), and
store the `nlminb` code as an advisory diagnostic only. The bootstrap
exclusion rule must key on the new criterion.

### F3 — MEDIUM. The reflection canonicalization rule mirrors the theoretical configuration for clockwise-keyed instruments

§2.3's rule — reflect so `(θ̂₂ − θ̂_ref) mod 360° ∈ (0°, 180°)` — is
deterministic, but it canonicalizes to a fixed CCW orientation of *scale 2*,
not to the user's theoretical configuration. If a user supplies angles keyed
clockwise (theory = 0°, 315°, 270°, 225°, …, perfectly legal input), then for
data that match theory *exactly*, the theory-matching solution has
`(θ̂₂ − θ̂_ref) mod 360 = 315 ∉ (0,180)` and is rejected in favor of its
mirror (0°, 45°, 90°, …): the scale with theoretical angle 270° is reported
at 90° — 180° off — and §5.1's side-by-side "estimated vs theoretical angles"
table shows catastrophic misfit on perfectly circumplex data. A user would
plausibly conclude their instrument lacks circumplex structure. Package
instruments are CCW-keyed, so defaults are safe — but nothing constrains
user-supplied `angles`.

**Required:** canonicalize by **proximity to the theoretical/starting
configuration** (choose the reflection minimizing the summed angular distance
from θ̂ to the theory angles), falling back to the doc's CCW-of-scale-2 rule
as the deterministic tie-break when theory is reflection-symmetric or the
distances tie. This is strictly more robust and preserves the doc's
determinism and its undecidable-case fall-through.

### F4 — MEDIUM. Multi-start jitter consumes the global RNG stream on the default path, violating DESIGN.md's reproducibility contract

DESIGN.md: "`ssm_analyze()` is the package's only entry point that consumes
R's global RNG stream." §3.5's "3–5 jittered starts (θ ± U(−30°, 30°)
noise)" makes `cpm_fit()` **point estimates** depend on the RNG state even
with `ci_method = "analytic"`. Two identical calls could return different
estimates when multimodality is real, and `set.seed()` semantics would
silently extend to a second entry point undocumented.

**Required:** use a deterministic jitter set (e.g., fixed offset patterns
±15°/±30° applied in a documented order), or an internal fixed-seed RNG that
provably does not touch the global stream — and update DESIGN.md's
reproducibility table when this ships. The bootstrap path consuming RNG is
fine and expected.

### F5 — MEDIUM. RMSEA CI spec is missing the λ_U = 0 branch

§5.3 guards `λ_L = 0` when `pchisq(T, df) ≥ .95` but not the symmetric edge:
when `pchisq(T, df) ≤ .05` (excellent fit — e.g., T = 20, df = 40 gives
pchisq ≈ .0034), `pchisq(T, df, ncp = λ_U) = .05` has **no root** and the
specified `uniroot` call errors. Verified numerically; with the guard the CI
is correctly [0, 0]. **Required:** add `λ_U = 0` when `pchisq(T, df) ≤ .05`.
(With both guards the construction is internally consistent: point estimate
√(max(F̂/df − 1/n, 0)) always lies inside the interval — checked.)

### F6 — MEDIUM-MINOR. SRMR (and CI-shape) conventions are not pinned, and SRMR mismatch is guaranteed to bite

§5.3 defines SRMR over off-diagonals only (denominator p(p−1)/2). The other
common convention includes the diagonal (denominator p(p+1)/2; lavaan's
default for covariance input), and since diagonal residuals are identically 0
here, the two differ by the factor √((p−1)/(p+1)) ≈ 0.88 at p = 8 — far
beyond any §6.3 tolerance. Whichever CircE/CIRCUM used, the O2 SRMR test
will "fail" (or be waved through) unless this is in the checklist.
**Required:** add to §6.5 convention traps and the §6.3 mismatch-diagnosis
checklist: (a) SRMR denominator; (b) whether CIRCUM/CircE CIs are symmetric
on the natural scale vs back-transformed from an unconstrained scale (decides
what the §6.3 CI gate should compare); (c) BIC's `ln N` vs `ln n`.

### F7 — MINOR. The m-cap justification is wrong for even p (the cap itself is merely conservative)

§1.4 justifies `m ≤ floor((p−1)/2)` by "frequencies k and p−k alias exactly
… so higher m is never identified in variants B/D". Checked at p = 8
equally spaced: the basis {cos(k·δ) : k = 0..4} has **rank 5** — the Nyquist
harmonic k = 4 = p/2 *is* identified (it self-aliases; its sine column
vanishes but the cosine column is the alternating vector) — while k = 5
aliases k = 3 exactly (max abs difference 3×10⁻¹⁵). So for even p the strict
B/D cap is floor(p/2), and the doc's floor((p−1)/2) excludes one identified
harmonic. Being conservative is defensible (the Nyquist term is fragile in
variant A where angles move), but the stated reason is false as written.
**Required:** fix the prose (present the cap as a deliberate conservative
choice) or allow m = p/2 for variants B/D only. Default behavior at p = 8
(m = 3) is unaffected.

### F8 — MINOR. Tolerance table has three internal inconsistencies

- **RMSEA within 0.002** is tighter than the rounding half-width (0.005) of
  2-decimal published values — the test would fail on rounding alone even
  for a perfect implementation. Needs "0.005 if published to 2 decimals".
- **Angles within 0.5° of whole-degree values** allows zero slack beyond
  rounding; either compare after rounding ours to the published precision, or
  state 0.5° + optimizer slack.
- **Gradient test "relative agreement ≤ 1e-7"**: with plain central
  differences my scratch check achieved max *relative* error 1.9×10⁻⁶ at
  h = 1e-6 — dominated by FD truncation on small-magnitude components, not by
  the analytic gradient. As mandated, the test would be flaky. Specify the FD
  step and a mixed absolute/relative criterion (e.g.,
  |g_a − g_fd| ≤ 1e-7·max(1, |g_fd|)), or use a two-step Richardson FD.

### F9 — MINOR. β-boundary polish: wording and the χ² reference

The df bookkeeping (a boundary parameter is not a free parameter) is a
defensible convention and correctly feeds RMSEA/CFI via the reduced df. Two
notes: (a) strictly, when β_k is truly on the boundary, T is asymptotically a
~½χ²_df + ½χ²_{df+1} mixture, so the reduced-df reference is the
conservative-leaning choice — consistent with the doc's stated concern, worth
one sentence so implementers don't "fix" it; (b) a *interior-index* harmonic
(k = 1 with β̂₁ → 0, β̂₂, β̂₃ > 0) can hit the boundary, so "report it as the
m-reduced model" should read "the harmonic-removed model" — m only decreases
when the top harmonic is dropped. Also: published CIRCUM fits with active
boundary constraints likely report the *unreduced* df; §6.3 checklist item
(3) covers this — keep it. Consider triggering the removal-refit probe at
β̂_k < 1e-2 (accept on ΔF̂ < 1e-8) rather than 1e-4, since softmax tails
stall slowly.

### F10 — MINOR. Warm-started bootstrap replicates can still cross to the mirror

§5.2 claims warm starts make reflection re-handling unnecessary. Usually
true, but for weakly-determined data a resample's nearest optimum can be the
mirror; one mirrored replicate silently corrupts the circular quantiles.
**Required:** cheap per-replicate guard — reflect the replicate if it is
angularly closer to the mirror of γ̂ than to γ̂ (deterministic, no RNG).
Relatedly, §3.5's multimodality flag should also fire for *distinct optima
with equal F̂* beyond reflection pairs (the non-identification signature),
not only for optima differing in F̂.

---

## Prioritized required changes

1. **(F1)** Add the simulation-based **coverage oracle** and T-calibration
   simulation to §6.4; revisit the analytic-CI default or add an
   N-conditional caution; record the finite-N coverage results as a known
   limitation in §5.2; cross-link to Brief B.
2. **(F2)** Respecify convergence acceptance as scaled-gradient-norm (+
   multi-start F̂ agreement); `nlminb` code becomes advisory; bootstrap
   exclusion keys on the new criterion.
3. **(F3)** Canonicalize toward the theoretical configuration; CCW rule
   demoted to tie-break.
4. **(F4)** Deterministic multi-start jitter (no global RNG on the default
   path); update DESIGN.md reproducibility table at ship time.
5. **(F5)** Add the `λ_U = 0` guard to the RMSEA CI.
6. **(F6)** Pin SRMR/CI-shape/BIC-N conventions in §6.5 and the §6.3
   checklist.
7. **(F7, F8, F9, F10)** Prose/spec fixes as itemized.

None of these disturb the model, discrepancy, parameterization, gradient, df,
or phasing sections.

---

## Verified clean (computational evidence)

All checks run against an independent scratch implementation written from
the design doc's formulas (and my own prior derivation from Browne 1992,
which matched the doc's §1 exactly: decomposition, Fourier ρ with the
Herglotz condition, bipolarity via min ρ = 2β₀ − 1 at m = 1, low-rank
Λ representation of rank ≤ 2m+1).

- **§3.4 analytic gradient (θ, ζ, β + logit/softmax chain rules), including
  the vectorized forms:** matches central finite differences of F at 25
  random feasible points (random θ, ζ, β, random PD R); max relative error
  1.9×10⁻⁶, i.e., FD-truncation-limited. No sign errors, no missing factor
  of 2, no diagonal leakage. The softmax chain correctly annihilates the
  diagonal β-contribution (checked algebraically: Σ_k c·β_k(δ_kl − β_l) = 0).
- **§1.4 df table:** A/B/C/D at p = 8, m = 3 → 10/17/17/24; variant A m = 1:
  df = 0 at p = 5, df = 3 at p = 6. All confirmed.
- **§5.3 F₀ = −ln|R|:** F(I; R) − (−ln|R|) = −4×10⁻¹⁶.
- **§2.3 reflection invariance:** F(2θ_ref − θ) − F(θ) = 0 exactly.
- **§6.4 exact-recovery round trip is achievable as specified:** recovery of
  a known γ₀ from P(γ₀) to ~1×10⁻¹⁰ in all parameters with F̂ ≈ 9×10⁻¹⁶
  from a jittered start — the stated 1e-6/1e-12 tolerances have margin.
- **§3.2 scale invariance:** the algebraic identity
  F(D_σ P(γ) D_σ; D R D)|_{σ=d} = F(P(γ); R) holds to exactly 0; a full
  embedded refit (free scale parameters, arbitrary rescaling d ∈ [0.5, 3])
  reproduces γ̂ and F̂ to optimizer tolerance with σ̂ recovering d. Naive
  (2/n)H⁻¹ SEs vs the properly scale-embedded covariance-model SEs agree to
  0.5–3% (angles ~3% at high ζ, ≤0.6% at moderate ζ) — the §3.2 invariance
  argument for SEs is *asymptotically* sound (the finite-N problem in F1 is a
  different phenomenon and hits both versions equally).
- **Asymptotic validity of the full CI pipeline:** at N = 50,000, empirical
  95% coverage is .93–.98 for every parameter (θ, ζ, β on the natural scale,
  through the doc's delta-method maps) in both test configurations, and
  empirical-SD/analytic-SE ratios ≈ 1.00.
- **T = n·F̂ ~ χ²_df calibration:** confirmed at N = 2000+ (KS p = .54–1.0,
  mean/var matching df); the N = 500 near-boundary deviation is recorded
  under F1.
- **RMSEA point/CI construction:** internally consistent away from the F5
  edge (point inside CI; pchisq conditions satisfied at both endpoints);
  CFI/TLI/AIC/BIC and the RMSEA point formula are the standard definitions
  correctly written in terms of the discrepancy.
- **Aliasing arithmetic:** cos(5δ) ≡ cos(3δ) on the p = 8 grid to 3×10⁻¹⁵
  (the k ↔ p−k claim is right; only the even-p Nyquist statement is off, F7).
- **§6 oracle set and protocol:** O1–O5 are the right sources in the right
  priority order (the model's author; the software being replaced; the
  convention-pinning tutorial; breadth; the Brief-B bridge). The
  two-session independent-transcription protocol and the "our F̂ at our
  optimum ≤ F at published estimates + 1e-8" superiority criterion are
  genuinely strong — the latter is the correct mathematical arbiter when
  rounded published values disagree. The blank oracle tables are, as
  intended, a feature. The internal-oracle suite (§6.4) is sound; its one
  material gap is the missing coverage/calibration simulations (F1).

## Backend decision: CONFIRMED

Native optimization, R-first with a profiling-gated RcppArmadillo port and
the R implementation retained as permanent oracle, OpenMx/lavaan as
Suggests-only test oracles — confirmed on the merits, with direct evidence:
my scratch implementation of the complete model + gradient + `nlminb` fit is
~100 lines of base R; a full p = 8, m = 3, N = 500 ML fit converges in well
under a second, and 400-replicate simulation batches (fit + Hessian each)
ran in minutes. There is no performance case for a day-one C++ core, and no
capability case for OpenMx/lavaan as engines (heavy dependencies; the
circular machinery — reflection, wrapping, circular CIs — would still be
hand-rolled around them; lavaan's m ≥ 2 constraint syntax is exactly the
fragile path the doc says it is). The OpenMx `mxAlgebra` cross-fit as a
Suggests test oracle is apt: independent optimizer, same discrepancy.
One addition from F2: the Phase-2 C++ port criterion should include
byte-agreement of the *convergence-acceptance decision* (gradient-norm
criterion), not just F and gradient values.

## Scripts

Scratch implementation and checks (session scratchpad, not in repo):
`check1.R` (gradient/identities/recovery/aliasing/RMSEA), `check2.R`–
`check5.R` (naive-vs-embedded SEs; coverage simulations at N = 500/2000/50k;
Hessian spectra; nlminb message tabulation), `check6.R`–`check7.R` (scale-
invariance embedding). Seeds fixed; all reproducible with base R only.
