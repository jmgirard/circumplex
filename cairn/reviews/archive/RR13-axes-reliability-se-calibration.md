# RR13: `axes_reliability()` standard-error calibration — Review Report

- **Date:** 2026-07-27
- **Reviewer:** Independent expert review (Fable), per `cairn/reviews/RB13-axes-reliability-se-calibration.md`
- **Scope:** whether the ~45% SE overstatement measured at M65 is the predicted
  correlation-as-covariance consequence or a defect; whether to correct or
  caveat; what replaces RR12 BC13
- **Verdict:** **The 1.452 is fully explained — it is the exact delta-method
  prediction for this estimator, not a defect. Correct the SEs (supersede
  D-026 holding 5), in a dedicated milestone; M65 ships now under the
  replacement criterion below.**

Materials read: `R/axes_reliability.R` (`axes_syntax()`, `axes_fit()`,
`axes_fit_cormat()`, the SE extraction at L1478–1490), `R/ssm_sem.R`
`sem_fit_cfa()` (L744–757), `R/axes_fiml.R`, `R/axes_reliability_oop.R`
L41–58, RR09 §2, RR12 (§1, §9, BC13), `devel/m65-fiml-heavy-cells.R`, the
committed fixture `tests/testthat/fixtures/m65-heavy-cells.rds`, and the
Table 3 component values banked in `cairn/references/strack2013.md`.

Evidence: I reproduced the headline fixture numbers, then ran four
reviewer-authored computations (mechanisms and values recorded inline; the
core derivation is in the Reproduction appendix so the corrective milestone
can re-derive every number):

- **W-A (analytic delta method):** because Σ(θ) is linear in θ, the ML
  estimator linearizes exactly as ξ̂1 − ξ1 ≈ tr(W(input − Σ)), with W built
  from the model's own derivative structure. Pricing `input` as a sample
  **covariance** matrix (what lavaan's normal-theory SEs do) gives
  n·avar = 2 tr(WΣWΣ); pricing it as the sample **correlation** matrix the
  estimator actually consumes (Jacobian dr_ij = ds_ij − ½ρ_ij(ds_ii + ds_jj);
  the diagonal has zero sampling variance) gives n·avar = 2 tr(W_cΣW_cΣ) with
  W_c = W off-diagonal, diag(W_c)_i = −Σ_{j≠i} W_ij Σ_ij. At the probe
  population (8 scales × 3 items, ξ1=.35, ξ2=.10, ζ1=.08, N=600):
  predicted reported SE **0.01677** (measured 0.01681/0.01692), predicted true
  sampling SD **0.01164** (measured 0.01158/0.01165), predicted ratio
  **1.4412** (measured 1.452, MC SE ≈ 0.052). The naive quantity reproduces
  the ML information-matrix value [(Δ′VΔ)⁻¹]₁₁ to 6 decimals, ruling out any
  extraction defect.
- **W-B (parameter-space sweep):** the same computation over 1540+ populations
  (k = 4–24 scales, m = 1–6 items, components spanning Table 3's range).
  Ratio range [**0.81**, **1.97**]; driven primarily by ξ1; equals 1.001 at
  near-zero components. At Strack's own published configurations: COC Other
  (ξ1=.032, ξ2=.467) → **0.989** (anti-conservative), COC Meta → 0.997,
  CV-LI Other → 0.998, IIP S5-t2 → 1.074, IAL S1 Self → **1.300**.
- **W-C (live lavaan experiments, lavaan 0.6.21, draw seed 1001):**
  `correlation = TRUE` on the same syntax: npar drops to 3 (item errors
  determined, implied diagonal exactly 1 — the unit-total-variance class
  RR12 §9 rejects), ξ̂1 moves 0.378 → **0.436**, and raw-data
  `missing = "ml"` errors ("correlation structures only work for complete
  data"). `se = "robust.huber.white"` on the z-data: SE 0.01889 vs reported
  0.01787 — no fix. A pipeline bootstrap that re-standardizes per resample
  (200 boots): SD 0.01136, consistent with the delta prediction.
- **W-D (implementable correction, validated):** evaluating W-A at the
  *fitted* Σ̂ of each replicate (~40 lines of base R, no lavaan) over 50
  complete-data replicates: mean corrected SE(ξ1) **0.01164** against the
  fixture's empirical SD 0.01158 — calibration **1.005**. Applied
  multiplicatively to the FIML path (observed-information SE ÷ ratio at Σ̂),
  against the committed 200-replicate fixture: calibration **1.001 / 1.008 /
  1.018** at 2/5/10% MCAR.

---

## 1. Is 1.452 the expected magnitude?

**Yes — exactly. It is the analytically predicted consequence of analyzing
the correlation matrix as a covariance matrix for this specific structure,
derived to three decimals, and there is no separate defect anywhere in the SE
extraction, the model specification, or lavaan's treatment.**

The derivation is tractable precisely because the implied covariance is linear
in the parameters. Both sides of the ratio were derived, not simulated:

- **The reported SE is what normal-theory covariance ML must report.** The
  information-matrix value at the probe population predicts SE(ξ̂1) = 0.01677
  at N = 600; the measured mean reported SE is 0.01681 (listwise) / 0.01692
  (FIML at 5% MCAR) — agreement to 0.3%. The trace identity
  2 tr(WΣWΣ) = [(Δ′VΔ)⁻¹]₁₁ holds to 6 decimals, so lavaan's number is the
  textbook number and the extraction at L1478–1490 reads the right cell.
- **The true sampling SD is what correlation input must produce.** A sample
  correlation matrix is less variable than a sample covariance matrix of
  unit-variance normals — var(√n·r_ij) = (1−ρ²)² against (1+ρ²) — and its
  diagonal does not vary at all. Composing the estimator's linearization with
  the covariance→correlation Jacobian predicts SD = 0.01164; measured 0.01158
  (complete listwise), 0.01165 (FIML 5% MCAR).
- **Ratio: predicted 1.4412, measured 1.452.** The MC SE of the measured
  ratio over 200 replicates is ≈ 3.6% (≈ 0.052), so the measurement sits 0.2
  MC SEs from the prediction.

Why ξ1 specifically is hit so hard: ξ̂1 is, to first order, a
cos(Δ)-weighted average of the off-diagonal correlations, and at this
population the cos-weighted moments run through the largest |ρ| cells
(same-scale r = .53, adjacent-scale r ≈ .45), where the covariance-vs-
correlation variance gap — (1+ρ²) vs (1−ρ²)² — is widest. The other
components are barely affected at this population (question 2, per-parameter
table), which is additional evidence of mechanism rather than a generic
inflation bug.

RR09 §2 / D-026 holding (5) were right that the point estimates are correct
and the SEs approximate; what neither measured is that "approximate" here
means ×1.44 at a strong-axes population. The magnitude was never estimated
until M65, and it is exactly the Cudeck (1989) magnitude for this design.

## 2. Is the direction and size stable, or population-dependent?

**Population-dependent in size and — in a corner of the space — in sign.
"Conservative" is not a safe blanket description.**

Measured properties of the analytic ratio (W-B):

- **Driver.** The ratio grows primarily with ξ1 (and secondarily with overall
  correlation level). At near-zero components it is 1.001; at ξ1 = .50 it
  reaches ~1.97. At the probe population (ξ1 = .35 — higher than any
  two-axis row in Strack's Table 3) it is 1.441.
- **Per-parameter, at the probe population:** ξ1 **1.441**, ξ2 **1.067**,
  ζ1 **0.997**. The miscalibration concentrates in exactly the component
  reliability is read from.
- **Real instruments.** Over Table 3's published component configurations the
  ratio spans **0.989 to 1.300**: IAL-class strong-axes instruments ~1.3,
  IIP-class ~1.07, and the weak-axes/strong-general rows (COC Other:
  ξ1 = .032, ξ2 = .467) dip *below* 1 — reported SEs ~1% too **small**.
- **The anti-conservative region is real but bounded.** Pushing beyond the
  published range (ξ1 ≤ .05 with ξ2 ≈ .6, many items) drives the ratio to
  **0.81** — SEs 19% too small. Within the range instruments actually
  publish, anti-conservatism does not exceed ~1%; but nothing in the code
  refuses the region where it is material.
- **N-invariance.** The ratio is asymptotically N-free, and the fixture
  confirms it empirically across an order of magnitude of effective N: the
  listwise path under deletion holds ratio 1.42–1.47 from N = 600 complete
  down to the ~48 surviving rows of the 10% MCAR cell.
- **Missingness-invariance at mild rates.** FIML ratios 1.443/1.452/1.467 at
  2/5/10% MCAR — the drift is the observed-information SE correctly pricing
  missing information (numerator grows) while the correlation-map error stays
  fixed (question 4).

So the risk profile: for the instruments this function was built for
(octant IPC instruments with substantial axes variance) the SEs are
materially conservative, understating precision by 25–45%. For weak-axes,
strong-general instruments — the very instruments whose low reliability most
needs honest uncertainty — the SEs drift to the anti-conservative side,
albeit only by ~1% at published configurations. A static caveat cannot state
this honestly without effectively printing the correction.

## 3. Should the SEs be corrected rather than caveated?

**Yes. Supersede D-026 holding (5) and RR09 §2's "document, don't fix". The
recommended route is the delta-method correlation-structure correction
(the Browne/Cudeck corrected asymptotic covariance, specialized to this
linear model), implemented in ~40 lines of base R with no new dependency,
measured to calibrate at 0.5–1.8% on the existing fixture.**

Why the standing holding falls: RR09 justified "document, don't fix" on
(i) faithfulness to the paper's own LISREL practice and (ii) the SEs being
"approximate". Both grounds are now outweighed by measurement: the
approximation is ×1.44 at strong-axes populations and sign-unstable across
the parameter space (question 2), so the printed caveat ("approximate") does
not put a user within a factor of the truth, and no caveat wording short of
the correction itself can. Faithfulness is preserved where it matters — the
point estimates, the model class, and the df are untouched; the paper's
printed SEs simply carry the same defect this correction removes, which the
docs should say.

Candidates assessed:

- **Delta-method corrected asymptotic covariance (apply).** For each reported
  component variance, SE_corr = √(2 tr(W_c Σ̂ W_c Σ̂)/n), with W built from
  the analytic derivative structure {C, J, B, (K,) E_ii} at the fitted Σ̂ and
  W_c the correlation-map transform (Reproduction appendix). This is exactly
  the Cudeck/Browne correlation-structure correction; for this model it needs
  no numerical derivatives and no new dependency, and it runs identically on
  all three input paths — raw listwise, **cormat** (it consumes only Σ̂ and
  n, so the no-raw-data path is fully served), and FIML (question 4).
  Validated: corrected/empirical = 1.005 (complete data, 50 reps evaluated at
  per-replicate Σ̂), 1.001/1.008/1.018 (FIML at 2/5/10% MCAR, 200-rep
  fixture). Cost: ~40 lines plus tests; O(q·p³) per fit, milliseconds at
  p = 24.
- **lavaan `correlation = TRUE` (reject).** Measured: it fits a different
  model class — npar collapses to 3, the item errors become determined so
  the implied diagonal is exactly 1 (the per-item unit-total-variance
  constraint RR12 §9 rejects and this brief's constraints keep rejected), and
  the point estimate moves materially (ξ̂1 0.378 → 0.436 on draw 1001,
  ≈ 5 empirical SDs), violating this brief's own "point estimates are not in
  question" constraint. It also refuses `missing = "ml"` ("correlation
  structures only work for complete data") and is documented experimental.
- **Robust/sandwich SEs (reject).** Measured no fix: se = "robust.huber.white"
  on the z-columns gives 0.01889 against the naive 0.01787 on the same draw.
  The sandwich estimates the fourth-moment variability of sample
  *covariances* of the z-rows; it is structurally blind to the in-sample
  standardization that pins the diagonal, which is the entire effect.
- **Nonparametric bootstrap over respondents (consider, as oracle only).**
  Valid **only** if each resample re-runs the whole pipeline including
  re-standardization — lavaan's built-in `se = "bootstrap"` on the z-matrix
  does not re-standardize and would reproduce the covariance-metric
  variability, not fix it. A correct pipeline bootstrap measured SD 0.01136
  on draw 1001 (consistent with the delta value 0.01184 at that draw's Σ̂).
  Costs: infeasible on the cormat path (no rows) and expensive under FIML
  MAR (18–68 s per structured fit). Right role: an independent oracle in the
  corrective milestone's evidence, not the shipped estimator.
- **Refit on the covariance metric with unit-variance constraints (reject).**
  Foreclosed by the standing constraint (RR12 §9's first rejection stands),
  and on the merits: determined errors change df and fit class out of the
  paper's tau-equivalent model, and `correlation = TRUE` above is the
  measured demonstration that this class moves the point estimates.

One boundary on the recommendation: the correction fixes the component
**SEs**. The global χ² and fit indices carry the same approximation in the
other direction (measured E[T] = 261.1 against df = 273 at the probe
population — fit statistics flattered by ~4%) and should retain a caveat;
a scaled test statistic from the same Γ machinery is possible later but is
not part of this recommendation (Beyond the brief, B-2).

## 4. Does the FIML path change the analysis?

**No new error and no cancellation: there is one shared error, and the FIML
path's observed-information machinery is doing its own job correctly on top
of it.**

The brief's premise — "conditioning on estimated constants normally makes SEs
too small" — does not describe what happens here, because the reported SEs do
not *condition-and-shrink*; they **omit the standardization map entirely**
(they price vech(S)-variability while the estimator consumes vech(R)).
That omission *is* the correlation-as-covariance error — on both paths. The
listwise path conditions on `scale()` constants exactly as the FIML path
conditions on saturated-EM constants; in both cases in-sample standardization
*reduces* the estimator's true variability (the input diagonal is pinned),
so the unpriced map makes the reported SEs too **large**, not too small.
One error, not two cancelling.

Why the ratios match to three decimals: at ≤10% cellwise MCAR every
FIML-specific effect is second order, and the first-order effect that is
present is being priced correctly. Decomposed from the fixture: the reported
FIML SE rises with the missingness rate (0.01686 → 0.01692 → 0.01705 at
2/5/10%) — the observed information charging the missing information, as
RR12 §3 required — while the empirical SD stays ~0.0116. Dividing the
reported FIML SE by the deterministic delta ratio at Σ̂ leaves residual
miscalibration of **0.1% / 0.8% / 1.8%** at 2/5/10% MCAR. That residual is
the true size of everything the complete-data correction does not price at
these rates: the saturated-constants uncertainty and the missingness
correction to Γ jointly. It is an order of magnitude smaller than the effect
under correction and currently smaller than the MC noise of a 200-replicate
band.

So: **the same correction serves the FIML path**, composed multiplicatively —
corrected FIML SE = observed-information SE × (corrected/naive ratio at Σ̂) —
which preserves the observed-information pricing of the missingness while
removing the metric error. The standardization uncertainty does **not** need
separate propagation at the missingness levels the evidence covers; the
corrective milestone must verify the composition where the brief's evidence
does not reach (the 15% headline cell and the M1 MAR mechanism; BC5 below).
If those cells fall outside the band, the escalation path is the exact
route — Γ_R from the saturated FIML fit's observed-information acov,
delta-transformed to correlations — or the pipeline bootstrap; do not widen
the band.

## 5. Is RR12's BC13 band defensible, and what should replace it?

**BC13's *intent* (SE honesty) was right; its *criterion* was wrong, because
it silently assumed the reported SE targets the sampling SD of the estimator
as implemented. The shipped SE targets the sampling SD of a covariance-input
estimator that has never existed in this package, whose truthful ratio to the
real SD is a model-determined constant ≈ 1.44 at the probe population — so
no implementation of either path could have satisfied a band centered at 1.**

The property that actually discriminates a correct SE implementation from an
incorrect one is **agreement with the estimator-specific analytic
prediction**, which this review has now derived for both the naive and the
corrected estimator. Replacement criteria, by scenario:

- **For the corrected implementation** (the corrective milestone): a
  two-anchor criterion. (i) A deterministic anchor — on the exact probe
  population matrix the implementation must reproduce the derived values
  (naive SE(ξ1) 0.01677, corrected 0.01164 at n = 600) within 2e-4, which a
  broken Jacobian or a wrong weight matrix cannot pass by luck; (ii) an
  empirical anchor — mean corrected SE / empirical SD over ≥200 replicates in
  **[0.90, 1.10]**. Band justification: the MC SE of an empirical SD over
  200 replicates is ≈ 1/√(2·199) ≈ 3.6%, so the band is ≈ ±2.8 MC SEs —
  false-alarm-safe, and the uncorrected estimator fails it by ~12 MC SEs.
- **For M65 shipping before the correction** (the recorded-deviation route):
  BC13's band is *replaced*, not widened: the measured ratio at the 5% MCAR
  cell must lie in **[1.31, 1.57]** — the analytic prediction 1.441 ± 0.13
  (≈ ±2.5 MC SEs). This is a justified criterion, not a fitted one: the
  center comes from theory (derived before comparison, reproducible from the
  appendix), the width from MC error, and it still discriminates — an SE
  extraction bug, a wrong information matrix, or a metric regression moves
  the ratio off 1.44 and reddens it, which a band around 1.0 could never do
  (it was red for every correct implementation).

## 6. Scope and compatibility

**The correction belongs in its own milestone, not in M65. M65 ships now,
with the BC13 deviation recorded under the question-5 replacement criterion
and the caveat strengthened to quantify.**

- **Why not M65:** the correction changes the shipped listwise and cormat
  paths — strictly larger than M65's FIML scope — and it supersedes a
  recorded design decision (D-026 holding 5), which under this repo's
  doctrine takes its own plan, its own tests-first build against the
  criteria below, and its own review gate. M65's FIML path adds no
  miscalibration of its own (identical ratio to three decimals), so holding
  it hostage buys no user any accuracy.
- **Deprecation cycle: not warranted.** The SEs were always documented as
  approximate; making them accurate is a fix to auxiliary output, not an
  interface or estimand change. Point estimates, reliability, SEm, df, and
  fit statistics are all unchanged. The maintainer's explicit pre-1.0 waiver
  covers the residual formality.
- **Release note (draft):** "The component standard errors reported by
  `axes_reliability()` are now calibrated. Previously they were computed as
  if the item correlation matrix were a covariance matrix (the source
  paper's practice, documented as approximate since release): for
  strong-axes instruments this overstated SE(axes variance) by 25–45%, and
  for weak-axes/strong-general instruments it could understate it slightly.
  Point estimates, reliabilities, and SEm are unchanged. Corrected SEs are
  typically *smaller* than those printed in Strack et al. (2013), whose
  LISREL values carry the same approximation."
- **Docs:** the shared caveat (`axes_se_caveat`) drops its SE clause after
  the correction and keeps the χ²/global-fit clause (question 3); the FIML
  caveat sentence keeps its MAR/normality clauses and drops
  "remain approximate for the same correlation-as-covariance reason".

---

## Beyond the brief

- **B-1.** The χ²/fit-statistic side of Cudeck's caveat is quantifiable with
  the same machinery: E[T] = tr(UΓ_corr) = 261.1 against df = 273 at the
  probe population — the test is mildly conservative-toward-fit (RMSEA and
  p-values flattered by ~4%). Worth one docs sentence now; a scaled statistic
  (Satorra–Bentler-type with Γ_corr) is a possible later milestone, low
  priority.
- **B-2.** The per-parameter pattern (ξ1 1.44, ξ2 1.07, ζ1 1.00 at the probe
  population) means users comparing component SEs across rows of the
  `components` table have been comparing differently-miscalibrated numbers.
  The correction fixes the comparison as a side effect; the release note need
  not call it out.
- **B-3.** The anti-conservative corner (ratio 0.81 at ξ1 ≤ .05, ξ2 ≈ .6,
  large p) is outside every published Table 3 configuration but inside the
  function's accepted input space. No refusal is warranted — after the
  correction the region is simply *correct* — but until the correction
  lands, the strengthened caveat must not claim the approximation is safely
  conservative.
- **B-4.** The fixture's listwise columns at 2/5/10% MCAR (ratios
  1.42/1.44/1.47 at effective N ≈ 460/280/48) are a free N-invariance
  regression asset: the corrective milestone can assert calibration across an
  order of magnitude of N without new simulation.
- **B-5.** `ssm_sem()` is not implicated: it lives on the covariance metric
  (RR12 B-5), where normal-theory SEs price the input it actually consumes.
  No other estimator in the package analyzes a correlation matrix as
  covariance.

## Recommendations

1. **Apply.** Plan a dedicated corrective milestone (the next free slot)
   implementing the delta-method correlation-structure correction for every
   reported component SE on all three input paths, under BC1–BC7 below;
   supersede D-026 holding (5) with a new decision recording this report as
   authority. Model tier: Fable for the estimator change and its review
   (plausible-but-wrong statistics is the failure mode); Sonnet for the docs
   and NEWS mechanical edits.
2. **Apply.** M65 proceeds to review now: record the BC13 deviation in its
   "Deviations from RR12" table citing this report, replace the band per
   question 5 ([1.31, 1.57] at the 5% MCAR cell), and strengthen the two
   caveats to quantify ("for strong-axes instruments the reported component
   SEs can overstate sampling variability substantially — ×1.4 at axes
   variance .35 — and are slightly understated for weak-axes,
   strong-general instruments").
3. **Apply.** In the corrective milestone, validate against two independent
   oracles: the analytic anchors (BC2) and a pipeline bootstrap that
   re-standardizes per resample (BC6) — never lavaan's built-in
   `se = "bootstrap"`, which does not re-standardize.
4. **Consider.** One docs sentence on the χ² direction (B-1); the
   Satorra–Bentler-style scaled statistic as a future candidate; folding the
   per-instrument ratio table (W-B) into the vignette as a reader's guide to
   what changed.
5. **Reject (with reason).** lavaan `correlation = TRUE` — different model
   class (determined errors, npar 3), moves point estimates ~5 empirical SDs,
   refuses missing data, experimental. Robust/sandwich SEs — measured no fix;
   blind to in-sample standardization. Any caveat-only resolution — the
   approximation is sign-unstable across the accepted input space, so no
   static sentence states it honestly (question 2). Scalar effective-N or
   post-hoc rescaling repairs — remain rejected per RR12 §9.

## Binding criteria

Tolerances are absolute unless stated. "MC SE" = SD across replicates / √R.
BC1–BC6 bind the corrective milestone; BC7 binds M65 if it ships first.

- **BC1 (the correction).** Corrected component SEs are computed as
  SE_corr = √(2·tr(W_c Σ̂ W_c Σ̂)/n), where W = ½ Σ̂⁻¹(Σ_s c_s M_s)Σ̂⁻¹ with
  {M_s} the model's derivative matrices {C, J, B (, K), E_11…E_pp}, c the
  target parameter's row of (Δ′VΔ)⁻¹, and W_c equal to W off the diagonal
  with diag(W_c)_i = −Σ_{j≠i} W_ij Σ̂_ij. Implemented in base R (no new
  dependency; lavaan/OpenMx stay Suggests), evaluated at the fitted Σ̂,
  identical code on the data and cormat paths, and applied to every SE the
  `components` table reports (ξ1, ξ2, ζ1 and ζ2 when fitted).
- **BC2 (deterministic anchors).** Fitting the exact probe population matrix
  (8 scales × 3 items, ξ1=.35, ξ2=.10, ζ1=.08) at n = 600: the uncorrected
  SE(ξ1) must equal 0.01677 within 2e-4 and the corrected SE(ξ1) must equal
  0.01164 within 2e-4; the corrected/uncorrected ratio for (ξ1, ξ2, ζ1) must
  equal (1/1.441, 1/1.067, 1/0.997) within 0.01 each.
- **BC3 (empirical calibration, complete data).** Over ≥200 complete-data
  replicates at the probe population (the fixture seeds may be reused), mean
  corrected SE(ξ1) / empirical SD(ξ̂1) ∈ [0.90, 1.10] (band ≈ ±2.8 MC SEs;
  measured 1.005 over 50 replicates in this review).
- **BC4 (FIML composition).** The FIML path's corrected SE is the
  observed-information SE divided by the same per-parameter ratio evaluated
  at Σ̂. Against the committed 200-replicate fixture at 2, 5, and 10% MCAR,
  mean corrected FIML SE(ξ1) / empirical SD ∈ [0.90, 1.10] in every cell
  (measured 1.001/1.008/1.018).
- **BC5 (beyond-mild-missingness check).** One cell each at 15% MCAR
  (the BC14 headline fixture population) and mechanism M1 MAR with enough
  replicates that the MC SE of the SD is ≤ 5%: corrected SE / empirical SD
  ∈ [0.85, 1.15]. If either cell fails, escalate to the exact route
  (Γ_R from the saturated fit's observed-information acov, delta-transformed)
  or the pipeline bootstrap — the band must not be widened.
- **BC6 (independent oracle).** On ≥2 complete-data draws, a pipeline
  bootstrap (≥200 resamples, re-computing the correlation matrix per
  resample) must agree with the corrected SE(ξ1) within 15% relative; the
  test must not use lavaan's `se = "bootstrap"`.
- **BC7 (M65's replacement criterion).** If M65 ships before the corrective
  milestone: RR12 BC13's [0.85, 1.15] band is replaced by — at the 5% MCAR
  fixture cell, mean reported SE(ξ1) / empirical SD(ξ̂1) ∈ [1.31, 1.57]
  (analytic prediction 1.441 ± ≈2.5 MC SEs); the deviation is recorded in
  M65's "Deviations from RR12" table citing this report; and both printed
  caveats state magnitude and direction-dependence per Recommendation 2.
  The band is not a widening of BC13: it is centered on the derived truthful
  value of the shipped estimator's ratio, not on the measurement.

## Reproduction appendix

The core derivation (base R, no dependencies). `naive` reproduces
[(Δ′VΔ)⁻¹]₁₁; `actual` is the sampling variance of the estimator as
implemented; both are n·avar, so SE = √(value/n).

```r
ratio_fn <- function(k, m, xi1, xi2, zeta1) {
  ang <- rep(seq(0, 360 - 360/k, by = 360/k), each = m) * pi/180
  scl <- rep(seq_len(k), each = m); p <- k * m
  C <- cos(outer(ang, ang, "-")); J <- matrix(1, p, p)
  B <- (outer(scl, scl, "==")) * 1
  P <- xi1*C + xi2*J + zeta1*B; diag(P) <- 1
  Pi <- solve(P)
  derivs <- c(list(C, J), if (m >= 2) list(B),
              lapply(seq_len(p), function(i) {
                E <- matrix(0, p, p); E[i, i] <- 1; E }))
  PiM <- lapply(derivs, function(M) Pi %*% M)
  q <- length(derivs); I <- matrix(0, q, q)
  for (s in seq_len(q)) for (t in s:q)
    I[s, t] <- I[t, s] <- 0.5 * sum(PiM[[s]] * t(PiM[[t]]))
  cvec <- solve(I)[1, ]   # row 1 = xi1; rows 2/3 give xi2/zeta1
  W <- 0.5 * Pi %*% Reduce(`+`, Map(`*`, derivs, cvec)) %*% Pi
  WP <- W %*% P; naive <- 2 * sum(WP * t(WP))
  Wc <- W; diag(Wc) <- 0; diag(Wc) <- -rowSums(Wc * P)
  WcP <- Wc %*% P; actual <- 2 * sum(WcP * t(WcP))
  c(naive = naive, actual = actual, ratio = sqrt(naive / actual))
}
ratio_fn(8, 3, .35, .10, .08)
#  naive 0.168818   actual 0.081274   ratio 1.441229
#  sqrt(naive/600) = 0.01677 (measured mean reported SE 0.01681/0.01692)
#  sqrt(actual/600) = 0.01164 (measured empirical SD 0.01158/0.01165)
```

The fixture check (`tests/testthat/fixtures/m65-heavy-cells.rds`) reproduces
ratios 1.443/1.452/1.467 (FIML) and 1.422/1.437/1.470 (listwise under
deletion) at 2/5/10% MCAR; the multiplicative FIML repair divides `fiml.se`
by 1.4412 and compares to `sd(fiml.xi1)` per cell (1.001/1.008/1.018).
The `correlation = TRUE`, robust-sandwich, and bootstrap measurements used
lavaan 0.6.21 on the seed-1001 draw of `axes_simulate(600, octants(), 3,
.35, .10, .08)` via `circumplex:::axes_syntax()` / `axes_fit_cormat()`.

## Conclusion

The measured 1.452 is the exact, derivable behavior of normal-theory
covariance ML applied to a correlation matrix under this model — reproduced
analytically to 0.2 MC SEs, with no defect in the model, lavaan's treatment,
or the SE extraction. The conservatism is real but population-dependent and
sign-unstable, so the honest resolution is correction, not caveat: the
delta-method correlation-structure correction is dependency-free, exact for
this linear structure, serves all three input paths including cormat, and is
already validated to ≤1.8% against the committed fixture. M65 carries none of
this defect beyond what it inherited and should ship under the replacement
criterion; the correction supersedes D-026 holding (5) in its own reviewed
milestone.
