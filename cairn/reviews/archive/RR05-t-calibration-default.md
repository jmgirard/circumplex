# RR05: T_diag-vs-T_free inference-default decision — review report

- **Date:** 2026-07-16
- **Brief:** `cairn/reviews/RB05-t-calibration-default.md`
- **Reviewer:** independent Fable review (no conversation context assumed)

## Evidence verification (performed before answering)

All headline numbers were re-verified directly against
`devel/m21-t-calibration-results.rds`:

- 12 cells × 500 reps; used n per cell matches the table in
  `devel/m21-t-calibration.md` (412…500); total used 5,751.
- Paired mean `T_free − T_unit` ∈ [−0.0436, −0.0111] (≤ 0.44% of df = 10);
  minimum paired correlation .99799 (boundary_N1000); all other cells ≥ .998.
- Nesting violations 3/5,751 (max +5.52, boundary_N2000), as reported.
- α = .05 rejection decisions discordant in only 2/5,751 replicates
  (exact-binomial 95% upper bound on the rejection-rate difference
  ≈ 0.13 percentage points).
- Committed cross-references (`$committed`: m19 stage-3 free ks_T, m4
  stage-1 diag ks_T) present and consistent in direction.

Additionally, the two worst-exclusion cells (N = 250; 88 and 122 excluded)
were regenerated from the exact seeds to audit what the rds does not store —
the family breakdown of exclusions: not-accepted unit 52 vs free 53
(boundary), 99 vs 105 (interior); polished 23 vs 25 (boundary), 0 vs 0
(interior); kept counts reproduce the rds exactly (412, 378). The paired
keep-conditioning is family-symmetric in the margins and does not bias the
comparison toward either family.

## Answers

### 1. Decision: keep the unit family as the CPM model-test inference default

**Yes — the unit family should remain the inference default; the free family
stays the opt-in reproduction feature. No conditional default is warranted.**

Three independent grounds:

1. **The measured tie is real and tight.** No calibration metric (mean/df,
   var-ratio, rejection at α = .05, KS regime) separates the families in any
   of the 12 cells. The paired design makes this a strong null: with paired
   correlation ≥ .998, the standard error of the per-cell mean difference is
   ≈ 0.013 on the T scale (sd(T) ≈ √20 ≈ 4.5; sd(diff) ≈ 4.5·√(2(1−.998))
   ≈ 0.28; /√500), so a calibration difference of even 1% of df would have
   been detected decisively. The observed differences (0.1–0.4% of df,
   uniformly negative, shrinking in N) are consistent with the free family's
   σ̂ absorbing a second-order sliver of finite-N off-diagonal misfit —
   nothing more.

2. **The tie is structurally expected, not a coincidence of the two truths.**
   Under the correlation-input contract, `diag(R) = 1` identically: the p
   diagonal moments the free family adds carry zero sampling variability, and
   the p σ parameters have essentially nothing to fit (σ̂ ≈ 1, deviating only
   to trade against off-diagonal misfit — the same mechanism D-010 measured
   as median max variance-ratio ≈ 1.00). The df bookkeeping (+p moments, +p
   parameters, `R/cpm_fit.R:156-162`) exactly cancels, and the statistic
   itself nearly coincides replicate-by-replicate. Under this contract there
   is no room for the free family to be *better* calibrated: whatever
   calibration one family has, the other shares. (I state this as a
   structural expectation supported by the data, not a proved theorem; the
   measured gap is tiny and decreasing in N at both truths.)

3. **Against a null benefit stand known costs.** The free family's bordered
   information matrix is singular (NA SEs) in ~52–55% of N = 250 fits and
   13–14% at N = 1000 (D-010); it carries p extra parameters; σ̂² never
   carries an interval (D-009). A default should not steer users into a
   family whose SE surface degrades at realistic N when its test statistic
   buys nothing. Additionally, `scaling = "unit"` is the shipped pre-M18
   behavior; changing an exported default requires positive evidence, and
   there is none.

A conditional default (by N, truth region, or anything else) has no support:
no condition boundary exists in the evidence, and it would add API complexity
to deliver a statistic that differs by < 0.5% of df.

### 2. Sufficiency: yes — no further runs are required before the D-entry

The design is sufficient at the v2.0.0 ship bar, for reasons that go beyond
rep count:

- **The burden is asymmetric.** The decision confirms the status quo.
  Switching a default needs positive evidence of benefit; the paired design
  at 500 reps is amply powered (SE ≈ 0.013 T-units per cell, see 1.1) to
  have found any benefit worth acting on. It found none.
- **More correlation truths would be redundant.** The structural degeneracy
  (diag R = 1 ⇒ σ block near-inert) is truth-independent within the
  correlation-input contract. Additional truths, p, m, or df values would
  re-measure the same forced tie. The D-entry should state the measured
  envelope honestly (two truths, df = 10, p = 8, m = 3, variant A,
  N ∈ [250, 50000]) and lean on the structural reason for generality.
- **Polished-replicate stratification: not needed.** Polish changes q and df
  identically for both families (shared `cpm_spec_reduce`), and the
  unpolished-only convention is the right one for a calibration question
  (a removed harmonic changes the reference distribution's df).
- **Exclusion conditioning is clean.** My regeneration audit (above) shows
  marginal non-acceptance and polish rates are family-symmetric at the
  worst cells; the paired keep does not favor either family. (Per-replicate
  acceptance is discordant in 25/500 and 40/500 replicates at N = 250 —
  optimizer noise near the reproduction criterion, symmetric in the margins,
  not decision-relevant.)
- **Variants B–D spot checks: not required as a gate.** The σ block is
  orthogonal to variant structure and both families share one df formula
  (`R/cpm_fit.R:147-162`); the structural argument is variant-independent.
  A single smoke-level paired run at one non-A variant would be
  belt-and-suspenders — see recommendations (consider, not gate).

**No run is required before the D-entry is written.**

### 3. Scope caveat: the re-trigger is right, with two refinements

The condition — revisit the default decision if/when covariance-matrix input
ships (D-009 item 4) — is the correct and, under the current contract, the
*only possible* trigger: with `cor()` discarding variances, there are no
other well-posed truths, so nothing short of a covariance-input path can
change the answer. At genuine covariance input the p diagonal moments carry
real sampling noise, σ̂ becomes a genuine estimator, and T_free becomes a
genuinely different statistic with unmeasured calibration — the present
equivalence says nothing about that regime.

Two refinements to how the D-entry states it:

- **(a) Make the re-measure a gate inside the covariance-input milestone,
  not a post-hoc revisit.** Phrase it as: any milestone shipping
  covariance-matrix input must re-run the paired T calibration at non-unit
  σ truths *before* that feature ships, and the default decision is reopened
  there. "Revisit if/when it ships" is ambiguous about ordering.
- **(b) The re-trigger covers the documentation wording too.** Every
  "calibration-equivalent" claim in roxygen/vignette must be scoped to
  correlation input (see 4), so the docs do not silently become wrong the
  day a covariance path lands.

### 4. User-facing wording: five traps to avoid

The proposed sentence — "the families are calibration-equivalent; use unit
for inference, free for reproducing published CIRCUM/CircE output" — has
real traps as written:

1. **Scope the equivalence claim twice: to the model test, and to
   correlation input.** "The families are calibration-equivalent" unqualified
   is false — their SE surfaces differ drastically (NA SEs in ~52–55% of
   N = 250 free fits), and σ̂² never carries an interval. Say: "the model-test
   statistic T is calibration-indistinguishable between the families for
   correlation input" (optionally: "as expected, since a correlation matrix
   pins the variance scales at 1").
2. **Do not say "identical."** T_free ≤ T_unit systematically (nesting); the
   difference is ≤ 0.5% of df, not zero. "Indistinguishable in calibration"
   or "differ by well under 1% of df" is accurate; "identical" invites a
   user to be confused when the two printed T values differ in the second
   decimal.
3. **"Use unit for inference" must not imply the free family's p-value is
   invalid.** It is equally calibrated at correlation input. The correct
   framing: both families' model tests are equally calibrated here; unit is
   the default because the free family adds no inferential benefit for
   correlation input while costing p extra parameters and an SE surface that
   is frequently singular below N = 2000; free exists to reproduce published
   CIRCUM/CircE output exactly.
4. **"Calibration-equivalent" must not morph into "nominally calibrated."**
   Both families are mildly *conservative* at small/mid N (rejection .02–.04
   at α = .05; boundary truths reach nominal only near N = 50000, interior
   from N = 2000). The vignette should keep the D-010-consistent note that a
   non-rejected fit at small N is weak evidence of fit — the equivalence
   claim describes the *comparison*, not absolute calibration.
5. **Do not extrapolate beyond the envelope.** No claim about covariance
   structures generally, non-Gaussian data, or "all sample sizes"; state the
   measured envelope or use "at the truths and sample sizes measured
   (N = 250–50000)" phrasing. And wherever the free family is recommended
   for reproduction, keep the standing caveats adjacent: σ̂² carries no
   interval (D-009); analytic CIs follow the shared N-conditional caution
   ladder (D-010).

## Beyond the brief

- **B1 — RB headline slightly overstates.** "Identical rejection rates …
  cell-by-cell" is not literally true: two cells differ by one discordant
  replicate each (boundary_N250: .024 vs .022; boundary_N5000: .036 vs
  .034; 2/5,751 overall). `devel/m21-t-calibration.md` itself is accurate;
  the D-entry should inherit the .md's phrasing ("no cell separates the
  families in any metric"), not the RB's "identical."
- **B2 — nesting violations are an optimizer-tail signature.** The
  boundary_N2000 violation (+5.52 ≈ 0.55·df on one replicate) means the
  free engine's multi-start landed on a measurably worse optimum than the
  unit fit on the same R. 3/5,751 is immaterial to every summary, but it is
  fixable by construction: seed the free engine's start battery with the
  accepted unit solution (σ = 1 appended). That enforces T_free ≤ T_unit
  deterministically at the cost of one extra start.
- **B3 — acceptance discordance at small N.** At N = 250, exactly one of
  the two families fails the reproduction/acceptance criterion in ~5–8% of
  replicates (symmetric in the margins). Not decision-relevant, but a user
  fitting both families to the same R at small N can see one accepted and
  the other not; a one-line vignette expectation-setting sentence is cheap
  if that surface is ever documented side-by-side.

## Recommendations

1. **Apply** — Write the D-entry: the unit family remains the CPM model-test
   inference default; the free family remains opt-in for exact reproduction
   of published CIRCUM/CircE output; discharges (supersedes) D-009 item 3's
   deferral. Grounds as in answer 1 (measured tie + structural expectation +
   free-family costs + default stability).
2. **Apply** — Scope the equivalence claim in the D-entry and all
   user-facing docs to (i) the model test and (ii) correlation input, with
   the measured envelope stated and the structural reason given (answer 4,
   traps 1–2, 5).
3. **Apply** — State the re-trigger as a gate inside any future
   covariance-input milestone: re-run the paired T calibration at non-unit
   σ truths before that feature ships; the trigger covers both the default
   decision and the equivalence wording in docs (answer 3).
4. **Apply** — Adopt the wording guardrails of answer 4 in the `cpm_fit()`
   roxygen and CPM vignette, including the conservatism-at-small-N note and
   the adjacent σ²/caution-ladder caveats.
5. **Consider** — Seed the free engine's multi-start with the unit solution
   (σ = 1) to enforce nesting by construction (B2). Cheap, removes a class
   of optimizer-tail artifacts; ROADMAP candidate, not a gate.
6. **Consider** — A smoke-level paired run at one non-A variant (e.g. C) as
   belt-and-suspenders for the variant-independence argument. Minutes of
   compute; not required for the D-entry.
7. **Reject — more reps, more correlation truths, or polished-replicate
   stratification.** The paired design already resolves differences at the
   ~0.01 T-unit scale (far below any decision threshold); additional
   correlation truths re-measure a structurally forced tie; polish changes
   df identically for both families and answers a different calibration
   question.
