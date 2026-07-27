# RR12: FIML on items for `axes_reliability()` — the estimator-metric question — Review Report

- **Date:** 2026-07-26
- **Reviewer:** Independent expert review (Fable), per `cairn/reviews/RB12-axes-reliability-fiml-metric.md`
- **Scope:** M64 — whether `axes_reliability()` can offer FIML on item data honestly, and on what metric
- **Verdict:** **GO** (conditional on the Binding criteria below)

Materials read: `R/axes_reliability.R` (whole estimator, including every section
the brief names), `R/ssm_sem.R` (`sem_fit_cfa()` L744–757, the `missing`
argument L1234–1310, the listwise guard L1385–1400, the read-back L1681–1686),
`R/axes_reliability_oop.R` L61–105, `vignettes/axes-reliability.Rmd` L143–170,
`cairn/references/strack2013.md`, `cairn/reviews/archive/RR09-axes-reliability-strack.md`.

Evidence: I ran `devel/m64-fiml-probe.R` (output reproduced the brief's F1–F4
figures exactly; lavaan 0.6.21, circumplex dev 2.0.0), then ran three
reviewer-authored probes on the same fixture (8 octant scales × 3 items,
truth ξ1 = .35, ξ2 = .10, ζ1 = .08). Their mechanisms and measured values are
recorded inline below so the milestone can reproduce them; the probes
themselves should be folded into the committed probe file (Recommendation 8).
Reviewer-probe findings referenced below:

- **V-A** (lavaan options): under `missing = "ml"`, lavaan defaults to
  `information = "observed"` (`observed.information = "hessian"`).
- **V-B** (complete-data identities): on complete data,
  `lavCor(missing = "ml", output = "cor")` equals `cor(mat)` to 8.9e-16, and
  `cfa(missing = "ml")` on the same `scale()`-standardized complete data
  reproduces the listwise ξ1 to 5.6e-17.
- **V-C** (stationarity): at the shipped complete-data fit,
  max |diag(Σ̂⁻¹(S − Σ̂)Σ̂⁻¹)| = 8.0e-07 while max |diag(S − Σ̂)| = 0.0448.
- **V-D** (N-scaling): the F3 departure is 0.0456 at N = 600, 0.0195 at
  N = 9600, and 2.95e-13 on the exact population matrix (which recovers
  ξ1 = .35 exactly).
- **V-E** (moderate MAR, mechanism M1 below; 5 reps × N = 2400): listwise
  ξ̂1 mean 0.3205 (MC SE 0.0067; bias −0.030), available-case one-stage FIML
  0.3500 (0.0025), FIML-standardized one-stage 0.3479 (0.0023), two-stage
  FIML-correlation 0.3477 (0.0024). Available-case SDs of missing-prone items
  ranged [0.95, 1.04].
- **V-F** (never-jointly-observed pair): with items 1 and 4 never co-observed,
  `lavCor(missing = "ml")` **silently** returned r(1,4) = 0 against a
  population value of 0.3475 (the saturated likelihood is flat in that moment,
  so EM returns its start), and `cfa(missing = "ml")` converged silently.
- **V-G** (harsh MAR, mechanism M2 below; 4 reps × N = 2000): available-case
  SDs fell to [0.90, 0.98]; paired over the same draws, available-case
  one-stage minus FIML-standardized one-stage = **+0.0167 (paired SE 0.0006)**,
  while two-stage minus FIML-standardized = +0.0008 (0.0012).
- **V-H** (15% MCAR headline; the F1b fixture, seed 115): 12 complete cases of
  600 (listwise refuses); FIML-metric one-stage converged with
  ξ̂1 = 0.3573 (SE 0.0174) against the complete-data listwise 0.3592.
- **V-I** (ζ2 smoke; crossed blocks, 5% MCAR, N = 2000, one pinned draw):
  ξ̂1 = .2979 (SE .0080), ξ̂2 = .1019 (.0048), ζ̂1 = .0639 (.0037),
  ζ̂2 = .0490 (.0026) against truth .30/.10/.06/.05.

**MAR mechanisms (for reproduction; both are MAR by construction — missingness
depends only on always-observed values):**

- **M1 (moderate, cross-scale anchor):** the three scale-1 items are always
  observed; every other item's cells go missing independently with
  P = plogis(qlogis(.12) + 1.5·x_anchor), where x_anchor is the respondent's
  first scale-1 item. (Reviewer seeds 501–505, N = 2400.)
- **M2 (harsh, same-scale anchor):** the first item of every scale is always
  observed; items 2–3 of scale s go missing with
  P = plogis(qlogis(.30) + 2.5·x_first-item-of-s). Selection on a r ≈ .53
  same-scale correlate maximizes the available-case variance distortion.
  (Reviewer seeds 701–704, N = 2000.)

---

## 1. The metric

**Available-case column z-standardization is sufficient under MCAR only. Under
MAR it biases ξ1, and the correct construction is the FIML correlation
metric: standardize each item by the saturated-model FIML (EM) mean and SD,
then fit the one structured FIML model. The unit-total-variance constraint and
post-hoc rescaling are both rejected.**

*Why MCAR is fine.* Under MCAR the available-case mean and SD are consistent
for the population mean and SD, so the standardized columns converge to
unit-variance variables and the FIML fit targets the same population
correlation structure as the complete-data path — same estimand, same
interpretation, no changed meaning. F4's agreement (both FIML routes within
0.9–3.6% of an SE of each other) is exactly what this predicts.

*Why MAR breaks it.* Under MAR the available-case moments are inconsistent:
the observed values of item i are a selected subsample, so the standardizing
SD d_i converges to something other than σ_i. The standardized columns then
have true covariance k_i·k_j·ρ_ij with k_i = σ_i/d_i ≠ 1, and the model's
off-diagonal structure — ξ2 + ξ1·cos(θ_i − θ_j) + ζ1·[same] — has **no free
per-item parameter off the diagonal** to absorb an item-specific multiplicative
distortion (only the diagonal ε_i are free). The distortion therefore lands in
the components. Measured: under M2, available-case SDs fall to [0.90, 0.98]
(so k_i ∈ [1.02, 1.11]) and the available-case one-stage estimate sits
**+0.0167 above** the FIML-metric estimate, paired SE 0.0006 — a systematic
≈5% relative distortion of ξ1, about one full SE at N = 600, in the direction
theory predicts (shrunken d_i inflate the moments and hence ξ1). Under the
milder M1 the same divergence is ≈+0.002 — second-order, because the SD bias
is itself bounded by the selection strength times the squared item–anchor
correlation. So the failure is real, mechanism-dependent in size, and lands
precisely where FIML's advertised advantage over listwise lives (under MCAR
listwise is merely inefficient; MAR is the case FIML exists for). A FIML path
standardized by available-case moments would be MCAR-honest but MAR-dishonest
— which is not an honest offer.

*The correct construction.* Estimate the saturated (unstructured) model by
FIML — one EM fit, `lavCor(missing = "ml")` or equivalent — and use its
implied moments two ways: (i) standardize each item by the FIML mean and by
the FIML SD rescaled to the N−1 convention (multiply by
`sqrt(N_used/(N_used − 1))`), then fit the **single one-stage structured FIML
model** on those columns; (ii) convert to the FIML correlation matrix R̂ for
the internal machinery (question 5). The rescaling convention makes the
standardized matrix equal `scale(mat)` **exactly** on complete data (the FIML
mean is the sample mean and the FIML variance is the ML (N-divisor) variance,
so the correction restores the sample SD identically), which is what makes the
question-8 bar (b) an exact equality rather than a tolerance. Under
missingness the correction is O(1/N) and immaterial; what matters is that the
constants are the MAR-consistent saturated-FIML moments, not available-case
ones. V-G confirms the construction: the FIML-standardized one-stage and the
two-stage-R̂ routes — both on the FIML metric — agree to +0.0008 (0.0012)
while the available-case route diverges by +0.0167.

*Costs.* None in degrees of freedom or fit class: the structured model is
unchanged, lavaan saturates the mean structure on its own (F2: npar +p, df
still 273), and the χ² becomes the standard missing-data ML statistic
(structured covariance + saturated means against the unstructured H1).
Comparability with the shipped complete-data numbers is exact at zero
missingness (V-B: 5.6e-17; BC3). The price is one extra saturated fit, which
question 5 requires anyway.

*Rejected constructions.*

- **Explicit per-item unit-total-variance constraint**
  (ξ2 + ξ1 + ζ1 [+ ζ2] + ε_i = 1): this makes the ε_i determined rather than
  free — it removes p free parameters (df rises by p), moves the model out of
  the paper's tau-equivalent free-errors class ("the errors stay free", p. 3),
  and forces exact diagonal reproduction, which would move the complete-data
  numbers (F3's 0.046 departure would be redistributed into the off-diagonal
  fit). D-026 rejected equal-errors on exactly these df/fit-class grounds; a
  determined-errors constraint fails the same test. Reject.
- **Post-hoc rescaling of components by fitted implied variances:** the MAR
  distortion lives in the **off-diagonal** moments (k_i·k_j multiplying every
  r_ij); rescaling by the implied diagonal does not touch it. It would also
  report components that correspond to no fitted model, and on complete data
  it would "correct" a departure that is pure sampling noise (question 2).
  Reject.
- **Fitting the FIML correlation matrix as the reported estimator
  (two-stage):** correct point estimates (V-E/V-G) but dishonest SEs and χ²
  (question 4). Retained internally only.

## 2. The complete-data departure

**Expected behavior of a restricted ML fit, not a defect. Nothing to correct
in M64 or any milestone.**

The stationarity argument holds and I verified it at the shipped fit: the
first-order condition for a free ε_i is
∂F/∂ε_i ∝ [Σ⁻¹(S − Σ)Σ⁻¹]_ii = 0 — the **weighted** diagonal — not
(S − Σ)_ii = 0. V-C: at the fitted complete-data solution the weighted
diagonal is zero to 8.0e-07 while the raw diagonal departs by 0.0448. The
classical "ML reproduces the diagonal exactly" result of exploratory factor
analysis needs the free-loadings stationarity equations as well; with fixed
loadings only the weighted condition is available, so the off-diagonal
sampling misfit (the model has 273 df of restrictions; sample correlations at
N = 600 carry SE ≈ 1/√600 ≈ 0.041, the same order as the observed 0.046)
leaks into the raw diagonal. RR09 §2 anticipated exactly this ("the
reproduction is near-exact rather than a theorem").

Confirming it is finite-sample and not structural (V-D): the departure falls
from 0.0456 at N = 600 to 0.0195 at N = 9600 and to 2.95e-13 on the exact
population matrix, where ξ1 = .35 is recovered exactly. F3's FIML columns
(0.048–0.065) are the same phenomenon computed on noisier effective
information, as the brief's framing already suspected.

What the reported components mean: consistent ML point estimates of the
population shares of unit item variance. The population diagonal is exactly 1;
the implied sample diagonal equals 1 within sampling error; the reported
component sum (using mean ε̂) was 0.9994 on the fixture. No user-facing claim
is violated. At most, one optional documentation sentence noting that the
components sum to 1 exactly in the population and near-1 in samples
(Recommendation 9; consider, not binding).

## 3. The saturated mean structure

**Block-diagonality is lost under missingness, the SE does change, and the
correct response — observed information — is already lavaan's default under
`missing = "ml"`. One clause of caveat text; no new caveat class.**

Under complete data the mean and covariance parameter blocks of the
information matrix are orthogonal, so the saturated means cost the covariance
parameters nothing. Under missingness the per-pattern likelihood contributions
couple the blocks: the information matrix is no longer mean/covariance
block-diagonal, and — the Kenward–Molenberghs point — the **expected**
information is not valid under MAR; observed information is required for
correct SEs. V-A: lavaan's default under `missing = "ml"` is
`information = "observed"` (Hessian), which prices the coupling and the
pattern-specific information loss automatically. So ξ1's SE does change under
missingness relative to a complete-data formula — and it should; that is the
missing information being charged. The saturated mean structure itself imposes
no restriction (F2: df unchanged at 273), so no component estimate is affected
by it.

Two consequences for the build: (i) pin `information = "observed"` with a test
(BC4) so a future option change or user override cannot silently substitute
expected information, which would be wrong under MAR; (ii) extend the existing
correlation-as-covariance caveat by one clause — the FIML SEs are
observed-information SEs computed on the standardized metric, conditional on
the standardization constants, and remain approximate for the same Cudeck
reason as the shipped path. No caveat beyond that is warranted: the
approximation family is the one already documented, not a new one.

## 4. One-stage vs two-stage

**One-stage is the defensible default. The two-stage route's
`sample.nobs = N_total` is an information overstatement with no defensible
scalar repair — which settles the choice by itself.**

The two-stage χ²/SEs treat R̂ as if it were a sample correlation matrix from
N_total complete cases. Under missingness R̂'s sampling variance is strictly
larger (element-wise, roughly governed by each pair's joint coverage and the
EM borrowing), so two-stage SEs and χ² are anticonservative. F4's near-equality
of the two routes' SEs (0.0171 vs 0.0171–0.0173) is a property of *mild*
cellwise missingness at ≤10% — the actual information loss for ξ1 there is
small, as the one-stage observed-information SE shows by barely rising — and
must not be read as validating the N_total convention; at harsher rates or
concentrated patterns the gap opens with no warning built into the two-stage
numbers.

Is there a defensible effective N? No. The information loss is
parameter-specific and pattern-specific — each pairwise moment's precision
depends on its own joint coverage, and ξ1 pools moments unevenly (by cos Δ) —
so any scalar N is wrong for some parameter, and the "correction" that gets
every parameter right is precisely the observed-information matrix the
one-stage fit already computes. The principled two-stage repair exists in the
literature and in lavaan (`missing = "two.stage"`, Savalei–Bentler corrected
SEs), but lavaan's implementation operates on the covariance metric of the
supplied columns, so it would need the same FIML-metric standardization first
and then buys nothing over one-stage here. Note it as a possible future
alternative; do not use it in M64.

Ruling: the reported estimates, SEs, and fit measures all come from the single
one-stage FIML fit on the FIML metric (BC5). The two-stage matrix R̂ is
internal machinery only (question 5); its SEs and χ² must never surface.

## 5. Internal machinery

**Yes — both the OLS shadow and the positive-definiteness refusal switch to
the FIML correlation matrix R̂, which the metric construction already
requires. A FIML-estimated correlation matrix carries no positive-definiteness
guarantee, so the refusal keeps its job — but its promise changes, and it must
be preceded by the coverage refusals of question 7.**

The saturated FIML fit is already needed for the standardization constants
(question 1), so R̂ = the correlation form of its implied covariance is free.
On complete data R̂ equals `cor(mat)` to 8.9e-16 (V-B), so the shipped
behavior of both consumers is preserved exactly. The OLS shadow remains what
it is — exact on the population matrix, a method-of-moments approximation in
samples — and remains a sound start-value seed and cross-check on R̂.

Positive definiteness: EM iterates stay positive definite generically, but the
saturated MLE itself carries no PD guarantee — with thin coverage the
likelihood can climb toward a singular boundary (the incomplete-data analogue
of the N ≤ p failure), and lavCor can return a near-singular or boundary
estimate. So the min-eigenvalue ≤ 1e-8 refusal is retained on R̂ and still has
real work to do. What changes is what it can promise: it now certifies that
the **estimated** correlation matrix is numerically PD, not that any observed
set of complete-case correlations was consistent — the docs should not imply
otherwise.

One hazard is load-bearing (V-F): for an item pair with zero joint coverage
the saturated likelihood is **flat** in that moment, and lavCor silently
returns its starting value — measured r(1,4) = 0 against a population 0.3475,
with no error and no warning. An R̂ containing a fabricated moment would feed
the shadow, the PD check, and the standardization with fiction. The coverage
refusal (question 7, BC7-iii) must therefore run before any R̂ consumer.

## 6. Derived quantities needing respondents' own scores

**The milestone's position is right: report both as unavailable-with-reason
under FIML. Computing them from the FIML matrix is possible but would not mean
the same thing, and the paper's own use of col 14 supports withholding.**

The brief's premise needs one correction that strengthens the conclusion: the
shipped N–B chain is **not** a function of the correlation matrix alone.
`cronbach_alpha()` (R/axes_reliability.R L457–461) runs on the **raw**,
unstandardized item scores — covariance alpha — and the composite variance is
the observed variance of z-scored scale scores. Rebuilding the chain from R̂
would silently substitute standardized alpha (equal to covariance alpha only
when item variances are equal) and a model-estimated composite variance — a
different estimator chain, with no oracle (RR09 BC8's worked example does not
cover it), presented under the same column name. That is exactly the silent
swap RR09's recommendation 4 exists to prevent.

The deeper reason: the N–B column's role (paper Table 3 col 14, Figure 3) is
an **observed-score comparison** — it shows what the naive observed-score
reliability formula does, against the CFA's component-isolating estimate. Its
value lies in being computed the naive way from respondents' actual scores.
Computing it from a FIML-estimated matrix would launder the naive comparator
through the same model-based missing-data machinery as the main estimate,
mixing inference bases and forfeiting the very contrast the column exists to
display. The paper's precedent runs the same way: Strack et al. computed
col 14 from complete observed data and left it blank wherever its inputs were
undefined (p. 5, single-item instruments) — blank-with-reason, not
reconstructed.

Ruling: under `missing = "fiml"`, `nb_reliability` is NA with `nb_reason`
gaining a `"fiml"` value (accumulating with the existing reasons, per the M61
multi-reason contract), and `sd = "raw"` is refused with an informative error
(observed axis-score SDs need complete rows; a Σ̂-implied SD is a different,
model-based quantity), directing the user to `"std"` or numeric SDs — mirroring
the cormat path exactly (BC9).

## 7. Refusals and reporting

**The FIML analogue is a coverage contract, not a single N threshold.** The
refusals a FIML path must carry (all informative errors, package style):

1. **N_used ≤ p**, where N_used counts rows with at least one observed item —
   the direct analogue of the listwise N ≤ p refusal (the saturated moments
   cannot be estimated otherwise). Rows with **no** observed items are dropped
   before counting, with a message reporting how many (they carry no
   information and lavaan would drop them silently).
2. **Any item with fewer than 2 observed values, or zero variance among its
   observed values** — the FIML extension of the shipped zero-variance
   refusal; such an item has no estimable variance or correlation.
3. **Any item pair never jointly observed** — the moment is uninformed and the
   saturated fit fabricates it silently (V-F: r = 0 returned for a population
   0.3475 with no warning). The error must name at least one offending pair.
   This deliberately excludes planned-missingness designs (three-form and
   similar), where zero joint coverage is by design and FIML is legitimately
   used; the structured model itself would remain identified there (V-F's
   structured fit converged at ξ̂1 = 0.3531), but supporting it requires
   coverage-aware shadow/PD/standardization machinery M64 does not have.
   Refuse now, record as a deferred candidate (Beyond the brief, B-4).
4. **Saturated-stage (EM) non-convergence** — a new failure mode the listwise
   path does not have; it gates the standardization constants and R̂.
5. **Non-PD R̂** (min eigenvalue ≤ 1e-8) — the existing refusal retargeted
   (question 5).
6. **Structured-fit non-convergence** — the existing guard, unchanged.

No minimum per-pair coverage floor beyond zero is bound: any positive constant
would be arbitrary. Instead the message must **report** the minimum pairwise
joint coverage so thin designs are visible (BC8); a soft warning threshold may
be added at the milestone's discretion (consider, not binding).

Reporting: the hardcoded `"Complete N:"` (R/axes_reliability_oop.R L70)
misdescribes a FIML fit and must become path-dependent. Show **both** numbers:
the total N the fit used and the complete-case count alongside — the
complete-case count remains the honest diagnostic of how much work FIML is
doing (e.g. `Total N: 600 (12 complete)` for V-H's cell, which tells a reader
at a glance that nearly everything is partial information). Listwise output
keeps its current label. The startup message likewise reports N_used, the
complete-case count, any all-missing rows dropped, and the minimum pairwise
coverage. `details` must record what was actually fit: a `missing` field read
back from the fitted lavaan object (the ssm_sem L1681–1686 pattern — one
source of truth, not the argument echoed), plus `n_complete` and the minimum
pairwise coverage (`details` currently records no missing-data field at all;
Beyond the brief, B-1).

## 8. Evidence bar

**Necessary but not sufficient as listed. With the additions below —
principally a metric-falsification cell and an SE-honesty cell — it certifies
the feature.**

The listed bar, assessed:

- **(a) Known-population recovery under item-level missingness** — keep
  (BC10; measured at 2/5/10% MCAR by F4).
- **(b) Exact agreement with listwise when nothing is missing** — keep, and
  tighten to *exact* (BC3): with the standardization convention of question 1
  the two paths consume identical input on complete data (measured agreement
  5.6e-17), so a loose tolerance would hide a construction error.
- **(c) FIML's SEs beat listwise's as deletion bites** — keep but strengthen:
  "smaller SEs" alone rewards overconfidence (a path that understated its SEs
  would pass). Add SE honesty: the reported SE must track the empirical
  sampling SD, within a stated band (BC13).
- **(d) Agreement with the OLS shadow** — keep, on R̂ (BC6/BC10).
- **(e) One non-MCAR cell** — keep, with the mechanism named: **M1**, the
  cross-scale anchor-logistic mechanism defined in the header (always-observed
  anchor items; P(miss) = plogis(qlogis(.12) + 1.5·x_anchor)). It is MAR by
  construction, realistic in shape (skipping driven by an observed
  characteristic), and strong enough to bite: measured listwise bias −0.0295
  (4.4 MC SEs from truth) against FIML-metric −0.0021 (0.9 MC SEs). This is
  the reversal cell the brief asks for — listwise *fails* it, FIML passes
  (BC11).

Additions required:

- **(f) The metric-falsification cell** — the cell that would **expect
  failure** of the rejected construction, guarding the question-1 decision
  itself: mechanism **M2** (same-scale anchors, harsh selection), where the
  available-case one-stage route must diverge from the shipped FIML-metric
  route by the predicted amount and direction (measured +0.0167, paired SE
  0.0006) while the shipped route tracks the two-stage-R̂ route (measured
  +0.0008 ± 0.0012). If the implementation quietly used `scale()`, this cell
  reddens; nothing else in the bar would (BC12).
- **(g) Degenerate-coverage refusal cells** — every refusal of question 7
  exercised: never-jointly-observed pair, all-missing rows, item observed
  once, N_used ≤ p, EM non-convergence (mocked via the seam pattern), non-PD
  R̂ (BC7's test half).
- **(h) The headline cell** — 15% MCAR at N = 600 where the shipped function
  errors out: listwise refuses, FIML returns a converged non-boundary estimate
  near truth (measured ξ̂1 = 0.3573, SE 0.0174, vs complete-data 0.3592)
  (BC14).
- **(i) A ζ2 smoke cell** — M63 just added the fifth component; one pinned
  crossed-blocks cell under FIML confirms the machinery composes (measured
  V-I: all four fitted components within ~1 SE of truth) (BC15).

Sufficiency: with (a)–(i), yes. No published oracle exists and none is needed:
FIML here is generic multivariate-normal missing-data machinery grafted onto
an estimator whose own oracles (Layer A/B, RR09) are untouched — the synthetic
bar plus the exact complete-data equality is the right certification shape.
One boundary the docs must hold: no vignette or roxygen sentence may imply the
FIML variant is validated by Strack et al. — the paper reports no missing-data
analyses. Non-normality cells are out of scope: ML already assumes
multivariate normality on the shipped path; FIML leans on it harder
(conditional-linearity of the imputation-like step), which is a documentation
caveat (BC16), not a simulation cell.

## 9. GO / NO-GO

**GO.** The mechanical wiring is genuinely small (the `missing` argument
already threads through `sem_fit_cfa()`); the metric question has a clean
answer with a construction that is exact on complete data, MAR-consistent
under missingness, and essentially free given the machinery question 5 needs
anyway; the SEs come out honest by default (observed information); the
refusal contract extends naturally; and every load-bearing claim above was
measured, not argued. The feature is offerable honestly under the Binding
criteria below. Had the metric question required the unit-variance constraint
(fit-class change) or had the FIML routes disagreed materially with each other
under MAR, the answer would have been NO-GO; neither obtained.

---

## Beyond the brief

- **B-1.** `axes_reliability()`'s `details` records no missing-data policy
  field at all today. Even the listwise-only path should arguably carry
  `missing = "listwise"` for auditability; the FIML build must add it, read
  back from the fitted object (the ssm_sem L1681–1686 pattern), not echoed
  from the argument. Folded into BC8.
- **B-2.** F4's two-stage SE (0.0171 at `sample.nobs = N_total`) sitting at
  the one-stage FIML SE is a coincidence of mild missingness — at ≤10%
  cellwise MCAR the true information loss for ξ1 is small. It must not be
  cited as evidence that the N_total convention is valid (question 4).
- **B-3.** Documentation honesty: under MCAR, listwise is *consistent* —
  merely inefficient and, at item level, brutally wasteful (F1). FIML's
  bias-correction case is MAR. The docs must position FIML as
  "MAR-consistency plus efficiency", not as fixing a bias listwise does not
  have under MCAR, and must state the joint-normality reliance. Folded into
  BC16.
- **B-4.** Planned-missingness designs (three-form etc.) are excluded by the
  zero-joint-coverage refusal, yet are a legitimate and common FIML use case,
  and V-F shows the *structured* model stays identified there (fixed loadings,
  273 df of over-identification). A future milestone could support them by
  making the shadow/standardization/PD machinery coverage-aware. Recommend
  recording a ROADMAP candidate; out of M64 scope.
- **B-5.** The API spelling `missing = c("listwise", "fiml")` is right for
  this function too — endorsed as-is, per the constraint. The *internal*
  construction legitimately differs from `ssm_sem()`'s (which passes raw
  scale scores straight to `cfa(missing = "ml")`): `ssm_sem()` lives on the
  covariance metric, where plain FIML is exactly right and no standardization
  question exists. The metric problem is unique to this function's
  correlation-metric contract; nothing in `sem_fit_cfa()` needs to change.
- **B-6.** The reviewer probes (V-A through V-I, mechanisms M1/M2) should be
  folded into `devel/m64-fiml-probe.R` (or a sibling committed probe) with
  pinned seeds, so the BC evidence is reproducible by the same one-command
  route the brief used.
- **B-7.** Performance: the added saturated EM fit is minor next to the
  structured FIML fit itself (which dominates; with heavy cellwise
  missingness nearly every row is its own pattern). No performance concern at
  realistic N and p; no action needed.

## Recommendations

1. **Apply.** Build the FIML path on the FIML correlation metric: saturated
   FIML moments for standardization (N−1 convention), single one-stage
   structured fit via `sem_fit_cfa()`, `missing = "fiml"`, default unchanged
   (BC1–BC4).
2. **Apply.** Report estimates, SEs, and fit only from the one-stage fit;
   never surface two-stage SEs or χ² (BC5).
3. **Apply.** Retarget the OLS shadow and PD refusal to R̂; gate both behind
   the coverage refusals (BC6, BC7).
4. **Apply.** The question-7 refusal set and reporting changes, including the
   print label and the `details` read-back (BC7, BC8).
5. **Apply.** N–B and `sd = "raw"` unavailable-with-reason under FIML (BC9).
6. **Apply.** The full evidence bar (a)–(i), including the MAR reversal cell,
   the metric-falsification cell, SE honesty, and the refusal cells
   (BC10–BC15).
7. **Apply.** Documentation updates: the roxygen missing-data paragraph
   (currently "listwise deletion only", L681–682), the vignette caveat
   paragraph (L154–157), and the extended SE caveat (BC16).
8. **Consider.** Fold the reviewer probes into the committed probe file
   (B-6); a soft warning threshold on minimum pairwise coverage (question 7);
   a ROADMAP candidate for planned-missingness support (B-4); lavaan's
   `missing = "two.stage"` as a future SE-corrected alternative (question 4);
   one doc sentence on the finite-sample diagonal departure (question 2).
9. **Reject (with reason).** The per-item unit-total-variance constraint —
   changes df and fit class out of the paper's free-errors model and moves
   complete-data numbers (question 1; the D-026 equal-errors rejection
   generalizes). Post-hoc component rescaling — does not touch the
   off-diagonal MAR distortion and reports components from no fitted model
   (question 1). Any scalar effective-N repair for two-stage SEs —
   parameter-specific information loss makes every scalar wrong somewhere
   (question 4). Pairwise-deletion correlations — remain banned (RR09 BC13
   stands; note R̂ is the saturated FIML estimate, not a pairwise matrix, so
   no conflict).

## Binding criteria

"MC SE" means the standard deviation across replicates divided by √replicates.
Tolerances are absolute unless stated. "Must refuse" means an informative,
message-bearing error.

- **BC1 (API and back-compatibility).** `axes_reliability()` gains
  `missing = c("listwise", "fiml")` with `"listwise"` the default, matching
  `ssm_sem()`'s spelling; the `"fiml"` → lavaan `"ml"` translation goes
  through `sem_fit_cfa()`. Every pre-M64 test passes unchanged, and the
  listwise path's numbers are bit-identical to shipped.
- **BC2 (the metric construction).** Under `missing = "fiml"` the items are
  standardized by the saturated-model FIML (EM) means and by the FIML SDs
  rescaled by `sqrt(N_used/(N_used − 1))` — never by available-case
  `scale()` moments — and the reported fit is a single structured
  `lavaan::cfa(missing = "ml", orthogonal = TRUE)` on those columns. On
  complete data the standardized matrix must equal `scale(mat)` within
  1e-12 elementwise.
- **BC3 (complete-data equality).** On data with no missing cells,
  `missing = "fiml"` must reproduce `missing = "listwise"`'s ξ1, ξ2, ζ1 (and
  ζ2 when fitted), reliability, and SEm within 1e-8 each (measured: 5.6e-17
  on ξ1 for the probe fixture).
- **BC4 (observed information).** A test must assert on the fitted FIML
  lavaan object that `lavInspect(fit, "options")$information[1] ==
  "observed"`.
- **BC5 (no two-stage inference).** The reported component SEs and fit
  measures must come from the one-stage FIML fit; no SE or χ² computed from
  a correlation matrix with `sample.nobs` set to the total N may appear in
  `results`, `components`, `fit`, or any print/summary output.
- **BC6 (internal machinery on R̂).** Under `missing = "fiml"`, the OLS
  shadow (start values and stored cross-check) and the positive-definiteness
  refusal (min eigenvalue ≤ 1e-8, retained) must consume the saturated FIML
  correlation matrix R̂. On complete data R̂ must equal `cor(mat)` within
  1e-12 elementwise (measured: 8.9e-16).
- **BC7 (refusal contract).** Each of the following must refuse informatively
  under `missing = "fiml"`, with a test per clause: (i) N_used ≤ p, where
  N_used counts rows with ≥1 observed item; (ii) an item with < 2 observed
  values, or zero variance among observed values; (iii) an item pair never
  jointly observed, naming at least one such pair (evidence V-F: lavaan
  silently fabricates the moment otherwise); (iv) saturated-stage
  non-convergence (mockable seam); (v) non-PD R̂; (vi) structured-fit
  non-convergence. Rows with no observed items are dropped with a message
  reporting the count and excluded from N_used.
- **BC8 (reporting).** Under `missing = "fiml"`: the startup message reports
  N_used, the complete-case count, any all-missing rows dropped, and the
  minimum pairwise joint coverage; `print()` reports the total N with the
  complete-case count alongside (the listwise path keeps `"Complete N:"`);
  `details` gains `missing` (read back from the fitted lavaan object via
  `lavInspect(fit, "options")$missing`, not echoed from the argument),
  `n_complete`, and the minimum pairwise coverage.
- **BC9 (derived quantities).** Under `missing = "fiml"`,
  `nb_reliability` is NA with `nb_reason` including `"fiml"` (accumulating
  with any other applicable reason), and `sd = "raw"` is refused with an
  informative error naming `"std"` and numeric SDs as the alternatives;
  `print()`/`summary()` state the reason.
- **BC10 (MCAR recovery).** On the probe population (8 octant scales × 3
  items, ξ1 = .35, ξ2 = .10, ζ1 = .08, N = 600) at 2%, 5%, and 10% per-item
  MCAR, the mean ξ̂1 over ≥ 200 replicates must lie within 2 MC SEs of .35
  in every cell, and the stored OLS shadow's ξ1 must agree with the CFA ξ̂1
  within .05 in every replicate.
- **BC11 (MAR reversal cell).** Under mechanism M1 (defined in this report's
  header: always-observed scale-1 anchors,
  P(miss) = plogis(qlogis(.12) + 1.5·x_anchor)), with ≥ 5 replicates at
  N = 2400 (or an MC-equivalent budget): the FIML-path mean ξ̂1 must lie
  within 3 MC SEs of .35, and the listwise mean ξ̂1 must differ from .35 by
  more than 3 MC SEs (measured: FIML-metric −0.0021 at MC SE 0.0023;
  listwise −0.0295 at MC SE 0.0067).
- **BC12 (metric falsification).** Under mechanism M2 (same-scale anchors,
  P(miss) = plogis(qlogis(.30) + 2.5·x_anchor_s)), paired over identical
  draws (≥ 4 replicates at N = 2000): mean[ξ̂1(available-case-standardized
  one-stage) − ξ̂1(shipped FIML path)] must be ≥ +0.010 (measured +0.0167,
  paired SE 0.0006), and mean|ξ̂1(shipped FIML path) − ξ̂1(two-stage fit of
  the FIML correlation matrix)| must be ≤ 0.005 (measured 0.0008, paired SE
  0.0012).
- **BC13 (SE honesty).** At 5% and 10% per-item MCAR on the probe population,
  the mean reported FIML SE of ξ1 must be smaller than the mean reported
  listwise SE, with the FIML/listwise ratio decreasing from 5% to 10%; and at
  5% MCAR over ≥ 200 replicates, the ratio of the mean reported FIML SE to
  the empirical SD of ξ̂1 must lie in [0.85, 1.15]. If the ratio falls
  outside the band, the milestone must surface it in the "Deviations from
  RR12" table with a strengthened documented SE caveat — never widen the band
  silently.
- **BC14 (headline cell).** On the F1b fixture (probe population, N = 600,
  15% per-item MCAR, the pinned probe seed): `missing = "listwise"` refuses
  with the N ≤ p error; `missing = "fiml"` returns a converged, non-boundary
  estimate with |ξ̂1 − .35| ≤ .05 (measured ξ̂1 = 0.3573, SE 0.0174).
- **BC15 (ζ2 composition).** One pinned crossed-blocks cell (8 scales × 3
  items, `axes_crossed_blocks()`, truth ξ1 = .30, ξ2 = .10, ζ1 = .06,
  ζ2 = .05, N = 2000, 5% per-item MCAR): the FIML path fits the
  five-component model with each of ξ̂1, ξ̂2, ζ̂1, ζ̂2 within 3 reported SEs
  of its truth (measured: .2979/.1019/.0639/.0490 with SEs
  .0080/.0048/.0037/.0026).
- **BC16 (documentation).** The roxygen missing-data paragraph and the
  vignette caveat paragraph are rewritten to state: listwise remains the
  default; `missing = "fiml"` assumes MAR **and** multivariate normality;
  under MCAR listwise is consistent (inefficient, not biased); the FIML SEs
  are observed-information SEs on the standardized metric, conditional on the
  standardization constants, and approximate for the same
  correlation-as-covariance reason as the shipped path; and the FIML variant
  is certified by the package's synthetic oracle, not by Strack et al. (2013),
  who report no missing-data analyses.

## Conclusion

**GO** — offer FIML on item data in `axes_reliability()` under BC1–BC16. The
honest construction is the FIML correlation metric (saturated-FIML
standardization feeding one structured FIML fit), which is exact on complete
data, MAR-consistent where available-case standardization measurably is not
(+0.0167 divergence under harsh MAR), and whose observed-information SEs
price the missing information correctly where the two-stage shortcut cannot.
The complete-data diagonal departure the brief flagged is expected restricted-ML
behavior, verified at the stationarity condition, and requires no repair.
