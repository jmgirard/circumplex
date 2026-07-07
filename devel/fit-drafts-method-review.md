# Method review of the devel/ fit-statistic drafts (Brief D)

**Date:** 2026-07-03. **Reviewer tier:** Fable (statistical method review).
**Scope:** statistical methods in `devel/fit_analysis.R` and `devel/fit_oop.R`
only. Coding style (dplyr/rlang quasiquotation, `new_s3_scalar`) is out of
scope per the brief — it is a known mechanical rewrite for M4.
**No code was modified.**

## The source of these methods (and the missing citation)

All four exploratory criteria (`fit_fisher`, `fit_gap`, `fit_vt`, `fit_rt`) and
every numeric threshold in the drafts' roxygen come from one paper, which is
cited nowhere in the drafts:

> Acton, G. S., & Revelle, W. (2004). Evaluation of ten psychometric criteria
> for circumplex structure. *Methods of Psychological Research Online*, 9(1),
> 1–27. (PDF: personality-project.org/revelle/publications/acton.revelle.mpr110_10.pdf)

with the Fisher Test originally from Fisher (1997, in Plutchik & Conte, *Circumplex
Models of Personality and Emotions*) and the Gap family from Upton & Fingleton
(1989). `fit_randall` implements the correspondence index of Hubert & Arabie
(1987) as popularized by Tracey (1997, RANDALL). I re-derived every draft
statistic against the equations in Acton & Revelle (A&R Eqs. 2, 6, 8, 9) —
verified below per test.

**Shared context from A&R that constrains all of the revivals:**

- All criteria are applied to the **first two unrotated principal-axis factors**
  (A&R p. 13). The drafts' `psych::fa(nfactors = 2, rotate = "none", fm = "pa")`
  matches. Thresholds were derived under PA extraction; switching to ML (as the
  drafts do whenever `ridge > 0`) silently changes the loadings the thresholds
  were calibrated on.
- Every threshold pair in the roxygen is A&R's **deviation-scored** cutoff.
  A&R report *different* cutoffs for raw-scored data (e.g., Gap: .01/.04 raw vs
  .03/.05 deviation; RT: .04/.09 raw vs .14/.31 deviation). The drafts cannot
  know whether the user ipsatized, yet the docs present the deviation-scored
  cutoffs unconditionally. **Interpretive trap** — the revival must either
  surface both threshold sets keyed to a user-declared scoring, or require/
  perform deviation scoring.
- "Ipsatize first for most power" is approximately right but imprecise. A&R's
  finding is that **deviation scoring** (row-mean centering across variables —
  exactly what `ipsatize()` does) removes a general factor, without which the
  first two PA factors need not span the circumplex plane at all; for VT2 it
  converts a test that *actively mislabels* simple structure into a working one.
  A&R themselves note "ipsatizing" is ambiguous terminology (p. 9). Reword in
  the revival: the criteria assume no large general factor; deviation scoring is
  how you approximate that, not a power tweak.
- These are **heuristic classification cutoffs read off simulated CDF overlap**
  (150/600 subjects × 64/128 variables), not significance tests. Per the
  package's vignette-precision rule, never describe them as tests with error
  rates. The likelihood phrasing ("twice as likely") is A&R's own and can be
  kept verbatim *with the citation*.

---

## 1. `fit_fisher` — Fisher Test of equal axes

**Verdict: SOUND — needs citation only (plus one naming nit and one
denominator caveat).**

> **ADDENDUM 2026-07-07 (M4.5/T2): the h²-vs-prose ruling below is
> overturned.** The T2 sanity gate reproduced A&R's own simulation design and
> found their published .10/.15 cutoffs attach to the CV of *vector lengths*
> (the prose), not the CV of communalities (Eq. 6 as printed) — CV(h²) lands
> at roughly double the published values. The shipped `structure_fisher()`
> computes CV(√h²). Do **not** follow the "do not 'fix' it to match the
> prose" instruction below; see devel/ar2004-transcription.md ("Empirical
> adjudications"). The rest of this section (thresholds' source, naming nit,
> denominator caveat, rotation invariance) stands.

- A&R Eq. 6: Fisher Test = σ(X_v)/mean(X_v) with X_v = Σ_f φ_fv² (the two-factor
  communality). Draft computes `radius <- λ1² + λ2²; sd(radius)/mean(radius)` —
  **exactly Eq. 6**. Note A&R's *prose* describes SD of vector lengths (√h²)
  while their equation uses h²; the draft follows the equation, which is what
  the thresholds were computed from. Correct choice; keep it. (For small CV,
  CV(h²) ≈ 2·CV(h), so this distinction is material — do not "fix" it to match
  the prose.)
- Thresholds in roxygen ("< .10 almost certainly equal axes; < .15 twice as
  likely") **match A&R p. 17 verbatim** (deviation-scored; A&R add: .21 =
  equally likely, and discrimination vanishes above ≈ .40). Defensible once
  cited; add the raw-vs-deviation caveat above.
- Naming nit: `radius` holds the *squared* radius (communality). Rename in the
  revival (`commun` or `h2`) so nobody later "corrects" the formula to match
  the name.
- Denominator caveat: `sd()` is n−1; A&R's Pascal CIRC_STRUC denominator is
  unknown. Immaterial at their nv = 64–128; at nv = 8 octant scales, n vs n−1
  shifts the statistic by √(8/7) ≈ 7% — enough to matter right at a cutoff.
  Flag in docs; not worth blocking on.
- Rotation-invariance check: communalities are invariant to rotation of the
  factor pair, so extraction orientation doesn't matter for this test. Clean.

## 2. `fit_gap` — Gap Test of equal spacing

**Verdict: NEEDS REWORK (one genuine method bug, one boundary bug, one
threshold-applicability problem) — the underlying method is sound.**

- A&R Eq. 2: Gap Test = σ²(X_v) with X_v = θ_{v+1} − θ_v for v = 1…nv−1 **and
  X_nv = 2π + θ_1 − θ_nv** — the wrap-around gap is part of the definition.
  The draft's `gaps <- diff(sort(theta))` **omits the wrap-around gap**. This
  is not cosmetic: with the wrap gap included the gaps sum to 2π and their mean
  is exactly 2π/nv regardless of data; without it, both mean and variance are
  wrong, and the error is worst precisely for simple structure, where the
  wrap-around gap between the last and first cluster is often the *largest*
  gap — omitting it deflates the variance and biases the test **toward**
  declaring circumplexity. Classic 0°/360° boundary bug; per CLAUDE.md this
  demands a regression test with variables clustered near the ±π branch cut.
- Functional form otherwise correct: variance (not CV) of gaps in radians² is
  what A&R's Figure 5 axis and thresholds are on (I cross-checked the magnitude
  of the simple-structure distribution against the figure; the p. 3 remark
  about "CV as a new summary statistic" does not apply to the plotted/
  thresholded Gap Test values). Keep `var()` on gaps including the wrap gap.
- Angle recovery `theta <- sign(λ2) * acos(λ1/√h²)` is correct off the
  boundary but fails at it: `sign(0) = 0` collapses a variable at exactly 180°
  (λ2 = 0, λ1 < 0) to θ = 0, and h² = 0 (flat variable) yields NaN; float
  error can push |λ1/√h²| past 1 into NaN. Replace with `atan2(λ2, λ1)` and an
  explicit degenerate-communality policy. Gaps are invariant to global rotation
  of the factor pair, so extraction orientation is not a problem here.
- **Threshold applicability — the big caveat.** Roxygen thresholds (.03/.05)
  match A&R's deviation-scored cutoffs, *but* A&R found a substantial
  number-of-variables effect on the Gap Test (η² = .11, p. 18): simple-structure
  gap variance shrinks as nv grows, so cutoffs calibrated at nv = 64/128 are
  anti-conservative at nv = 8 (this package's canonical octant-scale case, where
  a simple structure's gap variance is intrinsically larger). A&R ran a
  follow-up at nv = 8/16/32 but publish no per-nv cutoffs. **The revival must
  either re-derive nv-specific cutoffs by simulation (cheap: the A&R generating
  model, Eq. 11.1–11.3, is fully specified) or refuse to print the .03/.05
  interpretation for small nv.** Do not ship the 64-variable cutoffs silently
  against 8 scales.
- Roxygen text bug: the threshold sentence says the values "indicate equal
  axes" — copy-paste from `fit_fisher`; the Gap Test detects equal spacing/
  interstitiality, not equal axes (A&R p. 17: it is *insensitive* to equal
  axes).

## 3. `fit_vt` — "Variance Test" (claims VT2)

**Verdict: NEEDS REWORK — the implemented statistic is neither VT2 nor VT1,
and one roxygen threshold is mistranscribed.**

- A&R Eq. 8 (VT2): CV over rotations θ of X_θ = var_v(Y_vθ) with
  **Y_vθ = φ_1vθ² / Σ_f φ_fvθ²** — the *squared* factor-1 loading normalized by
  the variable's *own* communality (geometrically cos² of the variable's angle
  to the rotated axis). The draft computes
  `var(rlambda[,1] / sum(rlambda^2))`, which errs twice:
  1. **raw loading, not squared** — that is VT1's numerator (A&R Eq. 7), and
     VT1 is one of the five criteria A&R found *ineffective* ("sometimes
     rendered results in the opposite of the correct direction… should be
     avoided", p. 15);
  2. **normalized by the scalar total Σ_v Σ_f φ²** (rotation-invariant), not
     per-variable communality — a constant divisor that cancels exactly in the
     CV, so the normalization is a no-op and the statistic reduces to
     CV_θ(var_v(φ_1vθ)): an unnormalized VT1 variant with no calibration.
  The .40/.48/.65 thresholds cannot legitimately be attached to this quantity.
  Fix: `Y <- rlambda[,1]^2 / rowSums(rlambda^2)`, criterion `var(Y)` per
  rotation.
- **Threshold transcription error:** roxygen says "< 0.48 → three times as
  likely." A&R p. 19 (deviation-scored VT2): **< .40** almost certainly,
  **< .58** at least three times as likely, **< .65** at least twice as likely.
  The 0.48 appears to be a typo for 0.58; re-verify against the paper when
  reviving. Raw-scored cutoffs (.25/.30) differ again — same scoring trap as
  above. Also record A&R's usage warning: VT2 is only trustworthy without a
  large general factor; deviation scoring is "strongly recommended in every
  case."
- Rotation-grid problems shared with `fit_rt` — see §4 (indexing bug, range,
  orientation dependence). For VT2 specifically, Y_vθ = cos²(θ_v − θ), whose
  cross-variable variance contains 2θ and 4θ harmonics → **period 180° in the
  rotation angle**; the draft's 0–45° grid covers a quarter period, making the
  statistic depend on the arbitrary orientation of the unrotated PA solution.

## 4. `fit_rt` — Rotation Test of interstitiality

**Verdict: per-rotation criterion SOUND; statistic as computed WRONG (indexing
bug); rotation grid NEEDS RE-DERIVATION/VERIFICATION.**

- A&R Eq. 9: per rotation, X_θ = Σ_v var_f(φ_fvθ²) with nf−1 denominator; RT =
  CV over rotations of X_θ. Draft's `sum(apply(rlambda^2, 1, var))` matches
  exactly (R's `var` over the nf = 2 squared loadings uses the nf−1
  denominator). Thresholds ".14 almost certainly / .31 twice as likely" match
  A&R p. 19 (deviation-scored; raw = .04/.09). Needs citation + scoring caveat
  only, as far as the formula goes.
- **Genuine bug (also in `fit_vt`):** `criterion <- rep(0, 10); for (i in 0:9)
  criterion[i] <- …`. In R, `criterion[0] <- x` is a silent no-op, so the 0°
  rotation is **dropped**, results for 5°–45° land in slots 1–9, and slot 10
  keeps its initialized **spurious 0**. Verified numerically. A hard zero
  entering a CV inflates the SD and deflates the mean, so both fit_rt and
  fit_vt statistics as drafted are badly inflated — plausible-looking but wrong
  for every input. Any revival must regression-test that the r0…r45 labels
  align with the rotations actually evaluated.
- **Rotation grid.** A&R only say the criteria are "computed over a range of
  values of θ broken down arbitrarily into intervals such as 5 degrees"; the
  exact range in CIRC_STRUC (which generated the thresholds) is unstated. The
  draft's 0°–45° guess is problematic: for RT, X_θ = c + Σ_v (r_v⁴/4)·cos(4(θ_v−θ))
  has **period 90°**, so a 0–45° window is half a period and the CV depends on
  where the arbitrary PA orientation drops that window (VT2 is worse — period
  180°, quarter-period coverage). Sampling one **full period on a uniform grid**
  (RT: 0–85° by 5°; VT2: 0–175° by 5°) makes the mean of the harmonic terms
  vanish exactly and the statistic orientation-invariant. Recommendation:
  full-period grids, then **re-derive the cutoffs by simulation under the A&R
  generating model** rather than trusting cutoffs whose grid we cannot
  reproduce. (The re-derivation can fix the nv = 8 problem from §2 in the same
  run.)

## 5. `fit_randall` — RANDALL correspondence index

**Verdict: NEEDS REWORK — the estimand is right, the inference machinery is
not the randomization test and is statistically confused.**

- The hypothesized-order machinery is correct: `n_away()`/`get_ranking()` build
  circular adjacency ranks whose `lower.tri` ordering matches `cor()`'s, and
  the counting loop tallies each strict order prediction once — the Hubert &
  Arabie (1987) correspondence index CI = (agreements − violations)/predictions,
  as in Tracey's RANDALL. (The draft's `2·ncorrect/ntotal − 1` counts ties as
  violations; with continuous correlations that's measure-zero — acceptable,
  but note it.)
- **The inference is wrong in kind.** RANDALL is a *randomization test*: the
  null distribution comes from permuting the assignment of variables to
  circular positions (all nv! relabelings, or a Monte Carlo sample), yielding
  an exact p-value for the order hypothesis — this is why A&R (footnote 3)
  excluded it from their simulation ("distributional properties are known").
  The draft instead (a) simulates **one** MVN dataset via `MASS::mvrnorm(…,
  empirical = TRUE)` reproducing the observed correlation matrix, then
  (b) bootstraps rows *of the simulated normal data* for a percentile CI. That
  yields no p-value against the random-order null; the CI reflects
  normal-theory sampling variability only (the real rows are discarded after
  computing `rmat`, so any non-normality in the actual data is erased); and
  `empirical = TRUE` makes the parametric detour pointless — t0 is just the
  sample CI. Fix: implement the permutation test for the p-value, and if a CI
  is also wanted, bootstrap the **actual** data rows. This also deletes the
  `MASS` dependency and the fragile `tolerance` argument.

## 6. `fit_oop.R`

No statistical content beyond reporting; two revival notes. (1) `summary.fit`
prints Gap-Test angles in radians — package convention is degrees in the user
API; convert at the print boundary. (2) The summary prints the bare statistic
with no interpretive scaffolding; given the raw-vs-deviation threshold split
and the nv-dependence above, the revived `summary()` should print the cutoff
interpretation *conditionally* (declared scoring, nv within calibrated range)
with the A&R citation, or print no interpretation at all.

## 7. Shared infrastructure bug: the ridge is applied to the wrong matrix

`get_loadings()` builds `ridgemat` as an **n × p** matrix and adds it to the
**raw data matrix**, i.e., it adds `ridge` to the first p observations' diagonal
entries of the data — not to the diagonal of the p × p correlation matrix the
roxygen describes. Statistically meaningless as written; it perturbs the first
p rows of data and hence the correlations in an arbitrary, n-dependent way. The
correct operation is R* = R + ridge·I rescaled back to unit diagonal (or proper
NPD smoothing via eigenvalue clipping). Separately, tying the extraction method
to the ridge (`ridge > 0` ⇒ ML) conflates two unrelated decisions *and* moves
off the PA extraction the thresholds were calibrated under; the revival should
use PA always for these criteria and treat NPD repair as an orthogonal,
explicit option.

## 8. The `psych` dependency: REPLACE (drop it)

`psych` is used for exactly one call: `psych::fa(x, nfactors = 2,
rotate = "none", fm = "pa"|"ml")`. Neither path justifies the dependency:

- **PA (the default, and the method the thresholds assume):** principal-axis
  FA with iterated communalities is ~15 lines of base R — initialize
  communalities with SMCs (`1 - 1/diag(solve(R))`), then iterate: eigen-decompose
  the reduced correlation matrix, reconstruct loadings from the first two
  eigenpairs, update communalities to convergence. No rotation is needed
  anywhere in these tests (Fisher and Gap are orientation-invariant; VT/RT
  become orientation-invariant with full-period grids per §4), so none of
  psych's rotation machinery is touched.
- **ML:** if kept at all, `stats::factanal(covmat = R, factors = 2,
  rotation = "none", n.obs = n)` is base R. (Caveat: factanal's canonical
  unrotated orientation differs from psych's — harmless once §4's full-period
  grids land.) Given §7, the cleaner move is to drop the ML/ridge switch
  entirely and offer PA + explicit NPD smoothing.
- **Recommendation:** implement a small internal `paf2()` (base R, eigen-based),
  validate it against `psych::fa` in the test suite with psych in `Suggests`
  only (psych's `circ.tests()` also implements Gap/Fisher/RT/VT and is a useful
  cross-check oracle there, with the caveat that it may reproduce some of the
  same ambiguities). `MASS::mvrnorm` disappears with the `fit_randall` rework
  (§5). Net new hard dependencies for M4's fit statistics: **zero.**

## Summary table

| Test | Formula vs source | Thresholds | Verdict |
|---|---|---|---|
| `fit_fisher` | Matches A&R Eq. 6 exactly | Match A&R p. 17; uncited | **Sound — needs citation** |
| `fit_gap` | Omits wrap-around gap (boundary bug); `sign·acos` fragile at 180°/h²=0 | Match A&R but nv-dependent; anti-conservative at nv=8; roxygen says "equal axes" in error | **Needs rework** |
| `fit_vt` | Implements neither VT2 nor VT1 (raw loading + no-op normalization) | 0.48 is a mistranscription of A&R's 0.58; thresholds don't attach to the computed quantity | **Needs rework** |
| `fit_rt` | Per-rotation criterion matches A&R Eq. 9; `criterion[0]` indexing bug zeroes the CV inputs; 0–45° grid orientation-dependent | Match A&R p. 19; grid provenance unverifiable → re-derive | **Needs rework (small formula surface, real bugs)** |
| `fit_randall` | Correspondence index correct; inference is not the randomization test (bootstraps simulated MVN data, no p-value) | n/a | **Needs rework** |
| ridge/`get_loadings` | Ridge added to raw data matrix, not R; ridge⇒ML conflation | — | **Needs rework** |
| `psych` dep | — | — | **Replace: internal base-R PAF (+ optional `stats::factanal`); psych to Suggests as test oracle** |

**Cross-cutting M4 requirements distilled from this review:** cite Acton &
Revelle (2004) everywhere thresholds appear; key thresholds to declared
raw/deviation scoring; re-derive cutoffs by simulation for the package's nv = 8
scale-level use (fixes the Gap nv problem and the VT/RT grid provenance in one
run — the A&R generating model is fully specified in their Eqs. 11.1–11.3);
boundary tests for angles at ±180° and the wrap-around gap; regression tests
pinning the rotation-grid/label alignment.
