# RB13: `axes_reliability()` standard-error calibration (M65)

- **Date:** 2026-07-27
- **Output required:** write findings to `cairn/reviews/RR13-axes-reliability-se-calibration.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is a CRAN R package for circumplex data analysis. `axes_reliability()`
estimates the reliability of the two circumplex axes with the item-level
restricted tau-equivalent CFA of Strack, Jacobs & Grosse Holtforth (2013). It
was built at M53–M59 and has shipped since; M60–M64 extended it; **M65 (in
flight) adds a `missing = "fiml"` path** for item-level missing data.

**The model.** Every item loads on two axis factors with *fixed* cosine weights
(cos θ_i, sin θ_i), on a general factor with weight 1, on its scale's
specificity factor with weight 1, and — for a blockwise instrument — on its
block's specificity factor with weight 1. All latent covariances are fixed at 0
(`orthogonal = TRUE`). The two axis variances are constrained equal. Item errors
are free. So the implied covariance structure is **linear in the four or five
variance components**:

    Σ = ξ1·C + ξ2·J + ζ1·B (+ ζ2·K) + diag(ε),   C_ij = cos(θ_i − θ_j)

Reliability is read off ξ1 (the axes variance) by Spearman–Brown. The model is
fit to the item **correlation** matrix — the items are z-standardized and the
correlation matrix is analyzed as if it were a covariance matrix, which is the
source paper's own practice.

**The standing decision this brief may overturn.** RB09→RR09 (Fable, 2026-07-23)
ruled the correlation-as-covariance issue **"document, don't fix"**, and that
holding is recorded as **D-026 holding (5)**: "analyzing a correlation matrix as
covariance gives correct point estimates but approximate SEs/χ² (Cudeck 1989;
the paper's own practice) — documented". RR09's stated reasoning was that the
model is not scale-invariant (fixed unit and cosine loadings), that the source
paper's own LISREL SEs carry the same approximation, and that reporting the
approximation faithfully is therefore correct.

**What is new.** RR09 and D-026 asserted the SEs are "approximate" but **never
measured by how much or in which direction**. M65 measured it, for the first
time, over 200 Monte-Carlo replicates:

| Path | Data | mean reported SE(ξ1) | empirical SD of ξ̂1 | ratio |
|---|---|---|---|---|
| `missing = "fiml"` (new) | 5% per-item MCAR, N = 600 | 0.01692 | 0.01165 | **1.452** |
| `missing = "listwise"` (shipped) | complete data, N = 600 | 0.01681 | 0.01158 | **1.452** |

Both over the same 200 seeds, same population (8 octant scales × 3 items,
truth ξ1 = .35, ξ2 = .10, ζ1 = .08). **The reported standard errors are ~45%
larger than the estimator's actual sampling variability — conservative, not
anti-conservative — and the new FIML path adds none of this: it inherits the
shipped path's miscalibration exactly, to three decimals.**

This surfaced because M65's driving review (RR12) set a binding criterion,
**BC13**, requiring that ratio to lie in `[0.85, 1.15]`. RR12 set that band
without measuring the shipped path, so **no implementation of either path could
have met it**. BC13's own escape clause forbids widening the band silently and
requires the deviation be recorded with a strengthened caveat. The maintainer
declined the record-and-caveat route and escalated here instead, because a
CRAN-shipped estimator reporting standard errors 45% too large is a bigger
question than M65's scope.

## Materials

Read these; do not read the rest of the package.

- `R/axes_reliability.R`
  - `axes_syntax()` (line 146) — the emitted lavaan model syntax.
  - `axes_fit()` (line 326) — the raw-data fit; `se = "standard"`, routed
    through `sem_fit_cfa()`.
  - `axes_fit_cormat()` (line 350) — the moment-matrix fit, `sample.cov = R`,
    `sample.nobs = n`. Its comment explains why `likelihood` is left at
    lavaan's `"normal"` default.
  - the SE extraction into the reported `components` table (line ~1478).
- `R/ssm_sem.R`, `sem_fit_cfa()` (line 744) — the single `lavaan::cfa`
  chokepoint; owns the `"fiml"` → `"ml"` translation.
- `R/axes_fiml.R` — M65's FIML metric layer: saturated-EM (`h1`) means/SDs with
  a `sqrt(N/(N−1))` rescaling, and R̂ from the same saturated fit.
- `R/axes_reliability_oop.R` lines 41 and 53 — the two SE caveats currently
  printed.
- `cairn/reviews/archive/RR09-axes-reliability-strack.md` §2 — the
  "document, don't fix" holding, verbatim.
- `cairn/reviews/archive/RR12-axes-reliability-fiml-metric.md` — M65's driving
  review; §1 (the FIML metric holding), and BC13 in its Binding criteria.
- `devel/m65-fiml-heavy-cells.R` — the seed-pinned harness that produced the
  measurement; `tests/testthat/fixtures/m65-heavy-cells.rds` is its committed
  output (200 replicates per cell).

**To reproduce the headline numbers.** From the repo root:

```
Rscript -e 'x <- readRDS("tests/testthat/fixtures/m65-heavy-cells.rds")
m <- x$mcar[["0.05"]]
cat("FIML  mean SE:", mean(m[,"fiml.se"]), " sd:", sd(m[,"fiml.xi1"]),
    " ratio:", mean(m[,"fiml.se"])/sd(m[,"fiml.xi1"]), "\n")'
```

The complete-data listwise comparator is regenerated by drawing 200 replicates
from `axes_simulate(600, octants(), 3, .35, .10, .08)` at seeds 1001:1200 and
fitting each with `missing = "listwise"`.

## Questions

1. **Is 1.452 the expected magnitude?** Given this model's exact structure
   (fixed unit and cosine loadings, orthogonal factors, equal axis variances,
   free item errors, linear-in-parameters Σ), is a ~45% overstatement of
   SE(ξ̂1) the predicted consequence of analyzing a correlation matrix as a
   covariance matrix, or does it indicate a separate defect — in the SE
   extraction, the model specification, or lavaan's treatment of this design?
   Derive the expected ratio analytically if that is tractable for this
   structure; state clearly if it is not.

2. **Is the direction and size stable, or population-dependent?** The
   measurement is one population (8 scales × 3 items, ξ1 = .35, ξ2 = .10,
   ζ1 = .08, N = 600). Does the conservatism hold across scale counts, items
   per scale, component values, and N — and does it vanish, shrink, or grow?
   In particular, is there any region of the parameter space where the SEs
   become **anti**-conservative, which would change the risk profile from
   "understates precision" to "overstates it"?

3. **Should the SEs be corrected rather than caveated?** This would supersede
   **D-026 holding (5)** and RR09 §2. If yes, which route, and what does each
   cost in a package whose only SEM dependency is lavaan (`Suggests`)?
   Candidates to assess, not an exhaustive list: a Cudeck/Browne corrected
   asymptotic covariance matrix for correlation structures; a robust/sandwich
   estimator; a nonparametric bootstrap over respondents; or refitting on the
   covariance metric with unit-variance constraints instead of standardizing.
   Address feasibility for the `cormat` input path, which has no raw data.

4. **Does the FIML path change the analysis?** M65's SEs are
   observed-information SEs on a metric standardized by *estimated* saturated-EM
   constants, treated as fixed. Conditioning on estimated constants normally
   makes SEs too **small**. The measured ratio is nonetheless identical to the
   listwise path's to three decimals. Is that because the effect is negligible
   here, or because two errors are cancelling? If a correction is recommended
   for the shipped path, does the same correction serve the FIML path, or does
   the standardization uncertainty need propagating separately?

5. **Is RR12's BC13 band defensible, and what should replace it?** `[0.85, 1.15]`
   was set without measuring the shipped estimator. Give the criterion you
   would bind in its place — either a defensible calibration band with its
   justification, or a different measurable property that actually discriminates
   a correct SE implementation from an incorrect one for this model.

6. **Scope and compatibility.** If correction is recommended: does it belong in
   M65, or in its own milestone with M65 shipping the caveat as-is? Changing
   reported SEs on a shipped CRAN function changes published behavior — does it
   warrant a deprecation cycle, and what should the release note say? Note the
   package is pre-1.0 in policy terms only insofar as the maintainer waives the
   cycle explicitly.

## Constraints

Fixed; flag disagreement explicitly rather than working around it silently.

- **The point estimates are not in question.** ξ̂1 recovers truth in every cell
  (|bias|/MCSE 1.24/1.17/1.36 at 2/5/10% MCAR over 200 reps), and an independent
  OLS least-squares route agrees with the CFA to < 0.0026 in every replicate.
  This brief is about the standard errors only.
- **The FIML metric is settled** (RR12 §1, D-033/D-034): standardization uses
  saturated-FIML moments, never available-case moments. Do not relitigate.
- **Pairwise-deletion correlations stay banned** (RR09 BC13, upheld by D-033).
- **RR12 §9's four rejections stand**: no per-item unit-total-variance
  constraint, no post-hoc component rescaling, no scalar effective-N repair, no
  pairwise correlations.
- **lavaan and OpenMx stay `Suggests`** (D-006/D-014, minimal dependencies). A
  recommendation requiring a new hard dependency must say so and justify it.
- **The band in BC13 must not be widened to accommodate the measurement.**
  Either it is replaced with a justified criterion (question 5) or the deviation
  is recorded; a band chosen to fit the observed number is not an option.

## Output format

In `RR13-axes-reliability-se-calibration.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into the constrained milestone's acceptance criteria
and mechanically diffed against this file; departures are legal only through
that milestone's shown "Deviations from RR13" table.
