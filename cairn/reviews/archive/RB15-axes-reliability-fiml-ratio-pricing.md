# RB15: Metric pricing of the corrected component SEs and the FIML ratio (M69)

- **Date:** 2026-08-03
- **Output required:** write findings to `cairn/reviews/RR15-axes-reliability-fiml-ratio-pricing.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for circumplex data analysis, on CRAN. Its
`axes_reliability()` function implements Strack et al.'s circumplex
axes-reliability model: a confirmatory factor model with fixed trigonometric
loadings, fitted through **lavaan** to the item **correlation** matrix treated
as if it were a covariance matrix (Strack's own LISREL practice). The estimated
components are `xi2` (general factor variance), `xi1` (axes variance), `zeta1`
and `zeta2` (scale- and block-specific variance), and a mean item-specific
residual.

Analyzing correlations under a covariance-metric likelihood introduces a
mismatch that the package corrects on two separate surfaces:

- **Component standard errors (milestone M66, shipped).** Normal-theory ML
  prices the SEs for a Wishart-distributed sample covariance matrix. The
  analyzed moments are correlations, whose diagonal has zero sampling
  variability. `axes_corrected_se()` folds the covariance-to-correlation
  Jacobian into the ML sandwich and reports the corrected SE.
- **The global test statistic (milestone M68, shipped).** The same mismatch in
  the opposite direction flatters fit by about 4%. `axes_scaling_factor()`
  applies a Satorra-Bentler scaled statistic.

**The problem this brief exists to settle.** The two surfaces are built from the
same machinery — the same derivative set, the same information matrix, the same
"Wc" construction that folds in the standardization Jacobian — but they are
priced at **different matrices**, and only one of them was ever deliberately
chosen.

Both receive `lavaan::fitted(fit)$cov`, the model-implied matrix. Because
lavaan's `sample.cov.rescale` multiplies the analyzed matrix by (N-1)/N, that
matrix does **not** have a unit diagonal: measured at 0.9983333 on an n = 600
octant probe. M68 noticed this and normalized with `stats::cov2cor()` before
building anything (a milestone-local decision, M68-D2), reasoning that the
entries of the correlation-metric asymptotic covariance are functions of
correlations and that `(1 - rho^2)^2` is meaningless at `rho = 1.03`. It also
measured that under misspecification the implied diagonal is **not even
constant** (range 0.951-1.026 on a deliberately perturbed probe), so no single
scalar undoes it.

`axes_corrected_se()` does no such normalization. It was written before that
observation and prices everything at the raw matrix.

Milestone M69 intends to reprice the corrected branch at `cov2cor(Sigma-hat)`.
Auditing that plan surfaced a second-order consequence that is the real reason
this review was convened, described in "The FIML composition" below.

## Materials

Read these files in the repository root.

**Primary code:**

- `R/axes_corrected_se.R` — the whole file (185 lines). The function
  `axes_corrected_se()` starts at line 108. The two branches:
  - `naive` (line 151): `2 * sum(ws * t(ws))` with `ws <- w %*% sigma`. This is
    the ordinary normal-theory ML variance.
  - `corrected` (lines 156-160): `wc <- w; diag(wc) <- 0;
    diag(wc) <- -rowSums(wc * sigma)`, then `2 * sum(wcs * t(wcs))`. This is the
    same quantity with the covariance-to-correlation Jacobian folded in.
  - Both are divided by `n` and square-rooted at line 161.
- `R/axes_scaled_fit.R` — read at least lines 44-178. Note line 104
  (`sigma <- stats::cov2cor(sigma)`), the rationale for it at lines 53-63, and
  the parallel Wc construction at lines 148-152, which builds the **same**
  operator at the **normalized** matrix.
  Note also that the comment at line 135 cites
  `R/axes_corrected_se.R:137-143` for that construction; the citation is stale
  (the construction is at 156-158) and M69 repairs it separately. Do not let the
  stale citation mislead you about which code is which.
- `R/axes_reliability.R` lines 1655-1800 — the caller. Specifically:
  - line 1664: `se_uncorrected` is read off lavaan's own parameter table.
  - lines 1679-1682: `axes_corrected_se()` is called with
    `lavaan::fitted(fit)$cov`.
  - lines 1683-1694: **the FIML composition**, quoted in full below.
  - lines 1781-1785: `axes_scaling_factor()` is called with the same matrix.

**Tests that currently fence this behaviour:**

- `tests/testthat/test-axes-corrected-se.R`:
  - lines 67-69 and 191-194 assert `abs(naive[[c]] - lav_se(c)) < 1e-7` for the
    components against lavaan's own reported SEs. Component SEs are of order
    0.0168, so this is roughly 6e-6 relative. This is the load-bearing check on
    the whole derivative structure: `naive` and `corrected` differ **only** in
    the Wc step, so pinning `naive` to lavaan pins nearly all of the code.
  - line 203 pins `corrected$zeta2` to 0.0042646 within 2e-6.
  - line 204 pins the ratio `naive[["zeta2"]] / corrected[["zeta2"]]` to 0.9978
    within 1e-3.

**The FIML composition.** `axes_reliability()` accepts three input paths: raw
data with listwise deletion, a supplied correlation matrix, and raw data with
`missing = "fiml"`. On the first two, the reported SE is
`corrected$corrected` directly. On the FIML path it is composed
multiplicatively (`R/axes_reliability.R:1691`):

```r
se_uncorrected * (corrected$corrected / corrected$naive)[names(se_uncorrected)]
```

The stated rationale, from the comment above it: lavaan's FIML
observed-information SEs price the **missing** information correctly (they rise
with the missingness rate), while the complete-data formula in
`axes_corrected_se()` does not price missingness at all. Multiplying by the
metric ratio removes the correlation-as-covariance error while keeping the
missing-information pricing; replacing the SE outright would discard it.

This composition was fixed by a prior review (RR13), whose binding criterion
BC4 reads, verbatim:

> **BC4 (FIML composition).** The FIML path's corrected SE is the
> observed-information SE divided by the same per-parameter ratio evaluated
> at Σ̂. Against the committed 200-replicate fixture at 2, 5, and 10% MCAR,
> mean corrected FIML SE(ξ1) / empirical SD ∈ [0.90, 1.10] in every cell
> (measured 1.001/1.008/1.018).

**The consequence that convened this review.** If M69 reprices only the
`corrected` branch at `cov2cor(Sigma-hat)` and leaves `naive` at the raw
`Sigma-hat` — which is what preserves the lavaan fence at lines 67-69 and
191-194 — then the numerator and denominator of that ratio are evaluated at
**different matrices**. The ratio is then no longer a purely metric quantity: it
acquires a factor of approximately (N-1)/N, systematically **shrinking** every
FIML standard error, by about 0.17% at n = 600 and about 1% at n = 100, growing
as N falls.

**Measurements already taken** on the n = 600 octant probe in this repository,
which you may re-derive or challenge:

- `diag(Sigma-hat)` = 0.9983333, exactly (N-1)/N.
- Repricing **only** the `corrected` branch at `cov2cor(Sigma-hat)` moves it by
  +1.05e-3 (xi1), +1.62e-3 (xi2), +1.72e-3 (zeta1) in relative terms.
- Repricing the `naive` branch as well would shift it by exactly the scalar
  1.6694e-3 **on this well-fitting probe** — but recall M68's measurement that
  the implied diagonal is not constant under misspecification, so this is not a
  scalar in general.

## Questions

1. **Is M69's premise correct?** Should `axes_corrected_se()`'s `corrected`
   branch be priced at `cov2cor(Sigma-hat)` rather than at the raw
   `lavaan::fitted(fit)$cov`? Give the argument from the estimand rather than
   from consistency with M68 — if M68's own choice was wrong, say so, since the
   whole milestone rests on this.

2. **Should the two sides of the FIML ratio be priced at the same matrix?**
   State the property the ratio is supposed to have (the comment claims it is a
   metric-only ratio) and whether a mixed-matrix ratio has it. If a residual
   (N-1)/N factor is genuinely harmless or genuinely correct, say so and explain
   why the direction — always shrinking the reported SE, more so at small N — is
   acceptable.

3. **If the two sides should share a matrix, which matrix, and how should the
   code be organized to keep the lavaan fence?** The concrete proposal under
   consideration is to have `axes_corrected_se()` return a third value — the
   normal-theory variance evaluated at `cov2cor(Sigma-hat)` — used *only* as the
   ratio denominator, while `naive` stays at the raw matrix so that the
   assertions at lines 67-69 and 191-194 continue to reproduce lavaan. Assess
   that design. If you prefer a different organization, specify it concretely
   enough to implement.

4. **Does any of this disturb RR13's BC4 rationale?** BC4 requires the FIML SE
   to be the observed-information SE divided by "the same per-parameter ratio
   evaluated at Σ̂". Does repricing the ratio's components change whether the
   missing-information pricing survives the composition? If BC4 must be
   superseded rather than merely re-satisfied, say so explicitly — the package's
   process requires superseding a prior binding criterion, never quietly working
   around it.

5. **What invariance should be asserted as a regression test?** Under
   same-matrix pricing, the reported FIML SE should presumably be invariant to
   multiplying the fitted `Sigma-hat` by an arbitrary positive scalar. Is that
   the right property to pin, is it exactly true or only approximately, and is
   there a sharper or additional property worth asserting? If you conclude the
   mixed-matrix pricing is correct after all, state instead the exact factor a
   test should pin, so the behaviour is fenced either way.

6. **Is the `n` divisor still right?** Lines 151-161 divide the variance by `n`
   and take the square root, where `n` is complete cases on the listwise path and
   the supplied `n` on the correlation-matrix path. Does repricing at
   `cov2cor(Sigma-hat)` interact with that divisor — in particular, is there any
   double-counting of the (N-1)/N rescaling between the divisor and the
   normalization?

7. **Anything the non-FIML paths need.** The listwise and correlation-matrix
   paths consume `corrected$corrected` directly with no ratio. Is repricing
   sufficient for them, or does anything else follow?

## Constraints

Fixed, and not to be relitigated — flag disagreement explicitly rather than
silently working around it:

- **D-035:** `axes_reliability()`'s component SEs are **corrected, not
  caveated.** Reverting to reporting the uncorrected SE with a warning is not
  available.
- **M68-D2 and D-036:** the scaled global test statistic ships, priced at
  `cov2cor(Sigma-hat)`. It is shipped code. If your answer to question 1 implies
  M68-D2 is wrong, say so as a finding — it would be handled as a superseding
  decision, not silently.
- **The failure contract:** on both surfaces, the naive and corrected values are
  `NA` together with a named `reason`, and neither ever falls back to the other.
  Reporting an uncorrected number in a field documented as corrected is the one
  failure a user could not detect. Any proposal must preserve this.
- **No new package dependency.** lavaan and OpenMx are already `Suggests`;
  anything beyond them requires a separate decision gate.
- lavaan is a `Suggests` with **no version floor**, and its unexported
  functions' argument names are not a contract. Do not propose anything that
  calls a lavaan internal by name.
- The package is **pre-release** for v2.0.0 — none of this behaviour has shipped
  to CRAN — so changing reported numbers costs no deprecation cycle. Do not
  weight backward compatibility.

## Output format

In `cairn/reviews/RR15-axes-reliability-fiml-ratio-pricing.md`: answer each
question by number with your reasoning and evidence; list any additional
findings separately under "Beyond the brief"; end with concrete
recommendations, each marked apply / consider / reject-with-reason.

Where findings bind implementation, also emit a `## Binding criteria` section:
numbered `BC1…`, each a measurable assertion checkable against evidence, with
any numeric projection stating its tolerance. These are ingested **verbatim**
into M69's acceptance criteria and mechanically diffed against this file.

Two cautions on binding criteria, both drawn from this repository's own
history of review returns:

- **Bound every universal.** A criterion asserting "no X" or "every Y" must name
  a procedure that enumerates the domain it quantifies over. A hand-list of
  sites is not a procedure, and a grep for numeric literals does not enumerate
  prose that states a figure as a rounded percentage or a bare sign claim.
- **Derive any tolerance from the discrimination required**, not from a value
  one machine printed. Numbers here have failed review twice for being quoted
  off a single run; this repository has also seen a bit-exact assertion pass
  locally and fail on CI at 1.3e-8 relative under coverage instrumentation.
