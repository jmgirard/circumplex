# RB12: FIML on items for `axes_reliability()` — the estimator-metric question (M64)

- **Date:** 2026-07-26
- **Output required:** write findings to `cairn/reviews/RR12-axes-reliability-fiml-metric.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is a CRAN R package for circumplex data analysis (interpersonal
and other circular-structure inventories). Its exported `axes_reliability()`
implements Strack, Jacobs & Grosse Holtforth (2013), *Reliability of Circumplex
Axes*, SAGE Open 3(2), doi:10.1177/2158244013486115 — an **item-level restricted
tau-equivalent CFA** that decomposes item variance into orthogonal components and
reads the reliability of the two circumplex axes off the axes component.

### The model as implemented

For an instrument of `k` circumplex scales at equally spaced angles θ_1…θ_k
(degrees; the package convention is [0, 360) with the left-most position
labelled 360, axes at communion 0° and agency 90°), each carrying one or more
items:

- item *i* on the scale at angle θ_s loads with **fixed** weights `cos(θ_s)` on
  a latent axis `AX`, `sin(θ_s)` on `AY`, `+1` on a general latent `GEN`, and
  `+1` on its own scale's specificity latent `SS_s` (and `+1` on its block's
  latent `BS_b` when a block map is supplied);
- every latent covariance is fixed at 0 (`lavaan::cfa(orthogonal = TRUE)`);
- `AX` and `AY` variances **share one label ξ1** — forced equal, the circumplex
  "no preferred rotation" axiom (paper p. 4);
- the general variance **ξ2** is free; the scale-specificity variances share one
  label **ζ1**; block specificity shares **ζ2**; item error variances are
  **free** (tau-equivalence, p. 3).

The implied off-diagonal moment is linear in the components:

    r_ij = ξ2 + ξ1·cos(θ_i − θ_j) + ζ1·[scale_i = scale_j] (+ ζ2·[block_i = block_j])

Axis reliability is Spearman–Brown on ξ1 alone:
`Rel = (item_n·ξ1) / (1 + (item_n − 1)·ξ1)` with `item_n = Σ_s n_s·w_s²` per
axis; `SEm = SD·√(1 − Rel)`.

### The metric this rests on

The five components are **shares of unit item variance** (paper p. 4: they sum
to 1), so the package z-standardizes the items and fits their **correlation**
matrix. Analyzing a correlation matrix as if it were a covariance matrix gives
correct point estimates but only approximate SEs and χ² (Cudeck 1989); that is
the paper's own practice and is already documented on the output.

### The current missing-data contract

**Listwise deletion only.** The raw-data path reduces to complete cases, messages
the complete-case count, **refuses when complete-case N ≤ p** (p = item count),
and never uses pairwise correlations. This was pinned by the feature's own prior
Fable review (RR09, recommendation 5 and binding criterion BC13).

### Why this needs review

Listwise deletion at the **item** level is punishing: the survival probability is
`(1 − rate)^p` in the item count, so a realistic 64-item instrument with 1%
per-item MCAR loses about half its respondents, and at 15% the shipped function
does not degrade — **it errors out**. RR09 named the fix as a possible future
extension:

> **Missing-data policy must be pinned in the build spec:** recommend
> complete-case (listwise) with an informative message, refusing if the
> complete-case N ≤ p; do **not** use pairwise correlations (non-PD risk).
> FIML on items is a possible future extension via the `sem_fit_cfa` pattern,
> not MVP.

M64 exists to settle whether that extension can be offered **honestly**. The
mechanical wiring is trivial (a `missing` argument already threads to
`lavaan::cfa`). The open question is the **metric**: FIML estimates the item
covariance structure using all available information, so pre-standardizing each
column by its available-case mean and SD no longer guarantees that the fitted
model sits on the correlation metric the component interpretation requires.

An empirical wrinkle complicates the picture and is question 2 below: on
**complete** data the shipped fit's implied per-item variances already depart
from 1 by up to 0.046, so whatever the FIML answer is, the departure is not
introduced by FIML.

## Materials

### Read these

- `R/axes_reliability.R` — the whole estimator. Specifically:
  - `axis_weights()` L19–22; `angles_spacing_status()` L46–58; `axis_item_n()`
    L81–84; `axis_reliability_sb()` L94–96; `axis_sem()` L103–105.
  - `axes_fits_zeta1()` L123; `axes_syntax()` L146–253 (the emitted lavaan
    syntax); `axes_ols_shadow()` L270–273; `axes_design()` L298–311.
  - `axes_fit()` L326–335 (the raw-data fit; note its `missing = "listwise"`
    default and that it routes through `sem_fit_cfa()`); `axes_fit_cormat()`
    L350–358 (the moment-matrix sibling, and the comment explaining why
    `likelihood` stays at lavaan's `"normal"`).
  - `axes_is_boundary()` L398–401; `axes_population_cor()` L412–425;
    `axes_simulate()` L442–450; `cronbach_alpha()` L457–461;
    `axis_reliability_nb()` L476–478.
  - the exported `axes_reliability()` from L817. Within it: the raw-data
    preparation and **listwise deletion** L967–1003 (the message, the `N ≤ p`
    refusal, the zero-variance refusal, `R <- cor(mat)`); the
    correlation-matrix input path L1005–1041; the **positive-definiteness
    refusal** L1045–1051; the OLS-shadow seed L1057–1063; the fit itself
    L1069–1076 (note `zdf <- as.data.frame(scale(mat))`); component extraction
    from L1086; the **Nunnally–Bernstein** block L1181–1225.
  - the roxygen missing-data paragraph at L681–682 ("handled by **listwise
    deletion only**"), and the boundary paragraph that follows it, L682–691.
- `R/ssm_sem.R` — the sibling exported SEM function and the shared chokepoint:
  - `sem_fit_cfa()` L744–757 — owns the `"fiml"` → lavaan `"ml"` translation.
  - the exported `missing = c("listwise", "fiml")` argument: roxygen L1234–1236,
    signature L1303, `match.arg` L1310.
  - the listwise `na.omit` and the group-emptied refusal L1385–1400.
  - the read-back of what lavaan actually did, L1681–1686.
- `R/axes_reliability_oop.R` L61–85 — the print method; note the hardcoded
  `"Complete N:   "` label at L70 and its comment at L68.
- `vignettes/axes-reliability.Rmd` L148–157 — the shipped user-facing prose on
  the corr-as-cov SE caveat and the listwise policy.
- `cairn/references/strack2013.md` — the source note for the paper (extracted
  values with table/page anchors; the paper itself is not required reading for
  this brief, but check the note before asserting anything the paper says).
- `cairn/reviews/archive/RR09-axes-reliability-strack.md` — the feature's prior
  Fable review. Recommendation 5 (quoted above) and **BC13** are the binding
  parts for this question; recommendation 4 is the precedent for the
  unavailable-with-reason treatment in question 6.

### Run this

    Rscript devel/m64-fiml-probe.R

`devel/m64-fiml-probe.R` is committed, seed-pinned, and reproduces every figure
quoted in this brief. Its output as of 2026-07-26 (lavaan 0.6.21, circumplex dev
tree 2.0.0), on 600 simulated respondents, 8 octant scales × 3 items (p = 24),
population ξ1 = .35, ξ2 = .10, ζ1 = .08:

```
== F1: expected complete-case share, (1 - rate)^p ==============
                1%    2%    5%   10%  15%
p = 24 items 0.786 0.616 0.292 0.080 0.02
p = 64 items 0.526 0.274 0.038 0.001 0.00

== F1b: the shipped function's behavior at 15% per-item MCAR ===
complete cases: 12 of 600 | items: 24
axes_reliability(): 12 complete case(s) used (588 removed by listwise deletion).
axes_reliability() says: Complete-case N (12) must exceed the number of items (24).

== F2: mean structure under FIML is saturated ==================
listwise (complete data)   meanstructure FALSE npar 27  df 273  free intercepts  0
FIML (5% per-item MCAR)    meanstructure TRUE  npar 51  df 273  free intercepts 24
p = 24 items, so npar rises by exactly p and df is unchanged.

== F3: implied per-item variance departure from 1 ==============
complete data, listwise : max |v - 1| = 0.0456
  2% per-item MCAR, FIML: max |v - 1| = 0.0494
  5% per-item MCAR, FIML: max |v - 1| = 0.0484
 10% per-item MCAR, FIML: max |v - 1| = 0.0647

reported components sum (complete data): 0.9994

== F4: xi1 by route, and route-to-route agreement ==============
truth xi1 = 0.35

 rate    cc | one-stage FIML    | two-stage FIML    | listwise          | |1st - 2nd| (as % of SE)
   2%   374 | 0.3594 (0.0171) | 0.3593 (0.0171) | 0.3639 (0.0219) | 0.00015 (0.9%)
   5%   179 | 0.3609 (0.0173) | 0.3603 (0.0171) | 0.3565 (0.0312) | 0.00062 (3.6%)
  10%    44 | 0.3590 (0.0173) | 0.3595 (0.0171) | 0.3882 (0.0684) | 0.00049 (2.8%)
```

Read "one-stage FIML" as standardized rows handed to
`lavaan::cfa(missing = "ml")`; "two-stage FIML" as a FIML correlation matrix from
`lavaan::lavCor(missing = "ml", output = "cor")` fed to `lavaan::cfa()` via
`sample.cov` with `sample.nobs` = the **total** N; "listwise" as the shipped
path. Estimates print as `est (SE)`.

Note `base::scale()` already standardizes column-wise with `na.rm`, so on
incomplete data each column's **available-case** mean is 0 and SD is 1 to
machine precision (verified: |mean| ≤ 6e-17, |SD − 1| ≤ 9e-16).

## Questions

1. **The metric.** Is available-case column z-standardization sufficient to keep
   the five components interpretable as shares of unit item variance under FIML,
   or does the reported ξ1 acquire a bias or a changed meaning that the
   complete-data path does not have? If it is insufficient, give the correct
   construction — an explicit per-item unit-total-variance constraint
   (ξ2 + ξ1 + ζ1 [+ ζ2] + ε_i = 1), post-hoc rescaling of the components by the
   fitted implied variances, fitting the FIML correlation matrix instead, or
   something else — and state what it costs in degrees of freedom, fit class,
   and comparability with the shipped complete-data numbers.

2. **The complete-data departure.** F3 shows the shipped fit's implied per-item
   variances departing from 1 by 0.046 on complete, exactly z-scored data drawn
   from the model's own population. Is that expected behavior of a restricted ML
   fit (if so, state the stationarity argument: the first-order condition for a
   free ε_i is on `Σ⁻¹(S − Σ)Σ⁻¹`, not on `S − Σ`), or is it a defect in the
   shipped estimator? If it is a defect, say what the reported components
   currently mean, and whether correcting it belongs in M64 or in a separate
   milestone.

3. **The saturated mean structure.** Under `missing = "ml"` lavaan frees all p
   item intercepts on its own (F2: npar 27 → 51, df unchanged at 273), so the
   mean structure is saturated and imposes no restriction. Does anything follow
   for the variance components — in particular, is the information matrix's
   mean/covariance block-diagonality lost under missingness in a way that
   changes ξ1's SE, and should the reported SE therefore carry a caveat beyond
   the existing correlation-as-covariance one?

4. **One-stage vs two-stage.** Given F4's agreement (ξ1 within 0.9 / 3.6 / 2.8%
   of ξ1's own SE; SEs within 2e-4), which route is the defensible default? In
   particular: is the two-stage route's `sample.nobs = N_total` an overstatement
   of the information actually available, such that its SEs and χ² should not be
   permitted to make it — and if so, is there a defensible effective N, or does
   that alone settle the choice in favour of one-stage?

5. **Internal machinery.** Beyond the fit, the function computes one item
   correlation matrix `R` from complete cases and uses it for two further
   purposes: the OLS-shadow method-of-moments cross-check and start values
   (`axes_ols_shadow()`, L270–273, exact on the population matrix), and the
   positive-definiteness refusal (min eigenvalue ≤ 1e-8, L1045–1051). Under FIML
   there may be too few complete cases for either. Should both switch to the
   FIML correlation matrix, and does a FIML-estimated correlation matrix carry
   any positive-definiteness guarantee that changes what that refusal can
   promise?

6. **Derived quantities needing respondents' own scores.** The Nunnally–Bernstein
   comparison reliability needs each scale's Cronbach alpha and the variance of
   the weighted axis composite; `sd = "raw"` needs observed axis-score SDs. The
   milestone's position is to report both as unavailable-with-reason under FIML,
   extending the existing reason mechanism (L1203–1206), mirroring what the
   correlation-matrix path already does per RR09 recommendation 4. Is that right?
   Both alpha and the composite variance are functions of the correlation matrix
   alone on the z metric, so computing them from the FIML matrix is possible —
   would the resulting N–B column mean the same thing as on the listwise path,
   and does the paper's own use of that column (its Table 3 col. 14) bear on the
   answer?

7. **Refusals and reporting.** The listwise contract refuses when complete-case
   N ≤ p. What is the FIML analogue: total N ≤ p, a minimum per-pair coverage
   requirement, a condition on the missingness pattern (for instance a pair of
   items never jointly observed, which leaves that moment uninformed), or
   something else? Name the specific refusals a FIML path must carry. Separately:
   `print()` hardcodes `"Complete N:"` (L70), which misdescribes a FIML fit —
   what should be reported, and should the complete-case count still be shown
   alongside the total?

8. **Evidence bar.** No published oracle exists — Strack et al. report no
   missing-data analyses. The milestone's synthetic bar is: (a) recovery of a
   known population under item-level missingness; (b) exact agreement with the
   listwise path when nothing is missing; (c) a demonstration that FIML's SEs
   beat listwise's as deletion bites; (d) agreement with the OLS shadow; plus
   (e) one non-MCAR cell. Is that sufficient to certify the feature? Name the
   specific MAR mechanism you would use for (e), and any cell you would add or
   change — in particular any cell that would **expect** a reversal or a failure,
   rather than only confirming the happy case.

9. **GO / NO-GO.** On the evidence above, should `axes_reliability()` offer FIML
   on item data at all? If GO, emit binding criteria (see Output format). If
   NO-GO, state what would have to change for the answer to become GO.

## Constraints

Fixed; flag disagreement explicitly rather than silently working around it.

- **D-026** (the feature's GO): the flat fixed-links form is
  covariance-equivalent to the paper's Figure 2 and stays; `orthogonal = TRUE`
  is mandatory; **item error variances stay free** — constraining them equal
  was considered and rejected on df/fit-class grounds; the
  correlation-as-covariance SE approximation is documented rather than
  eliminated. **BC13** of RR09 also stands: **pairwise correlation input must not
  occur** — pairwise deletion is not on the table as an alternative.
- **RR09 §4**: refusing quasi-circumplex / unequally spaced angle sets is
  scope-correct rather than merely cautious. Out of scope here.
- **D-006 / D-014 minimal dependencies**: `lavaan` and `OpenMx` are already
  `Suggests`. Any recommendation requiring a **new** package must say so
  explicitly — it would need its own dependency gate and decision entry, and
  cannot be assumed.
- **Backward compatibility**: `missing = "listwise"` must remain the default and
  the complete-data numbers must not move. This is a shipped CRAN function.
- **API spelling is precedent, not a proposal**: the sibling exported
  `ssm_sem()` already takes `missing = c("listwise", "fiml")` and
  `sem_fit_cfa()` already translates `"fiml"` → lavaan `"ml"`. Do not redesign
  the argument; if you think the spelling is wrong for this function, say so as
  a separate finding.
- **Package conventions**: base R plus minimal dependencies; angles in degrees
  [0, 360) with LM = 360; `stopifnot()`-style validation. None of these are at
  issue in this brief.

## Output format

In `cairn/reviews/RR12-axes-reliability-fiml-metric.md`: answer each question by
number with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Where findings bind implementation, also
emit a `## Binding criteria` section: numbered `BC1…`, each a measurable
assertion checkable against evidence, with any numeric projection stating its
tolerance. These are ingested VERBATIM into the constrained milestone's
acceptance criteria and mechanically diffed against this file; departures are
legal only through that milestone's shown "Deviations from RR12" table.
