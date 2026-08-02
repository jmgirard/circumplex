# RB14: Does a mean-calibrated but tail-miscalibrated scaled χ² ship? (M68)

- **Date:** 2026-08-02
- **Output required:** write findings to
  `cairn/reviews/RR14-axes-reliability-scaled-chisq-calibration.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `circumplex` is an R package on CRAN for circumplex data
analysis. `axes_reliability()` implements Strack, Jacobs & Grosse Holtforth's
(2013) estimator for the reliability of the two circumplex axes. It fits an
item-level confirmatory model whose implied matrix is **linear** in its
variance components,

    Σ(θ) = ξ1·C + ξ2·J + ζ1·B + ζ2·K + diag(ε),   C_ij = cos(θ_i − θ_j),

by normal-theory ML via `lavaan`, and reads axis reliability off the ξ1
component. `C` is the axes structure at the items' fixed circumplex angles, `J`
is all-ones (a general factor), `B` is same-scale membership, `K` is same-block
membership, and the `ε_i` are free item error variances.

**The standing metric problem.** Following the source paper's own LISREL
practice, the model is fit to the item **correlation** matrix as if it were a
covariance matrix. Point estimates are unaffected, but everything computed from
the input's sampling variability is mispriced, because a sample correlation
matrix has a non-varying diagonal and less variable off-diagonal cells than the
corresponding covariances (`var(√n·r_ij) = (1 − ρ²)²` against `(1 + ρ²)` for the
covariance). The package has already corrected one side of this: milestone M66
replaced lavaan's normal-theory component standard errors with correlation-metric
ones (`R/axes_corrected_se.R`), on the ground that the error changes sign across
the accepted input space and so could not be stated honestly by any fixed caveat.

**What M68 is doing.** M68 corrects the other side: the global test statistic,
which carries the same mismatch running the other way, so that fit is
**flattered**. It implements the Satorra–Bentler scaled statistic,

    T_s = T / c,        c = tr{U Γ_R} / df,
    U   = V − V Δ (Δ′ V Δ)⁻¹ Δ′ V,

with `V` the normal-theory ML weight matrix at the fitted matrix, `Δ = ∂σ/∂θ′`,
and `Γ_R` the asymptotic covariance of the sample **correlations** rather than
of the sample covariances. CFI additionally scales the independence model's own
statistic by its own factor `c_b`. `df` and `srmr` are deliberately unchanged.

**The problem this brief is about.** The milestone's acceptance criterion AC3
demanded two things of the shipped statistic at each of three complete-data
populations at N = 600, over 2000 replicates: `mean(T_s)/df ∈ [0.97, 1.03]`, and
an empirical rejection rate of the reported p-value at α = .05 within
`[.036, .064]`.

**The first passes everywhere. The second fails at two of three populations.**
The implementing session's reading is that the residual is the ML χ²'s own
finite-sample upward bias rather than an error in the scaling factor, and that
AC3's rejection-rate clause therefore asks M68 to remove an error that is not the
one it corrects. That reading loosens a criterion after seeing the result, which
is exactly the kind of move that warrants an outside skeptic. **That is what you
are here to check.** You are not being asked to bless it.

## Measured evidence

All numbers below were produced by the committed generator named under
Materials. Monte-Carlo standard errors are given where they matter.

**The three AC3 populations, N = 600, 2000 replicates each.** `c_pop` is the
scaling factor at the population correlation matrix; `d'` is the Satterthwaite
degrees of freedom for the *adjusted* statistic (see Q3).

| population | df | mean(T)/df | mean(T_s)/df | rej. unscaled | rej. scaled | rej. adjusted | sd(T_s)/√(2df) | c_pop | d' |
|---|---|---|---|---|---|---|---|---|---|
| strong-axes (8 scales × 3 items; ξ1 = .35, ξ2 = .10, ζ1 = .08) | 273 | 0.9757 | 1.0204 | .0270 | **.0790** | .0740 | 1.0350 | 0.9563 | 266.1 |
| weak-axes/strong-general (Strack Table 3, COC S16 Other: 16 single-item positions; ξ1 = .032, ξ2 = .467) | 118 | 0.9423 | 1.0139 | .0200 | .0630 | .0590 | 1.0365 | 0.9294 | 113.5 |
| anti-conservative corner (12 scales × 3 items; ξ1 = .05, ξ2 = .60, ζ1 = .05) | 627 | 0.9798 | 1.0227 | .0215 | **.1070** | .1030 | 1.0248 | 0.9581 | 607.1 |

**The sample-size sweep**, strong-axes population, 2000 replicates per cell.
`p*` is the number of distinct moments, `p(p+1)/2 = 300` here, so `p*/N` indexes
how strained the asymptotics are.

| N | p*/N | mean(T)/df | mean(T_s)/df | sd(T_s)/√(2df) | rej. unscaled | rej. scaled |
|---|---|---|---|---|---|---|
| 600 | 0.50 | 0.9755 | 1.0201 | 1.0368 | .0260 | .0920 ± .0065 |
| 1200 | 0.25 | 0.9695 | 1.0138 | 1.0402 | .0270 | .0785 ± .0060 |
| 2400 | 0.12 | 0.9623 | 1.0062 | 1.0149 | .0185 | .0615 ± .0054 |
| 4800 | 0.06 | 0.9579 | 1.0016 | 0.9974 | .0145 | **.0540 ± .0051** |

An independent 3000-replicate run at N = 4800 gave rej. scaled = .0500 ± .0040
and mean(T_s)/df = 1.0031; at N = 1200 it gave .0717 ± .0047. The two runs agree
within Monte-Carlo error.

**The implementing session's argument, stated so you can attack it.** As N
grows, `mean(T)/df` falls monotonically toward `c_pop = 0.9563`, `mean(T_s)/df`
toward 1, the standard-deviation ratio toward 1, and the scaled rejection rate
toward .05 — while `c` is a function of the population matrix and does not
change with N at all. Meanwhile the **unscaled** rejection rate moves *away*
from nominal as N grows (.0260 → .0145). The claimed reading: the metric
distortion is an asymptotic bias that the scaling removes, the residual at small
N is a separate finite-sample bias in T that no scaling factor addresses, and at
N = 600 the two errors partly cancel — which is why the uncorrected test looked
merely conservative.

**Independent check on the factor itself.** The shipped factor is computed
through p × p trace identities (never a p\* × p\* matrix). It is checked against
a deliberately dumb explicit vech-space recomputation — literal `Γ_R`, `V`, `Δ`,
`U` as matrices — which agrees to **1e-15** on three probe maps, and which
carries its own internal invariant (substituting the normal-theory `Γ_S` for
`Γ_R` must give exactly `c = 1`, since `tr{U Γ_S} = p* − q` by construction).
Separately, `1/c = 1.0457` at the strong-axes population against an
independently measured `E[T] = 261.1` versus `df = 273`, i.e. `1.0456`.

## Materials

Read these; you need nothing else.

- `R/axes_scaled_fit.R` (225 lines) — the shipped factor. The two closed forms
  that carry the derivation are at `:127` (the `tr{V Γ_R}` identity) and `:172`
  (the baseline factor `c_b`); the normalization decision is at `:53-70` and
  `:104`; the failure contract is `na_out()` at `:88`.
- `R/axes_corrected_se.R` (171 lines) — M66's standard-error correction, whose
  derivative set (`axes_se_derivs()`, `:50`) and correlation-Jacobian trick
  (`:137-143`) this reuses.
- `R/axes_reliability.R:1674-1800` — the wiring: the lavaan fit-measure guard,
  the call to `axes_scaling_factor()` at `:1742`, and the `details` fields.
- `tests/testthat/test-axes-scaled-fit.R` (515 lines) — the vech-space oracle
  (`vech_oracle_factor()`, `:37`) and the agreement tests.
- `devel/m68-scaled-fit-cells.R` (260 lines) — the generator producing every
  number in the table above, seed-pinned. Re-run with
  `Rscript devel/m68-scaled-fit-cells.R` (2000 reps, ~5 min on 8 cores) or
  `Rscript devel/m68-scaled-fit-cells.R 100 4` for a fast smoke.
  Its committed summary is `tests/testthat/fixtures/m68-scaled-fit-cells.rds`.
- `cairn/references/satorra1994.md` — the source note for the scaling
  correction, with eqs. 16.18 (p. 406) and 16.21/16.22 (p. 407) quoted
  verbatim, the p. 401 sentence licensing `Γ` as the acov of any moment vector,
  and a "What this does and does not license" section.
- `cairn/references/cudeck1989.md` — the source note for the metric problem,
  including why Cudeck's own "Error (b)" is a *different* error from this one.
- `cairn/milestones/M68-axes-reliability-scaled-chisq.md` — the milestone,
  including AC3 as written, and the two milestone-local decisions M68-D1 (the
  FIML path uses the complete-data `Γ_R`) and M68-D2 (pricing at
  `cov2cor(Σ̂)`).
- `cairn/DECISIONS.md`, entries D-035 and D-036 — the standing decisions that
  the standard errors and then the statistic would be corrected rather than
  caveated.

## Questions

1. **Is the derivation right?** Specifically: (a) the closed form
   `tr{V Γ_R} = Σ_{k<l}[1 − (Σ⁻¹)_kl ρ_kl (1 − ρ_kl²)]` at `R/axes_scaled_fit.R:127`;
   (b) the collapse of the baseline factor to `mean((1 − ρ²)²)` over item pairs
   at `:172`, and in particular the claim that the independence model's
   projection term vanishes identically; (c) M68-D2's decision to price both `U`
   and `Γ_R` at `cov2cor(Σ̂)` rather than at lavaan's raw Σ̂. Note that the
   1e-15 oracle agreement checks the shipped code against the *same* modelling
   assumptions, so it cannot catch an error shared by both routes — (a), (b) and
   (c) are exactly where such an error would live.

2. **Is the residual really the ML χ²'s finite-sample bias?** Assess the sweep
   evidence above. Is the monotone convergence of `mean(T)/df` to `c_pop`, with
   the factor held fixed, sufficient to establish it? Name any competing
   explanation the sweep does not rule out — in particular, whether the
   per-replicate estimation of `ĉ` (rather than using `c_pop`) contributes to
   the excess dispersion (`sd(T_s)/√(2df)` ≈ 1.04 at N = 600, ≈ 1.00 at
   N = 4800), and whether that source would also vanish with N or is being
   misattributed.

3. **Would the adjusted statistic have been the better choice?** The measured
   Satterthwaite-adjusted rejection rates (.0740 / .0590 / .1030) barely improve
   on the scaled ones, which the implementing session read as ruling out
   eigenvalue dispersion as the cause. But the adjusted statistic was evaluated
   with `d'` computed **once at the population matrix**, not per replicate. Does
   that shortcut invalidate the comparison? Would a properly per-fit adjusted
   statistic behave materially differently, and is your answer strong enough to
   reopen the scope decision that `$fit$df` stays an integer?

4. **Should this ship?** Given a statistic that is calibrated in mean at every
   population tested, is exactly nominal in the tail once `p*/N ≲ 0.1`, but
   over-rejects at .079–.107 at N = 600 where the uncorrected statistic
   under-rejects at .020–.027: is replacing the uncorrected values with the
   scaled ones a net improvement for this package's users, who typically have
   N in the hundreds? Answer for the four reported statistics separately if they
   differ — `chisq`, `pvalue`, `rmsea`, `cfi` — since a user reads RMSEA and CFI
   as descriptive indices and the p-value as a test. If your answer is "ship
   some but not all", say exactly which.

5. **What must AC3 become, and what must the documentation say?** If M68 ships,
   state the acceptance criterion that honestly fences what was achieved,
   including which cells are gated and which are recorded. State the substance
   the user-facing documentation must carry about small-sample behaviour, and
   whether it needs a runtime warning rather than prose. If instead M68 should
   not ship in this form, say what would have to change.

## Constraints

Flag disagreement with any of these explicitly rather than silently working
around it.

- **D-035 and D-036 are standing decisions** that the component standard errors
  and the global test statistic will be *corrected rather than caveated*. The
  rationale in both is that a caveat quantifying one population's distortion as
  though it were a constant is dishonest. You may recommend superseding either,
  but say so explicitly — do not treat "keep the caveat" as available by default.
- **`$fit$df` and `$fit$srmr` are out of scope** and unchanged: df is a count of
  overidentifying restrictions, srmr a residual summary. Q3 may reopen this, but
  only with an argument strong enough to name.
- **`ssm_sem()` is not implicated** — it lives on the covariance metric (D-035).
- **No new package dependency.** lavaan is already a Suggests; anything
  requiring another package is a candidate for later work, not a recommendation
  for M68.
- **The correction is not optional-by-argument.** The package deliberately does
  not offer a supported way to ask for the uncorrected number; it is exposed in
  `details$fit_uncorrected` for comparison only. Recommending a user-facing
  switch means arguing against that.
- A **small-sample (Bartlett/Swain-type) correction to T** is not in M68's
  scope. You may recommend it as future work; say so under that heading rather
  than as an M68 requirement.

## Output format

In `RR14-axes-reliability-scaled-chisq-calibration.md`: answer each question by
number with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Where findings bind implementation, also
emit a `## Binding criteria` section: numbered `BC1…`, each a measurable
assertion checkable against evidence, with any numeric projection stating its
tolerance. These are ingested VERBATIM into M68's acceptance criteria and
mechanically diffed against this file; departures are legal only through M68's
shown "Deviations from RR14" table.
