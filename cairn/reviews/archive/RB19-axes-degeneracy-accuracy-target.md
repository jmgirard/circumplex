# RB19: What relative error may a reported corrected SE carry, and what does that make τ? (M106)

- **Date:** 2026-08-22
- **Output required:** write findings to `cairn/reviews/RR19-axes-degeneracy-accuracy-target.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `circumplex` (R, on CRAN) does instrument-based circumplex
data analysis. `axes_reliability()` estimates the reliability of a circumplex
instrument's two axes by fitting a restricted τ-equivalent CFA
(Strack, Jacobs & Grosse Holtforth, 2013; `cairn/references/strack2013.md`)
with lavaan. It reports two component reliabilities with standard errors, and
four fit statistics scaled by a factor derived from the same fitted matrix.

**The mechanism under review.** The reported corrected SEs are built from an
information matrix `Δ'VΔ` assembled from the fitted covariance matrix's
INVERSE, taken twice. A matrix too ill-conditioned to invert accurately would
therefore yield SEs that are silently wrong. `axes_sigma_degenerate()`
(`R/axes_corrected_se.R:434-443`) refuses such a matrix. The criterion:

    refuse when  λmin ≤ λmax · sqrt(p · ε / τ)

evaluated on `cov2cor(Σ̂)`, with `ε = .Machine$double.eps` and
`τ = axes_degeneracy_tau = 1e-6` (`R/axes_corrected_se.R:398`). Equivalently,
refuse when `κ = λmax/λmin ≥ sqrt(τ/(p·ε))`, which at p = 24 is κ ≈ 1.37e4.
On refusal both the corrected SEs and the four scaled statistics go NA
together, each with a warning naming the reason.

**Why τ has this value, and why that is the problem.** τ is documented as
"the largest relative error tolerated in a reported corrected SE before the
matrix is refused instead" (`R/axes_corrected_se.R:386-397`). It was set at
1e-6 in milestone M89 against an exact-rational oracle
(`devel/degeneracy-oracle/`), which measured the double-precision SE relative
error to sit within a factor of 10 of `p · κ(cov2cor(Σ̂))² · ε` — ratios
3.28 / 2.4 / 1.27 across three decades of κ. So the floor caps a computed
answer's error at roughly 10τ = 1e-5 relative.

That calibration is entirely numerical. **No argument anywhere in the package
says what relative error a reported standard error can actually tolerate**, so
nothing distinguishes τ = 1e-6 from τ = 1e-3. The prior escalation on this
mechanism (RB18/RR18, archived) settled WHICH matrix the criterion prices; it
did not price the refusal region statistically.

**The measured consequence.** A review of M89 (round 3, finding F3) recorded:
an item set containing one pair correlating r = 0.9999 fits cleanly, reaches
κ(cov2cor(Σ̂)) = 3.3e4, and is refused — though its error bound
`p·κ²·ε` at p = 24 is ≈ 5.8e-6, far below any statistical use of an SE. The
package's guiding stance (GP2, `cairn/DESIGN.md`) is "compute anything
well-defined; caution loudly; fail closed", with guardrails that "never block
a defensible analysis".

**The window.** Working the two recorded exemplars through the same bound:

| exemplar | p | κ | bound `p·κ²·ε` |
|---|---|---|---|
| near-duplicate pair, r = .9999 | 24 | 3.3e4 | 5.8e-6 |
| RR18 counterexample B (a measured silent wrong number: corrected SEs wrong by 3.4% with `reason = NULL` under the pre-M89 floor) | 3 | 6.65e6 | 2.95e-2 |

For τ anywhere in (5.8e-6, 2.95e-2) the near-duplicate computes while
counterexample B still refuses — roughly three decades wide, less the
oracle's factor-of-10 calibration slack. τ = 1e-6 sits below that window's
lower edge. **This is context, not a conclusion**: the derivation you are
asked for decides where τ belongs, and τ may legitimately stay at 1e-6.

## Materials

Read these:

- `R/axes_corrected_se.R:335-450` — the criterion, the constant, and the full
  rationale comment block, including the separate `sqrt(p·ε)` band that
  decides the refusal LITERAL (`"indefinite"` vs `"ill_conditioned"`).
- `R/axes_corrected_se.R` in full for the corrected-SE construction: how
  `Δ'VΔ` is assembled and where the fitted matrix is inverted.
- `R/axes_scaled_fit.R:245-280` — the criterion's other consumer.
- `R/axes_reliability.R:700-745` and `:1020-1050` — the exported
  documentation of this behavior, and `:919-1010` for the `data` vs `cormat`
  input paths and the `n` argument.
- `devel/degeneracy-oracle/exact_oracle.R` and `exact_oracle.py` — the
  exact-rational oracle. Run it from the repo root with
  `Rscript devel/degeneracy-oracle/exact_oracle.R`; it needs Python 3
  (standard library only) and reproduces the counterexample anchors plus the
  Q4 κ sweep. The fixture is `cairn/reviews/rb18-counterexample-b.rds`
  (matrix `S` and item angles `ia` only; every other setting is named at the
  top of `exact_oracle.R` — p = 3, N = 600, df = 1).
- `cairn/DECISIONS.md`, entries **D-036**, **D-037**, **D-044** (read whole).
- `cairn/DESIGN.md` — the IP and GP blocks.
- `cairn/references/strack2013.md`, `browne1982.md`, `browne1992.md`,
  `satorra1994.md` as the SE-correction lineage requires.

## Questions

1. **The target.** What is the largest relative error a reported corrected SE
   may carry before the reported number misleads its user? Derive it rather
   than asserting it: name the quantity that prices the tolerance — the
   candidate this brief has in mind is the SE's own sampling variability,
   which for a normal-theory variance estimate has relative standard error of
   order `1/sqrt(2(n-1))`, but say so explicitly if a different quantity is
   the right yardstick. State every premise, including what `n` a package
   with no minimum sample size should calibrate at, and cite any published
   result you rely on by citekey and page.

2. **The constant.** Given your answer to Q1 and the floor
   `λmin ≤ λmax·sqrt(p·ε/τ)`, what value should `axes_degeneracy_tau` take?
   The current documentation reasons that the floor caps a computed answer's
   error at ~10τ because the oracle bound is within a factor of 10 of the
   truth. Is that one decade of slack the right allowance, or should the
   slack be larger, smaller, or expressed differently?

3. **The error model.** Is `p · κ(cov2cor(Σ̂))² · ε` the right error model for
   THIS quantity — the corrected SE specifically, not a generic linear solve?
   The correction inverts the fitted matrix twice in assembling `Δ'VΔ`. The
   oracle's measured ratios (3.28 / 2.4 / 1.27 across three decades of κ) come
   from a p = 3 fixture; state whether that generalizes to the p = 8, 12 and
   24 designs the package actually ships, and whether the model is optimistic
   or pessimistic in any regime.

4. **One constant, or several?** The SE's own sampling noise scales with `n`,
   and the floor already scales with `p`. Should τ instead depend on `n`
   (available on both input paths — required with `cormat`), on `p`, or on
   both? Weigh that against the cost of a threshold users cannot predict, and
   against the package's preference for a single stated criterion shared by
   both consuming surfaces.

5. **Removal.** This is the second escalation of this mechanism (RB18 was the
   first), so removal is on the table and you are asked to weigh it, not to
   assume the mechanism survives. Should the ill-conditioning limb of the
   refusal be dropped entirely — compute, and caution loudly naming the
   conditioning, per GP2's "never block a defensible analysis" — keeping
   refusal only for indefiniteness and exact singularity? Counterexample B is
   the case against: it is a measured silent wrong number, 3.4% off with no
   reason reported. Does a loud caution carrying the estimated relative error
   dominate a refusal, or not? If you recommend removal, say what evidence
   would show it was wrong.

6. **The near-duplicate case.** Is an item set containing a pair at r = .9999
   a defensible analysis the package must serve, or a design defect? If a
   defect, is a whole-fit refusal the right response, or should the package
   name the offending item pair and let the user decide? Answer for the
   `cormat` input path specifically, where the user supplies a correlation
   matrix directly and the package cannot inspect raw items.

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **D-044** settles WHICH matrix the criterion prices — `cov2cor(Σ̂)` for
  every user-reported quantity, raw Σ̂ only at `axes_corrected_se()`'s `naive`
  arm. Not reopened here. If your answer to Q3 or Q5 requires reopening it,
  say so as an explicit disagreement.
- **D-036** and **D-037** set the metric the scaled statistics and the FIML
  ratio are computed in; the criterion prices each surface in that metric.
- The separate constant `sqrt(p·ε)` that decides `"indefinite"` versus
  `"ill_conditioned"` WITHIN the refusal region belongs to a different
  milestone (M90) and is out of scope, except where your answer to Q5 would
  eliminate one of those literals.
- **IP1**: statistical correctness outranks release timing, API stability,
  convenience and performance. **IP3**: every shipped numeric result is
  validated against at least two independent oracle types.
- **GP2**: compute anything well-defined; caution loudly; fail closed.
  Refusal is reserved for statistically ill-defined or wrong-object inputs;
  guardrails must never block a defensible analysis.
- The package is pre-2.0.0 and unreleased on this line, so `τ`'s value is
  still free to move without a deprecation cycle. That freedom ends at the
  next release, which is why this is being settled now.

## Output format

In `RR19-axes-degeneracy-accuracy-target.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`.
