# RB03: Form of a print-independent, scale-free displacement-certification rule (M16)

- **Date:** 2026-07-12
- **Output required:** write findings to `cairn/reviews/RR03-cert-rule-form.md`

You are performing an independent expert review of a statistical design
decision. This brief is fully self-contained — do not assume any conversation
context. Read only what this brief directs you to read, answer the numbered
questions, and write your findings to the output path above using the same
numbering.

## Background

**circumplex** is an R package (on CRAN) for the Structural Summary Method
(SSM) of circumplex data. A fitted profile is summarized by five parameters;
two matter here:

- **Amplitude (a)** ≥ 0 — how strongly the profile is differentiated around
  the circle (0 = a flat profile with no angular signal).
- **Displacement (d)** ∈ [0°, 360°) — the angular location of the profile's
  peak. **Displacement is only interpretable when amplitude is meaningfully
  non-zero**: if a ≈ 0 the profile has no peak, so its reported angle is noise.

To protect users, `print.circumplex_ssm()` prints a **certification
guardrail**: when it judges the amplitude CI to exclude zero it stays silent;
otherwise it prints "the amplitude CI includes zero; the displacement is not
interpretable." The single definition of that rule is

```r
# R/ssm_oop.R:122
ssm_certified <- function(a_lci, digits = 3) {
  !is.na(a_lci) & round(a_lci, digits) > 0
}
```

where `a_lci` is the lower bound of the amplitude confidence interval and
`digits` is the print rounding precision (default 3).

**Two defects** motivate replacing this rule (M16):

1. **Print-dependent.** The rule moves with the display argument `digits`: the
   effective threshold is `a_lci > 0.5·10⁻ᵈⁱᵍⁱᵗˢ` (≈ 0.0005 at digits = 3).
   A cosmetic display choice silently changes an inferential verdict.
2. **Scale-dependent.** The threshold `0.5·10⁻ᵈⁱᵍⁱᵗˢ` is in **amplitude
   units**. Amplitude on a correlation metric is bounded (~0–1); on a raw
   score metric it is on the score's arbitrary scale. So the same rule means
   different things on different metrics.

The amplitude CI is a **percentile bootstrap** of strictly positive amplitude
replicates (bootstrap or Monte Carlo; `R/ssm_bootstrap.R`). Because a
percentile interval of strictly positive values can never contain 0, `a_lci`
is always > 0, and the rule certifies **even a truly zero amplitude** almost
always (see the seed below). The guardrail's promise is therefore empty in the
regime it exists to police.

**M16's task:** replace `ssm_certified()` with a **print-independent,
scale-free** rule, its false-certification behavior calibrated from the
package's own CI-accuracy diagnostic `ssm_ci_accuracy()`. The **form of the
rule is undecided** and is exactly what this review must settle.

## The seed evidence (already gathered)

A reproducible simulation characterizes the current rule.
Generator: `devel/m16-cert-rule-seed.R` (`set.seed(2026)`); data:
`devel/m16-cert-rule-seed.rds`; human summary: `devel/m16-cert-rule-seed.md`
(**read this summary first**). Three fits from the `jz2017` dataset span the
metric × signal space, each run through `ssm_ci_accuracy(reps = 500)`, which
simulates from a plug-in population and re-runs the object's own CI procedure
across an **amplitude ladder** `c ∈ {1, 0.5, 0.25, 0}` (population amplitude
scaled by `c`; `c = 0` is a genuinely zero-amplitude population).

Key numbers:

| Fit | metric | a_est | a_lci | a_uci | CI width | **a_lci/width** | Cert_rate @ c=0 |
|---|---|---|---|---|---|---|---|
| COR_healthy | correlation | 0.226 | 0.189 | 0.262 | 0.073 | **2.58** | **1.000** |
| COR_nearzero | correlation | 0.012 | 0.005 | 0.049 | 0.044 | **0.10** | **1.000** |
| RAW_means | raw Likert | 0.432 | 0.400 | 0.464 | 0.064 | **6.24** | **1.000** |

Two facts to build on:

- **False-certification ≈ 1 at c = 0 in every metric** (Wilson 95% LCI 0.992
  vs a user-expectation benchmark α/2 = 0.025). At `c = 0` amplitude coverage
  is structurally zero: the percentile interval of positive replicates cannot
  contain 0, so `a_lci > 0` always and the rule always certifies.
- The **scale-free ratio `a_lci / (a_uci − a_lci)`** separates genuine signal
  (2.58, 6.24) from near-zero noise (0.10) at the as-estimated condition,
  where the current rule cannot. This is the leading candidate statistic. It
  is characterized in the seed **only at c = 1**; its distribution at c = 0
  is not yet measured.

## Materials

Read, in this order:

1. `devel/m16-cert-rule-seed.md` — the seed summary (findings, candidate
   family, proposed target).
2. `devel/m4-ci-accuracy-spec.md` — the diagnostic's spec. Sections that bear
   directly: **§3.4** (the shipped rule as measured), **§4.3** (guardrail
   operating characteristics; the false-certification prediction), **§12.5**
   and **§13** (the decision to ship the current rule and design this
   replacement as a follow-up seeded by the diagnostic's output).
3. `R/ssm_oop.R:116-193` — `ssm_certified()` (the rule) and
   `print.circumplex_ssm()` (its consumer, lines 172-190: the fit-inadequate
   and amplitude-includes-zero notes; note the guardrail is applied to
   **profile rows only**, never the contrast row).
4. `R/ssm_bootstrap.R:104-190` — how the amplitude CI is built (percentile of
   the resampled amplitude column) and the two circular `quantile.*` methods.
5. `R/ssm_ci_accuracy.R:1-125` and `R/ssm_ci_oop.R` — the diagnostic that
   measures the rule (its `guardrail` false-certification output, the verdict
   wording, the plot's "Displacement (certified)" panel). Any replacement rule
   is measured by re-running this diagnostic against the new rule.

Optional: re-run `Rscript devel/m16-cert-rule-seed.R` to regenerate the seed,
or write a small extension that measures a candidate rule's false-cert at
`c = 0` (you may propose such an extension in your answer; you need not run
it).

## Questions

1. **Functional form.** Which scale-free functional of the amplitude CI triple
   `(a_est, a_lci, a_uci)` should certification use? Assess at least these
   candidates and any better one you see:
   (a) `a_lci / (a_uci − a_lci) ≥ k`;
   (b) `a_lci / a_est ≥ k`;
   (c) `a_lci ≥ k · a_uci`.
   Judge each on: invariance to a positive rescaling of the score metric and
   to print `digits`; monotone sensible behavior as true amplitude → 0;
   robustness of the denominator (division by ~0); and interpretability to a
   applied user. State a single recommended form.

2. **Threshold `k`: fixed or calibrated?** Should `k` be a fixed a-priori
   constant, or calibrated to a target? A hard constraint: the shipped rule
   must be a **fixed, self-contained function** of a single fit's amplitude CI
   — it **cannot** require the user to run `ssm_ci_accuracy()` (that diagnostic
   *measures* the rule; the rule cannot consume it at runtime). So any
   "calibration" means choosing one constant `k`, once, that we pin in the
   package. Recommend `k` (or the principle that fixes it), using the seed's
   separation (signal 2.58/6.24 vs noise 0.10 for form (a)) and any further
   analysis. If you recommend a form other than (a), give its `k`.

3. **Can any relative rule actually control false-certification at c = 0?**
   This is the crux. At `c = 0` the true amplitude is 0 but the percentile
   `a_lci` is still > 0, and the CI width also shrinks. Will the recommended
   statistic (e.g. `a_lci / width`) reliably fall **below** `k` when the
   population amplitude is zero — i.e., can a relative lower-bound rule drive
   false-cert@c=0 down to a small target — or does the numerator and
   denominator shrinking together defeat it? If a lower-bound-ratio rule
   **cannot** control c = 0 false-certification, say so plainly and recommend
   the alternative instrument (see Q4).

4. **Is a lower-bound rule even the right instrument?** Consider alternatives
   that use the whole amplitude bootstrap distribution rather than just its
   lower quantile — e.g. a bootstrap "fraction of replicates below a relative
   amplitude floor" rule, or an equivalence/ROPE-style test against a
   scale-free near-zero region. Constraint: it must be computable from the
   **existing** bootstrap/Monte-Carlo replicates already produced
   (`R/ssm_bootstrap.R`), introducing **no new estimator or dependency**. If
   such a rule controls false-certification better than any lower-bound ratio,
   recommend it and specify exactly what it computes.

5. **False-certification target.** Is "false-cert @ c = 0 ≤ α/2" (0.025 at the
   95% default) the right acceptance target for M16, or is a different or
   weaker target (e.g. ≤ 0.05) more defensible given the amplitude CI's
   structural positivity? State the target M16 should verify the shipped rule
   against, and how it should be verified (which `ssm_ci_accuracy()` output,
   at which ladder rungs, across which representative configurations).

6. **Contract and edge cases.** Specify the recommended rule's exact return
   for: `a_lci = NA` (a flat / zero-variance profile — currently returns
   `FALSE`, "not interpretable"); `a_est = 0` exactly; a degenerate CI
   `a_lci = a_uci`; and very small `n`. Confirm the rule remains a **pure
   function of `(a_est, a_lci, a_uci)`** (or of the amplitude replicate vector,
   if you recommend Q4's route) so that all five consuming surfaces — the
   print note, `ssm_ci_accuracy()`'s conditional coverage + `Cert_rate`, the
   `summary()` verdict wording, and the plot's certified panel — can apply it
   identically.

## Constraints

Fixed; flag disagreement explicitly rather than working around silently.

- **The replacement must be print-independent** (no dependence on the display
  `digits`) **and scale-free** (invariant under a positive rescaling of the
  score metric). This is M16's defining requirement. If you judge a fully
  scale-free rule that also controls false-certification to be impossible,
  say so directly and explain the least-bad compromise — do not quietly
  relax the requirement.
- **The rule applies to profile rows only.** A contrast row is a signed
  difference, never certification-gated (decision **M15-D1**, recorded in the
  M15 milestone and referenced in `R/ssm_ci_oop.R`). Do not propose gating
  contrasts.
- **The rule must be a self-contained function of one fit's amplitude
  uncertainty** — it may read the amplitude CI `(a_est, a_lci, a_uci)` and/or
  the amplitude bootstrap replicate vector, but it may **not** call or require
  `ssm_ci_accuracy()` at runtime.
- **Do not change the amplitude estimator or the percentile-bootstrap CI
  method.** The rule reads their output.
- **No new package dependency** (base R + rlang/ggplot2/boot/Rcpp only;
  no tidyverse in package code).
- Angle conventions (LM = 360; contrast = 2nd − 1st level) and the pole
  reporting rule (**D-003**: 0/360 reported as exactly 360.0) are out of scope
  and unaffected.

## Output format

In `RR03-cert-rule-form.md`: answer each question by number with your
reasoning and evidence (cite specific seed numbers, file:line, or spec
sections). List any additional findings separately under "Beyond the brief".
End with concrete recommendations, each marked **apply / consider /
reject-with-reason**, and — critically — a single **recommended rule**
(functional form + `k` + edge-case contract + the false-cert target to verify
it against) that M16 can implement directly.
