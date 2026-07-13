# M16 seed — behavior of the current displacement-certification rule

Feedstock for the M16 Review Brief (RB03). Characterizes the **shipped** rule
`ssm_certified(a_lci, digits = 3) = !is.na(a_lci) & round(a_lci, digits) > 0`
(`R/ssm_oop.R:122`) so the replacement's form can be argued from data, not
intuition. Grounds M16 AC3/AC4.

- **Generator:** `devel/m16-cert-rule-seed.R` (reproducible; `set.seed(2026)`).
- **Data:** `devel/m16-cert-rule-seed.rds`. Fits from `jz2017`, octant scales
  `PANO()`, `boots = 500`, `ssm_ci_accuracy(reps = 500)`.
- **Three fits span the amplitude metric × signal space:**
  - `COR_healthy` — correlation metric, `measures = "ASPD"` (a_est 0.226; strong)
  - `COR_nearzero` — correlation metric, `measures = "OCPD"` (a_est 0.012; ≈ noise)
  - `RAW_means` — raw Likert metric, scale means (a_est 0.432; strong, larger scale)

## Finding 1 — the rule cannot detect zero amplitude (false-certification ≈ 1)

At the `c = 0` ladder rung the population amplitude is exactly zero, yet the
current rule certifies displacement as interpretable **100% of the time** in
every metric:

| Fit | Cert_rate @ c=0 | Wilson 95% LCI | Benchmark (α/2) |
|---|---|---|---|
| COR_healthy | 1.000 | 0.992 | 0.025 |
| COR_nearzero | 1.000 | 0.992 | 0.025 |
| RAW_means | 1.000 | 0.992 | 0.025 |

This is **structural**, not sampling noise. At `c = 0` amplitude coverage is a
theorem-zero (`Structural = TRUE`): a percentile interval built from strictly
positive amplitude replicates can never contain 0, so `a_lci > 0` always, and
after rounding to 3 digits it clears the `0.5·10⁻³` threshold every time. The
rule's promise ("when I certify, the amplitude is non-zero") is empty — it
certifies a genuinely zero amplitude with probability ≈ 1 at typical n. This
confirms the spec's prediction (m4-ci-accuracy-spec.md §4.3) with numbers.

## Finding 2 — the rule is scale-blind; a relative statistic separates signal from noise

The implied threshold `0.5·10⁻³` is in **amplitude units** and fixed, so how
much headroom a fit has over it depends entirely on the metric's scale:

| Fit | a_est | a_lci | CI width | a_lci / threshold | **a_lci / CI_width** |
|---|---|---|---|---|---|
| COR_healthy | 0.226 | 0.189 | 0.073 | 377 | **2.58** |
| COR_nearzero | 0.012 | 0.005 | 0.044 | 9 | **0.10** |
| RAW_means | 0.432 | 0.400 | 0.064 | 800 | **6.24** |

The current statistic (`a_lci / threshold`) certifies all three alike — it even
certifies `COR_nearzero`, an amplitude of 0.012 that is substantively noise,
because 9 > 1. The **scale-free ratio `a_lci / CI_width`** cleanly separates the
near-zero case (0.10) from the genuine signals (2.58, 6.24): it is invariant to
a positive rescaling of the score metric (numerator and denominator scale
together) and to the print `digits`. This is the leading candidate family.

## Candidate rule family (for the brief to decide)

A **relative, scale-free** rule: certify when a scale-free function of the
amplitude interval exceeds a fixed constant `k`. Leading forms, all
print-independent and scale-invariant:

1. `a_lci / (a_uci − a_lci) ≥ k` — lower bound as a multiple of CI width (the
   ratio tabulated above).
2. `a_lci / a_est ≥ k` — lower bound as a fraction of the point estimate.
3. amplitude-CI excludes a relative margin (`a_lci ≥ k · a_uci`).

Open sub-questions the seed cannot settle (they need the **new** rule's own
false-cert measured across the ladder — M16 T5):
- Which functional form (1/2/3) and what `k`.
- Whether `k` is calibrated to a coverage target or fixed a priori.
- Behavior of the chosen statistic *at* `c = 0` (this seed characterizes the
  statistic only at the as-estimated `c = 1`; the c=0 distribution of the
  candidate ratio is what calibration must check).

## Proposed false-certification target (for the brief to ratify)

Aim: false-certification at `c = 0` ≤ **α/2** (the diagnostic's existing
user-expectation benchmark; 0.025 at the 95% default), matching what a user
reading a one-sided guardrail would expect. A weaker fallback (≤ 0.05) may be
the achievable compromise if the amplitude-CI's structural positivity resists
tighter control. Final target is the brief's to set and is recorded in the AC3
D-entry; M16 T5 verifies the shipped rule against it.
