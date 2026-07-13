# RR03: Review report — form of the print-independent, scale-free certification rule (M16)

- **Date:** 2026-07-12
- **Brief:** `cairn/reviews/RB03-cert-rule-form.md`
- **Reviewer:** independent Fable-tier statistical review
- **Verdict in one line:** a lower-bound ratio rule **works** — `a_lci / (a_uci − a_lci) ≥ 0.35` is
  asymptotically pivotal at c = 0, nearly n- and metric-invariant, and drives
  false-certification to ≈ α/2 where the shipped rule sits at 1.000.

Materials read: `devel/m16-cert-rule-seed.md`, `devel/m16-cert-rule-seed.R`,
`devel/m4-ci-accuracy-spec.md` (§3.4, §4.3, §12.5, §13), `R/ssm_oop.R:116-193`,
`R/ssm_bootstrap.R:1-190`, `R/ssm_ci_accuracy.R` (docs + simulation loop,
esp. 470-580), `R/ssm_ci_oop.R:40-150, 498-543`, `R/ssm_analysis.R` (object
contents). In addition to the seed, I ran fresh calibration simulations
(reproducible design given in Q3/Q5 below; the throwaway script lived in the
session scratchpad and its full design is specified here so M16 T5 can
re-derive it as a `devel/` extension).

---

## 1. Functional form

**Recommended: form (a), `a_lci / (a_uci − a_lci) ≥ k` — with the material
observation that (a) and (c) are the *same rule*.**

Algebra first: for `a_uci > a_lci > 0`,

```
a_lci/(a_uci − a_lci) ≥ k  ⟺  a_lci(1+k) ≥ k·a_uci  ⟺  a_lci/a_uci ≥ k/(1+k)
```

so (a) and (c) are monotone reparameterizations of one another (k = 0.35 in
(a) ⟺ k′ = 0.259 in (c)). My simulations confirm this numerically (the null
quantiles of `a_lci/a_uci` equal `T/(1+T)` of form (a)'s quantiles to 4
decimals). The choice between them is presentational and edge-case only; the
real decision is (a)/(c) vs (b).

Criteria, per the brief:

- **Invariance.** All three forms are invariant to a positive rescaling of the
  score metric (numerator and denominator carry the same scale factor) and
  contain no `digits`, hence are print-independent. No discriminating power
  here.
- **Monotone behavior as true amplitude → 0.** This kills form (b).
  Asymptotically, under a₀ = 0 the first-harmonic coordinates (x̂, ŷ) are
  ~bivariate normal about 0 with scale σ, so â/σ = ν ~ Rayleigh(1) (isotropic
  case) and the bootstrap amplitude replicates are ~σ·Rice(ν, 1). Then:
  - (a): T = Q₀.₀₂₅(ν)/(Q₀.₉₇₅(ν) − Q₀.₀₂₅(ν)) is **increasing in ν** and
    stochastically decreases as the signal fades. Correct direction.
  - (b): `a_lci/a_est` = Q₀.₀₂₅(ν)/ν → **∞ as ν → 0** (the Rice lower quantile
    tends to the Rayleigh 2.5% point ≈ 0.225 while the denominator vanishes) —
    the statistic *blows up in exactly the worst case*, a tiny estimate whose
    bootstrap distribution sits above it (noncentrality bias). Measured: under
    the idealized null the 97.5% quantile of (b) is **1.03** (realistic
    configurations: 1.09–1.65), and P(b-statistic ≥ 0.42) = **0.17** — where
    0.42 is exactly what the seed's COR_nearzero fit displays
    (0.005/0.012). No usable threshold exists. **Reject (b).**
- **Denominator robustness.** (a): width can only be 0 for a degenerate
  replicate set; `Inf`/`NaN` are caught fail-closed by an `is.finite()` guard
  (Q6). (c): bounded in [0, 1], denominator positive whenever replicates are;
  equally safe. (b): denominator ≈ 0 occurs *in the regime the rule polices* —
  structurally unsafe, not just an edge case.
- **Interpretability.** (a) reads "the amplitude CI's lower bound sits at
  least k CI-widths above zero" — a margin in units of the fit's own
  uncertainty, and via the normal approximation `a_lci ≈ â − z·SE`,
  `width ≈ 2z·SE`, it is a monotone transform of the t-like ratio â/SE
  (T ≥ k ⟺ â/SE ≥ z(1 + 2k) ≈ 3.3 at k = 0.35, 95% CIs). (c) reads "the lower
  bound is at least a quarter of the upper bound", which is serviceable but
  refers to an arbitrary-feeling fraction of another random endpoint.
- **Better candidates considered.** A midpoint-based ratio
  `(a_lci + a_uci)/(2·width)` is also pivotal but its certification event no
  longer implies "the CI excludes zero with margin", severing the link to the
  printed guardrail's meaning. No candidate beats (a).

**Recommendation: form (a)**, keeping continuity with the seed's tabulated
statistic (2.58 / 0.10 / 6.24) and the most teachable reading. Document the
(c)-equivalence in a comment so nobody later "improves" (a) into (c) thinking
it a different rule.

## 2. Threshold k: fixed or calibrated?

**Both, in the only coherent sense: k is a fixed constant pinned in the
package, whose value is calibrated once — now — from the statistic's null
distribution, then verified by `ssm_ci_accuracy()` in M16 T5.** The brief's
hard constraint (the rule cannot consume the diagnostic at runtime) is
satisfied: nothing about the shipped function depends on any diagnostic run.

The calibration principle: **k is (approximately) the 97.5% point of the
statistic's c = 0 null distribution in the least favorable realistic
configuration**, so that certifying a truly-zero-amplitude population happens
≈ α/2 of the time at the package defaults. The key fact making one constant
possible (full evidence in Q3): the null distribution of T is asymptotically
pivotal — free of the metric's scale, nearly free of n, and free of the score
covariance up to an anisotropy effect that only makes the rule *more*
conservative.

Measured null quantiles of T = a_lci/(a_uci − a_lci) at c = 0:

| Configuration | q95 | q97.5 | q99 |
|---|---|---|---|
| Idealized isotropic pivot (Rayleigh ν; 8000 reps) | 0.237 | 0.284 | 0.347 |
| Idealized degenerate-anisotropy extreme | 0.056 | 0.093 | 0.152 |
| jz2017 RAW means, n = 1166, boots = 500 (500 reps) | 0.255 | 0.311 | 0.371 |
| jz2017 COR (ASPD-like), n = 1166, boots = 500 (500 reps) | 0.245 | 0.304 | 0.345 |
| jz2017 RAW, n = 250, boots = 500 (800 reps) | 0.232 | 0.286 | 0.344 |
| jz2017 RAW, n = 100, boots = 500 (800 reps) | 0.241 | 0.308 | 0.399 |
| jz2017 RAW, n = 50, boots = 500 (800 reps) | 0.273 | 0.346 | 0.399 |
| jz2017 RAW, n = 1166, boots = 2000 (400 reps) | 0.226 | 0.265 | — |

**Recommend k = 0.35.** False-certification at c = 0 with k = 0.35: 0.007
(package-default boots = 2000, large n), 0.010–0.014 (boots = 500, large n),
0.020 (n = 100), 0.025 (n = 50) — i.e., ≤ ≈ α/2 across everything tested,
without collapsing power: the seed's genuine signals score 2.58 and 6.24,
passing k = 0.35 by factors of 7 and 18, while COR_nearzero (0.10) correctly
fails. k = 0.30 is defensible if ≤ 0.05 is the target (rates 0.022–0.052
across configurations) but flirts with the diagnostic's Caution trigger at
small n; k = 0.35's margin costs essentially nothing in the interpretable
regime (certification needs â ≈ 3.3 SEs, i.e. amplitude ≈ 0.06 on the seed's
correlation metric — well below any substantively interpretable amplitude).

## 3. Can a relative rule control false-certification at c = 0? — Yes

This was the crux, and the answer is an unqualified yes, with a clean reason:
**the numerator and denominator shrinking together is precisely why it
works.** Both are proportional to the amplitude sampling scale σ, so their
ratio is asymptotically free of σ and depends only on the pivot ν = â/σ. At
c = 0 that pivot has a *fixed* distribution (Rayleigh(1) when the (x̂, ŷ)
sampling covariance is isotropic — the natural situation for balanced octant
scores), so T has a fixed, non-degenerate null law, and any tail probability
can be dialed in by k. Contrast the shipped rule, whose statistic `a_lci` has
a null law proportional to σ — no fixed threshold in amplitude units can work
across metrics, which is defect 2 in the brief.

The simulations above verify this *beyond* the asymptotic argument, using the
real estimator and real resampling law: multivariate-normal populations built
from `jz2017`'s observed octant covariance (and, for the correlation metric,
the joint (measure, scales) covariance with the measure–scale correlation
profile projected to exact zero first-harmonic amplitude — the same
projection-through-the-estimator-functional the diagnostic's ladder uses),
n = 1166 draws, multinomial-weight bootstrap (the diagnostic's own
implementation shortcut, `R/ssm_ci_accuracy.R:507-511`), percentile 2.5/97.5
quantiles as in `R/ssm_bootstrap.R:117-124`. The realistic null quantiles
(0.311 raw, 0.304 correlation at 97.5%) sit within Monte-Carlo error of each
other and near the idealized pivot value (0.284): metric-invariance and
approximate pivotality hold in practice, not just in the limit.

Two systematic deviations from exact pivotality, both benign:

- **Anisotropy** of the (x̂, ŷ) sampling covariance moves the null *down*
  (degenerate extreme: q97.5 = 0.093 vs isotropic 0.284): a k calibrated to
  the isotropic-ish realistic case is *conservative* under anisotropy, never
  anti-conservative.
- **Small n and small boots** fatten the tail modestly (q97.5: 0.265 at
  n = 1166/boots = 2000 → 0.346 at n = 50/boots = 500). k = 0.35 covers the
  worst tested case at ≈ 0.025.

So: no need for the Q4 fallback. A lower-bound-ratio rule controls c = 0
false-certification to any reasonable target.

## 4. Is a lower-bound rule the right instrument? — Yes; replicate-vector rules add plumbing, not power

Given Q3's affirmative, the replicate-distribution alternatives were assessed
as *potential improvements* and rejected:

- **"Fraction of replicates below a floor" rules are reparameterizations, not
  alternatives.** P*(a* < x) ≤ p ⟺ q_p(a*) ≥ x; with any scale-free floor
  (x = r·a_uci, r·median(a*), r·a_est) this is exactly a quantile-ratio rule —
  the same family as (a)/(c), just reading different quantiles. An
  equivalence/ROPE-style test needs a near-zero region, and making that region
  scale-free reintroduces the same relative floors. There is no new
  information: asymptotically the entire bootstrap amplitude law is
  σ·Rice(ν, 1), determined by the same pivot the CI endpoints already encode.
  Finite-sample shape features (skewness, CV) would be second-order tweaks on
  top of an already target-meeting rule.
- **Plumbing cost is real.** The `circumplex_ssm` object stores `results`,
  `scores`, `details` — **not** the replicate matrix (`new_ssm()` calls at
  `R/ssm_analysis.R:421-425` and `543-547`). A replicate-consuming rule would
  need a new statistic computed inside `ssm_replicate_intervals()` and stored
  per row, would return `NA`/error on every pre-change object on disk, and
  would force the five consuming surfaces to depend on object vintage. The
  CI-pair rule works on **every existing stored object** (`a_lci`, `a_uci` are
  always in `results`) and identically for the bootstrap and Monte Carlo
  engines (both funnel through `ssm_replicate_intervals()`,
  `R/ssm_bootstrap.R:52-58`, and the MC amplitude replicates are likewise
  strictly positive norms of Gaussian draws, so the same pivot argument
  applies).

**Reject the replicate-vector route** — with the note that if some future
need arises, the identical rule *can* be evaluated replicate-side (it is a
function of two quantiles of the replicate vector), so nothing is foreclosed.

## 5. False-certification target

α/2 = 0.025 **as a hard bracket is the wrong kind of promise** — the rule has
no nominal level (the CI-excludes-zero ⟺ α/2-test duality already fails here,
as spec §4.3 correctly argues), its null law is only asymptotically pivotal,
and the operating point moves modestly with n, `boots`, and `interval`.
But the evidence shows ≈ α/2 is *achievable* at k = 0.35, so M16 should not
retreat to a bare ≤ 0.05 either. Recommend a **two-part acceptance target**:

1. **Hard gate:** observed false-certification at the c = 0 rung ≤ **0.05**
   (point estimate) in *every* verification configuration.
2. **Caution gate:** the diagnostic's stored guardrail `Caution`
   (`ssm_ci_guardrail_caution()`, `R/ssm_ci_oop.R:51-53` — Wilson 95% LCI of
   Cert_rate at c = 0 exceeding the α/2 benchmark) must **not fire** in any
   verification configuration. This reuses the diagnostic's own
   noise-immunized trigger and is the user-visible promise: the guardrail
   verdict line flips from the current "certified 100% of the time under a
   truly zero amplitude" indictment to the benign wording. At reps = 1000 the
   trigger fires when the observed rate exceeds ≈ 0.035, so k = 0.35's
   expected rates (0.007–0.025) pass with margin, while a materially
   miscalibrated rule cannot slip through.

**Verification protocol (M16 T5):** re-run the seed generator's design with
the new rule (the swap is automatic — the diagnostic calls `ssm_certified()`
at `R/ssm_ci_accuracy.R:564`, honoring the single-definition doctrine) at
**reps = 1000**, on the three seed configurations (COR_healthy, COR_nearzero,
RAW_means) **plus one small-n configuration** (e.g. a jz2017 subsample of
n ≈ 100, raw and measure paths — small n is where pivotality is weakest, per
the Q2 table). Read `guardrail$Cert_rate`, `Cert_lci`, `Caution` at
Condition = 0 for gates 1–2; additionally record `Cert_rate` at
c ∈ {0.25, 0.5, 1} (the power curve — expect ≈ 1 at c = 1 for the two genuine
signals, and expect COR_nearzero to now *fail* certification at c = 1, which
is the desired behavior change, not a regression) and the
`Coverage_conditional` displacement panel (conditional coverage at c = 1
should be ≥ the unconditional, as certification now selects informative
replicates).

## 6. Contract and edge cases

**Recommended implementation** (drop-in for `R/ssm_oop.R:122-124`; stays the
single definition both `print.circumplex_ssm()` and the diagnostic call):

```r
ssm_certified <- function(a_lci, a_uci, k = 0.35) {
  ratio <- a_lci / (a_uci - a_lci)
  is.finite(ratio) & ratio >= k
}
```

Vectorized, base R, no dependency. `k` is a pinned package constant — it must
**not** be exposed as a `print()` argument (that would recreate defect 1 in
display-knob form); the `digits` parameter disappears from the rule entirely.

Edge-case returns, in the brief's order:

- **`a_lci = NA`** (flat / zero-variance profile): `ratio` is `NA`,
  `is.finite(NA)` is `FALSE` → **FALSE, not certified** — preserves the
  current contract exactly.
- **`a_est = 0` exactly:** the rule does not consult `a_est` at all (a
  strength: the recommended form is a pure function of the CI *pair*, a
  subset of the permitted triple). The CI decides; with any real replicate
  spread around a zero estimate the ratio is far below k → FALSE.
- **Degenerate CI `a_lci = a_uci`:** width 0 gives `Inf` (if the common value
  is > 0) or `NaN` (if 0, the all-replicates-flat case); both fail
  `is.finite()` → **FALSE, fail-closed**. Deliberate: a zero-width bootstrap
  interval is a pathological resample, and a guardrail's failure mode should
  be silence, not endorsement. (`a_lci > a_uci` cannot occur — percentile
  quantiles are monotone in p.)
- **Very small n:** the rule is defined and pure at any n; what changes is the
  operating characteristic (null tail fattens slightly, covered by k = 0.35
  down to n = 50 in the Q2 table), and `ssm_ci_accuracy()` measures it at the
  user's own n — which is that diagnostic's whole job.

**Purity and the consuming surfaces.** The rule is a pure vectorized function
of `(a_lci, a_uci)`, so every surface applies it identically with data it
already has: the print note (`R/ssm_oop.R:183` — passes `dat$a_uci` instead of
`digits`), the diagnostic loop (`R/ssm_ci_accuracy.R:564` — `lean$uci[, a_col]`
is already computed two lines up), and the `summary()` verdict
(`R/ssm_ci_oop.R:114-143`), plot certified panel (`R/ssm_ci_oop.R:526-543`),
and conditional-coverage/`Cert_rate` columns, which consume the diagnostic's
stored outputs rather than calling the rule and therefore update for free.
Contrast rows remain ungated (M15-D1): the print guard at
`R/ssm_oop.R:174-175` and the diagnostic's joint-certification descriptive
(`R/ssm_ci_accuracy.R:565`) are untouched by the signature change. Works
retroactively on any stored `circumplex_ssm` object (both columns have always
existed in `results`).

---

## Beyond the brief

1. **The printed note's wording must change with the rule.** "The amplitude CI
   includes zero" (`R/ssm_oop.R:185`) was never literally true of a percentile
   interval of positive replicates, and under the new rule the note fires for
   CIs that are strictly positive yet too close to zero relative to their
   width. Suggested wording, meeting the vignette style bar (never describe an
   angular/amplitude CI as a significance test): *"Note: the amplitude is not
   reliably distinguishable from zero (its CI lower bound is less than 0.35
   CI-widths above zero); the displacement is not interpretable."* The same
   fix is needed in `vignettes/evaluating-circumplex-structure.Rmd:245, 313,
   359` and the guardrail wording in `R/ssm_ci_oop.R:127-142` ("the
   'amplitude CI excludes zero' rule…").
2. **`ssm_ci_accuracy()`'s `digits` argument and `Threshold` output column
   become vestigial** (`R/ssm_ci_accuracy.R:88-90`, docs at lines 30-42, the
   scale-dependent-threshold echo). Replace with an echo of the dimensionless
   `k`; deprecate `digits` gracefully (warn-and-ignore) since it is a
   documented argument on CRAN. Snapshot tests for print/summary/plot will
   need regeneration — the COR_nearzero-style fixture flipping from certified
   to not-certified is the headline *intended* behavior change and deserves
   its own regression test and NEWS sentence.
3. **Interval-level coupling (flagged, not silently accepted).** The statistic
   is built from the object's own `interval`-level quantiles, so the
   certification stringency tracks the user's chosen confidence level
   (interval = 0.80 loosens the gate to roughly a 2.2-SE rule ⇒ null
   false-cert ≈ 5–9%; 0.99 tightens it to ≈ 4.4 SE). Directionally coherent
   (looser CIs, looser gate) and measured at the user's own settings by the
   diagnostic, but the k = 0.35 calibration is *for the 95% default* and the
   docs should say so. A principled generalization exists if wanted later —
   `k(interval) = (t*/z_γ − 1)/2` with pinned t* ≈ 3.3,
   `z_γ = qnorm(1 − (1 − interval)/2)`, still a pure function of the fit —
   but I recommend the simple pinned constant plus a documentation sentence
   now.
4. **Statistical-invariant tests M16 should add** (per CLAUDE.md's boundary
   doctrine): certification at a profile peaking at 0°/360° (rule must be
   angle-blind), the flat-profile NA path, the degenerate-CI fail-closed path,
   a scale-invariance unit test (multiply raw scores by 1000; certification
   verdict identical), and a print-independence test (verdict identical across
   `digits` values).
5. **Oracle note.** Per the repo's validation doctrine, the k-calibration can
   be independently cross-checked against the closed-form pivot: under the
   isotropic null the exact asymptotic false-cert at threshold t* is
   exp(−t*²/2) (Rayleigh tail) with t* ≈ z(1 + 2k) — e.g. k = 0.35 ⇒
   t* ≈ 3.33 ⇒ 0.0039, consistent with the boots = 2000 measurement (0.007)
   once bootstrap-bias and quantile noise are added. `devel/g2xx1.txt` must
   not be used for any of this (untrusted, per standing note).

## Recommendations

- **Apply:** replace `ssm_certified()` with the pure function
  `is.finite(a_lci/(a_uci − a_lci)) & a_lci/(a_uci − a_lci) ≥ 0.35` (Q1, Q2,
  Q6 contract). Single definition; both call sites move together.
- **Apply:** two-part acceptance target — false-cert@c=0 ≤ 0.05 point estimate
  AND the diagnostic's Wilson-LCI `Caution` not firing — verified at
  reps = 1000 on the three seed configurations plus one small-n
  configuration, with the c > 0 power curve recorded (Q5).
- **Apply:** rewrite the print-note, verdict, and vignette wording to match
  the new rule's actual event (Beyond-the-brief 1).
- **Apply:** boundary/invariance regression tests (Beyond-the-brief 4).
- **Consider:** deprecation path for `ssm_ci_accuracy(digits =)` and the
  `Threshold` column → replace with a `k` echo (Beyond-the-brief 2; the
  mechanics are M16's to schedule, possibly a follow-up task).
- **Consider:** a documentation sentence pinning the calibration to the 95%
  default interval; the `k(interval)` generalization only if non-default
  intervals prove common (Beyond-the-brief 3).
- **Reject — replicate-vector / ROPE-style rules:** re-parameterizations of
  the same quantile information with real plumbing costs (object stores no
  replicates; legacy objects break) and no measurable control gain (Q4).
- **Reject — form (b) `a_lci/a_est`:** non-monotone as amplitude → 0; its null
  statistic diverges in exactly the regime the guardrail polices; no viable
  threshold (97.5% null quantile ≈ 1.0–1.65) (Q1).
- **Reject — keeping α/2 as a hard nominal-level claim:** the rule has no
  nominal level; α/2 survives as the benchmark inside the Caution gate, which
  is the honest form of the promise (Q5).

## Recommended rule (single statement for M16)

**Certify a profile row's displacement iff
`is.finite(r) && r ≥ 0.35`, where `r = a_lci / (a_uci − a_lci)`** — a pure,
vectorized, base-R function of the amplitude CI pair only; print-independent
(no `digits` anywhere) and scale-free (invariant under any positive rescaling
of the score metric); equivalent to `a_lci ≥ 0.259 · a_uci`. Edge contract:
`NA` lower bound → not certified; degenerate zero-width CI → not certified
(fail-closed); `a_est` never consulted; contrast rows never gated (M15-D1).
Verify against: false-certification at the c = 0 ladder rung ≤ 0.05 with the
diagnostic's Wilson-LCI Caution not firing, at reps = 1000, across
COR_healthy / COR_nearzero / RAW_means plus one small-n (≈ 100)
configuration; expected operating point ≈ 0.007–0.025, i.e. at or under the
α/2 = 0.025 user-expectation benchmark that the current rule misses by a
factor of 40.
