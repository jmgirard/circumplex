# M5 T1 review: adversarial review of `devel/m5-sem-design.md`

**Status:** independent review of the 2026-07-07 spec, *post* the same-day
4-angle review (its 16 accepted findings, per the spec's change log, are
treated as already folded in; nothing below merely re-reports them —
change-log claims were re-verified only where load-bearing, and all held).
**Reviewer:** Fable, fresh session, 2026-07-07. No involvement in the spec,
Brief E, or the same-day review; judged only what is on the page, against the
binding sources (Brief E §M5, MILESTONES.md M5, CLAUDE.md, DESIGN.md), the
shipped code, and seeded numerical probes (probe script preserved in the
session scratchpad, `m5_review_checks.R`; key numbers reproduced inline).

## Verdict: ACCEPT WITH CHANGES

The load-bearing architecture survives adversarial checking: the
profile-then-transform estimand is well-defined (and invariant to the model's
reflection indeterminacies), the OLS-weights decision and its
harmonic-balance characterization are exactly right (re-derived and
reproduced numerically), the scaled tier is locally identified with free
g–plane covariances at p ≥ 6 (verified by Jacobian rank, consistent with the
spec's indicative gates), the CI architecture correctly reuses the tested
circular machinery with no interval logic re-derived, and every claim the
spec makes about shipped package code checked out true. Oracle hygiene is
clean — I found no number from memory presented as fact.

What must change: §4.3's direction-integrity claim is **false as written**
(stationarity is not sufficient — verified counterexample below), §4.2's
fit attribution is incomplete for the same underlying reason, and three
under-specifications (the misspecified-cell truth in §8.3, the Θ→0 pin's
mechanism in §8.2, the non-reference-group factor metric in §6) would
otherwise surface as design work mid-implementation, in Fable-critical
territory. All are spec-text fixes; no architectural rework.

---

## Required changes (prioritized)

### F1 (high) — §4.3's direction-integrity claim is false as written: stationarity is necessary but not sufficient

§4.3: "d\* = atan2(y\*, x\*) is undistorted as a latent-plane direction
**under §3.1's stationarity assumption** … — conditional on that assumption
holding". The same-day review made the claim conditional on stationarity;
that condition is not enough. Under *exact* stationarity (unit isotropic
orthogonal plane) and even zero g–plane covariance, heterogeneous
saturations rotate d\* away from the measure's latent-plane direction.

Evidence (seeded probe, octant angles, measure purely in the plane at
δ = 20°, σ_Mg = 0, φ_gx = φ_gy = 0, a_i ≡ 0.6 — stationarity holds exactly):

- homogeneous c_i ≡ 0.5: d\* = 20.000°, fit = 1 (control — exact recovery);
- c_i = 0.5 + 0.3·cos 2θ_i (range .2–.8): **d\* = 13.59°** — a 6.4°
  rotation — with fit = **.982**, comfortably above the .70 guardrail, so
  nothing in the shipped interpretation machinery flags it;
- same pattern phase-shifted 70°: d\* = 25.00° (direction of the error
  depends on the saturation pattern's phase, so it does not cancel);
- heterogeneous a_i only (a_i = 0.6 + 0.25·cos(2θ_i − 70°), c_i
  homogeneous): d\* = 16.18° — the general-factor saturations distort too,
  through Var(t_i) in ρ\*'s denominator.

Mechanism: ρ\*_i = w_i·cos(θ_i − δ) with w_i = c_i/SD(t_i); any
second-harmonic component of w_i beats against the first harmonic and
shifts its direction. d\* equals the measure's latent-plane angle
atan2(σ_My, σ_Mx) exactly only when the effective saturations are
homogeneous around the circle **and** the g–plane covariances are zero
(both a_i·σ_Mg in the numerator and the 2a_ic_i·(cos θ_i·φ_gx +
sin θ_i·φ_gy) term in Var(t_i) otherwise modulate the profile — see F2).
Note the strict tier is no refuge: its Φ is free, so it does not even
impose the stationarity §4.3 conditions on.

This is not a defect of the estimand — the transform of ρ\* is a
well-defined summary, and the *observed* d has the same property in worse
form (reliability modulation on top of saturation modulation; the latent
layer removes exactly the reliability part, since ρ_i = ρ\*_i·√rel_i). But
the spec's sentence licenses an interpretation the estimand does not
support, and §4.2–§4.3 are the designated trace targets for T5's vignette
and the T3 Rd docs — "every reported CI/estimand claim traces to T1's
spec". Shipping it as-is ships plausible-but-wrong teaching about a
headline output.

**Required:** (i) reword §4.3: what the fixed isotropic metric buys is
that the transform adds no axis-anisotropic distortion of its own (the
genuine content of the rejected-design contrast); d\* is the
first-harmonic direction of the saturation-modulated disattenuated
profile, equal to the measure's factor-space angle only under homogeneous
effective saturations and zero g–plane lean, and its improvement over the
observed d is precisely the removal of reliability modulation — the §6.4
move ("shared with the existing estimand, not a distortion the SEM layer
introduces"), applied here. (ii) Add the corresponding §10 limitation
bullet. (iii) Consider one §8.1 analytic cell with heterogeneous
saturations so the vignette's caveat carries a demonstrated magnitude
(design-time computation, not a literature number). Sections: 4.3, 4.2,
10, (8.1).

### F2 (medium) — §4.2 attributes latent fit < 1 to differential saturation alone; the free g–plane covariances are a second channel

§4.2: "the latent fit measures how far the measure's latent profile departs
from a pure cosine wave (**differential saturation of the scales**)".
Incomplete: with *equal* saturations (a_i ≡ .6, c_i ≡ .5) and a modest
g–plane lean (φ_gx = .4, φ_gy = .2, σ_Mg = .3), the probe gives
fit = **.988** (and d\* shifted to 19.02°); the zero-lean control gives
fit = 1 exactly. Var(t_i) = a² + c² + 2ac·(cos θ_i·φ_gx + sin θ_i·φ_gy)
carries a first harmonic whenever the g factor leans into the plane —
which §3.1 *defaults to allowing*, on the argument that real general
factors do lean. Under the strict tier, anisotropic Φ̂ is a third channel.
So a user with perfectly uniform saturations can still see latent
fit < 1, and the vignette as traced would tell them their scales are
differentially saturated.

**Required:** name both channels in §4.2 (differential saturation;
Var(t_i) modulation from g–plane covariance, plus Φ̂ anisotropy under
strict), and carry the correction into the §10 bullet and T5's teaching
table. One sentence each. Sections: 4.2, 10.

### F3 (medium) — §8.3's realistic-misspecification cells have no defined truth, so their "coverage" is not yet a measurement

§8.3 builds realism cells from `cpm_fit()`'s P̂ plus observed cross-
correlations and says coverage "is thereby also measured **under realistic
misspecification** of the fixed-angle model … part of the verdict". But the
latent estimand is model-conditional: when the population was not generated
by the §3.1 model, there is no closed-form ρ\*₀, and the spec never says
what value the CIs are supposed to cover. The B-spec's plug-in philosophy
does not transfer silently — there the functional was directly computable
on the population matrix; here the estimand requires a model fit. Without a
pinned target, an implementer must invent one mid-T3 (the classic place
plausible-but-wrong slips in), or the Bradley/Wilson verdict gets applied
to an ill-posed rate.

**Required:** pin the target. The natural choice is the **pseudo-true
value** (the QMLE probability-limit sense, White 1982, *Econometrica* —
direction citation, no numbers): fit the model tier to the repaired
population joint matrix itself (lavaan on population moments), apply the
§4.1 map and §2 transform, and measure coverage of that value; state
explicitly that these cells assess coverage of the model-conditional
estimand under misspecification, not of any "true" circumplex parameter.
Alternatively, re-scope the realism cells as descriptive robustness cells
excluded from the banded verdict. Either is defensible; the spec must say
which. Sections: 8.3.

### F4 (medium) — the §8.2 Θ→0 equivalence pin is untestable as specified

"with (near-)zero residual variances the disattenuated profile equals the
observed profile, so `ssm_sem()`'s point estimates must match
`ssm_analyze()`'s on the same data … to numerical tolerance." Three
collisions the spec does not resolve:

1. At Θ ≈ 0 the population scale block is rank 3; fitted residual
   variances sit on the boundary and lavaan emits exactly the
   negative/near-zero-variance warnings that §4.5's global-health gate
   says must be surfaced as refusals — the pin's own guards would refuse
   the fit the pin needs.
2. At any finite n with moderate Θ, the fitted model has df > 0 and
   smooths the sample moments, so the model-implied ρ̂\* never equals the
   observed sample profile exactly — the match is O(1 − reliability) plus
   misfit, and "numerical tolerance" is unspecified and cannot be machine
   precision. Written literally, the test is either vacuous (loose
   tolerance) or flaky (tight tolerance).
3. `ssm_analyze()`'s observed profile is a saturated statistic; the
   equivalence is exact only in the population limit.

**Required:** specify the mechanism. Cleanest: run the pin at the
population level — construct the analytic Σ from the §3.1 model at small
positive Θ, hand it to lavaan as `sample.cov` (misfit is exactly zero at
the optimum, so parameters recover the generating values), and verify the
estimand map's output converges to the observed-profile functional as
Θ → 0 along a stated ladder with a stated tolerance schedule; keep a
data-level smoke test only with a documented loose tolerance. Sections:
8.2 (and a cross-reference from §4.5 noting the boundary interaction).

### F5 (medium) — the multi-group scaled tier's factor metric in non-reference groups is unspecified, and it is load-bearing for cross-group direction comparability

§6.4: under metric invariance "factor covariances — including SD(t_i)'s
group differences … — stay free". But §3.1's scaled tier *fixes*
var(g) = var(cx) = var(cy) = 1, cov(cx, cy) = 0. In a multi-group model
with (a_i, c_i) equality-constrained, the non-reference groups' factor
(co)variances must be freed for the metric constraint to be testable —
and the spec never says *which shape* the freed Φ_g takes. If Φ_g is fully
free, group 2's plane can go anisotropic/oblique, which is exactly the
direction distortion §3.1/§4.3 exclude ("a free-plane variant is
deliberately not offered in v1") — reintroduced silently, and worse:
asymmetrically, so the Δd\* contrast confounds axis-metric differences
with genuine rotation. If instead per-group isotropy is imposed
(var(cx_g) = var(cy_g) = φ_g free, cov(cx, cy)_g = 0), that is an
*additional per-group stationarity assumption* that must be stated,
emitted by the generator, and listed in §10. The identification of the
freed parameters under the equality constraints also needs the same T2
check §3.4 promises for the single-group model.

**Required:** pin the multi-group parameterization per tier — proposed:
reference group as §3.1; non-reference groups free var(g_g), free g–plane
covariances, and a single free plane scale φ_g with isotropy/orthogonality
retained (per-group stationarity, documented as an assumption exactly as
§3.1 does); extend §3.4's identification gate to the multi-group
constrained models; state it in §3.5's emission contract for
`invariance =` syntax. Sections: 6.2, 6.4, 3.4, 3.5, 10.

### F6 (low) — the Δχ² verdict is silent on robust estimators

§6.2 defaults the invariance verdict to "the nested Δχ² test (a computed
quantity)", and §7.2 passes `estimator` through (with "ML" merely the
default). Under MLM/MLR-family estimators the naive difference of scaled
χ² statistics is not χ²-distributed; the scaled difference test
(Satorra & Bentler 2001, *Psychometrika* — direction citation) is
required, which `lavaan::anova()` applies automatically. One sentence
pinning "the verdict statistic is lavaan's own nested-model test, which is
the scaled difference under robust estimators" closes the gap — still a
computed quantity, no literature constant. Sections: 6.2, 7.2.

### F7 (low) — §4.5's inadmissible-draw causes omit Var(M) ≤ 0, and "Var(M) observed/implied" needs pinning

The engine-side filter enumerates Var(t_i) ≤ 0 and |ρ\*_i| ≥ 1. Var(M) is
also a free parameter drawn unconstrained by the MVN engine; an excursion
to Var(M)⁽ᵇ⁾ ≤ 0 makes the whole draw undefined by the same logic and
should be named in the same filter (not left to NaN propagation, whose
comparison semantics silently differ). Relatedly, §4.1's "Var(M)
observed/implied" is ambiguous, and under `missing = "fiml"` an "observed"
variance is not even well-defined; pin Var(M) to the model-implied value
everywhere (which equals the saturated-block estimate at the optimum).
Sections: 4.1, 4.5.

### F8 (low) — §7.2's `invariance` default is incoherent for the mean path, and single-group `measures = NULL` is undefined

The signature defaults `invariance = "metric"` ("ladder always fitted up
to `invariance`"), but `measures = NULL` selects the mean path, which §6.2
gates on **scalar**. Under the defaults, the mean path arrives at a state
the spec has no words for: not "invariance rejected" (§6.3's
non-comparison is for *failed* steps) but "required step never tested".
Also unstated: `measures = NULL` with `grouping = NULL` has no product at
all (§1.3 excludes single-group mean-based SSM) and must be a validation
error. **Required:** make the default path-dependent (scalar when
`measures = NULL`) or validate with a clear error; state the single-group
mean-path refusal in §7.2's validation contract. Sections: 7.2, 6.2.

### F9 (low) — MILESTONES T2's acceptance line still encodes the falsified "only when equally spaced" test

The spec (§2.1/§3.5, change log) corrects closed-form ≡ OLS to the
harmonic-balance condition and re-aims the T2 test at a balance-violating
angle set — but MILESTONES T2's acceptance still reads "emitted weights
equal the closed-form **only when angles are equally spaced** and the OLS
weights otherwise". An implementer working from MILESTONES literally could
assert the "only" direction, which the spec's own counterexample (and my
reproduction, F-clean list) falsifies: a balanced-but-unequal set matches
the closed form to machine precision. The spec amended T3's wording the
same day but left T2's stale. **Required:** the same amendment treatment
for T2's acceptance line (or an explicit pointer that spec §3.5 supersedes
its test description). Sections: 3.5/11; MILESTONES.md T2 (at revision
time, per the workflow rule).

### F10 (low) — the strict-tier inspection `:=` lines are covariance-metric quantities; "superseded by the package's own intervals" misstates the relationship

§3.5 emits e/x/y `:=` lines under strict "where the covariance-metric
profile is linear in free parameters", commented as inspection values
"whose SEs are lavaan's delta approximations, superseded by the package's
own intervals". But the package's reported parameters transform the
**correlation-metric** ρ\* (§4.1), so the `:=` *values* — not just their
SEs — will differ from the reported e/x/y. A user comparing
`summary(fit)`'s `elev` to the printed elevation will see two different
numbers and the comment as worded implies they are the same quantity with
different intervals. §8.4's cross-check is correctly scoped ("the same
quantity" — the covariance-metric one, checked against MVN propagation of
itself) but §3.5's comment contract is not. **Required:** label the `:=`
lines with their metric (e.g., `cov_e`-style names plus a comment:
covariance-metric inspection values, not the reported latent SSM
parameters), and one clarifying clause in §8.4. Sections: 3.5, 8.4.

### F11 (informational) — the memory-file invariant "closed-form = OLS only for equally spaced angles" is now known-imprecise; record the sharpening

CLAUDE.md and DESIGN.md both state the iff-equal-spacing form. The spec's
harmonic-balance characterization (verified exactly, see clean list) shows
equal spacing is sufficient but not necessary. The safe direction every
existing use relies on (equal spacing ⟹ equality) is untouched, so nothing
shipped is wrong — but once T2's tests encode the balance condition, the
memory files will contradict the test suite's own counterexample fixture.
The spec already amends DESIGN.md's lavaan note (§7.4); add the invariant
sharpening to the same at-implementation amendment list. Sections: 7.4
(amendment list); DESIGN.md/CLAUDE.md at T2.

### F12 (informational) — reflection indeterminacies are real, harmless, and worth one sentence in §3.4

The scaled tier has two discrete sign indeterminacies (g → −g with
a_i, σ_Mg negated; (cx, cy) → (−cx, −cy) with c_i, σ_Mx, σ_My negated).
Derivation check: the §4.1 estimand map is exactly invariant under both
(all products preserved), as is the §6.4 mean path (α_g1 flips with the
a-column) — so a `"boot"` replicate converging to the opposite reflection
still yields the identical ρ\*⁽ᵇ⁾, an elegant robustness property of
profile-then-transform. But T2's local-identification check should be told
about them (discrete indeterminacy does not reduce information-matrix
rank, so no flip of the §3.1 default is warranted on their account), and
T3's docs can note the boot-engine invariance. Also worth one cross-ref:
the ported amplitude guardrail (§5.2) inherits B-review F1.ii's known
display-precision-threshold caveat, now additionally on a raw-score metric
for the mean path — a package-level open item, not an M5 defect.
Sections: 3.4, 5.2.

---

## Verified and found clean (genuine effort, no break)

- **Harmonic-balance characterization (§2.1), algebraically and
  numerically.** Derived: (BᵀB)⁻¹Bᵀ equals the closed form iff
  BᵀB = diag(p, p/2, p/2) iff Σcos = Σsin = 0 and Σcos 2θ = Σsin 2θ = 0
  (via Σcos² = (p + Σcos 2θ)/2 etc.; B full rank makes the matrix
  identity force (BᵀB)⁻¹ = diag(1/p, 2/p, 2/p)). Both numerical
  assertions reproduced: interleaved rotated 4-grids give max
  |W − closed| = 8.3e−17; {0°, 30°, 90°, 200°, 290°} gives 0.1754
  (spec: ≈ 8e−17, ≈ 0.18). Equal spacing implies the condition; the T2
  test re-aim is correct (modulo F9's MILESTONES wording).
- **Scaled-tier identification (§3.1/§3.4).** Numeric Jacobian of
  vech(Σ) at generic interior points: full column rank 3p + 2 with free
  g–plane covariances at p = 6, 7, 8 (smallest singular-value ratios
  ~.02, locally identified and not near-singular); rank-deficient at
  p = 5 (rank 15 < 17, matching the counting) and at p = 4 even with the
  g–plane fixed (rank 9 < 12). The spec's proposed free-by-default and
  its "roughly p ≥ 6" indicative gate are both consistent with the
  numerics; the T2 flip plan has a live but currently un-triggered
  trigger. The §3.4 df arithmetic (26/14 free, df 10/22 at p = 8;
  14 > 10 at p = 4) checks out.
- **§3.1's elliptical-plane claim** reproduced: an isotropic-metric
  scaled model cannot reproduce a Φ_plane = diag(2, 1) population
  (best least-squares residual 0.27, not ~0).
- **§6.4's strict-tier exactness and scaled-tier non-exactness**
  reproduced: W recovers α to 1.1e−16 under Λ = B; heterogeneous-loading
  recovery error 0.26.
- **The estimand's disattenuation algebra (§4.1).** ρ_i = ρ\*_i·√rel_i
  (scale-side correction only, measure observed by contract) — coherent,
  matches the stated "exact latent analog", and makes F1's honest framing
  available. OLS fit ∈ [0, 1] at any spacing (§2.1): confirmed by the
  nesting argument (intercept-only OLS returns the mean, so
  SSE_OLS ≤ SST).
- **Every claim about shipped code, spot-checked true.**
  `summary.circumplex_ssm()` keys on `details$method == "montecarlo"`
  else prints "Bootstrap Resamples" plus a Listwise line
  (R/ssm_oop.R:184–200) — `method = "mvn"` would indeed print a false
  method statement. `ssm_ci_accuracy()` gates on
  `inherits(x, "circumplex_ssm")` (R/ssm_ci_accuracy.R:173) and its
  method switch (line 192: unknown/`"mvn"` falls into the bootstrap
  replay branch) makes "meaningless, silently" accurate, not rhetorical.
  The degenerate-replicate warning's cause-specific wording
  (R/ssm_bootstrap.R:91–101), fit CI suppression, contrast branch
  alignment, `ssm_replicate_intervals(t0, t, interval, contrast,
  replicate_label)` signature, `param_diff()`/`angle_dist()` semantics,
  `mvn_draws()`/`mvn_root()` single-root convention, and
  `ssm_montecarlo()`'s |r| ≥ 1 − 1e−12 refusal (R/ssm_montecarlo.R:111)
  all match the spec's descriptions. `cpm_simulate()` is scale-only per
  its shipped Rd contract (R/cpm_fit.R). `circumplex_instrument`
  exposes `$Scales$Angle`/`$Scales$Abbrev`. lavaan is already in
  Suggests (DESCRIPTION), so §7.4's "zero net-new" holds.
- **The devel-sketch characterizations.** `circum_lavaan.Rmd`'s model0
  is exactly .71·(1, cos θ_i, sin θ_i) with Φ free (checked against the
  jz2017 octant angles), so §3.2's rescaling-equivalence framing is
  right; `lavaan_ssm.Rmd` does hard-code the 0.25/±0.7071068 closed-form
  weights and `std.ov = TRUE`, the two traps §3.5/§4.4 ban.
- **"All shipped instruments are equally spaced" (§2.1):** verified —
  all 15 instrument objects have p = 8 scales at uniform 45° spacing.
- **Boundary suite vs CLAUDE.md (§5.5):** all four danger zones covered
  (0°/360° pole with straddling CI, a ≈ 0, flat, ±180° contrast) plus
  the unequal-spacing functional test, correctly aimed at a
  balance-violating set.
- **Oracle hygiene:** every fixed number is a package convention
  verified in code (2000/0.95; 1e−12), a labeled design default (5%),
  or an indicative count flagged for T2 re-derivation; ΔCFI and the
  Cudeck citation are TBT; the .71 literal is explicitly banned from
  transcription; the two "verified in review" numerics were re-verified
  here. Brief E deviations (§4.4, §5.2, §8.3) are all flagged inline as
  required. MILESTONES T3's amendment is already in place and matches
  §5.2/§8.3 (T2's is not — F9).
- **Traceability (§11):** every T2–T5 acceptance hook resolves to a real
  section; T1's own acceptance (estimator/CI design, API, validation,
  phasing) is genuinely covered.

## Bottom line

The spec's statistics are sound where they are load-bearing for the
machinery: estimand, weights, identification, CI reuse, invariance gating,
and the code contracts all held under attack. The required changes are
concentrated in what the spec *says the estimand means* (F1/F2 — the exact
sentences T5 must teach from) and in three validation/parameterization
pins (F3/F4/F5) that would otherwise be improvised mid-implementation.
Fix F1–F5 in the spec text before T3/T4 consume those sections; F6–F10
are one-to-three-sentence edits that can land in the same pass; T2 can
start once F9's wording amendment is made (nothing in F1–F8 blocks the
syntax generator, though F5's emission contract should land before T2
writes the `invariance =` branch).

**Recommended tier for the revision:** Fable for F1–F5 (estimand-meaning
prose and validation-target design — exactly where plausible-but-wrong
lives); the F6–F10 edits are Sonnet-grade if split out.
