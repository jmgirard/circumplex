# RR20: the accuracy target's sampling-variability premise, and its sample-size calibration

- **Date:** 2026-08-23
- **Brief:** `cairn/reviews/RB20-axes-degeneracy-target-premise.md`
- **Reviewer:** Fable (independent review; advisory — binding criteria not requested)
- **Materials read:** `R/axes_corrected_se.R` (full, incl. lines 150–330,
  336–612), `R/axes_reliability_oop.R` (axes_fmt and both print call sites),
  RR19 (archived, §§1–6), RR18 (archived), D-037/D-044/D-048, DESIGN.md
  IP/GP blocks, M106 milestone file (AC4/AC5, F4/F17, plan-gate falsifier).
- **Measurements performed for this review:** (i) the committed exact-rational
  oracle re-run from the repo root — ANCHORS PASS, SWEEP PASS, REACHABLE PASS,
  and the family-A p = 8, κ = 1e5 SE relative error reproduced at 3.004e-12;
  (ii) a delta-method measurement of the corrected SE's relative sampling SD,
  built on the package's own `axes_se_pricing()`/`axes_se_derivs()` with
  cross-term corrected asymptotic covariance, over six geometries at p = 4, 8,
  9, 24 plus two parameter-grid sweeps; (iii) Monte Carlo confirmation of (ii)
  (normal data, the same ML-on-correlation fit the package computes, 300–500
  replications, two sample sizes). Constructions are stated where used so a
  milestone can rebuild them.

---

## 1. Is the corrected SE's relative sampling SD correctly priced at 1/√(2(n−1))?

**As an order (∝ 1/√n): yes. As a number (coefficient 1/√2 ≈ 0.707): no — the
coefficient is geometry- and component-dependent, measured 0.045 to 1.38
across reachable designs, and it has no established positive uniform lower
bound.** The objection is right that the single-variance anchor is asserted
rather than argued, right in direction about the coefficient moving, but wrong
about the mechanism and the available magnitude: the p(p+1)/2-entry averaging
it proposes is structurally unavailable.

**The delta-method argument, written out.** The reported corrected SE is not a
functional of a free p(p+1)/2-dimensional Σ̂: it is the plug-in
se_r(θ̂) = n^{−1/2} √(v_r(P̂)), where P̂ = cov2cor(Σ(θ̂)) is the *fitted*
matrix, Σ(θ) = ξ1·C + ξ2·J [+ ζ·B] + diag(ε) is linear in θ, and
v_r(P) = 2 tr(W_c,r P W_c,r P) is what `axes_se_pricing()` computes
(`R/axes_corrected_se.R:170-187`). So every path from the data to the reported
SE runs through the q-dimensional fitted parameter vector, q = n_comp + p
(q = 10 at p = 8, ~27 at p = 24), not through 300 free moments at p = 24.
The chain is:

- θ̂ linearizes exactly (the model is linear in θ) as
  θ̂_s − θ_s ≈ tr(W_s(R̂ − P)), with R̂ the sample correlation matrix; its
  asymptotic covariance under correlation input is the package's own W_c fold
  with cross terms, Acov[s,t] = 2 tr(W_c,s P W_c,t P)/n — Wishart-type,
  order 1/n.
- Var(se_r) ≈ ∇_θ se_r′ · Acov(θ̂) · ∇_θ se_r, so the relative sampling SD is
  a_r(θ)/√n with the n-free coefficient
  a_r = √(∇log se_r′ [n·Acov] ∇log se_r).

The gradient with respect to Σ̂ that the brief asks for is therefore the
composite ∂se_r/∂θ · (∂Σ/∂θ)⁺ restricted to the model manifold's tangent —
the off-manifold directions of Σ̂-space never move the reported number,
because the criterion's consumers are fed `lavaan::fitted(fit)$cov` = Σ(θ̂).

**Measured coefficients** (delta method, n = 600; anchor 1/√2 = 0.707; MC
confirmation in brackets where run):

| geometry | p | q | κ(cov2cor) | a(ξ1) | a(ξ2) | a(ζ1) |
|---|---|---|---|---|---|---|
| C: minimum design, ξ1=ξ2=.3 | 4 | 6 | 4 | 0.62 | 0.44 | — |
| A: 1/octant, ξ1=ξ2=.3 | 8 | 10 | 7 | 0.32 [0.33] | 0.49 [0.50] | — |
| A′: weak axes ξ1=.1, ξ2=.5 | 8 | 10 | 11 | 0.95 | 0.15 | — |
| 3/scale + ζ1, ξ1=ξ2=.25, ζ1=.2 | 24 | 27 | 23 | 0.42 | 0.64 | 0.61 |
| B: near-duplicate pair r≈.9999 | 9 | 12 | 2.96e4 | 0.18 | 0.69 | 0.53 |
| B at r≈.99999 (past the floor) | 9 | 12 | 2.96e5 | 0.18 | 0.69 | 0.53 |

Grid sweeps over (ξ1, ξ2) ∈ [.02, .6]² at p = 8 and (ξ1, ξ2, ζ1) at p = 24
find minima **0.045–0.046** (p = 8, near (ξ1, ξ2) = (.4, .2) and (.4, .5))
and **0.063** (ξ2 at p = 24), maxima 1.16–1.38. Monte Carlo at the p = 8
near-stationary point confirms the tiny coefficient is real first-order noise,
not a delta-method artifact: measured a(ξ1) = 0.052 at n = 600 and 0.051 at
n = 10⁴ (the 1/√n rate holds, with the small coefficient).

**Why the objection's mechanism is wrong but its direction half-right.**
Equal-weight averaging over k independent variance-like directions shrinks the
coefficient by √k; the manifold restriction caps k at q, so the available
averaging shrink is at most ~√q ≈ 5.2 at p = 24 — not the √300 ≈ 17 the
p(p+1)/2 reading suggests. But averaging is not the operative mechanism at the
measured minima: those are near-cancellations of the gradient (the SE as a
function of θ passing near a stationary point, the Bernoulli-p-at-½
phenomenon), and cancellation has no cap. The honest answer is therefore a
range, with endpoints:

- **Upper: a ≈ 1.4** (measured; weak-axes geometries), above the anchor —
  the harmless direction for the derivation.
- **Typical: a ≈ 0.3–0.7**, bracketing the anchor — the anchor is a fair
  typical-case number.
- **Lower: a ≈ 0.045 measured on a coarse grid, with no theorem preventing
  lower** at geometries closer to an exact stationary point. Assumptions at
  this endpoint: normal data, the swept design families; a finer search or a
  different design could go lower.

Two subsidiary findings: the coefficient is *stable at the refusal floor*
(family B identical to four digits at κ = 3e4 and 3e5 — the yardstick does not
collapse or explode where the criterion operates), and the correlation-input
(W_c-folded) covariance is the right pricing for both user paths, since both
feed a correlation matrix (MC end-to-end agreement within simulation error:
0.325/0.503 against delta 0.317/0.488, MC standard errors ~0.01–0.016).

## 2. Does this move δ\* = 1e-4?

**Under the block's own stated frame — bias ≤ one tenth of the noise,
uniformly, calibrated at n = 5e5 — yes: the corrected derivation lands at
0.1 × 0.046/√(5e5) ≈ 6.5e-6, a 15× correction that consumes the stated margin
and sits at the milestone's falsifier line (6e-6). But the right conclusion is
that the frame is unsound, not that τ = 1e-6 was right — because the same
frame also rejects τ = 1e-6, and every other finite constant.** Check: the old
cap was C·τ = 1e-5; worst-geometry noise at n = 5e5 is 6.5e-5; ratio 0.15,
already over the tenth. And since the coefficient has no established positive
floor (§1) and the cormat path's n has no ceiling, "bias ≤ 0.1 × the smallest
noise any reachable geometry at any typed n can have" is a bound that no
positive δ\* satisfies. A criterion that must yield a *fixed constant* cannot
be founded on a uniform-over-geometries, uniform-over-n noise comparison; that
foundation yields only a sliding scale, which is exactly what RR19 §4 rejected
for τ.

**What the corrected derivation does deliver.** The sampling-noise channel,
priced honestly, is a typical-case corroboration with a stated range:

| calibration | implied δ\* |
|---|---|
| n = 5e5, anchor a = 0.71 (shipped) | 1.0e-4 |
| n = 5e5, measured worst a = 0.046 | 6.5e-6 |
| n = 1e4 (published ceiling), worst a = 0.046 | 4.6e-5 |
| n = 1e4, anchor a = 0.71 | 7.1e-4 |

The 6.5e-6 corner is reached only by stacking both conservatisms — the worst
measured geometry *and* a sample size 1.7 decades past the published ceiling
(RR19 §1 P5: published circumplex matrices run n ~ 10²–10⁴). Each conservatism
alone stays within a factor ~2 of 1e-4. At every published n and every
measured geometry, a 1e-4 bias is at most ~0.22 of the SE's own noise
(worst case: a = 0.046, n = 1e4), and at typical geometries a tenth or less.

**Plain verdict:** δ\* = 1e-4 and τ = 1e-5 **stand**, but not on the argument
written beside them. The load-bearing support becomes the two n-free channels
(question 4: both survive), which sit a full decade above 1e-4, with the
sampling-noise channel demoted to typical-case corroboration carrying its
measured coefficient range and a stated calibration domain. The falsifier is
not triggered: a target at or below 6e-6 follows only from a frame that no
constant — including 1e-6 — satisfies, so it cannot ground a return to 1e-6.
If the maintainer instead wants the tenth-margin to hold at the measured
worst geometry with headroom past the published ceiling (n = 5e4), the
derivation gives δ\* = 2e-5, τ = 2e-6 — thresholds tighten by √5 ≈ 2.2
(p = 24: 4.3e4 → 1.9e4; p = 9: 3.2e4), the r = .9999 exemplar still computes
(κ 2.87e4, with ~10% headroom) and counterexample B still refuses. I do not
recommend it (recommendation 3): it buys protection for no measurable channel
and re-narrows GP2's computable region on a worst-case-product argument.

## 3. What sample size is the target calibrated for, and what holds outside it?

**The tenth-margin claim, as shipped, holds for n up to ~5e5 at the anchor
coefficient, ~2e4 at the measured worst geometry. Beyond, the fixed cap δ\*
degrades relative to the noise, reaching parity at n ≈ 5e7 (anchor), ≈ 3.2e6
(worst near-floor coefficient, a = 0.18), ≈ 2.1e5 (global worst measured,
a = 0.046 — but that geometry is superbly conditioned, κ ≈ 7, where actual
numerical error is ~1e-15).** The brief's n = 1e8 arithmetic is correct.

**Real or academic?** Academic for error, real for the guarantee's wording.
Two independent shields: (i) the exposure is to the *certificate*, not the
number — measured attainment of the p·κ²·ε bound in every reachable geometry
is ≤ 4e-6, so actual errors run ~1e-12, eight decades inside δ\* — a user at a
fabricated n = 1e8 would need to also occupy a bound-attaining geometry, and
none reachable has been found; (ii) no circumplex item-correlation matrix at
n > ~1e4 exists in the literature, and even generous modern-panel readings top
out around 1e6, where the anchor-coefficient margin is still 7×. But the block
currently *acknowledges* the binding direction ("the cormat path's unbounded n
is the binding one") and then fences nothing — an acknowledged exposure with
no stated domain is the one state the comment should not be in.

**Right response: a documented calibration domain, nothing operational.**
State in the block (and mirror in the exported docs' criterion paragraph):
the tenth-margin comparison is calibrated for n ≤ 5e5; above that the fixed
target progressively equals (n ≈ 5e7 at the anchor) and then exceeds the SE's
own sampling noise, at which point the criterion's guarantee is the fixed cap
δ\* alone — a print-resolution and coverage guarantee (both n-free), no longer
a noise-dominance guarantee. That is the honest content; nothing needs to
refuse or warn:

- **No n-dependent target.** This would rebuild the sliding scale RR19 §4
  rejected for τ, and §2 above shows the uniform-noise frame yields no fixed
  constant anyway. My answer therefore *agrees with* RR19 §4 rather than
  contradicting it: a documentation fence is a statement about the guarantee,
  not an n-dependent constant; refusal remains a property of the refused
  matrix.
- **No warning above some n.** It would fire on no real dataset, and a
  warning keyed to typed n is n-dependent surface behavior of exactly the
  gameable kind §4's reasoning disfavors.

So: nothing operational is needed, and the acknowledgement should change from
an aside implying unbounded coverage into a stated domain plus the
channel-primary framing of §2, which is n-free.

## 4. The two cross-checks

**(a) Print resolution — the objection is wrong about the inversion; the
claim survives, resting on one silent premise.** Confirmed mechanics:
`axes_fmt()` is `formatC(round(x, digits), format = "f", digits = digits)`
with `digits = 3` — three **decimal places**, not significant digits
(measured: 0.0123456 prints "0.012"), used for the components' SE column at
`R/axes_reliability_oop.R:344`. So absolute resolution is 1e-3 (half-step
5e-4), and relative resolution ≈ 5e-4/SE — *finest at the largest SE*, exactly
as the objection says. But that is not an inversion of the claim: "at least
1e-3 relative **at the largest printable SE**" already evaluates the channel
at its binding endpoint. The components are variance shares of a
unit-diagonal matrix, so their SEs are bounded by ~0.5 (typical 0.01–0.17),
and 5e-4/0.5 = 1e-3 is the finest relative change print can ever resolve;
coarser everywhere else. The claim is correct *given* SE ≤ ~0.5 — which the
block nowhere states, and should (it is the premise the whole check hangs on;
without a scale bound, relative print resolution has no floor). Corrected,
the check corroborates: the finest print channel sits one decade above δ\*.

**(b) Coverage — the number is right in RR19's units and 87× wrong in the
natural reading; the transcription dropped the disambiguating word.**
Confirmed: 2·1.96·φ(1.96) = 0.2291; δ = 1e-4 shifts 95% Wald coverage by
2.29e-5 *in probability*, which is **0.0023 percentage points**. RR19's own
P3 says "0.023 percentage points" for δ = 1e-3, so its §1 "0.002 points" for
1e-4 meant percentage points and is arithmetically right. The comment block
copied "0.002 points" without the unit; read as coverage probability (0.950 →
0.948) it is ~87× too large, exactly as the objection computes. One word
fixes it — write "0.0023 percentage points" or "2.3e-5 in coverage
probability". Corrected, the check corroborates strongly (the shift is
invisible against the asymptotic approximation's own error at any finite n).

**Net:** neither cross-check falls; both need a one-line repair (state the
SE ≤ 0.5 premise; restore the unit). This matters for §2: with the sampling-SD
channel demoted, these two are now the load-bearing supports, and they hold.

## 5. Removal

**Verdict: keep the limb as-is — questions 1–3 do not change RR19 §5's
holding — but the reopening trigger is one cheap measurement from being met,
and that measurement should be run now rather than awaited.**

Why my answers don't move the verdict: §§1–2 weaken the *derivation's prose*,
not the constant, and nothing measured here shows the criterion refusing more
certifiably-good numbers than RR19 already knew. The three grounds of RR19 §5
stand untouched: past the floor there is still no shipped means of certifying
a number to δ\* (IP3); the finite probe set is still evidence of absence, not
a bound; and the only a-priori estimate a replacement caution could carry is
still the 5–8-decades-loose envelope. One of my findings mildly *strengthens*
keeping: the noise yardstick is stable at the floor (§1, family B), so
refusal there is not sitting where the statistical ground itself gives way.

The four options weighed:

- **Keep as is — recommended.** With the documentation repairs of §§2–4.
- **Keep but recalibrate (τ = 2e-6)** — available, coherent, not recommended
  (§2): it re-narrows the computable region on a stacked-worst-case argument
  no channel can see, and leaves the r = .9999 exemplar at 10% headroom.
- **Replace with an a-posteriori per-fit estimator** — not yet, but this is
  now close. D-048's trigger (i) is half-met on this branch's own record:
  AC4 case 2 (p = 8, κ = 1.0e5, floor 7.5e4) is refused through a genuinely
  converged fit while the oracle — re-run for this review — measures that
  construction's SE relative error at 3.004e-12, eight decades inside δ\*.
  Only the `cval` half is unmeasured, and only because the oracle's
  EXACT_CVAL line is wired to the p = 3 fixture's counts. Measure it (small
  driver change; the counts-as-arguments seam already exists per the
  driver's own comment). If `cval` lands inside δ\* too, trigger (i) is met
  in full and D-048's recorded remedy — the a-posteriori estimator — becomes
  *scheduled* work, not an emergency: no field report exists, probe fits sit
  at κ ≤ 10.45, ~3.6 decades under the floor, so no user is waiting. What
  would be built: a per-fit error certificate (two independent factorization
  routes compared, or on-demand exact-rational recomputation of v_r and
  cval), validated against the exact oracle before shipping (its own IP3
  obligation); user-visible behavior: the `"ill_conditioned"` refusal region
  becomes computed-with-certificate where the certificate passes, refusal
  retained for `"indefinite"`, `"singular"`, and certificate failure. Tier:
  Fable — estimator-adjacent design where plausible-but-wrong is possible.
- **Retire the limb entirely** — rejected, same IP3 ground as RR19: it would
  ship numbers the package cannot certify anywhere past the floor on the
  strength of finite probing, and the fixture proves the pipeline *can*
  return 3.4%-wrong SEs with every guard green.

No recommendation here sits against IP3; the a-posteriori path is the one
that eventually *extends* IP3's reach into the currently refused region.

## 6. Silent load-bearing premises

Beyond question 1's coefficient premise, the block uses without stating:

1. **The q-manifold premise.** "Smooth plug-in functional of Sigma-hat"
   suppresses that Σ̂ is the *fitted* matrix, so the SE varies only along the
   q-dimensional model manifold. This is what makes the objection's
   √(p(p+1)/2) averaging unavailable — load-bearing for §1 and unstated.
2. **SE ≤ ~0.5** (variance-share scale) — the print cross-check's premise
   (§4a).
3. **Units of the coverage figure** — percentage points (§4b); also that the
   check linearizes at δ = 0 for a two-sided nominal-95% Wald interval.
4. **Normal-theory sampling law.** Both the χ² anchor and the W_c pricing
   assume normal data (Wishart-order moments). Excess kurtosis inflates the
   noise (harmless direction); platykurtic bounded item scales deflate it
   modestly (O(1), same order) — the direction the block never prices.
5. **The typed n is an independent-observation count.** On the cormat path
   the yardstick silently trusts that `n` is the iid sample size behind the
   matrix; pairwise-complete or clustered data breaks the 1/√n pricing in
   either direction.
6. **The SE-derived target is silently extended to the scaling surface.**
   The criterion refuses *both* surfaces at a floor priced to an SE-noise
   target; nothing in the block derives an accuracy target for `cval` (or
   states RR19's fiml_ratio ≤ 2δ\* remark). RR19 §3c's df-collapse
   measurements are evidence `cval` is fine in reachable geometries, but
   that is a measurement, not a target derivation — the block should say the
   extension is by fiat plus that evidence. This is the largest silent
   premise after the coefficient.
7. **Noise stability at the floor.** The derivation implicitly needs the
   yardstick not to collapse exactly where the criterion operates; measured
   true (§1, family B), but it was luck until measured.

## Beyond the brief

- **B1.** The plan-gate falsifier ("target at or below 6e-6 ⇒ loosening
  unjustified") is unsatisfiable as a frame rather than false as a number:
  priced uniformly it rejects every finite τ, including the 1e-6 it would
  restore (§2). The milestone record should note this when ingesting, so the
  gate doesn't read as failed.
- **B2.** "Calibrating at n = 5e5, a decade past any published circumplex
  sample" understates itself — 5e5 is 1.7 decades past the 1e4 ceiling RR19
  records. Harmless, but the rewrite may as well say what's true.
- **B3.** Measurement machinery for the coefficient is cheap and re-usable:
  the delta-method driver (cross-term corrected Acov + finite-difference
  gradient through `axes_se_pricing()`) reproduces Monte Carlo within
  simulation error at every point checked. If the rewrite quotes the
  coefficient range, a devel/ script pinning it would keep the quoted range
  falsifiable. Constructions used here: Σ = ξ1·C + ξ2·J [+ ζ1·B] + diag(ε)
  at unit diagonal; families as tabled in §1 (family B rescaled to constant
  diagonal c = 0.7 + ε_pair so cov2cor is an exact model rescaling);
  n = 600; sweeps as stated.
- **B4.** The oracle re-run reproduced everything it pins, including the
  figures this brief quotes (3.4e-2 at the fixture; 3.004e-12 at AC4 case
  2's construction; attainment ≤ 4e-6 reachable). No drift.

## Recommendations

Tier notes advisory, per the repo's model-tier doctrine.

1. **Apply — rewrite the derivation paragraph, constants unchanged.** Keep
   δ\* = 1e-4, C = 10, τ = 1e-5. Restructure `THE TARGET AND THE CEILING` so
   the two n-free channels are primary (print, with the SE ≤ 0.5 premise
   stated and the unit fixed to "0.0023 percentage points") and the
   sampling-SD channel is typical-case corroboration carrying: the q-manifold
   premise, the measured coefficient range [~0.05, ~1.4] around the 1/√2
   anchor, and the calibration domain of §3 (tenth-margin to n ≈ 5e5 at the
   anchor; fixed-cap-only guarantee beyond; parity ≈ 5e7). Add the §6 items
   1–7 as stated premises (one line each; item 6 needs the "by fiat plus
   RR19 §3c's measurements" sentence). Mirror the domain sentence at the two
   exported-doc criterion paragraphs. Tier: Opus — transcription of a
   settled argument, numbers supplied here.
2. **Apply — measure the cval half of D-048 trigger (i) now.** Extend the
   oracle driver to emit exact `cval` for the family-A p = 8, κ = 1e5
   construction (the counts-as-arguments seam exists; EXACT_CVAL is
   currently read only at the fixture) and record the result in the
   milestone. If it lands inside δ\*, record trigger (i) as met and register
   the a-posteriori estimator as a planned milestone per D-048's remedy
   clause — scheduled, not urgent (no field exposure). Tier: Opus for the
   driver change with the oracle run gating; the estimator itself, if
   triggered, is Fable.
3. **Consider — τ = 2e-6 (δ\* = 2e-5) only if the maintainer wants the
   tenth-margin uniform over measured geometries with headroom past
   published n.** Thresholds tighten ×2.2; both recorded exemplars land the
   same way, the near-duplicate with ~10% headroom. I advise against: no
   user-resolvable channel gains, and GP2's computable region narrows on a
   stacked-worst-case argument (§2). Tier if taken: Sonnet (constant + NEWS
   + threshold tests), against this RR's numbers.
4. **Reject — an n-dependent target or a runtime warning above some n**
   (§3): the uniform-noise frame yields no fixed constant, a typed-n warning
   fires on no real data, and both rebuild the yardstick-dependence RR19 §4
   rejected. The documented calibration domain in recommendation 1 is the
   whole remedy. This agrees with RR19 §4; nothing here reverses a recorded
   decision.
5. **Reject — removal or retirement of the ill-conditioning limb** (§5):
   IP3's ground is unchanged — past the floor the package still cannot
   certify to δ\*, measurement is not a bound, and the honest replacement
   caution still has no truthful number to carry. The exit path is
   recommendation 2's evidence-gated a-posteriori estimator, which is
   D-048's own recorded remedy.
