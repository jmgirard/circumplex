# RR19: the accuracy target for a reported corrected SE, and what it makes τ

- **Date:** 2026-08-22
- **Brief:** `cairn/reviews/RB19-axes-degeneracy-accuracy-target.md`
- **Reviewer:** Fable (independent review; advisory)
- **Materials read:** `R/axes_corrected_se.R` (full), `R/axes_scaled_fit.R:200-330`,
  `R/axes_reliability.R:690-760, 900-1060`, `R/axes_reliability_oop.R` (print
  precision), `devel/degeneracy-oracle/exact_oracle.{R,py}`, D-036/D-037/D-044,
  DESIGN.md IP/GP blocks, satorra1994.md, browne1982.md, browne1992.md,
  strack2013.md.
- **Oracle work performed for this review:** the committed exact-rational
  oracle was re-run from the repo root and reproduced both anchors and the Q4
  sweep exactly (ANCHORS PASS, SWEEP PASS, ratios 3.28/2.4/1.27). It was then
  driven — same `exact_oracle.py`, new R drivers — over constructed
  unit-diagonal matrices at p = 4, 8, and 9, including the near-duplicate-pair
  geometry, on and off the model manifold, for both the corrected SEs and the
  scaling factor `cval`. Those measurements are new evidence and are tabulated
  where used; the constructions are stated in full so a milestone can rebuild
  them without this session.

---

## 1. The target: the largest relative error a reported corrected SE may carry

**Answer: δ\* = 1e-4.** The yardstick the brief proposes — the SE's own
sampling variability — is the right primary one, with two n-free channels as
cross-checks. Derivation, every premise stated:

**P1 (what "misleads" means).** A numerical error in a reported SE is a
deterministic bias, not extra noise. It misleads when it changes something a
user can resolve through any channel by which SEs are consumed: (i) the
printed digits, (ii) Wald intervals and tests built from the SE, (iii)
programmatic reuse at full precision (e.g., meta-analytic weights 1/SE²). A
bias no channel can resolve cannot mislead.

**P2 (printed digits).** `print.circumplex_axes_reliability()` formats the
components table at 3 decimal places (`axes_fmt`, `R/axes_reliability_oop.R:31`,
`digits = 3`). Component SEs are on the variance-share scale, bounded well
below 0.5 in practice (typical 0.01–0.17). Print resolution is therefore
5e-4 absolute, i.e., ≥ 1e-3 *relative* at the largest printable SE and looser
at typical ones. Channel tolerance: δ ≥ 1e-3.

**P3 (Wald use).** For a nominal 95% interval, coverage as a function of a
relative SE error δ is 2Φ(1.96(1+δ)) − 1, with derivative 2·1.96·φ(1.96) ≈
0.23 at δ = 0. A δ of 1e-3 moves coverage by 0.023 percentage points; the
normal-theory asymptotic approximation itself carries coverage error orders of
magnitude larger at any finite n. Channel tolerance: δ well above 1e-3.

**P4 (the SE's own sampling variability — the binding channel).** The
corrected SE is a smooth plug-in functional of Σ̂ (the information sandwich,
satorra1994 eq. [16.10], p. 403), so its relative sampling error is O(1/√n)
with the normal-theory variance estimate as the order anchor: for
(n−1)s²/σ² ~ χ²_{n−1}, the relative SD of s² is √(2/(n−1)), and the delta
method halves it for the square root, giving relative SD ≈ 1/√(2(n−1)) for
the SE itself. *This is a textbook-standard result; no citekey on the repo's
shelf carries it, and I state that explicitly rather than manufacture a
citation.* Requiring the numerical bias to be at most one tenth of this
statistical noise — the conventional "numerical error ≪ statistical error"
margin, at which the bias perturbs every downstream quantity by ≤ 10% of its
own uncertainty — gives δ ≤ 0.1/√(2(n−1)).

**P5 (what n to calibrate at).** The absence of a minimum sample size is the
harmless direction: small n *loosens* this channel. The binding direction is
large n, and the package has no maximum either — the `cormat` path takes any
`n` (`R/axes_reliability.R:981-983`). Published circumplex item-correlation
matrices in this literature run n ~ 10²–10⁴; calibrating a decade beyond
anything plausible, at n = 5×10⁵, gives 0.1/√(2(n−1)) = 1.0e-4.

**Conclusion.** δ\* = 1e-4. At that level the numerical bias is ≤ one tenth of
the SE's own sampling SD for every n up to 5×10⁵ (one seventh at n = 10⁶),
one fifth of the last printed digit at the largest printable SE, and shifts
95% coverage by 0.002 points. One decade looser (1e-3) reaches the printed
resolution and half the sampling SD at n = 10⁵ — marginally resolvable; one
decade tighter (1e-5) is invisible through every channel and buys nothing.
The same target covers `fiml_ratio`: it is a ratio of two SEs priced at the
same matrix, so its relative error is at most ~2δ\*.

The currently enforced cap (~10τ = 1e-5) is therefore one decade tighter than
any statistical use requires — τ = 1e-6 sits below the defensible range, as
the brief's window analysis suspected.

## 2. The constant: τ = 1e-5, and how the slack should be expressed

Given δ\* = 1e-4 and the floor λmin ≤ λmax·√(p·ε/τ):

**Set `axes_degeneracy_tau = 1e-5`.** The enforcement logic is: refuse when
the error bound p·κ²·ε exceeds τ; the oracle measured actual error within a
factor C = 10 of the bound; so a computed answer's error is capped at C·τ,
which must equal δ\*. Hence τ = δ\*/C = 1e-4/10 = 1e-5.

Consequences at the shipped designs: the refusal threshold κ becomes
√(τ/(p·ε)) ≈ 4.3e4 at p = 24, 7.5e4 at p = 8, 1.06e5 at p = 4. Both recorded
exemplars land correctly: the near-duplicate pair (κ = 3.3e4, bound 5.8e-6 ≤
τ) computes — and its actual error, measured for this review against the
exact oracle, is 2.0e-13 (§3) — while counterexample B (bound 2.95e-2 ≫ τ)
still refuses. τ = 1e-5 sits inside the brief's window, at its lower edge,
which is where a fail-closed package should sit.

**Is one decade of slack right?** Yes — keep C = 10, for two reasons. The
measured attainment ratios at the fixture (1.27 → 3.28) drift upward with κ
at roughly ×1.6 per decade; a factor of 10 covers that trend with ≥3×
headroom at every κ below the new floors. And everywhere else measured
(§3), attainment is ≤ 4e-6 — the slack is not the exposed edge.

**But the slack should be expressed differently.** The current documentation
is internally inconsistent: `R/axes_corrected_se.R:386-397` defines τ as "the
largest relative error tolerated in a reported corrected SE," then states the
floor "caps a computed answer's error at ~10τ" — the stated definition and
the enforced cap differ by exactly the slack factor. Restate with two named
quantities: the **accuracy target δ\* = 1e-4** (derived in §1, the number that
means what τ's docstring currently claims τ means) and the **calibration
ceiling C = 10** (the oracle's measured factor, rounded up), with
τ = δ\*/C the implementation constant. One shipped constant, two documented
numbers, no redefinition needed the next time either moves.

## 3. The error model: right envelope, wrong predictor — and calibrated outside the reachable input space

**Verdict: p·κ²·ε is an acceptable conservative envelope but is not an error
model for this quantity, and its calibration evidence comes entirely from a
matrix the criterion can never be handed in production.** Three findings,
each measured for this review with the committed exact-rational oracle.

**(a) In every reachable geometry measured, actual error sits 5–8 decades
below the bound.** Constructions: model-implied unit-diagonal matrices
Σ = ξ1·C + ξ2·J [+ ζ1·B] + diag(ε), `cov2cor`'d — exactly the form every
lavaan-fitted Σ̂ has. Family A: p = 8, one item per octant scale
(ξ1 = ξ2 = 0.3, ε·I sweep; q = 10, df = 26). Family B: p = 9, two items on
one scale at the same angle — the near-duplicate-pair geometry of M89 F3 —
(ξ1 = 0.3, ξ2 = ζ1 = 0.2, pair ε = 7e-3…7e-6, others 0.30; q = 12, df = 33).
Family C: p = 4, the minimum design the API accepts (four scales, one item
each; q = 6, df = 4), several parameter regimes. N = 600 throughout. Maximum
relative SE error against exact values, vs the bound p·κ²·ε:

| family | κ(R) | actual rel. err | bound | attainment ratio |
|---|---|---|---|---|
| A (p=8, ε=2.4e-4) | 1.0e4 | 1.1e-13 | 1.8e-7 | 6.3e-7 |
| A (p=8, ε=2.4e-5) | 1.0e5 | 3.2e-12 | 1.8e-5 | 1.8e-7 |
| B (p=9, pair r=.9999) | 2.9e4 | 2.0e-13 | 1.7e-6 | 1.2e-7 |
| B (p=9, pair r=.99999) | 2.9e5 | 1.3e-12 | 1.7e-4 | 8.1e-9 |
| C (p=4, ε=1.2e-5) | 1.0e5 | 6.5e-13 | 8.9e-6 | 7.3e-8 |
| C (p=4, big ξ2, tiny ε) | 3.6e4 | 2.9e-12 | 1.2e-6 | 2.5e-6 |

The bound's only measured attainment (ratios 1.27–3.28) is the RR18 fixture.

**(b) The fixture is outside the criterion's production input space on two
independent counts.** First, it is p = 3 with df = 1, and `axes_reliability()`
requires at least four scales (`R/axes_reliability.R:990-991`), so no exported
call can produce a fitted matrix of its dimension, and the minimum reachable
df is 4 (p = 4, q = 6). Second, it is not a model-implied matrix at its own
stated configuration: solving ξ1, ξ2 from its first two off-diagonals implies
+24.31 for the third where the fixture holds −0.9994 (deviation 25.3), with
implied item errors of −9.3 — yet every matrix the criterion prices in
production is `lavaan::fitted(fit)$cov` = Σ(θ̂), exactly on the model
manifold by construction. (Manifold membership per se is not the mechanism —
perturbing family B off-manifold by up to 1e-6 entrywise left the error
unchanged at ~1e-13 — but the fixture's dimension/df regime is unreachable
regardless.)

**(c) What actually drives the error is coupling, which κ does not price.**
κ(Δ'VΔ) ≈ κ(R)² in *all* geometries measured (fixture: κ(R) = 6.65e6,
κ(info) = 2.9e15; family B at r = .9999: κ(R) = 2.9e4, κ(info) = 1.6e8). The
SE error reaches κ(info)·ε only when the ill-conditioned directions of the
information matrix load on the *component rows* of its inverse — maximal in
the near-saturated p = 3/df = 1 fixture where q = 5 parameters chase 6
moments, and negligible in every reachable design, where the amplified
directions live in the item-error block and the component rows are shielded.
The same df-dependence shows in the criterion's other consumer: measured
`cval` relative error is 3.4e-1 with a sign flip at df = 1 (the fixture),
1.1e-8 at df = 4 (family C, κ = 1e5), 1.1e-13 at df = 26 (family A, κ = 1e5).

**Generalization to p = 8, 12, 24:** measured directly at p = 4, 8, 9 — the
model is *pessimistic* there by ≥5 decades, and nothing about p = 24 points
the other way (larger p at fixed model family means larger df and weaker
coupling). The linear-in-p factor is untested at p = 24 but immaterial under
decades of headroom. **No measured regime shows the model optimistic** beyond
the fixture's own 3.3×, which the C = 10 slack covers.

This finding does **not** reopen D-044: `cov2cor(Σ̂)` remains the right matrix
to price (the metric argument is untouched); what it recalibrates is how
sharp the κ-based bound is inside that metric.

## 4. One constant, or several?

**One constant, n-free, shared by both surfaces — as now.** Four reasons:

1. The target's n-dependence is absorbed once, at calibration (§1 P5), by
   pricing the worst plausible n. Every real user sits at smaller n, where
   the true tolerance is looser — the fixed τ errs only toward caution.
2. An n-dependent τ makes refusal depend on the yardstick rather than the
   defect: the identical matrix with the identical numerical error would
   compute at n = 200 and refuse at n = 20,000, a threshold users cannot
   predict and can game (lower `n` until it computes — on the `cormat` path
   `n` is user-typed). A refusal whose trigger is not a property of the
   refused object is the wrong shape.
3. The κ floor moves as √τ, so τ ∝ 1/√n moves the threshold as n^(1/4): four
   decades of n buy one decade of κ. Negligible discrimination at real cost.
4. The criterion is shared with the scaled-fit surface (D-044's one-criterion
   contract), whose natural tolerance scales with df (§3c), not n. A shared
   n-dependent constant would fit neither surface; a df-dependent one would
   split the criterion. p already appears inside the bound where it belongs.

## 5. Removal

**Recommendation: keep the ill-conditioning limb, recalibrated per §2 —
removal is rejected, but narrowly, and with the reopening evidence stated.**

The case for removal is real and §3 strengthened it: within the reachable
input space, every measured computed value — SEs and `cval` alike — is
accurate to 1e-8 or (usually) far better, and the only measured catastrophic
failure lives at a dimension the exported API refuses. Today's floor blocks
computations that are near machine-exact.

Why refusal still wins:

- **"Measured on a finite probe set" is not "bounded."** The reachable set is
  every converged fit's Σ(θ̂) at p ≥ 4, including boundary and pathological
  θ̂; no theorem here bounds the coupling of §3c, and the M90 search's
  30,000 draws plus hill-climbs is evidence of absence, not absence of the
  regime. The fixture proves the *pipeline* can return SEs 3.4% wrong and a
  sign-flipped scaling factor with every guard green; the criterion's
  enforcement point is the internal helpers, which tests and oracles drive
  with constructed matrices — precisely the route B took.
- **IP3 cannot be satisfied in the refused region.** Past the floor, the
  package has no shipped means of certifying the number to δ\*; at the one
  refused point where truth is known (B), the double-precision value is
  measurably wrong. Printing a number the package's own validation doctrine
  fails on, caution or no caution, subordinates IP1 to convenience.
- **GP2 read in full supports this.** "Never block a defensible analysis"
  sits beside "undecidable edge cases fail closed (not certified, not
  computed)." Past the floor the a-priori certificate runs out — the number
  is a guess with an error bar spanning "exact" to "sign-flipped." The GP2
  violation in the near-duplicate exemplar was τ's miscalibration (three
  decades tighter than the derived target), not the mechanism's existence;
  §2 removes it.
- **A loud caution does not dominate refusal *with the estimate available
  today*.** The only shipped a-priori error estimate is the bound itself,
  which §3 shows overstates the actual error by 5–8 decades in every
  geometry users occupy: the caution would cry "up to 3% numerical error"
  over numbers accurate to 1e-13 — honest as a bound, false as information.
  And in the one geometry where the bound is tight, the sibling quantity
  fails by sign flip, which no error-bar caution can honestly describe. A
  *trustworthy per-fit* estimate (a-posteriori: two independent
  factorizations compared, or exact-rational recomputation on demand) would
  genuinely dominate refusal — but that is new machinery with its own IP3
  obligation, not a recalibration.

**Evidence that would show keeping the limb wrong** (GP7, record in the
D-entry): (i) an exhibited fitted Σ̂ from a *converged `axes_reliability()`
fit* that the recalibrated criterion refuses while an exact oracle shows its
computed SEs and `cval` within δ\*; or (ii) field reports of
`"ill_conditioned"` refusals on real data (the probe fits sit at κ ≤ 10.45,
~3.6 decades below the recalibrated p = 24 floor, so none is expected). On
either, the remedy is the a-posteriori estimator replacing this limb —
refusal retained for indefiniteness and exact singularity — not another
decade on τ.

## 6. The near-duplicate case

**A defensible analysis over almost-certainly-defective data — the package
should compute where it can certify, and name the cause where it cannot.**

In substance, r = .9999 between two items essentially never occurs in real
item responses (item-level measurement error caps inter-item correlations
well below 1); on the `cormat` path it typically signals a duplicated
row/column, an item and its recode, or an artifact of a smoothed or
reconstructed published matrix. But the analysis is statistically
well-defined — the model is identified, the fit converges, the estimands
exist — and GP2 reserves refusal for ill-defined or wrong-object input, which
this is not. Decisively: the computed corrected SEs in this geometry are
accurate to 2.0e-13 (§3, family B, measured at the exemplar's own r and κ).
The recorded refusal was pure miscalibration, and at τ = 1e-5 (§2) this
exemplar computes. The refusal onset moves to roughly r ≈ .99992 at p = 24 —
deeper duplication, same verdict on the data, but there the certification
argument of §5 takes over.

**When the criterion does refuse on the `cormat` path:** whole-fit refusal of
the corrected SEs remains right — the corrupted object is the one shared
information matrix, and there is no per-parameter salvage. But the refusal
should be actionable. The package cannot inspect raw items on this path, and
it does not need to: the offending pair is identifiable from the matrix
itself — the two dominant loadings of the smallest eigenvector of
`cov2cor(Σ̂)`, or equivalently the maximal off-diagonal |r| of the input
`cormat` in this geometry. Extend the `"ill_conditioned"` warning to carry κ
and, when a single pair dominates the near-null direction, name it ("items X
and Y are nearly collinear, r = 0.9999; near-duplicate items make the fitted
matrix numerically degenerate — consider dropping one"). A hint, not a
diagnosis; the user decides. This converts a dead end into a repair path on
exactly the input path where the user has the least visibility.

---

## Beyond the brief

- **B1 (doc defect).** τ's stated definition and its enforced cap differ by
  the slack factor (§2): `R/axes_corrected_se.R:386-397` calls τ the largest
  tolerated reported error while enforcing error ≤ ~10τ. The exported docs
  hardcode the criterion and `1e-6` in two places
  (`R/axes_reliability.R:718-722` and `:1028-1035`); both need the
  recalibrated constant and, ideally, the δ\*/C restatement.
- **B2 (calibration gap, IP3).** The oracle battery's only error-attainment
  measurements come from a fixture outside the production input space (§3b).
  The sweep should gain at least one reachable-geometry family (families A/B
  constructions are one-liners recorded in §3a); note the sweep's pass window
  `ratio ∈ [0.1, 10]` (`exact_oracle.R:138`) encodes "the bound is tight,"
  which is a property of the fixture, not the criterion — a reachable family
  needs its own window (measured ratios ~1e-7).
- **B3 (df is the exposure axis).** The measured error of both consumers
  collapses with df (§3c: df = 1 sign-flip → df = 4 at 1.1e-8 → df = 26 at
  1.1e-13). The four-scale minimum is what keeps the reachable set out of
  the fixture's regime; any future design change that lowers the minimum
  scale count re-exposes it and should re-run the exact oracle at the new
  minimum before shipping.
- **B4 (measurement note).** In the near-duplicate family the SE error grew
  roughly linearly in κ (2.0e-13 → 1.3e-12 across one decade), far below the
  bound's κ² — recorded for a future error-model revision, not load-bearing.

## Recommendations

Tier notes are advisory per the repo's model-tier doctrine.

1. **Apply.** Set `axes_degeneracy_tau <- 1e-5`; rewrite the rationale block
   and both exported-doc sites in terms of the accuracy target δ\* = 1e-4
   (derivation of §1) and calibration ceiling C = 10, with τ = δ\*/C;
   NEWS-document the threshold move (κ at p = 24: 1.37e4 → 4.3e4). Add a
   regression test that the near-duplicate exemplar's geometry (family B at
   r = .9999) computes and counterexample B still refuses. Tier: Opus
   (mechanical once decided; the statistics are settled here).
2. **Apply.** Keep the ill-conditioning limb; record in the superseding
   D-entry the two reopening evidence classes from §5. Tier: Sonnet (tracking
   edit).
3. **Apply.** Keep one n-free constant shared by both surfaces (§4). No code
   change; record the ruling so the question does not resurface. Tier: Sonnet.
4. **Consider.** Extend the `"ill_conditioned"` refusal warning with κ and
   the dominant collinear item pair (§6). Small, user-facing, testable at the
   family-B geometry. Tier: Opus.
5. **Consider.** Add a reachable-geometry family to the exact-oracle driver
   (B2), with its own pass window asserting attainment stays decades below 1
   — this is the tripwire that would catch a future coupling regime. Tier:
   Opus, with the oracle run gating.
6. **Reject — removal of the ill-conditioning limb** (the brief's Q5 option):
   reasons in §5; the refused region is exactly where the package cannot
   certify its own output, and the caution that would replace refusal has no
   trustworthy error number to carry today.
7. **Reject — n-dependent or per-surface τ:** reasons in §4; refusal must be
   a property of the refused matrix, and the shared-criterion contract is
   worth more than n^(1/4) of threshold movement.
8. **Consider (future, evidence-gated).** An a-posteriori per-fit error
   estimate (two independent factorization routes, or on-demand
   exact-rational recomputation) replacing the a-priori κ bound for this
   limb, validated against the exact oracle before shipping. Only on the
   reopening evidence of §5. Tier: Fable (estimator-adjacent design where
   plausible-but-wrong is possible).
