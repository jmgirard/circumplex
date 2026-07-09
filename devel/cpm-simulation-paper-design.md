# CPM confidence-interval simulation study — publication-grade design (Brief H)

**Status:** design document only (Brief H of the 2026-07 Fable window). No
implementation, no simulation runs, no task breakdown.
**Author:** Fable, 2026-07-08 (fresh session). Revised 2026-07-08 against
the H-review findings (Brief H-revision, separate fresh session; see the
Revision log).
**Charter:** ROADMAP post-M4 note (agreed with Jeff, 2026-07-06): a
publication-grade simulation study extending the B6 coverage oracle —
factorial over ζ level/heterogeneity, p, (mis)specification, and N;
competitor intervals (BCa at minimum); MC error budget; candidate venues
*Behavior Research Methods* or *Assessment*. Decoupled from the v2.0.0
release cadence.
**Settled inputs, taken as given (not re-opened here):** the `cpm_fit()`
estimator design (`devel/m4-browne-design.md` — variants A–D, ML discrepancy,
softmax/logit parameterization, boundary polish, acceptance criterion,
analytic + bootstrap CIs); the B6 coverage-oracle machinery
(`devel/m4-coverage-oracle.R`, findings recorded in DESIGN.md); the
`ssm_ci_accuracy()` spec (`devel/m4-ci-accuracy-spec.md`); the
marker-validation run (`devel/cpm-marker-validation.md`, ratified
2026-07-08).

> ⚠️ **Oracle discipline (binding).** No number in this document is an
> *expected* simulation outcome. Prior B6/G results are cited as *measured*
> facts about specific cells, motivating design choices; the study's own
> truths are generated from known parameter values (or pseudo-truths computed
> by deterministic projection, §2.4), never from memory or any local file
> (`devel/g2xx1.txt` remains banned). Every claim in §2 is stated as a
> question the factorial answers, not a result to confirm.

---

## 1. Motivation: what is already measured, and what it does not establish

The B6 coverage oracle (DESIGN.md, "CPM confidence intervals: measured
coverage"; 500 reps/cell, nominal 95%, p = 8 octants, ζ = .75, two β
configurations, N ∈ {250, 500, 1000} + an analytic-only ladder to 50000)
and the Brief-G marker validation (70k analytic-only fits, trailing-β
ladder, N ∈ {2000, …, 50000}) established, at those cells:

- **M1 — bootstrap-percentile ζ under-covers one-sided at N ≤ 500** (down to
  .758 at interior N = 250; misses concentrated above truth — the ζ̂
  boundary bias, which percentile intervals inherit).
- **M2 — bootstrap-percentile β under-covers at boundary truths at every N
  measured (~.77, flat in N)** — the signature of a *structural* percentile
  failure for a parameter near its boundary, not a small-sample effect.
- **M3 — analytic Wald CIs mis-cover at field N** (angle coverage .76–.88,
  not improving 250→1000 at boundary truths; interior truths re-enter
  [.90, .98] at N ≈ 2000, boundary truths only by N ≈ 50000).
- **M4 — Heywood solutions (any ζ̂ > .995) are the norm at field N** for
  octant truths at ζ = .75: 59–91% of fits at N ≤ 500, driven by the weakly
  identified alternating/Nyquist mode of equally spaced grids interacting
  with β₀.
- **M5 — T = n·F̂ departs χ²_df in the boundary regime at field N** (KS
  rejects in 5 of 6 B6 cells), tracking Heywood rates; passes at a
  well-identified non-octant truth at N = 2000.
- **M6 — analytic mis-coverage peaks *near* the β boundary, not at it**
  (Brief G): trailing β = 0 and .02 truths cover ≈ nominally (polish absorbs
  the boundary); trailing β = .05 is the worst regime measured (angle
  .70–.82 across the band). A β̂ < .05 marker discriminates *backwards*; the
  shipped .10 cut points the right way.
- **M7 — the runtime marker set predicts analytic mis-coverage** in the
  2000–50000 band (fired ≈ .88–.91 vs quiet ≈ .94–.95 for angles), with the
  discrimination vanishing at the N = 50000 gate; `illcond` and `heywood`
  additionally predict *nonexistent* CIs (65–78% NA rates given firing).

What these runs do **not** establish — the gaps this study exists to close:

1. Everything above sits at ζ = .75 homogeneous, p = 8 octants, m = 3
   correctly specified. Generalization over ζ level/heterogeneity, p, angle
   spacing, and misspecification is unmeasured.
2. No competitor interval has ever been run: whether BCa (or basic,
   studentized) repairs M1/M2 is open — it was the explicit BCa follow-up
   recorded when the B6 acceptance band failed.
3. The near-boundary-peak finding (M6) is measured for *analytic* CIs in the
   large-N band only; whether the bootstrap default shows the same
   non-monotonicity at field N is unknown.
4. `multimodal`'s conditional-coverage estimate rests on 114 firings in one
   provocation config; G's memo explicitly hands its re-measurement, and the
   retained-harmonics β-min refinement, to this study.
5. Heywood/acceptance/polish behavior has never been mapped as an outcome
   surface in its own right — B6 treated it as a conditioning nuisance.

---

## 2. Research questions, claims, and estimands

Conventions binding all of §2: angles are degrees [0, 360) at the API with
LM = 360, radians internally; **item angles θ are circular parameters** —
coverage is scored on the circle by the anchor-free span rule (B6:
`((truth − lci) mod 360) ≤ ((uci − lci) mod 360)`), the reference angle is
excluded from coverage (it is fixed, not estimated), and angle errors are
reference-relative signed shortest rotations in (−180°, 180°].
**Order-statistic interval refinements (BCa, basic, studentized) are
undefined for circular parameters** — bias-correction and acceleration are
order-statistic concepts needing a line, not a circle (the recorded M2
BCa-drop rationale). Competitor-interval comparisons therefore apply to ζ
and β only; θ is compared across exactly two methods (circular-percentile
bootstrap vs analytic Wald on the unwrapped branch). If the Assessment-style
SSM arm (§9, recommended out of scope) were ever added, SSM amplitude would
join ζ/β as a linear parameter and SSM displacement would join θ as
circular.

### RQ1 — Coverage map of the shipped default

**Question.** Across population structure (ζ level and heterogeneity, angle
set, β configuration) and N, when does the shipped default interval
(percentile bootstrap on the raw-data path) attain nominal coverage for θ,
ζ, and β, and with what one-sided error structure?

**Estimand.** Per cell, parameter family, and nominal level (95% primary;
90% and 99% computed from the same replicate sets as secondary levels):
empirical coverage `P(truth ∈ CI)` **conditional on the shipped acceptance
criterion**, with the conditioning rate itself a first-class outcome (§5.4)
and a worst-case unconditional bound reported for every headline claim
(§6.3). Truth is the generating value in correctly specified cells and the
pseudo-true projection (§2.4) under misspecification.

**Claim shape.** A coverage surface with named adequate/inadequate regions
(Bradley bands, §6.1) — the paper's descriptive backbone and the empirical
justification (or indictment) of the package default.

### RQ2 — Is the boundary-β percentile failure structural, and does any competitor repair it?

**Question.** M2 measured flat-in-N β under-coverage at one boundary
configuration. Does it (a) persist across the trailing-β ladder and other
boundary geometries (β₀-dominant), (b) show the M6 near-boundary peak at
field N, and (c) get repaired — fully, partially, or not at all — by BCa,
basic, or studentized intervals built from the *same* bootstrap replicates?

**Estimand.** Coverage by interval method (paired within fitted dataset —
same data, same replicate draws, §6.2) for β across the boundary axis and N;
the method contrast is estimated as a paired per-fit difference with
cluster-robust MC error. The bootstrap arm's N-range extends to
N ∈ {5000, 10000} via the pre-registered stage-3(g) cells (§3.4), so the
"flat in N / structural" reading is tested into the regime where the
analytic ladder shows the Wald method recovering (M3) — not just one octave
past B6.

**Claim shape.** Either "BCa (or X) restores nominal coverage for
near-boundary β, at these widths" or "no order-statistic refinement repairs
the failure; the pathology is in the bootstrap distribution itself, not the
quantile rule" — both publishable; neither assumed.

### RQ3 — The ζ boundary bias and directional coverage

**Question.** M1's ζ under-coverage is one-sided (interval above truth). How
does it scale with ζ level (worse near the ζ → 1 boundary?), heterogeneity
(is the weak item's ζ or the strong items' ζ mis-covered?), and N; and does
BCa's bias correction — mechanically aimed at exactly this median bias —
repair it without breaking the upper side?

**Estimand.** Left and right non-coverage rates separately (each nominally
α/2), per ζ item grouped by its true ζ; plus median bias of ζ̂ and the
Heywood pile-up mass P(ζ̂ > .995) as companions (§5.3), because the
percentile interval's behavior is a deterministic function of the replicate
distribution these describe.

**Note on the BCa mechanics near the boundary (design consideration, not a
prediction).** With replicate mass piled at ζ̂* ≈ 1, the bias-correction
z₀ = Φ⁻¹(#{t* < t̂}/B) can saturate and the adjusted quantile indices can
fall outside [1, B]. The engine must count and report endpoint clamping and
use the mid-rank tie convention (#{t* < t̂} + ½#{t* = t̂})/B — pre-registered
in §6.3 so that "BCa fails by saturation" is a measurable outcome rather
than a silent artifact.

### RQ4 — The analytic-CI regime and its runtime markers, on the full factorial

**Question.** Where are analytic Wald CIs (the cormat-path default)
trustworthy, and do the shipped `cpm_boundary_markers()` (Heywood, harmonic
removed, min β̂ < .10, Hessian condition > 1e8, multimodal) predict
mis-coverage *outside* the G run's ζ = .75/.97 octant slice? Sub-questions
carried over from G's memo: (a) re-measure `multimodal`'s conditional
coverage where a wider factorial makes it fire more than 114 times; (b) the
retained-harmonics-only variant of the β-min marker (G measured a marginal
gain; decide with adequate power whether it is real); (c) the
Heywood-at-huge-N hint (15 firings at N = 50000 with angle coverage .762 —
underpowered in G).

**Estimand.** Coverage conditional on each fired marker and on the
any-marker composite, by N band; NA-CI and zero-width-CI rates given firing
(both shipped behaviors: `solve()` rejecting the FD Hessian → all-family NA;
indefinite-but-invertible → variances clamped to zero, §5.2); false-alarm
rates on covering fits.

**Claim shape.** A validated, factorial-general characterization of when the
cheap analytic CIs can be used and which observable flags disqualify them —
the practical guidance chapter of the paper, and the generalization of the
release-scoped G subset.

### RQ5 — Estimator trustworthiness as a first-class outcome surface

**Question.** How do Heywood rate, acceptance rate, boundary-polish rate,
multimodality rate, and T = n·F̂ calibration vary over the factorial — in
particular, is the equally-spaced-grid × β₀ interaction (M4) the dominant
driver of Heywood behavior, and does it dissolve off-grid (perturbed
angles) and at p = 16?

**Estimand.** Per cell: rates of each flag; the KS statistic of T against
χ²_df on unpolished accepted replicates in correctly specified cells (the
B6 statistic); under misspecification, the empirical T distribution is
*described* against the noncentral benchmark ncp ≈ n·F* (F* the population
minimum discrepancy, §2.4) — reported, not tested, since the noncentral
approximation's own accuracy is part of what is being observed.

**Claim shape.** "When is the model trustworthy" is part of the paper, not a
filtered nuisance: the convergence/Heywood surface is reported with the same
prominence as coverage (the brief's explicit requirement).

### RQ6 — Coverage under (mis)specification

**Question.** Real analyses never fit the true m or the true angles. What do
the intervals cover when (a) m is over-fitted (true m = 2, fitted m = 3 —
manufacturing a true boundary, exercising polish), (b) m is under-fitted
(true m = 3, fitted m = 2 — a genuinely misspecified correlation function),
(c) angles are wrongly fixed (variant B fitted at theory angles when the
truth is perturbed off-theory), and (d) the population is mildly
out-of-family (a perturbed P₀ with population RMSEA ≈ .05, the realistic
case; secondary arm, §3.3)?

**Estimand.** Coverage of the **pseudo-true parameter** γ*(P₀) (§2.4) — the
only estimand a CI can be held to under misspecification. For (a), the
generating value is inside the fitted family, so truth is the generating γ₀
with β₃ = 0 exactly; the polish scoring rule (§2.5) then carries the weight.

### 2.4 Truth and pseudo-truth (definitions)

- **Correct specification:** truth is the generating γ₀ = (θ₀ ref-relative,
  ζ₀, β₀). The population matrix is P₀ = P(γ₀) built by `cpm_implied_cor()`,
  and the B6 `make_truth()` pattern (an exact `cpm_fit()` to P₀, asserted to
  recover γ₀ to 1e-6) doubles as the simulator via `cpm_simulate()`.
- **Misspecification:** the pseudo-truth is the ML projection
  γ*(P₀) = argmin_γ F(P₀; P(γ)) over the *fitted* family — computed once per
  cell, deterministically, by fitting the misspecified model to the exact
  population matrix (large-n limit of the estimator; no simulation noise).
  Guard: the projection fit must pass the acceptance criterion, must not
  flag multimodality, and its mirror/canonicalization must be checked; a
  cell whose pseudo-truth is itself ill-defined (near-tied distinct
  projections) is **redesigned or dropped at design time, with the reason
  recorded** — coverage of an ill-defined estimand is not a claim. Do not
  assume the under-fitted β* equals a truncation of β₀; take whatever the
  projection returns. F* = F(P₀; P(γ*)) is stored per cell for the RQ5
  noncentral benchmark and the population RMSEA √(F*/df). Each cell's
  pseudo-truth **boundary status** — whether the projection fit polished a
  harmonic or landed near a parameter bound — is recorded as a config-table
  column at table-build time: it determines which RQ2 regime that misspec
  cell actually tests, and it is pre-registration content, not a run-time
  observation.

### 2.5 Scoring rules fixed in advance (danger-zone cases)

- **Angles:** anchor-free span rule, reference excluded, everything
  reference-relative; a truth at the 0°/360° pole is handled by the span
  rule without special-casing (DESIGN G2: ≈0 ≡ ≈360).
- **Polished (harmonic-removed) β:** a polished-out harmonic is **not a
  free parameter and has no method-specific interval.** The shipped
  bootstrap refits every replicate under the reported post-polish spec, so
  the replicate column is identically 0 by construction (R/cpm_fit.R,
  `cpm_bootstrap`): percentile, basic, and BCa would all inherit the same
  degenerate [0, 0] — by construction, not by quantile rule — while BCa's
  point-mass guard would instead return NA, silently breaking the paired
  method contrasts. Rule: the removed harmonic reports β̂_k = 0 with the
  degenerate [0, 0] interval, is **scored once** (covering iff the truth is
  exactly 0, else a miss, with both one-sided tallies attributing it to the
  side of the truth), that single score is **attributed identically to
  every bootstrap-family method**, and the parameter is **excluded from all
  paired method contrasts** (§6.2) — there is nothing to contrast. The
  §4.2 point-mass guard is scoped to kept parameters only. Polish rate is
  reported per cell, and boundary cells report coverage both including and
  excluding polished fits (B6 already ran this sensitivity at one cell;
  here it is systematic).
- **Analytic NA CIs:** counted as an NA-rate outcome, never dropped
  silently; coverage is over defined CIs (the G convention), with the NA
  rate printed beside every conditional coverage that excludes them.
- **Zero-width clamped analytic CIs:** score as misses unless the truth
  exactly equals the point estimate (shipped behavior; measure, don't
  patch).
- **Bootstrap replicate exclusion inside a fit:** the shipped rule (the
  acceptance criterion, never the nlminb code) with the shipped
  count-warning convention; the per-fit `boots_used` is recorded.

---

## 3. Factorial design

### 3.1 Factors and levels (generating side)

| Factor | Levels (core) | Justified by |
|---|---|---|
| **ζ level** (homogeneous) | .5, .75, .9 | RQ1/RQ3: brackets the field range; .9 approaches the Heywood boundary from below, .5 tests whether low-communality data rescue or worsen identification. B6 measured only .75. |
| **ζ heterogeneity** (at ζ̄ = .75 only) | homogeneous; alternating .6/.9; one-weak-item (.9 × (p−1), .4) | RQ3: whether mis-coverage localizes to the weak item's own parameters or contaminates neighbors; heterogeneous ζ is the empirical norm. Crossed only at the central ζ level to contain the factorial. |
| **Angle set / p** | p = 8 equal (octants: {45, 90, 135, 180, 225, 270, 315, 360}) ⚑; p = 8 perturbed (octant grid with alternating +15°/−15° offsets in listed order: {60, 75, 150, 165, 240, 255, 330, 345}); p = 8 clustered (three scales within a 20° arc plus a single maximal 90° empty arc: {45, 90, 100, 110, 200, 245, 290, 360}); p = 16 equal ({22.5, 45, …, 337.5, 360}, 22.5° steps) ⚑. All sets in degrees, LM-convention 360 where the pole is occupied; the engine's fixed reference scale follows the shipped convention and is recorded per set in the config table (B6's `drop_ref`); coverage excludes it (§2). | RQ1/RQ5: octant p = 8 is the pathological equally-spaced identification case (aliasing/Nyquist mode × β₀ — M4) and the field's most common instrument geometry; the perturbed set breaks the grid aliasing (the M4 dissolution test); clustered angles are §2.5-of-A's empirical non-identification hazard (local redundancy + a sparse arc); p = 16 (df = 86, variant A m = 3) tests whether more scales on the same circle buy identification. ⚑ marks equally-spaced pathology cells, pre-flagged for the RQ5 interaction analysis. Sets are pinned numerically here because they are *generating* conditions and hence pre-registration content (the original prose gesture "three scales displaced into the opposite semicircle with one 90° gap" was geometrically unrealizable as stated — vacating three adjacent octant slots leaves a 180° arc — and is replaced by the pinned clustered set, which preserves the statistical intent). |
| **β configuration** (m₀ = 3 unless noted) | interior (.35, .30, .20, .15); trailing ladder β = (.50−t, .35, .15, t), t ∈ {0, .02, .05, .10}; β₀-dominant (.70, .15, .10, .05); m₀ = 2 truth (.45, .35, .20) for the misspec arm | RQ2/RQ6: the B6 boundary/interior axis, refined by G's finding that the failure peaks *near* the boundary — the ladder makes the peak location a measured curve at field N. β₀-dominant is the general-factor-swamps-the-circle hazard (A §2.5) and the other half of the M4 interaction. |
| **N** | 100, 250, 500, 1000, 2000 (bootstrap-armed); 5000, 10000, 20000, 50000 (analytic-only extension) | RQ1–RQ4: field N through the recovery regime. 100 extends B6 downward into small-sample practice; 2000 spans the analytic caution threshold; the extension reproduces/generalizes the B6 stage-2 ladder and G's band. |
| **Provocation** (secondary) | ζ = .97 homogeneous × interior β × octants × all N | RQ4: the only regime G found that fires `heywood`/`illcond`/`multimodal` often enough to measure; retained to re-measure `multimodal` with adequate firings. |

### 3.2 Factors and levels (fitting side)

| Factor | Levels | Justified by |
|---|---|---|
| **Variant fitted** | A (default, free angles) everywhere; B (fixed angles) in two modes — correctly fixed (θ₀ = theory) and wrongly fixed (θ₀ = perturbed set, theory angles supplied) | RQ1/RQ6(c). C/D are excluded from the core: C (equal communality) because equal-communality designs are rarer in practice and the parameterization gives them nothing new statistically; D (= B + C, fixed angles *and* equal communality) explicitly for the union of B's and C's reasons — it introduces no free-parameter structure not already covered by the B and C arguments, so its exclusion follows a fortiori from C's. One small C arm is an optional extension (§12). |
| **m fitted** | m₀ (correct); m₀ + 1 (overfit, true boundary manufactured); m₀ − 1 (underfit, pseudo-truth estimand) | RQ6(a,b). |
| **Interval method** | θ: circular-percentile bootstrap, analytic Wald. ζ, β: percentile, basic, BCa (all from the same replicate set), analytic Wald; studentized on a targeted subset (§4.4) | RQ1–RQ4; applicability map in §4.1. |
| **Nominal level** | 95% primary; 90%, 99% secondary (same replicates, extra quantiles — near-free) | Standard BRM completeness; the Bradley machinery applies per level. |

### 3.3 Out-of-family arm (secondary, recommended small)

Four to eight cells where the population is P₀' = nearest-PSD repair of
P(γ₀) + E, with E a fixed, seeded, symmetric off-diagonal perturbation
scaled so the population RMSEA of the projection ≈ .05 (computed, not
assumed, via §2.4's F*). Claim: the paper's practical guidance holds under
the realistic condition that no model fits exactly. Kept small because the
perturbation space is unbounded — this is a robustness demonstration, not a
factor.

### 3.4 Staging (the factorial is not run flat)

The full generating × fitting crossing is far beyond any bootstrap budget;
the study is a **pre-registered two-stage adaptive design** — stage-2 cell
*selection rules* are fixed here, before stage 1 runs, so adaptivity does
not become cherry-picking:

- **Stage 0 — smoke.** Every stage below has a `SMOKE` mode (reps ≈ 25)
  exercised end-to-end before any full run; the B6/G scripts' pattern.
- **Stage 1 — analytic screening, full core factorial.** No bootstrap.
  All §3.1 × {variant A, correct m} cells, plus the misspec and provocation
  arms, plus the large-N extension on a config subset. Cheap (§7.2), and it
  yields outright: RQ4 (markers, on far more firings than G), RQ5 (the
  trustworthiness surface and T-calibration), the analytic half of RQ1/RQ3,
  and M6's peak-location curve for Wald CIs at field N.
- **Stage 2 — bootstrap-family intervals on the pre-registered core.**
  Percentile + basic + BCa (+ Wald from the same engine fit, as B6 did) on:
  all 6 β configs × ζ = .75 homogeneous × {octants, perturbed} × all five
  field N — 60 cells — **plus** up to 12 cells stage 1 flags under the
  selection rule, which is deterministic (an ambiguous rule is a
  discretionary rule): per non-core stage-1 cell compute the **selection
  scalar** = the maximum *downward* deviation from nominal of the
  cluster-level coverage point estimate (§6.2), taken across the angle and
  ζ parameter families; rank all non-core cells by it (worst first) and
  admit the worst-ranked cell at each factor-axis level, in overall rank
  order, until the 12-cell cap binds; ties break to smaller N first, then
  to §3.1 row order. So every factor gets bootstrap-interval evidence where
  it looks worst, with no judgment call left open. **Plus** one
  B-sensitivity cell: trailing-t = .05 × octants × N = 500 re-run at
  B = 2000, the shipped default (§7.2).
- **Stage 3 — targeted arms.** (a) BCa acceleration validation (full vs
  grouped jackknife, §4.3) on two cells; (b) studentized intervals on ≤ 8
  cells ranked by the same selection scalar computed on stage-2 percentile
  β coverage (worst first, same tie-break);
  (c) the ζ-heterogeneity and ζ-level bootstrap slices (ladder t = .05 +
  interior only) — 2 het patterns × 2 configs × 3 N plus 2 ζ levels × 2
  configs × 3 N ≈ 24 cells; (d) misspec bootstrap slice (m ± 1, wrong-fixed
  B; 2 configs × 3 N × 3 specs = 18 cells); (e) out-of-family arm (§3.3);
  (f) provocation bootstrap cells for `multimodal` (interval behavior given
  firing), sized by G's power-floor lesson: target ≥ 400 expected firings
  (§6.2); (g) **large-N bootstrap extension** (the RQ2 structural-claim
  N-range): trailing-t = .05 × octants × N ∈ {5000, 10000}, R = 500,
  percentile/basic/BCa from B = 1000 plus the grouped jackknife — ~1.1 M
  engine fits, the cheap insurance that the paper's most quotable claim
  ("structural, not small-sample") holds where the analytic ladder shows
  the Wald regime recovering, rather than being scoped to field N.

---

## 4. Interval methods under comparison

### 4.1 Applicability map (fixed by the geometry, §2 conventions)

| Method | θ (circular) | ζ | β | Notes |
|---|---|---|---|---|
| Percentile bootstrap (shipped default) | ✔ via `quantile.circumplex_radian` (circular mean → unwrap → quantile → re-wrap) | ✔ | ✔ | The package default; the baseline everywhere. |
| Analytic Wald (shipped cormat default) | ✔ on the unwrapped branch, ± z·SE | ✔ | ✔ | From the FD Hessian delta method (A §5.2); NA/zero-width pathologies are outcomes (§5.2). |
| Basic (reflected) bootstrap | ✘ | ✔ | ✔ | 2t̂ − percentile; free from the same replicates. Known to misbehave for bounded parameters (can exit [0, 1] / the simplex). **Scoring is pre-registered as the raw (untruncated) interval** — the method as defined is what the paper measures; truncating into the natural bounds can flip a miss to a cover exactly at boundary truths (e.g. truth β = 0, raw interval entirely below 0), so the truncated variant is not silently substituted. The truncation *rate* is reported beside it as interval geometry (§5.2). Included *because* it is the cheap classical alternative a reviewer will ask about. |
| BCa | ✘ | ✔ | ✔ | §4.2–4.3. The minimum competitor the charter names. |
| Studentized (bootstrap-t) | ✘ | ✔ | ✔ | Needs a per-replicate SE — available from `cpm_analytic_se()` per resample; targeted subset only (§4.4). |
| Double/calibrated bootstrap | ✘ | argued out | argued out | B² refit cost (≥ 10⁶ refits per fitted dataset) buys a demonstration, not a factorial; noted as future work in the paper. One illustrative cell is optional (§12). |

θ's exclusion from the order-statistic refinements is a *stated design
principle in the paper*, cited to the same geometry that dropped BCa from
the SSM layer (M2): bias-correction and acceleration re-index the ordered
replicates, and a circle has no order. Wrapped alternatives (e.g., inverting
circular-distribution tests) are out of scope and named as future work.

### 4.2 BCa construction (pre-registered mechanics)

For each linear parameter t (each ζ_i, each β_k) with point estimate t̂ and
B accepted replicates t*_b:

- z₀ = Φ⁻¹( (#{t*_b < t̂} + ½·#{t*_b = t̂}) / B ) — mid-rank tie convention.
  Mid-rank is kept as the standard, harmless convention, but exact ties are
  **not** expected from the shipped replicate pipeline (verified against
  `cpm_bootstrap`, R/cpm_fit.R): replicates are refit under the reported
  post-polish spec and never individually polished, so a *kept* β is
  softmax-strictly-positive and every ζ is logit-strictly-below 1 in every
  replicate — mass piles *near* the bounds, not *at* them — and a
  polished-out harmonic carries no BCa interval at all (§2.5). The live
  hazard in this regime is therefore not ties but **z₀ saturation** from
  one-sided near-boundary replicate mass, counted below.
- Acceleration a from the jackknife skewness of the parameter's influence
  values (§4.3 for the jackknife's cost design).
- Adjusted quantile levels α₁, α₂ per Efron's formulas; endpoint indices
  clamped to [1, B] with **clamping counted and reported per cell** — a
  saturated z₀ (all replicate mass one side of t̂) is an informative failure
  mode of the method in exactly the regimes under study, and must surface as
  data, not as a silent floor.
- Degenerate guards: if B_used < 100 after exclusions, or the replicate
  distribution of a **kept** parameter is a point mass (a genuinely
  degenerate replicate set), the BCa interval is NA with a counted reason.
  The guard is scoped to kept parameters only: a polished-out harmonic's
  constant-0 column is not a BCa case at all — it is scored once under
  §2.5 and attributed to every bootstrap-family method identically.

### 4.3 The acceleration estimate (cost-bearing decision)

The textbook jackknife needs one CPM refit per left-out observation: N
refits per fitted dataset, on top of B bootstrap refits — at N = 2000 it
would dominate the budget. Design:

- **Primary: grouped (delete-d) jackknife with g = 100 groups** (d = N/100,
  contiguous blocks of the seeded dataset — exchangeable rows, so block
  composition is arbitrary; fixed blocks keep it deterministic). The
  acceleration is the **plain BCa skewness formula applied to the g
  delete-group pseudo-values** t₍ᵢ₎ (the statistic refit on the data minus
  block i), with t̄ = (1/g)·Σᵢ t₍ᵢ₎:

      a = Σᵢ (t̄ − t₍ᵢ₎)³ / { 6 · [ Σᵢ (t̄ − t₍ᵢ₎)² ]^{3/2} }

  **No delete-d correction factor is applied — deliberately.** The plain
  formula on delete-group pseudo-values is first-order identical to the
  full delete-1 jackknife: (i) a is invariant to any common rescaling of
  the deviations (cubes over squares^{3/2}), so the 1/(N−d) constant
  relating (t̄ − t₍ᵢ₎) to the block-summed influence values Σ_{j∈i} L_j
  cancels between numerator and denominator; (ii) the block sums' central
  moments then give Σᵢ(·)³ ≈ g·d·μ₃ and Σᵢ(·)² ≈ g·d·μ₂, so
  a ≈ (g·d·μ₃)/(6·(g·d·μ₂)^{3/2}) = skew(L)/(6·√(g·d)) = skew(L)/(6√N) —
  the full jackknife's value to first order. (Re-derived independently at
  revision time; agrees with the H-review derivation.) The classic
  implementation hazard is importing the delete-d *variance* estimator's
  (N−d)/(N·d) factor into the denominator alone, which breaks the
  cancellation — the engine must not. The cost of grouping is a noisier
  skewness estimate from g = 100 terms — second-order for an O(N^{−1/2})
  correction, and empirically gated by stage 3a. Refit cost: +100
  warm-started refits per fitted dataset at every N — ~10% of a
  1000-replicate bootstrap, flat in N.
- **Jackknife-refit failure rule (pre-registered).** The grouped refits run
  warm-started in exactly the boundary regimes where refits fail acceptance
  or go degenerate. A jackknife refit failing the §3.5 acceptance criterion
  (same rule and deterministic-restart retry as bootstrap replicates) or
  hitting a degenerate correlation matrix is excluded with a counted
  per-cell rate; if fewer than **g_used = 50** pseudo-values survive, a is
  NA and the BCa interval is NA with a counted reason — parallel to the
  B_used < 100 guard in §4.2.
- **Validation (stage 3a):** on two cells (one small-N, one N = 1000), full
  n-out jackknife vs grouped on the same fitted datasets; agreement of the
  resulting BCa endpoints (to a tolerance set by their own MC variation)
  gates the grouped estimator. If they disagree materially, BCa is run with
  full jackknife at N ≤ 500 only and reported as such — the claim scope
  shrinks rather than the estimator silently changing.
- Refits are warm-started from the full-data γ̂ with the shipped
  per-replicate mirror guard (A §5.2) applied to jackknife refits as well —
  a mirrored jackknife point corrupts the influence values just as it would
  the quantiles.

### 4.4 Studentized arm (targeted)

Bootstrap-t for ζ/β uses per-replicate analytic SEs. Two reasons to keep it
small: (i) cost roughly doubles per replicate (an FD Hessian per resample);
(ii) the per-replicate SEs are NA/unstable exactly in the boundary regime
(M7's NA mechanism), so the method's *feasibility rate* is itself an
outcome. ≤ 8 cells ranked by the §3.4 selection scalar on stage-2
percentile β coverage; replicates whose SE is NA are excluded with a
counted rate, and a cell where that rate exceeds 20% reports the method as
infeasible rather than its coverage. Like the basic interval, bootstrap-t
endpoints can exit the natural bounds ([0, 1] for ζ, the simplex for β):
the **raw (untruncated) interval is scored**, with the truncation rate
reported as interval geometry (§5.2) — the same rule as the basic interval,
for the same reason.

---

## 5. Outcome metrics (all per cell × parameter family × method × level)

### 5.1 Coverage

Two-sided empirical coverage; **left and right non-coverage separately**
(each nominally α/2 — the ζ story is directional, M1); for θ, the miss side
is the shorter angular direction from the interval, and an exact tie
between the two arc distances is attributed deterministically to the
upper (counterclockwise, uci) side — consistent with the package's
(−180°, 180°] convention including +180. Conditioning per §2.5.

### 5.2 Interval geometry

Median and IQR of interval width (angular width for θ, capped at 360°);
rates of: NA intervals (analytic; BCa per the §4.2–4.3 guards), zero-width
intervals (clamped analytic; polished β), truncation at natural bounds
(basic and studentized — raw intervals scored, §4.1/§4.4), endpoint
clamping and z₀ saturation (BCa), jackknife-refit failure (BCa, §4.3),
infeasibility (studentized).

### 5.3 Estimator behavior (companions to coverage)

Bias and median bias of ζ̂ and β̂ (per true value); circular mean and
circular SD of reference-relative angular error for θ̂; RMSE; Heywood
pile-up mass P(any ζ̂ > .995) and per-item P(ζ̂_i > .995).

### 5.4 Trustworthiness surface (RQ5, first-class)

Per cell: acceptance rate, error rate (worker/`cpm_fit` failures),
Heywood-flag rate, polish rate (and which harmonic), multimodality rate,
Hessian-condition-warning rate, `boots_used` distribution; T-calibration KS
p (correct specification, unpolished accepted replicates) or the descriptive
noncentral comparison (misspecified cells, §RQ5).

### 5.5 Marker analyses (RQ4)

Conditional coverage given each marker and the any-marker composite;
false-alarm rate on covering fits; the retained-harmonics β-min variant
re-scored on the same fits (a scoring change, no extra compute); marker ×
N-band interaction. All cluster-level (§6.2).

---

## 6. Monte Carlo error budget and pre-registered rules

### 6.1 Precision targets and decision rule

- **Stage 1 (analytic): R = 2000 reps/cell.** Planning bound: treating each
  fit as a single Bernoulli, SE at true coverage .95 ≈ .0049 (interval
  half-width ≈ 1.0 pp), decisive against the Bradley liberal band
  [.925, .975] for 95% CIs (Bradley, 1978 — the same citable criterion the
  Z1 spec pinned). The Bernoulli arithmetic here and below is a *planning*
  bound only, and it is conservative for the inference interval actually
  used: a per-fit coverage proportion is a [0, 1]-valued mean, and no
  [0, 1] variable has variance above the Bernoulli variance at the same
  mean, so the cluster-level SE is bounded above by the planning SE.
  Inference always uses the §6.2 cluster-level interval.
- **Stage 2 (bootstrap core): R = 1000 reps/cell** (planning SE ≈ .0069 at
  .95). The claims here are mostly method *contrasts* on cells whose
  failures are large (tens of pp, per M1/M2), where R = 1000 is ample;
  adequacy claims ("BCa is nominal here") use the cluster-interval-vs-
  Bradley rule below.
- **Stage 3: R = 500 minimum** (planning SE ≈ .0097), with wider MC
  intervals printed; the provocation `multimodal` cells are instead sized
  by expected *firings*: G's ~100-firing estimate carried ±.035 — target
  ≥ 400 expected firings (≈ ±.017) using G's measured firing rates to size
  reps.
- **Decision rule (fixed):** a cell/parameter/method is claimed non-nominal
  iff its **cluster-level 95% normal-theory (t) interval on the mean
  per-fit coverage proportion (§6.2 — the study's one and only MC inference
  interval; no Wilson or other binomial interval is used for inference
  anywhere, at any level)** lies entirely outside the Bradley band; claimed
  adequate iff entirely inside; otherwise reported borderline. One-sided
  claims apply the same cluster-level interval to the per-fit one-sided
  miss proportions against the per-side band [.5·(α/2), 1.5·(α/2)].
- **Region-aggregation rule (fixed):** RQ1's claim shape is region-level,
  and with ~600 stage-1 cells × 3 parameter families roughly 5% of
  truly-nominal cells will come out borderline or worse by MC chance alone,
  so per-cell verdicts aggregate under a pre-registered rule rather than
  narrative grouping. A named region (a contiguous cell set declared in the
  claims register *before* stage 1 runs) is claimed **adequate** iff ≥ 95%
  of its cells are individually adequate and none is individually
  non-nominal; claimed **inadequate** iff ≥ 95% of its cells are
  individually non-nominal; anything else is *described* as a mixed surface
  without a region-level verdict. The expected false-flag count under the
  global null (all cells truly nominal), computed from the per-cell
  decision rule's error rates, is printed beside every region claim. Region
  *boundaries* (e.g. "adequate for N ≥ X at interior truths") are monotone
  claims fit to the whole surface, never read off a single best/worst
  cell. (Default rule surfaced for confirmation, §12.)

### 6.2 Clustering and pairing

A fitted dataset contributes p − 1 angle indicators, p ζ indicators, and
m + 1 β indicators that are correlated within fit. **All MC intervals are
cluster-level** (per-fit coverage proportions; normal-theory interval on
their mean — the G convention), never naive binomial over pooled
indicators (the B6 tables' Wilson intervals were naive and thus
anti-conservative; this study upgrades the rule). Method contrasts
(percentile vs BCa etc.) are **paired within fit and within replicate
set** — same simulated data, same bootstrap draws — estimated as mean
per-fit differences with cluster-robust SEs; pairing is the main variance
lever and is why competitor intervals share replicates by design.
Polished-out harmonics contribute no method contrast (§2.5): the paired
differences are computed over kept parameters only, so no parameter enters
a contrast as scored on one side and NA on the other.

### 6.3 Pre-registered exclusion and reporting rules

1. Per-replicate seeds are derived (`BASE_SEED` + cell/replicate offsets,
   `set.seed()` locally per replicate — the B6/G pattern), so results are
   identical for any core count and scheduling. **`BASE_SEED` = 20260710**,
   pinned here at design time (a seed chosen after any code exists is
   formally post-hoc), disjoint from 20260706 (B6) and 20260708 (G), and
   never changed; any re-run under a different seed is reported as such.
2. Worker errors and `cpm_fit()` failures are counted per cell, never
   silently dropped (B6 rule); a cell with > 2% errors is flagged and its
   coverage annotated.
3. Primary coverage conditions on acceptance (B6/G convention: they used
   411–494 of 500 and 99.6% respectively — the conditioning event must stay
   visible). Every headline claim also reports the worst-case bound
   (non-accepted fits scored as misses); a claim that survives only under
   conditioning says so in the paper.
4. NA/zero-width/truncation/clamping events per §5.2 — outcomes, not
   exclusions.
5. Polish scoring per §2.5, with the include/exclude sensitivity at
   boundary cells.
6. No optional stopping: stage budgets are fixed by §7; a stage that ends
   underpowered reports wider intervals rather than topping up selected
   cells (topping up *all* cells of a stage uniformly, with a disjoint seed
   offset block, is permitted and reported).

---

## 7. Reproducibility and compute

### 7.1 RNG contract

The study engine is a devel/ script, not package API, but it follows the
package invariant (DESIGN.md): stochastic output ⇒ documented global-RNG
consumption with local `set.seed()` per replicate; **no per-call seed
argument** on any package function it drives. `cpm_fit()` point estimates
are deterministic (A F4); the only RNG consumers per replicate are
`cpm_simulate()` and the bootstrap index draws inside
`cpm_fit(ci_method = "bootstrap")`. Committed artifacts: cell-level
aggregate RDS (with configs, seeds, dates, versions) — per-fit records are
written per cell for regeneration but not committed (G's ~1.7 MB precedent;
this study's per-fit records will be larger and stay uncommitted, with the
schema documented so any cell is exactly reproducible from its seed).

### 7.2 Feasibility estimate (measured-throughput based)

Anchor: B6 ran ≈ 6 cells × 500 reps × (1 fit + 1000 boot refits) ≈ 3.0M
engine fits in ~3.5 h on the dev machine (`detectCores() − 1`), i.e.
**~240 fits/sec aggregate at N ≤ 1000 — a warm-refit rate**, dominated by
B6's warm-started bootstrap refits. **Cold and warm rates differ and are
budgeted separately:** a cold `cpm_fit()` call (stage 1's unit) runs ~5–7
optimizer starts under the §3.5 multi-start scheme, and large-N cells add
O(N·p²) simulation + `cor()` per fit — G measured **~60–120 fits/s
all-in** for analytic-only fits at N up to 50000 (the optimizer is not the
large-N cost). The table uses the cold-fit rate for stage 1 and the
warm-refit rate for the bootstrap-dominated stages 2–3, with headroom for
p = 16 (~4× per-fit linear-algebra cost) and N ≥ 2000 (~2×+
resample-`cor()` cost):

| Stage | Cells (approx.) | Reps | Refits/rep | Engine fits | Wall estimate |
|---|---|---|---|---|---|
| 1 — analytic screen | ~600 | 2000 | 1 | ~1.2M | ~4–10 h (cold-fit rate) |
| 2 — bootstrap core | 61 + ≤12 | 1000 | ~1101 (1 + 1000 boots + 100 jack; the B = 2000 sensitivity cell ~2101) | ~67–82M | ~3–5 days |
| 3 — targeted arms | ~57 | 500–1000 | 1101–2201 (studentized ~2×; (g) at N ≥ 5000 pays the large-N `cor()` premium) | ~36–47M | ~2–3 days |
| **Total** | | | | **~1.05–1.35 × 10⁸** | **~1 week wall** |

**B = 1000 vs the shipped default (`boots = 2000`).** The study evaluates
the shipped percentile method at B = 1000 — the B6 budget compromise — not
the signature default B = 2000; the paper states this openly rather than
letting a reviewer discover it. The method *comparisons* are unaffected by
B because all bootstrap-family methods share the same replicate sets
(§6.2), so quantile MC noise at the 2.5% tail enters every method
identically; absolute coverage at B = 1000 vs 2000 is checked, not
asserted, by the pre-registered stage-2 sensitivity cell (trailing-t = .05
× octants × N = 500 at B = 2000, §3.4), with agreement judged by
overlapping cluster-level intervals.

Knobs if the appetite is smaller, in order of preference (each is a scope
statement, not a quality cut): drop the perturbed-angle axis from stage 2's
core (−30 cells, ~−1.5 days); boots = 1000 is already the floor for stable
2.5% quantiles — do not cut it; reps = 500 across stage 2 halves it at the
price of borderline verdicts on adequacy claims (contrast claims survive).
Cell-level checkpointing (each cell its own RDS, resumable, the
`CPM_COV_CELLS`-filter pattern) is mandatory so a week-long run is
restartable and partially reportable. If Jeff prefers, stages 2–3 are a
natural candidate for a rented many-core box (embarrassing parallelism,
deterministic seeds ⇒ identical results anywhere); the design is
indifferent.

### 7.3 Software provenance

The paper pins: package version (the v2.0.0 release), R version, BLAS
(the CI-portability episode showed reference-vs-optimized BLAS moves CPM
optima at the 3rd decimal and can land β exactly on the boundary — the
run records `sessionInfo()` and `La_library()`, and the paper's
reproducibility appendix states them). Engine scripts live in a research
compendium (recommended: a separate repo citing the package version, so the
package repo carries only the release-scoped oracles; §12).

---

## 8. Analysis and reporting plan (paper-facing)

- **Fig. 1 (RQ1):** coverage vs N, one panel per parameter family, lines =
  interval method, faceted by β configuration; Bradley band shaded — the
  backbone figure.
- **Fig. 2 (RQ2/M6):** coverage vs trailing-β t at each N — the
  near-boundary peak curve, percentile vs BCa vs Wald overlaid.
- **Fig. 3 (RQ3):** one-sided miss decomposition for ζ (stacked
  below/above), by ζ level and N, percentile vs BCa.
- **Fig. 4 (RQ5):** the trustworthiness heatmap — Heywood/acceptance/polish
  rates over (angle set × β config × N), equally-spaced cells flagged.
- **Table (RQ4):** the marker table — G's per-marker layout (n fired, NA-CI
  rate, conditional coverage fired/quiet, false-alarm rate) on the full
  factorial, plus the `multimodal` re-measurement and the retained-β
  variant verdict.
- **Applied illustration (genre-expected at BRM; no new simulation):** one
  short worked example on a bundled dataset — the `jz2017` octant scales —
  running `cpm_fit()` with both CI methods, showing the boundary markers
  firing (or not) on real data, and walking the paper's guidance through
  the real decision it implies; also the natural home of the
  `ssm_ci_accuracy()` cross-reference (§9). One figure plus roughly half a
  page — the CircE-successor framing expects implementation + example, as
  CircE's own BRM paper had.
- **Supplement:** full cell-level tables (aggregate RDS rendered), scoring
  rules (§2.5), exclusion accounting per §6.3, seeds, and the engine's
  smoke-test protocol.
- Every claim sentence in the paper maps to one pre-registered
  cell-set × metric × decision-rule triple from this document; a claims
  register (RQ → cells → figure/table) is an appendix of the design, kept
  current as stages run.

## 9. Venue framing — recommendation: *Behavior Research Methods*

**Recommendation: BRM, with the CircE-successor + simulation core framing.**
Argued both ways:

- **BRM (recommended).** The paper's contributions are an estimator
  implementation (the only maintained R implementation of Browne's model —
  CircE, itself published in BRM in 2010, is archived), a factorial
  operating-characteristics study of its intervals, and validated practical
  guidance (the marker set, the N thresholds, the boundary geography). That
  is BRM's core genre (software + simulation + guidance), the audience that
  lost CircE is BRM's audience, and every RQ above is in scope without
  bending. The reproducibility posture (§7) is a BRM strength.
- **Assessment (not recommended for this paper).** The Z&W-2017-successor
  framing — CI trustworthiness for applied SSM users, built on
  `ssm_ci_accuracy()` — is real but it centers the *SSM layer* (e/a/d
  coverage at the user's n) rather than the CPM's own intervals; folding
  both estimand families into one paper dilutes each, and the SSM-layer
  story leans on applied examples more than factorial breadth. **Shape
  decision that follows:** the SSM-layer arm is *out of scope here* and is
  recorded as the natural companion paper (Assessment, applied-tutorial
  register, `ssm_ci_accuracy()` as the vehicle), where the Z&W transcription
  work (O5 bridge, W1 vignette) already points.

Shaping consequences adopted throughout this design: competitor-interval
methodology gets full weight (§4); estimator behavior is a finding, not a
nuisance (§5.4); the marker set is presented as user-actionable guidance
(RQ4); `ssm_ci_accuracy()` appears in the discussion as the in-package
diagnostic operationalizing the paper's advice (and its recorded §12.4 idea
— a `circumplex_cpm` method with per-replicate CPM refits — is exactly this
study's engine productized; noted as future software work, not a study
arm).

Title sketch (working): *"Confidence intervals for Browne's circular
stochastic process model: coverage, boundary pathologies, and practical
guidance from the circumplex R package."*

## 10. Engine requirements (design-level deltas vs `devel/m4-coverage-oracle.R`)

For the later implementation session — what the B6 script does not yet do:

1. **Config-table-driven cells** (generating γ₀ or perturbation spec,
   angle set, N, fitted variant/m, stage tag) replacing the two hardcoded
   configs; pseudo-truth computation (§2.4) at table-build time with its
   guards, including the recorded **boundary-status column** (projection
   polished a harmonic / landed near a bound) and the engine's reference
   scale per angle set.
2. **Interval-method plug-ins on a shared replicate matrix:** one bootstrap
   pass per fitted dataset stores the replicate parameter matrix once;
   percentile/basic/BCa/levels are scoring passes over it (pairing by
   construction, §6.2). BCa adds the grouped-jackknife refit loop and the
   §4.2–4.3 accounting (z₀ saturation, endpoint clamping, jackknife-refit
   failures and the g_used floor).
3. **Per-fit record schema:** γ̂, all interval endpoints by method × level,
   marker vector, acceptance/polish/Heywood/multimodal flags, boots_used,
   NA/clamp/truncation events, T, df — written per cell (uncommitted),
   aggregated to the committed RDS.
4. **Cluster-level summarization** (§6.2) replacing pooled-indicator
   Wilson.
5. **Checkpoint/resume** per cell; stage driver honoring the pre-registered
   stage-2/3 selection rules as code, not judgment.
6. The B6 `angle_covered()` span rule, `make_truth()` exactness assertion,
   error accounting, and smoke mode carry over unchanged.

No package code changes are required by this study (it drives shipped API
plus the two internal entry points B6 already uses: `cpm_engine()`,
`cpm_analytic_se()`); if the retained-β marker variant (RQ4b) wins, the
resulting `cpm_boundary_markers()` change is a *post-study* package task
with its own tests, outside this design.

## 11. Limitations (stated in the paper, decided now)

- Gaussian populations throughout (matching `cpm_simulate()` and Z&W's
  design logic); non-normal generation is future work and the discussion
  says so — the bootstrap's nonparametric promise is assessed under a
  parametric truth.
- Complete data only; listwise behavior under missingness is not studied.
- Correlation-structure fits only (the shipped model); the CIRCUM
  free-scaling family (ROADMAP continuous track) is out of scope.
- Pseudo-truth coverage (§2.4) is the standard but not the only possible
  misspecification estimand; the paper states the choice and its rationale
  (a CI can only be held to what its estimator estimates — the same
  principle the Z1 spec §1 pinned for the SSM layer).
- p ≤ 16, m ≤ 4: the factorial does not reach item-level fits (p ≈ 32–64),
  where the model is also used; noted as scope.

## 12. Open decisions for Jeff

1. **Venue confirmation** — BRM per §9 (shapes reporting; the Assessment
   companion stays parked either way).
2. **Stage-2/3 compute appetite** — ~1 week wall on the dev machine with
   checkpointing, vs trimming per §7.2's knobs, vs renting cores. The
   design is indifferent; the budget table is the menu.
3. **Studentized arm** — include as specified (§4.4, recommended: it is the
   textbook "should have tried" and its infeasibility rate is itself a
   finding), or drop to save ~1 day.
4. **Out-of-family arm** (§3.3) — recommended in (small); droppable without
   touching any RQ1–RQ5 claim. Optionally, one of its cells can use a
   non-Gaussian generating distribution as referee insurance against the
   standard "Gaussian-only" objection (§11) — H-review's venue note;
   include only if you want that pre-emption.
5. **Variant C mini-arm** (equal communality, §3.2) — optional; include
   only if the paper wants the full A–D menu on display.
6. **One illustrative double-bootstrap cell** (§4.1) — optional flourish;
   recommended against (cost, and it answers no registered question).
7. **Compendium form** — separate research-compendium repo citing the
   released package version (recommended), vs growing devel/ further.
8. **Region-aggregation rule** (§6.1) — the ≥ 95%-adequate / none-non-nominal
   default is pre-registered in this revision (H-review R6); confirm it or
   substitute an equivalent rule *before stage 1 runs* — after stage 1 it is
   post-hoc.

## Revision log (vs H-review)

Findings R1–R11 from `devel/cpm-simulation-paper-design-review.md`
(2026-07-08). Every finding was weighed, not blind-applied; R2's central
mathematical claim was re-derived independently before adoption. **None was
rejected.** Settled decisions (the `cpm_fit()` estimator design, B6
machinery, staged-factorial architecture, BRM venue) are untouched; no
finding required a package-side change.

| Finding | Resolution |
|---|---|
| **R1** (must-fix): §6.1 "Wilson" vs §6.2 cluster-level interval contradiction | **Fixed.** The decision rule (§6.1) now names one MC inference interval everywhere — the cluster-level 95% normal-theory (t) interval on the mean per-fit coverage proportion — including the one-sided per-side bands; no Wilson/binomial interval is used for inference at any level. The Bernoulli arithmetic in the stage bullets is retained but explicitly labeled a *planning* bound, with the review's conservativeness argument (a [0,1]-valued per-fit proportion cannot exceed Bernoulli variance at the same mean) carried in the text. |
| **R2** (must-fix): BCa grouped-jackknife formula unstated; ties rationale wrong; no refit-failure rule | **Fixed, after independent re-derivation.** I re-derived the grouped-jackknife acceleration from the von Mises expansion before adopting: t̄ − t₍ᵢ₎ ∝ (S_i − S̄) with a common 1/(N−d) constant that cancels in the scale-invariant a, and block-sum moments give a ≈ skew(L)/(6√N) — first-order identical to delete-1, **no delete-d correction**. My derivation agrees with the review's, so §4.3 now pins the plain formula, the two-step argument, and the explicit warning against importing the delete-d variance factor. Added the pre-registered jackknife-refit failure rule (§3.5 acceptance keying, counted exclusions, g_used < 50 ⇒ a = NA ⇒ BCa NA-with-reason). Corrected the §4.2 ties rationale: verified against `cpm_bootstrap()` (R/cpm_fit.R) that kept β is softmax-strictly-positive and ζ logit-strictly-below 1 in every replicate, so exact ties are not expected; mid-rank is kept as convention and the live hazard is correctly named as z₀ saturation. |
| **R3** (must-fix): polished-out harmonics scored asymmetrically across bootstrap-family methods | **Fixed.** §2.5 now rules that a polished-out harmonic is not a free parameter and has no method-specific interval: scored once (covers iff truth exactly 0), attributed identically to every bootstrap-family method, excluded from all paired method contrasts (§6.2 states the pairing consequence). The §4.2 point-mass guard is scoped to kept parameters only. |
| **R4** (should-fix): no bootstrap evidence for the "structural" RQ2 claim above N = 2000 | **Fixed — chose the add-cells option over softening the claim.** New pre-registered stage-3(g): trailing-t = .05 × octants × N ∈ {5000, 10000}, R = 500, percentile/basic/BCa at B = 1000 + grouped jackknife (~1.1 M engine fits). Rationale: the cost is trivial against the ~1-week budget and it buys the paper's most quotable claim its N-range; softening RQ2 to field-N scope would have conceded the headline for no real savings. RQ2's estimand text now names the extension. |
| **R5** (should-fix): basic-interval truncation scoring ambiguous | **Fixed.** §4.1 pre-registers that the **raw (untruncated) basic interval is scored** — the paper measures the method as defined — with the truncation rate reported as interval geometry (§5.2). The same raw-scoring + counted-truncation rule is extended to the studentized interval (§4.4), which §5.2 now also tracks. |
| **R6** (should-fix): no region-level aggregation rule | **Fixed.** §6.1 adds a pre-registered region-aggregation rule: regions declared in the claims register before stage 1; adequate iff ≥ 95% of cells individually adequate and none non-nominal; inadequate iff ≥ 95% non-nominal; otherwise described, not claimed; expected false-flag count under the global null printed beside every region claim; region boundaries are monotone surface fits, never single-cell reads. Surfaced as §12 item 8 for Jeff to confirm or substitute (the review explicitly left the exact rule to Jeff). |
| **R7** (should-fix): stage-2/3 selection rules name no ranking metric | **Fixed.** §3.4 defines the selection scalar (maximum downward deviation from nominal of the cluster-level coverage point estimate, across the angle and ζ families), rank order, one-cell-per-axis-level admission until the 12-cell cap binds, and the deterministic tie-break (smaller N first, then §3.1 row order). Stage 3(b) uses the same scalar on stage-2 percentile β coverage (§3.4, §4.4). |
| **R8** (should-fix, venue): no applied illustration | **Fixed.** §8 adds the worked example: `jz2017` octant scales, `cpm_fit()` with both CI methods, boundary markers in the wild, guidance applied to the real decision; the natural home of the `ssm_ci_accuracy()` cross-reference. One figure + ~half a page; no new simulation. |
| **R9** (minor): stage-1 wall estimate used warm-refit throughput for cold fits | **Fixed.** §7.2 now distinguishes the warm-refit anchor (~240 fits/s) from the cold-fit rate (~5–7 multi-starts per fit; G measured ~60–120 fits/s all-in at large N); stage-1 wall revised to ~4–10 h. Stage-2/3 arithmetic unchanged (the review verified it); total stays ~1 week. |
| **R10** (minor): study runs B = 1000 vs shipped default boots = 2000 | **Fixed — chose the stronger both-halves option.** §7.2 states the compromise openly *and* explains why method comparisons are B-invariant (shared replicate sets), and §3.4 adds one pre-registered stage-2 sensitivity cell (trailing-t = .05 × octants × N = 500 at B = 2000, ~2.1 M fits) so absolute-coverage invariance is checked, not asserted. |
| **R11** (hygiene bundle) | **All five fixed.** (i) `BASE_SEED` = 20260710 pinned in §6.3.1 at design time (disjoint from B6's 20260706 and G's 20260708). (ii) Angle sets pinned numerically in §3.1: octants {45, 90, …, 360}; perturbed {60, 75, 150, 165, 240, 255, 330, 345} (alternating ±15° on the octant grid); clustered {45, 90, 100, 110, 200, 245, 290, 360} (three scales in a 20° arc + a single maximal 90° empty arc); p = 16 at 22.5° steps. Note: the original clustered prose ("three scales displaced into the opposite semicircle, one 90° gap") was geometrically unrealizable — vacating three adjacent octant slots leaves a 180° arc — so the pinned set preserves the statistical intent (local redundancy + sparse arc) and the table says so. (iii) Pseudo-truth boundary status recorded as a config-table column (§2.4, §10.1). (iv) θ one-sided tie rule: exact arc-distance ties attribute to the upper/counterclockwise side (§5.1). (v) Variant D's exclusion argued explicitly in §3.2 (a fortiori from C's, as the union of B's and C's reasons). |

**Not adopted as a numbered change:** the review's unnumbered venue note
about non-Gaussian referee insurance is recorded as an option inside §12
item 4 (out-of-family arm) rather than a design change — it was offered as
Jeff's-choice insurance, not a required fix.

**New §12 items for Jeff:** item 8 (confirm or substitute the
region-aggregation rule before stage 1). Items 1–7 unchanged; the R4/R10
added cells fold into item 2's compute menu (they move the total by hours,
not days).

## References

- Bradley, J. V. (1978). Robustness? *BJMSP, 31*(2), 144–152.
- Browne, M. W. (1992). Circumplex models for correlation matrices.
  *Psychometrika, 57*(4), 469–497.
- Cudeck, R. (1989). Analysis of correlation matrices using covariance
  structure models. *Psychological Bulletin, 105*(2), 317–327.
- Efron, B. (1987). Better bootstrap confidence intervals. *JASA, 82*(397),
  171–185.
- Efron, B., & Tibshirani, R. J. (1993). *An introduction to the bootstrap.*
  Chapman & Hall. (Basic/BCa/studentized constructions; grouped jackknife.)
- Grassi, M., Luccio, R., & Di Blas, L. (2010). CircE: An R implementation
  of Browne's circular stochastic process model. *Behavior Research
  Methods, 42*(1), 55–73.
- Morris, T. P., White, I. R., & Kenward, M. G. (2019). Using simulation
  studies to evaluate statistical methods. *Statistics in Medicine, 38*(11),
  2074–2102. (The ADEMP reporting conventions §2/§5/§6 follow.)
- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
  interpersonal construct validation. *Assessment, 24*(1), 3–23.

## Change log

- 2026-07-08 — Initial design (Brief H, fresh Fable session). Venue
  recommendation (BRM) and the seven §12 decisions surfaced for Jeff before
  any implementation planning.
- 2026-07-08 — Revision vs H-review (Brief H-revision, fresh Fable
  session). All findings R1–R11 adopted, none rejected; R2's
  grouped-jackknife formula independently re-derived before adoption (it
  agrees). Must-fixes: one MC inference interval (cluster-level t), pinned
  BCa acceleration formula + jackknife-refit failure rule + corrected ties
  rationale, polished-harmonic scoring symmetry. Should-fixes: stage-3(g)
  large-N bootstrap cells; raw-scored basic/studentized intervals;
  region-aggregation rule; deterministic selection scalar; applied
  `jz2017` illustration. Hygiene: cold/warm throughput split; B = 2000
  sensitivity cell; `BASE_SEED` = 20260710; angle sets pinned numerically;
  boundary-status config column; θ tie rule; variant-D exclusion sentence.
  New §12 item 8 (region rule confirm/veto). See "Revision log
  (vs H-review)".
