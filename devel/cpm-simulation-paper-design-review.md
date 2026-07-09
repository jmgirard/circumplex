# Adversarial review of the CPM simulation-study design (Brief H-review)

**Reviewing:** `devel/cpm-simulation-paper-design.md` (Brief H, 2026-07-08).
**Reviewer:** Fable, fresh session, 2026-07-08 — did not author the design.
**Read:** CLAUDE.md, DESIGN.md (B6 record), ROADMAP post-M4 charter,
`devel/m4-coverage-oracle.R`, `devel/m4-browne-design.md` (§1–3, §5),
`devel/cpm-marker-validation.md`, the Brief H commission; design claims about
shipped behavior verified directly against `R/cpm_fit.R`.
**Oracle discipline held here too:** no coverage number below is asserted as
an expected outcome; B6/G figures are cited only as measured facts.

---

## Verdict

**Needs change — targeted revisions; the architecture stands.** The staged
factorial, the estimand framework (pseudo-truth projection with guards), the
interval-method set and θ's exclusion from order-statistic refinements, the
trustworthiness-surface framing, and the BRM venue recommendation all survive
adversarial reading and are consistent with the settled A/B6/G foundations.
Three findings (R1–R3) are must-fix before any implementation session builds
from this document — each is a place where the design as written would let an
implementer produce a plausible-but-wrong engine or an internally inconsistent
method comparison. The rest are should-fix strengthening and pre-registration
hygiene. Nothing re-opens `cpm_fit()` design decisions.

---

## Required changes (prioritized)

### R1 (must-fix). §6.1 vs §6.2 contradict each other on the MC interval — pick the cluster-level interval and say what it is

§6.1's decision rule says "cluster-level 95% **Wilson** interval"; §6.2
mandates "per-fit coverage proportions; **normal-theory interval on their
mean**… never naive binomial." These are different intervals, and Wilson is
not defined for the §6.2 data: a Wilson interval applies to a binomial count,
but a per-fit coverage proportion is a mean of correlated indicators taking
fractional values in [0, 1] — there is no k-out-of-n to feed it. As written,
an implementer can code either and cite the design.

**Fix:** the decision rule operates on the cluster-level normal-theory (t)
interval for the mean of per-fit coverage proportions (the G convention §6.2
already names), everywhere — including the one-sided per-side bands. Strike
"Wilson" from §6.1, or retain the binomial arithmetic only as the *planning*
bound with an explicit sentence that it is conservative for planning: treating
each fit as a single Bernoulli (SE ≈ √(.95·.05/R)) upper-bounds the
cluster-level SE because within-fit averaging cannot increase the variance of
a [0,1]-valued proportion beyond the Bernoulli case. (The B6 tables' naive
pooled-indicator Wilson was anti-conservative for the opposite reason — it
used n = fits × parameters; the design's critique of it is correct.)

### R2 (must-fix). BCa acceleration: the grouped-jackknife estimator is valid, but the design must state the formula and the reason — and fix the ties rationale

§4.3 commits to a delete-d (g = 100 groups) jackknife for the acceleration
without stating the estimator. This is the design's most implementation-
sensitive formula; leaving it implicit invites exactly the wrong "correction."

**The correct form.** With delete-group pseudo-values t₍ᵢ₎ (statistic on the
data minus group i), t̄ = (1/g)Σ t₍ᵢ₎, apply the *plain* BCa skewness formula
to the g grouped values:

    a = Σᵢ (t̄ − t₍ᵢ₎)³ / { 6 · [ Σᵢ (t̄ − t₍ᵢ₎)² ]^{3/2} }

with **no delete-d correction factor**. Why this is right (the design should
carry this argument or cite it): (i) a is invariant to any common rescaling of
the influence values, so the group-size constant relating (t̄ − t₍ᵢ₎) to the
group-summed influence Σ_{j∈i} L_j cancels between numerator and denominator;
(ii) the moment scaling then gives, with Σᵢ(ΣL)³ ≈ g·d·μ₃ and
Σᵢ(ΣL)² ≈ g·d·μ₂,

    a ≈ (g·d·μ₃) / (6·(g·d·μ₂)^{3/2}) = skew(L) / (6·√(g·d)) = skew(L)/(6√N),

identical to the full delete-1 jackknife's skew(L)/(6√N) to first order. So
the answer to "does it need a correction the design omits" is **no — but only
for the plain formula on delete-group pseudo-values**. The classic hazard is
importing the delete-d *variance* estimator's (N−d)/(N·d) factor into the
denominator only, which breaks the cancellation. Pin the formula in §4.3.
Costs of grouping are (a) a noisier skewness estimate from g = 100 terms —
second-order for an O(N^{-1/2}) correction and empirically gated by stage 3a,
which is well designed — and (b) nothing else.

**Two additions while in §4.2–4.3:**

1. **Jackknife-refit failure rule.** The grouped refits run warm-started in
   exactly the boundary regimes where refits fail acceptance or go degenerate,
   and the design is silent on what happens then. Pre-register: failed
   jackknife refits are excluded with a counted rate; if fewer than a stated
   floor of pseudo-values survive (suggest g_used < 50), a is NA and the BCa
   interval is NA-with-counted-reason, parallel to the B_used < 100 guard.
2. **The ties rationale is factually wrong for the shipped pipeline.** §4.2
   justifies the mid-rank z₀ convention by "ties are real here: polish mass at
   β* = 0, Heywood mass near ζ* = 1." Verified against `R/cpm_fit.R`
   (`cpm_bootstrap`, ~line 1017): replicates are refit **under the reported
   post-polish spec** and are never individually polished, so a polished-out
   harmonic is fixed at 0 in *every* replicate (a point mass — see R3), and a
   *kept* β is softmax-strictly-positive in every replicate — no exact ties at
   0. Likewise ζ* is logit-strictly-below 1 — mass *near* 1, no exact ties.
   Keep mid-rank (harmless, standard) but correct the stated reason; the real,
   correctly-identified hazard in that paragraph is z₀ saturation from
   one-sided *near*-boundary mass, which needs no exact ties.

### R3 (must-fix). Polished-out harmonics are scored asymmetrically across the bootstrap-family methods — the paired contrasts silently break

For a polished-out β_k, the shipped replicate column is identically 0
(R/cpm_fit.R:1017–1019). Under the design as written: the percentile interval
is the degenerate [0, 0] and is *scored* per §2.5; BCa's degenerate guard
("replicate distribution is a point mass → NA with a counted reason", §4.2)
returns *NA*; basic returns [2·0 − 0, 2·0 − 0] = [0, 0] and is scored. So the
same non-free parameter enters the pre-registered paired
percentile-vs-BCa contrast (§6.2) as a scored observation on one side and a
missing value on the other — a silent inconsistency in exactly the boundary
cells RQ2 is about, and one that mechanically flatters or damages BCa
depending on how the pairing handles NAs.

**Fix:** a polished-out harmonic is not a free parameter and has no
method-specific interval; score it **once** under the §2.5 rule (covering iff
truth exactly 0), attribute it to every bootstrap-family method identically
(they all inherit the same [0, 0] by construction, not by quantile rule), and
**exclude it from method contrasts** — there is nothing to contrast. The
§4.2 point-mass guard should be scoped to *kept* parameters (where a point
mass would signal a genuinely degenerate replicate set), not to
polished columns.

### R4 (should-fix). RQ2's headline "structural, not small-sample" claim has no bootstrap evidence above N = 2000

The claim shape for RQ2 ("the pathology is in the bootstrap distribution
itself…") and the M2 motivation ("flat in N — structural rather than
small-sample") promise an N-robust statement, but the bootstrap arm tops out
at N = 2000 (§3.1) — B6 already measured flatness to N = 1000, so the design
adds one octave. A referee gets to ask: does percentile β under-coverage at
near-boundary truths persist at N = 10⁴, where the analytic ladder shows the
Wald regime recovering? Nothing in the design answers.

**Fix (cheap):** add one or two pre-registered stage-3 cells — e.g.
trailing-t = .05 × octants × N = 10000, R = 250–500, percentile + BCa — at
roughly 0.3–1.1 M engine fits (~1–2 h at the §7.2 warm-refit rate). That buys
the paper's most quotable claim its N-range. Alternatively, soften RQ2's claim
language to field-N scope now, in the design, not later in the paper.

### R5 (should-fix). Basic-interval truncation scoring is ambiguous and can flip coverage verdicts at boundary truths

§4.1 says basic intervals' bound violations are "truncation counted and
reported" but never says whether the *scored* interval is raw or truncated.
The two disagree exactly in the study's key regime: truth β = 0 (overfit arm,
unpolished fit), t̂ small, replicate quantile q_lo > 2t̂ gives raw
[2t̂ − q_hi, 2t̂ − q_lo] entirely below 0 → raw scores a miss; truncating
endpoints into [0, 1] produces an interval abutting 0 → scores a cover. The
same ambiguity exists for ζ near 1.

**Fix:** pre-register that the **raw (untruncated) basic interval is scored**
— that is the method as defined, and the paper is measuring the method — with
the truncation rate reported beside it as interval-geometry (§5.2 already has
the slot). Note the studentized interval can also exit the natural bounds;
give it the same raw-scoring + counted-truncation treatment (§4.4 currently
tracks only SE-infeasibility).

### R6 (should-fix). Region-level claims need a pre-registered aggregation rule

The §6.1 decision rule is per cell × parameter × method × level, but RQ1's
claim shape is "a coverage surface with **named adequate/inadequate
regions**." With ~600 stage-1 cells × 3 parameter families (plus methods and
levels downstream), roughly 5% of truly-nominal cells will come out
"borderline" or worse by MC chance alone; the design never says how per-cell
verdicts aggregate into a region claim, which is where forking-paths room
re-enters after the cell-level rules closed it.

**Fix:** pre-register the aggregation: e.g. a named region is claimed adequate
iff ≥ 95% of its cells are individually adequate and none is individually
non-nominal (or an equivalent rule of Jeff's choosing), with the expected
false-flag count under the global null stated alongside; region *boundaries*
(e.g. "adequate for N ≥ X at interior truths") must be monotone claims fit to
the surface, not the single worst/best cell.

### R7 (should-fix). The stage-2/3 selection rules name no ranking metric

Stage 2 admits "the cells with the worst analytic angle **or** ζ coverage per
factor axis" — when the angle-worst and ζ-worst cells differ, which enters?
Can the per-axis-level picks exceed the 12-cell cap, and what breaks the tie?
Stage 3(b)'s "worst percentile performers" has the same gap. These rules are
the design's anti-cherry-picking mechanism; an ambiguous rule is a
discretionary rule.

**Fix:** define one scalar — suggest the maximum downward deviation from
nominal across the angle and ζ families (each measured by its cluster-level
point estimate) — rank by it, take one cell per axis level in rank order until
the cap binds, deterministic tie-break (e.g. smaller N first, then the §3.1
row order). Same scalar for stage 3(b) over β.

### R8 (should-fix, venue). Add an applied real-data illustration to the reporting plan

§8 contains no worked example. For BRM — and for the "CircE successor"
framing §9 leans on (CircE's own BRM paper was implementation + examples) —
a short applied section is genre-expected and cheap: one bundled dataset
(e.g. `jz2017` octant scales), `cpm_fit()` with both CI methods, the boundary
markers firing (or not) in the wild, and the paper's guidance applied to a
real decision. It is also the natural home for the `ssm_ci_accuracy()`
cross-reference §9 already plans. One figure + half a page; no new
simulation.

### R9 (minor). Stage-1 wall estimate applies warm-refit throughput to cold multi-start fits

§7.2's 240 fits/s anchor is dominated by B6's warm-started bootstrap refits
(3.0 M of its fits). Stage-1 fits are cold `cpm_fit()` calls — each is ~5–7
optimizer runs under the §3.5 multi-start scheme — and the large-N extension
cells pay O(N·p²) simulation/`cor()` per fit (G measured ~60–120 fits/s
all-in at those N). Stage 1 is realistically ~4–10 h, not 2–5 h. Immaterial
to the design (stage 1 is not the bottleneck) but the throughput section
should distinguish cold-fit from warm-refit rates so the stage-2/3 numbers —
whose arithmetic checks out against B6 (≈ 79 M fits / 240 s⁻¹ ≈ 3.8 days) —
stay credible.

### R10 (minor). The study runs boots = 1000 while the shipped default is boots = 2000

`cpm_fit()`'s signature default is `boots = 2000` (A §5.4); the study
evaluates "the shipped default interval" at B = 1000 (the B6 compromise,
§7.2). Defensible, but say it: either state openly that B = 1000 is the
study's budget compromise and why it does not change the percentile/BCa
comparison (same replicate sets, quantile stability at the 2.5% tail), or
spend one stage-2 sensitivity cell at B = 2000 to show invariance.

### R11 (minor, pre-registration hygiene — bundle)

- **Fix `BASE_SEED` now**, in the design document, not "at first full run"
  (§6.3.1): a seed chosen after any code exists is formally post-hoc.
- **Pin the angle sets numerically** at design time: the perturbed set's
  exact ±15° pattern and the clustered set's exact eight angles ("one 90°
  gap — three scales displaced into the opposite semicircle" is not yet an
  implementable spec, and it is a *generating* condition, so it belongs in
  the pre-registration).
- **Record each pseudo-truth's boundary status** (did the §2.4 projection
  polish a harmonic / land near a bound) in the config table — it determines
  which RQ2 regime a misspec cell actually tests.
- **Define the θ one-sided tie**: §5.1's "shorter angular direction from the
  interval" needs a deterministic rule when the two arc distances tie.
- **Name variant D's exclusion explicitly** in §3.2: the charter lists
  variants A–D; C's exclusion is argued, D (= B + C) is dismissed only by
  inheritance. One sentence closes the audit trail.

---

## Verified clean (genuine effort, not rubber-stamping)

- **Estimand framework (§2.4).** The pseudo-truth as deterministic ML
  projection of P₀ onto the fitted family is well-defined, n-free (F does not
  depend on n), and the right coverage target under each misspecification
  arm; the acceptance/multimodality/mirror guards plus
  redesign-or-drop-with-reason for ill-defined projections are the correct
  discipline. Angle handling is coherent end-to-end: reference excluded
  (fixed, matching B6's `drop_ref`), errors reference-relative on the
  shortest arc, and the fitted canonicalization (closest-to-theory, A §2.3)
  agrees with the projection's canonicalization so fits and pseudo-truths
  share a branch.
- **Scoring rules (§2.5), with R3/R5 excepted.** The anchor-free span rule is
  the B6-hardened rule and handles the 0/360 pole per DESIGN G2 without
  special-casing; zero-width intervals score consistently under it
  (lci = uci ⇒ covers iff exact); the analytic NA-CI and zero-width-clamp
  treatments match the shipped mechanisms I verified in `R/cpm_fit.R`
  (all-family NA when `solve()` rejects the FD Hessian; `pmax(diag(avar), 0)`
  clamping at ~line 940).
- **θ's exclusion from order-statistic refinements (§4.1)** is argued
  correctly — bias-correction and acceleration re-index an ordered replicate
  set and a circle has no total order — and matches the recorded M2 BCa-drop
  rationale. The basic interval's exclusion for θ is likewise right (2t̂ − q
  is branch-dependent on a circle).
- **Grouped-jackknife acceleration is valid as sketched** (the R2 derivation:
  plain formula on delete-group pseudo-values is first-order identical to the
  full jackknife, no correction factor) — the design's cost move is sound and
  the stage-3a full-vs-grouped gate with a shrink-the-claim fallback is the
  right failure posture. R2 asks only that the formula and argument be
  stated.
- **Bradley machinery.** The per-side band [.5·(α/2), 1.5·(α/2)] is the
  correct one-sided analogue of the liberal band ([.0125, .0375] at
  nominal .025); the two-sided [.925, .975] matches Bradley (1978). Stage-1
  R = 2000 is decisive at the stated precision under the R1-corrected
  interval.
- **Pairing design (§6.2).** Sharing one replicate matrix across
  percentile/basic/BCa/levels and contrasting within fit is the right
  variance lever and matches the §10.2 engine delta; cluster-level
  summarization correctly repairs B6's naive pooled Wilson.
- **Oracle discipline held.** Every B6/G number in the design is cited as a
  measured fact about named cells (M1–M7, §1); none is converted into an
  expected outcome; using G's measured firing rates to *size* the
  provocation arm (§6.1) is power planning, not oracle use; `g2xx1.txt` is
  banned in the preamble.
- **Charter audit (Brief H items 1–7).** All seven numbered requirements are
  covered: estimands with circular scoring (1 → §2); the full factorial with
  ζ heterogeneity, p, misspecification, β geometry, and the N ladder into
  the recovery regime (2 → §3, with the equally-spaced pathology cells
  ⚑-flagged and analyzed as the RQ5 interaction claim, not an aside); the
  competitor set with per-method applicability and the argued-out
  double bootstrap (3 → §4); two-sided + one-sided coverage, widths, bias,
  and Heywood/convergence as first-class outcomes (4 → §5); the MC error
  budget with pre-registered exclusion/reporting rules including the
  worst-case unconditional bound (5 → §6); the RNG contract with
  no-per-call-seed and the BLAS-provenance pin (6 → §7); venue argued both
  ways and shaped (7 → §9). The brief's "ζ, β, amplitude — not displacement"
  sentence is translated to CPM parameters (θ circular; SSM
  amplitude/displacement scoped to the parked Assessment companion) — a
  defensible reinterpretation, made explicitly rather than silently, and
  consistent with the venue shaping. Variant C/D exclusion is argued for C
  and optioned in §12; see R11 for the D sentence.
- **Compute (stages 2–3).** The arithmetic against B6's measured throughput
  is honest (headroom multipliers for p = 16 and N = 2000 stated; stage-2
  ≈ 66–80 M fits ⇒ 3–5 days at ≈ 240 fits/s checks out); the knobs are scope
  statements with the right refusal to cut boots below 1000; checkpointing
  is mandatory. Only the stage-1 cold-fit rate needs the R9 caveat.
- **Venue (§9): BRM confirmed on the merits.** The contribution profile
  (only maintained implementation of Browne's model, factorial
  operating-characteristics study, user-actionable guidance, strong
  reproducibility posture) is BRM's core genre, and the audience that lost
  CircE is there; splitting the SSM-layer/Assessment story into a companion
  paper is the right dilution call. Survivability at BRM review is good
  *conditional on* R8 (applied illustration) and R4 (the structural-claim
  N-range); the Gaussian-only limitation (§11) is honestly stated and
  standard, though the discussion should expect a referee to ask for a
  non-normal robustness arm and may pre-empt it by folding one non-Gaussian
  condition into the §3.3 out-of-family arm if Jeff wants insurance.

---

## Summary for the revision

Must-fix before implementation: **R1** (one MC interval, defined), **R2**
(BCa acceleration formula + refit-failure rule + ties rationale), **R3**
(polished-harmonic scoring symmetry). Should-fix: **R4–R8**. Hygiene:
**R9–R11**. None of these re-opens the staged factorial, the estimand
framework, the interval set, or the venue; the revision is a targeted edit,
not a redesign.

---

# Re-check of the H-revision (2026-07-08)

**By:** the original H-review session (kept independent of both the design
author and the reviser for exactly this pass). **Checked:** the revised
`devel/cpm-simulation-paper-design.md` including its "Revision log
(vs H-review)", finding by finding against the review above; shipped-
mechanics claims re-verified against `R/cpm_fit.R` directly, not the
revision's paraphrase; the pinned angle sets verified computationally
(gap arithmetic and the unrealizability claim checked by exhaustive
enumeration, not by eye). Scope: closure on R1–R11 plus defects the edits
themselves introduced; no new adversarial territory.

## Verdict table

| Finding | Status | Basis |
|---|---|---|
| R1 (Wilson vs cluster interval) | **RESOLVED** | §6.1 now names exactly one MC inference interval (cluster-level 95% normal-theory t on mean per-fit coverage proportions), applied to two-sided and per-side bands alike; Bernoulli arithmetic explicitly demoted to a planning bound with the correct conservativeness argument (no [0,1] variable exceeds Bernoulli variance at the same mean). Remaining "Wilson" mentions (§6.2, §10.4) are historical references to the B6 rule being replaced — correct usage. |
| R2 (BCa acceleration, refit failures, ties) | **RESOLVED** | §4.3's formula is the one the review meant — plain skewness formula on the g delete-group pseudo-values, no delete-d correction — and the revision's independent derivation is *correct and sharper than the review's*: t̄ − t₍ᵢ₎ = (S_i − S̄)/(N − d) exactly (centered block-summed influences with one common constant), from which the review's moment argument follows. The anti-hazard sentence (do not import the delete-d variance factor into the denominator alone) survives. The refit-failure rule (acceptance keying with the deterministic-restart retry — matching the shipped `cpm_bootstrap` retry at R/cpm_fit.R:1067–1077 — counted exclusions, g_used < 50 ⇒ a = NA ⇒ BCa NA-with-reason) is sound and parallel to the B_used guard. The ties rationale is now factually correct against `cpm_bootstrap`: kept β softmax-strictly-positive, ζ logit-strictly-below-1, polished columns carry no BCa interval, live hazard correctly renamed z₀ saturation. |
| R3 (polished-harmonic scoring symmetry) | **RESOLVED** | Internally consistent across all three sections: §2.5 (scored once, attributed identically, excluded from contrasts), §4.2 (point-mass guard scoped to kept parameters), §6.2 (paired differences over kept parameters only). Re-check went one step further than the review: the *analytic* side coheres too — `cpm_analytic_se()` returns SE = 0 for a polished-out harmonic (R/cpm_fit.R:916–917, 961–975), so its zero-width [0, 0] under §2.5's clamped-CI rule yields the same cover-iff-truth-exactly-0 verdict as the bootstrap-family rule. No method anywhere scores the polished harmonic differently. |
| R4 (large-N bootstrap evidence for RQ2) | **RESOLVED** | Stage-3(g) as specified (trailing-t = .05 × octants × N ∈ {5000, 10000}, R = 500, all bootstrap-family methods + grouped jackknife); arithmetic checks (2 × 500 × 1101 ≈ 1.1 M fits); RQ2's estimand text names the extension; the add-cells-over-soften choice is justified in the log and is the option the review recommended. |
| R5 (basic-interval truncation scoring) | **RESOLVED** | Raw (untruncated) interval pre-registered as scored (§4.1) with the flip rationale stated; extended to the studentized interval (§4.4); §5.2 tracks truncation for both. |
| R6 (region-aggregation rule) | **RESOLVED** | The pinned default is sound — judged on the merits, as asked. The "none non-nominal" conjunct is robust (at stated stage precisions a truly-nominal cell essentially cannot be verdicted non-nominal; that requires a ≳4·SE deviation of the point estimate). The ≥ 95%-adequate threshold against ~5% borderline-by-chance means some truly-adequate regions fail to be *claimed* (e.g., a 20-cell region has ~26% chance of ≥ 2 borderline cells under independence) — an error in the conservative direction: the rule fails-to-claim, never false-claims. Pre-stage-1 region declaration + mechanical verdicts + the printed false-flag expectation close the forking-paths room the review flagged. Correctly surfaced as §12 item 8, since the review explicitly left the exact rule to Jeff. Endorse the default. |
| R7 (selection-rule metric) | **RESOLVED** | Scalar, ranking, admission order, cap, and tie-break are all pinned and deterministic; stage-3(b) reuses the same scalar. One trivial wording nit, non-blocking: state that admitted cells are *deduplicated* (one cell can satisfy several axis levels) — the natural reading, but one word makes the driver's implementation unambiguous. |
| R8 (applied illustration) | **RESOLVED** | §8 adds the `jz2017` worked example (a real bundled dataset, complete-case per the M4.5 record) with both CI methods, markers in the wild, and the `ssm_ci_accuracy()` cross-reference — exactly the genre-fit fix. |
| R9 (cold-vs-warm throughput) | **RESOLVED** | §7.2 splits the rates (~240 fits/s warm anchor; ~60–120 fits/s cold all-in, 5–7 multi-starts), stage 1 revised to ~4–10 h. Stage-2/3 arithmetic re-verified: 61 + ≤ 12 cells ≈ 67–82 M (incl. the B = 2000 cell's ~2.1 M), stage 3 ~57 cells ≈ 36–47 M, total ~1.05–1.35 × 10⁸ — all consistent. |
| R10 (B = 1000 vs shipped 2000) | **RESOLVED** (one wording nit) | Compromise stated openly + pre-registered stage-2 sensitivity cell at B = 2000 with an agreement criterion — the stronger both-halves option. Nit, non-blocking: "method *comparisons* are unaffected by B … noise … enters every method identically" overstates — BCa's adjusted quantile levels reach different order statistics than percentile's, so B affects its endpoint clamping rate differentially. The design already neutralizes this in substance (the sensitivity cell re-runs *all* bootstrap-family methods by stage-2 construction, and clamping is a counted §5.2 outcome), so only the sentence overreaches; soften to "shared replicates remove most B-noise from the paired contrasts" when convenient. |
| R11 (hygiene bundle, incl. the pinned angle sets) | **RESOLVED** | (i) `BASE_SEED` = 20260710 pinned, disjoint from 20260706/20260708 ✓. (ii) **The reviser's geometry claim is TRUE — verified exhaustively.** Over all C(8,3) = 56 ways to vacate three octant slots, no pattern yields exactly one 90° maximal gap (16 patterns reach max-gap = 90° but always as *multiple* 90° gaps, e.g. removing {90°, 180°, 270°}; adjacent removals give 135° or 180°) — so the original clustered prose was unrealizable as stated. The revision's quoted reason ("three adjacent slots ⇒ 180° arc") covers only the adjacent case, but the conclusion holds in full generality. The substituted set {45, 90, 100, 110, 200, 245, 290, 360} checks out: gaps (45, 10, 10, 90, 45, 45, 70, 45) sum to 360 with a *unique* maximal 90° gap; the 90–110 triplet spans exactly 20°; the pole (360, LM convention) is occupied. It serves the A-§2.5 purpose the cell exists for — near-duplicate angles (10° spacings → ζ trade-off/Hessian-conditioning hazard) plus a sparse arc (weakly determined ρ over the empty region). The perturbed set is exactly octants + alternating (+15°, −15°) ✓, faithful to H's original prose. (iii) boundary-status config column in §2.4/§10.1 ✓. (iv) θ tie rule deterministic (CCW side) ✓. (v) variant-D exclusion argued explicitly ✓. |

## New-defect sweep (RESOLVED-BUT hunting)

Actively checked, none found blocking: the pinned octant/p = 16 sets match
the shipped LM = 360 convention; the stage-2 cell count (60 + 1 sensitivity
+ ≤ 12 flagged) is consistent between §3.4 and the §7.2 table; the
revision's §2.5 code claim (replicates refit under the post-polish spec,
removed-harmonic column identically 0) re-verified at R/cpm_fit.R:1016–1019
with `cpm_unpack()` zero-filling removed harmonics; oracle discipline
intact — the only new number is `BASE_SEED` (a design constant), no
coverage figure became an expected outcome, and §6.1's "~5% borderline by
chance" is a property of the decision rule under a hypothetical, not a
predicted result. The two wording nits (R7 "deduplicated", R10
"unaffected") are editorial and do not block implementation.

## Overall call

**The revision closes the review. Ready for Jeff's §12 decisions** —
chiefly 1 (venue), 2 (compute appetite, now including the R4/R10 cells),
and the new 8 (confirm or substitute the region-aggregation rule before
stage 1 runs). The two editorial nits can ride along with any future touch
of the design; neither needs a dedicated pass.
