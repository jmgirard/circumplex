# Brief B-review: adversarial review of `devel/m4-ci-accuracy-spec.md`

**Reviewer:** Fable, fresh session, 2026-07-03 (no involvement in Brief A, B,
or A-review; judged only what is on the page, per the brief).
**Scope:** the five attack fronts in Brief B-review
(`devel/fable-briefs-2026-07.md`): Z&W number hygiene, the (a)-vs-(b)
decision, the amplitude-near-zero target, the A↔B contract, and overall
statistical soundness. Findings below were checked against the shipped code
(`R/ssm_oop.R`, `R/ssm_bootstrap.R`, `R/ssm_montecarlo.R`,
`R/ssm_analysis.R`), Brief A's §5.4 sketch, and two seeded simulations run
against the package itself (probe script preserved in the session scratchpad;
key numbers reproduced inline below).

## Verdict: NEEDS CHANGE

The spec's overall architecture is sound and its oracle hygiene is exemplary
— I could not find a single confabulated Z&W number (the top suspect came up
clean). But the amplitude-near-zero module — the absorbed-M2 requirement that
is the diagnostic's core justification — is built on a certification event
that is **degenerate as literally defined and different from the shipped
guardrail**, and the contrast ladder targets a regime in which the
contrast-level pathology it claims to probe **does not occur** (verified by
simulation). F1–F3 must be fixed before implementation; F4–F9 are cheap.

---

## Required changes (prioritized)

### F1 (high) — The certification event "amplitude lci > 0" is degenerate and is not the shipped guardrail

§3.4 records "amplitude lci > 0 (displacement certified)"; §4.3 defines the
false-certification rate as P(amplitude lci > 0 | a₀ = 0) with "nominal level
α/2"; §5.1 classifies it against the band [0.5·α/2, 1.5·α/2]; §5.2's sketch
imagines "wrongly certified 8.9% of the time (nominal 2.5%)".

Two independent defects:

1. **Degeneracy.** The percentile lower bound is a quantile of bootstrap
   amplitude replicates, and the amplitude of any non-degenerate profile is
   strictly positive, so `lci > 0` holds with probability 1 (exact zeros
   require flat/zero-amplitude resamples — measure-zero for continuous
   data). Under the spec's literal rule: false-certification rate ≡ 1 at
   c = 0, the §4.3 "power curve" ≡ 1 at every rung, and §10's known-bad
   direction oracle is satisfied vacuously. Empirical spot-check: two groups
   with population amplitude 0.02 (≈ zero relative to sampling noise,
   n = 150, unit-variance scales) still produced amplitude lci of 0.0132 and
   0.0085 — strictly positive, as the argument requires. The §5.2 sketch's
   "8.9%" is unattainable under the rule as written; the author evidently
   imagined a non-degenerate rate the definition cannot produce.
2. **Mismatch with the shipped rule.** The actual guardrail in
   `print.circumplex_ssm` (R/ssm_oop.R:159) is
   `round(a_lci, digits) <= 0` with `digits = 3` by default — i.e.
   certification requires a_lci ≥ 0.0005 **in amplitude units, dependent on
   a print argument**. That rounded rule is non-degenerate (which is exactly
   why the shipped guardrail functions at all), but its threshold is an
   accident of display precision and is scale-dependent (0.0005 means
   something on a correlation-metric amplitude and almost nothing on a raw
   score metric). DESIGN.md's phrase "amplitude CI excluding 0" glosses this;
   the spec inherited the gloss without checking the operationalization.

**Also wrong:** the "nominal α/2" framing. The CI-excludes-0 ⟺
level-α/2-one-sided-test duality fails for a boundary-constrained nonnegative
parameter whose percentile interval cannot contain 0; there is no test here
with nominal level α/2. Comparing the measured rate to α/2 is defensible only
as a *user-expectation benchmark* ("users read CI-excludes-0 as a 2.5% test"),
and the spec should say so and state the theoretical prediction (rate far
above α/2; exactly 1 under the strict rule).

**Required:** (i) define the certification event exactly as the shipped
decision rule, rounding included, and pin what `digits` means for the
diagnostic; (ii) surface to Jeff the companion package decision this exposes
— the shipped guardrail's effective threshold is a display-precision artifact
and arguably needs a principled definition (that decision belongs to the
package, not silently to this spec); (iii) reframe §4.3/§5.1's α/2 comparison
as a benchmark, not a nominal level, and drop or recompute the Bradley band
around it accordingly. Sections touched: 3.4, 3.5, 4.3, 5.1, 5.2, 10.

### F2 (high) — The contrast ladder targets a regime where the pathology does not occur

§4.1: "Contrast objects get the same ladder applied to the **contrast**: the
second group's … profile is moved toward the first's so the population
contrast amplitude is scaled by c while each row stays a realistic profile —
this targets the ROADMAP's contrast-level pathology (near-uniform contrast
draws; estimate outside a very wide circular CI) **in exactly the regime it
occurs**."

The italicized claim is refuted by simulation against the shipped machinery
(seed 42, octant scales, n = 150/group, 500 resamples):

- *Spec's ladder regime* — both rows at population amplitude 0.8, population
  profiles nearly identical (population contrast amplitude ≈ 0): contrast
  displacement CI **14.3° wide**, estimate comfortably inside. Concentrated
  draws; no pathology. This is expected: Δd̂ = d̂₂ − d̂₁, and each row's
  displacement is precisely determined when its own amplitude is large
  relative to sampling noise, so their difference is too — regardless of how
  close the profiles are.
- *Actual pathology regime* — both rows at population amplitude 0.02
  (≪ sampling noise): contrast displacement CI **326.2° wide** — the
  near-uniform contrast draws the ROADMAP describes.

The contrast-level pathology is driven by a **row** amplitude that is small
relative to its sampling error (making that row's displacement replicates
near-uniform, hence Δd near-uniform), not by the between-profile difference
shrinking. The spec's ladder deliberately holds row amplitudes realistic, so
its branch-pathology counter would read ≈ 0 at every rung and the module
would report the contrast machinery healthy in a regime that never stresses
it. (§10's seeded "contrast configured so the branch pathology occurs" test
would eventually force an implementer to discover this — good — but the spec
directs the design effort at the wrong regime.)

**Required:** redefine the contrast module so the ladder scales the **row**
first harmonics (the same §4.1 row ladder, applied to the rows of a contrast
object — one row, or both), which manufactures the near-uniform-Δd regime;
keep the profiles-converging ladder only if separately justified (it measures
Δa-near-zero behavior, a different and much milder question), and correct the
"exactly the regime it occurs" sentence. Sections touched: 4.1, 4.3, 10.

### F3 (medium) — Ladder truth claims hold only for equally spaced angles

§4.1 claims c = 0 yields "a population with amplitude exactly 0", and §3.3
repeats it ("amplitude exactly 0 as in the §4 c = 0 condition"); implicitly
the ladder promises a₀(c) = c·â and d₀(c) = d̂. These follow from
resid ⊥ {1, cos θ, sin θ} under the closed-form estimator — which holds
**iff the angles are equally spaced** (the estimator is linear in the
profile; orthogonality fails off the equal-spacing design, which is the very
CLAUDE.md invariant §1 of the spec leans on). For unequally spaced angles the
closed-form (x, y) image of `resid` is nonzero, so a₀(c = 0) ≠ 0 and the §4.3
false-certification rate is not evaluated at a₀ = 0 — inconsistent with the
spec's own selling point that option (a) covers "unequal spacing, any p".

**Required:** either (i) define the ladder through the estimator functional —
choose the subtracted component so the closed-form (x, y) image scales
exactly by c (a 2×2 linear solve; cheap, exact for any spacing) — or
(ii) restrict the exactly-zero claim to equal spacing and key the §4.3 rates
to the actual per-rung truth a₀(c) recomputed from the population vector
(§3.3 already requires recomputing truths, so the machinery is there; only
the claims and the c = 0 conditioning are wrong). Option (i) is cleaner.
Sections touched: 3.3, 4.1, 4.3.

### F4 (medium) — c = 0 amplitude coverage is a theorem, not a measurement

Corollary of F1's root cause: since percentile amplitude intervals cannot
contain 0, amplitude coverage at the c = 0 rung is identically 0 with all
misses on the low side. §4.2 presents "amplitude CI coverage with its
one-sided decomposition" at every rung as an empirical signature; at exactly
c = 0 it is a mathematical certainty and carries no information. State this
in §4.2 and note the informative rungs are the small c > 0 ones (where
coverage of a₀ = c·â is a genuine, non-trivial quantity). No design change —
a claims correction that prevents an implementer from "validating" the
module against a tautology.

### F5 (low) — Wilson interval level unspecified

§5.1 classifies via "the Wilson score interval of the empirical coverage"
without stating its confidence level. Pin it (presumably 95%); the
adequate/borderline boundary moves with it.

### F6 (low) — The pinned `cpm_fit()` call is incomplete against A's signature

§3.2's call `cpm_fit(cormat = R_w, n = …, m = …, model = "quasi-circumplex",
ci_method = "analytic")` omits `scales` (no default in A §5.4) and `angles`
(defaults to `octants()`). For a non-octant analysis the CPM must be started
and canonicalized at the user's `details$angles`, and the reference
convention follows from them. Since §8.1 is "pinned", pin the full call.
(The n = Σn_g − G + 1 device is verified correct: A's internal multiplier is
n_passed − 1, giving the pooled-within Wishart df Σ(n_g − 1).)

### F7 (low) — Z&W-reproduction gate assumes their generating process is MVN-reproducible

§10's O5 bridge requires the diagnostic's coverage to match published Z&W
values "within combined Monte Carlo error". If Z&W's studies generated
non-MVN data or resampled real datasets, agreement may be unattainable for
reasons other than bugs. Add the hedge: the gate is conditional on their
generating process (TBT from the supplement) being expressible under this
simulator; if transcription shows otherwise, the gate is re-scoped at that
point, documented, not silently loosened.

### F8 (informational) — The (a)-vs-(b) rationale rests on remembered qualitative properties of Z&W

No numeric confabulation (see the clean list below), but the case against (b)
— "their condition grid is coarse and fixed", "specific instruments, specific
n values", "their software stack" — is a from-memory qualitative
characterization of Studies 1–5 doing real load-bearing work in a decision
already made. It is almost certainly right (every simulation study has a
finite grid) and the decision was properly surfaced to Jeff before locking
(§2, §13 — the brief's process requirement is satisfied). Requirement: when
the paper is transcribed for the vignette, re-confirm the grid
characterization and note the confirmation in §2 or the change log.

### F9 (low) — Multi-row ladder under-specified

For objects with several profile rows (multi-group, or several measures
sharing one joint matrix), §4 does not say whether the ladder is applied per
row independently (others held at c = 1) or jointly. Independent-per-row is
natural for groups (separate populations); for measures the conditions
couple through the shared joint matrix and its PSD repair. One sentence pins
it.

---

## Attack fronts that came up clean (genuine effort, no break)

- **Z&W numbers (front 1, the top suspect): clean.** Every Z&W-specific
  value is marked TBT with the transcription protocol named; §5.2's numbers
  are labeled "illustrative placeholders only"; §6's fixture cells are all
  TBT with provenance required. The only fixed numbers are package
  conventions verified against shipped code (fit ≥ .70 → R/ssm_oop.R:152;
  |r| ≥ 1 − 1e-12 → R/ssm_montecarlo.R:65), cited criteria (Bradley 1978
  liberal band; [.925, .975] at 95% is the correct algebra), design constants
  (reps, ladder, PSD-repair 0.01, 1e-8 consistency check), and arithmetic
  that all checks out (SE ≈ 0.7 pp at reps = 1000 and ≈ 1.0 pp at 500;
  ±2.5 pp band; reps × boots = 2×10⁶ evaluations per condition).
- **The (a)-vs-(b) decision (front 2): justified, not asserted**, with the
  trade-off stated both ways, the decision surfaced to Jeff before locking,
  and (b) retained as a requirements record. The (a) loop estimates the
  right estimand (coverage of the package's own closed-form functional at
  the plug-in truth, correlation-path truth taken from the *repaired*
  matrix — the internally consistent choice), replays the user's exact
  procedure (engine, boots, interval), and reps = 1000 is adequate for the
  Bradley band. Subject to F1/F3, the coverage logic is sound.
- **The A↔B contract (front 4): no invented interface.** Every consumed
  field exists in A §5.4 (`matrices$Phat`; `fit$rmsea/srmr/chisq/df/pvalue`;
  `details` acceptance/boundary/multimodality/m; `results$Angle`,
  `results$Zeta`; `betas$Beta`; `cpm_simulate(object, n)`). Gaps G1–G4 are
  genuine under-specifications in A, flagged rather than papered over —
  exactly what the brief hoped a cold read would do — and G4's correction of
  A §8's Phase-2 trigger is right (B performs one CPM fit, zero refits;
  its hot loop is the SSM procedure). §8.3's claim that `circumplex_ssm`
  stores no n_g/SDs/correlation matrices is verified true against
  `R/ssm_analysis.R` (details = boots, interval, listwise, angles, contrast,
  score_type, method), so the companion storage change is genuinely
  prerequisite. Residual nit: F6.
- **Circular soundness (front 5):** displacement coverage as angular
  membership modulo 360° is well-defined against
  `quantile.circumplex_radian`'s construction (independently wrapped
  endpoints, lci→uci counterclockwise, width < 360°), makes pole coverage
  insensitive to the 0-vs-360 report (DESIGN G2), and the branch-aligned
  membership for contrasts matches `ssm_replicate_intervals`'s shifting.
  No amplitude/displacement conflation anywhere; the
  certification-conditional displacement coverage is a deliberate,
  correctly-reasoned choice of the decision-relevant estimand (conditional
  coverage has no automatic 95% guarantee, and the spec knowingly holds the
  guardrail to that user-facing promise). The degenerate-population
  taxonomy (flat → refuse; pure-higher-harmonic c = 0 → proceed, d₀ NA)
  matches the estimator's shipped semantics.
- **RNG contract:** per-replicate L'Ecuyer-CMRG substreams give the right
  user-facing guarantee (byte-identical across ncpus) and the spec correctly
  distinguishes it from the bootstrap's master-pre-draw mechanism.
  (Implementation note, not a defect: switching RNGkind requires
  save/restore of the caller's RNG state.)

## Bottom line

The spec is closer to implementable than most first drafts of this
difficulty — the estimand, the contract discipline, and the oracle hygiene
are all right — but it cannot go to Opus as-is: F1 would ship a
false-certification module that always reads 1 (or silently measures a
display-rounding artifact), F2 aims the contrast module at a regime where
the pathology it exists to measure cannot occur, and F3 breaks the ladder's
truth claims for exactly the unequal-spacing generality the spec advertises.
All three are spec-text fixes plus one surfaced package decision (the
guardrail's principled threshold, F1.ii) — no architectural rework.

**Recommended tier for the revision:** Fable (it is estimator-adjacent
decision-rule design — F1.ii in particular changes what the shipped
guardrail *means*); the mechanical F5/F6/F9 edits are Sonnet-grade if split
out.
