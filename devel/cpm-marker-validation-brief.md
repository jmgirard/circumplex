# Fable brief — validate the CPM analytic-CI boundary-marker set (B6 follow-up)

**Status:** handoff brief for a Fable session (estimator-adjacent; the failure
mode is a *plausible-but-wrong calibration* of an advisory, so it earns the
tier). **Tier: Fable.** Surfaced by the v2.0.0 pre-release item "B6 analytic-CI
caution — Jeff to confirm/veto" (ROADMAP `## CRAN release strategy` →
v2.0.0 pre-release items). Jeff has ratified the two N thresholds and the
caution tone; this brief researches the one part he could not adjudicate by
judgment — **the marker set itself**.

**Read first:** CLAUDE.md invariants (angles: degrees [0,360) in the API,
LM=360, radians internally); DESIGN.md displacement-boundary + CPM sections;
ROADMAP.md v2.0.0 pre-release items (the B6 caution bullet, W1 outcome);
`R/cpm_fit.R:1150-1197` (the two threshold constants and
`cpm_boundary_markers()`); `R/cpm_oop.R:216-242` (the `summary()` caution that
consumes them); `devel/m4-browne-design.md:458-491` and `:806-839` (the
analytic-vs-bootstrap CI decision and the B6 validation-battery log);
`devel/m4-coverage-oracle.R` (the existing oracle — this brief extends it).

---

## Background — what is and isn't already validated

`cpm_fit()`'s **analytic (Wald) confidence intervals** are the only CI option on
the `cormat` path. The B6 coverage oracle measured that they **mis-cover
materially at field-typical N** (angle coverage .76–.88 at N ≤ 1000; recovering
to nominal by N ≈ 50000, but staying .70–.81 in near-boundary regimes into the
tens of thousands). The mitigation is an N-conditional caution printed by
`summary()`:

- **N < 2000** → caution **unconditionally**.
- **2000 ≤ N < 50000** → caution **only if a boundary/weak-identification marker
  fires**, via `cpm_boundary_markers()`.
- **N ≥ 50000** → silent.

**Validated already (do not re-litigate):** the two thresholds (2000 / 50000).
The oracle measured coverage as a function of N and truth regime and the cuts
follow directly. Jeff has ratified them.

**NOT yet validated — the object of this brief:** the marker set
(`R/cpm_fit.R:1179`) as a **runtime-observable predictor of mis-coverage**:

```r
cpm_boundary_markers <- function(object) {
  # fires on any of:
  #   Heywood communality           (d$heywood)
  #   boundary harmonic removed      (length(d$removed_harmonics) > 0)
  #   small correlation-function weight   (min(betas$Beta) < 0.10)   # judgment call #1
  #   ill-conditioned Hessian        (d$hessian_condition > 1e8)
  #   competing near-tied optima     (d$multimodal)                  # judgment call #2
}
```

Two calibration choices in there rest on reasoning, not measurement, and are
exactly what Jeff flagged he lacks the expertise to adjudicate:

1. **The β = 0.10 cut** for "small correlation-function weight" — chosen looser
   than the 1e-2 polish trigger to flag the oracle's near-boundary config
   (trailing β = .05) with margin while passing its all-interior config
   (smallest β = .15). Is 0.10 the right cut, or should it be .05 / .15?
2. **Including `multimodal`** — the coverage oracle never measured multimodality
   separately; it was added on the reasoning that near-tied optima are the same
   weak-identification regime. Does it actually predict mis-coverage?

## The gap in the current oracle (why new measurement is needed)

The oracle (`devel/m4-coverage-oracle.R`) aggregates coverage by **known truth
configuration** (`boundary` vs `interior`, lines 54–57) and by N — and its N
grid is `{250, 500, 1000}`, entirely **below** the 2000 threshold where markers
even operate. So it validated the *thresholds* but says nothing about the
markers as **runtime predictors**: at runtime the user cannot see the truth
config, only which markers fired. The operative, unmeasured question is:

> Among analytic fits where marker M fired, what was the coverage? Among those
> where it did not? Does M separate the mis-covering fits from the good ones —
> and at N in the 2000–50000 band where the caution is marker-conditional?

That is a conditional operating characteristic the current oracle discards.

---

## The measurement to build

Extend the oracle into a **marker-validation run** with these design choices.

### Scope: analytic-only, no bootstrap

The markers gate the **analytic** caution; the bootstrap is the *recommended
alternative*, not the thing under test. So drop the bootstrap arm entirely.
Fit each replicate on the **`cormat` path** — the literal code path the caution
applies to — which invokes no bootstrap machinery at all:

```r
X   <- cpm_simulate(truth_fit, N)          # existing simulator
R   <- stats::cor(X)
fit <- cpm_fit(cormat = R, n = N, scales = scales, angles = angles, m = 3)
# record: cpm_boundary_markers(fit), each marker's raw input, and analytic coverage
```

This is what makes the run cheap (one fit per rep, zero bootstrap refits) and is
also *more correct* than fitting via `data=`, because it exercises exactly the
analytic-CI path `summary()` cautions about. Analytic coverage per parameter is
already computed in the oracle (`ana_angle` / `ana_zeta` / `ana_beta`,
`m4-coverage-oracle.R:118-124`) — reuse that membership logic.

### N grid: the marker band only

`NS <- c(2000, 5000, 10000, 20000, 50000)`. Below 2000 the caution is
unconditional (markers irrelevant); 50000 is the upper gate. Include 50000 as
the "markers should mostly stop firing / coverage recovered" upper control.

### Truth configs: span the boundary continuum so markers fire *and* mis-cover

The conditional analysis pools fitted replicates and conditions on the
**observed** marker, so you need truths that (a) actually produce mis-coverage
and (b) make markers fire across a range. Use a small ladder of trailing-harmonic
magnitudes plus interior controls, e.g.:

```r
# zeta fixed at 0.75 (as in B6) unless a config needs a Heywood-prone level
trailing_beta <- c(0.00, 0.02, 0.05, 0.10, 0.15)   # last harmonic; rest interior
```

Keep the B6 `boundary`/`interior` pair as anchors so results tie back to the
committed oracle. If Heywood and ill-conditioned-Hessian markers fire too rarely
under ζ = 0.75 to estimate their conditional coverage, add one higher-ζ /
octant-like config that provokes them (note it explicitly; don't silently widen
scope). It is acceptable for some markers to be **untestable for lack of
firings** — report that as a finding (see "honest nulls" below), don't force it.

### The core deliverable: per-marker conditional coverage

Pool all fitted replicates within the marker band and, **per marker** and for
**any-marker**, tabulate:

| marker | fired? | n fits | coverage: angle / ζ / β | MC ± |
|---|---|---|---|---|

The discriminating question, per marker M and parameter family:

- `coverage | M fired` — should be **well below** nominal .95 if M earns its place.
- `coverage | M not fired` — should be **near** .95 (else M misses mis-covering fits).

Report all three parameter families (angle, ζ, β); angle and ζ were the worst
mis-coverers in B6, so they carry the signal. Also report the **any-marker**
row — that is what `summary()` actually gates on.

### The two sensitivity sweeps (post-hoc, free)

Both are re-tabulations of already-recorded per-fit inputs — **no refitting**:

1. **β cut.** Recompute the "small weight" marker at cut ∈ {0.05, 0.10, 0.15}
   from recorded `min(β̂)`. Report, per cut: `coverage | fired`, and the
   **false-alarm rate** (fraction of *well-covered* fits the marker flags).
   The right cut minimizes false alarms while keeping `coverage | fired`
   diagnostically low. State whether 0.10 is defensible or another cut dominates.
2. **Multimodality.** From recorded `d$multimodal`, report `coverage | multimodal`
   vs `coverage | not`. Answers judgment-call #2 empirically: keep the marker
   only if multimodal fits actually mis-cover.

### Precision / MC-error budget

Conditional coverage needs enough marker-fired fits. To pin a conditional
coverage to **±0.02** at p ≈ 0.9 needs ≈ 225 marker-fired reps per cell/estimate.
**Smoke-first (below) measures the fire rates**, then size `REPS` so the
*smallest* marker subset you care about clears ~200 firings. Report every
conditional coverage with a binomial MC interval; mark any estimate with < ~100
firings as **underpowered, not evidence of a null**.

### Reproducibility

Match the existing oracle's seed discipline: deterministic per-cell/replicate
offset off a fixed `BASE_SEED`, `set.seed()` locally per rep
(`m4-coverage-oracle.R:85`). Save per-fit records (markers + raw marker inputs +
per-parameter covered flags) to a `devel/*.rds`, and never clobber the B6
`m4-coverage-oracle-results.rds`.

### Smoke-first (do this before the full run)

Run a ~2-minute smoke (small REPS, the N band, the config ladder) whose **only
job** is to report per-marker fire rates by config and N. Use it to (a) confirm
markers fire often enough to estimate conditional coverage, (b) size `REPS` per
the budget above, and (c) decide whether an extra Heywood/ill-conditioning
config is needed. Report the smoke numbers before committing to the full run.

---

## Deliverable

`devel/cpm-marker-validation.md` — a memo containing:

1. The per-marker and any-marker conditional-coverage tables (with MC intervals),
   by parameter family, across the marker band.
2. The two sensitivity sweeps, each with an explicit recommendation:
   **keep / adjust / drop**, per marker and for the β cut.
3. A one-paragraph verdict Jeff can act on: *is the shipped marker set
   defensible as-is, and if not, what minimal change* (a constant, a dropped
   marker) *does the evidence support?* Frame the stakes honestly — the caution
   is advisory, over-inclusion costs one spurious line, so a slightly
   over-inclusive superset is the safe direction; recommend a change only if the
   evidence is clear.
4. **Honest nulls.** If a marker cannot be tested (too few firings) or shows no
   discrimination, say so plainly — that is itself the finding, and "keep the
   conservative default, defer to the post-M4 simulation paper" is an acceptable
   verdict.

Commit the runnable script as `devel/cpm-marker-validation.R` alongside the memo
(same committed-provenance discipline as `m4-coverage-oracle.R`), so the numbers
are reproducible.

## Out of scope

- Changing any constant, marker, or `summary()` wording — this brief **measures**;
  a later small Opus/Sonnet pass applies whatever Jeff decides (own tests + NEWS +
  `summary()` snapshot updates if a marker changes).
- The two ratified N thresholds (2000 / 50000) — settled.
- Any bootstrap-arm measurement, and any change to the estimator or its CIs.
- The full publication-grade factorial (that is the separate post-M4 CPM
  simulation paper; this run is a release-scoped subset that the paper subsumes).
