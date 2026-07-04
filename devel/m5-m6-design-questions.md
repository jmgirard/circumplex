# M5/M6 — hard statistical design questions (scoping memo)

**Status:** design questions + recommended directions only (Brief E of the
2026-07 Fable window). NOT a spec, NOT a task breakdown. These milestones sit
behind M4; several questions below cannot and should not be settled until the
Browne model (`cpm_fit`/`cpm_simulate`, devel/m4-browne-design.md) and
`ssm_ci_accuracy()` (devel/m4-ci-accuracy-spec.md) land — those are flagged
**[blocked on M4]** rather than force-answered.

Conventions binding everything here: angles degrees [0, 360) in the API with
LM = 360, radians internally; contrasts second minus first in (−180°, 180°]
via `angle_dist()`; displacement CIs via circular quantiles (center on
circular mean, unwrap, quantile, re-wrap); closed-form SSM estimator = OLS
only for equally spaced angles.

---

## M5 — SEM-based SSM

### Q5.1 — CIs for SSM parameters from a lavaan measurement model: delta method vs bootstrap, per parameter

**The setup** (devel/lavaan_ssm.Rmd): the scale–measure covariances/loadings
`s1..s8` are model parameters; the SSM parameters are lavaan defined
parameters (`:=`): `elev` (linear in the s), `xval`/`yval` (linear),
`ampl := sqrt(x² + y²)` (nonlinear, boundary at 0), `disp := atan2(y, x)`
(nonlinear, **circular**). lavaan's `:=` machinery gives delta-method SEs
automatically (numeric Jacobian of the defining expression against the
parameter vcov), which is what makes "just use lavaan's output" tempting.

**Does the delta method even apply?** Per parameter:

- **Elevation, x, y** — linear functions of model parameters, so the delta
  method is not an approximation at all here: the CI is exact given the
  (asymptotic-normal) vcov of the s. Delta is fine.
- **Amplitude** — differentiable everywhere except a = 0, but the boundary is
  exactly where the SSM guardrails live: near a ≈ 0 the sampling distribution
  of √(x²+y²) is folded/Rice-like (right-skewed, strictly positive, upward
  biased), and a symmetric Wald interval both misstates shape and can cross 0.
  Delta is acceptable only when a is clearly non-null — i.e., in exactly the
  regime where the package already says displacement is interpretable.
- **Displacement** — the delta method *locally* applies (atan2 is smooth away
  from the origin; this is the standard directional-statistics asymptotic:
  for concentrated (x, y), the angle is approximately normal — Mardia & Jupp,
  *Directional Statistics*), but with two failure modes lavaan cannot see:
  (i) the Jacobian entries scale as 1/a², so the SE explodes as a → 0, and
  (ii) lavaan's `:=` value and its Wald interval live on whatever branch
  atan2 returned — an estimate near 0°/360° gets an unwrapped, possibly
  sign-flipped interval, and lavaan's own *bootstrap percentile* CI on the
  `:=` quantity is worse still (naive quantiles of replicates straddling the
  cut produce the classic wrong-way interval). The delta method applies to d
  as a local approximation for concentrated estimates; the *interval
  construction and reporting* must be circular-aware, which no generic SEM
  machinery is.

**Recommended direction.** Do not delegate interval construction to lavaan
for the nonlinear parameters. Extract the fitted/bootstrap draws of
(e, x, y) from lavaan (delta vcov → MVN draws, or `se = "bootstrap"`
replicates of the free parameters) and push them through the package's
existing pipeline: `ssm_parameters`-style transform to (a, d), then
`ssm_replicate_intervals()` + `quantile.circumplex_radian()` for d. That is
exactly the Monte Carlo engine's architecture (asymptotic MVN on the
"scores", propagated through the SSM transform) with lavaan supplying the
mean and covariance instead of the empirical influence function — so the
tested circular machinery is reused, not re-derived. Report per-parameter:
delta/Wald acceptable as a *printed SE* for e (exact) and, with the guardrail
caveat, a; interval construction for a and d goes through the replicate
pipeline. Whether the cheap MVN-propagation route is accurate enough at
realistic n (vs re-fitting lavaan per bootstrap resample, which is far more
expensive) is an empirical coverage question — **[blocked on M4]**: the
`ssm_ci_accuracy()` simulation harness and `cpm_simulate()` plug-in
populations are the right tool to answer it, so don't pre-commit.

**A trap to write down now:** the `:=` weights in devel/lavaan_ssm.Rmd
hard-code the equal-spacing closed form (the 0.25·Σ cosine weights). Any
lavaan-syntax generator (the `circumplex_instrument` → syntax tool in the M5
scope) must either restrict to equally spaced instruments or emit the correct
non-orthogonal (OLS) weights — the closed-form-equals-OLS-only-for-equal-
spacing invariant applies inside the SEM exactly as outside it.

### Q5.2 — Multi-group invariance-constrained contrasts: what is the estimand, and how does it differ?

**Current contrast estimand:** difference (second minus first level) of SSM
parameters computed from each group's *observed* scores/correlations,
independently bootstrapped with group stratification. This confounds three
things: true structural difference, differential reliability (attenuation
differs by group), and measurement non-invariance (the scales may not mean
the same thing across groups).

**SEM multi-group estimand:** fit a multi-group measurement model with
invariance constraints (configural → metric → scalar, per the standard
Meredith 1993 / Vandenberg & Lance 2000 sequence), then define the contrast
on the *latent* (disattenuated) SSM parameters under those constraints. That
is a different estimand, not a better estimator of the same one:

- it is **disattenuated** — group differences in reliability no longer leak
  into e/a/d differences;
- it is **conditional on invariance** — if metric/scalar invariance fails,
  the constrained contrast is not "more principled", it is misspecified, and
  the honest output is "these groups cannot be compared on this scale", not
  a number;
- the displacement contrast must still be computed **in-package** as
  `angle_dist(d2, d1)` on the two groups' latent displacements, with the
  (−180°, 180°] branch handling and the circular-contrast CI machinery —
  a lavaan `:=` difference of two atan2's has the same branch-cut problem as
  Q5.1, squared.

**Recommended direction.** Ship this as a *separate, explicitly-named
workflow* (function + vignette section), not a replacement for the observed
contrast: document both estimands side by side ("observed contrast: do the
groups' measured profiles differ; latent contrast: do the groups' constructs
differ, assuming the instrument measures the same thing in both"). Require or
at least report the invariance-testing sequence before the contrast. CIs by
the same draws-through-the-transform route as Q5.1. Whether partial
invariance (freeing some intercepts/loadings) should be supported is an API
question to defer until the vignette is drafted against real data.

### Q5.3 — Scope boundary with M4 (note, not a question to answer here)

M5 should assume **fixed theoretical item/scale angles** (the measurement
model of devel/circum_lavaan.Rmd's `model0`, with known-angle loadings).
Freely *estimated* item angles under circumplex constraints is Browne's
model, which M4 implements natively — the abandoned free-loading sketch in
circum_lavaan.Rmd (`model1`, with its unfinished inequality constraints) is
exactly the territory lavaan handles poorly and `cpm_fit()` exists for.
Whether M5 later offers a hybrid ("estimate angles via cpm_fit, then SSM on
them") depends on M4's output object and is **[blocked on M4]** by
construction.

---

## M6 — Longitudinal & intraindividual SSM

### Q6.1 — Growth models on displacement without the 0°/360° boundary breaking it

A linear growth model on raw d fails at the cut: a trajectory drifting from
350° to 10° looks like a −340° plunge, person intercepts near the cut get
averaged to the antipode, and residuals are not even approximately normal
around the boundary. Three candidate framings:

1. **Unwrap-then-LMM.** Choose a reference branch per person (or per sample),
   unwrap the timepoint displacements to a continuous branch (cumulative
   `angle_dist` between adjacent timepoints), fit an ordinary mixed model.
   Cheap and transparent; breaks when true between-timepoint change
   approaches 180° (branch ambiguity) and when persons are heterogeneous in
   location so no common branch exists. Fine as a documented recipe, fragile
   as *the* method.
2. **Bivariate (x, y) growth model** — model the Cartesian SSM coordinates
   x(t) = a·cos d, y(t) = a·sin d jointly (bivariate LMM / latent growth
   model), then map the fitted mean trajectory back to (a(t), d(t)). No
   boundary anywhere; uses the coordinates the estimator already works in;
   elevation rides along as a third ordinary outcome. Two caveats to
   document, not fix: the derived d(t) is the direction of the *mean* (x, y)
   trajectory, not the mean of directions, and the derived a(t) is shrunk
   toward 0 wherever persons disperse in direction (mean resultant length
   ≤ mean amplitude) — the same aggregation fact SSM users already live with
   in group-level profiles.
3. **Proper circular regression** — projected normal regression (Presnell,
   Morrison & Littell 1998), i.e., framing 2 with a coherent distributional
   model; its Bayesian mixed-model incarnation exists (`bpnreg`, Cremers &
   Klugkist). Von Mises / wrapped-normal link models (Fisher & Lee 1992) are
   the alternative family but are notoriously awkward to fit and don't share
   the SSM's (x, y) geometry.

**Recommended direction:** framing 2 as the primary in-package direction —
it is boundary-free, reuses the package's native coordinates, and its
uncertainty propagates through the existing transform-the-draws machinery
(fitted-model draws of the (x, y) trajectory → a(t), d(t) draws → circular
quantiles). Projected-normal (bpnreg) is the principled model-based upgrade
and folds naturally into the Bayesian question (Q6.3). Whether d-trajectory
inference is *usable* at realistic panel sizes (few timepoints, moderate n,
modest amplitude) is a coverage/power question for the `ssm_ci_accuracy()`
harness generalized over time — **[blocked on M4]**.

### Q6.2 — Dependent/paired resampling for timepoint contrasts

The current bootstrap stratifies by group and resamples groups independently
— correct for between-person contrasts, wrong for timepoint contrasts, where
the sampling unit is the **person** and timepoints are dependent within
person. The correct scheme is the **case (cluster) bootstrap**: resample
persons with replacement; each drawn person contributes their *entire* row of
timepoint scores; recompute each timepoint's SSM parameters and the contrast
within every replicate. Within-person dependence is then preserved
nonparametrically — no model for the dependence is needed, which fits the
package's percentile-bootstrap philosophy. Downstream, nothing circular
changes: contrast displacement replicates go through
`quantile.circumplex_contrast_radian()` exactly as now.

Design consequences to note (questions, not tasks):

- `ssm_analyze()` needs a repeated-measures notion (an id variable / wide
  timepoint columns) so the resampling unit can be the person rather than
  the row — an API question, settle when M6 is scoped.
- Missing timepoints: does a person with a missing wave enter replicates
  (pairwise) or not (listwise)? The existing `listwise` semantics need a
  stated extension, not an accidental one.
- The **Monte Carlo engine has a clean analogue**: estimate the joint
  covariance of the stacked timepoint score vectors from the paired data
  (the empirical influence-function machinery already stacks measures
  jointly within group — stacking timepoints is the same move), draw MVN
  jointly, transform. Worth keeping the two engines in lockstep as now.

**Recommended direction:** person-level case bootstrap as the default paired
scheme; extend the existing stratified-`boot::boot` design (strata = group,
unit = person) rather than inventing a new resampler.

### Q6.3 — Bayesian estimation: worth it, and in-package or companion?

**Where Bayes actually earns its keep here:** (a) uncertainty propagates
through the nonlinear transform for free — a posterior over (e, x, y) *is* a
posterior over (a, d), no delta method, no interval-construction choices
beyond circular summaries; (b) hierarchical partial pooling for
**intraindividual SSM** — per-person parameters from short intensive
time series are exactly where flat per-person estimates are noisy/degenerate
and pooling helps; this is M6's own headline use case, not a bolt-on; (c)
principled behavior for near-degenerate profiles via priors. The
devel/bayesian_ssm.Rmd sketch (brms: `score ~ cos(rad) + sin(rad)`, then
transform the draws) already demonstrates the whole pattern — including its
line-114 `#TODO: Account for 360 boundary`, which is the same lesson as
everywhere else in this memo: posterior draws of d need **circular
summaries** (circular mean + interval after centering/unwrapping), and the
package's existing circular-quantile code applies to posterior draws exactly
as to bootstrap replicates.

Two statistical footnotes worth recording now: independent priors on (x, y)
induce a non-obvious prior on (a, d) — roughly Rayleigh-shaped on a, pushing
mass away from a = 0 — which should be *documented* as a modeling choice,
and posterior summaries of a inherit the usual positive skew, so report
medians/HDIs, not means±SD.

**In-package or companion?** The dependency policy (minimal Imports, no
heavy chains) decides this almost by itself: Stan via rstan/brms/cmdstanr is
a compile-time and maintenance burden far out of proportion to the package's
footprint, and ROADMAP already leans companion. The cheapest defensible
architecture:

- **In-package:** only a thin *draws adapter* — take a matrix of posterior
  draws of (e, x, y) (from brms, bpnreg, raw Stan, anything) and return SSM
  parameter draws + circular-aware summaries through the existing
  `ssm_replicate_intervals()`-style path. Near-zero dependency cost, and it
  makes every external Bayesian workflow inherit the package's boundary
  correctness.
- **Vignette:** the brms recipe (bayesian_ssm.Rmd, finished properly) using
  that adapter.
- **Companion package** (Stan models for hierarchical/projected-normal
  intraindividual SSM): only if the intraindividual milestone makes pooled
  per-person estimation central — decide *then*, not now.

**Recommended direction:** worth it for the hierarchical/intraindividual
case, not as a parallel engine for what the bootstrap already does well;
in-package footprint limited to the draws adapter + vignette; full Stan
machinery, if ever, in a companion package. Revisit once M6 is actually
scoped — no commitment needed before that.

---

## Summary of M4 dependencies (things deliberately not settled here)

| Question | What must land first |
|---|---|
| Q5.1 — is cheap MVN/delta propagation of lavaan estimates accurate enough vs refit-per-resample bootstrap | `ssm_ci_accuracy()` harness + `cpm_simulate()` populations |
| Q5.3 — hybrid estimated-angle + SSM workflows | `cpm_fit()` output object/API |
| Q6.1 — is displacement-trajectory inference usable at realistic panel n/T | ssm_ci_accuracy-style coverage simulation, generalized over time |

Everything else above (branch handling in SEM-defined parameters, the case
bootstrap, the draws adapter, the estimand distinction for invariance
contrasts) is durable regardless of M4's specifics.

*Next step when these milestones activate: turn each Q into a spec the way
Brief B did for `ssm_ci_accuracy()` — Fable tier for Q5.1/Q6.1 (estimator/CI
design where plausible-but-wrong is possible), Opus for the API/resampling
plumbing questions.*
