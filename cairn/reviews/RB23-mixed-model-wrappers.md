# RB23: Wrapper functions that fit the growth model with glmmTMB or brms (no active milestone)

- **Date:** 2026-09-17
- **Output required:** write findings to `cairn/reviews/RR23-mixed-model-wrappers.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully self-contained. Do not assume any conversation context. Read only what this brief directs you to read. Answer the numbered questions. Write your findings to the output path above with the same numbering.

## Background

circumplex is a CRAN R package for circumplex data analysis. Its core is the Structural Summary Method (SSM). A profile of scores on scales placed around a circle is summarized by elevation e, amplitude a, displacement d, and a fit index. Displacement is an angle in degrees in [0, 360), with the 0/360 pole reported as 360. The estimator works in Cartesian coordinates, x = a cos d and y = a sin d, and recovers d with atan2(y, x). Angles wrap, so the package's statistical care concentrates at the 0/360 boundary.

The package supports longitudinal SSM through a reviewed design spec, `devel/longitudinal-ssm-spec.md`, made binding by D-013. Section 4 of that spec covers growth models of the SSM profile over time. Its decision (spec section 4.1) is that the package owns the coordinate transform and not the mixed-model fit. The package ships `ssm_parameters_id()` to build a person-by-occasion (e, x, y) coordinate table on the way in. It ships `ssm_draws()` to convert fitted-model draws of (e, x, y) back to (a, d) with circular-correct summaries on the way out. The user fits the growth model in a dedicated engine. The reference recipe in the growth vignette uses glmmTMB (D-016), with nlme named as the base-R alternative. The Bayesian vignette fits a cosine regression with brms and hands the posterior draws to `ssm_draws()`.

The spec gives two reasons for not wrapping the fit (spec section 4.1, lines 309 to 320). The first is the minimal-dependencies doctrine: no mixed-model engine in Imports (GP3 in `cairn/DESIGN.md`). The second is the spec's own words: a growth-fitting wrapper freezes one modeling framework into the API. The independent review of that spec (RR06, 2026-07-16) accepted the architecture and did not challenge the no-wrapper stance. This brief is therefore the second escalation of the adapter-not-engine holding. The maintainer asks you to weigh it fresh, with retirement of the mechanism listed among the options (Question 6).

The maintainer's prompt for this brief: fitting mixed models seems convenient. The question is whether thin wrapper functions that build and fit the reference growth model for the user are worthwhile. The engines in scope are glmmTMB, brms, or both. If they are worthwhile, the question is what shape they must take and what they must refuse.

Three facts make this hard.

1. The joint-fit requirement carries the statistics. RR06 R4 and spec section 4.1 require the mixed model to be fit jointly on (x, y) with correlated cross-outcome random effects. Two univariate fits set Cov(x̂(t), ŷ(t)) to zero and produce wrong d(t) intervals. The M27 coverage oracle, `devel/m27-coverage-oracle.R`, cell `xycor`, makes that shortcut fail coverage: pointwise d(t) coverage drops from 95% to roughly 86%. A wrapper that fits the correct joint model by construction removes a documented plausible-but-wrong path. A wrapper that lets users request the wrong model institutionalizes it.
2. The random-effects structure is a modeling choice, not a package invariant. The reference recipe uses correlated person random intercepts only (`us(0 + dv | person)`) and per-outcome residual variances (`dispformula = ~ 0 + dv`). It uses linear fixed trajectories in `wave`, and REML. Real analyses add random slopes, nonlinear time, covariates, time-varying predictors, or an autocorrelated residual. A wrapper that exposes these becomes a formula builder for an engine it does not own. A wrapper that fixes them covers only the vignette's case.
3. The package has precedent on both sides. lavaan is a Suggests-only runtime engine for the SEM-based SSM family (`ssm_sem()`). That entry point gates on `requireNamespace()` with an install-hint error (`cairn/DESIGN.md`, "Dependency policy"). glmmTMB and brms are in Suggests today for the vignettes only. GP4 makes any export a post-2.0 API commitment with a deprecation cycle. The ROADMAP already parks one related candidate on exactly the irreversible-export ground. It is an exported helper from coefficient draws to a trajectory table (`cairn/ROADMAP.md` line 24).

## Materials

Read in this order. Line numbers are as of commit `3a4687a0` on `master`.

1. `devel/longitudinal-ssm-spec.md` section 4 (lines 305 to 363): the reviewed growth decision. Section 5.1 (lines 401 to 415): the draws adapter's contract. Section 5.4 (lines 476 to 484): the Stan-companion stay-out criteria, the closest existing precedent for "do not wrap an engine".
2. `cairn/DECISIONS.md` D-013 (lines 311 to 345) and D-016 (lines 393 to 412): the binding-contract and engine-choice decisions, with their re-trigger clauses.
3. `cairn/DESIGN.md` "Design Principles" (lines 147 to 207, in particular IP1, IP3, GP2, GP3, GP4, GP7) and "Dependency policy" (lines 556 to 572): the lavaan-as-Suggests-engine precedent.
4. `vignettes/growth-ssm-analysis.Rmd`. Section 2 (lines 35 to 69): the division of labor as taught. Section 4 (lines 108 to 158): the joint glmmTMB fit and why. Section 5 (lines 159 to 256): MVN draws from the fixed-effect vcov into `ssm_draws()` per wave. Section 7 (lines 362 to 379): the REML small-sample caution. Section 9 (lines 428 to 453): caveats, with bpnreg named as the model-based upgrade and not wrapped.
5. `vignettes/bayesian-ssm-analysis.Rmd` section 4 (lines 110 to 164): the brms cosine regression with a person random intercept. Section 6 (lines 218 to 253): the induced prior on amplitude, a modeling consequence a wrapper must own or disclose.
6. `devel/m27-growth-recipe.R` (132 lines): the reference pipeline as code. `devel/m27-coverage-oracle.R` header (lines 1 to 60): the coverage cells, including the one that discriminates against the univariate shortcut.
7. `R/ssm_draws.R` lines 1 to 82: the roxygen contract of the output adapter. `R/ssm_parameters_id.R` lines 1 to 55: the input adapter.
8. `R/ssm_sem.R` lines 1431 to 1440: the `ssm_sem()` signature, the package's one existing Suggests-engine wrapper, for shape comparison. The `requireNamespace("lavaan")` gate at `R/axes_reliability.R:1147`.
9. `cairn/ROADMAP.md` line 24 (the parked draws-to-trajectory helper) and line 40 (longitudinal deferrals, D-013 lineage).
10. `DESCRIPTION` Imports and Suggests (lines 33 to 58).

To run the reference recipe (requires glmmTMB):

```
Rscript devel/m27-growth-recipe.R
```

## Questions

1. Is a wrapper worthwhile at all? Weigh the convenience gain against the spec section 4.1 reasons (dependency doctrine, freezing a framework into the API) and GP4's export commitment. State what class of user benefits. State whether the growth vignette's current teaching, where a reader copies a six-line glmmTMB call, leaves a real gap or an imagined one. Is there a statistical-correctness argument for a wrapper, that it removes the univariate-shortcut hazard by construction, strong enough to outweigh the API cost under IP1?
2. If worthwhile, which engine or engines? The options are four. The first is glmmTMB only (frequentist, CRAN-hosted, no toolchain). The second is brms only (Bayesian, needs Stan, cannot run in CRAN checks, which is why the Bayesian vignette ships precomputed draws under D-015). The third is both engines behind one function with an `engine` argument. The fourth is both engines as separate functions. For each option, state the check and CI consequence (what a CRAN builder or a user without the engine sees) and the API cost.
3. What must the wrapper fix, and what can it expose? Enumerate the modeling choices in the reference recipe: joint stacked outcome, correlated random intercepts, per-outcome residual variance, linear `wave`, REML, MVN propagation from the fixed-effect vcov. For each, say whether a wrapper must hard-code it, expose it as an argument, or accept a user-supplied formula fragment. Say what the wrapper must refuse under GP2 (refuse, do not coerce). In particular, must it refuse any specification that breaks the joint structure, for example a random-effects term that drops the cross-outcome correlation? Can that be checked mechanically from a glmmTMB or brms fit object after the fact?
4. Return object and downstream contract. Must the wrapper return the engine's fit object, a circumplex object that wraps it, or the per-wave `ssm_draws()` trajectory table directly? The engine's fit object lets users call `fixef`, `vcov`, `anova`, and diagnostics. Consider three facts. A trajectory table is what `ssm_plot_trajectory()` consumes. The ROADMAP parks a draws-to-trajectory helper on irreversible-export grounds. The per-wave certification in `ssm_draws()` (`details$certified`) must survive whatever shape is chosen. Does IP3 (two independent oracles per shipped numeric result) bind a wrapper that only delegates to an engine? If so, what is the second oracle?
5. Uncertainty propagation as a hidden default. The recipe's MVN draw from the REML fixed-effect vcov ignores variance-component uncertainty and is anticonservative at small N (vignette section 7). A wrapper makes that step silent. Four designs are possible. (a) Refuse to propagate and return only the fit. (b) Propagate with a documented caution. (c) Offer a parametric-bootstrap alternative. (d) Require the user to choose. Which must a wrapper adopt? Which of these is consistent with GP2 and with the existing `method = "bootstrap" | "mc"` vocabulary in `ssm_analyze()`?
6. Retirement option (second escalation). The adapter-not-engine holding is on its second review. Weigh three options: keep as is, add wrappers, and retire the growth recipe from the package. Retirement has three parts. Drop glmmTMB from Suggests. Reduce the growth vignette to the coordinate transform plus a pointer to external modeling. Leave `ssm_draws()` as the only growth-facing export. State which of the three you recommend, with one paragraph on each.
7. Reopening evidence (GP7). Whatever you recommend, name the class of evidence that reopens the decision, so the maintainer can record it with the decision. Examples are N users filing issues that ask for a fitter, a published misuse traceable to the univariate shortcut, or glmmTMB or brms API instability.

## Constraints

These are fixed. Flag disagreement explicitly rather than working around it.

- D-013: the RR06-reviewed spec is the binding contract. Its holding (3), that growth recipes must fit (x, y) jointly, is not up for relitigation. If a wrapper ships, it must fit jointly by construction.
- D-016: glmmTMB is the reference engine and lives in Suggests only. The package never Imports a mixed-model engine. A wrapper must gate on `requireNamespace()` with an install-hint error, exactly as `ssm_sem()` gates on lavaan (`cairn/DESIGN.md`, "Dependency policy").
- D-015: brms is Suggests-only and never loaded by package code, tests, or vignette build. Any brms-facing wrapper must be testable without Stan, for example by testing the formula, prior, and data it constructs and not the fit.
- GP3 and GP4 (`cairn/DESIGN.md`): minimal dependencies. Exported signatures are post-2.0 commitments and change only with a deprecation cycle.
- IP1, IP2, and IP6: any exported numeric path carries boundary tests at profiles peaking at 0°/360°, intervals straddling the pole, and low-amplitude (uncertified) waves.
- Dependency changes are never unilateral. Moving an engine from Suggests to Imports is out of scope for any recommendation.
- The package uses base R plus its existing Imports. No tidyverse in package code. The user API is standard evaluation (character names or numeric indices), per D-014.

## Output format

In `RR23-mixed-model-wrappers.md`, answer each question by number with your reasoning and evidence. List any additional findings separately under "Beyond the brief". End with concrete recommendations, each marked apply, consider, or reject-with-reason. Your report is advisory: this brief's header slot does not say `requested`, so emit no `## Binding criteria` section. End with a one-paragraph verdict. The maintainer will quote it verbatim in the recorded decision.
