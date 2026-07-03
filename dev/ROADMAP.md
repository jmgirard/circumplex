# circumplex Roadmap

> **Forward direction across releases.** Drafted 2026-07-02 from a full
> audit of the package (v1.1.0.9000). M1–M3 are done (M1 shipped v1.2.0;
> M2+M3 bundled for v1.3.0); M4–M6 remain planned and their
> sequencing/scope are open to revision. Each milestone is a releasable
> unit: correctness first, then inference quality, then new capabilities
> in order of increasing scope.
>
> **This file carries direction and milestone-level status only.**
> Task-level status (checkboxes, acceptance criteria, per-task logs)
> lives in MILESTONES.md for the active milestone and
> MILESTONES-ARCHIVE.md for finished ones — never duplicate task
> checkboxes here. Per-submission version numbers are decided by the
> CRAN release strategy below, not by per-milestone tags.

## Guiding principles

1.  **Correctness before capability.** Known bugs and silent-failure
    modes get fixed and regression-tested before any new features land.
2.  **Every statistical routine gets an independent numerical
    cross-check.** New estimators are validated against a reference
    implementation (e.g., [`lm()`](https://rdrr.io/r/stats/lm.html) for
    OLS-equivalent fits, published worked examples, or simulation
    recovery).
3.  **Angles are the danger zone.** Any change touching displacement,
    contrasts, or the 0°/360° boundary requires tests at the boundary
    (profiles peaking at 0°/360°, CIs straddling the boundary, contrasts
    near ±180°).
4.  **One convention, stated everywhere.** Degrees in the user API (\[0,
    360), with LM at 360 by convention), radians internally, contrasts
    reported as *second minus first* level in (-180°, 180°\].

**Design verdict from the audit:** the architecture (thin R dispatch →
C++ estimation core → `boot` resampling; S3 classes) is sound and does
*not* warrant a rewrite. Needed refactors are localized and listed in
the continuous track below; new capabilities should be added as new
functions rather than more arguments on
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md).

------------------------------------------------------------------------

## CRAN release strategy

Milestones are **GitHub** units of work; they are *not* one-to-one with
CRAN submissions. CRAN asks maintainers not to submit more than roughly
once every 1–2 months and pushes back on churn, so **accumulate finished
milestones on GitHub and submit to CRAN only when there is a coherent
user-facing story a CRAN-only user needs** (they will not
`install_github`). Decoupling the two lets us keep shipping to GitHub
continuously while spacing CRAN submissions.

**Triage of the roadmap into CRAN submissions:**

- **Tier 1 — submit on its own, promptly. v1.2.0 (M1).** Correctness
  fixes (silently-wrong results) justify interrupting cadence; ship
  alone, do not wait to bundle features.
- **Tier 2 — flagship, its own slot. M4** (fit statistics + Browne’s
  model / CircE replacement). Highest new-user value and strongest
  standalone story: CircE is archived on CRAN, so no R package currently
  estimates Browne’s model. Also the riskiest statistically, so it
  benefits from not sharing a release.
- **Tier 3 — bundle, don’t spend a slot each. M2 + M3 → one release
  (~v1.3.0).** M3 (ggplot2 extension) is mostly infrastructure whose
  payoff is realized by later milestones — weak as a solo CRAN
  submission; M2 (inference quality) is incremental. Together they make
  a substantial “faster, more flexible, composable plots + new
  visualization vignette” release. Caveat: both touch fragile internals
  (do the named-column results-assembly refactor before M2), so the
  bundle has a larger check/review surface — budget a `/code-review max`
  pass accordingly (the deepest local review; reserve the billed cloud
  `/code-review ultra` for a flagship release like M4).
- **Tier 4 — naturally their own releases; cadence is moot. M5** (SEM)
  and **M6** (longitudinal, v2.0.0) are large and far enough out that no
  bundling decision is needed now.
- **Never a CRAN submission on its own:** the continuous/infra track
  below (refactors, test renames, CI upkeep, coverage). Ships to GitHub
  whenever convenient, folded into whichever milestone touches that
  code.

**Suggested submission train:** (1) now — v1.2.0 (M1); (2) next slot —
v1.3.0 (M2 + M3 bundled) once the viz extension is stable; (3) headline
slot — M4 (CircE replacement); (4) M5, then M6/v2.0.0 as they land.

Note: a quick **patch** (e.g. v1.2.1) shortly after a release is
acceptable to CRAN when it fixes a real bug — bug-fixes are the accepted
exception to the cadence rule. It is *feature* releases that must be
spaced out.

------------------------------------------------------------------------

## Milestone 1 — Correctness & robustness patch

**Status: shipped in v1.2.0** (CRAN-approved 2026-07-02). Fixes for the
2026-07 audit: six correctness bugs
([`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
`angles` forwarding, `is_null_or_char()` length validation, NA-grouping
crash, degenerate-profile NA handling,
[`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md)
angle matching, contrast branch harmony near ±180°), guardrails (low-fit
/ zero-amplitude interpretation notes,
[`inherits()`](https://rdrr.io/r/base/class.html) cleanup, matrix input,
unused-`...` warnings), and documentation corrections. Full task list,
acceptance criteria, and log: **MILESTONES-ARCHIVE.md**.

------------------------------------------------------------------------

## Milestone 2 — Inference quality

**Status: complete on GitHub** (bundled with M3 into the v1.3.0 CRAN
submission). Parallel bootstrapping (`parallel`/`ncpus` on
[`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)),
a Monte Carlo alternative to the bootstrap, vectorized
[`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md),
and seed/reproducibility documentation. **BCa CIs were dropped** —
undefined for circular displacement (bias-correction/acceleration are
order-statistic concepts needing a line, not a circle); the one real
beneficiary (amplitude coverage near zero) moved to M4’s
CI-trustworthiness diagnostic. Full task list and rationale:
**MILESTONES-ARCHIVE.md**.

## Milestone 3 — Visualization layer: ggplot2 circumplex extension

**Status: complete on GitHub** (the active milestone in MILESTONES.md
until the v1.3.0 submission ships; bundled with M2). Promoted the
internal plotting code to a public ggplot2 extension: exported
[`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
canvas, polar-native geoms
([`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md)
/
[`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)),
[`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md),
the three `ssm_plot_*()` functions refactored onto it (behavior
unchanged, vdiffr snapshots byte-identical), an “Advanced Circumplex
Visualization” vignette, and a design review recorded in DESIGN.md.
Deliberately sequenced *before* the fit-statistics/SEM/longitudinal
milestones, whose new visualizations build on this layer. Full task
detail, acceptance criteria, and log: **MILESTONES.md**.

## Milestone 4 — Circumplex fit & structure statistics

**Status: planned** (flagship; its own CRAN slot — see the release
strategy). Revive and modernize the drafts in `devel/fit_analysis.R` /
`devel/fit_oop.R` (currently written in superseded tidyverse-heavy style
with a `psych` dependency decision pending).

- Fisher test of equal axes (draft exists).
- Gap test of equal spacing (draft exists).
- Variance test of equal communalities / interstitiality indices.
- **Browne’s (1992) stochastic process model — native reimplementation
  (CircE replacement).** CircE (Grassi, Luccio, & Di Blas, 2010) is
  archived on CRAN, leaving R users without an estimator for Browne’s
  model. Implement estimation of the circular stochastic process model
  (free/constrained item angles, communality index, Fourier correlation
  function; point estimates, CIs, fit indices such as RMSEA/CFI from the
  discrepancy function). Decide backend: native optimization vs. OpenMx/
  lavaan. Validate against published CircE/CIRCUM output. This is the
  anchor feature of the milestone.
- **SSM CI trustworthiness diagnostic (Zimmermann & Wright, 2017).**
  Z&W’s simulation studies show bootstrap SSM CI accuracy depends on
  sample size and the population circumplex structure; they used Browne-
  model estimates to characterize that structure. Reimplement as a
  user-facing diagnostic: fit Browne’s model to the user’s data (item-
  or scale-level), then either (a) simulate from the fitted model to
  estimate empirical CI coverage for the user’s n, or (b) map the
  estimated parameters onto the accuracy results of Z&W Studies 1–5.
  *Spec from the paper + supplemental materials before implementation;
  depends on the CircE replacement above.* Surface as something like
  `ssm_ci_accuracy(ssm_object)` with a plain-language verdict in
  [`summary()`](https://rdrr.io/r/base/summary.html). *Absorbed from the
  dropped M2 BCa task (2026-07-02): the diagnostic should specifically
  assess percentile-CI coverage for amplitude near zero (nonnegative,
  upward-biased, skewed — the one SSM parameter where percentile
  intervals are theoretically weakest), since the amplitude CI drives
  the “displacement not interpretable” guardrail.*
- `ssm_fit()`-style API returning a typed object with `print`/`summary`/
  `plot` methods, consistent with `circumplex_ssm` (plots built on the
  M3 extension).
- New vignette: “Evaluating Circumplex Structure” (fit statistics, CI
  trustworthiness, when to trust SSM parameters, ipsatization guidance).

## Milestone 5 — SEM-based SSM

**Status: planned.** Builds on the lavaan explorations in
`devel/lavaan_ssm.Rmd` and `devel/circum_lavaan.Rmd`.

- Latent-variable SSM: estimate SSM parameters from a lavaan measurement
  model (disattenuated correlations), with delta-method or bootstrap
  CIs.
- Multi-group SEM contrasts (invariance-constrained comparisons as a
  more principled alternative to bootstrap group contrasts).
- Tooling to generate lavaan syntax for circumplex measurement models
  from `circumplex_instrument` objects.
- `lavaan` moves to `Suggests`; features degrade gracefully without it.
- Vignette: “SEM-based SSM Analysis” (adapt `devel/lavaan_ssm.Rmd`).

## Milestone 6 — Longitudinal & intraindividual SSM

**Status: planned** (v2.0.0-scale). The largest extension; benefits from
Milestones 2–5 (fast estimation, the visualization layer, fit
diagnostics, SEM infrastructure).

- Repeated-measures SSM: parameter trajectories over time (growth models
  on e/a/d, with circular handling for d).
- Intraindividual SSM: per-person parameters from intensive longitudinal
  data (builds on vectorized
  [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)),
  with multilevel summaries.
- Contrasts across timepoints (paired/dependent resampling — the current
  bootstrap assumes independent groups).
- Optional Bayesian estimation (revisit `devel/bayesian_ssm.Rmd`; likely
  a separate companion package if it drags in Stan).

## Continuous / infrastructure track (any release)

Targeted refactors — the 2026-07 audit’s verdict is that these are
worthwhile but none block feature work; fold each into whichever
milestone first touches the relevant code:

- ~~**Named, long-format internal results assembly.**~~ **Done in M2** —
  positional column arithmetic (`d_vars <- 1:(ncol/6)*6 - 1`, fixed
  6-parameter block) replaced with name-driven assembly via
  `ssm_param_names()`; done first so the interval work could build on
  it. (See MILESTONES-ARCHIVE.md.)
- **Deduplicate Group/Measure/Label construction** — the same block is
  built twice each in `ssm_analyze_means()`/`ssm_analyze_corrs()`;
  extract one helper. (Do with M1 or M2.)
- **Move degree/radian/contrast classes onto `vctrs`** (or S7) so
  arithmetic, printing, and quantile behavior are centralized and harder
  to misuse. (Nice-to-have; natural companion to M2.)
- **Rewrite the `devel/` fit drafts in current package style** (base R,
  no dplyr/rlang quasiquotation) when M4 begins — they predate the
  package’s tidyverse-ectomy.
- Rename `tests/testthat/test-RcppExport.R.R` (double extension).
- Boundary-condition test suite: displacement at 0°/360°, CIs straddling
  the boundary, contrasts near ±180°, flat profiles, single-scale edge
  cases.
- Keep GitHub Actions workflows current; add R-devel to the check
  matrix.
- Track code coverage on the statistical core (`ssm_*`, `src/`)
  specifically.

Explicitly **not** planned: a ground-up rewrite. The R-dispatch → C++
core → `boot` architecture, the S3 class design, and the minimal
dependency policy all hold up; inefficiencies found in the audit are
local (see M2 vectorization and the items above).
