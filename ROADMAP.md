# circumplex Roadmap

> **Forward direction across releases.** Drafted 2026-07-02 from a full audit
> of the package (v1.1.0.9000). M1 shipped as v1.2.0; M2+M3 are
> GitHub-complete; M4 was split on 2026-07-07 (Browne model + CI
> trustworthiness stay in M4, nearly done; structure tests moved to a new
> M4.5); M5–M6 remain planned. Everything since v1.2.0 accumulates into a
> single v2.0.0 release (see the CRAN release strategy below): correctness
> first, then inference quality, then new capabilities in order of
> increasing scope.
>
> **This file carries direction and milestone-level status only.** Task-level
> status (checkboxes, acceptance criteria, per-task logs) lives in MILESTONES.md
> for the active milestone and MILESTONES-ARCHIVE.md for finished ones — never
> duplicate task checkboxes here. Per-submission version numbers are decided by
> the CRAN release strategy below, not by per-milestone tags.

## Guiding principles

1. **Correctness before capability.** Known bugs and silent-failure modes get
   fixed and regression-tested before any new features land.
2. **Every statistical routine gets an independent numerical cross-check.**
   New estimators are validated against a reference implementation (e.g., `lm()`
   for OLS-equivalent fits, published worked examples, or simulation recovery).
3. **Angles are the danger zone.** Any change touching displacement, contrasts,
   or the 0°/360° boundary requires tests at the boundary (profiles peaking at
   0°/360°, CIs straddling the boundary, contrasts near ±180°).
4. **One convention, stated everywhere.** Degrees in the user API ([0, 360),
   with LM at 360 by convention), radians internally, contrasts reported as
   *second minus first* level in (-180°, 180°].

**Design verdict from the audit:** the architecture (thin R dispatch →
C++ estimation core → `boot` resampling; S3 classes) is sound and does *not*
warrant a rewrite. Needed refactors are localized and listed in the
continuous track below; new capabilities should be added as new functions
rather than more arguments on `ssm_analyze()`.

---

## CRAN release strategy

Milestones are **GitHub** units of work; they are *not* one-to-one with CRAN
submissions. CRAN asks maintainers not to submit more than roughly once every
1–2 months and pushes back on churn, so **accumulate finished milestones on
GitHub and submit to CRAN only when there is a coherent user-facing story a
CRAN-only user needs** (they will not `install_github`). Decoupling the two
lets us keep shipping to GitHub continuously while spacing CRAN submissions.

**Current plan (Jeff, 2026-07-07 — supersedes the original tiered
submission train):** progress has been much faster than the original plan
budgeted (M4's Browne model and CI-trustworthiness diagnostic were built,
validated, and reviewed in days, not weeks), so instead of spacing three or
four feature submissions we fold **everything since v1.2.0 into one giant
v2.0.0 release**: the held v1.3.0 content (M2 + M3 — never submitted
separately), M4 (Browne model + CI trustworthiness), M4.5 (structure tests),
M5 (SEM), and M6 (longitudinal). Target: **~2026-08-02**, one month after the
v1.2.0 submission (CRAN-approved 2026-07-02) — comfortably inside CRAN's
cadence expectations precisely because everything ships as one submission.
Milestones remain GitHub units of work: each completes, gets archived, and
accumulates on the branch/master until the v2.0.0 train leaves.

**v2.0.0 pre-release items** (release-gating, collected here because they
outlive individual milestones' MILESTONES.md sections):

- **CircE published-oracle second re-read (from M4/B6).** The Grassi et al.
  (2010) fixture values in `tests/testthat/helper-cpm-oracles.R` were
  transcribed via two automated channels but still need the §6.1 protocol's
  second independent *human* re-read against the paper (Jeff). Only a
  transcription typo is at risk. Fold into the pre-release review.
- **B6 analytic-CI caution — Jeff to confirm/veto** (adopted default,
  reversible until release): the `cpm_boundary_markers()` marker set and the
  N-conditional `summary()` caution wording in R/cpm_fit.R / R/cpm_oop.R.
  Natural review point: when the M4 vignette (W1) documents CI
  trustworthiness.
- **Release review depth:** `/code-review max` minimum; this is now the
  single flagship release, so it is *the* candidate for the billed
  `/code-review ultra` — but only if Jeff asks for it.
- v2.0.0 is a major-version bump carrying multiple feature families; run
  `/release-checklist` once, after the last milestone (or a deliberate
  descope) lands.

Note: a quick **patch** (e.g. v2.0.1) shortly after a release is acceptable
to CRAN when it fixes a real bug — bug-fixes are the accepted exception to
the cadence rule. It is *feature* releases that must be spaced out.

---

## Milestone 1 — Correctness & robustness patch

**Status: shipped in v1.2.0** (CRAN-approved 2026-07-02). Fixes for the
2026-07 audit: six correctness bugs (`ssm_score()` `angles` forwarding,
`is_null_or_char()` length validation, NA-grouping crash, degenerate-profile
NA handling, `norm_standardize()` angle matching, contrast branch harmony near
±180°), guardrails (low-fit / zero-amplitude interpretation notes, `inherits()`
cleanup, matrix input, unused-`...` warnings), and documentation corrections.
Full task list, acceptance criteria, and log: **MILESTONES-ARCHIVE.md**.

---

## Milestone 2 — Inference quality

**Status: complete on GitHub** (bundled with M3 into the v1.3.0 CRAN
submission). Parallel bootstrapping (`parallel`/`ncpus` on `ssm_analyze()`),
a Monte Carlo alternative to the bootstrap, vectorized `ssm_score()`, and
seed/reproducibility documentation. **BCa CIs were dropped** — undefined for
circular displacement (bias-correction/acceleration are order-statistic
concepts needing a line, not a circle); the one real beneficiary (amplitude
coverage near zero) moved to M4's CI-trustworthiness diagnostic. Full task
list and rationale: **MILESTONES-ARCHIVE.md**.

## Milestone 3 — Visualization layer: ggplot2 circumplex extension

**Status: complete on GitHub** (the active milestone in MILESTONES.md until the
v1.3.0 submission ships; bundled with M2). Promoted the internal plotting code
to a public ggplot2 extension: exported `ggcircumplex()` canvas, polar-native
geoms (`geom_ssm_point()` / `geom_ssm_arc()`), `scale_x_circumplex()`, the three
`ssm_plot_*()` functions refactored onto it (behavior unchanged, vdiffr
snapshots byte-identical), an "Advanced Circumplex Visualization" vignette, and
a design review recorded in DESIGN.md. Deliberately sequenced *before* the
fit-statistics/SEM/longitudinal milestones, whose new visualizations build on
this layer. Full task detail, acceptance criteria, and log: **MILESTONES.md**.

## Milestone 4 — Browne model & SSM CI trustworthiness

**Status: in progress on GitHub** (branch `m4-fit-statistics`; active
milestone in MILESTONES.md). **Rescoped 2026-07-07:** the original M4 also
carried the Acton & Revelle structure tests; with the Browne/CI work complete
much faster than planned, the structure tests split off into **M4.5** (below)
so M4 can close as a coherent unit. Folds into the v2.0.0 release (no longer
its own CRAN slot — see the release strategy).

Scope and state:

- **Browne's (1992) circular process model — native reimplementation (CircE
  replacement), complete.** `cpm_fit()` (raw data or cormat; variants A–D;
  analytic + bootstrap CIs; fit indices), `cpm_simulate()`,
  `plot.circumplex_cpm()` on the M3 extension, and the full validation
  battery (published CIRCUM/CircE oracles, OpenMx/lavaan cross-implementation
  oracles, simulation coverage oracle recorded in DESIGN.md). The anchor
  feature: CircE is archived on CRAN, so no other R package estimates this
  model.
- **SSM CI trustworthiness diagnostic (Zimmermann & Wright, 2017),
  complete.** `ssm_ci_accuracy()` (spec `devel/m4-ci-accuracy-spec.md`):
  simulation-only plug-in coverage assessment at the user's own n/engine/
  settings, the amplitude-near-zero ladder with guardrail
  false-certification measurement, plain-language verdicts, plot method.
  Absorbed the M2 BCa follow-up (amplitude-near-zero percentile coverage)
  and the contrast branch-pathology observation.
- **Remaining:** the "Evaluating Circumplex Structure" vignette scoped to
  the above (CPM fitting, CI trustworthiness, Z&W transcription + the §10
  O5 reproduction bridge) and ship-time documentation. Task detail:
  MILESTONES.md.

Post-M4 (agreed with Jeff, 2026-07-06): draft a publication-grade simulation
study design as a devel/ brief (Fable-tier design task) extending the B6
coverage oracle — factorial over zeta level/heterogeneity, p, m
(mis)specification, and N; competitor intervals (BCa at minimum, motivated by
the observed one-sided percentile under-coverage from the zeta boundary
bias); MC error budget; candidate venues Behavior Research Methods (CircE
successor + simulation core) or Assessment (CI-trustworthiness framing with
the `ssm_ci_accuracy()` work). The B6 script and Z1/Z2 machinery are the
intended simulation engine.

## Milestone 4.5 — Structure tests (Acton & Revelle, 2004)

**Status: queued** (split from M4 on 2026-07-07; opens as the active
milestone when M4's vignette/ship tasks close; folds into v2.0.0). Revive and
modernize the drafts in `devel/fit_analysis.R` / `devel/fit_oop.R`.

A Fable method-review of those drafts (2026-07-03; full report in
`devel/fit-drafts-method-review.md`) found they are **mostly a rework, not a
revival**, and traced every formula/threshold to their uncited source, Acton &
Revelle (2004, *MPR-Online* 9(1)): Fisher test sound (needs citation +
scoring-keyed cutoffs); gap test has a 0°/360° wrap-around omission (boundary
bug) and nv-dependent cutoffs anti-conservative at the canonical nv=8; the
variance test implements the *ineffective* variant and a mistranscribed
threshold; the rotation test has an indexing bug corrupting the statistic; the
randomization test isn't actually implemented. Cross-cutting task: one
simulation under A&R's generating model re-derives all cutoffs at nv=8. The
`psych` dependency is unnecessary (a small base-R principal-axis FA replaces the
one `psych::fa()` call; psych → Suggests as a test oracle) — **net new hard
dependencies for the fit statistics: zero**.

- Base-R principal-axis loadings + shared infrastructure (fix the
  ridge-on-wrong-matrix bug; psych → Suggests as oracle).
- Cutoff re-derivation simulation at nv=8 (fixes the Gap nv-dependence and
  VT/RT threshold provenance in one reproducible, committed run).
- Fisher test of equal axes; gap test of equal spacing (wrap-around fix);
  variance test (the *effective* VT2 variant) + rotation test (indexing and
  grid fixes); RANDALL correspondence index with an actual randomization
  inference.
- `ssm_fit()`-style user-facing API returning a typed object with
  `print`/`summary`/`plot` (plots on the M3 extension), consistent with
  `circumplex_ssm` conventions; pkgdown reference section.
- Extend the "Evaluating Circumplex Structure" vignette (written in M4 for
  the CPM/CI-trustworthiness content) with the structure-test section.

The detailed task list with acceptance criteria (T1–T7 + the vignette
extension) is parked verbatim in MILESTONES.md under the queued-milestone
heading and becomes the active task list when M4.5 opens — milestone-level
status only here, per this file's contract.

Post-M4 (agreed with Jeff, 2026-07-06): draft a publication-grade simulation
study design as a devel/ brief (Fable-tier design task) extending the B6
coverage oracle — factorial over zeta level/heterogeneity, p, m
(mis)specification, and N; competitor intervals (BCa at minimum, motivated by
the observed one-sided percentile under-coverage from the zeta boundary
bias); MC error budget; candidate venues Behavior Research Methods (CircE
successor + simulation core) or Assessment (CI-trustworthiness framing with
the `ssm_ci_accuracy()` work). The B6 script and Z1/Z2 machinery are the
intended simulation engine.

## Milestone 5 — SEM-based SSM

**Status: planned.** Builds on the lavaan explorations in `devel/lavaan_ssm.Rmd`
and `devel/circum_lavaan.Rmd`.

- Latent-variable SSM: estimate SSM parameters from a lavaan measurement
      model (disattenuated correlations), with delta-method or bootstrap CIs.
- Multi-group SEM contrasts (invariance-constrained comparisons as a more
      principled alternative to bootstrap group contrasts).
- Tooling to generate lavaan syntax for circumplex measurement models from
      `circumplex_instrument` objects.
- `lavaan` moves to `Suggests`; features degrade gracefully without it.
- Vignette: "SEM-based SSM Analysis" (adapt `devel/lavaan_ssm.Rmd`).

## Milestone 6 — Longitudinal & intraindividual SSM

**Status: planned.** The largest extension; benefits from Milestones 2–5
(fast estimation, the visualization layer, fit diagnostics, SEM
infrastructure). The last milestone of the v2.0.0 train (the version number
now belongs to the combined release, not to M6 specifically — see the CRAN
release strategy).

- Repeated-measures SSM: parameter trajectories over time (growth models
      on e/a/d, with circular handling for d).
- Intraindividual SSM: per-person parameters from intensive longitudinal
      data (builds on vectorized `ssm_score()`), with multilevel summaries.
- Contrasts across timepoints (paired/dependent resampling — the current
      bootstrap assumes independent groups).
- Optional Bayesian estimation (revisit `devel/bayesian_ssm.Rmd`; likely
      a separate companion package if it drags in Stan).

## Continuous / infrastructure track (any release)

Targeted refactors — the 2026-07 audit's verdict is that these are worthwhile
but none block feature work; fold each into whichever milestone first touches
the relevant code:

- ~~**Named, long-format internal results assembly.**~~ **Done in M2** —
  positional column arithmetic (`d_vars <- 1:(ncol/6)*6 - 1`, fixed 6-parameter
  block) replaced with name-driven assembly via `ssm_param_names()`; done first
  so the interval work could build on it. (See MILESTONES-ARCHIVE.md.)
- **Deduplicate Group/Measure/Label construction** — the same block is built
  twice each in `ssm_analyze_means()`/`ssm_analyze_corrs()`; extract one
  helper. (Do with M1 or M2.)
- **Move degree/radian/contrast classes onto `vctrs`** (or S7) so arithmetic,
  printing, and quantile behavior are centralized and harder to misuse.
  (Nice-to-have; natural companion to M2.)
- **Rewrite the `devel/` fit drafts in current package style** (base R,
  no dplyr/rlang quasiquotation) when M4 begins — they predate the package's
  tidyverse-ectomy.
- Rename `tests/testthat/test-RcppExport.R.R` (double extension).
- Boundary-condition test suite: displacement at 0°/360°, CIs straddling the
  boundary, contrasts near ±180°, flat profiles, single-scale edge cases.
- Keep GitHub Actions workflows current; add R-devel to the check matrix.
- Track code coverage on the statistical core (`ssm_*`, `src/`) specifically.
- **Deferred `/code-review max` findings (2026-07-03, v1.3.0 bundle).** Non-blocking;
  the review found no wrong-number correctness bugs and the one guard worth acting
  on (C++ stride ↔ `ssm_param_names()`) shipped. Fold the rest in when the relevant
  code is next touched (mostly M4):
  - *Visualization extension robustness (fold into M4's new plots):* the three
    degenerate-profile filters (`GeomSsmPoint` on the estimate, `StatSsmArc` on the
    CI bounds, `ssm_plot_circle()` on `d_est`) use inconsistent NA criteria, so a
    profile with a defined estimate but an undefined CI renders a point with no
    wedge and no message — unify behind one plottability predicate. `StatSsmArc`
    also returns a structurally wrong 0-row frame when all rows are dropped. The
    now-exported `geom_ssm_arc()` needs documented/validated displacement input
    range (a `min>max` span silently draws the short-way arc).
    [Closed in M4/B5, 2026-07-06.] Residual, package-wide: both
    `ssm_plot_circle()` and `plot.circumplex_cpm()` colour by a Set2 brewer
    palette (max 8 colours), so a fit/analysis with >8 keyed levels warns and
    recycles/NA-fills — a single palette policy (hue fallback beyond 8, or a
    `palette=` hook like `ssm_plot_circle()`'s) belongs here, not per-plot.
  - *Monte Carlo engine efficiency (fold into M4's `ssm_ci_accuracy` work, which
    hammers the MC path):* the `psi` inner double loop recomputes squares
    per-element; per-profile `group_parameters()` + `do.call(cbind)` could be one
    batched C++ call; the MC correlation path re-introduces positional block
    indexing the M2 refactor retired elsewhere (name-drive it).
    [Closed in M4/Z1, 2026-07-07: vectorized psi, one batched
    `group_parameters()` call, name-driven keys with an ambiguity guard;
    seeded output pinned byte-identical to the pre-refactor engine.]
  - *`ssm_ci_accuracy()` Phase-2 performance (design §8/§11 trigger fired
    2026-07-07 at full-jz2017 scale):* clean seeded defaults take 111 s at the
    spec's n≈300 cost-model scale (inside the ~5-min envelope) but 427 s
    serial at n=1166 (274 s at `ncpus = 4`). Profiling: ~75% of the loop is
    `rmultinom()` RNG draws, and `sample.int()`+`tabulate()` benchmarked only
    ~10% faster — the loop is RNG-bound, so the anticipated "port the inner
    simulate-and-quantile loop to C++" will NOT clear the envelope while it
    consumes R's RNG at one draw per resampled row. Any Phase-2 design must
    first decide the draw strategy (and its reproducibility contract) before
    porting; the R loop stays the permanent oracle either way. Not
    release-blocking: verdicts are correct, `parallel=` exists, and the
    envelope holds at the scale the spec budgeted.
  - *Minor cleanup (any release):* `ssm_replicate_intervals()` computes CI bounds in
    two `sapply(quantile)` passes (re-sorting each displacement column twice) —
    pass `probs = c(lo, hi)` once; drop the redundant `scores <- obs_scores` alias
    in both analysis paths; tighten the `ssm_score()` roxygen (an unnamed extra
    fills `angles` positionally and errors on `is.numeric`, not the documented
    named-args message); bump the testthat Suggests floor — DESCRIPTION declares
    `>= 3.0.0` but the suite has long used `expect_no_error()`/`expect_no_warning()`
    (testthat 3.1.5+), so the declared floor understates the real requirement
    (noticed in the B6 review, 2026-07-07).
- **CIRCUM-compatibility mode for `cpm_fit()`** (surfaced by M4/B6's published-
  oracle triage, 2026-07-06): CIRCUM/CircE fit Browne's *free-scaling*
  covariance structure `Σ = D_σ P(γ) D_σ`, so their fitted diagonal is not
  constrained to 1 and their finite-sample estimates/χ² differ from our
  correlation-structure fit (same df, asymptotically equivalent; details in
  devel/m4-browne-design.md §11). A `free_scaling = TRUE` option would let
  users reproduce published CIRCUM/CircE output exactly; the OpenMx test
  oracle already demonstrates the parameterization. Decide post-M4 whether
  the reproduction value justifies a second fitted family.

Explicitly **not** planned: a ground-up rewrite. The R-dispatch → C++ core →
`boot` architecture, the S3 class design, and the minimal dependency policy
all hold up; inefficiencies found in the audit are local (see M2 vectorization
and the items above).
