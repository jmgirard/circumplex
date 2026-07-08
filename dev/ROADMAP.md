# circumplex Roadmap

> **Forward direction across releases.** Drafted 2026-07-02 from a full
> audit of the package (v1.1.0.9000). M1 shipped as v1.2.0; M2+M3 are
> GitHub-complete; M4 was split on 2026-07-07 (Browne model + CI
> trustworthiness stay in M4, nearly done; structure tests moved to a
> new M4.5); M5–M6 remain planned. M2 through M5 accumulate into a
> single v2.0.0 release with M6 as its own later release (see the CRAN
> release strategy below): correctness first, then inference quality,
> then new capabilities in order of increasing scope.
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

**Current plan (Jeff, 2026-07-07 — supersedes the original tiered
submission train):** progress has been much faster than the original
plan budgeted (M4’s Browne model and CI-trustworthiness diagnostic were
built, validated, and reviewed in days, not weeks), so instead of
spacing three or four feature submissions we fold **M2 through M5 into
one v2.0.0 release**: the held v1.3.0 content (M2 + M3 — never submitted
separately), M4 (Browne model + CI trustworthiness), M4.5 (structure
tests), and M5 (SEM). Target: **~2026-08-02**, one month after the
v1.2.0 submission (CRAN-approved 2026-07-02) — comfortably inside CRAN’s
cadence expectations precisely because everything ships as one
submission. **Freeze rule (agreed 2026-07-07): code freeze ~2026-07-26**
— whatever milestones are GitHub-complete *and reviewed* by freeze get
on the train; anything not ready rides the next release. Scope, never
the date and never the statistics, is the variable: the likely outcome
is v2.0.0 = M2–M5, and a surprise in M5 degrades the release to M2–M4.5,
not the quality bar. **M6 (longitudinal) is deliberately excluded** and
becomes its own ~v2.1.0 on its own schedule: it is the largest and
least-designed milestone, its new statistical machinery
(paired/dependent circular resampling, growth models on displacement)
must not be compressed by a release date, and it explicitly benefits
from field feedback on the fit diagnostics and SEM layer it builds on —
a design brief precedes it (see devel/m5-m6-design-questions.md).
Milestones remain GitHub units of work: each completes, gets archived,
and accumulates on the branch/master until the v2.0.0 train leaves.

**v2.0.0 pre-release items** (release-gating, collected here because
they outlive individual milestones’ MILESTONES.md sections):

- **CircE published-oracle second re-read (from M4/B6).** The Grassi et
  al.
  2010. fixture values in `tests/testthat/helper-cpm-oracles.R` were
        transcribed via two automated channels but still need the §6.1
        protocol’s second independent *human* re-read against the paper
        (Jeff). Only a transcription typo is at risk. Fold into the
        pre-release review. Same status and treatment for the Zimmermann
        & Wright (2017) transcription from M4/W1
        (`devel/m4-zw-transcription.md`, feeding the “Evaluating
        Circumplex Structure” vignette and the O5 bridge) — two
        automated channels diffed and internally cross-validated, human
        re-read pending; the record flags one verified coincidence
        (15.5% appears for two distinct quantities) for explicit
        attention.
- **B6 analytic-CI caution — RATIFIED, CLOSED (2026-07-08)** (adopted
  default): the `cpm_boundary_markers()` marker set and the
  N-conditional [`summary()`](https://rdrr.io/r/base/summary.html)
  caution wording in R/cpm_fit.R / R/cpm_oop.R. Natural review point:
  when the M4 vignette (W1) documents CI trustworthiness. *W1 review
  outcome (2026-07-07, advisory): confirmed — the wording is hedged,
  directional, and names its markers; the vignette now teaches the same
  guidance (“prefer the bootstrap on the raw-data path”, caution below N
  = 2000 and marker-conditional above). Jeff’s veto window stays open
  until release.* *Ratification (Jeff, 2026-07-08): the two N thresholds
  (2000 / 50000) and the caution tone are confirmed. The one open piece
  is the marker set itself as a runtime predictor of mis-coverage — Jeff
  flagged he lacks the expertise to adjudicate the two calibration
  judgments (the β = 0.10 “small weight” cut and including `multimodal`,
  which the B6 oracle never measured). Resolution is empirical, not by
  judgment: a release-scoped, analytic-only marker- validation run
  (~10–20 min, no bootstrap) measures coverage conditional on each fired
  marker across the 2000–50000 band, plus β-cut and multimodality
  sensitivity sweeps. Spec: `devel/cpm-marker-validation-brief.md`
  (Fable brief G in `devel/fable-briefs-2026-07.md`). Subsumed later by
  the post-M4 CPM simulation paper; this is the release subset. Since
  the caution is advisory (over-inclusion costs one spurious line),
  “ship the conservative marker superset as-is” is a defensible fallback
  if the run isn’t done by freeze.* ***RATIFIED (Jeff, 2026-07-08): ship
  the marker set unchanged.*** The validation run (70k analytic-only
  fits, `devel/cpm-marker-validation.md`) confirmed both calibration
  judgments: β = 0.10 is the only swept cut that discriminates in the
  right direction (0.05 discriminates *backwards* — mis-coverage peaks
  near the boundary, not at it; 0.15 is dominated), and `multimodal`
  fits mis-cover ζ (.815 vs .933) with ~zero false alarms. No code,
  constant, or [`summary()`](https://rdrr.io/r/base/summary.html)
  wording change follows. **B6 item CLOSED.**
- **Cross-platform CI portability (release blocker; surfaced 2026-07-07
  by the M4.5 PR \#28). RESOLVED 2026-07-08 — master is green on all
  platforms.** All three classes fixed and merged: classes 2–3 via
  `skip_on_ci()` guards ([PR
  \#29](https://github.com/jmgirard/circumplex/pull/29), merged), and
  the class-1 `cpm_pack` β = 0 boundary via the start-value interior
  floor (`cpm_beta_start_interior()`, in PR \#29). The M5 merge ([PR
  \#30](https://github.com/jmgirard/circumplex/pull/30)) then surfaced
  and fixed three further portability issues the CI-blocked branch had
  hidden: the `ssm_sem*` pkgdown reference-index gap, a knife-edge
  \|ρ\*\| = 1 guard test, and non-byte-portable
  [`ssm_sem_syntax()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_syntax.md)
  emission of libm-noise near-zero cos/sin loadings (`snap_trig()`).
  Both PRs are green across the full matrix (macOS/Windows/ubuntu
  release+devel+oldrel, covr, pkgdown). The reproduction method that
  unblocked class 1 is worth keeping: a `rocker/r-ver` amd64 container
  switched to reference (netlib) BLAS reproduces the ubuntu-runner
  numerics the macOS dev machine cannot. Historical detail follows. Ten
  original failures in three classes, all in M4 code:
  1.  **estimator boundary — `cpm_pack: all(b_keep > 0)` errors**
      (`test-cpm_fit.R` exact-recovery/mirror/multimodal/free-angle
      tests): the CPM optimizer converges to a harmonic weight *exactly*
      on the β = 0 boundary on the **ubuntu runners only** (macOS and
      Windows pass), which `cpm_pack`’s softmax-inverse
      log-parameterization deliberately refuses (`R/cpm_fit.R:170`) — a
      real Linux-BLAS estimator robustness bug, **Fable-tier**, not
      reproducible on macOS (reproduce under a `rocker/r-ver` container
      / capture the values via a CI debug run); handoff brief in
      `devel/cpm-pack-boundary-brief.md`;
  2.  **seeded-bootstrap snapshots** (`test-cpm_api.R`,
      `test-ci_accuracy.R`) whose printed CI endpoints differ at the 3rd
      decimal by BLAS — need `skip_on_ci()`/`skip_on_cran()` (local-only
      regression pins) or numeric masking; (3) **vdiffr** plot snapshots
      (`test-cpm_plot.R`, `test-ci_accuracy.R`) — platform
      font/rendering, standard `skip_on_ci()`. This MUST be green before
      the v2.0.0 CRAN submission (CRAN is multi-platform). Classes 2–3
      are done; **class 1 (`cpm_pack`) is the remaining work — a
      dedicated Fable session**, with a Linux reproduction set up first
      (per the brief; not reproducible on the macOS dev machine).
      **Branch note (2026-07-08):** M5 development moved to its own
      `m5-sem-ssm` branch, stacked on this CI fix, so M5’s own CI
      inherits the class-1 red — fixing `cpm_pack` is on the critical
      path for **both** PR \#29 and M5’s green merge to master.
      (`ci-cross-platform` itself is parked at PR \#29’s scope; the
      class-1 fix belongs there.)
- **Release review depth:** `/code-review max` minimum; this is now the
  single flagship release, so it is *the* candidate for the billed
  `/code-review ultra` — but only if Jeff asks for it.
- v2.0.0 is a major-version bump carrying multiple feature families; run
  `/release-checklist` once, after the last milestone (or a deliberate
  descope) lands.

**Between releases (working practice, adopted 2026-07-07).** Real
version numbers, annotated tags, and GitHub Releases are bound to CRAN
submissions only — never mint a real version that CRAN won’t see. At
each milestone close instead: (1) archive the milestone to
MILESTONES-ARCHIVE.md; (2) bump the DESCRIPTION dev suffix (restart the
discipline at 2.0.0.9000 after the v2.0.0 release; one increment per
milestone) so `install_github` users’
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) identifies
the milestone-state they run; (3) add a lightweight git tag
(e.g. `m4-complete`) so milestone diffs are stable review scopes; (4)
run a **milestone-close `/code-review` over the milestone’s full
cumulative diff** — `high` for ordinary milestones, `max` for
statistically risky ones. The milestone-close review is the layer that
buys release-review depth: the CRAN-release review (`max`/ultra) then
verifies already-reviewed strata and the seams between them rather than
making a first deep pass over everything, and the freeze rule’s
“reviewed by freeze” means this review is done.

Note: a quick **patch** (e.g. v2.0.1) shortly after a release is
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

## Milestone 4 — Browne model & SSM CI trustworthiness

**Status: in progress on GitHub** (branch `m4-fit-statistics`; active
milestone in MILESTONES.md). **Rescoped 2026-07-07:** the original M4
also carried the Acton & Revelle structure tests; with the Browne/CI
work complete much faster than planned, the structure tests split off
into **M4.5** (below) so M4 can close as a coherent unit. Folds into the
v2.0.0 release (no longer its own CRAN slot — see the release strategy).

Scope and state:

- **Browne’s (1992) circular process model — native reimplementation
  (CircE replacement), complete.**
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  (raw data or cormat; variants A–D; analytic + bootstrap CIs; fit
  indices),
  [`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md),
  [`plot.circumplex_cpm()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_cpm.md)
  on the M3 extension, and the full validation battery (published
  CIRCUM/CircE oracles, OpenMx/lavaan cross-implementation oracles,
  simulation coverage oracle recorded in DESIGN.md). The anchor feature:
  CircE is archived on CRAN, so no other R package estimates this model.
- **SSM CI trustworthiness diagnostic (Zimmermann & Wright, 2017),
  complete.**
  [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  (spec `devel/m4-ci-accuracy-spec.md`): simulation-only plug-in
  coverage assessment at the user’s own n/engine/ settings, the
  amplitude-near-zero ladder with guardrail false-certification
  measurement, plain-language verdicts, plot method. Absorbed the M2 BCa
  follow-up (amplitude-near-zero percentile coverage) and the contrast
  branch-pathology observation.
- **Remaining:** the “Evaluating Circumplex Structure” vignette scoped
  to the above (CPM fitting, CI trustworthiness, Z&W transcription + the
  §10 O5 reproduction bridge) and ship-time documentation. Task detail:
  MILESTONES.md.

Known limitation recorded at the M4 review-#1 fix (2026-07-07,
pre-existing, not introduced by that fix): in `cpm_engine()`’s
fixed-angle branch (B/D), the deterministic zeta-jitter starts
(`sv$zeta * c(0.85, 1.1, 0.7)` clamped to \[0.05, 0.99\]) can collapse
to identical start vectors when `sv$zeta` sits at a clamp boundary, and
identical starts still count as separate independent reproductions in
the convergence-acceptance criterion — the same duplicate-start hazard
the review-#1 fix closed for the free-angle g0/mirror pair. Revisit if
B/D acceptance behavior is ever reworked (e.g., dedupe byte-identical
starts before grouping).

Post-M4 (agreed with Jeff, 2026-07-06): draft a publication-grade
simulation study design as a devel/ brief (Fable-tier design task)
extending the B6 coverage oracle — factorial over zeta
level/heterogeneity, p, m (mis)specification, and N; competitor
intervals (BCa at minimum, motivated by the observed one-sided
percentile under-coverage from the zeta boundary bias); MC error budget;
candidate venues Behavior Research Methods (CircE successor + simulation
core) or Assessment (CI-trustworthiness framing with the
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
work). The B6 script and Z1/Z2 machinery are the intended simulation
engine.

## Milestone 4.5 — Structure tests (Acton & Revelle, 2004)

**Status: GitHub-complete 2026-07-07** (split from M4 and promoted the
same day on M4’s close; folds into v2.0.0; archived to
MILESTONES-ARCHIVE.md with its full T1–T8 log). Revived and modernized
the drafts in `devel/fit_analysis.R` / `devel/fit_oop.R`.

A Fable method-review of those drafts (2026-07-03; full report in
`devel/fit-drafts-method-review.md`) found they are **mostly a rework,
not a revival**, and traced every formula/threshold to their uncited
source, Acton & Revelle (2004, *MPR-Online* 9(1)): Fisher test sound
(needs citation + scoring-keyed cutoffs); gap test has a 0°/360°
wrap-around omission (boundary bug) and nv-dependent cutoffs
anti-conservative at the canonical nv=8; the variance test implements
the *ineffective* variant and a mistranscribed threshold; the rotation
test has an indexing bug corrupting the statistic; the randomization
test isn’t actually implemented. Cross-cutting task: one simulation
under A&R’s generating model re-derives all cutoffs at nv=8. The `psych`
dependency is unnecessary (a small base-R principal-axis FA replaces the
one [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) call; psych →
Suggests as a test oracle) — **net new hard dependencies for the fit
statistics: zero**.

- Base-R principal-axis loadings + shared infrastructure (fix the
  ridge-on-wrong-matrix bug; psych → Suggests as oracle).
- Cutoff re-derivation simulation at nv=8 (fixes the Gap nv-dependence
  and VT/RT threshold provenance in one reproducible, committed run).
- Fisher test of equal axes; gap test of equal spacing (wrap-around
  fix); variance test (the *effective* VT2 variant) + rotation test
  (indexing and grid fixes); RANDALL correspondence index with an actual
  randomization inference.
- `ssm_fit()`-style user-facing API returning a typed object with
  `print`/`summary`/`plot` (plots on the M3 extension), consistent with
  `circumplex_ssm` conventions; pkgdown reference section.
- Extend the “Evaluating Circumplex Structure” vignette (written in M4
  for the CPM/CI-trustworthiness content) with the structure-test
  section.
- Future intention (out of the active task list, surfaced by T2’s
  review): calibrate structure-test cutoffs for non-octant scale counts
  (nv = 4 poles/quadrants, nv = 16 instruments) — the derivation script
  is nv-generic, and the package constant is already keyed by nv so
  uncalibrated counts fail the lookup rather than borrowing nv = 8
  cutoffs.
- Future intention (surfaced by T7’s scoping): a correlation-matrix
  input path for
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  (CircE-style, as `cpm_fit(cormat = )` offers), for users who have only
  a published correlation matrix. Data-only for now; the factor-analytic
  criteria and RANDALL need only the correlations, but deviation scoring
  needs the raw observations, so a cormat path would be raw-scoring
  only.
- Follow-ups deferred from the M4.5 close-review (`/code-review max`,
  2026-07-07; the 6 fix-now findings already landed — see the archived
  M4.5 log). For the v2.0.0 train: (#4) `structure_rt()`’s degeneracy
  guard is on a loadings⁴ scale, so it voids a valid but weak circumplex
  where VT/Fisher stay defined — make the guard
  scale-invariant/consistent across the four tests; (#5) missingness is
  silently pairwise-deleted before the complete-data cutoffs are
  applied, with no `listwise` control or warning — decide a missing-data
  policy; (#13)
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  at nv ≥ 10 without `n_perm` errors only after computing the four
  criteria, discarding them — validate up front or return the criteria
  with RANDALL marked unavailable. Test/quality hardening: assert the
  `almost < thrice < twice` cutoff ordering end-to-end (script
  `stopifnot` + a test over `structure_cutoffs`); a marginal-circumplex
  MC-p reproducibility test that isn’t pinned at the add-one floor; an
  exact-path `.Random.seed`-not-created test; the `data[scales]`
  matrix-misindex in the two internal helpers; and DRY the duplicated
  angle/communality geometry (summary/plot) and the double
  `cor(scored)`.

The detailed task list with acceptance criteria (T1–T8) and its full
running log are archived in MILESTONES-ARCHIVE.md — milestone-level
status only here, per this file’s contract.

Post-M4 (agreed with Jeff, 2026-07-06): draft a publication-grade
simulation study design as a devel/ brief (Fable-tier design task)
extending the B6 coverage oracle — factorial over zeta
level/heterogeneity, p, m (mis)specification, and N; competitor
intervals (BCa at minimum, motivated by the observed one-sided
percentile under-coverage from the zeta boundary bias); MC error budget;
candidate venues Behavior Research Methods (CircE successor + simulation
core) or Assessment (CI-trustworthiness framing with the
[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
work). The B6 script and Z1/Z2 machinery are the intended simulation
engine.

## Milestone 5 — SEM-based SSM

**Status: GitHub-complete 2026-07-08** (all T1–T5 tasks done and
close-reviewed; folds into v2.0.0). Full task list and log archived in
MILESTONES-ARCHIVE.md. Builds on the lavaan explorations in
`devel/lavaan_ssm.Rmd` and `devel/circum_lavaan.Rmd`; the statistical
design questions are scoped in `devel/m5-m6-design-questions.md` (Brief
E §M5). The detailed task list with acceptance criteria (T1–T5) is the
active list in MILESTONES.md.

- Latent-variable SSM: estimate SSM parameters from a lavaan measurement
  model (disattenuated correlations), with delta-method or bootstrap
  CIs.
- Multi-group SEM contrasts (invariance-constrained comparisons as a
  more principled alternative to bootstrap group contrasts).
- Tooling to generate lavaan syntax for circumplex measurement models
  from `circumplex_instrument` objects.
- `lavaan` moves to `Suggests`; features degrade gracefully without it.
- Vignette: “SEM-based SSM Analysis” (adapt `devel/lavaan_ssm.Rmd`).
- Follow-ups deferred from the M5 close-review (`/code-review max`,
  2026-07-08; the statistics were confirmed clean — two correctness
  angles returned empty after empirical validation — and the 9 fix-now
  findings landed same-day, see the M5 log). Post-v2.0.0, deliberately
  not pre-freeze because they churn validated code: (a) vectorize
  `sem_estimate()`’s per-draw
  [`apply()`](https://rdrr.io/r/base/apply.html) into one matrix pass
  (spec §9’s stated form) — floating point reorders, so every seeded pin
  must be re-verified in the same change; (b) `make_pop_2g()` in
  `devel/m5-coverage-oracle.R` should call `sem_pop()` (the one-copy
  truth-algebra discipline its own header claims), re-recording the
  affected two-group cells; (c) one shared contrast-arity validator for
  [`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)/[`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md); (d)
  a fit-this-syntax chokepoint owning the estimator/se/missing
  translation and `group.label` protection for the two
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) sites; (e)
  `summary.circumplex_ssm_sem()` delegating shared detail lines through
  a label seam instead of re-implementing them; (f) a package-wide
  scalar-count validation helper (`is_count(x, n = 1)`-style) adopted
  uniformly across the
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)/[`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)/[`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)
  families, which today interpret the CLAUDE.md `is_*()` rule two
  different ways;
  7.  minor: strict-tier syntax emission single-sourced across the
      single-/multi-group branches, test-fixture consolidation, the
      unused `npar` struct field, `sem_details()`’s always-overwritten
      `score_type`.

## Milestone 6 — Longitudinal & intraindividual SSM

**Status: planned.** The largest extension; benefits from Milestones 2–5
(fast estimation, the visualization layer, fit diagnostics, SEM
infrastructure). Deliberately NOT on the v2.0.0 train (decided
2026-07-07; see the CRAN release strategy): ships as its own ~v2.1.0
after a design brief, so its new statistical machinery —
paired/dependent circular resampling, growth models on displacement —
gets a full design window and the benefit of v2.0.0 field feedback.

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

- **Deferred `/code-review max` findings (2026-07-03, v1.3.0 bundle).**
  Non-blocking; the review found no wrong-number correctness bugs and
  the one guard worth acting on (C++ stride ↔︎ `ssm_param_names()`)
  shipped. Fold the rest in when the relevant code is next touched
  (mostly M4):

  - *Visualization extension robustness (fold into M4’s new plots):* the
    three degenerate-profile filters (`GeomSsmPoint` on the estimate,
    `StatSsmArc` on the CI bounds,
    [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)
    on `d_est`) use inconsistent NA criteria, so a profile with a
    defined estimate but an undefined CI renders a point with no wedge
    and no message — unify behind one plottability predicate.
    `StatSsmArc` also returns a structurally wrong 0-row frame when all
    rows are dropped. The now-exported
    [`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)
    needs documented/validated displacement input range (a `min>max`
    span silently draws the short-way arc). \[Closed in M4/B5,
    2026-07-06.\] Residual, package-wide: both
    [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)
    and
    [`plot.circumplex_cpm()`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_cpm.md)
    colour by a Set2 brewer palette (max 8 colours), so a fit/analysis
    with \>8 keyed levels warns and recycles/NA-fills — a single palette
    policy (hue fallback beyond 8, or a `palette=` hook like
    [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)’s)
    belongs here, not per-plot.
  - *Monte Carlo engine efficiency (fold into M4’s `ssm_ci_accuracy`
    work, which hammers the MC path):* the `psi` inner double loop
    recomputes squares per-element; per-profile `group_parameters()` +
    `do.call(cbind)` could be one batched C++ call; the MC correlation
    path re-introduces positional block indexing the M2 refactor retired
    elsewhere (name-drive it). \[Closed in M4/Z1, 2026-07-07: vectorized
    psi, one batched `group_parameters()` call, name-driven keys with an
    ambiguity guard; seeded output pinned byte-identical to the
    pre-refactor engine.\]
  - *[`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
    Phase-2 performance (design §8/§11 trigger fired 2026-07-07 at
    full-jz2017 scale):* clean seeded defaults take 111 s at the spec’s
    n≈300 cost-model scale (inside the ~5-min envelope) but 427 s serial
    at n=1166 (274 s at `ncpus = 4`). Profiling: ~75% of the loop is
    [`rmultinom()`](https://rdrr.io/r/stats/Multinom.html) RNG draws,
    and
    [`sample.int()`](https://rdrr.io/r/base/sample.html)+[`tabulate()`](https://rdrr.io/r/base/tabulate.html)
    benchmarked only ~10% faster — the loop is RNG-bound, so the
    anticipated “port the inner simulate-and-quantile loop to C++” will
    NOT clear the envelope while it consumes R’s RNG at one draw per
    resampled row. Any Phase-2 design must first decide the draw
    strategy (and its reproducibility contract) before porting; the R
    loop stays the permanent oracle either way. Not release-blocking:
    verdicts are correct, `parallel=` exists, and the envelope holds at
    the scale the spec budgeted.
  - *Minor cleanup (any release):* `ssm_replicate_intervals()` computes
    CI bounds in two `sapply(quantile)` passes (re-sorting each
    displacement column twice) — pass `probs = c(lo, hi)` once; drop the
    redundant `scores <- obs_scores` alias in both analysis paths;
    tighten the
    [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
    roxygen (an unnamed extra fills `angles` positionally and errors on
    `is.numeric`, not the documented named-args message); bump the
    testthat Suggests floor — DESCRIPTION declares `>= 3.0.0` but the
    suite has long used `expect_no_error()`/`expect_no_warning()`
    (testthat 3.1.5+), so the declared floor understates the real
    requirement (noticed in the B6 review, 2026-07-07).

- **CIRCUM-compatibility mode for
  [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)**
  (surfaced by M4/B6’s published- oracle triage, 2026-07-06):
  CIRCUM/CircE fit Browne’s *free-scaling* covariance structure
  `Σ = D_σ P(γ) D_σ`, so their fitted diagonal is not constrained to 1
  and their finite-sample estimates/χ² differ from our
  correlation-structure fit (same df, asymptotically equivalent; details
  in devel/m4-browne-design.md §11). A `free_scaling = TRUE` option
  would let users reproduce published CIRCUM/CircE output exactly; the
  OpenMx test oracle already demonstrates the parameterization. Decide
  post-M4 whether the reproduction value justifies a second fitted
  family.

- **Guardrail certification-rule replacement** (surfaced by M4/Z2’s
  false-certification measurements; `devel/m4-ci-accuracy-spec.md`
  §12.5/§13; Jeff, 2026-07-03). `print.circumplex_ssm()`’s
  displacement-certification rule (`round(a_lci, digits) > 0`, `digits`
  pinned to 3) is a display-precision artifact: its implied threshold
  moves with a print argument and means different things on a
  correlation-metric amplitude than a raw-score one (the mean-based path
  on raw-score metrics over-certifies). Decided as a two-phase package
  decision: ship the current rule now
  ([`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  assesses it as-is, digits echoed in output), design a principled,
  print-independent, scale-free replacement later, informed by the
  diagnostic’s own false-certification output (Z2 measured near-100%
  false certification at true zero amplitude on real data). Likely form:
  a relative rule (e.g., `a_lci` as a fraction of the amplitude CI
  width) rather than a fixed absolute cut — Z&W’s `a ≥ .15` “marked”
  threshold is correlation-metric only and answers a different question.
  Own tests, NEWS entry, and `print.circumplex_ssm()` snapshot updates
  when it lands; not an M4 deliverable.

- **0-vs-360 pole-snap alignment**
  (`devel/estimator-audit-2026-07-fable.md` F6, low/cosmetic, still
  parked). The point estimator emits exactly `360.0` for a profile
  peaking on the 0°/360° pole (the G2 decision; see DESIGN.md’s
  displacement-boundary entry), but `quantile.circumplex_radian()`
  (R/ssm_bootstrap.R) snaps a re-wrapped confidence interval endpoint
  within 2ε of 2π to `0` instead — the opposite pole label — so a
  pole-hugging profile can print `d_est = 360.0` with a CI endpoint of
  `0.0`. Both values name the same direction and every consumer already
  handles the wrap (`StatSsmArc` unwraps `d_uci < d_lci`; tests accept
  either pole label), so this is cosmetic, not a bug. Follow-up: pick
  one snap direction (360, matching the point estimator and the
  package’s LM=360 convention, is the natural choice) and align both
  call sites.

- **Milestone-close review deferrals (2026-07-07, M4).** The M4-close
  `/code-review max` over the cumulative diff found no release-blocking
  bug; most findings were fixed in the close commit (CFI/TLI
  degenerate-baseline guard, `cpm_gradient()` hot-path recompute dedup,
  suff-stats fallback environment forwarding, dead Hessian-singular
  branch,
  [`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md)
  draw-root factoring, `is_*()` scalar validation). Three were deferred:

  - *CPM convergence-acceptance vacuous “reproduced” (Fable-tier;
    `R/cpm_fit.R` ~592).* For free-angle variants (A/C, incl. the
    default “quasi-circumplex”) the multi-start acceptance check
    `reproduced = (>= 2 starts hit min F)` is satisfied by the theory
    start and its own mirror, whose F is identical by reflection
    isometry — so it passes off a single basin and a start-dependent
    *local* optimum can be reported `accepted` with no warning (the
    comment at ~496-498 documents this exact hazard for B/D but the
    mirror start reintroduces it for A/C; finder reproduced on ~8% of
    random p=8 matrices). Fix: count *distinct* basins toward
    reproduction (reuse the circular mirror detection already
    implemented for the multimodality flag), or require a non-mirror
    start to reproduce. Estimator-acceptance semantics — Fable.
  - *Contrast certification consistency (`R/ssm_ci_accuracy.R` ~508 /
    `R/ssm_oop.R` ~161).*
    [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
    certifies the contrast row as `cert[1] && cert[2]` (a deliberate Z1
    definition for conditioning the contrast’s conditional-displacement
    coverage) and emits a false-certification CAUTION for it, but
    `print.circumplex_ssm()` applies no certification rule to contrasts
    — so the “shipped guardrail rule, shared with
    `print.circumplex_ssm()`” comment is inaccurate for contrasts and
    the diagnostic reports the operating characteristics of a gate the
    package never displays for contrasts. Decide: gate contrasts in
    [`print()`](https://rdrr.io/r/base/print.html) too, or scope the
    contrast’s guardrail/false-cert framing to the conditional-coverage
    use only and correct the comment. Reporting semantics — Jeff’s call
    (Fable if the certification rule itself changes).
  - *Analytic-CI Hessian recomputation (`R/cpm_fit.R` ~619/843, minor
    perf).* On the analytic (cormat) path the Hessian at the solution is
    computed twice — `optimHess` for the condition-number diagnostic and
    `cpm_hessian_fd` for the SEs. Not fixed at close because the two are
    deliberately separate (Richardson-robust conditioning vs the FD
    Hessian pinned to 1e-8 by the delta-method SE test, which calls
    `cpm_analytic_se` with an engine that has no stored Hessian);
    unifying them shifts either the reported analytic SEs or the
    default-path ill-conditioning warning. Fold in only alongside a
    redesign of that SE-test contract.

Explicitly **not** planned: a ground-up rewrite. The R-dispatch → C++
core → `boot` architecture, the S3 class design, and the minimal
dependency policy all hold up; inefficiencies found in the audit are
local (see M2 vectorization and the items above).
