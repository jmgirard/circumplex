# Active milestone

## M3 — Visualization layer: ggplot2 circumplex extension (v1.4.0)

Source: ROADMAP.md Milestone 3. Turn the internal, single-purpose plotting
code into a public ggplot2 extension so users (and later milestones) can
compose arbitrary layers in circumplex space instead of rebuilding the
circular canvas from scratch. Sequenced before the fit-statistics/SEM
milestones, whose visualizations should build on it.

Scope decision (2026-07-02, with Jeff): **full extension** as ROADMAP specifies
— exported canvas constructor, custom ggproto geoms/stats, and scale helpers —
not the lighter "public canvas + ggforce" alternative. Rationale: maximal
composability for the M4+ visualizations that will depend on this layer.

Per ROADMAP.md's CRAN release strategy, M3 is bundled with the (already
GitHub-complete) M2 into a single v1.3.0 CRAN submission. Keep both on GitHub
until M3 is done, then run `/release-checklist` once.

Cross-cutting guardrails for every task below:
- **Behavior of the three public `ssm_plot_*()` functions must not change**
  until the explicit refactor task (V4); their vdiffr snapshots in
  `tests/testthat/_snaps/ssm_plot/` are the regression pins — any snapshot
  delta must be justified as an intended rendering change, not accepted blindly.
- **Dependency policy** (DESIGN.md): new user-facing API is base R + ggplot2;
  keep `ggforce` only where it genuinely simplifies arcs/circles. No tidyverse.
- Everything exported gets roxygen with a runnable `@examples` block and enters
  `_pkgdown`/reference cleanly (`devtools::document()` no-diff after).

### Tasks

- [x] **V1. Public circular canvas.** Promote `circle_base()`
  (`R/ssm_plot.R:469`) to an exported, documented API: a `ggcircumplex()`
  constructor and/or `annotation_circumplex()` (rings, spokes, scale labels,
  amplitude gridlines), with instrument-aware labeling from
  `circumplex_instrument` objects.
  *Accept:* exported + documented; a call reproducing the current
  `circle_base(angles, amax, labels)` output is vdiffr-identical to a
  snapshot of today's canvas (or the delta is justified); instrument input
  auto-labels angles from the instrument's scales; invalid input errors via
  the `is_*()` helpers.
- [ ] **V2. Polar-native geoms/stats (ggproto).** `geom_ssm_point()` /
  `geom_ssm_arc()` (or a unifying `stat_ssm()`) that accept
  amplitude/displacement aesthetics directly and internalize the
  degree→canvas transform (`ggrad()`), amplitude rescaling
  (`* 10/(2*amax)`), and wrap-around arc handling now inline in
  `ssm_plot_circle()` (`R/ssm_plot.R:75-84`).
  *Accept:* a plot built from `ggcircumplex() + geom_ssm_*()` on an
  `ssm_analyze()` result is vdiffr-equivalent to the corresponding
  `ssm_plot_circle()` output (same arcs, points, wrap-around at the 0/360
  boundary); boundary case — a profile arc spanning the 0/360 seam renders as
  one contiguous arc; degenerate/NA-displacement rows are dropped or handled
  without error.
- [ ] **V3. Scales.** `scale_*_circumplex()` helpers for angle-labeled axes
  and amplitude gridlines, with defaults matching the current appearance.
  *Accept:* helpers produce the current tick/label placement on both the
  circular canvas and the curve plot's angle axis; custom `angle_labels`
  and instrument labels flow through; documented with examples.
- [ ] **V4. Refactor existing plots onto the extension.** Reimplement
  `ssm_plot_circle()`, `ssm_plot_curve()`, `ssm_plot_contrast()` on top of
  V1–V3 with **behavior unchanged**.
  *Accept:* every existing vdiffr snapshot in
  `tests/testthat/_snaps/ssm_plot/` stays byte-identical, or each change is
  individually justified and re-approved; `chkDots()`/argument surfaces of the
  three functions are preserved; full suite green.
- [ ] **V5. Vignette: "Advanced Circumplex Visualization."** The third
  vignette, already announced as "still in progress" in the intermediate
  vignette (`vignettes/intermediate-ssm-analysis.Rmd:276`). Demonstrate
  composing raw data, SSM results, and annotations via the new extension.
  *Accept:* builds clean; uses only exported API; teaching prose meets the
  statistical-precision bar (CLAUDE.md — e.g., never describe an angular CI
  excluding 0° as a significance test); intermediate vignette's "in progress"
  note updated to point at it.
- [ ] **V6. Design review vs. ggplot2 extension best practices.** Check
  ggproto lifecycle, `after_stat()`/`after_scale()` usage, theme integration,
  and the `ggforce` dependency decision (keep iff it simplifies arcs).
  *Accept:* a short written verdict appended to DESIGN.md (a "Visualization
  extension" subsection) recording the ggproto/scale architecture and the
  ggforce keep/drop decision with rationale.

## Log

- 2026-07-02 — V1 Public circular canvas (Opus): exported `ggcircumplex()`,
  a documented ggplot2 canvas constructor, as a thin public wrapper over the
  existing internal `circle_base()` (left untouched, so all 11 existing
  ssm_plot vdiffr snapshots are structurally unable to regress — verified: git
  shows only the two NEW ggcircumplex snapshots added, no existing snap
  modified). Signature `ggcircumplex(angles, labels, amin, amax, font_size,
  instrument)`; when an `instrument` is supplied it derives angles from
  `Scales$Angle` and defaults labels to `Scales$Abbrev` (validated:
  ggplot_build data of the instrument path == the explicit angles+labels path,
  device-independent; LM=360 scale labels correctly). Deferred
  `annotation_circumplex()` to V2 where the ggproto layer machinery is built
  (a half-baked annotation now would be worse than focused). Dropped a
  would-be single-member `@family`; used `@seealso ssm_plot_circle()` instead.
  Validation via is_* helpers (labels length, is_instrument, scalar numerics).
  Test-first (failed on missing function); one self-inflicted test bug fixed
  mid-task (duplicate vdiffr snapshot name → replaced the second render with a
  ggplot_build data-equality assertion). Suite 509/509; check 0/0/0. NEWS.md
  added. (R/ssm_plot.R, man/ggcircumplex.Rd, NAMESPACE,
  tests/testthat/test-ssm_plot.R, tests/testthat/_snaps/ssm_plot/*.svg [new],
  NEWS.md, MILESTONES.md).

# Completed milestones

## M2 — Inference quality (v1.3.0) — GitHub-complete 2026-07-02 (bundled into v1.3.0 with M3)

Source: ROADMAP.md Milestone 2. Upgrades to the existing bootstrap machinery;
no new statistical scope. Per ROADMAP.md's CRAN release strategy, this
milestone is bundled with M3 (ggplot2 extension) into a single v1.3.0 CRAN
submission — keep both milestones' work on GitHub until both are done, then
run `/release-checklist` once.

### Tasks

- [x] **Parallel bootstrapping** via `boot`'s built-in `parallel`/`ncpus`
  arguments, exposed through `ssm_analyze()`.
- ~~**BCa confidence intervals**~~ **DROPPED 2026-07-02** — see log entry and
  ROADMAP.md; amplitude-coverage question folded into M4's CI-trustworthiness
  diagnostic.
- [x] **Monte Carlo alternative to bootstrapping**: sample SSM parameters from
  the asymptotic sampling distribution of the mean vector / correlation
  vector (multivariate normal with estimated covariance), propagate through
  the parameter transformation. Validate against bootstrap results on
  `jz2017`.
- [x] **Vectorize `ssm_score()`** (currently row-wise `apply` + `rbind` of
  data frames): elevation/x/y are single matrix products; amplitude,
  displacement, and fit follow element-wise.
- [x] Seed/reproducibility documentation for all resampling paths.
- [x] **Continuous-track item to do first**: named, long-format internal
  results assembly (`ssm_bootstrap()` identifies displacement columns by
  positional arithmetic `d_vars <- 1:(ncol/6)*6 - 1`; `reshape_params()`
  assumes a fixed 6-parameter block). Replace with named columns / one-row-
  per-parameter internal format before starting the interval work above,
  which touches exactly this code (per ROADMAP.md continuous track).

### Log

- 2026-07-02 — Continuous-track refactor (Opus): replaced positional parameter
  arithmetic with name-driven assembly. New single source of truth
  `ssm_param_names()` (canonical C++ order e/x/y/a/d/fit); `reshape_params()`
  derives block width + names from it; `ssm_bootstrap()` names replicate columns
  `<param>_<group>` and locates displacement via `param_of_col == "d"` (dropping
  `1:(ncol/6)*6-1` and `contrast_d_vars <- ncol-1`). Behavior-preserving:
  name-based selection is column-identical to the old arithmetic in both
  contrast and non-contrast paths; all seeded bootstrap pins byte-identical.
  Chose the named-columns route (not a full one-row-per-parameter melt) — lower
  churn, and the interval work only needs to locate columns by name. Test-first:
  added contract pins for `ssm_param_names()`/`reshape_params()` + a 3-group
  non-contrast case exercising multi-block name selection. Suite 432/432; check
  0/0/0. No NEWS.md (internal only). Unblocks BCa / Monte Carlo / parallel.
  (R/utils.R, R/ssm_bootstrap.R, tests/testthat/test-ssm_bootstrap.R,
  MILESTONES.md).
- 2026-07-02 — Vectorize `ssm_score()` (Sonnet): replaced row-wise
  `apply(FUN = ssm_parameters) + do.call(rbind, ...)` (per-row data frame
  construction and rbind, O(n) R-level overhead) with a single call to the
  existing `group_parameters()` C++ routine (already used by
  `ssm_bootstrap()`) plus one `matrix()` reshape keyed off
  `ssm_param_names()`. Deliberately reused the already-tested compiled
  degenerate-profile/tolerance logic (B4) rather than re-deriving it in R —
  duplicating that tolerance math was the likelier place for a boundary bug,
  per CLAUDE.md's correctness bar. Bit-for-bit identical to the pre-refactor
  output on `aw2009` (max abs diff 0); ~68x faster at n=5000 (0.65s -> 0.0096s).
  Two behavior changes made deliberately: (1) degenerate-row warnings
  consolidated from one-per-row to a single "`n` of `total`" warning,
  matching the precedent set by `ssm_bootstrap()`'s resample warning; (2)
  found and fixed a regression introduced by the first draft of this refactor
  — using `modifyList()` for label/prefix/suffix `...` forwarding silently
  swallowed unrecognized argument names, where the old `apply(FUN =
  ssm_parameters, ...)` used to error "unused argument" on a typo (verified
  against pre-refactor code); added an explicit unknown-name check to restore
  the error, with a regression test. Test-first: added coverage for label/
  prefix forwarding (previously untested), the consolidated warning, and the
  unused-argument error. Suite 441/441; check 0/0/0. No NEWS.md (internal
  perf only, no API change). (R/ssm_analysis.R, tests/testthat/test-ssm_analysis.R,
  MILESTONES.md). [Correction, same day: a NEWS.md bullet WAS added during the
  parallel-bootstrapping task — the consolidated warning is user-visible and
  the speedup worth announcing; "internal only" was the wrong call.]
- 2026-07-02 — Parallel bootstrapping (Fable): `ssm_analyze()` gains
  `parallel`/`ncpus` (validated via match.arg + boots-style stopifnot),
  threaded explicitly through ssm_analyze_means/corrs -> ssm_bootstrap ->
  boot::boot(). Key statistical fact, verified against the installed boot
  source AND empirically: for this nonparametric bootstrap, boot draws the
  full resample index array in the master process before dispatch and our
  statistic is deterministic, so seeded results are BYTE-IDENTICAL for any
  parallel/ncpus setting (tested: snow + multicore vs serial on the grouped
  contrast path and the correlation path; master .Random.seed state identical
  after serial vs parallel; B4 degenerate-resample warning + results identical
  under PSOCK workers). Docs state the reproducibility guarantee on @param
  parallel; multicore documented as ignored on Windows (boot silently falls
  back to serial). Defaults unchanged -> all seeded pins intact. Suite
  447/447; check 0/0/0. NEWS.md bullet added (plus the missing ssm_score
  bullet, see correction above). (R/ssm_analysis.R, R/ssm_bootstrap.R,
  man/ssm_analyze.Rd, tests/testthat/test-ssm_analysis.R, NEWS.md,
  MILESTONES.md).
- 2026-07-02 — BCa task DROPPED (decision: Jeff, on Fable analysis; no code).
  Deciding fact: BCa is undefined for circular displacement — z0 =
  qnorm(P(t* < t0)) and the jackknife acceleration are order-statistic
  concepts requiring a linear scale; on a circle "below" depends on an
  arbitrary branch cut. So any BCa option is necessarily mixed-method
  per-parameter (BCa e/x/y/a, percentile d) forever. Costs judged not worth
  it: per-parameter method labels on every CI surface (print/summary/
  ssm_table/plots/vignettes) and in users' methods sections; against field
  convention (Z&W 2017 percentile); boot.ci incompatible with our circular
  quantiles and B4 NA-filtered degenerate resamples (hand-rolled BCa =
  classic plausible-but-wrong trap); opt-in-only feature doubling the CI test
  surface. Steelman acknowledged: amplitude (nonnegative, upward-biased,
  skewed near zero, drives the G1 guardrail) is the one real beneficiary —
  that question moved to M4's ssm_ci_accuracy diagnostic (ROADMAP.md updated
  both ends). Monte Carlo task remains as the independent cross-check on
  percentile CIs. (MILESTONES.md, ROADMAP.md).
- 2026-07-02 — Monte Carlo engine (Fable): `ssm_analyze(method = "montecarlo")`.
  Design decisions, each validated: (1) EMPIRICAL influence-function covariance
  for correlations (psi_i = z_x z_y − (r/2)(z_x²+z_y²), acov = crossprod(psi)/n²)
  instead of normal-theory Pearson–Filon — on non-normal simulated data psi
  tracks direct simulation at max err .022 (n·acov units) where PF errs .39
  (17x worse); matters because jz2017 measures are skewed counts. Verified psi
  ≡ PF on MVN data (.003) and var(r) ≡ (1−ρ²)². (2) JOINT draws across measures
  within group (they share the sample): measure-contrast e-CI width — bootstrap
  .0741, MC joint .0759, independent draws would be .098 (32% too wide; the
  main trap of this task). (3) Fisher-z sampling for correlations (delta-method
  cov, tanh back-transform) — keeps draws in (−1,1). (4) PSD-safe eigen-based
  MVN sampler (ipsatized/singular covariance tested). (5) Scope: MC + missing
  data requires listwise (informative error); n_g ≥ 2 required; |r| = 1 errors.
  Reuse: extracted ssm_replicate_intervals() from ssm_bootstrap() (behavior-
  preserving; label param keeps bootstrap warning byte-identical) so MC shares
  the validated circular-quantile/branch/degenerate machinery; propagation via
  vectorized group_parameters(). t0 computed identically to boot's ⇒ point
  estimates byte-equal. Validation: MC ≡ bootstrap CIs on jz2017 (means, corr
  + measure contrast, group contrast; all endpoints within 15% of interval
  width, encoded as tests); MC e/x CIs ≡ closed-form analytic normal-theory
  CIs (<2% of width); rotation equivariance; 0/360 straddle; contrast branch
  harmony at ±180 (est inside CI); flat data → NA + count warning via shared
  machinery. details$method recorded; summary() label conditional ("Monte
  Carlo Draws"), old objects default to bootstrap label. Suite 499/499; check
  0/0/0; seeded pins untouched (default engine unchanged). NEWS.md added.
  (R/ssm_montecarlo.R [new], R/ssm_bootstrap.R, R/ssm_analysis.R, R/ssm_oop.R,
  man/ssm_analyze.Rd, tests/testthat/test-ssm_montecarlo.R [new], NEWS.md,
  MILESTONES.md).
- 2026-07-02 — Seed/reproducibility documentation (Sonnet, doc-only). Added a
  DESIGN.md "Reproducibility" section: a per-engine table (serial bootstrap,
  parallel bootstrap, Monte Carlo) of what a fixed seed guarantees and exactly
  what RNG each consumes (index-array-then-dispatch for bootstrap; one
  rnorm() block per group, groups jointly across measures, in group_ids order
  for Monte Carlo — traced from R/ssm_montecarlo.R, not assumed), plus an
  explicit "what this does NOT mean" list (no cross-engine agreement from a
  shared seed; no stability across `boots`; ordinary cross-R-version caveat).
  Refreshed DESIGN.md's data-flow diagram, stale since the continuous-track
  refactor and Monte Carlo addition (now shows both engines and the shared
  ssm_replicate_intervals() assembly). Fixed a second stale line found in the
  same table (BCa listed as "planned" — dropped last task). Added a matching
  `@section Reproducibility` to `?ssm_analyze` (was previously scattered
  across the `parallel`/`method` @param entries only). Vignette: the
  "randomness inherent to bootstrapping" sentence in the introduction
  vignette was actually imprecise (implied the PANO()/octants() shortcuts
  caused the CI difference between `results`/`results2`; they return
  identical values to the manual vectors — the real cause is both calls
  sharing one un-reseeded RNG stream from the vignette's single top-level
  set.seed()) — corrected per CLAUDE.md's vignette-precision bar, with a
  cross-reference to the new roxygen section. No code changes; doc-only, no
  NEWS.md bullet. Suite 499/499 (unchanged); vignette re-rendered clean;
  check 0/0/0. M2 COMPLETE (all tasks checked or explicitly dropped with
  rationale). (DESIGN.md, R/ssm_analysis.R, man/ssm_analyze.Rd,
  vignettes/introduction-to-ssm-analysis.Rmd, MILESTONES.md).

## M1 — Correctness & robustness patch (v1.2.0) — released 2026-07-02, CRAN-approved

Source: ROADMAP.md Milestone 1 (2026-07 audit). Every bug fix landed with a
regression test that failed on the pre-fix code.

### Bugs

- [x] **B1. `ssm_score()` forwards `angles`** — `R/ssm_analysis.R:517`
  passes `...` to `apply()` but never `angles`.
  *Accept:* `ssm_score(aw2009, scales = PANO(), angles = rotated)` differs
  from octant results and matches row-wise `ssm_parameters(x, rotated)`;
  4-scale case with `poles()` works; existing tests still pass.
- [x] **B2. `is_null_or_char()` honors `n`** — `R/utils.R:146` passes
  `n = NULL`.
  *Accept:* `ssm_analyze(..., measures = c("A","B"), measures_labels = "one label")`
  errors informatively; NULL still accepted; audit other call sites
  (`caption`, `angle_labels`) for behavior changes.
- [x] **B3. NA grouping values handled** — NA in `grouping` with
  `listwise = FALSE` crashes in `mean_scores()` (`unique(): detected NaN`).
  *Accept:* NA-group rows dropped with a `message()` reporting the count, in
  both deletion modes; results match manually pre-filtered data.
- [x] **B4. Degenerate profiles return NA + warning** — zero-variance scores
  give `Fit = -Inf` and noise displacement (`src/parameters.cpp`).
  *Accept:* flat profile returns NA displacement/fit with one warning;
  near-zero amplitude documented behavior decided and tested; bootstrap
  containing some degenerate replicates doesn't error.
- [x] **B5. `norm_standardize()` robust matching** — exact float equality on
  `Angle` vs norms table (`R/tidying_functions.R:181-186`).
  *Accept:* 0° vs 360° convention mismatch either works or errors with a
  message naming the expected angles; duplicate-angle norms error clearly.
- [x] **B6. Contrast displacement branch harmony at ±180°** — point estimate
  in (-180°, 180°] can disagree with CI branch from circular centering.
  *Accept:* simulated contrast near ±180° has estimate inside its CI;
  test added at the boundary.

### Guardrails & UX

- [x] **G1.** `print`/`summary.circumplex_ssm` note when fit < .70 or the
  amplitude CI includes 0 (displacement not interpretable).
- [x] **G2.** Document displacement boundary convention (0° prints as 360°),
  or normalize; decide once, record in DESIGN.md.
- [x] **G3.** `inherits()` instead of `class(x) ==` everywhere; fix or drop
  matrix input support in `ssm_analyze()`/`ssm_score()`. Also (found during
  B1 review): `ssm_score()` validates `is.character(scales)` and so rejects
  numeric column indexes, contradicting both its own roxygen ("variable names
  or column numbers") and `ssm_analyze()`'s `is_var()` validation — align on
  `is_var()`.
- [x] **G4.** Consider warning on unused `...` in plot functions.

### Docs

- [x] **D1.** Purge `ssm_plot()` references (intermediate vignette line ~271,
  introduction vignette line ~409).
- [x] **D2.** Fix `angle_lables` typo in `ssm_plot_curve()` example.
  (Done with G4, since the typo would otherwise trip the new warning.)
- [x] **D3.** `instruments()` count: says 14, lists 15.
- [x] **D4.** Document contrast direction in `?ssm_analyze`.
- [x] **D5.** Document equal-spacing assumption of the closed-form estimator.
- [x] **D6.** Intro vignette: fix "displacement significantly different from
  zero" phrasing.
- [x] **D7.** Delete stale `CRAN-SUBMISSION` file. (`.Rbuildignore` already
  updated for the md files and `.claude` — done 2026-07-02.)
- [x] **D8.** NEWS.md cleanup: remove the duplicated `# circumplex 1.1.0`
  heading (lines 3/5); skim the rest for similar artifacts.

### Release

- [x] R CMD check clean on CI matrix; NEWS.md updated per user-facing change;
  version to 1.2.0; `/release-checklist`.

### Final log

- 2026-07-02 — Milestone opened from audit. Scaffolding added (CLAUDE.md,
  DESIGN.md, ROADMAP.md, MILESTONES.md, skills), `.Rbuildignore` updated.
- 2026-07-02 — ROADMAP revised: added CI-trustworthiness diagnostic (Z&W 2017
  via CircE replacement, now M4), inserted ggplot2 extension as M3 (before
  fit stats so later milestones plot through it), renumbered M4-M6, added
  refactor verdict + targeted refactor list to continuous track.
- 2026-07-02 — B1: `ssm_score()` now forwards `angles` to `ssm_parameters()`;
  regression tests incl. 0°/360°-peak boundary; validated vs OLS at ~1e-13;
  check clean 0/0/0 (R/ssm_analysis.R, tests/testthat/test-ssm_analysis.R,
  NEWS.md). Review found pre-existing `scales` validation inconsistency →
  noted in G3. NB: dev env had lost ggforce/htmlTable + stale .so; reinstalled
  and rebuilt via clean_dll().
- 2026-07-02 — B2: `is_null_or_char()` now forwards `n`; call-site audit
  (measures_labels, angle_labels, caption) confirmed all tightenings only
  reject previously-wrong inputs; check clean (R/utils.R, tests, NEWS.md).
  Collateral fix: seeded the five unseeded vdiffr blocks in test-ssm_plot.R
  and regenerated 11 snapshots — they had depended on RNG state leaking from
  earlier test files, so ANY upstream test that consumes RNG broke them
  (diagnosed when B2's bootstrap tests did exactly that; only arc coordinates
  changed, rendering verified unchanged via the seeded cross-zero snapshot).
  NB: test-ssm_plot.R is stored with CRLF line endings (repo outlier) —
  preserved; normalize deliberately someday if desired.
- 2026-07-02 — B3 (Opus): NA `grouping` rows now dropped in the `ssm_analyze()`
  dispatcher (on the user's real grouping column, once) with a count message +
  empty-data guard; fixes the pairwise `unique(): detected NaN` crash. No src/
  change needed — the R-layer guard keeps NaN out of Armadillo. Review moved
  the drop from a per-subfunction helper up to the dispatcher, which also
  fixed a would-be column-name collision (a scale named "Group" vs the renamed
  grouping column). Regression tests cover both modes, contrast, the collision
  (expect_no_message), and the all-NA clean error. Check clean 0/0/0
  (R/ssm_analysis.R, R/utils.R, tests, NEWS.md).
- 2026-07-02 — B4 (Fable): degenerate-profile handling. C++ detects flat
  (sd ≤ 8·ε·n·max|s| — cannot test var==0 exactly; constant 0.1 gives ~2e-34)
  → NA disp/fit, and zero-amplitude-with-variance (pure higher harmonic) →
  NA disp, fit exactly 0. C++ silent; R warns once for observed profiles and
  once with a count for degenerate bootstrap resamples (quantiles now na.rm;
  CIs conditional on estimability, disclosed). Decision: NO threshold beyond
  machine noise — small real amplitudes keep point estimates (validated to
  1e-9 amplitude); their uncertainty is the CI's/G1's job. Validation: 15/15
  incl. NA-excluded CI == independent boot+filter reference (1e-10, 16/300
  degenerate). Seeded pins unchanged. Documented in roxygen + DESIGN.md
  (src/parameters.cpp, R/ssm_analysis.R, R/ssm_bootstrap.R, tests, NEWS.md).
- 2026-07-02 — B5 (Opus): `norm_standardize()` matches scale→norm row by
  circular angular distance (`pmin(|Δ| %% 360, 360 - ...) < 1e-6`) instead of
  exact `==`, so 0≡360 just works; clear errors for zero matches (names
  available angles) and >1 match (duplicate-angle norms). Fixes cryptic
  "replacement has length zero". Seeded values unchanged; check 0/0/0; review
  clean. Note: still uses `class(instrument) ==` — G3 scope.
  (R/tidying_functions.R, man/norm_standardize.Rd, tests, NEWS.md).
- 2026-07-02 — B6 (Fable): contrast displacement CI now reported on the
  estimate's branch. Defect: near ±180° the angle_dist estimate and the
  circular-mean-centered CI could land on opposite branches (est +179.4 vs CI
  (−196.6, −159.0) at data seed 70 — reproduced through the real pipeline
  after a seed search; flip probability ~10% per boundary dataset, hence
  intermittent). Fix: shift both CI endpoints by 2πk, k = round((est −
  mid)/2π), in ssm_bootstrap before degree conversion — identity (k=0) away
  from the boundary (all seeded pins byte-identical), width/contiguity
  preserved, cannot fabricate coverage (|est − mid| ≤ π ⇒ k=0 for wide CIs).
  Validation: pkg CI == independent reimplementation (same RNG stream) to
  ~1e-13 on 3 boundary seeds; numeric ≡ geometric membership on 25 seeds;
  rotation equivariance. Review: 1 finding (stale CLAUDE.md invariant bullet)
  fixed. ALL M1 BUGS COMPLETE. (R/ssm_bootstrap.R, tests, CLAUDE.md,
  DESIGN.md, NEWS.md).
- 2026-07-02 — G1 (Opus): print/summary.circumplex_ssm now note when a profile
  has fit < .70 ("interpret only elevation") or amplitude CI includes 0
  ("displacement not interpretable"). Profile rows only (contrast fit/amplitude
  are differences, not prototypicality). "Includes 0" operationalized as
  round(a_lci, digits) <= 0 since amplitude is structurally >= 0 (real profiles
  ~0.003; flat ~6e-17) — note tracks the displayed precision, so it stays
  consistent with the printed table. summary() inherits via print(). Non-ASCII
  R-squared written as ² (check 0/0/0). Review: inline (proportionate to a
  ~20-line print change); no other snapshot/expect_output affected; vignette
  summaries will gain notes on low-fit profiles (non-breaking).
  (R/ssm_oop.R, tests/testthat/test-ssm_oop.R, NEWS.md).
- 2026-07-02 — G2 (Opus): DECISION = document, do not normalize. Profile
  displacement range is [0°, 360°) (estimator modu(atan2,2π)). A peak exactly
  at the boundary reports ≈360° deterministically (y ≈ −2.78e-17 → atan2 small
  negative → wraps just under 2π), equivalently ≈0°, same pole. Not
  canonicalized: measure-zero float artifact, any snap is an arbitrary
  tie-break, ≈360 matches LM=360. Recorded in DESIGN.md conventions table,
  ?ssm_analyze return docs, and intro vignette. B1 boundary test already
  accepts {~0,~360}, so no test change. Doc-only; check 0/0/0.
  (DESIGN.md, R/ssm_analysis.R, man/ssm_analyze.Rd, intro vignette).
- 2026-07-02 — G3 (Opus): DECISION = support matrix input (not drop). Coerce
  `if (is.matrix(data)) data <- as.data.frame(data)` at entry of ssm_analyze,
  ssm_score, ipsatize, score, norm_standardize, self_standardize (guarded, so
  data.frame path byte-identical → seeded pins unchanged). ssm_score scales
  validation `is.character` → `is_var` (now accepts numeric indexes per its
  roxygen). All 5 `class(x) ==` sites → `inherits()` (tidying×2, ssm_table,
  ssm_plot_curve, is_instrument). Roxygen @param data aligned to "data frame
  or matrix". Regression tests: matrix≡data.frame for ssm_score/ssm_analyze/
  self_standardize/ipsatize, numeric scales for ssm_score; edge-checked
  matrix+grouping and matrix+append. Review inline (mechanical + input
  coercion). check 0/0/0. (R/ssm_analysis.R, R/tidying_functions.R,
  R/instrument_oop.R, R/ssm_table.R, R/ssm_plot.R, man/*, tests, NEWS.md).
- 2026-07-02 — G4 + D2 (Opus): DECISION = warn (not silent). Added base R
  `chkDots(...)` to ssm_plot_circle/curve/contrast (`...` is a pure sink in all
  three — no forwarding — so any arg landing there is a genuine typo; partial
  matching routes valid abbreviations to formals first, so no false positives).
  chkDots immediately surfaced a real latent bug: an existing test passed
  `drop_xy = TRUE` to ssm_plot_circle (which has no such arg — silently
  ignored); removed it (snapshot unchanged). Also fixed the D2 typo
  `angle_lables` → `angle_labels` in the ssm_plot_curve example (would have
  tripped the new warning). @param ... docs updated. check 0/0/0.
  (R/ssm_plot.R, man/*, tests/testthat/test-ssm_plot.R [CRLF preserved],
  NEWS.md).
- 2026-07-02 — D1 (Sonnet): purged the two stale `ssm_plot()` references
  (deleted function, split into `ssm_plot_circle()`/`_curve()`/`_contrast()`).
  Intermediate vignette line 271: `ssm_plot(results6)` → `ssm_plot_contrast(results6)`
  (results6 is a contrast result, matching the `ssm_plot_contrast(results6)`
  call already used earlier for the same object at line 221). Introduction
  vignette line 409: prose updated to name `ssm_table()`, `ssm_plot_circle()`,
  and `ssm_plot_curve()` instead of the single deleted function, matching the
  plot3/plot4 code chunks that follow. Left NEWS.md's historical `ssm_plot()`
  changelog entries untouched (accurate past-tense references) and the
  gitignored `doc/` build artifacts alone (regenerate on next vignette build).
  Doc-only; full test suite still 424/424 pass. (vignettes/intermediate-ssm-analysis.Rmd,
  vignettes/introduction-to-ssm-analysis.Rmd).
- 2026-07-02 — D3-D8 (Sonnet): remaining Docs batch.
  D3: `instruments()` said "14 instruments" but listed 15 (verified against 15
  `.rda` instrument files in data/); fixed the count string, regenerated the
  vdiffr/testthat snapshot. D4: documented contrast direction on `@param
  contrast` in `?ssm_analyze` — verified against code, not just restated from
  CLAUDE.md: for two groups, second level minus first is alphabetical unless
  `grouping` is already a factor (code coerces via `factor()`, which preserves
  existing level order — R/ssm_analysis.R:243); for two measures, it's simply
  `measures[2] - measures[1]` in the order given, never reordered
  (R/ssm_analysis.R:373) — corrected an over-generalized first draft that
  wrongly implied `measures` could also be alphabetized. D5: documented on
  `@param angles` that the closed-form estimator equals OLS only for equally
  spaced angles, wording matched to DESIGN.md's reviewed table entry (dropped
  an unverified "may not be minimally biased" claim from a first draft — not
  asserted anywhere in DESIGN.md, so cut per the statistical-correctness bar).
  D6: intro vignette no longer describes the displacement CI as a
  "significantly different from zero" test (displacement is angular; 0 degrees
  is an arbitrary reference direction, not a null value) — elevation/amplitude
  (linear, zero is meaningful) still described that way. D7: deleted the
  stale, untracked `CRAN-SUBMISSION` file (recorded the old 1.1.0 submission;
  already `.Rbuildignore`d). D8: removed the duplicated `# circumplex 1.1.0`
  heading in NEWS.md (only duplicate found — checked all version headings).
  Doc/roxygen-only; devtools::document() regenerated man/ssm_analyze.Rd; full
  suite 424/424 pass. (R/instrument_oop.R, R/ssm_analysis.R, man/ssm_analyze.Rd,
  tests/testthat/_snaps/instrument_oop.md, vignettes/introduction-to-ssm-analysis.Rmd,
  NEWS.md, CRAN-SUBMISSION [deleted]). ALL M1 DOCS COMPLETE.
- 2026-07-02 — Release prep (Sonnet, `/release-checklist`): pre-flight clean
  (working tree clean, all M1 Bugs/Guardrails/Docs boxes checked,
  `devtools::document()` no-diff). Verification: `devtools::test()` 424/424;
  `devtools::check(args = "--no-manual")` 0/0/0 locally. Ran
  `/statistical-validation` as a final consolidated pass since B1/B4/B5/B6 all
  touched estimation-adjacent code since v1.1.0: 22 independent reference
  checks (OLS equivalence at equal spacing, hand-computed 2/n Gurtman formula
  at unequal spacing, circular-quantile rotation invariance, angle_dist
  sign/antisymmetry, C++ helpers vs base R, end-to-end jz2017 sanity, plus all
  5 CLAUDE.md-mandated boundary cases: 0°/360° peak, flat profile, contrast
  near +/-180°) — all passed at ~1e-9 to ~1e-16. No CRAN revdeps
  (`tools::package_dependencies(reverse = TRUE)` returns none). Version bumped
  1.1.0.9000 -> 1.2.0 (DESCRIPTION); NEWS.md dev heading renamed to `# circumplex
  1.2.0` (no breaking changes to flag); cran-comments.md rewritten with test
  environments, revdep summary, and a change summary.
  NOT checking the Release box yet: local branch is 15 commits ahead of
  `origin/master` (nothing from this milestone has been pushed), so the actual
  GitHub Actions CI matrix has not run against this code — only the local
  macOS/R-4.6.1 check has. Awaiting user decision on push before that box can
  be honestly checked. (DESCRIPTION, NEWS.md, cran-comments.md).
- 2026-07-02 — Release box checked (Sonnet): user approved commit + push.
  Committed DESCRIPTION/NEWS.md/cran-comments.md/MILESTONES.md (f08248e),
  pushed 16 commits to `origin/master`. GitHub Actions R-CMD-check matrix
  green on all 5 legs (macos-latest/release, windows-latest/release,
  ubuntu-latest/devel, ubuntu-latest/release, ubuntu-latest/oldrel-1);
  test-coverage.yaml and pkgdown.yaml also green. M1 fully complete —
  package is CRAN-submission-ready pending the user's own
  `devtools::submit_cran()` (never run by the assistant).
- 2026-07-02 — win-builder R-devel clean; cran-comments.md updated to record
  it. ROADMAP.md gained a CRAN release strategy section (decouple GitHub
  milestones from CRAN submissions; M1 solo, bundle M2+M3, flagship M4).
  Fixed a NEWS.md line-wrap artifact (lone "0" digit on its own line).
- 2026-07-02 — **v1.2.0 approved by CRAN.** Post-acceptance: tagged `v1.2.0`;
  deleted the regenerated `CRAN-SUBMISSION` file; DESCRIPTION bumped to
  `1.2.0.9000`; NEWS.md gained a fresh `# circumplex (development version)`
  heading; milestone moved here to Completed; M2 promoted to the active slot.
