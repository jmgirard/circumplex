# Active milestone

## M1 — Correctness & robustness patch (v1.2.0)

Source: ROADMAP.md Milestone 1 (2026-07 audit). Every bug fix lands with a
regression test that fails on the pre-fix code. Order below is suggested
(independent tasks; bugs before guardrails before docs).

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

- [ ] **G1.** `print`/`summary.circumplex_ssm` note when fit < .70 or the
  amplitude CI includes 0 (displacement not interpretable).
- [ ] **G2.** Document displacement boundary convention (0° prints as 360°),
  or normalize; decide once, record in DESIGN.md.
- [ ] **G3.** `inherits()` instead of `class(x) ==` everywhere; fix or drop
  matrix input support in `ssm_analyze()`/`ssm_score()`. Also (found during
  B1 review): `ssm_score()` validates `is.character(scales)` and so rejects
  numeric column indexes, contradicting both its own roxygen ("variable names
  or column numbers") and `ssm_analyze()`'s `is_var()` validation — align on
  `is_var()`.
- [ ] **G4.** Consider warning on unused `...` in plot functions.

### Docs

- [ ] **D1.** Purge `ssm_plot()` references (intermediate vignette line ~271,
  introduction vignette line ~409).
- [ ] **D2.** Fix `angle_lables` typo in `ssm_plot_curve()` example.
- [ ] **D3.** `instruments()` count: says 14, lists 15.
- [ ] **D4.** Document contrast direction in `?ssm_analyze`.
- [ ] **D5.** Document equal-spacing assumption of the closed-form estimator.
- [ ] **D6.** Intro vignette: fix "displacement significantly different from
  zero" phrasing.
- [ ] **D7.** Delete stale `CRAN-SUBMISSION` file. (`.Rbuildignore` already
  updated for the md files and `.claude` — done 2026-07-02.)
- [ ] **D8.** NEWS.md cleanup: remove the duplicated `# circumplex 1.1.0`
  heading (lines 3/5); skim the rest for similar artifacts.

### Release

- [ ] R CMD check clean on CI matrix; NEWS.md updated per user-facing change;
  version to 1.2.0; `/release-checklist`.

## Log

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

# Completed milestones

(none yet — move finished milestones here with their final log)
