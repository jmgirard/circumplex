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
- [ ] **B2. `is_null_or_char()` honors `n`** — `R/utils.R:146` passes
  `n = NULL`.
  *Accept:* `ssm_analyze(..., measures = c("A","B"), measures_labels = "one label")`
  errors informatively; NULL still accepted; audit other call sites
  (`caption`, `angle_labels`) for behavior changes.
- [ ] **B3. NA grouping values handled** — NA in `grouping` with
  `listwise = FALSE` crashes in `mean_scores()` (`unique(): detected NaN`).
  *Accept:* NA-group rows dropped with a `message()` reporting the count, in
  both deletion modes; results match manually pre-filtered data.
- [ ] **B4. Degenerate profiles return NA + warning** — zero-variance scores
  give `Fit = -Inf` and noise displacement (`src/parameters.cpp`).
  *Accept:* flat profile returns NA displacement/fit with one warning;
  near-zero amplitude documented behavior decided and tested; bootstrap
  containing some degenerate replicates doesn't error.
- [ ] **B5. `norm_standardize()` robust matching** — exact float equality on
  `Angle` vs norms table (`R/tidying_functions.R:181-186`).
  *Accept:* 0° vs 360° convention mismatch either works or errors with a
  message naming the expected angles; duplicate-angle norms error clearly.
- [ ] **B6. Contrast displacement branch harmony at ±180°** — point estimate
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

---

# Completed milestones

(none yet — move finished milestones here with their final log)
