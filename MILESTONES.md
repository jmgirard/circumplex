# Active milestone

## M1 — Correctness & robustness patch (v1.2.0)

Source: ROADMAP.md Milestone 1 (2026-07 audit). Every bug fix lands with
a regression test that fails on the pre-fix code. Order below is
suggested (independent tasks; bugs before guardrails before docs).

### Bugs

**B1.
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
forwards `angles`** — `R/ssm_analysis.R:517` passes `...` to
[`apply()`](https://rdrr.io/r/base/apply.html) but never `angles`.
*Accept:* `ssm_score(aw2009, scales = PANO(), angles = rotated)` differs
from octant results and matches row-wise `ssm_parameters(x, rotated)`;
4-scale case with
[`poles()`](http://circumplex.jmgirard.com/reference/poles.md) works;
existing tests still pass.

**B2. `is_null_or_char()` honors `n`** — `R/utils.R:146` passes
`n = NULL`. *Accept:*
`ssm_analyze(..., measures = c("A","B"), measures_labels = "one label")`
errors informatively; NULL still accepted; audit other call sites
(`caption`, `angle_labels`) for behavior changes.

**B3. NA grouping values handled** — NA in `grouping` with
`listwise = FALSE` crashes in `mean_scores()`
(`unique(): detected NaN`). *Accept:* NA-group rows dropped with a
[`message()`](https://rdrr.io/r/base/message.html) reporting the count,
in both deletion modes; results match manually pre-filtered data.

**B4. Degenerate profiles return NA + warning** — zero-variance scores
give `Fit = -Inf` and noise displacement (`src/parameters.cpp`).
*Accept:* flat profile returns NA displacement/fit with one warning;
near-zero amplitude documented behavior decided and tested; bootstrap
containing some degenerate replicates doesn’t error.

**B5.
[`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
robust matching** — exact float equality on `Angle` vs norms table
(`R/tidying_functions.R:181-186`). *Accept:* 0° vs 360° convention
mismatch either works or errors with a message naming the expected
angles; duplicate-angle norms error clearly.

**B6. Contrast displacement branch harmony at ±180°** — point estimate
in (-180°, 180°\] can disagree with CI branch from circular centering.
*Accept:* simulated contrast near ±180° has estimate inside its CI; test
added at the boundary.

### Guardrails & UX

**G1.** `print`/`summary.circumplex_ssm` note when fit \< .70 or the
amplitude CI includes 0 (displacement not interpretable).

**G2.** Document displacement boundary convention (0° prints as 360°),
or normalize; decide once, record in DESIGN.md.

**G3.** [`inherits()`](https://rdrr.io/r/base/class.html) instead of
`class(x) ==` everywhere; fix or drop matrix input support in
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)/[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md).
Also (found during B1 review):
[`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
validates `is.character(scales)` and so rejects numeric column indexes,
contradicting both its own roxygen (“variable names or column numbers”)
and
[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)’s
`is_var()` validation — align on `is_var()`.

**G4.** Consider warning on unused `...` in plot functions.

### Docs

**D1.** Purge `ssm_plot()` references (intermediate vignette line ~271,
introduction vignette line ~409).

**D2.** Fix `angle_lables` typo in
[`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md)
example. (Done with G4, since the typo would otherwise trip the new
warning.)

**D3.**
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md)
count: says 14, lists 15.

**D4.** Document contrast direction in
[`?ssm_analyze`](http://circumplex.jmgirard.com/reference/ssm_analyze.md).

**D5.** Document equal-spacing assumption of the closed-form estimator.

**D6.** Intro vignette: fix “displacement significantly different from
zero” phrasing.

**D7.** Delete stale `CRAN-SUBMISSION` file. (`.Rbuildignore` already
updated for the md files and `.claude` — done 2026-07-02.)

**D8.** NEWS.md cleanup: remove the duplicated `# circumplex 1.1.0`
heading (lines 3/5); skim the rest for similar artifacts.

### Release

R CMD check clean on CI matrix; NEWS.md updated per user-facing change;
version to 1.2.0; `/release-checklist`.

## Log

- 2026-07-02 — Milestone opened from audit. Scaffolding added
  (CLAUDE.md, DESIGN.md, ROADMAP.md, MILESTONES.md, skills),
  `.Rbuildignore` updated.
- 2026-07-02 — ROADMAP revised: added CI-trustworthiness diagnostic (Z&W
  2017 via CircE replacement, now M4), inserted ggplot2 extension as M3
  (before fit stats so later milestones plot through it), renumbered
  M4-M6, added refactor verdict + targeted refactor list to continuous
  track.
- 2026-07-02 — B1:
  [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
  now forwards `angles` to
  [`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md);
  regression tests incl. 0°/360°-peak boundary; validated vs OLS at
  ~1e-13; check clean 0/0/0 (R/ssm_analysis.R,
  tests/testthat/test-ssm_analysis.R, NEWS.md). Review found
  pre-existing `scales` validation inconsistency → noted in G3. NB: dev
  env had lost ggforce/htmlTable + stale .so; reinstalled and rebuilt
  via clean_dll().
- 2026-07-02 — B2: `is_null_or_char()` now forwards `n`; call-site audit
  (measures_labels, angle_labels, caption) confirmed all tightenings
  only reject previously-wrong inputs; check clean (R/utils.R, tests,
  NEWS.md). Collateral fix: seeded the five unseeded vdiffr blocks in
  test-ssm_plot.R and regenerated 11 snapshots — they had depended on
  RNG state leaking from earlier test files, so ANY upstream test that
  consumes RNG broke them (diagnosed when B2’s bootstrap tests did
  exactly that; only arc coordinates changed, rendering verified
  unchanged via the seeded cross-zero snapshot). NB: test-ssm_plot.R is
  stored with CRLF line endings (repo outlier) — preserved; normalize
  deliberately someday if desired.
- 2026-07-02 — B3 (Opus): NA `grouping` rows now dropped in the
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  dispatcher (on the user’s real grouping column, once) with a count
  message + empty-data guard; fixes the pairwise
  `unique(): detected NaN` crash. No src/ change needed — the R-layer
  guard keeps NaN out of Armadillo. Review moved the drop from a
  per-subfunction helper up to the dispatcher, which also fixed a
  would-be column-name collision (a scale named “Group” vs the renamed
  grouping column). Regression tests cover both modes, contrast, the
  collision (expect_no_message), and the all-NA clean error. Check clean
  0/0/0 (R/ssm_analysis.R, R/utils.R, tests, NEWS.md).
- 2026-07-02 — B4 (Fable): degenerate-profile handling. C++ detects flat
  (sd ≤ 8·ε·n·max\|s\| — cannot test var==0 exactly; constant 0.1 gives
  ~2e-34) → NA disp/fit, and zero-amplitude-with-variance (pure higher
  harmonic) → NA disp, fit exactly 0. C++ silent; R warns once for
  observed profiles and once with a count for degenerate bootstrap
  resamples (quantiles now na.rm; CIs conditional on estimability,
  disclosed). Decision: NO threshold beyond machine noise — small real
  amplitudes keep point estimates (validated to 1e-9 amplitude); their
  uncertainty is the CI’s/G1’s job. Validation: 15/15 incl. NA-excluded
  CI == independent boot+filter reference (1e-10, 16/300 degenerate).
  Seeded pins unchanged. Documented in roxygen + DESIGN.md
  (src/parameters.cpp, R/ssm_analysis.R, R/ssm_bootstrap.R, tests,
  NEWS.md).
- 2026-07-02 — B5 (Opus):
  [`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
  matches scale→norm row by circular angular distance
  (`pmin(|Δ| %% 360, 360 - ...) < 1e-6`) instead of exact `==`, so 0≡360
  just works; clear errors for zero matches (names available angles) and
  \>1 match (duplicate-angle norms). Fixes cryptic “replacement has
  length zero”. Seeded values unchanged; check 0/0/0; review clean.
  Note: still uses `class(instrument) ==` — G3 scope.
  (R/tidying_functions.R, man/norm_standardize.Rd, tests, NEWS.md).
- 2026-07-02 — B6 (Fable): contrast displacement CI now reported on the
  estimate’s branch. Defect: near ±180° the angle_dist estimate and the
  circular-mean-centered CI could land on opposite branches (est +179.4
  vs CI (−196.6, −159.0) at data seed 70 — reproduced through the real
  pipeline after a seed search; flip probability ~10% per boundary
  dataset, hence intermittent). Fix: shift both CI endpoints by 2πk, k =
  round((est − mid)/2π), in ssm_bootstrap before degree conversion —
  identity (k=0) away from the boundary (all seeded pins
  byte-identical), width/contiguity preserved, cannot fabricate coverage
  (\|est − mid\| ≤ π ⇒ k=0 for wide CIs). Validation: pkg CI ==
  independent reimplementation (same RNG stream) to ~1e-13 on 3 boundary
  seeds; numeric ≡ geometric membership on 25 seeds; rotation
  equivariance. Review: 1 finding (stale CLAUDE.md invariant bullet)
  fixed. ALL M1 BUGS COMPLETE. (R/ssm_bootstrap.R, tests, CLAUDE.md,
  DESIGN.md, NEWS.md).
- 2026-07-02 — G1 (Opus): print/summary.circumplex_ssm now note when a
  profile has fit \< .70 (“interpret only elevation”) or amplitude CI
  includes 0 (“displacement not interpretable”). Profile rows only
  (contrast fit/amplitude are differences, not prototypicality).
  “Includes 0” operationalized as round(a_lci, digits) \<= 0 since
  amplitude is structurally \>= 0 (real profiles ~0.003; flat ~6e-17) —
  note tracks the displayed precision, so it stays consistent with the
  printed table. summary() inherits via print(). Non-ASCII R-squared
  written as ² (check 0/0/0). Review: inline (proportionate to a
  ~20-line print change); no other snapshot/expect_output affected;
  vignette summaries will gain notes on low-fit profiles (non-breaking).
  (R/ssm_oop.R, tests/testthat/test-ssm_oop.R, NEWS.md).
- 2026-07-02 — G2 (Opus): DECISION = document, do not normalize. Profile
  displacement range is \[0°, 360°) (estimator modu(atan2,2π)). A peak
  exactly at the boundary reports ≈360° deterministically (y ≈ −2.78e-17
  → atan2 small negative → wraps just under 2π), equivalently ≈0°, same
  pole. Not canonicalized: measure-zero float artifact, any snap is an
  arbitrary tie-break, ≈360 matches LM=360. Recorded in DESIGN.md
  conventions table, ?ssm_analyze return docs, and intro vignette. B1
  boundary test already accepts {_(0,)360}, so no test change. Doc-only;
  check 0/0/0. (DESIGN.md, R/ssm_analysis.R, man/ssm_analyze.Rd, intro
  vignette).
- 2026-07-02 — G3 (Opus): DECISION = support matrix input (not drop).
  Coerce `if (is.matrix(data)) data <- as.data.frame(data)` at entry of
  ssm_analyze, ssm_score, ipsatize, score, norm_standardize,
  self_standardize (guarded, so data.frame path byte-identical → seeded
  pins unchanged). ssm_score scales validation `is.character` → `is_var`
  (now accepts numeric indexes per its roxygen). All 5 `class(x) ==`
  sites → [`inherits()`](https://rdrr.io/r/base/class.html) (tidying×2,
  ssm_table, ssm_plot_curve, is_instrument). Roxygen @param data aligned
  to “data frame or matrix”. Regression tests: matrix≡data.frame for
  ssm_score/ssm_analyze/ self_standardize/ipsatize, numeric scales for
  ssm_score; edge-checked matrix+grouping and matrix+append. Review
  inline (mechanical + input coercion). check 0/0/0. (R/ssm_analysis.R,
  R/tidying_functions.R, R/instrument_oop.R, R/ssm_table.R,
  R/ssm_plot.R, man/\*, tests, NEWS.md).
- 2026-07-02 — G4 + D2 (Opus): DECISION = warn (not silent). Added base
  R `chkDots(...)` to ssm_plot_circle/curve/contrast (`...` is a pure
  sink in all three — no forwarding — so any arg landing there is a
  genuine typo; partial matching routes valid abbreviations to formals
  first, so no false positives). chkDots immediately surfaced a real
  latent bug: an existing test passed `drop_xy = TRUE` to
  ssm_plot_circle (which has no such arg — silently ignored); removed it
  (snapshot unchanged). Also fixed the D2 typo `angle_lables` →
  `angle_labels` in the ssm_plot_curve example (would have tripped the
  new warning). @param … docs updated. check 0/0/0. (R/ssm_plot.R,
  man/\*, tests/testthat/test-ssm_plot.R \[CRLF preserved\], NEWS.md).
- 2026-07-02 — D1 (Sonnet): purged the two stale `ssm_plot()` references
  (deleted function, split into
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md)/`_curve()`/`_contrast()`).
  Intermediate vignette line 271: `ssm_plot(results6)` →
  `ssm_plot_contrast(results6)` (results6 is a contrast result, matching
  the `ssm_plot_contrast(results6)` call already used earlier for the
  same object at line 221). Introduction vignette line 409: prose
  updated to name
  [`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md),
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md),
  and
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md)
  instead of the single deleted function, matching the plot3/plot4 code
  chunks that follow. Left NEWS.md’s historical `ssm_plot()` changelog
  entries untouched (accurate past-tense references) and the gitignored
  `doc/` build artifacts alone (regenerate on next vignette build).
  Doc-only; full test suite still 424/424 pass.
  (vignettes/intermediate-ssm-analysis.Rmd,
  vignettes/introduction-to-ssm-analysis.Rmd).
- 2026-07-02 — D3-D8 (Sonnet): remaining Docs batch. D3:
  [`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md)
  said “14 instruments” but listed 15 (verified against 15 `.rda`
  instrument files in data/); fixed the count string, regenerated the
  vdiffr/testthat snapshot. D4: documented contrast direction on
  `@param contrast` in
  [`?ssm_analyze`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  — verified against code, not just restated from CLAUDE.md: for two
  groups, second level minus first is alphabetical unless `grouping` is
  already a factor (code coerces via
  [`factor()`](https://rdrr.io/r/base/factor.html), which preserves
  existing level order — R/ssm_analysis.R:243); for two measures, it’s
  simply `measures[2] - measures[1]` in the order given, never reordered
  (R/ssm_analysis.R:373) — corrected an over-generalized first draft
  that wrongly implied `measures` could also be alphabetized. D5:
  documented on `@param angles` that the closed-form estimator equals
  OLS only for equally spaced angles, wording matched to DESIGN.md’s
  reviewed table entry (dropped an unverified “may not be minimally
  biased” claim from a first draft — not asserted anywhere in DESIGN.md,
  so cut per the statistical-correctness bar). D6: intro vignette no
  longer describes the displacement CI as a “significantly different
  from zero” test (displacement is angular; 0 degrees is an arbitrary
  reference direction, not a null value) — elevation/amplitude (linear,
  zero is meaningful) still described that way. D7: deleted the stale,
  untracked `CRAN-SUBMISSION` file (recorded the old 1.1.0 submission;
  already `.Rbuildignore`d). D8: removed the duplicated
  `# circumplex 1.1.0` heading in NEWS.md (only duplicate found —
  checked all version headings). Doc/roxygen-only; devtools::document()
  regenerated man/ssm_analyze.Rd; full suite 424/424 pass.
  (R/instrument_oop.R, R/ssm_analysis.R, man/ssm_analyze.Rd,
  tests/testthat/\_snaps/instrument_oop.md,
  vignettes/introduction-to-ssm-analysis.Rmd, NEWS.md, CRAN-SUBMISSION
  \[deleted\]). ALL M1 DOCS COMPLETE.
- 2026-07-02 — Release prep (Sonnet, `/release-checklist`): pre-flight
  clean (working tree clean, all M1 Bugs/Guardrails/Docs boxes checked,
  `devtools::document()` no-diff). Verification: `devtools::test()`
  424/424; `devtools::check(args = "--no-manual")` 0/0/0 locally. Ran
  `/statistical-validation` as a final consolidated pass since
  B1/B4/B5/B6 all touched estimation-adjacent code since v1.1.0: 22
  independent reference checks (OLS equivalence at equal spacing,
  hand-computed 2/n Gurtman formula at unequal spacing,
  circular-quantile rotation invariance, angle_dist sign/antisymmetry,
  C++ helpers vs base R, end-to-end jz2017 sanity, plus all 5
  CLAUDE.md-mandated boundary cases: 0°/360° peak, flat profile,
  contrast near +/-180°) — all passed at ~1e-9 to ~1e-16. No CRAN
  revdeps (`tools::package_dependencies(reverse = TRUE)` returns none).
  Version bumped 1.1.0.9000 -\> 1.2.0 (DESCRIPTION); NEWS.md dev heading
  renamed to `# circumplex 1.2.0` (no breaking changes to flag);
  cran-comments.md rewritten with test environments, revdep summary, and
  a change summary. NOT checking the Release box yet: local branch is 15
  commits ahead of `origin/master` (nothing from this milestone has been
  pushed), so the actual GitHub Actions CI matrix has not run against
  this code — only the local macOS/R-4.6.1 check has. Awaiting user
  decision on push before that box can be honestly checked.
  (DESCRIPTION, NEWS.md, cran-comments.md).
- 2026-07-02 — Release box checked (Sonnet): user approved commit +
  push. Committed DESCRIPTION/NEWS.md/cran-comments.md/MILESTONES.md
  (f08248e), pushed 16 commits to `origin/master`. GitHub Actions
  R-CMD-check matrix green on all 5 legs (macos-latest/release,
  windows-latest/release, ubuntu-latest/devel, ubuntu-latest/release,
  ubuntu-latest/oldrel-1); test-coverage.yaml and pkgdown.yaml also
  green. M1 fully complete — package is CRAN-submission-ready pending
  the user’s own `devtools::submit_cran()` (never run by the assistant).

# Completed milestones

(none yet — move finished milestones here with their final log)
