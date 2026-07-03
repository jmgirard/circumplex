# Active milestone

## M3 — Visualization layer: ggplot2 circumplex extension

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
- [x] **V2. Polar-native geoms/stats (ggproto).** `geom_ssm_point()` /
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
- [x] **V3. Scales.** `scale_*_circumplex()` helpers for angle-labeled axes
  and amplitude gridlines, with defaults matching the current appearance.
  *Accept:* helpers produce the current tick/label placement on both the
  circular canvas and the curve plot's angle axis; custom `angle_labels`
  and instrument labels flow through; documented with examples.
  *Scope refined during implementation (2026-07-02):* only the curve plot's
  linear angle axis is a genuine ggplot scale (`scale_x_circumplex()`). The
  circular canvas's angle labels and amplitude rings are drawn geometry under
  `theme_void()`, owned by `ggcircumplex()` (V1) — not ggplot scale breaks — so
  they are NOT re-expressed as `scale_*` (that abstraction fits poorly over
  drawn geometry and would jeopardize V4's snapshot stability). Consistency
  across the two contexts is instead guaranteed by a shared internal
  `resolve_circumplex_labels()` used by both `scale_x_circumplex()` and
  `ggcircumplex()`, so identical angle/label/instrument inputs yield matching
  labels on the axis and the canvas (asserted in tests). No `scale_y_*` shipped
  (no linear circumplex plot has an amplitude axis; would be speculative API).
- [x] **V4. Refactor existing plots onto the extension.** Reimplement
  `ssm_plot_circle()`, `ssm_plot_curve()`, `ssm_plot_contrast()` on top of
  V1–V3 with **behavior unchanged**.
  *Accept:* every existing vdiffr snapshot in
  `tests/testthat/_snaps/ssm_plot/` stays byte-identical, or each change is
  individually justified and re-approved; `chkDots()`/argument surfaces of the
  three functions are preserved; full suite green.
- [x] **V5. Vignette: "Advanced Circumplex Visualization."** The third
  vignette, already announced as "still in progress" in the intermediate
  vignette (`vignettes/intermediate-ssm-analysis.Rmd:276`). Demonstrate
  composing raw data, SSM results, and annotations via the new extension.
  *Accept:* builds clean; uses only exported API; teaching prose meets the
  statistical-precision bar (CLAUDE.md — e.g., never describe an angular CI
  excluding 0° as a significance test); intermediate vignette's "in progress"
  note updated to point at it.
- [x] **V6. Design review vs. ggplot2 extension best practices.** Check
  ggproto lifecycle, `after_stat()`/`after_scale()` usage, theme integration,
  and the `ggforce` dependency decision (keep iff it simplifies arcs).
  *Accept:* a short written verdict appended to DESIGN.md (a "Visualization
  extension" subsection) recording the ggproto/scale architecture and the
  ggforce keep/drop decision with rationale.

### Pre-release review fixes (v1.3.0 bundle)

From a `/code-review high` pass over `v1.2.0..HEAD` (2026-07-03, Fable finders +
verifiers). 10 findings survived verification (9 CONFIRMED, 1 PLAUSIBLE); the
statistical core — Monte Carlo covariance math, bootstrap named-column refactor,
`ssm_param_names()`↔C++ ordering, circular-quantile/branch machinery — verified
clean. All correctness findings sit at the new/changed **public API surface**
introduced by M2+M3, so they must be fixed (with regression tests) before the
v1.3.0 CRAN submission. Each fix ships with a test reproducing the reviewer's
executed failure.

Correctness (fix first):

- [x] **R1. `ssm_score()` silently drops unnamed `...` args** — v1.2.0 forwarded
  them positionally to `ssm_parameters()` (a bare `"IIP_"` filled `prefix`); the
  vectorized path inspects only `names(extra_args)` + `modifyList()`, discarding
  unnamed elements with no error. (`R/ssm_analysis.R:689`)
  *Accept:* the v1.2.0 call form errors or works as before (decision below), never
  silently returns unprefixed columns; regression test at the executed example.
- [x] **R4. `ssm_score()` lost scalar validation on `prefix`/`suffix`/labels** —
  non-scalar/non-character values recycle through `paste0()` into interleaved
  garbage column names instead of erroring via `is_char(x, n = 1)`.
  (`R/ssm_analysis.R:715`) *Accept:* `prefix = c("a_","b_")` errors again; test added.
  (R1+R4 are one root cause — the refactor's `...` handling — fix together.)
- [ ] **R3. `ggcircumplex(amin=)` mislabels the amplitude axis** — `amin`
  relabels rings on an `amin..amax` scale while the geoms map amplitude as
  `a*5/amax` (amin=0), so any nonzero `amin` silently misplaces every point/arc;
  `amin > amax` unvalidated. New/exported this cycle (unreachable in v1.2.0).
  (`R/ssm_plot.R:510`, `R/geom_ssm.R:11`) *Accept:* per the decision below —
  geoms honor `amin`, or `amin` is gated/removed — plus a test that the labeled
  ring radius and a same-amplitude point's radius agree.
- [x] **R2. Flat / zero-amplitude profiles silently vanish from
  `ssm_plot_circle()`** — geoms drop NA-displacement rows with no message;
  v1.2.0 drew the point at the canvas origin with a "Removed N rows" warning.
  (`R/geom_ssm.R:62`) *Accept:* per the display decision below; a flat-profile
  test asserts the chosen behavior (origin point and/or explicit message).
- [x] **R5. `ggcircumplex()` rounds angles to integers** —
  `as.integer(round(resolved$angles))` shifts fractional angles (round-half-even)
  on the canvas while geoms/scale use exact values, so 22.5° spokes draw at 22°.
  (`R/ssm_plot.R:508`) *Accept:* fractional-angle canvas places spokes/labels at
  exact angles; test on `seq(22.5, 337.5, 45)` (16-scale); ssm_plot snapshots stay
  byte-identical (details$angles are integer octants, so no snapshot delta).
- [x] **R6. `scale_x_circumplex()` rounds fractional angle labels** —
  `sprintf("%.0f°", x)` labels 22.5→"22°", 67.5→"68°" while breaks sit at exact
  angles. (`R/scale_circumplex.R:55`) *Accept:* fractional angles label without
  misleading rounding (format decision with R5); test added. (R5+R6 share the
  rounding root cause — fix together.)

Cleanup (fold in where the bug fixes already touch the file; no behavior change):

- [ ] **C2. Duplicated engine-dispatch block** — the
  `if (method=="montecarlo") ssm_montecarlo() else ssm_bootstrap()` branch is
  near-verbatim in `ssm_analyze_means()` and `ssm_analyze_corrs()`
  (`R/ssm_analysis.R:348,486`); extract one dispatcher (method already validated
  once in `ssm_analyze()`). *Accept:* single helper, both callers routed, all
  seeded pins byte-identical.
- [ ] **C4. Monte Carlo recomputes `t0` scores** — `ssm_montecarlo()` re-runs
  `mean_scores()`/`corr_scores()` on the same unmutated `bs_input` the caller
  already computed (`R/ssm_montecarlo.R:43`); pass the caller's matrix in.
  *Accept:* one fewer O(n·p·q) pass; results byte-identical.
- [ ] **C3. Repel-branch trig duplication** — `ssm_plot_circle()`'s repel branch
  hand-recomputes the polar→canvas transform duplicated in
  `GeomSsmPoint$setup_data()` (`R/ssm_plot.R:192` vs `R/geom_ssm.R:64`); extract a
  shared `ssm_to_cartesian()` helper. *Accept:* both sites call it; snapshots
  byte-identical. (Natural companion to R2/R3, same files.)
- [ ] **C1. Monte Carlo contrast re-derives `param_diff()`** — the MC replicate
  contrast inlines "second minus first, disp via `angle_dist`" (`R/ssm_montecarlo.R:110`)
  that `param_diff()` owns for the bootstrap path. PLAUSIBLE, not a bug: fix by
  generalizing `param_diff()` to an R×6 matrix (named via `ssm_param_names()`) and
  calling it in both places. *Touches the contrast convention → run
  `/statistical-validation` and boundary tests (±180°) after.* *Accept:* MC and
  bootstrap contrasts share one convention implementation; boundary tests green;
  seeded pins unchanged.

## Log

- 2026-07-03 — Pre-release review fixes R1/R4/R5/R6/R2 (Opus, test-first). Five
  API-surface correctness bugs from the `/code-review high` pass, each with a
  regression test that failed on the pre-fix code. R1+R4 (`R/ssm_analysis.R`
  `ssm_score()`): unnamed `...` args now error (was: silently dropped, losing a
  column prefix vs v1.2.0) and each label/prefix/suffix is re-validated as a
  length-1 string via `is_char()` (was: vectors recycled into garbled column
  names). Decision (Jeff): error on unnamed rather than restore v1.2.0's fragile
  positional forwarding. R5+R6 (`R/scale_circumplex.R`, `R/ssm_plot.R`): extracted
  a shared `circumplex_degree_labels()` and routed both the canvas
  (`ggcircumplex()`/`circle_base()`) and the axis (`scale_x_circumplex()`) through
  it; removed the `as.integer(round())` coercion in `ggcircumplex()` AND the twin
  at `ssm_plot_circle()`'s internal angle line, so fractional angles (e.g. 22.5°)
  render exactly instead of rounding to 22°. R2 (`R/ssm_plot.R`): `ssm_plot_circle()`
  now detects undefined-displacement profiles (flat/zero-amplitude, `d_est = NA`)
  up front, warns naming them, and removes them cleanly (decision (Jeff): drop +
  named warning, not v1.2.0's origin point) — also fixes the messy NA-row the
  low-fit filter otherwise produced. No estimator math touched → no
  `/statistical-validation` needed. All ssm_plot/geom_ssm vdiffr snapshots
  byte-identical (built-in angles are integers). Suite 543/543, 0 warnings.
  NEWS.md bullets added. R3 (amin) pending Jeff's confirm; C1–C4 cleanups next.
  (R/ssm_analysis.R, R/scale_circumplex.R, R/ssm_plot.R, tests/testthat/
  test-ssm_analysis.R, test-scale_circumplex.R, test-ssm_plot.R, NEWS.md,
  MILESTONES.md).
- 2026-07-02 — V6 Extension design review (Opus, doc-only). Audited the V1–V4
  ggproto code against ggplot2 extension idioms; appended a "Visualization
  extension" section to DESIGN.md recording architecture + verdict. Findings:
  (1) after_stat/after_scale correctly unused (the arc Stat feeds GeomArcBar's
  aes directly, ggforce-style; nothing needs post-scale remap). (2) ggforce =
  KEEP (the acceptance's "iff it simplifies arcs"): StatSsmArc inherits its
  annular-wedge polygon tessellation (StatArcBar/arcPaths) instead of
  reimplementing a wrap-aware tessellator, and geom_circle draws the rings;
  already a mature Import. (3) Recorded known trade-offs, each deliberate and
  each risky to "fix" because it would threaten V4 byte-identical snapshots:
  amax is a per-layer param not shared state (idiomatic fix = a CoordCircumplex
  owning amax + the polar transform, deferred); the theme_void canvas doesn't
  respond to themes; na.rm is effectively always TRUE (minor convention
  deviation); the GeomSsmPoint/StatSsmArc generators aren't exported (cheap
  future add for subclassers). No code/test/NEWS change (DESIGN.md is
  .Rbuildignore'd internal memory). Verdict claims verified against code/
  NAMESPACE. M3 COMPLETE — all V1–V6 done. (DESIGN.md, MILESTONES.md).
- 2026-07-02 — V5 Advanced Visualization vignette (Sonnet). New
  vignettes/advanced-visualization.Rmd: builds custom circumplex figures by
  composing the exported extension — bare/instrument-labeled ggcircumplex()
  canvas; geom_ssm_arc()+geom_ssm_point() on an ssm_analyze() result (with the
  amax-must-match rule made explicit); a composed custom layer (per-person
  ssm_score() point cloud behind a group point — the payoff no built-in
  produces); scale_x_circumplex() for a linear angle axis; and a closing note
  that ssm_plot_* are built on these same pieces. Every chunk verified to run;
  render produces no warnings (per-person degenerate row filtered, chunk
  warning=FALSE) and no chunk errors. Statistical-precision pass (CLAUDE.md):
  the arc described as displaying two separate marginal CIs (amplitude radial,
  displacement angular) shown together — explicitly NOT a joint confidence
  region with its own coverage, NOT a significance test; angular extent framed
  as plausible directions since 0 deg is an arbitrary reference, not a null
  (consistent with the D6 intro-vignette fix and DESIGN.md). Intermediate
  vignette's "still in progress" note replaced with a concrete pointer.
  _pkgdown.yml: added the vignette to the navbar AND a new "Visualization Layer"
  reference section for ggcircumplex/geom_ssm_point/geom_ssm_arc/
  scale_x_circumplex (V1–V3 had exported these without listing them — they were
  orphaned on the site and would trip a pkgdown missing-topics warning). Vignette
  ASCII-clean; check builds all vignettes 0/0/0; suite 529/529 (doc-only).
  (vignettes/advanced-visualization.Rmd [new],
  vignettes/intermediate-ssm-analysis.Rmd, _pkgdown.yml, NEWS.md, MILESTONES.md).
- 2026-07-02 — V4 Refactor plots onto the extension (Opus). ssm_plot_circle:
  removed the inline amplitude/displacement→canvas transform (rescale + ggrad +
  0/360 wrap); now circle_base→ggcircumplex (V1), ggforce::geom_arc_bar→
  geom_ssm_arc, geom_point(x_est,y_est)→geom_ssm_point(a_est,d_est) (V2); the
  repel branch recomputes canvas coords from a_est/d_est via ssm_radius (the
  formerly-precomputed x_est/y_est no longer exist post-transform-removal).
  ssm_plot_curve: scale_x_continuous(breaks,labels)→scale_x_circumplex (V3);
  dropped the inline degree-label function (the scale supplies it). All palette/
  vary_shapes/drop_lowfit/guides/theme logic untouched. ssm_plot_contrast: NOT
  refactored — it is a Cartesian faceted point-range difference plot with no
  circular canvas, polar geom, or angle axis, so nothing in V1–V3 applies
  (honest scope call, not an omission). Behavior-preserving: ALL 11 existing
  ssm_plot vdiffr snapshots stayed byte-identical (incl. the stochastic repel
  one and the cross-zero arc) — the V2 geometric-equality proof held at full
  render. -7 net lines; ggrad no longer referenced in ssm_plot.R (still used
  internally by geom_ssm.R); circle_base still reached via ggcircumplex. Suite
  529/529; check 0/0/0. No NEWS (behavior unchanged; the plots are now built on
  the public extension, but nothing user-visible changed). (R/ssm_plot.R,
  MILESTONES.md).
- 2026-07-02 — V3 Scales (Opus): exported `scale_x_circumplex()`, a ggplot2
  continuous position scale for the angle axis of linear circumplex plots (the
  ssm_plot_curve score-by-angle axis). Breaks at the scale angles; default
  labels = degrees (sprintf "%.0f\\U00B0", matching ssm_plot_curve exactly);
  accepts a labels vector or a circumplex_instrument (abbreviations). Extracted
  a shared internal `resolve_circumplex_labels(angles, labels, instrument)` and
  routed BOTH the new scale and `ggcircumplex()` (V1) through it, so identical
  inputs label the linear axis and the circular canvas consistently — asserted
  by a test comparing the scale's get_labels() to the canvas's drawn label
  layer. ggcircumplex refactor verified output-identical (0 snapshot changes).
  Scope call recorded in the V3 task entry: circular-canvas gridlines/labels
  are theme_void drawn geometry (ggcircumplex's job), not ggplot scales, so no
  scale_* is forced over them; no speculative scale_y_*. Non-ASCII degree sign
  written as \\U00B0 per the R/ convention (avoids the R CMD check non-ASCII
  note). Tests via standalone Scale$get_labels() (device-independent) plus a
  build-level check that the curve plot's axis labels match. Suite 529/529;
  check 0/0/0. NEWS.md added. (R/scale_circumplex.R [new], R/ssm_plot.R
  [ggcircumplex refactor], man/scale_x_circumplex.Rd [new], NAMESPACE,
  tests/testthat/test-scale_circumplex.R [new], NEWS.md, MILESTONES.md).
- 2026-07-02 — V2 Polar-native geoms (Opus): exported `geom_ssm_point()` and
  `geom_ssm_arc()`, ggplot2 layers taking amplitude/displacement aesthetics and
  internalizing the polar transform formerly inline in ssm_plot_circle (radius
  = amplitude*5/amax, angle = ggrad(displacement), 0/360 wrap = +360 when
  d_max<d_min). Architecture: `GeomSsmPoint` subclasses GeomPoint and computes
  x/y in setup_data (runs before scale training, so the canvas range picks the
  points up); `StatSsmArc` subclasses `ggforce::StatArcBar`, injecting
  x0/y0/r0/r/start/end in an overridden compute_panel then delegating to the
  parent (ggproto_parent) for the arcPaths polygon expansion — reuses ggforce's
  arc machinery rather than reimplementing it. `amax` is a layer param (ggplot
  can't share canvas state with a geom; documented). NA-displacement/degenerate
  rows dropped in setup_data/compute_panel (StatArcBar needed an nrow==0 guard —
  scalar assignment to a 0-row frame errors). `extra_params` needed on the geom
  so ggplot2 accepts `amax`. Correctness proven device-independently: the arc
  and point layers' built x/y are byte-equal to ssm_plot_circle's (layers 6/7 in
  both, since both share circle_base's 5 canvas layers) on single- and
  multi-profile results and the cross-zero case; plus a synthetic wrap test
  (350→10 arc has ~same vertex count as 170→190, not the ~17x of a long-way
  span). ssm_plot_circle untouched → its 11 vdiffr snapshots unchanged (verified
  via git). One example bug (Ampl/Disp vs a_est/d_est) caught before check.
  Suite 518/518; check 0/0/0. NEWS.md added. (R/geom_ssm.R [new],
  man/geom_ssm_point.Rd [new], man/geom_ssm_arc.Rd [new], NAMESPACE,
  tests/testthat/test-geom_ssm.R [new],
  tests/testthat/_snaps/geom_ssm/*.svg [new], NEWS.md, MILESTONES.md).
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

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0; M2 →
GitHub-complete, bundled into v1.3.0). When the active milestone ships,
`/release-checklist` moves it there too. This file stays scoped to the active
milestone so it is cheap to re-read at the start of each task.
