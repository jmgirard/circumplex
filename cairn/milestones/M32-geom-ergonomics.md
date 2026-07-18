<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M32: Circumplex geom & layer ergonomics

- **Status:** review
- **Priority:** normal
- **Depends on:** M31
- **Principles touched:** —
- **Branch/PR:** m32-geom-ergonomics · [PR #56](https://github.com/jmgirard/circumplex/pull/56)

## Goal

Make the M31 circumplex ggplot2 layers extensible and ergonomic: export the
`GeomSsmPoint` / `GeomSsmArc` / `CoordCircumplex` ggproto generators for
subclassing, give `na.rm` opt-in ggplot2 warn-parity, fix the due-East
`0.5`/`LM` label overlap on the canvas, and firm up the repel/label ergonomics
users need to compose custom figures.

## Scope

**In:**
- Export the `GeomSsmPoint`, `GeomSsmArc`, and `CoordCircumplex` ggproto
  generators (`@format NULL` docs) so downstream packages can subclass them
  ([DESIGN.md:330-334](../DESIGN.md) defers "the `GeomSsmPoint`/`GeomSsmArc`/coord
  ggproto generators" to M32; the arc is `GeomSsmArc ⊂ GeomRect`, **not** a Stat —
  the DESIGN.md:319 `StatSsmArc ⊂ ggforce::StatArcBar` phrase is stale post-M31/
  D-020 and is swept here).
- `na.rm` **opt-in warn-parity**: default stays `na.rm = TRUE` (silent — no new
  warnings in existing plots/vignettes); when a geom is given `na.rm = FALSE` it
  **warns with the dropped-row count** before dropping degenerate rows
  ([R/geom_ssm.R:113-120](../../R/geom_ssm.R), :187-193 currently always-silent).
  `ssm_plot_circle()`'s own by-name warning is unchanged (its internal geoms keep
  the silent default).
- Fix the `0.5`/`LM` (amplitude-ring vs. due-East angle) label overlap on the
  canvas furniture (M31 handoff, [archive:24-25](archive/M31-coord-system-build.md)):
  relocate/nudge so the radial-axis labels and the due-East spoke label no longer
  collide. This intentionally changes default rendering — its vdiffr baseline is
  regenerated.
- Firm up repel/label ergonomics (the `ssm_plot_circle(repel=)` path,
  [R/ssm_plot.R:204-221](../../R/ssm_plot.R), currently flagged experimental) plus
  any additional styling/aesthetic options for custom composition, **each new
  option with a default that reproduces current rendering**.

**Out:**
- The coordinate-system rewrite → M31 (done; this builds on its shipped layer
  contract).
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown → M34.

## Acceptance criteria

- [x] `GeomSsmPoint`, `GeomSsmArc`, and `CoordCircumplex` are exported (NAMESPACE +
      `@format NULL` docs); a test defines a trivial subclass of each and renders
      it, proving the generators are usable downstream. The stale
      `StatSsmArc ⊂ ggforce::StatArcBar` phrase is gone from DESIGN.md.
- [x] With `na.rm = FALSE`, each geom (`GeomSsmPoint`, `GeomSsmArc`) given a
      degenerate (missing amplitude/displacement or incomplete-CI) row **warns**
      with the dropped-row count before dropping; with `na.rm = TRUE` (the default)
      it stays silent — all four cases asserted by a test.
- [x] The default canvas draws its amplitude (radial) axis off every displacement
      spoke — the built coord's `r_axis_inside` sits at the widest-gap midpoint
      (22.5° for octants) and coincides with no spoke, so the `0.5` amplitude
      label no longer collides with the due-East `0.5`/`LM` label — asserted at
      the built-coord + helper level, plus a regenerated vdiffr baseline;
      unrelated plot baselines (curve/contrast/diagnostic) regenerate
      byte-identically. <!-- amended 2026-07-18 at review gate; see work log -->
- [x] Each new styling/repel aesthetic has a default that reproduces current
      rendering (existing vdiffr/snapshot baselines unchanged where no new option
      is set) and a test exercising the non-default path; `repel = TRUE` yields
      non-overlapping labels.
- [x] `devtools::test()` and `devtools::check()` clean (0 errors / 0 warnings /
      0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5
- AC5 → T6

## Tasks

- [x] **T1** — Export `GeomSsmPoint`, `GeomSsmArc`, `CoordCircumplex` with
      `@format NULL` roxygen; add a downstream-subclass test (trivial subclass of
      each, rendered). Sweep the stale `StatSsmArc` phrase from DESIGN.md:319.
- [x] **T2** — Implement `na.rm = FALSE` warn-by-count in `GeomSsmPoint$setup_data`
      and `GeomSsmArc$setup_data` (default TRUE stays silent); test all four
      geom×flag cases.
- [x] **T3** — Fix the due-East `0.5`/`LM` label overlap in `coord_circumplex()` /
      `ggcircumplex()` furniture; regenerate the affected vdiffr baseline; assert
      label separation at grob level.
- [x] **T4** — Firm up the `ssm_plot_circle(repel=)` path (non-overlapping
      labels); test.
- [x] **T5** — Add the styling/aesthetic options with output-preserving defaults;
      test the non-default path.
- [x] **T6** — `document()`; full `test()` + `check()`.

## Work log

- 2026-07-17: created by /milestone-plan (viz expansion, area B).
- 2026-07-18: re-planned by /milestone-plan (amend-via-gate). Corrected stale
  `StatSsmArc` → `GeomSsmArc` refs and DESIGN.md line citations (M31 shipped no
  Stat); added `CoordCircumplex` to the export set (DESIGN.md:330-334 deferral;
  user gate); folded in the M31 handoff (`0.5`/`LM` label overlap + repel/label
  ergonomics, archive:24-25); pinned `na.rm` to opt-in parity (default stays TRUE,
  user gate). Tasks 4→6.
- 2026-07-18: T1 — exported `GeomSsmPoint`/`GeomSsmArc`/`CoordCircumplex` under
  shared `circumplex-ggproto` Rd page (`@keywords internal`, `@format/@usage
  NULL`); swept stale `StatSsmArc ⊂ ggforce::StatArcBar` from DESIGN.md; updated
  DESIGN.md export note. Added test-ggproto-classes.R (export + subclass-render
  for all three). Viz test files green.
- 2026-07-18: T2 — `na.rm` opt-in warn-parity via shared `ssm_warn_dropped()`
  helper wired into both geoms' `setup_data` (reads `params$na.rm`; default TRUE
  silent, FALSE warns by count on NA-drops; zero-width arc stays a silent
  geometry rule). Updated both `@param na.rm` docs + DESIGN.md. 4 geom×flag test
  cases; no double-warn in `ssm_plot_circle`/`plot.circumplex_cpm`.
- 2026-07-18: T3 — fixed the due-East `0.5`/`LM` overlap: coord auto-places the
  amplitude (radial) axis in the widest spoke gap (new `ssm_r_axis_angle()`
  helper: octants→22.5°, poles→45°, 12-pt→15°; off every spoke) via
  `setup_panel_params`, with a new `r_axis_angle=` override on
  `coord_circumplex()`. Fenced at helper + built-coord level (`r_axis_inside`
  moved off theta 0, not on any spoke). Regenerated 14 canvas vdiffr baselines;
  all cartesian curve/contrast/ladder baselines byte-identical (env-fidelity
  signal, M31 lesson).
- 2026-07-18: T4 — firmed up `repel`: gated on new `has_ggrepel()` with a clear
  install-hint error (Suggests idiom), rewrote the stale "experimental" doc.
  Tests: coord-aware repel layer present + maps to amplitude/displacement;
  mocked-absent ggrepel errors by name.
- 2026-07-18: T5 — exported the canvas theme as `theme_circumplex(base_size)`
  (was internal `circumplex_theme`; DESIGN.md deferral discharged), added
  `_pkgdown.yml` row. Default path output-preserving (`ggcircumplex()` uses it;
  baselines unchanged); non-default `base_size` path tested.
- 2026-07-18: T6 — NEWS entry for the user-visible viz-ergonomics changes;
  `document()` no-diff; full `devtools::check()` clean (0/0/0, 5m25s, suite
  passes, examples + vignettes rebuilt). Status → review.
- 2026-07-18: review (amend-via-gate) — AC3 amended: the radial-axis label grobs
  have no stable handles (nested unnamed gTrees; positions in viewports), so a
  literal grob position/extent-intersection assertion would couple to ggplot2
  internal grob-naming (M31 lesson #3 fragility). Reworded to fence the robust
  built-coord property — radial axis drawn off every spoke (`r_axis_inside` =
  widest-gap midpoint), which deterministically guarantees non-overlap — plus the
  vdiffr baseline. User-approved at the review gate.

## Decisions

## Review

**Acceptance criteria — fresh evidence (2026-07-18):**

- **AC1 (exports + subclass + StatSsmArc swept):** `NAMESPACE` exports
  `GeomSsmPoint`, `GeomSsmArc`, `CoordCircumplex`; `man/circumplex-ggproto.Rd`
  aliases all three (`@format NULL` → no `\format` block, by design).
  `test-ggproto-classes.R` (4 tests) asserts the exports via
  `getNamespaceExports()` and renders a trivial subclass of each. `grep StatSsmArc
  cairn/DESIGN.md` → 0 hits. PASS.
- **AC2 (na.rm parity):** `test-geom_ssm.R` T2 block — point `na.rm=FALSE` warns
  "Removed 1 row", point `TRUE` silent, arc `FALSE` warns "Removed 2 rows", arc
  `TRUE` silent, plus a no-degenerate-no-warn case. PASS.
- **AC3 (amplitude axis off spoke; amended):** `ssm_r_axis_angle()` unit tests
  (octants→22.5, poles→45, 12-pt→15, off every spoke; degenerate fallbacks) and
  the built-coord test (`r_axis_inside` = 22.5, not in `octants() %% 360`;
  `r_axis_angle=` override honored). 14 canvas vdiffr baselines regenerated; every
  cartesian curve/contrast/ladder baseline byte-identical vs `origin/master`
  (`git diff --name-only` → none). PASS (criterion amended at this gate — see work
  log).
- **AC4 (styling defaults + non-default paths):** `theme_circumplex()` default
  equals the canvas theme (baselines unchanged) and a larger `base_size` differs;
  `r_axis_angle` default auto + override tested; `repel=TRUE` adds a coord-aware
  `GeomLabelRepel` layer (maps to amplitude/displacement) and errors by name when
  `ggrepel` is mocked absent. PASS.
- **AC5 (test + check clean):** `devtools::check(args="--no-manual")` →
  **0 errors / 0 warnings / 0 notes** (5m25s; full testthat suite [207s], examples
  `--run-donttest`, and vignette rebuild all OK). `devtools::document()` no-diff.
  PASS.

Combined targeted re-run of the four viz test files at review: 46 tests / 129
assertions, 0 fail / 0 error / 0 skip.

**Consistency gate (2026-07-18):** `cairn_validate` all-pass (incl.
coverage-complete, mirror agreement, weight caps); no DESIGN principle changed →
`cairn_impact` skipped. Toolchain: `document()` no-diff, `pkgdown::check_pkgdown()`
"No problems found", README in sync (untouched), NEWS entry present, `check()`
0/0/0.

**Independent three-lens review (2026-07-18):**
- [O] diff-bug (Opus): 1 finding (below) — else clean (auto-placement logic,
  `r_axis_inside` re-entrancy, na.rm parity, exports/roxygen all verified sound;
  LM=360/pole/seam untouched).
- [S] blame-history (Sonnet): no regressions — D-018/D-019/D-020 and M31's pinned
  invariants intact; no double-warn in the three wrapper plots; repel/theme-rename
  clean.
- [S] prior-PR-comments (Sonnet): no prior-PR evidence (merged PRs carry zero
  inline review comments; this repo reviews via cairn docs).

**Finding (scored 78 — sub-threshold, but actioned):** `r_axis_angle` validation
guarded `NA` but not `±Inf` (`coord_circumplex(r_axis_angle = Inf)` → `Inf %% 360`
= `NaN` → cryptic render-time error never naming the argument). Scorer put it at
78 (verified/real but low-severity edge case). Elected to **fix now** rather than
log-only: it is a one-line correctness guard in code M32 itself introduced —
switched the guard to `!is.finite()` (catches NA/NaN/±Inf), added `Inf`/`NA`
regression tests. The sibling `amax`/`center` guards have the identical **pre-
existing** gap (out of M32 scope) → spawned a background task
(`task_010f992f`) to guard them via `/hotfix` or the next coord-touching
milestone.
