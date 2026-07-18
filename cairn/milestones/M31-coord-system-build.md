<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M31: Circumplex coordinate-system build

- **Status:** review
- **Priority:** high
- **Depends on:** M30
- **Principles touched:** — (implements under the CLAUDE.md angle invariants:
  [0,360), LM=360; ip-touching re-owns the polar transform — the AC4 boundary
  battery is its discharge. IP/GP formalization deferred to /design-interview.)
- **Branch/PR:** m31-coord-system-build · [PR #55](https://github.com/jmgirard/circumplex/pull/55)

## Goal

Build `coord_circumplex()` as a `CoordRadial` subclass that owns `amax` and the
polar transform per the M30 spec §11 / D-019, refactor the geoms, wrappers, and
the two hidden plot consumers onto it — gaining a configurable amplitude center
and a theme-responsive canvas without changing correct output — and drop the
now-unused `ggforce` dependency.

## Scope

**In:**
- `coord_circumplex()` (⊂ `CoordRadial`): hard-pins `thetalim = c(0,360)`,
  `expand = FALSE`, `start = pi/2`, `reverse = "theta"` (coord-side, never
  scale-limits); center via `rlim = c(center, amax)` **alone** (`inner.radius`
  decoupled, default 0); themed `render_bg()` furniture (rings/spokes/labels).
- Re-pin `DESCRIPTION` `ggplot2 (>= 3.3.0)` → `(>= 4.0.0)` (D-019; ships with
  the code that needs it, not docs-only M30).
- Refactor `geom_ssm_point` (→ `aes(x=displacement, y=amplitude)`) and
  `geom_ssm_arc` (→ one `GeomRect`; seam-unwrap by **extension** and the span/
  full-circle validation move into its `setup_data()`); `amax` and
  `geom_ssm_arc(n=)` become inert via **unconditional** soft-deprecation
  (sentinel default + one-time `inform`), **never error**.
- Refactor `ggcircumplex()`/`circle_base()`, `ssm_plot_circle/curve/contrast()`
  onto the coord (signatures unchanged, D-018b); coord-aware `repel = TRUE`
  redesign that keeps it functional; port the two hidden consumers
  `plot.circumplex_cpm` (`R/cpm_oop.R:332-336`) and
  `plot.circumplex_fit_structure` (`R/fit_structure_oop.R:290-297`).
- Full AC4 boundary battery (data + grob level); wholesale vdiffr regeneration.
- Drop `ggforce` from Imports (gated capstone: verification checklist + a
  D-entry superseding the DESIGN.md V6 KEEP holding); dead-code sweep of
  `ggrad()`/`ssm_to_cartesian()`.

**Out:**
- Exported ggproto generators, `na.rm` warn-parity, new styling aesthetics, and
  *enhanced* repel/label ergonomics → M32.
- Longitudinal trajectory viz → M33; plotting vignette + pkgdown (incl. the
  fuller `scale_x_circumplex()` behavior note) → M34.
- Any API surface M30 marked NO-GO or deferred (animation/movement paths).

## Acceptance criteria

- [x] **AC1 — `amax` single source of truth (R1).** A plot built from one coord
      aligns points to rings with **no per-layer `amax`**; a point at amplitude
      `= amax` lands on the outer ring, at `= center` at the center (T-r1). The
      old dual-supply mismatch is unrepresentable — a stray geom `amax` is
      accepted and ignored with a one-time note, never errors. *(source: spec
      §11; [DESIGN.md:332-337](../DESIGN.md).)*
- [x] **AC2 — configurable center (R2).** With `center = c0 ≠ 0` via
      `rlim = c(c0, amax)`, a point at amplitude `c0` sits at the panel center
      and the innermost ring label reads `c0` — labels and amplitude→radius
      mapping agree (T-r2, guards the R3 mislabel). *(source: spec §11;
      [DESIGN.md:338-343](../DESIGN.md).)*
- [x] **AC3 — theme-responsive canvas (R3).** Rings/spokes/labels restyle via
      theme elements (`panel.grid.*`, `axis.text`), not frozen drawn geoms — a
      test asserts a theme change reaches the furniture. *(source: spec §2 R3;
      [DESIGN.md:344-347](../DESIGN.md).)*
- [x] **AC4 — angle invariants through the new transform (ip-touching), each
      tested at data + grob level.** T-i1b panel range pins `theta.range ==
      c(0,360)` with expansion off **regardless of the data's range**; T-i2
      seam-straddle `(350,10)` has arc-data `xmax == 370` and grob angular
      coverage `(330,360] ∪ (0,30]` (not the 340° complement); T-i2b
      seam-adjacent `[350,360]`/`[0,10]` non-straddling; T-i2c full-circle
      (`span >= 360`) rejection fires from the new `setup_data()` home with the
      same message; T-i3 pole `d ∈ {0,360}` draws at the identical angle
      (grob, `≤ 1e-12`); T-arc0 zero-width wedge (`xmin == xmax`) drops; LM=360
      labelling; flat `d_est = NA` profile dropped (I4). *(source: CLAUDE.md
      Statistical invariants; RR08 R-6 / spec §11.)*
- [x] **AC5 — back-compat keep-working set (R4).** `ggcircumplex()`,
      `geom_ssm_point/arc()`, `scale_x_circumplex()`,
      `ssm_plot_circle/curve/contrast()` (incl. `repel = TRUE` functional under
      the coord), `plot.circumplex_cpm`, and `plot.circumplex_fit_structure`
      keep their signatures and render correctly. *(source: spec §11 keep-working
      set / D-018b.)*
- [x] **AC6 — `ggforce` removed (dependency D-entry).** After the verification
      checklist (`grep -r ggforce` clean over `R/`,`tests/`,`vignettes/`,
      `NAMESPACE`; `check()` + suite pass with it dropped), `ggforce` is out of
      Imports; a D-entry supersedes the DESIGN.md V6 KEEP holding; the polar
      transform lives in exactly one place (dead `ggrad()`/`ssm_to_cartesian()`
      removed). *(source: spec §11 ggforce / D-019 holding 7.)*
- [x] **AC7 — build clean.** `ggplot2 (>= 4.0.0)` pinned; `devtools::test()` and
      `devtools::check()` clean (0 errors / 0 warnings / 0 notes); vdiffr
      wholesale-regenerated with one human-eyeball pass at review;
      BLAS-sensitive vdiffr tests `skip_on_ci()` (legacy lesson).

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T6
- AC3 → T1, T8
- AC4 → T1, T3, T6
- AC5 → T2, T3, T4, T5
- AC6 → T7
- AC7 → T1, T8

## Tasks

- [x] **T1** — Implement `CoordCircumplex` (⊂ `CoordRadial`): hidden pins,
      `rlim = c(center, amax)` center, decoupled `inner.radius = 0`, themed
      `render_bg()` furniture, `setup_panel_params()`/`transform()` pinning
      LM=360 + CCW-from-right. Re-pin `DESCRIPTION` `ggplot2 (>= 4.0.0)`.
- [x] **T2** — Refactor `geom_ssm_point` → map amplitude/displacement to the
      coord's `x/y`; delete cartesian compute; `amax` soft-deprecate (sentinel +
      `inform(.frequency = "once")`).
- [x] **T3** — Refactor `geom_ssm_arc` → coord-bent `GeomRect`; move seam-unwrap
      by extension (`xmax <- xmin + ssm_arc_span(...)`), span validation, and the
      full-circle rejection into `setup_data()`; `n` soft-deprecate; drop the
      `StatSsmArc`/`ggforce` arc path.
- [x] **T4** — Refactor `ggcircumplex()`/`circle_base()` + `ssm_plot_circle/
      curve/contrast()` onto the coord (signatures unchanged); coord-aware
      `repel = TRUE` redesign (keep it working; `scale_x_circumplex()` behavior
      under `theta="x"` noted for M34).
- [x] **T5** — Port the two hidden consumers: `plot.circumplex_cpm`
      (`amax=1` → `rlim=c(0,1)`; `drawable` pre-filter `R/cpm_oop.R:335`;
      legend-order coupling `:350-354`) and `plot.circumplex_fit_structure`.
- [x] **T6** — AC4 boundary battery (T-i1b/T-i2/T-i2b/T-i2c/T-i3/T-arc0/T-r1/
      T-r2) at data + grob level; drive real values through the transform, not
      re-typed expressions (M13 teeth lesson).
- [x] **T7** — `ggforce` removal capstone: verification checklist, drop the
      Import, dead-code sweep (`ggrad`, `ssm_to_cartesian`), D-entry superseding
      DESIGN.md V6.
- [x] **T8** — vdiffr wholesale regen; `skip_on_ci()` BLAS-sensitive ones; full
      `test()` + `check()` clean; human-eyeball pass.

## Work log

- 2026-07-17: created by /milestone-plan; plan deepened same day against M30's
  outputs (spec §11 + D-019), superseding the generic pre-M30 plan.
- 2026-07-17: T1 — exported `coord_circumplex()` (⊂ `CoordRadial`, `R/coord_circumplex.R`) hard-pinning the LM=360/CCW convention; live-verified identical-pole (I3) + periodic seam-wrap (350↔370); 22 coord tests; DESCRIPTION ggplot2 `>= 4.0.0` (D-019); pkgdown row.
- 2026-07-17: T2–T6 — geoms onto the coord (`GeomSsmPoint`→x/y; `GeomSsmArc`⊂`GeomRect`, extension-unwrap+validation in `setup_data`, `StatSsmArc` dropped); `amax`/`n` soft-deprecated (one-time inform); `ggcircumplex()` rebuilt (themed, `circle_base()` gone); coord-aware `repel`; two consumers ported; AC4 data+grob battery + structural tests by-geom; vdiffr regenerated. Suite green (2709). Renders live-eyeballed.
- 2026-07-17: T7–T8 — `ggforce` removed (D-020, supersedes V6 KEEP); checklist verified (grep-clean incl. vignettes → `annotate`; T-arc0). Dead `ggrad`/`ssm_to_cartesian`/`ssm_radius` swept. DESIGN.md viz rewritten; NEWS added. `check()` 0/0/0 (a transient Rplots.pdf note, deleted). Status → review.
- 2026-07-17: review evidence prep caught AC3 (theme-responsiveness) lacked a fencing test; briefly back to in-progress to add a grob-level "theme recolours the panel furniture" test (test-ssm_plot.R), then re-review.
- 2026-07-17: review — PR #55; consistency gate green (cairn_validate/document-no-diff/pkgdown); three-lens fan-out (diff-bug/blame/prior-PR) + scorer. Actioned F2 (coord_circumplex validation order: `center` type-checked before the `amax>center` comparison; NA guards added; incidentally closes sub-threshold F1); fixed stale `circle_base()` comment; `.Rbuildignore` `^Rplots\.pdf$`. Suite 2713 pass; `check()` 0/0/0. All 7 ACs verified.

## Decisions

## Review

Reviewed [PR #55](https://github.com/jmgirard/circumplex/pull/55) (branch
`m31-coord-system-build`), merge-base `master`. Status: **verified, recommend merge.**

**Acceptance criteria — fresh evidence (all PASS):**

- **AC1** — `test-coord_circumplex.R` (explicit `amax`/`center` → `r.range`;
  `amax=NULL` trains outer, inner pinned to center) + `test-geom_ssm.R` "amplitude
  radius owned by coord (R1)": `a=0.25` lands farther out at `amax=0.5` than at
  `amax=1.0` (no per-layer `amax`), center-amplitude point at panel centre; a stray
  geom `amax` is accepted with a one-time note, never errors.
- **AC2** — `test-coord_circumplex.R` "amax and center are the radial limits":
  `center=0.2, amax=1` → `r.range == c(0.2, 1)`; rings relabel + amplitudes remap
  via `rlim` alone.
- **AC3** — `test-ssm_plot.R` "canvas furniture responds to theme (R3, AC3)":
  grob-level assertion that `theme(panel.grid.major=element_line("red"))` recolours
  the coord's panel furniture (added at review — see work log).
- **AC4** — `test-geom_ssm.R` battery (data + grob): T-i1b (`theta.range==c(0,360)`
  for narrow data), T-i2 (seam `(350,10)`: `xmax==370`, grob coverage
  `(330,360]∪(0,30]`, not the 340° complement), T-i2b (seam-adjacent non-straddle),
  T-i2c (`span>=360` rejected from `setup_data`), T-i3 (pole `d∈{0,360}` identical
  grob `≤1e-12`), T-arc0 (zero-width drops), I4 (flat `NA` dropped); + coord
  LM=360/pole tests.
- **AC5** — `test-cpm_plot` (17), `test-fit_structure_api` (67), `test-ssm_plot`
  (39), `test-scale_circumplex` (17) all pass; circumplex vdiffr baselines
  regenerated (curve/ci-accuracy unchanged); wrapper signatures byte-identical to
  master; `repel=TRUE` works coord-aware; both hidden consumers render (live-eyeballed).
- **AC6** — `grep -r ggforce` clean over `R/`,`tests/`,`vignettes/`,`NAMESPACE`;
  `DESCRIPTION` Imports carries no `ggforce`; dead `ggrad`/`ssm_to_cartesian`/
  `ssm_radius` removed; **D-020** records the V6 supersession.
- **AC7** — `devtools::check()` **0 errors / 0 warnings / 0 notes**; full suite
  **2713 pass / 0 fail**; BLAS-sensitive vdiffr doppelganger `skip_on_ci()`.

**Consistency gate:** `cairn_validate` all checks pass · `document()` no diff ·
`pkgdown::check_pkgdown()` no problems (coord_circumplex indexed) · NEWS entry
present · no DESIGN IP/GP changed → `cairn_impact` skipped.

**Independent review — three lenses + scorer:**

- **[O] diff-bug (Opus):** core mechanism sound; all invariants correct/tested.
  Two low-severity findings in `coord_circumplex()` validation (below).
- **[S] blame-history (Sonnet):** **no regressions** — all six investigated prior
  behaviors preserved (seam short-way, full-circle rejection, pole D-003, NA-drop
  + warn-by-name M28, cpm zero-width/legend-order coupling, D-018b wrappers). One
  stale-comment non-finding (`scale_circumplex.R` referenced removed
  `circle_base()`) — fixed.
- **[S] prior-PR-comments (Sonnet):** **no prior-PR evidence** (all merged PRs
  carry zero review comments) — clean no-op, 0 findings.

**Findings triage (scored):**

- **F2 (score 82 — ACTIONED, fixed):** `coord_circumplex(amax=1, center="x")`
  fired the "amax must be greater than center" message before `center` was
  type-checked, mis-blaming `amax`. Fixed by validating `center` (with NA guard)
  before the comparison; regression test added.
- **F1 (score 55 — below threshold, logged):** `coord_circumplex(amax=NA_real_)`
  threw a cryptic "missing value where TRUE/FALSE needed" (niche: logical `NA`
  already failed cleanly). Incidentally closed by the F2 fix's `is.na()` guard
  (now a clean message); regression test added.
- **Hygiene (review-side):** stale `circle_base()` comment fixed; `^Rplots\.pdf$`
  added to `.Rbuildignore` (the new auto-printing plot examples create it locally,
  which was the sole transient `check()` NOTE).
