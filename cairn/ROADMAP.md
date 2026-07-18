# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-18 (M35 done + archived, PR #58 — `ssm_plot_trajectory()` gains a `data.frame` method for model-based trajectory tables on a continuous time axis; growth vignette figure retired. M29 terminal row pruned per retention. Terminal-row retention keeps the 5 most recent: M30–M33, M35)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | planned | M25, M26, M27, M31, M32, M33, M34, M35 | high | milestones/M7-v2-release-prep.md |
| M30 | Circumplex coordinate-system design (Fable-reviewed) | done | — | high | milestones/archive/M30-coord-system-design.md |
| M31 | Circumplex coordinate-system build | done | M30 | high | milestones/archive/M31-coord-system-build.md |
| M32 | Circumplex geom & layer ergonomics | done | M31 | normal | milestones/archive/M32-geom-ergonomics.md |
| M33 | Longitudinal trajectory visualization (occasions objects) | done | — | high | milestones/archive/M33-trajectory-visualization.md |
| M34 | Plotting vignette + pkgdown reference | in-progress | M31, M32, M33 | normal | milestones/M34-plotting-vignette-pkgdown.md |
| M35 | Model-based trajectory plotting (`ssm_draws()` tables) | done | M33 | normal | milestones/archive/M35-model-based-trajectory.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal deferrals (D-013 spec lineage; Builds A/B/C promoted → M25/M26/M27, 2026-07-16; §1.1 sugar → M28 and §1.4 `ssm_ci_accuracy()` occasions → M29, 2026-07-17). Remaining, each with standing rationale against building now: pairwise-deletion occasions semantics (spec §1.3 — ships listwise-only on RR06 R6 estimand grounds; reopening needs a use case + a superseding D-entry); occasions × measures correlation path (spec §1.2a — defer until a concrete use case exists); occasions × contrast × grouping difference-of-differences (spec §1.2b — a new estimand needing its own design pass, i.e. a design milestone before any build); Stan companion (spec §5.4 stay-out criteria, revisit only if ≥2 of 3 conditions hold).
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done); `coord_circumplex()` `amax`/`center` non-finite guard (2026-07-18, M32 review) — both accept `±Inf` because `is.na(Inf)` is FALSE, surfacing only as a cryptic render-time error; apply the `!is.finite()` treatment M32 gave `r_axis_angle` (see LESSONS 2026-07-18), or ship as a `/hotfix`. Also (2026-07-18, M33 review, scored 45): the `ssm_plot_*` trio carries no `@family` tag, so `ssm_plot_trajectory()`'s "See Also" cross-links only to `plot.circumplex_ci_accuracy()` instead of its three siblings — a docs fix for M34, which owns the reference index. Also (2026-07-18, M35): in
  `ssm_plot_trajectory()`'s "Displacement interpretable" legend, the `FALSE` key
  renders as a label with **no glyph** whenever the data contain no uncertified
  point — `scale_shape_manual(limits=, drop = FALSE)` keeps the break but
  ggplot2 draws key glyphs only for values present in the layer data, and an
  `override.aes` `shape` vector does not reach it; affects both the occasions
  and table paths (cosmetic, M33-inherited, found by render-and-inspect). (Nesting seed promoted → M22, 2026-07-16. The RR01 S3-follow-ups clause re-added by the 2026-07-16 cleanup was stale — all four items, including the as_degree/as_radian keep-internal decision M13-D1, had already shipped in M13, PR #37 squash 95936f2; struck.)
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): bootstrap σ CIs; covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
- Plotting/visualization expansion (2026-07-17): promoted → M30–M34 and folded into v2.0.0 (D-018); `ssm_plot_*` retained as convenience wrappers (D-018b). One remainder deferred from M33: **on-circle animated/arrow movement paths** across occasions (paths/arrows connecting a profile's (a, d) position over occasions on the circular canvas) — revisit after the trajectory viz ships and the coord rewrite (M31) settles the canvas API.
