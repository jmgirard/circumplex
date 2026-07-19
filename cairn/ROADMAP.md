# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-18 (M37 done + archived, PR #61 — `geom_ssm_path()` draws movement across occasions on the circumplex canvas, with a `ssm_plot_circle(path = )` wrapper; the seam is crossed the short way and an undefined occasion breaks the path. Review removed the `order` aesthetic, which ggplot2's `add_group()` fragmented into a zeroGrob (M37-D2), and fixed a `drop_lowfit` bypass. M32 terminal row pruned per retention (keeps M33–M37). Earlier: M36 done + archived, PR #60 — certification legend draws both keys, `coord_circumplex()` rejects non-finite `amax`/`center`. Earlier: M34 done + archived, PR #59 — plotting vignette rewritten over the M31–M33 API. Terminal-row retention keeps the 5 most recent: M33–M37)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | planned | M25, M26, M27, M31, M32, M33, M34, M35, M36, M37, M38 | high | milestones/M7-v2-release-prep.md |
| M38 | Guaranteed rim ring for the circumplex canvas | planned | — | normal | milestones/M38-rim-ring-guarantee.md |
| M36 | Visualization polish — certification legend key + non-finite guards | done | — | normal | milestones/archive/M36-viz-polish-legend-guards.md |
| M37 | On-circle movement paths across occasions | done | M31, M32, M33 | normal | milestones/archive/M37-on-circle-movement-paths.md |
| M33 | Longitudinal trajectory visualization (occasions objects) | done | — | high | milestones/archive/M33-trajectory-visualization.md |
| M34 | Plotting vignette + pkgdown reference | done | M31, M32, M33 | normal | milestones/archive/M34-plotting-vignette-pkgdown.md |
| M35 | Model-based trajectory plotting (`ssm_draws()` tables) | done | M33 | normal | milestones/archive/M35-model-based-trajectory.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal deferrals (D-013 spec lineage; Builds A/B/C promoted → M25/M26/M27, 2026-07-16; §1.1 sugar → M28 and §1.4 `ssm_ci_accuracy()` occasions → M29, 2026-07-17). Remaining, each with standing rationale against building now: pairwise-deletion occasions semantics (spec §1.3 — ships listwise-only on RR06 R6 estimand grounds; reopening needs a use case + a superseding D-entry); occasions × measures correlation path (spec §1.2a — defer until a concrete use case exists); occasions × contrast × grouping difference-of-differences (spec §1.2b — a new estimand needing its own design pass, i.e. a design milestone before any build); Stan companion (spec §5.4 stay-out criteria, revisit only if ≥2 of 3 conditions hold).
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done). (The `coord_circumplex()` `amax`/`center` non-finite guard and the `ssm_plot_trajectory()` missing-legend-glyph defect were promoted → M36, 2026-07-18.) (Nesting seed promoted → M22, 2026-07-16. The RR01 S3-follow-ups clause re-added by the 2026-07-16 cleanup was stale — all four items, including the as_degree/as_radian keep-internal decision M13-D1, had already shipped in M13, PR #37 squash 95936f2; struck.)
- Rim-ring and bare-coord-vignette-figure candidates (added 2026-07-18 out of the PR #62 hotfix) promoted → M38 the same day; the vignette-figure item rides along as M38's consumer-side scope rather than a separate milestone.
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): bootstrap σ CIs; covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
- Plotting/visualization expansion (2026-07-17): promoted → M30–M34 and folded into v2.0.0 (D-018); `ssm_plot_*` retained as convenience wrappers (D-018b). The M33 on-circle movement-path remainder was promoted → M37 (2026-07-18) as **static arrowed paths**; its revisit conditions (trajectory viz shipped, M31 canvas API settled) both held. **True animation** (gganimate or otherwise) was excluded from M37 at that plan gate on minimal-deps + custom-ggproto-coord grounds and stays a candidate: revisit only if a concrete teaching use case appears, and only via the dependency gate — no new Import or Suggests without a D-entry.
