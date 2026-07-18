# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-18 (M31 done + archived, PR #55 — coord_circumplex() build; ggforce removed / D-020. M26 terminal row pruned per retention. Terminal-row retention keeps the 5 most recent: M27–M31)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M27 | Longitudinal Build C — growth-model support on displacement | done | M25, M26 | high | milestones/archive/M27-growth-model-support.md |
| M7 | v2.0.0 CRAN release preparation | planned | M25, M26, M27, M31, M32, M33, M34 | high | milestones/M7-v2-release-prep.md |
| M28 | Occasions long-format sugar (`ssm_analyze_long()`) | done | M25 | normal | milestones/archive/M28-occasions-long-format-sugar.md |
| M29 | `ssm_ci_accuracy()` occasions extension | done | M25 | normal | milestones/archive/M29-ci-accuracy-occasions.md |
| M30 | Circumplex coordinate-system design (Fable-reviewed) | done | — | high | milestones/archive/M30-coord-system-design.md |
| M31 | Circumplex coordinate-system build | done | M30 | high | milestones/archive/M31-coord-system-build.md |
| M32 | Circumplex geom & layer ergonomics | in-progress | M31 | normal | milestones/M32-geom-ergonomics.md |
| M33 | Longitudinal trajectory visualization | planned | — | high | milestones/M33-trajectory-visualization.md |
| M34 | Plotting vignette + pkgdown reference | planned | M31, M32, M33 | normal | milestones/M34-plotting-vignette-pkgdown.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal deferrals (D-013 spec lineage; Builds A/B/C promoted → M25/M26/M27, 2026-07-16; §1.1 sugar → M28 and §1.4 `ssm_ci_accuracy()` occasions → M29, 2026-07-17). Remaining, each with standing rationale against building now: pairwise-deletion occasions semantics (spec §1.3 — ships listwise-only on RR06 R6 estimand grounds; reopening needs a use case + a superseding D-entry); occasions × measures correlation path (spec §1.2a — defer until a concrete use case exists); occasions × contrast × grouping difference-of-differences (spec §1.2b — a new estimand needing its own design pass, i.e. a design milestone before any build); Stan companion (spec §5.4 stay-out criteria, revisit only if ≥2 of 3 conditions hold).
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done). (Nesting seed promoted → M22, 2026-07-16. The RR01 S3-follow-ups clause re-added by the 2026-07-16 cleanup was stale — all four items, including the as_degree/as_radian keep-internal decision M13-D1, had already shipped in M13, PR #37 squash 95936f2; struck.)
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): bootstrap σ CIs; covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
- Plotting/visualization expansion (2026-07-17): promoted → M30–M34 and folded into v2.0.0 (D-018); `ssm_plot_*` retained as convenience wrappers (D-018b). One remainder deferred from M33: **on-circle animated/arrow movement paths** across occasions (paths/arrows connecting a profile's (a, d) position over occasions on the circular canvas) — revisit after the trajectory viz ships and the coord rewrite (M31) settles the canvas API.
