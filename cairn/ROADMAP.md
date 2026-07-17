# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-17 (M27 done + archived, PR #51; M22 terminal row pruned per retention — archive file stays authoritative)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M25 | Longitudinal Build A — occasions API + paired contrasts | done | — | high | milestones/archive/M25-occasions-core.md |
| M26 | Longitudinal Build B — per-person layer + draws adapter + Bayesian vignette | done | — | high | milestones/archive/M26-perperson-draws-adapter.md |
| M27 | Longitudinal Build C — growth-model support on displacement | done | M25, M26 | high | milestones/archive/M27-growth-model-support.md |
| M7 | v2.0.0 CRAN release preparation | planned | M25, M26, M27 | high | milestones/M7-v2-release-prep.md |
| M24 | Tidyverse NSE in the user API — evaluation + standing decision | done | — | normal | milestones/archive/M24-nse-evaluation.md |
| M23 | Longitudinal & intraindividual SSM — Fable-reviewed design + build-ready spec | done | — | high | milestones/archive/M23-longitudinal-ssm-design.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal deferrals (D-013 spec lineage; Builds A/B/C promoted → M25/M26/M27, 2026-07-16): pairwise-deletion occasions semantics (spec §1.3); occasions × measures correlation path and occasions × contrast × grouping difference-of-differences (spec §1.2); `ssm_ci_accuracy()` occasions extension (spec §1.4); `ssm_analyze_long()` sugar (spec §1.1); Stan companion per spec §5.4 stay-out criteria.
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done). (Nesting seed promoted → M22, 2026-07-16. The RR01 S3-follow-ups clause re-added by the 2026-07-16 cleanup was stale — all four items, including the as_degree/as_radian keep-internal decision M13-D1, had already shipped in M13, PR #37 squash 95936f2; struck.)
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): bootstrap σ CIs; covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
- Plotting/visualization expansion on the `ggcircumplex()` ggplot2 extension (2026-07-17): broaden the plotting tools, options, and docs built on the package's ggplot2 extension layer (`ggcircumplex()`, `geom_ssm_arc()`, `geom_ssm_point()`, `scale_x_circumplex()`) — richer geoms/aesthetics/options and a plotting-focused vignette / pkgdown coverage. Scope, API surface, and whether it supersedes any older `ssm_plot_*` functions to be set at planning time.
