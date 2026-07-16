# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-16 (candidate cleanup: promoted/struck breadcrumbs trimmed, sim-study row reworded to paper-track pointer, D-006's RR01 S3 follow-ups folded into the infra row)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | planned | M20, M21 | high | milestones/M7-v2-release-prep.md |
| M20 | 0-vs-360 pole CI-endpoint alignment | done | — | high | milestones/archive/M20-pole-endpoint-alignment.md |
| M21 | T_diag-vs-T_free inference-default decision + application | done | — | high | milestones/archive/M21-t-calibration-decision.md |
| M19 | CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration | done | M18 | high | milestones/archive/M19-free-family-coverage-oracle.md |
| M18 | CIRCUM free-scaling — implementation + oracle validation | done | M17 | high | milestones/archive/M18-circum-free-scaling-build.md |
| M17 | CIRCUM free-scaling — Fable-reviewed design decision + spec | done | — | high | milestones/archive/M17-circum-free-scaling-design.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal & intraindividual SSM (legacy "Milestone 6"): repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it stays a candidate — replan when the v2.1.0 window opens.
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done); seed the free engine's multi-start with the unit solution to enforce T_free ≤ T_unit nesting by construction (RR05 B2/R5, 2026-07-16); RR01 S3-local follow-ups per D-006 — a `new_contrast_radian()` constructor for the two inline `structure()` sites, deciding export status of the internal `as_degree`/`as_radian` generics, and `NA_real_` all-NA return + CPM angle-CI oracle path when the quantile methods are next touched.
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): (2) bootstrap σ CIs; (4) covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
