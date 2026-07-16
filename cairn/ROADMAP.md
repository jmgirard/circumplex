# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-16 (candidate cleanup pass 2: dropped dangling M18 numerals; pass 3 at M22 planning: struck the stale RR01-follow-ups clause — shipped in M13 — and promoted the nesting seed → M22)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | planned | M22 | high | milestones/M7-v2-release-prep.md |
| M22 | Free-engine multi-start nesting seed (T_free ≤ T_unit by construction) | review | — | high | milestones/M22-free-multistart-nesting-seed.md |
| M20 | 0-vs-360 pole CI-endpoint alignment | done | — | high | milestones/archive/M20-pole-endpoint-alignment.md |
| M21 | T_diag-vs-T_free inference-default decision + application | done | — | high | milestones/archive/M21-t-calibration-decision.md |
| M19 | CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration | done | M18 | high | milestones/archive/M19-free-family-coverage-oracle.md |
| M18 | CIRCUM free-scaling — implementation + oracle validation | done | M17 | high | milestones/archive/M18-circum-free-scaling-build.md |
| M17 | CIRCUM free-scaling — Fable-reviewed design decision + spec | done | — | high | milestones/archive/M17-circum-free-scaling-design.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal & intraindividual SSM (legacy "Milestone 6"): repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it stays a candidate — replan when the v2.1.0 window opens.
- CPM simulation paper (research-paper track, not a package milestone): the engine is written, reviewed, and ratified in `devel/cpm-sim/` (registered plan `devel/cpm-simulation-paper-plan.md`; not yet run). A package-side milestone (e.g., sim-backed guidance in docs/vignettes) is replanned only if the paper's results motivate one.
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done). (Nesting seed promoted → M22, 2026-07-16. The RR01 S3-follow-ups clause re-added by the 2026-07-16 cleanup was stale — all four items, including the as_degree/as_radian keep-internal decision M13-D1, had already shipped in M13, PR #37 squash 95936f2; struck.)
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): bootstrap σ CIs; covariance-matrix input — shipping it re-triggers the D-011 paired T calibration at non-unit σ truths first. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18.
