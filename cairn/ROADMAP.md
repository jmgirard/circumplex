# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-16 (all validate checks green; no open issues/PRs/RBs; git reconciled — nothing since M19 close)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | planned | M20, M21 | high | milestones/M7-v2-release-prep.md |
| M20 | 0-vs-360 pole CI-endpoint alignment | planned | — | high | milestones/M20-pole-endpoint-alignment.md |
| M21 | T_diag-vs-T_free inference-default decision + application | planned | — | high | milestones/M21-t-calibration-decision.md |
| M19 | CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration | done | M18 | high | milestones/archive/M19-free-family-coverage-oracle.md |
| M18 | CIRCUM free-scaling — implementation + oracle validation | done | M17 | high | milestones/archive/M18-circum-free-scaling-build.md |
| M17 | CIRCUM free-scaling — Fable-reviewed design decision + spec | done | — | high | milestones/archive/M17-circum-free-scaling-design.md |
| M16 | Print-independent, scale-free displacement-certification rule | done | — | high | milestones/archive/M16-cert-rule-replacement.md |
| M15 | Contrast certification-conditional reporting consistency (ci_accuracy ↔ print) | done | — | normal | milestones/archive/M15-contrast-cert-consistency.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal & intraindividual SSM (legacy "Milestone 6"): repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it stays a candidate — replan when the v2.1.0 window opens.
- Post-M4 publication-grade simulation study design (legacy ROADMAP M4/M5 blocks; Fable-tier). This is a research-paper track — the CPM simulation paper engine is already written in `devel/cpm-sim/` (reviewed, not yet run), separate from any package milestone. _(contrast certification consistency promoted → M15; guardrail cert-rule replacement → M16; CIRCUM free-scaling promoted → M17/M18 per D-008; "CPM convergence-acceptance vacuous reproduced" struck — already fixed in M4 review #1, tested at `test-cpm_fit.R:595`, 2026-07-12.)_
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): analytic-CI Hessian recomputation (minor perf; oracle-validate when done) (legacy ROADMAP "Continuous / infrastructure track" + deferred `/code-review` findings). _(vctrs/S7 angle-class migration dropped 2026-07-12 → D-006, RR01; 0-vs-360 pole-snap alignment promoted → M20, 2026-07-16.)_
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- CIRCUM free-scaling post-v2.0.0 extensions (deferred out of M18; see `milestones/archive/M18-circum-free-scaling-build.md` + D-009): (2) bootstrap σ CIs; (4) covariance-matrix input. The point-estimate oracle (frozen Grassi App. A + live OpenMx) already shipped in M18. _(Item (1) free-family coverage oracle promoted → M19, 2026-07-13; item (3) T_diag-vs-T_free calibration decision promoted → M21, 2026-07-16.)_
