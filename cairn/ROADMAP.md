# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-12 (M14 planned: instruments() list automation)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | blocked | — | high | milestones/M7-v2-release-prep.md |
| M14 | Automate the instruments() list | review | — | normal | milestones/M14-instruments-list-automation.md |
| M13 | Angle-class S3 follow-ups (RR01) | done | — | normal | milestones/archive/M13-angle-class-s3-followups.md |
| M12 | Result-label DRY + statistical-core coverage tracking | done | — | normal | milestones/archive/M12-label-dry-coverage.md |
| M11 | Boundary-coverage hardening + test-suite tidiness | done | — | normal | milestones/archive/M11-boundary-coverage-hardening.md |
| M8 | SEM-layer DRY single-sourcing | done | — | normal | milestones/archive/M8-sem-dry-single-sourcing.md |
| M9 | sem_estimate() vectorization + oracle single-sourcing | done | — | normal | milestones/archive/M9-sem-estimate-vectorize.md |

## Candidates

_Candidates carry no milestone ID — an `M<NN>` is assigned only at planning time. Legacy milestone/decision IDs (≤ M6) remain valid citations into `cairn/legacy/`; M7 is the first cairn-era ID._

- Longitudinal & intraindividual SSM (legacy "Milestone 6"): repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it stays a candidate — replan when the v2.1.0 window opens.
- v2.0.0 pre-release oracle re-reads: second independent human re-read of the Grassi et al. (2010) CircE and Zimmermann & Wright (2017) transcriptions before release (legacy ROADMAP "v2.0.0 pre-release items").
- Statistical follow-ups deferred to post-v2.0.0 (Fable-tier where noted): CPM convergence-acceptance vacuous "reproduced" for free-angle variants; contrast certification consistency between `ssm_ci_accuracy()` and `print.circumplex_ssm()`; guardrail certification-rule replacement (print-independent, scale-free); CIRCUM free-scaling compatibility mode for `cpm_fit()`; post-M4 publication-grade simulation study design (legacy ROADMAP M4/M5 blocks).
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): 0-vs-360 pole-snap alignment (cosmetic, D-003 parked); analytic-CI Hessian recomputation (minor perf; oracle-validate when done) (legacy ROADMAP "Continuous / infrastructure track" + deferred `/code-review` findings). _(vctrs/S7 angle-class migration dropped 2026-07-12 → D-006, RR01.)_
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
