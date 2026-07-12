# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-12 (M11 merged)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | blocked | — | high | milestones/M7-v2-release-prep.md |
| M12 | Result-label DRY + statistical-core coverage tracking | in-progress | — | normal | milestones/M12-label-dry-coverage.md |
| M11 | Boundary-coverage hardening + test-suite tidiness | done | — | normal | milestones/archive/M11-boundary-coverage-hardening.md |
| M8 | SEM-layer DRY single-sourcing | done | — | normal | milestones/archive/M8-sem-dry-single-sourcing.md |
| M9 | sem_estimate() vectorization + oracle single-sourcing | done | — | normal | milestones/archive/M9-sem-estimate-vectorize.md |
| M10 | Package-wide scalar-count validator | done | — | low | milestones/archive/M10-scalar-count-validator.md |

## Candidates

_New cairn IDs continue from the legacy maximum (M6); M7 is the first cairn-era ID. Legacy milestone/decision IDs stay valid as citations into `cairn/legacy/`._

- M6 — Longitudinal & intraindividual SSM: repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it maps to a candidate, not `planned` — replan when the v2.1.0 window opens (legacy ROADMAP "Milestone 6").
- v2.0.0 pre-release oracle re-reads: second independent human re-read of the Grassi et al. (2010) CircE and Zimmermann & Wright (2017) transcriptions before release (legacy ROADMAP "v2.0.0 pre-release items").
- Statistical follow-ups deferred to post-v2.0.0 (Fable-tier where noted): CPM convergence-acceptance vacuous "reproduced" for free-angle variants; contrast certification consistency between `ssm_ci_accuracy()` and `print.circumplex_ssm()`; guardrail certification-rule replacement (print-independent, scale-free); CIRCUM free-scaling compatibility mode for `cpm_fit()`; post-M4 publication-grade simulation study design (legacy ROADMAP M4/M5 blocks).
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): move degree/radian/contrast classes onto vctrs/S7 (IP-touching — needs a `/milestone-brief` before it is plannable); 0-vs-360 pole-snap alignment (cosmetic, D-003 parked); analytic-CI Hessian recomputation (minor perf; oracle-validate when done) (legacy ROADMAP "Continuous / infrastructure track" + deferred `/code-review` findings). _(2026-07-12: the boundary-condition test suite and the `test-RcppExport.R.R` rename → M11; Group/Measure/Label dedup + statistical-core coverage tracking → M12; "add R-devel to CI matrix" retired — already present in `.github/workflows/R-CMD-check.yaml:25`.)_
- M5 close-review follow-ups (items a–g) promoted 2026-07-12 to M8 (DRY: c/d/e/g), M9 (numeric: a/b), M10 (scalar validator: f); the remaining `is_flag()` sliver at `R/instrument_oop.R:68` was absorbed into M11 (2026-07-12). Fully dispatched.
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- SEM test-fixture consolidation (deferred from M8 T5): absorbed into M11 (2026-07-12) as the `sem_canonical_pop()` helper task.
