# circumplex Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Last hygiene check: 2026-07-12 (cairn-init migration)_

Pre-migration history: see `cairn/legacy/` and git log.

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M7 | v2.0.0 CRAN release preparation | blocked | — | high | milestones/M7-v2-release-prep.md |
| M8 | SEM-layer DRY single-sourcing | review | — | normal | milestones/M8-sem-dry-single-sourcing.md |
| M9 | sem_estimate() vectorization + oracle single-sourcing | planned | — | normal | milestones/M9-sem-estimate-vectorize.md |
| M10 | Package-wide scalar-count validator | planned | — | low | milestones/M10-scalar-count-validator.md |

## Candidates

_New cairn IDs continue from the legacy maximum (M6); M7 is the first cairn-era ID. Legacy milestone/decision IDs stay valid as citations into `cairn/legacy/`._

- M6 — Longitudinal & intraindividual SSM: repeated-measures/intraindividual SSM, paired/dependent circular resampling, growth models on displacement, optional Bayesian; deliberately deferred to its own ~v2.1.0 after a design brief. Scope defined but no acceptance criteria/tasks written, so it maps to a candidate, not `planned` — replan when the v2.1.0 window opens (legacy ROADMAP "Milestone 6").
- v2.0.0 pre-release oracle re-reads: second independent human re-read of the Grassi et al. (2010) CircE and Zimmermann & Wright (2017) transcriptions before release (legacy ROADMAP "v2.0.0 pre-release items").
- Statistical follow-ups deferred to post-v2.0.0 (Fable-tier where noted): CPM convergence-acceptance vacuous "reproduced" for free-angle variants; contrast certification consistency between `ssm_ci_accuracy()` and `print.circumplex_ssm()`; guardrail certification-rule replacement (print-independent, scale-free); CIRCUM free-scaling compatibility mode for `cpm_fit()`; post-M4 publication-grade simulation study design (legacy ROADMAP M4/M5 blocks).
- Continuous / infrastructure refactors (fold into the milestone that next touches the code): dedup Group/Measure/Label construction; move degree/radian/contrast classes onto vctrs/S7; rename `test-RcppExport.R.R`; boundary-condition test suite; add R-devel to CI matrix; statistical-core coverage tracking; 0-vs-360 pole-snap alignment (cosmetic); analytic-CI Hessian recomputation (minor perf) (legacy ROADMAP "Continuous / infrastructure track" + deferred `/code-review` findings).
- M5 close-review follow-ups (items a–g) promoted 2026-07-12 to M8 (DRY: c/d/e/g), M9 (numeric: a/b), M10 (scalar validator: f). Remaining sliver: the `is_flag()` length-1-logical sibling at `R/instrument_oop.R:68` (a different predicate from scalar-count) — fold into whatever milestone next touches that validation.
- Strict-tier syntax *emission* single-sourcing across the single/multi-group branches in `R/ssm_sem_syntax.R` (descoped from M8 T4, 2026-07-12): the two emitters are structurally distinct (plain vs `c()`-vector cross-group labels) and byte-pinned by `exp_strict_*` snapshots — a high-risk rewrite of statistical output for minor DRY gain. Revisit only if that emitter is reworked for another reason.
- SEM test-fixture consolidation (deferred from M8 T5, 2026-07-12): the canonical 8-scale population (`a=rep(0.55,8)`, `cc=rep(0.6,8)`, `theta=seq(0.3,0.6,length.out=8)`, `sem_pop(...oct...)`) is rebuilt in ~17 `test-ssm_sem*.R` blocks; hoist to a shared helper. Pure test tidiness, no behavioural payoff — fold into the next milestone touching those tests.
