# M8: SEM-layer DRY single-sourcing (done)

- **Shipped:** 2026-07-12 · PR #32 (squash `a7cf4cc`) · merged to master.
- **Goal:** single-source the SEM layer's duplicated contrast-arity, fit,
  label, and strict-tier logic without changing exported behaviour.

## Outcome
Behaviour-preserving DRY refactor of `R/ssm_sem.R` (M5 items c/d/e/g). Five
concerns each collapsed to one place:
- `sem_check_contrast_arity()` — three duplicated arity `stop()` blocks.
- `sem_fit_cfa()` — the two `lavaan::cfa()` sites (fiml/listwise + multi-group
  `group.label` ordering).
- `sem_detail_labels()` — the `summary()` method/missing → label mapping.
- `sem_strict_metric_vacuous()` — the `strict && metric` rule (3 ladder sites).
- `score_type` into `sem_details()` (new `path` param); dead `npar` removed.

Added contrast-arity + label-seam tests. Suite green; `check()` clean; two-lens
independent review found nothing.

## Decisions

- T4 narrowed (user-gated): cross-branch strict *emission* unification descoped
  (byte-pinned, structurally distinct emitters) → ROADMAP candidate.
- Test-fixture consolidation deferred → ROADMAP candidate (not in AC5).
- Base `stop(call.=FALSE)` kept (not `cli_abort()`) to preserve messages.
