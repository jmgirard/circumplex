# M11: Boundary-coverage hardening + test-suite tidiness (done)

- **Merged:** 2026-07-12 · PR https://github.com/jmgirard/circumplex/pull/35
- Internal/test-only; no exported-behaviour change. `check()` clean (0/0/0);
  387 tests; both independent reviewers 0 findings.

## Outcome

- **Boundary coverage matrix** (`cairn/boundary-coverage.md`): 4 invariant
  classes (0/360 peak, straddling CI, ±180° contrast, flat) × 6 entry points
  (`ssm_analyze` mean/correlation, bootstrap, Monte Carlo, `ssm_ci_accuracy`,
  SEM), every cell cited by `file:line`. Mean & SEM paths already complete; the
  real gaps were on the **correlation** path.
- **Two new deterministic tests** (`test-ssm_analysis.R`): correlation-path flat
  → NA displacement; correlation-path profile peaking at the 0/360 pole.
- **SEM fixture DRY**: `sem_canonical_pop()` replaced 15 inline canonical-pop
  rebuilds in `test-ssm_sem.R`, bit-identical (`identical()`-guarded), no re-pin.
- **Tidiness**: `scales()` validates `items` via `is_flag()` (+ error test);
  `test-RcppExport.R.R` → `test-RcppExports.R`.

## Notes

- T3 regressed one block that rebuilt `lambda1` from `a`/`cc`; `load_all()`+
  `test()` masked the error, clean-env `check()` caught it → LESSONS.md.
