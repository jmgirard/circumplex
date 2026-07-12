# M13: Angle-class S3 follow-ups (RR01) — done 2026-07-12

- **Outcome:** shipped the four RR01/D-006 S3-local follow-ups (PR #37,
  squash `95936f2`); no user-visible behavior change. `check()` clean
  (0 errors / 0 warnings / 0 notes), full CI green.
- `new_contrast_radian()` constructor single-sources the `circumplex_contrast_radian`
  tag; the two inline `structure()` sites (`ssm_bootstrap.R`, `ssm_ci_accuracy.R`)
  route through it — byte-identical (pinned by `expect_identical`).
- All-NA return of both `quantile.circumplex_*` methods normalized to `NA_real_`
  (length-1 preserved for the `ssm_ci_accuracy.R` guard); documented CI output unchanged.
- CPM bootstrap angle-CI path (`cpm_fit.R:1119`) now oracle-guarded: a deterministic
  0/360 pole-straddle through `cpm_fit()` asserts the CI wraps; verified red under a
  temporary call-site linearization (teeth).
- **M13-D1:** `as_degree`/`as_radian` kept deliberately internal (generics unexported,
  methods S3-registered); no D-entry (changes no exported surface).

## Review
- Round 1 sent back on **F1** (score 93): the AC3 test's `jz2017` fixture had no
  pole-straddle, so a linearized quantile passed — no teeth. Fixed by the T3 redo.
- Round 2 clean: diff-bug no findings; blame-history **F2** (add `skip_on_ci()` per
  BLAS-snapshot precedent) scored **35, not actioned** — this test's threshold
  assertion has a ~25–44° margin (robust to ≫BLAS perturbations) and Windows CI passed.
