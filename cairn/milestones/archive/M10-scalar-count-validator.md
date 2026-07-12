# M10: Package-wide scalar-count validator (done)

- **Shipped:** 2026-07-12 · PR #34 (squash `a1bee33`) · merged to master.
- **Goal:** one scalar-count predicate in `R/utils.R`, adopted uniformly across
  the `ssm_analyze()` / `cpm_fit()` / `ssm_sem()` families; resolve the two
  divergent readings of the CLAUDE.md `is_*()` rule with a recorded decision.

## Outcome
Validation-only, strictly stricter. New internal `is_scalar_count(x, min=1L)`
(scalar sibling of `is_count()`; length-1 like `is_flag()`, `min` floor, returns
`FALSE` not `NA`), adopted at **20 call sites** across `ssm_analysis`,
`ssm_ci_accuracy`, `cpm_fit`, `ssm_sem`, `ssm_sem_syntax` — replacing bolted
`length(x)==1`, inline `is.numeric && ceiling==floor` (no length guard), and the
`is_num(n=1L)+is_count` stack. Count args that used to accept a length>1 vector
now abort. Regression tests for all families; suite green; `check()` 0/0/0.

## Decisions
- **D-005**: length lives in the predicate name/argument, never at the call site;
  `is_count()` retained only as the internal `n=` guard.
- Gate: new `is_scalar_count()` (not extend `is_count()`); scope amended to the
  `is_count()`-only cpm_fit sites lacking a length guard.
- Review caught the Goal names `ssm_analyze()` but its `boots`/`ncpus`
  (`ssm_analysis.R:212,217`) were omitted — converted during review. Pre-existing
  `Inf` acceptance noted, untouched (scored <80).
