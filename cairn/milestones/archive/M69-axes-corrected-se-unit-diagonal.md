# M69: Correlation-metric pricing for `axes_reliability()`'s corrected component SEs

**Status:** done (2026-08-04, PR #95 https://github.com/jmgirard/circumplex/pull/95)

**Goal:** Price `axes_corrected_se()`'s corrected branch at `cov2cor(Sigma-hat)`
so both halves of the correlation-metric correction sit on one metric.

**Outcome:** `axes_se_pricing()` extracted and called twice — `naive` at the raw
Sigma-hat (still reproducing lavaan to 1e-7), `corrected` and a new
`fiml_ratio` at `cov2cor(Sigma-hat)`. `R/axes_reliability.R:1691` consumes
`fiml_ratio`, removing an N/(N-1) INFLATION of every FIML SE (0.17% at n = 600,
1% at n = 100). Adds RR15 B2's nonpositive-diagonal refusal, NA-safe, with the
NA-together contract extended to all three vectors; repairs the stale
`axes_scaled_fit.R` Wc citation behind a parsed-range guard. Evidence: a
vech-space oracle at 1e-6, the bootstrap moving closer on all three draws, and
`1/fiml_ratio` restoring RR13's published 1.441229 (shipped pricing gave
1.44034). Calibration re-run 201 reps/cell: 0.9598 / 0.9267 / 1.0156.

**Decisions:** D-037 (FIML ratio at `cov2cor(Sigma-hat)`, superseding RR13 BC4;
M68-D2 affirmed). Driving RR15: BC1-BC6 ingested verbatim, five deviations.

**Review:** Two rounds. Round 1 returned on F16 (93), F1 (92, a regression this
branch introduced), F21 (80), plus F13/F20 unticking criteria. Round 2 actioned
A7 (80); 17 logged. A1 (48) → candidate row: the sibling guard at
`axes_scaled_fit.R:103` carries the same NA-unsafe defect.
