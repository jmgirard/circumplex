# M59: Correlation-matrix input path for `axes_reliability()`

**Status:** done (2026-07-26, PR #85 https://github.com/jmgirard/circumplex/pull/85)

**Goal:** Let `axes_reliability()` estimate from a published item correlation
matrix plus its sample size, so a reanalysis needs no raw data.

**Outcome:** `axes_reliability()` gains `cormat` + `n` beside `data` on
`cpm_fit()`'s house pattern (exactly one; `n` required with `cormat`; symmetric,
PD, unit-diagonal, dimnames identical on both dimensions). `axes_fit_cormat()`
fits `sample.cov`/`sample.nobs`, bypassing `sem_fit_cfa()` and keeping lavaan's
default `likelihood = "normal"` — what makes both paths feed identical moments.
N–B → `NA`-with-reason, `sd = "raw"` refused (RR09 §7.4); `details$input` added.
Roxygen + vignette §4 + NEWS + RR09 §7.8's blockwise-ζ2 note (M54 F3).

**Decisions:** D-030 (narrow D-001 supersession admitting this to v2.0.0; M7
gains no dependency). Implement gate swapped the planned `nobs`-switches-`data`
surface for `cormat` + `n`, on the `cpm_fit()` precedent the plan missed.

**Review:** 3-lens + scorer; blame-history and prior-review clean. Fixed F1 (95,
dimnames guard checked one dimension while the subset indexed two), F3 (85,
permuted-cormat test asserted a lavaan-invariant quantity → now `ols_shadow`),
F4 (85, cross-engine residual misattributed); logged F2 (65), F5, F6. CI red
once on `covr` alone (macOS-calibrated 1e-8 measured 1.3e-8) — three tolerances
made portable, both mutations still red. 7/7 ACs; check OK, PDF manual run.
