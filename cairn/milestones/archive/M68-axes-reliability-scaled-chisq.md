# M68: Scaled global test statistic for `axes_reliability()`

**Status:** done (2026-08-03, PR #94 https://github.com/jmgirard/circumplex/pull/94)

**Goal:** Report a correlation-metric-calibrated global test statistic for
`axes_reliability()` in place of normal-theory values that flatter fit ~4%.

**Outcome:** `R/axes_scaled_fit.R` ships `axes_scaling_factor()` (Satorra-Bentler
`c = tr(U Gamma_R)/df` at `cov2cor(Sigma-hat)`, satorra1994 pp. 406-407) and
`axes_scale_fit_measures()`: `$fit$chisq`, `$pvalue`, `$rmsea`, `$cfi` scaled on all
three input paths, `$fit$df`/`$fit$srmr` bit-identical, lavaan's six in
`details$fit_uncorrected`, factors in `details$scaling_factor`, failures NA with
reason. Trace via p x p identities; baseline collapses to `mean((1-rho^2)^2)`. Small-N
over-rejection (.06-.11 vs .02-.03 uncorrected) documented on three surfaces.

**Decisions:** M68-D1 (FIML uses the complete-data `Gamma_R` at Sigma-hat; a failed
factor NAs the four), M68-D2 (priced at `cov2cor(Sigma-hat)`, whose raw diagonal is
(N-1)/N), M68-D3 (RR14 confirms; small-N residual documented). Cross-cutting: D-036.

**Review:** Two rounds; round 1 returned 5 criterion failures plus F1/F2. Round 2
actioned F1 (92), F2 (85), F11 (80); other two lenses zero findings; 9 below
threshold. CI caught F1/F2 empirically. Weight-cap FAIL (205 vs <150) is logged.
