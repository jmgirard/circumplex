# M65: FIML item-level missing data for `axes_reliability()` — the build

**Status:** done (2026-07-27, PR #91 https://github.com/jmgirard/circumplex/pull/91)

**Goal:** Give `axes_reliability()` a `missing = "fiml"` path built on the FIML
correlation metric, so item-level missingness is handled honestly under MAR.

**Outcome:** `missing = c("listwise", "fiml")`, default unchanged and bit-identical to
shipped. `R/axes_fiml.R` adds `axes_fiml_h1()` (lavaan `h1` EM moments),
`axes_fiml_moments()` (the `sqrt(N/(N-1))` rescaling making the metric reduce exactly to
`scale()`), `axes_fiml_coverage()`, `axes_fiml_em_args()` (version-safe cap spelling),
`axes_fiml_em_stalled()`, and the shared MAR mechanisms. R̂ is `cov2cor()` of the EM
covariance, feeding the OLS shadow and the PD guard; one structured `cfa(missing = "ml")`
on the standardized columns. Six-clause refusal contract, clauses (i)-(iii) firing before
EM because lavaan fabricates unidentified moments. `nb_reliability` NA and `sd = "raw"`
refused under FIML. BC10-BC13 from a seed-pinned `devel/` harness with a committed `.rds`;
the suite re-runs ~10 replicates live, ξ1 and reported SEs both.

**Decisions:** M65-D1 (saturated moments via lavaan `h1` EM), M65-D2 (soft overlap warning
at 30), M65-D3 (live smoke never skipped), M65-D4 (EM cap 50000), M65-D5 (cap spelled at
run time, reaching both EM sites; a stall refuses). Cross-cutting: D-035.

**Review:** Four passes, two returns — CI failing on lavaan 0.7-2 plus F1 (88) and F3 (83);
then AC8 failing as written plus F1 (87, SRMR silently the mean-inclusive variant on
complete data) and F4 (88). Sub-threshold findings carried to the SE-correction ROADMAP row.
