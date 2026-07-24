# M55: Axes-Reliability vignette — teaching `axes_reliability()`

**Status:** done (2026-07-23, PR #81 https://github.com/jmgirard/circumplex/pull/81)

**Goal:** A user-facing vignette that teaches `axes_reliability()` — what circumplex axis reliability is, how to run it, and how to read its output with its caveats.

**Outcome:** Shipped `vignettes/axes-reliability.Rmd`, a new article teaching the Strack (2013) `axes_reliability()` estimator built in M54. Covers what axis reliability is (vs. `ssm_sem()` disattenuation and `fit_structure()` structure evaluation), a worked example on `simulated_items` (recovers ~.77 reliability / .18 axes variance), reading the per-axis reliability/SEm/`NB_Reliability` table and variance components, and three caveats: Nunnally–Bernstein overestimation under scale specificity (Strack 2013 Fig. 3), correlation-as-covariance → approximate SEs/χ² (Cudeck 1989), and listwise-only + boundary→`NA`. lavaan-gated like `sem-based-ssm-analysis.Rmd` so it builds without lavaan. Docs-only — no R/`src`/roxygen change. Also added M55 to M7's v2.0.0 `Depends on`. Lineage: D-025 → D-026 → M53 → M54 → M55.

**Decisions:** none.

**Review:** Three independent lenses (diff-bug/Opus, blame-history/Sonnet, prior-review/Sonnet) — zero findings; scorer/triage no-op, no follow-ups. All 5 ACs met with fresh evidence; `devtools::check` 0/0/0; `cairn_validate` exit 0; `pkgdown::check_pkgdown` clean; PR #81 CI green (pkgdown, test-coverage, ubuntu-latest).
