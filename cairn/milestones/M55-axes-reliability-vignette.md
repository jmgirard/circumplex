# M55: Axes-Reliability vignette — teaching `axes_reliability()`

- **Status:** review
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

A user-facing vignette that teaches `axes_reliability()` — what circumplex axis
reliability is, how to run it, and how to read its output with its caveats.

## Scope

**In:** A new `vignettes/axes-reliability.Rmd` teaching the Strack, Jacobs &
Grosse Holtforth (2013) axis-reliability estimator shipped in M54. It motivates
the quantity (reliability of the two circumplex axes; SEm for a single-profile
location CI), runs the primary workflow on `simulated_items` (map items→scales,
`angles = octants()`, read `print`/`summary`), interprets the per-axis table
(reliability, SEm, `nb_reliability`), and states the three doc-level caveats in
statistically precise prose. lavaan-gated like `sem-based-ssm-analysis.Rmd` so
it still builds where lavaan is absent.

**Out:** Any change to `axes_reliability()` behavior or its reference `.Rd`
(the function shipped in M54 — a code change reopens a build milestone, not this
docs milestone). The deferred-in-spec extensions (non-octant types, correlation-
matrix input, blockwise ζ2, FIML) → the ROADMAP axes-reliability-extensions
candidate. A conceptual reworking of the three reliability mentions already in
other vignettes (`using-instruments`, `sem-based-ssm-analysis`,
`evaluating-circumplex-structure`) → not in scope; this vignette only
cross-references them.

## Acceptance criteria

- [ ] `vignettes/axes-reliability.Rmd` exists with a `%\VignetteIndexEntry{}`,
      the `knitr::rmarkdown` engine, and UTF-8 encoding, and knits without error
      when lavaan is installed.
- [ ] The vignette builds when lavaan is **not** installed — model-fitting
      chunks are gated on `requireNamespace("lavaan")` and a fallback note is
      shown, mirroring `sem-based-ssm-analysis.Rmd:10-25`.
- [ ] The vignette runs the primary workflow on `simulated_items`: it maps the
      32 item columns to their 8 octant scales, calls `axes_reliability(...,
      angles = octants())`, and displays both `print()` and `summary()` output,
      naming the per-axis columns (reliability, SEm, `nb_reliability`).
- [ ] The vignette states, in prose citing its source, each of the three caveats
      the function documents: (a) `nb_reliability` **overestimates** when scale
      specificity is large (Strack et al. 2013, Fig. 3); (b) fitting the item
      **correlation** matrix as covariance makes component SEs and χ²
      **approximate** but point estimates and reliabilities correct (Cudeck,
      1989); (c) missing data are **listwise-only** and a boundary fit returns
      `NA` reliability/SEm, not a clipped value.
- [ ] `R CMD build` / `tools::buildVignettes()` produces the vignette cleanly,
      and `devtools::check(args = "--no-manual")` reports no new
      WARNING/NOTE attributable to it.

## Coverage

- AC1 → T1, T5
- AC2 → T1, T5
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Create `vignettes/axes-reliability.Rmd` — YAML + `VignetteIndexEntry{Axes Reliability}`, the lavaan-gating setup chunk and absent-lavaan note copied from `sem-based-ssm-analysis.Rmd:10-25`, and the opening motivation (what axis reliability is; when to reach for it vs. `fit_structure()`/`ssm_sem()`).
- [x] T2: Worked-example section — `data("simulated_items")`, build the items list (`split(names(simulated_items), rep(1:8, each = 4))`), call `axes_reliability(..., angles = octants())`, show `res`; cross-reference the `instrument =` convenience path without running it (no 500-row registered instrument in the package).
- [x] T3: Interpretation section — `summary(res)` (variance components + global fit), reading the per-axis reliability, SEm, and `nb_reliability` table; why X and Y match for a balanced octant instrument (differ only via `item_n`).
- [x] T4: Caveats section — the three doc-level caveats (AC4) in precise prose, each naming its source (Strack et al. 2013; Cudeck 1989); no significance-test language for any CI (CLAUDE.md style; DECISIONS.md D-entry on vignette wording).
- [x] T5: Build + register — knit locally, confirm lavaan-absent build path, `tools::buildVignettes()` + `devtools::check(args = "--no-manual")` clean; confirm pkgdown auto-discovers the article (no `_pkgdown.yml` articles list to edit).

## Work log

- 2026-07-23: created by /milestone-plan (focused scope: teach `axes_reliability()`; lineage D-025 → D-026 → M53 design → M54 build → M55 docs).
- 2026-07-23: T1–T4 — wrote `vignettes/axes-reliability.Rmd` (motivation, worked example on `simulated_items`, component/NB interpretation, three caveats). Knits clean with lavaan present (23 KB) and with lavaan forced absent (note shown, no fit output leaked).
- 2026-07-23: T5 — `devtools::check(args = "--no-manual")` clean (0 errors, 0 warnings, 0 notes; 6m28s); vignette builds under R CMD check and is auto-registered (no `_pkgdown.yml` edit needed). All tasks done → status `review`.

## Decisions

## Review
