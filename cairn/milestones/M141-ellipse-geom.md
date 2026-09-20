# M141: A confidence-ellipse layer on the Cartesian SSM coordinates, fed from draws

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M140
- **Driving RR:** —
- **Principles touched:** GP4, GP5
- **Resolves:** —
- **Surface tier:** user-facing — two new exports and a vignette subsection
- **Branch/PR:** `m141-ellipse-geom`

## Goal

Export `geom_ssm_ellipse()`, which draws a joint confidence ellipse for a profile's `(x, y)` location from a centre and a 2×2 covariance, and `ssm_ellipse_data()`, which computes those from an `ssm_draws()` object, and teach both beside the wedge in the visualization vignette.

## Scope

**In:** `GeomSsmEllipse` (⊂ `GeomPath`) in `R/geom_ssm.R` with aesthetics `x0`, `y0`, `var_x`, `var_y`, `cov_xy` and parameters `level` (default 0.95) and `n` (vertices, default 100); the S3 generic `ssm_ellipse_data(x, ...)` with a `circumplex_ssm_draws` method; `_pkgdown.yml` rows under "Visualization - Building Blocks"; `@family circumplex layers`; tests; a subsection of the M140 vignette section; NEWS.

**Out:** a covariance from `ssm_analyze()` or `ssm_sem()` objects, which retain no draws (re-deriving one from `x$sem` and `x$model$weights` is estimator-adjacent) → the parked remainders in the "Covariate extension of the CPM" candidate row; the tangent lines of Nagy et al. (2019) Fig. 4 → the same remainders; a Wald test wrapper → the "Covariate extension of the CPM" candidate row.

## Acceptance criteria

- [ ] AC1: For an input row with centre `(x0, y0)` and covariance `S`, every vertex `v` the layer emits, converted back from the coord's `(x = degrees, y = amplitude)` to Cartesian, satisfies `|(v − c)ᵀ S⁻¹ (v − c) − qchisq(level, 2)| < 1e-8`; tested on at least three rows spanning a negative `cov_xy`, an anisotropic diagonal `S`, and a `level` other than 0.95 with `n` other than 100; for the diagonal case the vertex farthest from the centre lies at distance `sqrt(qchisq(level, 2) · max(diag(S)))` within 1e-8 (closed-form oracle).
- [ ] AC2: For a centre whose displacement is within 5° of 360° and for a covariance whose ellipse contains the origin, the emitted path has no `NA` and every consecutive pair of vertices differs in `x` by less than 180°, so the coord bends it the short way.
- [ ] AC3: `geom_ssm_ellipse()` aborts via `stop()` (matching `geom_ssm_arc()`) naming `level` when `level` is outside `(0, 1)`, and aborts naming every offending row index when a covariance is not positive definite (`var_x <= 0`, `var_y <= 0`, or `cov_xy² >= var_x · var_y`); each branch has a test asserting the condition's message.
- [ ] AC4: Rows with a non-finite aesthetic are dropped under the `ssm_warn_dropped()` convention: silent at `na.rm = TRUE`, warned by count at `na.rm = FALSE`; both tested.
- [ ] AC5: `ssm_ellipse_data()` on a `circumplex_ssm_draws` object returns a one-row data frame whose `x0`, `y0` equal the object's `results$x_est`, `results$y_est` and whose `var_x`, `var_y`, `cov_xy` equal `stats::cov()` of the `x`, `y` columns of `x$draws` (oracle: that call); for a `circumplex_ssm` object it aborts naming the class and `ssm_draws()`.
- [ ] AC6: The M140 vignette section gains a subsection that draws `geom_ssm_ellipse()` from `ssm_ellipse_data(ssm_draws(readRDS("bayesian_ssm_draws.rds"), type = "parameters"))` on the same canvas as the profile's `geom_ssm_arc()` wedge and `geom_ssm_point()`, and its prose contains these phrases verbatim in the rendered `.Rmd`: (i) "a joint region on the Cartesian coordinates under a normal approximation to the draws"; (ii) "the wedge is the marginal circular-quantile interval on amplitude and displacement"; (iii) "the two need not coincide"; (iv) "an ellipse that excludes the origin is a Wald test of zero amplitude at that level, and only under that approximation"; (v) "the ellipse is centred on the plotted point estimate, and its covariance comes from the draws".
- [ ] AC7: `devtools::check()` 0/0/0; `pkgdown::check_pkgdown()` clean; `tools/check-vignette-width.R`, `tools/check-vignette-staleness.R` and `tools/check-pkgdown-vignettes.R` exit 0; `NEWS.md` names both exports.

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T2, T3
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1: `GeomSsmEllipse` in `R/geom_ssm.R`: `setup_data()` validates, computes `n` Cartesian vertices from the Cholesky factor of `S` scaled by `sqrt(qchisq(level, 2))`, converts to radius and degrees with `d + 2*pi*(d < 0)` (M26 lesson), unwraps by extension along the path (`xmax = xmin + span` pattern, may exceed 360), emits `group` per input row; constructor `geom_ssm_ellipse()` mirrors `geom_ssm_path()`; roxygen with `@family circumplex layers`; export `GeomSsmEllipse` on the `circumplex-ggproto` page.
- [x] T2: `ssm_ellipse_data()` generic + `circumplex_ssm_draws` method in `R/ssm_draws.R`; roxygen; export.
- [x] T3: Tests in `test-geom_ssm.R` and `test-ssm_draws.R` for AC1–AC5 (closed-form oracle over the three rows, seam and origin cases, every abort branch, `na.rm` both ways); two vdiffr snapshots (seam, origin) under `_snaps/geom_ssm/`; a non-visual CRAN guard in `test-plot-cran-guards.R`.
- [x] T4: Vignette subsection + prose with the five AC6 phrases; re-render; look at the figure; add the five phrases to M140's `test-vignette-latent-figures.R`.
- [ ] T5: `_pkgdown.yml` rows; NEWS; `document()`; `check()`; guards.

## Work log

- 2026-09-19: created by /milestone-plan; criteria audit record is in M140's work log (one reader, both files).
- 2026-09-19: plan gate chose centring the ellipse on `results$x_est`/`y_est` (the plotted medians) with covariance from the draws over centring on the draws' mean because the ellipse must attach to the point a reader sees, and the two coincide under the normal approximation the ellipse already assumes; falsified by a draws object where the median and mean centres differ by more than the ellipse's own semi-minor axis at the default level.
- 2026-09-19: plan gate chose exporting `ssm_ellipse_data()` over vignette-only code because the five-column contract is small and a later SEM/bootstrap method needs a generic to hang on; falsified by the generic gaining no second method within two minor releases (then it is a candidate for folding into the geom's docs).
- 2026-09-20: /milestone-implement started on branch `m141-ellipse-geom`. The question gate was skipped because the plan gate fixed the API and the three remaining choices were routine: `level` and `n` are validated in the constructor, the path repeats its first vertex (n + 1 rows per ellipse), and each input row gets its own `group`.
- 2026-09-20: T1 and T2 done. AC1 to AC5 tests pass (64 in test-geom_ssm.R, 140 in test-ssm_draws.R) and two vdiffr snapshots were added (seam, origin). Check discrimination: three planted defects in the geom (transposed Cholesky factor, chi-square with 1 df, `abs()` angle wrap) each move the AC1 residual from 3e-14 to over 2, so the oracle can fail. The seam, origin, and draws figures were rendered and inspected.
- 2026-09-20: T3 done. The CRAN guard in test-plot-cran-guards.R asserts the built vertices against the chi-square contour and the centre (mean of the antipodal vertex pairs). Full suite under NOT_CRAN: 11377 pass, 1 fail, which was the guard's first draft asserting a diagonal-only property of the first vertex. Fixed, the file passes (27).
- 2026-09-20: T4 done. Subsection "A joint confidence ellipse from posterior draws" closes section 7 of the visualization vignette, with the Overview map sentence extended. Re-rendered with tools/precompute-vignettes.R after reinstalling (the first render ran against a stale install and errored on both new functions). Rendered diff is two hunks (Overview line, the new subsection). The figure was inspected. A first prose claim, "the wedge sits inside the ellipse", was refuted by a corner check (all four wedge corners at quadratic form 7.1 to 8.1 against 5.99) and replaced with what the numbers show. Phrase test passes with the five AC6 phrases added (12).

## Decisions

## Review
