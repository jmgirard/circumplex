# M141: A confidence-ellipse layer on the Cartesian SSM coordinates, fed from draws

- **Status:** review
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

- [x] AC1: For an input row with centre `(x0, y0)` and covariance `S`, every vertex `v` the layer emits, converted back from the coord's `(x = degrees, y = amplitude)` to Cartesian, satisfies `|(v − c)ᵀ S⁻¹ (v − c) − qchisq(level, 2)| < 1e-8`; tested on at least three rows spanning a negative `cov_xy`, an anisotropic diagonal `S`, and a `level` other than 0.95 with `n` other than 100; for the diagonal case the vertex farthest from the centre lies at distance `sqrt(qchisq(level, 2) · max(diag(S)))` within 1e-8 (closed-form oracle).
- [x] AC2: For a centre whose displacement is within 5° of 360° and for a covariance whose ellipse contains the origin, the emitted path has no `NA` and every consecutive pair of vertices differs in `x` by less than 180°, so the coord bends it the short way.
- [x] AC3: `geom_ssm_ellipse()` aborts via `stop()` (matching `geom_ssm_arc()`) naming `level` when `level` is outside `(0, 1)`, and aborts naming every offending row index when a covariance is not positive definite (`var_x <= 0`, `var_y <= 0`, or `cov_xy² >= var_x · var_y`); each branch has a test asserting the condition's message.
- [x] AC4: Rows with a non-finite aesthetic are dropped under the `ssm_warn_dropped()` convention: silent at `na.rm = TRUE`, warned by count at `na.rm = FALSE`; both tested.
- [x] AC5: `ssm_ellipse_data()` on a `circumplex_ssm_draws` object returns a one-row data frame whose `x0`, `y0` equal the object's `results$x_est`, `results$y_est` and whose `var_x`, `var_y`, `cov_xy` equal `stats::cov()` of the `x`, `y` columns of `x$draws` (oracle: that call); for a `circumplex_ssm` object it aborts naming the class and `ssm_draws()`.
- [x] AC6: The M140 vignette section gains a subsection that draws `geom_ssm_ellipse()` from `ssm_ellipse_data(ssm_draws(readRDS("bayesian_ssm_draws.rds"), type = "parameters"))` on the same canvas as the profile's `geom_ssm_arc()` wedge and `geom_ssm_point()`, and its prose contains these phrases verbatim in the rendered `.Rmd`: (i) "a joint region on the Cartesian coordinates under a normal approximation to the draws"; (ii) "the wedge is the pair of marginal intervals on amplitude and displacement, a percentile interval on amplitude and a circular-quantile interval on displacement"; (iii) "the two need not coincide"; (iv) "an ellipse at confidence level 1 − α that excludes the origin rejects zero amplitude in a Wald test at significance level α, and only under that approximation"; (v) "the ellipse is centred on the posterior medians of `x` and `y`, not on the plotted point, and its covariance comes from the draws".
- [x] AC7: `devtools::check()` 0/0/0; `pkgdown::check_pkgdown()` clean; `tools/check-vignette-width.R`, `tools/check-vignette-staleness.R` and `tools/check-pkgdown-vignettes.R` exit 0; `NEWS.md` names both exports.

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
- [x] T5: `_pkgdown.yml` rows; NEWS; `document()`; `check()`; guards.

## Work log

- 2026-09-19: created by /milestone-plan; criteria audit record is in M140's work log (one reader, both files).
- 2026-09-19: plan gate chose centring the ellipse on `results$x_est`/`y_est` (the plotted medians) with covariance from the draws over centring on the draws' mean because the ellipse must attach to the point a reader sees, and the two coincide under the normal approximation the ellipse already assumes; falsified by a draws object where the median and mean centres differ by more than the ellipse's own semi-minor axis at the default level.
- 2026-09-19: plan gate chose exporting `ssm_ellipse_data()` over vignette-only code because the five-column contract is small and a later SEM/bootstrap method needs a generic to hang on; falsified by the generic gaining no second method within two minor releases (then it is a candidate for folding into the geom's docs).
- 2026-09-20: /milestone-implement started on branch `m141-ellipse-geom`. The question gate was skipped because the plan gate fixed the API and the three remaining choices were routine: `level` and `n` are validated in the constructor, the path repeats its first vertex (n + 1 rows per ellipse), and each input row gets its own `group`.
- 2026-09-20: T1 and T2 done. AC1 to AC5 tests pass (64 in test-geom_ssm.R, 140 in test-ssm_draws.R) and two vdiffr snapshots were added (seam, origin). Check discrimination: three planted defects in the geom (transposed Cholesky factor, chi-square with 1 df, `abs()` angle wrap) each move the AC1 residual from 3e-14 to over 2, so the oracle can fail. The seam, origin, and draws figures were rendered and inspected.
- 2026-09-20: T3 done. The CRAN guard in test-plot-cran-guards.R asserts the built vertices against the chi-square contour and the centre (mean of the antipodal vertex pairs). Full suite under NOT_CRAN: 11377 pass, 1 fail, which was the guard's first draft asserting a diagonal-only property of the first vertex. Fixed, the file passes (27).
- 2026-09-20: T4 done. Subsection "A joint confidence ellipse from posterior draws" closes section 7 of the visualization vignette, with the Overview map sentence extended. Re-rendered with tools/precompute-vignettes.R after reinstalling (the first render ran against a stale install and errored on both new functions). Rendered diff is two hunks (Overview line, the new subsection). The figure was inspected. A first prose claim, "the wedge sits inside the ellipse", was refuted by a corner check (all four wedge corners at quadratic form 7.1 to 8.1 against 5.99) and replaced with what the numbers show. Phrase test passes with the five AC6 phrases added (12).
- 2026-09-20: T5 checks: `devtools::check()` 0 errors, 0 warnings, 0 notes (10m16s); `devtools::test()` under NOT_CRAN 11383 pass, 0 fail; `pkgdown::check_pkgdown()` clean; width, staleness, and pkgdown-vignette guards exit 0; `document()` no diff, no link warnings.
- 2026-09-20: claim audit: 62 claims read, 3 corrected — NEWS.md, R/geom_ssm.R, R/ssm_draws.R, vignettes/advanced-visualization.Rmd and .Rmd.orig, tests/testthat/test-plot-cran-guards.R, test-geom_ssm.R, test-ssm_draws.R, test-vignette-latent-figures.R. Corrected: a guard comment calling (x_est, y_est) the plotted point (fixed in place), and AC6 phrases (ii) and (v), taken to the mini gate below. Flagged, not counted: phrase (iv) "at that level"; "sampling distribution" in the geom's roxygen where the shipped supplier is a posterior covariance.
- 2026-09-20: AC6 amended at a mini gate (user chose amend over keep-and-correct). Phrase (ii) was "the wedge is the marginal circular-quantile interval on amplitude and displacement" and is now "the wedge is the pair of marginal intervals on amplitude and displacement, a percentile interval on amplitude and a circular-quantile interval on displacement". Phrase (v) was "the ellipse is centred on the plotted point estimate, and its covariance comes from the draws" and is now "the ellipse is centred on the posterior medians of `x` and `y`, not on the plotted point, and its covariance comes from the draws". Reason: the amplitude interval is a plain percentile interval (R/ssm_bootstrap.R:128-137), and the centre (x_est, y_est) sits 4.66e-4 from the plotted point (a_est at d_est). The 2026-09-19 plan-gate line called those medians "the plotted medians"; they are not the plotted point, but its falsifier (a gap over the semi-minor axis, here 0.0574) is unmet, so the centring choice stands.
- 2026-09-20: re-audit: AC6 (full) — (ii) and (v) verified against R/ssm_bootstrap.R and R/ssm_draws.R; satisfiable, reachable, bounded, deliverable-level, proportionate, no probe applies; one finding: phrase (iv) "at that level" reads as a test at level 0.95 where the significance level is 0.05, replacement proposed ("an ellipse at confidence level 1 − α that excludes the origin rejects zero amplitude in a Wald test at significance level α, and only under that approximation"), and two vignette sentences now duplicate (ii) and (v) and must be folded.
- 2026-09-20: AC6 amended again at a second mini gate (user chose amend over keep). Phrase (iv) was "an ellipse that excludes the origin is a Wald test of zero amplitude at that level, and only under that approximation" and is now "an ellipse at confidence level 1 − α that excludes the origin rejects zero amplitude in a Wald test at significance level α, and only under that approximation". Reason: "at that level" read as a test at level 0.95 where the significance level is 0.05, and the ellipse is the test's acceptance region, not the test.
- 2026-09-20: re-audit: AC6 (full) — nothing statistically false: (iv)'s equivalence verified both ways (origin outside the level-(1 − α) ellipse iff c'S⁻¹c > qchisq(1 − α, 2) iff the Wald test rejects at α), (ii) and (v) match the code, D-058 exempts the .rds fixture, no IP blocks it; two notes: spell "1 − α" and "α" as raw UTF-8 in the test file (a \u escape fails to match under LC_ALL=C), and the three phrases add long-sentence findings to tools/prose-sweep.R, which gates nothing. This is AC6's second re-audit line, so no further reader runs on it.
- 2026-09-20: T5 done on the amended tree: `devtools::check()` 0/0/0 (7m16s), `devtools::test()` 11383 pass 0 fail, phrase test passes under the default locale and LC_ALL=C, width and staleness guards exit 0, `cairn_validate` all checks passed. After the check, one roxygen sentence in `geom_ssm_ellipse()` was reworded from "sampling distribution" to "distribution, sampling or posterior" (the claim audit's flag), followed by `document()` with no link warnings and the geom test file; review's own check covers it. Status set to review.
- 2026-09-20: /milestone-review started. AC1 to AC6 verified with fresh test runs and ticked. AC7 waits on the running `devtools::check()` and full suite. Three fresh-context reviewers are running. Checkpoint, review not finished.

## Decisions

## Review

Review on 2026-09-20 by /milestone-review, on branch `m141-ellipse-geom` at c5c46954. The branch contains `origin/master` (d9721bd0), so no merge was needed. No PR existed at review start. Driving RR is `—`, so no projection-vs-outcome pairs apply. `cairn/DESIGN.md` is not in the diff, so the impact report is a no-op.

- AC1: verified. The two "(AC1)" tests in `test-geom_ssm.R` pass in a fresh run (file total 64 pass, 0 fail). They cover three rows: a negative `cov_xy`, an anisotropic diagonal, and `level = 0.8` with `n = 64`. Each built vertex, mapped back to Cartesian, sits within 1e-8 of `qchisq(level, 2)`. The diagonal row's farthest vertex sits within 1e-8 of `sqrt(qchisq(0.95, 2) * 0.040)`.
- AC2: verified. The "(AC2)" test passes. A centre at 358° and an origin-containing covariance both build with no `NA` in `x` or `y`, and every consecutive `x` difference is below 180. The seam path closes on its own branch, and the origin path's endpoints differ by 360. The two vdiffr snapshots (seam, origin) match.
- AC3: verified. Both "(AC3)" tests pass. A `level` of 1, 0, 1.5, or length 2 aborts through `stop()`, the call `geom_ssm_arc()` uses, with a message that names `level`. A four-row frame with `var_x = 0`, `var_y < 0`, and `cov_xy^2 == var_x * var_y` aborts with a message that names "positive definite" and rows "2, 3, 4". The clean row builds.
- AC4: verified. The "(AC4)" test passes. An `Inf` centre and an `NA` variance are removed silently under `na.rm = TRUE`, leaving 101 vertices. Under `na.rm = FALSE` the build warns "Removed 2 rows".
- AC5: verified. Both "(AC5)" tests in `test-ssm_draws.R` pass (file total 140 pass, 0 fail). `x0` and `y0` equal `results$x_est` and `results$y_est`, which are the draws' medians. `var_x`, `var_y`, and `cov_xy` equal `stats::cov()` of the `x` and `y` draw columns. An `ssm_analyze()` result aborts with a message that names `circumplex_ssm` and `ssm_draws()`.
- AC6: verified. `vignettes/advanced-visualization.Rmd.orig` lines 497 to 580 hold the subsection "A joint confidence ellipse from posterior draws". It builds `post` from `ssm_draws(readRDS("bayesian_ssm_draws.rds"), type = "parameters")` and `ellipse` from `ssm_ellipse_data(post)`. One canvas draws `geom_ssm_arc()` from `post$results`, `geom_ssm_ellipse()` from `ellipse`, and `geom_ssm_point()`. The phrase test (`test-vignette-latent-figures.R`, 12 pass) reads the rendered `.Rmd` and matches all five phrases verbatim. The rendered figure was inspected, and the wedge's corners fall outside the ellipse as the prose says.
- AC7: verified. `devtools::check(args = "--no-manual")` at c5c46954: 0 errors, 0 warnings, 0 notes (7m28s). `devtools::test()` under NOT_CRAN: 11383 pass, 0 fail, 1 skip. `pkgdown::check_pkgdown()`: no problems found. `tools/check-vignette-width.R`, `tools/check-vignette-staleness.R`, and `tools/check-pkgdown-vignettes.R` each exit 0. `NEWS.md` names `geom_ssm_ellipse()` and `ssm_ellipse_data()`.

Consistency gate: `cairn_validate.py` all checks passed. `devtools::document()` with `cli.width = 500` produced no diff and no "resolve link" line. Master watches: the newest push runs of R-CMD-check.yaml and test-coverage.yaml on master (6a958d17) both concluded success. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`, and `tools/check-branch-protection.R` each exit 0. README.Rmd is not in the diff. No new top-level files.

Independent review (three lenses, fresh context). The history lens read the commits behind `geom_ssm_arc()`, `angle_unwrap()`, `ssm_warn_dropped()`, D-003, D-058, and the M26 and M123 lessons, and found no conflict. The prior-review lens found the GitHub comment probe empty and one regression against an archived review. Findings, ranked, with their disposition at the gate:

- O1 (fix now): `R/ssm_draws.R` roxygen calls `x_est`, `y_est` "the posterior medians the package also plots", but the package plots `a_est` at `d_est`, a point 4.66e-4 away on the vignette draws. The claim audit removed this wording elsewhere. Disposition: TBD.
- O2 (criterion overlap): AC3 and AC4 conflict on a row that is both non-finite and not positive definite (`x0 = NA`, `var_x = -1`). AC3 as written asks for an abort that names the row. AC4 asks for the row to be dropped. The code drops it, so AC3 fails on that input. Disposition: TBD.
- O3: the positive-definiteness test `cov_xy^2 >= var_x * var_y` underflows at 1e-300 and overflows at 1e300. The score metric is order 1, and the sqrt form would misjudge the exact-boundary case the AC3 test asserts (row 4). Disposition: TBD.
- O4 (test only): the AC2 assertion `max(abs(diff(d$x))) < 180` is not split by group, so a two-row fixture would measure the jump between ellipses. Disposition: TBD.
- O5: `setup_data()` overrides a user-supplied `group` aesthetic with the row index, and the roxygen does not say so. Disposition: TBD.
- O6: `cairn/DESIGN.md` "Visualization extension" still lists only the point and arc geoms and three exported ggproto classes. `GeomSsmPath` was already missing. Disposition: TBD.
- O7: `@family ssm functions` on `ssm_ellipse_data()` adds a see-also link to every estimation page, while its sibling geom sits in `circumplex layers`. Disposition: TBD.
- O8: a character aesthetic column reads as non-finite and would be dropped, but ggplot2's continuous-scale check fires first. Disposition: TBD.
- O9: under `na.rm = FALSE` a dropped-row warning precedes an abort on another row. Disposition: TBD.
- S1 (history lens observation): `angle_unwrap()` assumes steps under a half-turn, and `n` may be as small as 3. The diff-bug lens showed that for a convex outline containing the origin every step is strictly under 180° at any `n`, with a supremum of 179.97° over a search of offsets from 1e-1 to 1e-12. Disposition: TBD.
- P1 (prior-review lens): `tools/prose-sweep.R` (the M123 sentence cap, not CI-gated) reports seven new sentences over 25 words in the vignette subsection, at `.Rmd.orig` lines 68, 545, 561, 567, 571, 573, and 580. Lines 545, 561, and 571 carry AC6 phrases (v), (i) with (ii), and (iv) verbatim, and phrases (ii) and (iv) are themselves over the cap. The implement work log flagged this. Disposition: TBD.
