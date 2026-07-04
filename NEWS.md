# circumplex 1.3.0

* New `ggcircumplex()` function builds an empty circumplex plotting canvas
  (amplitude rings, displacement spokes, and scale labels) as a ggplot2
  object that you can add layers to with `+`. It accepts a set of scale
  `angles` and `labels`, or a `circumplex_instrument` object to derive both
  automatically. This is the first piece of a public circumplex visualization
  layer; the package's own `ssm_plot_circle()` draws on the same canvas.
* New `geom_ssm_point()` and `geom_ssm_arc()` layers draw SSM profile points
  and their confidence-region arcs directly in circumplex space on a
  `ggcircumplex()` canvas, taking amplitude and displacement as aesthetics and
  handling the polar transform (including wrap-around at the 0/360 degree
  boundary) internally. These make it possible to build custom circumplex
  figures by composing ggplot2 layers.
* New `scale_x_circumplex()` provides an angle-labeled x-axis scale for linear
  circumplex plots (such as the score-by-angle curve). It labels axis breaks
  with their angle in degrees by default, or with custom labels or a
  `circumplex_instrument`'s scale abbreviations, using the same conventions as
  `ggcircumplex()`.
* New vignette "Advanced Circumplex Visualization" shows how to build custom
  circumplex figures by composing `ggcircumplex()`, `geom_ssm_point()`,
  `geom_ssm_arc()`, and `scale_x_circumplex()` with other ggplot2 components.
* `ggcircumplex()` and `scale_x_circumplex()` now label and place circumplex
  scales at their exact angles, including non-integer angles (for example,
  the 22.5-degree spacing of a 16-scale instrument), instead of rounding them
  to whole degrees.
* `ssm_plot_circle()` now warns and names any profile it cannot place on the
  circle because its displacement is undefined (a flat or zero-amplitude
  profile), instead of dropping it from the figure without notice.
* `ssm_analyze()` gains a `method` argument offering a Monte Carlo alternative
  to the bootstrap (`method = "montecarlo"`): SSM parameter replicates are
  drawn from the asymptotic sampling distribution of the group mean vector or
  measure-scale correlation vector (a multivariate normal with empirically
  estimated covariance; correlations are drawn jointly across measures on the
  Fisher z scale) and propagated through the SSM transformation. It produces
  intervals closely matching the bootstrap on large samples while running in a
  fraction of the time, but relies on asymptotic normality and requires
  listwise-complete data, so the bootstrap remains the default and the
  recommended choice for small samples. `summary()` reports which method
  produced the intervals.
* `ssm_analyze()` gains `parallel` and `ncpus` arguments (passed to
  `boot::boot()`) to distribute the bootstrap computation across multiple CPU
  cores. Because the resample indices are drawn in the main R process before
  any work is distributed, results for a given `set.seed()` are identical
  regardless of these settings, so parallelizing never changes your estimates
  or confidence intervals.
* `ssm_score()` is now vectorized internally (one compiled call instead of a
  row-wise loop), making it much faster on large data sets. Results are
  unchanged. Rows whose profile has undefined displacement now produce a
  single warning reporting how many such rows there are, rather than one
  warning per row. Extra arguments passed through `...` must now be named
  (e.g. `prefix = "IIP_"`) and must be single strings; an unnamed or
  non-scalar argument is now an error rather than being silently ignored
  (previously it could yield unlabeled or garbled output columns).
* Fixed a bug where a bootstrap resample under pairwise deletion
  (`listwise = FALSE`) could crash `ssm_analyze()` with `mean(): object has no
  elements` when the resample happened to draw only missing values for one
  scale. Such a scale now yields an `NA` mean (matching the correlation path),
  and the affected resample is excluded from the confidence intervals as a
  degenerate profile, consistent with the existing degeneracy handling.
* Clarified in the documentation of `ssm_parameters()`, `ssm_score()`, and
  `ssm_analyze()` that the reported model fit is a bounded R-squared in
  `[0, 1]` only when `angles` are equally spaced; for unequally spaced angles
  the closed-form estimator is not a least-squares fit and the reported fit can
  fall below 0.
* Fixed a bug where the displacement of a group contrast between two exactly
  opposed profiles (a half-turn apart) was reported as `-180` degrees instead
  of `+180`, inconsistent with the documented `(-180, 180]` convention for
  contrasts. Such a contrast is now reported as `+180`.

# circumplex 1.2.0

* The SSM plotting functions (`ssm_plot_circle()`, `ssm_plot_curve()`,
  `ssm_plot_contrast()`) now warn when given an unrecognized argument (e.g., a
  misspelled parameter name) instead of silently ignoring it.
* Matrix input now works wherever it is documented. `ssm_analyze()`,
  `ssm_score()`, `ipsatize()`, `score()`, `norm_standardize()`, and
  `self_standardize()` previously errored when given a matrix despite
  advertising matrix support; they now coerce it to a data frame internally.
* `ssm_score()` now accepts numeric column indexes for `scales` (e.g.,
  `scales = 1:8`), consistent with its documentation and with `ssm_analyze()`;
  it previously required character names.
* Printing an SSM object (via `print()` or `summary()`) now adds a note under
  any profile whose model fit is inadequate (R-squared < .70; interpret only
  elevation) or whose amplitude confidence interval includes zero (the
  displacement is not interpretable). The notes apply to profiles only, not to
  contrast rows.
* Contrast displacement estimates and their confidence intervals are now
  always reported on the same angular branch. Previously, for contrasts near
  ±180 degrees, the estimate (reported in (-180, 180]) could fall numerically
  outside a confidence interval it was geometrically inside, because the
  interval was centered on the bootstrap circular mean's own branch. The
  interval is now shifted by a full circle when needed (its width and meaning
  are unchanged; results away from the boundary are identical), so interval
  endpoints may exceed ±180 degrees when the contrast straddles the boundary.
* `norm_standardize()` now matches each scale to its normative data by angular
  position rather than exact numeric equality, so 0 and 360 degrees are treated
  as the same angle (previously passing 0 for a scale stored at 360 failed with
  a cryptic error). An angle with no matching normative row, or with more than
  one, now produces an informative error naming the available angles.
* Degenerate profiles are now handled explicitly instead of returning
  numerical noise. A flat (zero-variance) profile returns `NA` displacement
  and fit with a warning (previously an arbitrary angle and `-Inf`); a profile
  with real variance but zero amplitude returns `NA` displacement and a fit
  of 0. Bootstrap resamples that produce degenerate profiles (e.g., a
  resampled measure with zero variance) no longer crash `ssm_analyze()`; they
  are excluded from the confidence intervals with a warning reporting the
  count. Genuinely small amplitudes are unaffected — the degeneracy test
  operates at machine-noise scale only.
* Fixed a bug where a missing (`NA`) value in the `grouping` variable of
  `ssm_analyze()` crashed with a cryptic error under pairwise deletion
  (`listwise = FALSE`). Such observations are now dropped before analysis with
  a message reporting how many were removed, in both deletion modes; if no
  observations remain, a clear error is given.
* Fixed a bug where length requirements on character arguments were never
  enforced (`is_null_or_char()` dropped its `n` argument). `ssm_analyze()` now
  errors if `measures_labels` does not match the number of `measures` (or is
  given without `measures`), `ssm_plot_circle()`/`ssm_plot_curve()` now error
  if `angle_labels` does not match the number of angles (previously mismatched
  labels could be silently recycled onto the wrong scales), and
  `ssm_table()`/`html_render()` now require `caption` to be a single string.
* Fixed a bug where `ssm_score()` silently ignored its `angles` argument and
  always used `octants()`: custom angle sets of the same length produced
  incorrect results without warning, and angle sets of a different length
  (e.g., `poles()` with four scales) errored. Results from `ssm_score()` with
  the default `angles = octants()` are unaffected. (found in 2026-07 audit)

# circumplex 1.1.0

## Minor improvements and fixes

* Improve handling of radian distributions crossing the 0/2pi boundary

* Add unit tests regarding the above cases

* Optimize pairwise correlation C++ code

* Fix bug with angular median calculation retaining rejected candidates

# circumplex 1.0.2

## Minor improvements and fixes

* Update RcppArmadillo dependency

* Fix some deprecated ggplot args

# circumplex 1.0.1

## New features

* Add the `self_standardize()` function for standardizing variables using sample means and SDs

## Minor improvements and fixes

* Fix some typos in documentation

* Change plot tests to accommodate changes to ggplot2

# circumplex 1.0.0

## Breaking changes

* Nearly all code rewritten/refactored to streamline and reduce dependencies.

* Removed support for non-standard evaluation

* The `contrast` argument to `ssm_analyze()` is now TRUE or FALSE instead of "none", "model", or "test". Model contrasts were removed and TRUE yields test contrasts.

* Many arguments renamed (e.g., `.data` to `data`, `.ssm_object` to `ssm_object`, `xy` to `drop_xy`)

* Removed `ssm_plot()` function in favor of `ssm_plot_circle()`, `ssm_plot_curve()`, and `ssm_plot_contrast()`.

* Renamed `standardize()` function to `norm_standardize()`

## New features

* Added `ssm_plot_curve()`

* Added CAIS and IEI instrument data

* Added profile scores, results, and plotting to models with contrasts

* Added `PANO()` function for conveniently creating scale names

* All internal and external data are now data frames instead of tibbles

* Rewrote all vignettes to use the updated functions, arguments, etc.

## Minor improvements and fixes

* Harmonized the `results` and `scores` fields in the output of `ssm_analyze()`

* Added many unit tests, increasing the package to 100% code coverage

* Added many assertions to check for invalid input arguments

* Harmonized the tidying function arguments (e.g., `prefix`, `suffix`, `append`)

* Added print methods for degree and radian classes

* Replace internal non-standard evaluation with `.data` references

* Minor visual improvements to print and summary methods for ssm_objects

# circumplex 0.3.10

## Minor improvements and fixes

* Fix a bug when comparing R versions

* Update \{vdiffr\} tests

* Update GitHub Actions

----

# circumplex 0.3.9

## Minor improvements and fixes

* Fixed a bug related to `NaN` values and `dplyr::na_if()`

* Updated package website using new version of \{pkgdown\}

----

# circumplex 0.3.8

## Minor improvements and fixes

* Fix testing error on Solaris systems

* Update package description paragraph

* Add cpp11 plugin for Rcpp

* Exclude devel folder from linguist statistics

----

# circumplex 0.3.7

## New features

* Add `angle_labels` argument to `ssm_plot()` to allow users to customize the angle labels around a circular plot

* Add `palette` argument to `ssm_plot()` to allow users to customize the color palette (from \{RColorBrewer\}) of a circular plot

* Replaced the `font_size` argument to `ssm_plot()` with the `legend_font_size` and `scale_font_size` arguments to allow users to customize the font size of different elements of a circular plot

## Minor improvements and fixes

* Update `ggsave()` documentation for future compatibility

* Update \{Rcpp\} code for future compatibility

* Added a black border to the points in a circular plot to greater distinguish them visually

* Change CI notation from [] to () to play nice with pandoc

* Update to \{testthat\} 3E and add `ssm_plot()` tests using \{vdiffr\}

* Recompile vignettes with new version of \{roxygen2\}

* Replace TravisCI with GitHub Actions

----

# circumplex 0.3.6

## Minor improvements and fixes

* Update dependency versions and require R >= 3.4.0

* Fix issues related to how R 4.0.0 handles S3 methods

* Modernize ssm_plot() function to use new tidyr syntax

* Update travis CI configuration to be more explicit

----

# circumplex 0.3.5

## Minor improvements and fixes

* Remove several unit tests that were causing problems for CRAN checks

----

# circumplex 0.3.4

## Minor improvements and fixes

* Adjust the test of `quantile.radian()` to account for changes to `%%` starting in R 3.6.1 Patched

* Add the name of the package to the S3 class names (e.g., `circumplex_radian` instead of `radian`) to minimize the risk of overlapping classes between packages

* Add some supplementary files to the R build ignore list to avoid notes during CRAN check

----

# circumplex 0.3.3

## Minor improvements and fixes

* Add APA-style citations to instrument documentation in addition to DOI links.

* Add "Instruments" menu to package website for viewing documentation pages.

* Adjust the test of `quantile.radian()` to account for changes to `%%` starting in R 4.0.0

----

# circumplex 0.3.2

## New features

* New `iitc` provides instrument information for the Inventory of Influence
  Tactics Circumplex.

## Minor improvements and fixes

* Fix CRAN warnings by setting `LazyData: true`.

* Fix CRAN note by replacing relative URLs with absolute URLs.

* Nonstandard evaluation is now handled using `{{}}` notation.

* Updated the formatting on this NEWS changelog to match tidyverse style.

----

# circumplex 0.3.1

## Minor improvements and fixes

* Avoid a bug with dplyr 0.8.1 and S3 methods on Linux systems.

* Update the web address for Johannes in the README document.

----

# circumplex 0.3.0

## New features

* New `ssm_parameters()` calculates SSM parameters (without
  confidence intervals) from a vector of scores.

* New `ssm_score()` calculates SSM parameters by row.

* Added support for older versions of R (3.3.x).

## Minor improvements and fixes

* Updated the "Introduction to SSM" vignette's figures.

* Replaced use of `dplyr::funs()` as this function is being deprecated.

* Fixed a bug in the normative data for `ipipipc` that prevented standardization.

* Fixed a bug caused by changes in how random numbers are generated in R 3.6.x.

* Fixed several broken links by running package through new version of `usethis`.

* Fixed warnings related to documentation inherited from other packages.

----

# circumplex 0.2.1

## New features

* `iis32` now has normative data.

* Added open-access (i.e., full item text) to the `iis32` and `iis64`.

## Minor improvements and fixes

* `iis32` item ordering and scoring now match the author's version.

* `iis32` response anchors now range from 1 to 6 and match norms.

* Changed use of `tibble` functions to avoid problems when new version releases.

* Removed dependency on `MASS` package (until it is used by exported functions).

----

# circumplex 0.2.0

## New features

* Added functions and documentation for numerous circumplex instruments.

* Added functions for ipsatizing and scoring item-level data.

* Added function for standardizing scale-level data using normative data.

## Minor improvements and fixes

* Changed OpenMP flags in Makevars to fix a compile problem on Debian machines.

* Fixed a bug related to calculating angular medians in the presence of NAs.

* Changed the default to plot profiles with low fit (but with dashed borders).

* Import and export functions from rlang tidy evaluation.

* Added unit testing of various functions to increase code coverage.

* Redesigned package website to be more attractive and clear.

* Updated the SSM vignette to use the `standardize()` function.

----

# circumplex 0.1.2

## New features

* `ssm_plot()` now uses dashed borders to indicate that a profile has low prototypicality/fit.

## Minor improvements and fixes

* Fixed bug that prevented compilation on Solaris systems.

* Fixed bug that prevented CRAN checks on old R versions.

* Improved the formatting of vignette source code.

----

# circumplex 0.1.1

## New features

* [Package website](https://circumplex.jmgirard.com) added using [pkgdown](https://pkgdown.r-lib.org/).

## Minor improvements and fixes

* Fixed documentation to meet CRAN standards.

----

# circumplex 0.1.0

* Package submitted to CRAN.
