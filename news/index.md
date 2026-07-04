# Changelog

## circumplex 1.2.0

CRAN release: 2026-07-02

- The SSM plotting functions
  ([`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md),
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md),
  [`ssm_plot_contrast()`](http://circumplex.jmgirard.com/reference/ssm_plot_contrast.md))
  now warn when given an unrecognized argument (e.g., a misspelled
  parameter name) instead of silently ignoring it.
- Matrix input now works wherever it is documented.
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md),
  [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md),
  [`ipsatize()`](http://circumplex.jmgirard.com/reference/ipsatize.md),
  [`score()`](http://circumplex.jmgirard.com/reference/score.md),
  [`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md),
  and
  [`self_standardize()`](http://circumplex.jmgirard.com/reference/self_standardize.md)
  previously errored when given a matrix despite advertising matrix
  support; they now coerce it to a data frame internally.
- [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
  now accepts numeric column indexes for `scales` (e.g.,
  `scales = 1:8`), consistent with its documentation and with
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md);
  it previously required character names.
- Printing an SSM object (via
  [`print()`](https://rdrr.io/r/base/print.html) or
  [`summary()`](https://rdrr.io/r/base/summary.html)) now adds a note
  under any profile whose model fit is inadequate (R-squared \< .70;
  interpret only elevation) or whose amplitude confidence interval
  includes zero (the displacement is not interpretable). The notes apply
  to profiles only, not to contrast rows.
- Contrast displacement estimates and their confidence intervals are now
  always reported on the same angular branch. Previously, for contrasts
  near ±180 degrees, the estimate (reported in (-180, 180\]) could fall
  numerically outside a confidence interval it was geometrically inside,
  because the interval was centered on the bootstrap circular mean’s own
  branch. The interval is now shifted by a full circle when needed (its
  width and meaning are unchanged; results away from the boundary are
  identical), so interval endpoints may exceed ±180 degrees when the
  contrast straddles the boundary.
- [`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
  now matches each scale to its normative data by angular position
  rather than exact numeric equality, so 0 and 360 degrees are treated
  as the same angle (previously passing 0 for a scale stored at 360
  failed with a cryptic error). An angle with no matching normative row,
  or with more than one, now produces an informative error naming the
  available angles.
- Degenerate profiles are now handled explicitly instead of returning
  numerical noise. A flat (zero-variance) profile returns `NA`
  displacement and fit with a warning (previously an arbitrary angle and
  `-Inf`); a profile with real variance but zero amplitude returns `NA`
  displacement and a fit of
  0.  Bootstrap resamples that produce degenerate profiles (e.g., a
      resampled measure with zero variance) no longer crash
      [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md);
      they are excluded from the confidence intervals with a warning
      reporting the count. Genuinely small amplitudes are unaffected —
      the degeneracy test operates at machine-noise scale only.
- Fixed a bug where a missing (`NA`) value in the `grouping` variable of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  crashed with a cryptic error under pairwise deletion
  (`listwise = FALSE`). Such observations are now dropped before
  analysis with a message reporting how many were removed, in both
  deletion modes; if no observations remain, a clear error is given.
- Fixed a bug where length requirements on character arguments were
  never enforced (`is_null_or_char()` dropped its `n` argument).
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  now errors if `measures_labels` does not match the number of
  `measures` (or is given without `measures`),
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md)/[`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md)
  now error if `angle_labels` does not match the number of angles
  (previously mismatched labels could be silently recycled onto the
  wrong scales), and
  [`ssm_table()`](http://circumplex.jmgirard.com/reference/ssm_table.md)/[`html_render()`](http://circumplex.jmgirard.com/reference/html_render.md)
  now require `caption` to be a single string.
- Fixed a bug where
  [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
  silently ignored its `angles` argument and always used
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md):
  custom angle sets of the same length produced incorrect results
  without warning, and angle sets of a different length (e.g.,
  [`poles()`](http://circumplex.jmgirard.com/reference/poles.md) with
  four scales) errored. Results from
  [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
  with the default `angles = octants()` are unaffected. (found in
  2026-07 audit)

## circumplex 1.1.0

CRAN release: 2026-05-24

### Minor improvements and fixes

- Improve handling of radian distributions crossing the 0/2pi boundary

- Add unit tests regarding the above cases

- Optimize pairwise correlation C++ code

- Fix bug with angular median calculation retaining rejected candidates

## circumplex 1.0.2

CRAN release: 2025-09-23

### Minor improvements and fixes

- Update RcppArmadillo dependency

- Fix some deprecated ggplot args

## circumplex 1.0.1

CRAN release: 2025-07-28

### New features

- Add the
  [`self_standardize()`](http://circumplex.jmgirard.com/reference/self_standardize.md)
  function for standardizing variables using sample means and SDs

### Minor improvements and fixes

- Fix some typos in documentation

- Change plot tests to accommodate changes to ggplot2

## circumplex 1.0.0

CRAN release: 2024-10-28

### Breaking changes

- Nearly all code rewritten/refactored to streamline and reduce
  dependencies.

- Removed support for non-standard evaluation

- The `contrast` argument to
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
  is now TRUE or FALSE instead of “none”, “model”, or “test”. Model
  contrasts were removed and TRUE yields test contrasts.

- Many arguments renamed (e.g., `.data` to `data`, `.ssm_object` to
  `ssm_object`, `xy` to `drop_xy`)

- Removed `ssm_plot()` function in favor of
  [`ssm_plot_circle()`](http://circumplex.jmgirard.com/reference/ssm_plot_circle.md),
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md),
  and
  [`ssm_plot_contrast()`](http://circumplex.jmgirard.com/reference/ssm_plot_contrast.md).

- Renamed `standardize()` function to
  [`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)

### New features

- Added
  [`ssm_plot_curve()`](http://circumplex.jmgirard.com/reference/ssm_plot_curve.md)

- Added CAIS and IEI instrument data

- Added profile scores, results, and plotting to models with contrasts

- Added [`PANO()`](http://circumplex.jmgirard.com/reference/PANO.md)
  function for conveniently creating scale names

- All internal and external data are now data frames instead of tibbles

- Rewrote all vignettes to use the updated functions, arguments, etc.

### Minor improvements and fixes

- Harmonized the `results` and `scores` fields in the output of
  [`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)

- Added many unit tests, increasing the package to 100% code coverage

- Added many assertions to check for invalid input arguments

- Harmonized the tidying function arguments (e.g., `prefix`, `suffix`,
  `append`)

- Added print methods for degree and radian classes

- Replace internal non-standard evaluation with `.data` references

- Minor visual improvements to print and summary methods for ssm_objects

## circumplex 0.3.10

CRAN release: 2023-08-22

### Minor improvements and fixes

- Fix a bug when comparing R versions

- Update {vdiffr} tests

- Update GitHub Actions

------------------------------------------------------------------------

## circumplex 0.3.9

CRAN release: 2023-02-14

### Minor improvements and fixes

- Fixed a bug related to `NaN` values and `dplyr::na_if()`

- Updated package website using new version of {pkgdown}

------------------------------------------------------------------------

## circumplex 0.3.8

CRAN release: 2021-05-28

### Minor improvements and fixes

- Fix testing error on Solaris systems

- Update package description paragraph

- Add cpp11 plugin for Rcpp

- Exclude devel folder from linguist statistics

------------------------------------------------------------------------

## circumplex 0.3.7

CRAN release: 2021-05-17

### New features

- Add `angle_labels` argument to `ssm_plot()` to allow users to
  customize the angle labels around a circular plot

- Add `palette` argument to `ssm_plot()` to allow users to customize the
  color palette (from {RColorBrewer}) of a circular plot

- Replaced the `font_size` argument to `ssm_plot()` with the
  `legend_font_size` and `scale_font_size` arguments to allow users to
  customize the font size of different elements of a circular plot

### Minor improvements and fixes

- Update
  [`ggsave()`](http://circumplex.jmgirard.com/reference/ggsave.md)
  documentation for future compatibility

- Update {Rcpp} code for future compatibility

- Added a black border to the points in a circular plot to greater
  distinguish them visually

- Change CI notation from \[\] to () to play nice with pandoc

- Update to {testthat} 3E and add `ssm_plot()` tests using {vdiffr}

- Recompile vignettes with new version of {roxygen2}

- Replace TravisCI with GitHub Actions

------------------------------------------------------------------------

## circumplex 0.3.6

CRAN release: 2020-04-29

### Minor improvements and fixes

- Update dependency versions and require R \>= 3.4.0

- Fix issues related to how R 4.0.0 handles S3 methods

- Modernize ssm_plot() function to use new tidyr syntax

- Update travis CI configuration to be more explicit

------------------------------------------------------------------------

## circumplex 0.3.5

CRAN release: 2020-01-10

### Minor improvements and fixes

- Remove several unit tests that were causing problems for CRAN checks

------------------------------------------------------------------------

## circumplex 0.3.4

CRAN release: 2019-12-05

### Minor improvements and fixes

- Adjust the test of `quantile.radian()` to account for changes to `%%`
  starting in R 3.6.1 Patched

- Add the name of the package to the S3 class names (e.g.,
  `circumplex_radian` instead of `radian`) to minimize the risk of
  overlapping classes between packages

- Add some supplementary files to the R build ignore list to avoid notes
  during CRAN check

------------------------------------------------------------------------

## circumplex 0.3.3

CRAN release: 2019-09-26

### Minor improvements and fixes

- Add APA-style citations to instrument documentation in addition to DOI
  links.

- Add “Instruments” menu to package website for viewing documentation
  pages.

- Adjust the test of `quantile.radian()` to account for changes to `%%`
  starting in R 4.0.0

------------------------------------------------------------------------

## circumplex 0.3.2

CRAN release: 2019-08-21

### New features

- New `iitc` provides instrument information for the Inventory of
  Influence Tactics Circumplex.

### Minor improvements and fixes

- Fix CRAN warnings by setting `LazyData: true`.

- Fix CRAN note by replacing relative URLs with absolute URLs.

- Nonstandard evaluation is now handled using `{{}}` notation.

- Updated the formatting on this NEWS changelog to match tidyverse
  style.

------------------------------------------------------------------------

## circumplex 0.3.1

CRAN release: 2019-05-15

### Minor improvements and fixes

- Avoid a bug with dplyr 0.8.1 and S3 methods on Linux systems.

- Update the web address for Johannes in the README document.

------------------------------------------------------------------------

## circumplex 0.3.0

CRAN release: 2019-04-26

### New features

- New
  [`ssm_parameters()`](http://circumplex.jmgirard.com/reference/ssm_parameters.md)
  calculates SSM parameters (without confidence intervals) from a vector
  of scores.

- New
  [`ssm_score()`](http://circumplex.jmgirard.com/reference/ssm_score.md)
  calculates SSM parameters by row.

- Added support for older versions of R (3.3.x).

### Minor improvements and fixes

- Updated the “Introduction to SSM” vignette’s figures.

- Replaced use of `dplyr::funs()` as this function is being deprecated.

- Fixed a bug in the normative data for `ipipipc` that prevented
  standardization.

- Fixed a bug caused by changes in how random numbers are generated in R
  3.6.x.

- Fixed several broken links by running package through new version of
  `usethis`.

- Fixed warnings related to documentation inherited from other packages.

------------------------------------------------------------------------

## circumplex 0.2.1

CRAN release: 2018-11-29

### New features

- `iis32` now has normative data.

- Added open-access (i.e., full item text) to the `iis32` and `iis64`.

### Minor improvements and fixes

- `iis32` item ordering and scoring now match the author’s version.

- `iis32` response anchors now range from 1 to 6 and match norms.

- Changed use of `tibble` functions to avoid problems when new version
  releases.

- Removed dependency on `MASS` package (until it is used by exported
  functions).

------------------------------------------------------------------------

## circumplex 0.2.0

CRAN release: 2018-10-26

### New features

- Added functions and documentation for numerous circumplex instruments.

- Added functions for ipsatizing and scoring item-level data.

- Added function for standardizing scale-level data using normative
  data.

### Minor improvements and fixes

- Changed OpenMP flags in Makevars to fix a compile problem on Debian
  machines.

- Fixed a bug related to calculating angular medians in the presence of
  NAs.

- Changed the default to plot profiles with low fit (but with dashed
  borders).

- Import and export functions from rlang tidy evaluation.

- Added unit testing of various functions to increase code coverage.

- Redesigned package website to be more attractive and clear.

- Updated the SSM vignette to use the `standardize()` function.

------------------------------------------------------------------------

## circumplex 0.1.2

CRAN release: 2018-08-06

### New features

- `ssm_plot()` now uses dashed borders to indicate that a profile has
  low prototypicality/fit.

### Minor improvements and fixes

- Fixed bug that prevented compilation on Solaris systems.

- Fixed bug that prevented CRAN checks on old R versions.

- Improved the formatting of vignette source code.

------------------------------------------------------------------------

## circumplex 0.1.1

CRAN release: 2018-07-31

### New features

- [Package website](https://circumplex.jmgirard.com) added using
  [pkgdown](https://pkgdown.r-lib.org/).

### Minor improvements and fixes

- Fixed documentation to meet CRAN standards.

------------------------------------------------------------------------

## circumplex 0.1.0

- Package submitted to CRAN.
