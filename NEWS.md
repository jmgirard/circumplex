# circumplex (development version)

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
