circumplex 0.2.0
=============================

#### NEW FEATURES ####

* Added functions and documentation for numerous circumplex instruments
* Added functions for ipsatizing and scoring item-level data
* Added function for standardizing scale-level data using normative data

#### BUG FIXES ####

* Changed OpenMP flags in Makevars to fix a compile problem on Debian machines
* Fixed a bug related to calculating angular medians in the presence of NAs

#### MINOR IMPROVEMENTS ####

* Changed the default to plot profiles with low fit (but with dashed borders)
* Import and export functions from rlang tidy evaluation
* Added unit testing of various functions to increase code coverage

#### DOCUMENTATION FIXES ####

* Redesigned package website to be more attractive and clear
* Updated SSM vignette to use standardization function

circumplex 0.1.2
=============================

#### NEW FEATURES ####

* Profiles with low fit now have dashed borders when plotted

#### BUG FIXES ####

* Fixed bug that prevented compilation on Solaris systems
* Fixed bug that prevented CRAN checks on old R versions

#### DOCUMENTATION FIXES ####

* Improved the formatting of vignette source code

circumplex 0.1.1
=============================

#### NEW FEATURES ####

* [Package website](https://circumplex.jmgirard.com) added using [pkgdown](https://pkgdown.r-lib.org/)

#### DOCUMENTATION FIXES ####

* Fixed documentation to meet CRAN standards

circumplex 0.1.0
=============================

#### NEW FEATURES ####

* Submitted to CRAN
