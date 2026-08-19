# Contributing to circumplex

This outlines how to propose a change to circumplex. The package is
maintained by one person, so the surest route to a merged change is to
agree on the problem before you write the fix.

### Fixing typos

Small typos or grammatical errors in documentation may be edited
directly using the GitHub web interface, so long as the changes are made
in the *source* file.

- YES: you edit a roxygen comment in a `.R` file below `R/`.
- NO: you edit an `.Rd` file below `man/`.

### Generated files

Some files in this repository are generated, and a hand edit to one is
overwritten the next time it is rebuilt. Never edit these directly —
change the source and regenerate:

- `man/*.Rd` and `NAMESPACE` — regenerate with `devtools::document()`
  after changing a roxygen comment.  
- `R/RcppExports.R` and `src/RcppExports.cpp` — regenerate with
  [`Rcpp::compileAttributes()`](https://rdrr.io/pkg/Rcpp/man/compileAttributes.html)
  after changing anything below `src/`.  
- `README.md` — knitted from `README.Rmd` by `devtools::build_readme()`.

### Prerequisites

Before you make a substantial pull request, please raise it first, so we
can agree the change is wanted before you spend time on it:

- A bug → open an [issue](https://github.com/jmgirard/circumplex/issues)
  illustrating it with a minimal
  [reprex](https://reprex.tidyverse.org/).
- A feature idea, or a question about how something works → open a
  [discussion](https://github.com/jmgirard/circumplex/discussions).

### Pull request process

- We recommend that you create a Git branch for each pull request
  (PR).  
- Pull requests are checked by GitHub Actions: `R-CMD-check.yaml` runs
  `R CMD check`, and `pkgdown.yaml` rebuilds the documentation site. How
  wide the check matrix goes depends on which paths your PR touches —
  `tools/ci-matrix.R` holds that rule and the platform list. A PR
  touching only tracking files, `man/`, or `README.md` may not trigger a
  check run at all. Please make sure whatever does run is green before
  and after your changes.  
- `README.md` carries badges for these workflows. Note that the coverage
  badge tracks `master`, not your PR.  
- New code should match existing code style. This package keeps to
  base-R style with few dependencies (its numeric core is C++ via Rcpp),
  and no automatic formatter is run over it, so follow the conventions
  of the file you are editing, and please don’t restyle code that has
  nothing to do with your PR.  
- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown
  syntax](https://roxygen2.r-lib.org/articles/rd-formatting.html), for
  documentation.  
- We use [testthat](https://cran.r-project.org/package=testthat).
  Contributions with test cases included are easier to accept.  
- For user-facing changes, add a bullet at the top of `NEWS.md` — under
  the development-version heading if one is open, otherwise above the
  most recent release heading — describing the change, followed by your
  GitHub username and links to the relevant issue(s)/PR(s).

### Code of Conduct

Please note that the circumplex project is released with a [Contributor
Code of Conduct](http://circumplex.jmgirard.com/CODE_OF_CONDUCT.md). By
contributing to this project you agree to abide by its terms.
