# Contributing to circumplex

This outlines how to propose a change to circumplex. The package is maintained
by one person, so the surest route to a merged change is to agree on the
problem before you write the fix.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, please raise it first, so we can
agree the change is wanted before you spend time on it:

*  A bug → open an
   [issue](https://github.com/jmgirard/circumplex/issues) illustrating it with a
   minimal [reprex](https://reprex.tidyverse.org/).
*  A feature idea, or a question about how something works → open a
   [discussion](https://github.com/jmgirard/circumplex/discussions).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).  
*  Pull requests are checked by the `R-CMD-check.yaml` GitHub Actions workflow.
A PR touching `R/`, `src/`, `tests/`, `vignettes/`, `data/`, `inst/`,
`DESCRIPTION` or `NAMESPACE` runs `R CMD check` on Windows, macOS and Ubuntu;
any other PR gets a single Ubuntu job. Check that it is green before and after
your changes. (Pushes to `master` run a wider matrix that adds R-devel and
oldrel.) `README.md` carries a badge for this workflow and for the coverage and
pkgdown ones.  
*  New code should match existing code style. This package is base R with
minimal dependencies and no automatic formatter is run over it, so follow the
conventions of the file you are editing, and please don't restyle code that has
nothing to do with your PR.  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation.  
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.  
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made followed by your
GitHub username, and links to relevant issue(s)/PR(s).

### Code of Conduct

Please note that the circumplex project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
