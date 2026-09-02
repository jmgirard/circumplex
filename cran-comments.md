## Test environments

* local macOS 26.6.2, R 4.6.1 — `devtools::check(manual = TRUE)` and
  `R CMD check --as-cran` on the built tarball
* win-builder, R-devel (2026-08-31 r90457)
* GitHub Actions — macOS/release, windows/release, ubuntu/devel,
  ubuntu/release, ubuntu/oldrel-1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None on CRAN.

## Notes

This is a major release; the user-visible behavior changes that motivate the
version bump are listed under "Breaking changes and changed behavior" in
NEWS.md. Two points on dependencies:

* `ggplot2` moves to (>= 4.0.0), whose coordinate-system API the rebuilt
  plotting layer subclasses. `ggforce` is dropped and base R's `grid` and
  `parallel` are added, so Imports goes from seven packages to eight with no
  new third-party dependency.
* `Depends: R` moves from (>= 3.4) to (>= 4.1). This corrects an understated
  declaration rather than adding a restriction: ggplot2 (>= 4.0.0) and
  htmlTable already require R (>= 4.1).
