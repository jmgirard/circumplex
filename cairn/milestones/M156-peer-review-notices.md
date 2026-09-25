# M156: Peer-review status notices on the four package-original vignettes

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — vignette text that ships inside the package and on the pkgdown site
- **Branch/PR:** m156-peer-review-notices

## Goal

Each vignette that teaches a method the package proposes without a peer-reviewed source opens with a notice that says so.

## Scope

**In:** A warning notice on four pages: "SEM-Based SSM Analysis", "Latent Group Contrasts", "Bayesian SSM Analysis" and "Growth Models on SSM Parameters". The notice is a pandoc fenced div `::: {.alert .alert-warning role="alert"}` placed after the Level paragraph and before `## 1. Overview`. pkgdown's Bootstrap styles it as a warning box, and the installed vignette HTML shows it as a plain paragraph. Its text, with the method name changed per page:

> **Not yet peer reviewed.** The latent Structural Summary Method this page teaches is the package's own proposal. Its authors have not yet published it in a peer-reviewed venue. Read it as a research tool, and state that status when you report results from it.

The method names: the latent Structural Summary Method (SEM-Based SSM Analysis), the invariance-gated latent contrast (Latent Group Contrasts), the Bayesian SSM recipe (Bayesian SSM Analysis), the growth-model recipe on SSM coordinates (Growth Models on SSM Parameters). A `frame_notice` vector in `tests/testthat/helper-vignette-frame.R` lists the four pages, and `tests/testthat/test-vignette-frame.R` pins the notice's presence, position and lead on those pages and its absence elsewhere. The three pre-computed pages are re-rendered from their `.Rmd.orig` sources. A NEWS entry records the change.

**Out:** The same notice on the help pages of `ssm_sem()`, `ssm_growth_data()` and their siblings → the "Peer-review status notices beyond M156" candidate row. Notices on the three pages that extend a published method with package-derived pieces (Axes Reliability Caveats, Confidence Interval Accuracy, Structure Tests) → the same candidate row. A change to the frame test's Level-line rule → not needed, since the notice sits below the Level paragraph.

## Acceptance criteria

- [ ] AC1: Each page in `frame_notice` carries exactly one fenced div that opens with `::: {.alert .alert-warning role="alert"}`, placed after the blank line that ends the Level paragraph and before `## 1. Overview`, and its first prose line begins `**Not yet peer reviewed.**`. Every other page in `names(frame_levels)` has no line that matches `alert-warning` or `peer reviewed` (case-insensitive). `tests/testthat/test-vignette-frame.R` asserts both over every name in `names(frame_levels)`, reading the source as `frame_path()` resolves it.
- [ ] AC2: On each `frame_notice` page, the lines between the notice's opener and its closing `:::`, piped to `Rscript tools/prose-sweep.R -`, exit 0. The first sentence after the bold lead names the page's method as the Scope lists it. One sentence states that the authors have not yet published the method in a peer-reviewed venue.
- [ ] AC3: `rmarkdown::render()` of each `frame_notice` page's shipped `vignettes/<name>.Rmd` produces HTML in which a `<div class="alert alert-warning" role="alert">` element occurs before the `<div id="overview"` element.
- [ ] AC4: With lavaan and glmmTMB installed, after `Rscript tools/precompute-vignettes.R <name>` runs for each of the three pre-computed `frame_notice` pages and the regenerated `.Rmd` files are committed, `Rscript tools/check-vignette-staleness.R` exits 0 on a clean tree.
- [ ] AC5: `Rscript -e 'devtools::test()'` is clean. `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that the same command does not report on the default branch.
- [ ] AC6: `NEWS.md` carries, under the development-version heading, a Documentation entry that names the four pages that gained the notice.

## Coverage

- AC1 → T1, T2, T5
- AC2 → T2
- AC3 → T4
- AC4 → T3
- AC5 → T6
- AC6 → T6

## Tasks

- [x] T1: Add `frame_notice` to `tests/testthat/helper-vignette-frame.R` and the AC1 test to `tests/testthat/test-vignette-frame.R`. Run the test on the unchanged tree and record in the work log that it is red for the four pages.
- [x] T2: Write the notice into `vignettes/sem-based-ssm-analysis.Rmd.orig`, `vignettes/sem-latent-contrasts.Rmd.orig`, `vignettes/growth-ssm-analysis.Rmd.orig` and `vignettes/bayesian-ssm-analysis.Rmd`, after the Level paragraph. Pipe each notice to the prose sweep. Read each notice against the Scope's method list and the AC2 claim.
- [ ] T3: Run `devtools::install()`, re-render the three pre-computed pages with `tools/precompute-vignettes.R`, and commit the regenerated `.Rmd` files. Then run the staleness check on the clean tree. Make sure that `git diff --stat` against the default branch touches only the intended files (M112: stage by path, no figure churn).
- [ ] T4: Render each shipped `.Rmd` to the scratchpad and grep for the div before the overview section. Build one page with `pkgdown::build_article()` and look at it once (M33).
- [ ] T5: Plant checks, each reverted after its red run: remove one page's notice; add a second notice to one page; change one page's bold lead; move one notice below `## 1. Overview`; add a notice under a different opener form (`<div class="alert alert-warning">`) to a page outside `frame_notice`.
- [ ] T6: Write the NEWS entry. Run `devtools::test()` and `devtools::check(args = "--no-manual")`, and compare the notes with a run on the default branch.

## Work log

- 2026-09-25: created by /milestone-plan.
- 2026-09-25: criteria audit ran in full mode ([O] reader). It returned findings on AC1 (instrument-bound wording, an opener-spelling proxy for "no other page carries a notice", a two-line Level paragraph), AC2 (the bold lead is the first sentence, an unbounded literature claim, an unnamed method), AC4 (the staleness check reads the committed copy) and T5 (plants varied neither form nor location). All were fixed in the wording above.
- 2026-09-25: plan gate chose a warning box after the Level paragraph over a box above it or a plain bold paragraph, because the frame test stays untouched and the site styles it; falsified by a reader report that the notice goes unseen below the Level line, or that the unstyled installed copy misleads.
- 2026-09-25: plan gate chose the four package-original pages over adding the three pages that extend a published method, because those apply a peer-reviewed method and already state what the package measured; falsified by a reader citing one of those pages as a validated method as a whole.
- 2026-09-25: implement started on `m156-peer-review-notices`; the question gate was skipped, since the plan gate settled form, place, wording and the test shape. glmmTMB loads with a TMB version-mismatch warning on this machine, watched at T3.
- 2026-09-25: T1 done. `frame_notice` added, the notice test appended to `test-vignette-frame.R`; on the unchanged tree it fails for exactly the four listed pages (zero openers) and passes the other ten. A `skip_if` inside the loop first hid three of the four, replaced by `next` (M146).
- 2026-09-25: plan gate chose vignettes only over adding the help pages now, because the help-page change roughly doubles the milestone; falsified by a user reaching a package-original method from its help page with no status shown.
- 2026-09-25: T2 done. The notice is in the three `.Rmd.orig` sources and in `bayesian-ssm-analysis.Rmd`, each body exits 0 from the prose sweep on stdin, each first sentence after the lead names the Scope's method, and the frame test file is green.

## Decisions

## Review
