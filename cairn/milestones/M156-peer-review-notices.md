# M156: Peer-review status notices on the four package-original vignettes

- **Status:** review
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

- [x] AC1: Each page in `frame_notice` carries exactly one fenced div that opens with `::: {.alert .alert-warning role="alert"}`, placed after the blank line that ends the Level paragraph and before `## 1. Overview`, and its first prose line begins `**Not yet peer reviewed.**`. Every other page in `names(frame_levels)` has no line that matches `alert-warning` or `peer reviewed` (case-insensitive). `tests/testthat/test-vignette-frame.R` asserts both over every name in `names(frame_levels)`, reading the source as `frame_path()` resolves it.
- [x] AC2: On each `frame_notice` page, the lines between the notice's opener and its closing `:::`, piped to `Rscript tools/prose-sweep.R -`, exit 0. The first sentence after the bold lead names the page's method as the Scope lists it. One sentence states that the authors have not yet published the method in a peer-reviewed venue.
- [x] AC3: `rmarkdown::render()` of each `frame_notice` page's shipped `vignettes/<name>.Rmd` produces HTML in which a `<div class="alert alert-warning" role="alert">` element occurs before the `<div id="overview"` element.
- [x] AC4: With lavaan and glmmTMB installed, after `Rscript tools/precompute-vignettes.R <name>` runs for each of the three pre-computed `frame_notice` pages and the regenerated `.Rmd` files are committed, `Rscript tools/check-vignette-staleness.R` exits 0 on a clean tree.
- [x] AC5: `Rscript -e 'devtools::test()'` is clean. `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that the same command does not report on the default branch.
- [x] AC6: `NEWS.md` carries, under the development-version heading, a Documentation entry that names the four pages that gained the notice.

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
- [x] T3: Run `devtools::install()`, re-render the three pre-computed pages with `tools/precompute-vignettes.R`, and commit the regenerated `.Rmd` files. Then run the staleness check on the clean tree. Make sure that `git diff --stat` against the default branch touches only the intended files (M112: stage by path, no figure churn).
- [x] T4: Render each shipped `.Rmd` to the scratchpad and grep for the div before the overview section. Build one page with `pkgdown::build_article()` and look at it once (M33).
- [x] T5: Plant checks, each reverted after its red run: remove one page's notice; add a second notice to one page; change one page's bold lead; move one notice below `## 1. Overview`; add a notice under a different opener form (`<div class="alert alert-warning">`) to a page outside `frame_notice`.
- [x] T6: Write the NEWS entry. Run `devtools::test()` and `devtools::check(args = "--no-manual")`, and compare the notes with a run on the default branch.

## Work log

- 2026-09-25: created by /milestone-plan.
- 2026-09-25: criteria audit ran in full mode ([O] reader). It returned findings on AC1 (instrument-bound wording, an opener-spelling proxy for "no other page carries a notice", a two-line Level paragraph), AC2 (the bold lead is the first sentence, an unbounded literature claim, an unnamed method), AC4 (the staleness check reads the committed copy) and T5 (plants varied neither form nor location). All were fixed in the wording above.
- 2026-09-25: plan gate chose a warning box after the Level paragraph over a box above it or a plain bold paragraph, because the frame test stays untouched and the site styles it; falsified by a reader report that the notice goes unseen below the Level line, or that the unstyled installed copy misleads.
- 2026-09-25: plan gate chose the four package-original pages over adding the three pages that extend a published method, because those apply a peer-reviewed method and already state what the package measured; falsified by a reader citing one of those pages as a validated method as a whole.
- 2026-09-25: implement started on `m156-peer-review-notices`; the question gate was skipped, since the plan gate settled form, place, wording and the test shape. glmmTMB loads with a TMB version-mismatch warning on this machine, watched at T3.
- 2026-09-25: T1 done. `frame_notice` added, the notice test appended to `test-vignette-frame.R`; on the unchanged tree it fails for exactly the four listed pages (zero openers) and passes the other ten. A `skip_if` inside the loop first hid three of the four, replaced by `next` (M146).
- 2026-09-25: plan gate chose vignettes only over adding the help pages now, because the help-page change roughly doubles the milestone; falsified by a user reaching a package-original method from its help page with no status shown.
- 2026-09-25: T2 done. The notice is in the three `.Rmd.orig` sources and in `bayesian-ssm-analysis.Rmd`, each body exits 0 from the prose sweep on stdin, each first sentence after the lead names the Scope's method, and the frame test file is green.
- 2026-09-25: T3 done. After `devtools::install(upgrade = FALSE)` the three pre-computed pages re-rendered with a diff of the seven notice lines each and nothing else (no figure churn, no glmmTMB warning in the output); the renders are committed and the staleness check runs on the clean tree in the same commit's log line below.
- 2026-09-25: staleness check on the clean tree after the T3 commit: all 11 pre-computed vignettes up to date, exit 0. T4 done: the four shipped `.Rmd` files rendered to scratch each put the `alert alert-warning` div before the `id="overview"` section (line 347 vs 353, 349 vs 356, 347 vs 353, 347 vs 354); `pkgdown::build_article("sem-based-ssm-analysis")` served locally shows a styled warning box between the Level line and the Overview heading.
- 2026-09-25: T5 done. Five plants, each with a non-empty `git diff --stat` before its run and reverted after: a removed notice (sem-based) fails at the opener count; a doubled notice (sem-latent-contrasts) fails at the opener count; a changed lead (bayesian) fails at the lead match; a notice moved below Overview (growth) fails at `opener < overview`; a `<div class="alert alert-warning">` notice on ci-accuracy fails at the unlisted-page line check. The clean tree is green.
- 2026-09-25: claim audit: 17 claims read, 0 corrected — tests/testthat/helper-vignette-frame.R, tests/testthat/test-vignette-frame.R, the four notice sources, the three re-rendered `.Rmd`, NEWS.md. Qualified, not corrected: the Bayesian and growth pages carry no provenance text of their own, and Nagy, Etzel & Lüdtke (2019), a covariate extension of Browne's free-angle model, is a published relative the SEM pages do not cite; neither bears on the notice's claims of package authorship and non-publication by its authors.
- 2026-09-25: T6 done. `devtools::test()` exit 0 with no failure; `devtools::check(args = "--no-manual")` on the branch: 0 errors, 0 warnings, 0 notes; the same check in a master worktree: 0 errors, 0 warnings, 1 note (hidden files, the worktree's own `.git` file), so the branch adds no note. Status set to review.

## Decisions

## Review

- 2026-09-25 AC1: fresh grep over all 14 `vignettes/*.Rmd`: the four `frame_notice` pages each carry exactly one `::: {.alert .alert-warning role="alert"}` opener at lines 19/21/20/25, each after the Level paragraph's blank line and before `## 1. Overview` at lines 28/30/27/32; the other ten pages have zero lines matching `alert-warning` or `peer reviewed` case-insensitively. `test-vignette-frame.R` run on its own via `testthat::test_file()`: all expectations pass, no failure, no skip.
- 2026-09-25 AC2: the notice body of each of the four pages, cut between the opener and its closing `:::` and piped to `Rscript tools/prose-sweep.R -`, exits 0 (four of four). Each first sentence after the bold lead names the Scope's method verbatim (latent Structural Summary Method; invariance-gated latent contrast; growth-model recipe on SSM coordinates; Bayesian SSM recipe), and each carries the sentence "Its authors have not yet published it in a peer-reviewed venue."
- 2026-09-25 AC3: `rmarkdown::render()` of the four shipped `.Rmd` files in a scratch copy: one `<div class="alert alert-warning" role="alert">` each, at HTML line 347/349/347/347, before `<div id="overview"` at 353/356/354/353.
- 2026-09-25 AC4: `Rscript tools/check-vignette-staleness.R` on the clean tree (git status empty): all 11 pre-computed vignettes up to date, exit 0; the three regenerated `.Rmd` files are committed on the branch (diffstat: 7 lines added each, nothing else).
- 2026-09-25 AC6: `NEWS.md` development-version heading carries a Documentation entry naming the four pages by title.
- 2026-09-25 consistency gate: `cairn_validate` all checks passed; `devtools::document()` no diff, 0 `resolve link` lines; `pkgdown::check_pkgdown()` no problems; README.md newer than README.Rmd; master watches: newest push runs with a verdict on `R-CMD-check.yaml` and `test-coverage.yaml` both `success` (715a34e3; the current tip 7a21d997 is a cairn-only commit, path-skipped); master-red alert check and dry run exit 0; branch-protection check exit 0; no new top-level files.
- 2026-09-25 AC5: `devtools::test()` exit 0: FAIL 0, WARN 12 (lavaan messages in the SEM and scaled-fit tests, no R code changed on this branch), SKIP 1 (`test-axes-scaled-fit.R:930`, environment skip), PASS 14452. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes, Status OK (7m 23s), so no note the default branch does not report.
- 2026-09-25 independent review, three lenses: [O] diff-bug eight candidates, ranked: (1) `test-vignette-frame.R` position checks index `[[1]]` on possibly empty vectors, so a page missing its closer or `## 1. Overview` errors out and the loop stops before the other pages report; (2) the listed-page count matches the exact opener only, so a second notice under another spelling passes on a listed page; (3) the unlisted-page regex `peer reviewed` misses the hyphenated `peer-reviewed`; (4) under `devtools::test()` the pre-computed pages are read from `.Rmd.orig`, shipped copies covered by check and the staleness check, informational; (5) NEWS "its authors" can read as the notice's authors; (6) notice "Its authors" could be read as an outside group; (7) the notice's authorship claim not checked against Zimmermann & Wright (2017); (8) `role="alert"` is meant for dynamic messages. [S] blame-history: zero findings, the diff is purely additive and contradicts no D-entry. [S] prior-review record: no prior-review evidence on the touched files, PR-thread probe empty, zero findings.
- 2026-09-25 triage at the gate (user chose merge with the fix-now edits): (1) fix now — the position checks now collect each landmark (Level line, blank line, Overview heading, closer) and fail with the missing name, then `next`; a planted missing closer on sem-based-ssm-analysis yields 1 failure, 0 errors, other pages still reported. (3) fix now — regex `peer[- ]reviewed`; a planted "not peer-reviewed yet" line in `ci-accuracy.Rmd.orig` fails at the notice-lines check. (5) fix now — NEWS clause now reads "the package's authors have not yet published the method". (2) rejected: AC1 defines the notice by its exact opener. (4) noted, informational. (6) rejected: Scope-fixed wording. (7) rejected: the claim as written stands, already qualified by the claim audit. (8) rejected: Bootstrap's own alert markup, the opener the plan fixed. Clean frame test green after the edits; NEWS paragraph passes the prose sweep.
- 2026-09-25: step-7 approval: m156-peer-review-notices approved for merge (with the three fix-now edits).
- 2026-09-25: correction to the triage line above: the NEWS paragraph did not pass the prose sweep at that point (two sentences over 25 words, one of them pre-existing); both are now split and the paragraph exits 0 from `Rscript tools/prose-sweep.R -`.
