<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M173: Shorten NEWS and fix two pre-release details

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — NEWS.md, a vignette and DESCRIPTION ship to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Ready the development version for its CRAN release walk: a testthat floor the tests actually need, a correct vignette sentence on the analytic caution, and a development NEWS section cut to about 150 lines that loses no change a v2.0.1 user would notice.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** DESCRIPTION's testthat floor with a D-entry; the one "very large $N$" sentence in `evaluating-circumplex-structure` (`.Rmd.orig` and shipped `.Rmd`); a rewrite of NEWS.md lines 1-506 (51 bullets) that merges related bullets, drops steps between states that never shipped, and points to help pages or vignettes for detail.

**Out:** the version bump, `cran-comments.md`, `devtools::check(manual = TRUE)` and the arm64 harness, which belong to `/cairn-release`, so this milestone claims no release readiness. The other parts of the "Vignette prose follow-ons" row stay on that row. NEWS sections for released versions (2.0.1 and earlier) are not touched.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: DESCRIPTION's Suggests entry reads `testthat (>= V)`, where V is 3.1.7, or, if the 3.1.7 run fails, the lowest version under which the run passes, the versions tried including every CRAN release from 3.1.7 up to V. The run is the package's test suite under `testthat::test_local()` with `NOT_CRAN=true` set and testthat V loaded from a temporary library placed ahead of the user library; it passes when it reports 0 failures and 0 errors and no more skipped tests than the same run under the installed testthat 3.3.2.
- [ ] AC2: In `vignettes/evaluating-circumplex-structure.Rmd.orig` and the shipped `vignettes/evaluating-circumplex-structure.Rmd`, the sentence that said the analytic caution prints "up to very large $N$" when a boundary marker is present instead states that, when a boundary marker is present, the analytic caution prints for every $N$ below $N = 50000$, matching `cpm_analytic_ci_n_boundary_caution` in `R/cpm_fit.R`. `grep -n 'very large' vignettes/*.Rmd vignettes/*.Rmd.orig` prints nothing, and `git diff` of each of the two files against the plan commit shows only that sentence changed.
- [ ] AC3: Each row of the change ledger in this file's Decisions section (one row per user-visible change stated by the 51 development-section bullets of NEWS.md at the plan commit, each row citing its bullet) has one disposition that holds after the edit: (a) stated, where a post-edit development-section bullet states the change; (b) pointed, where a post-edit bullet names the change's subject and a help page (`?topic`) or vignette that, at the post-edit commit, states the change's after-state; or (c) dropped, where the row's before-state is absent from v2.0.1, shown by `git show v2.0.1:<path>` on the path the row names. Every row from a `## Breaking changes` bullet is (a), and its post-edit bullet sits under `## Breaking changes`. Every row naming a function, argument, dataset or vignette that v2.0.1 shipped, whose change alters returned values, refusals, warnings or printed output, is (a) or (b).
- [ ] AC4: Every function and argument named in backticks in a `## Breaking changes` bullet at the plan commit is named in backticks in a post-edit bullet under that heading (a script compares the two token sets).
- [ ] AC5: The post-edit development section of NEWS.md, from the `# circumplex (development version)` line to the line before `# circumplex 2.0.1`, is at most 150 lines (`awk` line count), down from 506. Where AC3 and AC5 cannot both hold, AC3 holds and AC5's figure is amended at a gate.
- [ ] AC6: Every backticked token and every numeric literal in the post-edit development section also appears in the development section at the plan commit, except a backticked `?topic` whose topic is an `\alias{}` in `man/*.Rd`, or a backticked `vignette("<name>")` whose `<name>.Rmd` is in `vignettes/` (a script checks both).
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures with the installed testthat, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors and 0 warnings.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3, T4
- AC4 → T4, T5
- AC5 → T4, T5
- AC6 → T4, T5
- AC7 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits). -->

- [ ] T1: Install testthat 3.1.7 (and any older dependency it needs) from the CRAN archive into a scratchpad library; run `testthat::test_local()` with `NOT_CRAN=true` under it and under 3.3.2, comparing failures, errors and skips. If 3.1.7 fails, try each later CRAN release in order and name in the work log the feature that raised the floor. Set the DESCRIPTION floor and append a D-entry for the re-pin.
- [ ] T2: Rewrite the sentence at `evaluating-circumplex-structure.Rmd.orig:167-168` and the shipped `.Rmd:228-229` identically, from `R/cpm_oop.R:383-394` and `R/cpm_fit.R:1566-1567` read in the same session.
- [ ] T3: Write the change ledger into this file's Decisions section and commit it before any commit that changes NEWS.md: one row per user-visible change, citing its bullet, naming the path whose v2.0.1 copy shows the before-state, and its planned disposition (a, b or c) with the post-edit bullet or the page it points to.
- [ ] T4: Rewrite NEWS.md lines 1-506 from the ledger, merging related bullets (for example the print-wrapping entries and the vignette splits), keeping the four headings, with Breaking changes first.
- [ ] T5: Run the checks: the AC4 and AC6 token scripts, the AC5 line count, a read of each (b) row's page, `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-28: created by /milestone-plan.
- 2026-10-28: tier check: milestone — a dependency re-pin needs a D-entry, and the NEWS rewrite is more than one sitting's quick edit.
- 2026-10-28: idea vet: no concern; the floor matches `local_mocked_bindings()` (testthat 3.1.7, 72 uses in tests), the vignette fix matches `R/cpm_oop.R:383-394`, and the user chose the aggressive NEWS cut knowing fine detail moves to help pages.
- 2026-10-28: lessons used: 2026-07-19 (M7 family; a step that doesn't run reports success), which shaped AC1's `NOT_CRAN=true` and skip-count terms.
- 2026-10-28: question set: NEWS depth — aggressive, 150 lines or fewer, detail pointed to help pages and vignettes.
- 2026-10-28: question set: testthat — may download 3.1.7 and older dependencies from the CRAN archive into the scratchpad; raise the floor to the lowest passing version if 3.1.7 fails.
- 2026-10-28: question set: Breaking changes wording may be shortened; every refusal and affected function stays listed under that heading.
- 2026-10-28: criteria audit (full mode, fresh Opus reader, two passes): fixed AC1 self-contradiction, moved the work-log naming to T1, added NOT_CRAN and skip count, reworded AC2 to "every N below 50000" (cpm_oop.R:384, 394), made AC3 quantify over a ledger, exempted Breaking rows from (c) per D-065, used "after-state" in (b), added AC4's Breaking token check, exempted resolvable pointers in AC6, and made AC3 win over AC5's figure.
- 2026-10-28: collision check: absorbs candidate "Raise the testthat floor to 3.1.7" in full and item (iv) of "Vignette prose follow-ons" in part (items i-iii stay on that row); no D-entry or archive covers a NEWS cut since M7's consolidation; inbox sweep found 0 open issues and 0 external PRs.
- 2026-10-28: sizing: three small parts kept in one milestone, one pre-release PR, 7 criteria and 5 tasks, under the tripwires.
- 2026-10-28: plan gate chose the aggressive NEWS cut over the about-half cut at the user's choice; falsified by a v2.0.1 user change that a pointed page does not state.
- 2026-10-28: plan gate chose a scripted token check (AC6) over a reviewer read alone for new claims; falsified by a new behavior claim in plain prose that the script cannot see, which review must still catch.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive -->
