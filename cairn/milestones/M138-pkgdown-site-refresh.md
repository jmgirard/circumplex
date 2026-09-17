# M138: The website has a light navbar, a theme switch and grouped vignette menus

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — changes the published website
- **Branch/PR:** `m138-pkgdown-site-refresh`

## Goal

The pkgdown site drops its dark navbar for a theme the maintainer picks, gains a light and dark mode switch, and groups its Vignettes menu by level.

## Scope

**In:** `_pkgdown.yml`. The theme moves from the old `template: params: bootswatch: flatly` spelling to the current `template: bslib: preset:` slot, with a preset the maintainer picks from rendered candidates at a gate during implementation. `template: light-switch: true` is added. The hand-written `navbar: right:` list is replaced by a `navbar: structure: right:` list, so pkgdown's own search and theme-switch components come back. The `navbar: left:` list stays hand-written, and its Vignettes menu gains one heading per level with dividers, in the articles index's order. The dead `template: params: docsearch` keys go. `tools/check-pkgdown-vignettes.R` is repaired to read a grouped menu, keeping the promise it makes today.

**Out:** the guard's other gaps, and running it in a workflow, stay on the candidate row "Harden the vignette frame and split guards". Grouping the 15-entry Instruments menu is out, because it needs a grouping rule of its own. A Quarto-based site through `altdoc` is out and now has its own candidate row. Vignette prose and figures are out. No NEWS.md entry is owed, because no package behavior changes.

## Acceptance criteria

- [x] AC1: Read with `yaml::read_yaml()`, `cfg$template$bslib$preset` is `"zephyr"`. The search `grep -rn "preset" _pkgdown.yml` finds exactly one line, and `grep -rn "bootswatch" _pkgdown.yml` finds none. This review makes both builds below on one machine and toolchain: a branch build into an empty directory, and a build of master. The procedure for each build is a grep over the `<nav ...>` opening tags of every `.html` file that build wrote, and the clauses below are about those tags, not the lines that hold them. In the branch build, no `<nav ...>` opening tag carries a `bg-` class or a `data-bs-theme` attribute. In each build, the `<nav ...>` opening tags bearing `aria-label="Site navigation"` number one per nav-bearing `.html` file, over the same page set AC2 pins, and in the master build every one of them carries `bg-primary` and `data-bs-theme="dark"`.
- [x] AC2: `_pkgdown.yml` sets `template: light-switch: true` and declares `navbar: structure: right:` naming the search, lightswitch and github components. Read with `yaml::read_yaml()`, `cfg$navbar$right` is NULL and `cfg$navbar$structure$right` is `[search, lightswitch, github]`. In the final site, built by `pkgdown::build_site()` into an empty directory at T6, the check reads every `.html` file that holds a `<nav` element. Each such file holds the strings `id="dropdown-lightswitch"`, `data-bs-theme-value` and `id="search-input"`. No `<nav`-bearing file of T1's build of master holds any of the three. The final build writes the same set of `.html` paths as that build, and the files carrying no `<nav` are the same eight in both. The procedure is a grep over every `.html` file each build wrote.
- [x] AC3: The Vignettes menu stays hand-written under `navbar: left:`, with a `text:`-only entry per level and `text: "---------"` separator entries, and no `articles:` group carries a `navbar:` key. In the freshly built `index.html`, that dropdown holds exactly three `h6.dropdown-header` elements reading Introductory, Intermediate and Advanced in that order, a `hr.dropdown-divider` between consecutive groups, and under each heading the same pages in the same order as that level's group in the `articles:` index.
- [x] AC4: `tools/check-pkgdown-vignettes.R` exits 0 on the grouped `_pkgdown.yml`. In a scratch copy of the repo it exits 1 on each of five planted defects, one per checking path: a page dropped from the navbar menu, a menu entry whose text is not the vignette's title, a page moved to another level group in the `articles:` index, a page moved under the wrong level heading in the navbar menu alone, and an extra vignette file on disk, carrying a well-formed `\VignetteIndexEntry{}`, that the level map does not list. The script's message names the defect it found in each case.
- [x] AC5: `_pkgdown.yml` holds no `docsearch` key. The freshly built site has a `search.json` whose entries include the Bayesian article's title, and the search input of AC2 is present.
- [ ] AC6: `pkgdown::check_pkgdown()` reports no problem. The final build goes into an empty directory. T1 builds master on the same machine and toolchain. The final build writes no warning that T1's build did not also write, and the check compares the two warning lists. The `pkgdown.yaml` workflow is green on the pull request.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T4
- AC4 → T5
- AC5 → T3
- AC6 → T1, T6

## Tasks

- [x] T1: Build the site once from master into a temporary directory. Keep its warning list for AC6 and its built directory for AC1. Then render two or three candidate themes with a light navbar, each into its own directory. Screenshot the home page of each.
- [x] T2: Show the maintainer the screenshots at a gate chip. Apply the chosen preset under `template: bslib: preset`, removing `template: params: bootswatch`. Record the choice and the three screenshots of the picked build in the work log.
- [x] T3: Add `template: light-switch: true`. Replace `navbar: right:` with a `navbar: structure:` declaration that keeps the GitHub icon and restores pkgdown's search and lightswitch components. Delete the `docsearch` keys. Rebuild, grep the nav-bearing pages for the three strings, compare the page set with T1's master build, and read `search.json`.
- [x] T4: Group the Vignettes menu in `_pkgdown.yml` with a heading per level and a divider between groups, in the `articles:` index order. Rebuild and read the rendered dropdown.
- [x] T5: Repair `tools/check-pkgdown-vignettes.R` so it skips heading and divider entries, reads each group's pages under its heading, and fails when a page sits under the wrong heading. Plant AC4's five defects in a scratch copy of the repo, one at a time, and record each exit status and message as review evidence.
- [x] T6: Run `pkgdown::check_pkgdown()` and a final clean build. Compare the warning list with T1's. The pull request and the `pkgdown.yaml` wait belong to the review phase.

## Work log

- 2026-09-17: created by /milestone-plan. The plan gate chose a preset with a light navbar, picked from rendered candidates, over hand-tuned bslib colors, because a preset ships tested color pairings. Falsified by no candidate preset fitting the package's look.
- 2026-09-17: the plan gate chose to stay on pkgdown over trying a Quarto site through `altdoc`, because pkgdown 2.2.1 is current and no Quarto version of pkgdown exists. Falsified by a site need that pkgdown cannot meet. The alternative now has a candidate row.
- 2026-09-17: the plan gate chose to repair `tools/check-pkgdown-vignettes.R` only, over absorbing the guard-hardening candidate row or deleting the guard, because a repair keeps the guard's promise unchanged. Falsified by the repair needing the guard's promise to widen.
- 2026-09-17: criteria audit (full mode, fresh [O] reader) found 6 items on the draft. All were fixed before writing. pkgdown emits no `navbar-dark` class, so AC1 now reads the built pages. A hand-written `navbar: right:` list drops the switch and the search box, so AC2 requires `navbar: structure:` and checks for the switch's own elements. `docs/` is not committed, so AC1, AC2, AC3 and AC5 name a fresh build. AC4's four defects hit one checking path each. AC6 names how the warning baseline is produced.
- 2026-09-17: second fresh [O] audit of the written criteria found 5 items, all fixed. With the light switch on, pkgdown writes no navbar background class and no dark theme attribute, so AC1 now reads the config, a `bg-dark` search and the gate screenshots. A present `navbar: left:` key beats `structure:`, so the structure list covers `right:` only. AC3 now rules out pkgdown's own `articles: navbar:` grouping, which adds a divider before the first heading. AC4's planted vignette carries an index entry, and a fifth defect probes the repair's new wrong-heading path.

- 2026-09-17: implement started on branch `m138-pkgdown-site-refresh`. The step-3 gate merges into the T2 preset gate. The plan already puts the preset pick after rendered candidates.
- 2026-09-17: pkgdown's guide says a bootswatch preset is unlikely to work with the light switch. T1 renders each candidate with the switch on. Plain Bootstrap 5 joins the candidate set.
- 2026-09-17: T1 rendered four candidates, `zephyr`, `litera`, `cosmo` and plain Bootstrap 5. All four gave a light navbar, a working search box and a working switch. All four rendered correctly in dark mode, so pkgdown's caveat did not bite.
- 2026-09-17: T1 built master into a scratch directory. Its warning list is 262 `--mathml` and 14 `--mathjax` pandoc deprecation lines, and nothing else. Its navbar line is `<nav class="navbar navbar-expand-lg fixed-top bg-primary" data-bs-theme="dark" ...>`.
- 2026-09-17: T2 gate. The maintainer picked `zephyr` from the four rendered candidates.
- 2026-09-17: amendment, substantive, taken at the T2 gate. AC1's `bg-dark` search passed on master as well, because master's navbar carries `bg-primary` and `data-bs-theme="dark"`. AC1 now names `zephyr`, forbids any `bg-` class and any `data-bs-theme` attribute on a `<nav>` line, and names its grep procedure. The screenshot clause and the gated-pick clause moved to T2.
- 2026-09-17: amendment, substantive, taken at the same gate. AC6 allowed only `--mathml` lines, so a build identical to master fails it. AC6 now defers to T1's kept master list and requires the final build to go into an empty directory. Coverage became AC1 to T1, T2, T3 and AC6 to T1, T6. T1 also keeps the master build directory.
- 2026-09-17: T2 applied `template: bslib: preset: zephyr` and removed `template: params: bootswatch`. Three screenshots of the picked build were taken in this session. They are the home page, `reference/octants.html` and `articles/using-instruments.html`, all with a light navbar.
- 2026-09-17: the T2 checkpoint landed while `devtools::test()` was still running. The next work-log line records what the suite returned.
- 2026-09-17: that suite run came back clean. It reported 0 failures, 11319 passes, 1 skip and 11 pre-existing warnings, in 9 minutes 50 seconds.
- 2026-09-17: T3 added `light-switch: true`, replaced `navbar: right:` with `navbar: structure: right: [search, lightswitch, github]`, and deleted the `docsearch` keys. Its build wrote 110 pages, 102 of them with a navbar.
- 2026-09-17: the T3 checks ran on that build. All 102 nav-bearing pages hold the three strings, and no nav-bearing page of the master build holds any of them. The page sets and the eight stub paths match master exactly. `search.json` has 688 entries, including the Bayesian article.
- 2026-09-17: amendment, substantive, taken at a T3 gate. AC2 asked every `.html` file for the switch, but 8 of the 110 are pkgdown redirect stubs with no navbar, on master too. AC2 now reads the nav-bearing pages only. It also pins the page set against master, adds the master negative control, and names a `yaml::read_yaml()` procedure for its configuration clauses. Coverage became AC2 to T1, T3.
- 2026-09-17: catch-up. The T5 repair of `tools/check-pkgdown-vignettes.R` was swept into the T3 commit by a `git add -A`. The repair belongs to T5 and its record sits below.
- 2026-09-17: T4 grouped the Vignettes menu with a heading per level and a divider between groups, in the `articles:` index order.
- 2026-09-17: T5 repaired the guard. It now skips dividers, reads each page under the heading above it, and fails a page under the wrong heading. It exits 0 on the grouped config and on an unplanted scratch copy.
- 2026-09-17: T5 planted AC4's five defects, one per scratch copy. All five exited 1, and each message named the defect. The five were a dropped menu page, a wrong menu text, a page moved in the articles index, a page under the wrong navbar heading, and an extra vignette file.
- 2026-09-17: the T4 read of the rendered dropdown in the final `index.html`. It holds three `h6.dropdown-header` elements, Introductory then Intermediate then Advanced, and two `hr.dropdown-divider` elements, one between each pair of groups. Each level's pages sit under its heading in the `articles:` index order, each with its vignette title.
- 2026-09-17: T6 ran `pkgdown::check_pkgdown()`, which reported no problems, and a final build into an empty directory. That build wrote 110 pages, 102 with a navbar, all holding the three strings, with the page set and the eight stubs matching master.
- 2026-09-17: the final log carries 25 more `--mathml` lines than the master baseline, and no warning of any other kind. The extra lines land before the build's first section marker, in the same process as the `check_pkgdown()` call that ran first. A second build without that call is running to give a like-for-like list.
- 2026-09-17: the second build, run without the `check_pkgdown()` call, wrote a warning list identical to the master baseline. Both are 262 `--mathml` and 14 `--mathjax` lines. The 25 extra lines came from that call, not from the build.
- 2026-09-17: claim audit: 14 claims read, 3 corrected — `_pkgdown.yml`, `tools/check-pkgdown-vignettes.R`
- 2026-09-17: the claim audit found the dash rule stated too loosely and a divider promise the guard never kept. The `_pkgdown.yml` comment now says three or more dashes. The guard comment drops the divider promise, and its dash test became pkgdown's own `^\s*-{3,}\s*$`. A planted one-dash entry now fails, because pkgdown renders it as a heading.
- 2026-09-17: amendment, minor. T6 no longer says to open the pull request. The git model opens it at the review phase, after the user approves at the merge gate.
- 2026-09-17: `devtools::test()` ran again after the last code change and came back clean, with 0 failures and 11319 passes.
- 2026-09-17: all six tasks are done and the local checks are clean, so the status moves to review.
- 2026-09-17: re-audit: AC2 (full) — 6 findings, all fixed before writing. Narrowing to nav-bearing pages left an undercount hole, and the master negative control was available and unused. The configuration clauses named no procedure, a flat grep cannot separate the two `right:` keys, Coverage omitted T1, and the T3 task text no longer matched.
- 2026-09-17: re-audit: AC1 (full) — 7 findings, all fixed before writing. They were an unsatisfiable grep sentence, two clauses binding the evidence record, and an unbounded causal claim. The rest were an unnamed nav search procedure, no named theme, a two-string darkness test, and a Coverage row missing T3.
- 2026-09-17: re-audit: AC6 (full) — 3 findings, all fixed before writing. The named pandoc line kinds pinned a toolchain version into the criterion. The dropped empty-directory condition let an incremental rebuild pass for the wrong reason. The baseline also needed a same-toolchain clause.

- 2026-09-17: review ran. AC2 to AC5 verified with fresh evidence, the consistency gate is clean, `devtools::check()` is Status OK, and the three-lens fan-out found no acceptance criterion failing. Evidence and every finding are in the Review section.
- 2026-09-17: amendment return: AC1 — "Every site-navigation `<nav>` line of a build of master, the line carrying `aria-label="Site navigation"`, carries `bg-primary` and `data-bs-theme="dark"`."

- 2026-09-17: amendment return: AC1 — "Read with `yaml::read_yaml()`, `cfg$template$bslib$preset` is `"zephyr"`. The search `grep -rn "preset" _pkgdown.yml` finds exactly one line, and `grep -rn "bootswatch" _pkgdown.yml` finds none. This review makes both builds below on one machine and toolchain: a branch build into an empty directory, and a build of master. The procedure for each build is a grep over the `<nav ...>` opening tags of every `.html` file that build wrote, and the clauses below are about those tags, not the lines that hold them. In the branch build, no `<nav ...>` opening tag carries a `bg-` class or a `data-bs-theme` attribute. In each build, the `<nav ...>` opening tags bearing `aria-label="Site navigation"` number one per nav-bearing `.html` file, over the same page set AC2 pins, and in the master build every one of them carries `bg-primary` and `data-bs-theme="dark"`."
- 2026-09-17: re-audit: AC1 (full) — 4 findings on the bare narrowing, all disposed at the mini gate. The narrowing could go vacuously true, because pkgdown emits the aria-label through gettext with catalogs for 12 locales. "A build of master" quantified over all builds. The branch clause stays a marker proxy for the Goal, accepted as a limitation. The AC2 cross-reference pointed at T6's build.
- 2026-09-17: re-audit: AC1 (full) — 4 findings on the gate-fixed wording, verdict not fit to write, all four repaired before writing. The AC2 build reference contradicted AC2's own "at T6". The marker test was scoped to the line, not the element, and every branch nav element carries `data-bs-theme-value` seven lines below its opening tag. The preset clause named no procedure for a nesting-and-value claim. The count 102 was imported from evidence, not from AC2. This is AC1's second re-audit line, so the stop is reached and the wording went to the user, who chose the repaired text.
- 2026-09-17: the mini gate accepted the marker-versus-appearance gap as a limitation: AC1 checks that pkgdown wrote no navbar background class, not that the navbar renders light. It goes to DESIGN.md Known issues at the hygiene pass.
- 2026-09-17: the mini gate also directed the two guard-comment corrections the review triaged fix-now. The docstring now states that the menu's headings must be exactly the level map's, in its order. The dash-rule comment now says pkgdown's `menu_type()` tests the dash pattern before it looks at `href`, while this check tests it only among the href-less entries, so an entry carrying both dash text and an href fails rather than passing wrongly — planted and observed: exit 1, naming `ci-accuracy`.
- 2026-09-17: `devtools::test()` clean after the comment corrections: 0 failures, 11319 passes, 1 skip, 11 pre-existing warnings. The AC4 five-defect battery still exits 0 on the control and 1 on each defect.

- 2026-09-17: claim audit: 13 claims read, 0 corrected — `_pkgdown.yml`, `tools/check-pkgdown-vignettes.R`. The reader checked each comment by planting its case in a scratch copy and by calling pkgdown's `menu_type()` and `navbar_html()` directly.
- 2026-09-17: the amendment is done and the verify slot is clean, so the status returns to review.
- 2026-09-17: re-review in progress. AC1 to AC5 verified with fresh evidence from two new builds; AC6's build half verified, its CI clause waits on the pull request. Consistency gate and the independent review are still running.

## Review

Second review pass, after the AC1 amendment. Fresh evidence gathered 2026-09-17 on
`m138-pkgdown-site-refresh` at `18a1c2ef`, with master unmoved since the branch was
cut (`git rev-list --left-right --count origin/master...HEAD` is `0 10`). Two builds
were made for this pass on one machine and toolchain: master built from a scratch
worktree into an empty directory, and the branch built into a second empty directory.
Both exited 0.

### Acceptance criteria

- **AC1 — verified.** Configuration half: `yaml::read_yaml("_pkgdown.yml")` gives
  `cfg$template$bslib$preset` equal to `"zephyr"`; `grep -rn "preset" _pkgdown.yml`
  returns exactly one line (`_pkgdown.yml:12`); `grep -rn "bootswatch" _pkgdown.yml`
  returns none. Build half, by the criterion's named procedure — every `<nav ...>`
  opening tag of every `.html` file each build wrote, matched as a tag and not as a
  line. Branch build: of all its `<nav ...>` opening tags, none carries a `bg-` class
  or a `data-bs-theme` attribute (0 of them). The two nav tag kinds it emits are
  `<nav class="navbar navbar-expand-lg fixed-top " aria-label="Site navigation">` and
  `<nav id="toc" aria-label="Table of contents">`. Site-navigation tags: in each
  build, the tags bearing `aria-label="Site navigation"` number exactly one per
  nav-bearing `.html` file — 102 files with exactly one and 8 with none, in both
  builds, and that file set equals the nav-bearing set AC2 pins. In the master build
  all 102 of them carry `bg-primary` and `data-bs-theme="dark"`
  (`<nav class="navbar navbar-expand-lg fixed-top bg-primary" data-bs-theme="dark"
  aria-label="Site navigation">`).
- **AC2 — verified.** `yaml::read_yaml("_pkgdown.yml")` gives `cfg$navbar$right` NULL,
  `cfg$navbar$structure$right` equal to `search, lightswitch, github`, and
  `cfg$template$"light-switch"` TRUE. The branch build is this pass's own, made by
  AC2's named procedure (`pkgdown::build_site()` into an empty directory), standing in
  for T6's build, which the review re-makes for freshness. It wrote 110 `.html` files,
  102 of them holding a `<nav` element. All 102 hold `id="dropdown-lightswitch"`,
  `data-bs-theme-value` and `id="search-input"` (102 of 102 for each of the three).
  Of the master build's 102 nav-bearing files, none holds any of the three (0 of 102
  for each). The two builds wrote the identical set of 110 `.html` paths, and the
  eight files carrying no `<nav` are the same eight in both
  (`reference/GeomSsmArc.html`, `reference/GeomSsmPath.html`,
  `reference/GeomSsmPoint.html`, `reference/circumplex.html`,
  `reference/simulated_growth_origin.html`, and the three
  `reference/ssm_plot_trajectory.*.html` stubs).
- **AC3 — verified.** The Vignettes menu stays under `navbar: left:` with three
  `text:`-only heading entries (Introductory, Intermediate, Advanced) and two
  `text: "---------"` separator entries, and no `articles:` group carries a `navbar:`
  key (zero, counted through `yaml::read_yaml()`). Parsed out of the freshly built
  `index.html`, the dropdown's element sequence is heading, 2 pages, divider, heading,
  4 pages, divider, heading, 8 pages: exactly three `h6.dropdown-header` elements
  reading Introductory, Intermediate, Advanced in that order, exactly two
  `hr.dropdown-divider` elements, one between each consecutive pair, and under each
  heading the same pages in the same order as that level's group in the `articles:`
  index, each carrying its vignette title.
- **AC4 — verified.** The guard exits 0 on the shipped `_pkgdown.yml` ("the articles
  index, the navbar menu and vignettes/ agree on 14 pages") and on an unplanted scratch
  copy. Five defects were then planted, one per scratch copy, and each exited 1 with a
  message naming the defect: a page dropped from the navbar menu (named the short
  Intermediate list against the expected one), a menu text that is not the vignette
  title (named `axes-reliability` and both strings), a page moved to another level
  group in the `articles:` index (named both affected article groups), a page moved
  under the wrong navbar heading alone (named both affected navbar groups, and no
  articles-group failure, so the two paths are distinguished), and an extra vignette
  file on disk carrying a well-formed `\VignetteIndexEntry{}` (named `stray-page`).
- **AC5 — verified.** `_pkgdown.yml` holds no `docsearch` key
  (`grep -rn "docsearch" _pkgdown.yml` returns nothing). The freshly built
  `search.json` has 688 entries and includes the title "Bayesian SSM Analysis". The
  AC2 search input, `id="search-input"`, is present in the built `index.html`.
- **AC6 — partially verified, not ticked.** `pkgdown::check_pkgdown()` reports "No
  problems found." Both builds went into empty directories on one machine and
  toolchain. The two warning lists are identical: 262 `[WARNING] Deprecated: --mathml`
  lines and 14 `[WARNING] Deprecated: --mathjax` lines in each, and no other warning
  or error line in either log, so the branch build writes no warning the master build
  did not also write. The remaining clause, that the `pkgdown.yaml` workflow is green
  on the pull request, cannot be evidenced before the pull request exists. It is
  checked at the step-8 CI wait, which merges only on green, so the box stays unticked
  at the approval gate.

## Decisions
