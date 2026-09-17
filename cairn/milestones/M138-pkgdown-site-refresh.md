# M138: The website has a light navbar, a theme switch and grouped vignette menus

- **Status:** in-progress
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

- [ ] AC1: `_pkgdown.yml` names `zephyr` under `template: bslib: preset`. The search `grep -rn "preset" _pkgdown.yml` finds exactly one line, and `grep -rn "bootswatch" _pkgdown.yml` finds none. In the site built for AC2, no `<nav>` line carries a `bg-` class or a `data-bs-theme` attribute. Every `<nav>` line of a build of master carries `bg-primary` and `data-bs-theme="dark"`. The procedure for each build is a grep over the `<nav ...>` lines of every `.html` file that build wrote.
- [ ] AC2: `_pkgdown.yml` sets `template: light-switch: true` and declares `navbar: structure: right:` naming the search, lightswitch and github components. Read with `yaml::read_yaml()`, `cfg$navbar$right` is NULL and `cfg$navbar$structure$right` is `[search, lightswitch, github]`. In the final site, built by `pkgdown::build_site()` into an empty directory at T6, the check reads every `.html` file that holds a `<nav` element. Each such file holds the strings `id="dropdown-lightswitch"`, `data-bs-theme-value` and `id="search-input"`. No `<nav`-bearing file of T1's build of master holds any of the three. The final build writes the same set of `.html` paths as that build, and the files carrying no `<nav` are the same eight in both. The procedure is a grep over every `.html` file each build wrote.
- [ ] AC3: The Vignettes menu stays hand-written under `navbar: left:`, with a `text:`-only entry per level and `text: "---------"` separator entries, and no `articles:` group carries a `navbar:` key. In the freshly built `index.html`, that dropdown holds exactly three `h6.dropdown-header` elements reading Introductory, Intermediate and Advanced in that order, a `hr.dropdown-divider` between consecutive groups, and under each heading the same pages in the same order as that level's group in the `articles:` index.
- [ ] AC4: `tools/check-pkgdown-vignettes.R` exits 0 on the grouped `_pkgdown.yml`. In a scratch copy of the repo it exits 1 on each of five planted defects, one per checking path: a page dropped from the navbar menu, a menu entry whose text is not the vignette's title, a page moved to another level group in the `articles:` index, a page moved under the wrong level heading in the navbar menu alone, and an extra vignette file on disk, carrying a well-formed `\VignetteIndexEntry{}`, that the level map does not list. The script's message names the defect it found in each case.
- [ ] AC5: `_pkgdown.yml` holds no `docsearch` key. The freshly built site has a `search.json` whose entries include the Bayesian article's title, and the search input of AC2 is present.
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
- [ ] T4: Group the Vignettes menu in `_pkgdown.yml` with a heading per level and a divider between groups, in the `articles:` index order. Rebuild and read the rendered dropdown.
- [ ] T5: Repair `tools/check-pkgdown-vignettes.R` so it skips heading and divider entries, reads each group's pages under its heading, and fails when a page sits under the wrong heading. Plant AC4's five defects in a scratch copy of the repo, one at a time, and record each exit status and message as review evidence.
- [ ] T6: Run `pkgdown::check_pkgdown()` and a final clean build. Compare the warning list with T1's. Open the pull request and wait for the `pkgdown.yaml` workflow.

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
- 2026-09-17: re-audit: AC2 (full) — 6 findings, all fixed before writing. Narrowing to nav-bearing pages left an undercount hole, and the master negative control was available and unused. The configuration clauses named no procedure, a flat grep cannot separate the two `right:` keys, Coverage omitted T1, and the T3 task text no longer matched.
- 2026-09-17: re-audit: AC1 (full) — 7 findings, all fixed before writing. They were an unsatisfiable grep sentence, two clauses binding the evidence record, and an unbounded causal claim. The rest were an unnamed nav search procedure, no named theme, a two-string darkness test, and a Coverage row missing T3.
- 2026-09-17: re-audit: AC6 (full) — 3 findings, all fixed before writing. The named pandoc line kinds pinned a toolchain version into the criterion. The dropped empty-directory condition let an incremental rebuild pass for the wrong reason. The baseline also needed a same-toolchain clause.

## Decisions
