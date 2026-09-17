# M138: The website has a light navbar, a theme switch and grouped vignette menus

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — changes the published website
- **Branch/PR:** —

## Goal

The pkgdown site drops its dark navbar for a theme the maintainer picks, gains a light and dark mode switch, and groups its Vignettes menu by level.

## Scope

**In:** `_pkgdown.yml`. The theme moves from the old `template: params: bootswatch: flatly` spelling to the current `template: bslib: preset:` slot, with a preset the maintainer picks from rendered candidates at a gate during implementation. `template: light-switch: true` is added. The hand-written `navbar: right:` list is replaced by a `navbar: structure: right:` list, so pkgdown's own search and theme-switch components come back. The `navbar: left:` list stays hand-written, and its Vignettes menu gains one heading per level with dividers, in the articles index's order. The dead `template: params: docsearch` keys go. `tools/check-pkgdown-vignettes.R` is repaired to read a grouped menu, keeping the promise it makes today.

**Out:** the guard's other gaps, and running it in a workflow, stay on the candidate row "Harden the vignette frame and split guards". Grouping the 15-entry Instruments menu is out, because it needs a grouping rule of its own. A Quarto-based site through `altdoc` is out and now has its own candidate row. Vignette prose and figures are out. No NEWS.md entry is owed, because no package behavior changes.

## Acceptance criteria

- [ ] AC1: `_pkgdown.yml` names exactly one theme, under `template: bslib: preset`. The searches `grep -rn "bootswatch" _pkgdown.yml` and `grep -rn "preset" _pkgdown.yml` show that one slot and no other. The preset is the one the maintainer picked at the T2 gate, and the Review holds the three screenshots of the picked build that show a light navbar. In the site built for AC2, no `<nav>` element carries a `bg-dark` class.
- [ ] AC2: `_pkgdown.yml` sets `template: light-switch: true` and declares `navbar: structure: right:` naming the search, lightswitch and github components, with no `navbar: right:` key left. In a site built by `pkgdown::build_site()` into an empty directory, every `.html` file under that directory holds an element with id `dropdown-lightswitch`, at least one `[data-bs-theme-value]` control and one element with id `search-input`. The procedure is a grep over every `.html` file the build wrote.
- [ ] AC3: The Vignettes menu stays hand-written under `navbar: left:`, with a `text:`-only entry per level and `text: "---------"` separator entries, and no `articles:` group carries a `navbar:` key. In the freshly built `index.html`, that dropdown holds exactly three `h6.dropdown-header` elements reading Introductory, Intermediate and Advanced in that order, a `hr.dropdown-divider` between consecutive groups, and under each heading the same pages in the same order as that level's group in the `articles:` index.
- [ ] AC4: `tools/check-pkgdown-vignettes.R` exits 0 on the grouped `_pkgdown.yml`. In a scratch copy of the repo it exits 1 on each of five planted defects, one per checking path: a page dropped from the navbar menu, a menu entry whose text is not the vignette's title, a page moved to another level group in the `articles:` index, a page moved under the wrong level heading in the navbar menu alone, and an extra vignette file on disk, carrying a well-formed `\VignetteIndexEntry{}`, that the level map does not list. The script's message names the defect it found in each case.
- [ ] AC5: `_pkgdown.yml` holds no `docsearch` key. The freshly built site has a `search.json` whose entries include the Bayesian article's title, and the search input of AC2 is present.
- [ ] AC6: `pkgdown::check_pkgdown()` reports no problem. The build writes no warning beyond the pandoc `--mathml` deprecation lines, checked by building master into a second empty directory and comparing the two warning lists. The `pkgdown.yaml` workflow is green on the pull request.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T3
- AC6 → T6

## Tasks

- [ ] T1: Build the site once from master into a temporary directory, and keep its warning list for AC6. Then render two or three candidate themes with a light navbar (for example `zephyr`, `litera`, `cosmo`, or `flatly` with a light navbar background), each into its own directory. Screenshot the home page, a reference page and an article page for each.
- [ ] T2: Show the maintainer the screenshots at a gate chip and apply the chosen preset under `template: bslib: preset`, removing `template: params: bootswatch`. Record the choice in the work log.
- [ ] T3: Add `template: light-switch: true`. Replace `navbar: right:` with a `navbar: structure:` declaration that keeps the GitHub icon and restores pkgdown's search and lightswitch components. Delete the `docsearch` keys. Rebuild and grep for the three element ids and for `search.json`.
- [ ] T4: Group the Vignettes menu in `_pkgdown.yml` with a heading per level and a divider between groups, in the `articles:` index order. Rebuild and read the rendered dropdown.
- [ ] T5: Repair `tools/check-pkgdown-vignettes.R` so it skips heading and divider entries, reads each group's pages under its heading, and fails when a page sits under the wrong heading. Plant AC4's five defects in a scratch copy of the repo, one at a time, and record each exit status and message as review evidence.
- [ ] T6: Run `pkgdown::check_pkgdown()` and a final clean build. Compare the warning list with T1's. Open the pull request and wait for the `pkgdown.yaml` workflow.

## Work log

- 2026-09-17: created by /milestone-plan. The plan gate chose a preset with a light navbar, picked from rendered candidates, over hand-tuned bslib colors, because a preset ships tested color pairings. Falsified by no candidate preset fitting the package's look.
- 2026-09-17: the plan gate chose to stay on pkgdown over trying a Quarto site through `altdoc`, because pkgdown 2.2.1 is current and no Quarto version of pkgdown exists. Falsified by a site need that pkgdown cannot meet. The alternative now has a candidate row.
- 2026-09-17: the plan gate chose to repair `tools/check-pkgdown-vignettes.R` only, over absorbing the guard-hardening candidate row or deleting the guard, because a repair keeps the guard's promise unchanged. Falsified by the repair needing the guard's promise to widen.
- 2026-09-17: criteria audit (full mode, fresh [O] reader) found 6 items on the draft. All were fixed before writing. pkgdown emits no `navbar-dark` class, so AC1 now reads the built pages. A hand-written `navbar: right:` list drops the switch and the search box, so AC2 requires `navbar: structure:` and checks for the switch's own elements. `docs/` is not committed, so AC1, AC2, AC3 and AC5 name a fresh build. AC4's four defects hit one checking path each. AC6 names how the warning baseline is produced.
- 2026-09-17: second fresh [O] audit of the written criteria found 5 items, all fixed. With the light switch on, pkgdown writes no navbar background class and no dark theme attribute, so AC1 now reads the config, a `bg-dark` search and the gate screenshots. A present `navbar: left:` key beats `structure:`, so the structure list covers `right:` only. AC3 now rules out pkgdown's own `articles: navbar:` grouping, which adds a divider before the first heading. AC4's planted vignette carries an index entry, and a fifth defect probes the repair's new wrong-heading path.

## Decisions
