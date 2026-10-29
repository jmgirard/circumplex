# M176: Replace the introduction GIFs with a slider applet

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP3, GP5
- **Resolves:** —
- **Surface tier:** user-facing, because the applet replaces three figures that readers of the introduction vignette see on CRAN and on the pkgdown site
- **Branch/PR:** m176-intro-ssm-applet

## Goal

One interactive figure, in which the reader moves sliders for elevation, amplitude and displacement, replaces the three animated GIFs in the introduction vignette.

## Scope

**In:** A JavaScript source, `vignettes/ssm-curve-applet.js`, draws an SVG plot of the SSM cosine curve and wires three sliders and a reset button to it. The precompute step writes that source inline into `vignettes/introduction-to-ssm-analysis.Rmd`, so the built HTML and the pkgdown article carry it with no extra file. A static figure inside `<noscript>` serves readers without JavaScript. A Node script, `tools/check-ssm-applet.mjs`, checks the applet's functions, and the vignette-precompute workflow runs it. The three GIFs are deleted.

**Out:**
- The vignette's other prose. Only the lead-in to the applet changes. The prose pass is the "Vignette prose follow-ons" candidate row.
- The M175 review filed three other gaps with this row: the `cpm_fit()` matrix example's Hessian warning, `knit()` truncation, and the short "Sourced by" note. They move to the candidate row "Fix three precompute and example gaps".
- Applets in other vignettes. No row holds them, because nobody asked for them.
- The pkgdown site deploy (`docs/`), which happens at release.

## Acceptance criteria

- [ ] AC1: The three GIFs are gone. `vignettes/VIG1-e.gif`, `VIG1-a.gif` and `VIG1-d.gif` do not exist at the branch tip, and `git grep -n 'VIG1-'` at the branch tip finds no line outside `cairn/`.
- [ ] AC2: The HTML vignette that `R CMD build` makes holds one applet, right after the displacement paragraph (the one that begins "The final SSM parameter"). Its SVG plot draws the curve from 0° to 360° with a fixed y range of −1 to 1. The x ticks and labels sit at the eight octant angles 45°, 90°, ..., 360°, with no 0° label. It has three labelled range sliders: e from −0.5 to 0.5 by 0.05, a from 0 to 0.5 by 0.05, and d from 5 to 360 by 5. Each slider shows its value. e and a show two decimals, and e never shows "-0.00". When a > 0, d shows whole degrees. A reset button returns the sliders to e = 0.10, a = 0.30 and d = 90. A dashed line marks e. An arrow from e up to the peak marks a. A marker at the peak angle marks d. A point on the curve marks each of the eight octant angles. Evidence is a screenshot and a `javascript_tool` query of the rendered SVG at the reset values in the built-in browser. The query reads the tick labels, the y-to-pixel mapping, `stroke-dasharray`, and the mark and point elements. AC4 covers the a = 0 and d = 360 states.
- [ ] AC3: The functions in `vignettes/ssm-curve-applet.js` follow the structural-summary curve e + a·cos(θ − d) of gurtman1998 (p. 349, Eq. 1). This holds at every slider setting (21 × 11 × 72) and every θ from 0 to 360 by 1. The curve function returns that value within 1e-12, with θ and d in degrees. If a = 0, the peak function returns `[]`. If a > 0 and d = 360, it returns `[0, 360]`. Otherwise it returns `[d]`. The octant function returns θ = 45, 90, ..., 360, never 0, each with the curve's value at that θ. The x mapping sends 0° to the plot's left edge and 360° to its right edge, increasing. The y mapping sends larger values to smaller pixel y, decreasing. The path's vertices run from θ = 0 to 360 by 1 and match the mapped curve within 0.01 px. Evidence is an exit-0 run of `node tools/check-ssm-applet.mjs`, which imports the functions from the source file itself. A green run of that check in the vignette-precompute workflow on the PR is also evidence.
- [ ] AC4: At e = 0.10, a = 0 and d = 90, the d readout reads "undefined". The arrow and the peak marker are then absent or hidden. At e = 0.10, a = 0.30 and d = 360, the peak marker sits at both 0° and 360°. Moving d from 360 to 5 changes the curve's path `d` attribute. Evidence is a `javascript_tool` read of the SVG at each setting.
- [ ] AC5: The built HTML contains a `<noscript>` element. It holds a static figure of the curve at e = 0.1, a = 0.3 and d = 90, with e, a and d marked. The figure has a descriptive `fig.alt` in place of the "plot of chunk" default. It also holds one sentence saying that the sliders need JavaScript. Evidence is a read of the built HTML.
- [ ] AC6: The applet works in the HTML vignette that `R CMD build` makes and in the article that `pkgdown::build_article("introduction-to-ssm-analysis")` makes. In each, moving each slider with the arrow keys changes its value and the curve, and `read_console_messages` shows no error. On pkgdown, the applet's colors follow `[data-bs-theme=dark]`. The html_vignette stays dark-on-white under an emulated dark color scheme. In both themes, the axis text, curve, marks and points keep a contrast of at least 3:1 against the background, measured from their computed colors.
- [ ] AC7: `R CMD build` makes one tarball from the branch tip and one from the merge base, and the branch tarball is at least 1,800,000 bytes smaller. The bound comes from the M175 measurement that the GIFs took 2.06 of the 5.13 MB tarball. `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors and 0 warnings at the branch tip, and no NOTE that the same check at the merge base lacks. The `Depends`, `Imports`, `Suggests` and `LinkingTo` fields of DESCRIPTION are unchanged.

## Coverage

- AC1 → T2
- AC2 → T1, T2, T4
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T2, T4
- AC6 → T1, T4
- AC7 → T2, T4

## Tasks

- [x] T1: Write `vignettes/ssm-curve-applet.js`. Its pure functions compute the curve, the peak angles, the octant points and the SVG path. Its DOM code builds the plot, sliders, readouts and reset button. The file runs as an inline classic `<script>`. A `module.exports` guard exposes its functions to Node. No line starts with a backtick. Colors follow `[data-bs-theme=dark]` and default to dark-on-white. Sliders carry `<label>` elements, and the SVG carries `role="img"` with an `aria-label` that states the current values. Write `tools/check-ssm-applet.mjs` for AC3. Plant twelve defects one at a time and see each one fail the check. Five change the curve: a wrong sign of e, a wrong a, a shifted d, degrees passed to `Math.cos`, and a flipped y axis. Three change the peaks: a peak at a = 0, and a lone 0° or lone 360° peak for d = 360. Four change the plot: an octant set with 0 for 360, an octant point off the curve, a flipped x mapping, and a short path.
- [x] T2: In `vignettes/introduction-to-ssm-analysis.Rmd.orig`, remove the three `include_graphics()` chunks. After the displacement paragraph, add a lead-in sentence and the applet block. An `echo = FALSE`, `results = "asis"` chunk writes the JavaScript inline from the source. Add the `<noscript>` fallback figure as an svglite chunk with a descriptive `fig.alt`. If R CMD check flags the `.js` source, add an `.Rbuildignore` entry for it. Delete the GIFs and run `Rscript tools/precompute-vignettes.R introduction-to-ssm-analysis`. Then run the staleness check, the width guard, and the prose sweep on the `.Rmd.orig`.
- [x] T3: Add a step to `.github/workflows/vignette-precompute.yaml` that runs `node tools/check-ssm-applet.mjs`.
- [ ] T4: Add one Documentation bullet for the applet under a new `# circumplex (development version)` heading in NEWS.md. Build the tarball and run `devtools::check(args = "--no-manual")` at the merge base and at the branch tip, and record both sizes and both NOTE lists. Read the `<noscript>` block of the built HTML for AC5. Open the built HTML vignette and the pkgdown article in the built-in browser. Record the AC2, AC4 and AC6 reads in light and dark themes.

## Work log

- 2026-10-29: created by /milestone-plan.
- 2026-10-29: tier check: milestone, because the request changes figures in a shipped vignette and needs a design choice (rebuild, re-encode or replace).
- 2026-10-29: idea vet: gap, how to shrink the GIFs (rebuild from a script or re-encode in place). The reading taken was a rebuild at 1000 px. The user then asked about an interactive applet and chose it over the rebuild, so the goal is now a replacement. No flaw found.
- 2026-10-29: lessons used: 2026-10-29 (M175) CRAN NOTE triggers, for the size bound. 2026-10-29 (M175) figure-format traps, for the svglite fallback figure. 2026-07-19 (M7 family) a step that doesn't run reports success, for the planted defects and the workflow run.
- 2026-10-29: question set: approach is the applet, from the user's message after the first chip. Layout is one applet with three sliders after the displacement paragraph. Octant points: mark the eight angles.
- 2026-10-29: criteria audit (full mode, Opus, fresh context): the superseded GIF draft drew 12 findings. The applet draft drew 18, all fixed. The fixes were the d slider at 5 to 360, readout formats, SVG queries as evidence, the check importing the source, and more planted defects. They also cover the a = 0 and d = 360 contradiction, `<noscript>` evidence from the HTML, console and dark-mode measures, and a merge-base check. DESCRIPTION is checked on its dependency fields only, because D-004 bumps the version. T3's allowlist clause went, and guard risks moved into T1 and T2. A re-audit of AC2 and AC3 after the answers drew 7, all fixed. Those fixes were octant tick labels with no 0°, a d readout only when a > 0, and an octant rule. They also cover a monotone y mapping with a 0.01 px path tolerance, five more plants, and AC3 stated as the property with the runs as evidence.
- 2026-10-29: plan gate chose inline JavaScript and SVG over Shiny, which needs a running server, and over htmlwidgets or plotly, which add a dependency. Falsified by evidence that html_vignette or pkgdown drops inline scripts.
- 2026-10-29: plan gate chose a Node check in CI over a testthat test through V8, which would add V8 to Suggests. Falsified by Node missing from the CI runner.
- 2026-10-29: plan gate chose a d slider from 5 to 360 over 0 to 360 or 0 to 355, because IP2 and D-003 label the pole 360. Falsified by a reader report that the missing 0 confuses.
- 2026-10-29: collision check: M175 (done) filed the source row and kept the GIFs out of its own scope. The row's GIF part is absorbed in full. Its three other gaps move to the new row "Fix three precompute and example gaps". No D-entry or open milestone overlaps.
- 2026-10-29: ROADMAP reached its 60-line cap and byte budget with the new rows, so the two parked peer-review rows (split 2026-10-17) merged back into one, "State peer-review status in more docs".
- 2026-10-29: inbox sweep: 0 open issues and 0 open PRs on jmgirard/circumplex.
- 2026-10-29: CRAN-SUBMISSION records a 2.1.0 submission today at `30d5e191`. This milestone ships in the release after it, so its NEWS bullet goes under a development heading.
- 2026-10-29: implement started on branch `m176-intro-ssm-applet`, cut from `c4e3b033`. The untracked `CRAN-SUBMISSION` file stays unstaged.
- 2026-10-29: T1 done. `vignettes/ssm-curve-applet.js` holds the pure functions and the DOM code. The amplitude arrow and displacement marker repeat at each peak angle, so a d = 360 curve shows both at 0° and 360°. `node tools/check-ssm-applet.mjs` exits 0 over 16,632 settings × 361 angles in 1.6 s. Each of the 12 plants, written to scratch copies, exits 1 at the rule it targets: the curve value (sign of e, a, d shift, degrees), yScale, peaks (a = 0, lone 0°, lone 360°), octant angles, octant point off the curve, xScale(0), and the vertex count.
- 2026-10-29: T2 done. The three GIF chunks and files are gone. The applet block follows the displacement paragraph, with a six-sentence lead-in. The `<noscript>` fallback is a 15,051-byte svglite figure with a descriptive alt. The re-rendered `.Rmd` changes only at the applet block (290 lines added, 5 removed). The width guard passes on all 14 vignettes. The prose sweep gives the same 29 findings on the `.Rmd.orig` before and after, so it adds none. The `.js` source needs no `.Rbuildignore` entry unless T4's check flags it.
- 2026-10-29: T3 done. The vignette-precompute workflow's last step runs `node tools/check-ssm-applet.mjs` with the runner's own Node, under `!cancelled()` so it reports beside a red earlier step. The header comment names it. `tools/check-ci-deps.R` stays in sync (15 Suggests), and the workflow parses as YAML. The step's first CI run comes when review opens the PR.
