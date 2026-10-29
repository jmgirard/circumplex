<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M175: Clear CRAN's 2.1.0 pretest notes

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the `cpm_fit()` help example and the vignette figures ship to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

The 2.1.0 tarball answers each NOTE of CRAN's 2026-10-29 incoming pretest.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- The `cpm_fit()` bootstrap example moves into `\donttest{}`.
- `tools/precompute-vignettes.R` renders each precomputed figure as SVG with `svglite`. The 46 committed PNG figures are replaced by the re-rendered SVGs.
- The vignette-precompute CI job installs `svglite`.
- One NEWS line, and the resubmission note in `cran-comments.md`. The note says why the tarball still exceeds 5 MB.

**Out:**
- The three hand-made GIFs in `vignettes/` (2018, no source in the repo) → candidate row "Shrink the introduction vignette GIFs".
- Figures of vignettes built live at `R CMD build`, and the README figures in `man/figures/` → not in this milestone. Neither is in `vignettes/figures/`.
- Refreshing the test environments in `cran-comments.md`, the arm64 and win-builder re-runs, and the resubmission → `/cairn-release` after the merge.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: After `devtools::document()`, the `\examples` section of `man/cpm_fit.Rd` holds the `boots = 100` `cpm_fit()` call and the line that prints its result inside one `\donttest{}` block. The matrix-input `cpm_fit(cormat = R, ...)` call is outside every `\donttest{}` and `\dontrun{}` block.
- [ ] AC2: `R CMD check --as-cran` with `_R_CHECK_DONTTEST_EXAMPLES_=false`, on the tarball that `R CMD build` makes from the branch, reports `Status: OK` under R's default incoming thresholds. On the maintainer's machine, its `circumplex-Ex.timings` gives `cpm_fit` at most 1.0 s of user plus system time and at most 1.0 s elapsed. (1.0 s is a local stand-in. At the pretest's ratios of about 6.9 on r-devel-windows and 3.25 on debian-gcc, taken from the 1.6 s local total of both examples, it is about 6.9 s and 3.3 s there. Only the resubmission shows CRAN's own times.)
- [ ] AC3: `git ls-tree --name-only HEAD:vignettes/figures` is non-empty. It lists `<name>.svg` for each `<name>.png` that `git ls-tree --name-only 30d5e191:vignettes/figures` lists, and no other file. `grep -E 'figures/[^ )"]*\.png' vignettes/*.Rmd` finds nothing.
- [ ] AC4: With the branch's package installed, `Rscript tools/precompute-vignettes.R` for every vignette, then `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R`, exit 0. Afterwards `git status --porcelain -- vignettes` has no `??`, `A` or `D` line and no line under `vignettes/figures/`. Its other modified lines lie only in declared volatile regions. A library path that holds each installed package except `svglite` is the no-`svglite` library. A control under it shows `requireNamespace()` TRUE for `knitr` and `circumplex` and FALSE for `svglite`. Under it, `Rscript tools/precompute-vignettes.R growth-ssm-analysis` stops with an error that names `svglite`, and `git status --porcelain -- vignettes` is then empty. Under it, `Rscript tools/check-vignette-staleness.R` exits 0.
- [ ] AC5: `.github/workflows/vignette-precompute.yaml` lists `any::svglite` in its `extra-packages`. `tools/check-ci-deps.R` lists `svglite` in the `extra` of that workflow, and `Rscript tools/check-ci-deps.R` exits 0. The vignette-precompute job concludes `success` on the head commit of the milestone's PR. A red, cancelled or absent run fails this criterion.
- [ ] AC6: The tarball of AC2 is at most 5,250,000 bytes. The Notes of `cran-comments.md` answer the three NOTEs the pretest email lists: the tarball size, the `cpm_fit` elapsed time on r-devel-windows, and the `cpm_fit` CPU time on debian-gcc. They give the three introduction GIFs as the reason for the size over 5 MB. The GIFs ship twice, as files (bytes from `tar tzvf` of the AC2 tarball) and base64-embedded in `introduction-to-ssm-analysis.html`. The results section says that CRAN's incoming check still gives the size NOTE. Any example time the file states is read from the check log of AC2 and is labelled as excluding `\donttest` code.
- [ ] AC7: `devtools::document()` leaves no diff, and `devtools::test()` and `devtools::check()` report 0 errors and 0 warnings. The 2.1.0 Documentation section of NEWS.md states that the precomputed vignette figures are SVG. It gives no size figure and makes no claim about how the figures look.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T7
- AC2 → T1, T7
- AC3 → T2, T3
- AC4 → T2, T3, T5
- AC5 → T4
- AC6 → T6, T7
- AC7 → T6, T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive change is amend-via-gate. -->

- [ ] T1: In the `cpm_fit()` roxygen examples (R/cpm_fit.R, `@examples` near line 1817), wrap the `boots = 100` call and `fit` in `\donttest{}`. Keep the comment true, then run `devtools::document()`.
- [ ] T2: In `knit_one()` of `tools/precompute-vignettes.R`, set `dev = "svglite"` and `fig.ext = "svg"`. knitr's own error names `svglite` when it is missing, so the script adds no check of its own. Nothing at the top level of the file loads `svglite`, because the staleness, width and parity scripts `sys.source()` the file in base R. The 144-dpi setting does not apply to SVG. If the rendered `.Rmd` files stay the same without it, remove it and its comment.
- [ ] T3: Install the branch's package, delete the PNGs in `vignettes/figures/` with `git rm`, and re-render every vignette. Run the staleness and width checks. Read the `.Rmd` diffs. A change outside the figure links and the declared volatile regions is a stop.
- [ ] T4: Add `any::svglite` to the `extra-packages` of the vignette-precompute job, and `svglite` to the `extra` of that workflow in `tools/check-ci-deps.R`. Run the guard.
- [ ] T5: Build a library of symbolic links to each installed package except `svglite`. Run the control, the failing re-render and the staleness check under it, with `git status` read after each.
- [ ] T6: Add one sentence on the SVG figures to the 2.1.0 Documentation section of NEWS.md. Rewrite the Notes of `cran-comments.md` as a resubmission that answers each NOTE of the pretest and gives the size reason (2.0.1 shipped at 9.84 MB).
- [ ] T7: Run `devtools::document()` (no diff), `devtools::test()` and `devtools::check()`. Run `R CMD build`, then `R CMD check --as-cran` with `_R_CHECK_DONTTEST_EXAMPLES_=false`, and read the size and the `cpm_fit` timing. For the merge question, rasterize each SVG at the old pixel width and rank the figures by RMSE against their `30d5e191` PNG. Stack the three largest, before and after, in one image in the scratchpad. Also name the four figures with a Δ label in Symbol font, for a look in a browser.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-29: created by /milestone-plan.
- 2026-10-29: tier check: milestone — a change to the precompute tooling, its CI job and the vignette figures, with an approach to choose, beside a help-example edit.
- 2026-10-29: idea vet: gap — "shrink the figures" left the method open; the reading taken is SVG figures, which the user chose at the question set. flaw — none. The palette-PNG trial darkened light greys at the default colour space (audit finding, confirmed by eye), which sRGB quantizing fixed before the user chose SVG.
- 2026-10-29: lessons used: 2026-07-19 (M7 family) on `devtools::check()` setting `NOT_CRAN`, which shaped AC2's tarball check.
- 2026-10-29: collision check: no D-entry or archive covers figure format or example time. Hotfix #152 set 144 dpi for PNG sharpness, and SVG keeps that aim. Inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-29: release-shaped tripwire not hit. M175 ships no version; the resubmission stays with `/cairn-release`, whose window the user opened on 2026-10-28.
- 2026-10-29: question set: example time fix — wrap the bootstrap example in `\donttest{}` (user's choice).
- 2026-10-29: question set: figure method — SVG with `svglite` (user's choice), over palette PNGs with `magick`, lower dpi, and shrinking in CI as well.
- 2026-10-29: question set: GIFs — left out, candidate row filed (user's choice). The re-audit then measured them at about 2.05 MB of the 5.13 MB tarball.
- 2026-10-29: question set: the user asked to see the three most-changed figures, before and after, at the merge question.
- 2026-10-29: plan gate chose SVG over 256-colour sRGB palette PNGs because the user picked it, and the trial tarball was 5,128,628 bytes against 7,008,563; falsified by an SVG that renders wrongly in a common browser or grows past its PNG.
- 2026-10-29: criteria audit (full mode, palette draft): 16 findings, all taken. AC2 read the donttest pass and bounded only elapsed time; AC3 promised format but not look; AC4's probes stood one case for a family; AC5 read as a precondition; AC6's reason and NOTE list were incomplete; AC7 named the verify slot.
- 2026-10-29: criteria audit (full mode, SVG draft): 5 findings, all taken. The AC2 bound moved from 0.7 s to 1.0 s, as runs measured 0.55 to 0.68 s; T2 drops its own `svglite` check, because knitr fails loudly; AC4 requires identical figure bytes; AC6 names the GIFs and labels example times. A look at the figures is not a criterion; it is T7's image and the merge question's look.
- 2026-10-29: plan decided svglite enters only the precompute CI job's allowlist, not DESCRIPTION, because `R CMD build` renders no precomputed figure.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
