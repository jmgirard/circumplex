<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M175: Clear CRAN's 2.1.0 pretest notes

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the `cpm_fit()` help example and the vignette figures ship to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m175-cran-pretest-notes   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

The 2.1.0 tarball answers each NOTE of CRAN's 2026-10-29 incoming pretest.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- The `cpm_fit()` bootstrap example moves into `\donttest{}`.
- `tools/precompute-vignettes.R` renders each precomputed figure as SVG with `svglite`. The 46 committed PNG figures are replaced by the re-rendered SVGs.
- The vignette-precompute CI job installs `svglite`.
- One NEWS line, and the resubmission note in `cran-comments.md`. The note says the tarball is now under the size threshold of R's incoming check (amended M175 implement; the plan's 5 MB premise was false).

**Out:**
- The three hand-made GIFs in `vignettes/` (2018, no source in the repo) → candidate row "Shrink the introduction vignette GIFs".
- Figures of vignettes built live at `R CMD build`, and the README figures in `man/figures/` → not in this milestone. Neither is in `vignettes/figures/`.
- Refreshing the test environments in `cran-comments.md`, the arm64 and win-builder re-runs, and the resubmission → `/cairn-release` after the merge.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: After `devtools::document()`, the `\examples` section of `man/cpm_fit.Rd` holds the `boots = 100` `cpm_fit()` call and the line that prints its result inside one `\donttest{}` block. The matrix-input `cpm_fit(cormat = R, ...)` call is outside every `\donttest{}` and `\dontrun{}` block.
- [x] AC2: `R CMD check --as-cran` with `_R_CHECK_DONTTEST_EXAMPLES_=false`, on the tarball that `R CMD build` makes from the branch, reports `Status: OK` under R's default incoming thresholds. On the maintainer's machine, its `circumplex-Ex.timings` gives `cpm_fit` at most 1.0 s of user plus system time and at most 1.0 s elapsed. (1.0 s is a local stand-in. At the pretest's ratios of about 6.9 on r-devel-windows and 3.25 on debian-gcc, taken from the 1.6 s local total of both examples, it is about 6.9 s and 3.3 s there. Only the resubmission shows CRAN's own times.)
- [x] AC3: `git ls-tree --name-only HEAD:vignettes/figures` is non-empty. It lists `<name>.svg` for each `<name>.png` that `git ls-tree --name-only 30d5e191:vignettes/figures` lists, and no other file. `grep -E 'figures/[^ )"]*\.png' vignettes/*.Rmd` finds nothing.
- [x] AC4: With the branch's package installed, `Rscript tools/precompute-vignettes.R` for every vignette, then `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R`, exit 0. Afterwards `git status --porcelain -- vignettes` has no `??`, `A` or `D` line and no line under `vignettes/figures/`. Its other modified lines lie only in declared volatile regions. A library path that holds each installed package except `svglite` is the no-`svglite` library. A control under it shows `requireNamespace()` TRUE for `knitr` and `circumplex` and FALSE for `svglite`. Under it, `Rscript tools/precompute-vignettes.R growth-ssm-analysis` stops with an error that names `svglite`, and `git status --porcelain -- vignettes` is then empty. Under it, `Rscript tools/check-vignette-staleness.R` exits 0.
- [ ] AC5: `.github/workflows/vignette-precompute.yaml` lists `any::svglite` in its `extra-packages`. `tools/check-ci-deps.R` lists `svglite` in the `extra` of that workflow, and `Rscript tools/check-ci-deps.R` exits 0. The vignette-precompute job concludes `success` on the head commit of the milestone's PR. A red, cancelled or absent run fails this criterion.
- [x] AC6: The AC2 tarball is at most 5,250,000 bytes. The Notes of `cran-comments.md` answer the three NOTE entries of the 2026-10-29 pretest email: the incoming-feasibility NOTE that reported `Size of tarball: 10970779 bytes`, `cpm_fit` at 11.09 s elapsed on r-devel-windows-x86_64, and `cpm_fit` at 5.2 s CPU on r-devel-linux-x86_64-debian-gcc. The results section states that the tarball is under the 10,000,000 bytes above which R 4.6.1's incoming check reports its size by default. The file makes no prediction about which NOTEs CRAN's own check of this tarball gives. Each example time the file gives for this tarball is labelled as excluding `\donttest` code. It is an upper bound on user plus system time and on elapsed time that the AC2 run meets. Per example it is checked against `circumplex-Ex.timings`. In total it is at least 0.5 s above the whole-second figure in `00check.log`.
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

- [x] T1: In the `cpm_fit()` roxygen examples (R/cpm_fit.R, `@examples` near line 1817), wrap the `boots = 100` call and `fit` in `\donttest{}`. Keep the comment true, then run `devtools::document()`.
- [x] T2: In `knit_one()` of `tools/precompute-vignettes.R`, set `dev = "svglite"` and `fig.ext = "svg"`. `knit_one()` checks `requireNamespace("svglite")` before it knits and stops with an error that names `svglite`, because knitr's own error comes after it has truncated the output `.Rmd`. Nothing at the top level of the file loads `svglite`, because the staleness, width and parity scripts `sys.source()` the file in base R. The 144-dpi setting does not apply to SVG. If the rendered `.Rmd` files stay the same without it, remove it and its comment.
- [x] T3: Install the branch's package, delete the PNGs in `vignettes/figures/` with `git rm`, and re-render every vignette. Run the staleness and width checks. Read the `.Rmd` diffs. A change outside the figure links and the declared volatile regions is a stop.
- [x] T4: Add `any::svglite` to the `extra-packages` of the vignette-precompute job, and `svglite` to the `extra` of that workflow in `tools/check-ci-deps.R`. Run the guard.
- [x] T5: Build a library of symbolic links to each installed package except `svglite`. Run the control, the failing re-render and the staleness check under it, with `git status` read after each.
- [x] T6: Add one sentence on the SVG figures to the 2.1.0 Documentation section of NEWS.md. Rewrite the Notes of `cran-comments.md` as a resubmission that answers each NOTE of the pretest and gives the size reason (2.0.1 shipped at 9.84 MB).
- [x] T7: Run `devtools::document()` (no diff), `devtools::test()` and `devtools::check()`. Run `R CMD build`, then `R CMD check --as-cran` with `_R_CHECK_DONTTEST_EXAMPLES_=false`, and read the size and the `cpm_fit` timing. For the merge question, rasterize each SVG at the old pixel width and rank the figures by RMSE against their `30d5e191` PNG. Stack the three largest, before and after, in one image in the scratchpad. Also name the four figures with a Δ label in Symbol font, for a look in a browser.

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
- 2026-10-29: implement started on `m175-cran-pretest-notes`. The untracked `CRAN-SUBMISSION` file that `devtools::submit_cran()` wrote stays unstaged.
- 2026-10-29: checkpoint (T1-T4 edited, none ticked): bootstrap example in `\donttest{}` and Rd regenerated, test suite still running; precompute renders SVG, 46 PNGs replaced by 46 SVGs, `.Rmd` diffs only figure links plus one volatile-region line; `svglite` added to the precompute job and `check-ci-deps.R`, guard exits 0.
- 2026-10-29: T5 probe, no-`svglite` library of 473 symlinked packages (control TRUE TRUE FALSE): without a check of its own the script exited 1 with knitr's "there is no package called 'svglite'" but left `growth-ssm-analysis.Rmd` truncated (35 insertions, 683 deletions), so the audit's reading that knitr writes no file was false for `knit_one()`. Minor amendment to T2: `knit_one()` now checks `requireNamespace("svglite")` first. Re-run: exit 1 with the script's error naming svglite, `git status -- vignettes` empty, staleness exits 0 under that library.
- 2026-10-29: T2/T3 evidence: full re-render exits 0, staleness and width checks exit 0, `git status -- vignettes` shows only `ci-accuracy.Rmd` modified on its volatile elapsed line (restored); a one-vignette re-render of `growth-ssm-analysis` left no change, so the SVGs reproduce byte for byte on this machine.
- 2026-10-29: checkpoint: NEWS 2.1.0 Documentation gains the SVG line; `cran-comments.md` waits for T7's tarball figures; T7 build and check running.
- 2026-10-29: T7 tarball (`R CMD build` of the working tree at 146adca3): 5,134,444 bytes; `R CMD check --as-cran` with `_R_CHECK_DONTTEST_EXAMPLES_=false` gives `Status: OK`, `cpm_fit` 0.814 user + 0.008 system, 0.823 elapsed; examples 12 s, tests 47 s, install 11 s. GIFs 350,510 + 347,288 + 325,217 bytes in `tar tzvf`, 3 `data:image/gif` URIs in the introduction HTML.
- 2026-10-29: T6 done: `cran-comments.md` rewritten as a resubmission from the T7 figures.
- 2026-10-29: T1 done: `devtools::test()` FAIL 0, WARN 11, SKIP 1, PASS 15476. Look image for the merge question at `scratchpad/M175-look-worst3.png` (largest RMSE: advanced-visualization latent-vectors 0.0945, latent-measure 0.0778, ellipse-figure 0.0771, mostly a few-pixel offset); Δ figures: intermediate-ssm-analysis group_contrast, measure_contrast, model plots and sem-latent-contrasts contrast-plot.
- 2026-10-29: the claim auditor found that R 4.6.1's incoming check reports tarball size only above `_R_CHECK_CRAN_INCOMING_TARBALL_THRESHOLD_` (default 1e7; read from `tools:::.check_package_CRAN_incoming`), so AC6's mandated "still gives the size NOTE" is false. AC6 amendment drafted, re-audit running. Implementation choices: `.Rbuildignore` `^Rplots\.pdf$` widened to `(^|/)Rplots\.pdf$`, since a test run's `tests/testthat/Rplots.pdf` reached the T7 tarball; the stray file deleted. `vignettes/view_ipipipc.png` (45 KB, referenced nowhere) left alone.
- 2026-10-29: re-audit: AC6 (full) — 7 findings: "the reason the tarball is over 5 MB" overclaims at 134,444 bytes over 5,000,000 with larger non-GIF parts; "claims no NOTE from CRAN's own run" reads two ways; point example times bind a recording act and drift between runs; a stated byte count is not bound across rebuilds; the pretest NOTEs should be named by their figures. Fixed wording sent to its one further reader. `cran-comments.md` rewritten to the fixed wording (upper bounds, bytes, threshold statement); the GIF candidate row corrected in place.
- 2026-10-29: re-audit: AC6 (full) — 5 findings on the fixed wording: "largest part" depends on how the tarball is divided; "in bytes" still allows an unqualified "over 5 MB"; the no-prediction clause bound only the results section; whole-second `00check.log` figures cannot confirm an upper bound; the `\donttest` label also caught the pretest's own times. This is AC6's second re-audit line, so the next wording goes to the user (stop). Proposed wording at `scratchpad/AC6-v3.md`.
- 2026-10-29: substantive amendment: AC6 and Scope In. At the stop the user chose to drop the GIF size explanation, which only mattered under the plan's false 5 MB premise. AC6 keeps the tarball bound, the three named NOTE entries, the 1e7 threshold statement, no prediction of CRAN's NOTEs, and upper-bound example times. No further reader runs on AC6 (its second re-audit line stands).
- 2026-10-29: T7 verify: `devtools::document()` no diff and 0 unresolved links; `devtools::check()` 0 errors, 0 warnings, 0 notes (11 min 32 s).
- 2026-10-29: T7 rebuild after the `.Rbuildignore` change: tarball 5,129,956 bytes, no `Rplots.pdf`; `--as-cran` without donttest `Status: OK`, incoming feasibility OK, `cpm_fit` 0.814 + 0.006 user+system, 0.821 elapsed; tests 56 s/59 s, so the cran-comments test bound widened from 60 s to 90 s. `cran-comments.md` now matches the amended AC6: no GIF paragraph, threshold "by default".
- 2026-10-29: claim audit: 40 claims read, 3 corrected — cran-comments.md (size NOTE prediction, NOTE count, vignette re-build sentence); one unsettled claim in tools/precompute-vignettes.R reworded to the 1e7 threshold. The later AC6 rewrite of cran-comments had no fresh reader at implement; review reads it against AC6.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

Pass 1, 2026-10-29, at `99bcc8aa` (master `84b06af6` had not moved; no PR; Copilot round off).

- AC1: `man/cpm_fit.Rd` `\examples` read: the `boots = 100` call and `fit` sit inside one `\donttest{}` block; `cpm_fit(cormat = R, ...)` follows it outside any `\donttest{}`/`\dontrun{}`.
- AC2: `R CMD build` of the tree at `99bcc8aa`, then `_R_CHECK_DONTTEST_EXAMPLES_=false R CMD check --as-cran`, no other variable set: `Status: OK`, incoming feasibility OK; `circumplex-Ex.timings` `cpm_fit` 0.815 user + 0.004 system = 0.819 s, 0.820 s elapsed.
- AC3: `git ls-tree --name-only HEAD:vignettes/figures` lists 46 files and equals the `30d5e191` list with `.png` → `.svg` (that list holds 0 non-PNG files); the grep over `vignettes/*.Rmd` finds 0 lines.
- AC4: branch package installed (`devtools::install()`, its Rd carries `\donttest`); full precompute exit 0, staleness and width checks exit 0; `git status -- vignettes` shows only ` M vignettes/ci-accuracy.Rmd`, its one changed line the elapsed figure at line 266, inside the volatile region at lines 258-324 (restored after). No-`svglite` library: control `TRUE TRUE FALSE`; `precompute-vignettes.R growth-ssm-analysis` stops with "svglite is required to render the pre-computed vignette figures"; `git status -- vignettes` empty; staleness exits 0.
- AC5 (partial, box open): `vignette-precompute.yaml:87` lists `any::svglite`; `check-ci-deps.R:63` `extra = c("svglite")`; guard exits 0. The job's conclusion on the PR head is read at step 8, before the merge.
- AC6: AC2 tarball 5,129,947 bytes (≤ 5,250,000). `cran-comments.md` Notes name the three entries by their figures (`Size of tarball: 10970779 bytes`, 11.09 s r-devel-windows-x86_64, 5.2 s r-devel-linux-x86_64-debian-gcc); the results section states the 10000000-byte threshold "by default"; the file predicts no CRAN NOTE (win-builder line is a past report). Times for this tarball, each labelled as excluding `\donttest` code: `cpm_fit` "under 1 s" (0.819/0.820); examples "under 15 seconds" (12 s in `00check.log`, so ≥ 0.5 s above); tests "under 90" (49 s); install "under 15" (11 s); re-building "under 15" (11 s).
- gate: `cairn_validate` all checks passed; no DESIGN principle changed (impact skipped); README.Rmd untouched; `pkgdown::check_pkgdown()` no problems; no new top-level file; master watches: newest push runs with a verdict, R-CMD-check `30d5e191` success and test-coverage `30d5e191` success (`84b06af6` is cairn-only, no run); `check-master-red-alert.R`, `master-red-alert-dryrun.R`, `check-branch-protection.R` exit 0. Pending: AC7 runs, `document()` no-diff.
- spawned: diff-bug, blame-history, prior-review, ripple
