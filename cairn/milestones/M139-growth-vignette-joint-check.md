# M139: The growth vignette shows the correlated person block and a zero check on the cross block

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette changes
- **Branch/PR:** `m139-growth-vignette-joint-check`

## Goal

The growth vignette shows the reader that the fit is joint and gives one check the reader can carry to their own fits. The recipe script gets the same check.

## Scope

**In:** RR23 recommendations 2 and 3 (D-060). The growth vignette prints `glmmTMB::VarCorr(fit)` after the first fit. Section 5 shows a check that the `x`/`y` cross block of the fixed-effect covariance is not identically zero. Prose beside it says what the check detects and what it does not. Section 4's warning gains one forward-pointing sentence. The recipe guard in `devel/m27-growth-recipe.R` becomes the same exact-zero test over the full cross block. The pre-computed vignette is re-rendered and committed. NEWS gains one entry.

**Out:** the second fit in Section 6 (the plan gate chose the first fit only). The shared-intercept `(1 | person)` coverage-oracle cell (RR23 rec 4) stays in the ROADMAP candidate row "RR23 follow-ons to the growth recipe" (iii). Any exported function (D-060 forecloses a fitter; the draws-to-trajectory helper stays its own candidate row).

## Acceptance criteria

- [x] AC1: `vignettes/growth-ssm-analysis.Rmd.orig` has an echoed chunk, evaluated under `has_glmmTMB`, directly after the `fit` chunk, whose only call is `glmmTMB::VarCorr(fit)`. The shipped `vignettes/growth-ssm-analysis.Rmd` carries that chunk's printed output with the `person` block's three standard deviations and three correlations. The output passes `tools/check-vignette-width.R`, through an exemption entry for that chunk if its print runs past the guard.
- [x] AC2: `vignettes/growth-ssm-analysis.Rmd.orig` has an echoed chunk, evaluated under `has_glmmTMB`, directly after the `fixef` chunk, that takes the cross block `V[c("dvx", "dvx:wave"), c("dvy", "dvy:wave")]` and prints `any(... != 0)`. The shipped `.Rmd` shows `#> [1] TRUE` as that chunk's output.
- [x] AC3: The prose beside the AC2 chunk states three things. The zeros come from assembling separate per-coordinate covariance matrices block by block. The check detects only that structure. A nonzero block does not show the model is right. Section 4's warning paragraph ends with one sentence that points to the check.
- [x] AC4: In `devel/m27-growth-recipe.R` the guard replaces the current `stopifnot(abs(xy_cov) > 0)`. It stops exactly when the full `x`/`y` cross block of `V` (intercept and slope terms) is identically zero, and passes whenever at least one of its four entries is nonzero; an entry of floating-point size counts as nonzero. A comment above it says it detects the independent-fits structure only. `Rscript devel/m27-growth-recipe.R` exits 0.
- [x] AC5: The re-rendered `vignettes/growth-ssm-analysis.Rmd` and its figures are committed, and on the committed tree `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R` exit 0.
- [x] AC6: `NEWS.md` carries one entry under the development version naming the two vignette additions.
- [x] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that the base commit's check does not report.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T2
- AC4 → T4
- AC5 → T3, T5
- AC6 → T5
- AC7 → T5

## Tasks

- [x] T1: Add the `varcorr` chunk after the `fit` chunk at `vignettes/growth-ssm-analysis.Rmd.orig:141` with one sentence of prose that names the three correlations as the joint structure. Render locally and read the width; add the `EXEMPT` entry in `tools/check-vignette-width.R` only if the print runs past 80 columns.
- [x] T2: Add the `cross-block` chunk after the `fixef` chunk (`.orig:184`) and the AC3 prose. Add the forward-pointing sentence to the end of Section 4's warning paragraph (`.orig:170`). Keep every name the chunks use defined in an echoed chunk (LESSONS 2026-07-21).
- [x] T3: `Rscript tools/precompute-vignettes.R`, then `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R`. Commit the regenerated `.Rmd` and figures.
- [x] T4: Replace the guard at `devel/m27-growth-recipe.R:86` with the full cross-block exact-zero test and its comment. Run the script.
- [x] T5: NEWS entry. `devtools::test()` and `devtools::check(args = "--no-manual")`, with the note list compared against the base commit.

## Work log

- 2026-09-17: created by /milestone-plan from D-060 (RR23 recommendations 2 and 3); absorbs items (i) and (ii) of the ROADMAP candidate row "RR23 follow-ons to the growth recipe".
- 2026-09-17: criteria audit (full): fixed AC1 (width-guard clause), AC2 (rendered line `#> [1] TRUE`), AC3 (zeros come from assembling separate matrices; word cap dropped), AC5 (guards run on the committed tree), AC7 (no new note rather than 0 notes). AC4 and AC6 returned nothing.
- 2026-09-17: plan gate chose the check in Section 5 after `V` exists over Section 4 beside the warning because the reader already holds `V` there; falsified by a reader report that the warning and the check sit too far apart to connect.
- 2026-09-17: plan gate chose first fit only over both fits because the second fit teaches certification, not joint structure; falsified by a reader who applies the check to the second fit and finds it fails.
- 2026-09-17: plan gate chose a width-guard exemption over a hand-built narrower print because the plain `VarCorr` call is what the reader reuses; falsified by an exempt print that later overflows the rendered page.
- 2026-09-17: T1 done. The `varcorr` chunk prints at 38 columns (measured by a scratch run of the fit), so no `EXEMPT` entry is added.
- 2026-09-17: T2 done. The `cross-block` chunk is one `all()` call over the indexed cross block; `V` comes from the echoed `fixef` chunk. Section 4's warning paragraph ends with one sentence pointing at Section 5.
- 2026-09-17: T3 done. Re-rendered `growth-ssm-analysis` only; the figures did not change (same seed, same draws). The `VarCorr` print carries a `Residual NA` row, so its prose gained one sentence saying why. Width guard: all fit, 0 exempted.
- 2026-09-17: T4 done. The recipe guard is `stopifnot(all(V_xy != 0))` (amended below to `any`) over the 2x2 intercept/slope cross block, printed above it; `Rscript devel/m27-growth-recipe.R` exits 0.
- 2026-09-17: claim audit: 13 claims read, 4 corrected — vignettes/growth-ssm-analysis.Rmd.orig, vignettes/growth-ssm-analysis.Rmd, NEWS.md, devel/m27-growth-recipe.R. The four were one defect: three of the four cross-block entries in the vignette's fit are floating-point residue (1e-21, 1e-38), so `all(... != 0)` passed by accident while RR23 asked for a test that the block is not identically zero.
- 2026-09-17: substantive amendment (mini gate, user chose the recommended option): AC2 and AC4 change from `all(... != 0)` to a check that the cross block is not identically zero (`any(... != 0)`); the Scope clause now reads "is not identically zero". The vignette, recipe and NEWS prose were corrected in the same turn and the vignette re-rendered.
- 2026-09-17: re-audit: AC2 (full) — nothing.
- 2026-09-17: re-audit: AC4 (full) — finding: "stops when every entry is exactly zero" is one-directional and also satisfied by the defective `all()` guard; reworded to state both directions and re-entered once.
- 2026-09-17: re-audit: AC4 (full) — nothing; the two-way wording is now written to the file. Second `re-audit: AC4` line: further churn on AC4 goes to the user.
- 2026-09-17: T5 done. NEWS entry added. `devtools::test()`: FAIL 0, WARN 11 (lavaan, pre-existing), SKIP 1, PASS 11319. `devtools::check(args = "--no-manual")` on the branch: Status OK, 0 notes; on the base commit (a scratch worktree): 1 note, `.git` as a hidden file, an artifact of the linked worktree and not of the package. Status set to review.

## Decisions

## Review

- 2026-09-17 AC1: `.orig` chunk `varcorr` (line 158) is the next chunk after `fit` (line 141), `eval = has_glmmTMB`, echoed, sole call `glmmTMB::VarCorr(fit)`. Shipped `.Rmd` lines 148-155 print the `person` block with three Std.Dev. values and three correlations. `tools/check-vignette-width.R`: growth-ssm-analysis 49 output lines, 0 exempted, all fit. Verified.
- 2026-09-17 AC2: `.orig` chunk `cross-block` (line 205) is the next chunk after `fixef` (line 196), `eval = has_glmmTMB`, echoed, indexes `V[c("dvx", "dvx:wave"), c("dvy", "dvy:wave")]` inside `any(... != 0)`. Shipped `.Rmd` line 205 reads `#> [1] TRUE`. Verified.
- 2026-09-17 AC3: the prose after the chunk states that separate fits assembled block by block hold exact zeros in every cross block, that the check detects only that assembled structure, and that a nonzero block does not show the model is right. Section 4's warning paragraph ends with "Section 5 shows one check that detects a covariance matrix assembled from separate fits." Verified.
- 2026-09-17 AC4: `devel/m27-growth-recipe.R:91-93` reads `V_xy <- V[c("dvx", "dvx:wave"), c("dvy", "dvy:wave")]; stopifnot(any(V_xy != 0))`, the old `stopifnot(abs(xy_cov) > 0)` gone; the comment above says it detects the independent-fits structure only. Predicate probed in a scratch R call: an all-zero 2x2 stops, a block whose single nonzero entry is 4e-21 passes. `Rscript devel/m27-growth-recipe.R` exits 0; its printed block has entries 8.0e-05, 4.2e-21, 1.6e-20, 8.1e-37. Verified.
- 2026-09-17 AC5: working tree clean on the branch; `Rscript tools/check-vignette-staleness.R` exit 0 (all 11 pre-computed vignettes up to date); `Rscript tools/check-vignette-width.R` exit 0. Figures unchanged from the base commit (the diff touches no figure file; the T3 log records the same seed and draws). Verified.
- 2026-09-17 AC6: `NEWS.md` development heading, "Minor improvements and fixes", one bullet naming the VarCorr print and the cross-block check; no milestone number in it. Verified.
- 2026-09-17 AC7: `devtools::test()`: FAIL 0, WARN 11 (lavaan, pre-existing), SKIP 1, PASS 11319, exit 0. `devtools::check(args = "--no-manual")`: Status OK, 0 errors, 0 warnings, 0 notes; the base commit's check (T5 log) reported one worktree-artifact note and nothing else, so no new note. Verified.
- 2026-09-17 consistency gate: `cairn_validate.py` all checks passed (exit 0, coverage complete, release window quiet). No DESIGN.md principle changed, `cairn_impact` skipped. Toolchain: `devtools::document()` did not execute — installed roxygen2 8.0.0 is older than the 8.1.0 pinned in `DESCRIPTION` `Config/roxygen2/version`, an environment fact; the diff touches no `R/` or roxygen source, so no `man/`/`NAMESPACE` drift is possible, and the tree is clean. `pkgdown::check_pkgdown()`: no problems. NEWS entry present, no milestone number. No new top-level file. Master watches: newest push runs with a verdict on master (M138 merge f294589c) are `success` for both `R-CMD-check.yaml` and `test-coverage.yaml`; the later docs-only plan commit triggered no run (paths-ignore). `check-master-red-alert.R`, `master-red-alert-dryrun.R`, `check-branch-protection.R` all clean.
- 2026-09-17 independent review, three lenses. [S] blame-history: zero findings (old guard and `cat()` line from M27 `ba76f61c`, intent preserved and widened; LESSONS 2026-07-21 respected; no decision contradicted). [S] prior-review record: zero findings (probe `pulls/comments?per_page=1` returned `[]`, walk skipped; RR23 recs 2 and 3 delivered as amended). [O] diff-bug, ranked, dispositions at the gate:
  - O1 `.orig:206-208` — "Most of its entries here are tiny, and only the intercept covariance is appreciable" describes entries of 1e-21 and 1e-38 as small estimates when they are floating-point residue, numerically zero; GP5 wants the check named as structural, not a magnitude test. Disposition: fix now (5f48cdcd): the three residue entries named as floating-point residue near 1e-20, numerically zero; the check named a structural test, not a test of size.
  - O2 `.orig:209-210` — "The check detects only that assembled structure" understates the other failure mode: any perturbation or an engine writing 1e-300 instead of exact 0 yields `TRUE`, so the check has no power outside the exact block-diagonal case, and the prose never says so. Disposition: fix now (5f48cdcd): prose now says the check has no power outside the exact-zero case, any nonzero value however small passes.
  - O3 `.orig:151` — "The person block of the variance components is where the fit is joint" locates joint-ness in the random-effects block, while Section 4 and 5 turn on the fixed-effect covariance; the person correlations matter because they propagate into it. Disposition: fix now (5f48cdcd): "is where the fit is joint" became "is one place where the fit shows that it is joint".
  - O4 `NEWS.md:14-19` — "all exact zeros only when the matrix was assembled from separate fits" states an unhedged "only when"; a degenerate joint fit could also produce all zeros. Disposition: fix now (5f48cdcd): the "only when" became "holds exact zeros throughout when the matrix was assembled from separate fits, and the check detects that structure only".
  - O5 `.orig:151`, `:198-200` vs AC1/AC2 — each new chunk is separated from the named chunk by one prose paragraph, so "directly after" holds in chunk order, not literally. Disposition: reject: the criteria name chunks and no chunk intervenes; a prose paragraph between chunks does not break "directly after" in chunk order.
  - O6 `.orig:202` — the check hardcodes `dvx`, `dvx:wave`, `dvy`, `dvy:wave`, and "Carry this check to your own fits" does not say those names must be rewritten for the reader's model. Disposition: fix now (5f48cdcd): one sentence tells the reader to replace the four names with their own model's intercept and slope terms.
  - O7 `devel/m27-growth-recipe.R:86-91` — the guard is correct in both directions, but the bare `print(V_xy)` no longer names what the printed 2x2 is, unlike the old labeled `cat()`. Disposition: fix now (5f48cdcd): a cat() line names the printed 2x2 as the x/y cross block of V.
  - O8 `.Rmd:155` — the `Residual NA` explanation verified correct for `dispformula = ~ 0 + dv`; no defect. Disposition: noted, no action.
- 2026-09-17: gate triage (user): O1-O4, O6, O7 fix now; O5 rejected; O8 noted. Fix-now committed as 5f48cdcd, vignette re-rendered, staleness and width guards exit 0, recipe exit 0; tests and check re-run recorded below.
