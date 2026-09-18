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

- [ ] AC1: `vignettes/growth-ssm-analysis.Rmd.orig` has an echoed chunk, evaluated under `has_glmmTMB`, directly after the `fit` chunk, whose only call is `glmmTMB::VarCorr(fit)`. The shipped `vignettes/growth-ssm-analysis.Rmd` carries that chunk's printed output with the `person` block's three standard deviations and three correlations. The output passes `tools/check-vignette-width.R`, through an exemption entry for that chunk if its print runs past the guard.
- [ ] AC2: `vignettes/growth-ssm-analysis.Rmd.orig` has an echoed chunk, evaluated under `has_glmmTMB`, directly after the `fixef` chunk, that takes the cross block `V[c("dvx", "dvx:wave"), c("dvy", "dvy:wave")]` and prints `any(... != 0)`. The shipped `.Rmd` shows `#> [1] TRUE` as that chunk's output.
- [ ] AC3: The prose beside the AC2 chunk states three things. The zeros come from assembling separate per-coordinate covariance matrices block by block. The check detects only that structure. A nonzero block does not show the model is right. Section 4's warning paragraph ends with one sentence that points to the check.
- [ ] AC4: In `devel/m27-growth-recipe.R` the guard replaces the current `stopifnot(abs(xy_cov) > 0)`. It stops exactly when the full `x`/`y` cross block of `V` (intercept and slope terms) is identically zero, and passes whenever at least one of its four entries is nonzero; an entry of floating-point size counts as nonzero. A comment above it says it detects the independent-fits structure only. `Rscript devel/m27-growth-recipe.R` exits 0.
- [ ] AC5: The re-rendered `vignettes/growth-ssm-analysis.Rmd` and its figures are committed, and on the committed tree `Rscript tools/check-vignette-staleness.R` and `Rscript tools/check-vignette-width.R` exit 0.
- [ ] AC6: `NEWS.md` carries one entry under the development version naming the two vignette additions.
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that the base commit's check does not report.

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
