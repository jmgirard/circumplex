# M177: Fix three precompute and example gaps

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the `cpm_fit()` help-page example ships to users; the precompute script is internal.
- **Branch/PR:** m177-precompute-example-gaps

## Goal

The `cpm_fit()` run example explains the warning it prints, and the vignette precompute script never leaves a shipped `.Rmd` half-written.

## Scope

**In:** a comment on the matrix-input `cpm_fit()` example (`R/cpm_fit.R:1829-1831`); `knit_one()` in `tools/precompute-vignettes.R` knitting with chunk errors fatal into a temporary file that replaces the shipped `.Rmd` only on success; the script's "Sourced by" comment (`tools/precompute-vignettes.R:79-80`).

**Out:** the introduction's pole sentence, which the plan gate left as it is (its "either value" wording matches 91 of 160 probes reporting about 0° and 69 reporting 360°); figure files that a failed knit rewrote before the failure (`git checkout -- vignettes/figures` restores them; not tracked).

## Acceptance criteria

- [x] AC1: The matrix-input `cpm_fit()` example in `R/cpm_fit.R` and `man/cpm_fit.Rd` keeps its call unchanged and is preceded by a comment saying that the fit reaches a boundary (a communality at its upper limit), so the call warns that the Hessian is ill-conditioned and its analytic intervals cannot be trusted, and naming `vignette("cpm-boundary-fits", package = "circumplex")`.
- [x] AC2: `knit_one()` knits with chunk errors fatal (`error = FALSE`) into a temporary file in `vignettes/` that replaces `vignettes/<name>.Rmd` only after `knitr::knit()` returns. In a scratch copy of the repo, `Rscript tools/precompute-vignettes.R introduction-to-ssm-analysis` run once with a chunk-level `stop()` and once with an inline `stop()` planted in `introduction-to-ssm-analysis.Rmd.orig` exits non-zero each time, leaves the shipped `introduction-to-ssm-analysis.Rmd` byte-identical (`cmp`), and leaves the list of `vignettes/*.Rmd*` files unchanged.
- [x] AC3: The "Sourced by" comment in `tools/precompute-vignettes.R` names each file that `grep -rlE '(sys\.)?source\("tools/precompute-vignettes' --include='*.R' .` returns, says they read only `VIGNETTES`, and no longer names `knit_one()`.
- [ ] AC4: After `Rscript tools/precompute-vignettes.R` re-renders all eleven vignettes with the new `knit_one()`, `Rscript tools/check-vignette-staleness.R` exits 0, and the vignette-precompute workflow passes on the PR head.
- [ ] AC5: `devtools::test()` passes and `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings, 0 notes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4

## Tasks

- [x] T1: Add the comment above `R/cpm_fit.R:1830`, run `devtools::document()`, and run the example to see the warning and the boundary note it describes. Name the existing test that asserts this fit warns.
- [x] T2: In `knit_one()`, add `error = FALSE` to `opts_chunk$set()` and knit to `paste0(name, ".Rmd.tmp")`, renaming it over `<name>.Rmd` after `knit()` returns; an `on.exit()` removes a leftover temporary file. Update the function's comments. Run both AC2 probes, plus a probe-validity control: the same two plants under the default branch's script, showing the `.Rmd` changed or the run exiting 0.
- [x] T3: Rewrite the "Sourced by" comment from the AC3 grep.
- [x] T4: Re-render all vignettes, run the staleness check, `devtools::test()`, and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-10-30: created by /milestone-plan.
- 2026-10-30: tier check: milestone — the request mixes a user-visible example change with a code fix to the precompute script.
- 2026-10-30: idea vet: flaw — the pole candidate's premise (an exact pole always reports 360) is false: 91 of 160 exact-pole profiles (4 to 16 scales, several amplitudes and elevations) gave about 0°; set answer: leave the sentence, delete the row, correct DESIGN.md and annotate D-003 (D-074). gap — "the example warns" read as "explain the warning", not "stop it"; set answer: keep the call, add a comment.
- 2026-10-30: lessons used: 2026-07-19 (M7 family; `knitr::knit()` defaults to `error = TRUE`), 2026-10-29 (M175; knit truncation and the symlink-library probe).
- 2026-10-30: inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-30: criteria audit (full mode, fresh Opus reader): 12 findings. Fixed: the pole criteria left with the dropped work (IP2 text, grep spelling, help pages); the example comment no longer promises NA output, which can differ by BLAS; AC2 adds a chunk-level plant and `error = FALSE`, since a chunk `stop()` exits 0 under knitr's default, and names the entry point; the default-branch control moved to T2 as a probe-validity check; AC3 bans the stale `knit_one()` mention and widens the grep; AC4 re-renders all eleven vignettes and adds the workflow on the PR head.
- 2026-10-30: plan gate chose a commented example over `model = "equal-communality"` because the default model stays the one users see; falsified by a CRAN check that flags the example's warning.
- 2026-10-30: plan chose `error = FALSE` in `knit_one()` because no `.Rmd.orig` sets `error` and no rendered `.Rmd` holds `#> Error`; falsified by a vignette that needs to show an error.
- 2026-10-30: implement started on branch m177-precompute-example-gaps; untracked `CRAN-SUBMISSION` left unstaged (not this milestone's).
- 2026-10-30: T1 done: comment added above the matrix example, `devtools::document()` rewrote `man/cpm_fit.Rd`; `run_examples(start = "cpm_fit")` printed the NA intervals and the Heywood note, and the plan probe showed the ill-conditioned-Hessian warning. Pinned by test-cpm_summary_markers.R ("bootstrap summary() prints the fired-marker note"), which expects jz2017's fit to fire the Heywood and ill-conditioned-Hessian markers.
- 2026-10-30: T2 probe-validity control (master's script, scratch copy): chunk plant exit 0 with the error text written into the shipped `.Rmd`; inline plant exit 1 with the `.Rmd` truncated before the References.
- 2026-10-30: T2 first try left `<name>.Rmd.tmp` behind: the `on.exit()` unlink ran after `setwd()` restored, so its relative path missed; fixed with `after = FALSE`. Re-run: chunk and inline plants each exit 1, `.Rmd` byte-identical (`cmp`), `vignettes/*.Rmd*` list unchanged; an unplanted render is byte-identical to the committed `.Rmd`.
- 2026-10-30: T4 re-render: all 11 vignettes knitted (33 s); only `ci-accuracy.Rmd` changed, one elapsed-time figure inside a masked region, restored from git; no figure file changed; `check-vignette-staleness.R` exit 0, all 11 up to date.
- 2026-10-30: claim audit: 12 claims read, 2 corrected — tools/precompute-vignettes.R (knitr writes partial output on failure, not a truncation before the first chunk), R/cpm_fit.R (intervals print NA, so "no usable analytic intervals" replaces "cannot be trusted").
- 2026-10-30: T4 checks: `devtools::test()` exit 0 with no failures; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes (7 m 57 s), run after the claim-audit edits. Status set to review.
- 2026-10-30: T3 done: the comment names the three files the AC3 grep returns (staleness, width, parity) and says they read only `VIGNETTES`.

## Decisions

## Review

- AC1 evidence (2026-10-30): `git diff master -- R/cpm_fit.R` changes comment lines only; the call `cpm_fit(cormat = R, scales = scales, n = nrow(jz2017))` is at R/cpm_fit.R:1834 and man/cpm_fit.Rd:255, once on master too. The comment says the fit reaches a boundary (a communality at its upper limit), so the call warns that the Hessian is ill-conditioned and reports no usable analytic intervals, and names `vignette("cpm-boundary-fits", package = "circumplex")`.
- AC2 evidence (2026-10-30, fresh scratch copies via the probe script): `Rscript tools/precompute-vignettes.R introduction-to-ssm-analysis` with a planted chunk `stop()` exited 1, `.Rmd` identical by `cmp`, `vignettes/*.Rmd*` list unchanged; the inline `stop()` plant gave the same three results. `error = FALSE` at tools/precompute-vignettes.R:63; `file.rename(tmp, out)` after `knit()` at :86.
- AC3 evidence (2026-10-30): the grep returns tools/check-vignette-staleness.R, tools/check-vignette-width.R, tools/m120-vignette-parity.R; the comment names those three, says they "read only VIGNETTES", and `grep -c "knit_one()"` on the file is 0.
- Gate (2026-10-30): `cairn_validate.py` all passed; `document()` 0 `resolve link` lines and no man/NAMESPACE diff; `pkgdown::check_pkgdown()` no problems; master R-CMD-check and test-coverage newest push runs `success` (53580402); both alert audits and the branch-protection check exit 0; no new top-level files; README untouched. NEWS lacked an entry for the example change: fixed now, one Documentation bullet. Staleness re-run on the branch: all 11 up to date.
