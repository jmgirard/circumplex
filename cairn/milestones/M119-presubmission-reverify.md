# M119: Re-verify v2.0.0 for submission: CRAN check time, vdiffr guards, release records

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m119-presubmission-reverify · https://github.com/jmgirard/circumplex/pull/150 (draft, opened for CI)

## Goal

Make the v2.0.0 tarball submittable after the 302 commits that followed M7's merge: bring the CRAN-mode test time down, guard the vdiffr sites against an absent Suggests package, and bring NEWS.md and cran-comments.md back in line with the tree.

## Scope

Surface tier: **user-facing** — the test edits ship inside the CRAN tarball, NEWS.md is user documentation, and cran-comments.md is read by CRAN reviewers (it is `.Rbuildignore`d, so it does not ship, but it is external-facing all the same).

**In:**
- `skip_on_cran()` on the heaviest oracle and Monte-Carlo blocks in `tests/testthat/test-axes-fiml.R`, `test-axes-reliability.R` and `test-axes-corrected-se.R`, with every input path keeping at least one asserting block live on CRAN, and CI (`NOT_CRAN=true`) still running everything.
- `skip_if_not_installed("vdiffr")` at every `vdiffr::expect_doppelganger()` site.
- NEWS.md's dependency bullet and cran-comments.md's dependency paragraph and summary of changes, corrected against the tree.
- Fresh release-check evidence on the branch tip, in both `devtools::check(manual = TRUE)` and true CRAN mode.

**Out:**
- win-builder, R-hub, `urlchecker`, the reverse-dependency check, and cran-comments.md's environment / URL-note / revdep lines → the release walk (`/cairn-release`, M7 T4), which re-derives them on the final tarball.
- Tightening the certificate suite's all-skip detector → left unchanged at the 2026-09-01 plan gate; the ROADMAP candidate row records today's pricing.
- The lavaan-version fixture skip at `test-axes-scaled-fit.R:921` → the existing ROADMAP candidate row on silent cross-check skips.
- A further accuracy pass on the M108–M118 statistics → declined at the plan gate (RR18–RR21 and the exact-rational oracles stand); no row.

## Acceptance criteria

- [x] AC1 — NEWS.md's `# circumplex 2.0.0` dependency bullet enumerates every change to DESCRIPTION's `Imports:` and `Depends:` fields between tag `v1.2.0` and the branch tip as listed by `git diff v1.2.0..HEAD -- DESCRIPTION` (ggforce removed; grid and parallel added; ggplot2 floor 3.3.0 → 4.0.0; R floor 3.4 → 4.1), none omitted.
- [x] AC2 — cran-comments.md's dependency paragraph states the Imports count read from DESCRIPTION at the branch tip, names grid and parallel as base-R additions and ggforce as the removal, and carries no "unchanged" claim; its summary of changes states the new-vignette count as the number of `vignettes/*.Rmd` at the tip minus the number at tag `v1.2.0`, and names `axes_reliability()`, the axes-reliability vignette, and the CAIS adult-sample withdrawal.
- [x] AC3 — `devtools::check(manual = TRUE)` on the branch tip: 0 errors / 0 warnings / 0 notes, and the log carries a `checking PDF version of manual` line and a `checking re-building of vignette outputs` line each ending in `OK`.
- [x] AC4 — `R CMD check --as-cran` on the tarball built from the branch tip, run with `NOT_CRAN` unset on the maintainer's machine, reports 0 errors / 0 warnings / 0 notes with `checking tests` at no more than 300 s elapsed, against 550 s measured by the same procedure on HEAD `28142f36` on 2026-09-01.
- [x] AC5 — Every `test_that()` block that gains `skip_on_cran()` in this milestone, enumerated from `git diff master...HEAD -- tests/testthat`, reports as passed (not skipped) in `withr::with_envvar(c(NOT_CRAN = "true"), testthat::test_local(reporter = "list"))` on the branch tip.
- [x] AC6 — With `NOT_CRAN` unset (`withr::with_envvar(c(NOT_CRAN = "false"), testthat::test_local(filter = "axes-", reporter = "list"))`), for each of `axes_reliability()`'s three input paths (raw data, correlation matrix, `missing = "fiml"`) at least one `test_that()` block in the three axes files exercising that path reports as passed, and each such block reddens in the same run under a planted defect on its own path (a perturbed scaling factor, SE correction, or FIML ratio respectively), the plant reverted before commit; the block names and plants are in the work log.
- [x] AC7 — Every `vdiffr::expect_doppelganger()` call site enumerated by `grep -rn "expect_doppelganger" tests/testthat` is preceded by `skip_if_not_installed("vdiffr")` in its own `test_that()` block or at its file's top level; a run of those files with vdiffr absent from the library path reports every such block as skipped and none as errored, and a run with vdiffr present and `NOT_CRAN=true` reports each of them as passed, not skipped.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T5
- AC4 → T2, T3, T5
- AC5 → T3
- AC6 → T3
- AC7 → T4

## Tasks

- [x] **T1** — NEWS.md dependency bullet (`NEWS.md:249-251`) and cran-comments.md (the `Notes on dependencies` paragraph at `:87-88`, the summary-of-changes bullets at `:56` and the highlights list) corrected per AC1/AC2, each figure derived by the command the criterion names.
- [x] **T2** — CRAN-mode per-file timing profile before any edit: `withr::with_envvar(c(NOT_CRAN = "false"), testthat::test_local(reporter = "list"))`, time summed by file and by block for the three axes files; the ranked list in the work log (baseline: fiml ≈ 334 s, reliability ≈ 53 s, corrected-se ≈ 45 s, measured 2026-09-01; `test-axes-corrected-se.R:471` already carries one `skip_on_cran()`).
- [x] **T3** — Wrap the heaviest oracle / Monte-Carlo blocks from T2's list in `skip_on_cran()` until AC4's budget is met, choosing the one live block per input path first and planting-and-reverting its defect (AC6), then the AC5 off-CRAN run.
- [x] **T4** — `skip_if_not_installed("vdiffr")` at the 29 sites in 9 files (`grep -rn "expect_doppelganger" tests/testthat`); absent-library run via `callr::r()` with a library path lacking vdiffr, then the present-library positive control (AC7).
- [x] **T5** — `devtools::check(manual = TRUE)` and the CRAN-mode `R CMD check --as-cran` on the tip; push; CI green on macOS, windows, ubuntu (AC3, AC4).

## Work log

- 2026-09-01: created by /milestone-plan at Jeff's release-window declaration ("Yes, open it"); M7 gains `Depends on: M119`.
- 2026-09-01: baseline measured on HEAD 28142f36: `devtools::check(manual = TRUE)` 0/0/0, tests 561 s (that command sets `NOT_CRAN=true`); `R CMD check --as-cran` on the built tarball with `NOT_CRAN` unset 0/0/0, tests 550 s, whole check 715 s, examples 16 s + donttest 34 s, 143 skips (77 on-CRAN); July's release check had tests at 242 s. Reverse dependencies: none. `urlchecker::url_check()`: all 28 URLs OK.
- 2026-09-01: criteria audit ran in full mode ([O] fresh reader): 11 FIX findings applied — the time budget measured in true CRAN mode because `devtools::check()` forces `NOT_CRAN=true`; a positive control on every new skip; sentence-level not integer-level record fixes; parallel alongside grid; log-line matching tolerant of `--timings` brackets; 29 not 33 vdiffr sites; `test-axes-corrected-se.R:471` already carries a `skip_on_cran()` — and 3 ASK findings disposed autonomously: AC2 scoped to what the tree made false with the walk re-deriving environment lines; AC7 kept at the grep-enumerated sites plus a positive control rather than a `_R_CHECK_DEPENDS_ONLY_` run; the tier reason corrected (cran-comments is `.Rbuildignore`d, reviewer-read).
- 2026-09-01: plan gate chose CRAN-skipping heavy oracle cells over submitting as-is because a check-time bounce costs a resubmission cycle and CI still runs every skipped cell; falsified by CRAN accepting an unchanged 550 s suite, or by a skipped cell being the only one that would have caught a regression.
- 2026-09-01: plan gate chose no extra accuracy pass over a Fable brief on the certificate surfaces because RR18–RR21 and the exact-rational oracles cover the shipped numbers and every open finding is harness-latent; falsified by a shipped `axes_reliability()` value found wrong by a user or a wide-check platform.
- 2026-09-01: plan gate chose leaving the all-skip detector unchanged over requiring a built anchor priced because the tightened form goes red on a libm that rounds one cosine differently; falsified by the five built anchors observed skipping together on any platform.
- 2026-09-01: gate prompt flagged unclear by Jeff, captured verbatim: "i dont understand what youre asking here" (the detector question, posed in terms of anchors, fixtures and pricing); re-asked in plain words as a guard that at least one case ran, which cannot fail and cannot notice five silent skips; answered "Leave it alone".
- 2026-09-01: T1 — `git diff v1.2.0..HEAD -- DESCRIPTION`: ggforce out; grid, parallel in; ggplot2 3.3.0→4.0.0; R 3.4→4.1; Imports count 8 at tip (was stated as seven, unchanged). Vignettes 9 at tip vs 3 at v1.2.0 → six new (was stated as five). NEWS bullet gains grid/parallel; cran-comments gains an `axes_reliability()` highlight naming the "Axes Reliability" vignette and a CAIS adult-sample withdrawal bullet.
- 2026-09-01: T4 — `skip_if_not_installed("vdiffr")` inserted as the first line of the 17 `test_that()` blocks holding the 29 `expect_doppelganger()` sites (9 files). `callr::r()` with a symlinked library lacking vdiffr (`installed.packages()` confirms absent), `NOT_CRAN=true`, the 9 files: all 17 blocks skipped, 0 errors, 0 failures in the run; same run with the real library: all 17 passed (1–5 expectations each), 0 skipped, 0 failures.
- 2026-09-01: T2 — CRAN-mode `test_local(reporter = "list")` on the branch before any test edit (T1/T4 only): 531 s summed over blocks; by file fiml 331 s, corrected-se 45 s, reliability 40 s, everything else 115 s. Ranked blocks: fiml `M65-D3: stored seeds reproduce live` 202 s; corrected-se `BC4: the shipped composition evaluates the ratio at Sigma-hat` 21 s; fiml `BC15` 20 s; reliability `BC6: Monte-Carlo mean xi1` 19 s; fiml `BC14` 17 s; corrected-se `M66: stored cells reproduce live` 16 s; then fiml `BC9 sd = "raw"` 10 s, `AC16` 10 s, and a tail of 7 s single-fit blocks. The six blocks above sum to 295 s, so skipping them is projected to leave ~236 s.
- 2026-09-01: T3 (part 1) — `skip_on_cran()` added to six blocks: fiml `M65-D3: stored seeds reproduce live`, `BC14`, `BC15`; corrected-se `BC4: the shipped composition evaluates the ratio at Sigma-hat`, `M66: stored cells reproduce live`; reliability `BC6: Monte-Carlo mean xi1`; the fiml file's two comments that described those blocks as unskipped now say CI is the fence. AC6 live blocks and plants, CRAN-mode `test_local(filter = "axes-")`, plants applied in copies of the tree and never committed: raw → reliability `AC2: the cormat path reproduces the raw path exactly` (passed 16/16 clean; 8 of 16 failed under `zmat <- scale(mat) * 1.01` at R/axes_reliability.R, the raw path's z-standardization); cormat → corrected-se `BC1: components$SE is the corrected value, details keeps the naive one` (9/9 clean; 3 failed under `1.01 * corrected$corrected[...]` in the listwise/cormat `se_reported` branch); fiml → corrected-se `AC7: the reported FIML SE is se_uncorrected times fiml_ratio` (6/6 clean; 3 failed under `1.01 * se_uncorrected * corrected$fiml_ratio[...]` in the FIML branch). Each plant reddened only blocks on its own path (raw also reddened the three fiml-file listwise-vs-FIML agreement blocks; cormat also the K-matrix BC1 block). Clean run: 254 blocks, 246 passed, 8 skipped, 0 failed; axes files summed 206 s while three other runs shared the CPU.
- 2026-09-01: T3 (part 2, AC5) — `NOT_CRAN=true` full `test_local(reporter = "list")` on the tip: 0 failures, 0 errors, 1 skipped block suite-wide; all six blocks that gained `skip_on_cran()` reported passed (M65-D3 49 expectations / 169 s, BC4 3, M66 5, BC14 7, BC15 5, BC6 2); suite 669 s summed real.
- 2026-09-01: T5 (part 1, AC3) — `devtools::check(manual = TRUE)` on 604f81cf: Status OK, 0 errors / 0 warnings / 0 notes; `checking re-building of vignette outputs ... [39s/40s] OK`, `checking PDF version of manual ... OK`; tests `[10m/10m]` (that command sets `NOT_CRAN=true`, so the six new skips do not fire there).
- 2026-09-01: T5 (part 2, AC4) — `R CMD build` then `R CMD check --as-cran --timings` on the tarball from 7edbe015 with `NOT_CRAN` unset, nothing else running: Status OK, 0 errors / 0 warnings / 0 notes; `checking tests ... [213s/214s]` (baseline 550 s on 28142f36); examples 11 s + donttest 23 s; testthat summary `FAIL 0 | WARN 4 | SKIP 149 | PASS 8123` (149 skips = the baseline's 143 + the six new).
- 2026-09-01: T5 (part 3) — branch pushed; PR #150 opened as a draft because R-CMD-check runs on pull_request only; CI on 324e604b: macOS 16m30s pass, ubuntu 26m31s pass, windows 29m43s pass, pkgdown pass. All tasks done; status → review.

## Decisions

## Review

Evidence gathered 2026-09-01 by /milestone-review on branch tip c72101f7 (master unmoved since the branch was cut; draft PR #150 open).

- AC1 — `git diff v1.2.0..HEAD -- DESCRIPTION` shows exactly: ggforce removed; grid and parallel added; ggplot2 `>= 3.3.0` → `>= 4.0.0`; R `>= 3.4` → `>= 4.1` (Suggests additions are outside the criterion). The `# circumplex 2.0.0` bullet at NEWS.md:249-254 names all four changes. PASS.
- AC2 — DESCRIPTION Imports at tip parsed to 8 entries; `vignettes/*.Rmd` 9 at tip vs 3 at `v1.2.0` → 6 new. cran-comments.md "Notes on dependencies" states eight Imports, names grid and parallel as base-R additions and ggforce as the removal, and contains no "unchanged" claim; the summary says "Six new vignettes" and names `axes_reliability()`, the "Axes Reliability" vignette, and the CAIS adult-sample withdrawal. PASS.
- AC4 — `R CMD build` then `R CMD check --as-cran --timings` on `circumplex_2.0.0.tar.gz` from c72101f7, `NOT_CRAN` unset, nothing else running: Status OK, 0 errors / 0 warnings / 0 notes; `checking tests ... [172s/173s] OK` (budget 300 s; baseline 550 s on 28142f36); testthat `FAIL 0 | WARN 4 | SKIP 149 | PASS 8123`. PASS.
- AC7 (static half) — a script over `tests/testthat/test-*.R` finds 29 `expect_doppelganger()` sites in 17 `test_that()` blocks across 9 files, every site preceded by `skip_if_not_installed("vdiffr")` inside its own block; 0 unguarded. Dynamic half below.
- AC7 (dynamic half) — `callr::r()` with a symlinked library omitting vdiffr (`installed.packages()` confirms absent), `NOT_CRAN=true`, `test_local(filter =` the 9 files`)`: 176 blocks ran, all 17 doppelganger blocks skipped, 0 errors, 0 failures. Same run with the real library: all 17 passed (1–5 expectations each), 0 skipped, 0 errors, 0 failures. PASS.
- AC6 — four `git archive HEAD` copies of the tip, CRAN mode (`NOT_CRAN="false"`, `test_local(filter = "axes-", reporter = "list")`), plants applied by `sed` to `R/axes_reliability.R` in the copies only (the working tree is untouched; `git status` clean apart from this file). Clean copy: 254 blocks, 246 passed, 8 skipped, 0 failed, and the three live blocks pass — raw: reliability `AC2: the cormat path reproduces the raw path exactly` (16/16); cormat: corrected-se `BC1: components$SE is the corrected value, details keeps the naive one` (9/9); fiml: corrected-se `AC7: the reported FIML SE is se_uncorrected times fiml_ratio` (6/6). Plants: raw `zmat <- scale(mat) * 1.01` reddens its live block (8 of 16 fail) plus three fiml-file listwise-vs-FIML agreement blocks, 4 failed blocks total; cormat `1.01 * corrected$corrected[...]` reddens its live block (3 of 9) plus the K-matrix BC1 block, 2 total; fiml `1.01 * se_uncorrected * corrected$fiml_ratio[...]` reddens its live block alone (3 of 6). Each live block reddens only under its own path's plant. PASS.
- AC5 — the six blocks gaining `skip_on_cran()` per `git diff master...HEAD -- tests/testthat` (fiml `M65-D3: stored seeds reproduce live`, `BC14`, `BC15`; corrected-se `BC4: the shipped composition evaluates the ratio at Sigma-hat`, `M66: stored cells reproduce live`; reliability `BC6: Monte-Carlo mean xi1`) all report passed in `with_envvar(c(NOT_CRAN = "true"), test_local(reporter = "list"))` on the tip (49 / 7 / 5 / 3 / 5 / 2 expectations); suite 1015 blocks, 1014 passed, 1 skipped (the pre-existing lavaan-version fixture skip in test-axes-scaled-fit.R), 0 failed, 680 s summed real while AC3 and AC6 shared the CPU. PASS.
- AC3 — `devtools::check(manual = TRUE)` on the tip: Status OK, 0 errors / 0 warnings / 0 notes; `checking PDF version of manual ... OK`; `checking re-building of vignette outputs ... [28s/28s] OK`; tests `[505s/509s]` (that command sets `NOT_CRAN=true`, so the six new skips do not fire; AC5 and AC6 ran concurrently). PASS.

Consistency gate (2026-09-01): `cairn_validate.py` exit 0, all checks pass (47 pre-existing M7 work-log-format advisories); no principle changed, `cairn_impact` skipped. Toolchain slot: `devtools::document()` no diff, 0 `resolve link` lines; generated files untouched by the diff; README.Rmd not touched; `pkgdown::check_pkgdown()` no problems; NEWS.md carries the dependency bullet; no new top-level files; full check = AC3; master watches — newest verdict-bearing push runs of R-CMD-check and test-coverage on master (45e532b1, M118) both success; `check-master-red-alert.R` and `master-red-alert-dryrun.R` clean; `check-branch-protection.R` matches. PR #150 CI on c72101f7: macOS pass, ubuntu pass, matrix pass, pkgdown pass, windows pending at gate time. Driving RR: none.

Fresh-context review (three lenses, ranked as reported):
- [O] F1 — `test-fit_structure_api.R:273-280`: the block guarded by `skip_if_not_installed("vdiffr")` also holds `expect_warning(plot(res, bogus = TRUE))`, the only test of `plot.circumplex_fit_structure()`'s unknown-dots warning, now untested when vdiffr is absent. Verified. Recommended: fix now (split the warning into its own block).
- [O] F2 — the same shape at `test-ci_accuracy.R:893`, `test-ssm_plot.R:111` and five `is_ggplot()` checks in test-ssm_plot.R. Recommended: reject — those assertions accompany the snapshot they describe, CI and the maintainer machine carry vdiffr, and the canvas equivalence is also asserted ungated in test-scale_circumplex.R.
- [O] F3 — NEWS.md:250-251 and cran-comments.md:93-94 say trajectory plots "draw grobs directly"; `R/ssm_trajectory.R` uses only `grid::unit()` at line 615, every grob call is in `R/coord_circumplex.R` (grid entered Imports at M39, f10b483a). Verified. Recommended: fix now.
- [O] F4 — NEWS.md:251-252 and cran-comments.md:94-95 attribute `parallel` to the `parallel`/`ncpus` bootstrapping arguments; the only `parallel::` calls are `ssm_ci_accuracy()`'s worker pool (`R/ssm_ci_accuracy.R:778-789`; parallel entered Imports at 6c7e5fc6 with that function), and `boot::boot()` handles the bootstrap arguments itself. Verified; the cran-comments clause predates the branch, NEWS newly adopts it. Recommended: fix now.
- [O] F5 — `test-axes-fiml.R:1138` says the block "runs on every CI push"; R-CMD-check triggers on pushes to master and on pull requests only. Verified. Recommended: fix now (wording).
- [O] F6/F7 — `test-axes-fiml.R:1019-1023` and `:1134-1141`: the amended comments read awkwardly ("on CI" after the aside; the four-green-checks list now hangs off the wrong clause). Recommended: fix now alongside F5, same comments.
- [O] F8 — cran-comments.md:1-7 environment block stale (Darwin 25.5.0, win-builder, CI matrix). Recommended: reject — the plan's Out list routes these lines to the release walk.
- [O] F9 — informational: the two fixture-staleness sentinels (M65-D3, M66) are CRAN-off; the plan gate's recorded trade. No action.
- [S-blame] B1 / [S-prior] P1 (same finding) — `skip_on_cran()` added to the M65-D3 block whose comment said "It carries no skip flag" citing four green-because-never-ran shipments; both lenses ask whether CI really sets `NOT_CRAN=true`. Verified at review: the PR's `check-r-package` step env shows `NOT_CRAN: true` and its testthat summary reads `SKIP 78` against `SKIP 149` in local CRAN mode, so the six skips do not fire on CI. Recommended: reject — the intentional change the plan called for, with its fence now verified.
- [S-blame] B2–B5 — BC14/BC15 comment narrowing, corrected-se and reliability skips consistent with file convention, NEWS/cran-comments figures match DESCRIPTION and D-052: no conflict found. Logged, no action. B-process note (AC boxes unticked at review start) — review ticks them under AC fencing; no action.
- [S-prior] P2 — `test-coord_circumplex.R`'s legibility snapshot is the sole fence M39 F4 named; the AC7 positive control shows it passes with vdiffr present. No action.
- [S-prior] probe: `gh api .../pulls/comments?per_page=1` returned `[]`; no archived `## Review` names the touched files.
