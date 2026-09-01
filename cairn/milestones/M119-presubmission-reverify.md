# M119: Re-verify v2.0.0 for submission: CRAN check time, vdiffr guards, release records

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m119-presubmission-reverify

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

- [ ] AC1 — NEWS.md's `# circumplex 2.0.0` dependency bullet enumerates every change to DESCRIPTION's `Imports:` and `Depends:` fields between tag `v1.2.0` and the branch tip as listed by `git diff v1.2.0..HEAD -- DESCRIPTION` (ggforce removed; grid and parallel added; ggplot2 floor 3.3.0 → 4.0.0; R floor 3.4 → 4.1), none omitted.
- [ ] AC2 — cran-comments.md's dependency paragraph states the Imports count read from DESCRIPTION at the branch tip, names grid and parallel as base-R additions and ggforce as the removal, and carries no "unchanged" claim; its summary of changes states the new-vignette count as the number of `vignettes/*.Rmd` at the tip minus the number at tag `v1.2.0`, and names `axes_reliability()`, the axes-reliability vignette, and the CAIS adult-sample withdrawal.
- [ ] AC3 — `devtools::check(manual = TRUE)` on the branch tip: 0 errors / 0 warnings / 0 notes, and the log carries a `checking PDF version of manual` line and a `checking re-building of vignette outputs` line each ending in `OK`.
- [ ] AC4 — `R CMD check --as-cran` on the tarball built from the branch tip, run with `NOT_CRAN` unset on the maintainer's machine, reports 0 errors / 0 warnings / 0 notes with `checking tests` at no more than 300 s elapsed, against 550 s measured by the same procedure on HEAD `28142f36` on 2026-09-01.
- [ ] AC5 — Every `test_that()` block that gains `skip_on_cran()` in this milestone, enumerated from `git diff master...HEAD -- tests/testthat`, reports as passed (not skipped) in `withr::with_envvar(c(NOT_CRAN = "true"), testthat::test_local(reporter = "list"))` on the branch tip.
- [ ] AC6 — With `NOT_CRAN` unset (`withr::with_envvar(c(NOT_CRAN = "false"), testthat::test_local(filter = "axes-", reporter = "list"))`), for each of `axes_reliability()`'s three input paths (raw data, correlation matrix, `missing = "fiml"`) at least one `test_that()` block in the three axes files exercising that path reports as passed, and each such block reddens in the same run under a planted defect on its own path (a perturbed scaling factor, SE correction, or FIML ratio respectively), the plant reverted before commit; the block names and plants are in the work log.
- [ ] AC7 — Every `vdiffr::expect_doppelganger()` call site enumerated by `grep -rn "expect_doppelganger" tests/testthat` is preceded by `skip_if_not_installed("vdiffr")` in its own `test_that()` block or at its file's top level; a run of those files with vdiffr absent from the library path reports every such block as skipped and none as errored, and a run with vdiffr present and `NOT_CRAN=true` reports each of them as passed, not skipped.

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
- [ ] **T5** — `devtools::check(manual = TRUE)` and the CRAN-mode `R CMD check --as-cran` on the tip; push; CI green on macOS, windows, ubuntu (AC3, AC4).

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

## Decisions

## Review
