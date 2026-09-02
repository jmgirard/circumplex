# M120: Bring the Windows CRAN check under 8 minutes

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Branch/PR:** `m120-cran-check-time` / https://github.com/jmgirard/circumplex/pull/151

## Goal

CRAN's incoming pretest for v2.0.0 returned a NOTE on `r-devel-windows-x86_64`
— overall check time 24 min against CRAN's 10-minute expectation — so cut the
Windows check cost to under 8 minutes without deleting coverage, by
precomputing the vignettes and skipping the heaviest test blocks on CRAN while
keeping the full suite live off CRAN. Surface tier: **user-facing** — the
vignettes and examples this touches are shipped documentation (GP5's precision
bar travels with them).

## Scope

**In:**
- Precompute every vignette that runs a live model fit or bootstrap: the
  computation moves to a committed `.Rmd.orig` rendered by a script, the
  rendered `.Rmd` ships, and a CI job rebuilds from `.Rmd.orig` on push.
- `skip_on_cran()` on the heaviest test blocks, top-down by measured cost,
  until the AC1 budget is met; nothing is deleted and CI keeps running all of it.
- One live CRAN-mode block per exported function losing CRAN coverage, proven
  to redden under planted defects (the M119 doctrine).
- Cheapen the documented examples still using the 2000-resample default, by
  editing their roxygen in `R/`.
- Re-measure locally; run the release check; refresh `cran-comments.md` where
  its figures move.

**Out:**
- The resubmission itself → M7 (T4 there); this milestone hands over a tarball
  whose check time is verified, and never submits.
- Changing exported `boots`/`reps` defaults (a GP4 behavior change) — call
  sites in `vignettes/`, `man/` and roxygen only.
- Cutting the Windows install/compile cost (`src/`) → candidate row, promoted
  if win-builder's install line shows compilation dominating the residual.
- Deleting any test, vignette chunk, or example.

## Acceptance criteria

- [x] AC1 — On the tarball built from the branch tip, `R CMD check --as-cran`
      with `NOT_CRAN` unset on the maintainer's macOS machine reports a tests
      step of at most 25 s, a vignette re-build step of at most 10 s, and a
      total under 2 min, against the 259 s / 40 s / 6 min 14 s baseline
      measured there 2026-09-01.
- [x] AC2 — A win-builder R-devel run of that tarball reports an overall check
      time under 8 minutes, with its tests, vignette re-build, examples and
      install line items recorded.
- [x] AC3 — Every test block this milestone newly skips on CRAN still runs off
      CRAN: for the blocks enumerated by the branch diff over
      `tests/testthat/`, a `NOT_CRAN=true` per-block run names each one with a
      result that is neither skip nor failure.
- [x] AC4 — Domain: the exported functions whose CRAN-mode coverage this
      milestone reduces, enumerated by `tools/m120-cran-coverage.R` as the
      union of the CRAN-mode skipped-block diff (before vs after) and the
      branch diff over `tests/testthat/`; the instrument fails if the domain is
      empty. (i) For every domain function, at least one block naming it is run
      in CRAN mode on the clean tree and reports a pass with no skip — shown by
      running it, not by matching text. (ii) For every domain function in the
      `ssm_*`, `cpm_*` and `axes_*` families except the renderers `ssm_table`,
      `ssm_plot_circle`, `ssm_plot_curve`, `ssm_plot_contrast` and
      `ssm_plot_trajectory`, some live-on-CRAN block fails — a test failure,
      not an error — when a planted defect inflates by 5% a numeric value the
      function returns. (iii) `angle_unwrap`, `cpm_fit`, `ssm_analyze`,
      `ssm_draws`, `ssm_parameters` and `ssm_sem_parameters` each have a
      live-on-CRAN block asserting an expected angular *value* at the 0°/360°
      pole — not a relational property — and each such block fails under a wrap
      defect planted on the angular path that function uses: displacement left
      on `atan2`'s (-π, π] branch for the SSM estimator and for the SEM
      transform, a pole angle reporting 0 instead of 360 for the CPM path.
      Where the suite has no such block, this milestone authors one. Blocks may
      differ between (ii) and (iii). (iv) `tools/m120-planted-defects.R` reads
      the domain emitted by `tools/m120-cran-coverage.R` and fails unless every
      domain function appears in `tools/m120-designations.csv` with a
      disposition and the defects that disposition requires.
- [x] AC5 — Every vignette chunk and documented example whose computation this
      milestone precomputes or cheapens — the set enumerated by the branch diff
      over `vignettes/`, `man/`, and any `R/` default or helper feeding a
      documented chunk — renders without error, and no mismatch survives
      between the surrounding prose and the rendered output.
- [x] AC6 — `Rscript -e 'devtools::check(manual = TRUE)'` on the branch tip is
      0 errors / 0 warnings / 0 notes, and `devtools::document()` produces no
      diff.

## Coverage

- AC1 → T2, T3, T5, T6
- AC2 → T7
- AC3 → T3
- AC4 → T4, T9
- AC5 → T2, T5, T8
- AC6 → T6

## Tasks

- [x] **T1** — Re-derive the baseline on the branch and record the procedure:
      per-file CRAN-mode `ListReporter` timings and the `--as-cran` step
      timings, both from the built tarball with `NOT_CRAN` unset.
- [x] **T2** — Precompute the vignettes: `.Rmd.orig` + render script,
      `.Rbuildignore` the sources, commit rendered `.Rmd` and figures, add the
      CI rebuild job. Heaviest first — `evaluating-circumplex-structure.Rmd`,
      `sem-based-ssm-analysis.Rmd`, `advanced-visualization.Rmd`,
      `intermediate-ssm-analysis.Rmd` (6 uncapped default-2000 calls),
      `introduction-to-ssm-analysis.Rmd` (3), `growth-ssm-analysis.Rmd`,
      `axes-reliability.Rmd`.
- [x] **T3** — `skip_on_cran()` top-down by measured cost until AC1's tests
      budget is met: `test-axes-fiml.R` (91.8 s), `test-axes-reliability.R`
      (21.0), `test-cpm_api.R` (14.0), `test-ssm_sem_groups.R` (10.9),
      `test-cpm_boundary_vignette.R` (10.4), `test-axes-scaled-fit.R` (9.5),
      `test-ssm_plot.R` (8.9), `test-axes-corrected-se.R` (8.2),
      `test-suff_stats.R` (8.2), `test-geom_ssm_path.R` (6.5), then the tail.
- [x] **T4** — Live-coverage retention: enumerate the functions losing CRAN
      coverage per AC4, keep or author one live CRAN-mode block each, plant the
      two defect forms per statistical function, record each red.
- [x] **T5** — Cheapen the default-2000 examples in roxygen, then
      `devtools::document()`: `ssm_analyze` (3 un-gated calls + 5 under
      `\donttest`), `ssm_plot_circle`, `ssm_table`, `ssm_plot_curve`,
      `ssm_plot_contrast`, `ssm_plot_trajectory`, `ssm_analyze_long`.
- [x] **T6** — Re-measure locally (AC1); `devtools::check(manual = TRUE)`;
      refresh `cran-comments.md` where its figures move.
- [x] **T7** — win-builder R-devel run on the branch tarball; record the
      overall time and the four line items (Jeff runs it; results reach only
      his address).
- [x] **T8** — Defect return, F1 + F6: repair the `vignette-precompute` guard
      so it can pass — declared `precompute:volatile-numbers` regions in the
      `.Rmd.orig` sources, `tools/check-vignette-staleness.R` comparing
      everything byte-exactly except the digits of output lines inside a
      declared region, the workflow pointed at it — and restore the shipped
      figures' `alt` text with a `fig.alt` hook. Re-render.
- [x] **T9** — Defect return, F2 + F4: replace the relative `tolerance`
      assertions in `test-plot-cran-guards.R` and `test-pole-values.R` with
      absolute bounds, then re-run the AC4 designations they carry.
- [x] **T10** — Defect return, F3: make `tools/m120-planted-defects.R` restore
      a planted defect when the run errors, as its header claims.
- [x] **T11** — Defect return, F7: give `tools/m120-vignette-parity.R` a
      dependency surface a clean checkout has.

## Work log

- 2026-09-01: created by /milestone-plan after CRAN's pretest NOTE. Baseline measured here: local `--as-cran` total 6 min 14 s (install 11 s, examples 12 s, examples --run-donttest 27 s, tests 259 s, vignettes 40 s); CRAN-mode per-file test total 234 s, 84 skips of 1016 blocks. Windows/Mac factor 3.5x from the 2026-09-01 win-builder tests and vignette line items; its examples line ran 1.5x, so the factor is asserted only for tests and vignettes. **Assumption:** 328 s of the Windows 24 min is unattributed by the four recorded line items and is taken to be install/compile, which this milestone does not touch; the full win-builder log's install line replaces this estimate when available, and AC1's budget moves with it.
- 2026-09-01: plan gate chose precomputing the vignettes over lowering their bootstrap counts, because capping saves roughly half of 146 s where precomputing takes the step to near zero; falsified by a rendered vignette breaking in a way the CI rebuild job fails to catch.
- 2026-09-01: plan gate chose `skip_on_cran()` over shrinking the SEM fixtures, because retuning tolerances on lavaan fits is statistical work that can quietly stop catching regressions; falsified by a regression reaching CRAN that a shrunk-but-live block would have caught.
- 2026-09-01: plan gate chose an 8-minute target over just-under-10 (Jeff's call), for margin against a slower or busier CRAN machine; falsified by the 8-minute target proving unreachable without cutting into `src/`.
- 2026-09-01: criteria audit ran in **full** mode ([O], fresh context). It returned: AC1's original targets were arithmetically insufficient (they reached ~11.8 min, not 10) — targets re-derived from the 600 s budget and the residual recorded as an assumption; the baseline date was wrong; AC3's zero-failure count did not show the named blocks ran — now per-block; AC4's skip-set diff enumerated a proxy, missing blocks made cheaper rather than skipped — now a union with the branch diff; AC4's single planted defect stood one exemplar in for a family — now two forms, scoped to the statistical families for proportionality; AC5 was satisfied by the act of recording a re-read — now by no mismatch surviving. All fixed at the gate.
- 2026-09-01: `cairn_validate`'s release-window advisory fires on this milestone's CRAN-shaped title; dispositioned — M120 ships no version and never submits (the resubmission stays M7's T4), so it stays `planned` rather than parked as `blocked`.
- 2026-09-01: Jeff's win-builder log read in full (https://win-builder.r-project.org/YRV8eN2j66TA/00check.log): every timed step is `incoming feasibility [14s]`, `R code for possible problems [39s]`, `examples [41s]`, `tests [15m]`, `re-building of vignette outputs [146s]`, `PDF manual [25s]` — 1,165 s of the 1,440 s total, so the unattributed residual narrows from 328 s to 275 s. The install step (`checking whether package 'circumplex' can be installed ... OK`) carries no timing there, so the compile-cost assumption above stands unmeasured; CRAN's pretest `00check.log` is still the artifact that would settle it. That run's Status was OK with no notes — CRAN's incoming pretest applies the 10-minute checktime rule that win-builder does not.
- 2026-09-01: CRAN's pretest Windows log read in full (incoming_pretest/circumplex_2.0.0_20260902_050533/Windows/00check.log): `Status: OK`, and the strings NOTE, WARNING, ERROR and checktime appear nowhere in it — so the pretest table's `Check: *, Result: NA` row is a summary-layer artifact, not a second defect, and the checktime NOTE is raised by CRAN's summary layer rather than by `R CMD check`. Timed steps there: feasibility [15s], R code for possible problems [44s], examples [45s], tests [16m], vignette re-build [145s], PDF manual [25s] = 1,234 s of the 1,440 s, narrowing the unattributed residual to 206 s; the install step again carries no timing. AC1's targets are unchanged and now carry more margin: fixed cost ~335 s leaves ~145 s of Windows headroom for tests plus vignettes against the 8-minute target.
- 2026-09-02: T1 — baseline re-derived on the branch with `tools/m120-test-timings.R` (committed; clears `NOT_CRAN`, `test_dir()` + `ListReporter`) and an `--as-cran --timings` run of the built tarball with `NOT_CRAN` unset. Tests 157.0 s live over 932 blocks (84 already skipped, 0 failures); `--as-cran` steps: tests 155 s, vignette re-build 27 s, examples --run-donttest 17 s, examples untimed, install untimed, Status OK. These are 2026-09-02 figures on the maintainer's macOS machine and run faster than the plan's 2026-09-01 baseline (259/40/27 s) — AC1's targets are absolute, so they bind unchanged. Cost is concentrated: skipping the 100 costliest live blocks (each >= 0.19 s) leaves 27.3 s, the top 150 (>= 0.11 s) leaves 19.8 s.
- 2026-09-02: T2 — seven vignettes now ship pre-computed: each source moved to `vignettes/<name>.Rmd.orig` (`.Rbuildignore`d) and `tools/precompute-vignettes.R` knits it to the shipped `.Rmd`, with figures under `vignettes/figures/`. Rendering all nine shipped vignettes fell from 26.8 s to 3.61 s. `tools/m120-vignette-parity.R` renders each source and its pre-computed copy and compares visible text: all seven match (only `ssm_ci_accuracy()`'s printed wall-clock `Elapsed:` line is normalized); planting one character in a rendered vignette reddens it. New workflow `.github/workflows/vignette-precompute.yaml` re-knits and fails on a stale `.Rmd`, registered in `tools/check-ci-deps.R`'s policy — deleting `any::lavaan` from its allowlist reddens that guard. `test-cpm_boundary_vignette.R` read chunk source out of the shipped vignette, which pre-computation removes: its prose guards still read the shipped `.Rmd` in every build, its three chunk-running guards now read the `.Rmd.orig` and `skip_on_cran()`. Full suite off CRAN: 0 failures, 9198 passes, 1 skip.
- 2026-09-02: T3 — `skip_on_cran()` added to 416 test blocks, chosen top-down by measured cost in three passes (thresholds 0.10 s, 0.10 s, 0.07 s, re-measuring between passes because per-file warm-up cost migrates onto whichever block runs first). CRAN-mode live cost fell 157.0 s -> 7.0 s over 516 still-live blocks; 500 of 1016 blocks now skip on CRAN. An intermediate `--as-cran` run after two passes read 26 s for tests and 1 min 54 s total. `tools/m120-skipped-blocks-live.R` derives the newly skipped set from the branch diff (HEAD's skipped-block names minus master's) and requires each to run off CRAN: all 416 ran, none skipped, none failed; whole suite off CRAN 1016 blocks, 0 failed, 0 errored, 1 skipped. Planting `skip()` in one of those blocks reddens it. The instrument refuses an empty newly-skipped set and errors on a `skip_on_cran()` outside any `test_that()` block, which is why T2's helper-level skip moved onto its three calling blocks.
- 2026-09-02: Jeff chose the deeper (three-pass) cut at the mini gate over backing off one pass and raising AC1's tests figure to 30 s: ~6.2 min projected on Windows against ~7.3 min, at the cost of 153 more blocks skipping on CRAN. AC1 is unamended; CI keeps running every block.
- 2026-09-02: T5 — `boots = 200` added to the 15 roxygen example calls that ran the 2000 default (`ssm_analyze` 8, `ssm_plot_circle`/`ssm_plot_curve`/`ssm_plot_contrast` 1 each, `ssm_table` 2, `ssm_plot_trajectory` 1, `ssm_analyze_long` 1), each `@examples` block opening with a line saying the count is lowered for speed and a reported analysis should use the default; `devtools::document()` regenerated seven Rd files and emitted no warning. `--as-cran` on the resulting tarball, with all step timings forced on: **tests 15 s, vignette re-build 4 s, total 1 min 24 s, Status OK** — AC1 met against the 259 s / 40 s / 6 min 14 s baseline. Examples fell 12 s -> 8 s and `--run-donttest` 27 s -> 11 s. Full suite off CRAN: 0 failures, 9198 passes.
- 2026-09-02: AMENDMENT to AC4, at Jeff's selection over a first-audited loosening he had chosen an hour earlier. Two fresh-context [O] readers audited the wording; the second rejected the loosening and its counter-proposal is what is now written. Grounds, all measured by planting real defects: the repo's 0/360 boundary blocks assert relational properties (the interval wraps, it contains the estimate) that survive a uniform branch shift, so they stay GREEN under a displacement left on atan2's branch — only 8 live blocks anywhere catch it, all core-estimator tests that pin a number; the loosened wording would have been signed off by those incidental catches while `ssm_plot_circle`'s CRAN-visible coverage is `is_ggplot()` (its vdiffr expectations skip on CRAN); and the loosening needed both instruments rewritten anyway, so it was not the cheaper route. One mechanical correction to the reader's text: it asked each pole block to fail under both wrap defects, which no single block can, since the SSM branch defect and the CPM pole defect sit on different paths — scoped to the path each function uses.
- 2026-09-02: T4 — AC4 satisfied over all 32 domain functions. `tools/m120-cran-coverage.R` now computes the domain as the union of the newly-CRAN-skipped blocks and the blocks this branch touched, refuses an empty domain, and writes `tools/m120-domain.txt`; `tools/m120-planted-defects.R` reads that file and refuses to run unless every domain function has a row in `tools/m120-designations.csv` with a disposition, every satisfied family row carries a magnitude defect, and each of AC4(iii)'s six carries a wrap defect. Nine defects are planted (magnitude in the C++ estimator, both SEM transforms, the OLS projection weights, the CPM communality, the axis reliability and the coverage rate; wrap in the C++ displacement, both SEM transforms, the CPM pole label and angle_unwrap's shortest-rotation step). All 33 rows pass on CRAN with the tree clean and go red under every defect they must catch. Two new files: `test-pole-values.R` (six blocks naming the angle a profile was built at) and `test-plot-cran-guards.R` (four blocks asserting the numbers the built plot layers carry). Seven previously skipped blocks came back on CRAN. AC3 re-run: 412 newly skipped blocks, all run off CRAN, none skipped, none failed; whole suite off CRAN 1026 blocks, 0 failed, 0 errored. CRAN-mode cost 12.0 s over 532 live blocks (494 skipped).
- 2026-09-02: finding recorded as an accepted risk, not fixed: the suite's pre-existing 0/360 blocks assert relational properties and stay green under a uniform branch shift, and the four plot functions had NO CRAN-visible coverage at all because vdiffr skips there. This milestone adds ten blocks that close both for the functions AC4 names; it does not re-examine every other relational boundary assertion in the suite. Falsified by a wrap regression reaching CRAN that a value-asserting pole test elsewhere in the suite would have caught.
- 2026-09-02: T6 — final `--as-cran` on the built tarball, PDF manual included, on an idle machine: **tests 17 s, vignette re-build 5 s, PDF manual 2 s, total 1 min 27 s, Status OK** against the 259 s / 40 s / 6 min 14 s baseline. `devtools::check(manual = TRUE)` Status OK (0/0/0); `devtools::document()` no diff and no unresolved-link warning. Vignette parity re-run after the T5 roxygen changes: all seven still match their sources. `cran-comments.md` now states the reduction and that nothing was deleted. Note on measurement: an intermediate run of the same tarball read tests 35 s and total 2 min 15 s while another R session was running `devtools::check()` on this machine — AC1's figures are only meaningful on an unloaded machine, and the recorded ones were taken with no other R process running.
- 2026-09-02: **blocked on T7** — the win-builder R-devel run is Jeff's to make (`devtools::check_win_devel()`); its results reach his address alone, so AC2 cannot be evidenced from this session. Everything else is done and committed; resume to record the four line items and the overall time once the email arrives.
- 2026-09-02: T7 — win-builder R-devel run of the branch tarball, log read in full (R-devel r90457 ucrt, x86_64-w64-mingw32, Windows Server 2022, started 2026-09-02 15:19:37 UTC): `Status: OK`, no NOTE/WARNING/ERROR. Timed steps: incoming feasibility [12s], R code for possible problems [25s], examples [27s], tests [68s] (`testthat.R` [68s]), re-building of vignette outputs [11s], PDF manual [15s] = 158 s, against 1,165 s for the same six steps on the 2026-09-01 pre-branch run (tests 15 min -> 68 s, vignettes 146 s -> 11 s, examples 41 s -> 27 s). The install step (`checking whether package 'circumplex' can be installed ... OK`) again carries no timing, so the three AC2 line items that are timed are recorded and the fourth is recorded as untimed — the third Windows log in a row to omit it. The log states no overall check time; whether AC2's overall figure can be evidenced at all is the open question below.
- 2026-09-02: T7 done — overall check time settled from win-builder's own results listing (https://win-builder.r-project.org/0SBISo37uvQ9), read 2026-09-02: `00check.log` last modified 02.09.2026 17:23, i.e. 15:23 UTC on German summer time, against the log's own `current time: 2026-09-02 15:19:37 UTC` start stamp — an elapsed **3 min 23 s to 4 min 23 s** at the listing's minute resolution, under AC2's 8 minutes against the 24 min CRAN measured on its pretest machine. The reading cross-checks against the log's contents: 158 s of that elapsed is the six timed steps, leaving 45-105 s untimed, and no offset other than UTC+2 gives an elapsed consistent with a 158 s timed sum (UTC+1 would require a 64-minute untimed install). AC2's four line items: tests 68 s, vignette re-build 11 s, examples 27 s, install untimed in the log but bounded at 105 s by that residual. Bears on the ROADMAP's Windows-compile-cost candidate row: its promotion condition — the install step alone eating the budget — is not met, since the whole untimed residual is at most 105 s.
- 2026-09-02: all seven tasks done; `devtools::test()` off CRAN 0 failures / 9242 passes / 1 skip / 9 warnings (the pre-existing zero-variance and lavaan-marker warnings). Status -> review.
- 2026-09-02: review gate FAILED and returned M120 to `in-progress`. What failed: the `vignette-precompute` workflow this milestone shipped is red on PR #151 — its byte-exact `git diff --exit-code -- 'vignettes/*.Rmd'` cannot pass, because `evaluating-circumplex-structure.Rmd` ships `ssm_ci_accuracy()` output that is not reproducible across machines (coverage 0.891 vs 0.896, N 147 vs 144, certification rate 0.735 vs 0.720 on the CI runner), on top of the wall-clock `Elapsed:` line and the machine-dependent condition numbers. All six acceptance criteria were executed with fresh evidence and met (Review section); the universal and toolchain consistency-gate checks all pass. Four further confirmed defects to fix with it: relative-`tolerance` assertions in `test-plot-cran-guards.R` and `test-pole-values.R`, a top-level `on.exit` in `tools/m120-planted-defects.R` that never fires, `fig.cap = ""` blanking every shipped figure's alt text, and `xml2` missing from the AC5 instrument's dependency surface. First defect return for this milestone.
- 2026-09-02: T8 — review F1 and F6 repaired. F1: the `vignette-precompute` guard is now `tools/check-vignette-staleness.R`, not `git diff --exit-code`. A vignette source may declare a `<!-- precompute:volatile-numbers start -- <reason> -->` region; inside one, numeric literals in knitr output lines (`^#>`) are replaced by `<n>` on both sides before comparing, and everything else stays byte-exact — all prose, all chunk source, every other vignette in full, and inside the region the output's line count, line positions and every non-numeric word. One region is declared, around `evaluating-circumplex-structure`'s `accuracy_run` chunk, which is where every difference in the failing CI run sat. Measured on the branch: the guard reads all seven up to date (162 output lines compared without their digits); it goes STALE on a prose word edited outside the region, on a word edited inside it, on an output line deleted inside it, and on a digit changed in an unmasked vignette; it stays green on a digit changed inside the region, which is the declared blind spot. It errors on a marker with no reason, a region never closed, and a region masking no output line. F6: `tools/precompute-vignettes.R` now sets `fig.alt` through an `opts_hooks` entry that fires only where its own empty caption is in force, so the shipped figures carry `alt="plot of chunk <label>"` again instead of `alt=""` — 44 `<img>` tags across six vignettes changed on the re-render, with no `<div class="figure">` caption returning.
- 2026-09-02: T9 — review F2 and F4 repaired. The three relative-`tolerance` assertions are now absolute. `test-pole-values.R`'s two SEM pole blocks assert `abs(d_est - truth) < 1e-3` degrees and `abs(a_est - truth) < 1e-4`, where `tolerance = 1e-2` and `1e-1` on a displacement near 350 had admitted +/-3.5 and +/-35 degrees; the population fit reproduces the closed form to 4.2e-07 degrees at both entry points, measured here, so the new bound keeps three orders of margin. `test-plot-cran-guards.R`'s curve block no longer compares sorted peaks at `tolerance = 5`: it asserts, per profile, that the curve's maximum sits within half a grid step of the grid point nearest that profile's displacement. Measuring the old assertion found it was covering a real 15-degree gap — the curve is drawn over [45, 360], so the profile built at 30 peaks at the window's edge, a property of the drawn range and not of the estimate; the new form states that instead of tolerating it, and stays linear rather than angular so a displacement left on atan2's branch (-10 for 350) still fails. All four designations these blocks carry pass unpatched on CRAN and go red under every defect they must catch (`--probe`: ssm_sem and ssm_sem_parameters red under sem-magnitude and sem-wrap, ssm_plot_circle and ssm_plot_curve red under ssm-wrap).
- 2026-09-02: T10 — review F3 repaired. `tools/m120-planted-defects.R`'s plant-run-restore loop now lives in `run_defects()`, whose `on.exit()` restores the patched files and rebuilds a clean package however the run leaves; the top-level `on.exit()` it replaces never fired at all. Measured both ways with a `stop()` injected between `apply_patch()` and the loop's `restore()`: the old form left `R/ssm_sem.R` patched in the working tree, the new form leaves `git status` over `R/` and `src/` empty. The refactored script still runs its probe unchanged (all four rows pass unpatched and redden under their defects) and leaves the tree clean.
- 2026-09-02: T11 — review F7 repaired, and the F6 blindness beside it. `tools/m120-vignette-parity.R` no longer uses `xml2` (which sits in no dependency surface this repo installs): xml2 was decoding HTML entities, both sides come from the same pandoc, so a six-entity base-R decoder replaces it and the instrument now needs only what the vignettes need. All seven still match their sources with it (1114 / 579 / 386 / 711 / 389 / 349 / 452 source lines), and planting a word into a shipped heading still reddens it. Figures stay collapsed to a placeholder in the comparison, for a reason measured here: the two sides legitimately differ on alt text, since the shipped copy is knitted with T8's `fig.alt` hook while a plain render of the `.Rmd.orig` emits an `<img>` carrying no `alt` attribute at all — so F6's premise that a live build produced `alt="plot of chunk <label>"` holds for the intermediate markdown, not for rendered HTML. The alt text is instead asserted directly: the instrument now requires every `<img>` in a shipped vignette to carry a non-empty `alt`, and setting one to `alt=""` reddens it.
- 2026-09-02: T8 correction, from CI rather than from this machine: the first push of the repaired guard still read `evaluating-circumplex-structure` STALE on the runner, on one line whose digits were already masked — `print()` right-aligns a column to its widest entry, so a count one digit shorter shifts the whole column and the padding survives a digit mask. Inside a marked region the guard now collapses runs of blank space in output lines along with their digits. Re-proved after the change: a column re-widened by different digit counts reads up to date, while a word changed inside the region and a prose word changed outside it both read STALE.
- 2026-09-02: defect return closed; status -> review. The `vignette-precompute` workflow is **green on CI** at `db1c227f` — the run that failed the gate now passes on the runner where the numbers actually drift, and its log shows the guard doing the work (`evaluating-circumplex-structure` up to date with 1 masked region over 162 output lines, the other six byte-exact there, which also answers the question the mini gate left open about whether any other vignette drifts across machines). Re-derived after the five repairs: full suite off CRAN 0 failures / 9244 passes / 1 skip / 9 warnings (the pre-existing zero-variance and lavaan-marker ones); AC3's instrument 412 newly skipped blocks, all run off CRAN, none skipped, none failed; AC4's two instruments exit 0 with all 33 designated blocks passing on CRAN unpatched and reddening under every defect they must catch; AC5's parity instrument all seven matching plus the new alt-text assertion; `devtools::check(manual = TRUE)` Status OK, 0 errors / 0 warnings / 0 notes. AC1 re-measured on a tarball built from the repaired branch with the machine otherwise idle: **tests 23 s** (budget 25), **vignette re-build 6 s** (budget 10), **total 1 min 58 s** (budget 2 min), Status OK. That total is 2 s inside its budget where the review run read 1 min 46 s and T6 read 1 min 27 s on the same budget — run-to-run variation on this machine, not a change in the package, but AC1's total clause now has little room and a review re-derivation could land either side of it.

## Decisions

## Review

Evidence gathered 2026-09-02 at review, on the branch tip (`0f9cfba5`), by
command. The two tracking-only commits after T6 leave the package content of
the tarball identical to the one T6 and T7 measured.

- **AC1 — met.** Tarball built from the branch tip with `R CMD build`; `R CMD
  check --as-cran` run with `NOT_CRAN` unset and `_R_CHECK_TIMINGS_=0` on the
  maintainer's macOS machine with no other R process running. Step timings:
  **tests 23 s** (budget 25 s), **re-building of vignette outputs 6 s** (budget
  10 s), **wall-clock total 106 s = 1 min 46 s** (budget 2 min), `Status: OK`,
  no NOTE/WARNING/ERROR. Against the 2026-09-01 baseline of 259 s / 40 s /
  6 min 14 s. Other steps: install 10 s, examples 10 s, examples
  --run-donttest 13 s, PDF manual 3 s. A first run of the same tarball read the
  same tests (23 s) and vignette (6 s) figures.
- **AC2 — met.** The win-builder R-devel run of the branch tarball is Jeff's to
  make; its artifacts were re-read this session from
  `https://win-builder.r-project.org/0SBISo37uvQ9`, so the figures below are
  read off the live log rather than recalled. Log: R Under development
  (2026-08-31 r90457 ucrt), x86_64-w64-mingw32, `current time: 2026-09-02
  15:19:37 UTC`, `Status: OK`, and NOTE / WARNING / ERROR appear nowhere.
  **Overall check time 3 min 23 s - 4 min 23 s**, under the 8-minute target and
  against the 24 min CRAN measured on its pretest machine: the directory
  listing gives `00check.log` last modified 02.09.2026 17:23 (German summer
  time, UTC+2 = 15:23 UTC) against the 15:19:37 UTC start, at the listing's
  minute resolution. Four line items: **tests 68 s**, **re-building of vignette
  outputs 11 s**, **examples 27 s**, **install untimed in the log** — the third
  Windows log in a row to omit a timing on the install step — and bounded above
  by the 45-105 s of elapsed time the six timed steps (158 s total) do not
  account for.
- **AC3 — met.** `Rscript tools/m120-skipped-blocks-live.R` (exit 0). It derives
  the newly CRAN-skipped set from the branch diff over `tests/testthat/` rather
  than a typed list, and refuses an empty set: **412 blocks**. Run with
  `NOT_CRAN=true`, each of the 412 is named with its own result and **none
  skipped, none failed**; the whole suite off CRAN reports 1026 blocks, 0
  failed, 0 errored, 1 skipped (a pre-existing skip outside the newly skipped
  set).
- **AC4 — met.** Two instruments, run in order, both exit 0.
  - Domain (`Rscript tools/m120-cran-coverage.R`): the union of the 412 newly
    CRAN-skipped blocks and the 424 blocks this branch touched yields **32
    exported functions**, written to `tools/m120-domain.txt`; the script
    refuses an empty domain and also refuses to pass if any of them loses all
    CRAN-mode coverage. 554 blocks remain live on CRAN. Six exports are called
    by no test block at all and are reported separately as not this milestone's
    doing (`CoordCircumplex`, `GeomSsmArc`, `GeomSsmPath`, `GeomSsmPoint`,
    `ggsave`, `html_render`).
  - (i), (ii), (iii) and (iv) (`Rscript tools/m120-planted-defects.R`): the
    script reads `tools/m120-domain.txt`, requires all 32 domain functions to
    carry a row with a disposition in `tools/m120-designations.csv`, rejects a
    stray row outside the domain, requires a magnitude defect on every
    `satisfied` row of the `ssm_*` / `cpm_*` / `axes_*` families, and requires a
    wrap defect for each of AC4(iii)'s six. Result: **all 33 designated blocks
    pass on CRAN unpatched** (control run, `NOT_CRAN` unset — a block that
    skipped there would be rejected as covering nothing) **and all 22
    block-defect pairs go red** under the ten planted defects: `ssm-magnitude`,
    `ssm-wrap`, `sem-magnitude`, `sem-wrap`, `semweights-magnitude`,
    `cpm-magnitude`, `cpm-pole`, `coverage-magnitude`, `unwrap-wrap`,
    `axes-magnitude`. `R/` and `src/` restored clean afterwards.
- **AC5 — met**, over both halves of the enumerated set.
  - Vignettes (7, the branch diff over `vignettes/`): `Rscript
    tools/m120-vignette-parity.R` (exit 0) against the tarball-installed
    package. For each of the seven it renders the live `.Rmd.orig` source and
    the shipped pre-computed `.Rmd`, strips both to visible text, and compares:
    **all seven match** (1114 / 579 / 386 / 711 / 389 / 349 / 452 source
    lines). A rendering error on either side is a failure there, not a skip, so
    the run also evidences that every source and every shipped vignette renders
    without error; the `--as-cran` run's `re-building of vignette outputs` step
    is OK. The only normalized line is `ssm_ci_accuracy()`'s printed wall-clock
    `Elapsed:`.
  - Examples (7 Rd files, the branch diff over `man/`, from the `R/` roxygen it
    is generated from): all 15 changed calls carry `boots = 200`
    (`ssm_analyze` 8, `ssm_table` 2, `ssm_plot_circle` / `ssm_plot_curve` /
    `ssm_plot_contrast` / `ssm_plot_trajectory` / `ssm_analyze_long` 1 each),
    and every one of the seven `\examples` blocks opens with the line saying the
    count is lowered for speed and that a reported analysis should use the
    default — so no surviving prose claims the default resample count for a
    call that no longer uses it. They render without error: the `--as-cran`
    `examples` step is OK at 10 s and `examples with --run-donttest` OK at 13 s.
- **AC6 — met.** `Rscript -e 'devtools::check(manual = TRUE)'` on the branch
  tip: **0 errors / 0 warnings / 0 notes**, `Status: OK`, duration 12 min 44 s
  (this is the full check with the PDF manual, not the check-time budget AC1
  measures). `Rscript -e 'options(cli.width = 500); devtools::document()'`
  emitted no line matching `resolve link` and left `man/` and `NAMESPACE`
  with an empty `git status` — no diff.

### Consistency gate

- `cairn_validate.py` exit 0, all checks PASS; 47 advisory WARNs, every one a
  multi-line work-log entry in M7's pre-existing log, none from this milestone.
  The `release window` advisory did not fire.
- No `DESIGN.md` principle changed (the diff does not touch it), so
  `cairn_impact.py --changed` does not apply.
- Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
  `document()` no diff and no unresolved-link warning (above);
  no generated file hand-edited; `devtools::build_readme()` leaves `README.md`
  unchanged; `pkgdown::check_pkgdown()` "No problems found";
  `.Rbuildignore` covers the one new top-level pattern
  (`^vignettes/.*\.Rmd\.orig$`) and the check reports no NOTE;
  `devtools::check()` clean (above); master watches — the newest push run on
  `master` reaching a verdict is 2026-09-02T04:58:32Z, `success` on both
  `R-CMD-check.yaml` and `test-coverage.yaml`;
  `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`,
  `tools/check-branch-protection.R` and `tools/check-ci-deps.R` all exit clean.
  NEWS.md: no entry written, because the milestone changes no exported
  behavior — the example calls and the vignette build change, not what any
  function does.

**Gate result: FAILED on the branch's own CI.** `gh pr checks 151` reports
`vignette-precompute` **fail** (21m8s) — the workflow this milestone shipped.
`pkgdown` and `matrix` pass; the three `R-CMD-check` platform jobs were still
pending when the gate was called.

### Independent review

Surface tier user-facing and the diff touches executable surface, so the full
three-lens fan-out ran, each lens fresh-context with a distinct evidence base.
[O] diff-bug returned 16 ranked findings; [S] blame-history returned 4; [S]
prior-review-record returned none — it found no archived `## Review` finding
this diff walks back, and its GitHub probe
(`gh api repos/jmgirard/circumplex/pulls/comments?per_page=1`) returned `[]`,
so no repo-wide inline-review surface exists to walk.

Two lens claims were checked before triage and hold: [O]'s reading that the
`vignette-precompute` guard cannot pass, and its reading of `tolerance` in
testthat edition 3.

**Findings, and their disposition.**

- **F1 (return-forcing; [O] rank 1, CONFIRMED by the CI run).** The
  `vignette-precompute` staleness guard cannot go green.
  `.github/workflows/vignette-precompute.yaml` compares the re-rendered
  vignettes byte-exactly (`git diff --exit-code -- 'vignettes/*.Rmd'`), but
  `vignettes/evaluating-circumplex-structure.Rmd` carries output that is not
  reproducible across machines. The CI failure log shows the drift is not only
  the wall-clock `Elapsed:` line [O] named: `ssm_ci_accuracy()`'s simulation
  numbers themselves move (coverage `0.891` -> `0.896`, N `147` -> `144`,
  certification rate `0.735` -> `0.720`), and the printed condition numbers
  (`1.83e+14`, `9.24e+16`) are machine-dependent too.
  `tools/m120-vignette-parity.R` normalizes only `Elapsed:`, so AC5's
  instrument does not see this; the guard does, on every run. This is the sole
  mechanism stopping a shipped `.Rmd` drifting from its `.Rmd.orig` — the
  falsifier the T2 plan-gate entry named for choosing pre-computation.
- **F2 (fix on the branch; [O] rank 2, CONFIRMED).**
  `tests/testthat/test-plot-cran-guards.R:91` — `expect_equal(sort(peaks),
  sort(d_est), tolerance = 5)`. The package is testthat edition 3, where
  `tolerance` is relative: a 10x difference passes at `tolerance = 5`
  (verified directly). The block reddens under `ssm-wrap` only because the
  expected side moves too; as the value assertion its comment claims, it is
  near-vacuous.
- **F3 (fix on the branch; [O] rank 3, CONFIRMED).**
  `tools/m120-planted-defects.R`'s `on.exit(restore(patched_files))` sits at
  script top level, where it never fires — verified with a minimal `Rscript`
  that `stop()`s after a top-level `on.exit`. The script's header asserts the
  opposite. A `stop()` between `apply_patch()` and the loop's `restore()`
  therefore leaves a planted defect in `R/` or `src/`; the next run's
  dirty-tree refusal catches it, so it is detectable, not silent.
- **F4 ([O] rank 6, same mechanism as F2; fix on the branch).**
  `tests/testthat/test-pole-values.R:139` uses `tolerance = 1e-1` on a `d_est`
  near 350 (relative, so about +/-35 degrees) and line 100 uses `1e-2`
  (+/-3.5 degrees). Both still redden under `sem-wrap`, which moves the value
  by 360, but a 30-degree estimator error would pass.
- **F5 ([O] ranks 4, 5, 10, 11; follow-up).** Instrument strength:
  `m120-planted-defects.R` gates its magnitude requirement on the CSV's own
  `disposition` label without checking that `renderer` names exactly AC4(ii)'s
  five, so relabelling a row to `covered` drops its defect silently; both
  instruments detect a skip only by the literal `^\s*skip_on_cran\(\)\s*$`
  line, so `vdiffr`, `expect_snapshot`, `skip_if_not_installed()` and
  `skip_on_os()` skips are invisible and the "554 live on CRAN" figure
  over-counts; coverage attribution counts textual mentions, not reached
  calls. The 33 designated blocks each get a real CRAN-mode control run, so
  AC4's own claims stand; the looser credit is for the 15 `covered` rows.
- **F6 ([O] rank 7; fix on the branch).**
  `tools/precompute-vignettes.R:34` sets `fig.cap = ""` globally, so every
  shipped vignette figure renders as `alt=""` where a live build produced
  `alt="plot of chunk <label>"` — an accessibility regression in shipped
  documentation. `tools/m120-vignette-parity.R:22` collapses `<img ...>` to a
  placeholder, so AC5's instrument is structurally blind to it.
- **F7 ([O] rank 12; fix on the branch).** `tools/m120-vignette-parity.R`
  needs `xml2`, which is in neither `DESCRIPTION` `Suggests` nor the
  `vignette-precompute` allowlist (confirmed), so the AC5 instrument does not
  run on a clean checkout. It is used elsewhere in CI only by
  `test-coverage.yaml`.
- **F8 ([O] ranks 8, 9, 13, 14, 15, 16; follow-up or rejected).** A ggplot2
  lifecycle warning is baked into `introduction-to-ssm-analysis.Rmd` as
  standalone output (bears on F1's nondeterminism, since it prints once per
  session); `exact_cov_sample()` reseeds the global RNG as a side effect;
  `vignettes/figures/` adds 1.3 MB of PNGs with no size figure recorded
  against CRAN's thresholds (AC6's clean check is the only evidence);
  the guard ignores figures by design, so a plot-only change ships a stale
  PNG silently; the occasions boundary battery is now off-CRAN with the wide
  equivalents still live (a narrowing, not a hole); the two vignette-guard
  readers now read different files.
- **F9 ([S] blame ranks 1-2; follow-up).** Two shipped-*data* sweeps left
  CRAN: `test-norms-provenance.R`'s "shipped angles follow the LM = 360
  convention" (added by M72 against the pole-canonicalization mistake the
  0/360 convention exists to prevent) and `test-norms-anchor-range.R`'s "no
  shipped norm sample's mean falls outside its instrument's anchors" (the
  D-040 hotfix's domain-wide sweep, count-pinned by M112). Both sit outside
  AC4's domain, which reaches computation paths rather than static data
  audits; the narrower `norm_standardize()` refusal test stays live on CRAN.
  Neither is an AC4 failure — AC4 is about functions losing coverage, and both
  functions keep a live designated block.
- **[S] blame ranks 3-4, and the whole [S] prior-review lens: no finding.**
  The `axes-fiml` / `axes-reliability` sweeps left the value-pinning blocks
  those decisions rest on live; the `norms-audit-*` skips are redundant with
  an existing `file.exists()` gate on a `.Rbuildignore`d dev script.

### Disposition

**Status returned to `in-progress`.** F1 is a confirmed defect in something
this milestone shipped, it is what makes the branch's CI red, and repairing it
is design work rather than a review-side patch: the
`evaluating-circumplex-structure` vignette's `ssm_ci_accuracy()` output has to
become reproducible across machines, or be excluded from the byte-exact
comparison, or the guard has to compare something other than bytes. F2, F3,
F4, F6 and F7 are the fix-now list to carry back with it. No acceptance
criterion failed: AC1-AC6 are all met with the evidence above, and their ticks
stand.
