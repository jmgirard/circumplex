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
- 2026-09-02: second review pass PASSED the gate — all six criteria met with fresh evidence, `cairn_validate` exit 0, branch CI green. Jeff approved the merge at the step-7 chip, choosing fix-G1-G3-then-merge and candidate rows for G4 and G5+G6. The three fixes are committed and pushed (`c1d4faa9`). **Merge not yet made:** `gh pr checks 151 --watch` exceeded the harness ceiling, so the watcher was stopped rather than left armed at the merge. At the stop, `vignette-precompute`, `matrix` and `pkgdown` all pass on the fixed branch and the three `R-CMD-check` platform jobs are pending. Resume: confirm green, write `cairn/.merge-approved`, squash-merge PR #151, then the step-9 hygiene pass (the two candidate rows are owed there).

## Decisions

## Review

Second review pass, after the first pass's defect return (F1–F7). Evidence
gathered 2026-09-02 at the branch tip (`321cf648`), by command, all of it
re-derived this session — nothing carried over from the first pass. The `R/`
diff is roxygen comments only and `src/` is untouched, so no runtime behavior
changes; the diff moves documentation, tests, tooling and CI.

- **AC1 — met.** Tarball built from the branch tip with `R CMD build`;
  `R CMD check --as-cran` with `NOT_CRAN` unset and `_R_CHECK_TIMINGS_=0` on
  the maintainer's macOS machine. Two runs on an **idle** machine: **tests 23 s
  and 24 s** (budget 25 s), **vignette re-build 6 s and 6 s** (budget 10 s),
  **wall-clock total 107 s and 115 s** (budget 120 s), `Status: OK` both times.
  Against the 2026-09-01 baseline of 259 s / 40 s / 6 min 14 s. Other steps
  (run 4): install 12 s, examples 11 s, examples --run-donttest 14 s, PDF
  manual 4 s. **Disclosed:** two earlier runs of the same tarball read tests
  28 s and 24 s while another project's `R CMD check` (`tidymedia`) was running
  concurrently on this machine — over budget on the first. AC1's protocol is
  the unloaded machine (the T6 work-log note), so the two idle runs are the
  recorded evidence and the contaminated pair is recorded as contaminated. The
  margin is genuinely thin: the tests clause has 1–2 s of headroom and the
  total clause 5–13 s.
- **AC2 — met.** Win-builder R-devel is Jeff's to run; the figures are read off
  the live artifacts at `https://win-builder.r-project.org/0SBISo37uvQ9`, not
  recalled. R Under development (2026-08-31 r90457 ucrt), x86_64-w64-mingw32,
  `current time: 2026-09-02 15:19:37 UTC`, `Status: OK`, no NOTE / WARNING /
  ERROR. **Overall check time 3 min 23 s – 4 min 23 s**, under the 8-minute
  target and against the 24 min CRAN measured on its pretest machine, derived
  from the listing's `00check.log` mtime (02.09.2026 17:23, UTC+2) against the
  log's own start stamp; no other UTC offset gives an elapsed consistent with
  the 158 s timed sum. Four line items: tests 68 s, vignette re-build 11 s,
  examples 27 s, install **untimed in the log** (the third Windows log in a row
  to omit it) and so bounded at 105 s by the untimed residual.
- **AC3 — met.** `Rscript tools/m120-skipped-blocks-live.R`, exit 0: the
  branch-diff-derived newly-CRAN-skipped set is **412 blocks**, and a
  `NOT_CRAN=true` per-block run names each one with a result that is neither
  skip nor failure — **all 412 ran, none skipped, none failed**. The instrument
  refuses an empty newly-skipped set.
- **AC4 — met**, over all four sub-clauses.
  - Domain (`Rscript tools/m120-cran-coverage.R`, exit 0): the union of the
    CRAN-mode skipped-block diff and the branch diff over `tests/testthat/`
    yields **32 exported functions**, written to `tools/m120-domain.txt`; the
    script refuses an empty domain and reports that **no exported function
    loses all of its CRAN-mode coverage**. Six exports are called by no test
    block at all and are reported separately as not this milestone's doing
    (`CoordCircumplex`, `GeomSsmArc`, `GeomSsmPath`, `GeomSsmPoint`, `ggsave`,
    `html_render`).
  - (i)–(iv) (`Rscript tools/m120-planted-defects.R`, exit 0): all 32 domain
    functions carry a row with a disposition in `tools/m120-designations.csv`;
    the control run (no defect, CRAN mode) has **all 33 designated blocks
    passing with no skip**; and **22 block-defect pairs go red** across the ten
    planted defects (`ssm-magnitude`, `ssm-wrap`, `sem-magnitude`, `sem-wrap`,
    `semweights-magnitude`, `cpm-magnitude`, `cpm-pole`, `coverage-magnitude`,
    `unwrap-wrap`, `axes-magnitude`). `git status` over `R/` and `src/` is
    empty afterwards — T10's restore-on-error repair holds.
- **AC5 — met**, over both halves of the enumerated set.
  - Vignettes (7, the branch diff over `vignettes/`): `Rscript
    tools/m120-vignette-parity.R`, exit 0, against the installed package. For
    each it renders the live `.Rmd.orig` and the shipped `.Rmd`, strips both to
    visible text, and compares: **all seven match** (1114 / 579 / 386 / 711 /
    389 / 349 / 452 source lines), and **every shipped figure carries alt
    text** (T11's assertion). A render error on either side is a failure, not a
    skip, so the run also evidences that all fourteen documents render.
  - Examples (7 Rd files, the branch diff over `man/`): the diff adds exactly
    **15 `boots = 200` calls**, and each of the seven touched Rd files carries
    the preamble line saying the count is lowered for speed and that a reported
    analysis should use the default — so no surviving prose claims the default
    resample count for a call that no longer uses it. They render: the
    `--as-cran` `examples` step is OK at 11 s and `--run-donttest` OK at 14 s.
- **AC6 — met.** `Rscript -e 'devtools::check(manual = TRUE)'` on the branch
  tip: **0 errors / 0 warnings / 0 notes**, `Status: OK`, duration 10 min 37 s.
  `Rscript -e 'options(cli.width = 500); devtools::document()'` emitted **zero**
  lines matching `resolve link` and left `man/` and `NAMESPACE` with an empty
  `git status` — no diff.

### Consistency gate

- `cairn_validate.py` **exit 0, every check PASS**. Advisories: M120's 11-task
  tripwire, and multi-line work-log entries in M7's pre-existing log. The
  `release window` advisory did **not** fire.
- `DESIGN.md` is not in the diff, so no principle changed and
  `cairn_impact.py --changed` does not apply.
- Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
  `document()` no diff and no unresolved-link warning (above); no generated
  file hand-edited; `devtools::build_readme()` leaves `README.md` and
  `man/figures/` unchanged; `pkgdown::check_pkgdown()` "No problems found";
  `.Rbuildignore` gains the one new top-level pattern
  (`^vignettes/.*\.Rmd\.orig$`) and the check raises no NOTE;
  `devtools::check(manual = TRUE)` clean (above); master watches — newest push
  run on `master` reaching a verdict is 2026-09-02T04:58:32Z, `success` on both
  `R-CMD-check.yaml` and `test-coverage.yaml`; `check-master-red-alert.R`,
  `master-red-alert-dryrun.R`, `check-branch-protection.R` and
  `check-ci-deps.R` all exit clean. NEWS.md: no entry, because the `R/` diff is
  roxygen only — no exported behavior changes.
- **Branch CI: fully green** on PR #151 at `321cf648` — `vignette-precompute`
  pass (12m2s), `matrix` pass, `pkgdown` pass, and macos / ubuntu / windows
  `R-CMD-check` all pass. The workflow that failed the first gate now passes on
  the runner where the numbers actually drift, and its log shows the guard
  working: `evaluating-circumplex-structure up to date, 1 masked region(s), 162
  output line(s) compared without their digits`, the other six byte-exact.

**Gate result: PASSED.**

### Independent review

Surface tier user-facing and the diff touches executable surface, so the full
three-lens fan-out ran, each fresh-context with a distinct evidence base. [O]
diff-bug returned **19** ranked findings; [S] blame-history **5**; [S]
prior-review-record **none** — it found no archived `## Review` finding this
diff walks back, and its GitHub probe returned `[]`, so no repo-wide inline
review surface exists to walk.

Three claims were verified against the implementation before triage, and all
three hold: the two `1.83e+14` condition numbers sit at lines 65 and 421 of the
shipped vignette, outside the declared volatile region (588–760); the staleness
guard compares the working tree to `git show HEAD:<path>` and sources its
`VIGNETTES` list from the render script; and `README.md`/`man/figures/` are
currently **in sync** with `README.Rmd` under `build_readme()`.

One claim the review called unmeasured was measured here: the source tarball
goes **5.61 MB on master → 6.21 MB on the branch** (+0.60 MB), installed size
5.2 Mb reported as INFO, and `--as-cran` raises no size NOTE. Master was
already above CRAN's 5 MB guidance; the branch worsens it by 11% without
crossing a new threshold.

**Findings and dispositions.**

- **G1 (fix now; [O] rank 1, and found independently at the gate).**
  `cran-comments.md:19-24` tells CRAN "1 minute 27 seconds in total, of which
  tests are 17 seconds and the vignette re-build 5 seconds" — T6's figures,
  measured before the T8–T11 repairs. This pass measures 1 min 47 s / 23 s /
  6 s. The headline number is 20 s off on the one dimension the milestone is
  about, in a document written to be read by CRAN.
- **G2 (fix now; [O] rank 4).** `vignettes/evaluating-circumplex-structure.Rmd`
  lines 65 and 421 ship `condition number 1.83e+14` **outside** the declared
  `precompute:volatile-numbers` region. The milestone's own F1 named that
  number as machine-dependent. A re-render on a different BLAS reports STALE
  for a non-defect — precisely the failure that returned this milestone. CI
  being green proves only that one ubuntu runner reproduces it, the same
  evidence base that already proved insufficient once.
- **G3 (fix now; [O] rank 19).** `tests/testthat/test-plot-cran-guards.R:32-37`
  defines `built_layers()`; nothing calls it.
- **G4 (follow-up candidate row; [O] ranks 2, 6, 7, 17 and [S] blame ranks
  1–4).** CRAN-mode boundary-invariant narrowing. Blocks a past milestone added
  against a specific, previously-fixed bug now skip on CRAN with no live
  equivalent that can see the same defect: `ssm_analyze_long`'s occasion order
  ("a T10/T2 pair must not flip"; its designated live block uses `T1`/`T2`,
  where the two orders coincide); the SEM group contrast near ±180° and the
  "reference group follows factor levels" flip; `test-ssm_trajectory.R`'s seam
  battery including its own teeth-check; `test-cpm_boundary.R`'s free-scaling
  pole recovery (D-009); and `angle_dist`'s exact-half-turn atom. **Not an AC4
  failure** — AC4's promise is one live block per *exported* function reddening
  under a magnitude or wrap defect, which is what was measured; these are
  invariants AC4 never quantified over, and the milestone's Scope plans the
  narrowing. CI runs all of them on every push.
- **G5 (follow-up candidate row; [O] ranks 3, 5, 8, 9, 12, 13, 14, 15, 18, and
  the first pass's F5 and F8, still open).** Instrument strength: the staleness
  guard shares its vignette list with the re-render, so dropping a name removes
  both in one edit, and it passes trivially if run without a preceding render;
  the volatile region masks every printed constant of the diagnostic (the 0.35
  certification rule, the RMSEA/SRMR line, the configuration echo), not only
  the simulated rates; both m120 instruments detect a skip only by a literal
  `skip_on_cran()` line, so `skip_if_not_installed("lavaan")` in
  `test-pole-values.R` makes two AC4(iii) designations silently conditional and
  the "live on CRAN" count over-reports; the domain is computed from `export()`
  only, so S3 method registrations are outside its reach; `m120-domain.txt` is
  a committed generated file nothing keeps fresh; the magnitude requirement is
  gated on the CSV's self-declared `disposition`; figures are compared by
  neither guard; `check_alt_text()` misses chunks that emit markdown images and
  is unreachable when any vignette mismatches.
- **G6 (follow-up; [O] rank 10).** `test-pole-values.R:29`'s
  `exact_cov_sample()` calls `set.seed()` inside a helper invoked from a test
  body, so later blocks in the process inherit the stream — against the
  discipline `test-ci_accuracy.R` pins.
- **G7 (recorded, no action; [O] rank 16).** Tarball weight, measured above:
  +0.60 MB, no new NOTE, master already over the 5 MB guidance.
- **G8 (rejected; [O] rank 11).** `man/figures/README-plot-1.png` regenerated
  inside a tracking commit whose message does not mention it. Out of the
  milestone's diff scope, and verified in sync: `build_readme()` on the branch
  tip leaves `README.md` and `man/figures/` unchanged, so the committed bytes
  are what the current toolchain produces.
- **[S] blame rank 5 and the whole [S] prior-review lens: no finding.** The
  un-skipped `test-ci_accuracy.R` oracle block is the AC4 designation working
  as designed; the prior-review lens found nothing this diff walks back.

**Return floor.** No finding demonstrates an acceptance criterion failing
inside its named procedure's domain, and none is a load-bearing defect in what
the package does for its users — the `R/` diff is roxygen only and `src/` is
untouched, so nothing users run changes behavior. G4 and G5 narrow what CRAN's
own check exercises, which is the milestone's planned trade and is bounded by
AC4 as written; the never-reinterpret rule keeps AC4 at its wording. No return.

### Fix-now work directed at the gate

Jeff triaged at the approval gate: fix G1–G3 on the branch, then merge; file
candidate rows for G4 and for G5+G6. G7 stands recorded, G8 rejected.

- **G1 fixed.** `cran-comments.md` now states 1 minute 47 seconds total, tests
  23 seconds, vignette re-build 6 seconds, and adds that win-builder R-devel
  completed the same tarball in under 5 minutes.
- **G2 fixed.** The `cpm` and `variants` chunks of
  `evaluating-circumplex-structure.Rmd.orig` now declare
  `precompute:volatile-numbers` regions, each naming the ill-conditioned fit as
  the reason. Re-rendered. The guard reads all seven up to date with **3 masked
  regions over 235 output lines** (was 1 region / 162). **Cost, recorded
  deliberately:** a region is chunk-granular, so the `variants` region also
  masks that chunk's fit-index table (`rmsea` / `srmr` / `cfi` / `tli`) — a
  drift in those digits would no longer redden the guard. This widens the
  blind spot G5 already names, and is the trade for closing the false-STALE
  mechanism that returned this milestone once.
- **G3 fixed.** The unused `built_layers()` helper is gone from
  `test-plot-cran-guards.R`; nothing referenced it.

Re-derived after the three fixes, on the branch tip:

- The re-render changed exactly **one** line of shipped content beyond the
  markers — `ssm_ci_accuracy()`'s wall-clock `Elapsed:` figure. Every printed
  number reproduced identically on this machine.
- Full suite off CRAN: **0 failures / 9244 passes / 1 skip / 9 warnings** (the
  pre-existing zero-variance and lavaan-marker ones).
- AC5's parity instrument, exit 0: all seven still match their sources
  (1114 / 579 / 386 / 711 / 389 / 349 / 452 source lines), every shipped figure
  still carries alt text.
- AC1 re-measured on a tarball built from the fixed branch: **tests 17 s**
  (budget 25), **vignette re-build 5 s** (budget 10), **total 94 s** (budget
  120), `Status: OK` — more headroom than the pre-fix runs, not less.
