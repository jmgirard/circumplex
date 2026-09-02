# M120: Bring the Windows CRAN check under 8 minutes

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Branch/PR:** —

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

- [ ] AC1 — On the tarball built from the branch tip, `R CMD check --as-cran`
      with `NOT_CRAN` unset on the maintainer's macOS machine reports a tests
      step of at most 25 s, a vignette re-build step of at most 10 s, and a
      total under 2 min, against the 259 s / 40 s / 6 min 14 s baseline
      measured there 2026-09-01.
- [ ] AC2 — A win-builder R-devel run of that tarball reports an overall check
      time under 8 minutes, with its tests, vignette re-build, examples and
      install line items recorded.
- [ ] AC3 — Every test block this milestone newly skips on CRAN still runs off
      CRAN: for the blocks enumerated by the branch diff over
      `tests/testthat/`, a `NOT_CRAN=true` per-block run names each one with a
      result that is neither skip nor failure.
- [ ] AC4 — For each exported function whose CRAN-mode coverage this milestone
      reduces — the set enumerated by the union of the CRAN-mode
      skipped-block-set diff (before vs after) and the branch diff over
      `tests/testthat/` — at least one block still runs on CRAN exercising that
      function; and for each such function in the `ssm_*`, `cpm_*` and `axes_*`
      families, that block is shown red under two planted defects on the path
      it covers: a magnitude error in a returned estimate, and — where the
      output has an angular component — a wrap error at 0°/360° or ±180°.
- [ ] AC5 — Every vignette chunk and documented example whose computation this
      milestone precomputes or cheapens — the set enumerated by the branch diff
      over `vignettes/`, `man/`, and any `R/` default or helper feeding a
      documented chunk — renders without error, and no mismatch survives
      between the surrounding prose and the rendered output.
- [ ] AC6 — `Rscript -e 'devtools::check(manual = TRUE)'` on the branch tip is
      0 errors / 0 warnings / 0 notes, and `devtools::document()` produces no
      diff.

## Coverage

- AC1 → T2, T3, T5, T6
- AC2 → T7
- AC3 → T3
- AC4 → T4
- AC5 → T2, T5
- AC6 → T6

## Tasks

- [ ] **T1** — Re-derive the baseline on the branch and record the procedure:
      per-file CRAN-mode `ListReporter` timings and the `--as-cran` step
      timings, both from the built tarball with `NOT_CRAN` unset.
- [ ] **T2** — Precompute the vignettes: `.Rmd.orig` + render script,
      `.Rbuildignore` the sources, commit rendered `.Rmd` and figures, add the
      CI rebuild job. Heaviest first — `evaluating-circumplex-structure.Rmd`,
      `sem-based-ssm-analysis.Rmd`, `advanced-visualization.Rmd`,
      `intermediate-ssm-analysis.Rmd` (6 uncapped default-2000 calls),
      `introduction-to-ssm-analysis.Rmd` (3), `growth-ssm-analysis.Rmd`,
      `axes-reliability.Rmd`.
- [ ] **T3** — `skip_on_cran()` top-down by measured cost until AC1's tests
      budget is met: `test-axes-fiml.R` (91.8 s), `test-axes-reliability.R`
      (21.0), `test-cpm_api.R` (14.0), `test-ssm_sem_groups.R` (10.9),
      `test-cpm_boundary_vignette.R` (10.4), `test-axes-scaled-fit.R` (9.5),
      `test-ssm_plot.R` (8.9), `test-axes-corrected-se.R` (8.2),
      `test-suff_stats.R` (8.2), `test-geom_ssm_path.R` (6.5), then the tail.
- [ ] **T4** — Live-coverage retention: enumerate the functions losing CRAN
      coverage per AC4, keep or author one live CRAN-mode block each, plant the
      two defect forms per statistical function, record each red.
- [ ] **T5** — Cheapen the default-2000 examples in roxygen, then
      `devtools::document()`: `ssm_analyze` (3 un-gated calls + 5 under
      `\donttest`), `ssm_plot_circle`, `ssm_table`, `ssm_plot_curve`,
      `ssm_plot_contrast`, `ssm_plot_trajectory`, `ssm_analyze_long`.
- [ ] **T6** — Re-measure locally (AC1); `devtools::check(manual = TRUE)`;
      refresh `cran-comments.md` where its figures move.
- [ ] **T7** — win-builder R-devel run on the branch tarball; record the
      overall time and the four line items (Jeff runs it; results reach only
      his address).

## Work log

- 2026-09-01: created by /milestone-plan after CRAN's pretest NOTE. Baseline measured here: local `--as-cran` total 6 min 14 s (install 11 s, examples 12 s, examples --run-donttest 27 s, tests 259 s, vignettes 40 s); CRAN-mode per-file test total 234 s, 84 skips of 1016 blocks. Windows/Mac factor 3.5x from the 2026-09-01 win-builder tests and vignette line items; its examples line ran 1.5x, so the factor is asserted only for tests and vignettes. **Assumption:** 328 s of the Windows 24 min is unattributed by the four recorded line items and is taken to be install/compile, which this milestone does not touch; the full win-builder log's install line replaces this estimate when available, and AC1's budget moves with it.
- 2026-09-01: plan gate chose precomputing the vignettes over lowering their bootstrap counts, because capping saves roughly half of 146 s where precomputing takes the step to near zero; falsified by a rendered vignette breaking in a way the CI rebuild job fails to catch.
- 2026-09-01: plan gate chose `skip_on_cran()` over shrinking the SEM fixtures, because retuning tolerances on lavaan fits is statistical work that can quietly stop catching regressions; falsified by a regression reaching CRAN that a shrunk-but-live block would have caught.
- 2026-09-01: plan gate chose an 8-minute target over just-under-10 (Jeff's call), for margin against a slower or busier CRAN machine; falsified by the 8-minute target proving unreachable without cutting into `src/`.
- 2026-09-01: criteria audit ran in **full** mode ([O], fresh context). It returned: AC1's original targets were arithmetically insufficient (they reached ~11.8 min, not 10) — targets re-derived from the 600 s budget and the residual recorded as an assumption; the baseline date was wrong; AC3's zero-failure count did not show the named blocks ran — now per-block; AC4's skip-set diff enumerated a proxy, missing blocks made cheaper rather than skipped — now a union with the branch diff; AC4's single planted defect stood one exemplar in for a family — now two forms, scoped to the statistical families for proportionality; AC5 was satisfied by the act of recording a re-read — now by no mismatch surviving. All fixed at the gate.
- 2026-09-01: `cairn_validate`'s release-window advisory fires on this milestone's CRAN-shaped title; dispositioned — M120 ships no version and never submits (the resubmission stays M7's T4), so it stays `planned` rather than parked as `blocked`.
- 2026-09-01: Jeff's win-builder log read in full (https://win-builder.r-project.org/YRV8eN2j66TA/00check.log): every timed step is `incoming feasibility [14s]`, `R code for possible problems [39s]`, `examples [41s]`, `tests [15m]`, `re-building of vignette outputs [146s]`, `PDF manual [25s]` — 1,165 s of the 1,440 s total, so the unattributed residual narrows from 328 s to 275 s. The install step (`checking whether package 'circumplex' can be installed ... OK`) carries no timing there, so the compile-cost assumption above stands unmeasured; CRAN's pretest `00check.log` is still the artifact that would settle it. That run's Status was OK with no notes — CRAN's incoming pretest applies the 10-minute checktime rule that win-builder does not.
- 2026-09-01: CRAN's pretest Windows log read in full (incoming_pretest/circumplex_2.0.0_20260902_050533/Windows/00check.log): `Status: OK`, and the strings NOTE, WARNING, ERROR and checktime appear nowhere in it — so the pretest table's `Check: *, Result: NA` row is a summary-layer artifact, not a second defect, and the checktime NOTE is raised by CRAN's summary layer rather than by `R CMD check`. Timed steps there: feasibility [15s], R code for possible problems [44s], examples [45s], tests [16m], vignette re-build [145s], PDF manual [25s] = 1,234 s of the 1,440 s, narrowing the unattributed residual to 206 s; the install step again carries no timing. AC1's targets are unchanged and now carry more margin: fixed cost ~335 s leaves ~145 s of Windows headroom for tests plus vignettes against the 8-minute target.

## Decisions

## Review
