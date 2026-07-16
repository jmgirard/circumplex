# M20: 0-vs-360 pole CI-endpoint alignment

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m20-pole-endpoint-alignment · https://github.com/jmgirard/circumplex/pull/45

## Goal

Make displacement-CI endpoints report the 0/360 pole as **360** (value-level,
in the stored object), matching the estimate path's LM=360 convention
(D-003) — closing D-003's parked cosmetic follow-up before v2.0.0 ships.

## Scope

**In:**
- `quantile.circumplex_radian` (`R/ssm_bootstrap.R:170-179`): replace the
  snap-to-0 at line 177 with a pole-adjacent snap to 2π, so an endpoint
  denoting the pole reports 360 (both the ~0 and ~2π float representations).
- Both consumers of that quantile machinery: the SSM bootstrap CI path and
  the CPM bootstrap angle-CI path (`R/cpm_fit.R:1222`).
- Consumer audit (M15 lesson): every reader of displacement-CI endpoints —
  `ssm_ci_accuracy()` arc-membership code (`R/ssm_ci_accuracy.R:888` ff),
  print/summary/plot surfaces, `lci > uci` straddle logic — verified
  unaffected or updated.
- CPM reported-angle pole alignment (2026-07-16 amendment): `theta_deg`
  (`R/cpm_fit.R:778`) reports the exact pole as 360 (degrees surface only;
  computational radians untouched), so a reference item at theory 360 stays
  internally consistent with its snapped CI.
- Mandatory boundary tests (CLAUDE.md): profiles peaking at 0°/360°, CIs
  straddling 0/360, flat profiles; guard teeth proven per the M13 recipe.
- `cairn/boundary-coverage.md` matrix update; NEWS.md entry (exported CI
  values change at the pole, measure-zero for real data).

**Out:**
- Estimate-path canonicalization — D-003 stands (estimate already reports 360).
- `quantile.circumplex_contrast_radian` — unwrapped branch, no pole snap; untouched.
- Analytic-CI Hessian recomputation — stays in the infra candidate row.

## Acceptance criteria

- [x] A CI endpoint denoting the 0/360 pole reports 360 (never 0) from
      `quantile.circumplex_radian`, exercised end-to-end through both
      `ssm_analyze()` bootstrap CIs and `cpm_fit()` bootstrap angle CIs, with
      regression tests at the mandatory boundary set (pole-peaking profile,
      pole-straddling CI, flat profile) that fail on the pre-change snap.
- [x] Consumer audit recorded in the work log: all `*_lci`/`*_uci` readers
      (incl. `ssm_ci_accuracy` arc membership and the straddle `lci > uci`
      convention) verified unaffected or updated with tests.
- [x] `cairn/boundary-coverage.md` updated for the new pole-endpoint cells.
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).
- [x] NEWS.md documents the exported change.
- [x] A CPM angle denoting the 0/360 pole reports 360 in the results table
      (`Angle` column), consistent with its snapped CI endpoints, with a
      regression test (reference item at theory 360) that fails pre-change.

## Coverage

- AC1 → T1, T2
- AC6 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T4
- AC5 → T4

## Tasks

- [x] **T1** — Write the red boundary regression tests first (SSM + CPM
      end-to-end pole cases; prove teeth by confirming they fail against the
      current snap-to-0).
- [x] **T2** — Implement the value-level snap in `quantile.circumplex_radian`
      (`R/ssm_bootstrap.R:177`): pole-adjacent endpoints → 2π; tests green.
      (+ CPM reported-angle pole alignment per the 2026-07-16 amendment.)
- [x] **T3** — Consumer audit: grep all displacement-CI endpoint readers
      (`ssm_ci_accuracy.R`, print/summary/plot, straddle logic); update any
      that assumed the 0 label, with tests.
- [x] **T4** — Update `cairn/boundary-coverage.md` + NEWS.md; run full
      `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the infra candidate
  row's pole-snap item; D-003's parked follow-up — completes, does not
  supersede, D-003). Value-level snap chosen over print-only at the plan gate.
- 2026-07-16: /milestone-implement started; branch m20-pole-endpoint-alignment.
- 2026-07-16: substantive amendment (gated): discovered the CPM estimate path
  wraps to [0, 360) — a reference item at theory 360 reports Angle = 0
  (deterministic, not measure-zero), contradicting the plan's "estimate already
  reports 360" premise (SSM-only). User approved extending scope: align the CPM
  reported angle at the exact pole to 360 (AC6 added).
- 2026-07-16: T1 done — red tests proven (6 pre-fix failures: primitive quantile
  pole cases + SSM deterministic pole-peaking e2e in test-ssm_bootstrap.R; CPM
  reference-at-360 e2e in test-cpm_angle_ci.R; no-over-fire straddle assertions
  included). Empirical finding: on this platform every pole-denoting input
  reaches the endpoint stage as exact 0 (R's %% clamps the fmod edge); the 2π
  side of the snap is defensive for other platforms.
- 2026-07-16: T2 done — snap to 2π in quantile.circumplex_radian (both pole
  representations, ~2-ulp tolerance) + CPM theta_deg pole → 360 (M20-D1;
  radians untouched). Old-label consumers updated with the change: 3 quantile
  unit expectations (test-ssm_bootstrap.R:1-25), the [0,360) domain pin
  (test-cpm_fit.R:295), M13 Oracle A snap convention, 4 cpm_api snapshot rows
  (pole row only, byte-identical otherwise). devtools::test(): 0 fail /
  2095 pass (4 warnings pre-existing).
- 2026-07-16: T3 done — consumer audit (AC2). Arithmetic-invariant to the
  0→360 relabel, verified unaffected: ssm_ci_d_cover arc membership
  (ssm_ci_accuracy.R:893, mod-2π; tested test-ci_accuracy.R:50), the straddle
  lci>uci convention ((uci−lci) %% 360 identical; M13 e2e tests green),
  ssm_arc_span + StatSsmArc (geom_ssm.R:46/:191; smoke-rendered a 360→20 seam
  arc), plot.circumplex_cpm pre-filter (cpm_oop.R:332; [360,360] → zero-width
  wedge, smoke-rendered), ggrad (utils.R:72, linear map), print/summary/table
  formatting surfaces (smoke-verified 360 rows), plot.circumplex_ssm contrast
  panel (contrast quantile untouched). Doc surfaces updated to the [0,360]/
  pole=360 wording: geom_ssm_arc roxygen + error message, cpm_fit.R engine
  comments, ssm_analyze roxygen CI-endpoint sentence, DESIGN.md G2 row
  (M20 completion note). fit_structure angles are a separate estimate surface
  (no displacement-CI reader) — out of scope. devtools::document() clean
  (2 pre-existing cpm link warnings, untouched lines).
- 2026-07-16: T4 done — boundary-coverage.md M20 cells + audit note (cited
  lines re-verified after test insertions); NEWS.md behavior-change entry;
  devtools::check(--no-manual): 0 errors / 0 warnings / 0 notes. All tasks
  complete → status review.

## Decisions

- M20-D1 (2026-07-16): CPM pole-angle alignment snaps the *reported degrees*
  only (`theta_deg`/`Angle`); computational radians (`theta_rad`,
  `theta_rad_unwrapped`) stay as-is so fitted matrices (`Phat`, residuals) are
  byte-identical. Chosen at the amendment gate over CI-snap-only (would ship
  Angle = 0 with CI [360, 360]) and re-planning.

## Review

Reviewed 2026-07-16 (PR #45). Fresh evidence per criterion:

- **AC1**: full suite fresh at review: 0 fail / 2095 pass (NOT_CRAN). Teeth
  re-proven fresh: pre-M20 snap reverted in-memory (assignInNamespace) → the
  two new SSM pole tests go red (primitive 3 failures, e2e 2 failures);
  restore → green. Boundary set in the green suite: pole-peaking
  (test-ssm_bootstrap.R:238, :261), pole-straddling (M13 tests
  test-cpm_angle_ci.R:8/:78 + test-ssm_bootstrap.R:210 unchanged), flat
  (test-ssm_bootstrap.R:137 + M13 all-NA quantile test).
- **AC2**: audit recorded in work log (T3 entry, 2026-07-16) — every reader
  verified invariant or doc-updated; smoke run of print/summary/table/plot
  surfaces with pole-valued CIs recorded there.
- **AC3**: boundary-coverage.md carries the M20 cells + "Audit notes (M20)"
  (6 M20 anchors; cited pre-M20 line numbers re-verified after insertions).
- **AC4**: devtools::check(--no-manual) after all package changes:
  0 errors / 0 warnings / 0 notes (4m42s); only cairn/ tracking edits since.
- **AC5**: NEWS.md dev-version entry present (no milestone numbers in text).
- **AC6**: CPM e2e regression test (test-cpm_angle_ci.R:50) green in the
  fresh suite; pre-fix red proven at T1 (3 failures, logged 2026-07-16).

Consistency gate: cairn_validate all-pass (exit 0). r-package toolchain
checks: document() no package diff; README.Rmd untouched by branch; 
pkgdown::check_pkgdown() no problems; NEWS entry present; no new top-level
files; full check clean (AC4).
