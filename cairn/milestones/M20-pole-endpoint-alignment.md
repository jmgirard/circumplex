# M20: 0-vs-360 pole CI-endpoint alignment

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

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
- Mandatory boundary tests (CLAUDE.md): profiles peaking at 0°/360°, CIs
  straddling 0/360, flat profiles; guard teeth proven per the M13 recipe.
- `cairn/boundary-coverage.md` matrix update; NEWS.md entry (exported CI
  values change at the pole, measure-zero for real data).

**Out:**
- Estimate-path canonicalization — D-003 stands (estimate already reports 360).
- `quantile.circumplex_contrast_radian` — unwrapped branch, no pole snap; untouched.
- Analytic-CI Hessian recomputation — stays in the infra candidate row.

## Acceptance criteria

- [ ] A CI endpoint denoting the 0/360 pole reports 360 (never 0) from
      `quantile.circumplex_radian`, exercised end-to-end through both
      `ssm_analyze()` bootstrap CIs and `cpm_fit()` bootstrap angle CIs, with
      regression tests at the mandatory boundary set (pole-peaking profile,
      pole-straddling CI, flat profile) that fail on the pre-change snap.
- [ ] Consumer audit recorded in the work log: all `*_lci`/`*_uci` readers
      (incl. `ssm_ci_accuracy` arc membership and the straddle `lci > uci`
      convention) verified unaffected or updated with tests.
- [ ] `cairn/boundary-coverage.md` updated for the new pole-endpoint cells.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).
- [ ] NEWS.md documents the exported change.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T4
- AC5 → T4

## Tasks

- [ ] **T1** — Write the red boundary regression tests first (SSM + CPM
      end-to-end pole cases; prove teeth by confirming they fail against the
      current snap-to-0).
- [ ] **T2** — Implement the value-level snap in `quantile.circumplex_radian`
      (`R/ssm_bootstrap.R:177`): pole-adjacent endpoints → 2π; tests green.
- [ ] **T3** — Consumer audit: grep all displacement-CI endpoint readers
      (`ssm_ci_accuracy.R`, print/summary/plot, straddle logic); update any
      that assumed the 0 label, with tests.
- [ ] **T4** — Update `cairn/boundary-coverage.md` + NEWS.md; run full
      `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the infra candidate
  row's pole-snap item; D-003's parked follow-up — completes, does not
  supersede, D-003). Value-level snap chosen over print-only at the plan gate.

## Decisions

## Review
