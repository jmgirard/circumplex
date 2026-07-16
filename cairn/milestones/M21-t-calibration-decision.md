# M21: T_diag-vs-T_free inference-default decision + application

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Decide — Fable-reviewed — whether the free-scaling family's test statistic is
the preferable CPM inference default (D-009 item 3), supersede D-009's
deferral with a D-entry, and apply the decided outcome in the package.

## Scope

**In:**
- Calibration analysis comparing T_diag vs T_free against their nominal χ²
  at matching truths/cells: `ks_T` summaries already in
  `devel/m19-free-coverage-results.rds` (free, 12 cells × 500 reps) and
  `devel/m4-coverage-oracle-results.rds` (diag); if per-replicate T vectors
  are needed, re-dump via the session-runnable analytic-only paths
  (`CPM_COV_FREE_ONLY=1` ≈ 3.5 min; diag analytic counterpart) — never the
  bootstrap-laden stages. Artifact in `devel/`.
- Mandatory Fable escalation on the decision (plan-gate ruling 2026-07-16):
  RB draft → RR ingestion per /milestone-brief.
- Superseding D-entry (D-009's "deferred, not committed to v2.0.0" clause is
  discharged either way the decision lands).
- Applying the decision: exported docs/caution/`summary()` wording — or the
  default-family change if the decision is GO — with tests and NEWS.

**Out:**
- Bootstrap σ CIs and covariance-matrix input → remain in the grouped
  free-scaling-extensions candidate row.
- Re-running the expensive bootstrap coverage oracle → not needed (M19
  collected T_free at the identical stage-1 truths by design).

## Acceptance criteria

- [ ] Calibration comparison recorded as a committed `devel/` artifact
      (script + summary), citing the M19/M4 rds provenance and seeds; any
      re-dump uses the analytic-only paths with disjoint documented seeds.
- [ ] Fable RB/RR pair completed and ingested for the inference-default
      decision (RB tripwire: irreversible-api).
- [ ] Superseding D-entry appended to `cairn/DECISIONS.md` recording the
      decision and its evidence, whichever direction it lands.
- [ ] The decided outcome is applied and tested: user-facing inference
      guidance (docs/caution/`summary()` wording, and the default itself if
      GO) matches the D-entry; a keep-diag decision is applied as documented
      rationale in the CPM docs.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes); NEWS.md
      entry if exported behavior changes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4

## Tasks

- [ ] **T1** — Assemble the calibration analysis (`devel/m21-t-calibration.R`
      + summary md): read both rds files; decide whether `ks_T` summaries
      suffice or per-replicate T re-dump is needed; compare calibration
      (KS, rejection rates at α=.05) per cell/family.
- [ ] **T2** — Draft the RB with the analysis attached → /milestone-brief;
      ingest the RR (RB tripwire: irreversible-api).
- [ ] **T3** — Append the superseding D-entry to `cairn/DECISIONS.md`.
- [ ] **T4** — Apply the decided outcome (code/docs/`summary()` wording/NEWS
      as decided) with tests; run full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the free-scaling
  extensions candidate row, item 3). Decide-and-apply scope + mandatory
  Fable RB chosen at the plan gate; supersedes-D-009 requirement noted.

## Decisions

## Review
