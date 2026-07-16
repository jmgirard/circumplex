# M21: T_diag-vs-T_free inference-default decision + application

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m21-t-calibration-decision · https://github.com/jmgirard/circumplex/pull/44

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

- [x] Calibration comparison recorded as a committed `devel/` artifact
      (script + summary), citing the M19/M4 rds provenance and seeds; any
      re-dump uses the analytic-only paths with disjoint documented seeds.
- [x] Fable RB/RR pair completed and ingested for the inference-default
      decision (RB tripwire: irreversible-api).
- [x] Superseding D-entry appended to `cairn/DECISIONS.md` recording the
      decision and its evidence, whichever direction it lands.
- [x] The decided outcome is applied and tested: user-facing inference
      guidance (docs/caution/`summary()` wording, and the default itself if
      GO) matches the D-entry; a keep-diag decision is applied as documented
      rationale in the CPM docs.
- [x] `devtools::check()` clean (0 errors / 0 warnings / 0 notes); NEWS.md
      entry if exported behavior changes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4

## Tasks

- [x] **T1** — Assemble the calibration analysis (`devel/m21-t-calibration.R`
      + summary md): read both rds files; decide whether `ks_T` summaries
      suffice or per-replicate T re-dump is needed; compare calibration
      (KS, rejection rates at α=.05) per cell/family.
- [x] **T2** — Draft the RB with the analysis attached → /milestone-brief;
      ingest the RR (RB tripwire: irreversible-api).
- [x] **T3** — Append the superseding D-entry to `cairn/DECISIONS.md`.
- [x] **T4** — Apply the decided outcome (code/docs/`summary()` wording/NEWS
      as decided) with tests; run full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the free-scaling
  extensions candidate row, item 3). Decide-and-apply scope + mandatory
  Fable RB chosen at the plan gate; supersedes-D-009 requirement noted.
- 2026-07-16: T1 done — ks_T summaries insufficient (one-family, unpaired,
  partial N overlap), so ran the paired per-replicate design instead
  (`devel/m21-t-calibration.{R,md}` + results rds; 500 reps × 12 cells,
  ~3.7 min, reproduced bit-identically). Result: families
  calibration-indistinguishable everywhere (paired ΔT̄ ≤ 0.5% of df, cor
  ≥ .998); evidence points to keep-unit-default. RB next.
- 2026-07-16: blocked on RB05 (`cairn/reviews/RB05-t-calibration-default.md`,
  the mandatory T2 Fable escalation; RB tripwire: irreversible-api).
- 2026-07-16: RR05 returned same-session (Fable subagent, user-approved) and
  ingested; T2 done. Decision: keep unit default (promoted → D-011,
  discharging D-009 item 3; T3 done in the same stroke). Pair archived;
  unblocked. R6 (variant-C smoke) scheduled into T4; R5 (unit-seeded free
  multi-start) → infra candidate row; R7 rejected per RR05 reasoning.
- 2026-07-16: T4 done — D-011 applied as docs under the RR05 guardrails
  (`@param scaling` roxygen, evaluating-circumplex-structure vignette §2,
  NEWS free-scaling bullet; no code-behavior change, so guidance-only NEWS).
  Variant-C paired smoke run (R6): same tie at df = 17, appendix added to the
  analysis md; variant-A smoke reproduces bit-identically. `document()` +
  `check()` clean (0 errors / 0 warnings / 0 notes). Status → review.

## Decisions

- 2026-07-16 (M21-D1, from RR05): keep the unit family as the CPM model-test
  inference default — promoted to `cairn/DECISIONS.md` **D-011** (supersedes
  D-009 item 3's deferral; re-trigger = a gate inside any future
  covariance-input milestone).
- 2026-07-16 (RR05 triage): R1–R4 apply (D-011 + T4 wording guardrails:
  equivalence scoped to model test + correlation input, never "identical",
  no invalid-p implication for free, conservatism-at-small-N note kept, no
  extrapolation beyond the measured envelope; B1 phrasing adopted). R5
  (unit-seeded free multi-start, B2) → infra candidate row. R6 (variant-C
  paired smoke) → run during T4 as belt-and-suspenders. R7 (more reps /
  truths / polish stratification) rejected — RR05's reasoning logged.

## Review

Fresh evidence, 2026-07-16, PR #44 (draft while reviewing):

- **AC1** — `devel/m21-t-calibration.{R,md}` + results/smoke rds all in the
  branch diff (`git diff --name-only origin/master..HEAD -- devel/`). Script
  header documents the paired design, M19/M4 provenance, and seeds
  (`BASE_SEED + 12e7 + 1e6·cfg_idx + 1e4·N_idx + i`, disjoint from stages
  1–3; level-indexed per the M19 lesson). Engine-only analytic paths; full
  run ~3.7 min, reproduced bit-identically twice (work log).
- **AC2** — `cairn/reviews/archive/{RB05,RR05}-t-calibration-default.md`
  exist; RR05 ingested 2026-07-16 (M21-D1 + triage in Decisions; commit
  `1291842`). Tripwire tagged on T2 as planned.
- **AC3** — D-011 appended to `cairn/DECISIONS.md` (grep: 1 heading match),
  recording keep-unit-default, grounds, envelope, and the covariance-input
  re-trigger gate; supersedes D-009 item 3.
- **AC4** — keep-diag outcome applied as documented rationale:
  `R/cpm_fit.R:1346` (`@param scaling`), regenerated `man/cpm_fit.Rd:62`,
  `vignettes/evaluating-circumplex-structure.Rmd:164`, `NEWS.md:176` — all
  carry the D-011-scoped wording (model test + correlation input, never
  "identical", envelope stated, no invalid-p implication, small-N
  conservatism note retained in the existing caution text).
- **AC5** — `devtools::check(args = "--no-manual")`: 0 errors / 0 warnings /
  0 notes (5m15s, this session, on the final code state). NEWS guidance
  added (no exported behavior change; no milestone numbers in user-facing
  text).

Consistency gate (2026-07-16): `cairn_validate` pass; Coverage complete
(AC1–5 → T1–T4, all tasks exist); no DESIGN principle touched (skip
`cairn_impact`); `document()` no diff to `man/`/`NAMESPACE`; README
untouched; `pkgdown::check_pkgdown()` no problems; `devel/` is
`.Rbuildignore`d; suite 2083 pass / 0 fail (work log).
