# M7: v2.0.0 CRAN release preparation

- **Status:** planned
- **Priority:** high
- **Depends on:** M25, M26, M27
- **Branch/PR:** —

## Goal

Ship the accumulated M2–M5 work (inference, visualization, Browne model + CI
trustworthiness, structure tests, SEM) plus the CIRCUM free-scaling family
(M17/M18, per D-008) and the longitudinal SSM builds (M25–M27, per D-012 +
the 2026-07-16 plan gate) to CRAN as one v2.0.0 release. **No target date** —
the release ships when its bundle is complete and validated (D-008).

## Scope

**In:**
- Version bump to 2.0.0; NEWS.md development heading → 2.0.0.
- Refresh `cran-comments.md` (test environments, clean check (0 errors / 0 warnings / 0 notes), no revdeps, the
  Moss DOI 403 = SAGE bot-block note from urlchecker).
- Second independent human re-read of the Grassi et al. (2010) CircE and
  Zimmermann & Wright (2017) transcriptions against their primary sources
  (a pre-submission oracle gate; absorbed from a ROADMAP candidate 2026-07-12).
- win-builder / R-devel checks; then hand `submit_cran()` to Jeff (never submit
  autonomously).

**Out:**
- New features beyond the bundled milestones (longitudinal deferrals stay
  ROADMAP candidates; D-012 governs any late-merging build).
- The billed `/code-review ultra` unless Jeff asks (legacy CLAUDE workflow).

## Acceptance criteria

- [ ] DESCRIPTION at 2.0.0; NEWS.md heading renamed; `cran-comments.md` accurate.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes) and
      win-builder / R-devel green across platforms.
- [ ] Second independent human re-read of the Grassi et al. (2010) CircE and
      Zimmermann & Wright (2017) norm/structure transcriptions against their
      primary sources completed before submission, with any discrepancies
      resolved (Jeff-attested in the work log).
- [ ] Release handed to Jeff for `submit_cran()` (not self-submitted).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4

## Tasks

- [ ] **T1** — Version bump + NEWS heading + refresh `cran-comments.md`
      (groundwork staged 2026-07-08; the accurate summary + urlchecker DOI note
      already written per legacy MILESTONES.md R2).
- [ ] **T2** — Full `check()` + win-builder / R-devel.
- [ ] **T3** — Second independent human re-read of the Grassi et al. (2010) and
      Zimmermann & Wright (2017) transcriptions vs primary sources; gates
      submission. Human task (Jeff); discrepancies resolved before T4.
- [ ] **T4** — Hand `submit_cran()` to Jeff.

## Work log

- 2026-07-12: created by /cairn-init migration from circumplex's legacy
  MILESTONES.md active unit ("v2.0.0 release preparation", task R2). R1
  (`cpm_pack` β-boundary fix, the last cross-platform CI red) is DONE — verified
  green 2026-07-08 (PR #29, all 7 checks). No-invention: criteria/tasks
  translated from R2's written accept text, not inferred.
- 2026-07-12: BLOCKED — held for the CRAN cadence window. v1.2.0 was
  CRAN-approved 2026-07-02; CRAN wants ~1 month between submissions, so the
  version bump / NEWS rename / win-builder / submit are deliberately deferred
  until ~2026-08-02 (freeze ~2026-07-26). Repo stays at 1.3.0.9002 until then
  (legacy MILESTONES.md R2).
- 2026-07-12: AMENDED (gated) — absorbed the "v2.0.0 pre-release oracle
  re-reads" ROADMAP candidate as AC3/T3 (second independent human re-read of
  the Grassi 2010 + Zimmermann & Wright 2017 transcriptions, gating submission);
  old submit-handoff task renumbered T3→T4. Candidate row retired. Scope
  unchanged otherwise; still blocked on the CRAN cadence window.
- 2026-07-12: gained `Depends on: M16` (/milestone-plan) — the v2.0.0 bundle
  now includes the print-independent certification-rule replacement, which the
  user placed before the ~2026-07-26 freeze. No release-prep task change.
- 2026-07-12: dependency M16 is now **done** (PR #40 merged, squash `cd0c140`).
  M7 stays `blocked` on the external CRAN cadence window (~2026-08-02), not on
  any remaining milestone.
- 2026-07-13: dependency re-pointed M18→M19 (/milestone-plan). M18 is done; the
  free-family analytic-CI **coverage** claim is not yet settled. Per D-009's
  "mandatory pre-ship gate before any analytic-CI-trust claim", M7 now depends on
  M19 (free-family coverage oracle + caution calibration). No release-prep task
  change.
- 2026-07-12: AMENDED (gated, /milestone-plan) — **the CRAN cadence window is no
  longer a constraint (D-008).** Jeff confirmed there is no release-time
  pressure: v2.0.0 has no target date and ships when its bundle is complete and
  validated. All the ~2026-08-02 / ~07-26 framing in the entries above is
  superseded (left as the historical record). Status blocked→planned. Dependency
  re-pointed M16→M18: the v2.0.0 bundle now includes the CIRCUM free-scaling
  family (M17 design → M18 build, per D-008). No release-prep task change.

- 2026-07-16: dependency re-pointed M19→{M20, M21} (/milestone-plan). M19 is
  done; Jeff routed two pre-release items into the v2.0.0 bundle at the
  /milestone status gate: M20 (pole CI-endpoint alignment — an exported print
  change cheapest bundled into the major release) and M21 (T_diag-vs-T_free
  inference-default decision + application, superseding D-009's deferral). No
  release-prep task change.

- 2026-07-16: dependency re-pointed {M20, M21}→M22 (/milestone-plan, gated).
  Both are done; Jeff routed one more pre-release item into the v2.0.0 bundle
  at the plan gate: M22 (free-engine multi-start nesting seed, RR05 B2/R5 —
  an exported-results improvement cheapest shipped before the free family's
  CRAN debut). No release-prep task change.

- 2026-07-16: AMENDED (gated, /milestone-plan) — dependency re-pointed
  M22→{M25, M26, M27}. M22 is done; Jeff chose "all builds before M7" at the
  longitudinal plan gate, so the v2.0.0 bundle now includes the longitudinal
  SSM builds (one submission carries everything, per D-001's anti-churn
  rationale + D-012). Goal/Out wording updated to match; the stale "M6
  longitudinal → its own ~v2.1.0" Out clause is superseded. No release-prep
  task change.

## Decisions

## Review
