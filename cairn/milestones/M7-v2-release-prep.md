# M7: v2.0.0 CRAN release preparation

- **Status:** blocked
- **Priority:** high
- **Depends on:** M16
- **Branch/PR:** —

## Goal

Ship the accumulated M2–M5 work (inference, visualization, Browne model + CI
trustworthiness, structure tests, SEM) to CRAN as one v2.0.0 release.

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
- New features (M6 longitudinal → its own ~v2.1.0).
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

## Decisions

## Review
