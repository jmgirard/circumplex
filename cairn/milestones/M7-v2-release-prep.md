# M7: v2.0.0 CRAN release preparation

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M25, M26, M27, M31, M32, M33, M34, M35, M36, M37, M38
- **Branch/PR:** `m7-v2-release-prep`

## Goal

Ship the accumulated M2–M5 work (inference, visualization, Browne model + CI
trustworthiness, structure tests, SEM) plus the CIRCUM free-scaling family
(M17/M18, per D-008) and the longitudinal SSM builds (M25–M27, per D-012 +
the 2026-07-16 plan gate) plus the visualization expansion (M30 design → M31
build, M32 ergonomics, M33 trajectory viz, M34 docs, M35 model-based
trajectories, per D-018) to CRAN as one
v2.0.0 release. **No target date** — the release ships when its bundle is
complete and validated (D-008).

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

- [x] **T1** — Version bump + NEWS heading + refresh `cran-comments.md`
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
- 2026-07-18: AMENDED (gated, /milestone-plan) — dependency gains M35, split out
  of M33 at that milestone's re-plan gate (the growth vignette's trajectory
  figure is model-based, not an occasions object). Extends the D-018 viz
  expansion to M30–M35. No release-prep task change.
- 2026-07-18: gated amendment — `Depends on:` gains M36 (viz polish: legend key
  glyph + non-finite guards) and M37 (static on-circle movement paths), the
  M31–M35 viz-track remainders. Jeff's plan-gate decision: both ship in v2.0.0
  under D-018's fold-in. No release-prep task change.
- 2026-07-18: gated amendment — `Depends on:` gains M38 (guaranteed rim ring for the circumplex canvas), spun out of the PR #62 hotfix. Jeff's plan-gate decision: the rim is visible in every figure the CRAN debut ships, so it goes in the bundle. No release-prep task change.

- 2026-07-18: started (/milestone-implement). Branch `m7-v2-release-prep` cut from master at 3d2d9a76; all 11 dependencies verified done. Status planned→in-progress.
- 2026-07-18: T1 done. DESCRIPTION 1.3.0.9002→2.0.0; NEWS dev section retitled to 2.0.0 and consolidated from 47 flat bullets into 8 thematic `##` groups (matching the 1.1.0/1.0.0 heading style) per the profile's release-walk slot — Jeff's gate choice "consolidate and group". Fixes to code that never shipped (the rim-ring omission, the trajectory legend key, the non-finite amax/center guard — all against features new in 2.0.0) were folded into their feature descriptions rather than listed as fixes; the two Advanced Visualization vignette bullets (added then rewritten in-cycle) merged into one. `cran-comments.md` refreshed: added the longitudinal and viz families, corrected "four"→five new vignettes, added brms/glmmTMB to the new-Suggests list, added a dependency note for the ggplot2 3.3.0→4.0.0 floor and the ggforce drop, and replaced the stale "one user-visible API tightening" claim with the three actual behavior changes (certification rule, pole labeling, argument validation). `devtools::test()`: 0 failures, 2986 passing, 0 skipped under NOT_CRAN=true; 4 pre-existing warnings in test-ci_accuracy.R (the diagnostic's own cautions, no code touched by T1).
- 2026-07-18: T3 aid written — `devel/m7-transcription-reread-checklist.md` (Jeff's gate choice "I prepare a checklist for you"). Enumerates every transcribed Grassi 2010 value (Table 1 matrix + N, the Appendix A full-precision block, Table 2/3 fit measures, the constrained-model rows, three quoted textual claims, the Listing 7-8 secondary fixture) and the Zimmermann & Wright subset that reaches shipped user-facing output (the vignette's Studies 1-2 accuracy table, Note 3 population matrices, the Eq. A6/A7/Eq. 3 scaling formulas, Study 5 + Table 4, Figure 1A octant angles), each with its repo location and its table/page anchor. Flags two things for the reader: the Appendix-A-vs-Table-2 mirror direction, and the one channel discrepancy `m4-zw-transcription.md` resolved by reasoning rather than a clean second read (Eq. A7's radicand). Instructs source-first reading. **T3 itself remains open** — the checklist is an aid, not the re-read, and AC3 needs Jeff's attestation.
- 2026-07-18: T2 partial. Local `devtools::check(args = "--no-manual")` on 2.0.0: **Status OK, 0 errors / 0 warnings / 0 notes** (5m10s; tests 171s, vignettes rebuilt clean) — `cran-comments.md`'s existing clean-check assertion holds as written, no correction needed. win-builder R-devel uploaded (Jeff's gate approval was conditional on the local check being clean; precondition met); results due to me@jmgirard.com in 15-30 min. T2 stays open pending those results. **Finding at build time (not a check NOTE):** `R CMD build` warns that the package now depends on R >= 3.5.0 because of the serialized `vignettes/bayesian_ssm_draws.rds` fixture (D-015), while DESCRIPTION declares `Depends: R (>= 3.4)`. Verified the real floor is higher still: ggplot2 (>= 4.0.0) and htmlTable both declare `Depends: R (>= 4.1)`, so the effective install floor is R >= 4.1 — exactly what D-014/D-019 recorded without DESCRIPTION ever being updated. Re-pinning the R floor is a dependency change (tracking-rules: question gate + D-entry, never unilateral), so it is gated to Jeff rather than applied.
- 2026-07-18: D-021 applied (user-approved): `Depends: R (>= 3.4)`→`R (>= 4.1)`, plus NEWS and cran-comments dependency notes. Re-ran the full local check on the corrected tarball: **Status OK, 0/0/0** (5m07s), and the `R CMD build` R>=3.5.0 serialization warning is gone as D-021 predicted. win-builder re-upload **failed: FTP 550** — most likely win-builder still holds the identically-named `circumplex_2.0.0.tar.gz` from the earlier (pre-D-021) upload and refuses to overwrite until that run clears. Not retried in a loop. T2 remains open: AC2 needs win-builder evidence for the *corrected* tarball, so the earlier in-flight run does not satisfy it.

## Decisions

## Review
