<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M15: Contrast certification-conditional reporting consistency (ci_accuracy ↔ print)

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Make `ssm_ci_accuracy()` report a contrast row's displacement coverage
unconditionally — matching `print.circumplex_ssm()`'s profiles-only
certification stance — resolving the split treatment left by the M4
milestone-close review.

## Scope

**In:**
- In the ci_accuracy print/summary path (`R/ssm_ci_oop.R`), for the contrast
  row, report displacement coverage **unconditionally** — drop the "when
  certified" (joint-certification) framing that `print.circumplex_ssm()` never
  exposes for a contrast (`R/ssm_oop.R:172-190`, contrasts are not
  certification-gated because Δa is a signed difference, not a prototypicality
  measure; M4 spec §4.1). Profiles keep both unconditional and
  certification-conditional lines plus the profiles-only guardrail line.
- Settle and apply the residual object-contract question: whether the
  contrast's certification-conditional coverage value is suppressed from
  print only, or also dropped/NA'd from the returned `ssm_ci_accuracy` object
  (`res$coverage` / guardrail `Cert_rate`). (RB tripwire: no-oracle)
- Supersede the "Milestone-close review #3" split decision now encoded in
  `tests/testthat/test-ci_accuracy.R:221-250`; update that test to the new
  expectation and add a regression test pinning the contrast's
  unconditional-only displacement line.
- Re-pin the `ci_accuracy` snapshot; roxygen note in `R/ssm_ci_accuracy.R`
  documenting the contrast exception.

**Out:**
- Any change to `print.circumplex_ssm()` — already correct; Direction A leaves
  it untouched (Direction B/C rejected at the plan gate 2026-07-12).
- Guardrail certification-**rule** replacement (print-precision dependence /
  scale-free rule) → stays its own ROADMAP candidate.

## Acceptance criteria

- [ ] For a contrast object, `print()`/`summary()` of `ssm_ci_accuracy()`
      report the contrast's displacement coverage unconditionally — no "when
      certified" framing on the contrast displacement line. Evidence: updated
      `ci_accuracy` snapshot + a test asserting the contrast displacement line
      carries no certification-conditional wording.
- [ ] Profile rows unchanged — both unconditional and certification-conditional
      coverage and the profiles-only guardrail line still emitted. Evidence:
      profile portions of the snapshot unchanged; existing profile assertions
      green.
- [ ] The returned `ssm_ci_accuracy` object handles the contrast
      certification-conditional value per the RR02 disposition, pinned by a
      regression test. (RB tripwire: no-oracle) Evidence: RR02 ingested; test
      pins the agreed object contract.
- [ ] Roxygen documents the contrast exception; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T2, T3
- AC2 → T3
- AC3 → T1, T4
- AC4 → T5

## Tasks

- [ ] **T1** — Draft RB02 and ingest RR02 (via `/milestone-brief`): settle the
      residual object-contract question (suppress-print-only vs drop/NA the
      contrast's certification-conditional value) and confirm unconditional-only
      supersedes Milestone-close review #3. (RB tripwire: no-oracle)
- [ ] **T2** — Regression test first: add/adjust tests pinning the contrast's
      unconditional-only displacement reporting (red before the change); revise
      the `test-ci_accuracy.R:221` expectation to the superseding behavior.
- [ ] **T3** — Modify `R/ssm_ci_oop.R` print/summary path so the contrast row
      reports displacement coverage unconditionally; profiles byte-unchanged.
- [ ] **T4** — Apply the RR02 object-contract disposition and pin it with a
      regression test.
- [ ] **T5** — Re-pin the `ci_accuracy` snapshot; roxygen note in
      `R/ssm_ci_accuracy.R`; `devtools::document()`; `devtools::check()` clean.

## Work log

- 2026-07-12: created by /milestone-plan (promoted from the "statistical
  follow-ups" grouped ROADMAP candidate — contrast-cert-consistency sub-item).
  Direction A (ci_accuracy matches print) and a Fable RB review both chosen at
  the plan gate; T1/AC3 carry the no-oracle tripwire. Reconciliation reverses
  the certification-conditional half of Milestone-close review #3
  (`test-ci_accuracy.R:221`), a deliberate prior decision — flagged for the
  blame-history reviewer.

## Decisions

## Review
