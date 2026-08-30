# M114: Pin the shared refusal predicate, and assert the dead literal rather than dropping it

- **Status:** review
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** GP2
- **Branch/PR:** `m114-refusal-predicate-fences`

## Goal

Fence the one-predicate-both-surfaces refusal design against the per-surface
alternative it was chosen over, and assert the deadness claim that motivated
dropping `"ill_conditioned"` from the nestedness set without paying the
backstop coverage dropping it would cost.

## Scope

Surface tier: **user-facing** — both criteria fence which refusal literal a
user is shown.

**In:** a committed input whose certificate estimates straddle
`axes_degeneracy_delta_star`, asserted to refuse at both surfaces and shown to
redden when the shared `max()` at `R/axes_corrected_se.R:765` is replaced by
each surface reading only its own field (M111 review F4); and an added
assertion on the AC8 grid in `tests/testthat/test-axes-scaled-fit.R` that no
`reason` field there returns `"ill_conditioned"`, with the literal kept in
`check_nested()`'s set (M111 review F12, as repaired by the criteria audit).

**Out:** the certificate's estimand and the FIML denominator → M113. The
packaged bracket's platform reach → M115. The M108/M111 cosmetic and
performance residue → the ROADMAP degeneracy candidate row.

## Acceptance criteria

- [ ] AC1 A committed input on which the certificate's fields straddle
      `axes_degeneracy_delta_star` — at least one at or below it, at least one
      above — is asserted to refuse `"uncertified"` at both surfaces; with the
      shared max replaced by each surface reading only its own field, that
      assertion reddens at one surface. Where no matrix producing a straddling
      set is found, the same assertion is driven by a stubbed
      `axes_accuracy_certificate()` return instead.
- [ ] AC2 `check_nested()`'s literal set in
      `tests/testthat/test-axes-scaled-fit.R:1409` still contains
      `"ill_conditioned"`, and the AC8 test additionally asserts that on every
      matrix its own grid drives, neither surface's `reason` field returns that
      literal.
- [ ] AC3 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4

## Tasks

- [x] T1 Search the anchor families and the committed counterexample for an
      input whose certificate fields straddle `axes_degeneracy_delta_star`;
      record the search and its outcome either way.
- [x] T2 Commit the straddling input (or the stub), assert `"uncertified"` at
      both surfaces, mutate the predicate to per-surface fields, record the
      redden, revert and verify the tree clean.
- [x] T3 Add the AC8 grid assertion, keeping `"ill_conditioned"` in the guard
      set; prove it able to fail by planting that return.
- [x] T4 NEWS entry if any user-visible behaviour moved; profile verify and
      consistency-gate slot.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (declared user-facing tier) as part of the joint M113/M114/M115 run; its finding on AC2 is disposed in the gate line below, and its instrument-property finding on AC1's "the search that failed is recorded" clause was fixed by moving that clause to T1.
- 2026-08-30: plan gate chose keeping `"ill_conditioned"` in `check_nested()`'s set and asserting its deadness on the AC8 grid over dropping it as M111 review F12 proposed, because `axes_scaling_factor()`'s `cval <= 0` backstop still emits that literal (`R/axes_scaled_fit.R:302`, asserted at `test-axes-scaled-fit.R:1764`), so dropping it would silently retire nestedness coverage there; falsified by that backstop being removed or proved unreachable.
- 2026-08-30: `Depends on: M113` because M113 adds a third certificate field, which changes what a straddling set means for AC1.
- 2026-08-30: pre-implementation gate: the per-surface alternative AC1 must redden against is `max(se, fiml_ratio)` at the SE helper and `cval` at the scaling surface — the partition each function's own return defines; the M113 review's F3 fail-open on a short certificate was declined for this milestone and stays on the ROADMAP degeneracy row.
- 2026-08-30: T3 run ahead of T1/T2 (minor reorder — the AC2 assertion is independent of AC1's input search, which runs long).
- 2026-08-30: T3 done. `check_nested()` now asserts `reason` is not `"ill_conditioned"` at both surfaces on every matrix the AC8 grid drives, with the literal kept in the guard set; planting `list(reason = "ill_conditioned", ...)` in `axes_degeneracy_refusal()` reddened both new assertions (`actual TRUE` at the near-singular matrix, all three p), reverted and green at 348 passing.
- 2026-08-30: T1 done. Searched the three anchor families' stated parameter space and a neighbourhood of the committed counterexample for a straddling certificate: 9,588 candidates, 6,666 of which reached the certificate, 388 straddling. Families A and B produce none at any parameter tried -- their three fields climb together to ~1e-7 and then both pricing routes fail at once, so all three become the sentinel with nothing between; the committed counterexample does not straddle either (se 3.4e-2, cval 4.9, fiml_ratio 8.7e-4, all above the target). Family C does, at p = 4: across item-error variances 2.5e-9 to 8e-9 with xi1 = 0.1, xi2 = 0.3 the cval estimate runs 400-5000x the target while the SE vector's and the quotient's sit about 1000x below it. Search scripts in the session scratchpad, not committed.
- 2026-08-30: T2 done. The straddle is committed as a closed-form call on the existing `m106_family_c()` builder at three band members (3e-9, 5e-9, 8e-9) -- deterministic, no fixture, no seed -- and both surfaces are asserted to refuse `"uncertified"` on each, the SE helper being the one whose own two fields are inside the target. Mutating `axes_degeneracy_refusal()` to read `max(se, fiml_ratio)` at the SE helper and `cval` at the scaling surface reddened the SE helper at all three members (`reason` NULL rather than `"uncertified"`, with the raw arm's `"ill_conditioned"` surfacing in `naive_reason`); reverted, `git status` clean under R/. A second stubbed-certificate test covers the other two fields, which the search found on the far side only in company.

- 2026-08-30: T4 done. No NEWS entry: the branch changes tests and tracking only, and no user-visible behaviour moved. `devtools::test()` 0 failures / 9,100 passing, with the suite's 5 warnings and 1 skip all in files this branch does not touch (`test-ci_accuracy.R`, `test-ssm_sem.R`, and the pre-existing fixture-version skip at `test-axes-scaled-fit.R:921`); `devtools::document()` no diff and no unresolved-link warning at pinned `cli.width`; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes in 8m 48.5s; `cairn_validate` all checks pass.

## Decisions

## Review
