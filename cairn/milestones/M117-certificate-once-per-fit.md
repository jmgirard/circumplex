<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M117: Price the accuracy certificate once per checked fit

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP2
- **Branch/PR:** m117-certificate-once-per-fit

## Goal

`axes_reliability()` prices the per-fit accuracy certificate once per checked
fit instead of the twice recorded at `R/axes_corrected_se.R:737-740`, with no
change to any refusal, warning or reported number.

## Scope

Surface tier: **user-facing** — it changes shipped R code on
`axes_reliability()`'s call path, which external consumers run, even though the
change is behavior-preserving.

**In:** `axes_reliability()` calls `axes_corrected_se()`
(`R/axes_reliability.R:1813`) and `axes_scaling_factor()` (:1922) with the same
matrix and the same derivative set, and each independently calls
`axes_degeneracy_refusal()` → `axes_accuracy_certificate()`. This milestone
gives both surfaces an optional pre-computed-refusal argument that
`axes_reliability()` fills once, keeps both standalone-callable with the
argument absent, and proves the two surfaces still agree on both sides of the
seam.

**Out:**
- memoizing across fits or across sessions → not planned; a per-call seam is
  what the duplication needs
- the three certificate-suite test weaknesses → M116
- surfacing the certificate on computed fits → ROADMAP degeneracy row

## Acceptance criteria

- [ ] AC1: For `axes_reliability()` calls whose realigned `cov2cor` matrix
      `axes_sigma_degenerate()` answers `"ill_conditioned"` for — injected at
      the `axes_fitted_cov` binding the way
      `tests/testthat/test-axes-reliability.R:3105` already does —
      `axes_accuracy_certificate()` is entered exactly once per call, counted by
      a trace, on both a listwise-default call and a `missing = "fiml"` call.
- [ ] AC2: `axes_corrected_se()` and `axes_scaling_factor()` called WITHOUT the
      new pre-computed-refusal argument each compute the certificate themselves
      and return the `"uncertified"` literal with a warning matching
      `"estimated relative error "` whose estimate is derived from that
      certificate, asserted by a test firing each surface standalone on an
      uncertifiable matrix.
- [ ] AC3: On one matrix refused as `"uncertified"`, the warning
      `axes_corrected_se()` emits and the warning `axes_scaling_factor()` emits
      report the same estimated relative error — asserted both where each
      surface computes its own certificate and where both receive the
      pre-computed refusal.
- [ ] AC4: `Rscript -e 'devtools::test()'` is clean and
      `git status --short tests/testthat/_snaps/` reports nothing.
- [ ] AC5: `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors
      and 0 warnings.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T5
- AC5 → T5

## Tasks

- [x] T1: Measure the wall-clock cost of one `axes_reliability()` call on an
      ill-conditioned p = 24 fit before the change. Record the command, date,
      machine and figure in the work log.
- [ ] T2: Add the optional pre-computed-refusal argument to
      `axes_corrected_se()` and `axes_scaling_factor()`, defaulting to computing
      it; `axes_reliability()` computes it once and passes it to both. The
      finiteness / `"singular"` / `"infinite_diagonal"` precedence and the
      `naive_reason` decoupling stay ahead of the seam.
- [ ] T3: Write the trace test (AC1, both `missing` paths), the two standalone
      tests (AC2), and the agreement test on both sides of the seam (AC3).
- [ ] T4: Re-measure T1's fit after the change; record beside T1's figure.
- [ ] T5: `devtools::test()` clean with no snapshot diff;
      `devtools::check(args = "--no-manual")` clean.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan gate chose an optional per-call pre-computed-refusal argument over memoizing inside `axes_degeneracy_refusal()` on a cache keyed by the matrix, because a cache would have to decide key identity for a floating-point matrix and would outlive the call; falsified by a second duplication appearing on a path that cannot thread an argument through.
- 2026-08-30: criteria audit ran in **full** mode (user-facing tier), fresh-context [O] reader, two passes. First pass returned three findings — a goal sentence claiming the duplication departs from D-051's decision text when D-051 states a cost model rather than a call-count contract, a criterion binding a before-measurement that cannot be reproduced from the merged tree, and a criterion already green on the pre-change tree. Second pass over the post-gate wording returned three more — a wrong injection-site line reference, one probe standing for a family free in the `missing` path, and an unbounded promise over a warning text whose estimate is machine-dependent by design. All disposed before this file was written.

- 2026-08-31: implement started; branch m117-certificate-once-per-fit. Question gate skipped — both seam surfaces are internal (`NAMESPACE` exports only `axes_reliability`), the plan gate settled the design, no tripwire tags; the two minor choices made here: the argument is named `refusal` (the `axes_degeneracy_refusal()` return, default `NULL` = compute it), and `axes_reliability()` builds it once inline at the seam.
- 2026-08-31: T1 done — scratchpad script (M89 AC6 injection shape: p = 24 octant matrix, item 2 duplicated + 1e-9 ridge, mocked at `axes_fitted_cov`), 5 timed reps after warm-up via `system.time`, Apple M5 Pro / R 4.6.1, 2026-08-31: median 0.033 s per `axes_reliability()` call; one `axes_accuracy_certificate()` evaluation on that matrix: median 0.002 s.

## Decisions

## Review
