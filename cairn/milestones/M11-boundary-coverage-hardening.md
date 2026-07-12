<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M11: Boundary-coverage hardening + test-suite tidiness

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m11-boundary-coverage-hardening   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Prove the four angular/boundary invariants are tested at every estimation
entry point, close any genuine gap, and clear the deferred test-suite tidiness
riders — all internal-only, to land before the v2.0.0 freeze (~2026-07-26).

## Scope

**In:**
- A committed **boundary coverage matrix**: the four invariant classes from
  CLAUDE.md "Statistical invariants" (profiles peaking at 0°/360°; CIs
  straddling 0°/360°; contrasts near ±180°; flat/zero-variance profiles) ×
  each estimation entry point (`ssm_analyze()` mean-based, `ssm_analyze()`
  correlation-based, bootstrap, Monte Carlo, `ssm_ci_accuracy()`, SEM), every
  cell citing an existing test or a new one.
- New tests only for genuine gaps the matrix reveals (existing coverage is
  already strong — this is audit-and-fill, not a rewrite).
- SEM fixture consolidation (deferred M8 T5): one shared
  `sem_canonical_pop()` helper for the 8-scale population rebuilt across the
  `test-ssm_sem*.R` blocks.
- `is_flag()` validator sliver at `R/instrument_oop.R:68` (behavior-preserving).
- Rename `tests/testthat/test-RcppExport.R.R` → `test-RcppExports.R`.

**Out:**
- New exported behavior / features (none; this is quality-only) — feature work
  → M6 (~v2.1.0).
- Physically relocating existing boundary tests into one file (churn against a
  freeze; the matrix cites where they live instead).
- The strict-tier syntax-emission single-sourcing and CIRCUM/contrast-
  certification statistical follow-ups → stay candidate rows in ROADMAP.
- `covr` statistical-core coverage tracking (CI tooling) → stays a candidate.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] A boundary coverage matrix is committed under `cairn/` mapping each of
      the 4 invariant classes (CLAUDE.md "Statistical invariants") to each of
      the 6 estimation entry points; every cell cites a test by `file:line` (or
      records a documented not-applicable with reason). No empty cells.
- [ ] Every gap the matrix reveals is closed by a new test that asserts the
      boundary result (the correct value/branch/NA-semantics at the boundary),
      not a smoke test; `devtools::test()` passes. If the audit finds zero
      gaps, the matrix's all-cited state is itself the evidence.
- [ ] The 8-scale canonical SEM population is constructed by a single shared
      `sem_canonical_pop()` helper; the `test-ssm_sem*.R` rebuild sites call
      it; the produced population is bit-identical to the prior inline
      construction (`identical()`), so no snapshot/coverage re-pin is needed.
- [ ] `scales()` validates `items` via `is_flag(items)` at
      `R/instrument_oop.R:68`; a test fires the invalid-`items` error branch.
- [ ] `tests/testthat/test-RcppExport.R.R` is renamed to `test-RcppExports.R`
      and is discovered/run by testthat.
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** — Boundary coverage matrix authored at `cairn/boundary-coverage.md`
      (4 classes × 6 entry points, every cell cited by `file:line`; no empty
      cells). Shared-engine cells documented as deliberately-once, not per-path.
- [x] **T2** — Add tests for any genuine gap. Audit showed the mean path fully
      covered (incl. the pole point-estimate at `test-ssm_analysis.R:519`); the
      genuine gaps were on the **correlation** entry point. Added two
      deterministic tests: flat correlation profile → NA displacement (class D)
      and a correlation profile peaking at the 0/360 pole (class A/B), at
      `test-ssm_analysis.R:229` and `:250`.
- [x] **T3** — Added `sem_canonical_pop()` to `helper-ssm-sem.R`; a
      context-aware pass converted 15 canonical single-pop rebuilds in
      `test-ssm_sem.R` (the 9 non-canonical `sem_pop()` sites — different
      loadings, attenuated theta, 5-scale, 2-group — left direct). Bit-identical
      guarded by a new `identical()` test; full SEM suite green, no re-pin. The
      groups/syntax files already route 2-group pops through `make_pop_2g()`.
- [x] **T4** — Replaced the hand-rolled predicate at `R/instrument_oop.R:68`
      with `is_flag(items)` (behavior-identical); added an invalid-`items`
      error-path test to `test-instrument_oop.R`.
- [x] **T5** — `git mv test-RcppExport.R.R → test-RcppExports.R`; testthat
      discovers and runs it.
- [x] **T6** — Full `devtools::test()` (387 tests, 0 failed) and
      `devtools::check()` **clean (0 errors / 0 warnings / 0 notes)**. No
      roxygen touched, so no `document()`. The 3 residual testthat warnings are
      pre-existing CPM ill-conditioned-Hessian advisories in the untouched
      `test-ci_accuracy.R` (not R CMD check warnings).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan. Theme + timing chosen at the plan
  gate (release-hardening tests, land in v2.0.0). Investigation found the four
  boundary invariants already covered at bootstrap/montecarlo/ci_accuracy/SEM
  entry points, so scope is audit-and-assure + the deferred tidiness riders,
  not a from-scratch suite. Absorbs ROADMAP candidates: boundary-condition
  test suite, SEM test-fixture consolidation (M8 T5 sliver), `is_flag()` sliver
  (M5 close-review remainder), and the `test-RcppExport.R.R` rename.
- 2026-07-12: branch cut, status → in-progress.
- 2026-07-12: T2 done before T1 (minor reorder — the coverage matrix cites the
  new test line numbers). Audit finding: mean-path boundary coverage already
  complete; the two real gaps were the correlation entry point's flat and pole
  corners. Two deterministic tests added, suite green (0 failed, 0 warnings).
- 2026-07-12: all tasks done. `check()` clean (0/0/0). Fixed a T3 regression at
  `test-ssm_sem.R:312` (block reconstructs `lambda1` from `a[1]`/`cc[1]`; those
  locals had been removed by the canonical-pop consolidation). Status → review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (implement): T3 near-miss worth a LESSON at review — the
  `sem_canonical_pop()` consolidation removed a block's `a`/`cc` locals that it
  still referenced (`test-ssm_sem.R:312`, `lambda1 <- c(a[1], ...)`).
  `devtools::load_all()` + `test()`/`test_file()` **masked** the resulting
  `object 'a' not found` (an env leak put `a` in scope); only the clean-env
  `devtools::check()` caught it. Lesson: validate mechanical test-fixture
  rewrites with `check()`, not just `test()`.

## Review
<!-- owner: review · exclusive -->
