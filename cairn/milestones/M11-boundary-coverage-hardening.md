<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M11: Boundary-coverage hardening + test-suite tidiness

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [ ] **T1** — Audit boundary coverage and author the matrix (4 classes × 6
      entry points) with `file:line` citations, in a `cairn/` reference file.
      Read the existing boundary tests in `test-ssm_bootstrap.R`,
      `test-ssm_montecarlo.R`, `test-ci_accuracy.R`, `test-ssm_sem.R`,
      `test-ssm_analysis.R` before writing cells.
- [ ] **T2** — Add tests for any genuine gap (suspected: a `ssm_analyze()`
      point-estimate at the exact 0°/360° pole at the user entry point — verify
      against the bootstrap/ci_accuracy pole tests before assuming it is
      missing). Assertions at the boundary value, per the validation doctrine.
- [ ] **T3** — Add `sem_canonical_pop()` to `helper-ssm-sem.R`; grep the whole
      `sem_pop(` canonical-population family across `test-ssm_sem*.R` (do not
      trust a fixed site count) and route each canonical rebuild through it;
      confirm bit-identical population via `identical()` (M9 lesson: no
      stochastic rerun needed).
- [ ] **T4** — Replace `is.logical(items) && length(items) == 1` at
      `R/instrument_oop.R:68` with `is_flag(items)`; keep/confirm the
      invalid-`items` error-path test.
- [ ] **T5** — `git mv tests/testthat/test-RcppExport.R.R
      tests/testthat/test-RcppExports.R`.
- [ ] **T6** — `devtools::document()` (if any roxygen touched),
      `devtools::test()`, then `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan. Theme + timing chosen at the plan
  gate (release-hardening tests, land in v2.0.0). Investigation found the four
  boundary invariants already covered at bootstrap/montecarlo/ci_accuracy/SEM
  entry points, so scope is audit-and-assure + the deferred tidiness riders,
  not a from-scratch suite. Absorbs ROADMAP candidates: boundary-condition
  test suite, SEM test-fixture consolidation (M8 T5 sliver), `is_flag()` sliver
  (M5 close-review remainder), and the `test-RcppExport.R.R` rename.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
