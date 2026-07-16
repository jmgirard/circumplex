<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M22: Free-engine multi-start nesting seed (T_free ≤ T_unit by construction)

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m22-nesting-seed

## Goal

Seed `cpm_fit()`'s free-scaling multi-start battery with the accepted
unit-family solution so the nesting property T_free ≤ T_unit holds by
construction, eliminating the optimizer-tail violations RR05 measured
(3/5,751, worst +5.52 ≈ 0.55·df at boundary_N2000).

## Scope

**In:**

- Under `scaling = "free"`, the **top-level** fit's start battery
  (`R/cpm_fit.R:600–630`) gains one extra start: the accepted unit-family
  solution packed into the free spec with σ = 1 (`cpm_pack()`'s default),
  obtained by an internal unit-family fit of the same `R`. Its own
  independence group; interaction with the `reproduced` acceptance
  criterion (≥2 independent groups at min F, `R/cpm_fit.R:699`) and the
  multimodality flag (`:637–650`) resolved and recorded as a
  milestone-local decision. Source: RR05 B2 / recommendation 5 (2026-07-16).
- Regression test regenerating RR05's nesting-violating boundary_N2000
  replicate from its exact seed (machinery: `devel/m21-t-calibration.R`),
  proven to fail before the fix (M13 guard-teeth rule).
- One-line by-construction nesting note in the `scaling` roxygen
  (`R/cpm_fit.R:1358` block) and the vignette equivalence passage
  (`vignettes/evaluating-circumplex-structure.Rmd:164`), staying inside
  D-011's claim scoping (model test, correlation input, measured envelope).

**Out:**

- Bootstrap-replicate warm starts unchanged — per-replicate nesting is not
  enforced (declined at the 2026-07-16 plan gate: nothing downstream
  compares per-replicate T cross-family; enforcing it would roughly double
  free-family bootstrap cost). Not planned anywhere; revisit only if a
  cross-family per-replicate consumer appears.
- Variant-C smoke run of the paired T calibration (RR05 rec 6) → rides the
  D-011 re-trigger inside any future covariance-input milestone.
- Analytic-CI Hessian recomputation → stays in the ROADMAP infra
  candidate row.
- `as_degree`/`as_radian` export status → already decided, M13-D1 (kept
  deliberately internal); reconfirmed standing at the 2026-07-16 plan gate.

## Acceptance criteria

- [ ] AC1 — Under `scaling = "free"`, the reported optimum satisfies
      F̂_free ≤ F̂_unit (hence T_free ≤ T_unit) on the same input `R`,
      enforced by the unit-solution start; the unit-family code path and
      the bootstrap warm-start path are behavior-unchanged (existing suite
      green, no snapshot drift).
- [ ] AC2 — The RR05 boundary_N2000 violating replicate, regenerated from
      its exact seed, satisfies T_free ≤ T_unit + 1e-8 after the change;
      the test is demonstrated red on pre-change code (evidence in the
      work log / Review section).
- [ ] AC3 — Oracle suite green at unchanged tolerances: live OpenMx
      free-scaling oracle + frozen Grassi et al. (2010) App. A anchors
      (`tests/testthat/test-cpm_oracles.R`) — the ≥2-independent-oracle-types
      bar for an estimation-code change (validation doctrine).
- [ ] AC4 — Roxygen + vignette wording gains the by-construction nesting
      note with no claim extended beyond D-011's scoping.
- [ ] AC5 — `devtools::test()` and `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4
- AC5 → T3

## Tasks

- [x] **T1** — Tests first: regression test regenerating the RR05
      boundary_N2000 violating replicate from its exact seed, asserting
      T_free ≤ T_unit + 1e-8 (red against current code — record the red
      run); plus a small deterministic nesting property battery (a few
      seeds × variants A and C). Runtime-budget with `skip_on_cran()` if
      needed; remember bare `Rscript` needs `NOT_CRAN=true` (M16 lesson).
- [x] **T2** — Implement the seed in `cpm_engine()` (`R/cpm_fit.R`):
      when `scaling == "free"`, run the unit-family fit internally
      (unit spec, existing battery machinery via the factored
      `cpm_start_set()`) and append its winning solution — block-exact
      embedding, s block = 0 — as one extra start in its own independence
      group. Resolve and record the `reproduced`/multimodality-flag
      interaction (milestone-local decision). T1 goes green; unit-path
      behavior untouched.
- [ ] **T3** — Oracle + gate: re-run `test-cpm_oracles.R` (M18 lesson:
      OpenMx `type="cov"` needs the (N−1)/N factor; CSOLNP agrees only to
      ~5e-4) and full `devtools::check()`.
- [x] **T4** — Docs: nesting note in the `scaling` roxygen block
      (`R/cpm_fit.R:1358`) and the vignette passage
      (`evaluating-circumplex-structure.Rmd:164`); `devtools::document()`.
      Plus a NEWS.md entry (consistency-gate requirement; no milestone
      numbers in user-facing text).

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the infra candidate
  row; source RR05 B2/R5). Plan-gate decisions: M13-D1 export status stands;
  seed applies to the top-level fit only; M7 re-pointed to depend on M22.
  Of the three items in the original ask, only the nesting seed remained —
  the RR01 S3 follow-ups (incl. the export decision, M13-D1) had already
  shipped in M13 (PR #37, squash 95936f2); the stale infra-row clause
  re-added by the 2026-07-16 candidate cleanup was struck.

- 2026-07-16: T1+T2 done (one checkpoint: red test committed with the fix so
  every branch commit keeps the suite green). Red run recorded: pre-fix, the
  RR05 replicate (seed 20260706+12e7+1e6·1+1e4·3+29, identified by a 500-rep
  scan of the boundary_N2000 cell) failed with F_free 0.0069 > F_unit 0.0041
  (diff ×(N−1) = +5.52 T-units, matching RR05's recorded max exactly);
  post-fix the file is 20/20 green. T4 done: roxygen + vignette + NEWS
  nesting notes, document() clean.

- 2026-07-16: T3 first full-suite run caught a real interaction (1 FAIL:
  free-family SE cross-check, test-cpm_oracles.R:677) — diagnosed to the
  seed's group semantics, not the oracle or the estimates (top-level fits
  bit-identical pre/post; deviant replicates are genuine better-F
  permutation basins found by native starts too). Fixed via sentinel group
  0 (see Decisions); NEWS refined; re-running nesting + oracle files.

## Decisions

- 2026-07-16 (T3, supersedes the T2 entry below): the seed gets **sentinel
  group 0, excluded from the `reproduced` acceptance count**. The own-group
  choice below was empirically wrong: the full suite caught the free-family
  SE cross-check oracle failing (ratio 0.34 vs > 0.7) because the seeded run
  — a warm start that at correlation input begins essentially at the free
  optimum — counted as the second "independent" group, silently accepting
  replicates whose optimum only ONE data-blind group could reach (verified:
  the 3 inflating replicates each had exactly 1 native group at min F, and
  the pre-seed commit passes the oracle file 108/108). The seed certifies
  nesting, never reproduction; acceptance semantics now match the pre-seed
  engine exactly. The seeded run still competes on F and participates in the
  multimodality comparison. Consequence (NEWS-documented): a fit whose
  better optimum only the seed reaches now reports that optimum WITH the
  acceptance warning, where pre-seed it reported a worse optimum silently.
- 2026-07-16 (T2): the unit-solution seed gets its **own independence group**
  in the `reproduced` acceptance criterion — it is a data-derived point (the
  unit objective's optimum), not an F-isometric image of g0 like the mirror,
  so its run reaching min F is genuine reproduction evidence. Multimodality
  flag: the seeded run is compared like any other; landing distinct-but-
  competitive correctly fires the flag. Belt: if the seeded run's final F
  ever exceeds its start value (PORT is monotone in practice but not by
  contract), the engine falls back to the seed point itself, whose objective
  equals F̂_unit bit-identically (block-exact embedding, s = 0) — nesting is
  unconditional.

## Review
