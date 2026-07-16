<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M22: Free-engine multi-start nesting seed (T_free ≤ T_unit by construction)

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m22-nesting-seed · https://github.com/jmgirard/circumplex/pull/46

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

- [x] AC1 — Under `scaling = "free"`, the reported optimum satisfies
      F̂_free ≤ F̂_unit (hence T_free ≤ T_unit) on the same input `R`,
      enforced by the unit-solution start; the unit-family code path and
      the bootstrap warm-start path are behavior-unchanged (existing suite
      green, no snapshot drift).
- [x] AC2 — The RR05 boundary_N2000 violating replicate, regenerated from
      its exact seed, satisfies T_free ≤ T_unit + 1e-8 after the change;
      the test is demonstrated red on pre-change code (evidence in the
      work log / Review section).
- [x] AC3 — Oracle suite green at unchanged tolerances: live OpenMx
      free-scaling oracle + frozen Grassi et al. (2010) App. A anchors
      (`tests/testthat/test-cpm_oracles.R`) — the ≥2-independent-oracle-types
      bar for an estimation-code change (validation doctrine).
- [x] AC4 — Roxygen + vignette wording gains the by-construction nesting
      note with no claim extended beyond D-011's scoping.
- [x] AC5 — `devtools::test()` and `devtools::check()` clean
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
- [x] **T3** — Oracle + gate: re-run `test-cpm_oracles.R` (M18 lesson:
      OpenMx `type="cov"` needs the (N−1)/N factor; CSOLNP agrees only to
      ~5e-4) and full `devtools::check()`.
- [x] **T4** — Docs: nesting note in the `scaling` roxygen block
      (`R/cpm_fit.R:1358`) and the vignette passage
      (`evaluating-circumplex-structure.Rmd:164`); `devtools::document()`.
      Plus a NEWS.md entry (consistency-gate requirement; no milestone
      numbers in user-facing text).

## Work log

- 2026-07-16: created by /milestone-plan (promoted from the infra row; RR05
  B2/R5). Gate: M13-D1 stands; top-level seed only; M7 now depends on M22.
- 2026-07-16: T1+T2 done, one green checkpoint (62752f7). Red run recorded:
  pre-fix, the RR05 replicate (seed 20260706+12e7+1e6·1+1e4·3+29) violated by
  +5.52 T-units — RR05's exact max; post-fix 20/20 green. T4: roxygen +
  vignette + NEWS notes, document() clean.
- 2026-07-16: T3 full suite caught 1 FAIL (SE cross-check oracle,
  test-cpm_oracles.R:677) — seed group semantics, not estimates (top-level
  fits bit-identical pre/post; deviant replicates are genuine better-F
  permutation basins natives also find). Fixed: sentinel group 0 (f72e2dc).
- 2026-07-16: T3 done — nesting 20/20, oracles 108/108, check() 0 errors /
  0 warnings / 0 notes. All tasks complete; status → review.

## Decisions

- 2026-07-16 (T3, supersedes the T2 entry): seed gets **sentinel group 0 —
  excluded from `reproduced`**. Own-group was empirically wrong: the seed is
  a warm start (starts ≈ at the free optimum at correlation input), so
  counting it silently accepted fits only ONE data-blind group could reach
  (the 3 SE-inflating replicates each had exactly 1 native group at min F;
  pre-seed commit passes the oracle file 108/108). The seed certifies
  nesting, never reproduction; acceptance semantics match the pre-seed
  engine exactly; the seed run still competes on F and in the multimodality
  comparison. NEWS-documented consequence: a seed-rescued fit now reports
  the better optimum WITH the acceptance warning.
- 2026-07-16 (T2): seed as own independence group in `reproduced` (genuine
  data-derived point, unlike the mirror). Belt: if the seeded run ends above
  its start value, fall back to the seed point (F = F̂_unit bit-identically).
  The group choice was superseded at T3 (above); the belt stands.

## Review

Reviewed 2026-07-16 (/milestone-review, same session as implement). PR #46.
Evidence gathered fresh, post final code commit (f72e2dc):

- **AC1**: test-cpm_nesting.R 20/20 (re-run at review) — nesting on the RR05
  replicate + deterministic battery (variants A, C; unpolished, equal df
  asserted). Unit path and bootstrap warm-start path unchanged: full suite
  green inside check() (all existing pins intact); cpm_bootstrap()
  warm-starts via cpm_optimize_one (R/cpm_fit.R:1180) and never re-enters
  the battery — the seed lives only in cpm_engine().
- **AC2**: red run recorded pre-fix (work log, 62752f7): F_free 0.0069 >
  F_unit 0.0041 (+5.52 T-units — RR05's exact recorded max) on the exact
  seed; green at review (same file, 20/20).
- **AC3**: test-cpm_oracles.R 108/108 at review — live OpenMx free-scaling
  oracle + frozen Grassi App. A anchors, tolerances unchanged (≥2 oracle
  types). The SE cross-check (:677) failed under the first group semantics
  and passes under sentinel-group-0; diagnosis logged in Decisions.
- **AC4**: nesting note added to the scaling roxygen (R/cpm_fit.R) and the
  vignette equivalence passage; wording stays inside D-011's scoping (model
  test, correlation input, measured envelope). document() no diff.
- **AC5**: devtools::check(args = "--no-manual") — 0 errors / 0 warnings /
  0 notes (5m17s), post f72e2dc.

Consistency gate: cairn_validate all pass (after trimming the milestone file
back under the 150-line cap); document() no diff; pkgdown::check_pkgdown()
clean; NEWS.md entry present (no milestone numbers); no new top-level files;
no principle change (cairn_impact skipped).

Independent review (three lenses + scorer): blame-history [S] — clean (M4
mirror-fold invariant preserved; D-011 wording guardrails honored; bootstrap
isolation verified). Prior-PR [S] — no prior-PR evidence (no GitHub-native
review comments exist); cross-checks clean. Diff-bug [O] — deep checks clean
(refactor byte-equivalent, embedding bit-identity verified empirically,
mirror interaction safe); 3 findings. Scorer triage:

- F1 (82, actioned → fixed): the sentinel-group-0 exclusion had no
  deterministic test — pinned only by the stochastic SE oracle. Fixed by
  extracting `cpm_reproduced()` and unit-testing the group-0 filter
  directly (6 cases) + acceptance pins on the clean battery fits
  (platform-robust; deliberately NOT asserting the RR05 fit's accepted
  flag, which rides knife-edge optimizer luck across BLAS builds).
- F2 (42, logged, not actioned): belt fallback would inject a
  non-stationary point into the multimodality comparison — requires the
  never-observed fallback AND a ≤1e-6 F-gap; errs conservative (spurious
  warning at worst).
- F3 (68, sub-threshold but fixed voluntarily — CLAUDE.md vignette-precision
  doctrine, two-line change): "never exceeds — by construction" overstated
  the post-polish guarantee; roxygen/vignette/NEWS now say "beyond numerical
  tolerance" with the polish caveat in the roxygen.

Post-fix: nesting file 34/34; full check() re-run after these edits —
0 errors / 0 warnings / 0 notes (4m 9s, on 768c0b9's tree). CI on the
pre-fix review commit was green across all 7 checks (macOS/Linux×3/Windows/
coverage/pkgdown); final-commit CI gates the merge.
