<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M13: Angle-class S3 follow-ups (RR01)

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m13-angle-class-s3-followups   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Close the small S3-local angle-class follow-ups RR01 surfaced (D-006): a
`new_contrast_radian()` constructor, a normalized all-NA quantile return, an
oracle for the CPM bootstrap angle-CI path, and a recorded keep-internal
decision for the `as_degree`/`as_radian` generics.

## Scope

**In:**
- A `new_contrast_radian()` constructor in `R/ssm_oop.R` replacing the two
  inline `structure(x, class = c("circumplex_contrast_radian", "numeric"))`
  sites (`R/ssm_bootstrap.R:112`, `R/ssm_ci_accuracy.R:899`) — behavior-preserving.
- Normalize the all-NA return of both `quantile.circumplex_radian()` and
  `quantile.circumplex_contrast_radian()` from logical `NA` to `NA_real_`
  (length-1, preserving the `length(qs) == 1` guard at
  `R/ssm_ci_accuracy.R:903`).
- An independent oracle for the CPM bootstrap circular-quantile angle CI
  (`R/cpm_fit.R:1119`), covering the 0/360-straddling case.
- Record the keep-internal decision for the `as_degree`/`as_radian` generics
  (a milestone-local decision + code comment; they stay unexported, methods
  stay S3-registered).

**Out:**
- Exporting/documenting `as_degree`/`as_radian` as public API — decided
  against at the plan gate (keep internal; reversible, no D-entry needed).
- Any vctrs/S7 migration of the angle classes → settled by D-006 (dropped).
- Changing the all-NA return *shape* to length-2 → rejected at the plan gate
  (would break the length==1 consumer guard); stays length-1.
- The pole-snap 0-vs-360 cosmetic (D-003 parked) and analytic-CI Hessian
  recompute → remain the "Continuous/infra refactors" candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — `new_contrast_radian()` exists in `R/ssm_oop.R`; both former inline
      `structure()` sites call it; the produced objects are `identical()` to the
      pre-change objects (byte-identical class + values), and the full existing
      suite (incl. `test-ci_accuracy.R` equality pins and contrast-CI snapshots)
      stays green.
- [ ] AC2 — both `quantile.circumplex_*` methods return `NA_real_` (not logical
      `NA`) on an all-NA input, asserted by a direct test; the flat/zero-variance
      displacement path through `ssm_analyze()`/`ssm_ci_accuracy()` still yields
      NA CIs without error (regression test at the boundary).
- [ ] AC3 — the CPM bootstrap angle CI (`cpm_fit.R:1119`) is verified against an
      independent oracle for a 0/360-straddling displacement, backed by ≥2
      oracle types (invariant agreement with `quantile.circumplex_radian` +
      a live deliberately-dumb circular-quantile recomputation).
- [ ] AC4 — the keep-internal decision for `as_degree`/`as_radian` is recorded in
      this milestone's Decisions section and as a code comment at the generic
      definitions (`R/ssm_oop.R:30,63`); NAMESPACE still exports no such generic.
- [ ] AC5 — `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** — Add `new_contrast_radian()` beside `new_radian()`/`new_degree()`
      in `R/ssm_oop.R` (use `new_s3_num` with class
      `c("circumplex_contrast_radian", "numeric")`); replace the inline
      `structure()` at `R/ssm_bootstrap.R:112` and the `cls` branch at
      `R/ssm_ci_accuracy.R:899`. Verify with `identical()` on both call sites'
      output (default + contrast row-name cases) per the M12 byte-identity idiom.
- [x] **T2** — Change `return(NA)` → `return(NA_real_)` in both quantile methods
      (`R/ssm_bootstrap.R:173,186`); add a direct all-NA return-type test and a
      flat-profile regression test exercising the displacement CI path (test
      first: assert `NA_real_` before the fix).
- [x] **T3** — Add a CPM angle-CI oracle test (in `test-cpm_oracles.R` or a
      dedicated file) for a 0/360-straddling displacement: invariant agreement
      between `cpm_fit()`'s reported `angle_lci/uci` and a direct
      `quantile.circumplex_radian` recompute, plus a dumb explicit
      circular-quantile oracle. Cite the design-sec anchor.
- [x] **T4** — Add a one-line code comment at `as_degree`/`as_radian` marking
      them deliberately internal (generic unexported, methods registered);
      record the decision in this file's Decisions section.
- [x] **T5** — `devtools::document()` (no NAMESPACE change expected) + full
      `devtools::test()` + `devtools::check()`; confirm 0/0/0.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan. Promotes the RR01 "Angle-class S3
  follow-ups" candidate row (lineage: D-006). Gate decisions: keep generics
  internal; all-NA return normalized type-only to NA_real_ (length-1, guard
  intact); CPM angle-CI oracle included in scope. No RB tripwire fires (the
  irreversible-api export path was declined). Targets landing before the
  ~2026-07-26 v2.0.0 freeze.

- 2026-07-12: T1 done — `new_contrast_radian()` added in `R/ssm_oop.R`; both
  inline `structure()` sites routed through it; byte-identity pinned by
  `expect_identical()` in `test-ssm_oop.R`; affected suites (ssm_oop,
  ssm_bootstrap, ci_accuracy) green.

- 2026-07-12: T2 done — both `quantile.circumplex_*` methods now return
  `NA_real_` (length-1) on all-NA input; test-first (red on logical `NA`, green
  after). 166 consumer tests (bootstrap, ci_accuracy, oop, analysis, cpm) green;
  the `ssm_ci_accuracy.R:903` length==1 guard is preserved.

- 2026-07-12: T3 done — new `test-cpm_angle_ci.R` covers the CPM angle-CI
  transform (`cpm_fit.R:1119-1121`) at a 0/360 straddle with two independent
  oracle types (live dumb circular-quantile-in-degrees recompute + rotation-
  equivariance invariant), plus an end-to-end `cpm_fit()` bootstrap guard
  (estimate-within-CI-on-circle + short-arc width) that fails on linearization.
  Verified running (not just skipped) under `NOT_CRAN=true`.

- 2026-07-12: T4 done — keep-internal decision recorded (M13-D1 below) + code
  comments at the `as_degree`/`as_radian` generics; NAMESPACE exports no generic
  (verified); load_all clean.

- 2026-07-12: T5 done — `document()` produced no NAMESPACE/man changes; full
  suite 392 tests 0F/0E/0S (3 expected degenerate warnings); `devtools::check()`
  clean (0 errors / 0 warnings / 0 notes). All tasks complete → status review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (M13-D1): `as_degree`/`as_radian` stay **deliberately internal** —
  the generics are not exported (methods remain S3-registered). Chosen at the
  plan gate over promoting them to a documented public converter API: fits the
  minimal-API / base-R doctrine, adds no API-maintenance commitment, and is
  reversible. Not cross-cutting enough for a D-entry (it changes no exported
  surface); recorded here + as a code comment at the generic definitions
  (`R/ssm_oop.R`). Reopen only if a public deg<->rad converter is wanted.

## Review
<!-- owner: review · exclusive -->
