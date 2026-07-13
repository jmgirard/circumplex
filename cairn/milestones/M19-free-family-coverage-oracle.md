<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M19: CIRCUM free-scaling — analytic-CI coverage oracle + caution calibration

- **Status:** review
- **Priority:** high
- **Depends on:** M18
- **Principles touched:** —
- **Branch/PR:** m19-free-family-coverage-oracle · [PR #43](https://github.com/jmgirard/circumplex/pull/43)

## Goal

Measure the empirical coverage of the free-scaling family's analytic (Wald)
confidence intervals via a simulation-coverage oracle, and replace M18-D3's
unconditional caution with the coverage-validated statement the data support.

## Scope

**In:**
- Extend `devel/m4-coverage-oracle.R` with **free-scaling** cells: circumplex-
  **correlation** truths P(γ) (σ_pop = 1 — the free family fits σ as free
  nuisance parameters), `scaling = "free"`, measuring **analytic-CI coverage for
  θ/ζ/β** at nominal 95% — boundary + interior β at N ∈ {250, 1000} + an
  analytic-only N-ladder spot with a Heywood-prone cell (spec §6). Seeded,
  committed `.rds`. Collect `T_free = n·F̂` per replicate (enables the deferred
  T_diag-vs-T_free comparison without a re-run).
- Second oracle type: on one spot cell, cross-check the FD-Hessian analytic SEs
  against an independent **live** source (parametric-bootstrap SE), meeting the
  ≥2-oracle-types bar.
- Record the coverage in `cairn/DESIGN.md` (table + what-it-decides, mirroring
  the M4/B6 subsection; spec §8).
- Replace the unconditional M18-D3 caution (`R/cpm_oop.R`) and the "not yet
  coverage-validated" roxygen (`R/cpm_fit.R`) with the coverage-validated
  statement the measurements support; DECISIONS.md entry superseding M18-D3.
- In-suite asserting smoke test + register the oracle (id/type/test/source).

**Out:**
- **Bootstrap θ/ζ/β coverage** for the free family → grouped free-scaling candidate.
- **Bootstrap σ CIs on the raw-data path** → candidate (2), deferred (spec §9).
- **The T_diag-vs-T_free inference-default decision** (stats emitted here) → candidate (3).
- **Covariance-matrix input** → candidate (4), rejected for now (RR04 rec 12).
- **`m4-browne-design.md` §3.2 rewrite** — already handled at M18 doc time.

## Acceptance criteria

- [x] `devel/m4-coverage-oracle.R` runs free-scaling cells (circumplex-
      correlation truths, σ_pop = 1; boundary + interior β at N ∈ {250, 1000} +
      ≥1 analytic-ladder spot including a Heywood-prone cell), seeded and
      reproducible, with committed `.rds` results; `T_free = n·F̂` collected per
      replicate. Source: `devel/circum-free-scaling-spec.md` §6.
- [x] Measured analytic-CI coverage for θ/ζ/β under `scaling = "free"` is
      recorded in `cairn/DESIGN.md` as a coverage table + what-it-decides prose
      (mirroring the M4/B6 subsection). Source: spec §8.
- [x] The analytic-CI coverage numbers are backed by **≥2 independent oracle
      types** — simulation-coverage (primary) + a live SE cross-check on a spot
      cell (FD-Hessian SE vs. OpenMx/parametric-bootstrap SE) — and the oracle is
      registered (id, type, asserting `test:line`, source). Source: tracking-rules
      "Validation doctrine" (≥2-types bar; a CI method's oracle is coverage).
- [x] The free-family `summary()` caution is coverage-validated: `R/cpm_oop.R`'s
      unconditional free branch and `R/cpm_fit.R`'s "not yet coverage-validated"
      roxygen are replaced by the statement the measurements support (new
      constants if N-conditional, or a justified unconditional caution), recorded
      as a DECISIONS.md entry superseding M18-D3's deferral; a test pins the
      new caution wording (per the diag-caution convention at `test-cpm_api.R` —
      `expect_match`, not a snapshot; analytic CI endpoints are BLAS-sensitive).
      Source: spec §4 (lines 159-163); M18-D3.
- [x] An in-suite asserting smoke test (small seeded reps, in-band coverage
      assertion, `skip_on_cran()`) exercises the free-family coverage path;
      `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1, T2
- AC2 → T4
- AC3 → T3, T5
- AC4 → T5, T6
- AC5 → T5, T6

## Tasks

- [x] **T1** — Extend `devel/m4-coverage-oracle.R`: reuse M4's circumplex-
      correlation truth configs (boundary/interior β at octant angles, ζ = .75),
      draw X ~ N(0, P(γ)) via `chol(P0)`, and add an analytic-only free-cell
      coverage pass fitting `cpm_engine(cor(X), scaling = "free")` + scoring θ/ζ/β
      via `cpm_analytic_se()` (already returns the σ-free bordered-Hessian SEs),
      with per-replicate `T_free` collection. Seeded/reproducible; smoke env var.
- [x] **T2** — Run the full seeded free-family oracle; commit the `.rds`
      results (analytic-only ⇒ session-runnable, no per-fit bootstrap).
- [x] **T3** — Interpret the coverage table: decide N-conditional vs. justified-
      unconditional caution for the free family; do **not** silently reuse the
      diag constants (spec §4). Draft the milestone-local decision.
      **Outcome:** N-conditional — the diag thresholds (2000/50000) are the
      correct free thresholds, now coverage-validated (not silently reused);
      small-N SE-fragility reinforces the N<2000 caution. → D-010 (T5).
- [x] **T4** — Record the measured coverage in `cairn/DESIGN.md` (new subsection
      mirroring "CPM confidence intervals: measured coverage"); update the CPM-CI
      section prose.
- [x] **T5** — Update `R/cpm_oop.R` free caution branch + `R/cpm_fit.R` roxygen
      to the validated statement (new free-family constants if N-conditional);
      add the in-suite coverage smoke test + register the oracle; update the
      caution-wording snapshot. Promote the T3 decision to DECISIONS.md
      (supersedes M18-D3's deferral).
      (RB tripwire: no-oracle — the coverage oracle IS the oracle here, so this
      is *not* a tripwire hit; noted to pre-empt a false escalation.)
- [x] **T6** — Live SE cross-check on one spot cell (parametric-bootstrap SE vs.
      FD-Hessian SE — self-contained, no OpenMx parameterization-matching);
      asserts agreement in the suite. Run `devtools::check()`; confirm 0/0/0.

## Work log

- 2026-07-13: created by /milestone-plan. Promotes free-scaling candidate (1); lineage D-009 finding (5) + M18-D3. Gate: analytic-CI coverage only, emit T_free (defer decision), live SE cross-check as 2nd oracle, M19 blocks M7. Machinery pre-exists — runs + calibrates.
- 2026-07-13: AMENDED (correctness, no gate) — AC1/Scope/T1 "non-unit σ truths" → circumplex-correlation truths (σ_pop=1): the engine only ever fits a unit-diagonal R (no covariance-input path), so σ is a free nuisance = 1 only at perfect fit. Deliverable/acceptance unchanged.
- 2026-07-13: T1–T2 done — stage 3 added to `devel/m4-coverage-oracle.R` (`CPM_COV_FREE_ONLY=1`); full 500-rep run → `devel/m19-free-coverage-results.rds`. Coverage tracks the diag family (σ̂≈1): interior in-band at N=2000, boundary near N=50000; NEW: σ-Hessian singular in ~52–55% of N=250 fits, ~0% at N≥2000.
- 2026-07-13: T3–T6 done — recorded the coverage subsection in `cairn/DESIGN.md`; merged free into the shared N-conditional caution in `R/cpm_oop.R` (D-010; M18-D3 removed) + σ² note; `cpm_fit.R` roxygen/comment; tests: free caution-wording (closes an M18 gap) + coverage smoke + parametric-bootstrap SE cross-check (O-M19-cov/O-M19-se). Suite 422, 0 fail; check 0/0/0.
- 2026-07-13: AMENDED (gated, minor) — AC4 "snapshot" → "test pins the wording" (repo's diag-caution convention uses `expect_match`, not a BLAS-sensitive snapshot). Deliverable unchanged. Status → review.
- 2026-07-13 (review): fixed the diff-bug finding — free-stage seed used `1e3*N` (not an index), colliding the (boundary,N=2000)/(interior,N=1000) streams; now `1e3*match(N, all_ns)`. Re-ran the oracle; refreshed rds + DESIGN table/ladder + D-010 numbers. Conclusion unchanged; suite/check unaffected (test seeds independent of the oracle formula).

## Decisions

- **M19-D1 (parametric bootstrap for the SE cross-check):** the 2nd (live)
  oracle uses a parametric bootstrap (refit free on data drawn from the fitted
  Σ̂), not OpenMx SEs — OpenMx's (θ_raw, w, s) parameterization would need a
  delta-method map to natural θ/ζ/β (error-prone, version-dependent). The
  bootstrap is FD-Hessian-independent, base-R, deterministic. Part of D-010.

## Review

**Reviewed 2026-07-13 · PR #43.**

### Acceptance-criteria evidence (fresh)
- **AC1** — rds: 12 cells (boundary/interior × N∈{250…50000}), 500 reps, `scaling="free"`, per-cell `ks_T` present; stage 3 env-gated + seeded. ✓
- **AC2** — DESIGN.md M19 coverage subsection (table + ladder + 4 bullets); matches the committed rds to printed decimals. ✓
- **AC3** — O-M19-cov (sim-coverage) + O-M19-se (live parametric-bootstrap SE) registered; both asserting tests pass fresh. ✓
- **AC4** — free branch folded into the shared N-conditional caution + σ² note; roxygen updated; D-010 supersedes M18-D3; `test-cpm_api.R` pins the wording (mis-cover at N<2000, absent at N≥2000 clean, retired wording gone). ✓
- **AC5** — `test-cpm_api.R`+`test-cpm_oracles.R` fresh 0 fail/err/skip; `devtools::check(--no-manual)` **0/0/0**. ✓

### Consistency gate
- `cairn_validate.py` all checks pass. Coverage-complete: every AC → existing tasks. No principle change → `cairn_impact` skipped. r-package gate: check 0/0/0; suite 422 tests, 0 fail/err.

### Independent review (3 lenses + scorer)
- **[O] diff-bug (Opus):** 1 finding — seed collision in `run_free_cell` (`1e3*N` couples the (boundary,N=2000)/(interior,N=1000) streams). **Scored 85** (real, trivially fixable; marginal coverage unbiased so the conclusion held). **Fixed** (`1e3*match(N, all_ns)`) + re-run + refreshed rds/DESIGN/D-010. Reviewer verified correct: chol→Cov=P0, cross-stage seed disjointness, reference-drop indexing, conditioning counted, KS/df, all four caution cases (incl. unchanged diag path), SE cross-check like-for-like.
- **[S] blame-history:** 0 findings — "legitimate, decision-backed; undoes nothing." D-010 properly supersedes M18-D3; σ-no-CI invariant (D-009) preserved; diag path untouched.
- **[S] prior-PR-comments:** no prior-PR evidence (findings live in commit messages here) — clean no-op, 0 findings.
- **Sub-threshold (<80):** none. **Actioned (≥80):** 1 — fixed.
