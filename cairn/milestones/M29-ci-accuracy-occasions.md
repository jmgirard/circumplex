# M29: `ssm_ci_accuracy()` occasions extension

- **Status:** review
- **Priority:** normal
- **Depends on:** M25
- **Principles touched:** —
- **Branch/PR:** m29-ci-accuracy-occasions · [PR #53](https://github.com/jmgirard/circumplex/pull/53)

## Goal

Replace `ssm_ci_accuracy()`'s occasions error guard with a correct
occasions-aware simulation path that respects within-person cross-occasion
dependence (stacked Monte Carlo population, spec §2.2), validated by a full
§2.3-style oracle battery.

## Scope

**In:**
- Design + implement the occasions plug-in population (§2.2, [[D-017]]): draw
  wide person-rows from the per-group stacked person-level score covariance
  (cross-occasion dependence in the off-diagonal p×p blocks), re-run
  `ssm_analyze(occasions = )` per simulated dataset, and report per-occasion
  CI-accuracy rows plus the paired-contrast row (when `contrast = TRUE`).
- Remove the `details$occasions` error guard
  ([R/ssm_ci_accuracy.R:197](R/ssm_ci_accuracy.R:197)); replace the
  informative-error test with the new run-don't-error contract.
- Full oracle battery (§2.3, amended per [[D-017]]): simulation-coverage + a
  width-based discrimination arm (zeroed-cross-blocks ≡ independent-groups
  diagnostic) + a closed-form Δe width target. Seeded, cell-indexed by level
  (LESSONS 2026-07-13), committed `devel/m29-*-results.rds` + regeneration
  script with pre-registered acceptance in the header.
- NEWS entry (exported behavior change: error → runs).

**Out:**
- occasions × measures ci_accuracy and occasions × contrast × grouping →
  remain ROADMAP candidate rows (their own design).
- Pairwise-deletion occasions (§1.3) untouched — listwise-only stands on the
  RR06 R6 estimand grounds; not reopened here.

## Acceptance criteria

- [x] **AC1** — `ssm_ci_accuracy()` on a ≥2-occasion object returns a valid
      accuracy object whose simulation population is the stacked cross-occasion
      covariance (off-diagonal within-person blocks non-zero), **not** the
      dependence-ignoring per-group sufficient statistics; asserted
      structurally on a fixture.
- [x] **AC2** — simulation-coverage oracle: across occasions populations with
      known cross-occasion dependence, the diagnostic's reported coverage
      tracks the true empirical coverage within a pre-registered tolerance, at
      ≥1 interior cell plus a boundary cell (pole-straddling or
      near-zero-amplitude occasion), with ≥1 cell exercising the paired-contrast
      row; seeded `devel/m29-*-results.rds` + regeneration script committed.
      (RB tripwire resolved: RR07 → [[D-017]])
- [x] **AC3** — a discriminating oracle beyond coverage: coverage alone is
      provably blind to a dependence-dropping population (the adaptive replayed
      procedures cover at nominal even from a wrongly-independent population —
      RR07/[[D-017]]), so the discriminating observable is interval width. Two
      arms: **(invariant)** an occasions run with the cross-occasion blocks
      zeroed reproduces the already-validated two-group independent-groups
      diagnostic (same marginals) on both coverage and `Median_width` within a
      pre-registered SE-based band; **(closed-form)** the dependent-vs-zeroed
      paired-contrast `Median_width` ratio matches the closed-form Δe width
      target √(w′Σw / w′Σ₀w), with a reversal-side Δd cell (|Δd| > 90°)
      expecting the paired-wider reversal. Meets the ≥2-independent-oracle-types
      bar (simulation-coverage + invariant + closed-form).
- [x] **AC4** — boundary contract per CLAUDE.md and [[D-017]]: a
      flat/zero-variance occasion is refused up front with an error naming the
      occasion (the shipped flat-profile refusal extended row-wise); a
      pole-straddling occasion produces a correctly-wrapped, non-erroring
      diagnostic; a near-zero-amplitude occasion runs, flags its `Structural`
      rows, and reports certification honestly; all tested.
- [x] **AC5** — the removed error guard's test is updated to the new contract;
      the exported behavior change is documented (NEWS, roxygen);
      `devtools::test()` clean and `devtools::check()` clean (0 errors /
      0 warnings; NOTEs justified).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T2, T3
- AC5 → T2, T5

## Tasks

- [x] **T1** — Design note: how the stacked cross-occasion covariance (§2.2)
      becomes the diagnostic's plug-in simulation population, and how
      per-occasion + contrast accuracy rows are reported. Written before code,
      grounded in [[D-017]] (RR07: construction (a), per-group observed stacked
      covariance, no CPM; `occ_k`-tagged storage; rank-deficiency
      warn-not-refuse; `(r−1) %/% k + 1` row↔group mapping).
- [x] **T2** — Implement the occasions path in `R/ssm_ci_accuracy.R`: remove
      the guard (:197), wire the stacked-MC population + wide-row simulation +
      per-occasion/contrast accuracy rows. Test-first (regression fixture
      asserting the correct population structure + boundary behavior).
- [x] **T3** — Simulation-coverage oracle:
      `devel/m29-ci-accuracy-occasions-oracle.R` + committed
      `devel/m29-*-results.rds`, seeded, cell-indexed by level, smoke-first;
      interior + boundary cells; pre-registered acceptance in the script
      header; asserting test with a provenance comment.
- [x] **T4** — Discrimination oracle (§2.3.2 + RR07): the zeroed-cross-blocks
      occasions run ≡ the two-group independent diagnostic on coverage +
      `Median_width` (SE band); plus the dependent-vs-zeroed contrast
      `Median_width` ratio vs the closed-form Δe width target and a
      reversal-side Δd cell. Committed script + results, asserting test.
- [x] **T5** — NEWS.md entry (error → runs behavior change); update the
      informative-error test; `devtools::document()`; `devtools::check()`
      clean.

## Work log

- 2026-07-17: created by /milestone-plan (deferral §1.4; sibling M28 §1.1);
  implement → in-progress, branch cut. Gate: storage settled (stacked per-group
  stats at analysis time); T1 ip-touching → RB07; blocked; ingested RR07 (Fable)
  → M29-D1 + [[D-017]], archived. Amendment gate (approved): AC2/AC3/AC4 +
  T1/T4 synced. T1 done — `devel/m29-design.md` blueprint per [[D-017]].
- 2026-07-17: implement gate → M29-D2 (R12 add; shared-W bootstrap replay).
- 2026-07-17: T2 done — occasions branch in `ssm_ci_accuracy.R` (stored stacked
  `(n,μ,Σ)`; guard removed; `MVN(μ,Σ̂_g)` via `mvn_root`; shared-W bootstrap +
  `ssm_mc_replicates(occ_k=)` MC; flat/rank/structure/legacy refusals;
  occasions-aware `summary()` + R12). AC1/AC4 tests; `test()` clean.
- 2026-07-17: T3 done — AC2 simulation-coverage oracle (`m29-*-oracle.{R,md}` +
  rds): diagnostic-reported vs direct empirical coverage at the plug-in; 27/27
  within the 4-SE band.
- 2026-07-17: T4 done — AC3 discrimination oracle (`m29-*-discrimination.{R,md}`
  + rds): invariant (zeroed-occasions ≡ two-group ref) + closed-form Δe width
  √(w′Σw/w′Σ₀w) <1% + Δd reversal sign; ≥2 oracle types met.
- 2026-07-17: T5 done — NEWS (error→runs); roxygen occasions Limitations +
  refusal note; `document()`; `check()` clean (0/0/0).
- 2026-07-17: review (PR #53) — AC4 gap (near-zero/c=0 Structural untested) sent
  back + test added; `test()` clean (307 pass).

## Decisions

### M29-D1 (2026-07-17): RR07 ingested — occasions population design settled

Design promoted to [[D-017]] (R1–R11 encoded in the amended AC2/AC3/AC4 + T1/T4
at the 2026-07-17 amendment gate; standing rejects R13 `data=` fallback, R14
CPM-diagonal+observed-cross, R15 shrinkage, R16 cross-group pooling all recorded
there). R12 (`summary()` joint-cert breadcrumb) resolved in M29-D2 below.

### M29-D2 (2026-07-17): R12 breadcrumb added; occasions bootstrap replay reuses the weight machinery

Settled at the implement gate. **R12 (add):** `summary()` notes when a paired
contrast's c=1 joint-certification rate (`cert[T1]&&cert[T2]`) is below `1−α` —
the §2.2 caveat made legible; gates nothing. **Bootstrap replay:** reuse the
weight path with one shared person-resample `W` across all k occasion blocks
(same law as the person-row bootstrap); MC reuses `ssm_mc_replicates(occ_k=)`.

## Review

_2026-07-17, PR #53 (branch cut from master @ 8dd2806; master unmoved, no merge needed)._

### Acceptance-criteria evidence (fresh)

- **AC1** ✓ — test "occasions ci_accuracy population is the stacked cross-occasion
  covariance (AC1)" (`test-ssm_occasions.R`): on a dependent fixture (ρ=0.7) the
  stored `suff_stats$groups$cov` is 16×16 with same-scale cross-occasion diagonal
  mean > 0.3, vs < 0.15 for an independent (ρ=0) fixture; the diagnostic runs,
  `structure=="observed"`, `occ_k==2`, `cpm` NULL. Passes.
- **AC2** ✓ — `devel/m29-ci-accuracy-occasions-oracle.R` (committed rds,
  `smoke=FALSE`, R1=1000/R2=800/boots=300). Diagnostic reported coverage vs direct
  empirical coverage of the real `ssm_analyze` procedure at the plug-in, cells
  interior (both engines) + pole (bootstrap), every cell exercising the contrast
  row: **27/27** within the pre-registered 4-SE band, max |Δ| 0.032. Pinned by the
  asserting test; provenance in the `.md`. Passes.
- **AC3** ✓ — `devel/m29-ci-accuracy-occasions-discrimination.R` (committed rds,
  `smoke=FALSE`, reps=1000/boots=400). (invariant) zeroed-occasions ≡ two-group
  reference on contrast coverage (4-SE) + `Median_width` ([0.90,1.11]);
  (closed-form) dependent/zeroed Δe width ratio = √(w′Σw/w′Σ₀w) to <1% (0.701 vs
  0.697; 0.725 vs 0.727); (reversal) Δd width ratio 0.69 at Δd=40 → 1.13 at Δd=135.
  ≥2 oracle types met. Pinned by the asserting test. Passes.
- **AC4** ✓ — boundary tests (`test-ssm_occasions.R`): flat occasion refused by
  name ("T2"); pole-straddling occasion → finite displacement coverage, both
  engines; rank-deficient (n≤k·p) warns not refuses + records `deficient`;
  near-zero/c=0 rung flags occasion amplitude rows `Structural` with zero coverage
  and reports certification honestly; a near-zero-amplitude occasion runs. All
  pass (test added at review to close a discovered gap — see work log).
- **AC5** ✓ — removed-guard test rewritten to the run-don't-error contract (T2);
  NEWS + roxygen (occasions Limitations sibling, `structure`/`cpm` refusal notes);
  `devtools::document()` no diff. `devtools::check()` **Status OK — 0 errors / 0
  warnings / 0 notes** (fresh, incl. the AC4 test). Full suite FAIL 0 / PASS 2620.

### Consistency gate

- Universal cairn checks: `cairn_validate.py` **all checks passed** (coverage
  complete, weight caps, mirror agreement, one-in-progress, ISO dates, …).
  No DESIGN principle changed → `cairn_impact` skipped.
- Toolchain (`r-package` profile): `document()` no diff; generated files
  unchanged (only `man/ssm_ci_accuracy.Rd` regenerated); README/pkgdown unaffected
  (no new exports); NEWS entry present; no new top-level files; `check()` 0/0/0.

### Independent fresh-context review (three lenses + scorer)

- **[O] diff-bug (Opus):** no real defects. Traced column/row ordering
  (occasion-major covariance vs group-major/occasion-minor profile rows) consistent
  end-to-end, contrast = T2−T1 on all three paths, shared-W bootstrap reproduces
  the person-row case bootstrap, plug-in vs interval covariances correctly
  distinguished, rank threshold correct, R12 reads the right quantity, classic path
  untouched. Two benign non-findings noted.
- **[S] blame-history (Sonnet):** no findings. The `suff_stats=NULL`→stacked-stats
  reversal is correct (not a resurrected M25 hazard); classic path behaviorally
  untouched; D-017/D-007/M15-D1/D-003 honored. One non-finding: the `@return`
  "no display consumes" phrasing sits near the new breadcrumb but is not
  contradicted (coverage-table conditional column vs guardrail `Cert_rate` — distinct
  objects).
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — every merged PR
  touching these files carries zero GitHub review comments (solo-maintainer; the
  review record lives in the cairn RB/RR archive). Clean no-op, zero findings.

**Actioned findings: none.** No lens produced a finding ≥80 (none produced any
finding at all), so the scorer step is a no-op. The lone doc-clarity non-finding
is **rejected**: the docstring is technically accurate (the coverage table's
`Coverage_conditional` column genuinely has no display consumer; the breadcrumb
reads the distinct `guardrail$Cert_rate`), and editing it risks introducing
inaccuracy.
