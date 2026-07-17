# M25: Longitudinal Build A — occasions API + paired contrasts

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m25-occasions-core` · PR #49 (https://github.com/jmgirard/circumplex/pull/49)

## Goal

Add wide-format `occasions` analyses to `ssm_analyze()` — per-occasion
mean-based profiles (k ≥ 2, crossable with grouping) and the paired
two-occasion contrast through both engines — oracle-validated and
output-surface complete per the binding D-013 spec
(`devel/longitudinal-ssm-spec.md` §§1–2, §7 Build A).

## Scope

**In:**
- `occasions` argument per spec §1.1: named list of same-length blocks,
  wide person-rows, mutually exclusive with `scales` (its `stopifnot`
  becomes conditional); k ≥ 2 (plan gate); default labels `T1..Tk`.
- Stem-matching cross-occasion alignment validation + one-time positional
  message when no stem is detectable (spec §1.1, the rotation channel).
- Listwise-only for occasions (pairwise + occasions errors with the estimand
  message); dropped-persons count messaged; selection-bias docs caution
  (spec §1.3).
- Composition cells (spec §1.2): occasions × mean-based × {1 group,
  grouping}; occasions × `contrast = TRUE` (exactly 2 occasions, 1 group);
  contrast order = `names(occasions)` list order (never alphabetical);
  occasion-major rows; conditional `Occasion` column; details metadata.
- Bootstrap engine: wide person-rows through the existing row resampler,
  contrast via `param_diff(occ2, occ1)` (spec §2.1). Monte Carlo engine:
  stacked k·p mean vector, sample covariance of stacked person vectors / n,
  joint MVN draws, per-block transform, contrast (spec §2.2).
- Full oracle battery per spec §2.3, incl. the ρ > 0 / Δd ≈ 135° reversal
  cell measuring the conditional efficiency identities, small-n (≈25–50) and
  k = 3 cells, degenerate-dependence invariant (re-paired persons reproduce
  the independent contrast), pre-registered SE-based boot-vs-MC tolerance,
  closed-form paired Δe check.
- Output surface (RR06 R12): `print.circumplex_ssm` + snapshots for occasions
  `details`; `ssm_table()` and `ssm_plot_*()` support or cleanly reject
  occasions objects; `ssm_ci_accuracy()` errors informatively (spec §1.4).
- DESIGN.md: occasions RNG reproducibility row + the oracle-registry pointer
  declaration (spec §6 gap — this is the first build adding a longitudinal
  oracle). CLAUDE.md: occasion-contrast-order clause (M23 review F2 carry).
  NEWS entry incl. the `Occasion` schema note.

**Out:**
- ROADMAP candidates (the "Longitudinal deferrals" row): occasions ×
  measures and occasions × contrast × grouping (spec §1.2);
  pairwise-deletion semantics (§1.3); `ssm_ci_accuracy()` occasions
  extension (§1.4); `ssm_analyze_long()` sugar (§1.1).
- Per-person layer + draws adapter → M26; growth support → M27.

## Acceptance criteria

- [x] AC1 — Intake contract per spec §1.1/§1.3, test-evidenced: `scales`/
      `occasions` mutual exclusivity; equal-length block validation;
      stem-order mismatch errors naming the offending block; no-stem
      positional message fires (fixtures both ways); pairwise + occasions
      errors with the estimand message; listwise drop count messaged.
- [x] AC2 — Occasions profiles correct: occasion-major rows within group,
      conditional-presence `Occasion` column, k = 3 supported; on
      complete-case data each occasion's profile row equals the equivalent
      single-`scales` run (exact invariant, fixture-tested).
- [x] AC3 — Paired contrast is second-listed minus first-listed by
      `names(occasions)` list order (a `T10`/`T2` name pair regression test
      proves alphabetical sorting would flip it), through **both** engines;
      the contrast validation rule admits exactly the (1 group, 0 measures,
      2 occasions) triple and errors on > 2 occasions with contrast.
- [x] AC4 — Oracle battery per spec §2.3 green: paired-contrast CI coverage
      at nominal (band pre-registered in the script before the run, M19
      precedent) across the named cells (Δd near 0° and ±180°, truths
      straddling 0/360, small-n ≈ 25–50, k = 3, and the ρ > 0 / Δd ≈ 135°
      reversal cell); measured efficiency matches the conditional identities
      (Δe: ρ_e sign; Δa/Δd: ∇g₂ᵀC∇g₁, reversal observed past 90°);
      degenerate-dependence invariant; closed-form paired Δe agreement;
      boot-vs-MC within the pre-registered tolerance. Results rds + seeded
      regeneration script committed as `devel/m25-*` (level-indexed seeds).
      ≥ 2 independent oracle types per numeric result.
- [x] AC5 — CLAUDE.md boundary battery for the new paths: contrast near
      ±180°, CIs straddling 0°/360° (wrap asserted), flat/zero-variance
      occasion, one occasion degenerate — tested on both engines.
- [x] AC6 — Output surface: print snapshots cover occasions `details`;
      `ssm_table()` and each `ssm_plot_*()` either supports occasions
      objects or rejects with an informative error (tested);
      `ssm_ci_accuracy()` errors informatively on occasions objects.
- [x] AC7 — Docs/NEWS carry only the conditional efficiency statement (grep
      evidence: no unconditional "paired is narrower" anywhere), plus the
      selection-bias and paired-interpretability cautions; DESIGN.md gains
      the occasions RNG row + oracle-registry pointer; CLAUDE.md gains the
      occasion-order clause; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4, T5, T6
- AC5 → T4, T5
- AC6 → T7
- AC7 → T8

## Tasks

- [x] **T1** — Intake/validation layer, tests first: `occasions` parsing,
      mutual exclusivity, block checks, stem-matching alignment (+ no-stem
      message), listwise-only enforcement + drop-count message
      (`R/ssm_analysis.R:197-208`, `is_*()` helpers).
- [x] **T2** — Scoring + result assembly: per-occasion per-group scoring,
      `build_result_labels()` occasion dimension (`R/ssm_analysis.R:301`),
      occasion-major order, conditional `Occasion` column, `details`
      metadata; exact single-`scales` equivalence fixture.
- [x] **T3** — Contrast rule extension + list-order convention (T10/T2
      regression; `ssm_by_group()` slice `R/ssm_bootstrap.R:162` reached
      only in the 2-occasion/1-group cell).
- [x] **T4** — Bootstrap engine path (existing row resampler; `param_diff`
      contrast; quantile/branch machinery untouched) + its boundary
      regressions.
- [x] **T5** — MC engine: stacked k·p covariance, joint draws, per-block
      transform, contrast (`R/ssm_montecarlo.R:119-149`) + its boundary
      regressions; docs note n_g ≫ k·p.
- [x] **T6** — Coverage oracle: seeded script (smoke-first, level-indexed
      seeds, pre-registered band/tolerances written before the run), full
      run, committed `devel/m25-*` rds + analysis; degenerate-dependence
      invariant, closed-form Δe, boot-vs-MC tolerance tests into testthat
      where deterministic.
- [x] **T7** — Output surfaces: print/snapshots, `ssm_table()`,
      `ssm_plot_circle/curve/contrast()` support-or-reject,
      `ssm_ci_accuracy()` informative error.
- [x] **T8** — Docs + NEWS + DESIGN.md (RNG row, oracle-registry pointer) +
      CLAUDE.md occasion-order clause; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (D-013 Build A; candidate promoted). Gate: full k ≥ 2; builds precede M7.
- 2026-07-16: T1 done — intake validation + stem matching + contrast rule; 12 tests; suite 2141. Implement gate skipped (spec pins all choices).
- 2026-07-16: T2 done — occ_scores/ssm_analyze_occasions/labels + MC occ_k plumbing (validated T5); exact equivalence fixtures; suite 2164.
- 2026-07-16: T3 done — list-order contrast locked (T10/T2 regression, hand-computed Δe, both engines cover construction truths).
- 2026-07-16: T4+T5 done (one commit; engines built in T2) — boundary battery both engines: pole wrap, ±175° branch, flat occasion NA, k=3 agreement; no engine fixes.
- 2026-07-16: T6 done — oracle green on all pre-registered gates (coverage [.922,.974]; reversal 0.526@30° vs 1.365@135°, theory 0.480/1.424). First run exposed an oracle bug (within-sample re-pairing is mean-invariant) → baseline rebuilt as fresh-person draws; rds + md committed, bands pinned by testthat.
- 2026-07-16: T7 done — output surfaces; consumer sweep caught ssm_plot_curve positional info-column drop (Occasion leak) → name-based.
- 2026-07-16: T8 done — roxygen/NEWS/DESIGN/CLAUDE docs; no unconditional claim (grep); check() 0 errors / 0 warnings / 0 notes; suite 2346 → review.
- 2026-07-16: review — work-log compressed (cap remedy; verbose in git history); PR #49; PROFILE.md `## changelog` slot backfilled (mid-session plugin update); findings F1 (96, positional occasion indices + overlap guard + cbind regression) and F2 (85, k x p Rd emph) fixed; suite 2353, check clean re-run.

## Decisions

## Review

### Acceptance-criteria evidence (fresh, by command, 2026-07-16)

All commands run this session against the final branch tree (HEAD =
`M25 T8`, identical package content to the checked tree).

- **AC1** — `testthat::test_file("tests/testthat/test-ssm_occasions.R")`
  (NOT_CRAN): 217 pass / 0 fail. Covers: mutual-exclusivity + both-absent
  errors; measures×occasions error; non-list/short/unequal/partial-name/
  duplicate-label shape errors; listwise=FALSE estimand error;
  rotation error naming block T2; different-stem error; no-stem positional
  message (fixtures both ways); listwise drop-count message ("2 person(s)…"
  with drop-equivalence fixture). ✔
- **AC2** — same run: k=3 profiles ordered occasion-minor, conditional
  `Occasion` column, `details$occasions` metadata; per-occasion estimates
  equal the single-`scales` runs exactly (1e-12) with and without grouping;
  group-major/occasion-minor rows and Labels ("T1: F" …) asserted. ✔
- **AC3** — same run: contrast = second-listed − first-listed via list
  order; T10/T2 name-pair regression (alphabetical sorting would flip);
  hand-computed Δe exact; Δd = angle_dist of profile rows; composition
  errors (k=3+contrast "2 occasions"; grouping+contrast "single group");
  both engines identical point estimates, CIs cover construction truths. ✔
- **AC4** — full oracle run committed (`devel/m25-paired-coverage-results.rds`,
  reps=500 boots=600, seeds level-indexed; analysis
  `devel/m25-paired-coverage.md`): every gated cell/engine coverage in
  [.922, .974] ⊂ [.91, .98]; small-n bootstrap [.922, .962] ⊂ [.89, .98]
  (MC small-n measured: worst .928); reversal observed (Δd̂ paired/indep
  var ratio 0.526 @30° / 1.365 @135°, theory 0.480/1.424; Δâ 0.440/1.294);
  Δe exact identity 1.032/1.044; k=3 .946–.950; degenerate-dependence
  (fresh-person) baseline covers nominally through the same code path.
  Registered bands pinned by the rds-reading test (passes in the 217).
  Closed-form textbook Δe interval agreement (0.15·SE at B=5000) and
  boot-vs-MC endpoint tolerance (0.30·SE) tests pass. ≥2 oracle types per
  result: coverage + closed-form + invariants. ✔
- **AC5** — same run, parametrized over both engines: pole-straddling CI
  wraps (lci > uci, est within 15° of pole); ±175° contrasts keep
  sign/branch and cover truth (width < 90°); flat occasion → NA d/fit +
  degeneracy warning, healthy occasion and linear contrast params intact;
  k=3 engine agreement 1e-12. ✔
- **AC6** — same run: print + summary snapshots (occasions details line;
  Δ contrast block); `ssm_table()` rows T1/T2/"T2 - T1"; plot_circle/
  curve accept occasions (curve data has 2 groups, no leaked Occasion/info
  columns — the fixed positional-drop bug); plot_contrast plots occasion
  contrast and refuses profiles-only with the occasions-aware message;
  `ssm_ci_accuracy()` errors "occasion by occasion". ✔
- **AC7** — grep: every "narrower" in R/, man/, NEWS.md sits inside the
  conditional formulation (cos-dependence, reversal past 90°) — no
  unconditional claim; selection-bias + paired-interpretability cautions in
  roxygen (`@param listwise`, Occasions section); DESIGN.md has the
  occasions RNG row + "Oracle records" pointer line; CLAUDE.md has the
  occasion-order clause; NEWS entry present (6 occasions mentions).
  `devtools::check(args = "--no-manual")`: **0 errors / 0 warnings /
  0 notes** (4m54s). ✔

### Independent review (three lenses + scorer, 2026-07-16)

- **[O] diff-bug**: 2 findings. **F1 (scored 96, fixed)** — numeric occasion
  indices were resolved to names and subset by name; with duplicated column
  names (cbind-ed waves both keeping PA..NO) every block first-match
  collapsed onto wave 1: a true Δe of 1 reported exactly 0 with the
  positional-fallback message giving false reassurance. Fixed: blocks
  resolve to positions (numeric stays positional; characters via match()
  with unknown-name errors), an overlapping-columns error covers the
  character variant, and the cbind regression test asserts the exact truth.
  **F2 (scored 85, fixed)** — the roxygen `k*p` asterisk pair rendered as a
  garbled `\emph{}` span in the Rd; reworded to "k x p", re-documented,
  span verified gone.
- **[S] blame-history**: zero findings (M12 label-builder branches verified
  byte-identical; cat() separator artifact verified; occ_k additive; D-003/
  M20 pole convention untouched).
- **[S] prior-PR-comments**: no prior-PR evidence repo-wide (review runs
  through cairn, not GitHub comments) — clean no-op, zero findings.
- Sub-threshold findings logged: none (both findings scored ≥ 80 and were
  fixed; nothing dropped).
- Post-fix evidence: occasions file 224 pass; full suite 2353 pass;
  `check()` re-run clean (0 errors / 0 warnings / 0 notes); CI 8/8 green
  on the pre-fix head, re-verified after the fix push.

### Consistency gate

- `cairn_validate.py`: pass (exit 0) after the work-log cap remedy
  (one-line compression; logged).
- No DESIGN.md IP/GP changed → `cairn_impact` skipped (none exist yet).
- Profile slot: `document()` no diff ✔; generated files untouched by hand ✔;
  README.Rmd/md untouched by this milestone ✔; `pkgdown::check_pkgdown()`
  "No problems found" ✔ (no new exports — occasions is an argument);
  NEWS entry present ✔; no new top-level files needing .Rbuildignore
  (devel/ already ignored) ✔; full `check()` clean (AC7) ✔.
