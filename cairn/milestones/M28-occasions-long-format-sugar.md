<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M28: Occasions long-format sugar (`ssm_analyze_long()`)

- **Status:** review
- **Priority:** normal
- **Depends on:** M25
- **Principles touched:** —
- **Branch/PR:** m28-occasions-long-format-sugar · https://github.com/jmgirard/circumplex/pull/52

## Goal

Ship `ssm_analyze_long()`, a long-format convenience wrapper that reshapes
id/occasion long data to wide and delegates to the validated
`ssm_analyze(occasions = )` path (spec §1.1 — "sugar, not design").

## Scope

**In:**
- A new exported `ssm_analyze_long(data, scales, angles = octants(), id,
  occasion, ..., contrast = FALSE)` (final signature at build): validate the
  `id` and `occasion` columns, reshape long → wide (one row per `id`, one
  score block per occasion, base R `stats::reshape()` or manual — no new
  dependency), build the `occasions` named list in temporal order, and
  delegate to `ssm_analyze(occasions = )`, forwarding the other arguments
  (`group`, `contrast`, `boots`, `interval`, `method`).
- **Occasion order contract:** factor levels when the `occasion` column is a
  factor, otherwise order of first appearance — never alphabetical sort;
  mirrors the occasions-list temporal contract (a `T10`/`T2` pair must not
  flip; spec §1.2 / CLAUDE.md occasion-contrast rule).
- Input validation: duplicate `(id, occasion)` rows error informatively;
  every occasion must contribute the same scale set (ragged → error).
- Roxygen docs (cross-referencing `ssm_analyze()`'s `occasions` argument) +
  a NEWS entry.

**Out:**
- No new statistical behavior — estimation/inference is inherited unchanged
  from the `occasions = ` path (§1.1 sugar); this wrapper only reshapes and
  delegates.
- Pairwise-deletion occasions (§1.3), occasions × measures (§1.2a), and
  occasions × contrast × grouping (§1.2b) → remain ROADMAP candidate rows;
  this milestone does not touch them.

## Acceptance criteria

- [x] **AC1** — `ssm_analyze_long()` on long data yields a `results` table
      identical to `ssm_analyze(occasions = <equivalent wide list>)` on the
      reshaped-wide data, for ≥1 multi-occasion fixture including a
      `contrast = TRUE` (2-occasion) case (round-trip equivalence invariant).
- [x] **AC2** — occasion ordering follows factor levels / first appearance,
      never alphabetical: a fixture with a `T10`/`T2`-style occasion pair
      produces the temporal (not sign-flipped) contrast.
- [x] **AC3** — input validation: duplicate `(id, occasion)` rows, fewer than
      two distinct occasions, and time-varying grouping within an `id` each
      error informatively; tested.
- [x] **AC4** — `ssm_analyze_long()` exported and documented (roxygen, NEWS
      entry, `devtools::document()` clean); `devtools::test()` clean and
      `devtools::check()` clean (0 errors / 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T2
- AC4 → T1, T3

## Tasks

- [x] **T1** — Implement `ssm_analyze_long()` (`R/ssm_analyze_long.R`):
      signature, `id`/`occasion` validation, long → wide reshape (base R, no new
      deps), temporal occasion ordering, delegation to `ssm_analyze(occasions =
      )`; roxygen docs.
- [x] **T2** — Tests (`tests/testthat/test-ssm_analyze_long.R`): the wide/long
      equivalence invariant (bootstrap + Monte Carlo + `contrast = TRUE` +
      numeric-index), the `T10`/`T2` ordering regression, factor-level order,
      and the validation errors (duplicate wave, < 2 occasions, time-varying
      grouping, unknown column, NA-row drop).
- [x] **T3** — NEWS.md entry; `devtools::document()`; `@export`; added to
      `_pkgdown.yml` reference index; `devtools::check()` clean.

## Work log

- 2026-07-17: created by /milestone-plan (longitudinal deferral §1.1, promoted
  from the "Longitudinal deferrals" ROADMAP candidate). Sugar over the M25
  occasions core; no new statistical surface. Sibling M29 planned in the same
  run (§1.4).
- 2026-07-17: in-progress (/milestone-implement). Branch
  m28-occasions-long-format-sugar. Gate: signature confirmed
  `ssm_analyze_long(data, scales, angles, id, occasion, grouping, contrast,
  ...)` with `id`/`occasion` as character/numeric column references (D-014 SE);
  occasion order = factor levels else first-appearance, never alphabetical.
  AC3 amended (substantive) — replaced the wide-format "ragged occasion scale
  sets" case (inapplicable: long input shares one score-column set) with the
  long-format validation surface: duplicate `(id, occasion)` rows, < 2
  occasions, and time-varying grouping within an `id`.
- 2026-07-17: T1 + T2 done. `ssm_analyze_long()` implemented in
  `R/ssm_analyze_long.R` (reshape-and-delegate; scores extracted by position
  per the M25 duplicate-name lesson). 15 tests in
  `test-ssm_analyze_long.R` green; full `devtools::test()` clean (0 failed / 0
  errors; the 4 CPM-Hessian warnings are pre-existing in `test-ci_accuracy.R`,
  untouched here).
- 2026-07-17: T3 done → status review. NEWS entry added; `@export` +
  `_pkgdown.yml` reference; roxygen `@family` back-links regenerated across
  siblings. `devtools::check(--no-manual)` clean: 0 errors / 0 warnings / 0
  notes. (The doc example was then corrected to a real two-occasion long
  fixture in a `\donttest` block — example-only, not run in a default check;
  runnable data-construction verified.) All acceptance criteria met; ready for
  /milestone-review.
- 2026-07-17: review (/milestone-review). PR #52 (draft). Fresh evidence
  recorded for AC1–AC4 (all ✓); consistency gate green (`cairn_validate` exit 0,
  pkgdown clean). Three-lens review: prior-PR none; diff-bug + blame-history
  converged on one order-dependent grouping data-loss bug (F1, scored 96) —
  fixed in `a7342cc` with a regression test, re-checked `check()` 0/0/0. No
  sub-80 findings. Awaiting merge-approval gate.

## Decisions

## Review

Reviewed 2026-07-17 · PR #52 · branch `m28-occasions-long-format-sugar` vs
`origin/master` (2 ahead / 0 behind at review start; default branch unmoved,
no merge needed).

### Acceptance criteria — fresh evidence
- **AC1** ✓ — `test-ssm_analyze_long.R` fresh run: "reproduces equivalent wide
  occasions call (grouped)", "wide paired contrast (single group)", "wide Monte
  Carlo call", and "numeric scale indexes" all pass. `ssm_analyze_long()$results`
  is byte-equal to the hand-built `ssm_analyze(occasions=)$results` under an
  identical seed, across bootstrap, Monte Carlo, and `contrast = TRUE`.
- **AC2** ✓ — "occasion order follows first appearance, not alphabetical
  (T10/T2)" (3 expectations: matches the temporal call, is *not* the
  alphabetical call, profile rows in c("T2","T10")) + "a factor occasion column
  uses its levels for order" pass.
- **AC3** ✓ — duplicate `(id, occasion)`, `< 2` occasions, time-varying
  grouping, unknown column, and NA-row drop each error/message as specified;
  all pass.
- **AC4** ✓ — exported (NAMESPACE), documented (`man/ssm_analyze_long.Rd`, NEWS,
  `_pkgdown.yml` reference). Full `devtools::test()` clean (0 failed / 0 errors;
  4 pre-existing CPM-Hessian warnings in `test-ci_accuracy.R`, untouched).
  `devtools::check(--no-manual)` clean **post-fix: 0 errors / 0 warnings /
  0 notes**.

### Consistency gate
- `cairn_validate.py`: all checks pass (exit 0), including `coverage complete`.
- No DESIGN principle changed (Principles touched: —) → `cairn_impact` skipped.
- Toolchain (r-package `consistency-gate`): `pkgdown::check_pkgdown()` clean
  (`ssm_analyze_long` present in the reference index); no new top-level files
  (`.Rbuildignore` n/a); full `check()` clean (AC4).

### Independent review — three lenses + scorer
- **prior-PR-comments (S):** no prior-PR evidence — merged PRs #47/#49/#50/#51
  carry no inline review comments. 0 findings.
- **diff-bug (O)** and **blame-history (S)** *independently converged* on one
  defect, each with a live reproduction:
  - **F1 — scored 96 → fixed now.** The wide grouping value was taken from each
    person's first *physical* row (`data[[grp]][match(ids, id)]`). The
    time-invariance validation is NA-tolerant, so a group recorded only at a
    later occasion passes validation but was then assigned `NA` and the person
    silently dropped by `ssm_analyze()` — order-dependent data loss in the new
    code. **Fix:** take each person's first non-NA grouping value, indexing the
    original column to preserve type (commit `a7342cc`). **Regression:**
    "grouping recorded only at a later occasion is retained (not dropped)" —
    blanking a person's first-occasion group must not change the result.
- No finding scored below 80 (the sole finding scored 96); none excluded.

### Outcome
All four acceptance criteria verified against fresh evidence; consistency gate
green; the one review finding fixed with a regression test and re-checked
clean. Recommended to merge.
