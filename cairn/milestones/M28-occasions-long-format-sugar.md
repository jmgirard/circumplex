<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M28: Occasions long-format sugar (`ssm_analyze_long()`)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M25
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] **AC1** — `ssm_analyze_long()` on long data yields a `results` table
      identical to `ssm_analyze(occasions = <equivalent wide list>)` on the
      reshaped-wide data, for ≥1 multi-occasion fixture including a
      `contrast = TRUE` (2-occasion) case (round-trip equivalence invariant).
- [ ] **AC2** — occasion ordering follows factor levels / first appearance,
      never alphabetical: a fixture with a `T10`/`T2`-style occasion pair
      produces the temporal (not sign-flipped) contrast.
- [ ] **AC3** — input validation: duplicate `(id, occasion)` rows and ragged
      occasion scale sets each error informatively; tested.
- [ ] **AC4** — `ssm_analyze_long()` exported and documented (roxygen, NEWS
      entry, `devtools::document()` clean); `devtools::test()` clean and
      `devtools::check()` clean (0 errors / 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T2
- AC4 → T1, T3

## Tasks

- [ ] **T1** — Implement `ssm_analyze_long()` (`R/ssm_analyze_long.R` or in
      `R/ssm_analysis.R`): signature, `id`/`occasion` validation, long → wide
      reshape (base R, no new deps), temporal occasion ordering, delegation to
      `ssm_analyze(occasions = )`; roxygen docs. Test-first.
- [ ] **T2** — Tests (`tests/testthat/test-ssm_occasions.R` or a new file):
      the wide/long equivalence invariant (incl. `contrast = TRUE`), the
      `T10`/`T2` ordering regression, the duplicate-wave error, the
      ragged-scale error.
- [ ] **T3** — NEWS.md entry; `devtools::document()`; `@export`; add to
      `_pkgdown.yml` reference index if maintained; `devtools::check()` clean.

## Work log

- 2026-07-17: created by /milestone-plan (longitudinal deferral §1.1, promoted
  from the "Longitudinal deferrals" ROADMAP candidate). Sugar over the M25
  occasions core; no new statistical surface. Sibling M29 planned in the same
  run (§1.4).

## Decisions

## Review
