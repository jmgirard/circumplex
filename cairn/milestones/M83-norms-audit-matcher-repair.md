# M83: Make the norms-audit abort matcher accept correct sites

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

The test machinery that enumerates and matches `data-raw/audit-norms.R`'s abort
sites accepts the message each declared site actually raises, and flags only
genuine denied abort spellings.

## Scope

**In:** `tests/testthat/helper-norms-audit-script.R` — the `stopifnot` stem
floor (`:583-592`), the stem extractor (`:494-498`), denylist rule (iii)
(`:195-204`), and `expect_abort_at_site()`'s argument contract (`:600-612`);
plus the consuming assertions in `test-norms-audit-markers.R` and
`test-norms-audit-denylist.R`.

**Out:** `data-raw/audit-norms.R` itself → M84, which adds the abort sites this
machinery must then accept. The coverage report's blank `sample` column → M85.
Widening `norms_audit_shared_key_sites()` to return same-binding twins — AC5
single-sources the matrix onto the helper's existing contract and does not
change it; a widening would change the pinned roster at
`test-norms-audit-markers.R:731-734` and belongs to whatever needs it.

## Acceptance criteria

- [ ] AC1. The registry gains an entry whose provoking fixture raises a
      `stopifnot` message carrying the `....` truncation marker with a stem
      shorter than `NORMS_AUDIT_STEM_FLOOR`, and the cross-site matrix's
      per-entry acceptance assertion (`test-norms-audit-markers.R`,
      `all(diag(accepts))`) passes for it. Without a new entry this criterion
      is already green today and cannot distinguish a fixed matcher from an
      unfixed one.
- [ ] AC2. The `stopifnot` matcher distinguishes truncated from untruncated
      messages: where the `....` marker is present it requires the stem to be a
      non-empty prefix of the squished key and applies no length floor; where
      absent it applies `min(nchar(squish(key)), NORMS_AUDIT_STEM_FLOOR)`
      unchanged. Evidence: a test builds a `stopifnot()` whose condition
      contains a braced `function(el) {...}`, asserts the raised stem is under
      the floor AND carries the marker AND prefixes the key, and asserts
      acceptance; an untruncated one-character stem is still rejected; and the
      cross-site matrix stays green with its off-diagonal count pinned at 2.
- [ ] AC3. No site under `tests/`, `cairn/ROADMAP.md`, or the M83/M84/M85
      milestone files states the superseded character-count truncation model.
      Procedure: `grep -rnE 'STEM_FLOOR|truncat|\.\.\.\.'` over that explicit
      path list, each hit inspected. (`cairn/milestones/archive/` and
      `cairn/reviews/archive/` are excluded — history, never edited.)
- [ ] AC4. Denylist rule (iii) is not consulted for the third operand of `$` or
      `@`, nor for the index symbol of `for`; it remains consulted everywhere
      else, assignment included. Evidence: `test-norms-audit-denylist.R`
      asserts an enumerated partition in both directions, each shape
      individually — accepted: `opts$abort`, `x@abort`, `df$stop <- 1`,
      `for (abort in x) f(1)`; denied: `for (i in abort) 1`, `abort <- 1`,
      `f(g = stopifnot)`, `lapply(x, stop)`.
- [ ] AC5. The cross-discrimination matrix derives its expected shared-key
      entries by calling `norms_audit_shared_key_sites()` rather than
      re-deriving them with `outer()`. Verified by two mutants against a
      two-part fixture: dropping the helper's `binding != binding[[i]]`
      conjunct reddens the matrix when a same-binding twin is registered, and
      mutating the helper to `list()` reddens it when a differing-binding pair
      is registered. One mutant alone is vacuous — on a same-binding fixture
      the correct helper already returns zero entries.
- [ ] AC6. `expect_abort_at_site()` refuses a `matcher` that is not a
      `norms_audit_matcher`, naming the argument in its message. Evidence: a
      test asserting that message, and a green `devtools::test()` run over the
      existing call sites.
- [ ] AC7. `devtools::test()` and `devtools::check(args = "--no-manual")` clean,
      with `document()` warning-free per the profile's consistency gate.

## Coverage

- AC1 → T6
- AC2 → T4, T5
- AC3 → T5, T6
- AC4 → T3
- AC5 → T2
- AC6 → T1
- AC7 → T7

## Tasks

- [ ] T1. Guard `expect_abort_at_site()`'s `matcher` argument
      (`helper-norms-audit-script.R:600-612`); add its refusal test near
      `test-norms-audit-markers.R:784-836`.
- [ ] T2. Single-source the matrix's expected set onto
      `norms_audit_shared_key_sites()` (`test-norms-audit-markers.R:704-714`);
      add the two-part fixture and run both mutants.
- [ ] T3. Widen denylist rule (iii)'s exemptions to `$`/`@` third operands and
      `for` index symbols (`helper-norms-audit-script.R:195-204`); update the
      exemption rationale at `:166-172`, which currently claims `::` is the
      only one; rewrite the partition in `test-norms-audit-denylist.R:41-70`.
- [ ] T4. Expose truncation-marker presence from
      `norms_audit_stopifnot_stem()` (`:494-498`, which discards it at `:496`);
      rewrite the matcher branch (`:583-592`). Two existing readers of the
      single-value contract: `test-norms-audit-markers.R:668`, `:834`.
- [ ] T5. Repair the fallout at `test-norms-audit-markers.R:658-676` — `:675`'s
      `expect_false(ml("condition ...."))` flips under T4, and `:674`'s
      `substr(long, 1L, 66L)` fixture encodes a truncation R does not perform —
      and the constants comment at `helper-norms-audit-script.R:537-542`.
- [ ] T6. Add the truncated-stem registry entry and its fixture; run AC3's
      restatement grep and clear each hit.
- [ ] T7. Full check; update NEWS only if user-visible (expected: not).

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose exempting only field access and `for` indices over also exempting safe assignment, because `abort <- rlang::abort` is the aliasing rule (iii) exists to catch and separating it from `abort <- 1` requires inspecting assigned values; falsified by a measured false positive on an assignment shape that no value inspection could classify.
- 2026-08-14: plan gate chose leaving `norms_audit_shared_key_sites()`'s contract narrow (differing bindings only) over widening it to same-binding twins, because AC5 needs one derivation not a changed one, and widening moves the pinned roster at `test-norms-audit-markers.R:731-734`; falsified by a shipped registry needing same-binding twins discriminated.
- 2026-08-14: criteria audit ([O], fresh context) returned eight clear-fix findings and one judgment call — AC1 vacuous against existing coverage, AC2 unbounded below, AC3's grep non-functional (`....` unescaped, unmatched glob aborts under zsh), AC4 self-contradictory (`fail <- x$stop` is the shape `opts$abort` exempts), AC5's single mutant vacuous on its own fixture, AC6's hand-authored count, and an "IP4" citation that resolves to this repo's RNG contract rather than the append-only-history rule. All fixed before the criteria were written; the miscitation was resolved by dropping the number, the same miscitation in `cairn/DECISIONS.md:1054` being history.

## Decisions

## Review
