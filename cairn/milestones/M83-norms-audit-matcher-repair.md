# M83: Make the norms-audit abort matcher accept correct sites

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m83-norms-audit-matcher-repair`

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

- [ ] AC1. A fixture script carrying three `stopifnot` conditions with
      distinct keys, none a prefix of another's stem and no two sharing a
      binding-and-key, is parsed by the helper's own parse call; its collected
      site set is asserted equal to that of a registry built over it, so each
      entry is bound to a parsed site rather than declared. Every entry carries
      a fixture that evaluates that same parsed script and raises. One entry's
      message is asserted to carry the `....` marker with a stem component
      shorter than `min(nchar(squish(key)), NORMS_AUDIT_STEM_FLOOR)` — the
      effective floor, so the entry provably fails an unfixed matcher; a second
      is truncated with a stem above that floor; the third is
      `stopifnot_named`, which never truncates. One procedure computes the
      acceptance matrix for both the shipped registry and this one; over this
      one `all(diag())` holds and the off-diagonal accepting set is empty,
      which is what AC5's derivation over `norms_audit_shared_key_sites()`
      yields here. This is not AC5's fixture: with no shared-key pair the
      expected side is empty under either derivation, and it is the six
      computed off-diagonal cells and the three diagonal ones that carry the
      criterion. The shipped registry cannot carry the truncated entry: it is
      pinned site-for-site to `data-raw/audit-norms.R`, out of scope (M84).
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

- AC1 → T2, T6
- AC2 → T4, T5
- AC3 → T5, T6
- AC4 → T3
- AC5 → T2
- AC6 → T1
- AC7 → T7

## Tasks

- [x] T1. Guard `expect_abort_at_site()`'s `matcher` argument
      (`helper-norms-audit-script.R:600-612`); add its refusal test near
      `test-norms-audit-markers.R:784-836`.
- [x] T2. Single-source the matrix's expected set onto
      `norms_audit_shared_key_sites()` (`test-norms-audit-markers.R:704-714`);
      add the two-part fixture and run both mutants. Extract the matrix
      computation into one procedure the shipped and AC1 registries both call.
- [x] T3. Widen denylist rule (iii)'s exemptions to `$`/`@` third operands and
      `for` index symbols (`helper-norms-audit-script.R:195-204`); update the
      exemption rationale at `:166-172`, which currently claims `::` is the
      only one; rewrite the partition in `test-norms-audit-denylist.R:41-70`.
- [x] T4. Expose truncation-marker presence from
      `norms_audit_stopifnot_stem()` (`:494-498`, which discards it at `:496`);
      rewrite the matcher branch (`:583-592`). Two existing readers of the
      single-value contract: `test-norms-audit-markers.R:668`, `:834`.
- [x] T5. Repair the fallout at `test-norms-audit-markers.R:658-676` — `:675`'s
      `expect_false(ml("condition ...."))` flips under T4, and `:674`'s
      `substr(long, 1L, 66L)` fixture encodes a truncation R does not perform —
      and the constants comment at `helper-norms-audit-script.R:537-542`.
- [x] T6. Add AC1's three-site fixture script and the registry over it, and
      assert T2's matrix procedure over that registry; run AC3's restatement
      grep and clear each hit.
- [ ] T7. Full check; update NEWS only if user-visible (expected: not).

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose exempting only field access and `for` indices over also exempting safe assignment, because `abort <- rlang::abort` is the aliasing rule (iii) exists to catch and separating it from `abort <- 1` requires inspecting assigned values; falsified by a measured false positive on an assignment shape that no value inspection could classify.
- 2026-08-14: plan gate chose leaving `norms_audit_shared_key_sites()`'s contract narrow (differing bindings only) over widening it to same-binding twins, because AC5 needs one derivation not a changed one, and widening moves the pinned roster at `test-norms-audit-markers.R:731-734`; falsified by a shipped registry needing same-binding twins discriminated.
- 2026-08-14: criteria audit ([O], fresh context) returned eight clear-fix findings and one judgment call — AC1 vacuous against existing coverage, AC2 unbounded below, AC3's grep non-functional (`....` unescaped, unmatched glob aborts under zsh), AC4 self-contradictory (`fail <- x$stop` is the shape `opts$abort` exempts), AC5's single mutant vacuous on its own fixture, AC6's hand-authored count, and an "IP4" citation that resolves to this repo's RNG contract rather than the append-only-history rule. All fixed before the criteria were written; the miscitation was resolved by dropping the number, the same miscitation in `cairn/DECISIONS.md:1054` being history.
- 2026-08-14: amendment return: AC1 — "A fixture script carrying three `stopifnot` conditions with distinct keys, none a prefix of another's stem and no two sharing a binding-and-key, is parsed by the helper's own parse call" — AC1 as planned required a registry entry, and `SCRIPT_ABORTS` is pinned site-for-site to `data-raw/audit-norms.R`, which Scope puts Out (M84), so the narrowing binds the entry to a fixture script instead; implement gate chose this over widening scope to add a real guard to the audit script or dropping AC1; falsified by a matcher fix that a fixture-script registry passes and the shipped registry would not.
- 2026-08-14: implement gate chose returning stem-plus-truncation-flag from `norms_audit_stopifnot_stem()` over a separate truncation predicate or a silent attribute, because the two existing readers then break loudly rather than pass a structure through unnoticed and the marker test has one home; falsified by a third reader needing the stem alone often enough that the pair is noise.
- 2026-08-14: amendment criteria audit ([O], two fresh readers) returned eleven clear-fix findings and two judgment calls — AC1's "bound to a real site" not requiring the fixture to evaluate the script, a vacuous 1x1 off-diagonal, unasserted marker/short-stem properties, a moving-target citation of the shipped matrix's assertions, an off-diagonal set compared to a list of entries no `identical()` relates, a floor stated as `NORMS_AUDIT_STEM_FLOOR` rather than the effective `min(nchar(squish(key)), 40)`, a missing T2 coverage row, and a non-vacuity claim aimed at the expected side rather than the observed. All fixed before the amended text was written. The judgment calls: the fixture gains an over-floor truncated site and a named site so both sides of the removed floor and the never-truncating kind are exercised; and AC2's no-floor-when-truncated rule is kept as planned with its residual leak pinned rather than closed (see Decisions).
- 2026-08-14: T1 done — `expect_abort_at_site()` refuses a non-`norms_audit_matcher` argument by name; test asserts the refusal for a string, a plain callable and `NULL`, with a passing control that shows the matcher accepts its own fixture's message. Suite green (FAIL 0, PASS 7130).

- 2026-08-14: T2 done — the acceptance matrix and its expected off-diagonal moved into `norms_audit_acceptance_matrix()` / `norms_audit_expected_offdiag()` in the helper; membership now comes from `norms_audit_shared_key_sites()` alone, with `shared_fn` injectable so AC5's two mutants run without editing source. Both mutants verified against the two-part fixture, and each shown vacuous on the other part. Suite green (FAIL 0, PASS 7136).
- 2026-08-14: T3 done — rule (iii) now skips `$`/`@` operand 3 and `for` operand 2 via `NON_VALUE_OPERANDS`; the four shapes it used to flag (`opts$abort`, `x@abort`, `df$stop <- 1`, `for (abort in x) f(1)`) were measured red against the partition before the fix and are green after, while `for (i in abort) 1` and `abort <- 1` stay denied by (iii). Suite green (FAIL 0, PASS 7146).
- 2026-08-14: T4 and T5 done, committed together because T4's matcher change is what creates T5's fallout and a commit carrying only one of them would be red — `norms_audit_stopifnot_stem()` now returns `list(stem, truncated)`, the matcher applies the floor only where R did not truncate, and both readers of the old contract were updated. The retired `substr(long, 1L, 66L)` fixture, a truncation R does not perform, is replaced by a fixture script whose key comes from the parse tree and whose message comes from R. Suite green (FAIL 0, PASS 7153).
- 2026-08-14: T6 done — a three-site fixture script (braced condition truncating below the removed floor, flat condition truncating above it, named condition that never truncates) with a hand-declared registry bound to it by site-id equality; the matrix procedure over it has a full diagonal and an empty off-diagonal, with no stem prefixing another key. AC3's grep run over `tests/`, `cairn/ROADMAP.md` and the M83/M84/M85 files: hits in `test-cpm_fit.R`, `test-cpm_oracles.R`, `test-axes-scaled-fit.R` and `ROADMAP.md:57` are unrelated senses of "truncate"; `ROADMAP.md:26` already states the first-deparsed-line model; two live restatements cleared — the walk's keying comment and a `truncated <- substr(key, ...)` variable that echoed the character-count model, renamed `shortened`. Suite green (FAIL 0, PASS 7169).
## Decisions

### 2026-08-14: AC2's no-floor-when-truncated rule keeps a measured residual leak, pinned rather than closed

Where R's `....` marker is present, AC2 applies no length floor and requires only
that the stem be a non-empty prefix of the squished key. That leaves a leak:
`stopifnot({ ... })` raises `{ .... is not TRUE`, a one-character stem (measured
2026-08-14 on R 4.6.1), which prefixes the key of any other braced condition, so
one such site's message is accepted by the other's matcher.

Closing it would need the matcher to re-derive R's own line break by re-parsing
the key — fragile, the key being a squished join of `deparse(cond)` — and even
then two conditions whose first deparsed lines are identical stay
indistinguishable by message alone, exactly as the shipped
`source note not found: {}` pair is. Message-level discrimination cannot settle
that class. The cross-site matrix can and does: two sites cross-accepting
off-diagonal without sharing a key reddens the off-diagonal equality.

So the leak is pinned by an assertion recording that the degenerate truncated
stem IS accepted, in the manner of the existing "the comparison cannot see a
shared-key binding SWAP" test, and the matrix is what stands behind
discrimination there. Reopen if a shipped condition acquires a braced form.

## Review
