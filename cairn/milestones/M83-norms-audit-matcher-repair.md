# M83: Make the norms-audit abort matcher accept correct sites

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m83-norms-audit-matcher-repair` / https://github.com/jmgirard/circumplex/pull/111

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

- [x] AC1. A fixture script carrying three `stopifnot` conditions with
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
- [x] AC2. The `stopifnot` matcher distinguishes truncated from untruncated
      messages: where the `....` marker is present it requires the stem to be a
      non-empty prefix of the squished key and applies no length floor; where
      absent it applies `min(nchar(squish(key)), NORMS_AUDIT_STEM_FLOOR)`
      unchanged. Evidence: a test builds a `stopifnot()` whose condition
      contains a braced `function(el) {...}`, asserts the raised stem is under
      the floor AND carries the marker AND prefixes the key, and asserts
      acceptance; an untruncated one-character stem is still rejected; and the
      cross-site matrix stays green with its off-diagonal count pinned at 2.
- [x] AC3. No site under `tests/`, `cairn/ROADMAP.md`, or the M83/M84/M85
      milestone files states the superseded character-count truncation model.
      Procedure: `grep -rnE 'STEM_FLOOR|truncat|\.\.\.\.'` over that explicit
      path list, each hit inspected. (`cairn/milestones/archive/` and
      `cairn/reviews/archive/` are excluded — history, never edited.)
- [x] AC4. Denylist rule (iii) is not consulted for the third operand of `$` or
      `@`, nor for the index symbol of `for`; it remains consulted everywhere
      else, assignment included. Evidence: `test-norms-audit-denylist.R`
      asserts an enumerated partition in both directions, each shape
      individually — accepted: `opts$abort`, `x@abort`, `df$stop <- 1`,
      `for (abort in x) f(1)`; denied: `for (i in abort) 1`, `abort <- 1`,
      `f(g = stopifnot)`, `lapply(x, stop)`.
- [x] AC5. The cross-discrimination matrix derives its expected shared-key
      entries by calling `norms_audit_shared_key_sites()` rather than
      re-deriving them with `outer()`. Verified by two mutants against a
      two-part fixture: dropping the helper's `binding != binding[[i]]`
      conjunct reddens the matrix when a same-binding twin is registered, and
      mutating the helper to `list()` reddens it when a differing-binding pair
      is registered. One mutant alone is vacuous — on a same-binding fixture
      the correct helper already returns zero entries.
- [x] AC6. `expect_abort_at_site()` refuses a `matcher` that is not a
      `norms_audit_matcher`, naming the argument in its message. Evidence: a
      test asserting that message, and a green `devtools::test()` run over the
      existing call sites.
- [x] AC7. `devtools::test()` and `devtools::check(args = "--no-manual")` clean,
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
- [x] T7. Full check; update NEWS only if user-visible (expected: not).

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
- 2026-08-14: T7 partial, AC7 blocked on the local toolchain — `devtools::test()` clean (FAIL 0, PASS 7169) and `document()` warning-free with `man/` and `NAMESPACE` byte-unchanged, but `devtools::check(args = "--no-manual")` cannot build: R's Makeconf links `-L/opt/gfortran/lib`, which is absent on this machine, so `R CMD INSTALL` fails at `ld: library 'emutls_w' not found`. Verified pre-existing and unrelated by installing master from a detached worktree carrying none of this branch's changes — identical failure. No NEWS entry: nothing user-visible changed (test machinery only).
- 2026-08-14: `document()` bumped `Config/roxygen2/version` in DESCRIPTION from 8.0.0 to 8.1.0 (the local roxygen2 is newer than the one that last generated `man/`); reverted rather than swept into this branch, since it is unrelated to M83 and is the maintainer's call.
- 2026-08-14: T7 done, AC7's blocker resolved — the maintainer installed the R-project gfortran build, and `devtools::check(args = "--no-manual")` now runs to `0 errors | 0 warnings | 0 notes` (7m 55s). The first post-install run carried one NOTE naming the installer left in the repo root by the download command; it was moved to `~/Downloads` and the check re-run clean. No NEWS entry: the milestone changes test machinery only.
- 2026-08-14: all tasks done; status → review.
- 2026-08-14: review round 1 — three fresh-context lenses plus a fresh scorer; ten findings, three actioned (F6 88, F1 85, F3 84), none meeting the return floor. F6 and F1 fixed on the branch and re-verified (`check` 0/0/0); F3's disposition goes to the maintainer at the merge gate. A stated bound and its ROADMAP candidate row come out of F1's repair.
- 2026-08-14: merge approved by Jeff at the review gate with F3 recorded rather than fixed; two ROADMAP candidate rows added (the same-binding-twin matrix defect, promote into M84; the value-passing field-access bound from F1's repair).
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

_Fresh evidence gathered 2026-08-14 on branch `m83-norms-audit-matcher-repair` at `742817d4`, PR #111. Every figure below was produced by running the code in this session, never recalled._

### Acceptance-criteria evidence

- **AC1.** The three-site fixture registry builds and its declared site ids are identical to those the walk collects from the parsed fixture (`TRUE`), so the hand-declared keys are the parse tree's. All three fixtures raise (no `NA` message). Measured per entry: `braced` truncated, stem 37 against an effective floor of 40; `flat` truncated, stem 62 above it; `named` untruncated, kind `stopifnot_named`. `all(diag(accepts))` holds, the off-diagonal accepting set is identical to `norms_audit_expected_offdiag()`, and its accept count is 0 over the six computed cells.
- **AC2.** On the truncated site's own message the pre-M83 predicate (`nchar(stem) >= min(nchar(squish(key)), 40) && startsWith(...)`) returns `FALSE` and the shipped matcher returns `TRUE` — the repair measured on one message rather than argued. Untruncated behaviour unchanged: `"i is not TRUE"` rejected against key `is.data.frame(batch)`, the full stem accepted. The shipped cross-site matrix stays green with its off-diagonal count pinned at 2.
- **AC3.** `grep -rnE 'STEM_FLOOR|truncat|\.\.\.\.'` over `tests/`, `cairn/ROADMAP.md` and the M83/M84/M85 files returns 61 hits, each inspected. Outside the norms-audit files there are five, all unrelated senses of the word (`test-cpm_fit.R:220,230` truncated DFT; `test-cpm_oracles.R:203` a truncated decimal; `test-axes-scaled-fit.R:1167` truncation of an excess; `ROADMAP.md:57` a truncated log tail). `ROADMAP.md:26` already states the first-deparsed-line model. Inside the audit files every statement is the new model; the two live restatements found at T6 were cleared.
- **AC4.** The partition runs in both directions: 21/21 denied shapes are caught **by the rule they are meant for** (not merely by some rule), and 0 of 13 accepted shapes are flagged — including the four this milestone exempts. The shipped `data-raw/audit-norms.R` sweep returns 0 hits. (Denied grew from 18 to 21 when review finding F1 was fixed; see below.)
- **AC5.** Controls: the same-binding twin yields an empty expected set, the differing-binding pair yields both ordered cells. Mutant 1 (helper without its `binding != binding[[i]]` conjunct) reddens the twin fixture and is vacuous on the pair; mutant 2 (helper returning `list()`) reddens the pair and is vacuous on the twin. Each mutant is shown vacuous on the other part, which is why one fixture cannot verify both.
- **AC6.** `expect_abort_at_site()` refuses all three shapes by name, the message naming the argument: `` `matcher` must be a norms_audit_matcher, not "character" `` / `"function"` / `"NULL"`. The `"function"` case matters most — a callable would otherwise be used. Green over the existing call sites (full suite below).
- **AC7.** Re-run after the F1 and F6 fixes: `devtools::check(args = "--no-manual")` — **0 errors, 0 warnings, 0 notes** (7m 29s), its `testthat.R` leg green at 369s. The pre-fix run recorded `devtools::test()` — `FAIL 0 | WARN 6 | SKIP 3 | PASS 7169`; the 6 warnings and 3 skips are pre-existing lavaan/CRAN ones, unchanged from master. `devtools::check(args = "--no-manual")` — **0 errors, 0 warnings, 0 notes** (7m 55s). `document()` emits zero `resolve link` lines with `cli.width = 500`, and leaves `man/` and `NAMESPACE` byte-unchanged.

### Consistency gate

**Universal cairn-file checks.** `cairn_validate` exits 0 — all 16 PASS checks pass, including `coverage complete`, `weight caps`, `binding criteria` and `mirror agreement`; 4 advisories, of which the only WARN is `work-log format` (47 hits, every one an M7 hard-wrapped line, none in this milestone's file). No `DESIGN.md` principle changed, so `cairn_impact` is skipped.

**Toolchain checks (r-package `consistency-gate` slot).** `document()` warning-free at `cli.width = 500` with zero `resolve link` lines and no diff in `man/` or `NAMESPACE`; no generated file hand-edited (the branch touches no `R/`, `src/`, `man/`, `NAMESPACE` or `DESCRIPTION`); `pkgdown::check_pkgdown()` — "No problems found"; `README.Rmd`/`README.md` untouched and unchanged from master; no NEWS entry owed (test machinery only, no user-visible change); no new top-level files, so no `.Rbuildignore` entry owed; `devtools::check(args = "--no-manual")` clean at 0/0/0.

**Returns.** This is the milestone's first review; no defect returns, and one amendment return (AC1, at the implement gate before any review).

### Independent review

Three fresh-context lenses, then a fresh scorer that generated none of the findings and was given the diff and this milestone file.

- **[S] blame-history:** no findings. Every behavioural change traces to one of M82's four graduated review findings (F1/F7/F8/F13); the one deliberate loosening is measured, decided here, and pinned by its own test.
- **[S] prior-PR-comments:** no findings. Primary surface (archived `## Review` sections) confirms the diff is the repair M82's review specified rather than a contradiction of it. The GitHub inline-comment probe returned `[]`, so that surface was skipped.
- **[O] diff-bug:** ten candidate findings, scored below.

**Actioned (score ≥ 80).** None demonstrated an acceptance criterion failing inside the domain that criterion's own named procedure quantifies over, and none scored ≥ 90, so none met the return floor; each was triaged.

- **F6 (88) — fixed now.** The truncation marker was detected on *any* message ending in `....`, and detecting it removes the floor entirely. Measured: the matcher for key `is.data.frame(batch)` accepted `is.d....` and `is.data....`. A fixture failing before reaching its guard, with an unrelated message ending that way, would have been reported as coverage for a site never reached. The marker is now recognised only in R's own shape — space, four dots, then the verdict — and `NORMS_AUDIT_VERDICT` gives that pattern one home.
- **F1 (85) — fixed now.** The `$`/`@` operand-3 exemption made a genuine aliased-abort *call* invisible: `opts$abort("boom")` and `x@abort("boom")` were flagged on master and silent on this branch (reproduced independently at the gate). The M81 walk cannot see such a call either — its head deparses to `opts$abort`, not in `ABORT_HEADS` — so it was an unregistered abort site with every count balancing. Repaired through rule (i), not rule (iii), which leaves AC4's wording untouched: a call whose head is a field or slot access naming a denied function is a denied spelling. Three fixtures added to the denied partition.
- **F3 (84) — see the disposition recorded at the merge gate.** `norms_audit_expected_offdiag()` is wrong for a registry holding a same-binding twin, and the error direction is a false FAILURE on correct code: measured, such a pair gives observed off-diagonal `TRUE, TRUE` against expected `FALSE, FALSE`, so the matrix assertion fails where master's `outer()` derivation passed. Same-binding twins are a supported shape — `norms_audit_assign_ordinals()` exists for them and a test asserts they stay separable. No in-scope repair exists: the matrix needs "pairs whose messages are indistinguishable" (any binding) while `norms_audit_shared_key_sites()` returns "pairs under differing bindings" for the stack assertions, and Scope Out forbids widening the latter — correctly, since widening would break those assertions. AC5 mandates deriving the first set from the second, which is the conflation.

**Below the action bar (logged, not actioned).** F10 (74) `norms_audit_acceptance_matrix()` does not guard that it was handed a built registry, the shape AC6 removed from its sibling. F2 (72) AC4's partition did not pin call-through-an-exempted-slot either way — closed incidentally by F1's fixtures. F5 (68) `outer()` copies input names into `dimnames` while the observed matrix has none, so a registry built from a *named* list would fail on dimnames alone (latent; every registry is an unnamed list). F4 (52) derivative of F3. F7 (45) the guard-must-fail assertion at the truncated-branch test is entailed by the two lines above it. F9 (38) the consumer header's call-site count is stale on an unmodified, self-disclaiming line. F8 (30) an orphaned comment block pre-existing on master.

**Stated bound introduced by F1's repair.** A field access passed as a *value* (`lapply(x, opts$abort)`) is no longer flagged; before M83 it was caught only as a side effect of the over-broad rule this milestone narrows, and separating it from an ordinary `opts$abort` read needs the assignment-target case exempted in turn, past what AC4 licenses. Pinned by its own test and carried by a ROADMAP candidate row.

