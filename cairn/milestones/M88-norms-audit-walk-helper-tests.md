# M88: Fence the norms-audit walk helpers M87 kept

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2
- **Branch/PR:** `m88-norms-audit-walk-helper-tests` / [PR #116](https://github.com/jmgirard/circumplex/pull/116)

## Goal

Give the abort-site walk helpers that survived M87's retirement direct tests,
and delete the ordinal the manifest identity no longer earns.

## Scope

Surface tier: **internal** — the deliverable is test machinery over
`data-raw/audit-norms.R`, which is `.Rbuildignore`d and ships to no user; no
external consumer of the package relies on any of it.

**In:** unit tests for the helpers on the manifest path that lost their tests
with M87's deleted apparatus — the truncation-marker discrimination
(`norms_audit_stopifnot_stem()`, `NORMS_AUDIT_VERDICT`), the matcher's
fail-closed unknown-kind refusal and its `stopifnot_named` branch, and the walk's
two `refuse_unenumerable()` sites (`norms_audit_stopifnot_conditions()`,
`norms_audit_stop_key()`). Removal of the ordinal from the walk, the generated
manifest and the four-part identity, replacing the property it carried with a
duplicate-refusal on both sides of the set comparison. Repairs to helper
behaviour that writing these tests exposes.

**Out:** restoring any part of the retired apparatus — the abort-site registry,
the per-site matchers, the cross-discrimination matrix, the denylist sweep
(D-042 refuses these, and this milestone adds no sweep and widens no promise).
Any non-`stop()` abort spelling → stays given up per D-042's consequence (1).
The `data-raw/audit-norms.R` guard surface itself → the standing roster-robustness
candidate row. Below-bar findings from M83–M87 that are not helper coverage →
their existing candidate rows.

## Acceptance criteria

- [x] AC1 A test asserts `audit_key_matches("stopifnot", key, msg)` over an
      enumerated vector of message shapes, each declared accepted or rejected and
      asserted in both directions, containing at least: a message R itself raised
      by overflowing a `stopifnot()` condition past one deparsed line, keyed on
      that condition's full deparsed text (accepted); the same message with its
      trailing verdict clause removed (rejected); an untruncated whole-condition
      message (accepted); a below-floor stem carrying the verdict but no
      truncation marker (rejected). The first shape is captured from a live
      `stopifnot()` at test time, never hand-typed.
- [x] AC2 A test asserts `audit_key_matches()` raises, naming the kind it got,
      for a kind outside `stop`/`stopifnot`/`stopifnot_named`; and asserts the
      `stopifnot_named` branch in both directions — a message equal to the key
      accepted, a message carrying the key as a strict superstring rejected.
- [x] AC3 A test asserts each `refuse_unenumerable()` site raises and names its
      cause: `norms_audit_stopifnot_conditions()` for a condition passed under any
      element of `STOPIFNOT_RESERVED`, iterated as the running R defines it and
      anchored non-vacuous by pinning `"exprs"` as a literal member; and
      `norms_audit_stop_key()` for a `stop()` carrying one and more than one
      message-concatenated named argument, the multi-name rendering asserted.
      Each is paired with a negative that must not raise — a positional
      condition, a named condition, a `stop()` with no names, and one carrying
      only `call.`/`domain`.
- [x] AC4 The abort-site identity is (kind, binding, key). A test asserts the
      field set directly rather than by spelling: `names(NORMS_AUDIT_MANIFEST)`
      is exactly `c("kind", "binding", "key")`, and every element of
      `norms_audit_abort_sites()` has `names()` equal as a set to
      `c("kind", "key", "binding")`, so a fourth field on either side reddens
      the suite under any spelling; `manifest_ids()` and `walked_ids()` each
      paste exactly those three. As a spot check on the three spellings the
      retired mechanism used,
      `git grep -nE '(\$|\.)ordinal|ordinal *=|assign_ordinals' -- tests tools`
      returns no line — prose explaining the removal may name `ordinal`.
- [x] AC5 A test asserts the walked identities and the manifest identities are
      each duplicate-free, so two guards identical in (kind, binding, key) redden
      the suite rather than collapsing onto one row — the separability the
      deleted ordinal carried, kept as a refusal.
- [ ] AC6 Each test this milestone adds reddens under a mutation of a helper line
      it locks. The Review section records one summary line — the tests, the
      mutations applied, the assertions that failed — and a `git hash-object`
      comparison showing each mutated helper file restored to its pre-mutation blob.
- [ ] AC7 `Rscript -e 'devtools::test()'` clean; `git status` empty before any
      gate is reported clean.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T5
- AC6 → T6
- AC7 → T1, T6

## Tasks

- [x] T1 Cut `m88-norms-audit-walk-helper-tests` from the up-to-date default
      branch; confirm a clean `devtools::test()` baseline before any edit.
- [x] T2 Add `tests/testthat/test-norms-audit-walk.R` with the AC1 partition,
      written as an enumerated accept/reject vector rather than examples
      (`helper-norms-audit-script.R:307-318`, `helper-norms-audit-manifest.R:193-196`).
- [x] T3 Add the AC2 matcher assertions to that file
      (`helper-norms-audit-manifest.R:176-197`).
- [x] T4 Add the AC3 refusal assertions with their negatives
      (`helper-norms-audit-script.R:103`, `:117-120`, `:130-144`, `:162-183`).
- [x] T5 Delete `norms_audit_assign_ordinals()` and its call
      (`helper-norms-audit-script.R:203-219`, `:247`), drop the `ordinal` column
      from `helper-norms-audit-manifest.R`, narrow the identity in
      `test-norms-audit-manifest.R:28-37`, and add the AC5 duplicate-refusal on
      both sides.
- [x] T6 Run the AC6 mutation pass over the added tests; record the summary line
      and restore hashes; re-run `devtools::test()`.

## Work log

- 2026-08-15: created by /milestone-plan.
- 2026-08-15: criteria audit ran — a fresh-context [O] reader returned five findings, four fixed at the gate (AC1 unsatisfiable: a truncated stem can never prefix a 20-char key, so the accepted probe is keyed on the full condition text; AC3 vacuous under `STOPIFNOT_RESERVED <- character(0)`, anchored by pinning `"exprs"`; AC5-as-drafted locked one axis of three, superseded by the ordinal deletion; AC6 disproportionate for an internal tier, narrowed to one summary line plus restore hashes) and one widened (AC4-as-drafted, one exemplar for a family free in two axes).
- 2026-08-15: plan gate chose deleting the ordinal over testing it because all 33 shipped sites are ordinal 1 and no duplicated (kind, binding, key) triple exists (measured 2026-08-15), so the field fences nothing today; falsified by a second guard identical in kind, binding and key appearing in the audit script, which AC5's duplicate-refusal reddens rather than silently absorbing.
- 2026-08-15: plan gate chose test-only-plus-repairs over tests-only because the audit found untested branches rather than defects, and a defect surfaced while writing a test is cheaper to fix in place than to route; falsified by a repair large enough to need its own design decision, which returns to plan.
- 2026-08-15: T1 in progress — branch cut from a synced master, status in-progress; the baseline `devtools::test()` is still running, so no task is checked off yet. The T2-T4 test file is drafted outside the repo and dry-runs clean (46 assertions, testthat 3.3.2); an in-memory reintroduction of the M83 marker regression reddens 5 of them, naming both shapes AC1 exists for, so the partition is not vacuous. Nothing is committed to `tests/` yet.
- 2026-08-15: T1 done — baseline `devtools::test()` clean at FAIL 0 / WARN 6 / SKIP 3 / PASS 7051 on master's tree.
- 2026-08-15: T2-T4 done in one commit, the three criteria being three sections of one new file (`tests/testthat/test-norms-audit-walk.R`, 177 lines, 46 assertions). All eight norms-audit test files pass. The file does not skip against the installed package as its siblings do: the helpers under test are pure and read no `data-raw/` path.
- 2026-08-15: T5 done — ordinal removed from the walk (`helper-norms-audit-script.R`), the manifest's columns and both identity builders; the duplicate refusal added on both sides of the set comparison, plus the AC4 field-set assertion. All eight norms-audit files pass; the manifest file goes 9 → 44 assertions.
- 2026-08-15: AC4 amended at a mini gate — the original wording promised "no code carries it" but checked three spellings of the word. A fresh-context [O] reader wrote nine reintroduction shapes to a scratch file and grepped them: nine escaped, including `[["ordinal"]]` (the field-access style this codebase already uses) and a renamed fourth field, so the procedure was a proxy for its own universal. Replaced by a direct assertion on the field set of both sides, which every escaping shape must violate to have any effect; the grep is retained and relabelled a spot check. Also fixed at the same gate: the original grep matched four comment lines explaining the removal, so satisfying it literally meant deleting the explanation D-043 asks the helpers to carry.
- 2026-08-15: T6 done — 7 mutants, 7 killed, 0 survived, each restored by copy from HEAD's blob and re-hashed before the next ran. AC1 truncation-marker regression → 5 failures; AC2 unknown-kind fall-through → 1; AC2 named-match loosened to containment → 2; AC3 reserved set emptied → 2; AC3 kinds collapsed → 1; AC3 stop() named-arg refusal removed → 1; AC4/AC5 fourth per-site field → 33. Both helpers end at their pre-mutation blobs (`50fedd235c`, `b24345448a`) with a clean tree.
- 2026-08-15: T6 run one mutant per invocation after an unattended seven-in-one run was interrupted and left mutant 2 on disk — an `atexit` restore does not survive a hard kill (extends M82's harness lesson, whose snapshot-and-restore discipline assumed the process gets to exit). Recovery was possible because each mutant is a one-hunk diff against a committed HEAD, so the restore was verifiable by blob hash; the single-shot form keeps the mutated window inside one call.
- 2026-08-15: T6's seventh anchor initially matched two sites and the harness refused to apply it rather than mutating the wrong one — the multi-site anchoring trap M87 hit, here failing closed. Re-anchored on the enclosing block.
- 2026-08-15: mutant 4 is the evidence for the criteria audit's vacuity finding: emptying `STOPIFNOT_RESERVED` drops the file from 46 assertions to 37 because the loop runs zero times, so without the two anchor assertions the test would pass green over its own mutation.
- 2026-08-15: mutant 7 is the evidence AC4's field-set assertion carries the claim its grep cannot: `out[[i]][["seq_within_group"]] <- 1L` reintroduces a per-site field, is not matched by `git grep -nE '(\$|\.)ordinal|ordinal *=|assign_ordinals'` (verified against the line itself), and reddens 33 assertions — one per walked site.
- 2026-08-15: CHECKPOINT — T6's mutation half is complete and recorded above; the confirming full `devtools::test()` was still running when this was committed, so AC7 is not yet evidenced and the milestone stays `in-progress`. The filtered `norms-audit` run was clean at the same tree.
- 2026-08-15: AC7 evidenced, superseding the checkpoint line above — full `devtools::test()` clean at FAIL 0 / WARN 6 / SKIP 3 / PASS 7132, warnings and skips unchanged from the T1 baseline's 7051 passes. The +81 is the new walk file's 46 assertions plus the manifest file's 9 → 44. Tree clean; status in-progress → review.
- 2026-08-15: return 1 fixed. F1 — the AC3 naming assertions now match `formal <nm>` and `argument named tail`, phrases only the refusal's own `what` clause composes, instead of a bare name the echoed deparsed call already carries. Proven by the mutation the old assertions survived: gutting the stopifnot naming clause now reddens 3, gutting the stop() one reddens 2. F2 — the AC1 key now comes from `norms_audit_stopifnot_conditions()` rather than a retyped copy, so test and walk cannot diverge; proven by construction, not mutation, since a derivation change correctly moves both sides together. B5 — the M60 citation corrected to M43. Walk file green at 46.
- 2026-08-15: a tenth mutant (walk key `collapse = " "` -> `"  "`) SURVIVED and is retracted as invalid, not recorded as a coverage hole: `squish()` collapses whitespace runs, so the two produce byte-identical keys (measured). M60's lesson exactly — a mutation perturbing a quantity the code is provably invariant to cannot redden, and reading that null as missing coverage is the error.
- 2026-08-15: REVIEW RETURN 1 — AC3 fails. F1 (85), verified independently: `refuse_unenumerable()` echoes the deparsed call, so `expect_match(msg, nm, fixed = TRUE)` passes even with the naming logic gutted; the single-name half of AC3's "names its cause" promise is unasserted. Riding the return: F2 (80), AC1's `long_key` retypes the shipped derivation instead of calling it, and B5, a misattributed M60 citation that should be M43 (verified against LESSONS). 28 findings below the bar logged in Review. Status review → in-progress.
- 2026-08-15: review in progress — PR #116 opened as draft; AC1-AC5 evidenced fresh and ticked, consistency gate clean (`cairn_validate` exit 0; the profile's toolchain checks are no-ops by file list, the branch touching only `tests/` and `cairn/`). AC6/AC7 deferred until the fresh-context reviewers finish, since a mutation pass would churn the tree they read.
- 2026-08-15: plan gate weighed D-042's bar on reopening this area and read this scope as distinct — no registry, matcher, matrix or denylist returns, no sweep is added, and the manifest check's promise is byte-unchanged; falsified by any criterion here widening what the manifest check promises.

## Decisions

## Review

### Fresh-context review (2026-08-15)

Three lenses, then a [S] scorer that generated none of the findings and was
given the diff and this plan. 30 candidates reported, filtered by none of the
lenses. Prior-PR-comments: no prior-review evidence, zero findings (the GitHub
inline-comment surface probed empty, so archived `## Review` sections were the
evidence base). Blame-history: 5 candidates, every one self-assessed a
non-violation — it traced the ordinal to M82, confirmed D-043 authorises the
deletion and names the weakening as the intended trade, and found D-042's
retirement bar respected. Diff-bug: 25 candidates.

**Actioned (>= 80): 2.**

- **F1 (85) — AC3's "names its cause" assertions are vacuous for every
  single-name case.** `refuse_unenumerable()` appends `deparse_call(cl)` to
  every message, so the probe call `stopifnot(exprs = x > 0)` already carries
  `exprs`. Verified independently at this branch tip: with the `what` clause
  replaced by a literal naming nothing, all three of `exprs`, `exprObject` and
  `local` still appear in the message and `expect_match(msg, nm, fixed = TRUE)`
  still passes. Only the `"tail, extra"` multi-name assertion is non-vacuous.
  **This is a return-floor finding — AC3 promises a test that asserts each site
  "raises and names its cause", and the naming half is unasserted.**
- **F2 (80) — AC1's `long_key` re-implements the shipped key derivation.** The
  test retypes `norms_audit_stopifnot_conditions()`'s own expression rather than
  calling it, the M76/M78 trap this repo has been bitten by three times. Not an
  AC failure — AC1's wording asks for the condition's full deparsed text, which
  the test does compute — so it rides the return as a quality fix.

**Below the bar, logged, not actioned (28).** F6 (78) two AC1 case labels
misdescribe the rejection mechanism, and only case 4 reaches the floor branch ·
F4 (72) `NORMS_AUDIT_VERDICT`'s `are not all TRUE` alternative is never
exercised · F5 (72) case 2 hard-codes the verdict instead of reusing the
constant · F11 (72) AC4/AC5 assertions sit behind the `data-raw/` skip, so they
do not run under `R CMD check`, including two that need no script · F12 (72)
`expect_setequal` is multiplicity-insensitive, so a fourth field duplicating an
existing name escapes · F17 (62) the AC1 fixture bypasses
`norms_audit_with_c_messages()` · F13 (62) nothing asserts the id builders paste
exactly the three fields · B5 (60) the M60 citation at
`test-norms-audit-walk.R:44` is misattributed; M43 is the lesson that says "the
probe was narrower than what it probed" (verified against LESSONS independently
of the scorer) · F18 (58) and F19 (55) two wrong line cross-references in new
comments · F9 (50) the `stopifnot_named`-absent assertion would redden on
legitimate future work · F3/F7/F8/F10/F22 (45) fixture self-certification,
two redundant assertion pairs, an unanchored loop rescued incidentally by the
assertion above it, and the stale word "fixture" · F14 (35) `$` partial-matching
· F21/F15 (30) a shadowed `c`, a tab separator unreachable today · F20 (20) a
dead parameter; its env-leak half was disproven by the scorer · B4 (15), B1/B2
(12), B3 (10) the blame lens's own non-findings · F23 (8) an artifact of reading
the milestone file mid-edit · F25 (8) D-042's text is untouched by this diff and
IP4 forbids editing it in place · F24 (12) and F16 (5) both disproven — the
per-AC counts reproduce exactly, and the twin-refusal verification found the
ordinal deletion sound.

### Re-review after return 1 (2026-08-15)

- **AC3 re-verified and re-ticked.** The naming assertions now match phrases
  only the refusal's own `what` clause composes. Evidence is the mutation the
  old assertions survived: gutting the `stopifnot` naming clause reddens 3, and
  the `stop()` one reddens 2. Test passes 11/11.
- **AC6 evidenced.** Nine valid mutants, nine killed, none survived — AC1's
  truncation regression 5 · AC2 unknown-kind 1 · AC2 named-match 2 · AC3
  reserved-set 2 · AC3 kind-collapse 1 · AC3 stop-refusal 1 · AC4/AC5
  fourth-field 33 · F1-A naming 3 · F1-B naming 2. Every mutant restored by
  copy from HEAD's blob, each restore confirmed by `git hash-object` against
  `git rev-parse HEAD:<path>` (`50fedd235c`, `b24345448a`) with a clean tree
  between runs.
- **One mutant retracted as invalid, not counted.** Changing the walk key's
  `collapse` from one space to two produces a byte-identical key, because
  `squish()` collapses whitespace runs (measured). A mutation perturbing a
  quantity the code is provably invariant to cannot redden, and reading that
  null as missing coverage is the error M60 names.
- **F2's fix is proven by construction, not by mutation.** The test and the
  walk now share one derivation and cannot diverge; a mutation of that
  derivation correctly moves both sides together, so no kill is claimable and
  none is claimed.

### Gate outcome: returned to `in-progress` (defect return 1)

F1 demonstrates AC3 failing inside the domain of the procedure it names, which
is the return floor. AC3 is unticked; AC1, AC2, AC4 and AC5 keep their evidence.

Reviewed 2026-08-15 against PR #116. Evidence executed fresh at this branch
tip, never recalled from the implement run.

### Acceptance criteria

- **AC1** — `test-norms-audit-walk.R::the stopifnot stem accepts and rejects as
  a partition` passes 10/10. The partition is asserted in both directions over
  five declared shapes, and the live-captured probe is asserted truncated
  before its accept case is trusted, so it cannot silently degrade into a
  non-truncated message.
- **AC2** — two tests, 6/6 and 5/5. The unknown-kind refusal names the kind it
  received; the `stopifnot_named` branch accepts equality and rejects both a
  strict superstring and a substring.
- **AC3** — three tests, 11/11, 5/5 and 9/9. Both `refuse_unenumerable()` sites
  raise and name their cause; the reserved-formal loop is anchored non-vacuous
  by pinning `"exprs"`, and the `stop()` refusal asserts the multi-name
  rendering as well as the single.
- **AC4** — `names(NORMS_AUDIT_MANIFEST)` is exactly `kind, binding, key`; all
  33 walked sites carry name-sets equal to `{kind, key, binding}`;
  `norms_audit_assign_ordinals` no longer exists. The spot-check grep
  `(\$|\.)ordinal|ordinal *=|assign_ordinals` over `tests tools` exits 1.
- **AC5** — `anyDuplicated()` asserted 0 on both the walked and the manifest
  identities (`test-norms-audit-manifest.R:65-66`).

### Consistency gate

- `cairn_validate` exit 0, all 16 checks PASS. The 47 `work-log format` WARNs
  are M7's pre-existing hard-wrapped history, untouched by this branch.
- Coverage completeness passes; no `DESIGN.md` principle changed, so
  `cairn_impact` is skipped by its own condition.
- Profile `consistency-gate` slot: the branch touches only `tests/` and
  `cairn/` — no `NAMESPACE`, `man/`, `data/*.rda`, `R/`, `src/`, `DESCRIPTION`,
  README, vignettes or `_pkgdown.yml` — so the generated-file, README and
  pkgdown checks are clean no-ops by inspection of the file list. No
  `.Rbuildignore` entry is owed: the one added file sits under
  `tests/testthat/`.
- No NEWS.md entry is owed. The deliverable is internal-tier test machinery
  over a `.Rbuildignore`d script; nothing a user of the package can observe
  changes.
