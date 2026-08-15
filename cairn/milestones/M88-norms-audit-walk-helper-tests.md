# M88: Fence the norms-audit walk helpers M87 kept

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2
- **Branch/PR:** `m88-norms-audit-walk-helper-tests`

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

- [ ] AC1 A test asserts `audit_key_matches("stopifnot", key, msg)` over an
      enumerated vector of message shapes, each declared accepted or rejected and
      asserted in both directions, containing at least: a message R itself raised
      by overflowing a `stopifnot()` condition past one deparsed line, keyed on
      that condition's full deparsed text (accepted); the same message with its
      trailing verdict clause removed (rejected); an untruncated whole-condition
      message (accepted); a below-floor stem carrying the verdict but no
      truncation marker (rejected). The first shape is captured from a live
      `stopifnot()` at test time, never hand-typed.
- [ ] AC2 A test asserts `audit_key_matches()` raises, naming the kind it got,
      for a kind outside `stop`/`stopifnot`/`stopifnot_named`; and asserts the
      `stopifnot_named` branch in both directions — a message equal to the key
      accepted, a message carrying the key as a strict superstring rejected.
- [ ] AC3 A test asserts each `refuse_unenumerable()` site raises and names its
      cause: `norms_audit_stopifnot_conditions()` for a condition passed under any
      element of `STOPIFNOT_RESERVED`, iterated as the running R defines it and
      anchored non-vacuous by pinning `"exprs"` as a literal member; and
      `norms_audit_stop_key()` for a `stop()` carrying one and more than one
      message-concatenated named argument, the multi-name rendering asserted.
      Each is paired with a negative that must not raise — a positional
      condition, a named condition, a `stop()` with no names, and one carrying
      only `call.`/`domain`.
- [ ] AC4 The abort-site identity is (kind, binding, key). A test asserts the
      field set directly rather than by spelling: `names(NORMS_AUDIT_MANIFEST)`
      is exactly `c("kind", "binding", "key")`, and every element of
      `norms_audit_abort_sites()` has `names()` equal as a set to
      `c("kind", "key", "binding")`, so a fourth field on either side reddens
      the suite under any spelling; `manifest_ids()` and `walked_ids()` each
      paste exactly those three. As a spot check on the three spellings the
      retired mechanism used,
      `git grep -nE '(\$|\.)ordinal|ordinal *=|assign_ordinals' -- tests tools`
      returns no line — prose explaining the removal may name `ordinal`.
- [ ] AC5 A test asserts the walked identities and the manifest identities are
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

- [ ] T1 Cut `m88-norms-audit-walk-helper-tests` from the up-to-date default
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
- 2026-08-15: plan gate weighed D-042's bar on reopening this area and read this scope as distinct — no registry, matcher, matrix or denylist returns, no sweep is added, and the manifest check's promise is byte-unchanged; falsified by any criterion here widening what the manifest check promises.

## Decisions

## Review
