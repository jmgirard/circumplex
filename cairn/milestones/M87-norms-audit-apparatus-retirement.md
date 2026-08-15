# M87: Retire the norms-audit abort apparatus for a manifest check

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m87-norms-audit-apparatus-retirement`

## Goal

Replace the abort-site registry, matcher-floor, cross-discrimination and
denylist machinery M81–M83 built around `data-raw/audit-norms.R` with one
short manifest test that keeps the property the machinery existed for —
an abort site the tests do not know about fails the suite.

## Scope

Surface tier: **internal**. `data-raw/` and `cairn/` are `.Rbuildignore`d and
the test machinery is not installed, so no external consumer of the package
relies on any deliverable here; 77 of the suite's 90 blocks already skip under
`R CMD check`.

**In:** a manifest test that parses the audit script, collects its `stop()` and
`stopifnot()` sites, and asserts set equality against a checked-in manifest;
replacement of the 27 `expect_abort_at_site()` call sites with message-asserting
`expect_error()` calls under a literal-length floor and a cross-site
non-ambiguity check; deletion of `tests/testthat/test-norms-audit-denylist.R`,
of the registry/matcher/matrix blocks in `test-norms-audit-markers.R`
(`SCRIPT_ABORTS` at `:354` included), and of every helper definition left
without a caller; re-pointing `tools/m82-gate-floor.R` at the surviving tests
with fresh recorded FAIL counts; a `DECISIONS.md` entry naming the
discrimination surrendered and its reopening condition.

**Out:** the audit script's own guards — `validate_batch()`
(`data-raw/audit-norms.R:82-132`), `validate_roster()` (`:162-227`),
`roster_from_objects()` (`:620-706`) — stay byte-unchanged; whether that
~225-line guard surface should shrink is parked on the M86-lineage candidate
row that already owns `roster_from_objects()`. The
substantive audit tests (coverage report, provenance pins, sample key, value
comparison) are untouched. Fixing the two latent machinery defects in place is
refused: the retirement closes both on the merits.

## Acceptance criteria

- [ ] AC1 — `tests/testthat/test-norms-audit-denylist.R` is absent, and no
      top-level name defined in `tests/testthat/helper-norms-audit-script.R`
      is callerless: for each name enumerated by parsing that file's top-level
      assignments, `git grep -n <name> -- tests tools data-raw` reports at
      least one hit outside its own definition.
- [ ] AC2 — a surviving test parses `data-raw/audit-norms.R`, collects every
      `stop()` call and every `stopifnot()` condition including the run block,
      and asserts set equality with a checked-in manifest. The manifest is
      held to the build-time floor the retired matcher enforced: every `stop`
      key carries ≥15 literal characters and no key is all-placeholder, so
      none renders a regex accepting an arbitrary message. Verified by planted
      defect varying both form and location: a `stop()` site and a
      `stopifnot()` condition, each planted in a different top-level binding,
      each reddens the test alone, and each restores green on removal.
- [ ] AC3 — the discrimination the retired matcher enforced survives as a
      declared site assertion, never as a judgment about regexp text: each of
      the 17 `expect_abort_at_site()` calls that assert a script abort site
      (`test-norms-audit-batch.R` 7, `test-norms-audit-compare.R` 10) is
      replaced by `expect_audit_abort(expr, key)`, which fails unless `key` is
      present in the AC2 manifest, `key` selects exactly one manifest site,
      the message raised by `expr` is matched by that key under its own kind
      (regex match for `stop`; for a positional `stopifnot`, the observed stem
      is a prefix of the key carrying at least `min(nchar(key), 40)`
      characters, with no floor where R itself truncated; string equality for
      a named one), and that raised message
      is rendered by exactly one manifest key — the last condition folding the
      retired acceptance matrix's cross-site property into the per-call check.
      For every `expect_audit_abort()` call enumerated by parsing
      `tests/testthat/test-norms-audit-*.R` and
      `tests/testthat/test-norms-provenance.R`, those conditions hold; the
      sole permitted ambiguous key is the identically-messaged pair at
      `data-raw/audit-norms.R:321` and `:352`, accepted only where the call
      declares the binding it expects. The other 10 `expect_abort_at_site()`
      calls, all in `test-norms-audit-markers.R`, test the retired helper
      itself and are deleted by T3 rather than replaced. Plain
      `expect_error()` calls asserting interpolated content are outside this
      criterion's domain by construction — the distinction is which function
      the test calls, never a property of the regexp.
- [ ] AC4 — `tools/m82-gate-floor.R` names only tests that exist after the
      retirement; running it reports every one of its five mutants as a hit,
      and the fresh FAIL counts are recorded in the work log beside the commit
      measured.
- [ ] AC5 — `testthat::test_dir("tests/testthat", filter = "norms-audit")` and
      a separate run over `tests/testthat/test-norms-provenance.R` both report
      0 failures and 0 errors; surviving block and assertion counts are
      recorded against the pre-milestone 90 blocks / 496 assertions (the
      filter run measured 2026-08-15 at `1fb6bce1`, which excludes
      `test-norms-provenance.R`), and every block the Coverage section names
      as substantive still exists by title.
- [ ] AC6 — `git diff $(git merge-base master HEAD) -- data-raw/` is empty at
      the review gate: the audit script, `norms-audit-ledger.csv`,
      `norms-audit-coverage.csv` and `norms-audit-dispositions.csv` are all
      byte-unchanged.
- [ ] AC7 — a `DECISIONS.md` entry records the retirement, names the
      discrimination given up — the denylist's coverage of non-`stop()` abort
      spellings, whose bound survives only as prose once its pinning test
      goes; the acceptance matrix's build-time sweep of every matcher against
      every site, which the AC3 per-call check reaches only for sites a test
      exercises; and the opt-in nature of the new guard, a site assertion
      written as a plain `expect_error()` receiving none of it — and the class
      of evidence that reopens it; the two candidate rows
      describing defects in the retired machinery are struck through as closed
      on the merits, not deferred.
- [ ] AC8 — `devtools::check(args = "--no-manual")` clean, and
      `devtools::document()` produces no diff in `man/` or `NAMESPACE`.

## Coverage

- AC1 → T3, T4
- AC2 → T1
- AC3 → T2
- AC4 → T5
- AC5 → T6
- AC6 → T6
- AC7 → T7
- AC8 → T6

## Tasks

- [x] T1 — author the manifest and its parsing test first: plant a `stop()`
      and a `stopifnot()` condition in two different bindings of
      `data-raw/audit-norms.R`, watch each redden alone, restore by scratch
      snapshot and re-hash (never `git checkout --`), then build the manifest
      from the unplanted script.
- [ ] T2 — write `expect_audit_abort()` and convert the 17 script-abort sites
      (batch 7, compare 10); the markers 10 go to T3. Keep the kind-aware
      match and the key-regex/stem helpers the retired matcher used, and
      record the `:321`/`:352` pair as the one declared-binding exception.
- [ ] T3 — delete `test-norms-audit-denylist.R` and the registry, ordinal,
      matcher-floor and acceptance-matrix blocks of
      `test-norms-audit-markers.R`, including the blocks the criteria audit
      named vacuous (`:742`, `:802`, `:1036`, `:1041`, `:1245`).
- [ ] T4 — strip `helper-norms-audit-script.R` to definitions that still have
      callers; run AC1's callerless-name sweep and fix what it names.
- [ ] T5 — re-point `tools/m82-gate-floor.R`'s three orphaned mutants at
      surviving tests, re-run all five, re-record their FAIL counts.
- [ ] T6 — full suite, provenance file, `check()`, `document()`; confirm
      `git status` clean and `data-raw/` untouched before recording any gate
      as green.
- [ ] T7 — `DECISIONS.md` entry, ROADMAP row dispositions, LESSONS retirement
      check (the M82 mutation-harness lessons lose their subject here).

## Work log

- 2026-08-15: created by /milestone-plan.
- 2026-08-15: plan gate chose a manifest test over deleting the script-quantified check outright because per-test regexps quantify over the tests, not the script, so an unguarded new abort site would leave the suite green; falsified by the manifest proving unmaintainable in practice — a review finding it stale against the script it claims to enumerate.
- 2026-08-15: plan gate chose re-pointing `tools/m82-gate-floor.R` over retiring it with the apparatus because it is the only instrument measuring whether these tests bite; falsified by its mutants proving unmaintainable against the surviving tests, or by the mutation harness moving into the suite itself.
- 2026-08-15: plan gate chose a tests-only scope over also shrinking the script's ~225-line guard surface because the batch guards still protect a hand-edit of `AUDIT_BATCH` when instrument 16 is added; falsified by evidence that no maintainer path reaches those guards.
- 2026-08-15: branch `m87-norms-audit-apparatus-retirement` cut from master at 484ae30a; status in-progress.
- 2026-08-15: amendment gate — AC2, AC3, AC7 and T2 amended after a fresh-context [O] audit of the proposed AC3 wording returned four findings: the conversion count was 27 where only 17 assert script abort sites (the markers 10 test the retired helper and have keys absent from the script, making the criterion unsatisfiable as written); "the message the key renders" was unimplementable against `{}` templates and needed the kind-aware match; the acceptance matrix's cross-site property was neither preserved nor named as surrendered; and dropping the literal floors fails open on an all-placeholder key, `norms_audit_key_regex()` returning "." (two incidents recorded at helper-norms-audit-script.R:594-598 and :706-708). All four repaired; Jeff accepted the message-uniqueness fold at the gate.
- 2026-08-15: AC2 amended a second time — the stem floor was written "≥40" where the retired rule is `min(nchar(squish(key)), 40)` (helper-norms-audit-script.R:718), unsatisfiable against the two 20/21-character `stopifnot` keys the script actually carries; the floor clause now covers `stop` keys only and the stem rule moved into AC3's kind-aware match. Jeff approved at the gate; the wrong number was mine, introduced in the first amendment.
- 2026-08-15: T1 — manifest derived by walking `data-raw/audit-norms.R` at 8604a203 rather than hand-typed: 33 sites, 30 `stop` and 3 positional `stopifnot`, minimum literal-character count 23 against the floor of 15, zero all-placeholder keys, and exactly one duplicated (kind, key) — `source note not found: {}` from `source_note_block_tags` and `parse_source_note`.
- 2026-08-15: T1 planted-defect probe (scratchpad, results here): baseline FAIL=0; a `stop()` planted in `empty_ledger` FAIL=2; a `stopifnot()` condition planted in the run block FAIL=2 — two forms in two bindings, each reddening alone. Restored by scratch snapshot with `git hash-object` re-checked against the clean blob, never `git checkout --`; `git status` confirmed `data-raw/` untouched afterwards.
- 2026-08-15: criteria audit ([O], fresh context) returned findings on AC1–AC5 and none on AC6–AC7; `_problems/` grep scoping, AC3 already-true-at-HEAD, AC4's comment-line and missing `stopifnot()` domain, and AC5's unrelated counts were fixed at the gate; the hand-list-vs-procedure finding became the first gate question.

## Decisions

## Review
