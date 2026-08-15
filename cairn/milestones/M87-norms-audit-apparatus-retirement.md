# M87: Retire the norms-audit abort apparatus for a manifest check

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** —

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
      and asserts set equality with a checked-in manifest. Verified by planted
      defect varying both form and location: a `stop()` site and a
      `stopifnot()` condition, each planted in a different top-level binding,
      each reddens the test alone, and each restores green on removal.
- [ ] AC3 — the discrimination the retired matcher enforced at build time
      survives as a floor: for every `expect_error()` call enumerated by
      parsing `tests/testthat/test-norms-audit-*.R` and
      `tests/testthat/test-norms-provenance.R`, its `regexp` carries ≥15
      literal characters (the retired `NORMS_AUDIT_STOP_KEY_FLOOR`), and
      matching that `regexp` against the AC2 manifest's message keys selects
      exactly one site. The pair of identically-messaged sites at
      `data-raw/audit-norms.R:321` and `:352` is asserted to be the one
      recorded exception, named in the manifest.
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
      discrimination given up (the denylist's coverage of non-`stop()` abort
      spellings, whose bound survives only as prose once its pinning test
      goes) and the class of evidence that reopens it; the two candidate rows
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

- [ ] T1 — author the manifest and its parsing test first: plant a `stop()`
      and a `stopifnot()` condition in two different bindings of
      `data-raw/audit-norms.R`, watch each redden alone, restore by scratch
      snapshot and re-hash (never `git checkout --`), then build the manifest
      from the unplanted script.
- [ ] T2 — replace the 27 `expect_abort_at_site()` sites (markers 10, compare
      10, batch 7) with `expect_error()` calls; add the literal-length floor
      and the manifest-key uniqueness check, recording the `:321`/`:352`
      exception.
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
- 2026-08-15: criteria audit ([O], fresh context) returned findings on AC1–AC5 and none on AC6–AC7; `_problems/` grep scoping, AC3 already-true-at-HEAD, AC4's comment-line and missing `stopifnot()` domain, and AC5's unrelated counts were fixed at the gate; the hand-list-vs-procedure finding became the first gate question.

## Decisions

## Review
