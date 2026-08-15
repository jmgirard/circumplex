# M87: Retire the norms-audit abort apparatus for a manifest check

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m87-norms-audit-apparatus-retirement` · https://github.com/jmgirard/circumplex/pull/115

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
conversion of the 17 script-abort `expect_abort_at_site()` calls to a new
`expect_audit_abort()` helper that resolves a manifest key to a single site and
checks the raised message against it, the other 10 being deleted with the
machinery they test; deletion of `tests/testthat/test-norms-audit-denylist.R`,
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

- [x] AC1 — `tests/testthat/test-norms-audit-denylist.R` is absent, and no
      top-level name defined in `tests/testthat/helper-norms-audit-script.R`
      is callerless: for each name enumerated by parsing that file's top-level
      assignments, `git grep -n <name> -- tests tools data-raw` reports at
      least one hit outside its own definition.
- [x] AC2 — a surviving test parses `data-raw/audit-norms.R`, collects every
      `stop()` call and every `stopifnot()` condition including the run block,
      and asserts set equality with a checked-in manifest. The manifest is
      held to the build-time floor the retired matcher enforced: every `stop`
      key carries ≥15 literal characters and no key is all-placeholder, so
      none renders a regex accepting an arbitrary message. Verified by planted
      defect varying both form and location: a `stop()` site and a
      `stopifnot()` condition, each planted in a different top-level binding,
      each reddens the test alone, and each restores green on removal.
- [x] AC3 — the discrimination the retired matcher enforced survives as a
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
- [x] AC4 — `tools/m82-gate-floor.R` names only tests that exist after the
      retirement; running it reports every one of its five mutants as a hit,
      and the fresh FAIL counts are recorded in the work log beside the commit
      measured.
- [x] AC5 — `testthat::test_dir("tests/testthat", filter = "norms-audit")` and
      a separate run over `tests/testthat/test-norms-provenance.R` both report
      0 failures and 0 errors; surviving block and assertion counts are
      recorded against the pre-milestone 90 blocks / 496 assertions (the
      filter run measured 2026-08-15 at `1fb6bce1`, which excludes
      `test-norms-provenance.R`), and every block the Coverage section names
      as substantive still exists by title.
- [x] AC6 — `git diff $(git merge-base master HEAD) -- data-raw/` is empty at
      the review gate: the audit script, `norms-audit-ledger.csv`,
      `norms-audit-coverage.csv` and `norms-audit-dispositions.csv` are all
      byte-unchanged.
- [x] AC7 — a `DECISIONS.md` entry records the retirement, names the
      discrimination given up — the denylist's coverage of non-`stop()` abort
      spellings, whose bound survives only as prose once its pinning test
      goes; the acceptance matrix's build-time sweep of every matcher against
      every site, which the AC3 per-call check reaches only for sites a test
      exercises; and the opt-in nature of the new guard, a site assertion
      written as a plain `expect_error()` receiving none of it — and the class
      of evidence that reopens it; the two candidate rows
      describing defects in the retired machinery are struck through as closed
      on the merits, not deferred.
- [x] AC8 — `devtools::check(args = "--no-manual")` clean, and
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
- [x] T2 — write `expect_audit_abort()` and convert the 17 script-abort sites
      (batch 7, compare 10); the markers 10 go to T3. Keep the kind-aware
      match and the key-regex/stem helpers the retired matcher used, and
      record the `:321`/`:352` pair as the one declared-binding exception.
- [x] T3 — delete `test-norms-audit-denylist.R` and the registry, ordinal,
      matcher-floor and acceptance-matrix blocks of
      `test-norms-audit-markers.R`, including the blocks the criteria audit
      named vacuous (`:742`, `:802`, `:1036`, `:1041`, `:1245`).
- [x] T4 — strip `helper-norms-audit-script.R` to definitions that still have
      callers; run AC1's callerless-name sweep and fix what it names.
- [x] T5 — re-point `tools/m82-gate-floor.R`'s three orphaned mutants at
      surviving tests, re-run all five, re-record their FAIL counts.
- [x] T6 — full suite, provenance file, `check()`, `document()`; confirm
      `git status` clean and `data-raw/` untouched before recording any gate
      as green.
- [x] T7 — `DECISIONS.md` entry, ROADMAP row dispositions, LESSONS retirement
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
- 2026-08-15: T2 — 17 sites converted to `expect_audit_abort()` (compare 10, batch 7, the last a multi-line `paste0` key done by hand); both files green at 28 and 49 assertions.
- 2026-08-15: T2 discrimination probe (scratchpad, results here): correct key 0 failures; a WRONG but real manifest key fails naming both sides ("AUDIT_BATCH$divisor is missing for: {} -- got: AUDIT_BATCH$divisor must be numeric, not character"); a key absent from the manifest refuses by name; an expression raising nothing fails; the ambiguous key without `binding` refuses naming its 2 sites.
- 2026-08-15: T2 method note — a first pass with a DOTALL regex spanned ACROSS call sites, renaming one call while stranding its `norms_audit_matcher` argument and leaving the next site with a bare string; caught by re-reading the file, reverted with a two-path `git checkout --` (no uncommitted work in either), and redone line-anchored requiring the matcher on the immediately following line. Verify a batched edit landed before trusting it.
- 2026-08-15: T3 — `test-norms-audit-denylist.R` removed (5 blocks); `test-norms-audit-markers.R` cut from 1292 to 348 lines and 30 blocks to 10, deleting the `SCRIPT_ABORTS` registry and the 20 M81–M83 apparatus blocks at :626–:1207 while keeping the 10 M79 source-note marker-parsing blocks. Cut verified first: `note_dir`, `table_head`, `site()` and `sites()` are defined and used only inside the deleted range, so no survivor lost a helper. Suite after: 55 blocks, 281 assertions, 0 failures, 0 errors, 0 skips (was 90 blocks / 496 assertions at 1fb6bce1, which counted the denylist and markers apparatus).
- 2026-08-15: T4 — helper cut 756 -> 358 lines, 46 top-level definitions to 27, by transitive reachability from the names with an external caller rather than by a hand-list; the 19 dropped definitions are the registry, matcher, acceptance matrix, stack-capture, site-id and denylist machinery, each removed with its own comment block. AC1 sweep after: 27 definitions, 0 callerless (a reference counts from the parse tree, so a comment naming a function is not a caller).
- 2026-08-15: T4 — the helper header's consumer list named three surfaces this milestone deleted and "17 expect_abort_at_site() calls" that no longer exist; replaced with the derivation (`git grep -l norms_audit_ -- tests`) rather than a fresh list, which is what the comment itself warns about. The manifest test also now calls `norms_audit_key_literals()` instead of retyping its `gsub()` (M78).
- 2026-08-15: T5 — the three mutants naming deleted tests re-pointed at "the manifest is set-equal to a fresh walk of the audit script (M87)"; measured at 326cfdb0, baseline failed=0, AC3-1 failed=2, AC3-2 failed=3, T4 failed=2, AC4 failed=2, AC4-control failed=0, GATE FLOOR OK, restored blob c1c11a6e matching baseline. The `recorded` values stay floors, as the tool's own header says.
- 2026-08-15: T5 — mutants 4 and 5 were ALREADY broken before this milestone: their anchor `nms <- get("instrument_names", envir = ns)()` carried four leading spaces where the script has two, so the tool errored out rather than running them. Pre-existing drift, not introduced here; fixed with the rest.
- 2026-08-15: T7 — D-042 appended recording the retirement, the three surrendered properties and the reopening condition; the two candidate rows were already struck through as closed by M87 at the plan gate. LESSONS: nothing retires here — the M82 mutation-harness lessons still hold and their instrument (tools/m82-gate-floor.R) survives; the DOTALL-regex-spanning-call-sites gotcha from T2 is a capture candidate for post-merge hygiene, not a retirement.
- 2026-08-15: T6 — full suite 925 blocks / 7051 assertions / 0 failures / 0 errors / 3 skips; norms-audit filter 55 blocks / 281 assertions; test-norms-provenance.R 13 blocks / 200 assertions, all 0/0. `document()` left man/ and NAMESPACE byte-identical and rewrote only DESCRIPTION's Config/roxygen2/version 8.0.0 -> 8.1.0, reverted rather than committed (M85: a machine-wide toolchain stamp, the maintainer's call).
- 2026-08-15: T6 — `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes, 32m25s, run with R_MAKEVARS_USER=FLIBS= for the known local gfortran `emutls_w` link failure (M82); AC6 verified, `git diff $(git merge-base master HEAD) -- data-raw/` empty, so the script and all three CSVs are byte-unchanged. Branch diffstat: 496 insertions, 1566 deletions across 11 files.
- 2026-08-15: review — PR #115 opened draft; all 8 criteria verified with fresh evidence and ticked; cairn_validate exit 0; profile consistency-gate clean (document() warning-free and no man/NAMESPACE diff, pkgdown no problems, README in sync, no NEWS owed at 0 runtime files changed).
- 2026-08-15: review — three fresh-context lenses returned 29 candidate findings; an independent [S] scorer scored 4 at >=80, none >=90, so the return floor did not fire. Fixed on the branch: F22 (88) D-042's "Net -1488 lines" was wrong, real total 498 insertions / 1567 deletions = net -1069, the figure having summed only the shrinking files; F24 (85) D-042's "24 expect_error() calls" restated as its derivation; F4 (82) audit_key_matches() fell through to the loosest branch on an unknown kind, fail-closed refusal restored; F14 (82) a floor-headroom claim naming a file that now asserts neither floor. Also fixed below the bar: F16 (72), the replacement consumer list that reinstated the stale-enumeration defect it warned against.
- 2026-08-15: review — F13 (78) logged not actioned: norms_audit_stopifnot_stem, the named-condition keying, refuse_unenumerable and norms_audit_assign_ordinals survive on the manifest path but lost every unit test with the deleted apparatus. Raised at the approval gate rather than silently logged; ROADMAP is at 59/60 lines so a new candidate row needs a fold-in, not an added line.
- 2026-08-15: review — post-fix `devtools::check(args = "--no-manual")` re-run on the final tree after the four scored fixes landed: Status OK, 0 errors / 0 warnings / 0 notes, 24m41s. AC8's tick rests on this run, not on the earlier pre-fix one.
- 2026-08-15: review — F22's repair was itself defective: the pinned figure written into D-042 went stale as soon as further review commits landed (branch tip now 618/1577, not the 498/1567 recorded). Replaced with the derivation and no figure at all, which is what the derived-figure rule asks for and what the original defect should have taken.
- 2026-08-15: criteria audit ([O], fresh context) returned findings on AC1–AC5 and none on AC6–AC7; `_problems/` grep scoping, AC3 already-true-at-HEAD, AC4's comment-line and missing `stopifnot()` domain, and AC5's unrelated counts were fixed at the gate; the hand-list-vs-procedure finding became the first gate question.

## Decisions

## Review

Evidence gathered 2026-08-15 at branch tip, PR #115. Every figure below is from
a command run at review, never recalled.

- **AC1** — `tests/testthat/test-norms-audit-denylist.R` absent (`ls` reports no
  such file). Callerless sweep over the helper's top-level assignments, counting
  a reference from the parse tree so a comment naming a function is not a
  caller: **27 definitions, 0 callerless**.
- **AC2** — manifest set-equality test green; **33 rows, 30 `stop` + 3 positional
  `stopifnot`**, minimum `stop`-key literal count **23** against the floor of 15,
  **0** all-placeholder keys. Planted-defect probe re-run fresh: baseline FAIL=0,
  a `stop()` planted in `empty_ledger` FAIL=2, a `stopifnot()` condition planted
  in the run block FAIL=2 — two forms, two bindings, each reddening alone; script
  restored from scratch snapshot with `git hash-object` matching the clean blob.
- **AC3** — parse of the audit test files finds **17 `expect_audit_abort()` call
  sites**, **0** with a key absent from the manifest, **0** ambiguous without a
  declared `binding`, and **0** surviving `expect_abort_at_site` references
  anywhere in `tests/` or `tools/`.
- **AC4** — `Rscript tools/m82-gate-floor.R`: baseline failed=0; AC3-1 failed=2,
  AC3-2 failed=3, T4 failed=2, AC4 failed=2, AC4-control failed=0; every mutant
  OK, `GATE FLOOR: OK`, restored blob c1c11a6e matching baseline.
- **AC5** — `test_dir(filter = "norms-audit")` **55 blocks / 281 assertions**,
  `test-norms-provenance.R` **13 blocks / 200 assertions**, both 0 failures and
  0 errors, against the pre-milestone 90 blocks / 496 assertions at `1fb6bce1`.
  Full suite **925 blocks / 7051 assertions / 0 failures / 3 skips**. The
  substantive blocks the Coverage section fences (coverage report, provenance
  pins) are present: 22 `test_that` titles across those two files.
- **AC6** — `git diff $(git merge-base master HEAD) -- data-raw/` is **empty**:
  the audit script and all three CSVs byte-unchanged.
- **AC7** — D-042 present in `cairn/DECISIONS.md` naming the three surrendered
  properties and the reopening evidence; both candidate rows struck through as
  closed by M87 in `cairn/ROADMAP.md`.
- **AC8** — `document()` emits **0** lines matching `resolve link` at
  `cli.width = 500` and leaves `man/` and `NAMESPACE` byte-identical (only
  DESCRIPTION's roxygen stamp moved, reverted per M85).
  `devtools::check(args = "--no-manual")` **0 errors / 0 warnings / 0 notes**.

**Consistency gate.** `cairn_validate` exit 0, every check PASS including
`coverage complete`; two advisories, `sizing` (8 acceptance criteria against a
7 tripwire) and `work-log format` (47, all but four pre-existing on M7). Profile
`consistency-gate` slot: `document()` no-diff and warning-free as above,
`pkgdown::check_pkgdown()` "No problems found", README.Rmd/README.md untouched
and in sync, no NEWS entry owed (0 runtime files changed — `R/`, `src/` and
`NAMESPACE` are untouched, which is what an internal-tier milestone should
show). No principle changed, so `cairn_impact` was not run.

**Independent review.** Three fresh-context lenses (diff-bug [O], blame-history
[S], prior-PR-comments [S]) reported 29 candidate findings; a separate [S]
scorer that generated none of them scored each. **Four scored ≥80, none ≥90.**
No finding demonstrated an acceptance criterion failing and none was ≥90 on
deliverable behaviour, so the return floor did not fire.

Actioned (≥80), all fixed on the branch:

- **F22 (88)** — D-042 claimed "Net −1488 lines"; the real branch total is 498
  insertions / 1567 deletions, net −1069. The figure summed only the three
  shrinking files and omitted the 335 lines the replacement adds. Replaced with
  its derivation plus a pinned measured figure, per the derived-figure rule.
- **F24 (85)** — D-042's "24 `expect_error()` calls" could not be reproduced by
  the reviewer under any scoping. It was in fact roster's 22 plus provenance's
  2, but the sentence claimed more than that set. Replaced with the grep that
  derives it.
- **F4 (82)** — `audit_key_matches()` fell through to the loosest branch on an
  unrecognised kind, dropping a refusal the retired matcher made by name after
  a stale dispatch did exactly this in M81. Fail-closed refusal restored.
- **F14 (82)** — `helper-norms-audit-script.R` claimed both floors' headroom was
  asserted in the markers file, which now asserts neither. Corrected to say
  STOP_KEY_FLOOR's headroom is asserted in the manifest test and STEM_FLOOR's
  nowhere.

Also fixed though below the bar: **F16 (72)**, the helper header's replacement
consumer list, which reinstated the stale-enumeration defect it had just warned
against and was already incomplete.

Logged, not actioned (below 80): F13 (78) surviving walk/matcher helpers lost
all their unit tests to the deletion — the most substantive sub-threshold item,
raised at the approval gate; F12 (75) the manifest test hand-rolls the parse
rather than calling the helper's single entry point; F16 (72) fixed anyway;
F6 (68) the test file header says "these skip" but block 2 does not; F15 (68)
two stale comment referents; F3 (62) declaring `binding` buys no site
discrimination for the ambiguous key; F20/F27 (62/60) three gate-floor mutants
now name one test; F21 (62) stale tool header; F25 (62) D-042's surrendered
list arguably omits two items; F26 (58) the manifest's "GENERATED" note has no
committed generator; F1 (55) a nested re-raised message can match two keys,
latent; F5 (55) dead `stopifnot_named` branch; F7 (55) empty-manifest skip;
F19 (45) gate-floor `recorded` floors unchanged; F2 (40) `binding` ignored when
the key is unambiguous; F9 (35), F8 (32), F18 (32), F28 (30), F29 (22),
F10/F11 (20), F17 (15), and **F23 (5), which the scorer refuted** — it claimed
the script is 1260 lines; `wc -l` gives 1262, so D-042 was right.
