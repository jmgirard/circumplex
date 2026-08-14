# M80: Give the norms-audit coverage report a machine-readable key

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M79, M81
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m80-norms-audit-report-schema` / https://github.com/jmgirard/circumplex/pull/109

## Goal

Make every `data-raw/norms-audit-coverage.csv` row keyed rather than
string-pasted, and refuse a batch whose comparison parameters are unusable.

## Scope

**In:** the coverage frame's schema and `AUDIT_BATCH` input validation. Six
coverage emitters paste their payload into whichever column is free —
`field` becomes `"M (sample 1)"` at `data-raw/audit-norms.R:406` and `:414`,
`instrument` becomes `"horowitz2003 (iip32)"` at `:448`, and a note-only row
puts its `scale` in `field` and its `value` in `scale` at `:382-383` — so no
row can be joined to anything by machine. Also here: three comparison-side
defects that let a check pass without comparing anything.

**Out:** the shipped-roster sweep, the shared-note key collision, and the
marker/tag parser → M79, which this depends on because both milestones rewrite
the same emitters. Changing any value in `data/` → not here.

## Acceptance criteria

- [ ] AC1 Every coverage row carries a machine-readable key: `field` holds a
      bare field name and any sample label rides in its own `sample` column
      rather than being pasted into `field` (`:406`, `:414`). A header comment
      states the cell contents for each `side`, including the four that carry
      no field or scale of their own — `note-only-sample`,
      `constructed-credit-reference`, `note-sample-not-audited`,
      `note-block-not-audited` — whose payloads ride in mislabelled columns
      today. Coverage rows and ledger rows are disjoint by construction, a
      missing-key coverage row having no ledger counterpart, so what is
      required is a joinable key and not a join.
- [ ] AC2 The coverage report's `instrument` column holds a shipped instrument
      name, and `NA` only where no batch row identifies one
      (`note-block-not-audited`); a `note-sample-not-audited` row carries the
      claiming instrument from `blocks[[bkey]]$instrument`. Citekey and block
      tag ride in their own columns rather than inside `instrument` (`:448`).
- [ ] AC3 A `note-only` coverage row appears once per citekey, block, and the
      row's own payload — the note row's `scale`, `value` and `anchor` cells,
      the key being taken from the note row rather than from the report, whose
      ten columns do not include `anchor` — not once per batch pass that reads
      the block. A test asserts the full run still emits 14 note-only rows, and
      a fixture test asserts that two note-only rows in one block differing only
      in their `anchor` cell emit two coverage rows (no committed note has that
      shape). Every note-only row in the repo carries `sample = "—"` in the
      note, so a key including `sample` but not the payload collapses those 14
      to the 9 blocks that carry them (measured 2026-08-13 over the committed
      notes).
- [ ] AC4 `validate_batch()` refuses a `divisor` that is missing,
      non-numeric, `NA`, non-finite, or not strictly positive, each shape with
      its own test asserting the specific message.
- [ ] AC5 `values_agree()` compares `Items` after normalising both sides
      through `normalise_items()` — today only the shipped side is normalised
      (`:294` against the fall-through at `:336`) — and `normalise_items()`
      aborts on a cell that is not a comma-separated list of integers rather
      than coercing it to the string `"NA"`. A test asserts two unparseable
      cells do not compare equal, which without the abort they would.
- [ ] AC6 Instrument-level note rows (`sample` = the NO_SAMPLE token) in a
      block that no `scales = TRUE` batch pass reads are reported as
      non-exempt coverage rather than discarded at `:375-377`. Rows in a block
      whose instrument does have a `scales = TRUE` pass are covered by that
      pass and produce no additional row. A fixture supplies the uncovered
      case; no note in the repo has one today.
- [ ] AC7 `devtools::test()` and `devtools::check(args = "--no-manual")` clean;
      `data-raw/norms-audit-coverage.csv` is regenerated and committed with the
      run that produces it, and the ledger CSV is unchanged but for its stamps.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1 Fix the coverage frame's columns — instrument, citekey, tag, side,
      field, sample, scale, label, detail, exempt — and document each side's
      cell contents in a header comment beside `audit_norms()`.
- [x] T2 Rewrite the seven emitters onto that schema; update the coverage
      assertions in `tests/testthat/test-norms-provenance.R` and
      `tests/testthat/test-norms-audit-roster.R`.
- [x] T3 Dedupe note-only rows on citekey, tag and payload; test the full run
      still emits 14.
- [x] T4 `divisor` validation in `validate_batch()` (`:82-99`), one test per
      refused shape.
- [x] T5 Make `normalise_items()` abort on a non-integer-list cell and
      normalise the source side in `values_agree()`; test the two-unparseable
      -cells case.
- [x] T6 Report instrument-level note rows in a block no `scales = TRUE` pass
      reads; fixture note plus test for both the uncovered and the
      already-covered case.
- [x] T7 Regenerate the coverage CSV, run `devtools::test()` and
      `devtools::check(args = "--no-manual")`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context, authored none of the criteria) returned findings on all 5 drafted criteria; all adopted. Two were load-bearing: AC1's stated rationale was impossible, coverage rows and ledger rows being disjoint by construction, so the criterion now asks for a joinable key rather than a join; and AC3's drafted dedupe key of (citekey, block, sample) would itself have deleted 6 of the 14 shipped note-only rows, every one of which carries `sample = "—"` with its payload in `scale` — the silent row loss the rest of the script refuses.
- 2026-08-08: plan gate chose two milestones over one, putting this one second because both rewrite the same coverage emitters and a shared-emitter conflict is cheaper to take in sequence than in review; falsified by M79 landing without touching the emitters, which would make the dependency spurious.
- 2026-08-08: AC6 arrives here from M79's draft, where the criteria audit measured that implementing it as drafted would emit 16 duplicate rows for each of 8 passes; the real hole is narrower and has no instance in the repo, which is why it sits in the report milestone rather than the silent-loss one.
- 2026-08-13: Depends-on mirror reconciled to ROADMAP (M79 → M79, M81); M81's plan added the dependency to the ROADMAP row and never mirrored it into this header. Bookkeeping only, no scope change.
- 2026-08-13: started by /milestone-implement on `m80-norms-audit-report-schema`.
- 2026-08-13: T1/T2/T3/T6 done as one rewrite — all four change the same emitters, and splitting them would have meant three passes over the same lines. Schema, dedupe and the AC6 sweep land together with `tests/testthat/test-norms-audit-coverage.R`.
- 2026-08-13: T1 minor amendment at the implementation gate: the schema takes two free-text columns, `label` and `detail`, beside the eight the plan named. A note-only row carries two free-text cells (the note's name for the unshipped material and its description) and the eight key columns have nowhere to put either once each holds only its own fact; Jeff chose the two-column shape over pasting them together or dropping the description.
- 2026-08-13: T4 done in `tests/testthat/test-norms-audit-batch.R`; the four new `stop()` sites are registered in M81's abort registry, which is what forced them to carry fixtures. The missing-column shape needed no new guard — M72's required-names `stopifnot()` already covers it, so its test asserts that site instead.
- 2026-08-13: T5 done in `tests/testthat/test-norms-audit-compare.R`; normalising the source side changes no committed ledger row, the notes' item keys having been written unpadded — the run stays 194 ledger rows, 15 coverage rows, 0 gaps.
- 2026-08-13: paused at the maintainer's choice with two items open, both decided but neither built: AC5's third attempt (approach not chosen — the routing chip offered a direct fix and a `/milestone-brief` escalation, and the maintainer paused instead of picking), and G3, which the maintainer ruled IN as D-M80-1 above. Resume with `/milestone-implement M80`; nothing is half-applied, the branch is pushed at `154c8524` and the suite is green but for the AC5 hole this pause leaves open.
- 2026-08-13: review round 2 returned to `in-progress` (defect return 2). AC5 fails again: round 1's fix replaced the `grepl("^[0-9]+$", p)` shape test with `as.integer()`'s verdict instead of composing the two, so `normalise_items("1.5, 9")` returns `"1, 9"` and `values_agree("Items", "1, 9", "1.4, 9", 1)` is TRUE — a decimal, hex, scientific or signed cell is now coerced where the pre-fix guard refused it. One finding at the bar (G1, 93); 15 logged below it, 5 of them round 1's own sub-threshold findings re-reported unchanged. Thrash trigger (b) fires — one criterion, two failures, both a guard admitting a cell it should refuse — and the plan gate recorded no alternative against, so `/milestone-brief` escalation is offered at the routing chip.
- 2026-08-13: return-1 verification: audit re-run at `a834445a` — 194 ledger rows, 15 coverage rows, 0 gaps, 14 note-only; ledger identical to the committed one bar its three stamp columns and the coverage CSV byte-unchanged, so none of the six fixes moved a reported value. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 7000 (up 26 from round 1's 6974, the new fixtures). `devtools::check(args = "--no-manual")` Status OK, 0/0/0, 7m 39s. Back to `review`.
- 2026-08-13: return-1 fixes. F3: the `Items` guard now refuses on `as.integer()`'s own verdict rather than on a shape test standing in for it, which is what let a digit string past integer range coerce to the string `"NA"` and compare equal to another; F4 came with it, the field count now taken from the separators so a trailing comma is malformed rather than a two-item key. F1: the note-only key is the note row — scale, value and anchor — and is applied within a pass as well as across passes; the post-hoc `duplicated()` over the assembled frame is gone, which takes F2's cross-side reach with it. F8, F9: the two value-level emitters and AC2's claiming-instrument clause have committed tests for the first time. F10: `nzchar(NA_character_)` is TRUE, so the payload assertion now tests `!is.na()` first. F12, F13 fixed as false comments though both scored below the bar, being prose this branch authored.
- 2026-08-13: the anchor mutation caught a second defect in the first fix: keying only against EARLIER passes left the key inert for a block read once, every row of a single pass being unseen whatever it says, so the anchor could be dropped with no test able to tell. Found by mutating the key line, not by reading it. All four rewritten guards now redden under mutation — anchor dropped from the key (3 failures), the pre-M80 paste restored in the value-level emitter (2), `note-sample-not-audited` carrying the citekey again (1), the `Items` shape test restored (3).
- 2026-08-13: correcting a figure this milestone recorded twice: the 2026-08-08 criteria-audit line says a (citekey, block, sample) key "would itself have deleted 6 of the 14 shipped note-only rows". Measured 2026-08-13 over the committed notes, it deletes 5 — the 14 rows span 9 blocks. The line stands as written, history not being edited; this supersedes its figure.
- 2026-08-13: amendment return: AC3 — "the note row's `scale`, `value` and `anchor` cells, the key being taken from the note row rather than from the report, whose ten columns do not include `anchor`"; the collapse figure corrected from 8 to the measured 9, and a fixture test for the anchor case added to the criterion. Audited before landing by a fresh-context [O] reader that authored none of it, which rejected the first draft: it asked for the anchor as a cell "the report carries", a set the ten columns provably exclude, so two different landing states satisfied it. Both of that audit's minimal changes adopted; accepted at the mini gate.
- 2026-08-13: review round 1 returned to `in-progress` (defect return 1). AC5 fails: `normalise_items()`'s `grepl("^[0-9]+$", p)` admits a digit string past integer range, `as.integer()` overflows to NA and the paste yields the string `"NA"`, so `values_agree("Items", "99999999999", "88888888888", 1)` is TRUE — the coercion the criterion names as closed. Five more findings action at ≥80 (F1 anchor-blind dedupe key, F8/F9 two unfenced criterion clauses, F10 a vacuous NA assertion, R1 a false figure). PR #109 open, draft.
- 2026-08-13: T7 — audit re-run at `9d56bf2a`: ledger 194 rows, coverage 15 rows, 0 gaps, 14 note-only, 1 constructed credit, 0 angle-copy splits, 0 IP2 breaches. Ledger compared row by row against the committed file with the three stamp columns stripped: identical, so no audited comparison changed; the coverage CSV was regenerated at T1/T2 and is unchanged since. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 6974 (the 6 warnings all outside this milestone's files, unchanged from M81's run). `devtools::check(args = "--no-manual")`: Status OK, 0 errors / 0 warnings / 0 notes, 7m 12s. No NEWS entry: this milestone changes `data-raw/` and `tests/` only, neither of which is installed, so there is no user-visible change to record.
- 2026-08-13: check needed M81's recorded gfortran workaround again — a scratch `FLIBS=` via `R_MAKEVARS_USER`, uncommitted. Without it the source install fails to link (`ld: library 'emutls_w' not found`), R's default `FLIBS` naming `/opt/gfortran/lib` unconditionally though `src/` is C++ only. Machine setup, not this branch: the first check run failed before compiling any of it.
- 2026-08-13: T2 found seven emitters, not the plan's six — M79 added `shipped-sample-not-audited` after this plan was written — and the roster test file carries coverage assertions the plan attributed to the provenance file alone.

## Decisions

- 2026-08-13 (D-M80-1): the note-only dedupe key will take the note row's
  `sample` cell as well as its `scale`, `value` and `anchor`. Decided by the
  maintainer at the round-2 routing chip, on G3 (scored 52, below the action
  bar): two note-only rows differing only in their sample collapse to one and
  the lost row is recorded nowhere, which the maintainer judged a silent loss
  worth closing whatever its score. This authorizes a SECOND amendment to
  AC3's wording, which the tracking rules otherwise stop and route to the
  maintainer — the authorization is this entry. The amendment still runs the
  full protocol at implement time: proposed text, a fresh-context reader that
  authored none of it, and the mini gate.

## Review

### Round 2 (2026-08-13) — returned to `in-progress`

**Outcome.** AC5 fails again, by a mechanism round 1's fix introduced. Defect
return 2. The thrash rule's trigger (b) fires: one criterion failing twice, each
time by a new mechanism of the same shape — a guard admitting a cell it should
refuse, so two unequal item keys compare equal.

**G1 (93), the only finding at or above the action bar.** Round 1's fix replaced
`grepl("^[0-9]+$", p)` with `as.integer()`'s own verdict, which closed the
overflow shape and opened a wider one: `as.integer()` parses a decimal, a
scientific literal, a hex literal and a signed integer without NA. Measured —
`normalise_items("1.5, 9")` returns `"1, 9"`, `"0x10, 9"` returns `"16, 9"`,
`"1e2, 9"` returns `"100, 9"`, `"+1, -9"` returns `"1, -9"`, and
`values_agree("Items", "1, 9", "1.4, 9", 1)` is TRUE. A note transcribing an
item key as `1.4, 9` agrees with a shipped key of `1, 9`. The pre-fix shape test
rejected every one of these; the fix replaced it where it should have composed
with it. No test covers a decimal, scientific, hex or signed cell.

**Trigger (b)'s remedy.** The plan gate recorded no alternative against for this
guard's design, so escalation via `/milestone-brief` is offered rather than a
third attempt made silently. The direct fix — conjoin the shape test with the
coercion check, so shape refuses what is not a plain integer literal and
coercion refuses what does not fit — is what this session recommends; the
choice is the maintainer's.

**Everything else verified.** All seven criteria re-measured against the fixed
code and all pass but AC5: the schema is the ten declared columns, the run is
15 coverage rows over 14 note-only rows spanning 9 blocks with 0 gaps, and
`devtools::test()` FAIL 0 | PASS 7000 with
`devtools::check(args = "--no-manual")` Status OK 0/0/0. `cairn_validate`
exit 0; `document()` no diff, zero `resolve link` warnings;
`pkgdown::check_pkgdown()` clean. Four guards re-confirmed load-bearing by
mutation: the NA-divisor guard (3 failures), the AC6 sweep (3), the anchor in
the dedupe key (3), the value-level emitter's key columns (2), and
`note-sample-not-audited`'s instrument (1).

**The other two lenses found nothing.** History: no prior intent undone, and the
`Items` guard still accepts every cell the committed notes rely on. Prior
review: all six of round 1's actioned findings verified genuinely fixed, and
none of round 1's fourteen sub-threshold findings reintroduced.

**Logged, below the 80 action bar (15):** G13 (58) and G14 (52) and G9 (52) and
G2 (45) and G8 (30) re-report round 1's F6, F5, F7, F16 and F15 unchanged, at
or below their round-1 scores. G3 (52) — the dedupe key omits the note row's
`sample` cell, so two note-only rows differing only in their sample collapse to
one (measured on a fixture); the scorer holds this to be AC3's own amended
wording rather than an implementation flaw, and amending AC3 again would be
this milestone's second amendment on one criterion, which the tracking rules
stop and send to the maintainer. G16 (50) the header comment's "every emitter"
overclaims, two emitters having pasted nothing. G4 (45) the within-pass half of
the dedupe filter is unfenced. G5 (40) the anchor test's comment claims
provenance the report does not carry. G10 (35) nothing binds the header
comment's per-side table to the emitted side set. G12 (30) the two paste-absence
assertions are vacuous over the committed run, though a fixture test fences
those emitters elsewhere. G15 (30) `validate_batch()` still does not type-check
`scales`; pre-existing. G6 (25) `is.na(one)` is a dead clause. G7 (15) a
list-column `Items` raises R's coercion error rather than the named abort.
G11 (12) the ledger's `script_commit` names the parent commit — the two-stamp
design's documented, intentional behaviour.

### Round 1 (2026-08-13) — returned to `in-progress`

**Outcome.** AC5 fails: `normalise_items()` still coerces an unreadable cell to
the string `"NA"` and still lets two such cells compare equal, by integer
overflow rather than by the alphabetic shape the guard closes. Defect return 1
of this milestone. AC3's own wording also carries a false figure and routes to
the amendment gate (R1 below).

**Evidence gathered this round** (fresh, by command; ticks withheld because the
fixes below will change the code these were measured against):

- AC1 — the run's coverage frame has exactly the ten declared columns,
  `identical(names(cov), COVERAGE_COLUMNS)` TRUE, and the emitted side names and
  the header comment's per-side table are the same set of 8, compared by
  extraction rather than by eye. Zero `field` cells carrying a pasted sample and
  zero `instrument` cells carrying a paren over the 15-row run. The two
  value-level sides were provoked on a fixture: `field` is the bare `"M"`, the
  sample rides in `sample`, the scale in `scale`.
- AC2 — dropping iip32 from the batch yields a `note-block-not-audited` row with
  `instrument` NA, `citekey` `horowitz2003`, `tag` `iip32`; dropping igicr
  sample 3 yields a `note-sample-not-audited` row carrying `igicr` with the
  sample in `sample`. Every non-NA `instrument` in the full run is a shipped
  instrument name.
- AC3 — the run emits 14 note-only rows, distinct on (citekey, tag, label,
  detail). Deleting the dedupe reddens 3 assertions in the two-passes test. But
  see F1 and R1: the key omits the note row's `anchor`, and the criterion's
  stated collapse figure is wrong.
- AC4 — 21 assertions in `test-norms-audit-batch.R` pass; deleting the
  strictly-positive guard reddens its 3 own assertions plus 2 in M81's abort
  registry, so every new guard is fenced twice.
- AC5 — **FAILS.** See F3.
- AC6 — deleting the `!isTRUE(blk$scales)` sweep reddens 3 assertions in the
  block test; the covered case emits no row.
- AC7 — `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 6974;
  `devtools::check(args = "--no-manual")` Status OK, 0/0/0, 7m 12s (needed
  M81's uncommitted `FLIBS=` workaround; machine setup, not this branch). Ledger
  identical to the committed one bar its three stamp columns; coverage CSV
  round-trips against a fresh run.

**Consistency gate.** `cairn_validate` exit 0, every CHECK PASS (47 pre-existing
work-log-format advisories, all M7's). `document()` no diff and zero
`resolve link` warnings, after reverting a `Config/roxygen2/version` bump it
wanted (outside Scope, as at M81). `pkgdown::check_pkgdown()` no problems. No
new top-level file, so no `.Rbuildignore` change. No NEWS entry: `data-raw/` and
`tests/` are not installed. `cairn_impact` skipped, no principle changed.

**Fresh-context review.** Three lenses. History: six candidates, every one
resolved negative by the lens itself — no prior intent undone. Prior review:
zero findings; the GitHub inline-comment probe returned empty, so the archived
`## Review` sections were the whole evidence base. Diff-bug: 19 findings.
Scored by a fourth agent that generated none of them.

**Actioned (score ≥ 80), all to be fixed on the branch:**

- F1 (82) — the note-only dedupe key omits the note row's `anchor`, so two
  distinct note-only rows differing only in their anchor collapse to one.
  Reproduced on a fixture: 1 row emitted where master emitted 2. The silent row
  loss this file exists to refuse.
- F3 (87) — **AC5 failure.** `grepl("^[0-9]+$", p)` admits any digit string and
  `as.integer()` then overflows: `normalise_items("99999999999")` returns the
  string `"NA"`, and `values_agree("Items", "99999999999", "88888888888", 1)` is
  TRUE. The guard closes the alphabetic shape and not the overflow shape, while
  the criterion and the new comment both claim the defect closed.
- F8 (84) — no committed test asserts a `shipped-value-not-in-note` or
  `note-value-not-shipped` row's cells, the two emitters AC1 names. Both were
  hand-verified at this gate; neither is fenced, and restoring their pre-M80
  paste would pass the suite.
- F9 (82) — AC2's claiming-instrument clause is likewise unfenced: the only
  assertion on that side checks the side name alone and passes on master.
- F10 (80) — `nzchar(NA_character_)` is TRUE, so the payload assertion in the
  note-only test passes against exactly the all-NA regression it is written to
  catch.
- R1 (90) — AC3's "collapses those 14 to 8" does not reproduce: the 14 rows span
  9 citekeys, so a payload-free key yields 9 and loses 5. The same false figure
  was copied into `data-raw/audit-norms.R:794` and
  `tests/testthat/test-norms-audit-coverage.R:143`. The two comments are fixed
  as branch-added prose; the criterion text goes to the amendment gate.

**Logged, below the 80 action bar (14):** F2 (60) the dedupe's `duplicated()`
runs over the whole frame and `paste()` renders NA as `"NA"` — latent, no
current side fills both free-text columns. F4 (65) a trailing comma is
normalised away while a leading one aborts. F13 (60) the CSV test's comment
disclaims a row-order pin the test performs. F6 (58) `exempt` is the one column
`coverage_rows()` leaves uncoerced. F11 (58) the uniqueness assertion restates
the dedupe's own key. F5 (52) `coverage_rows()` recycles mismatched-length
vectors. F7 (52) a `NaN` divisor is refused as "missing" rather than
"not finite". F12 (52) the `na.rm` comment's stated reason is wrong; `grepl()`
never returns NA. F14 (50) `COVERAGE_COLUMNS` is bound only by the test file.
F16 (45) `values_agree("Items", character(0), character(0), 1)` is TRUE.
F19 (35) T2's dropped printed-summary clause was edited out rather than logged.
F15 (30) an unparseable `Items` cell aborts the whole run — the plan's own
fail-loud choice. F17 (25) the `shipped_roster()` ROADMAP row naming M80 as a
landing site was neither taken up nor declined; Scope puts it Out. F18 (15) a
zero-row batch passes `validate_batch()`; pre-existing.
