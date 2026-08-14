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
      row's own payload — not once per batch pass that reads the block
      (`:379-385` sits inside the per-pass loop). A test asserts the full run
      still emits 14 note-only rows: every note-only row in the repo carries
      `sample = "—"`, so a key including `sample` but not the payload
      collapses those 14 to 8.
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
- 2026-08-13: review round 1 returned to `in-progress` (defect return 1). AC5 fails: `normalise_items()`'s `grepl("^[0-9]+$", p)` admits a digit string past integer range, `as.integer()` overflows to NA and the paste yields the string `"NA"`, so `values_agree("Items", "99999999999", "88888888888", 1)` is TRUE — the coercion the criterion names as closed. Five more findings action at ≥80 (F1 anchor-blind dedupe key, F8/F9 two unfenced criterion clauses, F10 a vacuous NA assertion, R1 a false figure). PR #109 open, draft.
- 2026-08-13: T7 — audit re-run at `9d56bf2a`: ledger 194 rows, coverage 15 rows, 0 gaps, 14 note-only, 1 constructed credit, 0 angle-copy splits, 0 IP2 breaches. Ledger compared row by row against the committed file with the three stamp columns stripped: identical, so no audited comparison changed; the coverage CSV was regenerated at T1/T2 and is unchanged since. `devtools::test()` FAIL 0 | WARN 6 | SKIP 3 | PASS 6974 (the 6 warnings all outside this milestone's files, unchanged from M81's run). `devtools::check(args = "--no-manual")`: Status OK, 0 errors / 0 warnings / 0 notes, 7m 12s. No NEWS entry: this milestone changes `data-raw/` and `tests/` only, neither of which is installed, so there is no user-visible change to record.
- 2026-08-13: check needed M81's recorded gfortran workaround again — a scratch `FLIBS=` via `R_MAKEVARS_USER`, uncommitted. Without it the source install fails to link (`ld: library 'emutls_w' not found`), R's default `FLIBS` naming `/opt/gfortran/lib` unconditionally though `src/` is C++ only. Machine setup, not this branch: the first check run failed before compiling any of it.
- 2026-08-13: T2 found seven emitters, not the plan's six — M79 added `shipped-sample-not-audited` after this plan was written — and the roster test file carries coverage assertions the plan attributed to the provenance file alone.

## Decisions

## Review

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
