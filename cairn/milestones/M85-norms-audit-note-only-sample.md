# M85: Carry the sample through the audit's note-only coverage rows

- **Status:** review
- **Priority:** low
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m85-norms-audit-note-only-sample` / https://github.com/jmgirard/circumplex/pull/113

## Goal

A note-only coverage row carries the sample its dedupe key already
distinguishes it by, so two rows differing only in sample stop emitting
identically and surviving a `unique()` as one.

## Scope

**In:** the `note-only-sample` emitter at `data-raw/audit-norms.R:713-719`,
which passes no `sample` and so inherits `coverage_rows()`'s
`sample = NA_character_` default at `:592`; the resulting regeneration of the
committed `data-raw/norms-audit-coverage.csv`; the coverage tests that assert
over it.

**Out:** roster validation → M84; the test machinery → M83. No change to the
dedupe key at `:702-703`, which already includes `sample` correctly (D-M80-1).

## Acceptance criteria

- [x] AC1. The `note-only-sample` emitter passes the note's own `sample`, so
      two note-only rows differing only in that field emit as two distinct
      rows. Evidence: the M80 fixture's `sample` axis asserts
      `nrow(unique(only)) == nrow(only)`; measured today as 1 against 2.
- [x] AC2. `test-norms-audit-coverage.R`'s four-axis note-only test gains a
      `sample`-column assertion, so it can distinguish the fixed emitter from
      the broken one on that axis. Today the four axes assert only `label` and
      `detail` and cannot.
- [x] AC3. `data-raw/norms-audit-coverage.csv` is regenerated from a run-block
      execution and committed, and the "the committed coverage report is the
      one this code emits" test passes against it column by column. The 14
      committed `note-only-sample` rows carry the `NO_SAMPLE` token from
      `data-raw/audit-norms.R:327` rather than `NA`.
- [x] AC4. No shipped audit verdict changes: the non-exempt gap count and the
      set of instruments named in the report are identical before and after,
      compared against the pre-change CSV.
- [x] AC5. `devtools::test()` and `devtools::check(args = "--no-manual")` clean,
      with `document()` warning-free per the profile's consistency gate.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4

## Tasks

- [x] T1. Pass `sample = note_only$sample[fresh]` from the emitter
      (`data-raw/audit-norms.R`, the `note-only-sample` `coverage_rows()` call).
- [x] T2. Add the `sample`-axis assertions to the note-only test
      (`test-norms-audit-coverage.R`, the four-axis test) — write them first and
      watch them redden against the current emitter.
- [x] T3. Regenerate `data-raw/norms-audit-coverage.csv`; diff it against the
      committed version and confirm only the 14 note-only `sample` cells move.
- [x] T4. Full check.

## Work log

- 2026-08-14: created by /milestone-plan.
- 2026-08-14: plan gate chose a separate milestone over folding this into M84, because its lineage is M80's report schema rather than M79's roster and it changes a committed data artifact, which deserves its own review surface; falsified by the regeneration turning out to be inseparable from a roster change.
- 2026-08-14: branch `m85-norms-audit-note-only-sample` cut from a synced master; no question gate, the plan leaving nothing open (AC3 settles the token-vs-NA call).
- 2026-08-14: T1–T3 landed as one checkpoint rather than three. The emitter, its test and the regenerated CSV are one atomic change — the "committed coverage report is the one this code emits" test fails unless all three are present — so separate commits would each leave the suite red at a resume point.
- 2026-08-14: T2 first. The four axes gained a `sample`-cell assertion and a `distinct` expectation (rows surviving `unique()`); reddened against the unfixed emitter with the sample axis at 1 unique against 2, AC1's measured baseline, and `sample` NA on all four axes.
- 2026-08-14: the `distinct` expectation is 1 on the `anchor` axis and 2 on the other three, deliberately. The M80 dedupe key runs one cell wider than the report — the anchor is provenance the report does not carry — so anchor-only twins stay indistinguishable to a reader by design, and pinning that at 1 keeps it from reading as the defect M85 removes.
- 2026-08-14: T1 passed `sample = note_only$sample[fresh]` and updated the schema comment's per-side table, which listed `instrument citekey tag` for this side.
- 2026-08-14: T3 regenerated the coverage CSV. Column-wise against the pre-change file: 14 cells moved, all of them `sample` on `note-only-sample`, every one NA → the NO_SAMPLE token; no other column and no other side moved. AC4 measured on the same pair — non-exempt gap count 0 before and after, instrument set identical (csie, csig, csip, csiv, iipsc, iis32, iitc, ipipipc, isc).
- 2026-08-14: T4 — `devtools::test()` clean (FAIL 0, PASS 7226; 6 warnings and 3 skips, all in `test-ssm_sem.R` and glmmTMB/lavaan load, none in a file this diff touches). `document()` emitted zero `resolve link` warnings and left `man/` and `NAMESPACE` byte-unchanged.
- 2026-08-14: `document()` also rewrote DESCRIPTION's `Config/roxygen2/version` 8.0.0 → 8.1.0, this machine's roxygen2 being newer than the one the repo was documented with. Reverted rather than committed: it is a toolchain stamp unrelated to M85 and repo-wide in effect, so it is the maintainer's to take deliberately, not a stranger to sweep into a milestone branch. It will recur for anyone running `document()` on roxygen2 >= 8.1.0.
- 2026-08-14: review CI — one blocking `gh pr checks 113 --watch` timed out at 10m; the F1-fix push had started a fresh run, so the earlier green results belong to the previous head. Fresh state at timeout: pkgdown pass, `ubuntu-latest (release)` and test-coverage pending. Nothing left watching; CI is re-checked at the merge step.
- 2026-08-14: T4 closed — `devtools::check(args = "--no-manual")` Status: OK, 0 errors / 0 warnings / 0 notes (7m 47.5s, circumplex 2.0.0). Status → review.

## Decisions

- 2026-08-14 (M85-D1): the regenerated `data-raw/norms-audit-ledger.csv` is not
  committed. The run block writes both CSVs, so regenerating the coverage report
  re-runs the ledger too; but its twelve columns are byte-identical apart from
  three stamp cells (`generated` 2026-08-13 → 2026-08-14, `script_commit` and
  `data_commit` ab46cbef → 1a31c8aa). M85 changes nothing the ledger records, and
  committing the re-stamp would date a provenance verification this milestone did
  not perform. Reverted rather than committed; the next milestone that moves a
  ledger value re-stamps it honestly.

## Review

Reviewed 2026-08-14 on `m85-norms-audit-note-only-sample` at PR #113.
Pre-change comparisons are against `git show master:data-raw/norms-audit-coverage.csv`, not a working copy.

**AC1** — PASS. `test-norms-audit-coverage.R` clean (FAIL 0, PASS 77); the four-axis
note-only test's `sample` axis asserts `nrow(unique(only))` at 2, the plan's
measured-broken baseline being 1. Discrimination verified by mutation rather than
by a green run: reverting the emitter's `sample =` argument in place reddens 7
assertions, including the sample axis's `distinct` at `:251` and the committed-CSV
test at `:378`; restored and re-verified clean.

**AC2** — PASS. The four axes now assert `only$sample` alongside `label` and
`detail`, plus a per-axis `distinct` count. Under the same mutation the
`sample`-cell assertion fires on all four axes, so the test distinguishes the
fixed emitter from the broken one on that axis — which it demonstrably could not
before.

**AC3** — PASS. `data-raw/norms-audit-coverage.csv` regenerated by a run-block
execution and committed at `ac02b355`. The "committed coverage report is the one
this code emits" test passes column by column (inside the 77 above; it is live —
it reddens under the mutation). All 14 `note-only-sample` rows carry the
`NO_SAMPLE` token `—`; none is `NA`.

**AC4** — PASS. Measured against `master`'s committed CSV: non-exempt gap count 0
before and 0 after; instrument set identical. Column-wise, exactly 14 cells moved,
all of them `sample` on `note-only-sample`. No other column and no other side
moved, so no shipped audit verdict changes.

The AC1–AC4 figures above were measured at `ac02b355`, before the F1 fix below;
re-confirmed after it at `3180c254`, where the same file reads FAIL 0, PASS 81
(the four added by F1's regression test) and the committed CSV is byte-unchanged.

**AC5** — PASS, re-run against the F1-fixed tree (the pre-fix results are stale and
not cited). `devtools::test()` FAIL 0, PASS 7230 — 6 warnings and 3 skips, all in
`test-ssm_sem.R` and the glmmTMB/lavaan load path, none in a file this diff
touches. `devtools::check(args = "--no-manual")` Status: OK — 0 errors, 0
warnings, 0 notes (8m 10.2s, circumplex 2.0.0). `document()` warning-free; see the
gate below.

### Consistency gate

`cairn_validate` exit 0 — every check PASS. 47 advisories, all `work-log format`
on M7's pre-existing hard-wrapped log; none on M85. No principle changed, so
`cairn_impact` is skipped. Profile consistency gate (`r-package`): `document()`
zero `resolve link` warnings with `man/` and `NAMESPACE` byte-unchanged; README
untouched by the diff; no new top-level file; no NEWS entry owed — `data-raw/`
and `cairn/` are both `.Rbuildignore`d, so the milestone ships no user-visible
change.

`document()` also rewrites DESCRIPTION's `Config/roxygen2/version` 8.0.0 → 8.1.0
on this machine (newer roxygen2 than the repo was documented with). Reverted, not
committed — a repo-wide toolchain stamp unrelated to M85. It will recur for
anyone on roxygen2 >= 8.1.0.

### Independent review

Three fresh-context lenses, then a scorer that did not generate the findings.

- **[O] diff-bug (Opus)** — 11 reported, including its own mutation testing.
- **[S] blame-history (Sonnet)** — no contradiction of recorded intent. Established
  that M80's round-3 review had already found this exact defect and logged it as
  its F1 at 72, below that milestone's action bar: the `NA` was a deferred known
  bug, not a design choice. Confirmed M85 correctly left the neighbouring `anchor`
  exclusion alone, which *was* deliberate (D-M80-1).
- **[S] prior-PR-comments (Sonnet)** — no prior-review regression. The GitHub
  inline-comment surface is empty (probe returned `[]`), so only the archived
  `## Review` sections were evidence.

**Actioned (>= 80): 1 of 11.**

**F1 (82) — FIXED at `3180c254`.** A blank note `sample` cell reached the report's
`sample` key column as `""`. `parse_source_note()` validates the *value* cell for
emptiness and not the sample cell, so once the emitter began passing the cell
through, a blank could leak where before it took the `NA` default — a defect this
milestone's own change introduced. `""` in a key column reads as a label whose
text went missing rather than a row that carries no such fact, which is what the
schema declares `NA` to mean and exactly why `tag_or_na()` exists for the sibling
`tag`. Fixed by `blank_to_na()` beside `tag_or_na()`, applied at the emitter.
Regression test first: reddens with `actual: ""` against the unguarded emitter.
The committed CSV is byte-unchanged — no committed note carries a blank sample
cell — so AC3 and AC4 are unaffected.

**Logged below the 80 action bar (10), surfaced not dropped:**

- F2 (45) — a note-only row's `sample` is unvalidated against the roster, so it
  can name a sample the package does not ship. Real, but roster validation is
  M85's Scope Out (→ M84).
- F4 (48) — the schema header says key columns are `NA` on a row with no such
  fact, while 14 committed rows carry the `NO_SAMPLE` stand-in. Pre-existing for
  `note-instrument-row-not-audited`; M85 makes it the majority case.
- F7 (40) — the `anchor` axis's `distinct = 1L` is an equality, so a future
  milestone carrying `anchor` in the report reddens it as a regression rather
  than passing as progress.
- F3 (35) — `sample` means "shipped sample audited" on value sides and "note-side
  label for unshipped material" here; latent, every committed row carrying the
  token.
- F9 (30) — the once-per-block test asserts nothing about `sample` over the
  committed run; pinned only indirectly by the committed-CSV test.
- F11 (20) — M85-D1's uncommitted ledger re-stamp leaves the ledger's
  `script_commit` naming a revision that would now emit a different coverage CSV.
- F10 (15) — Scope and AC3 carry line anchors gone stale since M84
  (`:713-719`, `:592`, `:702-703`, `:327`). Plan-owned text, amendable only by
  gate; not worth an amendment round for anchors alone.
- F6 (15) — the `is.na` → token change is undocumented for downstream consumers;
  none exist in-repo.
- F5 (5), F8 (5) — reported as no-findings (per-side text accurate; no assertion
  weakened or removed).

No finding falsified an acceptance criterion, and none scored >= 90 on package
behavior, so the return floor is not met: F1 took the fix-now triage on the
branch with no status change.
