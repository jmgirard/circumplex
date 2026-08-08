# M75: Norms provenance audit, batch 4 (IIP family)

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP4
- **Branch/PR:** `m75-norms-provenance-audit-batch4` · https://github.com/jmgirard/circumplex/pull/102

## Goal

Verify the shipped `iip32` and `iip64` norms against the IIP professional
manual and record their provenance, closing the norms-audit family.

## Scope

**In:** The two remaining unaudited instruments. Source: IIP-64/IIP-32
professional manual, 3rd ed., © 2003, Horowitz, Alden, Wiggins & Pincus,
Mind Garden — shelf path `cairn/references/sources/IIP Manual.pdf`, citekey
`horowitz2003`. Audits M, SD, Size, Population, Angle, Reference, URL and the
item→octant scoring key for both. Adds the credit line that the Mind Garden
permission (`cairn/references/sources/IIP Agreement.pdf`, condition (a))
requires wherever the licensed statistics are reproduced. Extends
`AUDIT_BATCH`, the dispositions CSV and `norms-audit.md` as batches 1–3 did.

**Out:** Item *text* in any form — the permission licenses "Normative Data
(Means and SDs)" only, so `$Items` keeps its placeholder row (AC3 pins it).
Shipping the manual's T-score conversion tables, or reworking
`norm_standardize()` around them → candidate row (outside the permission, and
a feature change rather than an audit). Whether these reference statistics are
well-named for users → the existing norms-fitness row.

## Acceptance criteria

- [x] AC1: `audit_norms()` compares all 48 shipped `iip64` M/SD values
      (3 samples × 8 octants × 2) against `horowitz2003`'s `iip64` audit-values
      block with `divisor = 8`; the regenerated `norms-audit-ledger.csv` carries
      **no `iip64` row for any field the manual publishes** — no M, SD, Size,
      Reference or Items row — and `norms-audit-coverage.csv` contains no
      non-exempt `iip64` row.
- [x] AC2: the same for all 48 shipped `iip32` M/SD values against the `iip32`
      block with `divisor = 4`: no `iip32` M, SD, Size, Reference or Items
      ledger row, and no non-exempt `iip32` coverage row. `horowitz2003.md`
      records that the IIP-32
      descriptives are printed in Table F.5 (p. 91) — inside Appendix F, after
      the T-score conversion tables, rather than beside the IIP-64's Table 4.4 —
      and that Appendices B, D, F and G's conversion tables are a separate
      T-score norming path the package does not implement.
- [x] AC3: the 96 shipped item→octant assignments (`iip32$Scales$Items` 1–32,
      `iip64$Scales$Items` 1–64) each match the manual's printed
      scale-membership lists at the anchors recorded in `horowitz2003.md`; and
      `iip32$Items` / `iip64$Items` each remain exactly one row with `Number`
      `NA` and the shipped placeholder `Text`. Both test-asserted.
- [x] AC4: each of the six `Population` strings describes the manual's
      national stratified standardization sample, and every `Population` row
      in the ledger is either agreeing or dispositioned.
- [x] AC5: the permission's condition-(a) credit line appears verbatim in the
      rendered help for both instruments, in `cairn/references/horowitz2003.md`,
      and in `data-raw/iip32.R` and `data-raw/iip64.R`. A test asserts it in
      both help pages, reading `man/` when present and falling back to
      `tools::Rd_db("circumplex")` otherwise (the M7/M70 dual-source pattern).
- [x] AC6: the citation year agrees across `@source`, `Norms[[2]]$Reference`
      and `Details$Reference` for both instruments; `@source` names the
      edition and publisher the shipped values are attributed to; and
      `horowitz2003.md` records that all 96 shipped M/SD values reconcile
      exactly against the 2003 3rd edition, which is the edition now cited.
- [x] AC7: `cairn/references/horowitz2003.md` exists carrying a Provenance
      block (shelf path, ingested date, pagination basis, extraction status
      naming two independent channels for pp. 27–29, 57–59, 91 and 101) and an
      `INDEX.md` line; `norms-audit.md`'s `iip32`/`iip64` status, citekey-map
      and shelf-manifest rows are updated; both instruments appear in
      `audited_objects`; `data-raw/iip32.R` and `data-raw/iip64.R` each state
      their divisor and why; `devtools::test()` and `devtools::check()` clean.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2, T3
- AC3 → T1, T2, T6
- AC4 → T1, T4, T5
- AC5 → T1, T5, T6
- AC6 → T5, T7
- AC7 → T1, T6, T7

## Tasks

- [x] T1: author `cairn/references/horowitz2003.md` from
      `templates/source-note.md` — Provenance block, condition-(a) credit
      line, and **two** instrument-tagged five-column audit-values blocks (one
      source, two instruments): Table 4.4's 48 raw statistics (pp. 27–29) and
      Table F.5's 48 (p. 91), Size/Population/Reference/URL/Angle, and the
      printed scale-membership lists (Appendix A pp. 57–59, Appendix H
      pp. 101–102). Second channel: 150-dpi renders of every cited page
      (M42-D1; `pdftotext` twice is one channel). Add the `INDEX.md` line.
- [x] T2a: extend `data-raw/audit-norms.R` so one source note can back two
      instruments — `<!-- audit-values-begin: <instrument> -->` tagged blocks
      selected by the batch row's instrument, with untagged single-block notes
      unchanged. Test both the selection and the abort when no block matches.
- [x] T2: add six `AUDIT_BATCH` rows to `data-raw/audit-norms.R:34-62`
      (iip32 samples 1–3 `divisor = 4`, iip64 samples 1–3 `divisor = 8`, one
      `scales = TRUE` per instrument); run and record ledger + coverage.
- [x] T3: record in `horowitz2003.md` where the IIP-32 descriptives actually
      sit (Table F.5, p. 91) and what Appendix F's other tables are; correct in
      place the two `cairn/ROADMAP.md` claims the plan gate premised on their
      absence — the norms-audit row's 2000-edition open item and the T-score
      row's "for the IIP-32 the tables are the *only* published norming
      content".
- [x] T4: add any Population/Angle/URL deviations to
      `data-raw/norms-audit-dispositions.csv`; re-run until no ledger row is
      `UNDISPOSITIONED`.
- [x] T5: update the six `Population` strings, resolve the citation year and
      publisher across `R/instrument_data.R:113-141` and both `data-raw`
      files, add the divisor/provenance comments and the credit line, rebuild
      `data/`.
- [x] T6: add the credit-line and `Items`-placeholder tests; add both
      instruments to `audited_objects`
      (`tests/testthat/test-norms-provenance.R:31-317`) and move
      `norms-audit.md`'s status cells off `unaudited` in the same change, so
      the bidirectional binding test at `:399-435` stays green.
- [x] T7: NEWS.md entry; `devtools::document()`; `devtools::test()`;
      `devtools::check()`.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: criteria audit ([O], fresh context) returned findings on 7 of 9 drafted criteria; all 7 fixed before writing. AC1 was satisfiable by an audit that never ran (no-`UNDISPOSITIONED` reads clean when the instrument is absent from `AUDIT_BATCH`) → now demands no ledger row at all; AC2's deviation bound was self-referential → pinned literally; AC4 named an `Rd_db()`-only test that errors under `devtools::test()` → dual-source pattern; AC6 asserted publisher agreement across two fields that carry no publisher → scoped to `@source`; AC7 left the committed source note reproducing licensed statistics with no credit line → AC5 extended to it; AC8's commit-granularity clause had no verifying procedure → dropped to tree state; AC9 named a manual table `iip32` has none of and the wrong divisor. Also surfaced: no criterion governed the instrument-level `Angle`/`Items` rows `validate_batch()` forces into every run → AC3.
- 2026-08-07: plan gate chose recording `iip32` as norms-unsourced with the Appendix F recovery attached as corroboration over calling it verified-by- derivation, because the strict reproduce-every-printed-T oracle succeeds on only 5 of 24 columns (the printed tables compress at the tails despite the text calling the transform linear), so the recovery bounds the values but cannot verify them; falsified by obtaining an edition that prints the IIP-32 descriptives, which would make this an ordinary transcription audit.
- 2026-08-07: plan gate chose keeping M/SD standardization over adopting the manual's T-score tables, because the permission licenses "Normative Data (Means and SDs)" only and the tables are demonstrably non-linear at the tails, so they are not equivalent to z-scoring; falsified by a fresh permission covering the conversion tables plus a use case needing them.
- 2026-08-07: `Scales$Items` ships for both instruments and is audited here rather than removed — maintainer confirmed 2026-08-07 that scoring the items is permitted. `cairn/ROADMAP.md`'s claim that the item-map field is "by-design absent for these two" is false and is corrected in this plan's commit.

- 2026-08-07: amendment gate — the plan's premise that the 3rd edition prints no IIP-32 descriptives is FALSE. Table F.5 (p. 91), inside Appendix F after the T-score tables, prints IIP-32 M/SD for Males/Females/Overall and all 48 shipped values reconcile exactly at `divisor = 4`; read in two channels (`pdftotext -layout` text layer, 150-dpi render of p. 91). AC2 amended to an ordinary comparison and its `no-oracle` tripwire tag dropped (a printed oracle exists); T3 repurposed from the Appendix F recovery script to recording the finding; T4's `iip32` `not-published-in-source` rows dropped; AC6's unresolved-edition clause and AC7's page anchors corrected; Scope's 2000-edition Out clause removed; Coverage AC2 → T1, T2, T3. Supersedes the 2026-08-07 plan-gate entry above.
- 2026-08-07: gate chose citing the 2003 3rd edition (Mind Garden) across `@source`, `Norms[[2]]$Reference` and `Details$Reference` for both instruments over the 2000 first edition (The Psychological Corporation) that `@source` carried, because the shipped values are verified against the 3rd edition and nothing here was checked against the first.
- 2026-08-07: corrected in place the two `cairn/ROADMAP.md` claims the plan gate premised on the descriptives' absence — the norms-audit row's 2000-edition open item (closed, never opened) and the T-score row's "the tables are the only published norming content".
- 2026-08-07: measured the published-vs-linear T gap for iip32 (all 24 Appendix F columns, shipped M/SD): excluding the 99-ceiling cells, median 0.3 T points (0.03 SD), 90th pct 1.3, max 4.0 (0.4 SD); below T=60 max 1.2. Confirms keeping M/SD standardization; recorded on the T-table candidate row.

- 2026-08-07: T1 done — `cairn/references/horowitz2003.md` + `INDEX.md` line; all 96 M/SD values, both item maps, the three sizes and the reference credit read in two channels (text layer, 150-dpi render) across pp. 27–29, 57–59, 91 and 101–102. `devtools::test()` clean (0 failures, 6060 passing).
- 2026-08-07: discovered sub-task T2a — one source note backing two instruments collides in the audit's `(field, sample, scale)` join key, since `parse_source_note()` assumes one note per instrument and requires exactly one audit-values block. Adding instrument-tagged blocks rather than splitting the manual across two citekey pages.

- 2026-08-07: T2a done — `parse_source_note()` takes an `instrument` and selects among instrument-tagged blocks; untagged single-block notes unchanged, and re-running the audit over batches 1-3 reproduces the committed ledger and coverage byte-for-byte. `devtools::test()` clean (0 failures, 6068 passing).

- 2026-08-07: T2 + T4 done — six `AUDIT_BATCH` rows added (iip32 divisor 4, iip64 divisor 8, one `scales = TRUE` each). Ledger grows 166 → 194 rows: all 96 M/SD, both item maps, the six Sizes and the six Reference credits reconcile and produce no row; the 28 rows added are exactly the fields the manual publishes for neither instrument (Angle × 8, Population × 3, URL × 3 each), now dispositioned `not-published-in-source`. Coverage gaps 0, angle-copy splits 0, IP2 breaches 0, `UNDISPOSITIONED` 0. `devtools::test()` clean (0 failures, 6068 passing).
- 2026-08-07: amendment gate — AC1 demanded the ledger carry no `iip64` row at all, which no instrument can satisfy, since every source leaves some audited field unpublished; measured here as 14 rows per instrument. AC1 and AC2 rewritten to demand no row for the fields the manual DOES publish (M, SD, Size, Reference, Items), the unpublished ones staying covered by AC4's disposition requirement.

- 2026-08-07: T3, T5 and T6 land together — the pins depend on the rebuilt `data/`, and `norms-audit.md`'s status cells must move in the same change or the bidirectional binding test fails. T3's content had already landed with T1 (source note) and the amendment commit (ROADMAP corrections). Population strings rewritten to "American adults, national standardization sample, {overall,females,males}"; `@source` repointed to the 2003 3rd edition, Mind Garden; divisor/provenance comments and the credit line added to both `data-raw` scripts and both help pages; `data/` rebuilt; both instruments pinned in `audited_objects`.
- 2026-08-07: the credit line's © tripped `test-rd-latex-safe.R`, which flags every non-ASCII character in `man/` after an M7 CRAN failure on Greek letters. Transliterating it would alter a permission condition, so the allowlist was extended instead — with evidence, not assumption: `tools::Rd2latex()` + `pdflatex` on both Rd files produces no "Unicode character … not set up" line and the glyph typesets (`pdftotext` cannot extract it, which is an extraction artifact). The guard comment records the check and requires the same evidence for any further addition.

- 2026-08-07: T7 done — NEWS entries for the two instruments, the citation change and the Population rewrite; `devtools::document()` produces no further diff; `devtools::test()` clean (0 failures, 6094 passing); `devtools::check(args = "--no-manual")` clean (0 errors, 0 warnings, 0 notes). Status → review.

## Decisions

## Review

Verified 2026-08-07 against branch `m75-norms-provenance-audit-batch4`, PR #102.
Every figure below was produced by running a command at review time, never read
off the work log.

- AC1 — regenerated ledger carries 0 `iip64` rows for any field the manual
  publishes (M, SD, Size, Reference, Items); its only `iip64` fields are Angle,
  Population and URL. 0 non-exempt `iip64` coverage rows. Independently of the
  audit script, all 48 shipped M/SD were recompared against the note's `iip64`
  block at `divisor = 8`: max |shipped − source/divisor| = 0.
- AC2 — same measurement for `iip32` against the `iip32` block at
  `divisor = 4`: 0 ledger rows for published fields, 0 non-exempt coverage rows,
  max deviation 0 over 48 values. `horowitz2003.md` carries the "On where the
  IIP-32 descriptives are" and "On the T-score tables" sections, recording
  Table F.5 (p. 91), its position after Appendix F's conversion tables, its
  omission from the manual's contents listing, and that Appendices B/D/F/G are
  a norming path the package does not implement.
- AC3 — all 16 item-map rows (8 per instrument) match the note's anchors
  exactly, checked by recomputing `normalise_items()` over the shipped
  `Scales$Items` and joining to the note. `iip32$Items` and `iip64$Items` are
  each 1 row with `Number` `NA`. Test-asserted by the M75 placeholder test and
  by the `audited_objects` pins.
- AC4 — the six `Population` strings read "American adults, national
  standardization sample, {overall,females,males}"; the manual describes a
  national standardization sample of 800 stratified against 1999 Census figures
  with separate norms by gender (p. 25). All 6 `Population` ledger rows carry a
  `not-published-in-source` disposition; 0 rows are `UNDISPOSITIONED`.
- AC5 — the credit line is present verbatim, on collapsed whitespace, in all
  six required places: `man/iip32.Rd`, `man/iip64.Rd`,
  `cairn/references/horowitz2003.md`, `data-raw/iip32.R`, `data-raw/iip64.R`
  and `R/instrument_data.R`. The dual-source test reads `man/` when present and
  falls back to `tools::Rd_db()`; mutation-checked — deleting the credit
  paragraph from either Rd flips the assertion to FALSE.
- AC6 — `Norms[[2]]$Reference` and `Details$Reference` both read "Horowitz,
  Alden, Wiggins, & Pincus (2003)" for both instruments, and both `@source`
  entries name the year, the edition ("3rd ed.") and the publisher ("Mind
  Garden"); no Rd still cites the 2000 edition as source. `horowitz2003.md`
  records the reconciliation against the third edition.
- AC7 — the note carries a Provenance block (shelf path, ingested date,
  pagination basis, and a one-physical-line extraction status naming two
  channels for pp. 27–29, 57–59, 91 and 101–102) and one `INDEX.md` line.
  `norms-audit.md` carries both status rows off `unaudited`, 6 citekey-map rows
  and 2 shelf-manifest rows. Both instruments are in `audited_objects`. Both
  `data-raw` scripts state their divisor and why. `devtools::test()` 6094
  passing / 0 failures; `devtools::check(args = "--no-manual")` 0 errors,
  0 warnings, 0 notes.

Consistency gate: `cairn_validate` exit 0, all 16 checks PASS; 47 advisory
`work-log format` warnings, every one of them pre-existing lines in M7's log
and none from this milestone. No `DESIGN.md` principle changed, so
`cairn_impact` does not apply. Toolchain gate (`r-package` profile):
`devtools::document()` leaves no diff, `pkgdown::check_pkgdown()` reports no
problems, generated files were regenerated rather than hand-edited, NEWS.md
carries the user-visible entries, no new top-level file needs an
`.Rbuildignore` entry, and the full check is clean.

Fixed at review: the note's `Extraction:` status was wrapped across five
physical lines. The staleness advisory reads the line its status starts on, so
a wrapped status can lose its trailing `— observed` stamp; rewritten as one
physical line, as the source-note template requires.

### Independent review

Three fresh-context lenses over `master..HEAD`: an [O] diff-bug reviewer, an
[S] blame-history reviewer and an [S] prior-PR-comments reviewer, then an [S]
scorer that did not generate the findings. 22 items reported; the prior-review
lens found no regression of any point M72–M74 raised on these files, and the
GitHub inline-comment probe returned empty, so that surface was skipped. Two
of the 22 are recorded clean verifications rather than defects (no numeric or
scoring change — `Norms[[1]]`, `Scales`, `Items` and `Details` are `identical()`
to master for both objects, only `Norms[[2]]$Population` differs; and the
Overall/Female/Male sample order is not transposed despite Table F.5's printed
column order).

Actioned (scored ≥ 80), both fixed on the branch:

- **An unclaimed source-note block was invisible (88).** `claimed`/`blocks`
  were populated only inside the batch loop, so a tagged block no batch row
  selected was never parsed and never counted. Measured: dropping the three
  `iip32` rows from `AUDIT_BATCH` made all 48 of that instrument's tabled
  values vanish with the ledger, the coverage report and every printed count
  reading clean — the same silent-loss shape the malformed-row and
  missing-sample aborts refuse, one level up. Fixed with a block-level sweep
  emitting a non-exempt `note-block-not-audited` row, and a test that names the
  orphaned block rather than counting gaps. Mutation-checked: removing the
  sweep reddens it.
- **NEWS overstated two things (80).** "every one of the fifteen bundled
  instruments has now been checked against its published source" contradicted
  `norms-audit.md`'s own `audited, norms unsourced` verdict for `iis32` and
  `ipipipc`, and "all six sample sizes … confirmed correct" glossed the three
  derived `iip32` sizes. Both rewritten.

Also fixed, below the action bar but defects in content this branch itself
added:

- **The `Items` placeholder test was false coverage (78).**
  `expect_false(grepl("^I ", Text))` matches only the subset of IIP items
  opening that way, and pasted text would already have failed the `nrow == 1`
  assertion above it. Replaced with the exact placeholder string, which is what
  AC3 names.
- **The six `Population` dispositions broke the batch-1–3 taxonomy (78).** They
  read `not-published-in-source`, so the shipped label was compared against
  nothing, where every prior instrument in the identical situation records the
  source's prose sample description with a page anchor and dispositions the
  deviation `intended-deviation`. The note now carries the manual's p. 25
  description; the six rows are `mismatch`/`intended-deviation` like their
  predecessors.
- **The `iip32` `Size` rows were derived, not transcribed, and nothing said so
  (78).** Table F.5 prints no group sizes; the shipped 800/400/400 carry over
  the IIP-64 standardization sample on the manual's p. 24 grounds. The three
  anchors now say `DERIVED, not printed for the IIP-32`, and the
  `norms-audit.md` verdict and the NEWS entry say the same.

Logged, not actioned (14 findings scoring 25–72): six against
`data-raw/audit-norms.R`, five of them in the tagged-block machinery — the
`instrument = NULL` fallback (55), the untagged-note `claimed` collision (55),
fence-unaware marker matching (50), permissive tag extraction (55), untested
abort paths (72) — plus the coverage report's `instrument` column now mixing
namespaces (50); all six absorbed into the existing `data-raw/audit-norms.R`
robustness candidate row rather than a new one. The rest: the credit-line
test's `Rd_db()` fallback may render `©` as an escape in a C locale (55) and
the sibling hazard in `test-rd-latex-safe.R`'s fallback (25, pre-existing); the
allowlist regex would create a character range if a future addition were a
hyphen (35); `attr(, "tag")` read-order fragility (28); an empty block parsing
to a 0-row frame (25, pre-existing); a stale M74 comment on an unmodified line
(25); a cosmetic `expect_true` before a `NULL` index (25); and the help pages'
`\source` citing 2003 beside a credit line reading "Copyright © 2000" (60) —
real but the credit line is verbatim-required text and the note explains it.

No finding met the return floor: neither actioned finding demonstrates an
acceptance criterion failing, and neither scored ≥ 90.

Re-verified after the fixes: `devtools::test()` 6096 passing / 0 failures;
`devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes;
`devtools::document()` no diff; `cairn_validate` exit 0; the regenerated ledger
still carries 0 rows for any published field of either instrument, 0 coverage
gaps and 0 `UNDISPOSITIONED`. CI on PR #102 was green before the fixes and is
re-running on the fix commit; the merge waits on it.

At the merge gate the maintainer chose to fix the one logged-not-actioned
finding that reaches a user-facing page (60): both help pages now say the
credit line's 2000 date is the publisher's required wording rather than the
edition the values come from. The licensed text itself is untouched.
