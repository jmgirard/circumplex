# M75: Norms provenance audit, batch 4 (IIP family)

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP4
- **Branch/PR:** `m75-norms-provenance-audit-batch4`

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

- [ ] AC1: `audit_norms()` compares all 48 shipped `iip64` M/SD values
      (3 samples × 8 octants × 2) against `horowitz2003`'s audit-values block
      with `divisor = 8`; the regenerated `norms-audit-ledger.csv` contains
      **no `iip64` row at all** and `norms-audit-coverage.csv` contains no
      non-exempt `iip64` row.
- [ ] AC2: `audit_norms()` compares all 48 shipped `iip32` M/SD values
      (3 samples × 8 octants × 2) against `horowitz2003`'s audit-values block
      with `divisor = 4`; the regenerated `norms-audit-ledger.csv` contains
      **no `iip32` M or SD row at all** and `norms-audit-coverage.csv` contains
      no non-exempt `iip32` row. `horowitz2003.md` records that the IIP-32
      descriptives are printed in Table F.5 (p. 91) — inside Appendix F, after
      the T-score conversion tables, rather than beside the IIP-64's Table 4.4 —
      and that Appendices B, D, F and G's conversion tables are a separate
      T-score norming path the package does not implement.
- [ ] AC3: the 96 shipped item→octant assignments (`iip32$Scales$Items` 1–32,
      `iip64$Scales$Items` 1–64) each match the manual's printed
      scale-membership lists at the anchors recorded in `horowitz2003.md`; and
      `iip32$Items` / `iip64$Items` each remain exactly one row with `Number`
      `NA` and the shipped placeholder `Text`. Both test-asserted.
- [ ] AC4: each of the six `Population` strings describes the manual's
      national stratified standardization sample, and every `Population` row
      in the ledger is either agreeing or dispositioned.
- [ ] AC5: the permission's condition-(a) credit line appears verbatim in the
      rendered help for both instruments, in `cairn/references/horowitz2003.md`,
      and in `data-raw/iip32.R` and `data-raw/iip64.R`. A test asserts it in
      both help pages, reading `man/` when present and falling back to
      `tools::Rd_db("circumplex")` otherwise (the M7/M70 dual-source pattern).
- [ ] AC6: the citation year agrees across `@source`, `Norms[[2]]$Reference`
      and `Details$Reference` for both instruments; `@source` names the
      edition and publisher the shipped values are attributed to; and
      `horowitz2003.md` records that all 96 shipped M/SD values reconcile
      exactly against the 2003 3rd edition, which is the edition now cited.
- [ ] AC7: `cairn/references/horowitz2003.md` exists carrying a Provenance
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

- [ ] T1: author `cairn/references/horowitz2003.md` from
      `templates/source-note.md` — Provenance block, condition-(a) credit
      line, and a five-column audit-values block carrying Table 4.4's 48 raw
      statistics (pp. 27–29), Size/Population/Reference/URL/Angle, and the
      printed scale-membership lists. Second channel: 150-dpi render of
      pp. 27–29 and 87–89 (M42-D1; `pdftotext` twice is one channel). Add the
      `INDEX.md` line.
- [ ] T2: add six `AUDIT_BATCH` rows to `data-raw/audit-norms.R:34-62`
      (iip32 samples 1–3 `divisor = 4`, iip64 samples 1–3 `divisor = 8`, one
      `scales = TRUE` per instrument); run and record ledger + coverage.
- [ ] T3: record in `horowitz2003.md` where the IIP-32 descriptives actually
      sit (Table F.5, p. 91) and what Appendix F's other tables are; correct in
      place the two `cairn/ROADMAP.md` claims the plan gate premised on their
      absence — the norms-audit row's 2000-edition open item and the T-score
      row's "for the IIP-32 the tables are the *only* published norming
      content".
- [ ] T4: add any Population/Angle/URL deviations to
      `data-raw/norms-audit-dispositions.csv`; re-run until no ledger row is
      `UNDISPOSITIONED`.
- [ ] T5: update the six `Population` strings, resolve the citation year and
      publisher across `R/instrument_data.R:113-141` and both `data-raw`
      files, add the divisor/provenance comments and the credit line, rebuild
      `data/`.
- [ ] T6: add the credit-line and `Items`-placeholder tests; add both
      instruments to `audited_objects`
      (`tests/testthat/test-norms-provenance.R:31-317`) and move
      `norms-audit.md`'s status cells off `unaudited` in the same change, so
      the bidirectional binding test at `:399-435` stays green.
- [ ] T7: NEWS.md entry; `devtools::document()`; `devtools::test()`;
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

## Decisions

## Review
