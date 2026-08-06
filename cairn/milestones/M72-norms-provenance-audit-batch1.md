# M72: Norms provenance audit, batch 1 (CSI family + IITC)

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** —

## Goal

Re-verify every shipped norm value, angle, and item map for csie, csig, csip,
csiv, and iitc against their primary published sources, with committed
provenance, a mechanical comparison ledger, and regression pins — the first
batch of the IP5 debt (DESIGN.md, Known fragilities).

## Scope

**In:**
- Five instruments / five sources: csie (Locke & Sadler 2007), csig (Locke
  2014), csip (Boudreaux et al. 2018), csiv (Locke 2000), iitc (Bliton &
  Pincus 2019). Jeff supplies the PDFs at implementation start.
- Audited field set (canonical, used by AC2/AC3/AC5): per-scale M and SD,
  scale angles (both shipped copies), item-to-scale assignments, sample N
  (`Size`), and `Population`.
- Committed artifacts: synthesis note `cairn/references/norms-audit.md`
  (citekey map, sha256 shelf manifest, scan verdicts, per-instrument audit
  status for all 15 shipped instruments); source notes
  `cairn/references/<citekey>.md` ×5; script `data-raw/audit-norms.R`;
  dispositions input `data-raw/norms-audit-dispositions.csv`; pre-fix ledger
  snapshot `data-raw/norms-audit-ledger-prefix.csv` and post-fix
  `data-raw/norms-audit-ledger.csv`; test
  `tests/testthat/test-norms-provenance.R`.
- Confirmed transcription errors fixed on this branch with NEWS entries
  (2026-08-06 gate choice).

**Out:**
- The remaining 10 instruments → later batches; per-instrument status is
  visible in `norms-audit.md` and the ROADMAP candidate row stays for them.
- Mind Garden manual instruments (iip32, iip64) → a batch that handles an
  unobtainable source; deferred at the 2026-08-06 gate (criteria-audit F1).
- Anchors/labels text and any scoring-code changes; `score()` /
  `norm_standardize()` behavior changes beyond corrected data.

## Acceptance criteria

- [ ] AC1: For each batch instrument and each row of its `Norms[[2]]`, the
      cited source's PDF is on the `cairn/references/sources/` shelf, and the
      committed `cairn/references/norms-audit.md` records its citekey, shelf
      filename, sha256, and scan verdict — the verdict from a stated positive
      probe (`pdftotext` text-layer check on the anchor pages plus
      `pdfimages -list`, with `pdfinfo` Producer as one input; an inconclusive
      probe is treated as a scan), re-verified against the live shelf at
      review with the re-check recorded in this file's Review section.
- [ ] AC2: A committed source note `cairn/references/<citekey>.md` exists for
      each batch source, carrying a machine-readable table of every
      audited-field value with a shipped counterpart — plus any norm sample
      the source publishes that the package does not ship, recorded as a
      note-only row — each with a page/table anchor; a field the source does
      not publish is listed as `not-published-in-source`. Where AC1's scan
      verdict is scan/OCR, extraction used two independent channels per
      M42-D1 and the note's provenance block says so.
- [ ] AC3: `data-raw/audit-norms.R`, committed, parses each source note's
      machine-readable table, enumerates every shipped audited-field value
      for the five instruments from the package objects (`Norms[[1]]`,
      `Norms[[2]]`, `Scales$Angle`, `Scales$Items`; join key normalized
      across the `Scale`/`Abbrev` column shapes), compares angles modulo 360,
      checks `Scales$Angle` against `Norms[[1]]$Angle`, and emits (i) a
      coverage report listing every shipped value with no source-side entry
      and every source-side value with no shipped counterpart — empty except
      rows the notes list as `not-published-in-source` or note-only — and
      (ii) `data-raw/norms-audit-ledger-prefix.csv`, dated and recording the
      commit SHA it was generated against, each mismatch row carrying the
      shipped value, the source value, and the source anchor.
- [ ] AC4: Every ledger mismatch row carries a disposition joined from the
      committed `data-raw/norms-audit-dispositions.csv` (keyed by instrument
      + field + scale) — `transcription-error` (fixed on this branch),
      `intended-deviation` (documented at the surface where users meet the
      value), `not-published-in-source`, `deferred:<ROADMAP row>`, or an
      escalation to Jeff resolved into one of those before review; the
      post-fix re-run `data-raw/norms-audit-ledger.csv` is committed beside
      the retained pre-fix snapshot, and its surviving mismatch rows all
      carry a disposition other than `transcription-error`.
- [ ] AC5: `tests/testthat/test-norms-provenance.R` enumerates shipped
      instruments via `instruments()` (never a hand-list), pins each audited
      instrument's full `Norms` and `Scales` objects to the post-audit
      shipped values — every audited field traceable to its ledger row:
      source value where verified or corrected, shipped value where
      `not-published-in-source` or an approved `intended-deviation` — failing
      on any edit to any audited field including sample N and population, and
      fails when any shipped instrument is absent from `norms-audit.md`'s
      status table.
- [ ] AC6: NEWS.md documents every `transcription-error` fix shipped, each
      entry narrowed to what AC5's pins enforce; if the post-fix ledger shows
      no `transcription-error` rows, the clean outcome is recorded in the
      work log and NEWS.md is untouched.
- [ ] AC7: `devtools::test()` clean, and `devtools::check(args =
      "--no-manual")` with no new ERRORs/WARNINGs/NOTEs relative to the
      master baseline.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T6

## Tasks

- [ ] **T1** — Shelf + manifest: receive the five PDFs from Jeff, shelve
      them, author `cairn/references/norms-audit.md` (citekey map, sha256
      manifest, scan verdicts via the AC1 probe, 15-instrument status table)
      + its INDEX.md line.
- [ ] **T2** — Author the five source notes from
      `templates/source-note.md`, copying an existing page's provenance
      idiom (M47 lesson); machine-readable tables with per-value anchors;
      two-channel extraction where T1's verdict is scan/OCR.
- [ ] **T3** — Write `data-raw/audit-norms.R`; generate the coverage report
      and the pre-fix ledger; verify the coverage report is clean.
- [ ] **T4** — Author `norms-audit-dispositions.csv` with Jeff on
      escalations; fix `transcription-error` rows in `data-raw/` scripts and
      regenerate `data/`; commit the post-fix ledger re-run.
- [ ] **T5** — Write `test-norms-provenance.R` (parameterized pins + status
      completeness assert); prove the pins redden by perturbing one shipped
      value (LESSONS guard-teeth rule).
- [ ] **T6** — NEWS entries (or clean-outcome work-log line); full
      `devtools::check()` against the master baseline.

## Work log

- 2026-08-06: created by /milestone-plan (promoted from the 2026-08-04 interview candidate row; batch, field set, fix route, and priority fixed at the plan gate).
- 2026-08-06: criteria audit ran, two rounds ([O] fresh-context reader): round 1 returned 20 findings (F1 batch-unreachability became the gate's batch question; F2–F20 repaired in redraft); round 2 on the final wording returned 5 surviving defects, all repaired in the committed text (dispositions input CSV, dual ledger paths, closed disposition vocabulary, AC5 pinned to post-audit shipped values, AC7 baseline-relative).
- 2026-08-06: plan gate chose parsing source-note machine-readable tables over hand-typing source values into the script, because a hand-typed table checks one reader's transcription against itself; falsified by the note-block format proving unable to carry a per-value anchor.
- 2026-08-06: plan gate chose a committed dispositions CSV joined by the script over hand-editing the generated ledger, because regeneration must not destroy dispositions; falsified by the instrument+field+scale key failing to identify a mismatch row uniquely.
- 2026-08-06: plan chose full-object regression pins over comparing `data/` to regenerated `data-raw/` output, because the `data-raw/` scripts are themselves the transcriptions under audit; falsified by a source note itself proven mis-extracted against the page image.

## Decisions

## Review
