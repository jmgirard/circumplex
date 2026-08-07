# M74: Norms provenance audit, batch 3 (multi-sample instruments)

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M73
- **Driving RR:** —
- **Principles touched:** IP2, IP3, IP5
- **Branch/PR:** `m74-norms-provenance-audit-batch3`

## Goal

Verify the shipped norms of the four multi-norm-sample instruments (cais, iei,
igicr, iipsc — nine samples across five sources) against their published
sources, which requires teaching the audit to key by sample and to join one
instrument to more than one source.

## Scope

**In:** rekeying `shipped_values()` and the `audit_norms()` join by
`(Sample, scale)`, closing M72's finding that the shipped-side key ignores
`Sample`; a per-(instrument, sample) citekey map so iipsc's two samples join to
their two different sources; source notes for `sodano2006` (cais),
`horner2024` (iei), `trucco2013` (igicr), `hopwood2008` and `soldz1995`
(iipsc's two samples); a clean audit run over all thirteen audited instruments;
correcting any shipped value the audit finds wrong; pins and status rows for the
four.

**Out:** iip32/iip64 → their ROADMAP candidate row, promoted when the IIP
manual reaches the shelf. The four M73 repairs, which land there. The csig
PA/NO duplicated-statistics question → its existing candidate row.

## Acceptance criteria

- [ ] AC1. `shipped_values()` and `audit_norms()` key every per-sample field
      (`M`, `SD`, `Size`, `Population`, `Reference`, `URL`) by
      `(Sample, scale)`, so a sample-2 shipped value compares against sample 2's
      source row — fenced by a regression test whose fixture gives two samples
      different `M` values and whose swap of those two samples reddens.
- [ ] AC2. `AUDIT_BATCH` takes a citekey per (instrument, sample); iipsc's two
      samples join to `hopwood2008` and `soldz1995` respectively, shown by
      their ledger or coverage rows carrying different anchors; and every
      single-sample entry from batches 1 and 2 audits unchanged under the new
      form.
- [ ] AC3. Each of `sodano2006`, `horner2024`, `trucco2013`, `hopwood2008` and
      `soldz1995` has a committed `cairn/references/<citekey>.md` authored from
      `skills/shared/templates/source-note.md`, carrying an audit-values block
      that `parse_source_note()` reads without error, a Provenance block whose
      extraction status names the channel that read the norm table itself and
      carries its own `— observed YYYY-MM-DD`, and a line in `INDEX.md`.
- [ ] AC4. With `AUDIT_BATCH` extended to all thirteen audited instruments,
      `Rscript -e 'devtools::load_all(); source("data-raw/audit-norms.R")'`
      reports zero coverage gaps, zero angle-copy splits and zero IP2 breaches,
      and the ledger it writes carries no `UNDISPOSITIONED` row.
- [ ] AC5. Every ledger row for the four new instruments whose `kind` is
      `mismatch` — the AC4 run being the procedure that enumerates them — is
      resolved either by correcting `data-raw/<inst>.R` and rebuilding
      `data/<inst>.rda` from a value read in two independent channels (M42-D1),
      or by a row in `data-raw/norms-audit-dispositions.csv` naming why the
      shipped value stands.
- [ ] AC6. `norms-audit.md` records a verified verdict for each of the four
      naming the fields actually compared and, for each, the sample count
      audited; all four appear in `test-norms-provenance.R`'s `audited_objects`
      with their full `Norms` and `Scales` objects pinned, and the M73
      status↔pin binding test passes over them.
- [ ] AC7. `devtools::check(args = "--no-manual")` is clean (0 errors, 0
      warnings, 0 notes); the PDF-manual step is run separately if this
      milestone touches roxygen.

## Coverage

- AC1 → T2
- AC2 → T3, T6
- AC3 → T1, T4, T5, T6
- AC4 → T7
- AC5 → T7, T8
- AC6 → T9
- AC7 → T9

## Tasks

- [x] T1. Shelf intake: confirm the four shelf PDFs are present and record each
      one's sha256 and scan verdict in `norms-audit.md`'s manifest; decide the
      channel plan per source by asking what channel reads the norm table.
- [x] T2. Rekey `shipped_values()` to carry `Sample` and rekey the
      `audit_norms()` join on `(field, Sample, scale)`; add the differing-values
      regression fixture and prove the swap reddens.
- [x] T3. Rework `AUDIT_BATCH` into a per-(instrument, sample) citekey map;
      confirm batches 1 and 2 audit unchanged under it.
- [x] T4. Author `sodano2006.md` (cais) and `trucco2013.md` (igicr).
- [ ] T5. Author `horner2024.md` (iei) from the shelf PDF's Table 1, and shelve
      the author's IEI norms page as retrieval evidence for the `URL` field.
- [ ] T6. Author `hopwood2008.md` and `soldz1995.md` for iipsc's two samples.
- [ ] T7. Extend the batch, run the audit, disposition every ledger row, commit
      the ledger and coverage CSVs.
- [ ] T8. Resolve every `mismatch` row from T7 — correct `data-raw/<inst>.R`
      and rebuild `data/`, two-channel per M42-D1, or record a disposition.
- [ ] T9. Extend `audited_objects` and the `norms-audit.md` status rows for the
      four; run the full check.

## Work log

- 2026-08-06: created by /milestone-plan alongside M73.
- 2026-08-07: status -> in-progress; branch m74-norms-provenance-audit-batch3 cut from master.
- 2026-08-07: shelf triage found horner2025.pdf (iei, Table 1 prints both samples' M/SD), trucco2013.pdf (igicr, Table 3, three samples) and soldz1995.pdf (iipsc s2, Table 4 n=106) all present and matching shipped; sodano2006 (cais) and hopwood2011a (iipsc s1) absent from the shelf, Jeff supplying both.
- 2026-08-07: amendment gate — AC3's "horner2024 is OSF-hosted rather than a shelf PDF" sentence deleted as false (the article is on the shelf and publishes the values); T5 rewritten to match. Rest of AC3 unchanged.
- 2026-08-07: amendment — iipsc's sample-1 citekey `hopwood2011a` renamed `hopwood2008` in Scope, AC2, AC3 and T6: the shelved paper's own to-cite line reads "(2008) ... Journal of Personality Assessment, 90:6", so a 2011 citekey would name a year the source does not carry. Makes the shipped `Reference` "Hopwood, Pincus, DeMoor, & Koonce (2011)" an AC5 correction rather than a match.
- 2026-08-07: gate chose the author's IEI norms page over the article DOI and the OSF project for iei's shipped norms URL (M74-D1); rejected because Jeff wants users landing on the norms table itself, falsified if that page goes dead or stops matching the shipped values.
- 2026-08-07: T1 done — all five batch-3 sources on the shelf (sodano2006, hopwood2008, horner2025, trucco2013, soldz1995), sha256 + scan verdict recorded in norms-audit.md's manifest. Channel plan: soldz1995 is an Acrobat Paper Capture OCR scan, the first shelf source for which M42-D1 fires; the other four are born-digital with the norm values as typeset table text.
- 2026-08-07: T2 done — shipped_values() now enumerates ONE sample at a time and emits a `sample` column, the join keys on (field, sample, scale), and a batch entry naming a missing sample aborts instead of auditing nothing. Fenced by tests/testthat/test-norms-audit-sample-key.R; reverting to the pre-M74 enumeration and key reddens 8 assertions.
- 2026-08-07: T3 done — AUDIT_BATCH is a per-(instrument, sample) data.frame carrying citekey, divisor and a `scales` flag, validated by validate_batch(); the nine source notes and the dispositions CSV migrated to the five-column schema. Batches 1 and 2 re-audit to the same 130 ledger rows and 13 coverage rows, 0 value differences on every shared key.
- 2026-08-07: sodano2006 extracted in two channels (pdftotext + 300-dpi render of pp. 323 and 325): both CAIS samples' M/SD match shipped exactly, but Table 2's note reads N = 204 where the package ships Size = 213 (the article's Participants text gives 213 for the analysis sample). An AC5 item for T8, not yet resolved.
- 2026-08-07: T4 done — sodano2006.md and trucco2013.md authored and indexed. igicr matches trucco2013 on all 48 M/SD, all three sizes, all 8 target angles, the numbered item map and Reference/URL; only its three Population strings deviate. cais matches sodano2006 on all 32 M/SD, all 8 target angles and Reference, but three findings land: the shipped item map is a strict 8-cycle that reproduces Table 1's grouping only through item 29 (JK has three items there, not four), so 7 of 8 octants mismatch and items 33-37 are unassigned; Table 2's note gives N = 204 where the package ships 213; and the article prints no DOI, so the shipped URL is not-published-in-source. Scoped audit run: 0 coverage gaps, 15 ledger rows.
- 2026-08-06: plan gate chose deferring the multi-sample rework to this milestone over doing it in M73 because M73's four instruments are all single-sample and would leave the rework unexercised; falsified by the rekey turning out to be a precondition for something M73 needs.

## Decisions

### M74-D1 (2026-08-07): iei's norms URL points at the author's IEI norms page

**Context:** The shipped `iei` norms `URL` is `https://osf.io/w37dj/`, the
article's OSF project. Three candidate anchors exist: that OSF project, the
article itself (`horner2025.pdf` Table 1, which prints both samples' octant
M/SD and matches every shipped value), and Locke's own norms page,
`https://kennethlocke.org/IEI/IEI_Norms.html`, whose eight octant M/SD and
N of 1,223 match the shipped sample-1 values exactly (fetched 2026-08-06).
**Decision:** Jeff chose the author's norms page at the M74 implementation
gate. The page is shelved as retrieval evidence alongside the other Locke
norms pages, and the article stays the source note backing the values.
**Consequences:** Applies D-039's standing allowance rather than opening a new
gate. The page is not peer-reviewed and is a live URL, so its shelf copy
carries a retrieval date and the audit compares against that copy.

## Review
