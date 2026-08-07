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
`horner2024` (iei), `trucco2013` (igicr), `hopwood2011a` and `soldz1995`
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
      samples join to `hopwood2011a` and `soldz1995` respectively, shown by
      their ledger or coverage rows carrying different anchors; and every
      single-sample entry from batches 1 and 2 audits unchanged under the new
      form.
- [ ] AC3. Each of `sodano2006`, `horner2024`, `trucco2013`, `hopwood2011a` and
      `soldz1995` has a committed `cairn/references/<citekey>.md` authored from
      `skills/shared/templates/source-note.md`, carrying an audit-values block
      that `parse_source_note()` reads without error, a Provenance block whose
      extraction status names the channel that read the norm table itself and
      carries its own `— observed YYYY-MM-DD`, and a line in `INDEX.md`.
      `horner2024` is OSF-hosted rather than a shelf PDF, so its provenance
      records the URL and the retrieval date.
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

- [ ] T1. Shelf intake: confirm the four shelf PDFs are present and record each
      one's sha256 and scan verdict in `norms-audit.md`'s manifest; decide the
      channel plan per source by asking what channel reads the norm table.
- [ ] T2. Rekey `shipped_values()` to carry `Sample` and rekey the
      `audit_norms()` join on `(field, Sample, scale)`; add the differing-values
      regression fixture and prove the swap reddens.
- [ ] T3. Rework `AUDIT_BATCH` into a per-(instrument, sample) citekey map;
      confirm batches 1 and 2 audit unchanged under it.
- [ ] T4. Author `sodano2006.md` (cais) and `trucco2013.md` (igicr).
- [ ] T5. Author `horner2024.md` (iei) from the OSF-hosted source, with the
      URL and retrieval record its provenance block needs.
- [ ] T6. Author `hopwood2011a.md` and `soldz1995.md` for iipsc's two samples.
- [ ] T7. Extend the batch, run the audit, disposition every ledger row, commit
      the ledger and coverage CSVs.
- [ ] T8. Resolve every `mismatch` row from T7 — correct `data-raw/<inst>.R`
      and rebuild `data/`, two-channel per M42-D1, or record a disposition.
- [ ] T9. Extend `audited_objects` and the `norms-audit.md` status rows for the
      four; run the full check.

## Work log

- 2026-08-06: created by /milestone-plan alongside M73.
- 2026-08-07: status -> in-progress; branch m74-norms-provenance-audit-batch3 cut from master.
- 2026-08-06: plan gate chose deferring the multi-sample rework to this milestone over doing it in M73 because M73's four instruments are all single-sample and would leave the rework unexercised; falsified by the rekey turning out to be a precondition for something M73 needs.

## Decisions

## Review
