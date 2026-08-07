# M74: Norms provenance audit, batch 3 (multi-sample instruments)

- **Status:** review
- **Priority:** high
- **Depends on:** M73
- **Driving RR:** —
- **Principles touched:** IP2, IP3, IP5
- **Branch/PR:** `m74-norms-provenance-audit-batch3` / https://github.com/jmgirard/circumplex/pull/101

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
correcting any shipped value the audit finds wrong; a sweep of all fifteen
bundled instruments for the same key-vs-source failure cais showed, with the
structural half locked by a test; pins and status rows for the four.

**Out:** iip32/iip64 → their ROADMAP candidate row, promoted when the IIP
manual reaches the shelf. The four M73 repairs, which land there. The csig
PA/NO duplicated-statistics question → its existing candidate row.

## Acceptance criteria

- [x] AC1. `shipped_values()` and `audit_norms()` key every per-sample field
      (`M`, `SD`, `Size`, `Population`, `Reference`, `URL`) by
      `(Sample, scale)`, so a sample-2 shipped value compares against sample 2's
      source row — fenced by a regression test whose fixture gives two samples
      different `M` values and whose swap of those two samples reddens.
- [x] AC2. `AUDIT_BATCH` takes a citekey per (instrument, sample); iipsc's two
      samples join to `hopwood2008` and `soldz1995` respectively, shown by
      their ledger or coverage rows carrying different anchors; and every
      single-sample entry from batches 1 and 2 audits unchanged under the new
      form.
- [x] AC3. Each of `sodano2006`, `horner2024`, `trucco2013`, `hopwood2008` and
      `soldz1995` has a committed `cairn/references/<citekey>.md` authored from
      `skills/shared/templates/source-note.md`, carrying an audit-values block
      that `parse_source_note()` reads without error, a Provenance block whose
      extraction status names the channel that read the norm table itself and
      carries its own `— observed YYYY-MM-DD`, and a line in `INDEX.md`.
- [x] AC4. With `AUDIT_BATCH` extended to all thirteen audited instruments,
      `Rscript -e 'devtools::load_all(); source("data-raw/audit-norms.R")'`
      reports zero coverage gaps, zero angle-copy splits and zero IP2 breaches,
      and the ledger it writes carries no `UNDISPOSITIONED` row.
- [x] AC5. Every ledger row for the four new instruments whose `kind` is
      `mismatch` — the AC4 run being the procedure that enumerates them — is
      resolved either by correcting `data-raw/<inst>.R` and rebuilding
      `data/<inst>.rda` from a value read in two independent channels (M42-D1),
      or by a row in `data-raw/norms-audit-dispositions.csv` naming why the
      shipped value stands.
- [x] AC6. `norms-audit.md` records a verified verdict for each of the four
      naming the fields actually compared and, for each, the sample count
      audited; all four appear in `test-norms-provenance.R`'s `audited_objects`
      with their full `Norms` and `Scales` objects pinned, and the M73
      status↔pin binding test passes over them.
- [x] AC7. `devtools::check(args = "--no-manual")` is clean (0 errors, 0
      warnings, 0 notes); the PDF-manual step is run separately if this
      milestone touches roxygen.
- [x] AC8. Every bundled instrument's `Scales$Items` key is a complete partition
      of `1:Details$Items` — no unkeyed item, no item keyed twice — enforced by
      a test sweeping all fifteen that reddens when a key drops or repeats an
      item; the nine instruments whose source publishes an item map stay
      additionally compared against it by the AC4 run.

## Coverage

- AC1 → T2
- AC2 → T3, T6
- AC3 → T1, T4, T5, T6
- AC4 → T7
- AC5 → T7, T8
- AC6 → T9
- AC7 → T9
- AC8 → T10

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
- [x] T5. Author `horner2024.md` (iei) from the shelf PDF's Table 1, and shelve
      the author's IEI norms page as retrieval evidence for the `URL` field.
- [x] T6. Author `hopwood2008.md` and `soldz1995.md` for iipsc's two samples.
- [x] T7. Extend the batch, run the audit, disposition every ledger row, commit
      the ledger and coverage CSVs.
- [x] T8. Resolve every `mismatch` row from T7 — correct `data-raw/<inst>.R`
      and rebuild `data/`, two-channel per M42-D1, or record a disposition.
- [x] T9. Extend `audited_objects` and the `norms-audit.md` status rows for the
      four; run the full check.
- [x] T10. Sweep every bundled instrument's item key for the cais failure mode;
      lock the structural half with a test over all fifteen.

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
- 2026-08-07: T5 done — horner2024.md authored (citekey 2024 against the shelf filename horner2025.pdf; the article prints 2024 throughout) and the author's IEI norms page retrieved 2026-08-07 and shelved as locke_iei_norms_2026-08-07.html with a manifest row. iei matches on all 32 M/SD, both Sizes and both References; Angle and Items are not published (the 64 items are supplemental); the two URL rows mismatch, which is M74-D1's correction, and sample 2 needs its own anchor because the norms page publishes nothing of Study 2.
- 2026-08-07: T6 done — hopwood2008.md and soldz1995.md authored and indexed; iipsc's two samples now join to two different sources, with the instrument-level Angle and Items rows carried by soldz1995 (the only one of the pair that prints the item grouping). All 32 M/SD match, sample 1 through the documented divisor of 4 (hopwood2008 Table 1 prints octant sums, corroborated by its Total M = 35.15); both Sizes and the sample-2 Reference match. Mismatches: two Population strings and the sample-1 Reference year (shipped 2011, source 2008). soldz1995 is the first M42-D1 firing: neither channel is its Acrobat OCR layer — a direct read of a 200-300 dpi render and an independent tesseract pass over it, agreeing on every value.
- 2026-08-07: T7 done — AUDIT_BATCH extended to 18 (instrument, sample) rows over all thirteen audited instruments. Full run: 166 ledger rows, 15 coverage rows, 0 coverage gaps, 0 angle-copy splits, 0 IP2 breaches, 0 UNDISPOSITIONED. 36 batch-3 dispositions appended (9 Population intended-deviation, 3 URL and 24 Angle/Items not-published-in-source); ledger and coverage CSVs committed.
- 2026-08-07: gate — Jeff chose correcting the cais item key AND sweeping every bundled instrument for the same failure (a scope amendment, AC8 + T10 added); 204 for the cais child-sample Size, on his stated condition that everything else matched, which it does; and one URL per iei sample. He first asked how the cais defect arose: traced to the first draft (1c75400e, 2018-10-23), never since edited, and diagnosed decisively — dealing sodano2006 Table 1's unequal octant blocks round-robin, skipping exhausted octants, reproduces the shipped 37-item ORDER exactly, so the ordering encoded the published grouping and only the Items key kept the eight-cycle template the package's other 32-item instruments use. Not a typo, not a second sample.
- 2026-08-07: T8 done — four corrections, each from values read in two channels: cais Items rekeyed to sodano2006 Table 1's 5/5/5/4/5/3/5/5 blocks (changes JK, LM and NO scores and brings items 33-37 into scoring), cais sample-1 Size 213 -> 204, iipsc sample-1 Reference year 2011 -> 2008, iei URL split per sample (norms page / article DOI). data/cais.rda, data/iei.rda and data/iipsc.rda rebuilt; ?iipsc now cites both normative sources; NEWS records the cais score change as a breaking behavior change.
- 2026-08-07: T10 done — sweep of all fifteen bundled instruments found cais the only key that is not a complete partition of its items; every other one keys its full item count with no gaps or repeats. Locked by two tests in test-norms-provenance.R: a partition sweep over all fifteen and an explicit pin of cais's key against Table 1. Both verified to redden against HEAD's pre-fix data/cais.rda (partition FALSE because items 33-37 are unkeyed) and to pass after.
- 2026-08-07: correction to horner2024.md's provenance block — its claim that "the article prints 2024 throughout" was written before reading R/instrument_data.R, which cites a 2025 version of record (JPA 107(2), 170-187). The shelf copy is the online-first version, paginated 1-18; the block now says so and states that every page anchor is that copy's pagination.
- 2026-08-07: T9 done — cais, iei, igicr and iipsc added to audited_objects with their full Norms and Scales objects pinned (9 normative samples), norms-audit.md's status rows and citekey map extended and a "What batch 3 found" section written; the M73 status<->pin binding test passes over all thirteen. devtools::check(args = "--no-manual") clean: 0 errors, 0 warnings, 0 notes, testthat 0 failures / 6060 passing. cairn_validate: all checks passed.
- 2026-08-07: status -> review. Norms-fitness question raised by Jeff at the gate captured as a ROADMAP candidate row (reference choice moves z-scores 0.44 SD against 0.12 SD from sampling error at the smallest shipped sample), deliberately out of M74.
- 2026-08-07: review return 1 (defect) — F2 at 92: NEWS.md, norms-audit.md and the T8 work-log line all state that the cais item-key correction changes JK, LM and NO scores and leaves the other five octants unaffected; diffing the two keys octant by octant at review shows 7 of 8 change (PA, BC, DE and HI each gain an item too) and only FG is unchanged. F5 at 85 rides with it. All 8 acceptance criteria hold with fresh evidence and the consistency gate is clean; the failure is user-facing documentation, not the audit. Status -> in-progress.
- 2026-08-07: F2/F5 fixed — NEWS.md's cais bullet now states that seven of the eight octants change (PA gains item 32 from NO; BC, DE and HI gain 33, 34 and 35; JK loses 30; LM and NO each lose one and gain two) with only FG unchanged, and its re-verification bullet no longer claims every sample size matched while the next bullet corrects one. norms-audit.md's "three octants" corrected in place and marked. Supersedes the "changes JK, LM and NO scores" clause in the 2026-08-07 T8 line above. Per-octant gains and losses derived by diffing master's data/cais.rda against the branch's, not from the earlier claim.
- 2026-08-07: the two candidate-row items the review disposition assigned to this fix are now ROADMAP rows, search-first clean (no existing candidate, archive summary or D-entry covers either): cais's sample-2 means exceeding its 1-5 anchors, and the audit-norms.R robustness family (F3, F7, F8, F9, F10, F12).
- 2026-08-07: devtools::check(args = "--no-manual") re-run against the committed fix at 54c1c129 — 0 errors, 0 warnings, 0 notes, 13m06s. Status -> review, second time. (An earlier run of the same command was clean but had snapshotted the tree before the last two prose edits, so it is not the AC7 evidence.)
- 2026-08-07: review pass 2 — all 8 criteria re-executed with fresh evidence, consistency gate clean, three lenses + scorer over 22 findings. No return: F5 (82) is the only actioned finding and falsifies no criterion. Five record repairs made review-side (F5's miscount in norms-audit.md; F6/F7/F8's pre-fix present-tense claims in sodano2006.md, hopwood2008.md and two INDEX.md lines; F3's NEWS undercount of the corrections).
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

Verified 2026-08-07 on `m74-norms-provenance-audit-batch3` at 810956c3, PR #101.
Every line below is a command run at review, never recalled from implementation.

### Acceptance-criteria evidence

- **AC1.** `shipped_values()` takes a `sample` argument and emits a `sample`
  column; `audit_norms()` keys `ship_key`/`note_key` on `(field, sample, scale)`
  and restricts each pass's note rows to that sample plus the instrument-level
  `—`. `test-norms-audit-sample-key.R`: 11 pass, including the two-sample
  fixture with differing `M` values and the swap test asserting 4 mismatch rows
  with their shipped values named. Fence verified by mutation, not by eye:
  reproducing the M72 defect (per-pass note filter removed AND `sample` dropped
  from the join key) reddens 6 assertions including the swap's; restored, 11
  pass. Recorded as an observation below: mutating the join key ALONE leaves all
  11 green, because the per-pass note filter already prevents the collision —
  the two mechanisms are redundant, and only their conjunction is load-bearing.
- **AC2.** `AUDIT_BATCH` is an 18-row data.frame over 13 instruments carrying
  `citekey`, `divisor` and `scales` per (instrument, sample); `validate_batch()`
  returns TRUE. iipsc's two rows join to `hopwood2008` (sample 1, `divisor = 4`,
  `scales = FALSE`) and `soldz1995` (sample 2, `divisor = 1`, `scales = TRUE`),
  and their ledger rows carry anchors from two different papers — sample 1
  "p. 615, Sample 1, 'The IIP-SC was administered to 475 (65% women)
  undergraduates…'", sample 2 "p. 55, 'seen in nine outpatient clinics of
  HCHP (Generic Outpatient sample)'". Batches 1 and 2 audit unchanged: 130
  ledger rows on master and 130 on the branch, the same key set, and 0 differing
  values across `shipped`, `source`, `kind` and `disposition`; 13 coverage rows
  both sides.
- **AC3.** All five notes parse without error — `sodano2006` 56 rows,
  `horner2024` 56, `trucco2013` 76, `hopwood2008` 20, `soldz1995` 38. Each
  carries a `**Provenance.**` block; each extraction status, read through
  `cairn_validate`'s own `_extraction_status()` rather than by grep, names the
  channel that read the norm table and carries `— observed 2026-08-07`. All five
  have an `INDEX.md` line (`references index<->disk` PASSes).
- **AC4.** `Rscript -e 'devtools::load_all(); source("data-raw/audit-norms.R")'`
  re-run at review: 166 ledger rows, 15 coverage rows, **0 coverage gaps, 0
  angle-copy splits, 0 IP2 breaches**, 0 `UNDISPOSITIONED` of 166, 13
  instruments. The re-run reproduces the committed ledger identically on every
  content column; only `script_commit`/`data_commit` differ, which is the
  run-time stamp behaving as its own header documents.
- **AC5.** 36 batch-3 ledger rows, 9 of kind `mismatch`, 0 undispositioned —
  all 9 are `intended-deviation` `Population` rows (cais 2, iei 2, igicr 3,
  iipsc 2). The four corrections are visible master→branch in `data/`: cais
  `Items` JK `6, 14, 22, 30`→`6, 14, 22` and LM `7, 15, 23, 31`→
  `7, 15, 23, 30, 36`; cais `Size` 213→204; iei `URL` one OSF link→the norms
  page and the article DOI; iipsc `Reference` 2011→2008. Each rests on values
  read in two independent channels per its source note (M42-D1).
- **AC6.** `audited_objects` holds 13 instruments over 18 pinned normative
  samples; cais (2), iei (2), igicr (3) and iipsc (2) each pin full `Norms` and
  `Scales`, and each pinned object is `identical()` to the shipped one.
  `norms-audit.md` records a verdict per instrument naming the fields compared
  and the sample count. The M73 status↔pin binding test passes: 163 assertions
  green in `test-norms-provenance.R`.
- **AC7.** `devtools::check(args = "--no-manual")` at 1f368082: **0 errors, 0
  warnings, 0 notes**, 21m22s, testthat 0 failures / 6060 passing. The only
  commits after it touch `cairn/` alone (`ROADMAP.md`, the milestone file,
  `horner2024.md`), so no package-facing file changed under the clean check.
- **AC8.** The sweep runs over all fifteen shipped instruments, not the thirteen
  audited. Verified by mutation in both directions the criterion names:
  dropping item 37 from cais's NO key reddens 3 assertions; repeating item 30 in
  its LM key — leaving the count at 37 — reddens the same 3. Restored, 163 pass.

### Independent review (three lenses + scorer)

Three fresh-context reviewers with distinct evidence bases, then a Sonnet
scorer that generated none of the findings.

- **[S] blame-history:** no regression. Traced each changed line to its origin —
  cais's key and `Size` came from the 2018 draft carrying an open `# TODO`
  (never a deliberate choice), PR #99's `each`/`times` hotfix is intact, and the
  `audit-norms.R` rewrite extends every guard M72/M73 added rather than dropping
  any.
- **[S] prior-PR-comments:** no regression. M74 closes the one M72 finding
  assigned to it (the shipped-side key ignoring `Sample`, logged at 55 and
  deferred) and regresses none of M73's. The GitHub inline-comment surface is
  empty (`pulls/comments?per_page=1` returned `[]`), so it was not walked
  further.
- **[O] diff-bug:** 12 findings, scored below.

Scores: F2 92 · F5 85 · F3 78 · F1 72 · F10 68 · F12 68 · F6 65 · F9 50 ·
F7 45 · F8 40 · F4 35 · F11 30.

**Actioned (>= 80), verbatim:**

- **F2 (92)** — `NEWS.md`, `norms-audit.md`'s "What batch 3 found", and the M74
  work log all say the cais key change alters "JK, LM and NO" scores and that
  "the other five are unaffected". Actually **7 of 8 octants change**: the new
  key also adds item 32 to PA, 33 to BC, 34 to DE and 35 to HI. Only FG
  (`4, 12, 20, 28`) is unchanged. Failure: a user reading NEWS concludes their
  PA/BC/DE/HI CAIS scores are stable across the upgrade and does not re-run
  analyses; `score()` in fact returns a different PA (mean over 5 items instead
  of 4) for every case. Verified at review by diffing the two keys octant by
  octant.
- **F5 (85)** — `NEWS.md` internal contradiction: "Every mean and standard
  deviation of all nine normative samples was confirmed correct, as was every
  sample size, scale angle and item-to-scale assignment the sources publish
  apart from the `cais` key above" — the exception clause covers only the item
  key, but the next bullet reports the cais child-sample `Size` changing
  213 -> 204. Two mutually exclusive statements about the same field.

**Logged below threshold (surfaced, not actioned):** ten findings. F3 (78) the
coverage report has no shipped-side equivalent of `note-sample-not-audited`, so
a norm sample no batch row names is dropped with every count clean; F1 (72) the
new `data-raw/cais.R` comment claims the octant means "sit inside" the 1-5
anchors, false for sample 2 (5.19, 6.52 and 6.14 exceed 5) — the values and
anchors both predate this diff, the comment does not; F10 (68) coverage rows
mangle `field` into `"M (sample 1)"` so the CSV cannot be joined back to the
ledger; F12 (68) `validate_batch()` does not validate `divisor`; F6 (65) D-039
withholds its standing authority from numeric norm changes and the `Size` change
carries no D-entry; F9 (50) `values_agree()` normalises `Items` on the shipped
side only; F7 (45) instrument-level rows in a `scales = FALSE` note are dropped
uncovered (not live); F8 (40) `note-only` coverage rows duplicate per pass for a
multi-sample citekey (not live); F4 (35) the sample-key regression file does not
fence the join key alone — already disclosed under AC1 above; F11 (30) `?iei`
prints 2025 and `norms(iei)` prints 2024 for the same article, pre-existing and
untouched here.

### Disposition

F2 scores 92 on a defect in what the package's documentation tells users about a
breaking change to their own scores, which is over the return floor. Status
returns to `in-progress` for F2 and F5; review stops here and re-runs after the
fix. The ten sub-threshold findings are logged rather than actioned. Two are
candidate-row material to sweep search-first at the fix: F1's pre-existing half
(cais sample-2 means exceeding the shipped anchor range, which this audit is
structurally blind to because both sides carry the same printed number), and the
`audit-norms.R` robustness family (F3, F7, F8, F9, F10, F12).

### Consistency gate

`cairn_validate` exit 0, all checks PASS (16 checks). No principle changed, so
`cairn_impact` does not apply. Toolchain slot: `devtools::document()` produces
no diff; `data/*.rda` regenerate byte-identically from `data-raw/`;
`pkgdown::check_pkgdown()` finds no problems; README.md untouched and in sync;
NEWS.md carries the user-visible changes and leaks no milestone numbers.

Two advisories, neither a gate failure: `sizing (split tripwires)` reports M74's
8 acceptance criteria against the 7 tripwire — a consequence of the gated AC8
amendment, accepted rather than split at this stage — and `work-log format` (47),
almost all of them M7's legacy multi-line entries.

## Review — second pass

Verified 2026-08-07 on `m74-norms-provenance-audit-batch3` after the return-1
fix at `54c1c129`, PR #101. Every line below is a command run at this pass. The
first pass's evidence above stands as the record of what it checked; nothing in
it is edited.

### Acceptance-criteria evidence (re-executed)

- **AC1.** `test-norms-audit-sample-key.R`: 11 pass, 0 fail. Fence re-verified by
  mutation: reproducing the M72 defect in full (per-pass note filter removed AND
  `sample` dropped from the join key) reddens 6 of 11; restored, 11 pass. The
  join-key-alone mutation again leaves all 11 green — the redundancy the first
  pass disclosed, unchanged and re-observed rather than recalled.
- **AC2.** `AUDIT_BATCH` is an 18-row data.frame over 13 instruments carrying
  `instrument, sample, citekey, divisor, scales`; `validate_batch()` TRUE. iipsc
  sample 1 → `hopwood2008` (`divisor = 4`, `scales = FALSE`), sample 2 →
  `soldz1995` (`divisor = 1`, `scales = TRUE`). Batches 1 and 2 unchanged:
  master's committed 130-row ledger against the branch's 130 batch-1+2 rows —
  identical key set, 0 differing values across `shipped`, `source`, `kind` and
  `disposition`.
- **AC3.** All five notes parse: `sodano2006` 56 rows, `horner2024` 56,
  `trucco2013` 76, `hopwood2008` 20, `soldz1995` 38. Each carries a
  `**Provenance.**` block whose extraction status names the channel that read
  the norm table and carries `— observed 2026-08-07`; each has an `INDEX.md`
  line, and `references index<->disk` PASSes.
- **AC4.** Audit re-run: 166 ledger rows, 15 coverage rows, **0 coverage gaps,
  0 angle-copy splits, 0 IP2 breaches**, 0 `UNDISPOSITIONED`, 13 instruments.
- **AC5.** 36 batch-3 ledger rows, 9 of kind `mismatch`, all 9
  `intended-deviation` `Population` (cais 2, iei 2, igicr 3, iipsc 2). The four
  corrections re-confirmed by loading master's and the branch's `.rda` side by
  side: cais `Items` in 7 of 8 octants, cais sample-1 `Size` 213→204, iei `URL`
  split per sample, iipsc sample-1 `Reference` 2011→2008.
- **AC6.** `audited_objects` holds 13 instruments over 18 pinned samples (cais 2,
  iei 2, igicr 3, iipsc 2); all 13 pins `identical()` to the shipped objects.
  `norms-audit.md` carries a verdict per instrument naming compared fields and
  sample count. `test-norms-provenance.R`: 163 pass, 0 fail.
- **AC7.** `devtools::check(args = "--no-manual")` re-run after this pass's five
  record repairs: **0 errors, 0 warnings, 0 notes**, 14m30s. It covers every
  package-facing file in the reviewed tree — the only writes after it started
  were this Review section and a work-log line, both under `cairn/`.
  `document()` produces no diff.
- **AC8.** Re-verified by mutation in both directions: dropping item 37 from
  cais's NO key reddens 3 assertions; repeating item 30 in LM at constant item
  count reddens the same 3. Restored, sha256 of `data/cais.rda` identical to the
  pre-mutation file.

### Consistency gate (re-run)

`cairn_validate` exit 0 — 16 PASS, no FAIL. No principle changed, so
`cairn_impact` does not apply. Toolchain slot: `document()` no diff;
`data/{cais,iei,iipsc}.rda` regenerate byte-identically from `data-raw/`;
`pkgdown::check_pkgdown()` finds no problems; README in sync; NEWS leaks no
milestone numbers. Same two advisories as the first pass.

### Independent review (three lenses + scorer)

- **[S] blame-history:** no regression. PR #99's `each` fix in `data-raw/iei.R`
  is untouched; M72's `csip` divisor of 8, its IP2 and angle-copy checks, and
  M73's note-only/constructed-credit coverage counters and `stamp_ledger()`
  zero-row fix all survive the `audit-norms.R` rewrite. D-039's carve-out
  followed rather than contradicted.
- **[S] prior-PR-comments:** no regression against M72's, M73's or this
  branch's own first-pass findings. It recomputed the cais key diff
  independently and confirms F2 is genuinely fixed rather than reworded, and
  F5's contradiction resolved. GitHub inline-comment surface empty (`[]`).
- **[O] diff-bug:** 22 findings, scored below.

Scores: F5 82 · F3 78 · F6 78 · F7 76 · F8 75 · F1 72 · P1 72 · F10 68 ·
F21 65 · F12 65 · F9 60 · F15 60 · F2 55 · F13 55 · F18 55 · F20 55 · F22 45 ·
F14 45 · F11 42 · F16 40 · F4 30 · F19 30 · F17 28.

**Actioned (>= 80), verbatim:**

- **F5 (82)** — `cairn/references/norms-audit.md`: "All 88 M/SD pairs across the
  nine normative samples matched" contradicts its own enumeration in the same
  sentence — cais 32 + iei 32 + igicr 48 + iipsc 32 = 144 values = 72 pairs.
  Neither reading gives 88. Fixed: the sentence now reads 144 values / 72 pairs
  and marks the correction.

**Also corrected, below threshold, under the correcting-a-record-proven-false
rule** (these are `references/` pages and `NEWS.md` — current knowledge whose
claims this branch itself falsified, not opinions the scorer's threshold
governs):

- **F6 (78), F7 (76), F8 (75)** — `sodano2006.md`, `hopwood2008.md` and two
  `INDEX.md` lines each described pre-fix shipped state in the present tense
  ("assigns exactly four items to each octant"; "the package currently credits
  these norms to a 2011 publication"), which the same branch changed. All four
  sites rewritten to past tense with the M74 correction named and marked.
- **F3 (78)** — `NEWS.md` said "Two shipped values did not match their source"
  while the following bullet describes three provenance records changing,
  double-counting the `Size` and undercounting the total. Corrected to four,
  enumerated as the key plus the three provenance records. Below the actioned
  floor, but the sentence was authored at the return-1 fix and a direct count of
  the corrections in `data/` shows it wrong.

**Logged below threshold, not actioned:** sixteen findings. F1 (72) / P1 (72)
the new `data-raw/cais.R` comment says the octant means sit inside the 1–5
anchors, false for sample 2 — its pre-existing half is now a ROADMAP candidate
row; F10 (68) coverage rows mangle `field` into `"M (sample 1)"`; F21 (65) the
first-pass Review section's commit anchors predate the fix, which this section
supersedes rather than edits; F12 (65) `validate_batch()` does not validate
`divisor`; F9 (60) no shipped-side equivalent of `note-sample-not-audited`;
F15 (60) the per-instrument iteration comment overstates the de-duplication it
achieves; F2 (55) the cais sample-2 metric question — the ROADMAP row again;
F13 (55) `values_agree()` normalises `Items` on the shipped side only; F18 (55)
the `Size` change carries no D-entry; F20 (55) the cais key pin compares exact
whitespace; F22 (45) `validate_batch()` does not type-check `scales`; F14 (45)
instrument-level rows dropped uncovered on a `scales = FALSE` pass (not live);
F11 (42) `note-sample-not-audited` rows file a citekey under `instrument`;
F16 (40) the join key alone is unfenced — disclosed at AC1 both passes; F4 (30)
a NEWS line runs long; F19 (30) `?iei` prints 2025 and `norms(iei)` 2024,
pre-existing and untouched. The `audit-norms.R` robustness family (F9, F10,
F11, F12, F13, F14, F15, F22) is already carried by the ROADMAP candidate row
added at the return-1 fix.

### Disposition

No finding reaches the return floor: F5 is the only actioned finding, it scores
82 rather than 90+, and it falsifies no acceptance criterion — AC6's verdict
rows name their fields and sample counts correctly; the miscount sits in the
narrative section beside them. Defect-return count for this milestone stays at
1, below the thrash threshold. The five corrections above were made review-side
as record repairs and re-verified by a full check; the milestone stays at
`review` and goes to the merge gate.

