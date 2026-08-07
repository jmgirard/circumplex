# M72: Norms provenance audit, batch 1 (CSI family + IITC)

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5
- **Branch/PR:** `m72-norms-audit-batch1` / [PR #98](https://github.com/jmgirard/circumplex/pull/98)

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
  (`Size`), `Population`, and the norms' provenance fields `Reference` and
  `URL`.
- Committed artifacts: synthesis note `cairn/references/norms-audit.md`
  (citekey map, sha256 shelf manifest, scan verdicts, per-instrument audit
  status for all 15 shipped instruments); source notes
  `cairn/references/<citekey>.md` ×5; script `data-raw/audit-norms.R`;
  dispositions input `data-raw/norms-audit-dispositions.csv`; coverage report
  `data-raw/norms-audit-coverage.csv`; pre-fix ledger
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

- [x] AC1: For each batch instrument and each row of its `Norms[[2]]`, the
      cited source's PDF is on the `cairn/references/sources/` shelf, and the
      committed `cairn/references/norms-audit.md` records its citekey, shelf
      filename, sha256, and scan verdict — the verdict from a stated positive
      probe (`pdftotext` text-layer check on the anchor pages plus
      `pdfimages -list`, with `pdfinfo` Producer as one input; an inconclusive
      probe is treated as a scan), re-verified against the live shelf at
      review with the re-check recorded in this file's Review section.
- [x] AC2: A committed source note `cairn/references/<citekey>.md` exists for
      each batch source, carrying a machine-readable table of every
      audited-field value with a shipped counterpart — plus any norm sample
      the source publishes that the package does not ship, recorded as a
      note-only row — each with a page/table anchor; a field the source does
      not publish is listed as `not-published-in-source`. Where AC1's scan
      verdict is scan/OCR, extraction used two independent channels per
      M42-D1 and the note's provenance block says so.
- [x] AC3: `data-raw/audit-norms.R`, committed, parses each source note's
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
- [x] AC4: Every ledger mismatch row carries a disposition joined from the
      committed `data-raw/norms-audit-dispositions.csv` (keyed by instrument
      + field + scale) — `transcription-error` (fixed on this branch),
      `intended-deviation` (documented at the surface where users meet the
      value), `not-published-in-source`, `deferred:<ROADMAP row>`, or an
      escalation to Jeff resolved into one of those before review; the
      post-fix re-run `data-raw/norms-audit-ledger.csv` is committed beside
      the retained pre-fix snapshot, and its surviving mismatch rows all
      carry a disposition other than `transcription-error`.
- [ ] AC5: `tests/testthat/test-norms-provenance.R` enumerates shipped
      instruments by the procedure `instruments()` uses (`data()` plus the
      `circumplex_instrument` class filter), never a hand-list; pins each
      audited instrument's full `Norms` and `Scales` objects to the post-audit
      shipped values — every audited field traceable to its ledger row: source
      value where verified or corrected, shipped value where
      `not-published-in-source` or an approved `intended-deviation` — failing on
      any edit to any audited field including sample N and population, and fails
      when any shipped instrument is absent from `norms-audit.md`'s status table.
- [x] AC6: NEWS.md documents every user-visible instrument-data change this
      milestone ships — each `transcription-error` fix, and each provenance
      correction to a shipped `Reference` or `URL` — each entry narrowed to
      what AC5's pins enforce; if the milestone ships no such change, the
      clean outcome is recorded in the work log and NEWS.md is untouched.
- [x] AC7: `devtools::test()` clean, and `devtools::check(args =
      "--no-manual")` with no new ERRORs/WARNINGs/NOTEs relative to the
      master baseline.

## Coverage

- AC1 → T1
- AC2 → T2, T7
- AC3 → T3, T7, T8
- AC4 → T4, T9, T10
- AC5 → T5, T11
- AC6 → T6, T12
- AC7 → T6, T12

## Tasks

- [x] **T1** — Shelf + manifest: receive the five PDFs from Jeff, shelve
      them, author `cairn/references/norms-audit.md` (citekey map, sha256
      manifest, scan verdicts via the AC1 probe, 15-instrument status table)
      + its INDEX.md line.
- [x] **T2** — Author the five source notes from
      `templates/source-note.md`, copying an existing page's provenance
      idiom (M47 lesson); machine-readable tables with per-value anchors;
      two-channel extraction where T1's verdict is scan/OCR.
- [x] **T3** — Write `data-raw/audit-norms.R`; generate the coverage report
      and the pre-fix ledger; verify the coverage report is clean.
- [x] **T4** — Author `norms-audit-dispositions.csv` with Jeff on
      escalations; fix `transcription-error` rows in `data-raw/` scripts and
      regenerate `data/`; commit the post-fix ledger re-run.
- [x] **T5** — Write `test-norms-provenance.R` (parameterized pins + status
      completeness assert); prove the pins redden by perturbing one shipped
      value (LESSONS guard-teeth rule).
- [x] **T6** — NEWS entries (or clean-outcome work-log line); full
      `devtools::check()` against the master baseline.

Review fix pass (2026-08-06 gate failure on AC2, AC4, AC5, AC6):

- [x] **T7** — Source-note tables gain note-only samples and `Reference`/`URL`
      rows; the script gains a note-only kind exempt from coverage.
- [x] **T8** — Harden `audit-norms.R`: IP2 convention check, failed joins, `NA`
      angles, dropped rows, parse failures all reported; coverage artifact.
- [x] **T9** — Pre-fix ledger rebuilt against pre-fix `data/` in a scratch
      checkout, stamped with both commits; post-fix re-run; dispositions extended.
- [x] **T10** — Document the deviation at `norms()`'s help; fix
      `R/instrument_data.R`'s csiv `@source`; `devtools::document()`.
- [x] **T11** — Pins over full `Norms`/`Scales` (incl. `Sample`, `Label`);
      status assertion anchored; teeth re-proven by mutation on an audited row.
- [x] **T12** — `norms-audit.md` verdicts name the fields they cover; NEWS
      narrowed to the amended AC6; full check.

## Work log

- 2026-08-06: created by /milestone-plan (promoted from the 2026-08-04 interview candidate row; batch, field set, fix route, and priority fixed at the plan gate).
- 2026-08-06: criteria audit ran, two rounds ([O] fresh-context reader): round 1 returned 20 findings (F1 batch-unreachability became the gate's batch question; F2–F20 repaired in redraft); round 2 on the final wording returned 5 surviving defects, all repaired in the committed text (dispositions input CSV, dual ledger paths, closed disposition vocabulary, AC5 pinned to post-audit shipped values, AC7 baseline-relative).
- 2026-08-06: plan gate chose parsing source-note machine-readable tables over hand-typing source values into the script, because a hand-typed table checks one reader's transcription against itself; falsified by the note-block format proving unable to carry a per-value anchor.
- 2026-08-06: plan gate chose a committed dispositions CSV joined by the script over hand-editing the generated ledger, because regeneration must not destroy dispositions; falsified by the instrument+field+scale key failing to identify a mismatch row uniquely.
- 2026-08-06: plan chose full-object regression pins over comparing `data/` to regenerated `data-raw/` output, because the `data-raw/` scripts are themselves the transcriptions under audit; falsified by a source note itself proven mis-extracted against the page image.
- 2026-08-06: status planned→in-progress (/milestone-implement); branch `m72-norms-audit-batch1` cut from pushed master. T1 waits on Jeff shelving the five source PDFs — exact citation list handed over, with two citations flagged as memory-derived for his verification.
- 2026-08-06: T1 shelf received — all five PDFs present and confirmed as the correct papers by first-page read; both memory-flagged citations verified correct. Scan probe (text-layer density, `pdfimages -list`, `pdfinfo` Producer): all five born-digital, no full-page images, so M42-D1's two-channel rule does not fire; norm tables were nonetheless read through page images as well, because table-structure reconstruction can misassign a cell even where every glyph is faithful.
- 2026-08-06: **audit result — every shipped M and SD in all five instruments is correct; zero transcription errors.** csig verified against locke2014 Figure 2 (p. 436 — the values are in a *figure*, unreachable by text extraction), csip against boudreaux2018 Table 1 overall column (n=712), iitc against bliton2019 Table 1. csie and csiv verified against Locke's website norms tables through two independent channels (model-mediated fetch + raw-HTML tag-stripped extraction), which agree exactly.
- 2026-08-06: **provenance defects found in csie and csiv (values correct, sourcing wrong).** Neither cited article contains octant M/SD: locke2007 Table 1 carries only Cronbach's α (its n=367 note does match the shipped `Size`), and locke2000 carries no octant descriptives and never reports N=1200 (its samples are 588/471/248/202). Both sets of values are published only on Locke's website, and both shipped `URL`s are dead — `webpages.uidaho.edu/klocke/*.htm` now 301s to the `kennethlocke.org` homepage without preserving the path. Retrieved pages archived to the gitignored shelf as `locke_csi{e,v}_norms_2026-08-06.html`.
- 2026-08-06: correction to the entry above (append, not edit — work log is history): the two Locke `URL`s are **not dead**. Both return HTTP 200 when redirects are followed; they 301 to `https://kennethlocke.org/` without preserving the path, so they resolve to a homepage instead of the cited norms table. The defect is a lost path, not a broken link. Measured with `curl -sL -w '%{url_effective}'`. Every other shipped norms URL also resolves: `iei` (OSF) and both Mind Garden links return 200, and the six publisher DOIs return 403, which is the known bot-block M7 already documents for `cran-comments.md`, not a broken link.
- 2026-08-06: T1/T2 complete — `cairn/references/norms-audit.md` (15-instrument status table, instrument→citekey map, sha256 + scan-verdict manifest over all ten shelved sources) and five source notes with machine-readable value tables, all six carrying INDEX lines. Item-to-scale assignments turned out to be verifiable after all for three instruments: Locke publishes the exact numbering on his CSIE/CSIV scoring pages and CSIG items page, and all 24 octant mappings match the shipped values. csip and iitc publish no item numbering (iitc's items are in an online appendix), recorded as `not-published-in-source`. Angles are recorded `not-published-in-source` for four of five — only locke2000 Figure 2 prints degrees — because the degree assignment is the package's own IP2 convention, not a source claim.
- 2026-08-06: T4 fix applied at Jeff's gate choice (update Reference and URL). Verified first that all five `data-raw/` generators reproduce their shipped `.rda` byte-identically, so the regeneration diff is exactly the intended one — confirmed after the edit: norm values, scales, anchors, details, `Size` and `Population` all unchanged in both objects. csie keeps `Reference = "Locke & Sadler (2007)"` because the audit proved it correct — the norms page states the sample is Study 1 of that article, and its n=367 matches the article's own Table 1 note — so only its URL moved. csiv's Reference was wrong and changed to `"Locke (n.d.); instrument published as Locke (2000)"`: its 1,200 Idaho undergraduates are a different sample from the article's N=588. Both URLs now point at the live norms tables. Ordering deviation from the plan (fix ran before T3's ledger): the pre-fix ledger is generated against the pre-fix commit rather than the working tree.
- 2026-08-06: two collateral observations for Jeff. csig's source figure prints *identical* α/M/SD (.73 / 2.96 / .68) for PA and NO — faithfully transcribed by the package, but possibly an error in the source itself. And both Locke pages carry norm sets the package does not ship (csie adult N=1,234; csiv Hopwood-2022 adult N=980 and 32-item MTurk N=1,244) — AC2 note-only rows, and possible future additions.

- 2026-08-06: T3 complete — `data-raw/audit-norms.R` parses each source note's machine-readable block and compares it against values enumerated from the package objects (join key normalised across the `Scale`/`Abbrev` column shapes, angles compared mod 360, csip's documented `/8` applied). Result: **0 coverage gaps, 0 angle-copy splits, 0 mismatches among comparable values.** 53 ledger rows: 48 `not-published-in-source` (angles for four instruments, item maps for csip and iitc) and 5 `Population` rows.
- 2026-08-06: the `Population` comparison was dead on first write and was fixed before it could certify anything — the notes recorded the package's own summary string as the source value, so both sides came from the same place and the check could never fail (the LESSONS "two sides built by the same line of code" trap). The notes now carry each source's actual wording, which surfaces all five as real `intended-deviation` rows.
- 2026-08-06: T4 complete — `data-raw/norms-audit-dispositions.csv` gives all 53 rows a disposition (48 `not-published-in-source`, 5 `intended-deviation`); no row is undispositioned and there are **no `transcription-error` rows at all**. Pre-fix and post-fix ledgers are both committed and are byte-identical outside the commit stamp, because the provenance fix touched `Reference` and `URL` — fields outside the plan's audited field set. Recorded rather than papered over: a provenance audit whose ledger cannot show the provenance defect it found is a real gap in the plan's field set, and it is only covered because AC5's pins reach `Reference` and `URL` even though AC3's ledger does not. Widening the audited field set is left as a decision for Jeff, not taken unilaterally mid-implementation.
- 2026-08-06: T5 complete — `tests/testthat/test-norms-provenance.R`, 76 assertions, enumerating instruments by the same `data()`-plus-class-filter procedure `instruments()` uses rather than a hand-list (AC5 names `instruments()`, which prints and returns `NULL`, so its return value cannot be consumed; the procedure is mirrored and this deviation recorded). Guard teeth proven by mutation, not by eye: perturbing M, SD, `Size`, `Population`, `Reference`, `URL`, an item map, and an angle copy each reddened the suite, as did deleting one instrument's row from `norms-audit.md`. The status-table half is split from the pins and marked development-only so a skip under `R CMD check` cannot silently take the pins with it (M70).

- 2026-08-06: T6 complete — NEWS entry under a new `## Instrument data` heading records the re-verification and the two provenance corrections, narrowed to what the AC5 pins enforce. `devtools::test()`: 0 failures, 5864 passing. `devtools::check(args = "--no-manual")`: **0 errors, 0 warnings, 0 notes** (16m52s). The suite's 4 warnings are pre-existing, not introduced: the branch changes no file under `R/` or `src/`, and both instrument-touching test files report 0 warnings. `--no-manual`'s skipped PDF-manual step (the M7/M57 lesson) carries no risk here because no roxygen changed.
- 2026-08-06: status in-progress→review. Session hygiene note: the working tree was briefly checked out to master mid-implementation to measure a warning baseline — a careless move with a live branch. It was stopped immediately; the branch, its commits and the working tree were verified intact and the stash empty, and the baseline question was then settled from the branch diff instead.

- 2026-08-06: review → **in-progress**. Gate failed on four acceptance criteria, each verified in-session rather than taken on a reviewer's word: AC6 (NEWS.md touched with zero `transcription-error` rows), AC2 (note-only rows in prose, not the machine-readable table), AC4 (`intended-deviation` undocumented at any user surface; and the committed pre-fix ledger is stamped with a descendant of the fix commit, so its work-log provenance claim is false), AC5 (`Norms[[2]]$Sample` unpinned — mutation leaves the suite green). Two records this review had to correct in itself: it ticked AC6 on a charitable reading of two clauses the never-reinterpret rule forbids, and its earlier mutation evidence for the status-table assertion held only for unaudited instruments. 18 further findings recorded in the Review section for the fix pass. AC1, AC3 and AC7 verified and stand.

- 2026-08-06: amendment return: AC6 — "NEWS.md documents every user-visible instrument-data change this milestone ships — each `transcription-error` fix, and each provenance correction to a shipped `Reference` or `URL` — each entry narrowed to what AC5's pins enforce; if the milestone ships no such change, the clean outcome is recorded in the work log and NEWS.md is untouched." Gate choice at Jeff's fix-pass gate: keep the NEWS entry rather than revert it, because the branch ships a user-visible change to what csiv reports as its source and the original wording assumed only a wrong number could ever change.
- 2026-08-06: amendment at the same gate — the audited field set in Scope widens to include the norms' `Reference` and `URL`, so the ledger can show the one defect this audit actually found (review finding 9, previously left as a decision for Jeff). Falsified by a source note proving unable to state a citation or link the shipped value can be compared against.
- 2026-08-06: fix-pass tasks T7–T12 added for the four failed criteria plus the 12 recorded review findings; Coverage remapped. Tasks section compressed in the same pass to stay under the 150-line plan-owned cap.
- 2026-08-06: gate answers on the other two open questions — the `Population` deviation is disclosed in `norms()`'s help rather than by reshipping five population strings; the false "pre-fix" ledger is rebuilt honestly against pre-fix `data/` rather than dropped.

- 2026-08-06: T7 complete — the five machine-readable tables now carry 7 note-only rows (csie adult N=1,234; csig Study 2 n=327 plotted-only; csip women n=121 and men n=70; csiv N=980 and N=1,244; iitc Study 2 N=608, the last four found by re-reading the shelf copies rather than taken from the earlier prose) plus a `Reference` and `URL` row each. `Reference` compares by containment — the shipped string must credit the author-year the source itself supports, and may carry context around it — so the pre-fix csiv "Locke (2000)" fails while the corrected string passes. locke2000's LM angle row now records the 0 the source prints instead of the package's 360; the IP2 translation moved to the shipped-side check in T8. Run: 0 coverage gaps, 7 note-only, 53 ledger rows unchanged.

- 2026-08-06: T8 complete, every new check proven by mutation rather than by eye (clean run: 0 gaps, 0 splits, 0 IP2 breaches, 53 ledger rows). Setting csiv's LM to 0 in both copies now raises 2 IP2 breaches where the mod-360 ledger stays byte-identical (review finding 1); breaking a scale name raises a split where `which(NA)` used to drop it (finding 2); an `NA` angle raises both a split and an IP2 row (finding 3); a note row carrying a literal `|` now errors instead of vanishing (findings 10, 11); and restoring the pre-fix csiv `Reference` produces a 54th row, `kind = mismatch` — the audit can now see the defect the milestone found. Coverage report written to `data-raw/norms-audit-coverage.csv` (finding 8); the hardcoded run date is gone and the single `commit` column is replaced by `script_commit` + `data_commit`, since a run's own HEAD is necessarily the parent of the commit that lands the ledger (finding 7).

- 2026-08-06: T9 complete — the pre-fix ledger is now a real pre-fix artifact, superseding the earlier claim that the committed one was (the review measured its stamp as a descendant of the fix commit). Rebuilt in a detached scratch worktree carrying `data/` from `c2453755`, the parent of the T4 fix commit, with today's script and notes; it stamps `script_commit 54fbf783` and `data_commit c2453755` rather than one ambiguous commit. The two ledgers are no longer byte-identical outside the stamp, which is the point: pre-fix carries 56 rows including three `transcription-error` rows (csie `URL`, csiv `URL`, csiv `Reference`), post-fix carries 53 with none. Neither has an undispositioned row. The widened field set closed the gap this milestone had to record as uncovered.

- 2026-08-06: T10 complete — `?norms` now says the population label is a standardized package label deliberately broader than the source's own, names the shared "American college students" case, and points the reader at the printed reference and URL for the source's description. That is the surface where a user meets the value, which is what AC4 asks of an `intended-deviation`. `?csiv` no longer contradicts `norms(csiv)`: its `@source` block separates the instrument's article from the unpublished norms table the shipped statistics come from, and `?csie` gains the parallel sentence (its Reference is correct, but the article is not where the octant statistics are published) — review findings 5 and 6. `devtools::document()` rewrote `csie.Rd`, `csiv.Rd`, `norms.Rd`; `tools::checkRd()` clean on the changed pages.

- 2026-08-06: T11 complete — the field-list pins are replaced by literal pins of the whole `Norms` and `Scales` objects, because a field list pins what its author remembered to name and this one forgot `Sample` and `Label` (review AC5 failure). 57 assertions, all green. Twelve mutations each redden the suite, measured not eyed: `Norms[[2]]$Sample`, `Norms[[1]]$Sample`, `Scales$Label`, M, SD, `Size`, `Population`, `Reference`, `URL`, an item map, one desynced angle copy, and LM 360→0 in both copies (the last caught by a new IP2 convention assertion mirroring the script's). The status-table assertion is now scoped to the `## Audit status` section, and deleting csie's row reddens it — the case the earlier evidence claimed but did not cover, since every audited instrument also appears in the citekey map below.

- 2026-08-06: T12 complete — `norms-audit.md`'s status column no longer says a bare `verified`: each verdict names the fields actually compared, so csip and iitc read `verified: M, SD, Size, Reference, URL (no angles, no item map)` rather than implying a transposed item map would have been caught (review finding 12). NEWS narrowed to what the pins enforce — the item-map claim now says "every item-to-scale assignment its source publishes" — and gains a line for the `?norms` population disclosure. `devtools::test()`: 0 failures, 5845 passing (the pins test dropped from 76 assertions to 57 when the field lists became whole-object comparisons); the same 4 pre-existing warnings. `devtools::check(args = "--no-manual")`: **0 errors, 0 warnings, 0 notes** (16m06s).
- 2026-08-06: status in-progress→review; all four failed criteria and all 12 recorded findings addressed on the branch.

- 2026-08-06: second review — AC1, AC2, AC3, AC4, AC6, AC7 verified with fresh evidence and ticked; two findings scored at or above 80, both fixed in-review (`norms-audit.md` verdicts now name `Population`; csig's Figure 2 re-read in two further independent channels, confirming every shipped value and placing the PA/NO duplication in the source rather than the extraction). 37 lower-scored findings logged in the Review section, none actioned.
- 2026-08-06: **amendment return: AC5 — "enumerates shipped instruments by the same procedure `instruments()` uses (`data()` plus the `circumplex_instrument` class filter), never a hand-list"** — proposed. `instruments()` returns `NULL` (measured), so the clause as written cannot be satisfied by any test; the deviation was recorded at T5 and should have been an amendment then. Status review→in-progress for this amendment alone.

- 2026-08-06: amendment return: AC5 — "enumerates shipped instruments by the procedure `instruments()` uses (`data()` plus the `circumplex_instrument` class filter), never a hand-list" — applied at Jeff's gate; the test is unchanged, only the criterion's text. Second amendment of the milestone, first naming AC5.
- 2026-08-06: D-039 recorded at Jeff's gate — the printed `norms()` provenance change is a factual correction under IP5, not a GP4 break; the angle-range wording in IP2 was left alone at the same gate. Status in-progress→review.

## Decisions

## Review

### Second review, 2026-08-06 (after the T7–T12 fix pass)

**AC1 — met.** All ten shelved sources re-hashed live against the committed
manifest: 10 match, 0 missing. Scan verdicts re-derived from the stated probe
(text-layer density per page, `pdfimages -list`, `pdfinfo` Producer): all five
PDFs born-digital, image counts matching the manifest, so M42-D1's two-channel
rule does not fire on the container verdict. See the AC2 note on where that
verdict was not sufficient.

**AC2 — met.** All five notes parse: 37/37/38/38/37 rows, 0 blank anchors, 0
duplicate keys, and the note-only rows are now table rows rather than prose —
7 of them (csie N=1,234; csig Study 2 n=327; csip women n=121 and men n=70;
csiv N=980 and N=1,244; iitc Study 2 N=608), each carrying an anchor, and each
exempt from the coverage report by construction. `Reference` and `URL` rows are
present for all five under the widened field set. Provenance blocks state the
extraction channels per source.

**AC3 — met.** Fresh run of `data-raw/audit-norms.R`: 0 coverage gaps, 7
note-only rows, 0 angle-copy splits, 0 IP2 breaches, 53 ledger rows. Re-running
reproduces the committed ledger byte-for-byte apart from its own commit stamp
(the stamp names the commit the run was made against, necessarily the parent of
the commit containing the file). Source values are parsed from the notes.

**AC4 — met.** Post-fix ledger: 53 rows, 0 undispositioned, 0
`transcription-error`. The pre-fix ledger is now genuinely pre-fix — its
`data_commit` is `c2453755`, which `git rev-parse 0dacb2f1^` confirms is the
parent of the T4 fix commit — and carries 56 rows including the three
`transcription-error` rows the pre-fix state should show. The
`intended-deviation` on `Population` is documented where users meet it:
`man/norms.Rd` states the label is a standardized package label deliberately
broader than the source's own and points at the printed reference.

**AC5 — the pins pass; the criterion's first clause cannot.** Evidence on the
pins: 57 assertions green; the mutation that passed at the first review now
fails (setting `Norms[[2]]$Sample` to 7 reddens the suite, as does editing
`Scales$Label`), and deleting iitc's row from `norms-audit.md`'s status table
reddens it — the audited-instrument case the first review's evidence did not
cover. But AC5 opens "enumerates shipped instruments via `instruments()` (never
a hand-list)", and `instruments()` prints its listing and returns `NULL`
(measured at review: `is.null(instruments())` is `TRUE`), so no test can consume
its return value. The criterion as written is unsatisfiable — the wording is
wrong, not the work, which mirrors the procedure `instruments()` uses. Reading
"via `instruments()`" as "by the procedure `instruments()` uses" is exactly the
charitable reading the never-reinterpret rule forbids, and is the reading that
cost this milestone its first review. Routed as an amendment return.

**AC6 — met against the amended criterion.** The ledger has 0
`transcription-error` rows post-fix, but the branch ships two provenance
corrections, which the amended AC6 requires NEWS to document; it does, and each
claim is enforced by an AC5 pin.

**AC7 — met.** Fresh at review HEAD: `devtools::check(args = "--no-manual")`
**0 errors, 0 warnings, 0 notes** (15m47s), tests inside it OK.

### Consistency gate (second review)

`cairn_validate`: all checks pass (48 advisories, none a gate failure).
DESIGN.md untouched, so `cairn_impact` is a clean skip. Toolchain gate
(`r-package`): `devtools::document()` produces no diff; all five instruments
regenerate content-identically from their `data-raw/` scripts; README.md in
sync; `pkgdown::check_pkgdown()` reports no problems; NEWS.md carries the
user-visible changes.

### Findings (second fan-out) and their triage

Three fresh-context reviewers on distinct evidence bases produced 39 candidate
findings; a separate scorer, given the diff and the plan, scored each. Two
scored ≥80, neither an acceptance-criterion failure, so no return under the
floor.

- **Population omitted from the verdict strings (82) — fixed now.** The status
  table named every compared field except the one that did not match. Verdicts
  now read `… ; Population deviates by design`.
- **csig's M/SD were a single-channel read (80) — fixed now, by doing the
  second channel.** The values are printed inside Figure 2 and no text layer
  carries them, so the born-digital container verdict did not license a single
  read. Figure 2 was re-read at 400 dpi and independently OCR'd: PA .73/2.96/.68
  · BC .77/2.53/.86 · DE .75/2.02/.88 · FG .59/1.88/.74 · HI .77/2.24/.90 · JK
  .77/2.89/.76 · LM .78/2.97/.71 · NO .73/2.96/.68 — matching the shipped values
  exactly. The PA/NO duplication is in the source, not the extraction;
  `locke2014.md`'s provenance block records the correction.

37 findings scored below 80 and are excluded from the actioned list, logged
here so none is dropped. The substantive ones, worth carrying to batch 2: the
`Reference` containment rule admits a string that credits the source and
contradicts it (76); the note-side `Reference` value is the note author's
constructed credit rather than a quoted line, the same both-sides-one-hand trap
this milestone fixed for `Population` (74); nothing binds the test's pin list to
`norms-audit.md`'s verdicts in either direction (65); the shipped-side join key
ignores `Sample`, so a two-sample instrument would compare sample 2 against
sample 1's source values — latent for batch 1, live for `iipsc` (55); the
coverage CSV repurposes its `field`/`scale` columns for note-only rows (78); an
empty ledger crashes the stamp assignment (70); `?norms`'s new disclosure
describes the narrower-population case but not csig's differently-worded one
(74); `?iitc` still says "(in press)" against the pinned "Bliton & Pincus
(2019)" (68, on a line the branch does not touch); the two `URL` repointings are
dispositioned `transcription-error` where link rot has no category (72); the
audit script prints its findings and always exits 0 (55). Also logged: DESIGN.md
IP2's range is written `[0, 360)` while stating LM = 360, which the new checks
implement as `(0, 360]` (38) — a principle-text question only the maintainer can
settle; and the printed `norms()` output changed without a GP4 decision entry
(76).

### First review, 2026-08-06 (gate failed)

PR [#98](https://github.com/jmgirard/circumplex/pull/98).

### Acceptance-criterion evidence

- **AC1 — met.** All ten shelved sources re-hashed against the committed
  manifest at review time, live: every sha256 matches, nothing missing. Scan
  verdicts re-derived from the stated positive probe (text-layer density,
  `pdfimages -list`, `pdfinfo` Producer); all five PDFs born-digital, so
  M42-D1's two-channel rule does not fire.
- **AC2 — FAILS as written.** The blocks parse and the
  `not-published-in-source` rows are right, but AC2 also requires "any norm
  sample the source publishes that the package does not ship, recorded as a
  **note-only row**" in the machine-readable table. Those samples (locke2007
  N=1,234; locke2000 N=980 and N=1,244; bliton2019 N=608) exist only as prose
  in the notes, never as table rows — and `audit-norms.R` has no note-only
  concept, so adding them would surface as coverage gaps with no exemption.
- **AC3 — met.** Fresh run of `data-raw/audit-norms.R`: 0 coverage gaps, 0
  angle-copy splits, 53 ledger rows carrying a generated date and commit stamp.
  Source values are parsed from the notes, not retyped into the script.
- **AC4 — FAILS as written.** The ledger is fully dispositioned (53 rows, 0
  undispositioned, 0 `transcription-error`), but AC4 requires
  `intended-deviation` be "documented at the surface where users meet the
  value". `norms()` prints `Population` verbatim with no indication it is a
  package-normalised summary — `norms(csiv)` says "American college students"
  where the source says "University of Idaho undergraduates, late 1990s and
  early 2000s", a materially narrower population, undisclosed in `R/`, `man/`
  or `vignettes/`. Separately, the committed "pre-fix" ledger is not a pre-fix
  artifact: its own stamp is `e4ecef4d`, a descendant of the T4 fix commit
  `0dacb2f1` (verified with `git merge-base --is-ancestor`), so the work-log
  line claiming it was generated against the pre-fix commit is false.
- **AC5 — FAILS as written.** 76 passing and the enumeration is procedural,
  but AC5 requires pinning the **full** `Norms` and `Scales` objects: mutating
  `Norms[[2]]$Sample` to 7 leaves the suite green (verified live), and
  `Norms[[1]]$Sample` and `Scales$Label` are likewise unpinned. `Sample` is the
  key linking the two norms frames, so a corrupted value would misroute
  `norm_standardize()` with the pins still passing. The status-table assertion
  also has no teeth for the five audited instruments: its regex matches
  anywhere in the file, and each batch-1 instrument also appears in the citekey
  map, so deleting its status row still passes — this review's earlier mutation
  evidence held only for the ten unaudited instruments and was overstated.
- **AC6 — FAILS as written.** The post-fix ledger has 0 `transcription-error`
  rows, so the criterion's second clause binds: "the clean outcome is recorded
  in the work log **and NEWS.md is untouched**". NEWS.md *was* touched — it
  gains `## Instrument data` for the csie/csiv provenance corrections. This
  review initially ticked AC6 by reading the two clauses as alternatives; that
  was a charitable reading of the kind the never-reinterpret rule exists to
  stop, and the tick is withdrawn. Caught by the blame-history reviewer.
- **AC7 — met.** `devtools::test()` 0 failures / 5864 passing;
  `devtools::check(args = "--no-manual")` **0 errors, 0 warnings, 0 notes**
  (16m52s). No new ERROR/WARNING/NOTE against the master baseline — the branch
  changes no file under `R/` or `src/`.

### Consistency gate

`cairn_validate`: all checks pass (47 advisories, none a gate failure).
DESIGN.md is untouched by the branch, so no principle changed and
`cairn_impact` is a clean skip. Toolchain gate (`r-package` profile):
`devtools::document()` produces no diff; all five instruments regenerate
byte-identically from their `data-raw/` scripts; README.md in sync;
`pkgdown::check_pkgdown()` reports no problems.

### Review findings (fresh-context fan-out, 2026-08-06)

Three reviewers ran on distinct evidence bases. The prior-review lens reported
**no prior-review evidence** (no archived `## Review` section touches these
files; the GitHub inline-comment probe returned empty, matching this repo's
recorded pattern). The blame-history lens found no contradiction of any D-entry
and confirmed the change is consistent with IP5. The diff-bug lens returned 22
findings, several measured live with reproductions.

**Gate failures — verified in this session, not taken on report:**

- **AC6** — NEWS.md touched while the ledger has 0 `transcription-error` rows.
- **AC2** — note-only rows exist in prose, not the machine-readable table.
- **AC4** — `intended-deviation` not documented at any user-facing surface.
- **AC5** — `Norms[[2]]$Sample` mutation leaves the suite green (measured).

**Other findings carried to the fix pass** (triage and scoring belong there;
these are recorded so none is lost):

1. The mod-360 angle comparison cannot detect an IP2 breach — mutating csiv's
   LM from 360 to 0 leaves the audit byte-identical to a clean run. The one
   instrument whose source publishes degrees is exactly where the check must
   bite, and it does not.
2. `angle_copies_agree()` returns "agree" when the join key fails to match:
   `which(NA)` drops the row rather than flagging it.
3. An `NA` in either shipped angle copy is invisible to every check.
4. `locke2000.md` records the Angle/LM value as `360` — the package's
   convention — while its own anchor says the source prints `0`. The source
   side was translated before comparison: the same "both sides from the same
   place" trap this milestone caught for `Population` and left standing for
   angles.
5. `R/instrument_data.R:63` and `man/csiv.Rd:11` still attribute csiv solely to
   Locke (2000) — the attribution this milestone condemns — so `norms(csiv)`
   and `?csiv` now disagree. The recurring sibling-surface miss (LESSONS,
   M56/M62/M63/M66/M68/M69).
6. csie is left in the same state csiv was corrected for, undisclosed in NEWS.
7. The ledger's `generated` date is hardcoded to 2026-08-06; the `commit` stamp
   makes the file unreproducible at the commit containing it.
8. The coverage report is printed, never written to an artifact.
9. `Reference`/`URL` sit outside the audited field set (already disclosed).
10. `parse_source_note()` silently drops any row not splitting into exactly 4
    cells — an anchor containing a literal `|` would vanish without diagnostic.
11. A parse failure lands in the ledger as `kind = "mismatch"` and is then
    eligible to be dispositioned away as an intended deviation.
12. `norms-audit.md` marks all five instruments `verified` although 48 of 53
    rows are `not-published-in-source`; for csip and iitc that is every angle
    and every item map, so a transposed item map would read as `verified`.
