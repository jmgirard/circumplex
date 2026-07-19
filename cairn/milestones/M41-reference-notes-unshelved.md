# M41: Source notes for the fit-index benchmark pair

- **Status:** review
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** `m41-reference-notes-benchmarks` · https://github.com/jmgirard/circumplex/pull/67

## Goal

Author committed source notes for the two fit-index benchmark sources the
package cites together wherever it reports RMSEA, SRMR, CFI, or TLI.

## Scope

**In:** Two source notes, following the page conventions M40 establishes.

- `browne1992a.md` — Browne & Cudeck, *Alternative Ways of Assessing Model
  Fit*. The shelf holds `sources/browne1992a.pdf` = the **1992** SMR 21(2)
  230–258 article. The page's anchor edition is that article; the 1993 Bollen
  & Long chapter (ch. 6, pp. 136–162) is named **neutrally**, never as a
  reprint, because verbatim-ness was investigated and could not be
  established (work log, 2026-07-19). The page also carries the authors' own
  hedge that the cutoffs rest on subjective judgment.
- `hu1999.md` — Hu & Bentler (1999), *SEM* 6(1) 1–55, shelf
  `sources/hu1999.pdf`. The SRMR and CFI/TLI cutoffs as the repo cites them.

The two are cited together at `R/ssm_ci_oop.R:341,350-351,404,415-419`,
`R/ssm_ci_accuracy.R:1016-1019`, and
`vignettes/evaluating-circumplex-structure.Rmd:93-96,613,625`.

**Citekey trap (page-naming decision, 2026-07-19 plan gate):** page filenames
follow Jeff's Better BibTeX citekeys, so the Browne & Cudeck page is
`browne1992a.md` even though `browne1992.pdf` is Browne *alone*. The `a`
suffix here marks a different author set, not a second Browne work — the page
and `INDEX.md` both say so explicitly.

The repo's two-channel transcription protocol applies to both pages: a visual
page read and an independent `pdftotext -layout` extraction, diffed on every
load-bearing numeral, with between-channel discrepancies recorded rather than
silently resolved.

**Cheung & Rensvold (2002) owes no page** (2026-07-19 plan gate, correcting
this milestone's previous Scope, which claimed a reliance that does not
exist). `sources/cheung2002.pdf` is shelved, but no shipped code, vignette, or
test cites it: the invariance gate ships Δχ² only, a computed quantity needing
no literature constant, and the ΔCFI option was left as an unexercised offer
(`devel/m5-sem-design.md:751-759`, §12.2 item 2). This milestone records the
no-page-owed finding in `INDEX.md`, dated, exactly as `acton2002.pdf` is
handled. Offering ΔCFI as a criterion is a ROADMAP candidate; if it is ever
taken up, that milestone authors the page.

**Out:** Browne (1992) CPM specification and Browne (1982) → M42. Acton &
Revelle (2004) and Wendt et al. (2019) → M43. Package code changes — the work
log closed the edition question and `R/ssm_ci_oop.R:415` keeps citing Browne &
Cudeck (1993). Editing the `devel/` transcriptions → after M7 archives
(ROADMAP candidate row).

## Acceptance criteria

- [x] `cairn/references/browne1992a.md` exists carrying every template
      section, with each extracted value page- or table-anchored; the 1992 SMR
      article is its anchor edition and the 1993 chapter is named neutrally
      ("also appears as ch. 6, pp. 136–162"), never as a reprint.
- [x] The p. 239 passage is quoted verbatim carrying all three of its
      thresholds (0.05 close fit, 0.08 reasonable error of approximation,
      and "greater than 0.1" would-not-employ), anchored by the printed
      running head rather than inferred from the 238/240 markers. The .05
      sentence's chapter anchor is recorded as p. 144 with its Google Books
      provenance; the .08/0.1 sentence's chapter page is recorded as
      **unverified and not derived**, with the −94 offset trap stated (the
      arithmetic predicts chapter p. 145; the real page is 144). The page
      also records two things the repo's own wording smooths over: the
      source prints **0.1**, not `.10`, and it states a preference ("would
      not want to employ") rather than the "fits poorly" verdict at
      `R/ssm_ci_oop.R:340` and `vignettes/evaluating-circumplex-structure.Rmd:93`.
- [x] `cairn/references/hu1999.md` exists carrying every template section,
      with every cutoff the repo cites (SRMR, CFI, TLI) quoted verbatim and
      page-anchored.
- [x] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names the specific citing lines
      listed in Scope, verified against the files.
- [x] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40), plus a dated note recording that `sources/cheung2002.pdf` is
      shelved and owes no page; `cairn_validate` reports
      `references index<->disk` PASS with no `references staleness` WARN.
- [x] `git diff --stat devel/` is empty, no file outside `cairn/` is modified,
      and each written page's tail bytes are checked for leaked tool-call
      scaffolding (M34).

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T1, T3
- AC4 → T2, T3, T4
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1. Re-verify `browne1992a.pdf` and `hu1999.pdf` on the shelf and stamp
      the observation — the shelf is live and moved twice during M40, so
      confirm rather than trust this plan's inventory.
- [x] T2. Author `browne1992a.md`: two-channel read of p. 239, the
      edition-relationship record carried over from the work log, and the
      offset trap.
- [x] T3. Author `hu1999.md`: two-channel read of the cutoffs the repo cites.
- [x] T4. Write both pages' `Traces to` sections against the actual citing
      lines, not this plan's list.
- [x] T5. `INDEX.md` entries + the dated `cheung2002.pdf` no-page-owed note;
      `cairn_validate` clean; tail-byte and untouched-tree checks.

## Work log

- 2026-07-19: all tasks done, status → review. The profile's `verify` slot did not fire: it is conditional on roxygen changes (`document()`) or code changes (`devtools::test()`), and the branch diff is `cairn/` markdown only — 0 files outside `cairn/`, matching M40's docs-only precedent.
- 2026-07-19: **T1-T5 done; both pages authored and validated.** T1 re-verified the shelf: `browne1992a.pdf` and `hu1999.pdf` both present, identified by first-page read rather than filename, with printed-page locations established from running heads (Browne & Cudeck printed 239 = PDF p. 10; Hu & Bentler PDF page n = printed page n) — observed 2026-07-19. T2/T3 authored both pages against M41-D1's two channels. T4 verified every `Traces to` anchor against the files, correcting one asserted line range (`R/ssm_ci_accuracy.R` comment starts at 1014, not 1015). T5: `INDEX.md` gained both entries plus a corrected owes-no-page ledger now covering acton2002, cudeck1983 and cheung2002; `cairn_validate` reports `references index<->disk` PASS and `references staleness` OK; `git diff --stat devel/` empty, nothing modified outside `cairn/`, and both pages' tail bytes checked clean (M34).
- 2026-07-19: **substantive amendment to AC2, at the implementation question gate.** The plan described "both RMSEA cutoff sentences" as the .05 and .08 sentences. Reading p. 239 shows one continuous passage carrying **three** thresholds, and the two the package actually ships are **.08 and 0.1** (`ssm_ci_rmsea_reasonable`, `ssm_ci_rmsea_poor`) — the .05 is the one with the verified 1993 chapter anchor but is used nowhere. AC2 now covers all three verbatim and additionally requires the page to record two departures the repo's own wording makes: the source prints `0.1` not `.10`, and states a preference ("would not want to employ") where the package says "fits poorly". Jeff chose this amendment at the gate over leaving AC2 as planned.
- 2026-07-19: two between-channel discrepancies found and recorded on `browne1992a.md`, both text-layer defects resolved in favour of the rendered page: `pdftotext` emits "RMSEAgreater" as one word, and drops the italics the print carries on "close fit". No numeral differed between channels on either page. Also recorded on `hu1999.md`: Hu & Bentler's own RMSEA cutoff is **.06**, not the .08/.10 the package ships from Browne & Cudeck — the repo's attribution is correct, but the two pages are cited in the same sentences and the confusion is one page-read away.
- 2026-07-19: **re-sized at the plan gate its own `Out:` clause called for.** The shelf inventory M41's T1 was to perform is done here — all seven sources are present, identified by first-page read, and recorded in the M42/M43 plans alongside this one. Seven notes plus a full CPM specification is three milestones' work against M40's demonstrated throughput of two notes, so M41 narrows to the fit-benchmark pair and keeps its ID because this work log is entirely about the Browne & Cudeck edition question; Browne (1992) + Browne (1982) → M42, Acton & Revelle + Wendt → M43. Two Scope corrections: **Cheung & Rensvold has no shipped reliance** (zero hits across `R/`, `vignettes/`, `tests/`; the ΔCFI offer at `devel/m5-sem-design.md:751-759` was never taken up, and its own caveats exclude the robust estimators `ssm_sem()` defaults to), so it owes no page and the ΔCFI feature question becomes a candidate row; and `browne1982_pp95-96.pdf` has replaced the four page images, which `grassi2010.md:137` still names — M42 corrects that in place.
- 2026-07-19: **Browne & Cudeck edition question CLOSED for T3 — do not reopen without the physical book.** Established by web research: SMR **21(2), Nov 1992** was a special issue edited by Bollen & Long (their own "Tests for Structural Equation Models" is article 1, DOI 10.1177/0049124192021002001), and the 1993 Sage volume is its book version — same editors, same papers, contiguous journal pagination (Bollen & Stine 205-229 immediately precedes Browne & Cudeck 230-258), and a matching TOC order with "Alternative Ways of Assessing Model Fit" sixth. **Whether the chapters are VERBATIM reprints is unresolved and stays unasserted**: the only claim found either way is an anonymous Amazon customer review ("the text is not identical") reaching us through a search-engine paraphrase, which contradicts Jeff's Google Books spot-check that the visible pages look identical; the book's preface, which would state the relationship outright, is omitted from the Google Books preview — observed 2026-07-19. **T3 therefore cites Browne & Cudeck (1992), SMR 21(2) 230-258, anchored p. 239** (both cutoff sentences, bracketed by the printed 238/240 markers, verified by pdftotext + Jeff's read) — the original publication, on the shelf, and re-readable. The chapter is named on the page NEUTRALLY ("also appears as ch. 6, pp. 136-162"), never as "Reprinted in", because verbatim-ness is exactly what we could not establish. Chapter anchor for the .05 sentence is p. 144 (Jeff, Google Books); the .08 sentence's chapter page was never verified and is not derived — the -94 offset predicts 145 against an actual 144.
- 2026-07-19: **supersedes the previous entry's T3 recommendation.** Jeff located the cutoff sentence in the 1993 chapter via Google Books: **chapter p. 144** carries "a value of the RMSEA of about 0.05 or less would indicate a close fit…". That removes the only reason to change the shipped citation — the 1993 now has a verified page anchor of its own — so **T3 keeps `R/ssm_ci_oop.R:415` citing Browne & Cudeck (1993)** and no package code changes. The 1992 article remains the shelved, re-readable companion: its p. 239 carries **both** cutoff sentences, now bracketed by the printed 238 and 240 page markers rather than inferred from one side. Two things the source note must record: the **.08 sentence's chapter page is still unverified** (~5 lines after the .05 sentence in the article, so probably also 144 — not banked), and the derived-offset trap is now demonstrated rather than hypothetical: article p. 239 − 94 predicts chapter p. 145, but the real page is **144**, so the arithmetic would have shipped a wrong number that read like a checked one.
- 2026-07-19: shelf investigation done ahead of T1, so it need not be redone. **(a) `cudeck1983.pdf` is Cudeck & Browne (1983), *Cross-validation of covariance structures*, MBR 18(2) 147-167 — a different paper from "Alternative ways of assessing model fit", with the author order reversed; it owes NO page, since the repo neither computes nor asserts a cross-validation index (Grassi's Appendix A merely prints an ECVI). (b) `browne1982_pp95-96.pdf` now shelves exactly the two pages T5 needs; the rest of Browne (1982) is not available and is not required. (c) Browne & Cudeck: the shelf holds `browne1992a.pdf` = the **1992** SMR 21(2) 230-258 article, NOT the 1993 chapter. The 1993 chapter is real — Bollen & Long ch. 6, pp. 136-162 — confirmed from Hu & Bentler (1999)'s reference list and by Jeff's Google Books check (chapter title/authors/pages match; visible pages appear identical to the article, but the preview is partial so equivalence is a spot-check, never established in full). **The RMSEA cutoffs the package ships are verified at 1992 p. 239**, quoted verbatim, by two channels (pdftotext extraction + Jeff's read). No verified page anchor exists in the 136-162 range, and deriving one by the -94 offset is rejected: reprints are re-typeset and a derived number would read like a checked one. **Recommendation for T3: correct `R/ssm_ci_oop.R:415` from 1993 to 1992** so the shipped citation names the edition we can open and anchor; record the chapter and the partial equivalence on the page, but let nothing in the repo depend on it. Note the authors' own hedge — the cutoffs are "based on subjective judgment... cannot be regarded as infallible or correct" — which the vignettes should reflect rather than presenting .05/.08 as settled.
- 2026-07-19: shelf state moved after this file was written, logged here rather than amending plan-owned Scope/Tasks (that is `/milestone-plan`'s to change at T1's re-scope gate). Two arrivals M41's Scope does not know about: **Browne (1982) pp. 95–96 are now on the shelf as four page images** (`browne1982_p95a/b`, `p96a/b.png`) — not the whole source, but exactly the pages T5's communality-CI derivation needs, so T5 may be partly workable already; and **`cudeck1983.pdf`** appeared, unrequested and unassessed — T1 must decide whether the repo relies on it at all before it earns a page. Found at M40's review by the diff-bug lens, which caught M40 asserting Browne 1982 was absent minutes after three of the four images had landed.
- 2026-07-19: created by /milestone-plan alongside M40, which carries the two sources already on the shelf. Not workable until M40 is done and the seven PDFs are shelved; T1 gates on their actual presence rather than assuming it, and expects to re-scope at its own plan gate once the real citekeys are known.

## Decisions

- **M41-D1 (2026-07-19): channel 2 is Claude reading rendered page images, not a human attestation.**
  M40's two channels were `pdftotext -layout` plus Jeff's own read. Here the
  second channel is a visual read of `pdftoppm`-rendered page images by the
  implementing session. It is genuinely independent of the text layer — the
  OCR artifact "RMSEAgreater" in the pdftotext output is exactly the class of
  error it catches — but it is **not** a human attestation, and no
  `Extraction:` status on either page may imply one. M40's lesson stands: a
  status is scoped to what each channel actually saw.

## Review

Reviewed 2026-07-19. PR #67.

### Acceptance-criteria evidence

- **AC1** — `cairn/references/browne1992a.md` exists; all template sections
  present (`Provenance`/`Citation`/`Role` blocks + `## Extracted values`,
  `## Traces to`, `## Open questions`), checked by grep. The 1992 SMR article
  is the anchor edition; the 1993 chapter appears as "also appears as chapter
  6, pp. 136–162", never as a reprint. The diff-bug reviewer independently
  confirmed the masthead ("Vol. 21, No. 2, November 1992 230-258") and that
  pp. 136–162 is corroborated by Hu & Bentler's own reference list.
- **AC2** — the p. 239 passage is quoted verbatim and machine-verified: a
  normalized substring comparison against `pdftotext -f 10 -l 10` output
  MATCHED, with the only normalizations being the two artifacts the page
  itself documents. All three thresholds present (0.05 / 0.08 / greater than
  0.1). Running-head anchor confirmed (`Browne, Cudeck / ALTERNATIVES IN
  ASSESSMENT 239`). Chapter p. 144 recorded with its Google Books provenance;
  the .08/0.1 chapter page recorded as unverified and not derived; the −94
  offset trap stated (predicts 145, real 144 — arithmetic independently
  re-checked: 230−136 = 94). Both wording departures recorded (`0.1` vs `.10`;
  "would not want to employ" vs "fits poorly").
- **AC3** — `cairn/references/hu1999.md` exists with all template sections.
  All three abstract quotes machine-verified MATCH against `pdftotext -f 1
  -l 1`. Every cutoff the repo cites is quoted verbatim and anchored to p. 1;
  pagination basis confirmed at PDF p. 28 (running head reads 28).
- **AC4** — `Extraction:` is one physical line on both pages (lines 15 and 8;
  the following line is empty in each), carries a verification verb, a date,
  and an explicit disclaimer that channel 2 is not a human attestation
  (M41-D1). Every `Traces to` anchor was opened and checked; three were
  corrected during review (see findings).
- **AC5** — `INDEX.md` carries one line per page with the filename as link
  text (all four entries checked). The dated `cheung2002.pdf` no-page-owed
  note is present with its reason. `cairn_validate` exit 0: 15/15 PASS
  including `references index<->disk`; `references staleness` OK, no WARN.
- **AC6** — `git diff --stat master..HEAD -- devel/` empty; 0 files outside
  `cairn/` in the branch diff (5 files, all under `cairn/`); tail bytes of
  both pages end `— observed 2026-07-19.` with no leaked scaffolding (M34).

### Consistency gate

- Universal: `cairn_validate` exit 0, 15/15 PASS (re-run after fixes). Two
  advisory WARNs, both pre-existing and untouched by this milestone —
  `record density` on LESSONS.md and 47 `work-log format` wraps, all in M7.
- `cairn_impact`: skipped, no principle changed (`Principles touched: —`).
- Toolchain (`consistency-gate` slot): `devtools::check(args="--no-manual")`
  → **Status: OK**, 0 errors / 0 warnings / 0 notes. No roxygen and no R file
  changed, so `document()` no-diff and the generated-file rules are trivially
  satisfied and the PDF-manual surface is untouched. No NEWS entry: the
  milestone has no user-visible change. `cairn/` is already in
  `.Rbuildignore` (`^cairn$`), so the new files add no check NOTE.

### Independent review — three lenses + scorer

- **[O] diff-bug (Opus):** 4 findings. Independently verified both pages'
  quotes character-exact, re-rendered p. 239 to confirm both between-channel
  discrepancy claims are real, and confirmed the offset arithmetic.
- **[S] blame-history (Sonnet):** no findings. Confirmed none of M40's three
  review defects recurred, and that the `INDEX.md` rewrite dropped no M40
  caveat in substance.
- **[S] prior-PR-comments (Sonnet):** no prior-PR evidence — scanned all 44
  merged PRs; only #13 and #18 ever carried comments, neither touching any
  file in this diff. Confirms the standing LESSONS calibration (M33) rather
  than assuming it.

**Actioned (scored ≥ 80):**

- **F1 (95) — FIXED.** `browne1992a.md` claimed eqs. 13/14/15 were not
  transcribed "because nothing in the repo computes them: RMSEA reaches the
  package through `lavaan` and `CircE`". False: `R/cpm_fit.R:1049` computes
  eq. 13 (`sqrt(max(Fhat/df - 1/n, 0))`) and `R/cpm_fit.R:1011-1028`
  computes eq. 14 by noncentral chi-square inversion, neither carrying any
  attribution. Fixed by banking eqs. 13 and 14 verbatim (eq. 14 read from
  p. 240, running head confirmed), splitting `Role` into a wording path and
  an estimation path, adding the estimation anchors to `Traces to`, and
  narrowing "Not extracted" to eq. 15 only — which grep confirms is genuinely
  not computed.
- **F2 (85) — FIXED.** The citekey-trap block asserted "Both pages say so, in
  both directions" when `browne1992.md` does not exist. Rewritten to state
  that it is owed by M42, which carries the reciprocal warning, with an
  `— observed` stamp.

**Logged, below the 80 threshold, not actioned as such (IP3):**

- **F3 (78)** — `hu1999.md` anchored `R/cpm_oop.R:44` as printing CFI and TLI;
  line 44 prints CFI only. **Fixed anyway**: the scorer's own justification
  calls it "a real, verifiable citation inaccuracy", confirmed here by
  command, and the correction is one line. Recorded as a deliberate departure
  from the threshold rather than a silent one.
- **F4 (63)** — undated repo-state claims in body prose (as distinct from the
  correctly dated Open questions). Not actioned as a class; the scorer noted
  the sibling M40 pages do not uniformly date body prose either, so applying
  it here alone would create an inconsistency. The one instance that was
  materially false is F1 and is fixed and dated. Left for a future
  convention-wide pass if the repo wants one.
