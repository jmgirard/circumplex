# M41: Source notes for the fit-index benchmark pair

- **Status:** planned
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] `cairn/references/browne1992a.md` exists carrying every template
      section, with each extracted value page- or table-anchored; the 1992 SMR
      article is its anchor edition and the 1993 chapter is named neutrally
      ("also appears as ch. 6, pp. 136–162"), never as a reprint.
- [ ] Both RMSEA cutoff sentences are quoted verbatim anchored at 1992 p. 239.
      The .05 sentence's chapter anchor is recorded as p. 144 with its Google
      Books provenance; the .08 sentence's chapter page is recorded as
      **unverified and not derived**, with the −94 offset trap stated (the
      arithmetic predicts chapter p. 145; the real page is 144).
- [ ] `cairn/references/hu1999.md` exists carrying every template section,
      with every cutoff the repo cites (SRMR, CFI, TLI) quoted verbatim and
      page-anchored.
- [ ] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names the specific citing lines
      listed in Scope, verified against the files.
- [ ] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40), plus a dated note recording that `sources/cheung2002.pdf` is
      shelved and owes no page; `cairn_validate` reports
      `references index<->disk` PASS with no `references staleness` WARN.
- [ ] `git diff --stat devel/` is empty, no file outside `cairn/` is modified,
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

- [ ] T1. Re-verify `browne1992a.pdf` and `hu1999.pdf` on the shelf and stamp
      the observation — the shelf is live and moved twice during M40, so
      confirm rather than trust this plan's inventory.
- [ ] T2. Author `browne1992a.md`: two-channel read of p. 239, the
      edition-relationship record carried over from the work log, and the
      offset trap.
- [ ] T3. Author `hu1999.md`: two-channel read of the cutoffs the repo cites.
- [ ] T4. Write both pages' `Traces to` sections against the actual citing
      lines, not this plan's list.
- [ ] T5. `INDEX.md` entries + the dated `cheung2002.pdf` no-page-owed note;
      `cairn_validate` clean; tail-byte and untouched-tree checks.

## Work log

- 2026-07-19: **re-sized at the plan gate its own `Out:` clause called for.** The shelf inventory M41's T1 was to perform is done here — all seven sources are present, identified by first-page read, and recorded in the M42/M43 plans alongside this one. Seven notes plus a full CPM specification is three milestones' work against M40's demonstrated throughput of two notes, so M41 narrows to the fit-benchmark pair and keeps its ID because this work log is entirely about the Browne & Cudeck edition question; Browne (1992) + Browne (1982) → M42, Acton & Revelle + Wendt → M43. Two Scope corrections: **Cheung & Rensvold has no shipped reliance** (zero hits across `R/`, `vignettes/`, `tests/`; the ΔCFI offer at `devel/m5-sem-design.md:751-759` was never taken up, and its own caveats exclude the robust estimators `ssm_sem()` defaults to), so it owes no page and the ΔCFI feature question becomes a candidate row; and `browne1982_pp95-96.pdf` has replaced the four page images, which `grassi2010.md:137` still names — M42 corrects that in place.
- 2026-07-19: **Browne & Cudeck edition question CLOSED for T3 — do not reopen without the physical book.** Established by web research: SMR **21(2), Nov 1992** was a special issue edited by Bollen & Long (their own "Tests for Structural Equation Models" is article 1, DOI 10.1177/0049124192021002001), and the 1993 Sage volume is its book version — same editors, same papers, contiguous journal pagination (Bollen & Stine 205-229 immediately precedes Browne & Cudeck 230-258), and a matching TOC order with "Alternative Ways of Assessing Model Fit" sixth. **Whether the chapters are VERBATIM reprints is unresolved and stays unasserted**: the only claim found either way is an anonymous Amazon customer review ("the text is not identical") reaching us through a search-engine paraphrase, which contradicts Jeff's Google Books spot-check that the visible pages look identical; the book's preface, which would state the relationship outright, is omitted from the Google Books preview — observed 2026-07-19. **T3 therefore cites Browne & Cudeck (1992), SMR 21(2) 230-258, anchored p. 239** (both cutoff sentences, bracketed by the printed 238/240 markers, verified by pdftotext + Jeff's read) — the original publication, on the shelf, and re-readable. The chapter is named on the page NEUTRALLY ("also appears as ch. 6, pp. 136-162"), never as "Reprinted in", because verbatim-ness is exactly what we could not establish. Chapter anchor for the .05 sentence is p. 144 (Jeff, Google Books); the .08 sentence's chapter page was never verified and is not derived — the -94 offset predicts 145 against an actual 144.
- 2026-07-19: **supersedes the previous entry's T3 recommendation.** Jeff located the cutoff sentence in the 1993 chapter via Google Books: **chapter p. 144** carries "a value of the RMSEA of about 0.05 or less would indicate a close fit…". That removes the only reason to change the shipped citation — the 1993 now has a verified page anchor of its own — so **T3 keeps `R/ssm_ci_oop.R:415` citing Browne & Cudeck (1993)** and no package code changes. The 1992 article remains the shelved, re-readable companion: its p. 239 carries **both** cutoff sentences, now bracketed by the printed 238 and 240 page markers rather than inferred from one side. Two things the source note must record: the **.08 sentence's chapter page is still unverified** (~5 lines after the .05 sentence in the article, so probably also 144 — not banked), and the derived-offset trap is now demonstrated rather than hypothetical: article p. 239 − 94 predicts chapter p. 145, but the real page is **144**, so the arithmetic would have shipped a wrong number that read like a checked one.
- 2026-07-19: shelf investigation done ahead of T1, so it need not be redone. **(a) `cudeck1983.pdf` is Cudeck & Browne (1983), *Cross-validation of covariance structures*, MBR 18(2) 147-167 — a different paper from "Alternative ways of assessing model fit", with the author order reversed; it owes NO page, since the repo neither computes nor asserts a cross-validation index (Grassi's Appendix A merely prints an ECVI). (b) `browne1982_pp95-96.pdf` now shelves exactly the two pages T5 needs; the rest of Browne (1982) is not available and is not required. (c) Browne & Cudeck: the shelf holds `browne1992a.pdf` = the **1992** SMR 21(2) 230-258 article, NOT the 1993 chapter. The 1993 chapter is real — Bollen & Long ch. 6, pp. 136-162 — confirmed from Hu & Bentler (1999)'s reference list and by Jeff's Google Books check (chapter title/authors/pages match; visible pages appear identical to the article, but the preview is partial so equivalence is a spot-check, never established in full). **The RMSEA cutoffs the package ships are verified at 1992 p. 239**, quoted verbatim, by two channels (pdftotext extraction + Jeff's read). No verified page anchor exists in the 136-162 range, and deriving one by the -94 offset is rejected: reprints are re-typeset and a derived number would read like a checked one. **Recommendation for T3: correct `R/ssm_ci_oop.R:415` from 1993 to 1992** so the shipped citation names the edition we can open and anchor; record the chapter and the partial equivalence on the page, but let nothing in the repo depend on it. Note the authors' own hedge — the cutoffs are "based on subjective judgment... cannot be regarded as infallible or correct" — which the vignettes should reflect rather than presenting .05/.08 as settled.
- 2026-07-19: shelf state moved after this file was written, logged here rather than amending plan-owned Scope/Tasks (that is `/milestone-plan`'s to change at T1's re-scope gate). Two arrivals M41's Scope does not know about: **Browne (1982) pp. 95–96 are now on the shelf as four page images** (`browne1982_p95a/b`, `p96a/b.png`) — not the whole source, but exactly the pages T5's communality-CI derivation needs, so T5 may be partly workable already; and **`cudeck1983.pdf`** appeared, unrequested and unassessed — T1 must decide whether the repo relies on it at all before it earns a page. Found at M40's review by the diff-bug lens, which caught M40 asserting Browne 1982 was absent minutes after three of the four images had landed.
- 2026-07-19: created by /milestone-plan alongside M40, which carries the two sources already on the shelf. Not workable until M40 is done and the seven PDFs are shelved; T1 gates on their actual presence rather than assuming it, and expects to re-scope at its own plan gate once the real citekeys are known.

## Decisions

## Review
