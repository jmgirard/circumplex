# M41: Source notes for the seven unshelved relied-on sources

- **Status:** planned
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Author committed source notes for the seven relied-on sources not yet on the
shelf, following the page conventions M40 establishes.

## Scope

**In:** Seven source notes. Two have transcriptions already but no shelf copy
and both record a re-read still pending — Acton & Revelle (2004),
`devel/ar2004-transcription.md`, cited in shipped code at
`R/fit_structure.R:722`; and Cheung & Rensvold (2002),
`devel/cr2002-transcription.md`, the ΔCFI cutoffs in the M5 SEM layer. Five
are cold reads with neither transcription nor shelf copy — Browne (1992), the
CPM engine's own model; Browne & Cudeck (1993) and Hu & Bentler (1999), the
fit benchmarks shipped as `@references` at `R/ssm_ci_oop.R:404,415`; Wendt et
al. (2019), four cited claims in `vignettes/sem-based-ssm-analysis.Rmd`; and
Browne (1982), the communality-CI derivation at pp. 95–96.

The Browne (1992) note carries the **full CPM model specification** — enough
to re-derive the estimand in `R/cpm_fit.R` without reopening the paper — not
only the values the repo currently cites (plan-gate decision, 2026-07-19).

The repo's established two-channel transcription protocol applies to every
cold read: a visual page read and an independent `pdftotext -layout`
extraction, diffed on every load-bearing numeral, with between-channel
discrepancies recorded rather than silently resolved.

**Out:** Re-sizing. This milestone is expected to exceed the sizing tripwires
once the PDFs land, and is **re-sized at its own plan gate** — the Browne
(1992) full specification is the most likely split-out, since it is a spec
document in a source note's clothes. Nothing is dropped by that split; the
remainder stays in M41 or its successors. Editing the `devel/` transcriptions
→ after M7 archives (M40's Out clause, same reason). Package code changes.

**Prerequisite (not a milestone dependency):** the seven PDFs on the shelf at
`cairn/references/sources/`, supplied by Jeff under his own Better BibTeX
citekeys, which become the page filenames. T1 gates on their actual presence
rather than assuming it.

## Acceptance criteria

- [ ] T1 records which of the seven PDFs are actually on the shelf, under
      their real citekeys, with an `— observed YYYY-MM-DD` stamp; any absent
      source is re-scoped at the gate rather than given a page written from
      memory (the primary-sources hard stop).
- [ ] A committed source note exists for every shelved source, carrying every
      template section, with each extracted value page- or table-anchored.
- [ ] The Browne (1992) note carries the full CPM model specification, and a
      reader can map each of `R/cpm_fit.R`'s estimated parameters to its
      published counterpart from the page alone.
- [ ] Every page's `Extraction:` status is one physical line and states its
      real standing — verified, partial, or unverified — never a verification
      the milestone did not perform.
- [ ] `INDEX.md` carries one line per committed page; `cairn_validate` reports
      `references index<->disk` PASS with no `references staleness` WARN.
- [ ] `git diff --stat devel/` is empty, no file outside `cairn/` is modified,
      and each written page's tail bytes are checked for leaked tool-call
      scaffolding (M34).

## Coverage

- AC1 → T1
- AC2 → T2, T3, T4, T5, T6, T7
- AC3 → T2
- AC4 → T2, T3, T4, T5, T6, T7
- AC5 → T8
- AC6 → T8

## Tasks

- [ ] T1. Inventory the shelf: record which PDFs arrived and under which
      citekeys; re-scope at the gate if any are missing.
- [ ] T2. Author the Browne (1992) note — full CPM model specification, two
      channels.
- [ ] T3. Author Browne & Cudeck (1993) and Hu & Bentler (1999) — the
      fit-benchmark pair cited together at `R/ssm_ci_oop.R:404,415`.
- [ ] T4. Author Wendt et al. (2019), reconciling against the existing design
      note `devel/m5-wendt-discrepancies.md` (read-only).
- [ ] T5. Author Browne (1982), scoped to the communality-CI derivation.
- [ ] T6. Author Acton & Revelle (2004) from its transcription, verified
      against the newly shelved PDF; carry the two paper-internal
      inconsistencies the transcription already records.
- [ ] T7. Author Cheung & Rensvold (2002) likewise.
- [ ] T8. `INDEX.md` lines; `cairn_validate` clean; tail-byte and
      untouched-tree checks.

## Work log

- 2026-07-19: **Browne & Cudeck edition question CLOSED for T3 — do not reopen without the physical book.** Established by web research: SMR **21(2), Nov 1992** was a special issue edited by Bollen & Long (their own "Tests for Structural Equation Models" is article 1, DOI 10.1177/0049124192021002001), and the 1993 Sage volume is its book version — same editors, same papers, contiguous journal pagination (Bollen & Stine 205-229 immediately precedes Browne & Cudeck 230-258), and a matching TOC order with "Alternative Ways of Assessing Model Fit" sixth. **Whether the chapters are VERBATIM reprints is unresolved and stays unasserted**: the only claim found either way is an anonymous Amazon customer review ("the text is not identical") reaching us through a search-engine paraphrase, which contradicts Jeff's Google Books spot-check that the visible pages look identical; the book's preface, which would state the relationship outright, is omitted from the Google Books preview — observed 2026-07-19. **T3 therefore cites Browne & Cudeck (1992), SMR 21(2) 230-258, anchored p. 239** (both cutoff sentences, bracketed by the printed 238/240 markers, verified by pdftotext + Jeff's read) — the original publication, on the shelf, and re-readable. The chapter is named on the page NEUTRALLY ("also appears as ch. 6, pp. 136-162"), never as "Reprinted in", because verbatim-ness is exactly what we could not establish. Chapter anchor for the .05 sentence is p. 144 (Jeff, Google Books); the .08 sentence's chapter page was never verified and is not derived — the -94 offset predicts 145 against an actual 144.
- 2026-07-19: **supersedes the previous entry's T3 recommendation.** Jeff located the cutoff sentence in the 1993 chapter via Google Books: **chapter p. 144** carries "a value of the RMSEA of about 0.05 or less would indicate a close fit…". That removes the only reason to change the shipped citation — the 1993 now has a verified page anchor of its own — so **T3 keeps `R/ssm_ci_oop.R:415` citing Browne & Cudeck (1993)** and no package code changes. The 1992 article remains the shelved, re-readable companion: its p. 239 carries **both** cutoff sentences, now bracketed by the printed 238 and 240 page markers rather than inferred from one side. Two things the source note must record: the **.08 sentence's chapter page is still unverified** (~5 lines after the .05 sentence in the article, so probably also 144 — not banked), and the derived-offset trap is now demonstrated rather than hypothetical: article p. 239 − 94 predicts chapter p. 145, but the real page is **144**, so the arithmetic would have shipped a wrong number that read like a checked one.
- 2026-07-19: shelf investigation done ahead of T1, so it need not be redone. **(a) `cudeck1983.pdf` is Cudeck & Browne (1983), *Cross-validation of covariance structures*, MBR 18(2) 147-167 — a different paper from "Alternative ways of assessing model fit", with the author order reversed; it owes NO page, since the repo neither computes nor asserts a cross-validation index (Grassi's Appendix A merely prints an ECVI). (b) `browne1982_pp95-96.pdf` now shelves exactly the two pages T5 needs; the rest of Browne (1982) is not available and is not required. (c) Browne & Cudeck: the shelf holds `browne1992a.pdf` = the **1992** SMR 21(2) 230-258 article, NOT the 1993 chapter. The 1993 chapter is real — Bollen & Long ch. 6, pp. 136-162 — confirmed from Hu & Bentler (1999)'s reference list and by Jeff's Google Books check (chapter title/authors/pages match; visible pages appear identical to the article, but the preview is partial so equivalence is a spot-check, never established in full). **The RMSEA cutoffs the package ships are verified at 1992 p. 239**, quoted verbatim, by two channels (pdftotext extraction + Jeff's read). No verified page anchor exists in the 136-162 range, and deriving one by the -94 offset is rejected: reprints are re-typeset and a derived number would read like a checked one. **Recommendation for T3: correct `R/ssm_ci_oop.R:415` from 1993 to 1992** so the shipped citation names the edition we can open and anchor; record the chapter and the partial equivalence on the page, but let nothing in the repo depend on it. Note the authors' own hedge — the cutoffs are "based on subjective judgment... cannot be regarded as infallible or correct" — which the vignettes should reflect rather than presenting .05/.08 as settled.
- 2026-07-19: shelf state moved after this file was written, logged here rather than amending plan-owned Scope/Tasks (that is `/milestone-plan`'s to change at T1's re-scope gate). Two arrivals M41's Scope does not know about: **Browne (1982) pp. 95–96 are now on the shelf as four page images** (`browne1982_p95a/b`, `p96a/b.png`) — not the whole source, but exactly the pages T5's communality-CI derivation needs, so T5 may be partly workable already; and **`cudeck1983.pdf`** appeared, unrequested and unassessed — T1 must decide whether the repo relies on it at all before it earns a page. Found at M40's review by the diff-bug lens, which caught M40 asserting Browne 1982 was absent minutes after three of the four images had landed.
- 2026-07-19: created by /milestone-plan alongside M40, which carries the two sources already on the shelf. Not workable until M40 is done and the seven PDFs are shelved; T1 gates on their actual presence rather than assuming it, and expects to re-scope at its own plan gate once the real citekeys are known.

## Decisions

## Review
