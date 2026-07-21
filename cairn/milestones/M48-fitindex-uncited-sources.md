# M48: Fit-index and uncited shelf sources (browne1993 twin + strack2013 prospect)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m48-fitindex-uncited-sources

## Goal

Account for the two remaining unaccounted shelf sources — cross-reference
browne1993 as the cited chapter-twin of the already-banked `browne1992a.md`, and
capture strack2013 as a deliberate research prospect.

## Scope

**In:** a resolving cross-reference added to `browne1992a.md` (the repo's
user-facing "Browne & Cudeck, 1993" citation is the Bollen & Long chapter
reprint of the same RMSEA cutoffs, with the chapter's cutoff page anchored from
`sources/browne1993.pdf`); a `strack2013` prospect `candidate` ROADMAP row
(supply-push, D-042); `INDEX.md` reconciliation.

**Out:** an own `browne1993.md` page (rejected at the plan gate — twin
duplication of the same values; a cross-reference on the existing page was
chosen); a `strack2013` source note (owed only on reliance, D-024); a new survey
synthesis note (strack2013 is a different cohort from the M46 four and a one-shot
disposition — a bare candidate row suffices); the estimator sources → M47.

## Acceptance criteria

- [ ] `browne1992a.md` carries a cross-reference resolving the 1992-article /
      1993-chapter twin: it names that the repo's user-facing citation is
      "Browne & Cudeck (1993)" at `vignettes/evaluating-circumplex-structure.Rmd:93`
      and `:613` and `tests/testthat/_snaps/ci_accuracy.md:38`, states the 1993
      chapter is the Bollen & Long reprint of the same RMSEA cutoffs, and anchors
      the chapter's cutoff page(s) from `sources/browne1993.pdf`.
- [ ] The chapter page anchors are extraction-verified against
      `sources/browne1993.pdf` (the .05 / .08 / .10 RMSEA cutoffs appear at the
      cited chapter pages, matching `browne1992a.md`'s banked 1992 values),
      recorded on the extraction status with a dated re-check.
- [ ] strack2013 is captured as a `candidate` ROADMAP row (search-first sweep
      first, D-042) characterizing the source (CFA circumplex-axes reliability;
      RANDALL-adjacent) and a seedable prospect; no per-source page authored (D-024).
- [ ] `cairn_validate` (references check + full run) is green; `INDEX.md`
      consistent.

## Coverage

- AC1 → T1, T2
- AC2 → T1
- AC3 → T3
- AC4 → T4

## Tasks

- [x] T1 — Read `sources/browne1993.pdf`; locate the RMSEA .05 / .08 / .10 cutoff
      passage; record the chapter page numbers; two-channel verify against
      `browne1992a.md`'s banked 1992 values (confirm same content).
- [x] T2 — Amend `browne1992a.md` (current knowledge, corrected in place) with
      the twin cross-reference: the 1993-chapter citation, its vignette/snapshot
      cite sites, and the chapter page anchors; update the extraction status.
- [x] T3 — Add the strack2013 prospect `candidate` ROADMAP row (search-first
      sweep first): characterize it and its seedable use; no page (D-024).
- [x] T4 — Run `cairn_validate` + references check; reconcile `INDEX.md`.

## Work log

- 2026-07-20: created by /milestone-plan (split from the 8-source triage; sibling M47).
- 2026-07-21: /milestone-implement → in-progress; branch m48-fitindex-uncited-sources cut from synced master. No question gate (plan decisions locked; no tripwires).
- 2026-07-21: T1 — verified `sources/browne1993.pdf` chapter p. 144 (PDF p. 9, bracketed by the 143/145 running heads) carries the .05/.08/.10 cutoff passage and the m=2 worked-example row, matching `browne1992a.md`'s banked 1992 values word/digit-for-digit; two channels (pdftotext -layout + rendered page image), no divergence.
- 2026-07-21: T2 — amended `browne1992a.md`: affirmative twin cross-reference in Citation (user-facing cite sites incl. `_snaps/ci_accuracy.md:38`, chapter p. 144 anchor), added the snapshot to Traces>Wording, added a dated 2026-07-21 chapter re-check to the extraction status. cairn_validate green (references index<->disk PASS).
- 2026-07-21: T3 — added the strack2013 prospect candidate row to ROADMAP (search-first swept clean; Strack, Jacobs & Grosse Holtforth 2013, *Reliability of Circumplex Axes*, CFA axes-reliability, RANDALL-adjacent); no page (D-024).
- 2026-07-21: T4 — cairn_validate full run green (weight caps PASS, references index<->disk PASS, record density OK); INDEX consistent (no new page authored; strack2013 captured as prospect, not owes-no-page). All tasks done → status review.

## Decisions

## Review
