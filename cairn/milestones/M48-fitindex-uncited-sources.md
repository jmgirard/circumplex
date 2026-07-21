# M48: Fit-index and uncited shelf sources (browne1993 twin + strack2013 prospect)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m48-fitindex-uncited-sources / https://github.com/jmgirard/circumplex/pull/74

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

- [x] `browne1992a.md` carries a cross-reference resolving the 1992-article /
      1993-chapter twin: it names that the repo's user-facing citation is
      "Browne & Cudeck (1993)" at `vignettes/evaluating-circumplex-structure.Rmd:93`
      and `:613` and `tests/testthat/_snaps/ci_accuracy.md:38`, states the 1993
      chapter is the Bollen & Long reprint of the same RMSEA cutoffs, and anchors
      the chapter's cutoff page(s) from `sources/browne1993.pdf`.
- [x] The chapter page anchors are extraction-verified against
      `sources/browne1993.pdf` (the .05 / .08 / .10 RMSEA cutoffs appear at the
      cited chapter pages, matching `browne1992a.md`'s banked 1992 values),
      recorded on the extraction status with a dated re-check.
- [x] strack2013 is captured as a `candidate` ROADMAP row (search-first sweep
      first, D-042) characterizing the source (CFA circumplex-axes reliability;
      RANDALL-adjacent) and a seedable prospect; no per-source page authored (D-024).
- [x] `cairn_validate` (references check + full run) is green; `INDEX.md`
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
- 2026-07-21: /milestone-review — PR #74 (draft); all 4 ACs verified with fresh evidence. Review fan-out: F1 (diff-bug, scored 90) — the T1 re-check line mis-anchored the m=2 worked-example row to chapter p. 144; it is Table 6.1 on chapter p. 152 (PDF p. 17). Fixed on branch (split the anchor on browne1992a.md; only the cutoffs are on p. 144). Other two lenses clean. Supersedes the T1 work-log line's conflated single-page phrasing.

## Decisions

## Review

_Reviewed 2026-07-21 (/milestone-review). PR #74. Docs/tracking-only — diff touches
only `cairn/` files (`.Rbuildignore`d `^cairn$`); no `R/`, `src/`, roxygen, NEWS,
README, or pkgdown surface._

**Acceptance-criterion evidence (fresh):**

- **AC1 (twin cross-reference) — PASS.** `browne1992a.md` Citation section (l. 24–36)
  affirmatively states the user-facing citation is "Browne & Cudeck (1993)" and names
  all three sites — `vignettes/evaluating-circumplex-structure.Rmd:93` & `:613` and
  `tests/testthat/_snaps/ci_accuracy.md:38` — says the 1993 Bollen & Long chapter carries
  the same cutoffs, and anchors them to chapter p. 144. All three cite sites re-confirmed
  present in the repo by grep (vignette :93 benchmark prose, :613 reference-list entry,
  snapshot :38). The snapshot site is also added to Traces>Wording (l. 165).
- **AC2 (extraction re-verified) — PASS (after F1 fix).** Provenance extraction status
  carries a dated chapter-twin re-check (l. 17, "observed 2026-07-21"), re-verified against
  `sources/browne1993.pdf` by two channels (`pdftotext -layout` + rendered page image),
  word/digit-for-digit, no channel divergence. Split page anchors (corrected per F1): the
  .05/.08/.10 cutoffs on chapter **p. 144** (PDF p. 9, bracketed by the 143/145 running
  heads); the m=2 worked-example row in Table 6.1 on chapter **p. 152** (PDF p. 17, bracketed
  by the 151/153 running heads). Both re-confirmed live at review by re-extracting each page.
- **AC3 (strack2013 prospect row) — PASS.** ROADMAP l. 43 carries the candidate row
  (search-first sweep recorded clean); characterizes the source (Strack et al. 2013,
  CFA circumplex-axes reliability, RANDALL-adjacent) and its seedable use; no
  `strack2013.md` page exists and it holds no `INDEX.md` line (D-024).
- **AC4 (validate green) — PASS.** `cairn_validate` exit 0 — all CHECKs PASS
  (weight caps, references index<->disk, coverage complete, mirror agreement,
  roadmap<->disk orphans), record density OK. 47 advisory warnings, all pre-existing
  M7 work-log-format rows (unchanged by this milestone).

**Consistency gate:** universal cairn-file checks green (`cairn_validate` above; no
principle changed → `cairn_impact` skipped). Toolchain (r-package) `consistency-gate`
slot is a clean no-op — the diff is entirely inside the `.Rbuildignore`d `cairn/` tree,
so `document()` no-diff, generated-file, README/pkgdown, NEWS, and `.Rbuildignore`
checks have no changed surface; the draft-PR CI (`R CMD check`) runs as backstop.

**Independent fresh-context review (3 lenses + scorer):**

- **[O] diff-bug (Opus) — 1 finding, F1 below.**
- **[S] blame-history (Sonnet) — no findings.** M48's re-check cites the same page
  (144 for the cutoffs) and the same digit-for-digit values as the 2026-07-20 resolution;
  no reversed decision, no re-opened Open-question, citekey trap intact, D-023/D-024 honored.
- **[S] prior-review (Sonnet) — no regression.** Prior-review evidence exists on
  browne1992a.md (M40-D2 "extraction status claims only what ran", M41-D1 "channel-2 is
  not human attestation", M47 F1 "don't drop the M41-D1 caveat"); the diff preserves the
  M41-D1 caveat verbatim and reproduces none of those defects.

- **F1 (diff-bug, scored 90 — CONFIRMED, actioned: fixed on branch).** The new
  "Chapter-twin re-check" line anchored *both* the RMSEA cutoffs *and* the m=2
  worked-example row to "chapter p. 144 (PDF p. 9)". Verified against the source: the
  cutoff passage is on chapter p. 144 (PDF p. 9), but the worked-example row (Table 6.1)
  is on chapter **p. 152** (PDF p. 17) — PDF p. 9 carries none of those numerals; PDF p. 17
  running-head-confirmed printed 152 (flanked by p. 151/p. 153). A false page anchor on an
  extraction-status line defeats the page's page-anchored-extraction discipline (same family
  as M40-D2). **Fix:** split the anchor on `browne1992a.md` l. 17 (cutoffs → p. 144;
  Table 6.1 → p. 152) and correct the AC2 evidence line above.

- **Below-threshold / considered-and-dropped:** none scored 60–79. The diff-bug lens noted
  in passing that the milestone's plan-owned Scope (l. 22) and AC3 (l. 43) cite "D-042"
  (a cairn *plugin* decision, not a circumplex-repo D-entry; the local prospect-capture
  decisions are D-023/D-024, which the shipped ROADMAP row cites correctly) — pre-existing
  plan-owned text on unmodified lines, out of scope for this diff (false-positive taxonomy:
  "a complaint about an unmodified line"); left for a future plan-owned amendment, not
  patched review-side.
