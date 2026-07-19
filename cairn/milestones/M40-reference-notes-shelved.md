# M40: Source notes for the two shelved primary sources

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m40-reference-notes-shelved`

## Goal

Retire the pre-migration deferral in `cairn/references/INDEX.md` by authoring
committed source notes for the two primary sources already on the shelf,
establishing the page conventions the remaining seven follow.

## Scope

**In:** Source notes `cairn/references/grassi2010.md` and
`cairn/references/zimmermann2017.md` (citekeys per M40-D1), authored from
`skills/shared/templates/source-note.md` — extracted values carrying page or
table anchors, a `Traces to` list naming specific files and lines, and a
Provenance block. Both record `Extraction: verified 2026-07-19`, sourced to
Jeff's M7 AC3 attestation (the second independent human re-read of both
sources against their primaries, which found no transcribed value wrong).
`INDEX.md` rewritten: the deferral comment dropped, one line per committed
page. Every claim about the repo's own state carries `— observed YYYY-MM-DD`.

**Out:** The seven remaining source notes → M41. The full Browne (1992) model
specification → M41. Editing, moving, or renaming the `devel/` transcription
files → deferred until M7 archives, because M7's open work log cites all three
by name (plan-gate decision, 2026-07-19); M40 leaves them byte-untouched. Any
package code change — this milestone is docs-only, so the profile's `verify`
slot is not applicable and AC7 fences that claim instead.

## Acceptance criteria

- [ ] `cairn/references/grassi2010.md` exists, carries every section the
      source-note template defines, and every extracted value carries a page
      or table anchor.
- [ ] `cairn/references/zimmermann2017.md` likewise.
- [ ] Both Provenance blocks carry an `Extraction:` status on a **single
      physical line** (a wrapped status silently loses its `— observed` stamp
      to the staleness guard), recording verification against the primary
      source and traceable to M7's AC3 attestation.
- [ ] `cairn/references/INDEX.md` carries exactly one line per committed page
      and no longer claims references live under `devel/`.
- [ ] `cairn_validate` reports `references index<->disk` PASS and no
      `references staleness` WARN for either page — **and the check is proved
      to have teeth by mutation**: delete one INDEX line, observe FAIL,
      restore, observe PASS.
- [ ] The three `devel/` transcription files are byte-identical to their
      pre-milestone state: `git diff --stat devel/` empty.
- [ ] No file outside `cairn/` is modified, and each written page's tail bytes
      are checked for leaked tool-call scaffolding (`tail -6 f | od -c`, M34).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T6

## Tasks

- [x] T1. Author `grassi2010.md` (citekey per M40-D1): extract values from the provenance
      headers in `tests/testthat/helper-cpm-oracles.R` and
      `tests/testthat/test-cpm_oracles.R` against the shelf PDF, preserving
      the M7 A3/A4/A5 corrections (unconstrained m = 1 fit measures come from
      Appendix A pp. 70–71, not Table 3 p. 60; the Table 2/3 column is ρ̂, the
      communality index, not ζ; the symmetry belongs on ln v_ii). Record
      Appendix A's own variable order explicitly — flat row order invited a
      false mismatch at M7 T3.
- [x] T2. Author `zimmermann2017.md` (citekey per M40-D1) from
      `devel/m4-zw-transcription.md` against the shelf PDF, carrying the Eq.
      A7 confirmation (√2 radicand **and** leading ½, both confirmed on the
      page 2026-07-19) and the Table 4 note that OCPD's withheld a/δ CIs
      track its low Prob (.130), not its R².
- [x] T3. Write both Provenance blocks; verify each `Extraction:` status is
      one physical line.
- [x] T4. Rewrite `INDEX.md` — drop the deferral comment, add one line per
      committed page.
- [x] T5. Mutation-prove the references check, then run `cairn_validate`
      clean.
- [x] T6. Tail-byte check every written page; confirm `git diff devel/` is
      empty and no package file is touched.

## Work log

- 2026-07-19: T4–T6 done; status → review. **T5 caught a real defect by mutation before it shipped.** The first `INDEX.md` used `[grassi2010](grassi2010.md)` as its entry, and `cairn_validate`'s `_INDEX_LINE` regex matches the first `[\w./-]+\.md` token after the bullet — so a bare citekey as link text is silently NOT a catalog entry, and BOTH pages read as unindexed while the file looked correct to a human. Entry text must be the filename (`[grassi2010.md](grassi2010.md)`); the trap is now documented in INDEX.md itself. Three mutations then proved the checks have teeth and all restored clean: dropping an INDEX entry FAILs, stripping a Provenance block FAILs, and blanking an `Extraction:` status fires the staleness WARN. Final state: `cairn_validate` 15/15 PASS with only M7's 47 pre-existing work-log-format advisories (unchanged from baseline); `git diff master...HEAD -- devel/` empty; the branch touches `cairn/` only; no leaked tool-call scaffolding in any written page.
- 2026-07-19: T2+T3 done — `cairn/references/zimmermann2017.md`, both Provenance blocks written and each `Extraction:` status confirmed a single physical line (420 and 657 chars). Machine re-check reproduced the Note 3 matrices, both Table 4 rows, Table 4's note, the Study 2 thresholds, the Study 5 CircE indices and IIP-SC parameters, and all eight constants Eq. A6/A7/Eq. 3 derive (no-√2 variant misses all three f_a values). **Table 4's printed note independently confirms M7's Prob-not-R² correction.** Three items are outside a text layer and rest on Jeff's read alone, recorded as such rather than claimed: Figure 1A's octant angles, Figure 5's panel readings, and Eq. A7's √2 radicand + leading ½, which `pdftotext` silently drops — the M40 channel reproduced that artifact rather than resolving it, so the page logs Eq. A7 as still lacking two genuinely independent channels.
- 2026-07-19: T1 done — `cairn/references/grassi2010.md`. The shelf changed mid-session: Jeff added seven PDFs and renamed the two originals to `author+year`, so `browne1992a.pdf` was identified as Browne & Cudeck (1992) SMR 21(2) 230–258 (not a second Browne solo paper) and `acton2002.pdf` as a bonus needing no page — the repo cites Acton & Revelle (2002) only as *others'* citation of prior work, which "consulted in passing owes nothing" excludes. Browne (1982) is still absent. Independent `pdftotext -layout` re-check reproduced EVERY fixture value: all 21 Table 1 correlations + N, the Table 2 model 1a row, the Table 3 fit row, all Appendix A full-precision estimates/SEs/fit measures, all seven communality indices and CIs after re-mapping the reordered block, all seven variance ratios, and the Listing 7 matrix + N. Three records confirmed on the page: A2's reordering note is printed verbatim ("variable names have been reordered to yield increasing polar angles"), A4's column really is ρ̂, and A6's retraction was right — p. 68 prints "Foreign Literature". New fact the fixtures never recorded: Appendix A's fit CIs are **90%** while its communality CIs are **95%**.
- 2026-07-19: created by /milestone-plan. Scope split at the plan gate — the nine owed source notes are three different jobs (shelved+verified / transcribed-but-unshelved / cold reads), and only this group is workable today; M41 carries the other seven. Jeff's plan-gate decisions: leave the `devel/` transcriptions byte-untouched while M7's open work log cites them, and give Browne 1992 a full model specification in M41 rather than a reliance-scoped extract.

## Decisions

- **M40-D1 (2026-07-19): citekeys follow the shelf's `author+year` filenames.** The pages are `grassi2010.md` and `zimmermann2017.md`, superseding the plan's `grassi2010CircE`/`zimmermann2017Description` — Jeff replaced the shelf mid-session with a consistently named nine-file set, and a citekey that disagrees with the file it points at is a permanent trap. Binds M41's seven pages too.
- **M40-D2 (2026-07-19): an `Extraction:` status claims only what actually ran.** These two pages record verification because *both* channels exist — Jeff's M7 AC3 human re-read against the primary source, and an independent M40 `pdftotext -layout` re-check of every recorded value. A page with only one channel says so; a page with neither is `unverified`. Authoring a source note is itself a fresh transcription step, so inheriting an attestation of the *old* record without re-checking would have overclaimed.
- **M40-D3 (2026-07-19): the Browne & Cudeck edition mismatch folds into M41's scope, not a candidate row.** `sources/browne1992a.pdf` is the 1992 *Sociological Methods & Research* 21(2), 230–258 article, while `R/ssm_ci_oop.R:415` cites the 1993 Bollen & Long chapter; resolving it edits package code, which M40 excludes. Noted here because Grassi p. 58 cites the **1992** version too, which is evidence toward correcting the shipped citation rather than hunting the chapter.

## Review
