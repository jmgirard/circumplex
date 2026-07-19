# M40: Source notes for the two shelved primary sources

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Retire the pre-migration deferral in `cairn/references/INDEX.md` by authoring
committed source notes for the two primary sources already on the shelf,
establishing the page conventions the remaining seven follow.

## Scope

**In:** Source notes `cairn/references/grassi2010CircE.md` and
`cairn/references/zimmermann2017Description.md`, authored from
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

- [ ] `cairn/references/grassi2010CircE.md` exists, carries every section the
      source-note template defines, and every extracted value carries a page
      or table anchor.
- [ ] `cairn/references/zimmermann2017Description.md` likewise.
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

- [ ] T1. Author `grassi2010CircE.md`: extract values from the provenance
      headers in `tests/testthat/helper-cpm-oracles.R` and
      `tests/testthat/test-cpm_oracles.R` against the shelf PDF, preserving
      the M7 A3/A4/A5 corrections (unconstrained m = 1 fit measures come from
      Appendix A pp. 70–71, not Table 3 p. 60; the Table 2/3 column is ρ̂, the
      communality index, not ζ; the symmetry belongs on ln v_ii). Record
      Appendix A's own variable order explicitly — flat row order invited a
      false mismatch at M7 T3.
- [ ] T2. Author `zimmermann2017Description.md` from
      `devel/m4-zw-transcription.md` against the shelf PDF, carrying the Eq.
      A7 confirmation (√2 radicand **and** leading ½, both confirmed on the
      page 2026-07-19) and the Table 4 note that OCPD's withheld a/δ CIs
      track its low Prob (.130), not its R².
- [ ] T3. Write both Provenance blocks; verify each `Extraction:` status is
      one physical line.
- [ ] T4. Rewrite `INDEX.md` — drop the deferral comment, add one line per
      committed page.
- [ ] T5. Mutation-prove the references check, then run `cairn_validate`
      clean.
- [ ] T6. Tail-byte check every written page; confirm `git diff devel/` is
      empty and no package file is touched.

## Work log

- 2026-07-19: created by /milestone-plan. Scope split at the plan gate — the nine owed source notes are three different jobs (shelved+verified / transcribed-but-unshelved / cold reads), and only this group is workable today; M41 carries the other seven. Jeff's plan-gate decisions: leave the `devel/` transcriptions byte-untouched while M7's open work log cites them, and give Browne 1992 a full model specification in M41 rather than a reliance-scoped extract.

## Decisions

## Review
