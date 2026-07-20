<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M44: LESSONS.md consolidation and retirement pass

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m44-lessons-consolidation`

## Goal

Bring `cairn/LESSONS.md` back under both weight axes with durable headroom,
without losing a live lesson.

## Scope

**In:**
- Consolidate thematic families into single multi-case lines in the style the
  file already uses at `:12` (five cases (a)–(e) on one line): the
  "green because it never looked" family (`:29` vdiffr auto-skip, `:39` the
  un-ablated fix, `:43` the blind snapshot suite, `:44` `--no-manual`, `:45`
  M39's own "the third") and the render-and-inspect family (`:34`, `:40`,
  `:42`).
- Retire lessons meeting D-051's two criteria — **enforcement** (a test now
  *fails* on the mistake the lesson warns about) or **ownership** (another
  tracking file's slot owns the content) — trimming a partly-covered lesson to
  its uncovered remainder rather than dropping it whole.
- Split `:33`, which carries two unrelated lessons on one line (occasions
  ordering under a discrete ggplot2 scale, and the prior-PR-comments review
  lens being a permanent no-op in this repo).
- Correct the file's own stale cap header (`:7-8` states `<17,000 chars`; the
  real advisory threshold is 20,500).
- Target: **≤42 total lines and ≤19,000 chars**, chosen for headroom so the
  next several milestones capture lessons without re-triggering this pass.
  The cap counts whole-file lines, so the 11-line header is budget too — the
  8-line preamble is a legitimate source of savings once AC4 stops it
  duplicating thresholds.

**Out:**
- Compressing the exact-formula lines — M20's ulp window (`:16`), M26's
  `%%`-vs-`modu` (`:22`), M27/M33's unwrap expressions (`:32`), M31's
  coord-side pinning (`:28`). These are statistical-correctness records and
  precision outranks a density advisory; they stay byte-identical. If the
  targets cannot be met without them, the target moves, not the formulas.
- Any committed package change. Mutation checks under T3 break a guarded line
  only to observe the test fail, then revert; nothing under `R/`, `src/`,
  `tests/`, `man/`, or `vignettes/` reaches the commit.
- Changing cairn's thresholds or `cairn_validate` — plugin-side, not this
  repo's to edit.
- Retiring a lesson that is merely disputed rather than redundant. D-051:
  retirement is not correction; a lesson proven *false* is corrected in place.

## Acceptance criteria

- [ ] `cairn_validate` reports `weight caps` PASS with no `record density`
      WARN for `LESSONS.md`; fresh script output shows **≤42 total lines** —
      the metric the cap actually measures, `LINE_CAPS` counting whole-file
      lines and FAILing at ≥50 — **and ≤19,000 chars**.
- [ ] Every retirement names its D-051 criterion, and each enforcement-based
      retirement carries **mutation evidence** in the work log — the guarded
      line broken, the named test observed FAILING, reverted. The pass leaves
      no package residue: `git diff` shows no committed change under `R/`,
      `src/`, `tests/`, `man/`, or `vignettes/`, and `devtools::test()` is
      clean on the final tree.
- [ ] The four exact-formula lines survive byte-identical, verified by grep
      against the pre-pass file rather than by eye.
- [ ] The header states both caps by naming `cairn_validate`'s `weight caps`
      and `record density` checks, duplicating no numeric threshold.
- [ ] A conservation ledger in this file maps **every one of the 38 starting
      lesson items** to exactly one disposition: kept / merged-into / retired
      (criterion named) / trimmed-to-remainder.
- [ ] The graduated-lessons list is drafted for the archive summary, per
      D-051's "a retired lesson leaves no line behind — the retiring
      milestone's archive summary names what it graduated".

## Coverage

- AC1 → T4, T5, T6
- AC2 → T2, T3
- AC3 → T7
- AC4 → T7
- AC5 → T1, T6
- AC6 → T7

## Tasks

- [x] **T1** — Inventory: build the conservation-ledger skeleton in this file,
      all 38 lessons of `cairn/LESSONS.md` with a one-phrase topic tag. No
      edit to `LESSONS.md` yet.
- [ ] **T2** — Retirement audit: apply D-051's two criteria to each item. For
      every enforcement claim, name the specific test that would have to fail.
      Produce the shortlist; mark partly-covered lessons and what their
      uncovered remainder is.
- [ ] **T3** — Mutation-verify the T2 shortlist: break each guarded line, run
      the named test, record the observed failure, revert. Confirm
      `git status` clean afterward. An enforcement claim that does not
      reproduce a failure is struck from the shortlist and the lesson stays.
- [ ] **T4** — Consolidate the "green because it never looked" family
      (`:29`, `:39`, `:43`, `:44`, `:45`) into one multi-case line, preserving
      each case's distinguishing mechanism — the cases differ in *why* the run
      never looked, which is the transferable part.
- [ ] **T5** — Consolidate the render-and-inspect family (`:34`, `:40`, `:42`)
      the same way.
- [ ] **T6** — Split `:33`'s two lessons; apply the T2/T3 retirements and
      remainder-trims; complete the ledger.
- [ ] **T7** — Correct the header to point at `cairn_validate`'s two checks;
      re-run `cairn_validate` and `devtools::test()`; grep the four formula
      lines against the pre-pass file for byte-identity; draft the
      graduated-lessons list for the archive summary.

## Conservation ledger

<!-- owner: implement. Indices are line numbers in the PRE-PASS LESSONS.md at
     commit 56577d32 (49 lines: 11 header + 38 lessons at :12-:49). Every one
     of the 38 appears exactly once across the groups below — that is what
     makes conservation checkable (AC5). Dispositions firm up at T2/T3/T6. -->

- **Protected — verbatim, no compression (Scope Out):** `:16` M20 pole ulp
  window · `:22` M26 `%%`-vs-`modu` · `:28` M31/M32 coord-side pinning ·
  `:32` M27/M33 unwrap expressions.
- **"Green because it never looked" family → T4:** `:29` vdiffr auto-skip ·
  `:39` un-ablated fix · `:43` blind snapshot suite · `:44` `--no-manual` +
  the guard that skipped under check · `:45` M39's plate (calls itself "the
  third").
- **Render-and-inspect family → T5:** `:34` M33 origin · `:40` M37 arrow
  under marker · `:42` M38 label width vs break spacing.
- **Split → T6:** `:33` M33 = occasions ordering + the prior-PR-lens no-op
  (two unrelated lessons on one line).
- **Retirement audit → T2/T3:** `:13` `:15` `:29` `:30` `:31` `:36` `:38`
  `:41` `:44` `:47` — the candidates with a plausible enforcing test or an
  owning slot elsewhere. Overlap with T4/T5 is deliberate: a line can be both
  consolidated and trimmed.
- **Kept as-is unless T2 finds otherwise:** `:12` `:14` `:17` `:18` `:19`
  `:20` `:21` `:23` `:24` `:25` `:26` `:27` `:35` `:37` `:46` `:48` `:49`.

## Work log

- 2026-07-19: created by /milestone-plan, promoted from the ROADMAP candidate added at M43's post-merge hygiene. Gate choices (Jeff): deliberate headroom (≤42 items) over minimum-viable; consolidation-first with retirement only where D-051 genuinely holds; header points at the validator rather than duplicating a threshold that moves; enforcement retirements **mutation-proven**, not inspected — D-051's discriminating word is *fails*, and `LESSONS.md:12` is itself the lesson that says prove by mutation, never by eye.
- 2026-07-19: T1 done. Ledger indexes the pre-pass file at `56577d32`; all 38 lessons assigned, verified programmatically (38 distinct indices, no gap, no extra). **AMENDED (gated) at Jeff's mini-gate:** the `weight caps` check measures whole-file LINES (`LINE_CAPS` in `cairn_scripts.py:44`, FAIL at >=50), not lesson items — the file is 49 lines = 11 header + 38 lessons, and the plan had carried the WARN's "49" in as "49 items". AC1 retargeted to **<=42 total lines** and AC5 to **38 starting lesson items**; Scope notes the 11-line header is budget too. Jeff chose "<=42 lines, header counts", preserving the plan gate's headroom intent.

## Decisions

## Review
