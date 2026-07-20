<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M44: LESSONS.md consolidation and retirement pass

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m44-lessons-consolidation`

## Goal

Bring `cairn/LESSONS.md` back under both weight axes with durable headroom,
without losing a live lesson.

## Scope

**In:**
- Consolidate thematic families into single multi-case lines, in the style the
  file already uses at `:12`: the "green because it never looked" family
  (`:29` `:39` `:43` `:44` `:45`) and the render-and-inspect family
  (`:34` `:40` `:42`).
- Retire lessons meeting D-051's **enforcement** (a test now *fails* on the
  mistake) or **ownership** (another tracking file's slot owns it) criteria,
  trimming a partly-covered lesson to its uncovered remainder, never whole.
- Split `:33`, which carries two unrelated lessons on one line.
- Correct the file's stale cap header (`:7-8` says `<17,000 chars`; the real
  advisory threshold is 20,500).
- Target **≤42 total lines and ≤19,000 chars**. The cap counts whole-file
  lines, so the header is budget too.

**Out:**
- Compressing the exact-formula lines — M20's ulp window (`:16`), M26's
  `%%`-vs-`modu` (`:22`), M27/M33's unwrap (`:32`), M31's coord-side pinning
  (`:28`). Statistical-correctness records; precision outranks a density
  advisory. If the target cannot be met without them, the target moves.
- Any committed package change. T3's mutation checks revert; nothing under
  `R/`, `src/`, `tests/`, `man/`, or `vignettes/` reaches a commit.
- Changing cairn's thresholds or `cairn_validate` — plugin-side.
- Retiring a lesson merely disputed rather than redundant (D-051: retirement
  is not correction).

## Acceptance criteria

- [ ] `cairn_validate` reports `weight caps` PASS with no `record density`
      WARN for `LESSONS.md`; fresh script output shows **≤42 total lines** —
      the metric the cap actually measures, `LINE_CAPS` counting whole-file
      lines and FAILing at ≥50 — **and ≤19,000 chars**. The work log records
      the measured density tension: at this repo's ~583-char mean lesson the
      char threshold permits ~35 items while the line cap permits 42, so the
      char axis binds first and line headroom overstates real headroom.
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
- [x] **T2** — Retirement audit: apply D-051's two criteria to each item. For
      every enforcement claim, name the specific test that would have to fail.
      Produce the shortlist; mark partly-covered lessons and what their
      uncovered remainder is.
- [x] **T3** — Mutation-verify the T2 shortlist: break each guarded line, run
      the named test, record the observed failure, revert. Confirm
      `git status` clean afterward. An enforcement claim that does not
      reproduce a failure is struck from the shortlist and the lesson stays.
- [x] **T4** — Consolidate the "green because it never looked" family
      (`:29`, `:39`, `:43`, `:44`, `:45`) into one multi-case line, preserving
      each case's distinguishing mechanism — the cases differ in *why* the run
      never looked, which is the transferable part.
- [x] **T5** — Consolidate the render-and-inspect family (`:34`, `:40`, `:42`)
      the same way.
- [x] **T6** — Split `:33`'s two lessons; apply the T2/T3 retirements and
      remainder-trims; complete the ledger.
- [x] **T7** — Correct the header to point at `cairn_validate`'s two checks;
      re-run `cairn_validate` and `devtools::test()`; grep the four formula
      lines against the pre-pass file for byte-identity; draft the
      graduated-lessons list for the archive summary.

## Conservation ledger

<!-- owner: implement. Indices are line numbers in the PRE-PASS LESSONS.md at
     commit 56577d32 (49 lines: 11 header + 38 lessons at :12-:49). All 38
     appear exactly once below — that is what makes conservation checkable
     (AC5); verified programmatically at T1 and re-checked at T7. -->

- **Protected, byte-identical (Scope Out; AC3 verified):** `:16` `:22` `:28` `:32`.
- **Merged → "green result is not coverage" line:** `:29` (vdiffr half) `:39`
  `:43` `:44` (remainder) `:45` (tail). `:45` survives separately for its
  `rot`-viewport grid technique, which is not family material.
- **Merged → "render and LOOK at it" line:** `:34` `:42` `:40` (tail). `:40`
  survives separately for its `add_group()` zeroGrob mechanism.
- **Merged → single M29 line:** `:25` `:26` (determinism + correctness).
- **Split into two lines:** `:33` → occasions ordering | prior-PR-lens no-op.
- **Retired — graduated (AC6):** (1) `:44` Greek-reaches-Rd half,
  **enforcement**, mutation-proven at T3 (`test-rd-latex-safe.R` failed
  `octants.Rd:5: θ` on a planted theta; reverted). (2) `:29` Rplots/.Rbuildignore
  clause, **ownership** — `PROFILE.md`'s consistency-gate owns it,
  `^Rplots\.pdf$` present. (3) `:48` dated-observation + re-verification
  doctrine, **ownership** — tracking-rules owns it; the repo-specific
  Acrobat-OCR/`pdfinfo` fact kept as the remainder.
- **Audited, NOT retired:** `:30` `:31` `:38` `:41` `:47` — each teaches a
  judgment a *future* author must make about *new* code, which D-051 excludes
  ("a guard in the same area is not enforcement when the lesson teaches the
  judgment that guard does not make"); `:41` says the suite is blind to it;
  `:36` has no guard at all.
- **Ownership retirement possible, out of scope:** `:13` `:15` — toolchain
  meta-knowledge `PROFILE.md`'s `test-doctrine` could own; expands scope and
  trades budget for a 120-line-capped file. Follow-up candidate; both kept.
- **Kept unchanged:** `:12`* `:14` `:17` `:18` `:19` `:20` `:21` `:23` `:24`
  `:27` `:35` `:37` `:46` `:49`. (*`:12` kept in substance, compressed per the
  AC1 amendment.)

## Work log

- 2026-07-19: created by /milestone-plan, promoted from the ROADMAP candidate added at M43's post-merge hygiene. Gate choices (Jeff): deliberate headroom (≤42 items) over minimum-viable; consolidation-first with retirement only where D-051 genuinely holds; header points at the validator rather than duplicating a threshold that moves; enforcement retirements **mutation-proven**, not inspected — D-051's discriminating word is *fails*, and `LESSONS.md:12` is itself the lesson that says prove by mutation, never by eye.
- 2026-07-19: T1 done. Ledger indexes the pre-pass file at `56577d32`; all 38 lessons assigned, verified programmatically (38 distinct indices, no gap, no extra). **AMENDED (gated) at Jeff's mini-gate:** the `weight caps` check measures whole-file LINES (`LINE_CAPS` in `cairn_scripts.py:44`, FAIL at >=50), not lesson items — the file is 49 lines = 11 header + 38 lessons, and the plan had carried the WARN's "49" in as "49 items". AC1 retargeted to **<=42 total lines** and AC5 to **38 starting lesson items**; Scope notes the 11-line header is budget too. Jeff chose "<=42 lines, header counts", preserving the plan gate's headroom intent.

- 2026-07-19: T2 done. **Retirement yields one line, not a haul** — and the finding that produced it is D-051's own carve-out: "a guard in the same area is not enforcement when the lesson teaches the judgment that guard does not make." `:30` `:31` `:38` `:47` all have live tests in their area, and all teach a judgment a future author must make about NEW code, so a test failing when today's guard is deleted enforces nothing; `:41` states outright that neither path is reachable through the package API; `:36` has no guard at all. Only `:44` (M7) clears the bar, and only its Greek-reaches-Rd half — the "a guard that skips under `R CMD check` is not a guard" half is unenforced and is T4 family material. `:13`/`:15` could retire on OWNERSHIP into `PROFILE.md`'s `test-doctrine` slot (D-051 permits moving content there), but that expands scope and trades this file's budget for a 120-line-capped one; left as a follow-up candidate. Consequence for the target: consolidation plus the header trim must carry it, and the arithmetic closes at 41 lines (49 - 3 T4 - 2 T5 + 1 split - 4 header).
- 2026-07-19: T3 done. Mutation evidence for the single enforcement retirement: planted U+03B8 in `man/octants.Rd` (generated file, probe only), ran `test-rd-latex-safe.R` under `NOT_CRAN=true`, guard **FAILED** with `octants.Rd:5: θ` against a clean-tree baseline of 0 failed / 2 passed; `git checkout -- man/octants.Rd` reverted and `git status man/` is clean. The failure names the exact mistake the lesson warns about, so the enforcement claim holds rather than merely plausibly holding. No package file is committed by this task.

- 2026-07-19: T4 done. Five family sources (`:29` `:39` `:43` `:44` `:45`) → two lines: one merged lesson carrying five labelled cases — skipped by FLAG, skipped by PATH, AUTO-skipped comparison, baselines that never exercised the change, probe that couldn't attribute — plus M39's structural-fence shape, and one surviving line for M39's `rot` viewport technique, which is a grid mechanism rather than family material and would have been destroyed by a naive 5→1 merge. `:39` (M36 ablation) was folded IN rather than kept out: "it works is not it is the mechanism" is the same failure — a green signal that never discriminated — applied to attribution. One clause retired on **ownership**, not dropped: `:29`'s `Rplots.pdf`→`.Rbuildignore` housekeeping is owned by `PROFILE.md`'s consistency-gate slot ("New top-level files have `.Rbuildignore` entries"), and `^Rplots\.pdf$` is present. 49→46 lines, 20,972→20,554 chars.

- 2026-07-19: T5 done. Render-and-inspect family `:34` `:40` `:42` → 2 lines, not 1: `:40`'s `add_group()` mechanism (a custom discrete aesthetic silently zeroGrob-ing a layer) is a distinct defect, not render-and-inspect material, so only its arrow-under-marker tail folded in. Saved 1 line.
- 2026-07-19: T6 done. `:33` split into its two unrelated lessons (occasions ordering under a discrete scale; the prior-PR lens being a no-op here) — +1 line, the one place this pass deliberately grew the file. Two further merges to fund it: M29's two `ssm_ci_accuracy()` validation lines merged (determinism + correctness, both disciplines kept), and M40/M42 trimmed to its uncovered remainder on OWNERSHIP grounds — tracking-rules now owns the dated-observation and re-verification doctrine it taught, leaving the repo-specific Acrobat-OCR/`pdfinfo` fact, which no other slot carries.
- 2026-07-19: T7 done. Header rewritten to name `cairn_validate`'s two checks and restate no threshold (AC4). **Final: 41 lines / 34 lessons / 18,994 chars**, against targets of ≤42 and ≤19,000 — `weight caps` PASS, `record density` OK. AC3 verified programmatically against the pre-pass snapshot: all four protected formula lines byte-identical (`:16` 578 chars, `:22` 433, `:28` 869, `:32` 940). **Density tension recorded for whoever plans the next pass:** at this repo's ~583-char mean lesson, the 20,500 char threshold permits ~35 lessons while the 50-line cap permits ~42 — the char axis binds first, so counting free LINES overstates real headroom. Jeff held the ≤19,000 target at the mini-gate rather than settling for merely clearing the advisory; it was met without cutting any substantive per-milestone lesson, entirely from the two consolidated multi-case lines plus the header.

- 2026-07-19: status in-progress→review. All 7 tasks done. `devtools::test()` on the final tree: 0 failed, 0 errors, **3082 passing**, 0 skipped. `cairn_validate` all checks pass — `weight caps` PASS (the M44 file itself hit the 150-line plan-owned cap twice during the pass; Scope and then the ledger were each compressed in a single rewrite rather than nibbled). Branch touches **no package file**: `git diff master...HEAD --name-only` under `R/ src/ tests/ man/ vignettes/` is empty, so T3's mutation probe left no residue.

## Decisions

## Review
