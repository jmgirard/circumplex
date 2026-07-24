# M56: Cite the published Browne equations `R/cpm_fit.R` implements

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m56-cite-browne-equations · https://github.com/jmgirard/circumplex/pull/82

## Goal

Give the CPM engine's implementing lines local provenance — inline comments
naming the published Browne equation and page each line implements — so a
future corrector sees which equation they are changing.

## Scope

**In:** inline `#` comments at the `R/cpm_fit.R` lines named in the "Traces to"
tables of `cairn/references/browne1992.md` and `browne1992a.md`, each citing the
equation number + printed page and matching that source note's banked form;
honest non-attribution comments at the two package-own departures (variant C,
the m-cap) marking them as the package's choice, not Browne's; Browne (1992)
Table 11 (p. 494) added as a co-anchor to the m = 1 oracle provenance in
`tests/testthat/test-cpm_oracles.R`; re-sync of both source notes' "Traces to"
line anchors after the edits shift line numbers.

**Out:** roxygen `@description`/`@references` augmentation (inline comments only,
so no `man/` regen) → the existing "design sec." roxygen refs stay as-is;
`browne1982.md` code comments → none owed (that note states nothing in `R/`
traces to it); the ΔCFI / `cheung2002` fold-in → its own candidate row;
*adjudicating* the recorded departures (m-cap, variant C provenance) → they
stay recorded, not decided — their source-note Open-Question status is unchanged.

## Acceptance criteria

- [x] AC1 — Every estimation-path line in `browne1992.md`'s "Traces to" table
      carries an inline comment naming the equation number and printed page,
      and each citation matches the source note's banked form: `cpm_rho`/`:30`
      (eq. 34, p. 486), `cpm_rho_deriv`/`:46`, `cpm_implied_cor`/`:66-72`
      (eq. 3 under the (3b\*) identity, pp. 471–472), `cpm_implied_cov`/`:84-87`
      (eq. 2, p. 471), `cpm_discrepancy`/`:96-105` (eq. 5, p. 472), `q`/`df`
      `:155,161-162` (eq. 6, p. 473), RMSEA point `:1049` (eq. 8, p. 473),
      Heywood marker `:1384` (p. 472). Evidence: grep the citations + read each
      against `browne1992.md`.
- [x] AC2 — `browne1992a.md`'s two estimation lines carry matching inline
      comments: `:1049` (eq. 13, p. 239 — the implemented arrangement of
      eq. 8) and `cpm_rmsea_ci`/`:1011-1028` (eq. 14, p. 240).
- [x] AC3 — The two package-own departures carry comments attributing the
      choice to the package, **not** to Browne, claiming no more than
      `browne1992.md`'s "Departures" section supports: variant C (`:112`,
      "appears nowhere in this paper") and the m-cap (`:135-145`, "the paper
      does not print" a cap; §6.4's guidance is advisory only).
- [x] AC4 — The m = 1 oracle in `test-cpm_oracles.R` cites Browne (1992)
      Table 11 (p. 494) as a co-anchor alongside the existing Grassi Table 2
      citation, with the digit-for-digit values recorded in `browne1992.md` §8
      (β₀ = .638, β₁ = .362, ρ₁₈₀° = .28, angles 0/55/112/123/192/210/269).
- [x] AC5 — Both source notes' "Traces to" line anchors are re-synced to the
      post-edit line numbers; no "Traces to" entry points at a stale line.
- [x] AC6 — `devtools::test()` clean and the m = 1 oracle test still passes
      (comments change no computation).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7

## Tasks

- [x] T1 — Build the equation→line checklist from both source notes' "Traces
      to" tables and `browne1992.md`'s "Departures" section; verify each cited
      equation against the note's banked verbatim form AND confirm each line
      still implements what the table claims (guard against M41-style drift —
      line numbers or code may have moved). No code change.
- [x] T2 — Add the inline provenance comments for the `browne1992.md`
      estimation lines enumerated in AC1.
- [x] T3 — Add the inline comments for `browne1992a.md`: `:1049` (eq. 13,
      p. 239) and `cpm_rmsea_ci` (eq. 14, p. 240). Coordinate `:1049` with T2's
      eq. 8 note — one comment naming both arrangements.
- [x] T4 — Add the honest non-attribution comments at variant C (`:112`) and
      the m-cap (`:135-145`) per AC3.
- [x] T5 — Add the Browne (1992) Table 11 (p. 494) co-anchor to the m = 1
      oracle provenance in `test-cpm_oracles.R` (~`:140-142`), alongside the
      existing Grassi Table 2 citation.
- [x] T6 — Re-sync the "Traces to" line anchors in `browne1992.md` and
      `browne1992a.md` to the post-edit line numbers. (Amended: the insertions
      also shifted cpm_fit.R cites in `browne1982.md`, `hu1999.md`, and
      `INDEX.md` — all re-synced, each new target content-verified.)
- [x] T7 — `Rscript -e 'devtools::test()'` clean; confirm the m = 1 oracle test
      passes.

## Work log

- 2026-07-23: created by /milestone-plan.
- 2026-07-23: T1–T4 — verified all 11 anchors vs banked forms (lines current, no drift); added inline eq comments to R/cpm_fit.R (T2–T4 folded into one checkpoint, finer split than the natural commit unit); non-ASCII `§`→`sec.` (M7 deny-by-default); devtools::test() clean FAIL 0 | PASS 3170.
- 2026-07-23: T5 — Browne (1992) Table 11 (p. 494) co-anchor added to the m=1 oracle header; fixture (beta .6378/.3622, mcsc .276, angles 0/55/112/123/192/210/269) matches Table 11's .638/.362/.28 to Browne's precision; test-cpm_oracles.R FAIL 0 | PASS 66 (9 CRAN-skip).
- 2026-07-23: T6 — amended to re-sync ALL source-note cpm_fit.R line anchors (browne1982/browne1992/browne1992a/hu1999/INDEX, not just the two planned — my insertions shifted them); each new target content-verified; cairn_validate references-staleness OK.
- 2026-07-23: T7 — full devtools::test() clean FAIL 0 | PASS 3170; status → review.
- 2026-07-23: review — three-lens + scorer; two lenses converged on stale "no citation" prose in browne1992a.md (:51,:145, score 85) and browne1992.md (:423-429, score 72) that M56's own comments falsified; fixed all three (past-tense/resolved framing); cairn_validate + check 0/0/0 still clean.

## Decisions

## Review

**Evidence (fresh, 2026-07-23, branch @ PR #82):**

- AC1 — `git diff master..HEAD -- R/cpm_fit.R` shows inline comments citing
  eq. 34/30 (`cpm_rho`), eq. 3+(3b*)/4 (`cpm_implied_cor`), eq. 2
  (`cpm_implied_cov`), eq. 5 (`cpm_discrepancy`), eq. 6 (`q`/`df`), eq. 8/13
  (RMSEA point), Heywood p. 472 — each with its printed page, matching
  `browne1992.md`'s banked forms.
- AC2 — same diff: `:1085` RMSEA point cites eq. 13 (p. 239); `cpm_rmsea_ci`
  closing line cites eq. 14 (p. 240). Both name `browne1992a.md`.
- AC3 — variant C comment: "the package's OWN constraint and appears nowhere
  in Browne"; m-cap comment: "Browne (1992) prints NO such cap … This bound is
  the package's own (browne1992.md Departures 2)". Neither over-attributes.
- AC4 — `test-cpm_oracles.R` header now cites Browne (1992) Table 11 (p. 494),
  β₀=.638/β₁=.362/ρ₁₈₀°=.28, angles 0/55/112/123/192/210/269, "DIRECT, not only
  transitive"; verified against fixture (.6378/.3622, mcsc .276) to Browne's
  printed precision.
- AC5 — grep for every old cite number across `cairn/references/*.md`: NONE
  STALE. Spot-checked 5 new anchors (`:1085` rmsea, `:108` discrepancy, `:1422`
  heywood, `:1039` rmsea_ci, `:72` implied_cor) — each points at the claimed
  code. `cairn_validate` "references staleness" OK.
- AC6 — `devtools::test()` FAIL 0 | WARN 4 (pre-existing) | PASS 3170; the m=1
  published-oracle test passes.

**Consistency gate:** `cairn_validate` all checks passed (scaffold present,
coverage complete, references staleness, dangling ids — 48 advisory work-log
warnings, all pre-existing M7 wrapping). Toolchain: `devtools::check(--no-manual)`
0 errors | 0 warnings | 0 notes; `pkgdown::check_pkgdown()` no problems. No new
top-level files; no `DESIGN.md` principle changed (`cairn_impact` skipped).

**Independent three-lens review + scorer:**

- [O] diff-bug (Opus): 0 findings — all citations, non-attribution comments,
  re-synced anchors, and the Table 11 co-anchor verified correct.
- [S] blame-history (Sonnet) and [S] prior-review-record (Sonnet) converged
  independently on ONE defect: three passages in the source notes still assert
  the engine code carries no equation citation — `browne1992a.md:51`,
  `browne1992a.md:145`, `browne1992.md:423-429` — which M56's own added
  comments falsify. The T6 re-sync updated only the numeric anchors, not this
  prose. (Prior-review lens confirmed GitHub PR-comment surface empty; this
  regresses nothing — it *completes* M41's finding but left the doc side open.)
- [S] scorer: 82 overall; sub-split loci 1–2 (`browne1992a.md`) = 85, locus 3
  (`browne1992.md` Open-questions bullet) = 72.

**Triage:**

- **Fixed now (loci 1–2, score 85 — actioned):** rewrote both `browne1992a.md`
  passages to state M56 added inline eq. 13/14 attribution (past tense + M56
  note), so the note no longer contradicts the code it describes.
- **Fixed now (locus 3, score 72 — below the 80 action threshold, logged and
  fixed anyway):** the same falsified claim in `browne1992.md`'s Open-questions
  bullet; marked "Resolved 2026-07-23 by M56" (historical framing preserved).
  Fixed despite the sub-threshold score because it is the identical defect in a
  file already being edited — leaving it would keep `browne1992.md` internally
  contradictory. `grep` confirms no "no attribution"/"no citation" claim
  remains; `cairn_validate` references-staleness still OK after the edits.
- No follow-ups spawned; nothing rejected.
