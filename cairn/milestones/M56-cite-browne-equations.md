# M56: Cite the published Browne equations `R/cpm_fit.R` implements

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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

- [ ] AC1 — Every estimation-path line in `browne1992.md`'s "Traces to" table
      carries an inline comment naming the equation number and printed page,
      and each citation matches the source note's banked form: `cpm_rho`/`:30`
      (eq. 34, p. 486), `cpm_rho_deriv`/`:46`, `cpm_implied_cor`/`:66-72`
      (eq. 3 under the (3b\*) identity, pp. 471–472), `cpm_implied_cov`/`:84-87`
      (eq. 2, p. 471), `cpm_discrepancy`/`:96-105` (eq. 5, p. 472), `q`/`df`
      `:155,161-162` (eq. 6, p. 473), RMSEA point `:1049` (eq. 8, p. 473),
      Heywood marker `:1384` (p. 472). Evidence: grep the citations + read each
      against `browne1992.md`.
- [ ] AC2 — `browne1992a.md`'s two estimation lines carry matching inline
      comments: `:1049` (eq. 13, p. 239 — the implemented arrangement of
      eq. 8) and `cpm_rmsea_ci`/`:1011-1028` (eq. 14, p. 240).
- [ ] AC3 — The two package-own departures carry comments attributing the
      choice to the package, **not** to Browne, claiming no more than
      `browne1992.md`'s "Departures" section supports: variant C (`:112`,
      "appears nowhere in this paper") and the m-cap (`:135-145`, "the paper
      does not print" a cap; §6.4's guidance is advisory only).
- [ ] AC4 — The m = 1 oracle in `test-cpm_oracles.R` cites Browne (1992)
      Table 11 (p. 494) as a co-anchor alongside the existing Grassi Table 2
      citation, with the digit-for-digit values recorded in `browne1992.md` §8
      (β₀ = .638, β₁ = .362, ρ₁₈₀° = .28, angles 0/55/112/123/192/210/269).
- [ ] AC5 — Both source notes' "Traces to" line anchors are re-synced to the
      post-edit line numbers; no "Traces to" entry points at a stale line.
- [ ] AC6 — `devtools::test()` clean and the m = 1 oracle test still passes
      (comments change no computation).

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7

## Tasks

- [ ] T1 — Build the equation→line checklist from both source notes' "Traces
      to" tables and `browne1992.md`'s "Departures" section; verify each cited
      equation against the note's banked verbatim form AND confirm each line
      still implements what the table claims (guard against M41-style drift —
      line numbers or code may have moved). No code change.
- [ ] T2 — Add the inline provenance comments for the `browne1992.md`
      estimation lines enumerated in AC1.
- [ ] T3 — Add the inline comments for `browne1992a.md`: `:1049` (eq. 13,
      p. 239) and `cpm_rmsea_ci` (eq. 14, p. 240). Coordinate `:1049` with T2's
      eq. 8 note — one comment naming both arrangements.
- [ ] T4 — Add the honest non-attribution comments at variant C (`:112`) and
      the m-cap (`:135-145`) per AC3.
- [ ] T5 — Add the Browne (1992) Table 11 (p. 494) co-anchor to the m = 1
      oracle provenance in `test-cpm_oracles.R` (~`:140-142`), alongside the
      existing Grassi Table 2 citation.
- [ ] T6 — Re-sync the "Traces to" line anchors in `browne1992.md` and
      `browne1992a.md` to the post-edit line numbers.
- [ ] T7 — `Rscript -e 'devtools::test()'` clean; confirm the m = 1 oracle test
      passes.

## Work log

- 2026-07-23: created by /milestone-plan.

## Decisions

## Review
