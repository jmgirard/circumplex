# M43: Source notes for the structure criteria and the validity source

- **Status:** planned
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Author source notes for the circumplex-structure criteria `R/fit_structure.R`
implements and for the substantive validity source the SEM vignette cites.

## Scope

**In:** Two source notes, following the page conventions M40 establishes.

- `acton2004.md` — Acton & Revelle (2004), *Evaluation of ten psychometric
  criteria for circumplex structure*, *MPR Online* 9(1), shelf
  `sources/acton2004.pdf`. The four criteria the package actually implements
  (Fisher, Gap, VT2, Rotation), their printed formulas and cutoffs, and the
  deviation-scoring guidance the repo cites at p. 9
  (`R/fit_structure.R:722`).
- `wendt2019.md` — Wendt et al. (2019), *The latent structure of
  interpersonal problems*, *J Abnorm Psychol* 128(8) 823–839, shelf
  `sources/wendt2019.pdf`. The four claims
  `vignettes/sem-based-ssm-analysis.Rmd` makes from it: the latent-structure
  framing (`:44`), the general–agency correlation of roughly −.3 (`:114`),
  RMSEA between .075 and .111 (`:368`), and the three-factor model (`:394`).

**Two discrepancies the pages must carry rather than resolve.**
`R/fit_structure.R:332-333` already records that Acton & Revelle's Eq. 6 **as
printed** uses the communalities h², while their prose describes vector
lengths √h²; the page records the printed form and the repo's resolution
**separately**, so the two stay distinguishable from the page alone. And
`devel/m5-wendt-discrepancies.md` is an existing read-only design record of
paper-internal inconsistencies; T5 reconciles against it and records what
disagrees rather than silently settling it.

The repo's two-channel transcription protocol applies to both pages: a visual
page read and an independent `pdftotext -layout` extraction, diffed on every
load-bearing numeral, with between-channel discrepancies recorded rather than
silently resolved.

**Out:** The fit-index benchmark pair → M41. Browne (1992) and Browne (1982)
→ M42. **Package code changes of any kind**, including any correction to the
Eq. 6 handling — if the note finds one is warranted, that is its own
milestone. Editing the `devel/` transcriptions and design records → after M7
archives (ROADMAP candidate row). Acton & Revelle (2002), shelved as
`acton2002.pdf` — owes no page, cited only as other authors' citation of
prior work (`INDEX.md`, 2026-07-19).

## Acceptance criteria

- [ ] `cairn/references/acton2004.md` exists carrying every template section,
      with each implemented criterion's formula and cutoff page-, equation-,
      or table-anchored.
- [ ] The page records the Eq. 6 discrepancy **as the paper prints it** and
      records the repo's resolution at `R/fit_structure.R:332-333` separately,
      such that a reader can tell the printed form from the shipped form from
      the page alone.
- [ ] The page records the cutoff calibration basis — Acton & Revelle
      calibrated at nv = 64/128 variables (`R/fit_structure.R:233`) — with its
      own anchor, so a reader can see what the shipped cutoffs were
      calibrated at and at what scale count the repo applies them.
- [ ] `cairn/references/wendt2019.md` exists carrying every template section,
      with each of the four vignette-cited claims quoted or table-anchored,
      and the reconciliation against `devel/m5-wendt-discrepancies.md`
      recorded — any disagreement stated, never silently resolved.
- [ ] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names the specific citing lines
      listed in Scope, verified against the files.
- [ ] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40); `cairn_validate` reports `references index<->disk` PASS with
      no `references staleness` WARN; `git diff --stat devel/` is empty, no
      file outside `cairn/` is modified, and each written page's tail bytes
      are checked for leaked tool-call scaffolding (M34).

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T3
- AC4 → T1, T4, T5
- AC5 → T2, T4, T6
- AC6 → T6

## Tasks

- [ ] T1. Re-verify `acton2004.pdf` and `wendt2019.pdf` on the shelf and stamp
      the observation — the shelf is live and moved twice during M40, so
      confirm rather than trust this plan's inventory.
- [ ] T2. Author `acton2004.md` — the four implemented criteria, their
      formulas and cutoffs, two channels.
- [ ] T3. Record the Eq. 6 printed-vs-prose discrepancy and the nv = 64/128
      calibration basis, cross-checked against `R/fit_structure.R:96,233,332-333`.
- [ ] T4. Author `wendt2019.md` — the four vignette-cited claims, two
      channels.
- [ ] T5. Reconcile against `devel/m5-wendt-discrepancies.md` (read-only) and
      record what disagrees.
- [ ] T6. `Traces to` sections written against the actual citing lines;
      `INDEX.md` entries; `cairn_validate` clean; tail-byte and untouched-tree
      checks.

## Work log

- 2026-07-19: created by /milestone-plan, splitting M41 at the re-size gate its own `Out:` clause called for. Carries the two remaining relied-on sources. Both verified present on the shelf at plan time by first-page read — `acton2004.pdf` = *MPR Online* 9(1), `wendt2019.pdf` = *J Abnorm Psychol* 128(8) 823–839 — observed 2026-07-19. Acton & Revelle is the heavier of the two: `R/fit_structure.R` implements four of its criteria and already records one printed-vs-prose discrepancy in its own comments, which AC2 makes the page carry rather than launder. Wendt is lighter — four vignette claims — but comes with an existing read-only design record of paper-internal inconsistencies that T5 must reconcile against.

## Decisions

## Review
