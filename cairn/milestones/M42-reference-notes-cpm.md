# M42: Source notes for the CPM model and its communality CIs

- **Status:** planned
- **Priority:** normal
- **Depends on:** M40
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Author the Browne (1992) source note carrying the full CPM model
specification, plus the Browne (1982) communality-CI derivation the CPM oracle
path relies on.

## Scope

**In:** Two source notes, following the page conventions M40 establishes.

- `browne1992.md` — Browne (1992), *Circumplex models for correlation
  matrices*, *Psychometrika* 57(4) 469–497, shelf `sources/browne1992.pdf`.
  This page carries the **full CPM model specification** — enough to
  re-derive the estimand in `R/cpm_fit.R` without reopening the paper — not
  only the values the repo currently cites (plan-gate decision, 2026-07-19,
  carried over from M41).
- `browne1982.md` — Browne (1982), scoped to **pp. 95–96 only**; the shelf
  holds exactly those two pages as `sources/browne1982_pp95-96.pdf` and the
  rest of the source is neither available nor required. The page records the
  communality-CI derivation as Browne states it: nonsymmetric CIs on
  ρ(xᵢ, cᵢ) obtained from **symmetric** CIs on ln vᵢᵢ.

**Correction owed to a committed page.** `cairn/references/grassi2010.md:134-140`
carries an open question pointing at four page images
(`sources/browne1982_p95a.png`, `p95b`, `p96a`, `p96b`) that no longer exist —
the shelf now holds the consolidated PDF instead. That is a current-knowledge
claim proven false, so it is corrected **in place** and marked, per the
tracking rule; the same bullet's open question ("has not been checked against
Browne himself") is what `browne1982.md` resolves.

The repo's two-channel transcription protocol applies to both pages: a visual
page read and an independent `pdftotext -layout` extraction, diffed on every
load-bearing numeral, with between-channel discrepancies recorded rather than
silently resolved.

**Out:** The fit-index benchmark pair → M41. Acton & Revelle (2004) and Wendt
et al. (2019) → M43. **Package code changes of any kind** — this is a
documentation milestone. If T3's parameter map finds `R/cpm_fit.R` departing
from Browne as published, that is a finding to record and escalate, never to
reconcile inside this milestone; the fix would be its own milestone. Editing
the `devel/` transcriptions → after M7 archives (ROADMAP candidate row).
Cudeck & Browne (1983), shelved as `cudeck1983.pdf` — assessed and owes no
page (M41 work log, 2026-07-19).

## Acceptance criteria

- [ ] `cairn/references/browne1992.md` exists carrying every template section,
      with each extracted value page-, equation-, or table-anchored.
- [ ] The page carries the full CPM model specification: a reader can map each
      parameter `R/cpm_fit.R` estimates to its published counterpart **from
      the page alone**, without opening the paper.
- [ ] `cairn/references/browne1982.md` exists carrying every template section,
      scoped to pp. 95–96, recording the communality-CI derivation with the
      log-scale symmetry stated as Browne states it and anchored to the
      printed equation numbers.
- [ ] `grassi2010.md`'s Browne-1982 bullet is corrected in place and marked:
      the stale PNG pointer names the shelved PDF, and the open question it
      records is resolved by reference to `browne1982.md`.
- [ ] Each page's `Extraction:` status is one physical line stating its real
      per-channel standing — never a verification a channel did not perform
      (M40) — and each page's `Traces to` names specific citing lines
      (`R/cpm_fit.R`, `R/ssm_ci_accuracy.R:169`,
      `tests/testthat/test-cpm_oracles.R:133`, `cairn/references/grassi2010.md`),
      verified against the files.
- [ ] `INDEX.md` carries one line per new page with the **filename** as link
      text (M40); `cairn_validate` reports `references index<->disk` PASS with
      no `references staleness` WARN; `git diff --stat devel/` is empty, no
      file outside `cairn/` is modified, and each written page's tail bytes
      are checked for leaked tool-call scaffolding (M34).

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T1, T4
- AC4 → T5
- AC5 → T2, T4, T6
- AC6 → T6

## Tasks

- [ ] T1. Re-verify `browne1992.pdf` and `browne1982_pp95-96.pdf` on the shelf
      and stamp the observation — the shelf is live and moved twice during
      M40, so confirm rather than trust this plan's inventory.
- [ ] T2. Author `browne1992.md` — the full CPM model specification, two
      channels. Expect this to fill a working session on its own.
- [ ] T3. Build the parameter map: each quantity `R/cpm_fit.R` estimates
      against its published counterpart, walked against the code rather than
      asserted. A departure is recorded and escalated, never reconciled here.
- [ ] T4. Author `browne1982.md` from pp. 95–96, two channels.
- [ ] T5. Correct `grassi2010.md:134-140` in place — PNG pointer → shelved
      PDF, open question resolved, correction marked.
- [ ] T6. `Traces to` sections written against the actual citing lines;
      `INDEX.md` entries; `cairn_validate` clean; tail-byte and untouched-tree
      checks.

## Work log

- 2026-07-19: created by /milestone-plan, splitting M41 at the re-size gate its own `Out:` clause called for. Carries the two Browne sources: the 1992 CPM paper (the split-out M41 predicted — "a spec document in a source note's clothes") and the 1982 pp. 95–96 communality-CI derivation, which belongs here rather than with the fit benchmarks because it serves the same CPM oracle path (`tests/testthat/test-cpm_oracles.R:133`, `grassi2010.md`). Both sources verified present on the shelf at plan time — `browne1992.pdf` confirmed by first-page read as *Psychometrika* 57(4) 469–497, distinct from `browne1992a.pdf` (Browne & Cudeck, M41) — observed 2026-07-19. T5 exists because that plan-time inventory found `grassi2010.md` still naming four page images the consolidated PDF replaced.

## Decisions

## Review
