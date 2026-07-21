# M45: Source notes for the RANDALL structure test pair

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M40
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m45-reference-notes-randall

## Goal

Author `cairn/references/` pages for the two sources the package relies on for
its RANDALL structure test, discharging the standing ROADMAP candidate now that
both PDFs are shelved.

## Scope

**In:** `hubert1987.md` (Hubert & Arabie 1987, *Psych Bull* 102(1) 172–178) and
`tracey1997.md` (Tracey 1997, *EPM* 57(1) 164–168) — the randomization
order-agreement test the package computes in `structure_randall()` /
`structure_randall_test()`; each page-anchored with a `Traces to` list, an
honest `Extraction:` status, and a code-vs-paper reconciliation. `INDEX.md`
entries for both; retire the standing "Hubert & Arabie / Tracey (1997) owe a
page" ledger note. Fix the one-line miscount comment at
`tests/testthat/test-fit_structure_api.R:2`.

**Out:** the four forward-looking shelf sources (nagy2019, weide2021,
rogoza2021, tracey2000) → M46. A standalone `browne1993` page → already ingested
into `browne1992a.md` (the cited chapter edition; no page owed). Per-line CPM
equation attribution in `R/cpm_fit.R` → the standing candidate that rides the
next CPM-engine milestone. `browne1992a` OQ3 (human read) → stays that page's
open question.

## Acceptance criteria

- [ ] `cairn/references/hubert1987.md` exists and banks Hubert & Arabie's
      order-agreement statistic and its randomization p-value definition, each
      page-anchored, with a `Traces to` list citing the `structure_randall*`
      implementation and an `Extraction:` status claiming only what each channel
      actually saw (M40-D2; check `pdfinfo` Producer before any independence
      claim).
- [ ] `cairn/references/tracey1997.md` exists and banks Tracey's RANDALL
      operationalization (exact enumeration vs. random relabeling; the
      circumplex application), page-anchored, with a `Traces to` list and an
      honest `Extraction:` status.
- [ ] Both banked definitions are reconciled against the shipped code — the
      statistic `2·mean(vals[ia] > vals[ib]) − 1` (`R/fit_structure.R`
      `structure_randall()`), the circular-distance predictions
      (`randall_predictions()`), and the exact / Monte-Carlo p-value including
      the add-one `(1 + Σ)/(n_perm + 1)` estimator (`structure_randall_test()`)
      — with any code-vs-paper departure recorded verbatim (the `browne1992.md`
      "departures" pattern).
- [ ] `INDEX.md` gains both entries with filename link text (the INDEX-parser
      rule, M40 lesson), and the standing RANDALL "owes a page" ledger note is
      retired.
- [ ] `tests/testthat/test-fit_structure_api.R:2` no longer calls the entry
      point "the five Acton & Revelle (2004) structure tests": it names the four
      Acton & Revelle criteria plus RANDALL (Hubert & Arabie 1987 / Tracey
      1997).
- [ ] `cairn_validate` is clean (both new pages indexed; no unindexed-page
      advisory) and the `fit_structure` test files pass under `NOT_CRAN=true`
      (comment-only edit must not perturb the suite).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T5, T6

## Tasks

- [ ] T1 — Check `pdfinfo` Producer on `hubert1987.pdf`; extract the
      order-agreement statistic and randomization p-value via the independent
      channels that exist; author `hubert1987.md` (banked definitions, page
      anchors, `Traces to`, `Extraction:` status).
- [ ] T2 — Same for `tracey1997.pdf`; author `tracey1997.md` (RANDALL
      operationalization: exact vs. random relabeling, circumplex use).
- [ ] T3 — Reconcile both notes against `R/fit_structure.R`
      (`randall_predictions()` / `structure_randall()` ~585–591 /
      `structure_randall_test()` ~647–701); record any departure verbatim
      (probe display equations so the probe sees what it checks — M43 lesson).
- [ ] T4 — Add both `INDEX.md` entries (filename link text); retire the RANDALL
      owes-a-page ledger note.
- [ ] T5 — Fix the miscount comment at `test-fit_structure_api.R:2`; run the
      `fit_structure` test files under `NOT_CRAN=true` to confirm green.
- [ ] T6 — Run `cairn_budget` + `cairn_validate`; commit.

## Work log

- 2026-07-20: created by /milestone-plan (promotes the RANDALL ROADMAP
  candidate; both PDFs now shelved).

## Decisions

## Review
