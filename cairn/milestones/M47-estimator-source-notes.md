# M47: SSM estimator source notes (Wright 2009 + the defining Gurtman work)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m47-estimator-source-notes

## Goal

Author `cairn/references/` source notes for the two shelf sources the package's
core closed-form SSM estimator relies on — Wright et al. (2009) and the Gurtman
work that defines the "conventional Gurtman estimator" — banking each estimator
formula with anchors and reconciling it against the implementing code.

## Scope

**In:** `wright2009.md` (the `aw2009` provenance + the closed-form SSM parameter
formulas it publishes, anchored); identification of which shelf Gurtman paper
the "conventional Gurtman estimator" attribution at `R/ssm_analysis.R:1183` /
`test-ssm_sem_syntax.R:6` relies on, plus a `gurtman<yyyy>.md` banking that
estimator's elevation/amplitude/angular-displacement/fit formulas verbatim;
`Traces to` and INDEX lines for each; disposition of the remaining shelf Gurtman
PDFs.

**Out:** the browne1993 twin cross-reference + strack2013 prospect → M48;
re-validating the estimator math itself (verified-correct core; this milestone
attributes and anchors, it does not re-derive) — the reconciliation is
citation-to-code, not a numeric oracle re-run; any package-code edit (docs
only). Gurtman 1992 and Gurtman & Pincus 2000 are cited in vignettes but are
**not on the shelf** → out (no source to ingest).

## Acceptance criteria

- [ ] `wright2009.md` exists at the M45 bar: full citation; the `aw2009`
      dataset provenance cross-referenced to `R/example_data.R:4` +
      `man/aw2009.Rd`; the closed-form SSM parameter formulas it publishes
      banked verbatim with page/table anchors; a `**Provenance.**` block whose
      extraction status names two independent channels (M40-D2) with dated
      re-check.
- [ ] The shelf Gurtman work defining the "conventional Gurtman estimator" is
      identified with cited evidence matching the estimator at
      `R/ssm_analysis.R:1183` and the closed-form weights at
      `test-ssm_sem_syntax.R:6`; `gurtman<yyyy>.md` banks its
      elevation/amplitude/angular-displacement/fit formulas verbatim with page
      anchors and a `**Provenance.**` block.
- [ ] Each new page's `Traces to` names the code lines the source backs (the
      `R/ssm_analysis.R` closed-form path; `test-ssm_sem_syntax.R:6`), and the
      banked formulas reconcile with the shipped estimator (weights/means match).
- [ ] Every remaining shelf Gurtman PDF (the non-defining ones among 1991 /
      1993 / 1994 / 1998 / 2003) is dispositioned — an owes-no-page `INDEX.md`
      comment with its reason, or a prospect `candidate` ROADMAP row where
      deliberately shelved — leaving none unaccounted.
- [ ] `INDEX.md` carries a line for each new committed page; `cairn_validate`
      (references check + full run) is green.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T2, T4
- AC4 → T5
- AC5 → T6

## Tasks

- [ ] T1 — Read `sources/wright2009.pdf`; extract the `aw2009` dataset
      provenance and the closed-form SSM parameter formulas with page/table
      anchors; two-channel verify (`pdftotext -layout` + a `pdftoppm`-rendered
      page-image read). **Jeff's warning (plan gate, 2026-07-20): the printed
      paper carries typos he had to correct for the oracle** — treat any banked
      value as suspect until the two channels agree, and record the printed
      errata verbatim beside the corrected value in a `wright2009.md` errata
      section (as `browne1992.md` does its five), never silently fix.
- [ ] T2 — Author `wright2009.md` from `templates/source-note.md`: citation,
      provenance block, banked formulas, `Traces to` `R/example_data.R:4` /
      `man/aw2009.Rd` and the estimator code.
- [ ] T3 — Identify the estimator-defining Gurtman paper: read `gurtman1998`
      (Circular Measurement Redux — elevation/amplitude/angular-displacement
      cosine estimator confirmed present at plan probe) and `gurtman2003`
      (Handbook chapter, vignette-cited), and match the published estimator to
      `R/ssm_analysis.R:1183` + the weights at `test-ssm_sem_syntax.R:6`.
      (RB tripwire: no-oracle — offer Fable escalation for the
      attribution/formula-identity correctness.)
- [ ] T4 — Author `gurtman<yyyy>.md` for the defining work: bank the
      elevation/amplitude/angular-displacement/fit formulas verbatim with
      anchors, a provenance block, and `Traces to` the estimator code.
- [ ] T5 — Disposition the remaining shelf Gurtman PDFs (1991 / 1993 / 1994 +
      whichever of 1998 / 2003 is not the definer): owes-no-page `INDEX.md`
      comment with reason, or a prospect `candidate` row (search-first, D-042)
      where deliberately shelved.
- [ ] T6 — Add `INDEX.md` lines for the new pages; run `cairn_validate` +
      references check; guard the known parser traps (unbolded `verified <date>`
      off line-start; filename as INDEX link text — M40).

## Work log

- 2026-07-20: created by /milestone-plan (split from the 8-source triage; sibling M48).

## Decisions

## Review
