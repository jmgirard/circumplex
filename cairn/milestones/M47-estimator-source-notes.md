# M47: SSM estimator source notes (Wright 2009 + the defining Gurtman work)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m47-estimator-source-notes · https://github.com/jmgirard/circumplex/pull/73

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

- [x] `wright2009.md` exists at the M45 bar: full citation; the `aw2009`
      dataset provenance cross-referenced to `R/example_data.R:4` +
      `man/aw2009.Rd`; the closed-form SSM parameter formulas it publishes
      banked verbatim with page/table anchors; a `**Provenance.**` block whose
      extraction status is dated and honest about channel independence — where
      the shelf PDF is an OCR scan the page image is the single authoritative
      channel (M42-D1), not two independent ones. (Amended 2026-07-20: wright2009
      is a Paper-Capture scan.)
- [x] `gurtman1998.md` (Gurtman & Balakrishnan 1998, *Circular Measurement
      Redux*) — identified as the source defining the "conventional Gurtman
      estimator" with cited evidence matching `R/ssm_analysis.R:1183` and the
      weights at `test-ssm_sem_syntax.R:6` — banks its elevation / amplitude /
      angular-displacement / R² formulas verbatim with page anchors and a
      `**Provenance.**` block.
- [x] `gurtman2003.md` (Gurtman & Pincus 2003, Handbook ch. 16) banks the
      structural-summary model (Eq 16.7) and its R² goodness-of-fit definition
      with page anchors and a `**Provenance.**` block; `Traces to` the two SSM
      vignettes that cite it (`introduction-`, `intermediate-ssm-analysis.Rmd`).
      (Added 2026-07-20 at the T3 gate: 2003 is relied-upon, not owes-no-page.
      Corrected same day: gurtman2003 does NOT print the .80/.70 R² cutoffs
      Wright 2009 attributes to it — that's recorded as a dated observation.)
- [x] Each new page's `Traces to` names the code lines the source backs (the
      `R/ssm_analysis.R` closed-form path; `test-ssm_sem_syntax.R:6`; the
      vignettes), and the banked estimator formulas reconcile with the shipped
      estimator (weights / means / R² match).
- [x] Every remaining shelf Gurtman PDF (1991 / 1993 / 1994) is dispositioned —
      an owes-no-page `INDEX.md` comment with its reason, or a prospect
      `candidate` ROADMAP row where deliberately shelved — leaving none
      unaccounted.
- [x] `INDEX.md` carries a line for each new committed page; `cairn_validate`
      (references check + full run) is green.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3, T4b
- AC4 → T2, T4, T4b
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1 — Read `sources/wright2009.pdf`; extract the `aw2009` dataset
      provenance and the closed-form SSM parameter formulas with page/table
      anchors; two-channel verify (`pdftotext -layout` + a `pdftoppm`-rendered
      page-image read). **Jeff's warning (plan gate, 2026-07-20): the printed
      paper carries typos he had to correct for the oracle** — treat any banked
      value as suspect until the two channels agree, and record the printed
      errata verbatim beside the corrected value in a `wright2009.md` errata
      section (as `browne1992.md` does its five), never silently fix.
      [PDF is an OCR scan → page image is the sole authoritative channel; OCR
      text located only. aw2009 = Table A exactly; SS_Total missing-square typo
      + unreconciled scalars recorded.]
- [x] T2 — Author `wright2009.md` from `templates/source-note.md`: citation,
      provenance block, banked formulas, `Traces to` `R/example_data.R:4` /
      `man/aw2009.Rd` and the estimator code.
- [x] T3 — Identify the estimator-defining Gurtman paper: read `gurtman1998`
      (Circular Measurement Redux — elevation/amplitude/angular-displacement
      cosine estimator confirmed present at plan probe) and `gurtman2003`
      (Handbook chapter, vignette-cited), and match the published estimator to
      `R/ssm_analysis.R:1183` + the weights at `test-ssm_sem_syntax.R:6`.
      (RB tripwire: no-oracle — offer Fable escalation for the
      attribution/formula-identity correctness.)
      [gurtman1998 confirmed the definer (Wright cites its p.349 for SS_Total);
      gurtman2003 also relied-upon. Gate 2026-07-20: both get pages; proceed on
      Opus finding, no Fable.]
- [x] T4 — Author `gurtman1998.md` (the defining work): bank the
      elevation/amplitude/angular-displacement/R² formulas verbatim with
      anchors, a provenance block, and `Traces to` the estimator code.
      [Eqs. 1–2 read from the p.349 image (OCR scan). Confirmed the p.349 Wright
      cites for SS_Total; estimator = vector-averaging closed form w/ 2/p scaling.]
- [x] T4b — Author `gurtman2003.md` (Gurtman & Pincus, Handbook ch. 16): bank
      the structural-summary model (Eq 16.7) + R² goodness-of-fit definition with
      anchors, a provenance block, and `Traces to` the two SSM vignettes.
      [Born-digital text layer. Recorded that 2003 does NOT print the .80/.70
      cutoffs Wright attributes to it.]
- [x] T5 — Disposition the remaining shelf Gurtman PDFs (1991 / 1993 / 1994):
      owes-no-page `INDEX.md` comment with reason, or a prospect `candidate` row
      (search-first, D-042) where deliberately shelved.
      [All three uncited, none a standalone prospect → owes-no-page ledger
      entries. gurtman1994's scaling correction captured transitively.]
- [x] T6 — Add `INDEX.md` lines for the new pages; run `cairn_validate` +
      references check; guard the known parser traps (unbolded `verified <date>`
      off line-start; filename as INDEX link text — M40).

## Work log

- 2026-07-20: created by /milestone-plan (split from the 8-source triage; sibling M48).
- 2026-07-20: T1–T2 — authored `wright2009.md`. Banked estimator Eqs. 7–13 from the p.315 page image (OCR scan; image is sole authoritative channel). `aw2009` confirmed identical to Wright Table A (all 40 cells). Recorded the Appendix printed typo (SS_Total final term missing its square) + the unreconciled hand-computed scalars per Jeff's warning; no shipped test transcribes them. INDEX line added.
- 2026-07-20: T3 gate + amendment (M47-D1) — both Gurtman pages, no Fable. Amended AC1 (wright2009 is an OCR scan → single authoritative channel, not two), AC2 (→gurtman1998), added AC3 (gurtman2003); AC5 remaining = 1991/1993/1994. Corrected an AC3 over-claim: gurtman2003 does not print the .80/.70 cutoffs.
- 2026-07-20: T4/T4b — authored `gurtman1998.md` (Eqs. 1–2 from the p.349 image; the SS_Total page Wright cites; estimator = vector-averaging closed form w/ 2/p scaling) and `gurtman2003.md` (Eq. 16.7 from the born-digital text layer; recorded the .80/.70-not-in-2003 finding). Two INDEX lines added.
- 2026-07-20: T5–T6 — dispositioned gurtman 1991/1993/1994 as owes-no-page (uncited; 1994's scaling correction captured transitively). Validate green (weight caps / references / coverage all PASS); budget 132/149. All tasks done → status review.
- 2026-07-21: review fan-out (3 lenses + scorer). F1 (92) fixed — added the M41-D1 not-a-human-attestation caveat to all three pages, each Extraction status now one physical line. F2 (78) fixed — corrected the M47-D1 .80/.70 self-contradiction. F3/F4 (45/45) logged, not actioned. Validate green.

## Decisions

- M47-D1 (T3 gate, 2026-07-20): both `gurtman1998` (the estimator definer) and
  `gurtman2003` (vignette-cited SSM methods reference) get
  full source notes — 2003 is relied-upon by the vignettes, not owes-no-page
  (its role is the structural-summary exposition, NOT the R² .80/.70 cutoffs,
  which it does not print — see `gurtman2003.md`; corrected 2026-07-21 per review F2)
  (amends AC2 → AC2+AC3; remaining to disposition = 1991/1993/1994). The
  estimator-identity attribution proceeds on the triangulated Opus finding
  (Wright's explicit "Gurtman & Balakrishnan, 1998, p. 349" + code match +
  vignette citations); Fable escalation was offered on the no-oracle tripwire
  and declined by Jeff.

## Review

**AC evidence (fresh, 2026-07-20; PR #73):**

- AC1 ✓ — `wright2009.md`: citation (JPA 91(4) 311–322, DOI 10.1080/00223890902935696);
  `aw2009` provenance → Table A re-confirmed identical fresh (`aw2009` 5×8,
  PA[1] = −1.09, LM[5] = 1.49 = Table A P1/P5); Eqs. 7–13 banked with p.315
  anchors; Provenance states the OCR-scan single authoritative channel (image),
  dated observed 2026-07-20.
- AC2 ✓ — `gurtman1998.md`: identified as definer; Eqs. 1–2 banked from p.349
  (the SS_Total page Wright cites) matching `test-ssm_sem_syntax.R:6`
  ("e = mean, x = (2/p)*cos") and `R/ssm_analysis.R:1183` ("conventional Gurtman
  estimator"); Provenance present.
- AC3 ✓ — `gurtman2003.md`: Eq. 16.7 + R² goodness-of-fit definition banked with
  p.417 anchor; born-digital text-layer Provenance; `Traces to` the two SSM
  vignettes; the .80/.70-not-in-2003 correction re-verified fresh (0
  cutoff-language hits in the source).
- AC4 ✓ — each page's `Traces to` names specific code lines; the banked estimator
  reconciles with the shipped one (e = mean; X/Y weighted sums, c = 2/p;
  a = √(X²+Y²); δ from Y/X; R² = 1 − SS_resid/SS_Total).
- AC5 ✓ — gurtman 1991/1993/1994 each carry an owes-no-page `INDEX.md` ledger
  entry with reason (3 lines).
- AC6 ✓ — `INDEX.md` has a catalog line for each of the 3 new pages;
  `cairn_validate` exits 0, all CHECKs PASS.

**Consistency gate:** `cairn_validate` exit 0 (all CHECKs PASS). Toolchain
r-package gate: no package files touched (`git diff --name-only master..HEAD` =
`cairn/` only), so `document()` no-diff and `check()` are clean no-ops.
Advisories (non-blocking): `record density (1)` on `ROADMAP.md:4` (the planning
hygiene stamp, 402/400 chars — shortened when the stamp is replaced at post-merge
hygiene); `work-log format (47)` all pre-existing M7 lines.

**Independent fresh-context review (3 lenses + scorer, 2026-07-21):** [O] diff-bug,
[S] blame-history, [S] prior-review, [S] scorer. Four findings; scores F1 92 /
F2 78 / F3 45 / F4 45.

- **F1 (92, actioned — FIXED):** all three pages omitted the M41-D1 caveat that
  the session's page-image read is *not a human attestation* and "no value has
  been read by a human" — carried by all 10 predecessor pages — and phrased the
  read as "first-hand visual read." Fix: each Extraction status rewritten as one
  physical line disclosing it is the implementing session's own read, not a human
  attestation, with the no-human caveat (also removes a wrapped-Extraction-line
  risk).
- **F2 (78, below threshold — FIXED anyway):** M47-D1 restated the debunked
  ".80/.70 cutoffs" attribution to gurtman2003 as fact, contradicting the
  milestone's own correction. Fix: M47-D1 parenthetical corrected (a durable
  Decisions-record self-contradiction is worth the one-phrase fix even sub-80).
- **F3 (45, logged, not actioned):** AC2's "amplitude/angular-displacement
  formulas verbatim" overstates what gurtman1998 banks (Eq. 1 + Eq. 2 +
  vector-averaging prose; the source prints no explicit closed-form a/δ formula).
  The INDEX line and the page itself are accurate; only the plan-owned AC wording
  is loose (gate-amend-only) — not patched review-side.
- **F4 (45, logged, not actioned):** the gurtman1991/1993 owes-no-page rationale
  leans on citation-count language D-023 disfavors, but the disposition is a
  defensible substantive call (content plausibly out of SSM scope; per-source
  content reasons given). No candidate row added.

Lens confirmations (clean): all code anchors resolve; banked formulas reconcile
with `src/parameters.cpp` (elev=mean, (2/n)·Σ, √(x²+y²), atan2,
gfit=1−SS_resid/(var·(n−1))); `aw2009` = Table A all 40 cells; INDEX
link-text / staleness-parser / OCR-single-channel / page-range all clean; the
browne1993 correction (pre-existing on master) verified accurate.
