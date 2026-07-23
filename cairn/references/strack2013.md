# strack2013 — the tau-equivalent CFA model for circumplex axes reliability

**Provenance.** Ingested 2026-07-23 by M53 from
`cairn/references/sources/strack2013.pdf` (gitignored). The shelf PDF is 12
pages and holds the whole article, printed pp. 1–12 on the pages themselves.
Pagination: the article's own page numbers. The PDF is **born-digital**
(`Creator: Adobe InDesign CS5.5`, `Producer: Adobe PDF Library 9.9`) — not an
OCR scan — so its `pdftotext` text layer is the typeset text itself, a faithful
witness (unlike the `hubert1987`/`tracey1997` OCR scans).
Extraction: verified 2026-07-23 against the born-digital `pdftotext -layout` text layer, with Table 3 and every formula below additionally cross-read against the layout-preserved rendering; no value read by a second human channel — observed 2026-07-23.

**Citation.** Strack, M., Jacobs, I., & Grosse Holtforth, M. (2013). Reliability
of Circumplex Axes. *SAGE Open*, 3(2), 1–12. DOI 10.1177/2158244013486115.
(The article prints an April–June 2013 issue line; SAGE Open is
continuously paginated per article, so "1–12" is the article's own extent.)

**Role.** The published source for a proposed circumplex **axes-reliability**
estimator (M53 design → build). It supplies the restricted CFA model, the
reliability/SEm/Nunnally–Bernstein formulas, and — via Table 3 — a
published-value oracle for the formula layer. RANDALL-adjacent: the paper
contrasts its component-isolating model with Tracey's RANDALL (`tracey1997.md`,
`hubert1987.md`), which it reports is sensitive to the axes/scale-specificity
ratio.

## Extracted values

### The five variance components (p. 4, Figure 2 note)

Each circumplex item's variance decomposes additively into five orthogonal
components summing to 100%:

> Item i 1.00 = General_Factor ξ2 + Axes ξ1 + Scale_specifity ζ1 +
> Block_specificity ζ2 + Item_specificity εi

- **Axes ξ1** — two axes forced orthogonal, variances set equal: "For a perfect
  circle, the variance of both axes are set equal (Y_axis = ξ1 = X_axis)"
  (p. 4). Only ξ1 is used for reliability (p. 4).
- **General factor ξ2**, **scale-specificity ζ1** (all scales' set equal),
  **block-specificity ζ2** (blockwise instruments only), **item-specificity εi**
  (free errors: "In tau-equivalent models, the errors stay free", p. 3).
- Free parameters "besides the free errors: 2, 3, or 4 parameters" (p. 5).

### The fixed item weights on the axes (pp. 2–4)

Loadings are fixed to the cosine of the scale's angular position: "Each weight
derives from the cosine of the scale's angular position in the circle" (p. 2).
`Axis_score = Σ(wi × scalei_score)` (p. 2). Type-a (canonical IPC) weights are
`0, ±.707, ±1.0`; type-b weights `±.38268 and ±.92388` (interstitial spacing
22.5°/67.5°, p. 3). For circumplex types a and b, `Σwi² = 4.00` (p. 3).

### Reliability, item_n, and SEm (pp. 3–4)

Spearman–Brown "list-length" reliability from the axes variance:

> Rel_axis = (item_n × ξ1) / (1 + (item_n − 1) × ξ1)   (p. 4)

where **item_n = Σ(wi² items per scale)** = the sum of squared weights over all
items adding to an axis (Table 3 col. 10; pp. 4–5). `SEm = SD × √(1 − Rel)`
(p. 3); the 90% location CI is `±1.65 × SEm` (p. 6).

### Nunnally–Bernstein comparison (p. 3)

> Rel_axis = 1 – ([Σwi² − Σwi² × Rel_scalei] / Var_axis)   (Nunnally &
> Bernstein, 1994, p. 271, Eqs. 7–17; computed on z-standardized scales)

Headline finding: N–B **overestimates** axis reliability when scale-specificity
is large (Figure 3; MEIL and CV-LI, scale-specificity > 70%, axes < 30%; p. 8).

### Table 3 — the published-value oracle (p. 7)

Variance components (cols 5–9, %), item_n (col 10), Reliability (col 11), SEm
(col 13), N–B reliability (col 14), RANDALL CI (col 15). Reliabilities range
**.13 to .92** across 13 instruments / 29 subsamples. Spearman–Brown on the
printed `%axes` (col 6) and `item_n` (col 10) reproduces col 11:

- IAL, Sample 1 Self: %axes 26.0, item_n 32 → Rel .92 (col 11 = .92).
- IPI-A, Sample 9 Self: %axes 13.4, item_n 16 → Rel .71 (col 11 = .71).
- OCAI, Sample 15 Self: %axes 11.7, item_n 8 → Rel .51 (col 11 = .51).
- COC, Sample 16 Self: %axes 2.8, item_n 8 → Rel .19 (col 11 = .19).

### Global fit and the RANDALL relation (pp. 5, 8)

Mean fit across the 29 models: `RMSEA = .088 (SD .014)`, `AGFI = .691
(SD .107)`, `PGFI = .651 (SD .070)` (p. 5). RANDALL's CI correlates with
scale-specificity `r = −.788` and axes variance `r = .637` (n = 23 subsamples;
p. 8) — RANDALL confounds the two components the CFA isolates.

## Traces to

- `devel/m53-axes-reliability-spec.md` — the M53 design spec; every model,
  formula, and oracle claim here anchors to this page.
- On a GO/NO-GO **GO**, the axes-reliability build's estimator and its
  Layer-A (Table 3) / Layer-B (synthetic + cross-engine) oracles will trace
  here — no `R/` or `tests/` code traces yet (design-only milestone) —
  observed 2026-07-23.

## Open questions

- **Exact model constraint set not yet pinned to lavaan syntax** — whether εi is
  free per item or implied by correlation-matrix diagonal reproduction, and the
  identification of the 2–4-variance fixed-links model, are RB09 questions for
  the build to settle (spec §2 F-1/F-2) — observed 2026-07-23.
- **No second human/independent channel** — extraction rests on the born-digital
  text layer plus a layout-rendering cross-read of the load-bearing pages; a
  born-digital text-layer defect would escape both. The source is not an OCR
  scan, so the two channels are less correlated than an OCR pair, but they are
  not two witnesses — observed 2026-07-23.
