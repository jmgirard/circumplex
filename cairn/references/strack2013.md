# strack2013 — the tau-equivalent CFA model for circumplex axes reliability

**Provenance.** Ingested 2026-07-23 by M53 from
`cairn/references/sources/strack2013.pdf` (gitignored). The shelf PDF is 12
pages and holds the whole article, printed pp. 1–12 on the pages themselves.
Pagination: the article's own page numbers. The PDF is **born-digital**
(`Creator: Adobe InDesign CS5.5`, `Producer: Adobe PDF Library 9.9`) — not an
OCR scan — so its `pdftotext` text layer is the typeset text itself, a faithful
witness (unlike the `hubert1987`/`tracey1997` OCR scans).
Extraction: verified 2026-07-23 against the born-digital `pdftotext -layout` text layer, with Table 3 and every formula below additionally cross-read against the layout-preserved rendering; re-verified 2026-07-25 (M60) on p. 7, where the `Type` column and the type-b and type-c rows banked below were read in both channels — the text layer and a 200-dpi page-image render — agreeing on every value (the type-e and type-f rows on that page are NOT banked here and carry no verification claim; they are M61's to bank); no value read by a second human channel — observed 2026-07-25.

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

**Non-blocked type-a rows — col 6 (%axes), col 10 (item_n), col 11
(Reliability), banked for the M54 Layer-A oracle (BC1, the ±.01 sweep over
every non-blocked type-a row).** The `(SE)` under the col-number header is the
parenthetical SE of col 6 %axes, not a separately numbered column, so col 10 =
item_n, col 11 = Reliability, col 12 = Raw variance, col 13 = SEm (as the note
above uses). Blocked type-a instruments carry block-specificity (col 9) and are
excluded from the sweep: CSIV (S7 Self, 2.8%), TRC-g (S10 Self, 3.7%), TRC-t
(S11 Self, 6.7%).

Variance components banked too (cols 5–9): %general (col 5), %axes (col 6),
%scale-specificity (col 7), %item-specificity (col 9); %block-specificity
(col 8) is `—` for all twelve (non-blocked), so the row sum is
%gen + %axes + %scale + %item (RR10 Q4 sum guard).

| Instrument | Sample | Persp. | %gen | %axes | %scale | %item | item_n | Rel | sum |
|---|---|---|---|---|---|---|---|---|---|
| IAL   | 1 | Self  |  2.1 | 26.0 | 6.5 | 65.4 | 32 | .92 | 100.0 |
| IAL   | 1 | Other |  2.0 | 26.1 | 5.7 | 66.2 | 32 | .92 | 100.0 |
| IAL   | 2 | Self  |  2.9 | 23.0 | 5.3 | 68.8 | 32 | .90 | 100.0 |
| IAS-R | 3 | Self  |  1.1 | 22.9 | 9.1 | 66.9 | 32 | .90 | 100.0 |
| IAS-R | 3 | Other |  1.4 | 21.5 | 8.7 | 68.4 | 32 | .90 | 100.0 |
| IIP   | 4 | Self  | 13.9 | 11.8 | 1.5 | 72.8 | 32 | .81 | 100.0 |
| IIP   | 5 | −t1   | 16.6 | 13.2 | 1.5 | 68.7 | 32 | .83 | 100.0 |
| IIP   | 5 | −t2   | 20.5 | 11.8 | 2.0 | 65.7 | 32 | .81 | 100.0 |
| IIP   | 6 | Self  | 17.7 | 13.0 | 2.4 | 67.9 | 32 | .81 | **101.0** |
| IMI   | 6 | Other |  1.7 | 27.9 | 5.9 | 64.5 | 32 | .92 | 100.0 |
| SAS-C | 8 | Self  |  4.8 | 17.8 | 6.2 | 71.2 | 32 | .87 | 100.0 |
| IPI-A | 9 | Self  | 19.2 | 13.4 | 2.8 | 64.6 | 16 | .71 | 100.0 |

**Column (1) is `Type`** — Table 3 labels every row with its Figure 1 circumplex
type (a–f), so the paper publishes reliability anchors for the non-octant types,
not only for type a. Banked below for M60 (types b, c) and M61 (the single-item
types e, f); type d (OCAI) carries block-specificity and waits on ζ2.

**Type-b rows (CV-LI, equal 45° spacing rotated 22.5° off the axes; p. 2).**
All four sum to 100.0, and all four reproduce col 11 by Spearman–Brown on
col 6 / col 10 within ±.01 (.367, .308, .237, .568 against .37, .31, .24, .57):

| Instrument | Sample | Persp. | %gen | %axes | %scale | %item | item_n | Rel | sum |
|---|---|---|---|---|---|---|---|---|---|
| CV-LI | 12 | Self  | 22.6 | 3.5 | 19.6 | 54.3 | 16 | .37 | 100.0 |
| CV-LI | 12 | Other | 42.9 | 2.7 | 15.0 | 39.4 | 16 | .31 | 100.0 |
| CV-LI | 12 | Meta  | 35.4 | 1.9 | 19.6 | 43.1 | 16 | .24 | 100.0 |
| CV-LI | 13 | Self  | 19.6 | 7.6 | 19.7 | 53.1 | 16 | .57 | 100.0 |

**Type-c row (MEIL, Sample 14 Self): %gen 4.3, %axes 5.5, %scale 27.9, %item
36.7, item_n 30, Rel .63 — components sum to 74.4, not 100.0.** A second source
defect, independent of the IIP S6 erratum below and already noted by RR10 from
the text layer; the page image agrees, so it is the source, not the
transcription. The 25.6 points are unaccounted for and col 8 is `—`. The
reliability column is nonetheless self-consistent: SB(.055, 30) = .6358
reproduces the printed .63 within ±.01, so the row is usable as a Layer-A
reliability anchor while its component row is not usable as a sum guard.
The instrument's scale count is not printed, so k cannot be derived from the
table — observed 2026-07-25.

**SEm cross-check inputs (col 12 Raw variance, col 13 SEm), banked for BC2:**
IAL S1 Self 0.98 → 0.28; OCAI S15 Self 15.95 → 2.78; COC S16 Self 6.70 → 2.33.
Check: `sqrt(col 12)·sqrt(1 − col 11)` gives IAL .2800 (→.28), COC 2.330 (→2.33)
exactly, OCAI 2.796 (printed 2.78, within input-rounding — the BC2 ±.02 slack).

Extraction of this Table 3 block: two channels on p. 7, agreeing on every value
above — the born-digital `pdftotext -layout` text layer, and an AI read of the
page-image rendering (not a human attestation; no value read by a human eye);
verified 2026-07-23 (M54 T2), components cross-read against the `pdftotext -raw`
mode in RR10.

**Erratum (IIP Sample 6 Self).** This row is internally inconsistent in the
printed table: its variance components sum to **101.0%** (17.7 + 13.0 + 2.4 +
67.9). Among the twelve **non-blocked type-a** rows above it is the only one not
summing to 100.0% (RR10 narrowed an earlier "only row in Table 3" claim:
table-wide, CSIV S7 Self sums to 102.9 and OCAI S15 Meta to 100.6, and the MEIL
S14 Self text layer reads 74.4 — none is in the BC1 sweep population). SB on the
printed %axes 13.0 / item_n 32 gives .827, missing the printed Reliability .81
by .017 — while all 11 other non-blocked type-a rows reproduce col 11 within
±.01. A single-digit correction of %axes 13.0 → **12.0** restores the 100.0% sum
*and* reproduces the printed .81 (SB(.12, 32) = .8136), so the printed "13.0" is
almost certainly a typo for 12.0 (true ξ1 ≈ .12). One nuance (RR10 Q1/B-2): the
printed SEm .23 (raw variance .30) fits a reliability-typo reading slightly
better, but that would need two independent print errors and cannot explain the
101.0% sum, so parsimony favors the single %axes typo. Both extraction channels
read "13.0", so this is a source defect, not the transcription. Handling ruled
by RR10: sweep the 11 self-consistent rows, pin this row with the printed-pair
inconsistency + corrected-pair assertions (revised BC1).

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
