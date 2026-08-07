# locke2014 — the CSIG and its octant descriptive statistics

**Provenance.** Ingested 2026-08-06 by M72 from
`cairn/references/sources/locke2014.pdf` (gitignored), 17 pages, printed
pp. 433–449. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Creator: Adobe InDesign CS5.5`, `Producer: Adobe PDF Library
9.9`), not an OCR scan, so M42-D1's two-channel requirement does not fire.
It was read in two channels regardless, and here that mattered: **the octant
M/SD are printed inside Figure 2, not in any table**, so the `pdftotext` layer
does not carry them at all and the page-image render is the only channel that
reads them. The item-to-scale mapping comes from a second source, the CSIG
Items & Scales page retrieved 2026-08-06 from kennethlocke.org.
Extraction: verified 2026-08-06 — Figure 2 read from a 170-dpi page-image
render; the item mapping read in two independent channels (model-mediated fetch
and raw-HTML extraction), agreeing on every value; no value read by a second
human channel — observed 2026-08-06.

**Citation.** Locke, K. D. (2014). Circumplex scales of intergroup goals: An
interpersonal circle model of goals for interactions between groups.
*Personality and Social Psychology Bulletin, 40*(4), 433–449.
DOI 10.1177/0146167213514280.

**Role.** The published source for every shipped CSIG norm value. All eight
M/SD match the article exactly.

**Source-fidelity observation.** Figure 2 prints *identical* statistics for the
PA and NO octants — α = .73, M = 2.96, SD = .68 for both. The package
transcribes this faithfully, so it is not a packaging error; whether the figure
itself is right is a question for the author, carried as a ROADMAP candidate
row (M72, 2026-08-06).

## Extracted values

Angles are not published: Figure 2 shows the octant arrangement but prints no
degrees, so the degree assignment is the package's own convention (IP2).

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | 2.96 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | BC | 2.53 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | DE | 2.02 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | FG | 1.88 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | HI | 2.24 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | JK | 2.89 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | LM | 2.97 | Figure 2, p. 436 (statistics printed inside the figure) |
| M | NO | 2.96 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | PA | 0.68 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | BC | 0.86 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | DE | 0.88 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | FG | 0.74 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | HI | 0.90 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | JK | 0.76 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | LM | 0.71 | Figure 2, p. 436 (statistics printed inside the figure) |
| SD | NO | 0.68 | Figure 2, p. 436 (statistics printed inside the figure) |
| Angle | PA | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | not published — Figure 2 shows octant arrangement without degrees; degrees are package convention (IP2) |
| Items | PA | 8, 16, 24, 32 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | BC | 5, 13, 21, 29 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | DE | 2, 10, 18, 26 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | FG | 7, 15, 23, 31 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | HI | 4, 12, 20, 28 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | JK | 1, 9, 17, 25 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | LM | 6, 14, 22, 30 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Items | NO | 3, 11, 19, 27 | CSIG_Items_Scales.html, 'Item Order' column (retrieved 2026-08-06) |
| Size | — | 665 | p. 435 text, 'All (n = 665) participants'; Table 3 CSIG Study 1 row, p. 437 |
| Population | — | MTurk respondents from the United States, Canada and India | p. 435 text, 'accessed and completed an online questionnaire through Amazon's Mechanical Turk website'; country breakdown same page |
<!-- audit-values-end -->
