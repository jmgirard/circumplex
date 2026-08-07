# hatcher2009 — the IIS-64 and its normative descriptive statistics

**Provenance.** Ingested 2026-08-06 by M73 from
`cairn/references/sources/hatcher2009.pdf` (gitignored), 16 pages, printed
pp. 554–569. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Producer: PDFlib+PDI 7.0.2 (PHP5/Win32)`, no full-page
images, 5311 text characters per page), not an OCR scan. The norm values live
in Table 1 on p. 558 as ordinary typeset text, so the text layer reads the
anchor itself rather than a rasterization of it; they were nonetheless read in
two independent channels — the `pdftotext` text layer and a 300-dpi page-image
render of p. 558 — because a table's row-to-column structure can be
reconstructed wrongly where every glyph is faithful (the M72 lesson: ask what
channel reads the anchor, and `pdftotext` twice is one channel read twice).
Extraction: verified 2026-08-06 — every value below read in both channels, the
channels agreeing on every value; no value read by a second human channel
— observed 2026-08-06.

**Citation.** Hatcher, R. L., & Rogers, D. T. (2009). Development and
validation of a measure of interpersonal strengths: The Inventory of
Interpersonal Strengths. *Psychological Assessment, 21*(4), 554–569.
DOI 10.1037/a0017269. The article's own byline prints the second author as
"Daniel T. Rogers"; the reference list of hatcher2012 (p. 645) gives the same
person as "Rogers, D. T.".

**Role.** The published source for every shipped IIS-64 norm value. Its
Appendix (p. 569) is also the source for the shipped item-to-octant grouping.

**A source-internal inconsistency.** The article states its Study 3 sample two
different ways. The Participants paragraph on p. 558 reads "An undergraduate
sample of 684 completed", and its own breakdown sums to that figure (265
Midwestern plus 419 Southern). Table 1's note on the same page reads
"N = 686". The package ships 684, which the sample description and its
arithmetic both support; the discrepancy is a question about the article, not
about the package. Both figures were confirmed in both channels.

**On the angles.** The article does publish octant angular locations (p. 558),
but as CIRCUM *estimates* from the combined N = 889 sample with Connect fixed
at 0° — Engage 43°, Lead 86°, Direct 127°, Balance 177°, Restrain 233°,
Cooperate 268°, Consider 307°. Those are measured locations, not an assigned
degree convention, and the package ships the ideal equally-spaced octant
degrees its own IP2 convention fixes. So the `Angle` rows below are recorded as
not published rather than compared against these estimates.

**On the item numbering.** The Appendix groups the 64 items by octant as item
*text* and assigns no numbers. The shipped item numbers are the package's own
ordering of its `Items` table, so the `Items` rows below carry a number string
derived by matching each Appendix line onto that table. Those rows therefore
test the source's **grouping** and not its numbering, and the two sides share
the shipped item-text table as the key that joins them. All 64 assignments were
matched line by line in both channels.

## Extracted values

M and SD are the `M` and `SD` columns of Table 1, p. 558, whose row labels are
the octant names (Connect = LM, Engage = NO, Lead = PA, Direct = BC,
Balance = DE, Restrain = FG, Cooperate = HI, Consider = JK).

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | 4.20 | Table 1, p. 558, 'Lead' row, M column |
| M | BC | 4.10 | Table 1, p. 558, 'Direct' row, M column |
| M | DE | 4.10 | Table 1, p. 558, 'Balance' row, M column |
| M | FG | 4.23 | Table 1, p. 558, 'Restrain' row, M column |
| M | HI | 4.59 | Table 1, p. 558, 'Cooperate' row, M column |
| M | JK | 4.66 | Table 1, p. 558, 'Consider' row, M column |
| M | LM | 4.61 | Table 1, p. 558, 'Connect' row, M column |
| M | NO | 4.16 | Table 1, p. 558, 'Engage' row, M column |
| SD | PA | 1.32 | Table 1, p. 558, 'Lead' row, SD column |
| SD | BC | 1.29 | Table 1, p. 558, 'Direct' row, SD column |
| SD | DE | 1.34 | Table 1, p. 558, 'Balance' row, SD column |
| SD | FG | 1.24 | Table 1, p. 558, 'Restrain' row, SD column |
| SD | HI | 1.17 | Table 1, p. 558, 'Cooperate' row, SD column |
| SD | JK | 1.14 | Table 1, p. 558, 'Consider' row, SD column |
| SD | LM | 1.24 | Table 1, p. 558, 'Connect' row, SD column |
| SD | NO | 1.36 | Table 1, p. 558, 'Engage' row, SD column |
| Angle | PA | not-published-in-source | p. 558 publishes CIRCUM estimates (Lead 86 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | p. 558 publishes CIRCUM estimates (Direct 127 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | p. 558 publishes CIRCUM estimates (Balance 177 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | p. 558 publishes CIRCUM estimates (Restrain 233 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | p. 558 publishes CIRCUM estimates (Cooperate 268 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | p. 558 publishes CIRCUM estimates (Consider 307 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | p. 558 fixes Connect at 0 deg as the CIRCUM reference, not as an assigned degree — degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | p. 558 publishes CIRCUM estimates (Engage 43 deg) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Items | PA | 11, 16, 22, 29, 31, 37, 40, 58 | Appendix, p. 569, 'Lead' block — grouping only; numbers are the package's own item ordering |
| Items | BC | 7, 10, 20, 28, 39, 46, 55, 59 | Appendix, p. 569, 'Direct' block — grouping only; numbers are the package's own item ordering |
| Items | DE | 4, 5, 15, 18, 36, 48, 50, 53 | Appendix, p. 569, 'Balance' block — grouping only; numbers are the package's own item ordering |
| Items | FG | 3, 8, 21, 34, 45, 51, 54, 56 | Appendix, p. 569, 'Restrain' block — grouping only; numbers are the package's own item ordering |
| Items | HI | 2, 9, 14, 32, 41, 47, 49, 60 | Appendix, p. 569, 'Cooperate' block — grouping only; numbers are the package's own item ordering |
| Items | JK | 17, 26, 27, 33, 35, 42, 43, 52 | Appendix, p. 569, 'Consider' block — grouping only; numbers are the package's own item ordering |
| Items | LM | 6, 13, 19, 23, 30, 44, 57, 61 | Appendix, p. 569, 'Connect' block — grouping only; numbers are the package's own item ordering |
| Items | NO | 1, 12, 24, 25, 38, 62, 63, 64 | Appendix, p. 569, 'Engage' block — grouping only; numbers are the package's own item ordering |
| Size | — | 684 | p. 558, Study 3 Participants, 'An undergraduate sample of 684 completed'; the same paragraph's 265 plus 419 breakdown sums to it. Table 1's note on the same page instead reads 'N = 686' |
| Population | — | undergraduates from a large Midwestern university (265) and a medium-sized Southern university (419) | p. 558, Study 3 Participants paragraph |
| Reference | — | Hatcher & Rogers (2009) | p. 554, journal header set as two stacked lines, 'Psychological Assessment' above '2009, Vol. 21, No. 4, 554–569' (en dash; the text layer inserts a space before it that the rendered page does not print), with the byline 'Robert L. Hatcher' and 'Daniel T. Rogers' printed side by side above their affiliations and carrying no superscripts |
| URL | — | https://doi.org/10.1037/a0017269 | p. 554, 'DOI: 10.1037/a0017269' printed in the journal header |
<!-- audit-values-end -->

## Traces to

- `data-raw/iis64.R` — every shipped IIS-64 norm value, item grouping and
  provenance string compared against the block above.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  iis64 comparison.
- `cairn/references/norms-audit.md` — carries iis64's audit verdict and this
  file's shelf-manifest row.
