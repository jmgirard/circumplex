# boudreaux2018 — the CSIP and its normative descriptive statistics

**Provenance.** Ingested 2026-08-06 by M72 from
`cairn/references/sources/boudreaux2018.pdf` (gitignored), 16 pages, printed
pp. 594–609. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Creator: XPP`, `Producer: Adobe LiveCycle PDF Generator`),
not an OCR scan, so M42-D1's two-channel requirement does not fire; Table 1 was
read in both the text layer and a layout-preserved rendering, which matters
here because the table has five numeric column groups and the shipped values
come from the fifth (overall) one, not the first.
Extraction: verified 2026-08-06 in both channels, agreeing on every value; no
value read by a second human channel — observed 2026-08-06.

**Citation.** Boudreaux, M. J., Ozer, D. J., Oltmanns, T. F., & Wright, A. G. C.
(2018). Development and validation of the Circumplex Scales of Interpersonal
Problems. *Psychological Assessment, 30*(5), 594–609.
DOI 10.1037/pas0000505.

**Role.** The published source for every shipped CSIP norm value.

**Unit deviation (intended).** The source prints octant *sum* scores on a 0–24
range (8 items × 0–3). The package divides both M and SD by 8 to express them
as item means on the 0–3 anchor range the instrument's `Anchors` table uses —
`data-raw/csip.R` does this explicitly (`/ 8`). The values below are the
source's, un-divided; the audit script applies the documented `/8` before
comparing.

## Extracted values

Item-to-scale assignment is not published in the article, which describes the
items but prints no numbering; angles are likewise unpublished.

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | 3.0 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | BC | 3.2 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | DE | 5.6 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | FG | 7.2 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | HI | 7.1 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | JK | 6.5 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | LM | 7.4 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| M | NO | 4.7 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | PA | 3.9 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | BC | 3.8 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | DE | 5.1 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | FG | 5.5 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | HI | 5.1 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | JK | 4.6 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | LM | 4.7 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| SD | NO | 4.0 | Table 1, p. 600, overall 'M (SD)' column (n = 712) |
| Angle | PA | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | not published — degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | not published — degrees are package convention (IP2) |
| Items | PA | not-published-in-source | not published in the article — items described but not numbered |
| Items | BC | not-published-in-source | not published in the article — items described but not numbered |
| Items | DE | not-published-in-source | not published in the article — items described but not numbered |
| Items | FG | not-published-in-source | not published in the article — items described but not numbered |
| Items | HI | not-published-in-source | not published in the article — items described but not numbered |
| Items | JK | not-published-in-source | not published in the article — items described but not numbered |
| Items | LM | not-published-in-source | not published in the article — items described but not numbered |
| Items | NO | not-published-in-source | not published in the article — items described but not numbered |
| Size | — | 712 | Table 1 note, footnote c, p. 600 |
| Population | — | undergraduate students enrolled at a public university in the western United States | p. 597 text, validation-sample description |
| Reference | — | Boudreaux, Ozer, Oltmanns, & Wright (2018) | Table 1, p. 600 — the venue of every shipped CSIP norm value |
| URL | — | https://doi.org/10.1037/pas0000505 | DOI printed on the article's first page, p. 594 |
| note-only | women subsample | n = 121, own M (SD) column | Table 1, p. 600, 'Women' column; n from table note a |
| note-only | men subsample | n = 70, own M (SD) column | Table 1, p. 600, 'Men' column; n from table note b |
<!-- audit-values-end -->
