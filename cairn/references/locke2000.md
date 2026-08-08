# locke2000 — the CSIV, and where its shipped norms actually come from

**Provenance.** Ingested 2026-08-06 by M72 from
`cairn/references/sources/locke2000.pdf` (gitignored), 19 pages, printed
pp. 249–267. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Producer: Acrobat Distiller 3.01 for Windows`), not an OCR
scan, so M42-D1's two-channel requirement does not fire; every table and
Figure 2 were read in both the text layer and a 150-dpi page-image render.
Two further sources are covered here and shelved beside it:
`locke_csiv_norms_2026-08-06.html` (sha256 536aa7b2…) and the CSIV scoring
page, both retrieved 2026-08-06 from kennethlocke.org.
Extraction: verified 2026-08-06 — every value read in two independent channels
(text layer and page image for the PDF; model-mediated fetch and raw-HTML
extraction for the web pages), agreeing on every value; no value read by a
second human channel — observed 2026-08-06.

**Citation.** Locke, K. D. (2000). Circumplex Scales of Interpersonal Values:
Reliability, validity, and applicability to interpersonal problems and
personality disorders. *Journal of Personality Assessment, 75*(2), 249–267.

**Role.** The article defines the instrument and supplies the **angle
assignment** (Figure 2, p. 255, which prints the degrees explicitly), but it
**publishes no octant means or standard deviations, and never reports the
shipped N = 1200** — its samples are N = 588 (Tables 1–2), 471, 248, 202, 181,
199, 124 and 84. The shipped M/SD and N come from Locke's website, which
describes them as "the 64-item version of the CSIV I administered to 1,200
University of Idaho undergraduates during the late 1990s and early 2000s" — a
**different sample** from the article's. That is why M72 corrected this
instrument's `Reference`: citing Locke (2000) alone attributed the norms to a
sample that did not produce them.

**Note-only rows** (published by the source, not shipped): the norms page also
tables a U.S. adult sample (Hopwood et al., 2022; N = 980) and a 32-item adult
MTurk sample (Locke & Adamic, 2012; N = 1,244).

## Extracted values

Angles here **are** source-published — Figure 2 prints 90/135/180/225/270/315/
0/45 against the octant labels. The table below records what the source
prints, LM = 0 included; translating it to the package's LM = 360 before
comparison would put both sides of the audit in the same hand. The comparison
is modulo 360, and the IP2 convention (LM = 360, never 0) is checked
separately on the shipped side by `data-raw/audit-norms.R`.

<!-- audit-values-begin -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 2.53 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | BC | 1.38 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | DE | 1.10 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | FG | 1.66 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | HI | 1.77 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | JK | 2.67 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | LM | 2.83 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| M | 1 | NO | 2.93 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | PA | 0.63 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | BC | 0.71 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | DE | 0.70 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | FG | 0.78 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | HI | 0.75 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | JK | 0.71 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | LM | 0.69 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| SD | 1 | NO | 0.57 | CSIV_Norms.html, 'U.S. Undergraduate Student Norms' table (retrieved 2026-08-06) |
| Angle | — | PA | 90 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | BC | 135 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | DE | 180 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | FG | 225 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | HI | 270 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | JK | 315 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Angle | — | LM | 0 | locke2000 Figure 2, p. 255 (degrees printed on the circle; the source prints 0 here, recorded verbatim) |
| Angle | — | NO | 45 | locke2000 Figure 2, p. 255 (degrees printed on the circle; LM printed as 0, compared mod 360) |
| Items | — | PA | 1, 9, 17, 25, 33, 41, 49, 57 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | BC | 4, 12, 20, 28, 36, 44, 52, 60 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | DE | 7, 15, 23, 31, 39, 47, 55, 63 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | FG | 2, 10, 18, 26, 34, 42, 50, 58 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | HI | 5, 13, 21, 29, 37, 45, 53, 61 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | JK | 8, 16, 24, 32, 40, 48, 56, 64 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | LM | 3, 11, 19, 27, 35, 43, 51, 59 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | — | NO | 6, 14, 22, 30, 38, 46, 54, 62 | CSIV_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Size | 1 | — | 1200 | CSIV_Norms.html sample description, 'N=1,200 University of Idaho undergraduates' |
| Population | 1 | — | University of Idaho undergraduates, late 1990s and early 2000s | CSIV_Norms.html sample description (retrieved 2026-08-06) |
| Reference | 1 | — | Locke (n.d.) | constructed-credit — on CSIV_Norms.html the *undergraduate* norms table and its sample description carry no citation and no date of their own, dating the sample only as 'the late 1990s and early 2000s'. The page does print citations elsewhere (Hopwood et al., 2022 above the adult table and Locke & Adamic, 2012 above the 32-item table, both recorded as note-only rows below) and a site footer '© 2024 Kenneth Locke', but each belongs to another table or to the site, not to this credit, so no printed text supplies its author or year; locke2000 publishes no octant statistics and a different sample (Tables 1-2, pp. 256-258) |
| URL | 1 | — | https://kennethlocke.org/CSIV/CSIV_Norms.html | retrieved 2026-08-06; the page serving the undergraduate norms table |
| note-only | — | U.S. adult sample | N = 980, Hopwood et al. (2022) | CSIV_Norms.html, adult norms table (retrieved 2026-08-06) |
| note-only | — | 32-item adult MTurk sample | N = 1,244, Locke & Adamic (2012) | CSIV_Norms.html, 32-item norms table (retrieved 2026-08-06) |
<!-- audit-values-end -->
