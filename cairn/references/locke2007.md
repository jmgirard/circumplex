# locke2007 — the CSIE, and where its shipped norms actually come from

**Provenance.** Ingested 2026-08-06 by M72 from
`cairn/references/sources/locke2007.pdf` (gitignored), 16 pages, printed
pp. 94–109. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Creator: PScript5.dll`, `Producer: Acrobat Distiller 6.0`),
not an OCR scan, so its `pdftotext` text layer is the typeset text itself and
M42-D1's two-channel requirement does not fire; the tables below were
nonetheless read in both the text layer and a page-image render, because a
table's *structure* can be reconstructed wrongly even where every glyph is
faithful. Two further sources are covered here and shelved beside it:
`locke_csie_norms_2026-08-06.html` (sha256 1165786f…) and the CSIE scoring
page, both retrieved 2026-08-06 from kennethlocke.org.
Extraction: verified 2026-08-06 — every value below read in two independent
channels (a model-mediated fetch and a raw-HTML tag-stripped extraction for the
web pages; text layer and page image for the PDF), the channels agreeing on
every value; no value read by a second human channel — observed 2026-08-06.

**Citation.** Locke, K. D., & Sadler, P. (2007). Self-efficacy, values, and
complementarity in dyadic interactions: Integrating interpersonal and
social-cognitive theory. *Personality and Social Psychology Bulletin, 33*(1),
94–109.

**Role.** The article is the CSIE's validation study and the origin of the
normative *sample*, but **it publishes no octant means or standard
deviations** — its Table 1 (p. 97) carries only Cronbach's α and the item text.
The shipped M/SD are published solely on Locke's website, whose norms table
states the sample is "the 367 undergraduates who participated in Study 1 of
Locke & Sadler (2007)"; the article's own Table 1 note gives that same n = 367.
So the package's `Reference` is correct for the sample and the article is not
the venue for the statistics — the finding that moved this instrument's `URL`
to the norms table (M72).

**Note-only rows** (published by the source, not shipped): the norms page also
tables an adult sample, N = 1,234, on a 0-to-4 scale, from Locke & Adamic
(2012) Studies 3–4. The package ships only the undergraduate 0-to-10 set.

## Extracted values

M/SD are the "All Participants" columns of the 0-to-10 undergraduate table;
the page also prints Women and Men columns, which the package does not ship.
Scale **angles are not published by any of these sources** — the octant
*ordering* is given (`PA (+A)` … `NO (+A+C)`), but the degree assignment is the
package's own convention (DESIGN.md IP2), so every Angle row below is
`not-published-in-source` rather than a verified match.

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | 7.23 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | BC | 6.44 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | DE | 6.93 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | FG | 7.24 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | HI | 7.31 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | JK | 8.51 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | LM | 7.90 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| M | NO | 7.30 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | PA | 1.68 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | BC | 1.66 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | DE | 1.82 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | FG | 1.54 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | HI | 1.53 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | JK | 1.11 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | LM | 1.20 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| SD | NO | 1.37 | CSIE_Norms.html, 'U.S. Undergraduate Student Norms (0-to-10 Scale)', All Participants column (retrieved 2026-08-06) |
| Angle | PA | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | not published — octant order given, degrees are package convention (IP2) |
| Items | PA | 4, 12, 20, 28 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | BC | 7, 15, 23, 31 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | DE | 2, 10, 18, 26 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | FG | 5, 13, 21, 29 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | HI | 8, 16, 24, 32 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | JK | 3, 11, 19, 27 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | LM | 6, 14, 22, 30 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Items | NO | 1, 9, 17, 25 | CSIE_Scoring_R.html, example R code item numbering (retrieved 2026-08-06) |
| Size | — | 367 | locke2007 Table 1 note, p. 97; and CSIE_Norms.html sample description |
| Population | — | undergraduates who participated in Study 1 of Locke & Sadler (2007) | CSIE_Norms.html sample description (retrieved 2026-08-06) |
<!-- audit-values-end -->
