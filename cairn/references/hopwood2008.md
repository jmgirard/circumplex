# hopwood2008 — the IIP-SC's college-student norms (iipsc sample 1)

**Provenance.** Ingested 2026-08-07 by M74 from
`cairn/references/sources/hopwood2008.pdf` (gitignored), 5 PDF pages: page 1 is
a Taylor & Francis cover sheet and pages 2–5 are the article's printed
pp. 615–618. Pagination: the article's own page numbers; the cover sheet is
cited as such where it is the anchor. The PDF is **born-digital**
(`Creator: dvips(k) 5.95a`, `Producer: Acrobat Distiller 6.0.1 (Windows)`,
3825 text characters per page), not an OCR scan, and Table 1 is typeset table
text. It was read in two independent channels anyway — the `pdftotext -layout`
text layer and a 300-dpi page-image render — because Table 1 stacks two
instruments' statistics in one column block and a misread row label would
attribute the IIP-C's numbers to the IIP-SC.
Extraction: verified 2026-08-07 — Table 1 (p. 616) read in both channels, which
agree on every value and on which block each row belongs to; no value read by a
second human channel — observed 2026-08-07.

**Citation.** Hopwood, C. J., Pincus, A. L., DeMoor, R. M., & Koonce, E. A.
(2008). Psychometric characteristics of the Inventory of Interpersonal
Problems–Short Circumplex (IIP–SC) with college students. *Journal of
Personality Assessment, 90*(6), 615–618. DOI 10.1080/00223890802388665. The
citekey follows the article's own year: the cover sheet's "To cite this article"
line reads "(2008) ... Journal of Personality Assessment, 90:6, 615-618", and
the package currently credits these norms to a 2011 publication.

**Role.** The published source for iipsc's **sample 1**, the college-student
norms. It is not the source for iipsc's sample 2, which is `soldz1995`, and it
publishes neither the item list nor any angle assignment — it takes the IIP-SC
itself from Soldz et al. (1995), so the instrument-level Angle and Items rows
live in that note.

**On the sample.** The article runs two college samples — Sample 1, 475
undergraduates who took only the IIP-SC (p. 615), and Sample 2, 397
undergraduates who took the full IIP-C with the IIP-SC items embedded (p. 615)
— and Table 1's IIP-SC block carries footnote b, "N = 872", which is the two
combined (475 + 397). The shipped `Size` of 872 is therefore the pooled college
sample, not either of the article's own numbered samples, and the article's
"Sample 1"/"Sample 2" are not the package's.

**On the unit.** Table 1's IIP-SC means are octant **sums** over four 0–4 items,
not item means: its Total column gives M = 35.15, which is the eight octant
means added up (35.18, to rounding), and the surrounding text compares "the
average item score" against Soldz et al.'s clinical item means rather than
against these numbers. `data-raw/iipsc.R` divides each value by 4 to express it
on the instrument's own 0–4 anchor range, which is the divisor the audit batch
carries for this sample. The values below are recorded as the source prints
them, undivided.

## Extracted values

All from Table 1 (p. 616), the row block headed `IIP–SC`. Sample 1 here is the
package's sample 1; this note tables no other sample.

<!-- audit-values-begin -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 3.04 | Table 1, p. 616, IIP-SC block, M row, PA column |
| M | 1 | BC | 3.17 | Table 1, p. 616, IIP-SC block, M row, BC column |
| M | 1 | DE | 3.60 | Table 1, p. 616, IIP-SC block, M row, DE column |
| M | 1 | FG | 4.19 | Table 1, p. 616, IIP-SC block, M row, FG column |
| M | 1 | HI | 5.68 | Table 1, p. 616, IIP-SC block, M row, HI column |
| M | 1 | JK | 5.54 | Table 1, p. 616, IIP-SC block, M row, JK column |
| M | 1 | LM | 5.86 | Table 1, p. 616, IIP-SC block, M row, LM column |
| M | 1 | NO | 4.10 | Table 1, p. 616, IIP-SC block, M row, NO column |
| SD | 1 | PA | 2.64 | Table 1, p. 616, IIP-SC block, SD row, PA column |
| SD | 1 | BC | 2.76 | Table 1, p. 616, IIP-SC block, SD row, BC column |
| SD | 1 | DE | 3.42 | Table 1, p. 616, IIP-SC block, SD row, DE column |
| SD | 1 | FG | 3.79 | Table 1, p. 616, IIP-SC block, SD row, FG column |
| SD | 1 | HI | 3.66 | Table 1, p. 616, IIP-SC block, SD row, HI column |
| SD | 1 | JK | 3.41 | Table 1, p. 616, IIP-SC block, SD row, JK column |
| SD | 1 | LM | 3.30 | Table 1, p. 616, IIP-SC block, SD row, LM column |
| SD | 1 | NO | 3.20 | Table 1, p. 616, IIP-SC block, SD row, NO column |
| Size | 1 | — | 872 | Table 1, p. 616, footnote b, 'N = 872', attached to the IIP-SC block; equals the article's Sample 1 (475) plus Sample 2 (397), both given on p. 615 |
| Population | 1 | — | 872 college undergraduates pooled across two samples: 475 (65% women) from a large Southwestern university, modal age 18 (range 18-23), and 397 (51% women) from a Midwestern University, modal age 18 (range 18-33); more than 80% White in both | p. 615, Sample 1, 'The IIP-SC was administered to 475 (65% women) undergraduates from a large Southwestern university whose modal age was 18 years (range = 18-23)', and Sample 2, 'We administered the full IIP-C to 397 college undergraduates (51% women) from a Midwestern University' |
| Reference | 1 | — | Hopwood, Pincus, DeMoor, & Koonce (2008) | cover sheet, 'To cite this article: Christopher J. Hopwood, Aaron L. Pincus, Rebecca M. DeMoor & Elizabeth A. Koonce (2008) ... Journal of Personality Assessment, 90:6, 615-618'; the same byline is printed on p. 615 |
| URL | 1 | — | https://doi.org/10.1080/00223890802388665 | cover sheet, 'To link to this article: https://doi.org/10.1080/00223890802388665'; the same DOI appears in the cover sheet's citation line |
<!-- audit-values-end -->

## Traces to

- `data-raw/iipsc.R` — the shipped sample-1 M, SD, `Size`, `Population`,
  `Reference` and `URL`, and the `/ 4` that converts these octant sums to item
  means.
- `data-raw/audit-norms.R` — parses the block above as the source side of
  iipsc's sample-1 pass, with `divisor = 4`.
- `cairn/references/soldz1995.md` — the source for iipsc's sample 2, and for
  the instrument-level item grouping this article does not print.
- `cairn/references/norms-audit.md` — carries iipsc's audit verdict and this
  file's shelf-manifest row.
