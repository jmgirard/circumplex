# trucco2013 — the IGI-CR and its three normative samples

**Provenance.** Ingested 2026-08-07 by M74 from
`cairn/references/sources/trucco2013.pdf` (gitignored), 16 PDF pages whose
printed numbers run pp. 98–113. Pagination: the article's own page numbers.
The PDF is **born-digital** (`Creator: Adobe InDesign CS5.5 (7.5)`,
`Producer: Adobe PDF Library 9.9`, 4853 text characters per page), not an OCR
scan, and every value below is typeset table text. The three anchor pages were
read in two independent channels regardless — the `pdftotext -layout` text layer
and a 250-dpi page-image render — because Table 3 lays three samples side by
side in six numeric columns, which is the arrangement a reflowed text layer
misattributes if it misattributes anything.
Extraction: verified 2026-08-07 — Table 3 (p. 102), Table 5's target-angle row
(p. 104) and the item Appendix (p. 111) each read in both channels, the channels
agreeing on every value, on which sample each column belongs to, and on every
item's octant block; no value read by a second human channel — observed
2026-08-07.

**Citation.** Trucco, E. M., Wright, A. G. C., & Colder, C. R. (2013). A revised
Interpersonal Circumplex Inventory of Children's Social Goals.
*Assessment, 20*(1), 98–113. DOI 10.1177/1073191111411672.

**Role.** The published source for every shipped IGI-CR norm value, for all
three of the instrument's norm samples: sample 1 is the combined sample
(N = 387), sample 2 the males (n = 174) and sample 3 the females (n = 213), all
three printed side by side in Table 3 (p. 102).

**On the three samples.** They are not independent: samples 2 and 3 partition
sample 1 (174 + 213 = 387), and the article presents them as a gender breakdown
of one community sample rather than as three recruitments. The shipped
`Population` strings say so, and the article's own sample description (p. 100)
covers all three at once — it gives an age range of 11 to 13 and a mean age of
11.60 (SD = 0.55) for the combined sample only, and describes no separate
demographics for the male and female halves.

**On the angles.** Table 5 (p. 104) is titled "Estimated Angles and
Communalities From CIRCUM Models" and most of it is estimates, per sample and
per model — but its first row, "Target angle", is the assigned degree
convention rather than an estimate, and it is that row the Angle values below
are read from. LM (+C) is printed as 0, which the package ships as 360 under
IP2; the audit compares angles modulo 360, so the two agree. The CIRCUM
estimates are not comparable to a shipped angle and are not recorded here.

**On the scale names.** The article names its octants by their agentic/communal
composition (+A, +A−C, −C, −A−C, −A, −A+C, +C, +A+C), never by the PA–NO letter
pairs. The package ships both: the letter pair as `Abbrev` and the article's own
name as `Label`. Each row below gives the letter pair the package keys on and
names the article's octant in its anchor, so the correspondence is checkable
rather than assumed.

**On the item text and response format.** Not audited fields, but both were
compared while the Appendix was open, and both match: all 32 shipped item texts
are the Appendix's verbatim, and p. 101 prints the revised instrument's own stem
— "When with your peers, in general how important is it to you that . . .?" —
and its "5-point response scale ranging from 0 (not at all important to me) to 4
(extremely important to me)", which are what the package ships as `Prefix` and
as the endpoints of `Anchors`. The article prints no label for the three
intermediate anchor values.

## Extracted values

Sample 1 = Combined, sample 2 = Males, sample 3 = Females, all from Table 3
(p. 102). Item numbers in the Items rows are the Appendix's own printed item
numbers, which the package's item ordering reproduces.

<!-- audit-values-begin -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 2.09 | Table 3, p. 102, '+A' row, Combined M column |
| M | 1 | BC | 0.97 | Table 3, p. 102, '+A−C' row, Combined M column |
| M | 1 | DE | 1.51 | Table 3, p. 102, '−C' row, Combined M column |
| M | 1 | FG | 2.23 | Table 3, p. 102, '−A−C' row, Combined M column |
| M | 1 | HI | 2.38 | Table 3, p. 102, '−A' row, Combined M column |
| M | 1 | JK | 2.74 | Table 3, p. 102, '−A+C' row, Combined M column |
| M | 1 | LM | 2.68 | Table 3, p. 102, '+C' row, Combined M column |
| M | 1 | NO | 2.35 | Table 3, p. 102, '+A+C' row, Combined M column |
| SD | 1 | PA | 0.80 | Table 3, p. 102, '+A' row, Combined SD column |
| SD | 1 | BC | 0.66 | Table 3, p. 102, '+A−C' row, Combined SD column |
| SD | 1 | DE | 0.85 | Table 3, p. 102, '−C' row, Combined SD column |
| SD | 1 | FG | 0.91 | Table 3, p. 102, '−A−C' row, Combined SD column |
| SD | 1 | HI | 0.80 | Table 3, p. 102, '−A' row, Combined SD column |
| SD | 1 | JK | 0.76 | Table 3, p. 102, '−A+C' row, Combined SD column |
| SD | 1 | LM | 0.77 | Table 3, p. 102, '+C' row, Combined SD column |
| SD | 1 | NO | 0.76 | Table 3, p. 102, '+A+C' row, Combined SD column |
| M | 2 | PA | 2.13 | Table 3, p. 102, '+A' row, Males M column |
| M | 2 | BC | 1.11 | Table 3, p. 102, '+A−C' row, Males M column |
| M | 2 | DE | 1.68 | Table 3, p. 102, '−C' row, Males M column |
| M | 2 | FG | 2.24 | Table 3, p. 102, '−A−C' row, Males M column |
| M | 2 | HI | 2.32 | Table 3, p. 102, '−A' row, Males M column |
| M | 2 | JK | 2.64 | Table 3, p. 102, '−A+C' row, Males M column |
| M | 2 | LM | 2.52 | Table 3, p. 102, '+C' row, Males M column |
| M | 2 | NO | 2.29 | Table 3, p. 102, '+A+C' row, Males M column |
| SD | 2 | PA | 0.75 | Table 3, p. 102, '+A' row, Males SD column |
| SD | 2 | BC | 0.68 | Table 3, p. 102, '+A−C' row, Males SD column |
| SD | 2 | DE | 0.87 | Table 3, p. 102, '−C' row, Males SD column |
| SD | 2 | FG | 0.92 | Table 3, p. 102, '−A−C' row, Males SD column |
| SD | 2 | HI | 0.79 | Table 3, p. 102, '−A' row, Males SD column |
| SD | 2 | JK | 0.70 | Table 3, p. 102, '−A+C' row, Males SD column |
| SD | 2 | LM | 0.72 | Table 3, p. 102, '+C' row, Males SD column |
| SD | 2 | NO | 0.73 | Table 3, p. 102, '+A+C' row, Males SD column |
| M | 3 | PA | 2.06 | Table 3, p. 102, '+A' row, Females M column |
| M | 3 | BC | 0.85 | Table 3, p. 102, '+A−C' row, Females M column |
| M | 3 | DE | 1.37 | Table 3, p. 102, '−C' row, Females M column |
| M | 3 | FG | 2.23 | Table 3, p. 102, '−A−C' row, Females M column |
| M | 3 | HI | 2.43 | Table 3, p. 102, '−A' row, Females M column |
| M | 3 | JK | 2.82 | Table 3, p. 102, '−A+C' row, Females M column |
| M | 3 | LM | 2.81 | Table 3, p. 102, '+C' row, Females M column |
| M | 3 | NO | 2.40 | Table 3, p. 102, '+A+C' row, Females M column |
| SD | 3 | PA | 0.83 | Table 3, p. 102, '+A' row, Females SD column |
| SD | 3 | BC | 0.61 | Table 3, p. 102, '+A−C' row, Females SD column |
| SD | 3 | DE | 0.82 | Table 3, p. 102, '−C' row, Females SD column |
| SD | 3 | FG | 0.91 | Table 3, p. 102, '−A−C' row, Females SD column |
| SD | 3 | HI | 0.81 | Table 3, p. 102, '−A' row, Females SD column |
| SD | 3 | JK | 0.80 | Table 3, p. 102, '−A+C' row, Females SD column |
| SD | 3 | LM | 0.78 | Table 3, p. 102, '+C' row, Females SD column |
| SD | 3 | NO | 0.78 | Table 3, p. 102, '+A+C' row, Females SD column |
| Angle | — | PA | 90 | Table 5, p. 104, 'Target angle' row, '+A' column |
| Angle | — | BC | 135 | Table 5, p. 104, 'Target angle' row, '+A−C' column |
| Angle | — | DE | 180 | Table 5, p. 104, 'Target angle' row, '−C' column |
| Angle | — | FG | 225 | Table 5, p. 104, 'Target angle' row, '−A−C' column |
| Angle | — | HI | 270 | Table 5, p. 104, 'Target angle' row, '−A' column |
| Angle | — | JK | 315 | Table 5, p. 104, 'Target angle' row, '−A+C' column |
| Angle | — | LM | 0 | Table 5, p. 104, 'Target angle' row, '+C' column (printed 0; the package ships 360 under IP2 and the comparison is modulo 360) |
| Angle | — | NO | 45 | Table 5, p. 104, 'Target angle' row, '+A+C' column |
| Items | — | PA | 1, 9, 17, 25 | Appendix, p. 111, '+A' block, printed as 'Item 1 / Item 9 / Item 17 / Item 25' |
| Items | — | BC | 2, 10, 18, 26 | Appendix, p. 111, '+A−C' block, printed as 'Item 2 / Item 10 / Item18 / Item 26' (the third entry prints without the space) |
| Items | — | DE | 3, 11, 19, 27 | Appendix, p. 111, '−C' block, printed as 'Item 3 / Item 11 / Item 19 / Item 27' |
| Items | — | FG | 4, 12, 20, 28 | Appendix, p. 111, '−A−C' block, printed as 'Item 4 / Item 12 / Item 20 / Item 28' |
| Items | — | HI | 5, 13, 21, 29 | Appendix, p. 111, '−A' block, printed as 'Item 5 / Item 13 / Item 21 / Item 29' |
| Items | — | JK | 6, 14, 22, 30 | Appendix, p. 111, '−A+C' block, printed as 'Item 6 / Item 14 / Item 22 / Item 30' |
| Items | — | LM | 7, 15, 23, 31 | Appendix, p. 111, '+C' block, printed as 'Item 7 / Item 15 / Item 23 / Item 31' |
| Items | — | NO | 8, 16, 24, 32 | Appendix, p. 111, '+A+C' block, printed as 'Item 8 / Item 16 / Item 24 / Item 32' |
| Size | 1 | — | 387 | Table 3, p. 102, table note, 'Combined N = 387'; also p. 100, 'This community sample of 387 early adolescents', and p. 102 text, 'the combined (N = 387), male (n = 174), and female (n = 213) samples' |
| Size | 2 | — | 174 | Table 3, p. 102, table note, 'Males n = 174'; also p. 102 text, 'male (n = 174)' |
| Size | 3 | — | 213 | Table 3, p. 102, table note, 'Females n = 213'; also p. 102 text, 'female (n = 213)' |
| Population | 1 | — | a community sample of 387 early adolescents from Erie County, New York, aged 11 to 13 (M = 11.60, SD = 0.55), 55.0% female and 83.1% Caucasian | p. 100, Sample, 'This community sample of 387 early adolescents was part of a larger 3-year longitudinal study', with the age, gender and race percentages from Table 1, p. 100 |
| Population | 2 | — | the 174 male members of that community sample | Table 3, p. 102, 'Males' column heading and its note 'Males n = 174'; the article gives no demographics for this half separately from the combined sample described on p. 100 |
| Population | 3 | — | the 213 female members of that community sample | Table 3, p. 102, 'Females' column heading and its note 'Females n = 213'; the article gives no demographics for this half separately from the combined sample described on p. 100 |
| Reference | 1 | — | Trucco, Wright, & Colder (2013) | p. 98, byline 'Elisa M. Trucco1, Aidan G. C. Wright2, and Craig R. Colder1', with the year from the same page's '© The Author(s) 2013' and 'Assessment 20(1) 98–113' |
| Reference | 2 | — | Trucco, Wright, & Colder (2013) | p. 98, byline 'Elisa M. Trucco1, Aidan G. C. Wright2, and Craig R. Colder1', with the year from the same page's '© The Author(s) 2013' and 'Assessment 20(1) 98–113' |
| Reference | 3 | — | Trucco, Wright, & Colder (2013) | p. 98, byline 'Elisa M. Trucco1, Aidan G. C. Wright2, and Craig R. Colder1', with the year from the same page's '© The Author(s) 2013' and 'Assessment 20(1) 98–113' |
| URL | 1 | — | https://doi.org/10.1177/1073191111411672 | p. 98, 'DOI: 10.1177/1073191111411672' printed in the article's masthead block |
| URL | 2 | — | https://doi.org/10.1177/1073191111411672 | p. 98, 'DOI: 10.1177/1073191111411672' printed in the article's masthead block |
| URL | 3 | — | https://doi.org/10.1177/1073191111411672 | p. 98, 'DOI: 10.1177/1073191111411672' printed in the article's masthead block |
<!-- audit-values-end -->

## Traces to

- `data-raw/igicr.R` — every shipped IGI-CR norm value and provenance string
  compared against the block above.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  igicr comparison, one pass per sample.
- `cairn/references/norms-audit.md` — carries igicr's audit verdict and this
  file's shelf-manifest row.
