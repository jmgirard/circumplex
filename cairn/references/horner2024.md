# horner2024 — the IEI and its two normative samples

**Provenance.** Ingested 2026-08-07 by M74 from two sources, both shelved under
`cairn/references/sources/` (gitignored):

- `horner2025.pdf`, 18 PDF pages whose printed numbers run pp. 1–18. This is the
  **online-first** version, and it prints 2024 throughout — "Accepted 12 August
  2024", "© 2024 Taylor & Francis Group, LLC", and a DOI whose stem is
  `2024.2400266` — with continuous pagination from 1 rather than an issue's page
  range. The version of record came later: `R/instrument_data.R` cites it as
  *Journal of Personality Assessment, 107*(2), 170–187 (2025), which is what the
  shelf filename records. The citekey follows the copy on the shelf, and every
  page anchor below is that copy's pagination, not the issue's.
  Pagination: the online-first copy's own page numbers. The PDF is **born-digital**
  (`Producer: Zotero`, 5404 text characters per page), not an OCR scan, and
  Table 1 is typeset table text.
- `locke_iei_norms_2026-08-07.html` (sha256 50230f1b…), retrieved 2026-08-07
  from `https://kennethlocke.org/IEI/IEI_Norms.html`. Unpaginated: `—`. This is
  the author's own IEI norms page, which publishes the Study 1 octant means and
  SDs and credits the article for them. M74-D1 chose it as the shipped `URL`
  anchor for the sample-1 norms.

Extraction: verified 2026-08-07 — Table 1 (p. 6) read in both the `pdftotext`
text layer and a 220-dpi page-image render, and the norms page read in both a
raw-HTML tag-stripped extraction and a model-mediated fetch; the channels agree
on every value, and the page's eight means and SDs agree with Table 1's Study 1
column value for value; no value read by a second human channel — observed
2026-08-07.

**Citation.** Horner, M. S., Locke, K. D., & Hulsey, T. L. (2024). Assessing
affective dimensions of the interpersonal circumplex: Development and
validation of the Interpersonal Emotion Inventory. *Journal of Personality
Assessment*. DOI 10.1080/00223891.2024.2400266.

**Role.** The published source for every shipped IEI norm value. Sample 1 is
the article's Study 1 (N = 1223 undergraduates) and sample 2 its Study 2
(N = 278 community participants); Table 1 (p. 6) prints both side by side. The
norms page republishes the sample-1 half.

**On a collision in the word "sample".** The article's Study 1 is itself built
from two undergraduate subsamples it calls "Sample 1" (n = 438 after exclusions,
University of Tennessee) and "Sample 2" (n = 785 after exclusions, University of
Tennessee and University of Idaho), pp. 4–5, which sum to the 1,223 of Table 1's
Study 1 column. Those are **not** the package's samples. The package's sample 1
is the article's Study 1 as a whole and its sample 2 is the article's Study 2,
and every row below uses the package's numbering.

**On the items and angles.** The article does not print the 64-item IEI or its
item-to-octant assignment — the final item set is "detailed in Supplemental
Table S2" (p. 6), which is not part of the article — so the Items rows are
recorded as not published. No assigned degree per octant is printed either: the
article states the octants each reflect a 45° segment and states hypotheses in
degree ranges, but never tabulates a degree per IEI scale, and Table 2's
Procrustes-rotated loadings are estimates rather than an assignment.

**On the two URL anchors.** The norms page publishes the sample-1 values and
nothing of Study 2, so it can only anchor sample 1. Sample 2's values are
published in the article and nowhere else, so its anchor below is the article's
own DOI. The shipped `URL` is currently the same OSF project link for both
samples — the article's open-data link, `http://doi.org/10.17605/OSF.IO/W37DJ`
(p. 17), which resolves to `https://osf.io/w37dj/` — which publishes neither
table.

## Extracted values

Sample 1 = the article's Study 1, sample 2 = its Study 2, both from Table 1
(p. 6). The sample-1 M and SD are also printed on the norms page, and the two
agree; the anchors name both.

<!-- audit-values-begin -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 2.00 | Table 1, p. 6, '(PA) +A Confident-Impressive' row, Study 1 M column; same value on the norms page, 'PA (+A)' row |
| M | 1 | BC | 1.21 | Table 1, p. 6, '(BC) +A-C Superior-Callous' row, Study 1 M column; same value on the norms page, 'BC (+A-C)' row |
| M | 1 | DE | 0.91 | Table 1, p. 6, '(DE) -C Rejecting-Suspicious' row, Study 1 M column; same value on the norms page, 'DE (-C)' row |
| M | 1 | FG | 1.18 | Table 1, p. 6, '(FG) -A-C Rejected-Ashamed' row, Study 1 M column; same value on the norms page, 'FG (-A-C)' row |
| M | 1 | HI | 2.03 | Table 1, p. 6, '(HI) -A Insecure-Anxious' row, Study 1 M column; same value on the norms page, 'HI (-A)' row |
| M | 1 | JK | 2.63 | Table 1, p. 6, '(JK) -A + C Needy-Empathic' row, Study 1 M column; same value on the norms page, 'JK (-A+C)' row |
| M | 1 | LM | 2.70 | Table 1, p. 6, '(LM) +C Welcoming-Trusting' row, Study 1 M column; same value on the norms page, 'LM (+C)' row |
| M | 1 | NO | 2.41 | Table 1, p. 6, '(NO) +A + C Included-Proud' row, Study 1 M column; same value on the norms page, 'NO (+A+C)' row |
| SD | 1 | PA | .71 | Table 1, p. 6, '(PA) +A' row, Study 1 SD column; norms page prints 0.71 |
| SD | 1 | BC | .61 | Table 1, p. 6, '(BC) +A-C' row, Study 1 SD column; norms page prints 0.61 |
| SD | 1 | DE | .68 | Table 1, p. 6, '(DE) -C' row, Study 1 SD column; norms page prints 0.68 |
| SD | 1 | FG | .84 | Table 1, p. 6, '(FG) -A-C' row, Study 1 SD column; norms page prints 0.84 |
| SD | 1 | HI | .86 | Table 1, p. 6, '(HI) -A' row, Study 1 SD column; norms page prints 0.86 |
| SD | 1 | JK | .60 | Table 1, p. 6, '(JK) -A + C' row, Study 1 SD column; norms page prints 0.60 |
| SD | 1 | LM | .66 | Table 1, p. 6, '(LM) +C' row, Study 1 SD column; norms page prints 0.66 |
| SD | 1 | NO | .73 | Table 1, p. 6, '(NO) +A + C' row, Study 1 SD column; norms page prints 0.73 |
| M | 2 | PA | 1.82 | Table 1, p. 6, '(PA) +A Confident-Impressive' row, Study 2 M column |
| M | 2 | BC | 1.22 | Table 1, p. 6, '(BC) +A-C Superior-Callous' row, Study 2 M column |
| M | 2 | DE | 1.08 | Table 1, p. 6, '(DE) -C Rejecting-Suspicious' row, Study 2 M column |
| M | 2 | FG | 1.30 | Table 1, p. 6, '(FG) -A-C Rejected-Ashamed' row, Study 2 M column |
| M | 2 | HI | 1.83 | Table 1, p. 6, '(HI) -A Insecure-Anxious' row, Study 2 M column |
| M | 2 | JK | 2.37 | Table 1, p. 6, '(JK) -A + C Needy-Empathic' row, Study 2 M column |
| M | 2 | LM | 2.43 | Table 1, p. 6, '(LM) +C Welcoming-Trusting' row, Study 2 M column |
| M | 2 | NO | 2.20 | Table 1, p. 6, '(NO) +A + C Included-Proud' row, Study 2 M column |
| SD | 2 | PA | .79 | Table 1, p. 6, '(PA) +A' row, Study 2 SD column |
| SD | 2 | BC | .53 | Table 1, p. 6, '(BC) +A-C' row, Study 2 SD column |
| SD | 2 | DE | .66 | Table 1, p. 6, '(DE) -C' row, Study 2 SD column |
| SD | 2 | FG | .89 | Table 1, p. 6, '(FG) -A-C' row, Study 2 SD column |
| SD | 2 | HI | .90 | Table 1, p. 6, '(HI) -A' row, Study 2 SD column |
| SD | 2 | JK | .51 | Table 1, p. 6, '(JK) -A + C' row, Study 2 SD column |
| SD | 2 | LM | .68 | Table 1, p. 6, '(LM) +C' row, Study 2 SD column |
| SD | 2 | NO | .79 | Table 1, p. 6, '(NO) +A + C' row, Study 2 SD column |
| Angle | — | PA | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | BC | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | DE | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | FG | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | HI | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | JK | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | LM | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Angle | — | NO | not-published-in-source | no assigned degree per octant is printed; the article gives 45-degree segment widths and degree-range hypotheses only, and Table 2's loadings are estimates (IP2) |
| Items | — | PA | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | BC | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | DE | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | FG | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | HI | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | JK | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | LM | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Items | — | NO | not-published-in-source | the 64 items are 'detailed in Supplemental Table S2' (p. 6), which is not part of the article; Table 1 prints one example item per octant |
| Size | 1 | — | 1223 | Table 1, p. 6, table note, 'Study 1 N = 1223'; also the abstract, p. 1, 'an initial development study (N = 1223 undergraduates)', and the norms page, 'the 1,223 U.S. undergraduate participants in Study 1' |
| Size | 2 | — | 278 | Table 1, p. 6, table note, 'Study 2 N = 278'; also the abstract, p. 1, 'a preregistered validation study (N = 278 community participants)', and p. 12, 'the final sample size was 278' |
| Population | 1 | — | 1,223 U.S. undergraduates from the University of Tennessee and the University of Idaho, pooled from two subsamples of 438 and 785 after exclusions | pp. 4-5, Participants, 'Undergraduates (n = 510) attending the University of Tennessee' with 'the final sample consisted of 438 participants', and 'Undergraduates attending the University of Tennessee (n = 621) or University of Idaho (n = 268)' with 'the final sample size was 785'; the norms page calls them '1,223 U.S. undergraduate participants in Study 1' |
| Population | 2 | — | 278 adults at least 18 years old, English-speaking and residing in the United States, recruited and compensated via prolific.com | p. 12, Participants, 'Participants who were at least 18 years old, English-speaking, and residing in the United States were recruited and compensated via the prolific.com platform' |
| Reference | 1 | — | Horner, Locke, & Hulsey (2024) | the norms page prints the credit verbatim, 'Horner, M. S., Locke, K. D., & Hulsey, T. L. (2024)...', directly above the norms table; also p. 1, byline 'Mark S. Horner, Kenneth D. Locke and Timothy L. Hulsey' |
| Reference | 2 | — | Horner, Locke, & Hulsey (2024) | p. 1, byline 'Mark S. Horner, Kenneth D. Locke and Timothy L. Hulsey' over their three affiliations, with the year from the same page's '© 2024 Taylor & Francis Group, LLC' and 'Accepted 12 August 2024' |
| URL | 1 | — | https://kennethlocke.org/IEI/IEI_Norms.html | the page publishing the sample-1 norms table, retrieved 2026-08-07 and shelved as `locke_iei_norms_2026-08-07.html`; chosen as this sample's anchor by M74-D1 |
| URL | 2 | — | https://doi.org/10.1080/00223891.2024.2400266 | p. 1, 'https://doi.org/10.1080/00223891.2024.2400266' printed under the journal name; the norms page publishes nothing of Study 2, so the article is this sample's only anchor |
<!-- audit-values-end -->

## Traces to

- `data-raw/iei.R` — every shipped IEI norm value and provenance string
  compared against the block above.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  iei comparison, one pass per sample.
- `cairn/references/norms-audit.md` — carries iei's audit verdict and this
  file's two shelf-manifest rows.
- `cairn/milestones/M74-norms-provenance-audit-batch3.md` — M74-D1, the choice
  of the norms page as the sample-1 `URL` anchor.
