# hatcher2012 — the IIS-32, and why its shipped norms are not from this article

**Provenance.** Ingested 2026-08-06 by M73 from
`cairn/references/sources/hatcher2012.pdf` (gitignored), 9 pages, printed
pp. 638–646. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Creator: dvips(k) 5.95a`, `Producer: iText 4.2.0 by 1T3XT`,
5609 text characters per page), not an OCR scan; `pdfimages -list` reports two
images only, a 750x204 rule on p. 638 and an 875x671 figure on p. 639, neither
full-page, so no table on this article's table pages is a rasterized image. The
Appendix on p. 646 — the one anchor this page extracts values from — was read in
two independent channels, the `pdftotext` text layer and a 300-dpi page-image
render, the channels agreeing on every octant block.
Extraction: verified 2026-08-06 — the Appendix values below read in both
channels and agreeing; the absence findings below verified by the bounded sweep
they name; no value read by a second human channel — observed 2026-08-06.

**Citation.** Hatcher, R. L., & Rogers, D. T. (2012). The IIS–32: A brief
inventory of interpersonal strengths. *Journal of Personality Assessment,
94*(6), 638–646. DOI 10.1080/00223891.2012.681818.

**Role.** The article that defines the IIS-32 and the source of its shipped
item-to-octant grouping. It is **not** the source of the shipped IIS-32 norms;
see the finding below.

**Finding: the shipped norm sample is not published here.** The article
contains six tables — Table 1 principal axis factoring (p. 641), Table 2
circumplex fit (p. 641), Table 3 octant polar angles and communalities
(p. 642), Table 4 reliabilities (p. 642), Table 5 IIS-64 and IIS-32
intercorrelations (p. 643), Table 6 correlations with Big Five dimensions
(p. 644). **None of them is a descriptive-statistics table**, and no octant mean
or standard deviation is printed anywhere in the article. Its samples are
N = 1,377 (development, p. 640), N = 956 (validation) and N = 497 (clinical),
per Table 2 on p. 641; the shipped normative `Size` of 1380 appears nowhere.
The sweep behind these absence claims, stated so a later reader can rerun it:
a full read of the article's text layer, an enumeration of every `TABLE`
caption in it (the six above), the `pdfimages -list` result above ruling out a
rasterized seventh table, and `grep -F` for each shipped value — `4.25`, `4.66`,
`1380`, `1,380` — each returning zero hits. The shipped IIS-32 M, SD and sample
size therefore have no identified published source; the package's own history
records only that they were added to "match author version" (2018-10-29), with
the underlying correspondence no longer recoverable.

**On the angles.** Table 3 (p. 642) publishes octant polar angles, but as
CIRCUM *estimates* per sample with Connect fixed at 0° — for the development
sample, Engage 41°, Lead 87°, Direct 133°, Balance 183°, Restrain 230°,
Cooperate 273°, Consider 314°. Those are measured locations, not an assigned
degree convention, so the `Angle` rows below are recorded as not published.

**On the item numbering.** The Appendix (p. 646) groups the 32 items by octant
as item *text* and assigns no numbers, so the `Items` rows below carry a number
string derived by matching each Appendix line onto the package's own `Items`
table. Those rows test the source's **grouping**, not its numbering, and the two
sides share the shipped item-text table as the key that joins them. All 32
assignments were matched line by line in both channels.

**Note-only rows** (published by the source, not shipped): the three study
samples above, Table 3's per-sample CIRCUM angle and communality estimates, and
Table 4's octant alpha reliabilities, none of which the package ships.

## Extracted values

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | BC | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | DE | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | FG | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | HI | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | JK | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | LM | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| M | NO | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | PA | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | BC | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | DE | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | FG | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | HI | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | JK | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | LM | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| SD | NO | not-published-in-source | no descriptive-statistics table exists in the article; see the Finding above for the sweep |
| Angle | PA | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Lead 87 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Direct 133 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Balance 183 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Restrain 230 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Cooperate 273 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Consider 314 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | Table 3, p. 642 fixes Connect at 0 deg as the CIRCUM reference, not as an assigned degree — degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | Table 3, p. 642 publishes CIRCUM estimates (Engage 41 deg, development sample) with Connect fixed at 0 deg, not assigned degrees — degrees are package convention (IP2) |
| Items | PA | 14, 16, 18, 20 | Appendix, p. 646, 'Lead' block — grouping only; numbers are the package's own item ordering |
| Items | BC | 3, 10, 19, 25 | Appendix, p. 646, 'Direct' block — grouping only; numbers are the package's own item ordering |
| Items | DE | 6, 8, 26, 28 | Appendix, p. 646, 'Balance' block — grouping only; numbers are the package's own item ordering |
| Items | FG | 11, 24, 29, 30 | Appendix, p. 646, 'Restrain' block — grouping only; numbers are the package's own item ordering |
| Items | HI | 5, 17, 27, 31 | Appendix, p. 646, 'Cooperate' block — grouping only; numbers are the package's own item ordering |
| Items | JK | 7, 13, 21, 22 | Appendix, p. 646, 'Consider' block — grouping only; numbers are the package's own item ordering |
| Items | LM | 2, 9, 15, 23 | Appendix, p. 646, 'Connect' block — grouping only; numbers are the package's own item ordering |
| Items | NO | 1, 4, 12, 32 | Appendix, p. 646, 'Engage' block — grouping only; numbers are the package's own item ordering |
| Size | — | not-published-in-source | the shipped 1380 appears nowhere in the article; its samples are 1,377 / 956 / 497 per Table 2, p. 641, and the Finding above records the sweep |
| Population | — | not-published-in-source | the shipped norm sample is unidentified, so the article describes no population for it; its development sample is described on p. 640 |
| Reference | — | Hatcher & Rogers (2012) | p. 638, journal header 'Journal of Personality Assessment, 94(6), 638-646, 2012' with the byline 'ROBERT L. HATCHER AND DANIEL T. ROGERS' |
| URL | — | https://doi.org/10.1080/00223891.2012.681818 | p. 638, 'DOI: 10.1080/00223891.2012.681818' printed in the journal header |
| note-only | study samples | development N = 1,377, validation N = 956, clinical N = 497 | Table 2, p. 641, 'Fit of circumplex model to Inventory of Interpersonal Strengths (IIS-32) data' |
| note-only | octant alphas | per-sample octant and full-scale alpha reliabilities for both IIS forms | Table 4, p. 642, 'Reliabilities of Inventory of Interpersonal Strengths-64 and Inventory of Interpersonal Strengths-32 in the samples' |
<!-- audit-values-end -->

## Traces to

- `data-raw/iis32.R` — every shipped IIS-32 item grouping and provenance string
  compared against the block above; its M, SD and Size are recorded here as
  unsourced.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  iis32 comparison.
- `cairn/references/norms-audit.md` — carries iis32's audit verdict and this
  file's shelf-manifest row.
- `R/instrument_data.R` — `?iis32`'s `@source` entries, which this page's
  finding required rewording.
