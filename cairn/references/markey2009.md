# markey2009 — the IPIP-IPC, and why its shipped norms are not from this article

**Provenance.** Ingested 2026-08-06 by M73 from
`cairn/references/sources/markey2009.pdf` (gitignored), 10 pages, printed
pp. 352–361. Pagination: the article's own page numbers. The PDF is
**born-digital** (`Creator: Adobe InDesign CS3 (5.0)`,
`Producer: Adobe PDF Library 8.0`, 4228 text characters per page) and
`pdfimages -list` reports **no images at all**, so the text layer is the whole
article and nothing can be hidden from it in raster — which is what makes the
absence findings below checkable. The two anchors this page extracts compared
values from, the Study 2 Participants paragraph (p. 357) and the Appendix
(p. 360), were each read in two independent channels, the `pdftotext` text
layer and a 200-dpi page-image render, the channels agreeing on every value.
Extraction: verified 2026-08-06 in both channels for every compared value; the
note-only Study 1 statistics below were read in the text layer only, which the
zero-image finding makes the article's own typeset text; no value read by a
second human channel — observed 2026-08-06.

**Citation.** Markey, P. M., & Markey, C. N. (2009). A brief assessment of the
interpersonal circumplex: The IPIP-IPC. *Assessment, 16*(4), 352–361.
DOI 10.1177/1073191109340382.

**Role.** The article that defines the IPIP-IPC and the source of its shipped
item-to-octant assignment, item text and response anchors. It is **not** the
source of the shipped IPIP-IPC octant means and standard deviations; see the
finding below.

**Finding: the shipped M and SD match no table in this article.** The article
publishes octant descriptive statistics exactly once, on p. 354, for Study 1's
*combined* sample: "PA: M = 2.21, SD = .77; BC: M = 2.09, SD = .73; DE:
M = 2.24, SD = .69; FG: M = 2.81, SD = .80; HI: M = 3.19, SD = .70; JK:
M = 3.80, SD = .57; LM: M = 3.92, SD = .57; NO: M = 3.17, SD = .77" — where
Study 1's two samples are n = 251 and n = 250 (Table 1, p. 355). The shipped
values are different (PA 2.66/0.71 through LM 4.37/0.47). The shipped `Size` of
274 is **Study 2's** n, and Study 2's entire Results section (p. 357) reports
only composite reliabilities, principal-component eigenvalues and a
randomization test — it publishes no octant mean or standard deviation. The
sweep behind the absence claim: a full read of the article's text layer, the
zero-image finding above ruling out a rasterized table, a page-image read of
p. 357 confirming Study 2's Results in the second channel, and `grep -F` for
`4.37`, `2.66` and `3.64` each returning zero hits. The shipped IPIP-IPC M and
SD therefore have no identified published source; the package's own history
records only the article's DOI as their `@source` (2018-09-03), and its
roxygen already attributed them to the 274-participant sample.

**On the angles.** The article gives each octant a theoretical location on the
circle and reports FFM trait angular locations in degrees (Table 3, p. 357),
but publishes no assigned degree per octant scale, so the `Angle` rows below
are recorded as not published.

**Note-only rows** (published by the source, not shipped): the Study 1 combined
octant M and SD quoted above, which the package does not ship for any sample.

## Extracted values

The `Items` rows are the article's own item numbering: its Appendix numbers the
32 items 1–32 and prints each item's octant as an italicized letter pair in
parentheses, so both the numbering and the grouping are source-side here.

<!-- audit-values-begin -->
| field | scale | value | anchor |
|---|---|---|---|
| M | PA | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | BC | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | DE | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | FG | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | HI | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | JK | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | LM | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| M | NO | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant M in the article is Study 1's, p. 354 — see the Finding above |
| SD | PA | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | BC | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | DE | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | FG | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | HI | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | JK | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | LM | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| SD | NO | not-published-in-source | Study 2 (the shipped sample) publishes no descriptive statistics; the only octant SD in the article is Study 1's, p. 354 — see the Finding above |
| Angle | PA | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | BC | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | DE | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | FG | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | HI | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | JK | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | LM | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Angle | NO | not-published-in-source | no assigned degree per octant is printed; degrees are package convention (IP2) |
| Items | PA | 6, 14, 22, 30 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; PA items 6, 14, 22, 30 |
| Items | BC | 7, 15, 23, 31 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; BC items 7, 15, 23, 31 |
| Items | DE | 8, 16, 24, 32 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; DE items 8, 16, 24, 32 |
| Items | FG | 1, 9, 17, 25 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; FG items 1, 9, 17, 25 |
| Items | HI | 2, 10, 18, 26 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; HI items 2, 10, 18, 26 |
| Items | JK | 3, 11, 19, 27 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; JK items 3, 11, 19, 27 |
| Items | LM | 4, 12, 20, 28 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; LM items 4, 12, 20, 28 |
| Items | NO | 5, 13, 21, 29 | Appendix, p. 360, items numbered 1-32 each tagged with its octant in parentheses; NO items 5, 13, 21, 29 |
| Size | — | 274 | p. 357, Study 2 Participants and Procedure, 'Data were collected from 274 undergraduate students' |
| Population | — | undergraduate students from a private Northeastern university in the Philadelphia area | p. 357, Study 2 Participants and Procedure |
| Reference | — | Markey & Markey (2009) | p. 352, the first three lines of the stacked masthead block, 'Assessment' above 'Volume 16 Number 4' above 'December 2009 352-361' (hyphen, as set), with the byline 'Patrick M. Markey' above 'Charlotte N. Markey', each printed above its affiliation and carrying no superscript |
| URL | — | https://doi.org/10.1177/1073191109340382 | p. 352, '10.1177/1073191109340382' printed in the journal header |
| note-only | Study 1 combined octant statistics | PA 2.21 / .77, BC 2.09 / .73, DE 2.24 / .69, FG 2.81 / .80, HI 3.19 / .70, JK 3.80 / .57, LM 3.92 / .57, NO 3.17 / .77 | p. 354, 'for the combined sample, PA: M = 2.21, SD = .77; ...' — a different sample from the one the package ships |
<!-- audit-values-end -->

## Traces to

- `data-raw/ipipipc.R` — every shipped IPIP-IPC item assignment and provenance
  string compared against the block above; its M and SD are recorded here as
  unsourced. The shipped response anchors were read against p. 360 by hand and
  agree, but `shipped_values()` enumerates no `Anchors` field, so no row of the
  block or the ledger records that comparison.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  ipipipc comparison.
- `cairn/references/norms-audit.md` — carries ipipipc's audit verdict and this
  file's shelf-manifest row.
- `R/instrument_data.R` — `?ipipipc`'s `@source` entries, which this page's
  finding required rewording.
