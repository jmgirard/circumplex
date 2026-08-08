# horowitz2003 — the IIP professional manual and its standardization sample

**Provenance.** Ingested 2026-08-07 by M75 from
`cairn/references/sources/IIP Manual.pdf` (gitignored), 118 PDF pages whose
printed numbers run 1–111; PDF pages 112–118 (the sample instrument and the
publisher's licensing pages) carry no printed number. Pagination: the manual's
own printed page numbers, which coincide with the PDF page indices throughout.
The PDF is **born-digital** (embedded subset TrueType fonts, a selectable text
layer, no image-only pages), not an OCR scan. Every page cited below was read
in two independent channels regardless — the `pdftotext -layout` text layer and
a 150-dpi page-image render — because Table 4.4 lays seven numeric columns side
by side across three pages and Table F.5 lays three two-column groups side by
side, which is the arrangement a reflowed text layer misattributes if it
misattributes anything.
Extraction: verified 2026-08-07 — pp. 27–29 (Table 4.4), p. 91 (Table F.5), pp. 57–59 (Appendix A) and pp. 101–102 (Appendix H) each read in both channels, the channels agreeing on every value, on which norm group each column belongs to, and on every item's scale; no value read by a second human channel — observed 2026-08-07.

**Citation.** Horowitz, L. M., Alden, L. E., Wiggins, J. S., & Pincus, A. L.
(2003). *Inventory of Interpersonal Problems manual: Includes IIP-64 and IIP-32
forms* (3rd ed.). Menlo Park, CA: Mind Garden. The manual carries two title
pages: p. 1 names the four authors and the publisher without an edition
statement, and p. 3 repeats the title with "Third Edition" beneath it. The
copyright page (p. 2) records that "This manual and normative data were
originally published by The Psychological Corporation, a Harcourt Assessment
Company" — the 2000 edition the package's `@source` cited until M75 — and prints
"Copyright © 2003" together with a separate "Normative data copyright © 2003"
line.

**Role.** The published source for every shipped IIP-64 and IIP-32 norm value —
all three norm groups of each instrument — and for both instruments' item-to-scale
keys. Two instruments, one source, so this page carries **two** audit-values
blocks, tagged `iip64` and `iip32`; `data-raw/audit-norms.R` selects the block
matching the batch row's instrument.

**Reproduction credit (required).** The Mind Garden publication agreement
(`cairn/references/sources/IIP Agreement.pdf`, condition (a)) licenses
"Normative Data (Means and SDs)" and requires this credit line on the same page
as any reproduced material. It is reproduced verbatim, including its own 2000
copyright year, which is the agreement's wording and not a claim about this
edition:

> "Reproduction by special permission of the Publisher, Mind Garden, Inc.,
> www.mindgarden.com from the Inventory of Interpersonal Problems by Leonard M.
> Horowitz, Lynn E. Alden, Jerry S. Wiggins, & Aaron L. Pincus. Copyright © 2000
> by Leonard M. Horowitz, Lynn E. Alden, Jerry S. Wiggins, & Aaron L. Pincus.
> Further Reproduction is prohibited without the Publisher's written consent."

The agreement licenses the means and SDs only. Item **text** is not licensed and
is not reproduced here or in the package; the item *numbers* below are the
scoring key, which the maintainer confirmed 2026-08-07 is permitted.

**On the normative sample.** One sample, three norm groups. The manual describes
"a national standardization sample of 800 cases representative of the U.S.
population of adults aged 18-89" (p. 25), stratified by race/ethnicity and
education within gender against 1999 U.S. Census figures, with equal numbers of
males and females (n = 100) in each of four age bands, recruited by random-digit
dialling in eight cities across four regions and tested in 1996. Table 4.5
(p. 29) reports significant gender effects on six of the eight scales, and p. 27
records the resulting decision "to create separate norms for each gender" — so
the three shipped samples are Overall (N = 800), Female (n = 400) and Male
(n = 400), not three recruitments. The package ships them as samples 1, 2 and 3
in that order for both instruments.

**On where the IIP-32 descriptives are.** They are **Table F.5, p. 91**, titled
"Means and Standard Deviations of IIP-32 Scale Scores: Males, Females and
Overall". It sits at the end of Appendix F, after that appendix's four T-score
conversion tables, rather than in chapter 4 beside the IIP-64's Table 4.4 — and
the manual's own table of contents lists Appendix F as Tables F.1–F.4 only,
omitting F.5 (p. 6). M75's plan gate concluded from chapter 4 and the contents
listing that the 3rd edition prints no IIP-32 descriptives at all; it does, and
all 48 shipped `iip32` values reconcile against them exactly.

**On the two divisors.** The manual prints raw **scale sums** for both
instruments: the IIP-64's eight scales have eight items each and the IIP-32's
have four, over a common 0–4 item anchor range. The package ships item means, so
`data-raw/iip64.R` divides Table 4.4's values by 8 and `data-raw/iip32.R`
divides Table F.5's by 4. Both divisors ride in `AUDIT_BATCH` and every shipped
value reconciles exactly at them.

**On the T-score tables, which the package does not use.** Appendices B, D, F
and G print per-scale lookup tables converting a raw or difference score to a
T-score, and for the IIP-32 those tables (F.1–F.4) are the manual's *operational*
norming path. `norm_standardize()` implements M/SD z-scoring instead, which is
what p. 30 describes — "A linear T-score transformation was performed … using
means and standard deviations specific to that norm group". The printed tables
are nevertheless not that linear transform at the tails (measured at M75's plan
gate over Appendix F: published-minus-linear T has median 0.3 points and maximum
4.0 outside the tables' 99 ceiling), and adopting them is outside the
reproduction permission in any case. No conversion table is reproduced here.

**On the scale names.** The manual numbers its scales 1–8 and names them by
content ("1. Domineering/Controlling"); the PA–NO letter pairs the package keys
on are printed in Appendix A as each scale's "Previous Scale Name" — Domineering
(PA), Vindictive (BC), Cold (DE), Socially Avoidant (FG), Nonassertive (HI),
Exploitable (JK), Overly Nurturant (LM), Intrusive (NO). Every anchor below
names the manual's own scale number so the correspondence is checkable rather
than assumed. The manual prints no octant angles anywhere, for either
instrument.

## Extracted values — IIP-64

Sample 1 = Overall (N = 800), sample 2 = Female (n = 400), sample 3 = Male
(n = 400); all M and SD from Table 4.4, which runs across pp. 27–29 (scales 1–3
on p. 27, scales 4–7 on p. 28, scale 8 on p. 29). Values are the manual's raw
scale sums; the package divides them by 8.

<!-- audit-values-begin: iip64 -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 4.9 | Table 4.4, p. 27, scale 1 mean row, Overall (N = 800) column |
| M | 1 | BC | 5.3 | Table 4.4, p. 27, scale 2 mean row, Overall (N = 800) column |
| M | 1 | DE | 5.7 | Table 4.4, p. 27, scale 3 mean row, Overall (N = 800) column |
| M | 1 | FG | 6.5 | Table 4.4, p. 28, scale 4 mean row, Overall (N = 800) column |
| M | 1 | HI | 7.4 | Table 4.4, p. 28, scale 5 mean row, Overall (N = 800) column |
| M | 1 | JK | 7.8 | Table 4.4, p. 28, scale 6 mean row, Overall (N = 800) column |
| M | 1 | LM | 8.2 | Table 4.4, p. 28, scale 7 mean row, Overall (N = 800) column |
| M | 1 | NO | 5.7 | Table 4.4, p. 29, scale 8 mean row, Overall (N = 800) column |
| SD | 1 | PA | 4.5 | Table 4.4, p. 27, scale 1 SD row, Overall (N = 800) column |
| SD | 1 | BC | 5.1 | Table 4.4, p. 27, scale 2 SD row, Overall (N = 800) column |
| SD | 1 | DE | 5.9 | Table 4.4, p. 27, scale 3 SD row, Overall (N = 800) column |
| SD | 1 | FG | 5.7 | Table 4.4, p. 28, scale 4 SD row, Overall (N = 800) column |
| SD | 1 | HI | 6.1 | Table 4.4, p. 28, scale 5 SD row, Overall (N = 800) column |
| SD | 1 | JK | 5.3 | Table 4.4, p. 28, scale 6 SD row, Overall (N = 800) column |
| SD | 1 | LM | 5.5 | Table 4.4, p. 28, scale 7 SD row, Overall (N = 800) column |
| SD | 1 | NO | 4.8 | Table 4.4, p. 29, scale 8 SD row, Overall (N = 800) column |
| Size | 1 | — | 800 | p. 25, "a national standardization sample of 800 cases"; Table 4.4 column head, p. 27 (N = 800) |
| Population | 1 | — | a national standardization sample of 800 U.S. adults aged 18–89, stratified by race/ethnicity and education within gender against 1999 Census figures | p. 25, Normative Sample, 'The IIP normative information presented in this manual is based on a national standardization sample of 800 cases representative of the U.S. population of adults aged 18-89' |
| Reference | 1 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 1 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| M | 2 | PA | 4.5 | Table 4.4, p. 27, scale 1 mean row, Female (n = 400) column |
| M | 2 | BC | 4.8 | Table 4.4, p. 27, scale 2 mean row, Female (n = 400) column |
| M | 2 | DE | 5.1 | Table 4.4, p. 27, scale 3 mean row, Female (n = 400) column |
| M | 2 | FG | 6.4 | Table 4.4, p. 28, scale 4 mean row, Female (n = 400) column |
| M | 2 | HI | 8.0 | Table 4.4, p. 28, scale 5 mean row, Female (n = 400) column |
| M | 2 | JK | 8.6 | Table 4.4, p. 28, scale 6 mean row, Female (n = 400) column |
| M | 2 | LM | 8.8 | Table 4.4, p. 28, scale 7 mean row, Female (n = 400) column |
| M | 2 | NO | 5.4 | Table 4.4, p. 29, scale 8 mean row, Female (n = 400) column |
| SD | 2 | PA | 4.1 | Table 4.4, p. 27, scale 1 SD row, Female (n = 400) column |
| SD | 2 | BC | 4.9 | Table 4.4, p. 27, scale 2 SD row, Female (n = 400) column |
| SD | 2 | DE | 5.6 | Table 4.4, p. 27, scale 3 SD row, Female (n = 400) column |
| SD | 2 | FG | 5.7 | Table 4.4, p. 28, scale 4 SD row, Female (n = 400) column |
| SD | 2 | HI | 6.1 | Table 4.4, p. 28, scale 5 SD row, Female (n = 400) column |
| SD | 2 | JK | 5.4 | Table 4.4, p. 28, scale 6 SD row, Female (n = 400) column |
| SD | 2 | LM | 5.5 | Table 4.4, p. 28, scale 7 SD row, Female (n = 400) column |
| SD | 2 | NO | 4.6 | Table 4.4, p. 29, scale 8 SD row, Female (n = 400) column |
| Size | 2 | — | 400 | Table 4.4 column head, p. 27 (Female, n = 400); Table 4.1 note, p. 26 ("400 females") |
| Population | 2 | — | the 400 female members of that standardization sample | p. 25 ('equal numbers of participants were selected by gender (n = 100) within each age group') with Table 4.1's note, p. 26 ('N = 800 (400 females, 400 males)'); the manual gives no demographics for this half separately |
| Reference | 2 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 2 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| M | 3 | PA | 5.3 | Table 4.4, p. 27, scale 1 mean row, Male (n = 400) column |
| M | 3 | BC | 5.8 | Table 4.4, p. 27, scale 2 mean row, Male (n = 400) column |
| M | 3 | DE | 6.3 | Table 4.4, p. 27, scale 3 mean row, Male (n = 400) column |
| M | 3 | FG | 6.6 | Table 4.4, p. 28, scale 4 mean row, Male (n = 400) column |
| M | 3 | HI | 6.8 | Table 4.4, p. 28, scale 5 mean row, Male (n = 400) column |
| M | 3 | JK | 7.1 | Table 4.4, p. 28, scale 6 mean row, Male (n = 400) column |
| M | 3 | LM | 7.7 | Table 4.4, p. 28, scale 7 mean row, Male (n = 400) column |
| M | 3 | NO | 5.9 | Table 4.4, p. 29, scale 8 mean row, Male (n = 400) column |
| SD | 3 | PA | 4.7 | Table 4.4, p. 27, scale 1 SD row, Male (n = 400) column |
| SD | 3 | BC | 5.2 | Table 4.4, p. 27, scale 2 SD row, Male (n = 400) column |
| SD | 3 | DE | 6.1 | Table 4.4, p. 27, scale 3 SD row, Male (n = 400) column |
| SD | 3 | FG | 5.7 | Table 4.4, p. 28, scale 4 SD row, Male (n = 400) column |
| SD | 3 | HI | 6.1 | Table 4.4, p. 28, scale 5 SD row, Male (n = 400) column |
| SD | 3 | JK | 5.1 | Table 4.4, p. 28, scale 6 SD row, Male (n = 400) column |
| SD | 3 | LM | 5.4 | Table 4.4, p. 28, scale 7 SD row, Male (n = 400) column |
| SD | 3 | NO | 5.0 | Table 4.4, p. 29, scale 8 SD row, Male (n = 400) column |
| Size | 3 | — | 400 | Table 4.4 column head, p. 27 (Male, n = 400); Table 4.1 note, p. 26 ("400 males") |
| Population | 3 | — | the 400 male members of that standardization sample | p. 25 ('equal numbers of participants were selected by gender (n = 100) within each age group') with Table 4.1's note, p. 26 ('N = 800 (400 females, 400 males)'); the manual gives no demographics for this half separately |
| Reference | 3 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 3 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| Angle | — | PA | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | BC | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | DE | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | FG | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | HI | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | JK | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | LM | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | NO | not-published-in-source | the manual prints no octant angles for either instrument |
| Items | — | PA | 17, 31, 44, 45, 50, 52, 57, 59 | Appendix A, p. 57, scale 1 (Previous Scale Name: Domineering (PA)) |
| Items | — | BC | 1, 22, 24, 29, 32, 40, 56, 64 | Appendix A, p. 57, scale 2 (Previous Scale Name: Vindictive (BC)) |
| Items | — | DE | 11, 15, 16, 20, 23, 27, 36, 60 | Appendix A, p. 57, scale 3 (Previous Scale Name: Cold (DE)) |
| Items | — | FG | 3, 7, 14, 18, 33, 35, 55, 62 | Appendix A, p. 58, scale 4 (Previous Scale Name: Socially Avoidant (FG)) |
| Items | — | HI | 5, 6, 8, 9, 12, 13, 19, 39 | Appendix A, p. 58, scale 5 (Previous Scale Name: Nonassertive (HI)) |
| Items | — | JK | 2, 10, 25, 34, 38, 42, 53, 61 | Appendix A, p. 58, scale 6 (Previous Scale Name: Exploitable (JK)) |
| Items | — | LM | 21, 28, 37, 46, 49, 51, 54, 63 | Appendix A, p. 59, scale 7 (Previous Scale Name: Overly Nurturant (LM)) |
| Items | — | NO | 4, 26, 30, 41, 43, 47, 48, 58 | Appendix A, p. 59, scale 8 (Previous Scale Name: Intrusive (NO)) |
<!-- audit-values-end -->

## Extracted values — IIP-32

Sample 1 = Overall, sample 2 = Females, sample 3 = Males; all M and SD from
Table F.5, p. 91. Values are the manual's raw scale sums over four items; the
package divides them by 4. Table F.5 prints no group sizes of its own — the
IIP-32 is scored from a subset of the same 64 items administered to the same
standardization sample (p. 24), so the sizes below are anchored to the sample
description rather than to Table F.5.

<!-- audit-values-begin: iip32 -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 2.0 | Table F.5, p. 91, scale 1 row, Overall Mean column |
| M | 1 | BC | 2.7 | Table F.5, p. 91, scale 2 row, Overall Mean column |
| M | 1 | DE | 2.7 | Table F.5, p. 91, scale 3 row, Overall Mean column |
| M | 1 | FG | 3.3 | Table F.5, p. 91, scale 4 row, Overall Mean column |
| M | 1 | HI | 4.0 | Table F.5, p. 91, scale 5 row, Overall Mean column |
| M | 1 | JK | 4.3 | Table F.5, p. 91, scale 6 row, Overall Mean column |
| M | 1 | LM | 4.3 | Table F.5, p. 91, scale 7 row, Overall Mean column |
| M | 1 | NO | 2.7 | Table F.5, p. 91, scale 8 row, Overall Mean column |
| SD | 1 | PA | 2.5 | Table F.5, p. 91, scale 1 row, Overall SD column |
| SD | 1 | BC | 3.3 | Table F.5, p. 91, scale 2 row, Overall SD column |
| SD | 1 | DE | 3.7 | Table F.5, p. 91, scale 3 row, Overall SD column |
| SD | 1 | FG | 3.3 | Table F.5, p. 91, scale 4 row, Overall SD column |
| SD | 1 | HI | 3.3 | Table F.5, p. 91, scale 5 row, Overall SD column |
| SD | 1 | JK | 3.0 | Table F.5, p. 91, scale 6 row, Overall SD column |
| SD | 1 | LM | 3.3 | Table F.5, p. 91, scale 7 row, Overall SD column |
| SD | 1 | NO | 2.6 | Table F.5, p. 91, scale 8 row, Overall SD column |
| Size | 1 | — | 800 | DERIVED, not printed for the IIP-32: p. 25's N = 800 for the standardization sample, carried over because the IIP-32 is scored from a subset of the same administration (p. 24). Table F.5 prints no group sizes |
| Population | 1 | — | a national standardization sample of 800 U.S. adults aged 18–89, stratified by race/ethnicity and education within gender against 1999 Census figures | p. 25, Normative Sample, 'The IIP normative information presented in this manual is based on a national standardization sample of 800 cases representative of the U.S. population of adults aged 18-89' |
| Reference | 1 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 1 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| M | 2 | PA | 1.8 | Table F.5, p. 91, scale 1 row, Females Mean column |
| M | 2 | BC | 2.0 | Table F.5, p. 91, scale 2 row, Females Mean column |
| M | 2 | DE | 2.7 | Table F.5, p. 91, scale 3 row, Females Mean column |
| M | 2 | FG | 3.0 | Table F.5, p. 91, scale 4 row, Females Mean column |
| M | 2 | HI | 4.3 | Table F.5, p. 91, scale 5 row, Females Mean column |
| M | 2 | JK | 4.8 | Table F.5, p. 91, scale 6 row, Females Mean column |
| M | 2 | LM | 4.7 | Table F.5, p. 91, scale 7 row, Females Mean column |
| M | 2 | NO | 2.5 | Table F.5, p. 91, scale 8 row, Females Mean column |
| SD | 2 | PA | 2.5 | Table F.5, p. 91, scale 1 row, Females SD column |
| SD | 2 | BC | 3.3 | Table F.5, p. 91, scale 2 row, Females SD column |
| SD | 2 | DE | 3.3 | Table F.5, p. 91, scale 3 row, Females SD column |
| SD | 2 | FG | 3.3 | Table F.5, p. 91, scale 4 row, Females SD column |
| SD | 2 | HI | 3.7 | Table F.5, p. 91, scale 5 row, Females SD column |
| SD | 2 | JK | 3.3 | Table F.5, p. 91, scale 6 row, Females SD column |
| SD | 2 | LM | 3.3 | Table F.5, p. 91, scale 7 row, Females SD column |
| SD | 2 | NO | 2.8 | Table F.5, p. 91, scale 8 row, Females SD column |
| Size | 2 | — | 400 | DERIVED, not printed for the IIP-32: Table 4.1's note, p. 26 ("400 females"), carried over on the same p. 24 grounds |
| Population | 2 | — | the 400 female members of that standardization sample | p. 25 ('equal numbers of participants were selected by gender (n = 100) within each age group') with Table 4.1's note, p. 26 ('N = 800 (400 females, 400 males)'); the manual gives no demographics for this half separately |
| Reference | 2 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 2 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| M | 3 | PA | 2.3 | Table F.5, p. 91, scale 1 row, Males Mean column |
| M | 3 | BC | 3.0 | Table F.5, p. 91, scale 2 row, Males Mean column |
| M | 3 | DE | 3.0 | Table F.5, p. 91, scale 3 row, Males Mean column |
| M | 3 | FG | 3.3 | Table F.5, p. 91, scale 4 row, Males Mean column |
| M | 3 | HI | 3.7 | Table F.5, p. 91, scale 5 row, Males Mean column |
| M | 3 | JK | 4.0 | Table F.5, p. 91, scale 6 row, Males Mean column |
| M | 3 | LM | 3.8 | Table F.5, p. 91, scale 7 row, Males Mean column |
| M | 3 | NO | 2.8 | Table F.5, p. 91, scale 8 row, Males Mean column |
| SD | 3 | PA | 2.5 | Table F.5, p. 91, scale 1 row, Males SD column |
| SD | 3 | BC | 3.3 | Table F.5, p. 91, scale 2 row, Males SD column |
| SD | 3 | DE | 3.7 | Table F.5, p. 91, scale 3 row, Males SD column |
| SD | 3 | FG | 3.7 | Table F.5, p. 91, scale 4 row, Males SD column |
| SD | 3 | HI | 3.3 | Table F.5, p. 91, scale 5 row, Males SD column |
| SD | 3 | JK | 3.0 | Table F.5, p. 91, scale 6 row, Males SD column |
| SD | 3 | LM | 3.3 | Table F.5, p. 91, scale 7 row, Males SD column |
| SD | 3 | NO | 2.8 | Table F.5, p. 91, scale 8 row, Males SD column |
| Size | 3 | — | 400 | DERIVED, not printed for the IIP-32: Table 4.1's note, p. 26 ("400 males"), carried over on the same p. 24 grounds |
| Population | 3 | — | the 400 male members of that standardization sample | p. 25 ('equal numbers of participants were selected by gender (n = 100) within each age group') with Table 4.1's note, p. 26 ('N = 800 (400 females, 400 males)'); the manual gives no demographics for this half separately |
| Reference | 3 | — | Horowitz, Alden, Wiggins, & Pincus (2003) | title page, p. 3 (four authors, "Third Edition"); copyright page, p. 2 ("Copyright © 2003", published by Mind Garden, Inc.) |
| URL | 3 | — | not-published-in-source | the manual prints only www.mindgarden.com in its copyright line (p. 1); the shipped value is the publisher's product page for the instrument |
| Angle | — | PA | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | BC | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | DE | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | FG | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | HI | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | JK | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | LM | not-published-in-source | the manual prints no octant angles for either instrument |
| Angle | — | NO | not-published-in-source | the manual prints no octant angles for either instrument |
| Items | — | PA | 22, 25, 28, 30 | Appendix H, p. 101, scale 1 |
| Items | — | BC | 14, 16, 17, 18 | Appendix H, p. 101, scale 2 |
| Items | — | DE | 10, 11, 13, 15 | Appendix H, p. 101, scale 3 |
| Items | — | FG | 2, 5, 9, 19 | Appendix H, p. 101, scale 4 |
| Items | — | HI | 4, 6, 7, 12 | Appendix H, p. 101, scale 5 |
| Items | — | JK | 1, 8, 20, 31 | Appendix H, p. 102, scale 6 |
| Items | — | LM | 23, 26, 27, 32 | Appendix H, p. 102, scale 7 |
| Items | — | NO | 3, 21, 24, 29 | Appendix H, p. 102, scale 8 |
<!-- audit-values-end -->

## Traces to

- `data/iip64.rda`, `data-raw/iip64.R` — every norm value, the item map, the
  sample sizes and the reference credit.
- `data/iip32.rda`, `data-raw/iip32.R` — the same for the IIP-32.
- `R/instrument_data.R` — the `@source` citation and the reproduction credit
  line for both instruments.
- `data-raw/audit-norms.R` — the six `AUDIT_BATCH` rows that read this page's
  two blocks.
- `tests/testthat/test-norms-provenance.R` — the `audited_objects` pins for both
  instruments and the credit-line assertions.
- `cairn/references/norms-audit.md` — the `iip32` and `iip64` status rows.
