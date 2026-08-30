# sodano2006 — the CAIS and its two normative samples

**Provenance.** Ingested 2026-08-07 by M74 from
`cairn/references/sources/sodano2006.pdf` (gitignored), 13 PDF pages whose
printed numbers run pp. 317–329. Pagination: the article's own page numbers.
The PDF is **born-digital** (`Creator: PScript5.dll Version 5.2.2`,
`Producer: Acrobat Distiller 6.0.1 (Windows)`, 5290 text characters per page),
not an OCR scan, and every value below is typeset table text rather than a
figure glyph. The anchor pages were nevertheless read in two independent
channels — the `pdftotext -layout` text layer and a 300-dpi page-image render —
because the item-to-octant grouping in Table 1 is carried by vertical
alignment rather than by any per-row token, which is exactly the kind of
structure a text layer can reflow.
Extraction: verified 2026-08-07 — Table 1's octant blocks (p. 322), Table 2's
M/SD rows (p. 323) and Table 4's M/SD rows (p. 325) each read in both channels,
the channels agreeing on every value and on every item's block membership; no
value read by a second human channel — observed 2026-08-07.

**Citation.** Sodano, S. M., & Tracey, T. J. G. (2006). Interpersonal traits in
childhood: Development of the Child and Adolescent Interpersonal Survey.
*Journal of Personality Assessment, 87*(3), 317–329. The article prints no DOI
(see the URL row below); its running copyright line reads "Copyright © 2006,
Lawrence Erlbaum Associates, Inc."

**Role.** The published source for every shipped CAIS norm value. The package
shipped two samples until 2026-08-30 — sample 1, the child sample (Table 2,
p. 323), and sample 2, the adult sample (Table 4, p. 325) — and now ships only
sample 1; see "Withdrawn" below. Both samples' extracted values are recorded
here regardless: this note records the source, not the roster.

**On the two sample sizes.** The article gives the child sample as 213 twice —
"consisted of 213 students (125 fourth graders and 88 sixth graders)" (p. 320)
and "The resulting samples utilized in subsequent analyses consisted of 213 and
194 child and adult surveys, respectively" (p. 321) — but the note to Table 2,
the table the shipped sample-1 M and SD are read from, reads "N = 204". The
article never reconciles the two, and nothing in it says which respondents the
204 excludes (Table 2 also carries the BFQ–C scales, so a listwise-complete
subsample is the obvious guess and is only a guess). The Size row below records
**204**, the N of the table the norm values themselves come from, so the
discrepancy reaches the ledger instead of being resolved silently by whichever
number the package happens to ship. The adult sample has no such split: p. 320,
p. 321 and Table 4's note all give 194.

**On the item-to-octant assignment.** Table 1 (p. 322) lists all 37 final CAIS
items grouped into octant blocks, and the blocks are **not** of equal size:
PA, BC, DE, HI, LM and NO have five items each, FG has four, and JK has three.
The Items rows below record that grouping. It is the published assignment, and
it differs from what the package ships — see "Traces to".

**On the angles.** Table 3 (p. 324) prints a "Target Angle" column, which is an
assigned degree per octant rather than an estimated angular location (the same
table's other columns carry the estimates). Those targets are recorded below as
published values. LM's target is printed as 0, which the package ships as 360
under IP2; the audit compares angles modulo 360, so the two agree.

**On the adult sample's M and SD — the source is internally inconsistent, and
the shipped sample-2 values are probably the IAS's** (found 2026-08-08 while
planning the norms-fitness work; the transcription below is unaffected and is
not changed by this note). Three of the eight adult means Table 4 prints under
its CAIS columns — PA 5.19, LM 6.52, NO 6.14 — lie outside the CAIS's own 1–5
response range, which `cais$Anchors` ships. The article states the 5-point
format for the CAIS twice, and the second statement is not confined to the
child pilot: p. 320 (right column), describing the instrument itself, reads
"Items were rated on the 5-point scale just mentioned". The validity screen
that produced both the 213 child and 194 adult counts is likewise phrased in
5-point anchor labels ("a lot", "never", "always"). So nothing in the article
licenses reading the adult administration as a wider scale, and the printed
CAIS means cannot be right as labelled.

Three checks agree on what happened: **Table 4's M and SD rows appear to be
transposed between the CAIS and the IAS column blocks.**

| check | printed under CAIS | printed under IAS |
|---|---|---|
| M fits the CAIS's 1–5 range | no (max 6.52) | yes (max 4.16) |
| slope regressed on the child CAIS profile (Table 2) | 1.78 | 1.12 |
| mean SD as a fraction of scale width | 0.249 | 0.077 |
| …the same, with the two blocks swapped | 0.134 | 0.142 |

The dispersion row carries most of the weight: as printed, two instruments
administered in one sitting differ threefold in relative dispersion and the
IAS's 0.077 is implausibly tight, while swapping them yields 0.134 and 0.142,
near-identical and both slightly below the child sample's 0.195. The slope
agrees — the IAS-labelled block sits essentially on the child metric, which is
what the same 5-point instrument in an older sample should do. The **α row is
not** transposed and corroborates the rest: the low α values (.29–.80) sit
under the CAIS, and JK's .29 matches the article's own note (p. 322) that JK
was the weakest child scale at .32. Two adjacent rows swapping while a third
does not is an ordinary typesetting slip.

Ruled out on the way: **sum scores** (for the adult means to be sums of 1–5
items, DE would need a per-item mean of 2.34 / 5 = 0.47, below the scale
minimum; DE's 2.34 caps any item count at 2 while every scale has 3–5), and a
**published correction** (Crossref carries no `update-to` or `updated-by`
relation for 10.1207/s15327752jpa8703_12; its relation field is present and
empty — Taylor & Francis returns 403 to automated fetches, so Crossref is the
authoritative check available).

The extracted values below stay exactly as the article prints them: this note's
job is to record the source faithfully, and the source does print these numbers
under these headings. The finding is about what they *mean*, not about whether
they were read correctly — they were, including the column block, which was
re-checked against Table 4's full 16-column layout on 2026-08-08.

**Outstanding.** An author query went to Sodano (Tracey copied) on 2026-08-08
asking whether the two rows are transposed, and carrying the Table 2 *N*
question above. A "swapped" reply makes the block printed under IAS the correct
sample-2 M and SD, which is a numeric change to shipped norms and needs its own
gate (IP5, D-039). A "correct as printed" reply leaves the adult sample
irreconcilable with a 5-point CAIS. Until either arrives, every reading agrees
the sample-2 values printed under the CAIS columns are unusable as CAIS norms.
The query is still unanswered.

**Withdrawn (2026-08-30, M112).** The adult sample no longer ships. The ground
is unusability, not the metric question being settled: from 2026-08-08 a
fail-closed refusal at `norm_standardize()` (D-040) made the sample data no
call could accept, and 22 days on, neither disposition D-040 named — a reply
identifying the sample's metric, or a second source printing the descriptives —
had arrived, so it shipped as unusable data carrying explanatory prose at three
user-facing surfaces. It was removed from `data-raw/cais.R` and `data/cais.rda`
rather than corrected (D-052); `data-raw/cais.R` keeps no copy of the numbers,
so this note is the only place they live.

What a reply reopens: a "swapped" reply re-adds the IAS-printed block as the
adult sample's M and SD, which is a numeric change to shipped norms and enters
under D-039's numeric-change gate — a fresh gate either way, whether or not the
wrong values were still shipped when it arrived. A "correct as printed" reply
leaves the withdrawal standing. The extracted values below are unchanged by the
withdrawal and are what such a gate would be argued from.

## Extracted values

Sample 1 = the child sample, Table 2 (p. 323); sample 2 = the adult sample,
Table 4 (p. 325). Item numbers in the Items rows are the package's own item
ordering — Table 1 identifies its items by content, not by number.

<!-- audit-values-begin -->
| field | sample | scale | value | anchor |
| --- | --- | --- | --- | --- |
| M | 1 | PA | 3.39 | Table 2, p. 323, M row, CAIS PA column |
| M | 1 | BC | 2.11 | Table 2, p. 323, M row, CAIS BC column |
| M | 1 | DE | 1.85 | Table 2, p. 323, M row, CAIS DE column |
| M | 1 | FG | 1.99 | Table 2, p. 323, M row, CAIS FG column |
| M | 1 | HI | 2.08 | Table 2, p. 323, M row, CAIS HI column |
| M | 1 | JK | 2.76 | Table 2, p. 323, M row, CAIS JK column |
| M | 1 | LM | 3.62 | Table 2, p. 323, M row, CAIS LM column |
| M | 1 | NO | 3.75 | Table 2, p. 323, M row, CAIS NO column |
| SD | 1 | PA | 0.84 | Table 2, p. 323, SD row, CAIS PA column |
| SD | 1 | BC | 0.85 | Table 2, p. 323, SD row, CAIS BC column |
| SD | 1 | DE | 0.77 | Table 2, p. 323, SD row, CAIS DE column |
| SD | 1 | FG | 0.74 | Table 2, p. 323, SD row, CAIS FG column |
| SD | 1 | HI | 0.64 | Table 2, p. 323, SD row, CAIS HI column |
| SD | 1 | JK | 0.81 | Table 2, p. 323, SD row, CAIS JK column |
| SD | 1 | LM | 0.86 | Table 2, p. 323, SD row, CAIS LM column |
| SD | 1 | NO | 0.73 | Table 2, p. 323, SD row, CAIS NO column |
| M | 2 | PA | 5.19 | Table 4, p. 325, M row, CAIS PA column |
| M | 2 | BC | 3.97 | Table 4, p. 325, M row, CAIS BC column |
| M | 2 | DE | 2.34 | Table 4, p. 325, M row, CAIS DE column |
| M | 2 | FG | 2.76 | Table 4, p. 325, M row, CAIS FG column |
| M | 2 | HI | 3.87 | Table 4, p. 325, M row, CAIS HI column |
| M | 2 | JK | 4.16 | Table 4, p. 325, M row, CAIS JK column |
| M | 2 | LM | 6.52 | Table 4, p. 325, M row, CAIS LM column |
| M | 2 | NO | 6.14 | Table 4, p. 325, M row, CAIS NO column |
| SD | 2 | PA | 0.89 | Table 4, p. 325, SD row, CAIS PA column |
| SD | 2 | BC | 1.08 | Table 4, p. 325, SD row, CAIS BC column |
| SD | 2 | DE | 0.98 | Table 4, p. 325, SD row, CAIS DE column |
| SD | 2 | FG | 1.11 | Table 4, p. 325, SD row, CAIS FG column |
| SD | 2 | HI | 1.12 | Table 4, p. 325, SD row, CAIS HI column |
| SD | 2 | JK | 0.99 | Table 4, p. 325, SD row, CAIS JK column |
| SD | 2 | LM | 0.93 | Table 4, p. 325, SD row, CAIS LM column |
| SD | 2 | NO | 0.87 | Table 4, p. 325, SD row, CAIS NO column |
| Angle | — | PA | 90 | Table 3, p. 324, Target Angle column, PA row |
| Angle | — | BC | 135 | Table 3, p. 324, Target Angle column, BC row |
| Angle | — | DE | 180 | Table 3, p. 324, Target Angle column, DE row |
| Angle | — | FG | 225 | Table 3, p. 324, Target Angle column, FG row |
| Angle | — | HI | 270 | Table 3, p. 324, Target Angle column, HI row |
| Angle | — | JK | 315 | Table 3, p. 324, Target Angle column, JK row |
| Angle | — | LM | 0 | Table 3, p. 324, Target Angle column, LM row (printed 0; the package ships 360 under IP2 and the comparison is modulo 360) |
| Angle | — | NO | 45 | Table 3, p. 324, Target Angle column, NO row |
| Items | — | PA | 1, 9, 17, 25, 32 | Table 1, p. 322, 'PA Assured-Dominant' block (tough / know a lot / think I can do a lot / speak up for myself / think I am right) — grouping only; numbers are the package's own item ordering |
| Items | — | BC | 2, 10, 18, 26, 33 | Table 1, p. 322, 'BC Arrogant-Calculating' block (call people names / like making trouble / trick people / tell people what to do / sneaky) — grouping only; numbers are the package's own item ordering |
| Items | — | DE | 3, 11, 19, 27, 34 | Table 1, p. 322, 'DE Cold-Hearted' block (hurt people / make people cry / mean to others / like it when others feel bad / grumpy) — grouping only; numbers are the package's own item ordering |
| Items | — | FG | 4, 12, 20, 28 | Table 1, p. 322, 'FG Aloof-Introverted' block (by myself a lot / alone / hard to get to know / play by myself) — grouping only; numbers are the package's own item ordering |
| Items | — | HI | 5, 13, 21, 29, 35 | Table 1, p. 322, 'HI Unassured-Submissive' block (shy / sad / know very little / give in easily / afraid) — grouping only; numbers are the package's own item ordering |
| Items | — | JK | 6, 14, 22 | Table 1, p. 322, 'JK Unassuming-Ingenuous' block (calm / quiet / tricking people is mean) — three items, not four; grouping only, numbers are the package's own item ordering |
| Items | — | LM | 7, 15, 23, 30, 36 | Table 1, p. 322, 'LM Warm-Agreeable' block (kind to others / try to help others feel better / friendly / help people / share) — grouping only; numbers are the package's own item ordering |
| Items | — | NO | 8, 16, 24, 31, 37 | Table 1, p. 322, 'NO Gregarious-Extraverted' block (fun to be around / happy / giving / play with others / have a lot of friends) — grouping only; numbers are the package's own item ordering |
| Size | 1 | — | 204 | Table 2, p. 323, table note, 'N = 204' — the N of the table the sample-1 M and SD are read from; pp. 320-321 give the child sample as 213 twice, and the article does not reconcile the two |
| Size | 2 | — | 194 | Table 4, p. 325, table note, 'N = 194', agreeing with p. 320 'The adult sample consisted of 194 undergraduate students' and p. 321 '213 and 194 child and adult surveys, respectively' |
| Population | 1 | — | 213 students from three suburban elementary schools in the southwest United States, 125 fourth graders and 88 sixth graders, ages 9 to 13 (M = 10.7, SD = 1.08) | p. 320, Participants, 'The child sample was drawn from three suburban elementary schools in the southwest United States and consisted of 213 students (125 fourth graders and 88 sixth graders)' |
| Population | 2 | — | 194 undergraduate students from a college of education at a large southwest state university, ages 17 to 50 (M = 20.8, SD = 3.19) | p. 320, Participants, 'The adult sample consisted of 194 undergraduate students from classes in a college of education at a large southwest state university' |
| Reference | 1 | — | Sodano & Tracey (2006) | p. 317, byline 'Sandro M. Sodano and Terence J. G. Tracey' over 'Department of Counseling and Counseling Psychology, Arizona State University', with the year from the same page's 'Copyright © 2006, Lawrence Erlbaum Associates, Inc.' |
| Reference | 2 | — | Sodano & Tracey (2006) | p. 317, byline 'Sandro M. Sodano and Terence J. G. Tracey' over 'Department of Counseling and Counseling Psychology, Arizona State University', with the year from the same page's 'Copyright © 2006, Lawrence Erlbaum Associates, Inc.' |
| URL | 1 | — | not-published-in-source | no DOI or URL is printed anywhere in the article; the shipped DOI 10.1207/s15327752jpa8703_12 follows the Erlbaum scheme and is consistent with the printed 'JOURNAL OF PERSONALITY ASSESSMENT, 87(3), 317-329' and with the file's own internal title '12sodano.vp', but the article itself prints no such string |
| URL | 2 | — | not-published-in-source | no DOI or URL is printed anywhere in the article; the shipped DOI 10.1207/s15327752jpa8703_12 follows the Erlbaum scheme and is consistent with the printed 'JOURNAL OF PERSONALITY ASSESSMENT, 87(3), 317-329' and with the file's own internal title '12sodano.vp', but the article itself prints no such string |
<!-- audit-values-end -->

## Traces to

- `data-raw/cais.R` — every shipped CAIS norm value and provenance string
  compared against the block above. Until M74 its `cais_scales$Items` map
  assigned exactly four items to each octant on a strict eight-cycle
  (`1, 9, 17, 25` and so on), which reproduced Table 1's grouping only through
  item 29: the article's JK block has three items rather than four, so from item
  30 on the cycle was one position out and items 33–37 fell off the end
  unassigned. M74 rekeyed it to Table 1's own 5/5/5/4/5/3/5/5 blocks — the
  comparison this note backs is what found the defect (corrected 2026-08-07;
  the paragraph previously described the pre-M74 key in the present tense).
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  cais comparison, one pass per shipped sample. Since the 2026-08-30
  withdrawal that is sample 1 alone; the block's sample-2 rows are the record
  of a sample the package no longer ships, and no audit pass reads them.
- `cairn/references/norms-audit.md` — carries cais's audit verdict and this
  file's shelf-manifest row.
