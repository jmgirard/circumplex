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

**Role.** The published source for every shipped CAIS norm value, for both of
the instrument's norm samples: sample 1 is the child sample (Table 2, p. 323)
and sample 2 the adult sample (Table 4, p. 325).

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
  compared against the block above. Its `cais_scales$Items` map assigns exactly
  four items to each octant on a strict eight-cycle (`1, 9, 17, 25` and so on),
  which reproduces Table 1's grouping only through item 29: the article's JK
  block has three items rather than four, so from item 30 on the cycle is one
  position out and items 33–37 fall off the end unassigned.
- `data-raw/audit-norms.R` — parses the block above as the source side of the
  cais comparison, one pass per sample.
- `cairn/references/norms-audit.md` — carries cais's audit verdict and this
  file's shelf-manifest row.
