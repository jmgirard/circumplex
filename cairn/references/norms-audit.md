# norms-audit — provenance status of every shipped instrument's norms

A synthesis note (no single `<citekey>.md` owns it): the citekey map, the
shelf manifest, and the per-instrument audit status for all 15 shipped
instruments. Batch 1 (M72) audited five, batch 2 (M73) four more, batch 3 (M74)
the four multi-sample instruments and batch 4 (M75) the two IIP forms, which
closes the family: every shipped instrument now carries an audit verdict below.

**Provenance.** Ingested 2026-08-06 by M72 (authored here rather than extracted
from one source — this is a synthesis note over the shelf and the shipped data,
so it has no single source pointer and no pagination basis: `—`).
Extraction: not applicable — the note restates no source's values; each source's
extraction status lives in its own `<citekey>.md`. Every status below is a claim about
this repo's own state, so each carries its observation date; the shelf is
gitignored and live, so the manifest is re-verified at review rather than
trusted from write time.

## Audit status (all 15 shipped instruments)

`verified` = every audited field either matched its source or carries a
recorded disposition. Audited field set: per-scale M and SD, scale angles,
item-to-scale assignment, sample `Size`, `Population`, `Reference`, `URL`.

A bare `verified` would overstate what was checked: most sources publish only
some of that set, and the rest carry a `not-published-in-source` disposition
rather than a comparison. So each verdict below names the fields actually
compared against the source — for csip and iitc that excludes both the angles
and the item map, either of which could be transposed without this audit
seeing it. Per-row detail is in `data-raw/norms-audit-ledger.csv`.

| instrument | status | batch | source note | observed |
|---|---|---|---|---|
| csie | verified: M, SD, Size, item map, Reference, URL; Population deviates by design | M72 | [locke2007.md](locke2007.md) | 2026-08-06 |
| csig | verified: M, SD, Size, item map, Reference, URL; Population deviates by design | M72 | [locke2014.md](locke2014.md) | 2026-08-06 |
| csip | verified: M, SD, Size, Reference, URL; Population deviates by design (no angles, no item map) | M72 | [boudreaux2018.md](boudreaux2018.md) | 2026-08-06 |
| csiv | verified: M, SD, Size, item map, angles, Reference, URL; Population deviates by design | M72 | [locke2000.md](locke2000.md) | 2026-08-06 |
| iitc | verified: M, SD, Size, Reference, URL; Population deviates by design (no angles, no item map) | M72 | [bliton2019.md](bliton2019.md) | 2026-08-06 |
| cais | verified: M, SD, angles, item map, Size, Reference (2 samples); the item map and the sample-1 Size were wrong and are corrected here; Population deviates by design (URL not published in source) | M74 | [sodano2006.md](sodano2006.md) | 2026-08-07 |
| iei | verified: M, SD, Size, Reference (2 samples); URL repointed here at the pages that publish the values; Population deviates by design (no angles, no item map) | M74 | [horner2024.md](horner2024.md) | 2026-08-07 |
| igicr | verified: M, SD, Size, angles, item map, Reference, URL (3 samples); Population deviates by design | M74 | [trucco2013.md](trucco2013.md) | 2026-08-07 |
| iip32 | verified: M, SD, item map, Reference (3 samples); `Size` is **derived**, not printed — Table F.5 gives no group sizes, so the shipped 800/400/400 carries over the IIP-64 standardization sample on the manual's own p. 24 grounds; Population deviates by design (no angles, no URL published) | M75 | [horowitz2003.md](horowitz2003.md) | 2026-08-07 |
| iip64 | verified: M, SD, Size, item map, Reference (3 samples); Population deviates by design (no angles, no URL published) | M75 | [horowitz2003.md](horowitz2003.md) | 2026-08-07 |
| iipsc | verified: M, SD, Size, item map, Reference (2 samples, two different sources); the sample-1 Reference year was wrong and is corrected here; Population deviates by design (no angles; sample-2 URL not published) | M74 | [hopwood2008.md](hopwood2008.md), [soldz1995.md](soldz1995.md) | 2026-08-07 |
| iis32 | audited, **norms unsourced**: item map, Reference, URL verified; M, SD, Size and Population are published in no identified source (angles not published) | M73 | [hatcher2012.md](hatcher2012.md) | 2026-08-07 |
| iis64 | verified: M, SD, Size, item map, Reference, URL; Population deviates by design (no angles) | M73 | [hatcher2009.md](hatcher2009.md) | 2026-08-06 |
| ipipipc | audited, **norms unsourced**: item map, Size, Reference, URL verified; M and SD are published in no identified source (no angles, Population deviates by design) | M73 | [markey2009.md](markey2009.md) | 2026-08-06 |
| isc | verified: M, SD, Size, Reference, URL; Population deviates by design (no angles, no item map) | M73 | [hopwood2011.md](hopwood2011.md) | 2026-08-06 |

## Citekey map

The shipped `Norms[[2]]$Reference` is a prose citation, not a citekey; this is
the mapping the audit script joins on.

| instrument | sample | citekey | statistics published in |
|---|---|---|---|
| csie | 1 | locke2007 | the author's website, **not** the cited article |
| csig | 1 | locke2014 | the article (inside Figure 2) |
| csip | 1 | boudreaux2018 | the article (Table 1) |
| csiv | 1 | locke2000 | the author's website, **not** the cited article |
| iitc | 1 | bliton2019 | the article (Table 1) |
| iis32 | 1 | hatcher2012 | **nowhere identified** — the article publishes no octant statistics |
| iis64 | 1 | hatcher2009 | the article (Table 1) |
| ipipipc | 1 | markey2009 | **nowhere identified** — the article publishes octant statistics only for a different sample |
| isc | 1 | hopwood2011 | the article (inside Figure 2) |
| cais | 1 | sodano2006 | the article (Table 2, child sample) |
| cais | 2 | sodano2006 | the article (Table 4, adult sample) |
| iei | 1 | horner2024 | the article (Table 1, Study 1) **and** the author's IEI norms page |
| iei | 2 | horner2024 | the article (Table 1, Study 2) |
| igicr | 1 | trucco2013 | the article (Table 3, Combined column) |
| igicr | 2 | trucco2013 | the article (Table 3, Males column) |
| igicr | 3 | trucco2013 | the article (Table 3, Females column) |
| iipsc | 1 | hopwood2008 | the article (Table 1, as octant SUMS; the package divides by 4) |
| iipsc | 2 | soldz1995 | the article (Table 4, Generic Outpatient column) |
| iip32 | 1 | horowitz2003 | the manual (Table F.5, p. 91, Overall column; SUMS over four items, divided by 4) |
| iip32 | 2 | horowitz2003 | the manual (Table F.5, p. 91, Females column) |
| iip32 | 3 | horowitz2003 | the manual (Table F.5, p. 91, Males column) |
| iip64 | 1 | horowitz2003 | the manual (Table 4.4, pp. 27–29, Overall column; SUMS over eight items, divided by 8) |
| iip64 | 2 | horowitz2003 | the manual (Table 4.4, pp. 27–29, Female column) |
| iip64 | 3 | horowitz2003 | the manual (Table 4.4, pp. 27–29, Male column) |

`horowitz2003` is the one citekey backing two instruments, so its note carries
two instrument-tagged audit-values blocks and `AUDIT_BATCH` selects between
them by instrument (M75).

## Shelf manifest

Filenames under `cairn/references/sources/` (gitignored). Scan verdict from the
positive probe: `pdftotext` text-layer density over the anchor pages plus
`pdfimages -list` for full-page images, with `pdfinfo` Producer as one input;
an inconclusive probe is treated as a scan. All nine batch-1/2 PDFs came back
born-digital (M72, extended M73), so M42-D1's two-channel rule did not fire —
the notes record that the norm tables were read in two channels regardless.
Batch 3 breaks that run: `soldz1995.pdf` is an Acrobat Paper Capture OCR scan,
the first shelf source for which M42-D1 does fire, so its text layer
corroborates nothing and its norm table needs a channel that is not OCR of the
same page image — observed 2026-08-07.

| file | sha256 | verdict | observed |
|---|---|---|---|
| locke2007.pdf | bca6ed8bcb1db59a646b92433ccc83554793044137fa05fc7edde24660ec21a8 | born-digital (0 images, 4310 text chars/page) | 2026-08-06 |
| locke2014.pdf | f69ede32328a70be7ab53343327ec1db46726fd9419ba29a22440c6cf2855549 | born-digital (8 images, none full-page; 4018 chars/page) | 2026-08-06 |
| boudreaux2018.pdf | 7cb5a192accb700e5867b8de7c52bde9832f2dc610f0c3e0435058d4a036cce0 | born-digital (0 images, 5205 chars/page) | 2026-08-06 |
| locke2000.pdf | 0b78d5855a680c58b7797e9a7bba2ef98130711bf5b799af9c6898c40ed4c0ef | born-digital (3 images, none full-page; 2154 chars/page) | 2026-08-06 |
| bliton2019.pdf | a0735307f6686802813a43a4a95c3bdf707a1ef5a20b83a16b5cd23bfbd428b4 | born-digital (6 images, none full-page; 3991 chars/page) | 2026-08-06 |
| locke_csie_norms_2026-08-06.html | 1165786f09a9a07f90572f14b7d71f76a4a67dcba4b2c44003af1f93edb3325e | retrieved HTML (kennethlocke.org/CSIE/CSIE_Norms.html) | 2026-08-06 |
| locke_csiv_norms_2026-08-06.html | 536aa7b2c11cd01c184d895797afcc63bc33832e581fb6c0d9908f2cfdd9a721 | retrieved HTML (kennethlocke.org/CSIV/CSIV_Norms.html) | 2026-08-06 |
| locke_csie_scoring_2026-08-06.html | 779394ec913f3a90350dc3fc5311641019e886377a9be6a12623fa44d3f75fb8 | retrieved HTML (kennethlocke.org/CSIE/CSIE_Scoring_R.html) | 2026-08-06 |
| locke_csiv_scoring_2026-08-06.html | 7f54e01905861a7ac33bf182827d78ce0a36eb5b9e81c5c97ad87bbf6674e13c | retrieved HTML (kennethlocke.org/CSIV/CSIV_Scoring_R.html) | 2026-08-06 |
| locke_csig_items_2026-08-06.html | 461c8d4dc6866b27c1f5ac33b86faaf7cac36bb1acd196ed40b4daa713c45814 | retrieved HTML (kennethlocke.org/CSIG/CSIG_Items_Scales.html) | 2026-08-06 |
| hatcher2009.pdf | 41488a551ee7eb349ff2e263a0ca22a0b27496de284103803c3b30ef4126b6d4 | born-digital (no full-page images, 5311 chars/page); norm table is typeset text (Table 1, p. 558) | 2026-08-06 |
| hatcher2012.pdf | 239b5474fcfeb3b6b9e226779056d24d7744ae0274427dec40c0ecaac1ea1edf | born-digital (2 images, neither full-page; 5609 chars/page); publishes no norm table at all | 2026-08-06 |
| markey2009.pdf | ce25c8ef65ddda8b14fec82377667b78bcecbbb4cc6c4a9858a51221853dedfa | born-digital (**zero** images, 4228 chars/page), so its text layer is the whole article | 2026-08-06 |
| hopwood2011.pdf | b65fd37b08f1165ec66e0d6075500b2984a6e0cb7417e581a4f42d8981f80527 | born-digital (1 image, not full-page; 2474 chars/page); norm values are text objects **inside Figure 2**, p. 717 | 2026-08-06 |
| sodano2006.pdf | c494d3c518bc6e2f1cd059ce074a0c6860d9236ec64f72db6a75100e02fee8db | born-digital (Distiller 6.0.1, 5290 chars/page); both norm tables are typeset text (Table 2, p. 323; Table 4, p. 325) | 2026-08-07 |
| hopwood2008.pdf | 490620d357dd0d1b733c6bf5b1d0658e49b01651095dbd39e67054408fc24170 | born-digital (Distiller 6.0.1, 3825 chars/page); norm table is typeset text (Table 1) | 2026-08-07 |
| horner2025.pdf | 69ce27cd48a2bddc105e5cde59e071e577508f387e2cf6f33127dd8e67eb80f2 | born-digital (Zotero, 5404 chars/page); both samples' norm values are typeset text in Table 1 | 2026-08-07 |
| trucco2013.pdf | 32c34116c0e47000eeac5fcd808ba1daa4998e5e5311b3151812a7682391834c | born-digital (Adobe PDF Library 9.9, 4853 chars/page); all three samples' norm values are typeset text in Table 3 | 2026-08-07 |
| locke_iei_norms_2026-08-07.html | 50230f1b83c68399560e997126df1aafeb3fb540ea4c2a9befd53da2a6cf4a4d | retrieved HTML (kennethlocke.org/IEI/IEI_Norms.html); publishes the iei sample-1 octant M/SD and N = 1,223, and credits horner2024 for them | 2026-08-07 |
| IIP Manual.pdf | 02397de4bc96d3b99134fa573a4e685fdf4489ce9d4b41684d9871833030b012 | born-digital (Power PDF Create, 6 images none full-page; 2024 chars/page); both instruments' norm tables are typeset text (Table 4.4, pp. 27–29; Table F.5, p. 91) | 2026-08-07 |
| IIP Agreement.pdf | a25589682a22a9e297e7ac6dbee0ea83ad6ecea4a40a6eb736ef25b5b1c2fb22 | born-digital; the Mind Garden publication agreement, source of the condition-(a) credit line — a permission record, not a norms source | 2026-08-07 |
| soldz1995.pdf | 758030aeebd66fc8989d181ba9fd7c402f4973b831ed1df5ceefa27fba7e7d14 | **OCR scan** (Acrobat Paper Capture plug-in, 3234 chars/page): the text layer is OCR of the page image, so it is not a second witness (M42-D1) and the norm table needs a genuinely independent channel | 2026-08-07 |

Channel plan for batch 2, decided by asking what channel reads the *norm
table* rather than what produced the file (the M72 lesson). Every anchor a
compared value came from was read in two independent channels — the `pdftotext`
text layer and a 200–300 dpi page-image render — because `pdftotext` run twice,
with and without `-layout`, is one channel read twice. hopwood2011 is the case
that most needed it: its values sit inside a figure. markey2009 is the case
that needed it least: with zero images in the file, nothing can hide from the
text layer, which is also what makes its absence findings checkable.

## What batch 1 found

No shipped M or SD was wrong in any of the five instruments — all 40 M/SD
pairs, all five sample sizes, and every item-to-scale assignment the sources
publish matched exactly. The defects were provenance: csie and csiv cited
articles that do not publish their norms, and both `URL`s resolved to a
homepage after a site move that dropped the path. Both were corrected on the
M72 branch; neither correction touched a number.

Two things the audit deliberately did not settle. The degree assignment for
scale angles is the package's own convention (DESIGN.md IP2) and only
locke2000 publishes degrees at all, so angles are recorded as
`not-published-in-source` for four of five instruments rather than claimed as
verified. And locke2014's Figure 2 prints identical α/M/SD for PA and NO, which
the package transcribes faithfully — a possible error in the source, carried as
a ROADMAP candidate row.

## What batch 2 found

Two of the four verified clean and two did not, and the failure in both is
provenance rather than a wrong digit.

iis64 and isc match their sources exactly — all sixteen M/SD pairs, both sample
sizes, and every IIS-64 item-to-octant assignment the Appendix publishes. The
only wrinkle is in hatcher2009 itself, which gives its Study 3 sample as 684 in
the text and 686 in Table 1's own note; the package ships 684, which the
article's own 265-plus-419 breakdown supports.

iis32 and ipipipc ship octant means and standard deviations that **no
identified source publishes**. hatcher2012 contains six tables, none of them
descriptive, and never prints the shipped N of 1380. markey2009 publishes
octant statistics for exactly one sample — Study 1's combined sample, whose
values are different — while the shipped N of 274 is its Study 2, for which the
article reports no descriptive statistics at all. Each note records the bounded
sweep behind its absence claim. The values ship unchanged, because nothing
establishes they are wrong either; what changed is that both instruments'
printed `Reference` and `?instrument` `@source` now say the norms source is
unconfirmed instead of crediting an article that does not carry them. A ROADMAP
candidate row carries the open question of asking the authors.

Four shipped item texts were also wrong and are corrected here. In `iis64`,
item 5 had been truncated to "I realize " (hatcher2009's Appendix reads "I
realize that I don't have to be friends with everyone") and item 7 read "not
agreeable with others" where that Appendix reads "not agreeable to others". In
`iis32`, item 28 read "I'm ok with not being included in all activities" where
hatcher2012's Appendix prints "okay" — the two IIS articles genuinely differ on
this word. In `ipipipc`, item 16 read "Don't fall for sob-stories" where
markey2009's Appendix prints "sob stories". None is a norms field, so none
appears in the ledger; each was caught by reading that instrument's own
Appendix in two channels.

## What batch 3 found

No shipped mean or standard deviation was wrong in any of the four. All 144
shipped means and standard deviations across the nine normative samples — 72
M/SD pairs — matched their sources exactly (corrected 2026-08-07 from "88 M/SD
pairs", which matched neither the pair count nor the value count enumerated in
this same sentence) — cais's
32 in sodano2006's Tables 2 and 4, iei's 32 in horner2024's Table 1, igicr's 48
in trucco2013's Table 3, and iipsc's 32 across hopwood2008's Table 1 and
soldz1995's Table 4. Eight of the nine sample sizes matched too, as did every
scale angle and item map the sources publish, apart from the one below.

The serious finding is cais's **item-to-scale key**, and it is a scoring defect
rather than a provenance one. sodano2006 Table 1 (p. 322) groups the CAIS's 37
items into octant blocks of unequal size — five each for PA, BC, DE, HI, LM and
NO, four for FG, three for JK — but `data-raw/cais.R` shipped the equal-four
eight-cycle key the package's other 32-item instruments use, from its first
draft in 2018 until M74. That key put item 30 (an LM item) in JK, 31 (NO) in LM
and 32 (PA) in NO, and left items 33–37 in no scale at all, so `score()`
returned wrong values for seven of the eight octants — every one but FG
(corrected 2026-08-07 from "three octants", which counted the three misassigned
items but not the five unscored ones, whose correct octants are BC, DE, HI, LM
and NO; PA changes too, gaining the item 32 the old key put in NO) — and
`norm_standardize()` compared them against norms computed the correct way. The diagnosis is decisive rather than
inferred: dealing Table 1's blocks round-robin, skipping each octant once its
items run out, reproduces the shipped 37-item *ordering* exactly — so the file
already encoded the published grouping and only the key contradicted it. A sweep
of all fifteen bundled instruments found no second instance, and a test now
requires every key to cover its instrument's items exactly once.

Three provenance corrections went with it. cais's child-sample `Size` moved from
213 to 204, the N printed on the very table its means come from — the article
gives 213 twice elsewhere and never reconciles the two. iipsc's college-student
norms were credited to a 2011 publication and now name Hopwood, Pincus, DeMoor,
& Koonce (2008), which is the paper that publishes them and the DOI the
instrument already carried. iei's `URL` pointed at the study's OSF project,
which publishes neither of its norms tables, and now gives one address per
sample (M74-D1).

Two source-internal inconsistencies are recorded and left unresolved, both of
the kind hatcher2009 already showed: sodano2006's 204-vs-213 above, and
soldz1995's Generic Outpatient sample, given as 105 patients on p. 55 and as
n = 106 in Table 4's own heading. In each case the package now ships the table's
number, because that is the sample the statistics were computed on.

Two fields stay uncompared for want of a published value. iei's 64 items are in
a supplemental table that is not part of the article, and neither iei nor iipsc
publishes an assigned degree per octant — cais and igicr both do, in a "target
angle" row, and both match. cais's and iipsc's sources print no DOI or URL at
all, so those `URL` values rest on later-assigned identifiers that are
consistent with the printed volume and issue but appear nowhere in the articles.

soldz1995 is also the first shelf source that is an OCR scan, so M42-D1's
two-channel rule fired for real rather than being honoured as a precaution: its
values were read by a direct read of a page-image render and by an independent
`tesseract` pass over that render, neither of them the shelved Acrobat text
layer, which was consulted afterwards and agreed.
