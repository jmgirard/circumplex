# norms-audit — provenance status of every shipped instrument's norms

A synthesis note (no single `<citekey>.md` owns it): the citekey map, the
shelf manifest, and the per-instrument audit status for all 15 shipped
instruments. Batch 1 (M72) audited five and batch 2 (M73) four more; the rest carry their
status here so an unaudited instrument is visible rather than merely out of
scope.

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
| cais | unaudited | — | — | 2026-08-06 |
| iei | unaudited | — | — | 2026-08-06 |
| igicr | unaudited | — | — | 2026-08-06 |
| iip32 | unaudited; source is the IIP-64/IIP-32 professional manual, obtainable — the maintainer holds publisher permission to ship the norming functions but not the item text (M72, corrected M73) | — | — | 2026-08-06 |
| iip64 | unaudited; source is the IIP-64/IIP-32 professional manual, obtainable — the maintainer holds publisher permission to ship the norming functions but not the item text (M72, corrected M73) | — | — | 2026-08-06 |
| iipsc | unaudited (two norm samples, two different sources) | — | — | 2026-08-06 |
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
