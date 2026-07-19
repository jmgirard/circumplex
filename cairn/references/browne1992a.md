# browne1992a — the RMSEA cutoffs the structure note reports against

**Citekey trap.** This page is Browne **& Cudeck**. The neighbouring
`browne1992.md` is Browne **alone** (the CPM paper, *Psychometrika* 57(4)).
The `a` suffix in this citekey marks a **different author set**, not a second
work by the same author — the usual reading of an alphabetical suffix. Both
pages say so, in both directions.

**Provenance.** Ingested 2026-07-19 by M41 from
`cairn/references/sources/browne1992a.pdf` (gitignored).
Pagination: the journal's own, *SMR* 21(2) 230–258; the shelf PDF is 29 pages
and PDF page *n* is printed page *n* + 229, so printed p. 239 is PDF p. 10,
confirmed by its running head rather than by arithmetic.
Extraction: verified 2026-07-19 against the source by two independent channels — `pdftotext -layout` and a visual read of the `pdftoppm`-rendered page image — covering every value on this page, all of which come from the single p. 239 passage; per M41-D1 the second channel is the implementing session's own read of the page image, which is independent of the text layer but is **not** a human attestation, and no value here has been read by a human — observed 2026-07-19.

**Citation.** Browne, M. W., & Cudeck, R. (1992). Alternative ways of
assessing model fit. *Sociological Methods & Research, 21*(2), 230–258. The
article carries no printed DOI. It appeared in a November 1992 special issue
edited by Bollen & Long.

The same paper **also appears as** chapter 6, pp. 136–162, of K. A. Bollen &
J. S. Long (Eds.), *Testing Structural Equation Models* (Sage, 1993) — the
edition the package's `@references` cite. It is named here **neutrally**, never
as "reprinted in": see Open questions, where the relationship is recorded as
established in structure but unestablished in text.

**Role.** The source of the two RMSEA thresholds the package hard-codes for
the structure-note wording of `summary.circumplex_ci_accuracy()`. Those
constants gate **wording only, never estimation**
(`R/ssm_ci_accuracy.R:1014-1023`).

## Extracted values

### The cutoff passage — p. 239, verbatim

All three thresholds occur in one continuous passage. Quoted as printed:

> Practical experience has made us feel that a value of the RMSEA of about
> 0.05 or less would indicate a *close fit* of the model in relation to the
> degrees of freedom. This figure is based on subjective judgment. It cannot
> be regarded as infallible or correct, but is more reasonable than the
> requirement of exact fit with the RMSEA = 0.0. We are also of the opinion
> that a value of about 0.08 or less for the RMSEA would indicate a reasonable
> error of approximation and would not want to employ a model with a RMSEA
> greater than 0.1.

Reduced to the three thresholds, in the source's own terms:

| Value as printed | What the source calls it |
|---|---|
| about **0.05** or less | "a *close fit* of the model in relation to the degrees of freedom" |
| about **0.08** or less | "a reasonable error of approximation" |
| greater than **0.1** | a model the authors "would not want to employ" |

**The authors' own hedge is part of the passage** and is not separable from
the numbers: "This figure is based on subjective judgment. It cannot be
regarded as infallible or correct." It is attached to the 0.05 figure
specifically.

### Two places the repo's wording departs from the source

Neither is an error in a shipped constant; both are characterizations the page
records rather than launders.

- The source prints **0.1**. The package's constant is written `0.10`
  (`ssm_ci_rmsea_poor <- 0.10`) and the vignette says ".10". Numerically
  identical; typographically not what the source printed.
- The source states a **preference** — the authors "would not want to employ"
  such a model. The package renders this as a property of the model: "the
  structural model fits **poorly**" (`R/ssm_ci_oop.R:340-341`) and "above
  about .10 poor fit" (`vignettes/evaluating-circumplex-structure.Rmd:93`).
  Browne and Cudeck do not use the word "poor" here.

### Between-channel discrepancies

Both are defects of the PDF's text layer, resolved in favour of the rendered
page; no numeral differed between channels.

- `pdftotext` emits **"RMSEAgreater"** as one word. The page image shows normal
  word spacing with tight kerning. Anything quoting the text layer directly
  inherits the artifact.
- `pdftotext` loses the **italics on "close fit"**, which the print carries.
  The italics matter: they mark it as the term being defined.

### Not extracted

The paper's substantive contribution — the RMSEA point estimate (eq. 13,
p. 239), its 90% confidence interval (eq. 14), and the test of close fit
(eq. 15, pp. 240–241) — is **not** transcribed here, because nothing in the
repo computes them: RMSEA reaches the package through `lavaan` and `CircE`,
and only the cutoffs above are ours. A milestone that ever computes an RMSEA
interval directly must extend this page rather than assume it is complete.

## Traces to

- `R/ssm_ci_accuracy.R:1014-1023` — the comment attributing these cutoffs, and
  the constants `ssm_ci_rmsea_reasonable <- 0.08` and
  `ssm_ci_rmsea_poor <- 0.10`.
- `R/ssm_ci_oop.R:340-341` — the poor-fit caution branch ("RMSEA > …;
  Browne & Cudeck, 1993").
- `R/ssm_ci_oop.R:349-352` — the adequate-fit branch, which pairs this
  source's RMSEA threshold with `hu1999.md`'s SRMR threshold.
- `R/ssm_ci_oop.R:404,415-418` — the `summary()` roxygen benchmark note and
  its `@references` entry, which cite the 1993 chapter.
- `vignettes/evaluating-circumplex-structure.Rmd:92-93,613` — the benchmark
  prose and the reference-list entry.

## Open questions

- **Whether the 1993 chapter is a verbatim reprint of this article is
  unresolved, and this page asserts nothing either way.** What is established:
  same editors, same papers, contiguous journal pagination (Bollen & Stine
  205–229 immediately precedes this article's 230–258), matching table of
  contents order. What is not: the book's preface, which would state the
  relationship outright, is omitted from the Google Books preview, and the
  only direct claim found — an anonymous customer review asserting the text is
  not identical — reaches us through a search-engine paraphrase and contradicts
  a Google Books spot-check that the visible pages look identical
  (M41 work log) — observed 2026-07-19.
- **The 0.08/0.1 sentence has no verified page anchor in the 1993 chapter.**
  The 0.05 sentence does: chapter **p. 144**, located by Jeff via Google Books.
  The 0.08/0.1 sentence follows it by about five lines in the article, so it is
  probably also p. 144 — **not banked, and deliberately not derived.** The
  offset trap here is demonstrated, not hypothetical: article p. 239 minus the
  94-page offset predicts chapter p. 145, and the real page is 144. A derived
  anchor would have read exactly like a checked one — observed 2026-07-19.
- No value on this page has been read by a human. Both channels are machine
  channels operating on the same PDF (M41-D1), so a defect in the scan itself
  — as opposed to the text layer — would not have been caught. A human read of
  p. 239 would close this — observed 2026-07-19.
