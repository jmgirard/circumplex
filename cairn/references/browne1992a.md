# browne1992a — the RMSEA cutoffs the structure note reports against

**Citekey trap.** This page is Browne **& Cudeck**. The citekey `browne1992`
— no suffix — belongs to Browne **alone** (the CPM paper, *Psychometrika*
57(4) 469–497). The `a` suffix here marks a **different author set**, not a
second work by the same author, which is how an alphabetical suffix normally
reads. `browne1992.md` now **exists** and carries the reciprocal warning,
authored by M42 (this sentence read "does not exist yet: it is owed by M42";
corrected in place 2026-07-19) — observed 2026-07-19.

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
as "reprinted in": the two editions were compared 2026-07-20 (see the resolved
items under Open questions) — the chapter is a reset, lightly copy-edited
typesetting, not a photographic reprint, but every value this page extracts is
identical between them.

**Role.** This source reaches the package on **two** paths, and they differ in
kind — conflating them is easy and was the substantive defect M41's review
caught.

1. **Wording.** The two RMSEA thresholds hard-coded for the structure-note
   wording of `summary.circumplex_ci_accuracy()`. These gate **wording only,
   never estimation** (`R/ssm_ci_accuracy.R:1014-1023`).
2. **Estimation.** `cpm_fit()` computes the RMSEA point estimate and its 90%
   interval **natively**, by this paper's eqs. 13 and 14 — see "The RMSEA
   estimator" below. This is a live numeric dependency on the source, not a
   citation, and the code carries no attribution to it.

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

### The RMSEA estimator — eq. 13 (p. 239) and eq. 14 (p. 240)

`cpm_fit()` implements both of these directly. They are banked here verbatim
so a future change to that code has a published form to check against.

- **Eq. 13, p. 239** — the point estimate:

  ε̂ₐ = √(F̂₀ / d) = √( Max{ (F̂/d − 1/n), 0 } )

  Implemented at `R/cpm_fit.R:1049` as
  `rmsea <- sqrt(max(Fhat / df - 1 / n, 0))`. The `Max{·, 0}` truncation and
  the `− 1/n` correction are both the equation's, not the implementation's.

- **Eq. 14, p. 240** — the 90% confidence interval:

  (ε̂ₐL ; ε̂ₐU) = ( √( λ̂L / (n d) ) ; √( λ̂U / (n d) ) )

  where λ̂L and λ̂U are the noncentrality parameters obtained by inverting the
  noncentral chi-square. Implemented at `R/cpm_fit.R:1011-1028`
  (`cpm_rmsea_ci()`), whose closing line
  `c(sqrt(lambda_l / (n * df)), sqrt(lambda_u / (n * df)))` is eq. 14 term for
  term, with λ̂ found by `uniroot` on `pchisq(Tstat, df, ncp = λ)`.

**A documented property of eq. 14 the code relies on**, p. 240: "If the lower
limit of the confidence interval is zero, the test, based on n × F̂, of the
null hypothesis of exact fit in equation 4 would *not reject* the null
hypothesis at the 5% level." `cpm_rmsea_ci()`'s `if (lower_fun(0) < 0) 0`
branch is what produces that zero lower limit.

### Not extracted

The **test of close fit** (eq. 15, pp. 240–241) and its exceedance probability
are not transcribed here: nothing in the repo computes a close-fit test —
verified by grep over `R/` for a close-fit statistic or p-value, which returns
nothing — observed 2026-07-19. A milestone that adds one must extend this page
rather than assume it is complete.

## Traces to

**Estimation path** (eqs. 13–14; no attribution in the code — a corrector
changing any of these lines is changing an implementation of this paper):

- `R/cpm_fit.R:1049` — eq. 13, the RMSEA point estimate.
- `R/cpm_fit.R:1011-1028` — eq. 14, `cpm_rmsea_ci()`, the 90% interval.
- `R/cpm_oop.R:40-42,183-185` — where that estimate and interval are printed.

**Wording path** (the cutoffs):

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

- **Resolved 2026-07-20 — the 1993 chapter is not a verbatim reprint, but
  every value this page extracts is identical between the two editions.** Jeff
  added the chapter to the shelf (`browne1993.pdf`) and it was compared against
  `browne1992a.pdf`. The chapter is a **fully reset, lightly copy-edited
  typesetting**: different pagination (136–162 vs 230–258), different line
  breaks throughout, and copy-edits such as "The authors thank" → "We would
  like to thank", "can only be judged" → "can be judged only", "but is more
  reasonable" → "but it is more reasonable", "scale invariant" →
  "scale-invariant". So the anonymous customer review's "text is not identical"
  is literally true and immaterial. **Identical between editions:** the three
  RMSEA thresholds (0.05/0.08/0.1) and their descriptors word-for-word; eqs.
  13/14 numbering with the surrounding eq-9/eq-12 derivation and the "lower
  limit is zero" property; eq. 15's presence; and the worked-example fit tables
  digit-for-digit (e.g. 4.915, 0.098, (0.083; 0.113), 7.282, (6.612; 8.086),
  417.81). The M41 extraction — read from the article but cited to the chapter
  — therefore holds for the cited edition. Channel: `pdftotext -layout` on both
  PDFs plus a read of that text layer, a machine channel; the equations render
  as images in both and were checked by their numbering and surrounding prose,
  not their typeset form, and no page was read by a human — observed
  2026-07-20.
- **Resolved 2026-07-20 — all three thresholds sit on chapter p. 144.** The
  direct read confirms it: the running head `144 Alternative Ways of Assessing
  Model Fit` tops the page, and `MICHAEL W. BROWNE and ROBERT CUDECK 145` does
  not appear until after the "…would not want to employ a model with a RMSEA
  greater than 0.1" sentence. Jeff's Google Books find (0.05 → p. 144) is
  confirmed and now extends to the 0.08/0.1 sentence. The +94 offset prediction
  of p. 145 was wrong, exactly as this page warned — a derived anchor would
  have read like a checked one — observed 2026-07-20.
- No value on this page has been read by a human. Both channels are machine
  channels operating on the same PDF (M41-D1), so a defect in the scan itself
  — as opposed to the text layer — would not have been caught. A human read of
  p. 239 would close this — observed 2026-07-19.
