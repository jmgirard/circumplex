# wendt2019 — the real-data benchmark the SEM vignette cites for the fixed-angle circumplex CFA

**Provenance.** Ingested 2026-07-19 by M43 from
`cairn/references/sources/wendt2019.pdf` (gitignored).
Pagination: the journal's own, *J Abnorm Psychol* 128(8) 823–839; the shelf
PDF's page *n* is printed page **822 + n** (17 PDF pages over 17 printed
pages), so PDF p. 8 is printed p. 830.
Extraction: verified 2026-07-19 against the source by two independent channels — `pdftotext -layout` and a visual read of `pdftoppm`-rendered page images — for every value on this page (all of which come from pp. 829–832, read in both channels); the rest of the article was read in the text channel only, the online supplements are not on the shelf and were not read at all, and no value here has been read by a human — observed 2026-07-19.

The image channel is load-bearing for this source rather than a formality.
`pdftotext` mis-renders this PDF's mathematical operators through the font
encoding — `=` extracts as `⫽`, `−` as `⫺`, `<` as `⬍`, so
`RMSEA = .075–.111, p < .001` comes out as `RMSEA ⫽ .075–.111, p ⬍ .001`.
Every numeral below was confirmed against the rendered page. `pdfinfo` reports
Producer `Adobe LiveCycle PDF Generator` over Creator `XPP`: born-digital, not
an OCR scan, so the two channels are genuinely independent (M42-D1).

**Citation.** Wendt, L. P., Wright, A. G. C., Pilkonis, P. A., Nolte, T.,
Fonagy, P., Montague, P. R., Benecke, C., Krieger, T., & Zimmermann, J.
(2019). The latent structure of interpersonal problems: Validity of
dimensional, categorical, and hybrid models. *Journal of Abnormal Psychology,
128*(8), 823–839. https://doi.org/10.1037/abn0000460

Note the commonly-cited short title drops the subtitle; the article's own
title is the full two-part form above.

**Role.** The substantive-validity source for `vignettes/sem-based-ssm-analysis.Rmd`.
It is the nearest published neighbour to this package's SEM layer — a
three-factor confirmatory circumplex model of the IIP octants — and the
vignette cites it for four distinct claims, all verified below. It is
**context, not an oracle**: nothing in the package is validated against it,
and no number here gates any computation. Their estimand is the latent
structure of the octant scales themselves; the package's is an *external*
measure's disattenuated SSM profile.

## Extracted values

### The four models' shared structure (p. 829)

> The factorial part is based on specifying agency and communion as orthogonal
> factors that show a circular pattern of factor loadings. A third, general
> factor loads equally on all octants. In contrast to common bifactor models,
> we let the general factor correlate freely with the group factors.

Their footnote 4 (p. 830) restates the last point: "in the current CFA model
the general factor is free to correlate with the group factors (unlike in a
true bifactor models)."

**CFA-PC** ("perfect circumplex") fixes equal spacing **and** equal
communalities; **CFA-QC** ("quasi-circumplex") frees both (p. 829, and the
Table 2 note on p. 831). Table 2 records κ = 20 free parameters for CFA-PC and
κ = 23 for CFA-QC, with **3 factors** for every dimensional model.

The dimensional candidates were "collectively based on three-dimensions, as
suggested by past research (Acton & Revelle, 2002)" (p. 829).

### Fit of the most restrictive model (p. 830, verbatim)

> The most restrictive CFA-PC model (i.e., equal spacing and communalities,
> latent normal distributions) yielded acceptable fit to the data, CFI =
> .938–.957, TLI = .928–.950, SRMR = .059–.078 (Samples 1–4). The RMSEA
> suggested shortcomings in fit, RMSEA = .075–.111, p < .001.

| Index | Range across Samples 1–4 |
|---|---|
| CFI | .938–.957 |
| TLI | .928–.950 |
| SRMR | .059–.078 |
| **RMSEA** | **.075–.111** |

Also on p. 830: standardized loadings λ_GENERAL = .57–.82,
λ_AGENCY = |.56|–|.64|, λ_COMMUNION = |.49|–|.54| (highest loading for the
marker octants); ECV = .69–.76, i.e. the general factor accounted for roughly
75% of the common variance.

### The general factor's correlation with the plane (p. 831, verbatim)

> The latent correlation between agency and the general factor was estimated
> to be negative (Sample 1 r = −.283, Sample 2 r = −.292, Sample 3 r = −.267,
> Sample 4 r = −.324). The statistical association between communion and the
> general factor was dependent from the sample investigated (Sample 1
> r = −.034, Sample 2 r = .142, Sample 3 r = .083, Sample 4 r = −.115).

| Sample | *N* | g–agency *r* | g–communion *r* |
|---|---|---|---|
| 1 | 5,400 | −.283 | −.034 |
| 2 | 491 | −.292 | .142 |
| 3 | 656 | −.267 | .083 |
| 4 | 712 | −.324 | −.115 |

The g–agency mean is **−.2915**, and all four are negative — this is the
"replicated across four samples" the vignette relies on. The g–communion
values are not replicated in sign and span **−.115 to +.142**.

### CFA-QC bought nothing (pp. 831–832, verbatim)

> The less restrictive CFA-QC (i.e., allowing for unequal spacing and
> communalities) produced virtually identical parameter and fit estimates.
> Therefore, it is not described any further. (p. 831)

> With regard to dimensional models in this study, relaxing the "equal spacing
> and equal communalities" restriction did not result in consistent
> improvements in terms of model fit. In contrast, relaxing the restriction of
> latent normality by means of skew-t-CFA and t-CFA resulted in greatly
> improved model fit. However, it did not increment the prediction of external
> variables when compared against the more simplistic CFA model. (p. 832)

### Their headline conclusion (p. 832, verbatim)

> Across four samples we found consistent support for the superior validity of
> a purely dimensional representation (i.e., confirmatory factor analytic
> models based on the IPC), especially when allowing for nonnormal latent
> distributions. No evidence was found for the incremental validity of
> categorical or hybrid approaches.

Table 2 (p. 831) bears this out on the model-selection side: a dimensional
model has the best AIC_C and BIC in three of four samples (Skew-*t*-CFA in
Sample 1, *t*-CFA in Samples 3 and 4), with the hybrid SP-FA winning only
Sample 2; LCA never wins, and its median *R*² is lower than the dimensional
models' in every sample.

## The four vignette claims, checked

| Vignette line | Claim | Verdict |
|---|---|---|
| `:44` | related work on the latent structure of circumplex *scales* | **Accurate.** Their estimand is the octants' own latent structure (pp. 829–832), not an external measure's profile. |
| `:114` | general–agency correlation of roughly −.3 replicated across four samples | **Accurate.** −.283/−.292/−.267/−.324, mean −.2915, all negative (p. 831). |
| `:368` | RMSEA between .075 and .111 for the fixed-loading circumplex CFA across four large samples | **Accurate and correctly scoped** — the range is exactly the CFA-PC figure on p. 830, and CFA-PC is the fixed-spacing, fixed-communality model. |
| `:394-397` | three-factor circumplex CFA with fixed unit-cosine plane loadings, four large samples, fully dimensional model competitive with categorical and hybrid alternatives | **Accurate, and conservative.** Three factors and four samples are confirmed in Table 2; "competitive" *understates* p. 832's "superior validity … No evidence … for the incremental validity of categorical or hybrid approaches." See the caveat below on "unit-cosine". |

**One qualifier on `:394`.** The article body establishes that CFA-PC fixes
equal spacing and equal communalities and gives the plane "a circular pattern
of factor loadings" (p. 829), but it does not print the loading values. The
specific claim that the plane loadings are **unit-cosine** rests on the online
supplement's lavaan syntax (R Code S25), read in 2026-07 and recorded in
`devel/m5-wendt-discrepancies.md` §8 — where it is also noted that the
supplement writes the geometry as the 3-digit literal `0.707` rather than at
full precision. **The supplements are not on the shelf and were not read for
this page**, so that one detail is second-hand here — observed 2026-07-19.

## Reconciliation with `devel/m5-wendt-discrepancies.md`

That file is a 2026-07-07 design-review record (Fable) evaluating where the
package's SEM layer departs from this paper. Read here **read-only**; M43
changes no `devel/` file.

**Agreement** on everything load-bearing: the three-factor structure with
equal general-factor loadings and freely correlating general factor; CFA-PC as
equal spacing + equal communalities; the four g–agency correlations quoted
digit for digit; CFA-QC producing "virtually identical parameter and fit
estimates"; MLR as their estimator; κ = 23 for QC vs 20 for PC; and its §7
correction, which is confirmed verbatim — the claim that relaxing the
perfect-circumplex restrictions improves fit without sacrificing validity is
**their citation of prior work**, attributed on p. 829 to "(Acton & Revelle,
2002; Gurtman & Pincus, 2000)", while their own finding on p. 832 is the
opposite. (This is also why `sources/acton2002.pdf` owes no page: the repo
meets it only as other authors' citation of prior work.)

**One disagreement, recorded rather than settled.** Its §1 gives the
g–communion correlations as "smaller and sample-dependent, **−.034 to
+.142**". The paper's own sentence (p. 831, quoted above) gives Sample 4
*r* = **−.115**, so the range is **−.115 to +.142**; the design record's lower
bound is Sample 1's value, not the minimum. Immaterial to anything the package
computes — no code or vignette line uses the communion correlation, and the
§1 argument rests on the *agency* correlations, which are correct — but the
stated range is wrong as printed and is not corrected here, because `devel/`
is out of M43's scope (the ROADMAP carries the post-M7 disposition of these
files).

**Two claims in that file this page cannot check**, both because the sources
are not on the shelf: its §8 verification of the equal-`L1` general loading
from supplement R Code S25, and its readings of Gurtman & Pincus (2003),
Moss (2026), and Cheung & Rensvold (2002) — observed 2026-07-19.

## Traces to

- `vignettes/sem-based-ssm-analysis.Rmd:44` — the latent-structure-of-scales
  framing in the novelty claim.
- `vignettes/sem-based-ssm-analysis.Rmd:114` — the roughly −.3 general–agency
  correlation, cited as evidence that the scaled tier's `φ_g = 0` orthogonality
  is known to be violated on IIP-family data.
- `vignettes/sem-based-ssm-analysis.Rmd:368` — the RMSEA .075–.111 real-data
  benchmark for reading the example fits' RMSEA ≈ .12.
- `vignettes/sem-based-ssm-analysis.Rmd:394-400` — the "Relation to the
  literature" section: their three-factor CFA as context for the strict tier.
- `vignettes/sem-based-ssm-analysis.Rmd:428` — the reference-list entry.
- `devel/m5-wendt-discrepancies.md` — the design-review record reconciled
  above (read-only).

## Open questions

- **The online supplemental materials are not on the shelf.** Tables S6–S13 and
  the lavaan/Mplus syntax (R Code S25) are cited by the article and by
  `devel/m5-wendt-discrepancies.md` §8, but only the article PDF is in
  `cairn/references/sources/`. The unit-cosine loading detail behind the
  vignette's `:394` wording is second-hand for that reason — observed
  2026-07-19.
- **No value on this page has been read by a human.** Both channels are machine
  channels operating on the same PDF, so a defect in the source document itself
  — as opposed to its text layer — would not have been caught — observed
  2026-07-19.
- **The vignette understates their conclusion, deliberately or not.** `:396-397`
  says the dimensional model was "competitive with categorical and hybrid
  alternatives"; p. 832 claims "superior validity" and "no evidence … for the
  incremental validity of categorical or hybrid approaches". Understating a
  cited source is safe, and M43 changes no package file, so nothing is done
  here — but a future vignette pass could strengthen the sentence with this
  page's quote — observed 2026-07-19.
