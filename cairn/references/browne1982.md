# browne1982 — the transformation-based CI method the communality intervals rest on

**Scope warning, read first.** These two pages state a **general** method for
putting a confidence interval on a bounded parameter. They say nothing about
communalities, nothing about vᵢᵢ, nothing about the circumplex, and nothing
about the circular process model — the words do not appear. The
communality-CI chain the repo cares about is **Grassi's assembly** of this
method with `browne1992.md` eq. (4); see "What this does and does not license"
below. A reader arriving from `grassi2010.md`'s "Browne, 1982, pp. 95–96"
citation should expect the machinery, not the application.

**Provenance.** Ingested 2026-07-19 by M42 from
`cairn/references/sources/browne1982_pp95-96.pdf` (gitignored).
Pagination: the source's own, printed 95 and 96 on the pages themselves; the
shelf PDF is exactly 2 pages and holds nothing else of Browne (1982). The
equation numbering is the source's chapter-internal `(1.6.n)` scheme, which
means these two pages sit inside a §1.6 whose earlier equations — (1.6.9)
among them — are **not on the shelf**.
Extraction: verified 2026-07-19 against the source by a read of the `pdftoppm`-rendered page images, which is the only channel that carries the equations at all — the PDF is an Acrobat "Paper Capture" OCR scan of typescript and its `pdftotext -layout` text layer drops **every display equation on both pages**, preserving only the equation numbers, so per M42-D1 the page images are authoritative and the text layer served only to cross-check the running prose; no value here has been read by a human and a defect in the scan itself would be caught by neither channel — observed 2026-07-19.

**Citation.** Browne, M. W. (1982). Covariance structures. In D. M. Hawkins
(Ed.), *Topics in applied multivariate analysis* (pp. 72–141). Cambridge:
Cambridge University Press. **The citation is transcribed verbatim from
`grassi2010.pdf`'s reference list, not read off the source** — including its
sentence-case rendering of the volume title. The shelf holds two interior pages
with no title page, running head, or copyright line, so nothing on the shelf
attests the volume, editor, publisher, or page range — observed 2026-07-19.
What the shelf pages do attest is the chapter-internal §1.6 numbering and the
printed page numbers 95 and 96.

**Role.** The published warrant for the CI transform reconstructed at
`tests/testthat/test-cpm_oracles.R:136`, which reproduces CircE's published
communality-index intervals from its published vᵢᵢ and their standard errors.
Nothing in `R/` implements this method — the package's own ζ intervals are
formed on the natural scale, by either route it offers (see "Traces to") — so
this source backs a **test's reconstruction of another program's output**, not
a shipped estimator — observed 2026-07-19.

## Extracted values

Every equation below is quoted from the page images. Notation is the source's:
γᵢ is the parameter, γ̂ᵢ its estimate, σ̂(γ̂ᵢ) an estimated standard error,
c_α "the (1 − ½α) percentage point of the standard normal distribution for
one-at-a-time confidence intervals".

### The untransformed interval — p. 95

- **Eq. (1.6.29), p. 95** — the ordinary symmetric interval, justified by
  "the asymptotic normal distribution of the estimators":

  γ̂ᵢ − c_α σ̂(γ̂ᵢ) < γᵢ < γ̂ᵢ + c_α σ̂(γ̂ᵢ)

  The same passage defines the simultaneous alternative: c²_α is "the (1 − α)
  percentage point of the chi-squared distribution with q degrees of freedom
  for simultaneous confidence intervals on q identified parameters". **The
  repo uses the one-at-a-time form**, which is also what Grassi's Appendix A
  header says it prints.

### The transformation — p. 95

The motivating sentence, p. 95, verbatim:

> The confidence interval in (1.6.9) is suitable in situations when γᵢ is not
> bounded. When γᵢ is contained in some bounded interval, γ_L < γᵢ < γ_U, the
> confidence interval (1.6.29) will not necessarily lie entirely in this
> interval even if γ̂ᵢ does. A suitable confidence interval may then be
> obtained by using a transformation.

Let h(γᵢ) be "a monotonic increasing function of γᵢ with unbounded range":

- **Eqs. (1.6.30a/b), p. 95** — θ = h(γᵢ) and θ̂ = h(γ̂ᵢ), both on
  γ_L < γᵢ < γ_U.
- **Eqs. (1.6.31a/b/c), p. 95** — the interval on θ, which is the **symmetric**
  one:

  θ̂_L < θ < θ̂_U,  where  θ̂_L = θ̂ − c_α σ̂(θ̂)  and  θ̂_U = θ̂ + c_α σ̂(θ̂)

- **Eq. (1.6.32), p. 95** — the delta-method standard error on the transformed
  scale:

  σ̂(θ̂) = { ∂h/∂γᵢ |_(γ = γ̂) } σ̂(γ̂ᵢ)

- **Eq. (1.6.33), p. 95** — the inverse transformation γᵢ = h⁻¹(θ).
- **Eq. (1.6.34), p. 96** — the resulting interval on γᵢ:

  h⁻¹(θ̂_L) < γᵢ < h⁻¹(θ̂_U)

**The asymmetry statement, p. 96, verbatim** — this is the sentence
`grassi2010.md` is standing on:

> In general the confidence interval in (1.6.34) will not be symmetric about
> γ̂ᵢ but unlike (1.6.9) cannot include inadmissible values of γᵢ below γ_L or
> above γ_U.

And the reason the transformed scale is preferred, p. 95:

> Since θ is not bounded the distribution of θ̂ will generally be approximated
> more closely by a normal distribution than will the distribution of γ̂ᵢ,
> particularly if γᵢ is close to γ_U or γ_L.

### The three named transformations — pp. 96

- **Fisher z, for a correlation (−1 < γᵢ < 1)** — eqs. (1.6.35)–(1.6.37):

  θ̂ = h(γ̂ᵢ) = ½ logₑ{ (1 + γ̂ᵢ) / (1 − γ̂ᵢ) }
  σ̂(θ̂) = σ̂(γ̂ᵢ) / (1 − γ̂²ᵢ)
  h⁻¹(θ̂) = {exp(2θ) − 1} / {exp(2θ) + 1}

- **Logarithmic, for a variance or standard deviation (γᵢ > 0)** — eqs.
  (1.6.38)–(1.6.40). **This is the instance the repo relies on.** Introduced,
  p. 96 verbatim: "If γᵢ represents a variance (such as a diagonal element of
  D_ψ in (1.1.15)) or a standard deviation so that γᵢ > 0, a logarithmic
  transformation is suitable":

  θ̂ = h(γ̂ᵢ) = logₑ γ̂ᵢ                    (1.6.38)
  σ̂(θ̂) = σ̂(γ̂ᵢ) / γ̂ᵢ                      (1.6.39)
  h⁻¹(θ̂) = exp(θ̂)                          (1.6.40)

- **Logit-like, for a proportion (0 < γᵢ < 1)** — eq. (1.6.41),
  θ̂ = h(γ̂ᵢ) = −logₑ(γ̂ᵢ⁻¹ − 1), introduced for the case where γᵢ "represents
  the ratio ψᵢᵢ/σᵢᵢ of a residual variance to an observed variable variance".
  Its σ̂(θ̂) and h⁻¹ fall on p. 97 and are **not on the shelf** — observed
  2026-07-19.

### Composing (1.6.38)–(1.6.40): the interval the repo reconstructs

Substituting the log transformation into (1.6.31) and inverting through
(1.6.34) gives, in closed form:

  γ̂ᵢ · exp( ± c_α σ̂(γ̂ᵢ) / γ̂ᵢ )

That expression is `tests/testthat/test-cpm_oracles.R:136` term for term, with
γᵢ = vᵢᵢ and c_α = 1.96:

```r
v_ci <- app$v[1] * exp(c(1, -1) * 1.96 * app$v_se[1] / app$v[1])
```

The `c(1, -1)` ordering — upper first — is the code's, not the source's: the
endpoints are reordered because the map that follows (eq. 4 of
`browne1992.md`) is *decreasing* in v, so the larger v yields the smaller ρ.

## What this does and does not license

The chain running from these pages to CircE's printed communality intervals
has three links, and only the first is Browne (1982):

1. **(1.6.38)–(1.6.40), these pages** — a symmetric interval on ln vᵢᵢ,
   inverted to a nonsymmetric interval on vᵢᵢ that cannot go below zero.
2. **`browne1992.md` eq. (4), p. 472** — ρ(xᵢ, cᵢ) = (1 + vᵢᵢ)^(−1/2),
   a monotone *decreasing* map from vᵢᵢ onto (0, 1].
3. **`grassi2010.md`, p. 57 and Appendix A p. 71** — the decision to report
   the interval on ρ rather than on v, and the "approximate 95% one at time
   confidence intervals" label.

Link 2 is Browne's, but from the *other* paper; link 3 is Grassi's. Browne
(1982) states neither, and the phrase "communality-CI derivation attributed to
Browne (1982)" — which `grassi2010.md` carried before M42 — overstates what
these two pages contain. Since h⁻¹ ∘ (decreasing map) is still monotone, the
composition inherits (1.6.34)'s admissibility guarantee, so the result is
correct; the correction is to the attribution, not to the arithmetic.

## Traces to

- `tests/testthat/test-cpm_oracles.R:131-137` — the oracle assertion that
  reconstructs CircE's communality CIs; line 136 is eqs. (1.6.38)–(1.6.40)
  composed, and lines 132-135 are the comment naming this source.
- `cairn/references/grassi2010.md` — the citing page, whose Appendix A
  communality-CI table is what the reconstruction is checked against, and
  whose Browne-1982 open question this page resolves (M42 T5).
- `cairn/references/browne1992.md` — supplies eq. (4), link 2 of the chain
  above.

**Nothing in `R/` traces here**, stated precisely: no shipped function forms a
confidence interval by this source's route — transform to an unbounded scale,
take the symmetric normal interval there, invert. The package offers two ζ
interval routes and neither is that one: a bootstrap **percentile** interval
(`R/cpm_fit.R:1316-1323`), which is asymmetric about ζ̂ but gets its asymmetry
from the replicate distribution rather than from a transformation, and an
analytic **Wald** interval ζ̂ ± z·σ̂(ζ̂) (`R/cpm_fit.R:1644-1645`), which is
symmetric on the natural scale and is exactly the form eq. (1.6.29) describes
and eqs. (1.6.30)–(1.6.34) exist to improve on. Neither transforms first.
Two `R/` sites do apply a transformation
and are **not** instances of this method: `R/ssm_montecarlo.R:158` wraps
Monte Carlo *draws* in `atanh`/`tanh` (variance stabilization for simulation,
not an interval), and `R/cpm_fit.R:244` exponentiates the log-scale
optimization parameter (the unconstrained parameterization, not an interval).
`structure_fisher()` in `R/fit_structure.R` is Fisher's equal-axes test and
unrelated to Fisher's z. Checked by grep over `R/` for `exp(`/`log(`/`atanh`
near standard errors — observed 2026-07-19.

## Open questions

- **The full citation is unverified against the source.** The shelf holds two
  interior pages only; volume, editor, publisher, and the 72–141 page range
  come from `grassi2010.md`'s reference list. Obtaining a title page, or any
  page carrying the running head, would close this — observed 2026-07-19.
- **Eq. (1.6.9), referenced twice on these pages, is not on the shelf.** Both
  references (p. 95's "The confidence interval in (1.6.9) is suitable" and
  p. 96's "unlike (1.6.9) cannot include inadmissible values") read as though
  they mean (1.6.29), which is the interval just derived and the one with the
  admissibility problem being fixed. The two pages are internally consistent
  in using the "(1.6.9)" form, so this is either a genuine earlier equation
  identical in content or a repeated typo in the print — **undecidable from
  the shelf**, and deliberately not resolved here. Nothing the repo does
  depends on the answer — observed 2026-07-19.
- **The p. 97 continuation of eq. (1.6.41) is missing**, so the
  proportion-transformation instance is banked incomplete. Nothing uses it —
  observed 2026-07-19.
- **No value on this page has been read by a human**, and here the usual
  two-channel reassurance is weaker than elsewhere: the text layer carries no
  equation at all (M42-D1), so every equation above rests on a single read of
  a single scan. A human read of pp. 95–96 would close this — observed
  2026-07-19.
