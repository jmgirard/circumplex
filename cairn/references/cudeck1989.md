# cudeck1989 — why a covariance structure fitted to a correlation matrix gets its standard errors and test statistic wrong

**Provenance.** Ingested 2026-08-02 by M68 from
`cairn/references/sources/cudeck1989.pdf` (gitignored), an 11-page scan of the
article alone. Pagination: the journal's own, printed 317–327; PDF page 1 is
printed page 317, so printed page N is PDF page N − 316.
Extraction: verified 2026-08-02 against the source by a read of the rendered page images for every quotation below (pp. 319–323) — this scan's `pdftotext -layout` text layer is far better than `satorra1994.pdf`'s (it is a typeset scan, and it carries display equations), but its two-column reflow interleaves the columns, so the page images are authoritative for anything read in order; the text layer served to locate sections and to cross-check wording, giving two channels on every quotation here; no value has been read by a human — observed 2026-08-02.

**Citation.** Cudeck, R. (1989). Analysis of correlation matrices using
covariance structure models. *Psychological Bulletin, 105*(2), 317–327. The
copyright line printed on p. 317 reads "Copyright 1989 by the American
Psychological Association, Inc." with "0033-2909/89/$00.75"; the running head is
"COVARIANCE STRUCTURES WITH CORRELATION MATRICES", which differs from the title.
No DOI is printed on the article — observed 2026-08-02.

**Role.** The published statement of the *problem* `axes_reliability()` has and
both M66 and M68 correct. `axes_reliability()` fits a covariance structure to a
matrix of sample correlations (Strack et al.'s own LISREL practice), and this
article is the canonical account of what that costs. It is cited without a page
at `R/axes_corrected_se.R` today; M68 supplies the anchors. It supplies the
diagnosis, **not** the fix — for the fix see `satorra1994.md`.

## Extracted values

### The three errors — abstract, p. 317

The article's own enumeration, verbatim from the abstract:

> Depending upon the model, applying a covariance structure to a matrix of
> correlations may (a) modify the model being studied, (b) produce incorrect
> values of the omnibus test statistic, or (c) yield incorrect standard errors.
> An important class of models are those that are scale invariant (Browne,
> 1982), for then Errors a and b cannot occur when a correlation matrix is
> analyzed.

Note carefully what the escape clause covers: scale invariance rules out (a) and
(b) — it does **not** rule out (c). p. 323 says so directly (below).

### Scale invariance, defined — p. 319

The definition as printed (the *Definition* paragraph on p. 319 defines
scale-*free*; scale *invariance* is defined on p. 318 and restated in the
summary paragraph of p. 319, quoted here because it is the operative form):

> Scale invariance is a property of a model such that any rescaling of the
> covariance matrix yields another covariance matrix that also satisfies the
> model. A parameter in a scale-invariant model is scale-free if it remains
> unchanged in all rescalings of the covariance matrix.

Operationally, from the worked Model-1 display at the top of p. 319: a structure
Σ(γ) is scale invariant if for any admissible diagonal **D**ₐ there is a
parameter vector γ\* with Σ(γ\*) = **D**ₐΣ(γ)**D**ₐ.

**The axes-reliability model is not scale invariant**, and the check is one
line: its implied covariance is
Σ = ξ₁C + ξ₂J + ζ₁B + ζ₂K + diag(ε) with C*ᵢⱼ* = cos(θ*ᵢ* − θ*ⱼ*), so the
(i, j) off-diagonal of **D**ₐΣ**D**ₐ is d*ᵢ*d*ⱼ*(ξ₁cos(θ*ᵢ* − θ*ⱼ*) + …), which
is of the model's form only if d*ᵢ*d*ⱼ* is constant over i ≠ j — that is, only
for **D**ₐ = cI. So all three of Cudeck's errors are in scope for this model,
subject to the scoping paragraph below. (This derivation is the repo's, not the
article's; the article never treats a circumplex structure.)

### Error (b), the omnibus test statistic — pp. 320–321

The hypothesis, eq. **(11)**, p. 320: H₀: Σ = Σ(γ). The ML discrepancy
function, eq. **(12)**, p. 321:

  M = tr(SΣ⁻¹) − log|SΣ⁻¹| − p

with, p. 321, "the statistic (n − 1)M for evaluating Hypothesis 11 is
distributed in large samples as χ², with df = ½p(p + 1) − q".

Fitting the same structure to R minimizes a *different* function, eq. **(13)**,
p. 321:

  M̃ = tr(RΣ̃⁻¹) − log|RΣ̃⁻¹| − p = tr[S(**D**ₛΣ**D**ₛ)⁻¹] − log|S(**D**ₛΣ**D**ₛ)⁻¹| − p

and the consequence, p. 321, verbatim:

> Rather obviously, at the respective minima, M̃ ≠ M, except when **D**ₛ = I. …
> This means that applying a model to S in general will produce a value of the
> test statistic different than that obtained from a corresponding analysis of
> R. In practical terms, it raises the undesirable possibility that two
> researchers examining the same model with the same data could reach
> substantively different conclusions about the plausibility of the model
> depending only on the scaling of the sample data.

The worked numbers, p. 322: for the scale-invariant Model 1, "one obtains
χ² = 0.70 with both S and R"; for the non-invariant Model 2, "the test statistic
χ² = 5.81 with sample covariances, but χ² = 1.50 with sample correlations."

### Error (c), standard errors — pp. 322–323

p. 322, verbatim:

> Most computer programs, following the usual derivation of standard errors
> (e.g., Lawley & Maxwell, 1971), assume that se(γ̂ᵢ) is estimated from a
> covariance matrix. When a correlation matrix is used instead, these procedures
> give incorrect results (de Pijper & Saris, 1982, section 1). Consequently, at
> least some standard errors are wrong in virtually all reported analyses of
> correlation structures.

p. 323, the scope statement — **this is the sentence `R/axes_corrected_se.R`
stands on**:

> If a model that is not scale invariant is applied to a correlation matrix with
> most computer programs, all of the estimated standard errors will be wrong. If
> a scale-invariant model is applied to a sample correlation matrix, the
> standard errors associated with scale-free parameters will be correct, but
> standard errors associated with scale-dependent parameters will be incorrect.

And that corrections exist but were not shipped, p. 323:

> Formulas are available that give correct standard errors for scale-invariant
> models when a correlation matrix is analyzed (see Lawley & Maxwell, 1971,
> sections 5.3, 7.7, for some factor analysis models estimated by maximum
> likelihood, and Browne, 1982, section 1.6, for other models and other
> estimation methods); but as of this writing, these corrections have not been
> included in most computer programs.

**Magnitude, Table 4, p. 323** — the article's own worked comparison of
uncorrected against corrected standard errors for a six-indicator two-factor
model, and the size of the error, verbatim:

> In the worst case, the standard deviation associated with λ₄ and λ₅ is
> incorrect by a factor of 100(.068 − .046)/.046 = 48%.

Table 4's own pairs (Parameter — Estimate — Uncorrected — Corrected), p. 323:
λ₁ .690 / .076 / .059; λ₂ .671 / .076 / .060; λ₃ .531 / .076 / .064;
λ₄ .764 / .068 / .046; λ₅ .764 / .068 / .046; λ₆ .616 / .069 / .053;
φ₂₁ .598 / .072 / .072 (the one scale-free parameter, "identical in the two
sets of coefficients"); ψ₁ .524 / .082 / .082; ψ₄ .416 / .069 / .070;
ψ₆ .620 / .072 / .065.

Recorded because it is a **published, independent confirmation of the direction
and rough magnitude** M66 measured for this repo: uncorrected normal-theory
standard errors on a correlation-matrix fit are mostly *too large*, by tens of
percent, with scale-free parameters unaffected. It is corroboration only — a
different model, different parameters — never an oracle for any repo number.

## What this does and does not license

The article licenses M66's and M68's *premise*, and neither one's *formula*.

**What it licenses.** That fitting this model to R with a program assuming S is
a real error rather than a rounding concern (p. 323's "all of the estimated
standard errors will be wrong"); that the error is large enough to matter
(Table 4's 48%); that corrections exist in the literature (p. 323's pointer to
Browne, 1982, §1.6); and that the profession had been ignoring it.

**What it does not license, and the distinction matters.** Cudeck's Error (b) is
**not** the error M68 corrects. His (b) is that the *value* of the discrepancy
function differs between an S-analysis and an R-analysis of the same structure —
a statement about which minimum you land on, made in a setting where a
covariance structure is the intended object and R is a rescaling of the data.
M68's error is different: this repo's estimand is defined on the population
**correlation** matrix by design, so there is no rival S-analysis to disagree
with, and the fitted value of T is the one the repo wants. What is wrong is its
**reference distribution** — T is referred to a χ² derived for a
Wishart-distributed S, while the analyzed moments are correlations, whose
sampling variability differs cell by cell. That is a sampling-distribution
claim, and **this article does not make it**; `satorra1994.md` (pp. 406–407)
supplies it.

The same distinction applies to `R/axes_corrected_se.R`'s header, which reads
this article as backing "the Browne/Cudeck corrected asymptotic covariance
specialized to that linear structure". Precisely: Cudeck states that a
correction is needed and points at Browne (1982, §1.6) for one; **he prints no
such formula in this article**. The formula M66 implements is derived in the
repo from the model's linear structure and validated by the repo's own oracles.
Note also that the pointer lands *inside* §1.6 of Browne (1982) — of which this
repo shelves only pp. 95–96, the CI-transformation part, which is a different
subsection and carries no corrected acov (see `browne1982.md`). **The Browne
§1.6 pages Cudeck points at are not on the shelf** — observed 2026-08-02.

## Traces to

- `R/axes_corrected_se.R:1-29` — the file header's diagnosis of the
  correlation-as-covariance problem, and its `(Cudeck, 1989)` citation at
  line 22, which M68 anchors to a page.
- `R/axes_scaled_fit.R` — the same premise on the test-statistic side.
- `cairn/references/satorra1994.md` — the companion page carrying the fix this
  page's problem statement calls for.
- `cairn/references/browne1982.md` — the shelf's partial copy of the §1.6
  Cudeck points at for correction formulas; the shelved pages are not the ones
  he means.

## Open questions

- **Whether Browne (1982) §1.6 in fact contains a corrected asymptotic
  covariance for this setting is unverified**, because the shelf holds only
  pp. 95–96 of that chapter and the corrected-acov material is not on them.
  Nothing in the repo depends on the answer — M66's formula is derived and
  oracle-validated in-repo, not transcribed from Browne — but the header
  wording at `R/axes_corrected_se.R:21-22` invokes the attribution. Obtaining
  the rest of Browne (1982) §1.6 would close this — observed 2026-08-02.
- **pp. 324–327 (multiple-group rescaling, the correlation-structure estimation
  approaches, and the discussion) are read only in the text layer**, not against
  the page images. Nothing in the repo depends on them — observed 2026-08-02.
- **No value on this page has been read by a human**, though unlike
  `satorra1994.md` every quotation here has two independent channels (page image
  and OCR text layer) that agree — observed 2026-08-02.
