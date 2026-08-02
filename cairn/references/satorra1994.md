# satorra1994 — the scaled test statistic T̄ = T/c and its scaling factor c = tr(UΓ)/r

**Provenance.** Ingested 2026-08-02 by M68 from
`cairn/references/sources/satorra1994.pdf` (gitignored), a 21-page scan of the
chapter alone. Pagination: the source's own, printed 399–419 on the pages
themselves; PDF page 1 is printed page 399, so printed page N is PDF page
N − 398. Equation numbering is the volume's chapter-internal `[16.n]` scheme
(this is chapter 16).
Extraction: verified 2026-08-02 against the source by a read of the rendered page images for every equation quoted below (pp. 401–403, 406–407) — the PDF is an Acrobat "Paper Capture" OCR scan and its `pdftotext -layout` text layer **drops the display equations of the two that matter most, 16.21 and 16.22, leaving only their equation numbers**, so per M42-D1 the page images are authoritative here and the text layer served only to locate sections and cross-check running prose; no value here has been read by a human — observed 2026-08-02.

**Citation.** Satorra, A., & Bentler, P. M. (1994). Corrections to test
statistics and standard errors in covariance structure analysis. Chapter 16 in
*Latent variables analysis: Applications for developmental research*
(pp. 399–419). **What the shelf attests and what it does not:** the chapter
title, authors, chapter number, and printed page range are on the pages
themselves; the volume title comes from the scan's own PDF metadata
(`Latent variables analysis : applications for developmental research`,
Internet Archive). The shelf holds no title page, so **the editors
(conventionally von Eye & Clogg) and the publisher (conventionally Sage,
Thousand Oaks) are not attested by anything on the shelf** and are deliberately
omitted above rather than transcribed from memory — observed 2026-08-02.

**Role.** The published warrant for the scaling correction `R/axes_scaled_fit.R`
implements. `axes_reliability()` fits its model to the item **correlation**
matrix as if it were a covariance matrix, so lavaan's χ² is referenced against
a normal-theory sampling distribution the sample correlation matrix does not
have. This chapter supplies the general fix — a mean-correcting scale factor
built from the model's own Jacobian and the actual asymptotic covariance of the
analyzed moments — and, at p. 401, the sentence that licenses applying it to
moments that are not variances and covariances.

## Extracted values

### The licensing sentence — p. 401

The chapter's own scope statement, verbatim, in the paragraph defining Γ and Δ:

> Even though we consider the case of covariance structure analysis, the theory
> to be described is general, and **σ** could be a vector on any type of
> moments. For example, *s* could be a vector of polychoric correlations, in
> which case **Γ** would be the asymptotic variance matrix of *s*.

This is what carries the machinery from covariances onto the sample
**correlation** matrix: Γ is defined as the asymptotic covariance of whatever
moment vector is analyzed, not specifically of covariances. The example given
is polychoric correlations; Pearson correlations are the same substitution.

### Notation — pp. 401–403

Quantities named as the source names them:

- **σ** — a p\* vector of population moments; **s** the corresponding sample
  moments; H₀: σ = σ(θ) — p. 401.
- **p\* ≡ p(p + 1)/2** for covariance structures, p the number of observed
  variables; **q** the number of free parameters — p. 401.
- **Γ** — "the asymptotic covariance matrix of n^½ s", assumed non-singular —
  p. 401. Note the n^½ scaling: Γ prices the variability of √n·s, so a factor
  built from it is free of n.
- **Δ ≡ ∂σ/∂θ′** — "the Jacobian matrix … evaluated at the true parameter
  value", assumed of full column rank — p. 401.
- **df r = p\* − q** — p. 402, stated as "T₁ is asymptotically chi-square
  distributed with degrees of freedom (df) r = p\* − q".
- **V** — the LS weight matrix in F(θ, Vₙ) = [s − σ(θ)]′Vₙ[s − σ(θ)], eq.
  **[16.1]**, p. 401. For ML, eq. **[16.4]**, p. 402:
  V*ₙ* = 2⁻¹D′(A*ₙ*⁻¹ ⊗ A*ₙ*⁻¹)D, with D the duplication matrix and A*ₙ* "any
  sequence of matrices that converges to Σ with probability one".
- **T₁ ≡ nF(θ̂, Vₙ)**, eq. **[16.2]**, p. 401 — the standard goodness-of-fit
  statistic. T₂ (eq. 16.6, p. 403) and T₃ (eq. 16.8, p. 403) are the other two
  variants the chapter treats; the correction below applies to all three.

The ML-estimate sandwich, eq. **[16.10]**, p. 403 — quoted because it is the
formula `R/axes_corrected_se.R` is a specialization of:

  acov(θ̂) = n⁻¹(Δ̂′VₙΔ̂)⁻¹ Δ̂′Vₙ Γₙ Vₙ Δ̂ (Δ̂′VₙΔ̂)⁻¹

### The asymptotic distribution of T — p. 406

- **Eq. [16.17], p. 406** — T converges in law to a weighted sum of independent
  1-df chi-squares:

  T →ᴸ τ = Σᵢ αᵢτᵢ = trace{UΓ}

  where "{αᵢ}, i = 1, 2, …, r are the nonnull eigenvalues of UΓ", r = p\* − q,
  and "the τᵢ's are independent chi-square variables with 1 df".

- **Eq. [16.18], p. 406** — the residual-projector matrix, verbatim:

  **U = H − HΔ(Δ′HΔ)⁻¹Δ′H**

  "and H = V (for T₁ or T₂) or H = C⁻¹ (for T₃)."

- **Eq. [16.19], p. 406** — E(τ) = Σᵢ αᵢ = trace{UΓ}.
- **Eq. [16.20], p. 406** — Var(τ) = 2Σᵢ αᵢ² = trace{(UΓ)²}.

Consistency, p. 406: "Let U*ₙ* and Γ*ₙ* be consistent estimates of U and Γ
respectively… The mean and variance of the asymptotic distribution of T will be
consistently estimated respectively by trace{U*ₙ*Γ*ₙ*} and
2trace{(U*ₙ*Γ*ₙ*)²}."

### The scaled statistic — p. 407

The two equations the shipped code implements, quoted verbatim from the page
image (**the text layer drops both**):

- **Eq. [16.21], p. 407** —  T̄ ≡ c⁻¹T
- **Eq. [16.22], p. 407** —  c ≡ trace{U*ₙ*Γ*ₙ*/r}

"and T̄ is referred to a chi-square distribution with r df."

The justification, p. 407, in the source's own three cases over the dispersion
of the nonnull eigenvalues αᵢ:

> a. all αᵢ's equal to one, thus T →ᴸ χ²ᵣ
> b. all αᵢ's equal to α, and thus T →ᴸ τ = Σ ατᵢ = α Σ τᵢ = α χ²ᵣ
> c. the αᵢ's are unequal, thus T →ᴸ τ = Σ αᵢτᵢ.

and, verbatim:

> Note that c of (16.21) estimates Σᵣ₁ αᵢ/r, which equals 1 in case (a), and α
> in case (b). Thus, in contrast with T, which is only exactly chi-square
> distributed in case (a), T̄ will have an asymptotically exact chi-square
> (df = r) distribution in case (a) and also in case (b). Moreover, in case (c)
> one is inclined to conjecture that T̄ will be better approximated by a
> chi-square variate than the uncorrected statistic T. To support this
> conjecture, note that the asymptotic distribution of T̄ and χ²ᵣ agree in mean.

**This is the exact strength of the correction, and it is worth stating plainly
because it is easy to overclaim: the scaled statistic matches the reference
chi-square in *mean*, always; it is *exactly* chi-square only when the αᵢ are
all equal.** Case (c) — unequal αᵢ — is a conjecture the authors argue for, not
a theorem, and is the case the axes model is in.

### The distinction from the Browne / Shapiro–Browne elliptical correction — p. 407

> When the distribution of z is elliptical, Satorra and Bentler (1986a, sec. 5)
> show that the scaling factor c provides an estimate of the common relative
> kurtosis of z. … It needs to be stressed, however, that the Shapiro-Browne
> scaling correction is justified only when z follows an elliptical
> distribution. In applications, as will be seen below, the Shapiro-Browne
> correction can distort considerably the chi-squaredness of the statistic. In
> contrast, (16.21) will be seen to provide an approximately valid chi-square
> distribution even when the data are not elliptical.

Recorded because the two are routinely conflated: the elliptical statistic
(eq. **[16.28]**, p. 409, dividing by a rescaled Mardia kurtosis measure k) is
**not** what this repo computes, and its assumption is not one the repo could
meet.

### The adjusted (Satterthwaite-type) statistic — pp. 407–409

The chapter's second correction, T̄̄, matches mean *and* variance by also
adjusting the df to a noninteger d′; p. 409 notes that "for noninteger df, one
would instead compute T̄̄ = (d′/trace{UₙΓₙ})T and use the fractional degrees of
freedom". **The repo does not implement this** — `$fit$df` is unchanged by M68
— so it is banked, not used.

### Scaling corrections for standard errors — §5, pp. 410–411

The same factor c scales the information-matrix variances. p. 411, following
eq. **[16.34]** (trace{UΓ} = trace{U(αV⁻¹ + βΔδδ′Δ′)} = trace{UV⁻¹α} = rα):

> which suggests the statistic given in (16.22), that is, c = trace{UₙΓₙ}/r, as
> an estimate of the scaling factor α. That is, the conventional variances
> provided by the typical inverse of the "information matrix" expression
> (Δ′VΔ)⁻¹ are simply multiplied by c.

This holds under the chapter's **Quasi Linear (QL)** condition, eq. **[16.30]**,
p. 410: σ = Δδ for a q × 1 vector δ. Banked as background for M66's SE
correction; M66 does **not** take this route — it computes the corrected acov
exactly for the model's linear structure rather than scaling the naive one — so
this section is context, not the warrant for `R/axes_corrected_se.R`.

## What this does and does not license

Three links stand between this chapter and the number `axes_reliability()`
prints, and only the first two are Satorra & Bentler's:

1. **Eqs. (16.17)–(16.22), pp. 406–407** — given Γ, the asymptotic covariance
   of the analyzed moments, and Δ and H, the scaled statistic T/c has the
   reference chi-square's mean. **This is general and fully licensed.**
2. **p. 401's scope sentence** — Γ may be the acov of *any* moment vector, so
   substituting the acov of the sample **correlations** for that of the sample
   covariances is the chapter's own contemplated use, not an extension of it.
3. **The specific Γ_R this repo forms, and the choice H = V at Σ̂** — the
   chapter names neither. Γ_R for Pearson correlations under normality is
   standard elsewhere; that it is correct for *this* model at *this* Σ̂ is what
   M68's own oracles (AC2's explicit vech-space recomputation, AC3/AC4's
   simulation coverage) exist to establish. **Nothing in these pages checks
   link 3**, and a reader arriving from `R/axes_scaled_fit.R`'s citation should
   expect the machinery, not the application.

The chapter is also silent on **CFI's baseline factor**: `c_b` — scaling the
independence model's own statistic by its own factor — is this repo's
construction by analogy, not a published recommendation. It follows from
applying (16.21)–(16.22) to the baseline fit exactly as to the target fit, but
the chapter never discusses incremental fit indices.

## Traces to

- `R/axes_scaled_fit.R` — implements eqs. (16.18), (16.21) and (16.22); the
  file header cites this page.
- `tests/testthat/test-axes-scaled-fit.R` — the closed-form vech-space oracle,
  which rebuilds U from eq. (16.18) as a literal matrix.
- `cairn/references/cudeck1989.md` — the companion page, which states the
  *problem* (a covariance structure fitted to a correlation matrix gets its
  standard errors and test statistic wrong) this chapter supplies the machinery
  to fix. Neither page contains the other's half.

## Open questions

- **The volume's editors and publisher are not attested on the shelf.** The
  scan holds the chapter only — no title page, no copyright line. Obtaining
  either would close this — observed 2026-08-02.
- **No value on this page has been read by a human**, and for eqs. (16.21) and
  (16.22) the usual two-channel reassurance is unavailable: the OCR text layer
  carries neither equation, so both rest on a single read of a single scan
  (M42-D1's situation, reproduced). A human read of pp. 406–407 would close
  this — observed 2026-08-02.
- **Sections 6 onward (pp. 411–419) — the Monte Carlo study and the discussion
  of related literature — are read only in the text layer**, not against the
  page images. Nothing in the repo depends on them; a value taken from them
  later must be re-read against the images first — observed 2026-08-02.
