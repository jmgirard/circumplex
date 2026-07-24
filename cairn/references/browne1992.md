# browne1992 — the CPM the package estimates, specified in full

**Citekey trap.** This page is Browne **alone**. The citekey `browne1992a` —
with the `a` suffix — belongs to Browne **& Cudeck** (1992), *Alternative ways
of assessing model fit*, *SMR* 21(2) 230–258. The suffix marks a **different
author set**, not a second work by the same author, which is how an
alphabetical suffix normally reads. Both pages carry this warning; `browne1992a.md`
carries the reciprocal.

**Provenance.** Ingested 2026-07-19 by M42 from
`cairn/references/sources/browne1992.pdf` (gitignored).
Pagination: the journal's own, *Psychometrika* 57(4) 469–497; the shelf PDF is
29 pages and PDF page *n* is printed page *n* + 468, confirmed by the running
head on each page read rather than by arithmetic.
The shelf copy is a **scan of a marked-up print copy** — yellow highlighter
runs across printed pp. 471, 486 and elsewhere. The highlights are a prior
reader's and carry no bibliographic meaning; they are noted because they are
visible in the page images this page was read from.
Extraction: verified 2026-07-19 against the source by a read of the `pdftoppm`-rendered page images for every equation and value recorded here, cross-checked against `pdftotext -layout`; per M42-D1 the PDF is an Acrobat "Paper Capture" **OCR scan**, so the text layer is OCR output of those same images and is a cross-check for reading slips only, never an independent witness — a defect in the scan itself would be caught by neither channel, and no value here has been read by a human — observed 2026-07-19.

**Citation.** Browne, M. W. (1992). Circumplex models for correlation matrices.
*Psychometrika, 57*(4), 469–497. The article carries no printed DOI. It is a
Psychometric Society presidential address ("This address will be concerned
with…", p. 471).

**Role.** The model `cpm_fit()` estimates. Everything the engine computes —
the correlation function, the model-implied matrix, the discrepancy, the
parameter count, the degrees of freedom, the communality index, the Heywood
definition — is this paper's, and the engine's `zeta` is this paper's ζ*ᵢᵢ.
This page carries the **full specification of the implemented model**, so a
reader can map every quantity `R/cpm_fit.R` estimates to its published
counterpart without reopening the paper. See "Not extracted" for what is
deliberately absent.

## Extracted values

Equation numbers below are the paper's own. Sections are the paper's own.

### §2, pp. 471–472 — the measurement model

- **Eq. (1), p. 471** — the data model, assumed throughout:

  x = μ + D_ζ(c + u)

  with 𝓔c = 0, Cov(c, u′) = 0, Cov(c, c′) = P_c "so that the cᵢ have unit
  variances", Cov(u, u′) = D_v diagonal, μ a p × 1 vector of manifest means,
  and D_ζ "a diagonal scaling matrix which allows different measurement scales
  for different manifest variables".

- **Eq. (2), p. 471** — the covariance structure it generates:

  Σ_x = D_ζ(P_c + D_v)D_ζ

- **Eq. (3), p. 471** — the corresponding correlation structure:

  P_x = D*_ζ(P_c + D_v)D*_ζ,  where  D*_ζ = Diag^(−1/2)[Σ_x] D_ζ

- **Eq. (4), p. 472** — the **communality index**, "a more interpretable
  quantity" than vᵢᵢ:

  ρ(xᵢ, cᵢ) = ( 1 / (1 + vᵢᵢ) )^(1/2)

  "which is the correlation between a manifest variable, xᵢ, and its common
  part, cᵢ. Alternatively, the communality or squared correlation ρ²(xᵢ, cᵢ)
  may be considered."

- **Eq. (3b\*), p. 472** — the identity that makes the index a structural
  parameter of the correlation model:

  ζ*ᵢᵢ = ρ(xᵢ, cᵢ)

  i.e. the diagonal elements of D*_ζ in (3) **are** the communality indices,
  "and are therefore functions of the vᵢᵢ given in (4)".

- **Heywood case, p. 472, verbatim**: "a zero estimate of a vᵢᵢ will be
  referred to as a 'Heywood case'. Equivalently, a Heywood case occurs if an
  estimate of a communality index, ρ(xᵢ, cᵢ), is equal to one."

### §3, pp. 472–473 — fitting and fit measures

- **Eq. (5), p. 472** — the normal-theory ML discrepancy function:

  F(S, Σ) = ln|Σ| − ln|S| + tr[SΣ⁻¹] − p

- **Eq. (6), p. 473** — degrees of freedom, for a structure with q free
  parameters:

  d = ½p(p + 1) − q

- **Eq. (7), p. 473** — Steiger & Lind's RMSEA, ε_a = (F₀/d)^(1/2), with the
  cutoff sentence, p. 473 verbatim: "One may regard a value of ε_a that is
  0.05 or less as indicating a close fit of the model and a value up to 0.08
  as indicating a reasonable fit."

- **Eq. (8), p. 473** — the point estimate:

  ε̂_a = { Max( (n × F̂ − d)/(n × d), 0 ) }^(1/2)

  **This is algebraically the same estimator as `browne1992a.md`'s eq. 13**,
  which prints it as √(Max{F̂/d − 1/n, 0}) — the two arrangements are equal
  term for term, since (nF̂ − d)/(nd) = F̂/d − 1/n. The package implements the
  `browne1992a` arrangement (`R/cpm_fit.R:1085`). n = N − 1 here (p. 473).

- **Scale invariance, p. 473, verbatim**: "Since all models under
  consideration will be special cases of model (2), and this is invariant
  under changes of scale, analyses may be performed on the sample correlation
  matrix, **R**, instead of on **S** without the fit measures being affected.
  The only estimates that will be affected are the estimates of the ζᵢᵢ, which
  are scale dependent and are therefore not of interest." This sentence is the
  paper's licence for the package's default `scaling = "unit"` family.

### §5.2, p. 477 — the six Requirements on a correlation function

Quoted as printed, since §6 discharges them one by one:

1. ρ(0) = 1.
2. ρ(θ_d) is continuous and monotonic decreasing on 0° ≤ θ_d ≤ 180°.
3. ρ(θ_d) = ρ(360 − θ_d).
4. ρ₁₈₀° ≥ −1. ("Requirements 1–4 ensure that correlations do not exceed one
   in absolute value.")
5. ρ(θ_d) = ρ(−θ_d), i.e. the correlation function is even.
6. "Given any p polar angles, θ₁, θ₂, …, θ_p, the correlation function
   generates a nonnegative definite p × p matrix P_c."

Also p. 477: because of Requirement 5, "the direction, clockwise or
anticlockwise, in which successive points are arranged around the circle is
arbitrary" — the published basis for the engine's reflection canonicalization.

### §6.4, pp. 485–486 — the FS correlation function (the implemented model)

- **Eq. (30), p. 485** — the Fourier-series (FS) correlation function:

  ρ(θ_d) = β₀ + Σ_{k=1..m} β_k cos(k × θ_d)

  "Coefficients of sine terms in the Fourier series must be zero since ρ(θ_d)
  must be an even function (Requirement 5)."

- **Eq. (31), p. 485** — the equality constraint enforcing Requirement 1:

  Σ_{k=0..m} β_k = 1,  so  β₀ = 1 − Σ_{k=1..m} β_k

  leaving "m free parameters, β₁, …, β_m, in the correlation function (30)".

- **Eq. (32), p. 485** — the inequality constraints:

  β_k ≥ 0,  k = 0, 1, …, m

- **Eq. (33), p. 485** — the minimal correlation:

  ρ₁₈₀° = 2 × (β₀ + β₂ + β₄ + ···) − 1 = 2 × Σ_{j=0..[m/2]} β_{2×j} − 1

- **Eq. (34), p. 486** — the same function written on the angle difference,
  which is the form the engine evaluates:

  ρ(cᵢ, cⱼ) = Σ_{k=1..m} β_k cos{k × (θⱼ − θᵢ)} + β₀

- **Free-parameter count, p. 486** — for the covariance structure obtained by
  substituting this P_c into (2): m weights β₁…β_m, p − 1 free polar angles
  θᵢ, p free unique variances vᵢᵢ, and p free scaling parameters ζᵢᵢ, yielding

  q = 3p + m − 1

- **ρ₁₈₀° = −1, p. 486**: constraining the minimal correlation to −1 requires
  the [m/2] + 1 equality constraints β₀ = β₂ = ··· = β_{2[m/2]} = 0.

- **Requirement 2 is not guaranteed for m > 1**, p. 486: it "will always be
  satisfied if m = 1 so that ρ(θ_d) = (1 − β₁) + β₁ cos(θ_d)"; for m > 1 it
  may fail, and enforcing it would need infinitely many constraints
  ∂ρ/∂θ_d > 0 on 0° ≤ θ_d ≤ 180°. The paper's only remedy is advisory:
  "if m is kept fairly small (substantially less than p/2) and the correlation
  matrix fitted does display a circumplex pattern, the fitted correlation
  function is likely to satisfy Requirement 2."

### §6.5, p. 487 — the equally spaced (circulant) special case

- **Eq. (35), p. 487** — fixed, equally spaced polar angles:

  θᵢ = (i − 1) × 360/p,  i = 1, …, p,  with m = [p/2] in (30)

- **Eq. (36), p. 487** — the linear map to Wiggins, Steiger & Gaelick's (1981)
  symmetric circulant: **ρ** = A**β** + **1**, with aᵢⱼ = cos(i × j × 360/p) − 1.

### §6.6, pp. 487–488 — the factor-analytic representation

- **Eq. (37), p. 487** — expanding cos(kθⱼ − kθᵢ) in (34):

  ρ(cᵢ, cⱼ) = Σ_{k=1..m} β_k{cos(kθᵢ)cos(kθⱼ) + sin(kθᵢ)sin(kθⱼ)} + β₀

- **Eq. (38), p. 487** — hence, when the β_k are nonnegative as in (32):

  P_c = Λ̄Λ̄′

- **Eq. (39), p. 487** — the p × (2m + 1) matrix Λ̄:

  λ̄_{i,2k−1} = β_k^(1/2) × cos(k × θᵢ)   } k = 1, …, m
  λ̄_{i,2k}   = β_k^(1/2) × sin(k × θᵢ)   }
  λ̄_{i,2m+1} = β₀^(1/2)

  From (38), rank(P_c) ≤ Min(p, 2m + 1), "Thus the FS correlation function
  satisfies Requirement 6 of section 5.2 whenever the inequalities in (32) are
  satisfied" (p. 488) — the published proof that β ≥ 0 buys positive
  semidefiniteness for free, at any angles.

- **Eq. (40), p. 488** — the manifest-variable factor model:

  Σ = ΛΛ′ + D_ψ,  where Λ = D_ζΛ̄ and D_ψ = D_ζ²D_v

  a factor analysis model "with 2m + 1 factors", whose 2pm + p loadings are
  functions of 2p + m − 1 free parameters: p scaling factors ζᵢᵢ, p − 1 free
  polar angles θᵢ, and m weights β₁…β_m.

### §6.7, pp. 488–490 — Browne's factor-analytic start values

Recorded for completeness of the specification; **the package implements none
of it** (see "Departures the parameter map found"). For m = 1, Λ̄ has the form
(41), p. 488, with columns β₁^(1/2)cos θᵢ, β₁^(1/2)sin θᵢ, β₀^(1/2). Given any
Λ satisfying (40): D_ζ² = Diag(ΛΛ′) (42); D_v = D_ζ⁻¹D_ψ (43); Λ* = D_ζ⁻¹Λ
(44); Λ̄ = Λ*U (45), where U is the 3 × 3 matrix of standardized eigenvectors
of the column covariance matrix of Λ*, eigenvalues descending — chosen to
minimize the variance of the last column of Λ̄, whose elements (41) are all
equal. Then

- **Eq. (46), p. 489** — β₀ = ( p⁻¹ Σᵢ λ̄_{i3} )², β₁ = 1 − β₀.
- **Eq. (47), p. 489** — ρ₁₈₀° = 2β₀ − 1 (the m = 1 case of (33)).
- **Eq. (48), p. 489** — after a second standardization
  λ̈ᵢⱼ = λ̄ᵢⱼ / (λ̄²ᵢ₁ + λ̄²ᵢ₂)^(1/2), j = 1, 2, the polar angles relative to a
  reference variable r with θ_r = 0:

  θᵢ = arc cos(λ̈ᵢ₁λ̈_{r1} + λ̈ᵢ₂λ̈_{r2})           if sin(θᵢ) ≥ 0
  θᵢ = 360 − arc cos(λ̈ᵢ₁λ̈_{r1} + λ̈ᵢ₂λ̈_{r2})     if sin(θᵢ) < 0

  Estimates from these are "consistent … not asymptotically efficient and are
  confined to the FS correlation function with m = 1" (p. 489); Jöreskog's
  (1963) IFA is used to obtain Λ̂ (p. 489). The paper's own use for them is as
  "initial approximations to initialize the iterative procedure for obtaining
  maximum likelihood estimates" (p. 490).

### §7, p. 490 — computation

All analyses in the paper were run with AUFIT (Browne & Du Toit, 1992) plus
purpose-written satellite subroutines; "the numerical and analytic derivatives
gave the same results". No estimation software of the paper's is available to
the repo, which is why the package's published-value oracle is `grassi2010.md`
(CircE), not this paper's own program.

## The parameter map (M42 T3)

Each quantity `R/cpm_fit.R` estimates against its published counterpart,
walked against the code.

| Code | Published counterpart | Where |
|---|---|---|
| `beta` (length m+1, indexed from k = 0, non-negative, sums to 1) | β₀…β_m of the FS correlation function | eqs. (30)–(32); `R/cpm_fit.R:30` (`cpm_rho`) |
| `cpm_rho(delta, beta)` | ρ(θ_d) evaluated on the angle difference | eq. (34); `R/cpm_fit.R:30` |
| `theta` (radians internally, degrees at the API) | the polar angles θᵢ, one held fixed at a reference | eq. (34) and §6.7's θ_r = 0 convention; `R/cpm_fit.R:142` (`reference`) |
| `zeta` (length p, in (0, 1]) | **ζ\*ᵢᵢ = ρ(xᵢ, cᵢ), the communality index** — *not* the unique variance vᵢᵢ and *not* its square | eqs. (3b\*) and (4); `R/cpm_fit.R:72` |
| `P = D_zeta C D_zeta + (I − D_zeta²)` | P_x of eq. (3) | verified below; `R/cpm_fit.R:72` (`cpm_implied_cor`) |
| `sigma` (free scaling, M18/D-009) | the scale-dependent ζᵢᵢ of D_ζ in the covariance structure | eq. (2); `R/cpm_fit.R:94` (`cpm_implied_cov`) |
| `cpm_discrepancy` | F(S, Σ), evaluated at R rather than S | eq. (5) + the p. 473 scale-invariance sentence; `R/cpm_fit.R:108` |
| `q` | the free-parameter count | eq. (6)'s q; §6.6's 2p + m − 1 (unit) / §6.4's 3p + m − 1 (free); `R/cpm_fit.R:180` |
| `df` | d | eq. (6); `R/cpm_fit.R:186-190` |
| `heywood` diagnostic | "a Heywood case occurs if an estimate of a communality index … is equal to one" | p. 472; `R/cpm_fit.R:1422` |
| reflection canonicalization | the p. 477 arbitrariness of rotation direction | Requirement 5, p. 477 |

**The `zeta` identity, verified rather than asserted.** Substituting
ζ*ᵢᵢ = ρ(xᵢ, cᵢ) = (1 + vᵢᵢ)^(−1/2) into eq. (3) gives, off the diagonal,
(P_x)ᵢⱼ = ζ*ᵢ ζ*ⱼ (P_c)ᵢⱼ (D_v is diagonal, so it contributes nothing
off-diagonal), and on the diagonal ζ*ᵢᵢ²(1 + vᵢᵢ) = 1. That is exactly the
code's `P <- (zeta %o% zeta) * C; diag(P) <- 1`
(`R/cpm_fit.R:72-82`). The engine therefore estimates Browne's communality
index directly, which is why `tests/testthat/test-cpm_oracles.R:131`
converts CircE's published vᵢᵢ by 1/√(1 + v) before comparing (the reasoning
is in the comment at `:128-130`).

**The df bookkeeping, checked in both families.** Browne's eq. (6) counts
½p(p + 1) covariance moments against q = 3p + m − 1 (variant A, §6.4). The
package's `scaling = "free"` family carries exactly that count
(`n_moments = p(p+1)/2`, and free_angles + n_zeta + n_sigma + n_beta =
(p−1) + p + p + m = 3p + m − 1). The default `scaling = "unit"` family instead
counts ½p(p − 1) correlation moments against q = 2p + m − 1, dropping the p
scale parameters the p. 473 sentence declares "not of interest" along with the
p unit-diagonal moments they absorb. The two differ by exactly p on each side
and give the **same d** — no departure, two equivalent renderings of eq. (6).
(This is the same cancellation D-011 measured empirically.)

### Departures the parameter map found

Recorded, not reconciled — this milestone changes no package file (M42 Scope).
Neither is a contradiction of the paper; both are places the package does
something the paper does not state.

1. **Browne's §6.7 start-value recipe is not implemented.** The engine does not
   perform an IFA, does not form Λ*, does not rotate by principal components,
   and does not derive angles from eq. (48). It takes the user's theoretical
   angles as the angle start, sets ζ from a max-|rᵢⱼ| rule clipped to
   [0.3, 0.95], and fits β by least squares of the off-diagonal rᵢⱼ on
   {cos(k·Δᵢⱼ)} (`R/cpm_fit.R:437-467`). The *recipe* — eqs. (41)–(47) and
   eq. (48)'s arc-cos angle formula — is therefore banked above as published
   context that no repo line computes. **One piece of §6.7 is not idle:** its
   θ_r = 0 reference-variable convention (p. 488) is the published warrant for
   the engine's `reference` pin (`R/cpm_fit.R:142`), as the parameter map's
   `theta` row records. Do not read this departure as licence to drop the
   §6.7 transcription.
2. **The engine imposes an identification cap on m that the paper does not
   print.** `cpm_spec()` caps m at floor((p−1)/2) for variants A/C and
   floor(p/2) for B/D (`R/cpm_fit.R:150-162`). Browne states no cap: §6.4's
   guidance is the advisory "substantially less than p/2" for Requirement 2,
   and §6.5 uses m = [p/2] for the circulant case. The cap is the package's,
   and it is stricter than the paper's advice on the A/C branch.
3. **Model variants B, C and D are not all Browne's.** Variant D (fixed
   equally spaced angles, one shared ζ) is the §6.5 circulant case; variant B
   (fixed angles, free ζ) is a partial one. **Variant C, equal communality
   with free angles, appears nowhere in this paper** — it is the design's own
   constraint (`R/cpm_fit.R:127`).

## Errata — the paper's own internal inconsistencies

Four misreferences and one definition are wrong as printed. All five were read
on the page images and agree with the OCR text layer, so they are the print's,
not the scan's. Anyone checking the code against the paper will hit them.

- **p. 472**: "the estimators yielded by the minimization of F(S, Σ) in (4)" —
  the ML discrepancy function is **(5)**; (4) is the communality index.
- **p. 485**: "[m/2] is the largest integer that is **less than** m/2".
  Taken literally this contradicts the expansion printed immediately above it
  in eq. (33): for m = 4, ρ₁₈₀° = 2(β₀ + β₂ + β₄) − 1 needs [m/2] = 2, but
  "largest integer less than 2" is 1. The intended meaning throughout is the
  **floor**, largest integer *not greater than* m/2 — which is also what §6.5's
  m = [p/2] requires. Load-bearing: an implementation following the printed
  words drops the top even harmonic from ρ₁₈₀°.
- **p. 488**: "Substitution of **(33)** into the expression for the covariance
  structure … shows that Σ = ΛΛ′ + D_ψ" — the substitution is of **(38)**,
  P_c = Λ̄Λ̄′; (33) is the minimal correlation.
- **p. 488**: the row-sums-of-squares identity Σ_{j=1..2m+1} λ̄²ᵢⱼ = 1 is
  printed with the range "i = 1, …, **m**". The rows of Λ̄ are variables, so
  the range is i = 1, …, **p**.
- **p. 489**: "After **U** has been obtained and the rotation **(41)** has been
  carried out" — the rotation is **(45)**, Λ̄ = Λ*U; (41) is the target form of
  Λ̄.

## Not extracted

Deliberately absent, per the M42 question gate (2026-07-19): the milestone
carries the model the package implements, not the whole paper.

- **§4, Simplex Models** (eqs. 9–14, pp. 473–476) and **§5, models yielding
  circumplex patterns with positive correlations** — including **Anderson's
  (1960) circular stochastic process model** and its correlation function
  (§5.2), and the Markov/simplex limiting-case result the abstract advertises.
  Nothing in the package implements them: `grep -rn "Anderson" R/` returns
  nothing, and no variant offers a correlation function other than the FS one
  — observed 2026-07-19. A milestone that ever adds an Anderson correlation
  function must extend this page rather than assume it is complete.
- **§6.1 (Cudeck's circular weighted sum) and §6.2 (the Wiggins–Steiger–Gaelick
  symmetric circulant, eq. 29)** beyond the §6.5 relationship recorded above.
- **§8, the worked applications** — Revelle's mood questionnaire (Table 6,
  N = 472) and the vocational-interest example (§8.2). The repo's oracle for
  the latter is `grassi2010.md`, which reanalyzes the same data and prints
  full-precision estimates this paper does not. No repo value is read *from*
  §8: `tests/testthat/helper-cpm-oracles.R` transcribes Grassi throughout, and
  its own header records that provenance — observed 2026-07-19.

  **But §8 independently corroborates the oracle, which is worth knowing
  before anyone treats this as dead weight.** Table 11, p. 494 ("Vocational
  Interest Scales: Estimates") prints, on its `FS m=1` row, β₀ = .638,
  β₁ = .362, ρ₁₈₀° = .28 and polar angles 0, 55, 112, 123, 192, 210, 269 —
  digit for digit the values `grassi2010.md` records from Grassi's Table 2
  (p. 60). Grassi's claim that CircE "coincide[s] precisely with … CIRCUM" is
  therefore checkable against Browne's own printed output at m = 1, not only
  against CircE's. A milestone wanting a second published anchor for the
  m = 1 oracle should extend this page to Table 11 rather than assume §8 holds
  nothing — observed 2026-07-19. Also unextracted from §8: the input
  correlation matrix is **Table 2, p. 470** (the introduction, not §8), which
  is the table `helper-cpm-oracles.R:12` names.
- **§9, Concluding Comments.**

## Traces to

- `R/cpm_fit.R:2` — the engine header naming this paper as the model.
- `R/cpm_fit.R:30,50` — `cpm_rho()` / `cpm_rho_deriv()`: eq. (34) and its
  derivative.
- `R/cpm_fit.R:72-82` — `cpm_implied_cor()`: eq. (3) under the (3b\*) identity.
- `R/cpm_fit.R:94-98` — `cpm_implied_cov()`: eq. (2)'s scaling.
- `R/cpm_fit.R:108-117` — `cpm_discrepancy()`: eq. (5).
- `R/cpm_fit.R:127` — the variant table, whose variant C has no counterpart here.
- `R/cpm_fit.R:150-162` — the m cap, which the paper does not print.
- `R/cpm_fit.R:180,186-190` — q and d: eq. (6).
- `R/cpm_fit.R:1085` — the RMSEA point estimate: eq. (8) here, printed as
  `browne1992a.md`'s eq. 13, which is the arrangement the code follows.
- `R/cpm_fit.R:1422` — the Heywood marker: p. 472.
- `R/cpm_fit.R:1542`, `R/cpm_fit.R:1819` — the `@references` entries.
- `R/cpm_oop.R:123,162` — the "Circular Process Model (Browne, 1992)" print
  headers.
- `R/ssm_ci_accuracy.R:169-170` — the `@references` entry for the CPM
  plug-in population.
- `R/ssm_ci_oop.R:329,436` — the "Browne circular model" structure-note wording.
- `R/ssm_sem_syntax.R:10` — the comment identifying the SEM layer's freely
  estimated angles as Browne's.
- `tests/testthat/helper-cpm-oracles.R:12,56,64` — the oracle fixtures,
  including the eq. (4) conversion of CircE's vᵢᵢ.
- `tests/testthat/test-cpm_oracles.R:128-138` — the eq. (4) identity and the
  communality-CI reconstruction it feeds (see `browne1982.md`).
- `tests/testthat/test-cpm_fit.R:1` — the engine test-file header.
- `vignettes/evaluating-circumplex-structure.Rmd:49,610` — the CPM
  introduction and the reference-list entry.
- `vignettes/sem-based-ssm-analysis.Rmd:80,415,421` — the SEM vignette's
  contrast with the CPM, and its reference-list entry.
- `cairn/references/grassi2010.md` — the published-value oracle for this
  model; its Table 2/3 communality column is eq. (4)'s ρ̂.
- `cairn/references/browne1982.md` — supplies the CI machinery that eq. (4)
  is composed with.

## Open questions

- **The three departures above are recorded, not adjudicated.** M42 is
  documentation-only by scope, so nothing has been decided about the m cap,
  variant C's provenance, or the unimplemented §6.7 recipe — observed
  2026-07-19.
- **No value on this page has been read by a human.** Both channels are
  machine channels over the same OCR scan (M42-D1), so a defect in the scan
  itself would not have been caught. A human read of pp. 471–473 and 485–490
  would close this — observed 2026-07-19.
- **The code carries no citation for the equations it implements.** This
  repeats the finding M41's review made about `browne1992a.md`'s eqs. 13/14:
  `R/cpm_fit.R` names Browne (1992) in its header and `@references`, but no
  line of the engine points at an equation number, so a corrector has nothing
  local telling them which published form they are changing. Whether to
  annotate the code is a package change and therefore outside M42's scope —
  observed 2026-07-19.
