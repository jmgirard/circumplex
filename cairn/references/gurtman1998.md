# gurtman1998 — the defining source of the "conventional Gurtman estimator"

**Provenance.** Ingested 2026-07-20 by M47 from
`cairn/references/sources/gurtman1998.pdf` (gitignored).
Pagination: journal pp. 344–360; the 17-page shelf PDF is one page per printed
page, so printed page = PDF page + 343 (title p. 344 = PDF p. 1; the estimator
equations on p. 349 = PDF p. 6).
The shelf PDF is an Acrobat Paper-Capture OCR scan (Producer "Adobe Acrobat 10.0
Paper Capture Plug-in"), so its text layer is a single OCR channel, **not**
independent of the page image (M42-D1), and the OCR mangles the display
formulae.
Extraction: verified 2026-07-20 against the source by a first-hand visual read
of the `pdftoppm`-rendered image of p. 349 — the authoritative channel for a
scan; the OCR text located passages only — observed 2026-07-20.

**Citation.** Gurtman, M. B., & Balakrishnan, J. D. (1998). Circular measurement
redux: The analysis and interpretation of interpersonal circle profiles.
*Clinical Psychology: Science and Practice, 5*(3), 344–360. The shelf scan
carries no printed DOI.

**Role.** The originating methodological definition of the Structural Summary
Method estimator the package computes — the "conventional Gurtman estimator" of
`R/ssm_analysis.R`. It defines the structural-summary decomposition and the R²
goodness-of-fit statistic; `wright2009.md` later gives the explicit weighted-sum
operationalization (its Eqs. 11–12), citing this page (p. 349) for `SS_Total`.

## Extracted values

From p. 349 (section "Structural Analysis of Circular Profiles: The Structural
Summary"), read from the page image and quoted verbatim:

- **Structural-summary decomposition** — `Z_ij = e_i + a_i * cos(Θ_j − δ_i) + d_ij`,
  Eq. (1), p. 349, where `Z_ij` is the standardized score of individual `i` on
  scale `j`; `e_i` is "the mean level, or elevation, of the individual's
  profile"; `a_i` is "the amplitude of the individual's best-fit cosine curve";
  `Θ_j` is "the angular location of scale, j (generally 0°, 45°, 90°, etc.)";
  `δ_i` is "the angular displacement, or peak shift, of the individual's curve,
  which is assumed to be uniformly distributed and uncorrelated with a_i"; and
  `d_ij` is "a deviation component, assumed to be a random variable, pairwise
  independent, with a mean of 0."
- **Estimation** — "Least-squares estimates of these parameters are easily
  obtained using any standard curve-fitting algorithm. With a slight scaling
  correction (see Gurtman, 1994), LaForge et al.'s (1954) vector-averaging
  formulas will also yield solutions for the two nonelevation parameters: Vector
  angle, it can be shown, solves for the curve's angular displacement, and vector
  length for its amplitude", p. 349.
- **Goodness of fit** — `R^2 = 1 − Σd^2_ij / SS_TOTAL`, Eq. (2), p. 349, where
  `Σd^2_ij` is "the sum of the squared deviations between the predicted scores
  from the best-fit cosine function of Equation 1 and the obtained scores in the
  profile, and SS_TOTAL is the profile's variability as the deviation sum of
  squares." (This is the p. 349 that `wright2009.md` cites for `SS_Total`.)
- **Worked example (Figure 3)** — "The model's structural parameters are as
  follows: e = .38, a = .79, δ = 249°. The reproduced profile is a close fit to
  the actual profile (R² = .85)", p. 349.

## Traces to

- `R/ssm_analysis.R:1183` — the "conventional Gurtman estimator" is this
  vector-averaging closed form: the two nonelevation parameters via LaForge's
  vector formulas with Gurtman's "slight scaling correction" (made explicit as
  `c = 2/p` in `wright2009.md` Eqs. 11–12), plus `e` = mean and `R²` = Eq. (2).
  The roxygen's own statement — the closed form "equals the ordinary-least-squares
  cosine fit for equally spaced angles … For angle sets violating that balance …
  it is the conventional Gurtman estimator, not a least-squares fit" — is exactly
  this page's distinction: Gurtman offers the least-squares fit *and* the
  vector-averaging closed form as compatible, coinciding under equal spacing.
- `tests/testthat/test-ssm_sem_syntax.R:6` — "The conventional (Gurtman)
  closed-form SSM weights: e = mean, x = (2/p)*cos, …" is this vector-averaging
  closed form with the `2/p` scaling correction.
- `cairn/references/wright2009.md` — Wright et al. (2009) operationalize this
  estimator (their Eqs. 7–13) and cite this page (p. 349) for `SS_Total`.

## Open questions

- No shipped vignette cites Gurtman & Balakrishnan (1998) by name; the repo's
  reliance is through the estimator code and Wright et al.'s citation of p. 349,
  not a vignette reference list — observed 2026-07-20.
