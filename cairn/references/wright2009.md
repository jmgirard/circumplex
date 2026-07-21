# wright2009 — the published closed-form SSM estimator + the `aw2009` example data

**Provenance.** Ingested 2026-07-20 by M47 from
`cairn/references/sources/wright2009.pdf` (gitignored).
Pagination: journal pp. 311–322; the 12-page shelf PDF is one page per printed
page, so PDF page *n* is printed page 310 + *n* (formulas p. 315 = PDF p. 5;
Appendix p. 322 = PDF p. 12).
The shelf PDF is an Acrobat Paper-Capture OCR scan (Producer "Adobe Acrobat 10.0
Paper Capture Plug-in"), so its text layer is a single OCR channel, **not**
independent of the page image (M42-D1); the OCR badly mangles the display
formulae.
Extraction: verified 2026-07-20 against the source by the implementing session's own visual read of the `pdftoppm`-rendered page images — the authoritative channel for a scan, independent of the OCR text layer (used only to locate passages) but, per M41-D1, not a human attestation; no value on this page has been read by a human — observed 2026-07-20.

**Citation.** Wright, A. G. C., Pincus, A. L., Conroy, D. E., & Hilsenroth,
M. J. (2009). Integrating methods to optimize circumplex description and
comparison of groups. *Journal of Personality Assessment, 91*(4), 311–322.
https://doi.org/10.1080/00223890902935696

**Role.** The published statement of the closed-form Structural Summary Method
point estimator the package implements (the "conventional Gurtman estimator" of
`R/ssm_analysis.R`), and the source of the `aw2009` example dataset (its
Table A). Wright et al. present the estimator; they attribute its goodness-of-fit
definition to Gurtman & Balakrishnan (1998) — see `gurtman1998.md` for the
estimator's originating source.

## Extracted values

The estimator, p. 315 (Eqs. 7–13), read from the page image and quoted verbatim:

- **Elevation** `e` — "the mean of the standardized scores on the octant
  scales", p. 315.
- **Amplitude** — `a = sqrt(X^2 + Y^2)`, Eq. (9), p. 315.
- **Angular displacement** — `δ = tan^-1(Y / X) × 180/π`, Eq. (10), p. 315
  (degrees).
- **Weighted axis scores** — `X = c × Σ(S_i × cos θ_i)`, Eq. (11); `Y = c × Σ(S_i × sin θ_i)`,
  Eq. (12), p. 315.
- **Constant** `c` — "a constant equal to two divided by the number of
  circumplex scales (in the case of most IPC measures, which are typically
  divided into octants, the value will be .25)", p. 315. `S_i` is the obtained
  group mean score on scale `i`; `θ_i` its angular location.
- **Goodness of fit** — `R^2 = 1 − (Σ d_i^2 / SS_Total)`, Eq. (13), p. 315, where
  "SS_Total is the profile's variability as the deviation sum of squares"
  (attributed to "Gurtman & Balakrishnan, 1998, p. 349"). `d_i` is the deviation
  (residual) of the obtained scale mean from the predicted `S_i`.
- **Predicted / obtained scale mean** — `S_p = e + a × cos(θ_i − δ)`, Eq. (7);
  `S_i = e + a × cos(θ_i − δ) + d_i`, Eq. (8), p. 315.
- **R² interpretation** — "R² values of .80 and greater have been interpreted
  as adequately sinusoidal, whereas values below .70 have been considered
  inadequate (Gurtman & Pincus, 2003)", p. 315.

**Table A (p. 322)** — the hypothetical five-person group, verbatim; this is the
`aw2009` dataset (angles: PA 90°, BC 135°, DE 180°, FG 225°, HI 270°, JK 315°,
LM 360°, NO 45°):

| | PA | BC | DE | FG | HI | JK | LM | NO |
|---|---|---|---|---|---|---|---|---|
| P1 | −1.09 | −1.04 | −0.97 | 0.61 | 1.41 | 2.49 | 1.78 | 0.27 |
| P2 | 1.13 | −1.04 | −0.97 | −0.79 | −0.56 | 0.79 | 1.78 | 1.52 |
| P3 | 0.91 | −0.65 | −0.80 | −0.96 | −0.23 | −0.34 | 1.24 | 0.27 |
| P4 | 0.47 | −0.45 | −0.29 | 0.26 | 1.57 | 1.36 | 1.60 | 0.48 |
| P5 | 0.45 | 0.32 | 0.43 | 0.96 | 1.25 | 1.41 | 1.49 | 0.85 |

Verified identical to `aw2009` (all 40 cells) 2026-07-20.

## Errata and cautions (Appendix worked example, p. 322)

Jeff's plan-gate warning (2026-07-20): the printed worked example carries typos
he corrected when validating against it. Do **not** transcribe its hand-computed
scalars as oracle values — recompute from Table A via Eqs. 7–13. Specifically:

- **Printed typo:** the final `SS_Total` term is set as `(.68 − .42)` — missing
  the square its seven sibling terms carry (`(x − .42)^2`), p. 322.
- The hand-computed scalars do not fully reconcile from the rounded Table A: the
  octant means (row M) average ≈ .46 while elevation is printed as `.42`; the
  squared deviations sum to ≈ 4.5 while `SS_Total` is printed as `4.03`; the
  reported `a = .97`, `δ = 345.10°`, `R² = .95` follow from those figures —
  observed 2026-07-20. No shipped test transcribes any of them.

## Traces to

- `R/example_data.R:4`, `man/aw2009.Rd` — the `aw2009` dataset is this page's
  Table A (five-person hypothetical group); exact match verified above.
- `R/ssm_analysis.R:1183` — the closed-form "conventional Gurtman estimator" the
  package computes is Eqs. 7–13 here (`e` = mean; `X`/`Y` weighted sums with
  `c = 2/p`; `a = √(X²+Y²)`; `δ` from `Y`/`X`; `R² = 1 − SS_resid/SS_Total`).
  Departure: the package uses a four-quadrant `atan2` for `δ`, not the paper's
  single-branch `tan^-1` (Eq. 10), which the paper itself notes "will be
  unstable and may shift drastically" for low-prototypicality profiles — a
  correct fix, not a discrepancy.
- `tests/testthat/test-ssm_sem_syntax.R:6` — "e = mean, x = (2/p)*cos, …"
  documents Eqs. 11–12 (`c = 2/p`) exactly.
- `vignettes/introduction-to-ssm-analysis.Rmd`, `intermediate-ssm-analysis.Rmd`,
  `advanced-visualization.Rmd` — cite Wright et al. (2009) as an SSM foundation.

## Open questions

- Whether the `gurtman1998` page (M47 T3–T4) confirms Gurtman & Balakrishnan
  (1998) as the estimator's originating source, as Wright et al.'s Eq. 13
  attribution and the "conventional Gurtman estimator" label both suggest —
  observed 2026-07-20.
