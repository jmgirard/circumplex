# grassi2010 — the published CPM oracle: CircE's worked vocational-interest example

**Provenance.** Ingested 2026-07-19 by M40 from
`cairn/references/sources/grassi2010.pdf` (gitignored).
Pagination: journal pages 55–73; the shelf PDF is 19 pages, so PDF page *n* is
printed page *n* + 54.
Extraction: verified 2026-07-19 twice and independently for every value that also appears in the test fixtures — Jeff's second human re-read against the primary source (M7 AC3 attestation, which confirmed every transcribed value and changed no fixture), and an independent `pdftotext -layout` re-check during M40 that reproduced all of them, including the reordered Appendix A communality block and the seventh variance ratio; eight values on this page were never in the fixtures and so rest on the M40 machine channel alone, marked **[1-channel]** where they appear — a₀ and its SE, the close-fit p value, NFI, GFI, AGFI, the sample discrepancy, and the 90%-vs-95% CI-level distinction — observed 2026-07-19.

**Citation.** Grassi, M., Luccio, R., & Di Blas, L. (2010). CircE: An R
implementation of Browne's circular stochastic process model. *Behavior
Research Methods, 42*(1), 55–73. doi:10.3758/BRM.42.1.55

**Role.** The package's published-value oracle (O2) for the Circular Process
Model engine. The paper states its example reanalyzes the data Browne (1992,
Table 2) used, and that CircE's m = 1..3 results "coincide precisely with the
ones obtained by CIRCUM" (p. 59), so these values transitively cover Browne's
own program (oracle O1) as well.

Two confidence levels appear in this source and must not be conflated: the
**fit-measure** intervals (F₀, RMSEA) are **90%**, printed as
`Confidence Interval 90 %` in the Appendix A output; the **communality-index**
intervals are "approximate **95%** one at time confidence intervals" (Appendix
A header, p. 71). **[1-channel]** — the fixtures record neither CI level, so
this distinction was first noted at M40 from the machine channel alone and no
human read has confirmed it.

## Extracted values

### Vocational interest scales — input

- Correlation matrix, 7 scales, all 21 off-diagonal values — Table 1, p. 58;
  reprinted as the "Sample Correlation Matrix" block, Appendix A p. 72, and as
  Listing 1, p. 58. Variable order: Health, Science, Technology, Trades,
  Business operations, Business contact, Social.
- N = 175 — Table 1 header, p. 58.

### Unconstrained model 1a, m = 1 — Table 2, p. 60

- β₀ = .638, β₁ = .362; ρ₁₈₀° = .28; polar angles 0, 55, 112, 123, 192, 210,
  269 (whole degrees).
- Communality indices .93, .81, .98, .78, .80, .94, .83.
- **The Table 2/3 communality column is ρ̂, the communality index (Browne,
  1992, Eq. 4) — not ζ.** The table's own footnote reads "Communality indices
  ρᵢ, polar angles θᵢ, and minimal common score correlation ρ₁₈₀° are
  unconstrained." (Corrected at M7 T3, finding A4; the repo's assertions were
  already right — only the label was wrong.)
- Table 2's angles are the **mirror** of Appendix A's: the paper prints both
  directions, as its "360-ang. pos." column (p. 71) shows.

### Unconstrained m = 1 fit measures — Appendix A, pp. 70–71

**Not Table 3** (corrected at M7 T3, finding A3). Table 3, p. 60 prints an
overlapping subset — F, F₀ and its CI, RMSEA and its CI — but only Appendix A
carries the test statistic, df, p values, null χ², TLI, CFI, and SRMR.

- F̂ = 0.089815 — iteration trace, "final value", p. 70.
- Sample discrepancy 0.09 **[1-channel]**; F₀ = 0.049, 90% CI (0.005 ; 0.139).
- RMSEA = 0.084, 90% CI (0.026 ; 0.141).
- Test statistic = 15.63; df = 7; p (H₀ perfect fit) = 0.029; p (H₀ close fit,
  RMSEA = 0.050) = 0.137 **[1-channel]**.
- Null model χ² = 747.663, df = 21; TLI (Tucker–Lewis NNFI) 0.964; CFI 0.988;
  SRMR 0.04. Also printed, but fixture-absent: NFI 0.979 **[1-channel]**,
  GFI 0.986 **[1-channel]**, AGFI 0.944 **[1-channel]**.

### Full-precision estimates — Appendix A, pp. 71–72

Printed in Table-1 variable order in the "Parameter estimates and Standard
Errors" block:

- Polar angles: 0.00000, 305.35328, 247.82980, 237.38218, 168.30615,
  149.83787, 91.25973; SEs 0.00000, 9.01111, 7.35838, 9.44904, 9.08050,
  7.95016, 8.72929.
- v: 0.15438, 0.51654, 0.03945, 0.63153, 0.54550, 0.13449, 0.44771; SEs
  0.13759, 0.12755, 0.04238, 0.13854, 0.12125, 0.05959, 0.13865.
- z: 0.91358, 0.81222, 1.00102, 0.79058, 0.79269, 0.92497, 0.84376.
- a₀ = 1.76119 (SE 0.26287) **[1-channel]**; betas b₀ = 0.6378, b₁ = 0.3622.
- MCSC, correlation at 180 degrees = 0.276.
- Ratios of reproduced to input variances: 0.963, 1.000, 1.042, 1.020, 0.971,
  0.971, 1.031. These are not 1, which is the CIRCUM/CircE free-scaling
  signature — their fitted diagonal is not constrained to the observed unit
  diagonal.

### Communality indices with CIs — Appendix A, p. 71

**This block is reordered.** Its header states: "Note: variable names have been
reordered to yield increasing polar angles", and it prints in the order
Health, Social, Business Contact, Business Operations, Trades, Technology,
Science — *not* Table 1's order. A value read off the page must be re-mapped
by scale before comparison. (M7 T3 finding A2: the flat row order invited a
false mismatch; re-mapped by scale, every index and CI agrees exactly.)

Re-mapped into Table-1 order, with their approximate 95% one-at-a-time CIs:

| Scale | ρ̂ | 95% CI |
|---|---|---|
| Health | 0.93 | (0.73, 0.99) |
| Science | 0.81 | (0.74, 0.87) |
| Technology | 0.98 | (0.87, 1) |
| Trades | 0.78 | (0.71, 0.84) |
| Business Operations | 0.80 | (0.74, 0.86) |
| Business Contact | 0.94 | (0.87, 0.97) |
| Social | 0.83 | (0.74, 0.90) |

The nonsymmetric CIs on ρ(xᵢ, cᵢ) are obtained from **symmetric** CIs on
ln vᵢᵢ (Browne, 1982, pp. 95–96) — the symmetry belongs to the log-v scale,
not to the communality CIs themselves (M7 T3 finding A5).

### Verbal ability tests — Listing 7, p. 68

Used for input-refusal behavior only; no numeric result depends on it.

- 6 scales: Spelling, Punctuation, Grammar, Vocabulary, Literature, **Foreign
  Literature** — printed exactly so on p. 68. (M7 T3 raised finding A6
  claiming "Foreign Language" and **retracted it the same day**; the M40
  re-read confirms the retraction was correct.)
- All 15 off-diagonal correlations; N = 1046 (Listing 8, p. 68).
- The source also attributes this matrix to Guttman (1954, p. 282) and Browne
  (1992, p. 470).

## Traces to

- `tests/testthat/helper-cpm-oracles.R:13-31` — `cpm_oracle_voc()`: the Table 1
  matrix, N, and the Table 2 model 1a start angles.
- `tests/testthat/helper-cpm-oracles.R:39-62` — `cpm_oracle_voc_appendix()`:
  every Appendix A full-precision estimate, fit measure, communality index and
  CI, and the variance ratios.
- `tests/testthat/helper-cpm-oracles.R:66-79` — `cpm_oracle_verbal()`: the
  Listing 7 matrix and N.
- `tests/testthat/test-cpm_oracles.R:1-47` — the validation battery and its
  published-oracle provenance header.

## Open questions

- The communality-CI derivation attributed to Browne (1982, pp. 95–96) is
  recorded here as *this* paper reports it, and has not been checked against
  Browne himself. Those two pages are on the shelf as images
  (`sources/browne1982_p95a.png`, `p95b`, `p96a`, `p96b`), added mid-session
  on 2026-07-19; the rest of Browne (1982) is not — so the check is now
  possible but has not been done. M41 T5 carries it — observed 2026-07-19,
  re-checked at merge (an earlier form of this bullet said the pages were
  absent, which was already false when written).
- The CIRCUM/CircE free-scaling model difference (why our F̂ exceeds their
  published 0.089815 at finite N) is analysed in
  `tests/testthat/test-cpm_oracles.R:30-47` and `devel/m4-browne-design.md`
  sec. 11, not here — this page records what the paper prints, not the
  reconciliation.
