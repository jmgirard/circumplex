# M7 T3 — second independent human re-read checklist

**Purpose.** The Grassi et al. (2010) and Zimmermann & Wright (2017)
transcriptions were each extracted through two *mechanical* channels (a
rendered-page visual read and a `pdftotext` text-layer extraction, diffed
against each other). Both records mark the remaining step as
`second independent human re-read: pending (Jeff)`. This checklist is that
step's worksheet. It gates the v2.0.0 submission (M7 AC3).

**How to use it.** Open the primary source at the anchor named in each row and
read the published value off the page, *then* compare it to the Value column.
Read source-first — comparing in the other direction invites confirmation of
what is already written. Tick the box when the two agree; note any discrepancy
in the Notes column and resolve it before T4.

This checklist was assembled from the repo, so it inherits any error the repo
already has. It tells you **where to look**, never what you should find.

---

## A. Grassi, Luccio & Di Blas (2010)

*CircE: An R implementation of Browne's circular stochastic process model.*
**Behavior Research Methods, 42**(1), 55–73. doi:10.3758/BRM.42.1.55

Repo locations: `tests/testthat/helper-cpm-oracles.R` (fixtures),
`tests/testthat/test-cpm_oracles.R` (provenance header + assertions).

### A1. Input correlation matrix — Table 1 (p. 58), N = 175

`helper-cpm-oracles.R:12-30`. Seven vocational-interest scales (Health,
Science, Technology, Trades, BusinessOperations, BusinessContact, Social).
Lower triangle, read row-wise:

- [ ] Row 2 (Science): `.654`
- [ ] Row 3 (Technology): `.453 .644`
- [ ] Row 4 (Trades): `.251 .440 .757`
- [ ] Row 5 (BusinessOperations): `.122 .158 .551 .493`
- [ ] Row 6 (BusinessContact): `.218 .210 .570 .463 .754`
- [ ] Row 7 (Social): `.496 .264 .366 .202 .471 .650`
- [ ] N = 175
- [ ] Scale names and their **order** match Table 1

> Cross-check available: the paper states this reanalyzes Browne (1992)
> Table 2. The matrix is also printed in the paper's Listing 1.

### A2. Appendix A full-precision m = 1 output (pp. 70–72)

`helper-cpm-oracles.R:33-55`. These are the frozen published-program values
the free-scaling oracle asserts against.

- [ ] Angles θ (deg): `0, 305.35328, 247.82980, 237.38218, 168.30615, 149.83787, 91.25973`
- [ ] Angle SEs: `0, 9.01111, 7.35838, 9.44904, 9.08050, 7.95016, 8.72929`
- [ ] v (communality parameter): `0.15438, 0.51654, 0.03945, 0.63153, 0.54550, 0.13449, 0.44771`
- [ ] v SEs: `0.13759, 0.12755, 0.04238, 0.13854, 0.12125, 0.05959, 0.13865`
- [ ] ζ: `0.91358, 0.81222, 1.00102, 0.79058, 0.79269, 0.92497, 0.84376`
- [ ] Communality indices: `.93, .81, .98, .78, .80, .94, .83`
- [ ] Communality 95% CIs: `(.73,.99) (.74,.87) (.87,1) (.71,.84) (.74,.86) (.87,.97) (.74,.90)`
- [ ] Variance ratios (reproduced/input): `0.963, 1.000, 1.042, 1.020, 0.971, 0.971, 1.031`
- [ ] β (correlation-function weights): `0.6378, 0.3622`
- [ ] MCSC ρ(180°): `0.276`
- [ ] F̂ (iteration trace, "final value"): `0.089815`

> **Direction caution.** The angle vector is the Appendix A direction, which is
> the *mirror* of the Table 2 model-1a start values (`th_start = 0, 55, 112,
> 123, 192, 210, 269` at `helper-cpm-oracles.R:29`). The paper prints both and
> labels one "360 − ang. pos." Confirm you are reading the Appendix A column,
> not Table 2, for this block — and confirm the start values separately.

### A3. Fit measures — Table 3 (p. 60) / Appendix A

`helper-cpm-oracles.R:44-48`.

- [ ] T (χ²): `15.63`, df `7`, p `0.029`
- [ ] F₀: `0.049`, 90% CI `(0.005, 0.139)`
- [ ] RMSEA: `0.084`, 90% CI `(0.026, 0.141)`
- [ ] Null χ²: `747.663`, null df `21`
- [ ] TLI: `0.964` · CFI: `0.988` · SRMR: `0.04`

### A4. Constrained-model rows — Table 2 / Table 3 (p. 60)

Asserted inline in `test-cpm_oracles.R`.

- [ ] Model 2b (equal communality): β `(.628, .372)`, ζ `.87`, F̂ `.299`, ρ(180) `.26` — `test-cpm_oracles.R:196-201`
- [ ] Model 1a m = 2: β `(.608, .355, .038)`; F̂ `.067` (Table 3) — `test-cpm_oracles.R:229-239`
- [ ] The m = 3 statement that β₃ attains "the lower bound of zero" (p. 59) — `test-cpm_oracles.R:251`
- [ ] Model 3c (equal spacing, free scaling) — `test-cpm_oracles.R:347, 564-565`

### A5. Quoted textual claims

- [ ] p. 59: CircE's m = 1..3 results "coincide precisely with the ones obtained by CIRCUM" — this is what makes the fixtures transitively cover Browne's own program (`test-cpm_oracles.R:18-20`)
- [ ] p. 57: communality CIs are symmetric Wald intervals on `ln(v)` (Browne, 1982) — decoded at `test-cpm_oracles.R:122-127`
- [ ] Appendix A prints variance ratios spanning `.963–1.042` (the free-scaling model difference)

### A6. Secondary fixture — Listing 7–8 (pp. 67–68)

`helper-cpm-oracles.R:59-72`. Verbal-ability matrix (Guttman, 1954, p. 282;
also Browne, 1992, p. 470), used for input-refusal behavior only, so an error
here cannot move a numeric result — verify last, or skip.

- [ ] Six scale names and lower triangle: `.621 / .564 .742 / .476 .503 .577 / .394 .461 .472 .688 / .389 .411 .429 .548 .639`
- [ ] N = 1046

---

## B. Zimmermann & Wright (2017)

*Beyond description in interpersonal construct validation: Methodological
advances in the circumplex Structural Summary Approach.*
**Assessment, 24**(1), 3–23. doi:10.1177/1073191115621795

Repo locations: `devel/m4-zw-transcription.md` (the full 211-line record,
already page-anchored), `vignettes/evaluating-circumplex-structure.Rmd`
(the user-facing claims).

**The full record is the authority for this half** — it is structured for
exactly this pass and every entry already carries its page anchor. Work
through it directly. The rows below are the subset that reaches **shipped,
user-facing output**, and so are the ones where an error would do the most
damage.

### B1. The accuracy table the vignette prints (Studies 1–2, pp. 6–11)

`vignettes/evaluating-circumplex-structure.Rmd:190-199`. This table is read
by users as guidance about their own sample sizes.

- [ ] Elevation, X, Y: essentially unbiased; CI accurate at n ≥ 50
- [ ] Amplitude: biased **upward** (strongly at small population amplitude); accurate at n ≥ 75 (general-factor instrument) / n ≥ 150 (no general factor), given population amplitude ≥ .10
- [ ] Displacement: unbiased but imprecise at low amplitude; accurate at n ≥ 100 (general factor) / n > 200 (no general factor), given population amplitude ≥ .10
- [ ] Fit (R²): biased **downward**; population R² < .9 only, unsuited near 1
- [ ] The band being applied is Bradley's (1978) liberal band, 92.5%–97.5%

### B2. Population octant matrices — Note 3 (p. 18)

`devel/m4-zw-transcription.md`. These define the simulation conditions the
diagnostic was validated against.

- [ ] Without a substantial general factor (IAS, 2,988 students; Gurtman & Pincus, 2000; Wiggins, 1995): ρ1 `.430`, ρ2 `.030`, ρ3 `−.360`, ρ4 `−.740`
- [ ] With a general factor (IIP-C) — values per the record; confirm all four ρ

### B3. Scaling-factor formulas — Eq. A6, A7 (p. 18) and Eq. 3 (p. 12)

The record flags **one resolved channel discrepancy** here. Please confirm the
resolution against the page image, since it was settled by reasoning rather
than by a clean second read:

- [ ] Eq. A7 radicand is `√2(ρ1−ρ3)+(1−ρ4)` — **not** `2(ρ1−ρ3)+(1−ρ4)`. The text layer rendered it without the √; the page image shows it with. (Only the √2 form reproduces all five published scaling factors.)
- [ ] Eq. A6: `f_e = √((2ρ1+2ρ2+2ρ3+ρ4+1)/8)`
- [ ] Eq. 3 (p. 12): `|AFF_min| = 2.95 · f_a · n^(−0.587)` — confirm both constants
- [ ] Derived values quoted on p. 9: f_e → IIP-C `.737`, IAS `.240`; f_a → IIP-C `.545`, IAS `.845`
- [ ] p. 14: IIP-SC f_a `.625`; `|.029|` at N = 1166
- [ ] p. 12 worked values: f_a = .545 → n = 100 gives `.108` ("as large as .11"); n = 1000 gives `.028` (".03")

### B4. Study 5 reproduction (p. 14) and Table 4

The vignette reproduces these analyses live on the bundled `jz2017` data, so a
transcription error here would surface as a mismatch between the vignette's
prose and its own computed output.

- [ ] The Study 5 pattern the vignette says it reproduces (`evaluating-circumplex-structure.Rmd:151`)
- [ ] The Table 4 elevation value for the obsessive–compulsive PD profile that the ipsatizing section compares against (`evaluating-circumplex-structure.Rmd:237, 572`)
- [ ] N = 1166 for Study 5

### B5. Octant angles — Figure 1A (p. 3)

- [ ] LM 0°, NO 45°, PA 90°, BC 135°, DE 180°, FG 225°, HI 270°, JK 315°

> Note the package's own convention reports LM as **360**, not 0. That is a
> deliberate package convention (D-003), not a transcription discrepancy —
> do not "correct" it.

---

## Attestation

When complete, record the outcome in the M7 work log: the date, that the
re-read was done against the primary sources, and either "no discrepancies"
or each discrepancy and its resolution. Then the two `pending (Jeff)` markers
in `tests/testthat/test-cpm_oracles.R:10-11`,
`tests/testthat/helper-cpm-oracles.R:6-7`, and
`devel/m4-zw-transcription.md` should be updated to record the completed
re-read and its date.
