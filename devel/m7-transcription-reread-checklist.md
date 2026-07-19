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

> **Section A completed 2026-07-19 by Jeff**, against the primary source.
> **Every transcribed value confirmed — no fixture changed.** Three
> corrections, all to records *about* the values (a page anchor, a column
> label, a comment's wording), applied the same day; see the A2–A6 notes.
> Section B (Zimmermann & Wright) is still outstanding — AC3 is not met
> until it is done.

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

> **Re-read result (2026-07-19).** All values confirmed. **Order correction:**
> Appendix A prints these blocks in its own variable order — Health, Social,
> BusinessContact, BusinessOperations, Trades, Technology, Science (ascending in
> its mirrored angle) — not Table 1's. The rows above (and the fixtures) are in
> **Table-1 order**; re-map by scale before comparing. Mapped that way the
> communality indices and all seven CIs agree exactly.
>
> **Direction caution.** The angle vector is the Appendix A direction, which is
> the *mirror* of the Table 2 model-1a start values (`th_start = 0, 55, 112,
> 123, 192, 210, 269` at `helper-cpm-oracles.R:29`). The paper prints both and
> labels one "360 − ang. pos." Confirm you are reading the Appendix A column,
> not Table 2, for this block — and confirm the start values separately.

### A3. Fit measures — Appendix A (pp. 70–71)

> **Re-read result (2026-07-19).** Values confirmed, **anchor corrected**: these
> unconstrained m = 1 fit measures come from Appendix A (pp. 70–71), not Table 3
> (p. 60). Table 3 is the anchor for the *constrained*-model F values (A4).
> The provenance header in `test-cpm_oracles.R` now splits the two.

`helper-cpm-oracles.R:44-48`.

- [ ] T (χ²): `15.63`, df `7`, p `0.029`
- [ ] F₀: `0.049`, 90% CI `(0.005, 0.139)`
- [ ] RMSEA: `0.084`, 90% CI `(0.026, 0.141)`
- [ ] Null χ²: `747.663`, null df `21`
- [ ] TLI: `0.964` · CFI: `0.988` · SRMR: `0.04`

### A4. Constrained-model rows — Table 2 / Table 3 (p. 60)

Asserted inline in `test-cpm_oracles.R`.

> **Re-read result (2026-07-19).** Values confirmed, **label corrected**: the
> table lists `.87` as ρ̂₁ (the communality index), not ζ. The assertion is
> still right — that column *is* our `Zeta` per design sec. 6.5 — but the code
> comments said "zeta" and now say so. Applied to the m = 2 rows on the same
> reading.

- [ ] Model 2b (equal communality): β `(.628, .372)`, ρ̂₁ `.87`, F̂ `.299`, ρ(180) `.26` — `test-cpm_oracles.R:196-201`
- [ ] Model 1a m = 2: β `(.608, .355, .038)`; F̂ `.067` (Table 3) — `test-cpm_oracles.R:229-239`
- [ ] The m = 3 statement that β₃ attains "the lower bound of zero" (p. 59) — `test-cpm_oracles.R:251`
- [ ] Model 3c (equal spacing, free scaling) — `test-cpm_oracles.R:347, 564-565`

### A5. Quoted textual claims

- [ ] p. 59: CircE's m = 1..3 results "coincide precisely with the ones obtained by CIRCUM" — this is what makes the fixtures transitively cover Browne's own program (`test-cpm_oracles.R:18-20`)
- [ ] p. 57, **as published**: "The nonsymmetric confidence intervals for the communality index estimates, ρ(x_i, c_i) (Browne, 1992, Eq. 4), are obtained from symmetric confidence intervals on ln v_ii (Browne, 1982, pp. 95–96)." The checklist's earlier paraphrase ("communality CIs are symmetric Wald intervals on ln(v)") put the symmetry on the wrong quantity; the comment at `test-cpm_oracles.R:122-127` has been reworded to the published statement. The decoding arithmetic was already correct.
- [ ] Appendix A prints variance ratios spanning `.963–1.042` (the free-scaling model difference)

### A6. Secondary fixture — Listing 7–8 (pp. 67–68)

`helper-cpm-oracles.R:59-72`. Verbal-ability matrix (Guttman, 1954, p. 282;
also Browne, 1992, p. 470), used for input-refusal behavior only, so an error
here cannot move a numeric result — verify last, or skip.

> **Re-read result (2026-07-19).** Correlations, N, and all six scale names
> confirmed. *(An earlier note here reported the sixth scale as
> "ForeignLanguage"; Jeff retracted that the same day as his own slip —
> the source reads **ForeignLiterature**, which is what the fixture has
> always said. No change was kept; corrected 2026-07-19.)*

- [ ] Six scale names — Spelling, Punctuation, Grammar, Vocabulary, Literature, ForeignLiterature — and lower triangle: `.621 / .564 .742 / .476 .503 .577 / .394 .461 .472 .688 / .389 .411 .429 .548 .639`
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

> **Worksheet refreshed 2026-07-19** after section A completed. Changes: line
> anchors re-derived against the current vignette (all of them had drifted);
> a new **B1a** covering six shipped numbers the first cut missed because they
> sit in prose rather than in the table; **B4** split — its single row had
> conflated OCPD's amplitude with PARPD's elevation — and given the Study 5
> fit indices it also ships; **B3** given Eq. A7's leading ½, which had never
> been listed; **B2** given the IIP-C ρ values instead of "per the record";
> and the transcription record's own flagged 15.5% coincidence surfaced here
> (B1a), where this pass will actually meet it.
>
> **What section A suggests to watch for.** Every one of its findings was an
> error in a *record about* a value — a page anchor, a column label, a
> variable order, a comment's wording — and **not one transcribed number was
> wrong**. Section B's analogue is that same label/anchor layer: which study a
> threshold comes from, which scale a Table 4 figure belongs to, whether a
> vignette number is the published value or a rounding of it.

### B1. The accuracy table the vignette prints (Studies 1–2, pp. 6–11)

`vignettes/evaluating-circumplex-structure.Rmd:190-199`. This table is read
by users as guidance about their own sample sizes.

- [ ] Elevation, X, Y: essentially unbiased; CI accurate at n ≥ 50
- [ ] Amplitude: biased **upward** (strongly at small population amplitude); accurate at n ≥ 75 (general-factor instrument) / n ≥ 150 (no general factor), given population amplitude ≥ .10
- [ ] Displacement: unbiased but imprecise at low amplitude; accurate at n ≥ 100 (general factor) / n > 200 (no general factor), given population amplitude ≥ .10
- [ ] Fit (R²): biased **downward**; population R² < .9 only, unsuited near 1
- [ ] The band being applied is Bradley's (1978) liberal band, 92.5%–97.5% (`:187-188`)

### B1a. The three "implications" bullets the vignette prints (`:203-222`)

Prose, not a table, so it escaped the first cut — but every number in it is
transcribed and shipped.

- [ ] Relative amplitude bias averaged `15.5%`, reached `135.8%` (pp. 6–7)
- [ ] ⚠ **`15.5%` occurs twice in the paper for two different quantities** — Study 1's average relative amplitude bias (p. 6) and Study 4's mean deviance when AFF₂ = 0 (p. 13). Both channels confirmed both occurrences independently; the record flags it so a reader does not "fix" one into the other. The vignette means the Study 1 sense. Please confirm on the page.
- [ ] At n = 50, no general factor, population amplitude exactly 0: expected sample amplitude ≈ `.15` (pp. 6–7)
- [ ] SE of displacement ≈ `50°` at n = 100 for a weakly differentiated profile (p. 8)
- [ ] Eq. 3 constants `2.95` and `n^(−0.587)`, and the f_a values as the vignette **rounds** them: `.55` IIP-C, `.63` IIP-SC, `.85` IAS (record: .545, .625, .845 — confirm the unrounded values in B3, the rounding here is the vignette's)
- [ ] At n = 100 with an IIP-C-like instrument the frontier is ≈ `.11`

### B2. Population octant matrices — Note 3 (p. 18)

`devel/m4-zw-transcription.md`. These define the simulation conditions the
diagnostic was validated against.

- [ ] Without a substantial general factor (IAS, 2,988 students; Gurtman & Pincus, 2000; Wiggins, 1995): ρ1 `.430`, ρ2 `.030`, ρ3 `−.360`, ρ4 `−.740`
- [ ] With a substantial general factor (IIP-C, 1,981 students; Gurtman & Balakrishnan, 1998): ρ1 `.683`, ρ2 `.500`, ρ3 `.345`, ρ4 `.288`

### B3. Scaling-factor formulas — Eq. A6, A7 (p. 18) and Eq. 3 (p. 12)

The record flags **one resolved channel discrepancy** here. Please confirm the
resolution against the page image, since it was settled by reasoning rather
than by a clean second read:

- [ ] Eq. A7 in full: `f_a = ½·√( √2(ρ1−ρ3) + (1−ρ4) )` — radicand `√2(ρ1−ρ3)+(1−ρ4)`, **not** `2(ρ1−ρ3)+(1−ρ4)`. The text layer rendered it without the √; the page image shows it with. **Confirm the leading ½ as well** — the first cut listed only the radicand, so the ½ has never been checked against the page.
- [ ] Eq. A6: `f_e = √((2ρ1+2ρ2+2ρ3+ρ4+1)/8)`
- [ ] Eq. 3 (p. 12): `|AFF_min| = 2.95 · f_a · n^(−0.587)` — confirm both constants
- [ ] Derived values quoted on p. 9: f_e → IIP-C `.737`, IAS `.240`; f_a → IIP-C `.545`, IAS `.845`
- [ ] p. 14: IIP-SC f_a `.625`; `|.029|` at N = 1166
- [ ] p. 12 worked values: f_a = .545 → n = 100 gives `.108` ("as large as .11"); n = 1000 gives `.028` (".03")

> **Numeric self-check, re-run 2026-07-19.** Feeding the B2 matrices through
> the formulas as transcribed reproduces all eight published constants:
> f_e → .7369 / .2398 (pub .737 / .240); f_a → .5454 / .8452 / .6246
> (pub .545 / .845 / .625); Eq. 3 → .1077, .0279, .0292 (pub .11, .03, .029).
> The no-√2 variant gives .589 / .911 / .675 and misses every one.
>
> **What this does and does not settle.** It is a *consistency* check, not an
> independent oracle: B2's ρ values and these formulas were transcribed by the
> same pass, so a compensating pair of errors would still close. It does mean
> the √2 reading, the ½, and the four IIP-C ρ values are mutually pinned — an
> error in any one of them alone would break the identity. Read the pages
> anyway; this only tells you the set is internally coherent.

### B4. Study 5 reproduction (pp. 13–16) and Table 4 (p. 15)

The vignette reproduces these analyses live on the bundled `jz2017` data, so a
transcription error here would surface as a mismatch between the vignette's
prose and its own computed output. **The first cut's single row here
conflated two different Table 4 values** — OCPD's *amplitude* and PARPD's
*elevation* — and cited a stale line for each; split and re-anchored below.

- [ ] The Study 5 CircE fit pattern the vignette narrates (`:151-157`), including the printed indices: equal spacing + equal communality CFI `.824`, TLI `.795`, RMSEA `.169`; unequal spacing CFI `.958`, TLI `.931`, RMSEA `.098` (p. 14)
- [ ] Table 4, **obsessive–compulsive** PD: amplitude `.012` at full sample size — the vignette's stated reason for picking this scale as the near-flat case (`:237-238`)
- [ ] Table 4, **paranoid** PD: elevation `.250`, the value the ipsatizing section says the raw-score analysis matches (`:572-573`)
- [ ] N = 1,166 undergraduates, IIP-SC octants + PDQ-4+ target measures (`:41-42`)

> Both Table 4 rows are transcribed in full in `devel/m4-zw-transcription.md`
> (Paranoid and Obsessive–compulsive, with CIs); that record also notes OCPD's
> a and δ print without CI brackets in the table. Worth confirming, since an
> absent bracket is easy to read as a transcription omission.

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
