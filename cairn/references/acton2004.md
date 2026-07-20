# acton2004 — the four circumplex-structure criteria `fit_structure()` implements, and why its cutoffs are not the paper's

**Provenance.** Ingested 2026-07-19 by M43 from
`cairn/references/sources/acton2004.pdf` (gitignored).
Pagination: the journal's own, *MPR Online* 9(1) 1–27; the shelf PDF's page *n*
is printed page *n* (confirmed against the running heads on pp. 3, 5, 9, 13),
so every anchor below is a printed page.
Extraction: verified 2026-07-19 against the source by two independent channels — `pdftotext -layout` and a visual read of `pdftoppm`-rendered page images — covering every equation (pp. 5–7, 10), every published cutoff (pp. 17, 19), and Table 2 (p. 15); the surrounding prose anchors (pp. 8, 9, 13, 18, 20, 22, 23) rest on the text channel alone, which carries prose faithfully but silently drops this paper's display equations, and no value here has been read by a human — observed 2026-07-19.

The two channels are genuinely independent for this source, which is not
something to assume. `pdfinfo` reports Producer `Acrobat Distiller 5.0
(Windows)` over Creator `PScript5.dll`, i.e. a born-digital PDF generated from
a Word document — not an OCR scan, where the text layer would be machine
output *derived from* the page image and would corroborate nothing (M42-D1).
Here the text layer is the embedded character stream and the image is the
rasterized glyphs, so a font-encoding fault shows up as a disagreement. The
independence is real but the channels are still both machine channels.

**Citation.** Acton, G. S., & Revelle, W. (2004). Evaluation of ten
psychometric criteria for circumplex structure. *Methods of Psychological
Research Online, 9*(1), 1–27.

**Role.** The source of the four factor-analytic criteria `fit_structure()`
computes — the Fisher Test, Gap Test, Variance Test 2, and Rotation Test — and
of the deviation-scoring guidance and exploratory framing the package's
documentation carries. It is **not** the source of the interpretive cutoffs the
package ships; those were re-derived, and the section "The cutoffs the package
ships are not the cutoffs on this page" below is the load-bearing part of this
page.

## Extracted values

### Notation (pp. 3–4)

`f` = factor, `nf` = number of factors (constrained to 2 in the simulations,
their footnote 4), `v` = variable, `nv` = number of variables, `θ` = angle of
rotation, `θ_v` = angular position of variable *v*, `φ_fv` = loading of
variable *v* on factor *f*, `φ_fvθ` = that loading after rotation by θ.

Every criterion is applied to "the first pair of factors extracted using
principal-axis factor analysis without rotation" (p. 13) — which is exactly
what `structure_loadings()` produces. From the Fisher Test onward the summary
statistic is the coefficient of variation, "the standard deviation divided by
the mean" (p. 6).

### The four implemented criteria, as printed

| Criterion | Eq. | Page | As printed |
|---|---|---|---|
| Gap Test | (2) | 5 | `Gap Test = σ²_Xv`, where `X_v = (θ_v+1 − θ_v)` for *v* = 1 to (nv − 1), and `X_v = (2π + θ_1 − θ_nv)` for *v* = nv |
| Fisher Test | (6) | 6 | `Fisher Test = σ_Xv / X̄_v`, where `X_v = Σ_{f=1}^{nf} φ_fv²` |
| Variance Test 2 | (8) | 7 | `VT2 = σ_Xθ / X̄_θ`, where `X_θ = σ²_Yvθ`, and `Y_vθ = φ²_1vθ / Σ_{f=1}^{nf} φ²_fvθ` |
| Rotation Test | (9) | 7 | `RT = σ_Xθ / X̄_θ`, where `X_θ = Σ_{v=1}^{nv} σ²_vθ`, and `σ²_vθ = Σ_{f=1}^{nf}(φ²_fvθ − φ̄²_vθ)² / (nf − 1)` |

Three of the four are implemented as printed, verified line by line:

- **Gap (Eq. 2)** — `structure_gap()` at `R/fit_structure.R:123-134` takes
  `var()` of the gaps **including the wrap-around gap** `2π + θ_1 − θ_nv`,
  which is part of A&R's printed definition rather than an addition. Note
  Eq. 2 is a variance (radians²), *not* a CV; the p. 6 sentence introducing
  the CV scopes it to "all of the circumplex criteria described hereafter",
  i.e. from Eq. 6 onward.
- **VT2 (Eq. 8)** — `structure_vt()` at `R/fit_structure.R:159-168` computes
  `var(rl[, 1]^2 / h2)` per rotation, then `sd(x)/mean(x)`.
- **RT (Eq. 9)** — `structure_rt()` at `R/fit_structure.R:195-199` computes
  `sum((rl2[, 1] - rl2[, 2])^2 / 2)` per rotation. At nf = 2 this **is**
  Eq. (9) exactly: `φ̄²_vθ = (φ²_1 + φ²_2)/2`, so
  `Σ_f (φ²_fvθ − φ̄²_vθ)²` = `(φ²_1 − φ²_2)²/2`, and `(nf − 1) = 1`.

A&R never state the range of the rotation grid — only that θ is "broken down
arbitrarily into intervals such as 5 degrees" (pp. 6–7). The package's
full-period grids (VT2 0–175°, RT 0–85°) are its own choice, made so the
statistics are exactly invariant to the arbitrary orientation of the unrotated
solution; the shipped cutoffs are calibrated on those grids.

**One ambiguity the paper leaves open and the package resolves consistently:**
A&R write `σ²` without saying whether it is the population (÷ n) or sample
(÷ n − 1) variance. Eq. (9) is the tell — it divides explicitly by `(nf − 1)`
— so the n − 1 convention is the paper's own, and `stats::var()` matches it.

### The Fisher Test: what the equation prints vs. what the prose says

This is the one criterion the package does **not** implement as printed, and
the two readings are recorded separately here so they stay distinguishable
from this page alone.

**As Eq. (6) prints it (p. 6):** `X_v = Σ_f φ_fv²` — the sum of squared
loadings over factors, i.e. the **communality h²**. So the printed Fisher Test
is the CV of the communalities.

**As the prose one paragraph above it says (p. 6), verbatim:**

> A variable's vector length on a circumplex is equal to the square root of
> its communality on the two circumplex dimensions. The mean vector length
> provides an estimate of the radius of the circle, and the standard deviation
> of vector lengths provides an estimate of scatter around or deviation from
> the circumference.

So the prose describes the CV of **vector lengths √h²**. The two differ by
roughly a factor of two in CV and cannot both carry one set of cutoffs.

**What the repo ships, and why:** `structure_fisher()`
(`R/fit_structure.R:104-114`) computes `sd(h)/mean(h)` on `h <- sqrt(h2)` —
the **prose** reading, not the printed equation. The resolution was empirical,
not editorial: reproducing A&R's own design yields the published .10/.15
cutoffs on the vector-length scale and roughly doubled values on the
communality scale, so whatever their CIRC_STRUC program computed, it evidently
computed vector lengths (as does `psych::circ.tests`). The reasoning is
recorded **twice** in the code, and a corrector changing one copy must change
the other: at `R/fit_structure.R:95-103` (the `structure_fisher()` header
comment) and again at `R/fit_structure.R:332-338` (the exported wrapper's
roxygen, which states the same printed-vs-prose split and the same
resolution), plus `devel/ar2004-transcription.md`. The departure is deliberate and is a
departure *in kind* from the printed equation — this page does not resolve it
in the paper's favour or the repo's, it records that they differ.

### The published cutoffs — nv = 64/128, read off Figures 4–8

Stated in prose, not tables. Every one below was confirmed in both channels.

| Criterion | Scoring | "Almost certainly/always" | "3× as likely" | "2× as likely" | Page |
|---|---|---|---|---|---|
| Fisher (equal axes) | raw & deviation | < .10 | — | .15 | 17 |
| Gap | raw | < .01 | — | < .04 | 17 |
| Gap | deviation | < .03 | — | < .05 | 17–18 |
| VT2 | raw | < .25 | — | < .30 | 19 |
| VT2 | deviation | < .40 | < .58 | < .65 | 19 |
| RT | raw | < .04 | — | < .09 | 19 |
| RT | deviation | < .14 | — | < .31 | 19 |
| MT (not implemented) | raw | < .03 | — | < .05 | 20 |
| MT (not implemented) | deviation | < .06 | — | < .16 | 20 |

Fisher also has a "equally likely" point at **.21** (p. 17), and in deviation
scoring the equal/unequal separation is gone above about **.40**.

These are **heuristic likelihood classifications read off cumulative
relative-frequency plots, never significance tests** — the package's own
vignette-precision rule forbids describing them otherwise, and the paper's
phrasing ("almost certainly indicated", "at least twice as likely to indicate")
is likelihood language throughout.

### The calibration basis (p. 8) and the nv effect (p. 18)

The published design is 384 samples in a
2 (raw vs. deviation) × 2 (unrotated vs. rotated) × 2 (equal vs. unequal axes)
× 2 (interstitiality vs. simple structure) × 3 (general factor: none / large /
variable) × 2 (150 vs. 600 subjects) × **2 (64 vs. 128 variables)** factorial,
two samples per cell (p. 8). So every published cutoff above is an
**nv = 64/128** quantity, pooled over 96 samples per curve.

The number-of-variables effect on the Gap Test is "substantial",
`F(1, 192) = 3,458.4, η² = .11` (p. 18), for a stated structural reason: the
Gap variance of a perfect circumplex is zero at any nv, but the Gap variance of
an eight-variable *simple structure* is much larger than that of a
128-variable one, "because many variables clumping together have a small
variance."

**A&R ran a further simulation at 8, 16, and 32 variables and never reported
its results.** They announce it twice — "a further simulation was conducted
using 8, 16, and 32 variables" (p. 10) and "necessitated the addition of a
further simulation using 8, 16, and 32 variables (in addition to 64 and 128).
The Gap Test was the only criterion to necessitate such treatment" (p. 18) —
and no per-nv cutoff appears anywhere in the Results or Discussion. This
absence is why the package could not look the nv = 8 cutoffs up.

### The generating model (Eqs. 11.1–11.3, pp. 10–11)

- **Eq. (11.1):** `X_fv = γ Z + ω φ_1v Z + ξ φ_2v Z + ε_v Z`, "where Z is a
  normally distributed random number, γ is the general factor weight, ω and ξ
  are factor weights for the first and second bipolar factors, and ε_v is the
  uniqueness."
- **Eq. (11.2)**, interstitial loadings: `φ_1v = cos(2πv/nv)`,
  `φ_2v = sin(2πv/nv)`.
- **Eq. (11.3)**, simple-structure loadings: assigned to the nearest axis by
  quarters of `v/nv` — `[7/8,1) ∪ [0,1/8)` → (1, 0); `[1/8,3/8)` → (0, 1);
  `[3/8,5/8)` → (−1, 0); `[5/8,7/8)` → (0, −1).
- **Uniqueness** (p. 11): "In all cases, ε_v = √(1 − (φ_1v² + φ_2v²))."

Two things about this model are underdetermined as printed, and
`data-raw/structure-test-cutoffs.R` had to settle both empirically rather than
by reading: Eq. (11.1) uses **one symbol Z for four different random numbers**
(read literally, every variable would be perfectly correlated with every
other), and the printed uniqueness formula gives **ε_v ≡ 0 in every
condition**, since both loading schemes satisfy `φ_1v² + φ_2v² = 1`. The
adjudications are recorded in `devel/ar2004-transcription.md`; the standardized
reading (`ε_v² = 1 − γ² − ω²φ_1v² − ξ²φ_2v²`) is the one that reproduces the
published cutoffs.

### Deviation scoring (pp. 8–9, 22–23)

Defined at pp. 8–9: "deviation scores are raw scores minus the mean of the
subject. Deviation scoring is often used to reduce the size of the general
factor, especially if the general factor is thought to have no substantive
interest (e.g., acquiescence)." The paper notes on p. 9 that "deviation scoring
is often called ipsatizing, but this latter term is ambiguous, because it could
mean either deviation scoring or z-scoring" — which is why the package's
`ipsatize()` is documented as row-mean centering specifically.

The mechanism is stated on p. 22: "Deviation scoring works because it removes a
general factor if there is one and has little effect if there is not."

The recommendation is not unconditional. Deviation scoring is "recommended"
for the Fisher Test (p. 17) and "strongly recommended in every case" for VT2
(p. 19), but the Discussion calls it "a mixed blessing: Used with the correct
interpretation, it can enhance the power of a test; used with an incorrect
interpretation, it can render fallacious results" (p. 23), and for the Gap Test
specifically "deviation scoring in some cases actually causes it to register
the opposite of the correct result" (pp. 22–23).

### The exploratory framing (p. 23)

> Thus, the criteria that are said to work are useful primarily for
> exploratory purposes.

This is the sentence behind `fit_structure()`'s roxygen calling these the
"exploratory circumplex-structure criteria" (`R/fit_structure.R:707`).

## The cutoffs the package ships are not the cutoffs on this page

`structure_cutoffs` (`R/fit_structure.R:262-281`) contains **nv = 8 constants
re-derived by this repo**, not values transcribed from this paper. They were
produced by `data-raw/structure-test-cutoffs.R` (seed 20260707) under A&R's own
generating model, after that script first reproduced the published nv = 64/128
design as a sanity gate on the simulation machinery (14 of 17 one-sided claims
reproduced; three left-tail limits documented in the script).

Side by side — published (nv = 64/128, this page) vs. shipped (nv = 8):

| Criterion | Scoring | Published almost / 3× / 2× | Shipped almost / 3× / 2× |
|---|---|---|---|
| Fisher | raw | .10 / — / .15 | .10 / .13 / .15 |
| Fisher | deviation | .10 / — / .15 | .07 / .12 / .15 |
| Gap | raw | .01 / — / .04 | .35 / .51 / .55 |
| Gap | deviation | .03 / — / .05 | .15 / .40 / .46 |
| VT2 | raw | .25 / — / .30 | .12 / .33 / .37 |
| VT2 | deviation | .40 / .58 / .65 | .19 / .59 / .64 |
| RT | raw | .04 / — / .09 | .13 / .30 / .35 |
| RT | deviation | .14 / — / .31 | .32 / .64 / .67 |

The Gap row is the one that matters most: the raw-scored "almost certainly"
cutoff moves from **.01 to .35**, a 35-fold shift, which is the nv effect
A&R documented but never quantified in cutoff terms. Fisher's raw row happens
to coincide at both published points, which is a coincidence of this criterion
being nearly nv-free, not evidence that the published values transfer
generally — `fit_structure()` refuses to interpret any scale count it has not
calibrated, and only nv = 8 is calibrated.

**Consequence for a corrector:** a value in `structure_cutoffs` that looks
wrong is not checkable against this page. It is checkable against
`data-raw/structure-test-cutoffs.R`, and *that script's machinery* is checkable
against this page's published-cutoff table.

## What this source does not license

- **RANDALL is not from this paper.** `fit_structure()` runs a fifth test,
  RANDALL, which the package correctly attributes to **Hubert & Arabie (1987)**
  and **Tracey (1997)** at `R/fit_structure.R:714,773-778`. Nothing about it
  traces here. One comment does drift:
  `tests/testthat/test-fit_structure_api.R:2` describes the entry point as
  covering "the five Acton & Revelle (2004) structure tests", which is wrong
  by one — it is four A&R criteria plus RANDALL.
- **The Minkowski Test is not implemented and its cutoffs are recorded above
  only for completeness.** A&R found MT effective — indeed it has their largest
  interstitiality effect, `F(1, 192) = 6,454.8, η² = .55` (Table 2, p. 15) — but
  it correlates **.99 with RT** (Table 1, p. 13, stated in the p. 13 prose), so
  it carries almost no information RT does not. Nothing in the repo computes it.
- **Five of the paper's ten criteria are reported as not working** and the
  package implements none of them: the Squared Loadings Index, Gap* Test, GDIFF
  Test, CDIFF Test, and VT1 (pp. 14–15).
- **These criteria yield no p-values.** The paper's own language is likelihood
  classification read off simulated distributions. RANDALL is the only test in
  `fit_structure()` with a genuine p-value, and it comes from a different
  source.

## Reconciliation with `devel/ar2004-transcription.md`

That file is the repo's 2026-07-07 transcription record (M4.5/T2), read
here **read-only**; M43 changes no `devel/` file. Its own status line records
"Second independent human re-read: pending (Jeff)", so it carries no human
attestation this page could inherit even if inheriting one were legitimate —
which, per M40, it is not: authoring this page is itself a fresh extraction.

**Agreement.** My independent two-channel extraction matches it on every point
compared: the four equations including the wrap-around gap in Eq. 2; the
Eq. 6 printed-vs-prose split and the empirical resolution; the p. 13
factor-extraction basis; the p. 6 CV sentence; the full published-cutoff
table (all nine rows); the pp. 8–11 design and generating model; and the
finding that no per-nv cutoffs are published.

**Two paper-internal inconsistencies it records, both independently confirmed
here:**

1. VT2's equal-axes effect is `24.6` in Table 2 (p. 15) but `24.5` in the
   p. 18 prose. Immaterial — nothing uses it.
2. The Minkowski Test paragraph on p. 20 reads "In deviation scored data, an
   **RT** value less than .06 almost certainly indicated interstitiality" where
   every surrounding sentence, and Figure 8, make clear **MT** is meant.

**A third it does not record, found here.** The same p. 20 MT paragraph gives
the deviation-scoring effect as `F(1, 192) = 1,265.5, η² = .11`, but Table 2
(p. 15) gives MT's IS × Dev cell as **`1,262.5** (.11)`**. The η² agrees at
.11, so the table's `1,262.5` is the coherent value and the prose's `1,265.5`
looks like a digit corruption — possibly contaminated by RT's genuine
`1,265.6` one row above it in the same table. Immaterial to the package
(nothing uses MT), and recorded here so the erratum list is complete rather
than because anything depends on it.

**No between-channel discrepancies** were found in this extraction. One
artifact worth naming so it is not mistaken for one later: the text channel
returns an *empty line* where each display equation sits (Eq. 6 extracts as
nothing at all), so the equations on this page rest on the image channel. That
is a silent-dropout failure mode, not a disagreement — the text channel does
not render a wrong equation, it renders none.

## Traces to

- `R/fit_structure.R:104-114` — `structure_fisher()`, the Eq. 6 prose reading.
- `R/fit_structure.R:95-103` — the comment recording the Eq. 6 discrepancy and
  its empirical resolution.
- `R/fit_structure.R:123-134` — `structure_gap()`, Eq. 2 including the
  wrap-around gap.
- `R/fit_structure.R:151-168` — `structure_vt()`, Eq. 8.
- `R/fit_structure.R:178-199` — `structure_rt()`, Eq. 9.
- `R/fit_structure.R:232-251` — the `structure_cutoffs` header comment, which
  cites p. 18's nv effect and names the re-derivation.
- `R/fit_structure.R:262-281` — `structure_cutoffs` itself: **re-derived
  nv = 8 constants, not values from this page.**
- `R/fit_structure.R:283-320` — `structure_interpret()`, the almost/thrice/twice
  likelihood ladder that renders A&R's phrasing.
- `R/fit_structure.R:705-778` — `fit_structure()`'s roxygen: the exploratory
  framing (p. 23), the p. 9 deviation-scoring citation at `:722`, and the
  `@references` entry.
- `R/fit_structure_oop.R:42,107,128,181` — the likelihood phrasing map and the
  print/summary headers naming the source.
- `vignettes/evaluating-circumplex-structure.Rmd:391,405,437,478-492,603` — the
  simple-structure framing, the p. 13 factor-extraction citation, the p. 9
  deviation-scoring citation, the "Where the cutoffs come from" section, and the
  reference-list entry.
- `data-raw/structure-test-cutoffs.R` — the re-derivation script; this page's
  published-cutoff table is its sanity-gate target.
- `devel/ar2004-transcription.md` — the 2026-07-07 transcription record
  reconciled above (read-only).

## Open questions

- **Figures 4–8 were not read.** Every cutoff on this page is quoted from the
  authors' prose statements of what those figures show; the figures themselves
  (the cumulative relative-frequency curves the cutoffs were read off) have not
  been examined, and neither has the exact procedure by which a cutoff was read
  off a curve — the paper gives no formula for it. `data-raw/structure-test-cutoffs.R`
  operationalizes one; whether it matches what A&R actually did is
  unverifiable from the paper — observed 2026-07-19.
- **No value on this page has been read by a human.** Both channels are machine
  channels operating on the same PDF, so a defect in the source document itself
  — as opposed to its text layer — would not have been caught — observed
  2026-07-19.
- **RANDALL's two sources owe pages and are not on the shelf.** `structure_randall()`
  and `structure_randall_test()` are shipped implementations relying on Hubert
  & Arabie (1987) and Tracey (1997); neither PDF is in
  `cairn/references/sources/` and neither has a page. Raised as a ROADMAP
  candidate at the M43 implementation gate rather than expanded into M43's
  scope; a milestone taking it up needs both sources shelved first — observed
  2026-07-19.
- **The `test-fit_structure_api.R:2` "five Acton & Revelle" comment is wrong**
  and this page does not fix it — M43 changes no package file. It is a comment,
  so nothing computes differently; it would ride along cheaply with whatever
  milestone next edits that test file — observed 2026-07-19.
