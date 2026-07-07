# Acton & Revelle (2004) transcription record (M4.5/T2)

**Source:** Acton, G. S., & Revelle, W. (2004). Evaluation of ten psychometric
criteria for circumplex structure. *Methods of Psychological Research Online,
9*(1), 1–27. Transcribed from the author-hosted PDF
(personality-project.org/revelle/publications/acton.revelle.mpr110_10.pdf,
printed pagination 1–27; page numbers below are the printed pages).

**Protocol (Brief A §6.1 two-session rule; B6 and Z&W-transcription
precedent):** Channel 1 = visual page read of the rendered PDF (Fable,
2026-07-07). Channel 2 = independent `pdftotext -layout` text-layer
extraction, diffed against channel 1 on every load-bearing numeral (same
date). **No between-channel discrepancies.** Two *paper-internal*
inconsistencies found (both confirmed in both channels, so they are the
paper's, not the transcription's):

- VT2's equal-axes effect is F(1, 192) = **24.6** in Table 2 (p. 15) but
  **24.5** in the p. 18 prose. Immaterial here (we use neither).
- The Minkowski Test paragraph (p. 20) reads "an **RT** value less than .06"
  where context (and Figure 8) make clear **MT** is meant.

**Second independent human re-read: pending (Jeff)** — same status convention
as the B6 and Z&W transcriptions.

## Criterion definitions (transcribed; the four we revive plus context)

Notation (pp. 3–4): f = factor, nf = number of factors (constrained to 2 in
the simulations, footnote 4), v = variable, nv = number of variables, θ =
angle of rotation, θ_v = angular position of variable v, φ_fv = loading of
variable v on factor f, φ_fvθ = that loading after rotation by θ. All criteria
are applied to **the first pair of factors extracted using principal-axis
factor analysis without rotation** (p. 13, "Statistical Analyses"; the
"rotated" design condition additionally varimax-rotates that pair, p. 9). The
summary statistic for every criterion from the Fisher Test onward is the
coefficient of variation, σ/mean (p. 6).

- **Gap Test** (Eq. 2, p. 5): Gap Test = σ²_{Xv} with X_v = (θ_{v+1} − θ_v)
  for v = 1 … (nv − 1) and **X_nv = (2π + θ_1 − θ_nv)** — the wrap-around gap
  is part of the definition. Angles in radians; the statistic is a variance
  (radians²), *not* a CV (the CV wording on p. 3 announces the summary
  statistic used from Eq. 6 onward; Eq. 2 itself is the variance, and the
  Figure 5 axis is on that scale).
- **Fisher Test** (Eq. 6, p. 6): σ_{Xv} / mean(X_v) with X_v = Σ_f φ_fv² —
  the CV of the two-factor **communalities** as printed; the prose describes
  vector lengths √h². *[The initial editorial ruling here ("the equation
  governs") was overturned by the T2 gate: the published cutoffs reproduce
  only on the vector-length scale — see Empirical adjudications below. The
  transcription of the equation itself stands.]*
- **VT1** (Eq. 7, p. 7): CV over rotations θ of X_θ = σ²_{Yvθ} with
  Y_vθ = φ_1vθ / Σ_f φ_fvθ². (Ineffective per the paper; not revived.)
- **VT2** (Eq. 8, p. 7): CV over rotations θ of X_θ = σ²_{Yvθ} with
  **Y_vθ = φ_1vθ² / Σ_f φ_fvθ²** (squared factor-1 loading normalized by the
  variable's own communality). Rotation grid: "computed over a range of
  values of θ that are broken down arbitrarily into intervals such as 5
  degrees" (pp. 6–7) — **the range is unstated** (grid provenance gap; see
  §Re-derivation notes).
- **RT** (Eq. 9, p. 7): CV over rotations θ of X_θ = Σ_v σ_vθ² with
  σ_vθ² = Σ_f (φ_fvθ² − mean_f(φ_vθ²))² / (nf − 1).
- **MT** (Eq. 10, p. 8): CV over rotations of Σ_v (Σ_f |φ_fv|^R)^(2/R),
  Minkowski-R generalized communality. (Correlates .99 with RT, Table 1;
  not revived.)

## Simulation design (transcribed, pp. 8–11)

384 samples: 2 (raw vs. deviation scored) × 2 (unrotated vs. varimax-rotated
PA) × 2 (equal vs. unequal axes) × 2 (interstitiality vs. simple structure)
× 3 (general factor: none / large / variable) × 2 (150 vs. 600 subjects) ×
2 (64 vs. 128 variables), **2 samples per cell**. So every relative-frequency
curve in Figures 4–8 pools 96 samples (2 reps × the 48 cells sharing that
scoring × structure/axes level) — the published cutoffs carry that
small-sample granularity.

- **Deviation scoring** (pp. 8–9): "raw scores minus the mean of the subject"
  — row-mean centering across variables (what `ipsatize()` does). The paper
  itself notes "ipsatizing" is ambiguous terminology.
- **Generating model** (Eq. 11.1, p. 10): X_fv = γZ + ωφ_1v·Z + ξφ_2v·Z +
  ε_v·Z, "where Z is a normally distributed random number, γ is the general
  factor weight, ω and ξ are factor weights for the first and second bipolar
  factors, and ε_v is the uniqueness."
- **Interstitial loadings** (Eq. 11.2): φ_1v = cos(2πv/nv), φ_2v = sin(2πv/nv).
- **Simple-structure loadings** (Eq. 11.3): assigned to the nearest axis by
  quarters of v/nv — [7/8,1)∪[0,1/8) → (1, 0); [1/8,3/8) → (0, 1);
  [3/8,5/8) → (−1, 0); [5/8,7/8) → (0, −1).
- **Factor weights** (p. 11):
  | condition | γ | ω | ξ |
  |---|---|---|---|
  | no GF, equal axes | 0.0 | 0.6 | 0.6 |
  | no GF, unequal axes | 0.0 | 0.7 | 0.5 |
  | large GF, equal axes | 0.5 | 0.4 | 0.4 |
  | large GF, unequal axes | 0.5 | 0.4 | 0.3 |
  | variable GF, equal axes | 0.3–0.7 by 0.1 | 0.4 | 0.4 |
  | variable GF, unequal axes | 0.3–0.7 by 0.1 | 0.4 | 0.3 |
- **Uniqueness** (p. 11): "In all cases, ε_v = √(1 − (φ_1v² + φ_2v²))."

### Transcription-level interpretation notes (ours, flagged for the re-read)

1. **The repeated Z.** Eq. 11.1 uses one symbol Z for four random numbers.
   Read literally (one Z), every variable would be perfectly correlated with
   every other. The only sensible reading of "the common factor model" is
   four *independent* standard normal scores per subject: X_v = γZ_g +
   ωφ_1v·Z_1 + ξφ_2v·Z_2 + ε_v·Z_v, with Z_g, Z_1, Z_2 shared across
   variables within a subject and Z_v unique per variable.
2. **The uniqueness is literally zero.** Both loading schemes satisfy
   φ_1v² + φ_2v² = 1 for every v (Eq. 11.2 by the Pythagorean identity;
   Eq. 11.3 assigns unit vectors), so the printed ε_v formula gives ε_v ≡ 0
   in *all* conditions. Candidate readings: **(a) literal** — ε_v = 0,
   variables are exact linear combinations of the factor scores and all
   sampling noise enters through the finite-sample correlations of the
   factor scores; **(b) standardizing** — the weights belong inside the
   radicand, ε_v² = 1 − (γ² + ω²φ_1v² + ξ²φ_2v²), making each variable unit
   variance. The T2 simulation runs both readings at the published design
   and keeps the one that reproduces the published cutoffs (see
   `data-raw/structure-test-cutoffs.R`).
3. **Variable-γ assignment.** "γ varied from 0.3 to 0.7 in increments of
   0.1" does not say how γ maps to variables. Consistent with "differential
   assignment of factor weights" (p. 8), we cycle γ over {.3, .4, .5, .6, .7}
   across variables.
4. **Footnote 4 cross-check** (p. 4): the model "implies a correlation of −1
   between variables at 180-degree angles" — consistent with reading (a)'s
   exact-linear-combination population structure (reading (b) attenuates
   that correlation only when ε_v > 0; with the transcribed weights, e.g.
   no-GF equal axes, (b) gives ρ(180°) = −.36/1 = −.36 — a discriminating
   fact the sanity gate can exploit).

## Published cutoffs (transcribed; the sanity-gate targets)

Read by the authors off cumulative relative-frequency plots (Figures 4–8);
they are **heuristic classification cutoffs, not significance tests** (per
package vignette-precision rule, never describe them otherwise).

| Criterion | Scoring | "Almost certainly/always" | "3× as likely" | "2× as likely" | Other |
|---|---|---|---|---|---|
| Fisher (equal axes) | raw & dev (p. 17) | < .10 | — | .15 | .21 equally likely; discrimination gone above ≈ .40 (dev) |
| Gap (interstitiality) | raw | < .01 | — | < .04 | |
| Gap | deviation | < .03 | — | < .05 | |
| VT2 | raw | < .25 | — | < .30 | |
| VT2 | deviation | < .40 | < .58 | < .65 | |
| RT | raw | < .04 | — | < .09 | |
| RT | deviation | < .14 | — | < .31 | |
| MT (not revived) | raw | < .03 | — | < .05 | |
| MT (not revived) | deviation | < .06 | — | < .16 | |

Directionality: for the interstitiality criteria, values are lower under
interstitiality throughout their range (pp. 17–20); the Fisher Test is lower
under equal axes.

## The nv effect (the reason T2 exists; p. 18)

"The effect of number of variables on the Gap Test was substantial,
F(1, 192) = 3,458.4, η² = .11. … the variance of an eight-variable simple
structure will be substantially larger than that of a 128-variable simple
structure … The large effect of number of variables on the Gap Test
necessitated the addition of a further simulation using 8, 16, and 32
variables (in addition to 64 and 128). The Gap Test was the only criterion to
necessitate such treatment." **No per-nv cutoffs are published** — the 8/16/32
follow-up is mentioned but its results are not reported. Hence the package
must re-derive nv = 8 cutoffs itself; the published nv = 64/128 cutoffs serve
only as the sanity gate for the re-derivation machinery.

Other usage guidance transcribed: deviation scoring recommended for the
Fisher Test (p. 17) and "strongly recommended in every case" for VT2/MT
(pp. 19, 21); VT2 mislabels raw-scored simple structure as interstitial under
a large/variable general factor (p. 19 and Discussion p. 22) — deviation
scoring is what prevents that, so VT2's interpretation is only trustworthy
without a large general factor.

## Re-derivation notes (ours, not the paper's)

- **Cutoff operationalization.** The paper gives no formula for how cutoffs
  were read off the CDF plots. T2 operationalizes: with equal condition
  priors, "k times as likely to indicate A as B at values below x" is the
  CDF ratio F_A(x)/F_B(x) ≥ k, and "almost certainly A below x" is
  F_B(x) ≤ .01 with F_A(x) materially positive. The sanity gate checks that
  this operationalization, applied to A&R's own design (nv = 64/128 pooled
  as in Figures 4–8), reproduces the published cutoffs within the
  granularity of their 96-sample curves; only then is it applied at nv = 8.
- **Rotation grids.** Because the CIRC_STRUC grid range is unstated and a
  partial-period grid makes VT2/RT depend on the arbitrary orientation of
  the unrotated PA solution (VT2 harmonics have period 180°, RT period 90°),
  the package computes both over one **full period on a 5° grid** (VT2:
  0–175°; RT: 0–85°), which makes them exactly orientation-invariant. The
  nv = 8 cutoffs are calibrated on those grids — i.e., on the statistic the
  package actually computes, not on an unverifiable reproduction of
  CIRC_STRUC. The gate therefore expects tight reproduction for Fisher/Gap
  (grid-free) and only qualitative agreement for VT2/RT if CIRC_STRUC's
  window differed.
- A&R's "rotated" design condition is irrelevant to the package pipeline:
  communalities and gaps are invariant to any orthogonal rotation of the
  factor pair, and full-period grids make VT2/RT invariant too, so
  `structure_loadings()` (always unrotated PA) covers every case.

## Empirical adjudications (T2 gate results, 2026-07-07)

`data-raw/structure-test-cutoffs.R` (seed 20260707) reproduced A&R's own
nv = 64/128 design under both candidate readings and both Fisher scales;
outcomes, recorded here because they settle transcription ambiguities:

- **Uniqueness reading: standardized** (ε_v² = 1 − γ² − ω²φ₁ᵥ² − ξ²φ₂ᵥ²).
  The literal reading (ε ≡ 0) is refuted outright: it makes equal-axes vector
  lengths asymptotically constant, collapsing the Fisher equal-axes
  distribution to ≈ 0, unlike Figure 4; its Gap distributions also miss the
  published cutoffs several-fold. Note the standardized reading attenuates
  footnote 4's "correlation of −1 at 180°" claim (interpretation note 4) —
  the footnote describes the noiseless ideal, not the simulated data.
- **Fisher scale: CV of vector lengths** (the prose), *not* CV of
  communalities (Eq. 6 as printed). Reproduced cutoffs: CV(√h²) raw
  .092/.133 and deviation .111/.171 against published .10/.15; CV(h²) gives
  ≈ doubled values (.18–.33). CIRC_STRUC evidently computed vector lengths
  (psych::circ.tests agrees). This **overturns the method-review §1
  recommendation** to follow the equation over the prose.
- **Claim-level reproduction: 14 of 17.** Because A&R's cutoffs are one-sided
  conservative reads ("almost certainly", "at least k times as likely"), the
  gate checks the claims themselves (F_other ≤ .031 at "almost" cutoffs,
  CDF ratio ≥ k/√2 at "k-times" cutoffs). All likelihood-ratio claims pass,
  including RT's. Three left-tail "almost" claims do not fully reproduce:
  vt.raw (.25; F_simple = .048) and fisher.raw (.10; F_unequal = .045) are
  marginal, gap.deviation (.03; F_simple = .176) is a genuine distributional
  difference — our deviation-scored simple-structure Gap values extend
  further left than theirs did, plausibly reflecting CIRC_STRUC's unstated
  upstream factor-extraction pipeline ("the input for the program is a
  factor matrix", footnote 2 — the extraction software is never named).

## Change log

- 2026-07-07 — Initial transcription (Fable, T2). Channels 1+2 diffed, no
  between-channel discrepancies; two paper-internal inconsistencies logged
  (Table 2 24.6 vs prose 24.5; p. 20 "RT" for MT). ε_v ≡ 0 ambiguity and
  repeated-Z notation documented with candidate readings for the empirical
  gate. Jeff's independent re-read pending (a full-text copy is in the
  Zotero library).
- 2026-07-07 — Empirical-adjudications section added after the T2 gate run:
  standardized uniqueness reading; Fisher = CV of vector lengths (overturns
  method-review §1); 14/17 published claims reproduced with three left-tail
  limits documented.
