# zimmermann2017 — SSM estimator accuracy: the vignette's sample-size guidance

**Provenance.** Ingested 2026-07-19 by M40 from
`cairn/references/sources/zimmermann2017.pdf` (gitignored).
Pagination: SAGE OnlineFirst pages 1–21 (the shelf PDF is 21 pages, so PDF
page *n* is printed page *n*); the journal's own pagination is *Assessment,
24*(1), 3–23.
Extraction: verified 2026-07-19 against the primary source by Jeff's complete second human re-read (M7 AC3, which found no value in the record wrong), and independently re-checked during M40 by `pdftotext -layout` across the Note 3 matrices, both Table 4 rows and Table 4's own note, the Study 2 thresholds, the Study 5 fit indices and IIP-SC parameters, and all eight published constants that Eq. A6/A7/Eq. 3 derive; three items lie outside a text layer's reach and rest on the human read alone — Figure 1A's octant angles, Figure 5's panel readings, and Eq. A7's √2 radicand and leading ½, which the text layer silently drops — observed 2026-07-19.

**Citation.** Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
interpersonal construct validation: Methodological advances in the circumplex
Structural Summary Approach. *Assessment, 24*(1), 3–23.
doi:10.1177/1073191115621795

**Role.** The source of the package's sample-size and CI-accuracy guidance —
`vignettes/evaluating-circumplex-structure.Rmd` ships an accuracy table and
prose bullets drawn from Studies 1–3, and `jz2017` is this paper's Study 5
sample. It is also the basis of `ssm_ci_accuracy()`'s framing.

**Scope warning carried from M7 T3.** Every threshold below is about
**95% percentile bootstrap CI coverage accuracy** — not point estimates, and
not CIs generally. The vignette's accuracy-table header was rescoped at M7 to
say *bootstrap* CI for exactly this reason.

## Extracted values

### Population octant matrices — Note 3, p. 18

Model-based ("reproduced") correlations from SEM analyses of real instruments.

- **Without** a substantial general factor (IAS, 2,988 students; Gurtman &
  Pincus, 2000; Wiggins, 1995): ρ₁ = .430, ρ₂ = .030, ρ₃ = −.360, ρ₄ = −.740.
- **With** a substantial general factor (IIP-C, 1,981 students; Gurtman &
  Balakrishnan, 1998): ρ₁ = .683, ρ₂ = .500, ρ₃ = .345, ρ₄ = .288.

### Scaling factors — Appendix, p. 18

- Eq. A6: f_e = √((2ρ₁ + 2ρ₂ + 2ρ₃ + ρ₄ + 1) / 8)
- Eq. A7: f_a = ½ · √(√2(ρ₁ − ρ₃) + (1 − ρ₄))

**Eq. A7's text layer is corrupt** — `pdftotext` renders the radicand as
`2(ρ1−ρ3)+(1−ρ4)`, dropping both the √2 and the leading ½. The M40 re-check
reproduced this artifact, so the machine channel cannot confirm the equation's
form; it rests on Jeff's 2026-07-19 page read, which confirmed both.

Published values the equations must reproduce: f_e = .737 (IIP-C) and .240
(IAS), both p. 9; f_a = .545 (IIP-C) and .845 (IAS), p. 9; f_a = .625
(IIP-SC), p. 14.

- Eq. 3, p. 12: |AFF_min| = 2.95 · f_a · n^(−0.587) (log-log r = −.994).

**Numeric consistency check (M40, reproduced independently):** the transcribed
ρ matrices with Eq. A6/A7/Eq. 3 give f_e = .7369 / .2398, f_a = .5454 / .8452
/ .6246, and Eq. 3 values .1077, .0279, .0292 — matching all eight published
constants; the no-√2 variant gives .5891 / .9110 / .6749 and misses every one.
**This is a consistency check, not an independent oracle** — the ρ values and
the equations come from the same transcription pass, so compensating errors
would still close it.

### Study 2 — bootstrap CI coverage thresholds, pp. 9–11

95% percentile bootstrap CIs from 2,000 replicates; accuracy = empirical
coverage inside Bradley's (1978) liberal criterion [92.5%, 97.5%].

- Elevation, affiliation, dominance: accurate from **n ≥ 50**.
- Amplitude: accurate from **n ≥ 75 with** a general factor, **n ≥ 150
  without**.
- Angular displacement: accurate from **n ≥ 100 with** a general factor,
  **n > 200 without**.
- Goodness of fit: coverage 0 when R² = 1 (boundary); accurate only when
  population R² < .9 — the bootstrap "seems unsuited" for goodness-of-fit CIs
  (p. 11).
- All amplitude/displacement recommendations presume A ≥ .1 (Discussion,
  p. 10).

**The paper states these thresholds twice, with different numbers.** Study 2
*Results* (p. 10) gives amplitude 75/150 and displacement 100/200; the Study 2
*Discussion*, same page, summarizes **both** parameters together as 100/200.
Figure 5 (p. 12) supports the Results, so the Discussion's figure is
displacement's thresholds applied to both — a conservative simplification, not
a competing measurement. The vignette follows the Results (75/150), a choice
ratified at M7 T3 rather than left implicit. The Figure 5 reading is Jeff's;
no per-condition deviances were published against which to check it.

### Study 3 — the accuracy frontier, pp. 11–13

Interpret a/δ CIs only when the "probability of accurate CIs" is ≥ .50; fully
trustworthy ≥ .95 (pp. 12–13, 16).

### Study 5 — real data, pp. 13–16

- N = 1,166 undergraduates; IIP-SC octants, PDQ-4+ PD scales as targets.
- IIP-SC CircE fit: equal spacing + equal communality rejected — CFI = .824,
  TLI = .795, RMSEA = .169; unequal spacing acceptable — CFI = .958,
  TLI = .931, RMSEA = .098 (p. 14).
- Model-based IIP-SC parameters: ρ₁ = .580, ρ₂ = .323, ρ₃ = .134, ρ₄ = .070
  → f_a = .625; at N = 1,166, AFF or DOM > |.029| suffices (p. 14).

**Table 4, p. 15** — the two rows the vignette uses:

| PD scale | e | aff | dom | a | δ | R² | Prob |
|---|---|---|---|---|---|---|---|
| Paranoid | .250 [.218, .280] | −.094 [−.129, −.060] | .117 [.080, .152] | .150 [.115, .189] | 128.9° [116.7°, 141.6°] | .802 | 1 |
| Obsessive–compulsive | .228 [.193, .261] | .011 [−.021, .041] | −.005 [−.038, .032] | .012 | 337.4° | .117 | .130 |

OCPD's amplitude and displacement CIs are **not printed**, and this is the
authors' deliberate omission. Table 4's own note reads: "Prob = probability of
accurate confidence intervals for amplitude and angular displacement" — the
two parameters withheld — and the authors' rule is not to interpret a/δ CIs
when that estimate is < .50 (pp. 12–13, 16). **The trigger is the low
Prob (.130), not the low R² (.117)**; R² is a separate quantity and governs
nothing about which intervals print. (M7 T3 first recorded R² here and
corrected it the same day; the M40 re-check confirms the correction against
Table 4's note.)

### Study 1 — bias, pp. 6–9

- Bias in e/dom/aff very small (average −.0007, most extreme −.013).
- Amplitude bias substantial and consistently positive: average relative bias
  **15.5%**, range 0–135.8%; at n = 50 without a general factor, E(â) = .153
  when A = 0 (p. 6).
- Displacement bias trivial (avg −.03°); goodness of fit underestimated
  (avg −.081, relative −9.5%).

**The value 15.5% appears twice in this paper for two different quantities** —
Study 1's average relative amplitude bias (p. 6) and Study 4's mean deviance
when AFF₂ = 0 (p. 13). Confirmed a genuine coincidence, not a paste error.

### Octant angles — Figure 1A, p. 3

LM = 0°, NO = 45°, PA = 90°, BC = 135°, DE = 180°, FG = 225°, HI = 270°,
JK = 315°. Figure content: not text-layer accessible, so this rests on Jeff's
read (M7 T3 section B5), not the M40 machine channel.

## Traces to

- `vignettes/evaluating-circumplex-structure.Rmd:190-199` — the accuracy table
  (its header rescoped to "95% bootstrap CI accurate when…" at M7 T3).
- `vignettes/evaluating-circumplex-structure.Rmd:203-222` — prose bullets
  carrying six shipped numbers, including one of the two 15.5% values.
- `vignettes/evaluating-circumplex-structure.Rmd:237-238` — OCPD's amplitude
  .012 as the cautionary near-flat case.
- `vignettes/evaluating-circumplex-structure.Rmd:151-157,572-573` — Study 5
  CircE fit indices; PARPD elevation .250.
- `devel/m4-zw-transcription.md` — the M4/W1 transcription record this page
  supersedes as the citable source; it retains the two-channel protocol
  history and the O5 bridge re-scope, which this page does not restate.
- `devel/m4-zw-bridge.R` — the seeded O5 bridge run built on these anchors.

## Open questions

- The article has **no supplemental materials** (Europe PMC PMID 26685192
  records `hasSuppl: no`; the SAGE endpoint returns none), so per-condition
  coverage values were never published — the published record is aggregate
  mean absolute deviances, Bradley-band classifications, threshold statements,
  and the Eq. 3 frontier. Any oracle wanting per-condition coverage must
  reconstruct it, not look it up — observed 2026-07-19.
- Eq. A7's form has never been confirmed by two *independent* channels: the
  text layer is corrupt, and the numeric cross-check shares a transcription
  pass with the values it checks. A second human read, or a clean text layer
  from a different rendering of the PDF, would close it — observed 2026-07-19.
