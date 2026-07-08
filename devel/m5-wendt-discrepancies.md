# M5 note: discrepancies from Wendt et al. (2019), evaluated

**Status:** design-review note (post-T3, pre-T4). **Author:** Fable,
2026-07-07. Requested evaluation of every point where M5's SEM layer departs
from the closest published neighbor — Wendt, Wright, Pilkonis, Nolte, Fonagy,
Montague, Benecke, Krieger, & Zimmermann (2019, *J. Abnormal Psychology*,
128(8), 823–839), read in full — plus a search for other SEM-based
circumplex/SSM work. One discrepancy was found partially unjustified and
fixed same-day (§2); one earlier misattribution of their findings is
corrected here (§7). Everything else is a deliberate, defensible difference
in estimand or scope.

**Their models, precisely:** octant scores (norm-standardized) as indicators;
three factors = general distress + agency + communion; the general factor
loads **equally on all octants** and **correlates freely** with the style
factors; CFA-PC fixes style loadings to the perfect-circumplex cosine pattern
(equal spacing + equal communalities — our strict tier up to rescaling);
CFA-QC frees the style loadings' spacing and communalities; skew-t/t-CFA
variants free the latent distribution; all estimated with **MLR**, mean
structures included. Their estimand is the latent structure of the octants
and person-level factor scores. **Ours is different:** the SSM profile of an
*external measure* against the latent circumplex content — a
profile-then-transform functional of disattenuated correlations
(devel/m5-sem-design.md §2/§4). Every verdict below is relative to that
difference.

## Verdict table

| # | Design choice | Wendt et al. | M5 | Verdict |
|---|---|---|---|---|
| 1 | General-factor loadings × g–plane covariances | equal g-loadings, **free** g–style correlations | scaled: free a_i, **fixed** φ_g = 0; strict: fixed loadings, free Φ | **Justified — and mutually explanatory** (§1) |
| 2 | Estimator / test statistic | MLR throughout | was: ML + sandwich se, naive fit indices printed | **Partially unjustified → fixed** (§2) |
| 3 | Latent distribution | skew-t explored | normality-agnostic second-moment estimand + sandwich | Justified (§3) |
| 4 | Mean structure | included | covariance-only (correlation path) | Justified (§4) |
| 5 | Indicator scaling | norm-standardized | raw covariances (spec §4.4) | Compatible, no action (§5) |
| 6 | Free spacing (their CFA-QC) | offered | refused (Q5.3 → `cpm_fit()`) | Justified (§6) |

## 1. The g-loading / g-covariance trade — the two parameterizations explain each other

Their dimensional models and our scaled tier make **mirror-image**
identification choices: they constrain the general factor's loadings equal
and free its correlations with the plane; we free the per-scale general
saturations and fix the correlations at zero. T3's identification finding
(spec §3.1: the exact first-order ridge {a_i += δ·c_i·cosθ_i, φ_gx −= δ,
σ_Mx −= δ·σ_Mg} at φ_g = 0) shows why the literature's choice and ours are
the two ends of a forced trade: **you cannot free both.** Their
equality-constrained g blocks the ridge (the trade direction δ·c_i·cosθ_i is
not in the constrained parameter space), which is why their free-correlation
models are identified where our scaled tier's would not be.

Their data make the trade consequential rather than academic: the latent
g–agency correlation was **negative and replicated across all four samples**
(−.283, −.292, −.267, −.324; g–communion smaller and sample-dependent,
−.034 to +.142). So on IIP-family data the φ_g = 0 assumption our scaled
tier fixes away is *known to be violated*, at a citable magnitude of roughly
−.3 toward low agency. Consequences, all already in place:

- the violation routes into global misfit under the scaled tier
  (documented, spec §3.1/§10) — and the misfit magnitude it produces on
  real octant data is exactly what the T3 realism cell simulates; the
  coverage study showed the sandwich-vcov mvn intervals hold under it;
- the lean is *estimable* under the strict tier (free Φ), which their
  CFA-PC — the same shape — demonstrates at scale;
- **new §12 option recorded (not implemented):** an "equal-g" middle tier
  (a_i ≡ a constrained equal, c_i free with fixed directions, φ_g free) —
  their parameterization transplanted onto fixed theoretical angles. The
  ridge argument says it is counting-identified (the trade needs free a_i);
  it would restore the g-lean channel inside a mostly-scaled model at the
  cost of a tau-equivalence assumption on g. Deferred: it needs its own
  empirical local-identification check (the T3 lesson is precisely that
  counting is not enough), and T4's invariance ladder should not grow a
  third tier mid-flight.

## 2. Estimator and test statistics — the one fix

They estimated everything with MLR. Our defaults were `estimator = "ML"` +
`se = "robust.huber.white"` — the **vcov the CI engines consume is verified
bit-identical to MLR's** (max |Δ| = 0 on jz2017, coefficients identical), so
the intervals and the recorded coverage study were never affected. But the
global fit block `print()` reports was computed from the **naive** ML
chi-square, which over-rejects under the skewed distributions typical of
octant scores (jz2017: naive χ² 380.2 vs scaled 321.0 for the same fit) —
and global fit is the interpretability gate for every latent quantity (spec
§10), so a distorted default there was not defensible. **Fixed 2026-07-07:**
`ssm_sem()` defaults to `estimator = "MLR"`, and
`print.circumplex_ssm_sem()` prefers the scaled/robust fit measures
(`chisq.scaled`, `cfi.robust`, `rmsea.robust`, with graceful fallback for
plain-ML or summary-moment fits, which cannot carry them). This also aligns
the default fitting path with T4's already-pinned scaled Δχ² invariance test
(spec §6.2, Satorra–Bentler).

## 3. Latent nonnormality — justified

Their skew-t models improved fit substantially, but factor scores correlated
> .95 with the Gaussian CFA's and external prediction did not improve. For
M5 the question dissolves at the estimand level: the disattenuated profile
is a functional of **second moments only**, ML covariance-structure
estimates are consistent under nonnormality, and the sandwich vcov (now
matched to their MLR) carries the inference. Latent distribution shape
matters for person-level scoring/classification — their target, not ours.

## 4. Mean structure — justified

They needed means for LCA/hybrid comparability. Means do not enter ρ* (the
correlation-path estimand); the latent mean path is a multi-group product
that arrives with T4's invariance ladder, where mean structure is emitted.

## 5. Norm-standardization — compatible

They standardized octants against *population norms* — fixed constants, a
linear rescaling that distorts nothing. Spec §4.4's ban is on fitting
**sample-standardized** data (`std.ov = TRUE`), whose vcov is wrong for
correlation-structure quantities. No conflict; raw-covariance fitting stays.

## 6. Free spacing — justified scope boundary

Their CFA-QC frees the style loadings' spacing (angles, in our terms). M5
holds angles as fixed theoretical claims (Q5.3, binding); freely estimated
circumplex geometry is Browne's model, owned by `cpm_fit()`. Their own
results reinforce the boundary's cost-benefit: relaxing the
perfect-circumplex restrictions "did not result in consistent improvements
in terms of model fit," with "virtually identical parameter and fit
estimates" (p. 831–832).

## 7. Correction of the 2026-07-07 §3.2 spec note

The note as first committed claimed Wendt et al. *found* that relaxing
perfect-circumplex constraints improved fit without sacrificing validity.
That was **their citation of prior work** (Acton & Revelle, 2002; Gurtman &
Pincus, 2000) as motivation for including CFA-QC; their **own** result was
the opposite — no consistent QC improvement (see §6). The spec's §3.2 note
is amended accordingly: support for the scaled-tier default rests on the
prior literature they cite plus this package's own design arguments
(saturation heterogeneity as an opt-in assumption, spec §12.4), **not** on
their model comparison — which, if anything, is evidence the strict tier is
often adequate for IIP-family instruments.

## Related SEM-based circumplex work (search, 2026-07-07)

No published work was found doing the **SSM estimand at the latent level**
(disattenuated profile → circular-aware intervals) — the M5 layer appears to
be novel, and T5 should position it explicitly. The neighbors:

- **Weide, Scheuble, & Beauducel (2021, *Frontiers in Psychology*,
  12:761378):** Bayesian + ML CFA of the IIP circumplex (three factors,
  higher-level Dominance/Love/Distress scores via factor scores and weighted
  sums). Latent structure and scoring of the octants themselves — the
  Wendt-adjacent tradition, same estimand family, not SSM.
- **Moss (2026, *Applied Psychological Measurement*): "Inference for
  Disattenuated Correlations."** Directly relevant to P1's ingredient:
  corrected CIs for disattenuated correlations accounting for reliability
  sampling error, recommending latent-variable (lavaan) modeling with
  bootstrap intervals where raw data exist. Convergent with M5's
  fit-the-model-and-propagate architecture (we go further: the SSM transform
  and circular quantiles sit on top). Candidate T5 citation.
- **The Browne/CSPM tradition** (CircE; Nagy et al.'s circular stochastic
  process modeling of RIASEC interests): free-angle circumplex SEM — the
  other side of the Q5.3 boundary, already owned by `cpm_fit()`.
- **Zimmermann & Wright (2017):** the observed-score SSM inference tradition
  M5 extends; already a spec reference.

## 8. Source verification against the primary documents (2026-07-07, same day)

Jeff supplied the Wendt et al. supplements, Moss (2026), Cheung & Rensvold
(2002), and Gurtman & Pincus (2003) in full. Every secondhand claim above
was checked; two required correction, and everything else firmed up:

- **§1's equal-g inference: CONFIRMED verbatim.** Supplement R Code S25
  (CFA-PC lavaan syntax): `GF =~ L1*pa + L1*bc + ... + L1*no` — a single
  equality-labeled general loading — with style loadings `L2` and
  `L3 == 0.707*L2`, `AG ~~ 0*COM`, and free g–style covariances. The same
  `(lg)` equality label appears in the skew-t and SP-FA Mplus code, so
  **every** dimensional model they specify constrains the general factor's
  loadings equal — the ridge-blocking condition. Their CFA-QC's exact
  specification appears in neither supplement; its κ = 23 (vs PC's 20)
  cannot contain eight free general loadings, so the conclusion holds for
  QC regardless. Also noted: their spec transcribes the plane geometry as
  the 3-digit literal `0.707`; our generator's full-precision cos/sin
  emission (spec §3.5) avoids that (tiny, deliberate) geometry error.
- **Pedigree attribution: CORRECTED.** Gurtman & Pincus (2003), read
  firsthand, present **Browne's (1992) CIRCUM** (the Fourier-series
  circulant — this package's `cpm_fit()` family) as their confirmatory
  method; "equal spacing" and "equal communalities" there name CIRCUM
  variants, not a factor model. The explicit fixed-cosine three-factor
  CFA is Wendt et al.'s own construction; they borrowed the constraint
  taxonomy. Spec §3.2 amended. A unifying corollary worth keeping: at
  φ_g = 0, equal saturations, and equal spacing, CFA-PC's implied
  structure equals the m = 1 equal-ζ equally-spaced CIRCUM — the exact
  meeting point of this package's two model families — and freeing the
  g–style covariances is what breaks the circulant symmetry. (Candidate
  cheap cross-model pin for T5/M6: strict-tier fit with φ_g = 0 and
  equality-constrained saturations vs `cpm_fit(m = 1)` on the same
  moments.) Locke (2010) remains cited via Wendt et al. only
  (scoring-formula correspondence; not independently verified).
- **Moss (2026): read in full; assessment above unchanged, sharpened.**
  His corrected interval is a *summary-statistics* delta method for
  τ = ρ/√(r₁r₂); his own concluding recommendation for raw data is
  latent-variable modeling in lavaan — M5's architecture. Citable
  magnitudes for T5: Hunter–Schmidt (reliability-as-known-constant)
  coverage falls to ~0.35 (α = .60, n_a = 100, n_ρ = 5000) while his
  corrected interval holds ~0.94–0.96 — the §4.4
  standardization-uncertainty trap in miniature. Estimand caution for T5:
  his τ disattenuates **both** variables; M5's ρ* disattenuates the scale
  side only (the measure stays observed) — same family, different
  estimand, no equivalence claim.
- **Cheung & Rensvold (2002): transcribed** into
  `devel/cr2002-transcription.md`, resolving the spec's TBT item
  (§6.2/§12.2). The paper's operative sentence (p. 251) is internally
  contradictory as printed ("ΔCFI smaller than or equal to −.01 indicates
  that the null hypothesis of invariance should **not** be rejected" —
  against its own 1%-null-tail construction on p. 250); the transcription
  note records the quote, the contradiction, the operational rule
  (ΔCFI < −.01 rejects the step; ΔGamma-hat < −.001; ΔMcDonald's NCI
  < −.02), and the binding scope caveats (two groups, plain ML,
  multivariate normality, Type I error only — the criterion was never
  validated for robust indices, which matters because this package's
  default estimator is MLR). Δχ² remains T4's default verdict statistic;
  offering the ΔCFI flag as a labeled secondary criterion is now
  unblocked and remains Jeff's call at T4.

## Actions taken with this note

1. `ssm_sem()` default `estimator = "MLR"`; robust/scaled fit indices in
   `print.circumplex_ssm_sem()` with fallback (code + tests + NEWS).
2. Spec §3.2 note corrected per §7; §12 gains the equal-g open option;
   change log updated.
3. MILESTONES T5 pointer wording corrected to match.
4. No harness rerun needed: vcov bit-identity means the recorded coverage
   evidence is unchanged.
5. (Same-day, after Jeff supplied the primary sources — §8:) spec §3.2
   pedigree re-corrected against Gurtman & Pincus (2003) read firsthand;
   equal-g inference confirmed verbatim from supplement R Code S25;
   Cheung & Rensvold (2002) ΔGFI criteria transcribed to
   `devel/cr2002-transcription.md` (spec §12.2's TBT resolved, with the
   source's internal contradiction documented); Moss (2026) assessed in
   full with citable magnitudes and the both-sides vs scale-side
   disattenuation estimand caution for T5.
