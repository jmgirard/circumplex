# M5 spec: SEM-based SSM — latent-variable estimation and invariance-gated contrasts

**Status:** design (M5 T1). Not implementation.
**Author:** Fable, 2026-07-07. Turns Brief E §M5 (`devel/m5-m6-design-questions.md`,
Q5.1/Q5.2/Q5.3) into an implementation spec, the way Brief B
(`devel/m4-ci-accuracy-spec.md`) did for `ssm_ci_accuracy()`. Brief E's
recommended directions are taken as the starting point; where this spec
refines them (notably §5.2's per-parameter table and §4.4's
correlation-as-covariance trap), the refinement is flagged inline.
**Downstream tasks:** every M5 task traces to a section here — see §11's
traceability table. No code ships from this document.
**Revised 2026-07-07 against the fresh-session review
(`devel/m5-sem-design-review.md`, verdict ACCEPT WITH CHANGES): all
findings F1–F12 resolved; see the "Revision log (vs the fresh-session
review)" section near the end.**

> ⚠️ **Oracle rule (inherited from Brief A §6.1, binding here):** no expected
> numerical value enters code or tests from memory. Published thresholds this
> spec needs but does not fix — invariance-decision cutoffs (e.g., ΔCFI),
> any reproduced lavaan example values — are marked **TBT** (to be
> transcribed) and arrive only by transcription from the cited source at
> implementation time, blank templates first. `devel/g2xx1.txt` remains
> banned. The only numeric conventions fixed here are package conventions
> already in force and indicative degrees-of-freedom counts explicitly
> labeled as such (§3.4), which must be re-derived by `lavaan` at T2.

Package conventions binding everything here (see also the review-revision
change-log entries): angles degrees [0, 360) in the
user API with LM = 360, radians internally; contrasts second minus first in
(−180°, 180°] via `angle_dist()`; displacement CIs via circular quantiles
(`quantile.circumplex_radian()`; contrast intervals branch-aligned to the
estimate by `ssm_replicate_intervals()`); closed-form SSM estimator = OLS
only for equally spaced angles; statistical correctness outranks everything.

---

## 1. Purpose and scope

### 1.1 The two products

**P1 — Latent-variable SSM (T2 + T3).** For one or more external measures,
estimate the SSM profile the measure would show against the *latent*
circumplex content of the scales — the disattenuated analog of the existing
correlation-based SSM — from a fixed-theoretical-angle lavaan measurement
model, with circular-aware CIs constructed in-package.

**P2 — Invariance-gated latent contrasts (T4).** A separately named
multi-group workflow: test an invariance sequence on the measurement model,
and only under sufficient invariance compute group contrasts on *latent* SSM
parameters — a different estimand from the existing observed contrast, shipped
alongside it, never replacing it (Brief E Q5.2).

### 1.2 Scope boundary with the CPM (Q5.3, binding)

Angles are **fixed theoretical constants** everywhere in M5. Freely estimated
angles under circumplex constraints are Browne's model, which `cpm_fit()`
owns; no M5 function estimates an angle, and the syntax generator hard-fails
if asked to free one. A hybrid workflow ("estimate angles via `cpm_fit()`,
then a fixed-angle SEM at those angles") is mechanically trivial under this
spec (the generator takes an `angles` vector, wherever it came from) but is
**not documented as a supported workflow in M5**: its inferential status
(angles treated as known when they were estimated on the same data) is
exactly the pretend-fixed problem this package elsewhere refuses to hide, so
it waits for its own design note. The generator does not accept a
`circumplex_cpm` object in M5.

### 1.3 Out of scope for M5 (recorded so their absence is a decision)

- **Single-group mean-based latent SSM.** With free intercepts, factor means
  are not identified in one group; with ν = 0 they merely re-weight the
  observed means (measurement error does not bias means, so there is no
  disattenuation payoff). Latent *mean-structure* SSM is a multi-group
  product only (§6). The single-group product is the correlation path (P1).
- **Latent external measures** (measures with their own indicators). v1
  treats each measure as a single observed variable, matching
  `ssm_analyze()`'s contract. The estimand map (§4) extends to a latent
  measure without redesign; deferred, not precluded.
- **Partial invariance** (freeing selected intercepts/saturations). Deferred
  per Brief E Q5.2 until T5's vignette is drafted against real data; the
  low-level API (§7.2) is the escape hatch: a user can modify the emitted
  syntax, fit it themselves, and hand the fit back for estimation.
- **Item-level measurement models** (per-scale factors over items). The
  honest full-disattenuation design, but a different data contract
  (item-level input) and a much larger model; recorded as a possible M6+/
  post-v2.0.0 direction, not attempted here.

---

## 2. The estimand: profile-then-transform (the load-bearing decision)

Latent SSM parameters are **not** defined as raw lavaan parameters or `:=`
quantities. They are defined by the same two-stage functional the package
uses everywhere:

1. a **profile vector** — here model-implied and disattenuated (§4) or
   latent-mean (§6) — evaluated at the p theoretical angles;
2. the **SSM transform** of that profile: (e, x, y) by linear weights (§2.1),
   then a = √(x² + y²), d = atan2(y, x) mapped to [0°, 360°), fit = R².

Why this is load-bearing:

- **Pipeline reuse becomes literal.** Draws of the model's free parameters
  map to draws of the profile, which push through the existing transform and
  `ssm_replicate_intervals()` + circular-quantile machinery unchanged
  (Brief E Q5.1's recommended direction). No interval logic is re-derived.
- **The estimand is model-shape-robust.** T3's estimation layer depends only
  on "a fitted model that implies a profile vector and a vcov", not on which
  §3 model tier produced it — T2 and T3 stay decoupled.
- **Observed and latent results are commensurable.** The same functional
  applied to the observed profile vs. the disattenuated profile is an
  apples-to-apples comparison the vignette can teach; differences are
  attributable to the profile, not to a different estimator.

### 2.1 The profile → (e, x, y) weights: OLS, with the equal-spacing identity

The linear stage uses the **ordinary-least-squares projection** onto the
cosine basis at the analysis angles: with B the p × 3 matrix with rows
(1, cos θ_i, sin θ_i),

    (e, x, y)ᵀ = W · profile ,   W = (BᵀB)⁻¹Bᵀ .

Facts this rests on (each becomes a T2/T3 test):

- **For equally spaced angles W is exactly the closed form** — B's columns
  are orthogonal (Σcos = Σsin = Σcos·sin = 0, Σcos² = Σsin² = p/2), so
  W = diag(1/p, 2/p, 2/p)Bᵀ, i.e., e = mean(profile),
  x = (2/p)Σ profile·cos θ, y = (2/p)Σ profile·sin θ — identical to
  `ssm_parameters_cpp()`. All shipped instruments are equally spaced, so for
  every stock analysis the SEM layer and `ssm_analyze()` share one functional
  exactly.
- **W is a true left inverse of B at any full-rank angle set; the closed
  form is one only under a balance condition.** An exactly-cosine profile is
  recovered *exactly* by W always. The closed form coincides with W exactly
  when the angle set satisfies **first- and second-harmonic balance**:
  Σcos θ_i = Σsin θ_i = 0 and Σcos 2θ_i = Σsin 2θ_i = 0 (equivalently
  BᵀB = diag(p, p/2, p/2)). Equal spacing (p ≥ 3) implies the condition —
  hence the CLAUDE.md invariant's safe direction — but is **not necessary**:
  structured unequal sets can satisfy it too (verified in review: two
  interleaved rotated 4-grids {0°, 90°, 180°, 270°, 10°, 100°, 190°, 280°}
  give max |W − closed form| ≈ 8e−17 despite 10°/80° alternating gaps).
  Generic irregular sets violate it and the two functionals genuinely differ
  there. Inside a model whose hypothesis *is* the cosine structure, the
  projection functional is the only coherent choice — Brief E's "trap to
  write down now", resolved as T2's acceptance criteria already commit.
- **Documented divergence:** at angle sets violating the balance condition,
  the latent SSM functional (OLS) differs from `ssm_analyze()`'s closed form
  (conventional Gurtman estimator). This is stated in the T3 docs and the T5
  vignette, not hidden: the SEM layer is model-based and projects; the
  observed layer keeps its field-conventional estimator. Under OLS the fit
  parameter is a genuine bounded R² in [0, 1] at any spacing (unlike the
  closed form's documented out-of-range fit off equal spacing). Note for
  §6.4: only the *strict* tier implies exactly-cosine latent mean profiles;
  under the scaled tier the per-scale saturations take the profile out of
  span{1, cos θ, sin θ}, so no exact-recovery claim is available there
  (review finding, numerically verified).
- Degenerate B (rank < 3 — e.g., fewer than 3 distinct angles, or angles
  spanning a line) is refused with a clear error at syntax-generation time.

Nonlinear stage: unchanged from the package (same a, d, fit definitions; d
degenerate-NA semantics inherited via the same tolerance philosophy —
though see §5.5: the latent profile is a smooth function of estimates, so
degeneracy handling mostly matters for *draws*, not the point estimate).

### 2.2 What is never delegated to lavaan (Q5.1, binding)

lavaan supplies estimates and a covariance (or bootstrap replicates) of its
free parameters — nothing else. Specifically **never** consumed:

- lavaan's delta-method SE or Wald CI for any `:=` amplitude or displacement
  (the atan2 branch problem: the value and interval live on whatever branch
  atan2 returned; a displacement near 0°/360° gets an unwrapped, possibly
  sign-flipped interval);
- lavaan's bootstrap **percentile CI** for a `:=` displacement (naive
  quantiles of replicates straddling the cut → the classic wrong-way
  interval);
- lavaan's `:=` machinery for the displacement *contrast* (a difference of
  two atan2's — the same trap squared, per Q5.2).

The emitted syntax therefore **never contains** `ampl :=` or `disp :=` lines
(§3.5). Interval construction for a and d happens in-package through the
replicate pipeline (§5).

---

## 3. The measurement model (T2's deliverable)

### 3.1 Canonical shape: the scaled fixed-angle circumplex CFA (`model = "scaled"`)

For scales s_1…s_p at fixed theoretical angles θ_1…θ_p, three latent
factors — g (general/elevation content) and the plane axes (cx, cy):

    s_i = ν_i + a_i·g + c_i·cos(θ_i)·cx + c_i·sin(θ_i)·cy + ε_i

- **Free:** general saturations a_i, circumplex saturations c_i (per scale),
  residual variances θ_i, intercepts ν_i (mean structure only when needed —
  multi-group, §6).
- **Fixed:** the angles (inside cos/sin — constants in the syntax, never
  parameters); the factor metric: var(g) = var(cx) = var(cy) = 1,
  cov(cx, cy) = 0. Only the *overall plane scale* is a pure convention
  (absorbed by the c_i). **Isotropy and orthogonality of (cx, cy) are
  substantive restrictions** — a single c_i scales the cos and sin loadings
  together, so it cannot absorb anisotropic latent dispersion (verified in
  review: an elliptical plane Φ = diag(2, 1) implies between-scale
  covariances at 45°/135° that no choice of c_i reproduces under the
  isotropic metric). This is the latent-plane **stationarity assumption**:
  stated in the docs as an assumption, detectable only through global
  misfit, and the premise of §4.3's direction-integrity property (which is
  therefore conditional, not guaranteed by construction). A free-plane
  variant is deliberately not offered in v1: freeing Φ_plane reintroduces
  exactly the direction distortion §4.3 exists to prevent, and its
  interaction with free c_i needs its own identification analysis.
- **g–plane covariances** cov(g, cx), cov(g, cy): **fixed to 0** (amended
  2026-07-07 at T3; originally "free by default" pending the empirical
  identification check this spec required). T3's check **failed, and not at
  small p**: with free per-scale saturations, the perturbation
  {a_i += δ·c_i·cosθ_i, φ_gx −= δ, σ_Mx −= δ·σ_Mg} changes every model-implied
  moment only at O(δ·φ + δ²), so the Jacobian is exactly rank-deficient at
  φ_g = 0 — the natural null point — and near-singular in its neighborhood
  (verified analytically and numerically: nlminb stalls, SEs of the a_i
  reach ~200, and MVN draws run down the flat ridge into inadmissible
  parameter space; a second ridge pairs φ_gy with the sinθ pattern). Because
  the singularity sits at the null itself rather than at unrealistic p, a
  documented `free_g_plane` switch is **not offered**: it would hand users a
  model that is unidentified exactly when their data look ordinary. The
  estimand map is ridge-invariant (profile-then-transform absorbs the trade,
  the §3.4 robustness property again), so point estimates were never wrong —
  but no interval engine can survive an information-singular parameterization.
  A general factor leaning into the plane remains expressible under the
  **strict** tier, whose fixed loadings remove the trade dimension and leave
  the full Φ free; the scaled tier's docs state g ⊥ plane as an assumption,
  exactly like plane isotropy. §4.2's second latent-fit channel is therefore
  strict-tier-only as an estimated quantity; under the scaled tier a true
  g-lean surfaces as misfit (a §8.3 misspecification cell, pseudo-true
  target).

Rationale over the literal `model0` of `devel/circum_lavaan.Rmd`: fixing
all loadings to a common saturation scaling of the unit cosine pattern
(model0's .71·(1, cos θ_i, sin θ_i)) imposes equal circumplex
saturation across scales — the tau-equivalence-style constraint that
predictably misfits real instruments, pushing reliability differences into
misfit that T3's CIs would inherit. Freeing (a_i, c_i) keeps the *angles*
theoretical (the Q5.3 boundary) while letting saturation be empirical — and
it is exactly what makes a metric-invariance analog testable in T4 (§6.2).

### 3.2 Constrained shape: the strict model (`model = "strict"`)

the unit-scaled parameterization of model0: loadings fixed to
(1, cos θ_i, sin θ_i) with the 3 × 3 factor covariance Φ free (6
parameters), residual variances free. This is equivalent to
`circum_lavaan.Rmd`'s literal model0 (which fixes .71·(1, cos θ_i, sin θ_i))
only up to rescaling of the free Φ (loadings differ by .71, factor
(co)variances by ≈ 1/.71²) — no test or vignette passage may pin numerical
equality of parameters between the two, and the .71 literal must never be
transcribed into the generator. Offered because
(a) it is the fully-theoretical benchmark the vignette contrasts against,
and (b) at small p the scaled model is not identified (§3.4) and strict is
what remains. Its equal-saturation assumption is stated in its
documentation, not discovered by users.

**Published pedigree (added post-T3, informational; source-verified
2026-07-07 against the primary documents — see
devel/m5-wendt-discrepancies.md §8).** The explicit three-factor CFA with
fixed unit-cosine plane loadings — this tier's shape — is **Wendt et
al.'s (2019) CFA-PC**, whose exact lavaan specification (their supplement,
R Code S25: one equality-labeled general loading `L1` on all eight
octants; style loadings `L2` with off-axis `L3 == 0.707*L2`;
`AG ~~ 0*COM`; free g–style covariances) matches the strict tier up to
rescaling, up to their 3-digit `0.707` literal (this generator emits exact
cos/sin at full double precision instead), and up to the strict tier
additionally freeing the plane block of Φ. Wendt et al. attribute the
"perfect circumplex" constraint language to Gurtman & Pincus (2003), but
that chapter's confirmatory model is **Browne's (1992) CIRCUM** — the
Fourier-series circulant, this package's `cpm_fit()` family — where "equal
spacing" and "equal communalities" name CIRCUM variants, not a factor
parameterization. The connection is nonetheless real and unifying: at
φ_g = 0, equal saturations, and equally spaced angles, CFA-PC's implied
structure **is** the m = 1 equal-ζ equally-spaced circumplex (β₀ ↔ the
general variance, β₁ ↔ the plane variance) — the point where this
package's two model families (fixed-angle SEM tier and `cpm_fit()`) meet —
and the free g–style covariances are exactly what breaks the circulant
symmetry (Σ_ij gains cosθ_i + cosθ_j terms). Locke (2010) is cited by
Wendt et al. for the corresponding IPC scoring formulas (secondhand here;
not independently verified). Their CFA-QC is a mild relaxation of CFA-PC
(κ 20 → 23; its exact specification is not in their supplements), **not**
a per-scale free-saturation model like this spec's scaled tier. Across
four large clinical/community samples (N = 5,400/491/656/712), the fully
dimensional models won on fit, prediction, robustness, and parsimony
against categorical/hybrid alternatives throughout. Two results are directly useful for T5's docs: (a) CFA-PC's
measured real-data fit (CFI .938–.957, RMSEA .075–.111 across samples) is
published evidence that the fixed-loading, fixed-angle model is a real
but imperfect approximation — exactly the model-conditional caveat §10
states, now with a citable magnitude; (b) their latent general–agency
correlation was negative and replicated across all four samples (−.267 to
−.324), so the g ⊥ plane assumption the scaled tier fixes for
identification is known to be violated on IIP-family data at a citable
magnitude — the strict tier (or the misfit channel) carries it, and the
T3 realism cell's coverage verdict covers exactly this regime. *(Corrected
2026-07-07, same day: point (b) originally credited them with finding that
relaxing the perfect-circumplex constraints improves fit without losing
validity — that was their citation of PRIOR work (Acton & Revelle, 2002;
Gurtman & Pincus, 2000); their own CFA-QC showed no consistent improvement
and "virtually identical" estimates. See
devel/m5-wendt-discrepancies.md §7.)* They estimated
throughout with MLR (robust SEs under nonnormality) — the same
misspecification-consistent-covariance logic behind this spec's §5.1
sandwich-vcov decision, arrived at independently for a different
inferential target (their factor-score SEs, not ours). Not a validation
oracle for M5 (they model the octants' latent structure per se — persons'
factor scores on severity/agency/communion — not an external measure's
disattenuated SSM profile, a different estimand), so no numeric value is
taken from it; cited for context and the strict-tier correspondence only.

### 3.3 The measure block (correlation path, P1)

Each external measure M_k enters as an observed variable covarying freely
with (g, cx, cy) and **with all measure–residual covariances fixed to 0**
(cov(M_k, ε_i) = 0 — M relates to scales only through their common
circumplex content; this is the identifying assumption that makes
disattenuation mean something, stated in docs). Multiple measures covary
freely with each other. The model is fitted to **raw covariances** (never
`std.ov = TRUE`; §4.4), with measures and scales on their observed metric.

### 3.4 Identification and size gates (indicative counts — T2 re-derives)

Indicative single-group covariance-structure counting at p = 8 (octant
instruments), no measures — **amended 2026-07-07 at T3 with the §3.1
g–plane flip (original text counted 2p + 2 + p = 26 → df ≈ 10 with free
g–plane covariances)**: 36 observed moments; scaled model frees
3p = 24 (→ df = 12); strict frees 6 + p = 14 (→ df = 22). At p = 4
the scaled model's count exceeds the moments (12 > 10): **not identified**;
p = 5 is just-identified (15 = 15, df = 0), so the counting gate is p ≥ 5
(pinned on both sides by the T2 tests). These counts are order-of-magnitude
guides only — the exact df is derived from `lavaan` in the T2 tests, and
the empirical local-identification check ran at T3 (finding the g–plane
flip recorded in §3.1/§12.3; lavaan's warnings surfaced, not passed through
silently). One footnote for that check (review F12): the scaled tier has
two discrete **reflection indeterminacies** (g → −g with a_i, σ_Mg
negated; (cx, cy) → −(cx, cy) with c_i, σ_Mx, σ_My negated). Both leave
the §4.1 estimand map exactly invariant (all products preserved — so a
`"boot"` replicate converging to the opposite reflection still yields the
identical ρ*⁽ᵇ⁾, a robustness property of profile-then-transform worth a
line in T3's docs), and discrete indeterminacy does not reduce
information-matrix rank, so the identification check must not misread a
sign flip as a failure.

### 3.5 Emitted syntax contract

The generator (working name `ssm_sem_syntax()`, §7) emits:

1. the measurement block per §3.1/§3.2 — angles appear only as evaluated
   cos/sin constants, formatted at full double precision; multi-group
   requests (`invariance =`) additionally emit §6.2's pinned per-group
   factor metric (non-reference groups: free var(g_g), free g–plane
   covariances, single isotropic plane scale φ_g);
2. the measure block per §3.3;
3. **inspection-only `:=` lines for e, x, y**, controlled by
   `include_defined` with default `NULL` = auto: emitted under `"strict"`
   (where the covariance-metric profile is linear in free parameters),
   omitted under `"scaled"` (products of free parameters make them
   nonlinear clutter). These are **covariance-metric quantities** and are
   named accordingly (`cov_e`/`cov_x`/`cov_y`, review F10) — they are *not*
   the reported latent SSM parameters, which transform the
   correlation-metric ρ* (§4.1), so their *values*, not merely their SEs,
   differ from the printed output; the generated comment says exactly that
   (inspection values on the covariance metric; SEs are lavaan delta
   approximations; the reported parameters and their intervals come from
   the package). The OLS weight matrix itself is **always** returned as an
   attribute regardless (see the weights invariant below);
4. **never** `ampl :=` / `disp :=` (§2.2) — with a generated comment saying
   so and pointing at the R estimation function;
5. a header comment recording instrument, angles (degrees), model tier, and
   generator version.

**The weights invariant (T2 acceptance, restated).** The generator always
returns the §2.1 OLS weight matrix W — evaluated at the instrument's actual
angles — as an attribute of its output (§7.1), and that attribute is what
the estimation layer consumes; the inspection `:=` lines, when emitted,
carry the same weights. The T2 test therefore targets **the attribute
always, plus the `:=` lines whenever emitted**, and asserts: equally spaced
instrument → weights equal the closed-form (2/p)·cos weights to machine
precision; unequally spaced instrument → they equal `(BᵀB)⁻¹Bᵀ` and
*differ* from the closed form — where the test's unequal-spacing arm must
use an angle set that **violates §2.1's harmonic-balance condition** (e.g.,
{0°, 30°, 90°, 200°, 290°}, verified in review to give max weight
difference ≈ 0.18); structured unequal sets that satisfy the balance
condition coincide with the closed form and would make the "differs"
assertion spuriously fail. Hard-coded octant constants (the
`devel/lavaan_ssm.Rmd` 0.25/±0.7071068 literals) must be impossible by
construction — weights are computed from the `angles` argument, never
tabulated.

**Fit-under-lavaan check (T2 acceptance, from MILESTONES):** a T2 test fits
the emitted syntax with `lavaan` on a reference instrument and real data
(octant scales of `jz2017`, both tiers, with and without a measure) and
asserts clean convergence and the expected free-parameter names —
`skip_if_not_installed("lavaan")`. Syntax that only ever exists as a string
is not accepted machinery.

Input: a `circumplex_instrument` (angles from `$Scales$Angle`, names from
`$Scales$Abbrev`) or explicit `scales`/`angles` vectors, mirroring
`ssm_analyze()`'s flexibility. Instrument norms/items are not consumed.

---

## 4. The latent profile (P1's estimand, precisely)

### 4.1 Definition

Let t_i = a_i·g + c_i·(cos θ_i·cx + sin θ_i·cy) be scale i's common
(circumplex) part under §3.1 (under §3.2, the fixed-loading equivalent). The
**latent profile of measure M** is the model-implied disattenuated
correlation vector

    ρ*_i = Cov(M, t_i) / ( SD(M) · SD(t_i) ) ,   i = 1…p,

with every ingredient (the factor covariances σ_Mg, σ_Mx, σ_My; Var(t_i)
from the loadings and factor metric; Var(M) **model-implied** — pinned, per
review F7: it equals the saturated-block estimate at the optimum for
complete data, and under `missing = "fiml"` an "observed" variance is not
even well-defined) evaluated from the fitted model. The latent SSM parameters of M are the §2 transform of ρ*.

This is the exact latent analog of the observed correlation-based SSM
(`ssm_analyze(..., measures = )`): same profile concept, same per-scale
standardization, with the scale's error-and-unique part removed from the
denominator and the covariance restricted to common content in the
numerator. ROADMAP's "SSM on disattenuated correlations", made precise.

### 4.2 What the fit parameter now means (vignette-critical)

Under §3.1's isotropic factor metric, Cov(M, t_i)/SD(M) is *not* forced
cosine (a_i, c_i vary by scale), and SD(t_i) varies by scale — so ρ* is not
exactly cosine and **fit < 1 is informative**: as an *estimand*, the latent
fit measures how far the measure's latent profile departs from a pure
cosine wave. Two channels feed that departure (review F2, both verified):
**differential saturation** of the scales, and **Var(t_i) modulation from
the g–plane covariances** — Var(t_i) carries a first harmonic whenever the
g factor leans into the plane, so a user with perfectly uniform saturations
can still see latent fit < 1 (probe: equal saturations, φ_gx = .4,
φ_gy = .2 → fit = .988); under the strict tier, anisotropic Φ̂ is a third
channel. (Amended 2026-07-07 at T3: the g-lean channel is expressible as an
*estimated* quantity under the **strict tier only** — the scaled tier now
fixes φ_g = 0 for identification, §3.1 — so under the scaled tier a true
g-lean feeds misfit, not the fitted latent fit.) What the latent fit removes is the
attenuation-heterogeneity component that contaminates the *observed*
profile's fit — that removal, not "removing sampling error", is the
improvement (both estimands are population quantities and contain no
sampling error). As an *estimate* at finite n, the latent fit is a function
of noisy parameter estimates and still reflects sampling noise, exactly
like the observed fit — a latent fit of, say, .85 in a modest sample is not
automatically substantive structure. The vignette must teach this changed
meaning, with both halves of the caveat, explicitly (T5 acceptance: "every
reported CI/estimand claim traces to T1's spec"; this section is the trace
target).

### 4.3 Direction integrity

What §3.1's fixed isotropic metric buys is precisely this: **the transform
adds no axis-anisotropic distortion of its own.** Designs that standardize
x and y by different quantities (e.g., x* = cor(M, cx), y* = cor(M, cy)
under a free anisotropic Φ) distort direction whenever var(cx) ≠ var(cy);
that was considered and rejected — recorded here so it is not re-invented.

What the metric does **not** buy (review F1, numerically verified —
stationarity is necessary, not sufficient): d* is the **first-harmonic
direction of the saturation-modulated disattenuated profile**, ρ*_i =
w_i·cos(θ_i − δ) with effective weights w_i = c_i/SD(t_i), and it equals
the measure's factor-space angle δ = atan2(σ_My, σ_Mx) only when the
effective saturations are homogeneous around the circle **and** the
g–plane covariances are zero. A second-harmonic pattern in the saturations
beats against the first harmonic and rotates d* (verified probe: octant
angles, exact stationarity, δ = 20°, c_i = 0.5 + 0.3·cos 2θ_i → d* =
13.6° with fit = .982 — above the .70 guardrail, so nothing downstream
flags it; the rotation's direction depends on the saturation pattern's
phase, so it does not cancel). This is not a defect of the estimand — the
*observed* d has the same property in worse form, since ρ_i = ρ*_i·√rel_i
adds reliability modulation on top; **the latent layer's improvement is
exactly the removal of the reliability modulation**, and that — not "the
latent angle of the measure in factor space" — is what T3's docs and T5's
vignette teach d* to be. Documented in §10; one §8.1 analytic cell uses
heterogeneous saturations so the caveat carries a demonstrated,
design-time-computed magnitude.

### 4.4 The correlation-as-covariance trap (refinement of Brief E)

`devel/lavaan_ssm.Rmd` fits standardized data (`std.ov = TRUE`). That is
**banned for inference** in this design: fitting a correlation matrix as if
it were a covariance matrix makes the parameter vcov wrong for
correlation-structure quantities (the classic Cudeck-style analysis of
correlation matrices in covariance-structure software — citation to be
completed at implementation; the *direction* of the problem, understated
SEs for standardization-dependent quantities, is what matters here). The
design instead fits **raw covariances** and performs all standardization
*inside the estimand map* (§4.1's ratios), so the §5 draws propagate
standardization uncertainty correctly. The observed-SSM parallel: the
bootstrap resamples people (standardization uncertainty included); the Monte
Carlo engine uses the influence-function acov of correlations — the SEM
layer must not be the one path that quietly drops that uncertainty.

### 4.5 Guards

- Any |ρ*_i| ≥ 1 − 1e−12 in the *point* profile is refused with a clear
  error naming the offending scale (disattenuated correlations exceeding 1
  signal model misspecification, not a profile to summarize) — mirroring
  `ssm_montecarlo()`'s refusal.
- In *draws*, an inadmissible replicate (Var(t_i) ≤ 0 or Var(M) ≤ 0 from
  an MVN excursion — Var(M) is a free parameter drawn unconstrained, named
  in the filter rather than left to NaN-propagation semantics (review F7) —
  or |ρ*_i| ≥ 1) is **filtered in the SEM draw engine, before interval
  assembly**: dropped whole (its entire parameter row is undefined — this is
  inadmissibility of the draw, not a degenerate profile), counted, and
  reported by the engine's own warning naming the actual cause. It must
  **not** be routed through `ssm_replicate_intervals()`'s degenerate-
  replicate warning, whose shipped text is cause-specific ("flat or
  zero-amplitude profiles", per-parameter exclusion of d and fit only) and
  would mislabel both the cause and the scope. This filtering is
  draw-generation code (the same layer as `ssm_montecarlo()`'s own
  admissibility guards), so §5.1's no-new-interval-assembly-code rule is
  untouched. A dropped fraction above a documented threshold (default 5%, a
  design default, not literature) upgrades the warning to an error advising
  `ci_method = "boot"` or model revision — the escalation lives in the same
  engine-side filter. Admissible draws whose *profile* is degenerate (flat,
  zero amplitude) still flow through `ssm_replicate_intervals()` and get its
  existing per-parameter NA contract, which is correctly worded for exactly
  that case.
- Global model health (nonconvergence, negative variances flagged by lavaan,
  failed identification checks) is surfaced *before* any SSM output; a
  latent profile from a broken fit is never printed.

---

## 5. Confidence intervals (Q5.1's architecture, pinned)

### 5.1 The draws-through-the-transform route

Two engines, one downstream path — deliberately the `ssm_montecarlo()`
architecture with lavaan supplying the mean and covariance:

- **`ci_method = "mvn"` (default):** draw ψ⁽ᵇ⁾ ~ MVN(ψ̂, V̂), b = 1…boots
  (default 2000, the package convention), where ψ̂ are the free parameters
  and V̂ lavaan's asymptotic vcov (observed-information or robust, per the
  estimator passthrough); map each draw through §4.1's profile map and §2's
  transform to a replicate row of (e, x, y, a, d, fit).
- **`ci_method = "boot"`:** lavaan bootstrap (refit per resample); each
  replicate's free-parameter vector goes through the *same* map. Far more
  expensive (a full lavaan refit per resample); nonconvergent replicates
  dropped with a count under the same warning contract as §4.5.

Then, identically for both engines: t0 from ψ̂, replicate matrix t →
`ssm_replicate_intervals(t0, t, interval, contrast, replicate_label)` —
which brings, for free and already tested: percentile assembly,
per-parameter NA exclusion, `quantile.circumplex_radian()` for profile
displacement (center on circular mean, unwrap, quantile, re-wrap),
`quantile.circumplex_contrast_radian()` plus branch alignment for contrast
displacement. **No new interval-assembly code is written for M5** (the
engine-side admissibility filter of §4.5 sits upstream of, and never
modifies, the interval assembly).

Whether cheap MVN propagation is accurate enough at realistic n versus the
refit bootstrap is Q5.1's empirical question, now answerable: §8.3's
coverage study compares the two engines and is the evidence that either
confirms `"mvn"` as default or flips it. The default is provisional until
that study runs (T3 acceptance).

**ANSWERED 2026-07-07 (T3; `devel/m5-coverage-oracle-results.rds`, seeded):
`"mvn"` confirmed as default, with one amendment — the propagated vcov must
be the sandwich.** On correctly specified populations (all §8.1 analytic
cells, N ∈ {250, 1000}, 500 reps/cell) mvn coverage sat at 0.918–0.976 for
every parameter including pole-straddling and ±180°-contrast displacement,
statistically indistinguishable from the 100-rep boot arm. On the §8.3
realism cell (fixed-angle model misspecified for jz2017's structure), mvn
with the plain ML vcov undercovered displacement at 0.886 (N = 250) and
0.878 (N = 1000) — N-stable, the signature of variance underestimation for
a pseudo-true target — while boot held 0.94/0.92. A directed probe (300
reps/N) confirmed the mechanism and the fix: with lavaan's
`se = "robust.huber.white"` sandwich vcov, mvn displacement coverage rose
to 0.937 (N = 250) and 0.960 (N = 1000), amplitude 0.943/0.960, nothing
degraded. `ssm_sem()` therefore fits with the sandwich by default (an `se`
argument, overridable); `ssm_sem_parameters()` warns when a raw-data
`se = "standard"` fit meets the mvn engine. This is White (1982) operating
as §8.3 anticipated: coverage of the model-conditional estimand under
misspecification requires misspecification-consistent variance. The final
recorded run replays the shipped `ssm_sem()` procedure end to end (post
review fixes, sandwich default active): **zero inadequate verdicts across
all cells, parameters, N, and both engines**; realism-cell mvn displacement
0.948 (N = 250) / 0.920 (N = 1000), all other mvn cells 0.916–0.970.

### 5.2 Per-parameter delta-method status and what is reported (refining Brief E)

Two refinements of Brief E's per-parameter table, both flagged:

**(i) Reporting surface.** Brief E suggested delta/Wald values "as a printed
SE" for e (and, caveated, a). The package has **no SE-printing surface**:
`print.circumplex_ssm()` and `ssm_table()` report estimate + interval only,
and §7.3's inheritance design keeps that contract (inventing an SE column
would break the snapshot-tested print conventions for no inferential gain).
Decision: **the package reports estimates and percentile/circular intervals
for all parameters — no SEs are printed anywhere.** Wald SEs for the
inspection `:=` quantities remain visible to users who `summary()` the
embedded lavaan fit directly, and the documentation explains their status
using the table below.

**(ii) Delta-method status under this estimand.** Brief E graded the delta
method for its *saturated* sketch, where e, x, y were linear in model
parameters. Under this spec's disattenuated estimand they are smooth but
**nonlinear** (ratios involving Var(t_i)):

| Parameter | Delta/Wald status (documentation guidance) | Interval reported (always in-package) |
|---|---|---|
| e, x, y | asymptotically valid (smooth, interior); no longer *exact* as in Brief E's linear case | replicate percentile (closely matches Wald here) |
| a | valid only away from a ≈ 0 — near the boundary the sampling distribution is folded/Rice-like and a symmetric interval misstates shape and can cross 0 (the guardrail regime) | replicate percentile only |
| d | locally valid for concentrated estimates but never used: the delta SE grows as 1/a (gradient of atan2 is (−y/a², x/a²), norm 1/a), and any Wald or naive-percentile interval lives on an arbitrary atan2 branch | circular quantiles via the replicate pipeline only |
| fit | not applicable | none (matches package convention) |

The amplitude-CI-excludes-zero guardrail and the
"displacement not interpretable" printed caution port to the latent object
unchanged — same decision rule, same `print` machinery (§7.3). Inherited
caveat, not an M5 defect (review F12): the shipped rule's effective
threshold is the display-precision artifact recorded in the B-spec (§12.5
there, decided (a)-now/(b)-follow-up), and on the latent *mean* path it
operates on a raw-score amplitude metric — the already-recorded package
follow-up covers it.

### 5.3 Multi-group and multi-measure dependence

All draws come from **one joint MVN on the full free-parameter vector** (or
one joint bootstrap refit). Dependencies that matter are then carried
automatically: two measures' profiles share scale-block parameters (their
contrast replicates are properly dependent — the Monte Carlo engine's "joint
draw within group" design, inherited); two groups' parameters are coupled
through invariance constraints (§6), so latent contrast draws carry exactly
the constraint-induced covariance. No per-block independent drawing.

### 5.4 RNG contract

`ci_method = "mvn"` consumes the global RNG stream (one `rnorm` block via
the package's `mvn_draws()`/`mvn_root()` — reusing THE single draw-root
convention so nothing drifts numerically); `ci_method = "boot"` consumes it
through lavaan's resampling. The estimation entry point joins DESIGN.md's
RNG-consuming list at ship time with the `set.seed()`-immediately-before
convention; model fitting itself is deterministic.

### 5.5 Boundary suite (CLAUDE.md danger zones, instantiated for the SEM layer)

Required tests for T3 (profiles) and T4 (contrasts):

- latent profile with d* at the 0°/360° pole (constructed population, §8.1):
  point estimate reported per the package's [0, 360) / exactly-360
  convention; CI straddles the pole contiguously (circular quantiles);
- latent a* ≈ 0 (measure ⊥ plane): displacement NA semantics or
  guardrail-uninterpretable path, no crash; draws near-uniform in d handled
  by the circular machinery, not by luck;
- flat / degenerate latent profile (measure ⊥ g and plane): the profile is
  ~0 everywhere; parameters degrade exactly as the observed path degrades;
- contrast displacement near ±180°: `angle_dist()` branch, CI endpoints
  legitimately outside ±180° on the estimate's branch (the
  `ssm_replicate_intervals()` alignment shift), estimate geometrically and
  numerically inside its interval;
- unequally spaced angles: the §2.1 OLS functional recovers a
  **constructed exactly-cosine profile** (fed directly to the functional —
  tier-agnostic, since only the strict tier implies exact cosine profiles,
  §6.4) to machine precision at an angle set violating the harmonic-balance
  condition, where the closed form provably does not (the invariant, tested
  inside the SEM layer).

---

## 6. Multi-group latent contrasts (P2 / T4)

### 6.1 The two estimands, side by side (Q5.2, binding framing)

- **Observed contrast** (existing, unchanged): "do the groups' *measured*
  profiles differ" — difference of SSM parameters computed from each group's
  observed scores/correlations, group-stratified resampling. Confounds
  structural difference, differential reliability, and non-invariance; that
  is a property of the estimand, documented, not a bug.
- **Latent contrast** (new, separately named): "do the groups' *constructs*
  differ, granted the instrument measures the same thing in both" —
  contrast on latent SSM parameters under invariance constraints.
  Disattenuated; conditional on invariance; when invariance fails it is not
  "more principled", it is misspecified, and the output is an explicit
  non-comparison (§6.3).

The side-by-side documentation of both estimands is a **T4 deliverable**
(MILESTONES T4 acceptance: "both estimands documented side by side"): it
ships in the contrast entry point's Rd documentation and in the printed
invariance/verdict block, with exactly this wording shape, at the moment
the latent contrast becomes callable — not deferred to the vignette. T5's
vignette then retells it in one table with worked examples.

### 6.2 The invariance sequence, adapted for fixed-angle models

The Meredith-style configural → metric → scalar ladder (Meredith 1993;
Vandenberg & Lance 2000 — sequence cited, all decision thresholds TBT)
cannot be applied verbatim: the *angles* are fixed constants in every group
by construction, so "loading invariance" is partly vacuous. The adapted
ladder, per model tier:

| Step | `"scaled"` tier | `"strict"` tier |
|---|---|---|
| 1. Configural | same fixed-angle shape fits acceptably in each group (per-group fit assessed) | same |
| 2. Metric-analog | saturations (a_i, c_i) equal across groups | **vacuous** (all loadings fixed) — reported as "imposed by model, not testable"; group metric differences load into Φ_g where they are indistinguishable from dispersion differences (documented honestly) |
| 3. Scalar | intercepts ν_i equal across groups (mean structure on) | same |
| 4. Strict (optional) | residual variances equal | same |

**Multi-group factor metric (pinned per review F5 — load-bearing for
cross-group direction comparability):** with (a_i, c_i) equality-
constrained at the metric step, the non-reference groups' factor
(co)variances must be freed for the constraint to be testable, and their
*shape* matters. A fully free Φ_g would let a non-reference group's plane
go anisotropic/oblique — the exact direction distortion §3.1/§4.3 exclude,
reintroduced silently and asymmetrically, confounding the Δd* contrast
with axis-metric differences. Pinned parameterization for the scaled tier:
reference group exactly per §3.1; non-reference groups free var(g_g), free
g–plane covariances, and a **single free plane scale φ_g with isotropy and
orthogonality retained** (var(cx_g) = var(cy_g) = φ_g, cov(cx, cy)_g = 0).
That retention is an additional **per-group stationarity assumption** —
stated in the docs exactly as §3.1 states the single-group one (listed in
§10), emitted by the generator's `invariance =` branch (§3.5 contract),
and covered by the §3.4 identification gate extended to the multi-group
constrained models (a T2 acceptance item alongside the single-group df
derivation).

**AMENDED 2026-07-07 at T4 (supersedes both the original pinned
parameterization above and the T3 note that stood here): under the scaled
tier, the g–plane covariances are fixed to 0 in ALL groups at ALL rungs —
non-reference groups free only var(g_g) and the single isotropic plane
scale φ_g.** Reason: nesting. Post-T3, the configural rung is the §3.1
model per group, with φ_g = 0 fixed everywhere. A metric rung that frees
non-reference φ_g can express g-lean structures configural cannot, so
metric would not be nested in configural and the §6.2 nested-Δχ² machinery
(lavaan `anova()`, Satorra–Bentler under robust estimators) would be
invalid for exactly the comparison that gates the latent contrast. With
φ_g ≡ 0, nesting holds by the classic rescaling argument (a metric-rung
point with var(g₂) = v and plane scale φ₂ is reproduced in configural by
a_i√v and c_i√φ₂), and the ladder is a pure sequence of equality
constraints plus metric-freeing — well-ordered. Consequences: (i)
cross-group differences in g-lean load into misfit under the scaled tier,
stated in the docs exactly like the single-group g ⊥ plane assumption
(§3.1/§10) — the strict tier, whose fully free per-group Φ has no ridge
and whose ladder involves only equality constraints (its metric rung being
vacuous, §6.2 table), remains the tier that can express and compare
g-lean; (ii) the T3 note's deferred empirical identification check of the
free-φ_g non-reference block is moot — the remaining metric-rung blocks
(shared loadings anchored by the reference group's fixed metric; var(g_g)
and φ_g absorbing group scale) still get the standard empirical SE-sanity
check in T4's tests, per the T3 lesson that counting is not enough.

**Gating rule:** the latent *mean-based* group contrast requires step 3; the
latent *measure-profile* group contrast requires step 2 (correlations carry
no intercepts). Step 4 is reported, never required. The decision statistic
defaults to the nested-model test **as lavaan's own `anova()` computes
it** — the plain Δχ² under ML, and the scaled difference test under
robust (MLM/MLR-family) estimators, where a naive difference of scaled χ²
statistics is not χ²-distributed (Satorra & Bentler, 2001 — direction
citation; review F6). Still a computed quantity, no literature constant
needed. Alternative-index cutoffs (ΔCFI etc.) are offered only
once transcribed (**TBT**, candidate source Cheung & Rensvold 2002), and
until then the summary prints the indices without a verdict attached to
them. Surfaced as open decision §12.2. **Corrected in place 2026-07-24
(M57):** the TBT is discharged and the ΔCFI cutoff IS now offered — as a
labeled, reported-only secondary criterion scope-gated to two groups plus a
plain normal-theory CFI, cited to `cairn/references/cheung2002.md`. Δχ²
remains the sole gating verdict statistic, exactly as this paragraph's
first half says. See §12.2 item 2 for the resolution.

### 6.3 The non-comparison path (acceptance-critical)

When the required step fails, the returned object contains the invariance
table and an explicit verdict field stating **which level failed and that
the latent contrast is therefore not computed** — the contrast parameter
slots are absent/NA by construction, `print()` says "these groups cannot be
compared on this instrument's latent metric (metric invariance rejected,
Δχ²(df) = …, p = …)", and no downstream method (plot/table) renders a
contrast from it. An override (`force = TRUE`-style) is deliberately **not
offered** in v1; the partial-invariance escape hatch is the low-level API
(§1.3, §7.2).

### 6.4 The latent contrast estimand and its CIs

Per-group latent profiles, correlation path: group-specific ρ*_g (all
ingredients group-subscripted; under metric invariance t_i has the same
composition in both groups, while the group factor (co)variances free
under §6.2's pinned multi-group metric — var(g_g), the g–plane
covariances, and the isotropic plane scale φ_g, i.e., SD(t_i)'s genuine
group dispersion differences — vary by group).
Mean path: group-g latent mean profile

    μ*_g = ν + Λ·α_g   (p-vector; ν invariant, α_reference ≡ 0)

which is invariant to the reference-group choice (shifting all α_g by a
constant and ν by its Λ-image leaves every μ*_g unchanged — a T4 test).
Each group's SSM parameters are the §2 transform of its profile. **Exact
recovery holds under the strict tier only** (there Λ = B, so μ*_g's
deviation from ν is exactly cosine and W recovers α_g's contribution
exactly at any full-rank spacing). Under the default scaled tier
Λα_g = α_g1·a + α_gx·(c∘cos θ) + α_gy·(c∘sin θ) is **not** in
span{1, cos θ, sin θ} for heterogeneous (a_i, c_i) (review finding,
numerically verified: OLS returns a nearby projection, not α_g), so no
exactness claim is made there — the estimand is simply the transform of the
latent mean profile, as everywhere else in this spec. One substantive
consequence is documented rather than hidden: under heterogeneous general
saturations a_i, a pure latent elevation shift (α_gx = α_gy = 0) moves the
profile non-uniformly and therefore leaks into the group's fitted x/y —
but the *observed* mean profile shifts by exactly the same a_i·Δα pattern,
so this is a property of profiles under differential saturation shared with
the existing estimand, not a distortion the SEM layer introduces. T4's
mean-path exactness tests are written against the strict tier (or a
constructed cosine profile, §5.5), never against scaled-tier fits.

Contrast: `param_diff()` semantics verbatim — second minus first,
displacement via `angle_dist(d2, d1)` in (−180°, 180°], per-draw across the
§5.3 joint replicates, contrast displacement column classed
`circumplex_contrast_radian`, interval branch-aligned to the estimate. The
existing machinery, fed latent replicates.

---

## 7. API surface

Names are proposals (open decision §12.1); signatures are the contract.

### 7.1 Syntax generator (T2)

    ssm_sem_syntax(
      instrument = NULL,            # circumplex_instrument, OR:
      scales = NULL, angles = NULL, #   explicit names + degrees
      measures = NULL,              # character vector of measure names
      model = c("scaled", "strict"),
      grouping = FALSE,             # emit multi-group-ready mean structure
      invariance = c("configural", "metric", "scalar", "strict"),
                                    # which constraint set to emit (T4)
      include_defined = NULL        # the §3.5 inspection-only := lines;
                                    #   NULL = auto (strict: yes, scaled: no)
    ) -> character (lavaan model syntax) + attributes (angles, model, weights)
                                    # `weights` = the §2.1 OLS matrix W,
                                    #   always present (§3.5's invariant)

Pure function of its arguments; no data touched. **Decided: emission does
not require lavaan** — the string is inspectable and the generator is
testable on CRAN without the Suggests package; only *fitting* gates on
lavaan. The graceful-degradation error lives in `ssm_sem()` (T2 acceptance's
"clear error, not a load failure" — worded to name `install.packages("lavaan")`).

### 7.2 Estimation (T3, T4)

    ssm_sem(
      data, scales, angles = octants(),
      measures = NULL,              # NULL → multi-group mean path; requires
                                    #   grouping (single-group mean path has
                                    #   no product, §1.3 → validation error)
      grouping = NULL, contrast = FALSE,
      model = c("scaled", "strict"),
      invariance = NULL,            # T4; ladder always fitted up to this.
                                    #   NULL = path-dependent default (review
                                    #   F8): "metric" when measures given,
                                    #   "scalar" on the mean path — each
                                    #   path's §6.2 gating level, so the
                                    #   default never reaches §6.3's verdict
                                    #   with its required step untested
      ci_method = c("mvn", "boot"),
      boots = 2000, interval = 0.95,
      estimator = "ML", missing = c("listwise", "fiml"),
      ...                           # narrow, documented lavaan passthrough
    ) -> circumplex_ssm_sem

    # Low-level adapter (also the partial-invariance escape hatch and the
    # future Bayesian draws-adapter's sibling, Brief E Q6.3):
    ssm_sem_parameters(fit, scales, angles, measures = NULL,
                       ci_method = "mvn", boots = 2000, interval = 0.95)
      # fit: a user-supplied lavaan fit of a compatible model; compatibility
      # checked structurally (named parameters present), not by provenance.

Validation mirrors `ssm_analyze()` house style (`stopifnot()` + `is_*()`
helpers). `contrast = TRUE` follows the existing arity rules (exactly two
groups or two measures; second minus first).

### 7.3 The returned object

`circumplex_ssm_sem`, a **subclass of `circumplex_ssm`**: `results`,
`scores` (here the latent profile vectors ρ*/μ*), `details` (boots,
interval, angles, contrast, listwise/missing, method = ci_method,
score_type = "Latent"), `call` — so `ssm_table()`,
`ssm_plot_circle()`/`_curve()`/`_contrast()`, and the guardrail-printing
`print.circumplex_ssm()` work by inheritance — **plus** `sem` (the lavaan
fit), `invariance` (the §6.2 table + verdict), and `model` (tier, syntax,
weights). A `print.circumplex_ssm_sem()` prepends the measurement-model
block (tier, global fit indices, invariance verdict or non-comparison
statement) then delegates.

**Inherited consumers that would misrepresent latent results — two
identified in review, both requiring T3 work, plus a standing audit:**

- `summary.circumplex_ssm()` keys its replicate label on
  `details$method == "montecarlo"` and otherwise prints "Bootstrap
  Resamples", and it prints a Listwise Deletion line. With
  `method = "mvn"` it would print a **false statement of the inferential
  method**. T3 ships a `summary.circumplex_ssm_sem()` override that owns
  the method/replicate-label and missing-data lines (snapshot-tested).
- `ssm_ci_accuracy()` accepts any `inherits(x, "circumplex_ssm")` object
  and would replay an observed-data resampling procedure on a latent
  estimand — meaningless, silently. T3 adds an explicit refusal guard for
  `circumplex_ssm_sem` there (with a message pointing at §8.3's harness).
- T3's acceptance includes a **method audit**: every exported function that
  dispatches on or checks `circumplex_ssm` is enumerated (Grep for the
  class string) and each is verified sensible for the subclass or
  overridden/guarded, with a test per decision.

### 7.4 Dependency policy (binding)

`lavaan` in **Suggests**. Zero net-new hard dependencies. Every SEM entry
point begins with a `requireNamespace("lavaan", quietly = TRUE)` gate and a
clear install-hint error; the package loads and all non-SEM functionality
runs without lavaan; tests `skip_if_not_installed("lavaan")`; the T5
vignette precomputes or eval-gates. Two memory-file amendments ride with
this spec's implementation (both applied at T2, recorded here so neither is
forgotten): (i) DESIGN.md's "test oracles only" note — lavaan becomes a
Suggests *runtime* path for the SEM feature family, still never
load-required; (ii) CLAUDE.md/DESIGN.md's "closed-form = OLS only for
equally spaced angles" invariant gains the §2.1 harmonic-balance
sharpening (review F11) — the safe sufficient direction every existing use
relies on is untouched, but once T2's tests carry a balanced-but-unequal
counterexample fixture, the memory files must not contradict the suite.

---

## 8. Validation strategy (T3/T4 acceptance)

Same oracle discipline as Briefs A/B; the SEM layer is estimation machinery
and gets the full treatment. All heavy scripts live in `devel/`, run by
`/statistical-validation`, never by `R CMD check`.

### 8.1 Analytic-truth populations (the primary oracle)

Construct population covariance matrices **from the §3.1 model itself** with
chosen (a_i, c_i, σ_Mg, σ_Mx, σ_My, Θ): the true latent profile ρ*₀ is then
available in closed form, and the true SSM parameters are the §2 transform
of it — no simulation needed for the truth, only for the sampling. Cells
must include: interior (a* comfortably positive, d* mid-quadrant), d* at the
0°/360° pole, a* ≈ 0, measure ⊥ everything (flat), a two-group design
with a latent contrast near ±180°, and a **heterogeneous-saturation cell**
(second-harmonic c_i pattern, §4.3) whose design-time-computed d* rotation
supplies the demonstrated magnitude for §4.3's caveat in the T3 docs and
T5 vignette. This is the SEM analog of the B6 coverage
oracle, and should share its harness/reporting style
(`devel/m4-coverage-oracle.R`).

### 8.2 Machinery pins (cheap, in testthat)

- **Θ → 0 equivalence (population-level, per review F4):** as Θ → 0 the
  disattenuated profile converges to the observed profile, but the naive
  data-level pin is untestable (at Θ ≈ 0 fitted residual variances sit on
  the boundary and trip §4.5's own health gate; at finite n a df > 0 model
  smooths the sample moments, so exact equality never holds). The pin
  therefore runs at the **population level**: construct the analytic Σ
  from the §3.1 model at each rung of a small-positive-Θ ladder, hand it
  to lavaan as `sample.cov` (misfit is exactly zero at the optimum, so
  parameters recover the generating values), and assert the estimand map's
  output converges to the observed-profile functional along the ladder
  under a stated tolerance schedule (tolerances shrinking with Θ, pinned
  at implementation). A *data-level* comparison survives only as a smoke
  test with a documented loose tolerance. A latent layer that disagrees
  with the observed layer when there is nothing to disattenuate is broken —
  §4.5's boundary-health interaction is why the assertion lives on
  population moments, not data.
- **Weights identity:** §3.5's equal-spacing/OLS test.
- **Reference-group invariance:** §6.4's μ*_g reparameterization test.
- **Transform equivalence:** the SEM layer's (e, x, y, a, d) transform on a
  fixed profile vector equals `ssm_parameters()`'s output at equally spaced
  angles (shared functional, pinned).

### 8.3 The coverage study (answers Q5.1's blocked-on-M4 question)

An empirical coverage loop over §8.1's cells at realistic n (order
100–1000), comparing `ci_method = "mvn"` vs `"boot"` per parameter
(e, a, d; contrast Δd in the two-group cells).

**Relationship to `ssm_ci_accuracy()` (deviation from MILESTONES T3's
literal wording, flagged):** the exported `ssm_ci_accuracy()` cannot be
called as-is — it replays `ssm_analyze()`'s observed-data procedures, while
assessing the SEM layer requires a lavaan (re)fit per simulated dataset, a
different procedure and cost class (and §7.3 adds a guard refusing
`circumplex_ssm_sem` objects for exactly this reason). What T3 does instead:
a seeded `devel/` harness (`m5-coverage-oracle.R`, the B6/`m4-coverage-
oracle.R` pattern) that **reuses the `ssm_ci_accuracy()` machinery** — its
Bradley-liberal banding + 95% Wilson-interval verdict conventions and its
plug-in-population philosophy — rather than reinventing them; shared
helpers are factored out rather than copied where practical. MILESTONES
T3's acceptance line is amended to this effect (same date as this spec's
review revision), per the workflow rule for criteria that turn out to be
imprecise.

**Population construction, corrected against the shipped machinery:**
realism cells are built from `cpm_fit()` on jz2017's scale block, taking
P̂ as the population scale structure; the joint (scales + measure)
population matrix embeds P̂ with the observed measure–scale
cross-correlations and is drawn from directly via the package's
`mvn_root()` (the single draw-root convention) after the PSD repair — the
approach `ssm_ci_accuracy()` itself uses for its correlation path.
`cpm_simulate()` is **scale-only by its recorded contract** (B-spec §8.2
gap G2: it cannot produce joint scales + measures draws) and is used only
where a cell needs scale draws alone. Coverage is thereby also measured
**under realistic misspecification** of the fixed-angle model — the
fixed-cosine structure is an approximation for real data, and the CIs'
robustness to that approximation is part of the verdict.

**The misspecified cells' truth is pinned (review F3):** the latent
estimand is model-conditional, so when the population was not generated by
the §3.1 model there is no closed-form ρ*₀ — the coverage target is the
**pseudo-true value** in the QMLE probability-limit sense (White, 1982 —
direction citation, no numbers): fit the model tier to the repaired
population joint matrix itself (lavaan on population moments), apply the
§4.1 map and §2 transform, and measure coverage of *that* value. These
cells therefore assess coverage of the model-conditional estimand under
misspecification — never of a "true" circumplex parameter, and the
harness's reporting says so. Output decides
the default engine (§5.1) and the documented small-n guidance.

### 8.4 Cross-implementation spot checks

Where lavaan's own delta SE is trustworthy (a linear `:=` under the strict
tier), it must agree with the MVN-propagation SD of the same quantity within
Monte Carlo error — "the same quantity" meaning the **covariance-metric**
`cov_e`/`cov_x`/`cov_y` (§3.5), checked against MVN propagation of itself,
never against the reported correlation-metric parameters (review F10). A
check that the vcov plumbing is wired correctly, not a validation of the
intervals (which deliberately diverge for a, d).

### 8.5 `/statistical-validation`

Runs after T3 and after T4 (both produce SSM parameters/intervals —
MILESTONES guardrail), covering §8.1–8.4's seeded scripts.

---

## 9. Cost and defaults

- `ci_method = "mvn"`: one lavaan fit + `boots` MVN draws + vectorized
  profile/transform math — seconds; the default for interactive use.
- `ci_method = "boot"`: `boots` lavaan refits — minutes at field n;
  documented as the robustness option pending §8.3's verdict.
- boots = 2000, interval = 0.95: package conventions, unchanged.
- The invariance ladder (T4) is ≤ 4 lavaan fits — cheap relative to `"boot"`.
- No C++ work anticipated: the hot loop is lavaan's, not ours; the profile
  map and transform are vectorized R over a boots × k matrix. If profiling
  falsifies this, the B-spec's phase-2 discipline applies (R stays as the
  oracle).

---

## 10. Stated limitations (user-facing docs, verbatim candidates)

- **Model-conditional:** every latent quantity is conditional on the
  fixed-angle measurement model (and the chosen tier) being adequate; global
  fit is reported alongside, and a poorly fitting measurement model makes
  the latent SSM parameters uninterpretable, not merely imprecise.
- **Fixed angles are theoretical claims**, not estimates; if the instrument's
  real geometry departs from theory, that departure is absorbed into misfit
  and residuals, not into the angles (use `cpm_fit()` to *examine* geometry).
- **Latent-plane stationarity is assumed**, not tested directly: the model
  fixes the plane factors isotropic and orthogonal (§3.1) — and, in
  multi-group models, per group up to a common scale (§6.2) — and
  anisotropic latent dispersion surfaces only as global misfit.
- **The scaled tier additionally assumes the general factor is orthogonal
  to the plane** (φ_g = 0, fixed for identification — §3.1's T3
  amendment): a true g-lean under the scaled tier surfaces as global
  misfit, not as an estimated covariance. To model the lean, use the
  strict tier, whose fixed loadings leave the full factor covariance
  matrix free.
- **Latent displacement is the first-harmonic direction of the
  saturation-modulated disattenuated profile** (§4.3), not "the measure's
  angle in factor space": heterogeneous saturations and g–plane lean
  rotate it (with fit possibly staying high), exactly as they do the
  observed displacement — the latent layer removes the *reliability*
  modulation, nothing more. Latent fit < 1 likewise reflects differential
  saturation and/or g–plane lean (§4.2), not only the former.
- **Disattenuated correlations can be large:** removing attenuation moves
  |ρ*| toward 1; values at/above 1 indicate misspecification and are refused
  rather than summarized (§4.5).
- **Invariance gating is a modeling decision** with a default test, not an
  oracle; the observed contrast remains available and answers its own
  (different) question.
- **Asymptotics:** `"mvn"` intervals inherit the asymptotic normality of
  lavaan's estimator at the user's n; §8.3's study, not hope, is the basis
  for the default.

---

## 11. Phasing and traceability (T2–T5 → sections)

| Task | Consumes | Acceptance hooks |
|---|---|---|
| **T2** syntax generator + Suggests gating | §3 (shapes, identification gates, emitted contract), §2.1 (weights), §7.1, §7.4 | weights identity test on the always-present attribute (§3.5/§8.2); fit-under-lavaan test on a reference instrument (§3.5); df/identification derivation (§3.4); graceful-degradation error (§7.1/§7.4) |
| **T3** latent estimation + CIs | §2, §4, §5, §7.2–7.3, §8.1–8.3, §8.5 | boundary suite (§5.5); Θ→0 and transform pins (§8.2); `summary` override + `ssm_ci_accuracy` guard + method audit (§7.3); coverage harness reusing the ssm_ci_accuracy machinery, incl. mvn-vs-boot verdict (§8.3); `/statistical-validation` |
| **T4** invariance contrasts | §5.3, §6, §7.2–7.3, §8.1 (contrast cells), §8.5 | adapted-ladder gating (§6.2); non-comparison path (§6.3); side-by-side estimand documentation in Rd/print (§6.1); ±180° branch tests (§5.5/§6.4); reference-invariance pin, strict tier (§8.2/§6.4); `/statistical-validation` |
| **T5** vignette | §1.1, §4.2 (fit's meaning), §6.1 (two-estimand table), §2.2/§5 (why intervals are in-package), §10 | statistical-precision bar; every CI/estimand claim traces here; TBT items transcribed with provenance |

Within-T3 ordering note: the coverage study (§8.3) is T3's last step, after
the machinery pins pass — it consumes the working estimator.

## 12. Open decisions (for Jeff — defaults proposed, none blocking T2)

1. **Naming:** `ssm_sem()` / `ssm_sem_syntax()` / `ssm_sem_parameters()`
   (proposed) vs `ssm_latent*()`. The class name follows the function name.
2. **Invariance verdict statistic** (§6.2): Δχ² default now, ΔCFI (TBT)
   added later as an option — or hold the whole verdict machinery until the
   transcription lands and print indices only? Proposed: Δχ² default now.
   **Update 2026-07-07: the transcription has landed**
   (`devel/cr2002-transcription.md`, from the full Cheung & Rensvold 2002
   text; includes the source's internally contradictory p. 251 sentence,
   the operational rule, and the binding scope caveats — two groups,
   plain ML, normality, never validated for robust indices). Offering the
   ΔCFI flag as a labeled secondary criterion is now unblocked; whether to
   offer it remains Jeff's call at T4. Δχ² stays the default either way.
   **RESOLVED 2026-07-24 (M57): offered.** `ssm_sem()`'s ladder table now
   carries a `dcfi` column and, inside the criterion's validated envelope
   (two groups AND a plain normal-theory CFI), a `cr` retain/reject column
   against −.01; outside that envelope the value prints with an explicit
   "cutoff not validated for this configuration" note and no verdict. It is
   reported-only — `comparable`, the verdict string, and the estimation-fit
   selection never read it, and Δχ² remains the sole gate. The citable
   record is now `cairn/references/cheung2002.md` (which re-verified every
   value against the PDF); the transcription file remains the
   first-channel protocol artifact. ΔGamma hat and ΔMcDonald's NCI stay
   transcribed but unwired.
3. **g–plane covariances free by default** (§3.1): proposed yes, pending
   T2's identification check; flip recorded here if it fails.
   **RESOLVED 2026-07-07 (T3): the check failed — flipped to 0-fixed, no
   `free_g_plane` switch.** The model is locally unidentified exactly at
   φ_g = 0 (rank-deficient Jacobian along the a↔φ_g trade; §3.1 has the
   direction and evidence), so the free variant fails at its own null and
   cannot be offered responsibly. g-lean is modeled via the strict tier.
4. **Default model tier:** `"scaled"` (proposed) with `"strict"` documented
   for small p — or `"strict"` default for maximal theory-fidelity?
   Proposed: `"scaled"`; equal-saturation is an assumption users should opt
   *into*, not out of.
5. **Whether the emitted inspection `:=` lines ship at all** (§3.5,
   `include_defined = NULL` auto): proposed emitted under `"strict"`,
   omitted under `"scaled"` (where they are nonlinear and add clutter
   without insight). Note this decision cannot affect the weights
   invariant or its T2 test, which target the always-present weights
   attribute (§3.5).
6. **(Added post-T3, from the Wendt et al. comparison —
   devel/m5-wendt-discrepancies.md §1.) An "equal-g" middle tier?**
   Constrain the general saturations equal (a_i ≡ a), keep the circumplex
   saturations free with fixed directions, and free the g–plane
   covariances — Wendt et al.'s parameterization transplanted onto fixed
   theoretical angles. Counting-identified, and the §3.1 ridge is blocked
   by the equality constraint, so it would restore the estimable g-lean
   channel (empirically real on IIP data: their replicated g–agency
   r ≈ −.3) inside a mostly-scaled model, at the cost of a
   tau-equivalence assumption on g. Not implemented: it needs its own
   empirical local-identification check (T3's lesson — counting is not
   enough), and no new tier lands during T4's invariance work. Proposed:
   revisit at T5/M6 if real-data misfit of the scaled tier proves to be
   dominated by the g-lean channel.

## Revision log (vs the fresh-session review, `devel/m5-sem-design-review.md`)

Every finding of the 2026-07-07 fresh-session review (verdict ACCEPT WITH
CHANGES), with its resolution. Nothing dropped; each was weighed on the
merits (the review's numerical probes were spot-verified where
load-bearing).

| Finding | Resolution |
|---|---|
| **F1 (high)** — §4.3's direction-integrity claim false as written: stationarity necessary, not sufficient (heterogeneous saturations rotate d* with fit still high) | **Fixed.** §4.3 rewritten: the fixed metric buys only that the transform adds no axis-anisotropic distortion of its own; d* defined honestly as the first-harmonic direction of the saturation-modulated profile, equal to the factor-space angle only under homogeneous effective saturations and zero g–plane lean; the improvement over observed d pinned as removal of reliability modulation (ρ_i = ρ*_i·√rel_i). §10 bullet added; §8.1 gains a heterogeneous-saturation cell supplying a demonstrated magnitude for the docs/vignette. |
| **F2 (medium)** — §4.2 attributed latent fit < 1 to differential saturation alone; g–plane covariance (and strict-tier Φ̂ anisotropy) are further channels | **Fixed.** Both (three) channels named in §4.2 with the review's probe values; carried into the §10 bullet. |
| **F3 (medium)** — §8.3's misspecified cells had no defined truth | **Fixed.** Coverage target pinned as the pseudo-true value (White, 1982 — direction citation): fit the tier to the repaired population moments, map, transform; cells explicitly assess the model-conditional estimand under misspecification, and the harness reporting says so. |
| **F4 (medium)** — §8.2's Θ→0 pin untestable as written (boundary-health collision; finite-n df > 0 smoothing) | **Fixed.** Pin respecified at the population level (analytic Σ as `sample.cov` along a small-positive-Θ ladder, stated tolerance schedule); data-level check demoted to a loose-tolerance smoke test; §4.5 boundary interaction noted in place. |
| **F5 (medium)** — multi-group scaled tier's non-reference factor metric unspecified; fully-free Φ_g would silently reintroduce anisotropy, asymmetrically | **Fixed.** §6.2 pins the parameterization (reference per §3.1; non-reference: free var(g_g), free g–plane covariances, single isotropic plane scale φ_g), named as an additional per-group stationarity assumption; §3.5 emission contract and §3.4 identification gate extended; §6.4 and §10 updated. |
| **F6 (low)** — Δχ² verdict silent on robust estimators | **Fixed.** §6.2 pins the verdict statistic as lavaan's own nested-model test (scaled difference under MLM/MLR; Satorra & Bentler, 2001 — direction citation). |
| **F7 (low)** — inadmissible-draw filter omitted Var(M) ≤ 0; "Var(M) observed/implied" ambiguous (undefined under FIML) | **Fixed.** Var(M) ≤ 0 named in the §4.5 filter; §4.1 pins Var(M) to the model-implied value everywhere. |
| **F8 (low)** — `invariance = "metric"` default incoherent for the mean path; single-group mean path undefined | **Fixed.** §7.2: `invariance = NULL` path-dependent default (metric with measures, scalar on the mean path — each path's gating level); `measures = NULL` without `grouping` is a validation error per §1.3. |
| **F9 (low)** — MILESTONES T2's acceptance still encoded the falsified "only when equally spaced" test | **Fixed (MILESTONES.md).** T2's acceptance line amended to the harmonic-balance form with a pointer to §3.5's test aim, same amendment treatment as T3's. |
| **F10 (low)** — strict-tier `:=` lines are covariance-metric values; "superseded by the package's own intervals" implied same-quantity-different-interval | **Fixed.** §3.5: lines named `cov_e`/`cov_x`/`cov_y` with a metric-stating comment (values, not merely SEs, differ from the reported parameters); §8.4 scoped to the covariance-metric quantity explicitly. |
| **F11 (informational)** — CLAUDE.md/DESIGN.md's iff-equal-spacing invariant now known-imprecise | **Recorded.** Added to §7.4's at-T2 memory-file amendment list (sharpening only; the sufficient direction in every existing use is untouched). |
| **F12 (informational)** — reflection indeterminacies undiscussed; ported guardrail threshold caveat worth a cross-ref | **Fixed.** §3.4 notes both sign flips, their exact invariance under the estimand map (boot-replicate robustness), and the instruction that the identification check must not misread them; §5.2 cross-references the B-spec §12.5 threshold caveat, noting the raw-score metric on the mean path. |

**Pushbacks:** none — every finding was verified correct before applying
(F1/F2's probes reproduce; F3–F5 are genuine under-specifications; the rest
are as stated).

## References (directions cited; all decision numbers TBT at implementation)

- Browne, M. W. (1992). Circumplex models for correlation matrices.
  *Psychometrika, 57*(4), 469–497. (Scope boundary: free-angle model.)
- Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit
  indexes for testing measurement invariance. *SEM, 9*(2), 233–255.
  (Candidate ΔCFI source — TBT.)
- Cudeck, R. (1989). Analysis of correlation matrices using covariance
  structure models. *Psychological Bulletin, 105*(2), 317–327. (§4.4's
  trap; citation details to re-verify at implementation.)
- Gurtman, M. B., & Pincus, A. L. (2003). The circumplex model: Methods and
  research applications. In J. A. Schinka & W. F. Velicer (Eds.),
  *Handbook of psychology: Research methods in psychology* (Vol. 2,
  pp. 407–428). Wiley. (§3.2: source of the "equal spacing / equal
  communalities" constraint taxonomy — verified 2026-07-07: its
  confirmatory model is Browne's CIRCUM, the `cpm_fit()` family, not a
  factor parameterization; also useful CPM-side background. Informational
  only, no numeric value taken from it.)
- Locke, K. D. (2010). Circumplex measures of interpersonal constructs. In
  L. M. Horowitz & S. Strack (Eds.), *Handbook of interpersonal
  psychology* (pp. 313–324). Wiley. (§3.2: cited by Wendt et al. (2019)
  for the IPC scoring formulas corresponding to their CFA-PC — secondhand,
  not independently verified.)
- Mardia, K. V., & Jupp, P. E. (2000). *Directional Statistics*. Wiley.
  (Concentrated-angle asymptotics behind §5.2's d row.)
- Meredith, W. (1993). Measurement invariance, factor analysis and factorial
  invariance. *Psychometrika, 58*(4), 525–543.
- Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
  *JSS, 48*(2). (Suggests dependency.)
- Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square test
  statistic for moment structure analysis. *Psychometrika, 66*(4), 507–514.
  (§6.2's robust-estimator nested test — direction citation.)
- White, H. (1982). Maximum likelihood estimation of misspecified models.
  *Econometrica, 50*(1), 1–25. (§8.3's pseudo-true coverage target —
  direction citation.)
- Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the
  measurement invariance literature. *ORM, 3*(1), 4–70.
- Wendt, L. P., Wright, A. G. C., Pilkonis, P. A., Nolte, T., Fonagy, P.,
  Montague, P. R., Benecke, C., Krieger, T., & Zimmermann, J. (2019). The
  latent structure of interpersonal problems: Validity of dimensional,
  categorical, and hybrid models. *Journal of Abnormal Psychology, 128*(8),
  823–839. (§3.2: large-sample CFA-PC/-QC comparison — informational
  context and the strict-tier correspondence, not a validation oracle for
  M5's disattenuated-profile estimand — post-T3 addition.)
- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
  interpersonal construct validation. *Assessment, 24*(1), 3–23.
  (Interpretation benchmarks; coverage-study framing.)

## Change log

- 2026-07-07 — T4 opening amendment (§6.2): scaled-tier multi-group metric
  re-pinned with g–plane covariances fixed to 0 in all groups at all rungs
  (non-reference groups free var(g_g) + isotropic plane scale φ_g only).
  The T3 φ_g flip made the originally pinned free-φ_g non-reference block
  non-nested against the configural rung, invalidating the ladder's Δχ²
  tests; the amendment restores exact nesting via the rescaling argument.
  Cross-group g-lean comparisons are a strict-tier capability, documented.
  The deferred free-φ_g identification check is moot.
- 2026-07-07 — Primary-source verification pass (Jeff supplied the Wendt
  et al. supplements, Moss 2026, Cheung & Rensvold 2002, Gurtman & Pincus
  2003 in full; devel/m5-wendt-discrepancies.md §8): §3.2 pedigree
  re-corrected — G&P 2003's confirmatory model is Browne's CIRCUM (the
  `cpm_fit()` family), so the fixed-cosine three-factor CFA is Wendt et
  al.'s own construction (their R Code S25, which confirms the equal-g
  ridge-blocking inference verbatim); the CFA-PC ≡ m = 1 equal-ζ CIRCUM
  equivalence at φ_g = 0 recorded as the meeting point of the package's
  two model families; §12.2's ΔCFI TBT resolved by transcription
  (devel/cr2002-transcription.md, with the source's internal
  contradiction and scope caveats documented); Locke (2010) demoted to
  secondhand-via-Wendt; Moss (2026) magnitudes and estimand caution
  recorded for T5.
- 2026-07-07 — Wendt et al. discrepancy evaluation
  (devel/m5-wendt-discrepancies.md): every departure from the closest
  published neighbor assessed. One partially unjustified discrepancy fixed
  same-day — `ssm_sem()` now defaults to `estimator = "MLR"` and
  `print.circumplex_ssm_sem()` reports robust/scaled fit indices with
  fallback (the naive chi-square over-rejects on skewed octant data; the
  vcov the CI engines consume is verified bit-identical to the previous
  ML+sandwich default, so the recorded coverage evidence is unchanged).
  §3.2's note corrected (their CFA-QC did NOT consistently improve fit —
  the improvement claim was their citation of prior work); §12.6 records
  the identified-but-deferred "equal-g" middle tier their parameterization
  suggests; their replicated g–agency correlation (≈ −.3) recorded as the
  citable real-data magnitude of the scaled tier's φ_g = 0 violation.
  Search for other SEM-based circumplex/SSM work found no prior
  latent-level SSM estimand (nearest: Weide et al. 2021; Moss 2026 on
  disattenuated-correlation inference; the Browne/CSPM free-angle
  tradition) — T5 should position the layer as novel.
- 2026-07-07 — Post-T3 literature note (§3.2, References; no code, no
  numeric values taken): identified that the strict tier is, up to the
  documented .71 rescaling, Gurtman & Pincus's (2003) "perfect circumplex"
  CFA (the basis of Locke's 2010 standard IPC scoring weights), and that
  Wendt et al. (2019) fit exactly this model across four large samples
  alongside a less-restrictive dimensional variant, categorical, and hybrid
  alternatives — the fully dimensional model won throughout. Two results
  flagged for T5: their measured CFA-PC fit (RMSEA .075–.111) is a citable
  real-data magnitude for §10's model-conditional-approximation caveat, and
  their finding that relaxing perfect-circumplex constraints improved fit
  without losing validity independently supports the scaled tier as
  default. Not a validation oracle (their estimand is factor scores on the
  octants themselves, not an external measure's disattenuated profile).
- 2026-07-07 — T3 engine decision (§5.1, §8.3): coverage study run
  (devel/m5-coverage-oracle.R, seeded; results .rds committed alongside).
  `"mvn"` confirmed as the default engine with the sandwich-vcov amendment
  (`se = "robust.huber.white"` default in `ssm_sem()`): plain-ML mvn
  undercovered displacement (0.88, N-stable) only under the realism cell's
  misspecification; sandwich mvn restored 0.94–0.96 everywhere; boot arm
  comparable throughout. Full numbers in §5.1's ANSWERED block.
- 2026-07-07 — T3 identification amendment (§3.1, §4.2, §6.2, §12.3): the
  empirical local-identification check this spec required found the scaled
  tier's free g–plane covariances **locally unidentified exactly at
  φ_g = 0** (exact first-order ridge {a_i += δ·c_i·cosθ_i, φ_gx −= δ,
  σ_Mx −= δ·σ_Mg}; verified analytically and numerically — nlminb
  nonconvergence, SE(a_i) ≈ 200, MVN draws inadmissible along the ridge).
  Default flipped to φ_g = 0-fixed per §3.1's pre-decided fallback; no
  `free_g_plane` switch (the failure is at the null, not at small p);
  g-lean modeled via the strict tier; §4.2's g-lean fit channel scoped to
  strict; T4 must re-verify the multi-group non-reference free-φ_g block.
  Generator (`ssm_sem_syntax()`) and `sem_free_params()` updated (scaled
  free count 3p + 2 → 3p; df at p = 8, m = 0 now 12).
- 2026-07-07 — Fresh-session review revisions (all findings F1–F12 of
  `devel/m5-sem-design-review.md`, verdict ACCEPT WITH CHANGES): §4.3's
  direction claim corrected to the saturation-modulated first-harmonic
  framing with the reliability-modulation-removal improvement pinned (F1);
  latent-fit channels completed (F2); pseudo-true coverage target pinned
  for misspecified cells (F3); Θ→0 pin moved to population level (F4);
  multi-group factor metric pinned with per-group stationarity (F5);
  robust nested test, Var(M) handling, path-dependent invariance default,
  cov-metric `:=` naming, memory-file sharpening note, reflection
  indeterminacies (F6–F12). MILESTONES T2 acceptance amended per F9. Full
  mapping: "Revision log (vs the fresh-session review)".
- 2026-07-07 — Review revisions (same-day 4-angle review; 16 findings, all
  accepted). Statistical corrections: closed-form ≡ OLS characterized by the
  harmonic-balance condition rather than "iff equal spacing", with a
  verified structured-unequal counterexample and the T2 test re-aimed
  (§2.1/§3.5/§5.5); exact-cosine latent mean recovery restricted to the
  strict tier, with the scaled-tier leak documented and shown shared with
  the observed estimand (§2.1/§5.5/§6.4); plane isotropy/orthogonality
  reclassified from scaling convention to substantive stationarity
  assumption, making §4.3 conditional (§3.1); latent-fit prose corrected
  (removes attenuation heterogeneity, not sampling error; estimates still
  noisy) (§4.2); displacement delta-SE rate corrected to 1/a (§5.2).
  Consistency corrections: no printed SEs — the package's estimate+interval
  surface kept, deviation from Brief E's "printed SE" flagged (§5.2);
  inadmissible draws filtered engine-side, never routed through the
  degenerate-replicate warning (§4.5/§5.1); joint population draws via
  mvn_root per the shipped ssm_ci_accuracy path, cpm_simulate() recorded as
  scale-only (G2), and the T3 coverage harness defined as reusing the
  ssm_ci_accuracy machinery with the MILESTONES wording amendment flagged
  (§8.3); summary()/ssm_ci_accuracy() subclass hazards enumerated with a T3
  method audit (§7.3); include_defined = NULL auto with the weights
  invariant moved onto the always-present attribute (§3.5/§7.1/§12.5);
  fit-under-lavaan T2 test added (§3.5/§11); T4's side-by-side estimand
  documentation made a T4 deliverable (§6.1/§11); §7.1 editing artifact
  removed; strict tier's relation to circum_lavaan model0 stated as
  rescaling-equivalence only (§3.1/§3.2).
- 2026-07-07 — Initial spec (M5 T1, Fable). Estimand pinned as
  profile-then-transform on model-implied disattenuated profiles (§2, §4);
  OLS weights with the equal-spacing identity (§2.1); scaled vs strict
  fixed-angle model tiers (§3); raw-covariance fitting with standardization
  inside the estimand map, superseding the devel sketch's `std.ov` (§4.4);
  MVN-draws/bootstrap dual engine feeding `ssm_replicate_intervals()`
  verbatim (§5); adapted invariance ladder with an explicit non-comparison
  path (§6); API, validation, phasing, and open decisions (§7–§12).
