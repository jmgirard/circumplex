# RR09: Axes-reliability (Strack 2013) design GO/NO-GO — Review Report

- **Date:** 2026-07-23
- **Reviewer:** Independent expert review (Fable), per `cairn/reviews/RB09-axes-reliability-strack.md`
- **Design under review:** `devel/m53-axes-reliability-spec.md` (M53, design-only)
- **Verdict:** **GO** (conditional on the Binding criteria below)

Materials read: the spec; `cairn/references/strack2013.md`; the source PDF
`cairn/references/sources/strack2013.pdf` pp. 2–8 (Figure 2, Tables 1–3,
Figure 3 read directly); `R/ssm_sem.R` (lines 94–160, 690–780);
`R/fit_structure.R` (lines 1–67, `paf2`); `R/ssm_sem_syntax.R` (lines 140–180,
`snap_trig`); `R/convenience_functions.R` (lines 33–35, `octants()`);
`cairn/references/hubert1987.md` / `tracey1997.md` context via the source note.
I additionally ran three independent numerical checks (reported inline):
the Layer-A Spearman–Brown/SEm reproduction, a design-matrix rank computation
for identification, and a population-matrix lavaan fit of the exact proposed
parameterization.

---

## 1. Model faithfulness (spec §2)

**Faithful, with one structural note the build must carry into its syntax
documentation.**

Checked against Figure 2 (p. 4) and pp. 3–4 of the PDF directly:

- **Axes.** Two latents forced orthogonal, variances constrained equal
  ("Y_axis = ξ1 = X_axis", p. 4), fixed cosine loadings (type-a
  `0, ±.707, ±1.0`, p. 3; Figure 2 right side shows exactly these on
  PA/BC/DE/LM/NO), fixed-links per Schweizer 2010 (p. 3). Spec §2 states all
  of this correctly. ✔
- **General factor.** +1 loadings on all items; "uniformly positive
  intercorrelation … response style … and/or a meaningful intensity" (p. 4). ✔
- **Scale-specificity.** One orthogonal latent per scale, +1 item loadings,
  all variances set equal ("The model sets the variances of all orthogonal
  scale-specificity sources as equal", p. 4). ✔
- **Block-specificity.** Blockwise instruments only (CSIV, TRC, OCAI; Table 1);
  correctly declared absent for the package's non-blocked instruments. ✔
- **Errors free.** "In tau-equivalent models, the errors stay free" (p. 3). ✔
- **Free-parameter count.** "besides the free errors: 2, 3, or 4 parameters"
  (p. 5). ✔ (see Q3 for the exact mapping: 2 = {ξ1, ξ2} for single-item-per-
  position instruments, 3 = + ζ1, 4 = + ζ2.)

**Structural note (not a misstatement, but must be documented):** Figure 2 is
drawn *hierarchically* — items load +1 on their scale latent; the scale latent
receives the fixed cosine paths from the axes plus a ζ1-variance disturbance;
in the blocked case the general factor likewise reaches items through +1 block
paths. The spec §2 renders it *flat* (items load directly on axes with cosine
weights, directly on a general factor, directly on scale-specificity latents).
The two are covariance-equivalent **because every intermediate path is fixed
(+1 or the cosine)** — the product of fixed paths equals the flat fixed
loading, and the disturbance becomes the specificity latent. The flat form is
the right one to implement (simpler lavaan syntax, identical fit); the build's
syntax generator should state the equivalence in a comment so a future reader
comparing against Figure 2 does not think the model differs.

One spec-text nit: §2 cites "Type-a weights … (p. 3–4)"; the type-b weights and
the Σwᵢ² = 4.00 statement are on p. 3. Anchors in `strack2013.md` are correct.

## 2. F-1: error freedom vs. the sum-to-100% identity

**The errors are genuinely free parameters (one per item); the sum-to-100%
identity is an emergent property of the fit, not a constraint to impose.
"Tau-equivalent, errors free" is compatible with fitting a correlation matrix
in the treat-as-covariance sense — exactly what the paper's LISREL ML run did
— with a documented SE caveat.**

Reasoning. The model-implied moments are *linear* in the component variances:

- Off-diagonal (items i ≠ j):
  `σ_ij = ξ1·cos(θ_i − θ_j) + ξ2 + ζ1·1{same scale} (+ ζ2·1{same block})`,
  using `w_xi·w_xj + w_yi·w_yj = cos(θ_i − θ_j)` — this identity is exactly
  why the equal-axis-variance restriction makes the axes enter as a single ξ1.
- Diagonal: `σ_ii = ξ1·(w_xi² + w_yi²) + ξ2 + ζ1 (+ ζ2) + ε_i
  = ξ1 + ξ2 + ζ1 (+ ζ2) + ε_i`, since `cos² + sin² = 1` for every θ.

So the implied *common* variance is the same constant for every item, and each
free ε_i appears in exactly one moment (its own diagonal). Fitting the sample
correlation matrix (unit diagonal), the ε_i give the model one parameter per
diagonal moment; the ML solution reproduces the unit diagonal essentially
exactly and every ε̂_i collapses to the common value `1 − (ξ1+ξ2+ζ1+ζ2)` —
the sum-to-100% identity of the Figure 2 note. Two precision points:

- With *fixed* loadings, the classical "ML reproduces the diagonal exactly"
  theorem of ML factor analysis (which needs the free-loadings stationarity
  condition) does not strictly apply; the reproduction is near-exact rather
  than a theorem. Table 3's component rows sum to 99.9–100.0, consistent with
  this. For a population-matrix input the fit is exact and the identity holds
  to numerical tolerance — a good test (BC5).
- I verified this empirically: fitting the exact proposed parameterization to
  a population matrix built from (ξ1=.15, ξ2=.20, ζ1=.10, ε=.55), all 16 ε̂_i
  returned identical values and χ² = 0 (see Q3 for the fit details).

**Exact constraint set a correct lavaan model must impose** (16-item octant
example; p items, 8 scales, non-blocked):

1. `AX =~ w_x1*it1 + … + w_xp*itp`, `AY =~ w_y1*it1 + … + w_yp*itp` — all
   loadings **fixed** at the snapped cosines (Q5); never `std.lv = TRUE`.
2. `G =~ 1*it1 + … + 1*itp` — fixed +1.
3. `S_k =~ 1*(items of scale k)` for each scale — fixed +1.
4. Variances: `AX ~~ v1*AX`, `AY ~~ v1*AY` (equality label = ξ1);
   `G ~~ v2*G` (ξ2); `S_k ~~ vs*S_k` for all k (shared label = ζ1).
5. **All latent covariances fixed to zero** — every pair among
   {AX, AY, G, S_1…S_8}. This is load-bearing: `lavaan::cfa()`'s default
   (`auto.cov.lv.x = TRUE`) *frees* exogenous latent covariances;
   `orthogonal = TRUE` (or explicit `~~ 0*` lines) is mandatory (BC4).
6. Item error variances: free per item (lavaan default). No mean structure.
7. Input: the item correlation matrix as `sample.cov` with `sample.nobs = N`
   (or standardized raw data through a chokepoint patterned on
   `sem_fit_cfa()`, `R/ssm_sem.R:736–757`).

**Correlation- vs covariance-matrix caveat (document, don't fix):** analyzing
a correlation matrix as if it were a covariance matrix yields correct point
estimates here but approximate SEs/χ², because this model is not
scale-invariant (fixed unit and cosine loadings) — the classic Cudeck (1989)
issue. The paper's own LISREL SEs (Table 3 col 6 SE) carry the same
approximation, so this is *faithful*; the docs must state it rather than
silently print SEs as exact. Note also lavaan's `likelihood = "normal"`
default rescales `sample.cov` by (N−1)/N — my population fit recovered
.14985 for a true .15 at nobs = 1000 for exactly this reason; oracle tests
must either use `likelihood = "wishart"` or build the rescaling into their
tolerance (BC5).

## 3. F-2: identification

**Identified — and robustly so, because the covariance structure is linear in
the parameters, making identification a rank condition on a fixed design
matrix, independent of parameter values.** This is a stronger situation than
typical CFA identification.

Write each off-diagonal moment as a linear function of (ξ1, ξ2, ζ1) with
design vector `(cos Δ_ij, 1, 1{same scale})`. For the octant geometry the
distinct rows include `(1,1,1)` (same-scale pairs), `(.707,1,0)`, `(0,1,0)`,
`(−.707,1,0)`, `(−1,1,0)` — rank 3. I computed the rank numerically for a
2-item-per-scale octant instrument: **rank 3 with ≥2 items per scale; rank
drops to 2 with 1 item per scale** (no same-scale pairs ⇒ ζ1 unidentified).
The paper is consistent: Table 3 prints "—" for %scale-specificity for COC and
SYMLOG (single item per spatial position; the "2-parameter" models of p. 5).
Free-parameter counting for the MVP (non-blocked, ≥2 items/scale):
`df = p(p+1)/2 − p − 3`. Verified empirically: my population-matrix lavaan fit
of the 16-item model converged with df = 117 = 136 − 16 − 3, χ² = 0, and
recovered (ξ1, ξ2, ζ1) = (.15, .20, .10) exactly (up to the (N−1)/N
rescaling noted in Q2).

**Failure modes as a component variance → 0.** Because identification does not
depend on parameter values, a true component of 0 causes *no* empirical
underidentification and no rank collapse — the failure mode is purely a
**boundary/Heywood** one: ML with unbounded variances can return a slightly
negative ξ̂1, ξ̂2, or ζ̂1 (e.g., an instrument with no general factor). The
estimator must:

- **Detect:** check every estimated variance (including ε̂_i) for negativity
  (and lavaan's post-fit checks / convergence flag; non-convergence →
  informative error).
- **Report:** a negative or zero ξ̂1 makes the Spearman–Brown reliability
  meaningless — report the component estimates with a boundary flag and set
  Reliability/SEm to `NA` with a warning, per the package's degeneracy
  precedent (`paf2()`'s NA-return on undefined input, `R/fit_structure.R:16–28`
  — refuse/NA, never coerce to 0 or clip silently). A *small positive* ξ̂1 is
  legitimate and must flow through (the paper reports COC = .19).

The remaining practical failure mode is an ill-conditioned **sample** matrix:
ML needs a positive-definite S, so N ≤ p (64 items needs N > 64) or a
singular/NA correlation matrix must be refused up front (Q7).

## 4. F-3: the equal-axis-variance restriction and spacing

**Forcing Var(X) = Var(Y) = ξ1 is correct for the package's octant
instruments** — it is the paper's encoding of the no-preferred-rotation axiom
("For a perfect circle, the variance of both axes are set equal", p. 4), and
for equally spaced octants the data cannot prefer a rotation, so the
restriction is substantively innocuous and is what makes the axes enter the
decomposition as the single ξ1 (Q2). Two clarifications the spec should absorb:

- The *per-item* identity `w_x² + w_y² = 1` holds for **any** θ (it is
  cos² + sin², not a spacing fact). What unequal spacing breaks is the
  substantive innocuousness of the equal-variance restriction and the paper's
  validated scope — Strack et al. explicitly excluded quasi-circumplex
  instruments (p. 5: Schwartz value circle "were not included"). Refusing
  unequal spacing in the MVP is therefore scope-correct, not merely cautious.
- Balance matters too: with unequal items per scale, item_n differs by axis.
  The build should compute item_n **per axis** as Σwᵢ² over that axis's items
  (Table 3 col 10 is per-axis and even fractional for SYMLOG, 8.67), which
  degrades gracefully; for the balanced MVP both axes give the same value
  (verified: Σw_x² = Σw_y² = 4 × items-per-scale for octants).

**The refuse contract must reject (MVP):**

1. Scale count ≠ 8 (this subsumes odd counts; hexagonal type c etc. are
   deferred-in-spec).
2. Angle multiset not equal to {45, 90, 135, 180, 225, 270, 315, 360} **modulo
   360** — the check must be convention-aware: `octants()` returns LM = 360
   (`R/convenience_functions.R:33–35`), so 0 and 360 must be treated as the
   same position, and the equal-spacing test must be modular (a naive
   sorted-diff that treats 360 as distinct from 0 is the classic pole bug).
3. Missing/NA angles; duplicate angles (two scales at one position is not the
   deferred type-e single-item case, it is a malformed instrument).
4. < 2 items on any scale (ζ1 identification, Q3) — the single-item-per-
   position type-e/f path (COC/SYMLOG, ζ1 dropped) is deferred, not silently
   approximated.
5. Items unmapped to scales / absent from the data.

Each refusal must be an informative error naming the offending scale/angle —
never silent re-weighting, per the spec's own M18 lesson (§5).

## 5. F-4: weight orientation vs. the angle convention

**The spec's `cos(θ − α)` with axes at α and α + 90° is correct and
sign-consistent with both the paper and the package.** With the package's
convention (communion/X at α = 0, agency/Y at 90 — matching `ssm_sem`'s
`cx`/`cy`), the weights are `w_x = cos θ`, `w_y = cos(θ − 90°) = sin θ`. This
is exactly the basis the existing SEM layer uses
(`R/ssm_sem_syntax.R:348–349`: `co <- fmt(snap_trig(cos(th)))`,
`si <- fmt(snap_trig(sin(th)))`; curve evaluation `R/ssm_sem.R:310`). Checked
against Figure 2: PA (90°) → Y +1.00, X 0; BC (135°) → Y +.707, X −.707;
LM (360°) → X +1.00; NO (45°) → +.707 on both. No sign or reference-axis error
found in the spec.

**The boundary case a test must pin:** LM at 360° (the LM=360 invariant). In
double precision `sin(2π) ≈ −2.4e−16` and `cos(π/2) ≈ 6.1e−17` — not 0. The
package already solved this: `snap_trig()` (`R/ssm_sem_syntax.R:160–165`)
snaps exact-zero/±1 loadings, with the comment explicitly noting a 90° scale's
cx loading "IS 0, not 6e-17" and that unsnapped noise is not byte-portable
across platforms' libm. The build **must** route its weights through
`snap_trig` (or an extracted shared helper). Tests must pin: θ = 360 →
(w_x, w_y) = (+1, 0) *exactly*; θ = 90 → (0, +1) exactly; θ = 0 and θ = 360
produce identical weights; per-axis Σw² = 4 × items-per-scale exactly. Output
labeling should name the axes by angle (X = 0°, Y = 90°) or construct name,
never bare "axis 1/2".

## 6. Oracle sufficiency (spec §4)

**Adequate for GO, with one overstatement to correct and two strengthenings
required.**

**(a) Layer A is a legitimate published-value oracle — not an identity.** It
tests three real things against printed values the implementation does not
control: (i) the Spearman–Brown implementation, (ii) the item_n = Σwᵢ²
derivation from instrument structure (col 10 is independently computable:
64-item octant → 32; 16-item, 4 scales at poles → 8), and (iii) the SEm
formula against cols 11–13. I reproduced all four anchors from the printed
Table 3 values myself: IAL .260/32 → .9183 → .92; IPI-A .134/16 → .7123 → .71;
COC .028/8 → .1873 → .19; OCAI .117/8 → .5146 → .51 — all match col 11. SEm:
IAL √.98·√.08 = .280 → col 13 .28 ✔; COC √6.70·√.81 = 2.33 ✔; OCAI 2.796 vs
printed 2.78 (within input-rounding propagation, which is why BC2's tolerance
is ±.02). It validates only the formula layer — which is all §4 claims for it.

**However, §4 overstates one item: the N–B column (col 14) is NOT a usable
printed cross-check.** The Nunnally–Bernstein formula needs Rel_scaleᵢ (scale
alphas) and Var_axis, neither of which Table 3 prints — col 14 cannot be
recomputed from the paper's printed values. The N–B layer therefore currently
has **no oracle at all** in the spec and needs its own (BC8): an
independently-computed worked example (hand-computed alphas/axis variance on a
small synthetic dataset, computed once by the formula and once by an
independent route), plus the Layer-B directional cell below. This is the one
"cell whose expected outcome the spec states wrongly" in the sense of Q6's
prompt — a claimed check that does not exist.

**(b) Layer B meets the ≥2-independent-oracle-types bar,** with strengthening:

- *Synthetic recovery* (oracle = generating truth) and *cross-engine
  lavaan/OpenMx* (oracle = independent implementation) are genuinely
  independent types. ✔
- **Add a deterministic population-matrix cell** (BC5): build Σ exactly from
  known components and fit Σ itself — recovery must then be exact to numerical
  tolerance (no Monte-Carlo slack; my check above recovered all three
  components and χ² = 0). This is stronger than finite-sample recovery alone
  and is where the (N−1)/N likelihood-rescaling trap (Q2) gets pinned.
- The **failure-expecting high-scale-specificity cell** is correctly stated:
  per Figure 3 and p. 8 (MEIL, CV-LI: scale-specificity > 70%, axes < 30%),
  N–B overestimates while the CFA reliability stays honest. The oracle must
  assert the *gap direction and size*, not mere agreement — as specced. ✔
- A cheap third internal check falls out of Q3's linearity: OLS-regress the
  off-diagonal correlations on the design `(cos Δ, 1, same-scale)` — an
  estimator independent of all SEM machinery that must agree with ML closely
  on synthetic data (consider, not binding).

Global-fit context correctly demoted to non-oracle. ✔

## 7. API and refuse contract (spec §5)

**The standalone shape is right; endorse with amendments before the surface
freezes.** Standalone-vs-ssm_sem is a fixed plan-gate direction and is also
technically correct — this is an item-level tau-equivalent fixed-links model,
a different measurement model from `ssm_sem`'s scale-level SSM CFA; folding it
in would conflate two estimands. Parallel-to-`fit_structure()` is the right
idiom. Amendments:

1. **Per-axis outputs will be identical for the entire MVP scope** (equal ξ1
   by restriction, equal item_n by balance) — keep the per-axis structure
   (future-proof for unbalanced/deferred types) but the docs and print method
   must say why the two rows match, or users will file it as a bug.
2. **SEs carry the treat-corr-as-cov approximation** (Q2) — document on the
   output, matching the paper's own practice.
3. **SEm's SD choice** (z-standardized vs raw axis scores; Table 3 cols
   12–13) is a researcher choice — expose it as an explicit argument with a
   documented default, never a silent one.
4. **N–B needs raw item data** (scale alphas + axis-composite variance); it is
   unavailable on the deferred corr-matrix-only input path. Endorse deferring
   the secondary input; when it lands, N–B must be `NA`-with-reason there, not
   dropped silently.
5. **Missing-data policy must be pinned in the build spec:** recommend
   complete-case (listwise) with an informative message, refusing if the
   complete-case N ≤ p; do **not** use pairwise correlations (non-PD risk).
   FIML on items is a possible future extension via the `sem_fit_cfa` pattern,
   not MVP.
6. **Refuse list** = spec §5's list plus Q4's items 1–5 plus: non-PD or
   N ≤ p correlation matrix, zero-variance items (cor → NA; refuse with an
   informative error — the exported-function analogue of `paf2`'s NA policy),
   non-convergence. Boundary (negative-variance) fits are *reported* with
   NA reliability + warning, not refused (Q3).
7. **Return an S3 list-classed object** with print/summary in package idiom;
   irreversibility argues for a minimal exported surface (one function, one
   class) and keeping the secondary input and types b–f unexported until they
   ship.
8. Blockwise instruments: the package's instrument objects carry no block
   structure, so ζ2 is out of scope; the docs must state that a blockwise-
   administered instrument analyzed without ζ2 will fold block variance into
   general/scale components (as the paper's Table 3 shows ζ2 up to 6.7%).

Name: `axes_reliability()` is fine (echoes the paper's title); no reason to
relitigate.

## 8. GO/NO-GO

**GO.** The model as specified is a faithful (flat-form-equivalent) rendering
of Strack et al. Figure 2; identification of the octant MVP is not merely
plausible but provable from the linear structure and was verified numerically
(rank condition + exact population recovery, df = 117 as counted); the
Layer-A oracle is genuine and reproduces (verified on all four anchors); the
Layer-B plan meets the two-independent-oracle-types bar once the
population-matrix cell is added; the API shape is safe with the Q7
amendments. Nothing found rises to a fatal problem with the fixed plan-gate
directions. The one real spec defect — the phantom N–B printed cross-check —
is repairable by BC8 and does not gate the direction.

Load-bearing holdings the build must honor: the Binding criteria below,
especially BC3 (orthogonality is not lavaan's default), BC5 (the (N−1)/N
trap), BC8 (N–B currently has no oracle), BC9 (snap_trig at the pole), and
BC11 (boundary policy: NA-with-warning, never a negative or clipped
reliability).

---

## Beyond the brief

- **B-1.** The linearity of the moment structure (Q3) means the whole
  estimator has a closed-form OLS shadow (regress off-diagonal r's on
  `(cos Δ, 1, same-scale)`). Beyond its oracle use (Q6), it is a cheap
  starting-values supplier and a diagnostic for "ML and the raw correlations
  disagree wildly" warnings. Consider, not binding.
- **B-2.** The paper's supplement ("a LISREL syntax is given in the supplement
  material," p. 4) would be a direct witness for the constraint set. Likely
  link-rotten (2013 SAGE supplement), but worth one retrieval attempt during
  the build; if found, bank it in `cairn/references/`.
- **B-3.** Docs synergy: `axes_reliability()` answers "how reliably does this
  instrument locate a profile on the axes," not "is this instrument a
  circumplex" — the docs should route the latter question to
  `fit_structure()`, and note the paper's own finding that RANDALL confounds
  the two components this model separates (r = −.788 with scale-specificity,
  p. 8; consistent with `tracey1997.md`).
- **B-4.** Example data: the package's bundled datasets are scale-level;
  item-level examples for the help page will need a bundled small item-level
  dataset or a simulated example. Flag for the build plan, not a design issue.

## Recommendations

1. **Apply.** Correct spec §4: strike the N–B col-14 "printed cross-check"
   claim; add the BC8 N–B oracle and the BC5 population-matrix cell.
2. **Apply.** Add the flat-vs-hierarchical equivalence note (Q1) to the spec
   and the generated-syntax comments.
3. **Apply.** Pin the lavaan parameterization exactly as the Q2 constraint
   set, including `orthogonal = TRUE` (or explicit zero covariances) and the
   likelihood-rescaling handling.
4. **Apply.** Extend the refuse contract with Q4's modular-angle check, the
   ≥2-items-per-scale requirement, and Q7's N ≤ p / non-PD / zero-variance
   refusals; pin the missing-data policy (listwise + message).
5. **Apply.** Boundary policy per Q3/BC11 (NA reliability + warning on
   ξ̂1 ≤ 0 or any negative variance; boundary flag in the output object).
6. **Apply.** Route weights through `snap_trig` (shared helper) and label axes
   by angle/construct in outputs; document the identical-per-axis rows and the
   corr-as-cov SE caveat.
7. **Consider.** The OLS shadow estimator as a third internal check and
   starting-values supplier (B-1); the supplement retrieval (B-2); a bundled
   item-level example dataset (B-4).
8. **Reject (with reason).** Constraining the item errors equal (they collapse
   numerically anyway): it would change the df and fit indices away from the
   paper's model class ("the errors stay free", p. 3) and break Layer-A/fit
   comparability for no gain.

## Binding criteria

Tolerances are absolute unless stated. "Must error" means an informative,
message-bearing error (never silent coercion or NA-without-warning).

- **BC1 (Layer A, reliability).** Spearman–Brown on Table 3's printed col 6
  (/100) and col 10 must reproduce col 11 for the four anchor rows (IAL S1
  Self; IPI-A S9 Self; OCAI S15 Self; COC S16 Self) within ±.005, and for
  every non-blocked type-a row of Table 3 within ±.01.
- **BC2 (Layer A, SEm).** `sqrt(col 12) · sqrt(1 − col 11)` must reproduce
  col 13 within ±.02 for at least the IAL, OCAI, and COC anchor rows.
- **BC3 (item_n).** item_n computed from instrument structure as the per-axis
  Σwᵢ² must be **exact** (after snapping): 64-item octant → 32; 32-item → 16;
  16-item → 8; and equal across the two axes for every balanced octant
  instrument.
- **BC4 (constraint set).** A test must assert, on the fitted lavaan object:
  all loadings fixed (zero free loadings); AX/AY variances equality-
  constrained; all scale-specificity variances share one label; **every**
  latent covariance fixed at 0; item errors free; and
  `df = p(p+1)/2 − p − 3` for the non-blocked MVP model.
- **BC5 (population recovery).** Fitting the exact population matrix built
  from known (ξ1, ξ2, ζ1, ε) for an octant instrument must recover every
  component within 1e−4 and give χ² < 1e−6, with the lavaan
  (N−1)/N likelihood rescaling explicitly handled (wishart likelihood or
  corrected expectation); all ε̂_i must be equal within 1e−6.
- **BC6 (finite-sample recovery).** ≥2 Monte-Carlo cells (distinct ξ1 levels)
  where the mean ξ̂1 across replicates is within 2 Monte-Carlo SEs of truth.
- **BC7 (cross-engine).** lavaan and OpenMx fits of the identical model on
  identical input must agree on all free component variances within 1e−3
  (expected agreement ~1e−5) on ≥2 datasets; the test skips (not passes)
  when OpenMx is unavailable; no new Imports (D-006/D-014).
- **BC8 (N–B oracle).** The Nunnally–Bernstein implementation must pass an
  oracle that does not share its code path: a worked example whose
  Rel_scaleᵢ, Σwᵢ², and Var_axis are computed independently (by hand or by
  an independent route in the test), agreeing within 1e−6; Table 3 col 14
  must **not** be cited as its oracle.
- **BC9 (N–B direction cell).** A synthetic high-scale-specificity cell
  (scale-specificity ≥ .40 of item variance, axes ≤ .15) where the test
  asserts `NB_reliability − CFA_reliability > 0` with a pre-registered margin
  (≥ .05), reproducing the paper's Figure 3 headline.
- **BC10 (pole/boundary weights).** Tests must assert: θ = 360 → weights
  exactly (+1, 0); θ = 90 → exactly (0, +1); θ = 0 and θ = 360 yield
  identical weights; weights pass through the snapping helper (no 1e−16
  residue in emitted syntax).
- **BC11 (boundary fits).** ξ̂1 ≤ 0, or any negative estimated variance, must
  yield reliability/SEm = NA plus a warning and a boundary flag in the output
  — never a negative, clipped, or silently-zeroed reliability; a small
  positive ξ̂1 (e.g. .03) must flow through to a small reliability (COC-style
  .19), not be treated as degenerate.
- **BC12 (refuse contract).** Each of the following must error informatively:
  scale count ≠ 8; angle multiset ≠ octants() mod 360 (including an
  unequal-spacing case and a duplicate-angle case); NA angle; any scale with
  < 2 items; item in the instrument map absent from the data; non-finite
  values in the data; zero-variance item; complete-case N ≤ p or non-PD
  correlation matrix; lavaan non-convergence.
- **BC13 (missing-data policy).** The chosen policy (recommended: listwise
  with an informative message reporting the complete-case N) must be
  documented and tested, including the refusal when complete-case N ≤ p;
  pairwise correlation input must not occur.

## Conclusion

**GO** — build `axes_reliability()` with the Layer-A/Layer-B oracles in
v2.0.0, subject to BC1–BC13 above. The design is a faithful and identified
rendering of Strack, Jacobs & Grosse Holtforth (2013); its one oracle gap
(N–B) and its lavaan-parameterization traps (orthogonality default,
likelihood rescaling, pole snapping) are all closed by the binding criteria.
