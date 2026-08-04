# RR15: Metric pricing of the corrected component SEs and the FIML ratio (M69)

- **Date:** 2026-08-03
- **Reviews:** RB15 (`cairn/reviews/RB15-axes-reliability-fiml-ratio-pricing.md`)
- **Materials read:** `R/axes_corrected_se.R` (whole file),
  `R/axes_scaled_fit.R:1-178`, `R/axes_reliability.R:1600-1820` and the fit
  helpers at `:320-345`/`:1420-1440`, `tests/testthat/test-axes-corrected-se.R`
  (whole file), RR13's binding criteria and reproduction appendix
  (`cairn/reviews/archive/RR13-axes-reliability-se-calibration.md`),
  `cairn/milestones/M69-axes-corrected-se-unit-diagonal.md`.
- **Fresh measurements taken for this review** (canonical octant probe, 8
  scales x 3 items, xi1 = .35, xi2 = .10, zeta1 = .08, n = 600, lavaan 0.6-21;
  components ordered xi1/xi2/zeta1 throughout):
  - `diag(Sigma-hat)` = 0.9983333 constant, = (N-1)/N exactly. Confirms the
    brief.
  - Repricing only `corrected` at `cov2cor(Sigma-hat)`: relative SE changes
    +1.0498e-3 / +1.6154e-3 / +1.7209e-3. Confirms the brief's +1.05e-3 etc.
  - Repricing `naive` the same way: exactly +1.669443e-3 = N/(N-1) - 1 on all
    three components. Confirms the brief's scalar (its 1.6694e-3).
  - Ratio `corrected/naive` under the three pricings (xi1 / xi2 / zeta1):
    both-raw 0.6942818 / 0.9369649 / 1.0031293; mixed (corrected at cov2cor,
    naive raw) 0.6950106 / 0.9384785 / 1.0048546; both-cov2cor 0.6938522 /
    0.9369143 / 1.0031815. **mixed / both-cov2cor = 1.0016694 = N/(N-1) on
    every component** — see Q2 for the direction finding.
  - Scaling Sigma-hat by 2: `naive` SEs scale by exactly 2.000000;
    `corrected` SEs scale by 1.538 / 2.009 / 2.114. The corrected branch is
    **not homogeneous** in the input matrix.
  - `cov2cor(D Sigma D)` for a random positive diagonal D (entries
    exp(U[-0.3, 0.3])): corrected SEs agree with the un-rescaled values to
    4.4e-16 relative; floating-point drift of `cov2cor(a*S)` vs `cov2cor(S)`
    measured 2.2e-16.
  - One shipped-path FIML fit (seed-1001 draw of
    `axes_simulate(600, octants(), 3, .35, .10, .08)`, 5% cellwise MCAR,
    z-scored, `axes_fiml_em_args()`): `diag(fitted(fit)$cov)` ranges
    **0.9433 to 1.0723, sd 0.0303**. The FIML path's fitted diagonal is
    nowhere near a constant (N-1)/N.

**Verdict in one paragraph.** M69's premise is correct and M68-D2 was right:
the corrected branch's formula is only the derived estimator variance at a
unit-diagonal matrix, so it must be priced at `cov2cor(Sigma-hat)`. The two
sides of the FIML ratio must share that matrix; the mixed-matrix ratio is not
metric-only, carrying a residual factor of **N/(N-1) — inflation, not the
brief's claimed (N-1)/N shrinkage** (the brief's direction claim is inverted;
finding B1). The third-return-value design is sound with one refinement:
return the ratio itself, not the bare denominator. RR13's BC4 must be
explicitly superseded (its "evaluated at Sigma-hat" no longer describes the
computation), but its rationale — the missing-information pricing lives in the
observed-information factor and survives untouched. The regression pin should
be invariance to positive *diagonal* rescaling of Sigma-hat, which is exact
and strictly sharper than scalar invariance.

## Answers

### 1. Is M69's premise correct? — Yes, from the estimand itself.

The corrected branch's estimand (RR13 BC1) is the delta-method variance of
components estimated from a **sample correlation matrix**: the Wc fold
`diag(wc)_i = -sum_{j!=i} W_ij Sigma_ij` is the compressed form of the
standardization differential

    dr_ij = ds_ij - 0.5 * rho_ij * (ds_ii/sigma_ii + ds_jj/sigma_jj)  (scaled),

and the compression `Sigma_ij = rho_ij` (so that the fold can use `Sigma_ij`
directly) holds **only when the diagonal is exactly 1**. Evaluated at a matrix
with diagonal c != 1, the fold weights by c*rho where rho belongs, and the
result is no longer the derived quantity at any scale. The decisive
measurement is non-homogeneity: multiplying the input by 2 multiplies the
three corrected SEs by 1.538 / 2.009 / 2.114 where any coherent
variance-metric quantity would give exactly 2. So `corrected` at the raw
Sigma-hat (diagonal 0.99833) is not "the right answer in a rescaled metric" —
it is a third quantity, close to the right one only because c is close to 1.
Under misspecification, where the diagonal is not even constant (M68's
0.951-1.026; my FIML measurement 0.943-1.072), there is no scale in which the
raw evaluation is the derived formula.

Independent confirmation from the repository's own record: **RR13's
reproduction appendix derives both `naive` and `actual` at the unit-diagonal
population matrix P** (`ratio_fn` builds `P` with `diag(P) <- 1` and never
rescales). The published anchors 0.01677/0.01164 and the constant 1.441229 are
unit-diagonal quantities. The shipped raw-Sigma-hat evaluation was plug-in
drift from RR13's own derivation, not a choice RR13 made. Repricing at
`cov2cor(Sigma-hat)` is therefore not "consistency with M68" — it is fidelity
to the estimand RR13 derived and this suite's anchors already encode.

**M68-D2 is affirmed on the same ground**, not merely left standing: pricing
Gamma_R's `(1 - rho^2)^2` entries requires actual correlations, and the
normalization is the unique evaluation point at which the derivations on both
surfaces are the formulas they claim to be.

One honest residual, so it is chosen rather than overlooked: lavaan's
`sample.cov.rescale` shrinks the reported **point estimates** by (N-1)/N, so
the finite-sample SD of the reported number carries a factor (N-1)/N that the
unit-diagonal pricing drops. That is an O(1/N) discrepancy (0.17% at n = 600)
in a formula that is itself only asymptotic (the n-vs-(n-1) divisor ambiguity
is the same order), far below the [0.90, 1.10] calibration instrument that
arbitrates such choices. The unit-diagonal convention is preferred because it
is the only one defined under misspecification and the only scale-free one.

### 2. Should the two sides of the FIML ratio share a matrix? — Yes; and the brief's direction claim is inverted.

**The property the ratio must have:** it is documented (and BC4-rationalized)
as a *metric-only* conversion — a dimensionless per-parameter factor that
depends only on the fit's estimated correlation structure, so that
`se_uncorrected * ratio` changes the metric of the observed-information SE and
nothing else. A quantity with that property must be invariant to the scale
indeterminacy of Sigma-hat — scalar and positive-diagonal rescalings, which
carry zero correlation information. Same-matrix pricing at
`cov2cor(Sigma-hat)` has that invariance exactly (measured 4.4e-16). The
mixed-matrix ratio does not.

**Direction finding (contradicts the brief).** The brief states the mixed
ratio "acquires a factor of approximately (N-1)/N, systematically shrinking
every FIML standard error." Measured, the mixed ratio equals the same-matrix
ratio times **N/(N-1) = 1.0016694 exactly, on every component** — the
numerator is repriced *upward* to the unit-diagonal metric while the
denominator keeps the (N-1)/N shrink, so the reported FIML SE would be
systematically **inflated**, by ~0.17% at n = 600 and ~1% at n = 100. The
brief's magnitudes are right and its algebraic diagnosis (a mixed ratio is not
metric-only) is right; the sign of the artifact is inverted. Conservative
direction or not, it fails the stated property, and two further measurements
close the question:

- On the **FIML path — the only path that uses the ratio** — the fitted
  diagonal is not (N-1)/N at all: measured 0.9433-1.0723 (sd 0.030) on a
  shipped-path 5% MCAR fit. The mixed-ratio contamination there is an
  item-profile-dependent factor, not a clean scalar: it cannot be pinned by a
  test, stated in a doc, or bounded by the (N-1)/N story. "Genuinely correct"
  is unavailable and "genuinely harmless" is unverifiable.
- The shipped both-raw ratio is *already* nearly metric-only — it differs from
  the same-matrix ratio by at most 6.2e-4 on the probe (the (N-1)/N is
  common-mode and cancels; the residue is the corrected branch's
  non-homogeneity). Mixed pricing would therefore be a **regression relative
  to shipped code** on the FIML surface, injecting a 1.7e-3 artifact where
  6.2e-4 stood. This alone rules out shipping M69 with the mixed ratio.

### 3. Which matrix, and how to organize the code — cov2cor for both sides; the third-value design is sound, but return the ratio, not the denominator.

Both ratio sides at `cov2cor(Sigma-hat)`. The proposed organization (a third
returned value: the normal-theory variance at `cov2cor(Sigma-hat)`, used only
as the ratio denominator, with `naive` kept at the raw matrix) is correct and
compatible with every constraint. Assessment and refinements:

- **Keep `naive` at raw Sigma-hat.** The 1e-7 fences at
  `test-axes-corrected-se.R:67-69` and `:191-194` are the only independent tie
  of the derivative set {C, J, B, K, E_ii} and the information inversion to
  lavaan's own implementation; normalizing `naive` internally would move it by
  1.7e-3 relative and destroy them. (AC2's vech oracle then fences the
  cov2cor-side evaluation, so both pricing points are independently checked.)
- **Return the composed per-component ratio** (e.g. `fiml_ratio =`
  corrected SE / normal-theory SE, both at `cov2cor(Sigma-hat)`) rather than —
  or in addition to — the bare denominator, and have
  `R/axes_reliability.R:1691` consume `corrected$fiml_ratio` directly. With a
  bare `naive_cor` in the return, the mixed ratio
  `corrected$corrected / corrected$naive` remains one plausible-looking
  expression away at every future call site; returning the ratio makes the
  same-matrix invariant a property of the helper, where it can be tested once.
- **One realignment, then two pricings.** Realign off dimnames once; refuse
  `any(diag(sigma) <= 0)` with a named reason **before** `cov2cor()` (the
  `axes_scaling_factor()` precedent at `R/axes_scaled_fit.R:103` —
  `axes_corrected_se()` currently has no such guard and `cov2cor` of a
  nonpositive diagonal produces NaN, see B2); then run the
  solve/info/acov/W pipeline at the raw matrix (naive) and at the normalized
  matrix (corrected + ratio). The duplicated linear algebra is 24x24 with
  q ~ 28 — negligible.
- **Failure contract:** `naive`, `corrected`, and `fiml_ratio` are NA together
  under one `reason`, whichever pricing point failed; no partial success. This
  extends the existing contract unchanged.
- **Rejected alternative — two helper calls from the caller** (once raw, once
  cov2cor): works arithmetically, but doubles the call-site surface, moves the
  pricing decision to the caller, and leaves the invariant untestable at the
  helper. The invariant belongs inside the function.
- The header must state which matrix each returned value is priced at; AC5's
  parsed-range guard then keeps the cross-file citation honest.

### 4. RR13 BC4 must be explicitly superseded; its rationale survives intact.

BC4's operative phrase — "the same per-parameter ratio **evaluated at
Sigma-hat**" — will no longer describe the computation once the ratio is
evaluated at `cov2cor(Sigma-hat)`. Under this repository's process that is a
superseding event, not a re-satisfaction: record a superseding criterion (BC4'
below) citing this report, exactly as D-035 superseded RR09 section 2.

What survives, stated so the supersession is not read as a retreat: the
**missing-information pricing is untouched**. It lives entirely in the
`se_uncorrected` factor (lavaan's FIML observed-information SE); the ratio
remains a per-fit quantity — `cov2cor(Sigma-hat)` differs across fits, so the
anti-constant test at `test-axes-corrected-se.R:284` remains meaningful and
green — and repricing moves the ratio by ~1.7e-3, not by anything that could
re-introduce or double-price missingness. The composition's architecture is
unchanged; only the evaluation point of the metric factor moves, to the matrix
RR13's own appendix derived it at. Fittingly, the same-matrix ratio *restores*
agreement with RR13's published constant: 1/0.6938522 = 1.44124 against the
appendix's 1.441229, where the shipped both-raw ratio gives 1.44034.

BC4's numeric band ([0.90, 1.10] at 2/5/10% MCAR) is projected to re-verify
trivially — the reported FIML SE moves by well under 1% at the fixture's
N = 600 (measured -0.062% on the probe's xi1) against a band 10% wide — but
the re-run is the evidence; the projection is not (BC4' below). The
adjacent pins at `:296-297` (mean live ratio above 1.4412, within 5%) are
projected to stay green: the live inverse ratio moves from ~1.450 by a factor
of ~1.0006.

### 5. The regression pin: exact invariance to positive diagonal rescaling.

The right property is **stronger than the brief's scalar proposal**: under
same-matrix pricing, `corrected` and `fiml_ratio` are invariant to
`Sigma-hat -> D Sigma-hat D` for any positive diagonal D, of which the scalar
a*I is the special case. It is **exactly** true (cov2cor is an exact
retraction onto unit-diagonal matrices; `se_uncorrected` does not depend on
Sigma-hat), up to floating point measured at 4.4e-16.

Diagonal invariance is worth the extra generality for a discrimination reason:
a scalar-only pin stays green under a "divide by the mean diagonal" or
"divide by (N-1)/N" pseudo-fix, which M68's non-constant-diagonal measurement
(and my FIML measurement) shows is materially wrong on real fits. Diagonal
invariance is the property only `cov2cor` delivers.

Two companions make the pin complete: (a) `naive` under `2 * Sigma-hat` scales
by exactly 2 — pinning that the lavaan-fence matrix stayed raw, so the
invariance of the other two is not the trivial consequence of normalizing
everything; (b) a wiring assertion that the reported FIML SE equals
`se_uncorrected * fiml_ratio` component-wise, so the invariant quantity is the
one actually shipped. Tolerance 1e-6 relative, derived: the raw/mixed
alternative violates the invariance by O(1) factors (measured 1.538-2.114 at
a = 2), six orders above; the measured floating-point drift is 4.4e-16 and the
repository's worst observed instrumentation drift is 1.3e-8, both at least two
orders below. Do not assert bit-identity.

Since mixed pricing is rejected, no (N-1)/N factor pin is needed — and Q2's
FIML-diagonal measurement shows none would be *possible*: on the path that
uses the ratio the mixed factor is not a function of N alone.

### 6. The `n` divisor is right; no double counting.

The divisor and the normalization operate on different objects: `n` is the
asymptotic 1/n of the delta method, while `cov2cor` removes the diagonal scale
of the **plug-in matrix** — it does not divide anything by a sample-size
factor. Concretely: repricing multiplies the corrected variance by
~(N/(N-1))^2 through the matrix entries, and the divisor stays n; nothing is
applied twice. On the FIML path the divisor cancels in the ratio entirely
(both sides carry the same /n), so it is irrelevant there.

What remains is the inherent O(1/N) ambiguity of any asymptotic SE (n vs
n - 1, and the point-estimate shrinkage noted in Q1), which the formula does
not resolve at its own order and the calibration bands arbitrate. Changing the
divisor to n - 1 to "match" would be exactly the kind of untethered
finite-sample tinkering the bands exist to referee — leave it alone.

### 7. The non-FIML paths: repricing plus one new guard suffices.

`grep -rn "axes_corrected_se(" R/` returns one production call site
(`R/axes_reliability.R:1679`); the listwise and cormat paths consume
`corrected$corrected` only. For them the repricing is the whole fix — the
corrected SEs move by +1.0e-3 to +1.7e-3 relative at n = 600, and AC3's
calibration re-run plus the prose sweep already cover the consequences. The
only new code they need is the shared nonpositive-diagonal refusal before
`cov2cor()` (B2), which must land in the common helper, not per-path. The
already-enumerated fallout (re-pins at `test-axes-corrected-se.R:203-204` —
note `:204` *will* redden: the 0.9978 pin moves ~1.7e-3 against a 1e-3
tolerance — fixture regeneration, AC5's citation repair) is correctly scoped
in M69; nothing further surfaced.

## Beyond the brief

- **B1 (direction error in the brief and in M69's own prose).** The mixed
  ratio inflates the FIML SE by N/(N-1); it does not shrink it by (N-1)/N.
  The brief's "systematically shrinking every FIML standard error" and the
  M69 milestone's T1 wording ("always shrinking the SE") and AC4 wording
  ("the recorded (N-1)/N factor") all state the inverted direction. Any test,
  doc, or decision text built from that wording would pin the wrong sign.
  Correct both files when this RR is ingested (BC6).
- **B2 (missing guard).** `axes_corrected_se()` has no
  `any(diag(sigma) <= 0)` refusal. Today that is latent; once `cov2cor()`
  enters the function it becomes reachable (NaN row/column from a nonpositive
  diagonal), and the failure would surface as `reason = "indefinite"` or as
  raw NaN rather than as the honest refusal `axes_scaled_fit.R:103` gives the
  sibling surface. Add the guard before normalization, with the shared
  NA-together contract (BC5).
- **B3 (new measurement worth recording).** The FIML path's fitted diagonal
  on a shipped-path 5% MCAR fit ranges 0.943-1.072 (sd 0.030) — the first
  measurement of this quantity on the FIML surface, and the fact that makes
  the mixed ratio un-pinnable where it would actually be used. Worth carrying
  into M69's work log alongside M68's misspecification range.
- **B4 (size perspective).** All reported-number movement from M69 is ~0.1-0.2%
  at n = 600 (growing like 1/N): this is a coherence fix that makes the
  pricing exact and scale-free, not a material recalibration. The prose sweep
  (AC3) should say so rather than imply users' numbers change meaningfully.
- **B5 (root cause, out of scope).** The (N-1)/N enters through lavaan's
  `sample.cov.rescale` default, which also shrinks the reported point
  estimates by (N-1)/N. Fitting with `sample.cov.rescale = FALSE` would
  remove the artifact at its source — but it changes every shipped point
  estimate, would disturb the lavaan SE fence's convention, and would *not*
  obviate `cov2cor()` (the fitted diagonal still departs from 1 under
  misspecification, which is a property of the model, not the rescale). A
  separate decision gate if ever; not part of M69.

## Recommendations

1. **Apply.** Reprice `axes_corrected_se()`'s corrected branch at
   `stats::cov2cor(Sigma-hat)` (Q1); affirm M68-D2.
2. **Apply.** Price both sides of the FIML ratio at `cov2cor(Sigma-hat)`;
   implement via the third-value design **returning the composed
   `fiml_ratio`** rather than a bare denominator, with `naive` kept at the raw
   matrix for the lavaan fence (Q2, Q3).
3. **Apply.** Supersede RR13 BC4 explicitly with BC4' below; record the
   supersession in M69's Decisions, citing this report (Q4).
4. **Apply.** Pin positive-diagonal invariance of `corrected` and
   `fiml_ratio` at 1e-6 relative, with the raw-`naive` scaling companion and
   the FIML wiring assertion (Q5).
5. **Apply.** Add the nonpositive-diagonal refusal before `cov2cor()` with the
   NA-together contract (B2).
6. **Apply.** Correct the inverted direction claim in
   `cairn/milestones/M69-axes-corrected-se-unit-diagonal.md` (T1 and AC4
   wording) when ingesting this RR; do not propagate "(N-1)/N, shrinking"
   into any test name, comment, or NEWS entry (B1).
7. **Consider.** Record B3's FIML-diagonal measurement in M69's work log.
8. **Reject — out of scope, separate decision gate if ever.**
   `sample.cov.rescale = FALSE` as the root-cause fix: it changes shipped
   point estimates and does not remove the need for `cov2cor()` (B5).
9. **Reject.** Leaving the mixed-matrix ratio in place as "harmless": it fails
   the metric-only property the code documents, is un-pinnable on the FIML
   path, and would be a regression against the shipped both-raw ratio's
   6.2e-4 fidelity (Q2).

## Binding criteria

Tolerances are relative unless stated.

- **BC1 (same-matrix pricing, ratio encapsulated).** `axes_corrected_se()`
  returns `naive` priced at the raw realigned Sigma-hat, `corrected` priced at
  `stats::cov2cor()` of that matrix, and a per-component `fiml_ratio` equal to
  the corrected SE divided by the normal-theory SE **both evaluated at
  `cov2cor(Sigma-hat)`**; the FIML composition in `R/axes_reliability.R`
  consumes `fiml_ratio`, and no code in `R/` forms a ratio of `corrected` to
  `naive`. Enumeration procedure for that universal: `grep -rn
  "axes_corrected_se" R/` lists every call site (one at review time); each hit
  and its surrounding function is inspected for ratio formation.
- **BC2 (invariance, with derived tolerance).** On the probe fits of
  `test-axes-corrected-se.R`, for a seeded random positive diagonal D (entries
  in [exp(-0.3), exp(0.3)]) and the scalar 2: `corrected` and `fiml_ratio`
  computed from `D %*% Sigma-hat %*% D` and from `2 * Sigma-hat` each equal
  their values at Sigma-hat within 1e-6, and `naive` at `2 * Sigma-hat` equals
  2 times `naive` at Sigma-hat within 1e-6. Tolerance derivation: the
  superseded raw/mixed pricing violates these identities by O(1) factors
  (measured 1.538-2.114 at scalar 2), ≥6 orders above the tolerance; the
  measured floating-point drift of the cov2cor path is 4.4e-16 and the
  repository's worst observed instrumentation drift is 1.3e-8, ≥2 orders
  below. No bit-identity assertions.
- **BC3 (lavaan fence preserved).** `tests/testthat/test-axes-corrected-se.R`
  lines 67-69 and 191-194 pass with their assertion lines unedited: `naive`
  reproduces lavaan's own component SEs within 1e-7 absolute.
- **BC4' (supersedes RR13 BC4, per this report).** The FIML path's corrected
  SE is the observed-information SE multiplied by the per-parameter ratio of
  correlation-metric SE to normal-theory SE, **both evaluated at the implied
  correlation matrix `cov2cor(Sigma-hat)` of that fit**. Against the committed
  200-replicate fixture at 2, 5, and 10% MCAR, mean corrected FIML SE(xi1) /
  empirical SD ∈ [0.90, 1.10] in every cell, re-run under the new pricing.
  Numeric projection: the reported FIML SE moves by less than 1% relative to
  the shipped value in every cell (measured -0.062% on the cormat probe's
  xi1; the 1% bound allows the FIML diagonal profile, sd 0.030), so the
  band's verdict cannot flip — but the re-run, not the projection, is the
  evidence.
- **BC5 (failure contract extended).** With any nonpositive diagonal entry in
  the realigned Sigma-hat, `naive`, `corrected`, and `fiml_ratio` are all NA
  under one named `reason`, refused **before** `cov2cor()` executes; and in
  every failure return of the function the three vectors are NA together with
  no fallback. Enumeration procedure: the `na_out()` calls in
  `axes_corrected_se()` are the function's only non-success returns; list and
  check each.
- **BC6 (direction prose).** Every statement of the mixed-ratio artifact's
  direction or factor in `cairn/milestones/M69-axes-corrected-se-unit-diagonal.md`
  and in the full diff M69 merges states inflation by N/(N-1) (equivalently,
  division by (N-1)/N), never shrinkage by (N-1)/N. Enumeration procedure:
  read the milestone file in full and read the complete `git diff` of the M69
  branch against its base — bounded sets that catch prose stating the claim
  without a searchable literal.
