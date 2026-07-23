# M53 spec — circumplex axes-reliability (Strack, Jacobs & Grosse Holtforth, 2013)

Status: **draft for Fable review (RB09)**. This is the build-ready design for a
circumplex **axes-reliability** estimator, per Strack, Jacobs & Grosse Holtforth
(2013), *Reliability of Circumplex Axes*, SAGE Open 3(2), DOI
10.1177/2158244013486115. Page anchors are the article's own page numbers;
values are banked verbatim in `cairn/references/strack2013.md`.

Plan-gate decisions carried in (M53, 2026-07-23): **standalone exported
function** (parallel to `fit_structure()`, not folded into `ssm_sem()`);
**octant / type-a MVP** with the general cosine-weight formulation defined and
non-octant types deferred-in-spec; **item-data + instrument** as the primary
input surface. Feature admitted to v2.0.0 as a design→build path by **D-025**;
this spec's GO/NO-GO (Fable-reviewed) is a later D-entry.

## 1. Purpose

Circumplex work has, per the paper, "a very basic psychometric parameter …
missing: the reliability of circumplex axes" (p. 1). Scale reliabilities are
routinely reported; the reliability of the *axes* (the agency/communion
dimensions a profile's location is read off) is not. The paper closes the gap
with a restricted CFA that decomposes item variance and reads axis reliability
off the isolated **axes variance component**. The package already owns the
angle machinery (instrument `Scales$Angle`, LM=360) and a `lavaan` SEM layer
(`ssm_sem`), so the estimator is a natural addition.

## 2. The model — a restricted tau-equivalent CFA (p. 3–4, Figure 2)

Fit the restricted CFA to the **item correlation matrix** by ML. Each item's
(unit) variance decomposes **additively into five orthogonal components**
summing to 100% (Figure 2 note, p. 4):

    Var(item_i) = general (ξ2) + axes (ξ1) + scale-specificity (ζ1)
                  + block-specificity (ζ2) + item-specificity (ε_i) = 1

Component structure (Figure 2, read right→left):

- **Axes (ξ1).** Two latent axes (X, Y), **forced orthogonal**, their variances
  **set equal** — "For a perfect circle, the variance of both axes are set
  equal (Y_axis = ξ1 = X_axis)" (p. 4) — encoding the circumplex axiom of no
  preferred rotation. Each scale's items load on the two axes with **fixed
  weights = the cosine of the scale's angular displacement to each axis**: for
  a scale at angle θ and axes at α and α+90°, `w = cos(θ − α)` and
  `cos(θ − (α+90°))`. This is a **fixed-links model** (Schweizer 2010): loadings
  fixed by the weights, latent axis variance freed (p. 3). Type-a weights are
  `0, ±.707, ±1.0` (p. 3–4); type-b `±.38268, ±.92388` (p. 3). Only ξ1 feeds
  reliability (p. 4).
- **General factor (ξ2).** All items load +1 on one general latent — "a
  uniformly positive intercorrelation between all items due to … response style
  … and/or a meaningful intensity of the construct" (p. 4). Not all instruments
  show a significant one (p. 4).
- **Scale-specificity (ζ1).** Each scale carries an orthogonal specificity
  latent (its items load +1); **all scale-specificity variances set equal** to
  ζ1 — the unintended variance from scales not projecting perfectly onto the
  axes (p. 3–4).
- **Block-specificity (ζ2).** Only for blockwise-presented instruments (CSIV,
  TRC, OCAI in the paper); one specificity latent per block, variances equal
  (p. 4). Absent for the package's non-blocked instruments.
- **Item-specificity (ε_i).** The item residual; **errors stay free**
  (tau-equivalent: "In tau-equivalent models, the errors stay free", p. 3).

Estimated free parameters: the latent variances present among {ξ1, ξ2, ζ1, ζ2}
— "2, 3, or 4 parameters" (p. 5) — plus the free item errors ε_i. **Engine:**
`lavaan::cfa` on the item correlation matrix, reusing the `R/ssm_sem.R:736`
`lavaan::cfa` chokepoint pattern (fiml/std handling); OpenMx as the independent
cross-engine oracle (§4). The paper used LISREL 8.8 ML (p. 5).

**Model-structure points flagged for Fable (RB09):**

- **F-1. Error freedom vs the sum-to-100% identity.** Fitting to a correlation
  matrix fixes each item's total variance at 1; with the common components
  determined by the (equal) latent variances and fixed weights, whether ε_i is
  genuinely free per item or implied by the diagonal reproduction needs the
  exact lavaan parameterization pinned. State the constraint set precisely.
- **F-2. Identification.** With loadings all fixed and only 2–4 latent
  variances + free errors, confirm identification for the octant type-a model
  (the paper asserts low parameter counts reduce required N, p. 9) and the
  degenerate cases (a component's variance → 0).
- **F-3. Equal-axis-variance restriction and unequal spacing.** The equal-axis
  restriction and the cosine weights assume exact equal spacing. The package's
  octant instruments are equally spaced by construction, but the refuse contract
  (§5) must reject non-equally-spaced angle sets rather than silently
  mis-weight them.
- **F-4. Weight sign/orientation vs the package angle convention.** The weight
  `cos(θ − α)` must be pinned against the package's LM=360 / `octants()`
  convention and the two chosen axis angles (agency at 90°, communion at 0/360),
  with a boundary test at the pole.

## 3. The estimand and reliability (p. 3–4)

The axes variance ξ1 "exactly estimates the mean correlation of two items
caused by a respective axis" (p. 4) — the mean inter-item correlation that
Spearman–Brown turns into a composite reliability.

- **Reliability (Spearman–Brown, p. 4):**

      Rel_axis = (item_n · ξ1) / (1 + (item_n − 1) · ξ1)

  with **item_n = Σwᵢ²** — the sum of squared weights over all items on the axis
  (Table 3 col. 10; p. 4–5). For a 64-item type-a instrument (8 items × Σw²=4.0)
  item_n = 32; 32-item → 16; 16-item → 8.
- **SEm (p. 3):** `SEm = SD · √(1 − Rel_axis)`, feeding the location CI
  `±1.65·SEm` (90%) for a single profile (p. 6). The raw-variance scale is a
  researcher choice (z-standardized vs raw; Table 3 cols 12–13).
- **Nunnally–Bernstein comparison (p. 3, "p. 271, Eqs. 7–17"):**

      Rel_axis(NB) = 1 − ([Σwᵢ² − Σwᵢ² · Rel_scaleᵢ] / Var_axis)

  computed on z-standardized scales. The paper's headline finding: N–B
  **overestimates** axis reliability when scale-specificity is large (Figure 3;
  p. 8) because scale-specificity fails to reduce the axis-variance denominator.

## 4. Validation / oracle strategy — two layers

There is **no published raw-data oracle** (the 17 German samples are not
available). But validation splits cleanly into two independently-oracled layers:

**Layer A — the closed-form reliability/SEm/N–B formulas → published oracle
(Table 3).** Spearman–Brown on the paper's *printed* `%axes` (col 6 = ξ1·100)
and `item_n` (col 10) must reproduce its printed `Reliability` (col 11) within
rounding. Verified by hand across four types while drafting: IAL .26→.92; IPI-A
.134→.71; COC .028→.19; OCAI .117→.51 (all match col 11). This is a genuine
**published-value oracle** for the formula layer — Table 3 is the oracle, no raw
data needed. SEm (col 13) and the N–B column (col 14) give two further printed
cross-checks.

**Layer B — the CFA fit that produces ξ1 → synthetic recovery + cross-engine.**
Because the raw matrices are unavailable, the *estimation* is validated by:
(1) **synthetic recovery** — simulate items from a known five-component
structure (chosen ξ1, ξ2, ζ1, ε), fit, and recover ξ1 within Monte-Carlo error;
(2) **cross-engine** — the same fixed-links model in lavaan and OpenMx must agree
on ξ1 to tight tolerance (the ≥2-independent-oracle-types bar, per the
validation doctrine). **Failure-expecting cell (M23 lesson):** a synthetic cell
with *high scale-specificity* must reproduce the paper's headline — N–B
overestimates while the CFA reliability stays honest — so the oracle asserts the
gap, not just agreement.

Global-fit context (not an oracle): mean RMSEA .088 (SD .014), AGFI .691, PGFI
.651 across the paper's 29 models (p. 5).

## 5. Proposed API (irreversible-api — for Fable + GO/NO-GO ratification)

A **standalone exported function**, working name `axes_reliability()` (final
name a GO/NO-GO decision):

- **Primary input:** raw item `data` + an `instrument` object (or an explicit
  item→scale→angle mapping), reusing the package's scoring/instrument machinery
  (`Scales$Angle`, `Scales$Items`) exactly as `score()`/`ssm_analyze()` do.
- **Secondary input:** a precomputed item correlation matrix + weights, for
  non-instrument use (noted; MVP may defer).
- **Outputs:** per-axis reliability, SEm, the five variance components with
  their SEs (Table 3 shape), item_n, and global fit indices; plus the N–B
  comparison value. A print/summary method in the package idiom.
- **Refuse-don't-coerce contract (M18 lesson):** informative error (never
  silent coercion) on non-equally-spaced angles, a missing item→scale map, an
  undefined/near-singular item correlation matrix (cf. `fit_structure`'s
  `paf2` NA-return degeneracy policy, `R/fit_structure.R`), fewer scales than a
  circumplex needs, and non-finite inputs (`!is.finite`, the M32/M35 guard).
- **Scope of MVP:** octant type-a instruments; non-octant types (b–f) and
  quasi-circumplex weight adaptation are deferred-in-spec (build candidate).

## 6. GO/NO-GO framing for RB09

GO ships the build (implement `axes_reliability()` + the Layer-A/B oracles) in
v2.0.0. The load-bearing questions for Fable: is the model (§2, F-1…F-4) a
faithful and identified rendering of the paper; is the two-layer oracle strategy
(§4) sufficient given no raw-data oracle; is the standalone API (§5) and its
refuse contract right; and does anything here touch the angle invariants in a way
that needs a boundary test the build must carry. A NO-GO (e.g., identification
or oracle insufficiency) drops/defers the feature from v2.0.0 with rationale
(D-025 anticipates this).
