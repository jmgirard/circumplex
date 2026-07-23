# RB09: Axes-reliability (Strack 2013) design GO/NO-GO (M53)

- **Date:** 2026-07-23
- **Output required:** write findings to `cairn/reviews/RR09-axes-reliability-strack.md`

You are performing an independent expert review of a statistical design. This
brief is fully self-contained — do not assume any conversation context. Read
only what this brief directs you to read, answer the numbered questions, and
write your findings to the output path above using the same numbering. You are
reviewing a **design spec**, not code — the estimator is not yet implemented.
Your review produces a GO/NO-GO on building it.

## Background

`circumplex` is a CRAN R package for circumplex data analysis (Structural
Summary Method). It scores circumplex instruments (interpersonal, values, etc.),
each defined by scales at fixed angular positions (degrees, LM=360 convention;
8 equally-spaced octant scales at 45° spacing are the dominant form). It has an
existing SEM layer, `ssm_sem()`, that fits a **scale-level** fixed-angle
circumplex CFA via `lavaan`, and a structure-test layer, `fit_structure()`,
implementing Acton & Revelle criteria plus Tracey's RANDALL order test.

Milestone **M53** designs a new feature: an estimator of **circumplex axes
reliability** from Strack, Jacobs & Grosse Holtforth (2013), *Reliability of
Circumplex Axes* (SAGE Open 3(2); DOI 10.1177/2158244013486115). The paper fits
a restricted tau-equivalent CFA to an instrument's **item** correlation matrix,
decomposing item variance into five orthogonal components (general factor, axes,
scale-specificity, block-specificity, item-specificity), and reads axis
reliability off the isolated **axes** variance component via Spearman–Brown.

This needs independent review because it is a statistical estimator with (a) **no
published raw-data oracle** — the paper's 17 German samples are unavailable —
(b) a **new exported API** (irreversible once shipped), and (c) it touches the
package's **angle invariants** (item weights derive from angular positions). The
milestone was tagged with all three RB tripwires (no-oracle, irreversible-api,
ip-touching). The feature is admitted to the v2.0.0 release scope conditionally
(D-025); your GO/NO-GO decides whether the build proceeds.

## Materials

Read, in this order:

1. **`devel/m53-axes-reliability-spec.md`** — the design under review. Its §2
   (the model + four flagged points F-1…F-4), §3 (reliability/SEm/N–B), §4 (the
   two-layer oracle strategy), §5 (proposed API), §6 (GO/NO-GO framing) are the
   spine of your review.
2. **`cairn/references/strack2013.md`** — the source note: the five-component
   model, the weights, the Spearman–Brown / SEm / Nunnally–Bernstein formulas,
   and Table 3 (the published-value oracle) with page anchors. The shelf PDF is
   `cairn/references/sources/strack2013.pdf` (12 pp., born-digital) if you need
   the original.
3. **`R/ssm_sem.R`** — the existing lavaan CFA layer. Note especially the
   `lavaan::cfa` chokepoint around line 736 (fiml/std handling, group ordering)
   the spec proposes to reuse, and how it maps a fitted `lavaan` parameter table
   to estimates (lines ~108–125).
4. **`R/fit_structure.R`** — the RANDALL/Acton–Revelle layer; note `paf2()`'s
   degeneracy policy (NA-return on an undefined correlation matrix) as the
   precedent for the refuse-don't-coerce contract.
5. **`cairn/references/hubert1987.md`, `cairn/references/tracey1997.md`** — the
   RANDALL sources, for context on what Strack contrasts against (RANDALL is
   sensitive to the axes/scale-specificity ratio; the CFA isolates both).

Reproduce the Layer-A oracle claim yourself if useful: Spearman–Brown
`(item_n·ξ1)/(1+(item_n−1)·ξ1)` on the paper's printed Table 3 `%axes` (col 6)
and `item_n` (col 10) should reproduce its `Reliability` (col 11) — e.g. IAL
.26/32→.92, IPI-A .134/16→.71, COC .028/8→.19, OCAI .117/8→.51.

## Questions

1. **Model faithfulness (spec §2).** Is the five-component restricted
   tau-equivalent CFA as specified a faithful rendering of Strack et al. Figure 2
   and pp. 3–4 — the orthogonal equal-variance axes with fixed cosine-weight
   loadings, the +1 general factor, the equal scale-/block-specificity latents,
   and free item errors? Name any misstatement or omission.
2. **F-1: error freedom vs. the sum-to-100% identity.** Fitting to a correlation
   matrix fixes each item's total variance at 1. Given the fixed weights and
   equal latent variances, are the item errors ε_i genuinely free per item, or
   determined by diagonal reproduction? State the exact constraint set a correct
   `lavaan` model must impose, and whether "tau-equivalent, errors free" is
   compatible with fitting a correlation (vs. covariance) matrix.
3. **F-2: identification.** Is the octant type-a model (all loadings fixed;
   2–4 free latent variances {ξ1, ξ2, ζ1, ζ2}; free item errors) identified? What
   are the failure modes as a component variance → 0 (e.g. an instrument with no
   general factor, or scale-specificity → 0), and how should the estimator
   detect and report a non-identified or boundary fit?
4. **F-3: the equal-axis-variance restriction and spacing.** The equal-axis
   restriction and cosine weights assume exact equal angular spacing. Is forcing
   `Var(X-axis) = Var(Y-axis) = ξ1` correct for the package's octant instruments,
   and what exactly must the refuse contract reject (non-equal spacing, an odd
   scale count, missing angles) to avoid silently mis-weighting?
5. **F-4: weight orientation vs. the angle convention.** The spec computes an
   item's axis weight as `cos(θ − α)` for axes at α and α+90°. Is this the correct
   and sign-consistent rendering of "the cosine of the scale's angular position"
   (Strack p. 2) under the package's LM=360 / octant convention (agency at 90°,
   communion at 0/360)? Identify any sign or reference-axis error and the
   boundary case (a scale exactly on an axis / on the 0=360 pole) a test must pin.
6. **Oracle sufficiency (spec §4).** Is the two-layer strategy adequate given no
   raw-data oracle? Specifically: (a) is Layer A (Spearman–Brown reproducing
   Table 3's printed reliabilities) a legitimate **published-value oracle for the
   formula layer**, or does it merely re-derive an identity that proves nothing?
   (b) does Layer B (synthetic ξ1 recovery + a cross-engine lavaan/OpenMx check,
   with a high-scale-specificity failure-expecting cell) meet the
   ≥2-independent-oracle-types bar for the CFA-fit layer? Name any missing
   oracle or any cell whose expected outcome the spec states wrongly.
7. **API and refuse contract (spec §5).** Is a standalone `axes_reliability()`
   (item data + instrument input; refuse-don't-coerce on non-circumplex,
   unequal-spacing, singular, or non-finite input) the right and safe API shape,
   or should any element change before it becomes an irreversible exported
   surface? Consider the outputs (per-axis reliability, SEm, five components with
   SEs, item_n, fit indices, N–B comparison).
8. **GO/NO-GO.** Given the above: **GO** (build the estimator + the Layer-A/B
   oracles in v2.0.0) or **NO-GO** (drop/defer with rationale)? If GO, list the
   load-bearing holdings the build must honor. If conditional, state the exact
   conditions.

## Constraints

Fixed; flag disagreement explicitly rather than silently working around:

- **Plan-gate directions (M53, do not relitigate the direction, but you may flag
  a fatal problem):** standalone exported function (not folded into `ssm_sem()`);
  **octant / type-a MVP** with non-octant types (b–f) and quasi-circumplex
  weights deferred-in-spec; **item-data + instrument** as the primary input.
- **Minimal dependencies (D-006, D-014).** No new hard dependency. `lavaan` and
  `OpenMx` are already `Suggests` (used by `ssm_sem` and its oracle); the
  estimator and its cross-engine oracle must live behind `Suggests`, not add an
  Import.
- **Angle invariants (CLAUDE.md / DESIGN.md).** Degrees [0,360), LM=360;
  boundary/pole behavior is where bugs hide and must be tested.
- **Validation doctrine.** Any numeric result needs ≥2 independent oracle types;
  a bug fix needs a regression test that fails before the fix. Your oracle
  verdict (Q6) is binding on the build.
- **Release scope (D-025).** The feature is admitted to v2.0.0 as a design→build
  path; a NO-GO drops/defers it from v2.0.0 — that is an anticipated, legitimate
  outcome, not a failure.

## Output format

In `RR09-axes-reliability-strack.md`: answer each question 1–8 by number with
your reasoning and evidence (cite spec sections, source-note anchors, and file
lines). List any additional findings under "Beyond the brief"; end with concrete
recommendations, each marked apply / consider / reject-with-reason. Because your
findings bind the build, also emit a `## Binding criteria` section: numbered
`BC1…`, each a measurable assertion the build must satisfy (e.g. an oracle that
must pass with its tolerance, a refuse case that must error, a boundary test that
must exist), with any numeric projection stating its tolerance. State the
GO/NO-GO verdict explicitly in your conclusion.
