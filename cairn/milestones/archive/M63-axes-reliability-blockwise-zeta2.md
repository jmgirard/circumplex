# M63: Blockwise instruments for `axes_reliability()` — the ζ2 component

**Status:** done (2026-07-26, PR #89 https://github.com/jmgirard/circumplex/pull/89)

**Goal:** Estimate Strack's block-specificity component so a blockwise
instrument's axes variance is recovered rather than approximated.

**Outcome:** `axes_reliability()` takes `blocks` (item columns, one element per
block) and fits ζ2 as a fifth component. `axes_design()` became the single
source of truth for the component set, read by the OLS shadow, syntax emitter
and reported components alike; ζ2 is kept only when the same-block indicator
raises the design's rank, catching blocks-are-scales, one-block, all-singleton
and collinear-with-cosine maps in one test. Recovery within 1e-4 on the exact
population, lavaan/OpenMx to 1e-3, OLS shadow .02; six blocked/type-d Table 3
rows banked in `strack2013.md`, two channels.

**Decisions:** M63-D1 `blocks` must partition the items. M63-D2 the omitted-ζ2
bias in ξ1 is conditional on block geometry, and the shipped caveat was wrong in
two of three parts. D-032 promotes this into v2.0.0, narrowly, not gating M7.

**Review:** three lenses + scorer; three actioned, all in prose. F1 (92) the
conditional's *condition* was wrong — antipodal blocks are maximally spread yet
bias ξ1 −9%, so even spread is not safety. F2 (90) a worked example held only at
k=4 though 8 is canonical. F3 (90) ξ2 inflation not unconditional. AC4/AC5
amended at the gate; F4 (58) fixed anyway, F5 (52) settled by M61's precedent.
