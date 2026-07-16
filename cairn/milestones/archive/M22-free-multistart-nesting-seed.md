# M22: Free-engine multi-start nesting seed — done 2026-07-16

**Goal:** seed `cpm_fit()`'s free-scaling multi-start battery with the
unit-family solution so T_free ≤ T_unit holds by construction (RR05 B2/R5;
3/5,751 optimizer-tail violations, worst +5.52 T-units).

**Outcome:** `cpm_engine()` under `scaling = "free"` runs an internal
unit-family fit and appends its optimum (block-exact embedding, s = 0; the
objective value is bit-identical) as one extra start, with a belt fallback
making nesting unconditional. Top-level fits only; bootstrap warm starts
untouched. Regression test regenerates RR05's exact worst violator (red
pre-fix); deterministic battery (variants A, C); oracles green at unchanged
tolerances (OpenMx + Grassi); check() 0 errors / 0 warnings / 0 notes.

**Key decisions:** (1) The seed is EXCLUDED from the `reproduced`
acceptance count (sentinel group 0, `cpm_reproduced()` helper, unit-tested)
— the initial own-group choice silently liberalized acceptance and was
caught by the free-family SE cross-check oracle (3 permutation-basin
replicates with exactly 1 native group at min F); acceptance semantics
match the pre-seed engine exactly; a seed-rescued fit reports the better
optimum WITH the acceptance warning (NEWS-documented). (2) Docs wording:
"never exceeds … beyond numerical tolerance" (post-polish caveat), inside
D-011's scoping. Review triage: F1 (82) fixed, F3 (68) fixed voluntarily,
F2 (42) logged (belt fallback vs multimodality loop; conservative-erring).
**PR:** https://github.com/jmgirard/circumplex/pull/46 (squash ec1f78f).
