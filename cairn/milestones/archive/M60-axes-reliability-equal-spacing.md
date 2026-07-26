# M60: Any equally spaced angle set for `axes_reliability()`

**Status:** done (2026-07-26, PR #86 https://github.com/jmgirard/circumplex/pull/86)

**Goal:** Let `axes_reliability()` estimate from any equally spaced set of scale
angles at any rotation, instead of only the canonical octant set.

**Outcome:** New internal `angles_spacing_status()` — modular (LM = 360 ≡ 0),
tolerance admitting float noise only — replaces the `octants()` set-identity
refusal. Accepts any equally spaced set at any rotation, k ≥ 4, ≥ 2 items/scale;
refuses unequal spacing, duplicates, non-finite angles, k < 4, naming the
offender. **k ≥ 4 is an identification floor**: at k = 3 every cross-scale pair
shares cos Δ = −0.5 and the moment design (cos Δ, 1, same-scale) drops to rank 2.
Σw² = k/2 at any rotation keeps equal axis variances innocuous. Table 3's `Type`
column + type-b (CV-LI ×4) and type-c (MEIL) rows banked in `strack2013.md`.

**Decisions:** D-031 (plan-time; admits M60+M61 to v2.0.0, M7 ungated). Local:
the wrap gap is implied by the interior gaps; a global weight rotation is
unfalsifiable by construction, so weight mutations must break relative geometry.

**Review:** 3 lenses + scorer. Fixed F1 (96, `Inf` escaped the refuse contract —
`anyNA()` misses it, `sort()` drops the `NaN`; master refused it only by
accident), F2 (83, false `split()` hazard in comments), F5 (82, provenance
claiming unbanked type-e/f). Logged F3 (62), F4 (45). 8/8 ACs; check 0/0/0, PDF
manual, suite 3748, CI green.
