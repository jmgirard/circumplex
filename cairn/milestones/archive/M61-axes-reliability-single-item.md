# M61: Single-item scale positions for `axes_reliability()` — dropping ζ1

**Status:** done (2026-07-26, PR #87 https://github.com/jmgirard/circumplex/pull/87)

**Goal:** Let `axes_reliability()` estimate an instrument with one item per scale
position by dropping the scale-specificity component ζ1, as Strack's types e and f do.

**Outcome:** `axes_fits_zeta1()` — any scale with ≥ 2 items — is the single source of the
drop decision, read by both `axes_syntax()` and the estimator, so the model and the
reported components cannot desync. The `SS` latents and shared `zeta1` label leave the
syntax; `axes_ols_shadow()` degrades to a two-element seed rather than dying in
`qr.solve()`; the components frame is variable-length (row absent, never `NA`); `details`
gains `zeta1_fitted` and `nb_reason`; N–B is `NA`-with-reason when any scale has < 2
items; the refusal relaxes to ≥ 1 item. Fractional item_n now carries through end to end.

**Decisions:** M61-D1 — the N–B `NA` rule is "any scale with < 2 items", a superset of
AC3's wording, since a mixed map fits ζ1 yet still leaks `NaN`. M61-D2 — RR11 ingested:
SYMLOG is a three-axis sphere, so item_n 8.67 = 26/3 is unreachable under this contract.

**Review:** 14 criteria verified fresh; `check()` 0/0/0 ×4; CI green. Oracles: six Table 3
type-e/f rows banked formula-layer-only, plus population/MC/OpenMx cells at single-item
and mixed maps. Three lenses: two 0 findings, diff-bug 4 scored 82/77/66/32; F1 fixed
(the AC2 fixture was two independent draws `cbind`-ed) closing F2's oracle gap too,
F3/F4 on Jeff's override. RB11/RR11 archived.
