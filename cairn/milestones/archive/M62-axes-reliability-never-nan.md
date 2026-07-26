# M62: Close `axes_reliability()`'s two never-NaN gaps — ξ1 ≥ 1 and an unvalidated `sd`

**Status:** done (2026-07-26, PR #88 https://github.com/jmgirard/circumplex/pull/88)

**Goal:** Make `axes_reliability()` incapable of reporting a NaN, negative, or
infinite standard error of measurement, by closing the two paths that still can.

**Outcome:** `axes_is_boundary(xi1, xi2, zeta1, eps)` is extracted from the
inline expression and brackets the axes variance both sides: `xi1 >= 1` joins
`xi1 <= 0`, since Spearman-Brown gives rel ≥ 1 exactly when ξ1 ≥ 1 and
`sqrt(1 - rel)` is then NaN; `zeta1` NULL-ness replaced M61's `fit_zeta1` flag.
A numeric `sd` must now be finite and positive — `-1`/`0`/`±Inf`/`NA_real_`/
`NaN` previously reached the results frame verbatim. ξ1 ≥ 1 is unreachable via
the exported function (a unit-diagonal metric forces `eps_i = 0` at ξ1 = 1), so
the seam is what tests it. Vignette + `print()` boundary prose widened to match.

**Decisions:** none milestone-local. No D-entry, deliberately: D-001 bars new
features and this hardens guards on a function that never shipped, so
D-030/D-031's narrow supersessions are not extended.

**Review:** three lenses + scorer. F3 (88) the mocked-seam test could not fail,
its assertions implied by the branch's `NA_real_` — fixed with an unmocked
warning-capture test. F2 (84) vignette and `print()` boundary prose stale;
widened. F1 (74) logged: the guard tests ξ1 not rel, so rounding admits SEm
exactly 0 — finite, not NaN, so outside the Goal. Four mutations ran.
