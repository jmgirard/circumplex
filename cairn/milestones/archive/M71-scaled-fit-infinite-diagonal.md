# M71: Refuse an infinite fitted diagonal in `axes_scaling_factor()`

**Status:** done (2026-08-04, PR #97 https://github.com/jmgirard/circumplex/pull/97)

**Goal:** `axes_scaling_factor()` refuses a fitted `sigma` carrying a `+Inf` diagonal entry with a named-reason NA, instead of computing a scaling factor from the corrupted correlation matrix `cov2cor()` returns for it.

**Outcome:** `if (any(is.infinite(diag(sigma)))) return(na_out("infinite_diagonal"))` at `R/axes_scaled_fit.R:145`, placed *after* the `<= 0` guard and testing positive infinity only — `+Inf` was the one entry `cov2cor()` launders (zeroed row, unit diagonal) rather than propagates, so `solve()` and `is.finite()` both accepted it and a factor was returned with `reason = NULL` (0.9579017 at the octant probe). `-Inf` keeps its `"singular"` route via the older guard and `NA`/`NaN` keep falling through to the `solve()`/`is.finite` pair; `is.infinite()` is `FALSE` on both, so the new line needs no `na.rm`. Tests now pin all four cells including `-Inf`, which nothing covered before. The `details` reason enumerations in `R/axes_reliability.R` were corrected against a grep of both helpers: `fit_scaling_failed` gained the new literal, `se_correction_failed` gained `"nonpositive_diagonal"` and `"indefinite"`, stale since M69.

**Decisions:** none milestone-local. Three plan-gate approach choices are recorded in the work log: `+Inf`-only over `!is.finite()`, the literal `"infinite_diagonal"` over reusing `"singular"`, and leaving `axes_corrected_se()`'s label alone (it refuses `+Inf` already, by a different route).

**Review:** three fresh-context lenses; blame-history and prior-review returned zero findings, diff-bug returned 10, one at ≥80. F4 (85, the sibling-behaviour comment naming `cov2cor()` where the zeros actually appear in `solve()`) fixed in place; F3 (70) fixed below the bar after verifying live that all six literals on this surface are reachable. Nine logged unactioned. F1 (15) — a huge-but-finite fitted diagonal (1e10) still scales silently while the sibling refuses it — graduated to a candidate ROADMAP row rather than left in the review section. One lesson captured; none retired.
