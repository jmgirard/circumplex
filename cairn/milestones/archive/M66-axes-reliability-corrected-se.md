# M66: Corrected component standard errors for `axes_reliability()`

**Status:** done (2026-07-27, PR #92 https://github.com/jmgirard/circumplex/pull/92)

**Goal:** Replace `axes_reliability()`'s correlation-as-covariance component
standard errors with the Browne/Cudeck corrected asymptotic covariance.

**Outcome:** New `R/axes_corrected_se.R`: `axes_se_derivs()` builds {C, J, B, K,
E_ii}; `axes_corrected_se()` returns naive and corrected SEs plus a failure
`reason`, realigning Σ̂ to item-map order (lavaan reorders model variables). Raw
and `cormat` take the corrected SE; FIML composes multiplicatively at Σ̂ so its
missing-information pricing survives. `details` gains `se_uncorrected` and
`se_correction_failed`. Calibration 0.96 / 0.93 / 1.02 (complete, 15% MCAR, M1
MAR); bootstrap 2.0–2.5% vs 27–32% from uncorrected. Anti-conservative ~7.5% at
15%: documented, pinned in-test.

**Decisions:** none milestone-local (authorized by D-035). Implement gate:
`se_uncorrected` retains lavaan's values; a failure gives NA with a named reason
and never falls back; no `se =` opt-out.

**Review:** three lenses + scorer; blame-history clean. Prior-review: the vignette
still asserted the falsified SE claims (fixed). Diff-bug: roxygen claimed the FIML
residual grows anti-conservatively while citing conservative figures (F1, 88,
fixed); the failure state printed a calibrated-SE caveat beside an all-NA column
(F3, 85, fixed). F2 (68) logged — hardened, unreproduced over 3822 draws.
