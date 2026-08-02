# M67: M65 review fold-ins for the `axes_reliability()` FIML path

**Status:** done (2026-08-02, PR #93 https://github.com/jmgirard/circumplex/pull/93)

**Goal:** Close the seven sub-threshold findings M65 logged against the FIML path, each
leaving a guard, comment, or documented claim saying something the code does not do.

**Outcome:** `axes_fiml_em_stalled()` matches lavaan's diagnosis at every wrap position via
`moments[[:space:]]+using[[:space:]]+EM`: `lav_msg()` re-wraps at emission-time width and only
those two gaps break the match, so the `em.h1` stem is a backstop now, not the sole detector.
The thin-overlap warning gained `min_coverage < n_used + n_dropped` — the caller's supplied
total, not the post-`axes_fiml_coverage()` count — so complete data under N = 30 draws no
missing-data warning and heavy unit nonresponse no longer silences one. The fit-measure guard
keys on membership then orders via `fm <- fm[want]`, making "(missing: )" unreachable. Four
comments corrected to measured behavior; the OLS-shadow assertion made discriminative (M2 MAR).

**Decisions:** none milestone-local.

**Review:** Two passes. First returned on F1 (95, regex left one of two gaps literal), F4 (92,
the clause silenced the warning under unit nonresponse), F2 (90, false comment), F3 (80, a
width-pinned test catching neither); AC1 amended at the implement gate. Second verified all six
criteria; of 17 candidates only F18 (90), a false claim about which lavaan rename broke M65's
CI, cleared 80 — fixed in place, no return. 16 below threshold, notably F19 (42).
