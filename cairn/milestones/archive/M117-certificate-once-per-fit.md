# M117: Price the accuracy certificate once per checked fit

**Status:** done (2026-08-31, PR #148 https://github.com/jmgirard/circumplex/pull/148)

**Goal:** `axes_reliability()` prices the per-fit accuracy certificate once per
checked fit instead of twice, with no change to any refusal, warning or number.

**Outcome:** `axes_corrected_se()` and `axes_scaling_factor()` take an optional
`refusal` argument (default `NULL` = price it yourself), consulted only at the
`axes_degeneracy_refusal()` seam, so every door guard ahead of it keeps its own
precedence and literals. New `axes_shared_refusal()` mirrors the shared matrix
doors, returns NULL where one would refuse, and attaches in `$priced` the matrix
it priced; `axes_check_shared_refusal()` aborts at both consumption sites on a
mismatch. `axes_reliability()` computes the fitted matrix and the refusal once
for both call sites. Ill-conditioned p = 24 fit: 0.033 s before, 0.029 s after.

**Decisions:** none; the plan gate's per-call-argument-over-cache choice is in the work log (git).

**Review:** three fresh-context lenses (user-facing tier); behavior-preservation
proved by execution — 26 identical return-and-warning comparisons over a
13-matrix battery, and end-to-end runs with the seam mocked out. Six findings,
none a floor return. Fixed at the gate: the unchecked matrix pairing (silent,
fail-open), the header's false guard-mirror claim, the derivative build's order,
the missing `item_block` default. Rejected: the per-call discarded third
derivative build (below this fit's resolution) and the guard triplication.
