# M89: Price the degeneracy criterion in the metric the reported numbers live in

**Status:** done (2026-08-16, PR #117 https://github.com/jmgirard/circumplex/pull/117)

**Goal:** Move `axes_reliability()`'s fitted-matrix degeneracy criterion onto
`cov2cor(Σ̂)` and tighten its floor to a stated accuracy target (τ = 1e-6).

**Outcome:** `axes_sigma_degenerate()` now prices `cov2cor(Σ̂)` at
`axes_scaling_factor()` and both arms at `axes_corrected_se()` (raw for its
`naive` arm only, D-037), refusals nested with one literal; floor
`sqrt(p·ε/τ)` with `axes_degeneracy_tau <- 1e-6`, calibrated by the
exact-rational oracle at `devel/degeneracy-oracle/` (clean-clone reproducible).
RR18's 3.4%-wrong-SE exemplar is refused; pure diagonal rescalings compute
(scale invariant to 1e-9, five-axis sweep at p = 24/12/8); near-threshold pins
discriminate the floor's `p` factor (both mutants verified red).

**Decisions:** D-044 (the metric choice, superseding the first cut's raw-matrix
entry, which carries a dated annotation); τ recorded beside the criterion with
the oracle's error table as calibration. First cut's entries superseded in file.

**Review:** Round 3 (re-cut): all nine criteria fresh-verified, no shortfall vs
RR18's projections; 19 [O] findings — F6 (80) fixed (threshold test), F9
comment corrected, F3 (68) → candidate row on τ's statistical calibration.
Rounds 1–2 verified the superseded cut; RB18/RR18 archived; M84 row pruned.
