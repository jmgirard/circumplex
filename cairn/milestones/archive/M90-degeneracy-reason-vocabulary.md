# M90: Say which degeneracy happened, and stop saying it when it didn't

**Status:** done (2026-08-16, PR #118 https://github.com/jmgirard/circumplex/pull/118)

**Goal:** Split the refusal vocabulary so indefiniteness (a statement about the user's
model) stops sharing one word with mere ill-conditioning, refuse a saturated model as
`"saturated"`, and stop the `cval <= 0` backstop claiming what it cannot diagnose.

**Outcome:** `axes_sigma_degenerate()` partitions its refusal region — `"indefinite"`
iff λmin < −λmax·sqrt(p·ε) (a convergence-noise band, rationale in-code), else
`"ill_conditioned"` — verified by a 16-cell near-threshold battery (two p, two λmax
scales, two forms) with drop-p/squared-p/drop-λmax mutants each verified red.
`axes_scaling_factor()` gains a `df == 0` `"saturated"` guard ahead of all matrix work;
its backstop relabels to `"ill_conditioned"` (30,000-draw search never reached it;
nearest miss cval +1.2e-5 at p = 3/df = 1; `devel/m90-ac5-search/`).
`axes_corrected_se()` evaluates its cov2cor arm first with a hoisted raw-matrix
finiteness check, so on double refusals the reported literal is the scaling surface's —
pinned by an arm-disagreement probe. Roxygen/Rd/NEWS carry reachability qualifiers
(`"saturated"` is helper-boundary-only). Decisions: none promoted; AC5 amended at
review round 1 (audited wording, work log).

**Review:** replan audit (2 passes) + three-lens fan-out + round-2 delta reviewer: 25
findings — 20 fixed, 2 rejected with reason, 1 routed to M91, 1 dissolved; 1 defect
return (AC4 grep miss) + 1 amendment return (AC5), both closed. RR18 anchors matched
(−0.3819 vs −0.382; −9.322e-16 vs −9.32e-16). Suite 8257/0; check 0/0/0; CI green.
