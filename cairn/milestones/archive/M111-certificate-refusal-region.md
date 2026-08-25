# M111: Shrink the ill-conditioning refusal to what the certificate cannot certify

**Status:** done (2026-08-24, PR #142 https://github.com/jmgirard/circumplex/pull/142)

**Goal:** Replace the a-priori condition-number refusal at both surfaces with M108's per-fit certificate, so a fit computes whenever its own certificate passes.

**Outcome:** `axes_degeneracy_refusal()` (R/axes_corrected_se.R) is the one place either surface decides a refusal. `axes_sigma_degenerate()`'s floor is unchanged but no longer refuses on its own: it selects which fits are checked. `"indefinite"` and `"singular"` forward untouched; `"ill_conditioned"` routes to `axes_accuracy_certificate()`, and a fit whose worse estimate (SE or cval — the max, one shared predicate at both surfaces) exceeds delta_star = 1e-4 refuses as the new literal `"uncertified"`, its warning naming the estimate ahead of the conditioning clause. delta_star, C and tau are untouched. Two of the five reachable oracle geometries (a5, b9b) moved from refused to computed. The raw `naive_reason` arm still calls the criterion directly and still carries `"ill_conditioned"`. New file `tests/testthat/test-axes-certificate-refusal.R`, 117 assertions. T8 corrected the accuracy block's stale `6.5e-6` corner to 6.36e-6 (M110 review F3) and pinned every figure there to `0.1*a/sqrt(n)`.

**Decisions:** none cross-cutting; D-051 pre-authorized the re-keying. Four gate choices and a mini-gate AC3 amendment (its seam route unsatisfiable at p = 3) are in the work log.

**Review:** three-lens fan-out, 17 findings. Eight fixed at the gate (F5-F11, F14 — doc, comment and test-assertion accuracy); five absorbed into the ROADMAP degeneracy row (F1+F2 the uncertified cov2cor naive arm behind the reported FIML SE, F3 a nestedness hazard, F4 the undiscriminated shared predicate, F12, F13); three rejected with reasons. CI went red on ubuntu and windows on the F5 fix itself, which pinned the fixture's estimate to its printed digits; repinned to the route, green on all three platforms.
