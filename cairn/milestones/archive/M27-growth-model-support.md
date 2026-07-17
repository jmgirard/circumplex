# M27: Longitudinal Build C — growth-model support on displacement (done)

**Status:** done · **PR:** [#51](https://github.com/jmgirard/circumplex/pull/51) (squash ba76f61, 2026-07-17) · Build C of the D-013 spec (§4).

## Outcome — growth-model support on SSM displacement (boundary-free)

- **`angle_unwrap(x)`** (exported): unwraps ordered angles onto a continuous
  branch (350, 10, 30 → 350, 370, 390); reals wrapped to [0, 360) first;
  exact-180° ascends (+180); NA propagates from the missing wave onward.
- **`ssm_draws()` per-t certification**: `details$certified` carries the D-007
  verdict; print/summary flag when the amplitude interval fails the rule.
- **Growth vignette**: joint glmmTMB recipe (univariate fits shown invalid —
  zero Cov(x̂, ŷ) → wrong d(t) CIs); draws → certified a(t)/d(t); REML caution;
  unwrap alternative + failure modes; Brief E caveats.
- **Coverage oracle** (`devel/m27-coverage-*`, seeded/pre-registered): 5/5
  green — pole nominal; low-amplitude caution fires; strong-correlation cell
  fails the univariate shortcut, passes joint; two invariants.

## Decisions & review

- **D-016**: glmmTMB → `Suggests`; vignette engine glmmTMB not nlme (gated
  D-013 amendment; joint-fitting holding unchanged).
- Three-lens + scorer. One HIGH (97): vignette plot inverted seam-straddling
  CI ribbons — fixed (per-endpoint signed circular distance). Two LOW fixes.
  `check()` 0/0/0; CI green.
