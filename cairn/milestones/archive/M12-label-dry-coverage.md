# M12: Result-label DRY + statistical-core coverage tracking

- **Status:** done · **PR:** #36 (merged 2026-07-12)

**Goal:** single-source the duplicated Group/Measure/Label construction in
`ssm_analyze*()` and add statistical-core coverage tracking; both landed on
master before the v2.0.0 freeze.

**Outcome:**
- `build_result_labels()` (`R/ssm_analysis.R`) replaces four byte-identical
  inline blocks (mean/corr × scores/results). Output byte-identical → no NEWS.
- New helper unit test pins all 8 branches, incl. two not covered end-to-end
  (corr no-contrast+grouping; corr multi-measure no-contrast).
- `statistical_core` codecov component (7 R sources + `circular.cpp` +
  `parameters.cpp`).

**Key decisions:** landed pre-freeze (byte-identical + tooling-only don't expand
v2.0.0 scope, D-001); no RB tripwire (pre-refactor snapshots are the oracle, M8
lesson). Routed siblings: vctrs/S7 → `/milestone-brief`; Hessian + pole-snap →
candidates; "R-devel CI" retired (already present).

**Evidence:** `test()` 1823/0; `check(--no-manual)` 0/0/0; two-lens independent
review 0 findings (refactor traces to `8c08945`, deliberately uniform); CI green
(7 checks: 5 platforms + pkgdown + test-coverage).
