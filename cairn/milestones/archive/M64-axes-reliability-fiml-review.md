# M64: FIML on items for `axes_reliability()` — the estimator-metric question

**Status:** done (2026-07-26, PR #90 https://github.com/jmgirard/circumplex/pull/90)

**Goal:** Settle under independent Fable review whether `axes_reliability()` can
honestly offer FIML on item data, and record the verdict.

**Outcome:** Docs-only; RR12 = GO under BC1–BC16, which bind the build. Its
load-bearing holding rules out the mechanism the shipped path uses:
available-case `scale()` standardization is MCAR-honest but MAR-dishonest
(+0.0167, ≈1 SE at N = 600). The build instead standardizes by saturated-FIML
(EM) moments on a `sqrt(N_used/(N_used − 1))` convention exact on complete data,
one one-stage fit, R̂ for the OLS shadow and PD refusal, a six-clause refusal
contract (incl. never-jointly-observed pairs, which lavaan fabricates), and a
nine-cell bar. Seed-pinned evidence: `devel/m64-fiml-probe.R`.

**Decisions:** D-033 (GO; narrow D-001 supersession; takes up D-026's last
deferral, now empty). D-034 (three corrections to D-033). Nine local, incl.
M64-D3: the complete-data implied-diagonal departure is expected restricted-ML.

**Review:** Returned once — AC2 failed as written (RB12 states two of three fixed
positions); amended at a gate, an archived brief being immutable. 3 lenses +
scorer, 8 findings: 4 actioned (F6 87, F7 85, F2 82, F1 80), 3 fixed below the
bar, 1 logged. Second pass re-verified with no fresh fan-out, recorded as such.
