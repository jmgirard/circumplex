# M21: T_diag-vs-T_free inference-default decision + application (done 2026-07-16)

**Goal:** decide — Fable-reviewed — whether the free-scaling family should
become the CPM model-test inference default (D-009 item 3); apply the outcome.

**Outcome:** **keep the unit family as the inference default (D-011).** A
paired calibration oracle (`devel/m21-t-calibration.{R,md}` + rds; both
engines fit to the same `R = cor(X)`, 500 reps × 12 cells, N 250–50000)
showed the families calibration-indistinguishable at correlation input:
paired ΔT̄ ≤ 0.5% of df, cor ≥ .998, same rejection/KS regime every cell — a
tie structurally forced (correlation input pins σ at 1); against null
benefit stand the free family's NA-SE costs (D-010). Applied as guidance in
`cpm_fit()` roxygen, the structure vignette, and NEWS under RR05's five
wording guardrails (scoped to model test + correlation input, never
"identical", envelope stated, no invalid-p implication, small-N conservatism
kept). Variant-C spot check: same tie. Covariance-input re-trigger recorded
as a gate in D-011.

**Key decisions:** D-011 (supersedes D-009 item 3); M21-D1 + RR05 triage
(R5 unit-seeded multi-start → infra candidate; R7 more-reps rejected).

**Review:** RB05 → RR05 (Fable; evidence re-verified from seed). 3-lens
fan-out: 1 finding (scored 78, sub-threshold) — vignette N-envelope omission —
fixed anyway to keep AC4 evidence accurate. check() 0 errors / 0 warnings /
0 notes; CI 7/7 green. **PR:** #44 (squash `eed13c0`).
