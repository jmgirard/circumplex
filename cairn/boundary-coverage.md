# Boundary-condition coverage matrix

_Repo-specific assurance artifact (M11). Not a status/decision file._
_Authored 2026-07-12. Owner: whichever milestone next touches boundary
behaviour — re-audit and re-cite when the line numbers move._

The four angular/boundary invariant **classes** are the ones CLAUDE.md
"Statistical invariants" requires testing whenever displacement, contrasts, or
`src/` change:

- **A** — profiles peaking exactly at 0°/360° (point estimate lands on the pole)
- **B** — displacement CIs straddling 0°/360°
- **C** — contrasts near ±180° (branch-cut agreement)
- **D** — flat / zero-variance profiles (graceful NA, no crash)

The **entry points** are the six places a user reaches the estimator. Each cell
cites a test by `file:line`, or records why the cell is covered elsewhere.
`ssm_parameters()` is the shared point-estimate engine, so class A/D at the
point-estimate level is an engine property tested once, not re-proved per path.

| Entry point | A — peak at 0/360 | B — CI straddles 0/360 | C — contrast near ±180 | D — flat / zero-variance |
|---|---|---|---|---|
| `ssm_analyze()` mean | `test-ssm_analysis.R:562` (pole via `ssm_score`); `test-ssm_bootstrap.R:261` (**M20**, pole CI endpoints report 360) | `test-ssm_montecarlo.R:94`; `test-ssm_bootstrap.R:210` | `test-ssm_bootstrap.R:147`, `:178`; `test-ssm_montecarlo.R:117`, `:224` | `test-ssm_bootstrap.R:137` (end-to-end); `test-ssm_analysis.R:488`, `:598` |
| `ssm_analyze()` correlation | `test-ssm_analysis.R:253` (**M11**, profile assembly at pole) | shared displacement/quantile engine (see mean B); correlation profile at pole → `:253` | `test-ssm_analysis.R:329`, `:386`; `test-ci_accuracy.R:72`, `:303` (correlation branch pathology) | `test-ssm_analysis.R:229` (**M11**, flat correlation profile) |
| Bootstrap engine | quantile handles the pole → `test-ssm_bootstrap.R:210`; `:238` (**M20**, pole-denoting endpoint = 2π both float representations, + no-over-fire guard) | `test-ssm_bootstrap.R:210`, `:1` | `test-ssm_bootstrap.R:147`, `:178` | `test-ssm_bootstrap.R:137`, `:95`, `:114` (all-NA column) |
| Monte Carlo engine | engine-level point estimate (see mean A); pole interval → `:94` | `test-ssm_montecarlo.R:94` | `test-ssm_montecarlo.R:117`, `:224` | `test-ssm_montecarlo.R:141` (flat + singular covariance) |
| `ssm_ci_accuracy()` | `test-ci_accuracy.R:343` (population peaks at 0/360) | `test-ci_accuracy.R:50` (membership mod 360) | `test-ci_accuracy.R:72`, `:562` | `test-ci_accuracy.R:419` (flat population refused) |
| SEM (`ssm_sem`) | `test-ssm_sem.R:181`; `test-ssm_sem_groups.R:442` | `test-ssm_sem.R:181`; `test-ssm_sem_groups.R:442`, `:463` | `test-ssm_sem_groups.R:182` (group contrast) | `test-ssm_sem.R:239`, `:58`, `:209`; `test-ssm_sem_groups.R:466` |
| `cpm_fit()` free scaling (**M18**) | `test-cpm_boundary.R:22`, `:40` (pole item recovers; σ̂ untouched) | shared circular-quantile engine (angle CIs reuse `quantile.circumplex_radian`; σ block orthogonal to angles) → unit-path B | N/A — a CPM fit has no angular contrast estimand | `test-cpm_boundary.R:60`, `:72`, `:86` (singular / zero-variance / near-flat refused, fail-closed) |

## Audit notes (M11)

- No empty cells. The **mean** path was already complete before M11; the two
  gaps were the **correlation** entry point's flat (D) and pole (A) corners,
  which run distinct profile-assembly plumbing — closed by
  `test-ssm_analysis.R:229` and `:253`.
- Cells marked "shared … engine" are deliberately not re-tested per path: the
  displacement value, its circular-quantile CI, and the ±180° branch cut are
  computed by one shared code path (`ssm_parameters()` / the displacement
  quantile / `angle_dist()`). Re-testing them per entry point would test the
  implementation, not the contract (tracking-rules "What gets a test").
- SEM class C lives only on the grouped path (`ssm_sem` contrasts require
  ≥2 groups); the single-group SEM path has no contrast estimand, so no C cell
  is expected there.
- **CPM free scaling (M18):** the σ (variance-scale) block is orthogonal to the
  angle block (spec sec. 5, pins 2–3), so the pole (A) and flat (D) behaviour is
  the estimator's, exercised on the new `scaling = "free"` path in
  `test-cpm_boundary.R`. Class B reuses the shared circular-quantile machinery
  unchanged; class C is not a CPM estimand (no angular contrast). The unit-path
  CPM pole/flat coverage stays in `test-cpm_fit.R:190`, `:245`.

## Audit notes (M20)

- **Pole-endpoint labeling (class A refinement):** a CI endpoint *denoting*
  the 0/360 pole now reports 360, never 0 (value-level snap in
  `quantile.circumplex_radian`; D-003's parked follow-up). Cells:
  `test-ssm_bootstrap.R:238` (primitive, both float representations of the
  pole + a no-over-fire straddle guard), `:261` (SSM end-to-end, deterministic
  pole-peaking profile), `test-cpm_angle_ci.R:50` (CPM end-to-end, unit path,
  reference scale at theory 360 — also covers the CPM reported-angle
  pole = 360 alignment, M20-D1). Guard teeth proven pre-fix (6 red).
- Cited line numbers above re-audited after the M20 test insertions: every
  pre-M20 cite in this matrix precedes the inserted blocks and is unchanged;
  the M13 CPM e2e test moved (49 → 78, not cited here).
