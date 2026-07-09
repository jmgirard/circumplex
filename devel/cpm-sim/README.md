# CPM confidence-interval simulation engine

Implements the engine required by the registered plan
[`devel/cpm-simulation-paper-plan.md`](../cpm-simulation-paper-plan.md) §10.
It drives the shipped `circumplex` estimator (plus the internal entry points
the B6 oracle already uses — `cpm_engine()`, `cpm_analytic_se()`, and the
resampling internals) to measure the operating characteristics of the CPM's
confidence intervals over the plan's factorial. **No package code is changed by
this study.**

> ## STATUS: written + review-fixed, NOT YET RUN (fixes 2026-07-09)
> No factorial cell, smoke, or stage-0 benchmark has been executed as the study.
> Sourcing any file only *defines* functions; `run.R` launches nothing unless
> invoked as a script with `CPM_SIM_GO=1`. Executed for verification only:
> `selftest.R` (27/27 pure-math fixtures), a config-table build (design-time
> projections), and a 3-rep single-cell kernel smoke — none of which is the run.
>
> The independent Fable review (`devel/cpm-simulation-engine-review.md`) verdict
> was *needs change before run*; all 6 must-fix (M1–M6), 11 should-fix (S1–S11),
> and the hygiene items are applied. The follow-up ratification (same file,
> "Ratification" section) is also applied: the §2.4 guard was rebuilt to the
> ratified 4-part form (N1–N3), the underfit arm re-pinned to `trail_t010`, and
> the numerical parity tripwire added to `selftest.R`. Verified: `selftest.R`
> 29/29 (incl. the tripwire, byte-identical); config builds 553 kept / 7 dropped
> (the reviewer's exact figures); underfit/overfit/wrong-fixed arms restored.

## Files (each maps to a plan §10 delta)

| File | Plan | Role |
|---|---|---|
| `common.R` | §2, §6.1, §7.1 | Pins: `BASE_SEED = 20260710`, angle/coverage conventions (span rule), Bradley bands. Sourced first. |
| `config.R` | §10.1, §2.4, §3.1 | Config-table-driven factorial; pseudo-truth projection γ\*(P₀) with its guards, F\*, population RMSEA, and boundary-status column. Ill-defined-estimand cells are dropped at design time with the reason recorded. |
| `intervals.R` | §10.2, §4 | Shared replicate-matrix generator (reimplements `cpm_bootstrap`'s loop so it can *return* the raw replicates), plus percentile / basic / BCa (grouped-jackknife acceleration + z₀-saturation/clamping accounting) / studentized / analytic-Wald / circular-θ constructors. |
| `kernel.R` | §10.3, §2.5, §5 | Per-fit fit-and-score kernel → one per-fit record (all interval endpoints × level, marker vector, flags, coverage by the §2.5 scoring rules, one-sided decomposition, T/df, NA/clamp/truncation events). |
| `summarize.R` | §10.4, §6 | Cluster-level MC intervals, the Bradley decision rule, paired method contrasts (kept params only), region aggregation. |
| `run.R` | §10.5, §7 | Portable parallel driver (PSOCK on Windows / fork elsewhere), per-cell checkpoint/resume, stage-0 throughput benchmark, the pre-registered stage-2/3 selection rules as code. |
| `selftest.R` | §2.5, §4, §6 | Pure-function fixtures for the delicate math. **22/22 pass.** |

## Review fixes applied (2026-07-09)

Against `devel/cpm-simulation-engine-review.md`:

- **M1** seed offset now `SEED_MULT (50000) * cell_index + i` with a range
  assertion in `build_config_table()` and an `i <= SEED_MAX_I` guard in the
  kernel — no `set.seed()` overflow. **M2** overfit arm generates from
  `m2_truth` (m0=2, fit m=3), and `project_truth()` wraps the engine call so an
  infeasible model is a recorded drop, not a build crash. **M3** `fit_prop()`
  returns NA for a method that scored nothing (no removed-only phantom). **M4**
  per-fit records saved by default (off via `CPM_SIM_NO_RECORDS`); schema adds
  θ̂/ζ̂/β̂ and primary-level interval endpoints. **M5** Wald-θ miss side corrected.
  **M6** stage-2/3 selection wired into `main()` (per-axis admission rule,
  derived cells, B-sensitivity cell, 3a full-vs-grouped jackknife validation, 3b
  studentized flag).
- **S1** OOF bracket expands upward (+ RMSEA assertion). **S2** BCa saturation
  counted separately from NA. **S3** one-sided fold + consistent denominator.
  **S4** secondary-level β folds the removed-harmonic score. **S5** studentized
  feasibility (>20% NA-SE ⇒ infeasible) stored + gated. **S6** jackknife floor
  scales with `g` (smoke BCa no longer all-NA). **S7** worst-case bound, >2%
  error flag, width/truncation geometry, marker-conditional coverage,
  per-item Heywood — added to the summarizer. **S8** fork `try-error` guarded.
  **S9** sanity-gate tolerances tightened to 1e-6. **S10** stage-3(d) second
  config; 3(f) reps sized for ≥400 firings. **S11** PSOCK cluster hoisted to one
  per stage. Hygiene: config-table caching, dead code removed, absolute pkg
  path, benchmark NULL-filter, stage-1 large-N restricted to a config subset.

## Design decisions worth a reviewer's eye before the real run

#1–#4 were **verified correct** by the review; #5 fixed (S4); **#6 ratified**
(the guard was rebuilt and the plan §2.4 amended):

1. **Percentile arm = verbatim `cpm_bootstrap()` reconstruction** — the one
   coupling to package internals. Now guarded by the **numerical parity
   tripwire** in `selftest.R` (DECIDE-2): asserts the engine's percentile CIs
   are byte-identical to a real `cpm_fit(ci_method="bootstrap")` call, failing
   loudly on any drift.
2. **Direct Cholesky simulation** from `P0` (covers out-of-family `P0'`; keeps
   the study Gaussian, §11), RNG contract preserved.
3. **BCa acceleration = plain skewness, no delete-d correction** (§4.3).
4. **T = (N − 1)·F̂** = the plan's `T = n·F̂` with the package's Wishart `n`.
5. **Secondary-level β** folds the removed-harmonic score at every level (S4).
6. **Pseudo-truth well-definedness (ratified, plan §2.4 amended).** The guard
   keys on **convergence + KKT-at-ceiling + statistical unimodality
   ((N_max−1)·ΔF ≥ 10) + circulant-symmetry**, not the engine's `accepted` flag.
   The ratification corrected the original rationale — `accepted = grad_ok &&
   reproduced`; it was the *reproduction* limb (not Heywood) that failed, and it
   certifies sample-fit trust, not estimand existence. A converged boundary
   projection (ζ*=1) is a legitimate estimand recorded in the boundary-status
   column (percentile can't cover ζ*=1 and analytic SE is NA there → reported as
   interval geometry, not method failure). The **underfit-interior** estimand
   was measured genuinely ill-defined (symmetry-broken cyclic orbit + a second
   basin ≤1.1 deviance units) and **re-pinned to `trail_t010`** (N3).
   Correct-spec cells still drop on numerical non-identification at F≈0. Result:
   553 kept / 7 dropped, matching the reviewer's measurement.

## How it will be run (later, on the §0.2 box)

**Operator handoff:** see [`RUNBOOK.md`](RUNBOOK.md) for the full Windows-box
checklist (Rtools/BLAS pinning, env vars, why the Mac's cache must not be
copied, the benchmark→smoke→stage sequence, resume behavior). Quick reference:

```sh
# 0. pure-math self-tests (safe; already green)
Rscript devel/cpm-sim/selftest.R

# 1. stage-0 throughput benchmark (calibrates the §7.2 wall estimate)
CPM_SIM_GO=1 CPM_SIM_MODE=benchmark Rscript devel/cpm-sim/run.R

# 2. a 25-rep end-to-end smoke over a cell subset
CPM_SIM_GO=1 CPM_SIM_SMOKE=1 CPM_SIM_MODE=1 \
  CPM_SIM_CELLS=s1_p8_equal_interior_z75_N500 Rscript devel/cpm-sim/run.R

# 3. the stages (each cell checkpoints to cache/<stage>/<id>.rds, resumable)
CPM_SIM_GO=1 CPM_SIM_MODE=all Rscript devel/cpm-sim/run.R
```

Env knobs: `CPM_SIM_MODE` (`benchmark`|`1`|`2`|`3`|`all`), `CPM_SIM_SMOKE`,
`CPM_SIM_CELLS` (comma-separated id filter), `CPM_SIM_CORES`, `CPM_SIM_CACHE`,
`CPM_SIM_SAVE_RECORDS` (write the large per-fit records), `CPM_SIM_PKG` (package
path). Stage-2/3 selection-driven admissions read the prior stage's cached
summaries.

## Home

Lives in the package repo's `devel/` for now (alongside the B6/G oracles it
extends). Per plan §7.3 the engine + committed artifacts migrate to a **separate
research-compendium repo** citing the released package version at paper time; the
package repo keeps only the release-scoped oracles.
