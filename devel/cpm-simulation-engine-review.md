# CPM simulation engine — pre-run statistical-correctness review

**Reviewer:** independent Fable-tier review, 2026-07-08.
**Scope:** `devel/cpm-sim/` (common.R, config.R, intervals.R, kernel.R,
summarize.R, run.R, selftest.R, README.md) against the registered plan
`devel/cpm-simulation-paper-plan.md`, DESIGN.md/CLAUDE.md conventions, and the
package internals it drives (`R/cpm_fit.R`: `cpm_engine`, `cpm_analytic_se`,
`cpm_bootstrap`, `cpm_unpack`, `cpm_boundary_markers`) plus the B6 oracle
(`devel/m4-coverage-oracle.R`).
**Method:** code trace + independent re-derivation of every challenged formula;
`selftest.R` executed (22/22 pass, confirmed); eight pure-function fixtures run
to confirm or refute specific concerns (no CPM fitting, no factorial/smoke/
benchmark cell executed; scratchpad script `review-fixtures.R`, results quoted
inline below). Oracle discipline observed: no outcome was checked against any
remembered coverage number; `devel/g2xx1.txt` untouched.

---

## Verdict

**Needs change before run.** The delicate mathematics the brief flagged is
almost entirely correct — the BCa construction, the no-delete-d acceleration,
the basic/studentized reflections, the span rule, and the percentile-arm
reconstruction all check out against independent derivation. But six defects
would corrupt or waste the run as-is, two of them fatal on the first
invocation: the per-replicate seed scheme overflows R's integer range for
roughly three-quarters of the cell table (every affected cell yields 100%
errored replicates), and the overfit misspecification arm requests `m = 4` at
`p = 8` under variant A, which exceeds the shipped identification cap and
crashes `build_config_table()` before anything runs. Stage 1 could run after
fixing M1–M5; stages 2–3 additionally require M6.

---

## Must-fix before run

### M1. Per-replicate seed offset overflows `set.seed()`'s integer range — ~74% of cells produce zero data

**Where:** [kernel.R:54–55](cpm-sim/kernel.R) (`offset <- 1e7 * cell_index + i;
set.seed(BASE_SEED + offset)`); `cell_index` assigned in
[run.R:164](cpm-sim/run.R) as the cell's position in the full config table.

**Defect.** The config builders produce ≈ 812 cells (stage 1: 648 core + 18
het + 9 provocation + 27 misspec + 9 fixed-B = 711; stage 2: 60; stage 3:
37 + 4 OOF). `set.seed()` requires a value representable as a 32-bit integer
(max 2,147,483,647). `BASE_SEED + 1e7·cell_index + i` crosses that at
`cell_index = 213`:

```
offset at cell 212: 2,140,260,711  -> set.seed ok
offset at cell 213:                -> Error: "supplied seed is not a valid integer"
```

(fixture-confirmed). The error is thrown inside `fit_and_score()`, is caught by
`run_cell()`'s `tryCatch(..., NULL)`, and is **counted as an ordinary worker
error** — so every cell with index ≥ 213 (~600 of 812) silently completes with
`n_error = reps` and an empty summary. Nothing halts. The study would "finish"
with three-quarters of the factorial empty, and stage-2/3 selection rules would
then operate on the surviving quarter.

**Why it corrupts the study:** total, silent loss of most of the factorial;
also violates §6.3.1 (per-replicate seeds "derived ... so results are identical
for any core count"): the seeds don't exist at all for most cells.

**Fix direction.** Shrink the multiplier and assert the bound at
config-build time. `i ≤ 2000` (stage-1 reps) and cells ≤ ~1000, so e.g.
`offset <- 4096L * cell_index + i` is unique per (cell, replicate), leaves the
`i ∈ (2048, 4096]` sub-block free for the §6.3.6 uniform top-up provision, and
tops out near `BASE_SEED + 3.3e6 ≪ 2^31`. Add
`stopifnot(BASE_SEED + 4096 * n_cells + 4096 < .Machine$integer.max)` in
`build_config_table()` and `stopifnot(i <= 2048)` in the kernel. Any scheme
works provided (a) uniqueness over (cell, replicate) including future top-up
blocks, (b) range-validity, (c) per-replicate locality (worker independence) —
the current scheme satisfies (a) and (c) but not (b).

### M2. Overfit arm is infeasible under the shipped m-cap — `build_config_table()` crashes; also mis-implements plan RQ6(a)

**Where:** [config.R:285–289](cpm-sim/config.R) (`s1_overfit_*`: `m0 = 3`,
`m_fit = 4`, variant A, `p8_equal`) and [config.R:371–375](cpm-sim/config.R)
(`s3d_overfit_*`, same); `project_truth()` at config.R:116–121 has no
`tryCatch`.

**Defect.** `cpm_spec()` caps `m ≤ floor((p−1)/2) = 3` for variant A at p = 8
(R/cpm_fit.R:112–121). Fitting `m = 4` therefore stops:
`"m = 4 exceeds the identification cap (3) for variant A at p = 8"`
(fixture-confirmed). The error propagates uncaught through
`project_truth → resolve_cell → build_config_table`, so **every invocation of
run.R dies during config build** — after burning ~675 deterministic projection
fits on the cells that precede it.

**This is also a plan deviation, and the plan shows the intended fix.** RQ6(a)
defines the overfit arm as *"true m = 2, fitted m = 3 — manufacturing a true
boundary"*, and §3.1 supplies exactly the generating config for it:
`m2_truth = (.45, .35, .20)` — which the engine defines
([config.R:29](cpm-sim/config.R)) and then **never uses in any cell builder**
(orphaned; grep-confirmed). The engine's substituted `m0 = 3 / m_fit = 4`
version is not a legal model in this family.

**Fix direction.** Overfit cells (stage 1 and 3d) generate from
`BETA_CONFIGS$m2_truth` (`m0 = 2`) and fit `m_fit = 3`, `truth_source =
"generating"` — the existing zero-padding in `resolve_cell()`
(config.R:195–197) already aligns the truth (`β₃* = 0` exactly). Additionally,
wrap `project_truth`'s engine call in `tryCatch` so an infeasible cell becomes
a recorded drop rather than a build crash.

### M3. `fit_prop()` fabricates coverage for methods that scored nothing (phantom bootstrap rows; contaminated Wald rows)

**Where:** [summarize.R:22–29](cpm-sim/summarize.R).

**Defect.** For `family = "beta"`, when the requested method has no row in the
fit's `cover` matrix, `cv` is `NULL` — and the removed-harmonic single score is
then appended to `NULL`, so the per-fit "coverage proportion" for that method
is computed **from the removed-harmonic score alone**. Fixture: an
analytic-only record with one removed harmonic returns
`fit_prop(rec, "beta", "percentile") = 1` for a method that produced no
interval. Consequences:

- **Stage-1 (analytic-only) cells report percentile/basic/BCa β coverage
  rows** built entirely from removed-harmonic indicator scores (any polished
  fit contributes; `summarize_cell`'s `all(is.na(props))` skip never triggers).
- **Wald β coverage in `se_na` fits** (analytic SEs unavailable — 65–78% of
  marker-fired fits per M7) is likewise replaced by the removed-only score
  instead of NA, contaminating the Wald column in exactly the boundary cells
  the study is about.

**Why it corrupts the study:** phantom and contaminated method×family coverage
estimates feed the Bradley verdicts, the stage-2/3 selection scalars, and the
RQ2 method-comparison story.

**Fix direction.** In `fit_prop()`, if `cv` is `NULL` (method scored nothing),
return `NA_real_` before folding the removed score. The §2.5 fold applies only
when the method actually produced kept-parameter scores in that fit.

### M4. Per-fit records are discarded by default, and the record schema omits γ̂ and all interval endpoints (plan §10.3)

**Where:** [run.R:186](cpm-sim/run.R) (`save_records` defaults to off via
`CPM_SIM_SAVE_RECORDS`), [run.R:84](cpm-sim/run.R) (records dropped after
summarization); [kernel.R:189–208](cpm-sim/kernel.R) (record contents).

**Defect, two parts.**

1. Plan §7.1 states per-fit records "are written per cell for regeneration but
   not committed" — writing them is the registered default, committing is the
   opt-out. The engine inverts this: unless an env var is set, `run_cell()`
   summarizes and throws the records away. Everything the summarizer does not
   compute (which per S3/S7 below includes several pre-registered outcomes —
   the §6.3.3 worst-case bound, the RQ4 conditional-coverage-given-marker
   table, §5.2 width/zero-width/truncation aggregation, the RQ4(b)
   retained-harmonics marker re-scoring) becomes **unrecoverable without
   re-running a multi-day study**.
2. Even with records saved, the schema deviates from §10.3, which requires
   "γ̂, all interval endpoints by method × level". The record stores coverage
   indicators, miss sides, widths, and ζ̂ (`heywood_zeta`) — but **no θ̂, no
   β̂, and no interval endpoints at any level**. That forecloses: §5.3
   estimator behavior for θ (circular mean/SD of angular error) and β
   (bias/median bias/RMSE); any post-hoc re-scoring; and exact regeneration
   checks.

**Fix direction.** Default `save_records = TRUE` (per-cell RDS, uncommitted —
the plan's own storage posture; 128 GB and per-cell files make size a
non-issue). Add to the record: `theta_hat` (degrees), `beta_hat`, and an
endpoints array (method × parameter × level, `lci/uci`) — the objects already
exist in `fit_and_score()`/`score_levels()` at the point of scoring.

### M5. Wald-θ one-sided miss labels are inverted

**Where:** [kernel.R:115–116](cpm-sim/kernel.R).

**Derivation.** `ang_signed(a, b) = −((a − b + 180) %% 360 − 180)` is the
signed shortest rotation **from a to b**, i.e. `≈ b − a` wrapped to
(−180, 180] (check: `ang_signed(30, 40) = +10`). The study's convention
(shared by `lin_miss_side` and `angle_miss_side`, selftest-pinned) names the
miss side by where the **truth** lies relative to the interval: truth below →
`"lower"`. For a Wald-θ miss, truth below the interval ⇔ truth clockwise of
the estimate ⇔ `tt − est < 0` ⇔ `ang_signed(est, tt) < 0`. The kernel instead
tests `ang_signed(tt, est) < 0`, which is `est − tt < 0` — the opposite side.
Fixture: truth 30 below estimate 60 → kernel labels `"upper"`; the shared
convention (`lin_miss_side(50, 70, 30)`, `angle_miss_side(50, 70, 30)`) labels
`"lower"`.

**Why it corrupts the study:** the §5.1 one-sided decomposition for analytic θ
intervals — part of the RQ3/RQ4 directional story — is recorded with flipped
sides, and (given M4's missing θ̂/endpoints) cannot be repaired post hoc.
Coverage itself is unaffected (it uses `abs()`).

**Fix direction.** Swap the arguments:
`ifelse(ang_signed(est_free, tt) < 0, "lower", "upper")`.

### M6. The pre-registered stage-2/3 adaptive machinery is unwired or absent (blocks stages 2–3, not stage 1)

**Where:** [run.R:106–126](cpm-sim/run.R), [run.R:168–169](cpm-sim/run.R),
[config.R stage builders](cpm-sim/config.R).

**Defects, itemized:**

- `stage2_admissions()` and `studentized_cells()` are defined but **never
  called** from `main()`/`run_stage()` (grep-confirmed), and no code path
  converts an admitted stage-1 cell id into a runnable bootstrap-armed stage-2
  cell (stage filtering is by the cell's own `stage` tag).
- `stage2_admissions()` implements a plain top-12-overall ranking. The
  registered rule (§3.4) is different: *"admit the worst-ranked cell at each
  factor-axis level, in overall rank order, until the 12-cell cap binds"*,
  deduplicated. The per-factor-axis guarantee ("every factor gets
  bootstrap-interval evidence where it looks worst") is the point of the rule;
  the implemented version can spend all 12 slots on one axis. Also, its
  `core_ids` exclusion compares stage-1 ids against stage-2 ids, which can
  never match (`s1_…_z75_N…` vs `s2_…_N…`; fixture-confirmed), so stage-1
  duplicates of core configs are admissible as redundant cells; and nothing
  excludes analytic-extension cells (N up to 50000) from admission to a
  bootstrap stage the budget can't carry.
- The stage-2 **B-sensitivity cell** (trailing-t=.05 × octants × N=500 at
  B = 2000, §3.4) is not built anywhere — the `boots2000` flag is read
  (run.R:169) but never set.
- The **stage-3a full-vs-grouped jackknife validation arm is absent.** This is
  not an optional extra: per §4.3 it *gates* the grouped acceleration
  estimator ("agreement ... gates the grouped estimator"). The mechanism
  exists (`grouped_jackknife(g = N)` yields delete-1), but no cells or driver
  logic invoke it.
- The **stage-3b studentized flag is never set**: `flags$studentized` is read
  (run.R:168) but no builder or admissions path sets it, so the studentized
  arm cannot run.

**Fix direction.** Wire the two selection functions into `main()` (stage-2/3
entry reads the prior stage's cache, constructs cells from admitted ids with
`stage`/`bootstrap`/flags set); implement the per-axis admission rule as
registered; build the B-sensitivity cell with `flags$boots2000`; add the 3a
pair of validation cells (grouped + full jackknife on the same fitted
datasets); set `flags$studentized` on the 3b admissions. Stage 1 is unaffected
— but per the plan these rules must exist *as code* before adaptivity begins,
so land them before stage 2, not after.

---

## Should-fix

### S1. Out-of-family bracket search moves the wrong way when RMSEA undershoots — silently degenerates the OOF arm to in-family

**Where:** [config.R:90–94](cpm-sim/config.R).

```r
s_hi <- 0.5
while (is.na(pop_rmsea(s_hi)) || pop_rmsea(s_hi) < target_rmsea) {
  s_hi <- s_hi / 2 ...
```

If `pop_rmsea(0.5)` comes back **below** target (or NA — e.g. the projection
failing acceptance on the heavily perturbed matrix — with the first non-NA
value below target), the loop *halves* `s_hi`, which moves the RMSEA further
below target, terminating at `s_hi < 1e-4`; bisection over [0, ~0] then
returns `s ≈ 0` and the "out-of-family" population is P₀ up to a vanishing
perturbation. The four `s3e_oof_*` cells would then be mislabeled duplicates
of in-family cells, and the paper's §3.3 robustness claim would be tested at
RMSEA ≈ 0, not ≈ .05. (Each condition evaluation also calls `pop_rmsea` —
a full multi-start engine fit — twice per iteration.)

**Fix direction:** on `r < target`, *double* `s_hi` (expand the bracket
upward); halve only on NA. And add the missing gate: after resolution, assert
`abs(cell$rmsea_pop − target_rmsea) < tol` for OOF cells in
`build_config_table()` — the plan's "computed, not assumed" deserves an
assertion, not just a recorded column.

### S2. BCa "z₀ saturation" accounting is indistinguishable from the NA rate

**Where:** [intervals.R:195–197](cpm-sim/intervals.R) (`na_ret` hard-codes
`saturated = TRUE` for **every** NA reason, including `B_used < floor` and
`acceleration NA`); [intervals.R:203–209](cpm-sim/intervals.R).

Mechanics check (fixture-confirmed): when z₀ = ±∞, `adj()` produces NaN for
every value of `a` (via `0·∞` when a = 0, `∞/∞` otherwise), so saturation
always routes through `na_ret("z0 saturated")` — and on the non-NA path
`saturated <- !is.finite(z0)` is consequently always FALSE. Net effect: the
per-fit `bca_acct$saturated` ≡ `bca_acct$na`. The plan pre-registered
saturation as a *separately measurable* outcome (§4.2, RQ3 note: "'BCa fails
by saturation' is a measurable outcome"); as built it cannot be separated from
jackknife-failure or B-floor NAs. **Fix:** set `saturated = TRUE` only in the
z₀ branch (pass it into `na_ret`), and have the kernel aggregate per-reason
counts (the `reason` field already exists but is dropped by `bca_acct`).
Returning NA for a saturated z₀ is itself a defensible reading of §4.2 —
Efron's construction is genuinely undefined there — provided it is *counted as
saturation*, which is exactly what the conflation breaks.

### S3. One-sided decomposition: removed harmonics omitted, and denominators inconsistent with coverage

**Where:** [summarize.R:33–46](cpm-sim/summarize.R) (`fit_miss_prop`).

Two related defects. (a) §2.5 requires the removed-harmonic single score's
miss to be attributed "to the side of the truth" in both one-sided tallies;
`fit_miss_prop` never folds `beta_removed` in, while `fit_prop` does fold it
into coverage — so covered + lower-miss + upper-miss ≠ 1 for β in polished
cells. (b) The miss denominator is `length(ms)` (all indicators, including
those whose interval was NA and thus scored neither cover nor miss), while
`fit_prop` excludes NA-scored indicators — a second reconciliation failure
that deflates one-sided rates wherever BCa/Wald NAs occur. **Fix:** fold the
removed score (side = `"upper"` iff truth > 0, i.e. above the degenerate
[0,0]; here always upper since β truths are ≥ 0, but write it generally), and
use the count of non-NA-scored indicators as the denominator. Both are
summary-layer fixes, recoverable post hoc *only if* M4 (records) lands.

### S4. Secondary-level (90/99) β coverage omits the removed-harmonic fold (README flagged choice 5)

**Where:** [kernel.R score_levels](cpm-sim/kernel.R),
[summarize.R:119–136](cpm-sim/summarize.R). The author flagged this; my
verdict: **fix before the run**, not because the secondary levels are headline
material but because the inconsistency produces an artifact: in polished cells
the primary-level β proportion folds a (usually covering, at trailing-t = 0)
extra indicator that the 90/99 proportions lack, so the 90% curve can sit
*above* the 95% curve — a visibly wrong level-monotonicity that a referee will
find. The fold is level-independent and one line at either layer; records
(`beta_removed`) plus the level table's `n` make it post-hoc recoverable, but
cheap now.

### S5. Studentized feasibility rule (§4.4) unimplemented and its input discarded

**Where:** [kernel.R:129–135, 154–161](cpm-sim/kernel.R) (only `lci/uci`
extracted; `studentized_one`'s `na_rate` dropped),
[summarize.R](cpm-sim/summarize.R) (no infeasibility handling despite
intervals.R:232's comment "handled by the summarizer"). The pre-registered
rule — a cell whose NA-SE rate exceeds 20% "reports the method as infeasible
rather than its coverage" — cannot be applied because the rate is never
stored. Store `na_rate` in the record (and aggregate + gate in
`summarize_cell`) before stage 3b runs.

### S6. Smoke mode silently kills BCa: `jack_g = 25` < `g_used_floor = 50`

**Where:** [run.R:32](cpm-sim/run.R) vs [intervals.R:109,
144](cpm-sim/intervals.R). Every smoke-mode acceleration is NA (floor test on
25 pseudo-values), so every smoke BCa interval is NA with reason
"acceleration NA". The end-to-end smoke — the §3.4 stage-0 gate this engine
must pass before the real run — would show BCa 100% NA and either falsely
block the run or, worse, train the operator to ignore BCa NAs. Scale the floor
with g (e.g. `g_used_floor = max(10L, g %/% 2L)`) or set smoke `jack_g ≥ 50`.

### S7. Pre-registered reporting rules missing from the summarizer

**Where:** [summarize.R:89–212](cpm-sim/summarize.R). Not computed anywhere:
(a) the §6.3.3 **worst-case bound** (non-accepted fits scored as misses)
promised beside every headline claim — the summarizer's own header comment
claims it, but no code computes it; (b) the §6.3.2 **> 2% error flag/
annotation**; (c) §5.2 aggregation of widths / zero-width / truncation /
clamping (per-fit values exist in records; nothing aggregates them); (d) the
RQ4/§5.5 conditional-coverage-given-marker, false-alarm-rate, and
retained-harmonics-variant tables. All are computable post hoc from saved
records (M4), so this is a should-fix for the summarizer — but (a) and (b)
guard headline claims and are cheap to add now.

### S8. Fork-backend worker crashes become "records"

**Where:** [run.R:77–78](cpm-sim/run.R). `mclapply` can return `try-error`
objects on fork-level failures; `vapply(out, is.null, ...)` passes them into
`summarize_cell`, whose `r$status$accepted` access then errors mid-stage. B6
explicitly counted `try-error`s; restore that guard
(`is.null(x) || inherits(x, "try-error")`). PSOCK is safe (the kernel-level
`tryCatch` catches evaluation errors), so this bites the dev-box path — where
the smoke will run.

### S9. Sanity-gate tolerances loosened vs the plan's exactness assertion

**Where:** [config.R:201–203](cpm-sim/config.R) (θ 1e-3°, ζ 1e-4, β 1e-3) vs
§2.4/§10.6: `make_truth()` "asserted to recover γ₀ to 1e-6 ... carries over
unchanged" (B6 used 1e-6 on ζ and β). The looser gate only weakens a guard —
the coverage truth is γ₀ regardless — but it is a silent deviation from a
registered constant and would mask a real projection defect two orders of
magnitude larger than B6 would have tolerated. Tighten ζ/β to 1e-6 (θ to
~1e-4°, the optimizer's realistic angle precision at `rel.tol = 1e-12`), or
record the deviation and its reason in the plan's change log.

### S10. Stage-3 scope deviations: 3(d) halved, 3(f) sizing rule absent

**Where:** [config.R:369–394](cpm-sim/config.R). (d) The plan registers the
misspec bootstrap slice as "2 configs × 3 N × 3 specs = 18 cells"; the engine
builds 9 (interior config only). (f) The provocation-multimodal cells are
pinned at 2 cells × stage-3 default 500 reps with no firing-rate arithmetic,
though the registered rule sizes reps for **≥ 400 expected firings** using G's
measured rates — the explicit lesson of G's underpowered 114-firing estimate.
Either implement (second config for d; reps-from-firing-rate for f) or record
the scope reduction in the plan before stage 3.

### S11. Per-cell cluster construction + `load_all()` will dominate stage-1 wall time

**Where:** [run.R:48–65, 68–87](cpm-sim/run.R). Every `run_cell()` builds a
fresh PSOCK cluster and every worker re-runs `devtools::load_all()` — tens of
seconds of setup per cell against stage-1 cells that themselves cost only
~20–35 s of compute (2000 cold fits at the measured 60–120 fits/s). Over ~711
stage-1 cells this roughly doubles the stage and distorts the stage-0
benchmark (whose timing includes one cluster construction). Hoist cluster
creation to `run_stage()` (create once, pass through), and have the benchmark
time only the mapped work. Not a correctness issue; flagged because the plan's
3–5-day gate arithmetic (§7.2) is part of the registered design.

---

## Hygiene

- **Projection caching claim is false; config table never cached.**
  `resolve_cell()`'s comment says the projection is paid "once per
  (population, fitted model), not once per N", but only the *population
  matrix* is cached — `project_truth()` runs per cell (~800 multi-start
  projections instead of ~90), single-threaded, on **every** run.R invocation
  (each stage, every resume). config.R's own header says "Cache the returned
  object; run.R consumes it" — run.R doesn't. Cache the resolved table to RDS.
- **Dead/misleading code:** `level_props()` (kernel.R:45–48) is never called;
  `defined <- !is.na(ms) | TRUE` (summarize.R:41) is a no-op left over from an
  edit; `ci_percentile_theta` computes a quantile for the constant reference
  column that is then discarded; `score_linear` computes truncation rates for
  percentile/BCa/Wald where they are structurally impossible (harmless, but
  the record column invites misreading).
- **θ interval widths are never computed** (kernel.R:106 comment promises a
  degree conversion "for width" that doesn't happen), so §5.2's angular-width
  geometry for θ is absent even from records — worth adding alongside M4's
  endpoint storage.
- **`project_truth()` cannot see the canonicalization-undecided warning**
  (config.R:117 `suppressWarnings`) — §2.4 asks that the projection's
  "mirror/canonicalization ... be checked"; capture `canon`-related warnings
  or expose a flag instead of suppressing everything.
- **Stage-1 het slice is a narrow reading of §3.1** (2 patterns × interior β ×
  octants only). Defensible given the plan's own ~600-cell arithmetic, but it
  is a scope choice the plan text doesn't pin — record it.
- **Region-verdict caller does not exist yet**, so the §6.1 expected
  false-flag count "printed beside every region claim" has no implementation;
  `region_verdict()` also silently drops "undetermined" cells from the
  denominator (reasonable, but unregistered — note it).
- **Per-item Heywood pile-up** (§5.3, `P(ζ̂_i > .995)` per item) is not
  aggregated; recoverable from `heywood_zeta` in records.
- **Benchmark arithmetic** counts errored replicates as full fit batches and
  runs `min(cores, 8)` replicates on `cores` workers (fine at 8+, odd below);
  worth a NULL filter.
- **`cpm_sim_pkg` defaults to a relative path** exported to PSOCK workers;
  normalize to an absolute path before export.
- **Stage-1 large-N extension runs on all configs** (711 cells vs the plan's
  "~600 ... extension on a config subset"). Cheap (analytic-only), but it is a
  quiet scope expansion; either trim or note.

---

## C. The five flagged translation choices

1. **Percentile arm as verbatim `cpm_bootstrap` reconstruction — CORRECT.**
   Line-by-line comparison of `sim_replicates()` (intervals.R:27–89) against
   `cpm_bootstrap()` (R/cpm_fit.R:1038–1144): identical up-front
   `sample.int(N, N*boots)` index block, identical degenerate-resample test
   (NA or min eigenvalue ≤ 1e-10, then symmetrize), identical warm start +
   scaled-gradient acceptance with one deterministic restart, identical mirror
   guard, identical complete-case `ok` rule, and the θ percentile path calls
   the same `quantile.circumplex_radian` on wrapped radians. The only
   additions (returning raw matrices; optional per-replicate SEs) don't touch
   the stream or the acceptance logic. The declared coupling is real; since a
   drift would silently de-couple the "shipped default" claim, add a cheap
   tripwire: a selftest that hashes `deparse(cpm_bootstrap)` against a pinned
   value, failing with "re-sync sim_replicates" when the package loop changes.
2. **Direct Cholesky simulation vs `cpm_simulate()` — CORRECT.**
   `simulate_dataset()` right-multiplies N×p standard normals by
   `chol(P0)` (upper-triangular U with UᵀU = P0), giving exact population
   correlation P0 — the same Gaussian law `cpm_simulate()` produces via the
   factor representation, and the only uniform option for out-of-family P₀′
   (no CPM factor form exists). PD is guaranteed on both paths (engine PD
   check for implied P0; eigen-clip at 1e-6 + congruence rescale for OOF,
   which preserves PD). Per-replicate `set.seed` before the draw keeps the
   §7.1 contract; that the stream *mapping* differs from `cpm_simulate` is
   irrelevant — BASE_SEED is disjoint from B6/G by design and no cross-run
   RNG identity is claimed.
3. **No-delete-d acceleration — CORRECT.** Independent re-derivation:
   the full delete-1 acceleration is a = Σ(t̄−t₍ᵢ₎)³ / {6[Σ(t̄−t₍ᵢ₎)²]^{3/2}}.
   With delete-d blocks, t̄ − t₍ᵢ₎ = c·Sᵢ where Sᵢ = Σ_{j∈block i} L_j and
   c = 1/(N−d) is common to all blocks; the numerator scales as c³ and the
   denominator as (c²)^{3/2} = c³, so c cancels exactly — no delete-d variance
   factor belongs anywhere, and importing (N−d)/(N·d) into the denominator
   alone would break this (the §4.3 hazard). Then E[Sᵢ³] ≈ d·μ₃, E[Sᵢ²] ≈ d·μ₂
   give a ≈ skew(L)/(6√(g·d)) = skew(L)/(6√N), the delete-1 value to first
   order (g = N, d = 1 recovers it identically). `bca_acceleration()`
   implements the plain formula with the correct Efron sign (deviations
   t̄ − t₍ᵢ₎, not t₍ᵢ₎ − t̄) and no correction factor; the selftest pins the
   rescaling invariance. The pseudo-value fed in is the delete-group refit
   statistic t₍ᵢ₎ — the right quantity; feeding influence values Uᵢ would give
   the same a by the same invariance, so no defect either way. Block
   construction (`ceiling(seq_len(N)/(N/g))`, fixture: N = 250 → 100 blocks of
   2–3), the failure rule, and the g_used = 50 floor match §4.3; the mirror
   guard is applied to jackknife refits as §4.3 requires.
4. **T = (N−1)·F̂ vs the plan's "n·F̂" — CORRECT, no conflict.** The plan's
   RQ5 statistic is "the B6 statistic", and B6 scored `fit$fit$chisq`, which
   is `cpm_fit_indices()`'s `T = n·F̂` with `n = N − 1` (the Wishart
   multiplier, design §3.1). The plan's "n" is the package's n, not the raw N.
   `Tstat <- (cell$N - 1L) * eng$F` (kernel.R:90) therefore implements the
   registered intent, and the χ²_df reference is the matching Wishart
   asymptotics. Recommend one clarifying sentence in the paper's methods
   ("T = (N−1)F̂ throughout") so "n·F̂" is never misread as N·F̂.
5. **Secondary-level β folding — DEFECT (mild), fix before run.** See S4: the
   omission is disclosed and level-independent, but the resulting
   primary-vs-secondary inconsistency manufactures non-monotone level curves
   in polished cells. One-line fix at either layer.

## B. The seed-range concern (explicit line)

**Confirmed, and worse than a range concern:** uniqueness holds
(`1e7 > max reps`, distinct `cell_index` per cell), but `BASE_SEED +
1e7·cell_index + i` exceeds 2³¹ − 1 from `cell_index = 213` of ~812, and
`set.seed()` then *errors*; the error is swallowed as a counted worker error,
so ~74% of cells complete empty rather than crash. Worker-count/schedule
independence (§7.1) is otherwise correctly achieved by per-replicate local
seeding — it just has to exist for all cells. Fix per M1.

---

## Verified correct (so the author need not re-litigate)

- `angle_covered()` span rule (anchor-free, wrapped-interval- and pole-safe;
  selftest-pinned) and its use with `truth %% 360`; Wald-θ coverage via
  `|ang_signed| ≤ z·SE`, which is exact for interval half-widths < 180° and
  correctly degenerates to "always covered" at ≥ 180°.
- `bca_one()`'s adjusted-quantile formula reproduces Efron's
  α₁ = Φ(z₀ + (z₀+z_α)/(1 − a(z₀+z_α))) exactly (fixture 7, hand-derived
  endpoints match to 5 dp); mid-rank z₀ ties per §4.2; B_used and point-mass
  guards scoped to kept parameters per §2.5/§4.2. Type-7 quantile
  interpolation is a defensible, internally consistent choice: it matches the
  shipped percentile default (`stats::quantile` defaults in `cpm_bootstrap`),
  keeping the paired methods on one interpolation rule; the O(1/B) difference
  from `boot.ci`'s normal-scale interpolation is pre-registered geometry, not
  error — worth one supplement sentence.
- Basic interval reflection (`[2t̂ − q_{1−α/2}, 2t̂ − q_{α/2}]`, upper/lower
  correctly crossed) and raw (untruncated) scoring with truncation-rate
  geometry, per §4.1. Studentized reflection likewise correct
  (`t̂ − q_{1−α/2}·SE, t̂ − q_{α/2}·SE`).
- Removed-harmonic scoring at the primary level: scored once, cover iff truth
  exactly 0, attributed identically across methods, excluded from contrasts
  (`contrast_ci` operates on kept-parameter rows only, NA-pairwise) — §2.5 and
  §6.2 as registered. Wald's fold is consistent too: the shipped analytic path
  gives a removed harmonic SE = 0, whose zero-width [0,0] interval scores
  identically to the single-score rule.
- Cluster-level inference: one per-fit proportion per fitted dataset,
  normal-theory t interval, no Wilson/binomial anywhere; Bradley bands
  (95% → [.925, .975]) and per-side bands ([.5α/2, 1.5α/2]); verdict logic and
  region aggregation (≥ 95%/none rule) match §6.1 (selftest-pinned).
- Pseudo-truth machinery: projection to the exact P₀ as the estimand
  definition, acceptance + multimodality guards dropping cells at build time
  with recorded reasons, F*/population-RMSEA storage, boundary-status column —
  §2.4 as registered (modulo S9's tolerances and the hygiene note on the
  canonicalization warning).
- Angle sets, β configs (including the trailing ladder's β₀ absorption), ζ
  patterns, N grids, stage-2 core (6 × 2 × 5 = 60), stage-3 (c)/(e)/(g)
  compositions, and the equal-spacing flags all match the plan's pinned
  values; `p8_clustered` matches the registered set with its single maximal
  90° gap.
- RNG contract inside a replicate: fixed consumption order (simulate → engine
  fit (deterministic) → index block → deterministic refits), no RNG in the
  jackknife, studentized toggle consumes no extra stream — worker-count and
  schedule independence hold per §7.1 once M1 lands.

## Suggested order of work

M1, M2, M3, M5 (small, unblock everything) → M4 + S3/S7 (record schema and
summarizer, together) → S1, S2, S4, S5, S6, S8, S9 (each ≤ ~20 lines) → M6 +
S10 (stage-2/3 wiring, before stage 2) → S11 + hygiene. Re-run `selftest.R`
plus new fixtures pinning M1's bound assert, M3's NA behavior, M5's side
convention, and S2's per-reason accounting; then the stage-0 smoke.

---

# Ratification of the §2.4 guard change + reconstruction tripwire (2026-07-09)

Same reviewer, follow-up scope only (the two decisions + defects they surface).
Evidence: design-time probes on exact population matrices — the underfit
projection's boundary forensics, an 11-start basin scan of every
projection-truth estimand family plus redesign candidates, and a full
`build_config_table()` build (allowed by the follow-up brief; no factorial/
smoke/benchmark cell, no data-fitting). `sim_replicates()` re-diffed against
`cpm_bootstrap()` post-fixes: still verbatim.

## DECIDE 1 — the §2.4 well-definedness guard

**Verdict: RATIFY the re-key away from `accepted` (with a corrected rationale
and an upgraded guard, amendment below); REJECT the specific estimand the new
guard admits — the underfit-*interior* pseudo-truth is ill-defined, and the 12
cells carrying it (`s1_underfit_*` ×9, `s3d_underfit_interior_*` ×3) must be
redesigned before the run. The measured redesign (`trail_t010` as the underfit
generating config) passes every check.**

### First, correct the record

The config.R:148–150 rationale — "`accepted` … bundles the Heywood condition"
— is factually wrong: `accepted = grad_ok && reproduced` (R/cpm_fit.R:632–635);
Heywood is a separate flag it never reads. What failed on the underfit
projection was the **`reproduced`** limb: of the six standard starts, exactly
one reached min-F (probe: `at_min = {6}`, the other basins at ΔF = 2.2e-5 and
8.7e-3). That failure was not a boundary false-alarm — it was the acceptance
criterion correctly reporting that the reported optimum rests on a single
start. Fix the comment: the reason `accepted` is the wrong design-time key is
that its reproduction limb certifies *sample-fit trustworthiness* (start-
independence of a data fit), not *estimand existence* — a legitimately hard
landscape can have a unique, well-separated global minimum that only one
deterministic start finds. Dropping cells on `reproduced` would conflate the
two. The re-key direction is therefore right; the stated mechanism is not, and
since this comment is quoted as design rationale it should be corrected before
the plan amendment enshrines it.

### Is a converged Heywood-boundary projection a legitimate estimand? Yes — and the probe confirms this one is a true boundary minimizer

Measured at the underfit-interior projection (true m₀ = 3 interior, fitted
m = 2, octants, ζ = .75): ζ*₁ = 1.0 to ten digits, ζ*₆ = 1 − 2.3e-9; the
natural-scale slope at the ceiling is **dF/dζ = −4.2e-3** (central FD, h ∈
{1e-6, 1e-7}) while the reported (logit-scale) gradient norm is 4.0e-7. The
derivation of why both are simultaneously true: dF/du = ζ(1−ζ)·dF/dζ, and at
ζ = 1 − ε the Jacobian factor ζ(1−ζ) ≈ ε annihilates any bounded natural
gradient. So (a) the `converged` criterion is **vacuous at ceiling
coordinates** — it certifies that the logit coordinates stopped moving, which
`plogis` saturation guarantees; but (b) the sign of the *natural* gradient
shows F still strictly decreasing at the bound, i.e. the KKT condition for a
**boundary minimizer of the closed parameter space** holds: the projection's
true value is ζ* = 1 exactly, and the recorded 1.0 is the correct closure
value, not optimizer fuzz (the F-profile probe confirms F is constant to
1e-12 for u beyond saturation). A CI's behavior at a boundary pseudo-truth is
exactly the pathology under study; the §2.4 boundary-status column exists for
precisely this. **So: boundary location alone does not make an estimand
ill-defined — ratified.** Two scoring consequences must be pre-registered if
any ceiling pseudo-truth is ever kept (dormant under the redesign below):
percentile intervals can never cover ζ* = 1 (every replicate is
logit-interior, so uci < 1 — coverage is structurally 0), and
`cpm_analytic_se()` is all-NA at a saturated solution (probe: NA at every N,
the FD Hessian is singular in the dead logit directions) — both must be
reported as interval geometry (the §2.5 zero-width analog), not method
failure.

### Why this particular estimand is nonetheless ill-defined — two measured mechanisms the guard misses

The 11-start basin scan (6 standard starts + ±7.5°/±22.5°/±45° jitters +
ζ-starts 0.5/0.9, all deterministic; every start converged) found:

1. **A cyclic symmetry orbit, not a point.** The population is circulant
   (homogeneous ζ on the equally spaced octant grid: P₀ᵢⱼ depends only on
   (i−j) mod 8), so F is invariant under the C₈ index rotation. The projection
   **breaks the symmetry** — ζ* = (1.0, .42, .50, .50, .42, 1.0, .97, .97),
   θ* grossly off-theory and non-monotone — and the probe shows the "distinct"
   minima at ΔF ≈ 4e-13 are the *same broken pattern rotated one octant*
   (start 7's ζ* is start 6's cyclically shifted). The argmin is an orbit of
   8 (×2 with reflection) parameter points with exactly equal F. A per-item
   truth vector ("ζ*₂ = 0.42") is then **not a functional of P₀** — on data,
   the fitted broken pattern anchors its rotational phase wherever sampling
   noise puts it, the engine canonicalizes reflection but nothing aligns the
   cyclic phase, and per-item coverage of any single orbit representative is
   meaningless (heuristically ~1/8 phase agreement, then noise). This is the
   M4 equally-spaced-grid degeneracy surfacing at design time.
2. **A second, genuinely distinct near-tied orbit at ΔF = 2.171e-5.** In
   deviance units, (N−1)·ΔF ranges from 0.002 (N = 100) to **1.1 (N =
   50000)** across the entire study grid. Basins separated by ~1 deviance
   unit are sample-selection-unstable: the multi-start winner on a given
   replicate is decided by noise, so ζ̂ for the affected items hops between
   values ~0.55 apart across replications *at every N the study runs*, and
   never settles until N ~ 10⁹. Coverage of either basin's truth measures
   basin choice, not interval quality. The `multimodal` flag is silent here
   because its competitive tolerance, 1e-6·max(1, |F|), is a *numerical
   identity* scale — the observed 2.2e-5 tie is 22× outside it while being
   statistically a dead heat at all study N. (For the shipped package flag
   this insensitivity is a *finding* the study will measure under RQ4 — do
   not change the package; but the design-time guard cannot rely on it
   alone.)

This is precisely the plan's own ill-definedness clause — "a cell whose
pseudo-truth is itself ill-defined (near-tied distinct projections) is
redesigned or dropped at design time" — reached here by measurement. The
follow-up's premise "unimodal" was an artifact of the flag's tolerance.

### The redesign (measured, recommended)

Replace `interior` as the underfit generating config with **`trail_t010`**
(β₀ = (.40, .35, .15, .10), fit m = 2) in both `s1_underfit_*` and the
stage-3(d) pair (which becomes {trail_t010, b0_dominant}):

- trail_t010 → m = 2: **one basin across all 11 starts** (including the ±45°
  and ζ 0.5/0.9 extremes), symmetric (homogeneous ζ* = 0.642, θ* = theory),
  interior (min β* = .114), population RMSEA .055 — a clean "genuinely
  misspecified correlation function" estimand at a meaningful misspecification
  size, and it keeps RQ6(b) on the trailing-harmonic axis the rest of the
  paper is built around.
- The already-present `b0_dominant` underfit cells are clean as-is (one basin,
  all 11 starts, homogeneous ζ* = 0.698, min β* = .077) — keep unchanged.
- Cell count is preserved (12 cells re-pinned, none lost); the 7 build-time
  drops (b0_dominant × perturbed × ζ=.5; two perturbed OOF cells) are
  legitimate multimodal drops with recorded reasons — verified by rebuilding
  the table (0.6 min; achieved OOF RMSEA .0494 ≈ .05 target, confirming S1's
  fix works).
- Bonus measured fact worth a paper footnote and a free build-time
  consistency check: the trail_t005 → m2 and b0_dominant → m2 projections
  have **identical F\* to ~5e-15 relative** (0.016988870786987 vs …992) with
  identical homogeneous ζ* = 0.698212 and *different* β* — both truths share
  ζ²β₃ = .5625×.05, and on the harmonic-balanced octant grid the m = 2 family
  absorbs the k ≤ 2 structure, leaving the same orphaned k = 3 energy. I have
  verified the identity empirically, not derived it exactly; recommend
  recording it and (optionally) asserting agreement to 1e-8 in
  `build_config_table()` as a projection self-check, labeled as an observed
  regularity pending derivation.

### The §2.4 amendment (exact wording; pre-registration content)

Replace the guard sentence of §2.4 ("Guard: the projection fit must pass the
acceptance criterion, must not flag multimodality, and its
mirror/canonicalization must be checked; …") with:

> **Guard (amended 2026-07-09, ratified after measurement).** The pseudo-truth
> γ*(P₀) is **well-defined** iff all of:
> (i) the projection fit **converges**: scaled gradient ≤ 1e-6·max(1, |F*|)
> at the reported solution (the engine's `accepted` flag is *not* the key —
> its multi-start reproduction limb certifies sample-fit trustworthiness, not
> estimand existence, and a legitimate boundary projection can fail it);
> (ii) any communality at the ceiling (ζ*ᵢ > 0.999) satisfies the KKT
> condition dF/dζᵢ ≤ 0 (natural-scale central FD at the solution); the
> boundary is then a constrained minimizer of the closed parameter space and
> ζ*ᵢ is recorded as exactly 1 — a saturated coordinate failing the sign
> check is an optimizer artifact, and the cell is dropped;
> (iii) the projection is **statistically unimodal**: over the engine's
> deterministic start set augmented by the pinned extras (alternating
> ±7.5°/±22.5°/±45° angle jitters; ζ-starts 0.5 and 0.9), every converged run
> whose mirror-aligned natural parameters differ from the best by more than
> 1e-3 satisfies (N_max − 1)·ΔF ≥ 10, where N_max is the largest N in the
> cell family — near-tied distinct basins under ~10 deviance units at the
> study's largest N are sample-selection-unstable, so coverage of any single
> basin's truth would measure basin choice, not interval behavior (10 is a
> registered convention: LR-scale separation 10 makes basin mis-selection
> probability negligible, ~exp(−10/2));
> (iv) if P₀ is **circulant** (invariant under the cyclic index shift — test
> the matrix, not the config inputs: out-of-family perturbations break
> circulance), the projection preserves the symmetry (max ζ*ᵢ − min ζ*ᵢ ≤
> 1e-6, θ* at theory ± 1e-4°); a symmetry-broken projection of a circulant
> population is defined only up to the cyclic orbit and has no per-item
> truth.
> A cell failing any of (i)–(iv) is redesigned or dropped at design time with
> the reason recorded, as before. A pseudo-truth at a parameter bound that
> passes (i)–(iv) is a legitimate estimand, recorded in the boundary-status
> column; its pre-registered scoring consequence is that percentile intervals
> cannot cover ζ* = 1 (replicates are logit-interior) and analytic SEs are NA
> at a saturated solution, so ζ coverage at ceiling items is reported as
> interval geometry (structural 0/NA; the §2.5 zero-width analog), not as
> method failure.

And in §3.1/§3.4(d), re-pin the underfit arm: *"underfit: true m₀ = 3, fitted
m = 2, generating config **trail_t010** (β = .40/.35/.15/.10; the interior
config's underfit projection was measured ill-defined — symmetry-broken
cyclic orbit plus a second basin at ΔF = 2.2e-5, i.e. ≤ 1.1 deviance units
across the N grid — and was replaced at design time, reason recorded); the
stage-3(d) underfit pair is {trail_t010, b0_dominant}."*

### New defects surfaced (fix with the redesign)

- **N1 (config.R:148–150):** the guard comment's `accepted`-semantics claim is
  wrong (no Heywood limb); correct it as above so the registered rationale is
  true.
- **N2 (config.R, the guard):** implement amendment items (ii)–(iv) —
  KKT sign check at ceiling coordinates, the (N_max−1)·ΔF ≥ 10 basin scan
  with the pinned extra starts, and the circulance/symmetry check keyed to
  the population matrix. All are design-time-cheap (the full 11-start scan
  costs ~2 s per unique estimand family; there are 7).
- **N3 (config.R misspec builders):** re-pin the underfit cells to
  trail_t010 per the amendment.

## DECIDE 2 — the reconstruction tripwire

**Verdict: adopt the numerical parity fixture; do not add the source hash.**

Rationale. The hash (`deparse(cpm_bootstrap)` pinned) certifies that *source
text* didn't change: it false-alarms on any behavior-preserving package edit,
is silent on drift in the *engine's* copy (the direction that actually
corrupts the study), and `deparse` output is not stable across R versions.
The parity fixture certifies the claim the paper makes — that the percentile
arm *is* the shipped default, draw-for-draw — in both directions, survives
benign refactors, and localizes a failure to the vector that diverged. Its
one blind spot (code paths the fixture doesn't exercise) is closed by pinning
a fixture that provably exercises them and asserting the exclusion counts.

Exact spec (append as the final section of `devel/cpm-sim/selftest.R`, under
a banner noting it fits the CPM — keep the pure-math tests first; it runs in
the standard step-0 protocol, ~2–4 s = 1 cold fit + 2×200 warm refits):

```r
# ---- coupling tripwire: percentile arm == shipped cpm_bootstrap (fits CPM) --
P0 <- make_population_matrix(ANGLE_SETS$p8_equal, rep(0.75, 8),
                             BETA_CONFIGS$trail_t005)
set.seed(BASE_SEED + 777L)          # calibration rule below
X <- simulate_dataset(P0, 60L)
colnames(X) <- scale_labels(8L)

set.seed(424242L)                   # path A: the shipped default
fit <- cpm_fit(data = as.data.frame(X), scales = scale_labels(8L),
               angles = octants(), m = 3, boots = 200)

eng <- cpm_engine(stats::cor(X), angles = octants(), m = 3, variant = "A",
                  reference = 1)    # deterministic; consumes no RNG
set.seed(424242L)                   # path B: the engine's reconstruction
reps <- sim_replicates(eng, X, 200L)
pth <- ci_percentile_theta(reps$theta_rad, reps$ok, 0.95)
pz  <- ci_percentile_linear(reps$zeta, reps$ok, 0.95)
pb  <- ci_percentile_linear(reps$beta, reps$ok, 0.95)

ok(max(abs(pth$lci - fit$results$Angle_lci)) == 0 &&
   max(abs(pth$uci - fit$results$Angle_uci)) == 0 &&
   max(abs(pz$lci  - fit$results$Zeta_lci))  == 0 &&
   max(abs(pz$uci  - fit$results$Zeta_uci))  == 0 &&
   max(abs(pb$lci  - fit$betas$Beta_lci))    == 0 &&
   max(abs(pb$uci  - fit$betas$Beta_uci))    == 0,
   "tripwire: percentile CIs byte-identical to cpm_fit(bootstrap)")
ok(fit$details$boots_used == reps$boots_used &&
   fit$details$boots_degenerate == reps$boots_degenerate &&
   fit$details$boots_nonconvergent == reps$boots_nonconvergent &&
   fit$details$boots_reflected == sum(reps$reflected[reps$ok]),
   "tripwire: exclusion/reflection accounting identical")
if (reps$boots_degenerate == 0)
  cat("NOTE: tripwire fixture exercised no degenerate resamples\n")
if (sum(reps$reflected) == 0)
  cat("NOTE: tripwire fixture exercised no mirror-guard reflections\n")
```

Specification notes, binding:

- **Exact zero, not a tolerance.** Both paths execute the same arithmetic on
  the same doubles in the same session; any nonzero difference *is* drift. A
  future R/BLAS change moves both paths together.
- **Why the two `set.seed(424242L)` calls align:** in path A the engine fit
  inside `cpm_fit()` is deterministic (the package RNG invariant), so the
  first stream consumer is `cpm_bootstrap()`'s single up-front
  `sample.int(N, N*boots)`; in path B it is `sim_replicates()`'s identical
  call — byte-identical index matrices, hence byte-identical replicates.
- **Fixture calibration rule (one-time, at authoring):** N = 60, p = 8,
  trail_t005 population; verify the pinned data seed yields
  `boots_degenerate ≥ 1` **and** `boots_reflected ≥ 1` so the exclusion and
  mirror-guard branches are exercised; if not, increment the data seed
  (`BASE_SEED + 777L + k`) and pin the first k that does (record k in a
  comment). If no k ≤ 10 exercises reflection, keep degeneracy as the hard
  requirement and rely on the printed NOTE. Deterministic thereafter.
- **Failure message** (use in the `ok()` label or a wrapper):
  *"PARITY FAILURE: sim_replicates()/percentile constructors no longer
  reproduce cpm_fit(ci_method='bootstrap') byte-for-byte — the engine's
  'shipped default by reconstruction' claim (README #1, plan §4.1) is broken.
  Re-sync devel/cpm-sim/intervals.R with R/cpm_fit.R::cpm_bootstrap() before
  any run."*
- Re-verified today that `sim_replicates()` remains verbatim against
  `cpm_bootstrap()` after the 2026-07-09 fixes, so the tripwire should pass
  green on first addition; a red first run means the fixture found real
  drift, not a spec problem.

## Probe audit trail (this session; nothing committed, engine untouched)

Scratchpad scripts, all deterministic design-time computation: boundary
forensics + per-start acceptance decomposition + natural-gradient/KKT probe +
population Wald-SE yardstick (`probe-underfit.R`); 11-start basin scan of the
six projection/redesign estimand families (`probe-basins.R`); F*-identity and
orbit-structure verification with rotated-pattern parameter dumps
(`probe-verify.R`); full `build_config_table()` rebuild — 553 kept / 7
dropped reproduced, drop reasons verified, OOF RMSEA .0494 (`probe-table.R`).
`selftest.R` re-run green before probing.
