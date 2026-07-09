# CPM simulation study — run book (moving to the 20-worker Windows box)

Operator checklist for the actual run on the §0.2 machine (single Windows box,
20 workers, 128 GB). The engine auto-selects a PSOCK cluster on Windows
(`make_backend()` keys on `.Platform$OS.type`), so no code change is needed —
this is environment + sequence only. Nothing here has been run yet.

## 0. Prerequisites on the box

- [ ] **R** — install the version you intend to pin for the whole run; record it.
- [ ] **Rtools** (matching that R) — the package has `src/` (RcppArmadillo), and
  every PSOCK worker runs `devtools::load_all()`, which compiles it. No Rtools ⇒
  workers fail to load.
- [ ] **The repo** — clone `circumplex` to a known absolute path. The engine
  drives it via `load_all`, not an installed build.
- [ ] **Package Imports** installed: `rlang`, `ggplot2`, `boot`, `Rcpp`,
  `RcppArmadillo` (base `parallel` is bundled). `lavaan` is only needed by the
  SEM layer, which the study never calls — not required.
- [ ] `devtools` installed.

## 1. Pin BLAS and record provenance (§7.3 — the ONLY cross-platform hazard)

Reference-vs-optimized BLAS moves CPM optima at the 3rd decimal and can flip
which boundary guard fires — so pick ONE BLAS build and keep it for the entire
run.

- [ ] Decide: default Rblas (reproducible, slower) vs an OpenBLAS/MKL build
  (faster, must stay fixed). Either is fine; **do not switch mid-run.**
- [ ] Record it: `Rscript -e 'sessionInfo(); print(La_library())'` — paste into
  the run log. The engine also stamps `sessionInfo()` + `La_library()` into every
  cell's RDS, so provenance is captured per cell automatically.

## 2. Do NOT copy the Mac's cache

- [ ] **Delete / don't transfer `devel/cpm-sim/cache/`.** The cached
  `config-table.rds` carries pseudo-truths and drop decisions computed under the
  Mac's Accelerate BLAS. The box must **rebuild the config table under its own
  BLAS** (step 4) — that rebuild is the pre-registered design-time computation.
- [ ] Likewise the `cell-index-registry.rds` and any stage caches are
  machine-specific; start clean on the box.

## 3. Environment variables

Set these for every invocation (a `.Renviron` or a wrapper script is cleanest):

```bat
set CPM_SIM_PKG=C:\path\to\circumplex          REM absolute repo root
set CPM_SIM_CORES=20
set CPM_SIM_CACHE=D:\cpm-sim-cache             REM big disk; per-fit records are large
set CPM_SIM_GO=1                               REM required — nothing runs without it
```

- `CPM_SIM_CACHE` on a roomy disk: per-fit records are saved by default (plan
  §7.1). To skip them for a summary-only pass, set `CPM_SIM_NO_RECORDS=1` — but
  then the deferred RQ tables (worst-case bound, marker-conditional, retained-β
  variant) can't be recomputed without re-running, so keep records for the real run.
- Cache is **resumable**: each cell writes `cache/<stage>/<id>.rds`; re-launching
  skips finished cells. Keep the cache dir intact across restarts — the
  `cell-index-registry.rds` in it pins seed stability for derived cells.

## 4. Sequence

Run from the repo root. Each step gates the next.

1. **Self-tests** (verifies the package builds/loads on the box + the math):
   ```
   Rscript devel/cpm-sim/selftest.R          REM expect 29/29; tripwire byte-identical
   ```
2. **Build the config table under the box's BLAS** and eyeball it:
   ```
   Rscript -e "options(cpm_sim_pkg=Sys.getenv('CPM_SIM_PKG')); for(m in c('common.R','config.R','intervals.R','kernel.R','summarize.R')) source(file.path('devel/cpm-sim',m)); cfg<-build_config_table(cache_file=file.path(Sys.getenv('CPM_SIM_CACHE'),'config-table.rds')); print(cfg$dropped)"
   ```
   - [ ] Expect **~553 kept / ~7 dropped**. If the box drops materially different
     cells than the Mac (BLAS moved a near-boundary projection across a guard
     threshold), that's a **BLAS-sensitivity finding to record**, not necessarily
     an error — note the diff in the run log before proceeding.
3. **Throughput benchmark** (the real §3.4 stage-0 gate on this box):
   ```
   set CPM_SIM_MODE=benchmark
   Rscript devel/cpm-sim/run.R
   ```
   - [ ] Read `~XXX fits/s`. At ~349 fits/s the full workload is ~3.5–4.5 days;
     20 workers should land well above that. If it's far below ~350, apply the
     §7.2 trim knobs (drop the perturbed-angle stage-2 axis first).
4. **End-to-end smoke** (25 reps, a couple cells, all stages exercised cheaply):
   ```
   set CPM_SIM_SMOKE=1
   set CPM_SIM_CELLS=s1_p8_equal_interior_z75_N500,s2_p8_equal_trail_t005_N500
   set CPM_SIM_MODE=1
   Rscript devel/cpm-sim/run.R
   REM then repeat with CPM_SIM_MODE=2 for a bootstrap cell; clear CPM_SIM_CELLS/SMOKE after
   ```
5. **The run**, stage by stage (stages 2–3 read the prior stage's cache for the
   selection rules, so run them in order — or use `MODE=all`):
   ```
   set CPM_SIM_MODE=1     REM analytic screen (cheap; yields RQ4/RQ5 + analytic RQ1/RQ3)
   Rscript devel/cpm-sim/run.R
   set CPM_SIM_MODE=2     REM bootstrap core + admitted cells + B-sensitivity
   Rscript devel/cpm-sim/run.R
   set CPM_SIM_MODE=3     REM targeted arms (studentized, jackknife-validation, etc.)
   Rscript devel/cpm-sim/run.R
   ```

## 5. During / after

- **Interrupt any time** (Ctrl-C): finished cells are cached; re-launch the same
  `MODE` to resume. Do not delete the cache mid-run.
- **Monitor** `cache/<stage>/` filling up; each cell RDS holds its summary +
  (unless `NO_RECORDS`) the per-fit records.
- **Provenance**: confirm a spot-checked cell RDS carries the expected
  `la_library`/`sessionInfo` before trusting the batch.
- **Compendium (§7.3)**: engine + committed aggregate RDS migrate to the separate
  research-compendium repo at paper time, citing the released package version;
  the package repo keeps only the release-scoped oracles.

## Gotchas

- The Mac's `~349 fits/s` is a fanless-laptop proxy — it will thermal-throttle
  over days, so it is **not** a run machine. Use the Windows box.
- If a stage-2/3 launch finds an empty prior-stage cache, its selection-rule
  admissions come up empty (only the pre-registered core/fixed cells run) — run
  stage 1 (then 2) to completion first.
- `CPM_SIM_GO` is the master switch; without it `run.R` only defines functions.
