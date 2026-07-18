# M29: `ssm_ci_accuracy()` occasions extension (done 2026-07-17)

**Goal.** Replace `ssm_ci_accuracy()`'s occasions error guard with a correct,
oracle-validated occasions-aware simulation path (within-person dependence).

**Outcome.** `ssm_ci_accuracy()` now runs on occasions objects (error → runs).
`ssm_analyze()` stores per-group stacked `(n, mean, k·p covariance)`
(`occ_k`-tagged); the diagnostic simulates persons from `MVN(stacked mean, Σ̂_g)`
via `mvn_root` (no CPM — D-017), replaying the object's engine (bootstrap = one
shared person-resample `W` across occasion blocks; MC = `ssm_mc_replicates(occ_k=)`),
reporting per-occasion + paired-contrast accuracy. Flat occasion refused by name;
rank-deficient covariance warns (fit-statistic descriptive); pole wraps; near-zero
flags `Structural`; explicit `structure`/`cpm` and legacy `suff_stats=NULL` refused;
`summary()` carries a joint-certification breadcrumb (R12). Classic path untouched.

**Validation (≥2 oracle types).** AC2 simulation-coverage: reported vs direct-
empirical coverage of the real `ssm_analyze` procedure at the plug-in, 27/27
within a pre-registered 4-SE band (max |Δ| 0.032). AC3 discrimination: invariant
(zeroed-occasions ≡ two-group reference) + closed-form Δe width √(w′Σw/w′Σ₀w) to
<1% + Δd reversal sign (0.69→1.13 across |Δd|=90°). Seeded scripts + committed
`smoke=FALSE` results in `devel/`.

**Decisions.** M29-D2 (R12 breadcrumb; shared-W bootstrap replay). Population
design = [[D-017]] (from RR07). `check()` 0/0/0; review found zero actionable
findings (three lenses). PR #53.
