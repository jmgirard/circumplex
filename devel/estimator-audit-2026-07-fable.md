# Estimator + angular boundary machinery audit — 2026-07-03 (Fable, Brief C)

Independent statistical review of the shipped estimation core, per
`devel/fable-briefs-2026-07.md` Brief C. Scope: `src/parameters.cpp`
(`ssm_parameters_cpp`, `group_parameters`, `mean_scores`, `corr_scores`,
`col_means`, `pairwise_r`), `src/circular.cpp` (`angle_mean`, `angle_dev`,
`angle_median`), `R/ssm_bootstrap.R` (circular quantile methods, replicate
interval assembly, contrast branch alignment), `R/ssm_montecarlo.R`
(asymptotic covariance math, `mvn_draws`), `R/ssm_analysis.R`, plus the
`angle_dist`/`param_diff` helpers in `R/utils.R`.

Method: line-by-line mathematical review plus numerical probes run against
`devtools::load_all()` (probe scripts in the session scratchpad; no package
code or tests were modified). Findings from the 2026-07-02 audit (memory file)
were treated as prior art and are not re-reported; all five of its bugs are
confirmed fixed in the current tree.

**Verdict in one line:** the core SSM math, degenerate-profile taxonomy,
circular CI machinery, and Monte Carlo covariance derivations all hold up;
the real problems found are one reachable crash on the pairwise-deletion
bootstrap path, one unguarded off-design behavior of the fit statistic, and
one boundary-convention violation in `angle_dist()`.

---

## Findings (ranked by severity)

### F1 — MODERATE: pairwise-deletion bootstrap crashes when a resample draws an all-NA scale column

`col_means()` (src/parameters.cpp:73–82) filters each column with
`find_finite()` and then calls `arma::mean()` on the result. If a bootstrap
resample happens to select only NA rows for one scale within one group, the
filtered vector is empty and Armadillo throws
`mean(): object has no elements`, aborting the entire `ssm_analyze()` call
mid-bootstrap. This is the same family as the fixed B3 crash
(`unique(): detected NaN`) — an un-guarded Armadillo precondition reachable
through a supported user option — but on the `listwise = FALSE` mean path
rather than the grouping path.

Concrete failing input (fails deterministically with this seed):

```r
set.seed(123)
df <- data.frame(
  S1 = c(1, 2, NA, NA, NA, NA),   # 4/6 missing
  S2 = rnorm(6), S3 = rnorm(6), S4 = rnorm(6),
  S5 = rnorm(6), S6 = rnorm(6), S7 = rnorm(6), S8 = rnorm(6)
)
ssm_analyze(df, scales = paste0("S", 1:8), boots = 500, listwise = FALSE)
# Error: mean(): object has no elements
```

With ~n·p(miss)^n per-resample odds, any small-n / high-missingness analysis
will hit this with non-trivial probability, and one bad resample kills all
`boots` of them. Note the correlation path is already guarded against the
analogous case (`pairwise_r` returns `NA_REAL` when fewer than 2 complete
pairs, src/parameters.cpp:134); the mean path should degrade the same way —
return NaN/NA for the empty column and let the existing
degenerate-replicate exclusion + warning absorb it.

Suggested fix shape (for the later Opus pass): in `col_means()`, return
`NA_REAL` for a column with zero finite elements instead of calling
`arma::mean()` on an empty vector. Regression test: the input above, expect
no error plus the degenerate-resample warning.

### F2 — MODERATE: with unequally spaced angles the "Model Fit (R²)" statistic is unbounded below — undocumented and unguarded

For equally spaced angles the closed-form estimator is the OLS projection, so
`fit = 1 − SSE/SST ∈ [0, 1]` is guaranteed (verified: minimum fit over 5,000
random octant profiles was ≥ 0). For unequally spaced angles the closed-form
(2/n)Σs·cos / (2/n)Σs·sin estimator is *not* a projection: SSE can exceed SST
without bound, so the quantity printed as "Model Fit" and interpreted as R²
prototypicality can be arbitrarily negative. This is exactly
plausible-but-wrong territory: nothing in the code or docs warns that fit can
leave [0, 1] off the orthogonal design.

Concrete failing input (full API):

```r
set.seed(8)
sc <- matrix(rnorm(80 * 4, mean = rep(c(1.1, .7, .78, .77), each = 80), sd = .05), 80, 4)
df <- data.frame(sc); names(df) <- paste0("S", 1:4)
r <- ssm_analyze(df, scales = paste0("S", 1:4), angles = c(0, 10, 20, 30), boots = 100)
r$results$fit_est   # -107.0  (printed as "Model Fit" = R²)
```

(`ssm_parameters(c(1.131, .6947, .7822, .7659), angles = c(0, 10, 20, 30))`
gives fit ≈ −91.8 without any resampling.) The severity is capped at
moderate because (a) the estimator itself is the documented Gurtman
convention, (b) the print guardrail "model fit is inadequate (R² < .70)"
does fire, and (c) tightly clustered angles are a pathological design. But a
mild spacing irregularity (real instruments have them) can still push fit
quietly below 0 while the output labels it R².

Related doc/guard gap (the brief's "guarded at every entry?" question): the
equal-spacing caveat is documented **only** in `ssm_analyze()`'s `@param
angles`. `ssm_parameters()` and `ssm_score()` accept the same free `angles`
argument with no mention of the Gurtman-vs-OLS distinction, and no entry
point emits a runtime note for unequal spacing. Recommend: mirror the caveat
in both roxygen blocks, document that fit ∈ [0,1] holds only for equal
spacing, and consider flooring/flagging (not silently) fit when angles are
unequally spaced — or emitting a one-time message that the estimator is not
least-squares for this design.

### F3 — LOW/MODERATE: `angle_dist()` returns −180° at exactly ±180°, violating the documented (−180°, 180°] contrast convention

`angle_dist(x, y) = ((x − y + π) %% 2π) − π` (R/utils.R:49–51) has range
**[−π, π)**, not (−π, π]: an exact half-turn maps to −π. This contradicts the
CLAUDE.md invariant ("reported in (−180°, 180°] via `angle_dist()`"), the
DESIGN.md contrast row, and the *assertion contract of an existing test*
(test-ssm_bootstrap.R:148 `expect_true(r$d_est > -180 && r$d_est <= 180)` —
that test's seed happens not to land on the atom, but the contract it asserts
is violable).

Reachable through the API. Exactly sign-flipped group profiles produce a
contrast displacement of exactly −180 (not +180) most of the time — in a
probe, 6 of 8 random profiles gave a float-exact −π difference via
`atan2(−y, −x)` vs `atan2(y, x)`:

```r
set.seed(42)
base <- matrix(rnorm(50 * 8), 50, 8) %*% diag(1:8 / 4)
df <- data.frame(rbind(base, -base)); names(df) <- paste0("S", 1:8)
df$G <- rep(c("a", "b"), each = 50)
res <- ssm_analyze(df, scales = paste0("S", 1:8), grouping = "G",
                   contrast = TRUE, boots = 200)
res$results$d_est[3]   # exactly -180, convention says +180
```

Geometrically ±180° is the same rotation, so no downstream statistic is
wrong (the branch-alignment shift and `sign(lci)==sign(uci)` significance
call both behave correctly at −180); this is a contract violation, not an
estimation error. One-line fix shape: map the −π atom to +π
(`d[d == -pi] <- pi`, or equivalently use `-((y − x + π) %% 2π − π)`).
Whichever way, add the exact-±180 test CLAUDE.md already calls for.

### F4 — LOW: degenerate-replicate warning overstates what is excluded

`ssm_replicate_intervals()` warns that degenerate resamples "were excluded
from the confidence intervals" (R/ssm_bootstrap.R:92–99), and DESIGN.md says
their "exclusion makes CIs conditional on estimability." Actually exclusion
is **per column, not per replicate**: quantiles use `na.rm = TRUE` per
parameter, so a zero-amplitude replicate's NA displacement is dropped but its
finite elevation/x/y/amplitude values still enter those parameters' CIs
(probe: with 10 of 40 replicates displacement-NA, the amplitude CI matched
the all-rows quantile, not the complete-rows quantile).

The *behavior* is statistically defensible — amplitude is perfectly
well-defined for a degenerate replicate, and discarding whole rows would bias
the amplitude CI away from 0 exactly where the near-zero-amplitude guardrail
needs it. Only the displacement CI is conditional on estimability. The fix is
to the warning text and the DESIGN.md sentence, not the code. (If anything,
per-row exclusion would be the bug; do not "fix" toward the warning's
wording.)

### F5 — LOW: `angle_median()` returns 0 for all-NA or empty input; latent initialization hazard

`angle_median` (src/circular.cpp:41–59) strips NAs; when nothing remains the
candidate-scan loop never runs, and `NumericVector candidates(1)` was
default-initialized to {0.0}, so the function returns `angle_mean({0}) = 0`
instead of `NA_real_`:

```r
circumplex:::angle_median(c(NA_real_))  # 0, expect NA
circumplex:::angle_median(numeric(0))   # 0, expect NA
```

The same {0.0} initialization would also be silently absorbed as a spurious
candidate by the tie branch (`fabs(dev_val − minimum) <= 1e-8` with
`minimum = π`), but that path is mathematically unreachable for n < ~3e8
(a candidate drawn from the data always contributes a |π − 0| = π term to its
own deviation, capping dev_val at π − π/n). Severity is low because
`angle_mean`/`angle_dev`/`angle_median` are not exported and nothing in
package code calls them (tests only) — but they ship compiled and are one
`:::` away. Fix shape: return `NA_REAL` when the post-NA vector is empty, and
initialize `candidates` empty (`NumericVector candidates(0)`).

### F6 — NIT: 0-vs-360 canonicalization is inconsistent between the point estimator and the CI quantile method (and DESIGN.md misstates the pole value)

Two small drifts around the G2 decision ("we do not canonicalize"):

- The point estimator *does* emit exactly 360.0 at the pole: for a profile
  peaking at 0°/360°, `atan2` returns ~−3e-17, and `modu(x, 2π)` rounds
  `−3e-17 + 2π` to exactly 2π (a classic fmod-at-the-edge artifact), so
  `Disp == 360` exactly — outside the documented half-open "[0, 360)" range,
  and not the "≈359.9999°" DESIGN.md describes.
- `quantile.circumplex_radian()` (R/ssm_bootstrap.R:176) snaps re-wrapped
  endpoints within 2ε of 2π to **0** — the opposite pole label. So a
  pole-hugging profile can print `d_est = 360.0` with a CI endpoint of `0.0`.
  The snap direction (0, not 360) is also test-pinned
  (test-ssm_bootstrap.R:13 expects the 100th-percentile of 180:360 degrees to
  come back as 0).

Both values name the same direction and every consumer handles the wrap
(StatSsmArc unwraps `d_uci < d_lci`; the tests accept either pole label), so
this is cosmetic. Worth one alignment decision at some point — either snap
both to 360 (LM convention) or neither — plus a DESIGN.md correction
("exactly 360.0", not "≈359.9999").

---

## Verified clean

Each item below was independently re-derived or numerically probed in this
audit (beyond the 2026-07 prior audit's confirmations).

1. **Closed-form estimator ≡ OLS at equal spacing.** Cross-checked against
   `lm(s ~ cos(θ) + sin(θ))`; at unequal spacing they diverge in *all three*
   of elevation/x/y as expected (probe P8), and fit stays in [0, 1] for equal
   spacing over 5,000 random profiles (see F2 for the off-design caveat).
2. **Scale-aware degeneracy tolerance** (`tol = 8·ε·n·max|s|`,
   src/parameters.cpp:41). Robust across score scales: constant profiles at
   0.1, 1e6, and 1e6/3 with n = 7 (non-power-of-two, so the mean is inexact
   and deviations are ~ulp-scale) are all correctly flagged flat — the 8·n
   margin dominates the corrected two-pass variance's cancellation noise,
   which is O(ε·max|s|) per element. A genuinely tiny real amplitude of 1e-9
   on a scale-10 profile is estimated to 7 significant digits with correct
   displacement (never NA'd). Misclassification requires amplitude below
   ~1.4e-14 relative to max|s| (≈ 64 ulps), which is below what the score
   encoding itself can represent meaningfully — the documented
   "float-cancellation scale only" claim holds. All-zero scores (max = 0 →
   tol = 0) and NaN scores (sd = NaN fails `sd > tol`) both fall into the
   flat branch correctly.
3. **Degenerate taxonomy.** Flat → disp NA, fit NA; pure second harmonic
   (real variance, zero first-harmonic amplitude) → disp NA, fit exactly 0.
   NA/NaN propagate to NA outputs with the single R-layer warning; the C++
   stays silent as designed.
4. **Circular displacement CI construction.** Center on circular mean →
   unwrap to [−π, π) → linear quantile → re-wrap is correctly implemented in
   both quantile methods; replicates straddling 0/360 give tight, contiguous
   intervals (mock-replicate check and full-API probe: boot CI [349.3, 8.5]
   vs MC CI [349.5, 8.8] around d_est 359.0). The concentrated-replicates
   validity condition is documented and guard-railed elsewhere (fit ≥ .70,
   amplitude CI excluding 0).
5. **Contrast branch alignment at ±180.** The
   `k = round((d_est − mid)/2π)` endpoint shift keeps the `angle_dist`
   estimate numerically inside its interval, preserves width/contiguity, is
   the identity away from the boundary, and is NA-guarded. Full-API probe at
   a true 180° contrast: est −179.15, CI [−182.0, −176.3] — same branch,
   sane width; `ssm_plot_contrast`'s `sign(lci)==sign(uci)` significance
   call remains correct for shifted endpoints. (Exact-±180 atom aside — F3.)
6. **Monte Carlo covariance math.** Mean path: `cov(X)/n` is the correct CLT
   covariance of the mean vector (the n−1 vs n denominator difference vs the
   bootstrap is O(1/n)). Correlation path: the influence function
   `ψ = z_x·z_y − (r/2)(z_x² + z_y²)` is the correct Hampel IF for Pearson r;
   with n−1 standardization Σψ = 0 *exactly* at the estimate, so
   `crossprod(psi)/n²` is a properly centered empirical acov; the Fisher-z
   delta-method scaling `acov_z = acov_r · dz dzᵀ` with `dz = 1/(1−r²)` is
   correct; drawing jointly across measures within a group preserves the
   cross-measure dependence that measure contrasts need, and group
   independence matches the bootstrap's stratification. `mvn_draws()`'s
   eigendecomposition square root is PSD-safe (ipsatized/singular
   covariances) with negative-eigenvalue clamping. Guards (complete data,
   ≥ 2 per group, |r| ≥ 1−1e-12) are all correct and fail loudly toward the
   bootstrap.
7. **Monte Carlo ↔ bootstrap agreement in expectation.** Beyond the shipped
   jz2017 tests: on strongly skewed (lognormal-transformed) correlation-path
   data at n = 600, all six CI endpoints agree to ~2 decimal places
   (displacement endpoints within 0.3°); at the 0/360 boundary (item 4) the
   engines agree through the wrap. The empirical-IF design (no normality of
   the raw data assumed) is what makes this hold off-normal, as intended.
8. **Contrast convention plumbing.** Second-minus-first is defined once
   (`param_diff`, shared by both engines, vector and matrix forms consistent);
   factor-level order → integer codes → `arma::sort(unique(...))` →
   profile-row order is consistent end-to-end, so "second level" means the
   same group in the estimate, the replicates, and the labels. `angle_mean`'s
   resultant-length NA guard and `angle_dev`'s circular mean deviation are
   correct for [0, 2π) inputs.
9. **Interval assembly.** Name-driven column layout (no positional
   arithmetic), radian classing of displacement columns (contrast column
   classed separately, permitting negatives), fit CIs correctly withheld,
   degrees conversion last. `boots = 1` and all-NA replicate columns degrade
   to NA without error.

Known limitations *not* re-flagged here because they are documented and owned
elsewhere: percentile-CI weakness for amplitude near zero (Brief B /
`ssm_ci_accuracy` owns it), circular-CI meaninglessness for flat profiles
(guardrails own it), quantile type-7 vs bootstrap-percentile interpolation
conventions (O(1/boots)).

---

## Suggested follow-up order (for the Opus test-first pass)

1. F1 (crash; small C++ guard + regression test — also the only src/ change,
   so bundle its boundary test run per CLAUDE.md).
2. F3 (one-line branch fix + the exact-±180 tests CLAUDE.md mandates).
3. F2 (docs at all three entry points; decide message-vs-silence for unequal
   spacing; document fit range).
4. F4/F6 (wording-only: warning text, DESIGN.md pole sentence).
5. F5 (two-line C++ fix, or delete the unused circular helpers outright).

Tier recommendation: Opus for F1–F3 (mechanical but statistically adjacent;
tests define correctness), Sonnet for F4–F6 (doc/wording).
