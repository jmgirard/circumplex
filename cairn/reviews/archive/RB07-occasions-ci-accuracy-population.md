# RB07: `ssm_ci_accuracy()` occasions plug-in population design (M29)

- **Date:** 2026-07-17
- **Output required:** write findings to `cairn/reviews/RR07-occasions-ci-accuracy-population.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `circumplex` is a CRAN R package for circumplex data analysis
via the Structural Summary Method (SSM). A profile of `p` circumplex scales
(default `p = 8` octants at 45° spacing) is summarized by a closed-form
estimator into six parameters: elevation `e`, the two harmonic coordinates
`x`/`y`, amplitude `a = sqrt(x² + y²)`, angular displacement `d ∈ [0, 360)`
degrees, and fit `R²`. Angles use degrees `[0, 360)` in the API (the pole is
reported as `360`, not `0`; D-003). Confidence intervals come from a
nonparametric case bootstrap or a Monte Carlo (asymptotic-normal) engine that
share one interval-assembly back end; displacement CIs use circular quantiles.

**The diagnostic under extension.** `ssm_ci_accuracy()`
([R/ssm_ci_accuracy.R](../../R/ssm_ci_accuracy.R)) is a simulation-based
CI-trustworthiness diagnostic (Zimmermann & Wright, 2017). Given a fitted
`ssm_analyze()` object it asks: *would this object's CI procedure cover the true
SSM parameters at the nominal rate if the population looked like the fitted
estimates, at the observed sample size(s)?* Its current mechanism (mean-based
path):

1. Build a **plug-in population** from per-group sufficient statistics: pool the
   within-group scale correlations across groups, fit Browne's (1992) circular
   process model (CPM, via `cpm_fit()`) to that pooled `p × p` correlation to
   get a model-implied circumplex correlation `P`
   ([R/ssm_ci_accuracy.R:319-355](../../R/ssm_ci_accuracy.R)). A
   `structure = "observed"` switch bypasses CPM and uses the raw pooled
   correlation directly.
2. Manufacture an **amplitude ladder** of populations whose closed-form
   amplitude is scaled toward zero (the regime where percentile amplitude
   intervals are theoretically weakest) while holding residual profile content
   and elevation fixed, via a per-row estimator-functional correction
   ([R/ssm_ci_accuracy.R:375-459](../../R/ssm_ci_accuracy.R),
   `ssm_ci_ladder_correction()` at :881-895).
3. For each ladder condition, simulate `reps` datasets at the object's exact
   group sizes, re-estimate, replay the object's own interval procedure (same
   engine, `boots`, `interval`), and tally coverage of the closed-form
   population truths per parameter
   ([R/ssm_ci_accuracy.R:481-613](../../R/ssm_ci_accuracy.R)).
4. Report per-row coverage, a certification guardrail (displacement is
   interpreted only when the amplitude CI's lower bound clears
   `a_lci / (a_uci − a_lci) >= 0.35`; D-007), and a Wilson-vs-Bradley verdict.

**What M29 adds: occasions objects.** M25 (PR #49) added *occasions* analyses:
the same `p` circumplex scales measured at `k ≥ 2` occasions on the same
persons, supplied as wide data (one row per person), one profile row per
occasion, plus an optional paired within-person contrast (second occasion minus
first) when `k = 2` and a single group. The person-row is the resampling unit,
so within-person cross-occasion dependence is preserved: the bootstrap resamples
persons; the Monte Carlo engine **stacks the `k` occasion mean vectors into one
length-`k·p` vector per group and draws jointly** — the within-person
cross-occasion covariance enters through the off-diagonal `p × p` blocks
([R/ssm_montecarlo.R:104-120](../../R/ssm_montecarlo.R), `occ_k` path). See the
binding spec `devel/longitudinal-ssm-spec.md` §2.2.

`ssm_ci_accuracy()` currently **errors informatively** on occasions objects
([R/ssm_ci_accuracy.R:197-207](../../R/ssm_ci_accuracy.R)) because its per-group
plug-in population would flatten the `k·p` columns and **ignore the
cross-occasion dependence** — silently simulating from the wrong population.
M29 removes that guard and builds a correct occasions-aware population.

**Why this needs independent review.** The spec (§2.2, RR06-reviewed) fixes the
*Monte Carlo engine's* asymptotic object (sample covariance of stacked person
vectors ÷ n). But the *diagnostic's* plug-in population is a distinct object: it
draws **raw persons**, forms the wide matrix, and re-runs the full occasions
analysis. The design question — what covariance structure to simulate persons
from — was deliberately left as "its own design" (spec §1.4) and tagged an
`ip-touching` tripwire at plan time. The crux is a genuine statistical tension:

- The existing diagnostic uses **CPM smoothing** precisely because raw
  within-group correlations are noisy at small `n`, and the diagnostic matters
  most at small `n` (the anticonservative regime the spec's `n ≈ 25–50` cell
  targets).
- For occasions, the `k·p` matrix is **not a single circumplex** — only the `k`
  diagonal `p × p` blocks have circumplex structure; the cross-occasion blocks
  are arbitrary. There is no coherent single Browne model to fit across
  occasions.
- So the diagnostic must either (a) use the **observed stacked `k·p`
  covariance** (which at `n ≈ 25` with `k·p = 16` is a near-singular, very noisy
  estimate), or (b) **CPM-smooth each occasion's diagonal block** and keep
  observed cross-blocks — a novel mixed-structure population nobody has
  validated, with reassembly + positive-semidefinite (PSD) repair fragility.

## Materials

Read these (paths relative to repo root):

- **The diagnostic:** `R/ssm_ci_accuracy.R` — whole file (≈1130 lines).
  Key regions: the occasions error guard (`:197-207`); argument validation and
  the `cpm`/`structure` handling (`:217-355`); the degenerate-ladder / margin
  rung logic (`:357-374`); `build_pop()` and the ladder (`:375-459`); the
  per-replicate simulation loop `run_one()` (`:481-613`); the RNG bracket and
  aggregation (`:615-766`); the verdict, PSD repair `ssm_ci_psd_repair()`
  (`:1016-1029`), and `mvn_root`'s sibling comment. Note `ssm_ci_d_cover()`
  (`:904-916`) handles angular interval membership incl. pole straddle and
  branch-shifted contrasts.
- **The occasions MC engine:** `R/ssm_montecarlo.R` — whole file (211 lines).
  The `occ_k` stacked-draw path (`:104-120`), and the shared draw root
  `mvn_root()` / `mvn_draws()` (`:200-210`) — note `mvn_root()` PSD-clamps
  negative eigenvalues to zero (tolerates singular covariance without erroring).
- **The occasions analysis path:** `R/ssm_analysis.R:726-854`
  (`ssm_analyze_occasions()`) — how the wide person-row `bs_input` is built
  (`:743-777`), scored (`occ_scores`), bootstrapped, and what `details` it
  stores (`:836-846` — note `suff_stats = NULL`, deliberately). Also
  `ssm_compute_suff_stats()` (`:994+`) and `ssm_suff_stats()` (`:1090-1139`) for
  how the non-occasions paths store/recompute their plug-in ingredients.
- **The binding spec:** `devel/longitudinal-ssm-spec.md` §2.2 (lines 178-198,
  the stacked-occasions MC construction and the small-`n`/`k·p` caveat), §2.3
  (200-243, the oracle strategy: simulation-coverage primary, the corrected
  conditional efficiency statement, the degenerate-dependence invariant, the
  boundary battery), and §1.4 (what occasions is *not*).
- **The M25 oracle** as a style reference: `devel/m25-paired-coverage.R`,
  `devel/m25-paired-coverage.md`, and the committed `…-results.rds` (seeded,
  cell-indexed, pre-registered acceptance bands read back by a testthat).
- **Relevant lessons:** `cairn/LESSONS.md` — the M18 return-value-semantics /
  consumer-population bug (2026-07-13), the M25 mean-based re-pairing invariance
  trap (2026-07-16), and the M19 seed-by-level rule (2026-07-13).

You may run R with `Rscript -e 'devtools::load_all(); …'` to inspect object
structure, e.g. build an occasions object with `make_occ_data()` from
`tests/testthat/test-ssm_occasions.R:10-25`.

## Questions

1. **Population structure (central).** For the occasions plug-in population from
   which the diagnostic draws raw persons, which construction is statistically
   correct, and what do you recommend across the `n` regime?
   - **(a) Observed stacked covariance:** per group, mean = the stacked `k·p`
     occasion profiles, covariance = the sample covariance of the stacked
     person-level score vectors (PSD-repaired), draw persons MVN, re-run.
   - **(b) CPM-diagonal + observed cross:** CPM-smooth each occasion's `p × p`
     diagonal block, retain observed off-diagonal cross-occasion blocks,
     reassemble the `k·p` matrix, PSD-repair, draw persons.
   - **(c) some other construction** (e.g. shrinkage/Ledoit-Wolf toward a
     structured target, block-CPM with a shrunk cross-block, or a hybrid).
   State which you recommend, why, and whether the recommendation changes with
   `n` (small `n ≈ 25–50` vs large). If (a), address the small-`n` noise
   objection head-on; if (b), specify exactly how the diagonal CPM blocks and
   observed cross-blocks reassemble into a valid (or PSD-repaired) covariance
   and whether the mixed structure biases coverage.

2. **Small-`n` stability and silent rank-deficiency.** At `n ≈ 25` with
   `k·p = 16`, the observed stacked covariance is near-singular. `mvn_root()`
   ([R/ssm_montecarlo.R:200-203](../../R/ssm_montecarlo.R)) PSD-clamps negative
   eigenvalues to zero, so a rank-deficient population draws **without erroring**
   — potentially degenerate persons. Does this silently miscalibrate the
   diagnostic (e.g., understate variability, collapse the cross-occasion
   dependence the whole extension exists to capture)? Should the occasions path
   (i) shrink/regularize the covariance, (ii) warn or refuse below an `n`/`k·p`
   ratio, or (iii) accept the noise because the diagnostic's remit is to assess
   the procedure *at the estimated structure, noise included*? Give a concrete
   rule if you recommend a guard.

3. **Amplitude-ladder coherence for occasions.** The ladder scales each row's
   closed-form amplitude toward zero by subtracting `(1 − c)` times a per-row
   estimator-functional correction from the profile
   ([R/ssm_ci_accuracy.R:375-459](../../R/ssm_ci_accuracy.R),
   `ssm_ci_ladder_correction()` at :881-895), holding the covariance fixed.
   Applied per occasion row with the `k·p` covariance held fixed, is the ladder
   still a valid family of populations? In particular: (i) does scaling occasion
   *means* while holding the cross-occasion *covariance* fixed produce coherent
   populations, or does it distort the dependence the diagnostic is testing?
   (ii) the paired-contrast truth (`Δe, Δa, Δd`) is recomputed per condition from
   the two scaled occasion profiles — is that the right contrast truth under the
   ladder, and does the `c = 0` structural-zero-amplitude flag (per occasion
   row) behave sensibly for the contrast row?

4. **Contrast row + certification-conditional coverage.** For non-occasions
   objects the diagnostic gates each *profile* row's displacement coverage on
   the D-007 certification rule but reports the *contrast* row's displacement
   coverage **unconditionally** (a contrast amplitude is a signed difference, not
   a prototypicality measure; M15-D1, `Parameter = "d"` not `"d_conditional"`).
   The occasions contrast is a within-person paired difference analogous to the
   measure contrast. Confirm the occasions contrast row should follow the same
   unconditional stance, that the per-occasion profile rows get the standard
   certification-conditional displacement treatment, and flag any occasions-specific
   subtlety (e.g. the spec §2.2 paired-interpretability caveat that a Δd CI is
   interpretable only when *both* occasions' amplitudes are reliably nonzero —
   should that gate anything the diagnostic reports, or is it docs-only?).

5. **The degenerate-dependence invariant oracle (AC3).** The spec (§2.3 item 2)
   and M29 AC3 require a second, deterministic oracle: *"independent-re-paired
   occasion blocks reproduce the independent-groups diagnostic within Monte
   Carlo error."* The M25 lesson (2026-07-16) warns that re-pairing persons
   *within a drawn sample* is **mean-invariant** for a mean-based estimator — the
   paired sampling distribution survives, so that construction fails to
   discriminate. For this diagnostic (which draws fresh persons from a specified
   population each rep), what is the correct construction of the independence
   baseline so the invariant genuinely discriminates the dependence handling?
   Confirm whether **zeroing the off-diagonal cross-occasion `p × p` blocks** of
   the population covariance (making the two occasions independent by
   construction, then checking the occasions diagnostic reproduces a
   two-independent-groups CI-accuracy run on the same marginals) is a valid and
   genuinely discriminating check, or specify the construction that is.

6. **Degenerate/boundary contract (AC4).** The non-occasions diagnostic refuses
   *any* flat (zero-variance) profile up front
   ([R/ssm_ci_accuracy.R:314-317](../../R/ssm_ci_accuracy.R)). The spec §2.3
   item 4 lists "one occasion degenerate" and "flat/zero-variance occasion" as
   required boundary cases. When exactly *one* occasion of `k` is flat (or a
   pole-straddling occasion coincides with a near-zero-amplitude partner), what
   is the correct contract: refuse the whole run, drop/flag the degenerate
   occasion, or run and report `NA` for that occasion's undefined parameters
   while still assessing the others and the contrast? Give the contract that is
   both statistically honest and consistent with the existing flat-profile
   refusal and the D-003 pole convention.

## Constraints

Fixed; do not relitigate — flag disagreement explicitly rather than silently
working around a constraint.

- **D-013** (`cairn/DECISIONS.md`): the RR06-reviewed longitudinal spec is the
  binding build contract. The stacked-occasions MC covariance (sample covariance
  of stacked person vectors ÷ n) is the correct asymptotic object for the
  *engine*; occasions are **listwise-only** (estimand grounds); the paired
  contrast is second-listed-minus-first. A build may amend the spec only through
  its own gate — challenges to reviewed holdings are in scope for *this* review
  (that is why it exists) but must be raised explicitly.
- **D-007**: the shipped displacement certification rule is
  `a_lci / (a_uci − a_lci) >= 0.35`, scale-free and print-independent. Not
  reopened here.
- **D-003 / CLAUDE.md invariants**: displacement in degrees `[0, 360)`, pole
  reported as `360`; contrasts reported in `(−180°, 180°]`; contrast CIs may be
  negative and ride the estimate's branch. Boundary behavior (profiles peaking
  at 0°/360°, CIs straddling the pole, contrasts near ±180°, flat profiles) is
  where bugs hide and must be handled.
- **Oracle doctrine** (`skills/shared/validation-doctrine.md`): every numeric
  result validated by **≥2 independent oracle types**. M29's battery is
  simulation-coverage (AC2) + a deterministic invariant (AC3); your Q5 answer
  constrains whether AC3 actually delivers an *independent* second type.
- **Settled at the M29 question gate (not under review):** the stacked
  person-level statistics are **stored in the occasions object at analysis
  time** (parallel to the mean/correlation `suff_stats` paths), not recomputed
  from a `data =` argument. The exact fields stored follow from your Q1/Q2
  answer — advise on what minimal sufficient object to store (e.g. per-group
  stacked mean + `k·p` covariance, vs. the raw wide person matrix), but the
  store-at-analysis-time decision itself is fixed.
- **Minimal dependencies**: base R + rlang/ggplot2/boot/Rcpp/RcppArmadillo only.
  A recommendation requiring a new dependency (e.g. a shrinkage package) must
  say so explicitly and justify it against the no-new-deps default; prefer a
  base-R implementation.

## Output format

In `cairn/reviews/RR07-occasions-ci-accuracy-population.md`: answer each question
by number with your reasoning and evidence (derive, don't assert — show the
statistics). List any additional findings separately under **"Beyond the
brief"**. End with concrete **recommendations**, each marked
**apply / consider / reject-with-reason**, and for Q1 name the single
recommended construction unambiguously so the implementation can proceed from
it.
