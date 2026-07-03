# M4 spec: `ssm_ci_accuracy()` — SSM CI-trustworthiness diagnostic (Zimmermann & Wright, 2017)

**Status:** design (Brief B of the 2026-07 Fable window). Not implementation.
**Author:** Fable, 2026-07-03 (fresh session; builds on Brief A's committed
design, `devel/m4-browne-design.md`, whose decisions are taken as given —
in particular the native, R-first backend and the `cpm_fit()`/`cpm_simulate()`
sketch in its §5.4).
**Central decision (§2): DECIDED with Jeff, 2026-07-03 — simulation only
in code; Z&W content as transcribed vignette context (see §13).**

> ⚠️ **Oracle rule (inherited from Brief A §6.1, binding here):** no numeric
> accuracy value from Zimmermann & Wright (2017) — coverage rates, bias
> figures, condition definitions, thresholds from Studies 1–5 or their
> supplement — appears in this document or may be written into code or tests
> from memory. Every such number is marked **TBT** (to be transcribed) and
> must be transcribed from the published paper/supplement at implementation
> time, with table/page recorded, under the same two-session transcription
> protocol as Brief A. The only numeric thresholds this spec fixes itself are
> (a) package conventions already in force (fit ≥ .70, amplitude-CI-excludes-0
> guardrail) and (b) the Bradley (1978) robustness bands, which are cited, not
> invented.

Package conventions that bind everything here: angles are degrees [0, 360) in
the user API with LM = 360, radians internally; contrasts are second minus
first in (−180°, 180°]; percentile bootstrap (default) and Monte Carlo CI
engines share `ssm_replicate_intervals()`; the amplitude CI drives the
"displacement is not interpretable" guardrail printed by
`print.circumplex_ssm()`; statistical correctness outranks everything.

---

## 1. Purpose and estimand

`ssm_analyze()` reports percentile bootstrap (or Monte Carlo) CIs whose
finite-sample coverage is not guaranteed to match the nominal level.
Zimmermann & Wright (2017; Z&W) showed empirically that bootstrap SSM CI
accuracy depends on the sample size and on the population circumplex
structure, which they characterized with Browne-model estimates. The
diagnostic answers, for *this user's data and analysis settings*:

> **If the population looked like your fitted estimates, would the CI
> procedure you just ran cover the true SSM parameters at its nominal rate,
> at your n?**

**Estimand.** For each SSM parameter φ ∈ {e, x, y, a, d} of each profile row
(and contrast row, if present), the empirical coverage probability

    C(φ) = P*[ φ₀ ∈ CI(X*) ] ,

where X* is a dataset of the user's exact size and grouping simulated from a
plug-in population (§3.2) built around the fitted Browne model, CI(·) is the
*same* interval procedure the object used (same engine — bootstrap or Monte
Carlo — same `boots`, same `interval`), and φ₀ is the parameter functional
evaluated on the population itself (§3.3). Coverage is estimated by Monte
Carlo over `reps` independent simulated datasets.

Three deliberate properties of this estimand:

- **It assesses the package's own functional.** φ₀ is the closed-form SSM
  estimator applied to population moments — not a hypothetical OLS target.
  For unequally spaced angles the closed form is not OLS (CLAUDE.md
  invariant); the diagnostic asks whether the CI covers what the estimator
  estimates, which is the only coverage question the CI can be held to.
- **It is a plug-in (parametric-bootstrap-style) calibration check.**
  Coverage is evaluated at the fitted structure, not the unknown truth. This
  is exactly Z&W's design logic (simulate from populations characterized by
  Browne-model fits) and its honesty limits are stated in §9, and in the
  user-facing docs.
- **It assesses the procedure as configured.** `boots` is part of the
  procedure; the diagnostic replays the user's `boots` and `interval`, not
  idealized ones.

The fit parameter (R²) has no CI in the machinery and is excluded from
coverage; it enters only through the guardrail operating characteristics
(§4.3).

---

## 2. The central design decision (surfaced for Jeff — not locked)

The ROADMAP offers two routes; they are genuinely different products.

### Option (a): simulate from the fitted Browne model (empirical coverage at the user's n)

One `cpm_fit()` to characterize the scale intercorrelation structure, then a
simulation loop: draw datasets at the user's n from the plug-in population,
rerun the user's CI procedure on each, tally coverage (§3). Note what the
loop does **not** contain: any refitting of the Browne model. The CPM is fit
once to define the population; the per-replicate work is the SSM CI
procedure, which is closed-form-cheap and already C++-backed.

- **For:** answers the actual question at the user's exact n, instrument,
  angles, group sizes, engine, `boots`, and interval; covers configurations
  Z&W never ran (contrasts, measure-based profiles, Monte Carlo engine,
  unequal spacing, any p); produces the amplitude-near-zero operating
  characteristics (§4) that the ROADMAP explicitly requires and that no
  lookup can provide (they depend on the user's own profile and n); is
  self-validating against Z&W by *reproducing* their conditions (§10) rather
  than trusting a transcription forever.
- **Against:** compute cost (minutes, not seconds — §11); the verdict
  inherits the plug-in and MVN assumptions (§9); a poor CPM fit weakens the
  population's realism (mitigated: surfaced in the verdict, §5).

### Option (b): map the user's estimates onto Z&W Studies 1–5 published results

Compute (n, structure summary from the CPM fit, amplitude level), find the
nearest published Z&W condition, and report their observed accuracy for that
condition (all values TBT from paper + supplement).

- **For:** instant, zero simulation cost; grounded in published, citable
  results; no MVN/plug-in machinery of our own.
- **Against:** their condition grid is coarse and fixed — specific
  instruments, specific n values, specific population structures — so most
  real analyses land *between* or *outside* conditions and the mapping
  quietly becomes extrapolation; it cannot speak to contrasts, measure
  profiles, the Monte Carlo engine, non-octant angle sets, or the user's
  actual `boots`; it cannot produce the amplitude-near-zero guardrail
  characteristics at the user's configuration (the ROADMAP's absorbed-M2
  requirement, §4); it hard-depends on an error-prone transcription of a
  large supplement; and their results were produced by their software stack,
  not ours, so residual implementation differences are unquantifiable.

### Recommendation

**Implement (a) as the diagnostic. Do not implement (b) as code.** Give (b)'s
content a home as *context, not computation*: a transcribed summary table of
Z&W's headline accuracy patterns in the "Evaluating Circumplex Structure"
vignette (already a ROADMAP M4 deliverable), cited properly, so users see the
published landscape alongside their own simulation-based verdict. A
nearest-condition lookup in code would imply a precision the coarse grid
cannot deliver, and every hard requirement in the ROADMAP bullet —
amplitude-near-zero percentile coverage, the contrast-displacement pathology,
a verdict at the user's n — is only reachable through (a).

The runner-up worth naming: **(a) + (b)-lite in code** — same simulation
core, plus an optional printed line "nearest published Z&W condition and
their observed coverage (TBT)" when and only when the user's configuration
falls inside their grid. Cheap to add later without rework; deferring it
costs nothing.

**Decision (Jeff, 2026-07-03): (a)-only.** Everything below specifies (a);
§6 is retained as the requirements record should (b)-lite ever be revisited
(cheap to add later without rework — see the runner-up note above).

---

## 3. What is computed (option a)

### 3.1 Inputs and the sufficient-statistics principle

The diagnostic is fully parametric: it never needs the raw data, only
sufficient statistics of the fitted analysis. Per profile-bearing group g
(and, on the correlation path, measure set):

| Quantity | Mean-based SSM | Correlation-based SSM |
|---|---|---|
| Profile vector (the SSM input) | group scale means μ_g (already in `ssm_object$scores`) | measure–scale correlations r_g (already in `ssm_object$scores`) |
| Dispersion | per-scale SDs s_g | — |
| Structure | within-group scale correlation matrix R_g | within-group joint (scales + measures) correlation matrix J_g |
| Size | n_g | n_g |

`circumplex_ssm` currently stores the profile vectors, `angles`, `interval`,
`boots`, `contrast`, `method`, `score_type` — but **not** n_g, s_g, R_g, or
J_g. §8.3 specifies the companion change (store these at analysis time) and
the `data =` fallback for old objects.

### 3.2 Population construction

**Step 1 — characterize structure with the Browne model.** Pool the
within-group scale correlation matrices, R_w = Σ_g (n_g − 1) R_g / Σ_g
(n_g − 1) (elementwise; groups share one circumplex structure — the standard
assumption; a per-group option is deferred, §12). Fit

    cpm <- cpm_fit(cormat = R_w, n = sum(n_g) - G + 1, m = <default or user>,
                   model = "quasi-circumplex", ci_method = "analytic")

taking P̂ = `cpm$matrices$Phat`. The CPM's CIs are irrelevant here (only P̂
and the fit indices are consumed); the cormat path's analytic-only CI
restriction is therefore harmless. If `cpm$details` reports acceptance
failure, boundary flags, or poor global fit, the verdict is downgraded and
annotated (§5) — a population built from a bad structural fit is the
diagnostic's main realism risk. `structure = "observed"` (§7) bypasses the
CPM and uses R_w directly — a sensitivity switch: if the verdict differs
between `"cpm"` and `"observed"`, structure uncertainty itself is material,
and `summary()` says so when both have been run.

**Step 2 — assemble the plug-in population.**

- *Mean-based:* group g's population is MVN with mean μ_g and covariance
  Σ_g = D_{s_g} P̂ D_{s_g}. Simulation uses `cpm_simulate()` (standardized,
  exactly-PSD draws from P̂ via the factor representation — Brief A §5.4),
  then rescales: X_g = Z D_{s_g} + 1μ_gᵀ.
- *Correlation-based:* group g's population correlation matrix is the joint
  (p + q) × (p + q) matrix J_g^pop with the scale block replaced by P̂, the
  measure–scale block(s) taken from the observed cross-correlations (this
  block *is* the profile), and the measure–measure block observed. Replacing
  the scale block can break PSD; repair by eigenvalue clamping at 0 followed
  by rescaling to unit diagonal (same philosophy as `mvn_draws()`); record
  and report max |ΔJ| introduced by the repair, and warn above 0.01. Simulate
  MVN(0, J_g^pop) directly (§8.2 explains why `cpm_simulate()` cannot do this
  as sketched — flagged gap G2). Guard: any population cross-correlation with
  |r| ≥ 1 − 1e−12 is refused, mirroring `ssm_montecarlo()`'s check.
- *Contrast objects* (`details$contrast`): both rows' populations are built
  as above (group contrast: two groups; measure contrast: one group, two
  measures within the joint matrix — the joint draw automatically preserves
  their dependence, matching the Monte Carlo engine's design).

Missing data are not simulated; the diagnostic assesses the complete-data
procedure (§9).

### 3.3 Truth

φ₀ is the closed-form SSM parameter vector computed from the population
profile vector — μ_g on scales (mean-based) or the *repaired* population
cross-correlation vector (correlation-based; internal consistency demands
truth be computed from the matrix actually simulated from, not the
pre-repair observed block). Contrast truth uses `param_diff()` semantics:
second minus first, displacement via `angle_dist()` in (−180°, 180°].
If the population profile is degenerate (flat, or amplitude exactly 0 as in
the §4 c = 0 condition), d₀ is undefined; displacement coverage is reported
NA for that condition and the guardrail characteristics (§4.3) carry the
inferential weight instead.

### 3.4 Simulation loop

For k = 1 … `reps` (default 1000; §11 for the Monte Carlo error this buys):

1. Simulate each group's data at its exact n_g (§3.2).
2. Run the object's own interval procedure on the simulated data: the same
   engine as `details$method` (bootstrap with stratified resampling and the
   object's `boots`, or `ssm_montecarlo()`), the same `interval`, the same
   `angles`, the same contrast setting. Implementation freedom: the inner
   bootstrap may bypass `boot::boot()` for direct index resampling — the
   requirement is statistical equivalence of the procedure (same resampling
   law, same quantile assembly via the `ssm_replicate_intervals()` logic),
   not byte-identity with any user run.
3. Record, per profile row and parameter:
   - coverage indicator: φ₀ ∈ [lci, uci]. For displacement this is **angular
     membership**: truth is inside the reported circular interval as an arc
     (membership modulo 360°), so a truth at the 0/360 pole is handled
     without special-casing and DESIGN G2's "≈0 ≡ ≈360" holds by
     construction. For the contrast row, membership is tested on the
     branch-aligned interval `ssm_replicate_intervals()` reports, again
     modulo 360°;
   - one-sided miss side (below lci / above uci; for d, the shorter angular
     direction of the miss);
   - CI width (angular width for d, capped at 360°);
   - guardrail events: amplitude lci > 0 (displacement certified); fit_est ≥
     .70; and the **branch pathology counter** — displacement point estimate
     geometrically outside its own reported CI (the ROADMAP's F3-review
     observation, folded in here);
   - degenerate-replicate warnings raised (count only; warnings suppressed
     inside the loop and re-summarized).

### 3.5 Outputs of the loop

Per profile row × parameter × condition (§4 adds conditions):

    coverage, mc_se (binomial), left_miss, right_miss, median_width

plus, per profile row × condition: certification rate (amplitude lci > 0),
fit-pass rate (fit_est ≥ .70), displacement-conditional coverage — coverage
of d **among the replicates where the guardrail certified it** (this is the
estimand users actually live under: d is only interpreted when certified;
unconditional d coverage is reported too, but the conditional number is the
one the verdict uses) — and the branch-pathology frequency.

---

## 4. The amplitude-near-zero module (the absorbed-M2 target)

Percentile intervals are theoretically weakest exactly where the guardrail
operates: amplitude is nonnegative, upward-biased, and skewed near zero, so
the percentile CI's lower tail misbehaves there, and that lower bound is
what certifies displacement interpretability. The user's own â may be far
from zero; the module therefore *manufactures* the near-zero regime around
the user's data rather than waiting for it.

### 4.1 Amplitude ladder

Decompose the observed profile vector into SSM-implied first harmonic plus
residual: with the fitted (ê, â, d̂) and scale angles θ_i,

    resid_i = profile_i − ê − â·cos(θ_i − d̂)   (the higher-harmonic content),

and define condition-c populations by scaling only the first harmonic:

    profile_i(c) = ê + c·â·cos(θ_i − d̂) + resid_i ,   c ∈ amplitude_factors,

default `amplitude_factors = c(1, 0.5, 0.25, 0)`. c = 1 is the as-estimated
condition (§3 unchanged); c = 0 is a population with amplitude exactly 0 but
*realistic residual harmonics* — deliberately the "pure higher harmonic"
territory the estimator already treats as degenerate-with-real-variance, and
deliberately not a flat profile. Keeping `resid` fixed keeps fit realistic
across the ladder instead of silently setting population R² = 1. If â itself
is ~0 (below, say, half its CI width), the ladder degenerates; the module
then also runs an absolute rung at the certification margin — c chosen so
c·â equals the half-width of the observed amplitude CI — and `summary()`
notes that the user's own analysis already sits in the near-zero regime.
Correlation-based profiles: `profile(c)` replaces the cross-correlation
block, then the §3.2 PSD repair and |r| < 1 guard re-run per condition.

Contrast objects get the same ladder applied to the **contrast**: the second
group's (or measure's) population profile is moved toward the first's so the
population contrast amplitude is scaled by c while each row stays a
realistic profile — this targets the ROADMAP's contrast-level pathology
(near-uniform contrast draws; estimate outside a very wide circular CI) in
exactly the regime it occurs.

### 4.2 What is evaluated on the ladder

At every rung: amplitude CI coverage with its one-sided decomposition (the
theory predicts asymmetric misses near 0 — the decomposition is the
diagnostic signature, not just the total), amplitude CI width, displacement
coverage (unconditional and certification-conditional; NA at c = 0), and the
guardrail operating characteristics of §4.3.

### 4.3 Guardrail operating characteristics

The printed guardrail is a decision rule; near zero it has error rates, and
the module measures them:

- **False certification rate (c = 0):** P(amplitude lci > 0 | a₀ = 0). The
  rule is the implicit one-sided test "lci > 0", nominal level
  α/2 = (1 − interval)/2 (0.025 at the default). Percentile intervals for a
  boundary-constrained, upward-biased estimator are expected to be
  anti-conservative here — this rate *is* the headline number the absorbed-M2
  task asked for.
- **Certification rate (c > 0):** the rule's power curve up the ladder.
- **Conditional displacement coverage (c > 0):** coverage of d among
  certified replicates — the guardrail's promise ("when I certify, the d
  interval is trustworthy") tested directly.
- **Branch pathology frequency** per rung (expected to concentrate at small
  c, especially for contrasts).
- **Fit-pass rate** per rung (context only; fit has no CI).

---

## 5. The verdict (`summary()` plain language)

### 5.1 Classification rule

Per profile row and parameter, at the as-estimated condition (c = 1),
compare empirical coverage to the nominal level using **Bradley's (1978)
liberal robustness band** — coverage within [1 − 1.5α, 1 − 0.5α], i.e.
[.925, .975] at 95% — the standard citable criterion, applied to the Wilson
score interval of the empirical coverage (so `reps`-level Monte Carlo error
cannot flip a verdict silently):

- **Adequate** — Wilson interval entirely inside the Bradley band;
- **Borderline** — Wilson interval overlaps the band boundary;
- **Inadequate** — Wilson interval entirely outside (direction reported:
  under- vs over-coverage).

The displacement verdict uses the certification-conditional coverage. The
false-certification rate at c = 0 is classified against its own nominal α/2
with the analogous band [0.5·α/2, 1.5·α/2]. The profile verdict is the worst
classification among {e, a, d(conditional), guardrail}; x and y are reported
but do not drive the verdict (they are intermediate quantities users rarely
interpret with CIs).

### 5.2 Wording sketch (profile-level; numbers illustrative placeholders only)

    CI trustworthiness (simulated at your n and settings; 1000 replications):

      Profile [Female] (n = 234, 95% bootstrap CIs, 2000 resamples):
        Elevation      coverage 94.8%  — adequate
        Amplitude      coverage 91.2%  — INADEQUATE (misses are almost all
                        below the interval: the amplitude CI sits too high
                        when the true amplitude is small)
        Displacement   coverage 96.1% when certified — adequate
        Guardrail      if the true amplitude were zero, displacement would
                        be wrongly certified 8.9% of the time (nominal 2.5%)

      Verdict: CAUTION — amplitude CIs and the interpretability guardrail
      are less reliable than nominal at this sample size. Displacement CIs
      are trustworthy when certified. Consider a larger sample or treat
      near-zero amplitudes as inconclusive rather than absent.

    Structure note: population simulated from a Browne circular model fit
    (RMSEA = 0.061, SRMR = 0.052); structure fits adequately, so the
    simulated population is a reasonable stand-in for yours.

Downgrade annotations, in order of severity: CPM acceptance flag failed
("verdict unreliable: the structural model did not converge cleanly");
CPM fit poor (RMSEA/SRMR thresholds cited from conventional benchmarks at
implementation, with citation — not invented here); PSD repair exceeded
0.01; `structure = "observed"` vs `"cpm"` verdicts disagree (both run).
`print()` shows only the per-profile verdict lines; `summary()` adds the
full coverage tables, the amplitude ladder, and the structure note.

Vignette cross-link (the (b)-content home, §2): the "Evaluating Circumplex
Structure" vignette situates these verdicts against Z&W's published
patterns (all TBT).

---

## 6. Option (b) requirements (only if Jeff wants it in code)

Held to one paragraph deliberately. A transcription module: for each Z&W
study, a fixture table of (design factors, n grid, population structure
descriptors, observed accuracy outcomes) — every cell TBT from the paper and
supplement under the two-session protocol, with table/page provenance in the
fixture file. A matching rule: nearest condition in (analysis type, n,
amplitude regime, structure descriptors), interpolating in n only,
**refusing** (returning "outside the published grid" rather than a number)
whenever the user's configuration is not bracketed by their conditions —
extrapolation is the failure mode that makes (b) dangerous. Output: their
observed accuracy for the matched condition, labeled as Z&W's result under
their conditions, never as a property of the user's data. If (a) also ran,
print both and never average them.

---

## 7. API sketch

    ssm_ci_accuracy(
      ssm_object,                     # circumplex_ssm
      reps = 1000,
      amplitude_factors = c(1, 0.5, 0.25, 0),
      structure = c("cpm", "observed"),
      m = NULL,                       # -> cpm_fit default min(3, floor((p-1)/2))
      cpm = NULL,                     # optional pre-fitted circumplex_cpm to reuse
      data = NULL,                    # fallback for objects predating §8.3 storage
      parallel = "no", ncpus = 1
    ) -> circumplex_ci_accuracy

`circumplex_ci_accuracy` (S3 list, `new_ssm()` house style):

    coverage    data frame: Profile, Parameter, Condition (c), Coverage,
                MC_se, Left_miss, Right_miss, Median_width,
                Coverage_conditional (d rows only)
    guardrail   data frame: Profile, Condition, Cert_rate, Nominal,
                Fit_pass_rate, Branch_pathology_rate
    verdict     data frame: Profile, Parameter, Class (adequate/borderline/
                inadequate), plus one overall row per profile
    cpm         the embedded circumplex_cpm fit (or NULL when
                structure = "observed")
    population  list per profile row: profile vector(s) by condition, P̂ or
                R_w used, PSD-repair magnitude, truth parameters by condition
    details     reps, amplitude_factors, structure, engine assessed
                (method/boots/interval echoed from the ssm object),
                degenerate-replicate counts, elapsed time, call

Methods: `print()` (verdict lines only), `summary()` (§5.2 in full),
`plot()` (coverage vs. nominal across the amplitude ladder, one panel per
parameter, Bradley band shaded; built on plain ggplot2 — this is a
Cartesian diagnostic plot, not a circumplex canvas).

**RNG contract:** `ssm_ci_accuracy()` is stochastic and joins the DESIGN.md
RNG-consuming entry-point list at ship time (alongside the two Brief-A
additions), with the `set.seed()`-immediately-before convention. Seed
guarantee under parallelism: per-replicate L'Ecuyer-CMRG substreams derived
deterministically from the master stream (the `parallel` package's standard
mechanism), so results are byte-identical for a fixed seed **regardless of
`ncpus`** — the same user-facing guarantee `ssm_analyze()`'s bootstrap
gives, achieved by substreams rather than master-process pre-draw (pre-
drawing all `reps` datasets centrally would be memory-hostile).

---

## 8. The A↔B dependency contract

### 8.1 What B consumes from Brief A's sketch (pinned)

From `cpm_fit(cormat =, n =, m =, model = "quasi-circumplex", reference =,
ci_method = "analytic")` — the **cormat path only**; B never hands raw data
to A:

| Field (A §5.4) | B's use |
|---|---|
| `matrices$Phat` | the population scale block (§3.2), incl. the correlation-path augmentation |
| `fit$rmsea`, `fit$srmr`, `fit$chisq`, `fit$df`, `fit$pvalue` | realism annotation in the verdict (§5) |
| `details$acceptance`, boundary flags, multimodality flag | verdict downgrade triggers (§5) |
| `details$m` (as fitted, post harmonic-removal) | reporting |
| `results$Angle`, `results$Zeta`, `betas$Beta` | descriptive structure characterization echoed in `summary()` (the Z&W move: describing the population the verdict is conditioned on) |

From `cpm_simulate(object, n)`: the mean-based simulation path (§3.2), n =
n_g per group per replicate.

### 8.2 Flagged gaps in A's interface (feedback to tighten A — not invented around)

- **G1 — `cpm_simulate()` return contract unspecified.** A §5.4 gives the
  generative formula but not the return: type, dimensions, column order,
  names, or scale. B requires: a plain numeric matrix, n × p, columns in the
  fitted scale order with `colnames` set to the scale names, zero-mean
  unit-variance margins (so `cor(·) → Phat`), one documented consumption of
  the global RNG stream. A should pin this in §5.4.
- **G2 — no augmented-simulation path.** A calls `cpm_simulate()` "the
  dependency contract with Brief B", but as sketched it simulates the p
  scales only. The correlation-based SSM needs joint (scales + measures)
  draws from an augmented matrix (§3.2), which `cpm_simulate(object, n)`
  cannot produce. Proposed resolution, in order of preference: (i) B owns
  the augmentation and simulates from the repaired joint matrix itself, so
  A's contract for this path *reduces to `matrices$Phat`* — no A change
  beyond documenting that reduction; or (ii) A extends the signature
  (`cpm_simulate(object, n, sigma = NULL)`), which B does not need if (i)
  stands. A's §5.4 sentence should be corrected either way: `cpm_simulate()`
  is sufficient for B's *mean-based* path only.
- **G3 — dimnames unspecified on `matrices$R`/`matrices$Phat`** (and row
  order of `results` vs. the `scales` argument). B indexes the scale block
  by name when augmenting; A should state both carry the scale names in
  fitted order.
- **G4 — A §8's Phase-2 trigger mis-anticipates B.** The trigger "the
  Brief-B diagnostic needing ≥ 10⁴ refits" will never fire: B performs
  exactly **one** `cpm_fit()` and zero CPM refits by design (§2). B's hot
  loop is the SSM resampling procedure, already C++-backed. Informational
  correction to A §8, so nobody implements B with per-replicate CPM refits
  believing they were intended.

### 8.3 The gap on B's own side: `circumplex_ssm` sufficient statistics

Not an A gap — a `circumplex_ssm` contract gap this spec creates a
requirement for. The object today stores no group n's, no scale SDs, no
correlation matrices (verified against `R/ssm_analysis.R`'s `details`
list), so `ssm_ci_accuracy(ssm_object)` cannot run from the object alone.
**Companion M4 change (prerequisite task for B's implementation):**
`ssm_analyze()` additionally stores in `details`, at analysis time:

- per-group n (post-listwise),
- mean-based path: per-group scale SDs and within-group scale correlation
  matrix,
- correlation-based path: per-group joint (scales + measures) correlation
  matrix.

All are O(G·(p+q)²) — negligible. Non-breaking (list additions). Objects
predating the change (including all saved objects in the wild) take the
`data =` fallback, which recomputes the same statistics from the re-supplied
data with a consistency check (recomputed profile vectors must match
`ssm_object$scores` within 1e−8, else error — the guard against handing the
diagnostic the wrong dataset). Re-evaluating `ssm_object$call` was
considered and rejected: the data may not exist in the caller's environment,
and silent re-evaluation against a *changed* object is exactly the mismatch
the consistency check exists to prevent.

---

## 9. Stated limitations (user-facing docs, verbatim candidates)

- **Plug-in optimism:** coverage is evaluated at the fitted structure, not
  the unknown truth; it answers "would the procedure work in a population
  like your estimates", not "did your interval cover".
- **Gaussianity:** simulated populations are multivariate normal with the
  fitted correlation structure. Heavy tails or skew in the real data can
  degrade coverage further than the verdict indicates (the bootstrap being
  assessed is nonparametric; its assessment here is parametric — same as
  Z&W's design).
- **Complete data:** the diagnostic assesses the complete-data procedure;
  pairwise-deletion analyses are assessed as if listwise.
- **Shared structure:** groups are assumed to share one circumplex
  structure (pooled R_w); per-group structure is a deferred option (§12).
- **CPM misfit:** when the Browne model fits the scale block poorly, the
  simulated population may misrepresent the data; the verdict is annotated,
  not silently trusted (§5).

---

## 10. Validation strategy for the diagnostic itself

Same oracle discipline as Brief A §6; the diagnostic is itself a piece of
estimation machinery and gets the same treatment.

- **Known-good oracle:** mean-based elevation is a plain mean of means; at
  moderate n under MVN, percentile-bootstrap coverage is textbook-adequate.
  The diagnostic must classify it adequate (Bradley band) in a seeded run.
  A diagnostic that flags healthy elevation CIs is broken.
- **Known-bad direction oracle:** at c = 0, the amplitude lower-tail miss
  rate and false-certification rate must exceed nominal *in the predicted
  direction* (one-sided binomial test at a seeded configuration). No
  magnitude is pinned — magnitudes are configuration-dependent and pinning
  one from expectation would violate the oracle rule; the *direction* is
  theory (nonnegative, upward-biased estimator), and a diagnostic that
  cannot detect the weakness it exists to detect fails its reason to exist.
- **Brute-force cross-check:** one tiny configuration (p = 4, small n,
  reps = 200) reproduced by an independent flat script (no shared helpers
  beyond the package's public API) to the same coverage counts under the
  same seed protocol.
- **Z&W reproduction (the O5 bridge, all TBT):** configure the simulator to
  the transcribed conditions of at least one Z&W study; the diagnostic's
  coverage estimates must agree with the published values within combined
  Monte Carlo error. Runs under `/statistical-validation` as a seeded,
  CI-tagged script, not on every check (cost) — same tier as Brief A's
  coverage oracle, and sharing its harness is an explicit implementation
  suggestion.
- **Boundary suite (CLAUDE.md danger zones):** population displacement at
  the 0°/360° pole (angular membership must make coverage insensitive to
  the 0-vs-360 report, per DESIGN G2); contrast truth near ±180° (branch-
  aligned membership correct on both sides); c = 0 rung (d coverage NA, no
  crash, guardrail rates produced); flat-profile population refused with a
  clear error (nothing to assess); a contrast configured so the branch
  pathology occurs (counter > 0 in a seeded run).
- **Engine parity spot-check:** on one configuration, assessing
  `method = "montecarlo"` vs `"bootstrap"` yields coverage differing by no
  more than combined MC error — operationalizing DESIGN.md's "statistical
  agreement between engines is a validated property" at the diagnostic
  level.

---

## 11. Cost, defaults, phasing

- `reps = 1000` gives binomial SE ≈ 0.7 pp at 95% nominal — small enough
  that the Bradley-liberal band (±2.5 pp) is decidable; `reps = 500` is the
  documented "quick look" floor (SE ≈ 1.0 pp), below which the Wilson
  interval rarely clears the band and verdicts come back borderline.
- Cost model: `reps × boots` SSM evaluations per condition, each a C++
  closed form over n rows, plus simulation. At n ≈ 300, p = 8,
  boots = 2000, reps = 1000: order 2×10⁶ resample-evaluations per condition,
  ~4 conditions plus contrast — minutes in vectorized R driving the existing
  C++ helpers, dominated by resampling overhead if `boot::boot()` is used
  per replicate; the §3.4 freedom to resample directly exists for exactly
  this reason. Parallelism across reps is embarrassing and seeded (§7).
- Phase 2 (gated on profiling, mirroring A §8): port the inner
  simulate-and-quantile loop to C++ only if default settings exceed ~5
  minutes on octant data. The R loop stays as the permanent oracle, same
  byte-agreement discipline as A.

---

## 12. Open decisions (for Jeff)

1. Default `amplitude_factors` — the proposed ladder c ∈ {1, .5, .25, 0} is
   a design choice, not literature; cheap to change until implementation
   pins snapshots.
2. Class/element naming: `circumplex_ci_accuracy` (proposed) and whether
   the embedded CPM fit is exposed (`$cpm`, proposed) or summarized only.
3. Per-group structure option (fit CPM per group instead of pooled R_w):
   defer (proposed) or ship in the first cut.
4. Whether `ssm_ci_accuracy()` should also assess the CPM's own analytic
   CIs when handed a `circumplex_cpm` object (Brief A §5.2 explicitly
   invites this — "ssm_ci_accuracy()'s machinery should cover CPM analytic
   CIs too"). Proposed: yes eventually, but as a *separate method*
   (`ssm_ci_accuracy.circumplex_cpm`) in a later cut; it needs per-replicate
   CPM refits, which changes the cost class entirely (and would revive A's
   §8 trigger — see G4).

## 13. Decided

- **Approach: simulation only in code ((a)-only).** Z&W Studies 1–5 content
  appears as transcribed, cited context in the "Evaluating Circumplex
  Structure" vignette, not as a lookup module; §6 is retained as the
  requirements record if (b)-lite is ever revisited. (Jeff, 2026-07-03 —
  see §2.)
- §8.3's companion sufficient-statistics storage change is a prerequisite
  requirement, recorded here rather than decided elsewhere.

## References

- Bradley, J. V. (1978). Robustness? *British Journal of Mathematical and
  Statistical Psychology, 31*(2), 144–152.
- Browne, M. W. (1992). Circumplex models for correlation matrices.
  *Psychometrika, 57*(4), 469–497.
- Zimmermann, J., & Wright, A. G. C. (2017). Beyond description in
  interpersonal construct validation: Methodological advances in the
  circumplex Structural Summary Approach. *Assessment, 24*(1), 3–23.
  (+ supplemental materials — the sole source for all Study 1–5 values,
  all TBT.)

## Change log

- 2026-07-03 — Central §2 decision made with Jeff same day: simulation only
  ((a)-only); §12 renumbered, §13 updated.
- 2026-07-03 — Initial spec (Brief B, fresh Fable session). Central §2
  decision surfaced for Jeff before locking.
