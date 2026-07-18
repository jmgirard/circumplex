# RR07: `ssm_ci_accuracy()` occasions plug-in population design (M29)

- **Date:** 2026-07-17
- **Brief:** `cairn/reviews/RB07-occasions-ci-accuracy-population.md`
- **Reviewer:** independent Fable-level statistical review (fresh context;
  materials read per the brief; numerical checks run against the working tree
  with `devtools::load_all()`, scripts in the session scratchpad)

**Headline (Q1): use construction (a) — the observed stacked per-group
covariance — at every `n`.** The small-`n` noise objection dissolves under a
projection argument (derived below and verified numerically): everything the
diagnostic tallies except the fit statistic depends on the `k·p × k·p`
population covariance only through a fixed `3k`-dimensional harmonic
projection, whose estimation error is `O(sqrt(2/(n−1)))` regardless of `k·p`.
Construction (b) is affirmatively broken: at `n = 25`, `ρ = 0.6`, the
reassembled mixed matrix was non-PSD in 98% of replicates, with median repair
magnitude 0.020 — above the diagnostic's own 0.01 population-realism warning
bar — and the repair both perturbs the cross-occasion blocks (the dependence
under test) and destroys the CPM structure of the diagonal blocks it was
supposed to deliver.

---

## Q1. Population structure (central)

**Recommendation: (a), observed stacked covariance, per group, at all `n`.**
Concretely: for group `g`, population mean = the stacked `k·p` vector of
occasion profiles (the object's own estimates), population covariance =
`Σ̂_g = stats::cov()` of the group's stacked person-level score vectors
(complete cases across occasions, which listwise-only guarantees), persons
drawn `MVN(μ_g(c), Σ̂_g)` via `mvn_root()`, full occasions analysis re-run per
replicate. No CPM anywhere in the occasions path. The recommendation does
**not** change with `n`.

### Derivation 1: what the coverage experiment actually depends on

The closed-form SSM estimator's `(e, x, y)` are linear functionals of the
profile (verified numerically: `max|f(v1+v2) − f(v1) − f(v2)| = 5.6e−17`;
`a`, `d` are functions of `(x, y)` only). Write `B` for the `p × 3` matrix of
those per-block functionals and `A` for the block-diagonal `3k × k·p`
stacking of `B'`. For a group of `n` persons with stacked scores
`w_i ∈ R^{k·p}`:

- **Truths** are functions of `μ` per block (plus `Δ`-truths via
  `param_diff`), independent of `Σ`.
- **Estimator distribution:** the stacked mean `m̄ ~ MVN(μ, Σ/n)` exactly
  under an MVN population, and every per-occasion parameter and the paired
  contrast are functions of `A m̄`, whose law depends on `Σ` only through
  `V = A Σ A'` (`3k × 3k`; `6 × 6` for `k = 2`).
- **Bootstrap replay:** a replicate's occasion means are linear in the
  resampled persons' *projected* scores `u_i = A w_i` (multinomial weights
  shared across occasions), so the replicate law is a functional of the
  empirical distribution of `{u_i}` — a `3k`-dimensional object.
- **Monte Carlo replay:** draws `MVN(m̄, S/n)` projected through `A` have
  covariance `A S A'/n`, and `A S A'` **is** the sample covariance of
  `{u_i}` — a `3k`-dim covariance estimated on `n − 1` df, regardless of
  `k·p`.

Only the fit statistic `R²` (full-`p` residual) and the fit-pass descriptive
escape the projection. So the "near-singular 16×16 matrix at n = 25"
objection targets the wrong object: the effective dimension of the plug-in
population, for everything the verdict is keyed to, is `3k = 6`, not
`k·p = 16`.

### Derivation 2: how noisy is the part that matters

For a fixed weight vector `w` (e.g., the paired-elevation contrast), the
plug-in functional `w'Sw` satisfies `Var(w'Sw) = 2(w'Σw)²/(n−1)` under
normality — relative SE `sqrt(2/(n−1)) = 28.9%` at `n = 25` (measured
28.7% over 2000 sims at the `ρ = 0.6` circumplex fixture; relative bias
+0.5%). That is *exactly the same noise scale* the shipped
`structure = "observed"` path already tolerates at that `n` on the
single-circumplex path — `k·p` does not multiply it. Meanwhile the raw
16×16 `S` is indeed badly conditioned (median min/max eigenvalue ratio
0.0016 at `n = 25`) — a vivid number that is irrelevant to the tallied
coverage, per Derivation 1.

Bias, not just variance: the diagnostic's estimand is coverage-at-the-plug-in.
Both replayed procedures are *adaptive* (they re-estimate their spread from
each simulated dataset), so coverage as a function of the population `Σ` is
locally flat and `E[cov(Σ̂)] − cov(Σ) = O(1/n)` by a second-order Taylor
argument — small against the Bradley liberal band's ±2.5pp half-width. The
committed M25 evidence corroborates: at `n = 30` (worse than the brief's 25
in ratio terms for `k = 2`) both engines covered .92–.97 across all paired
cells (`devel/m25-paired-coverage.md`).

### Why (b) fails — measured, not asserted

PSD coupling theorem: a partitioned symmetric matrix
`[[Σ₁₁, Σ₁₂], [Σ₁₂', Σ₂₂]]` with PSD diagonal blocks is PSD **iff**
`Σ₁₂ = Σ₁₁^{1/2} C Σ₂₂^{1/2}` for some contraction `C` (‖C‖₂ ≤ 1). The
observed cross-block satisfies this coupling with the *observed* diagonal
blocks; replacing the diagonals with CPM-smoothed versions breaks it, and at
small `n` the CPM smoothing moves are large (median max element change 0.27
at `n = 25` in my fixture), so the violation is not marginal. Measured (50
replicates, `n = 25`, `p = 8`, `k = 2`, within-block circumplex
`R = 0.4 + 0.4cos(Δθ)`, cross-block `0.6·R`; `cpm_fit(m = 3,
model = "quasi-circumplex")` per diagonal block):

- non-PSD reassembled matrices: **98%**;
- min eigenvalue: median −0.068, worst −0.399;
- `ssm_ci_psd_repair()` delta: median **0.0204**, max 0.123 — the median run
  exceeds the `ssm_ci_psd_warn = 0.01` realism bar, i.e., (b) would emit its
  own "population realism is reduced" warning on most small-`n` runs;
- repair-induced change to the **cross-blocks** (the dependence the extension
  exists to preserve): median 0.019, max 0.123;
- repair-induced change to the **diagonal blocks**: median 0.018, max 0.072 —
  post-repair the diagonals are no longer the CPM matrices either.

So (b) delivers neither of its selling points: after the (near-certain)
repair, the population is neither the observed structure nor a Browne
circumplex — an unvalidated third structure with distortion concentrated
where it does the most harm. There is also no coherent joint Browne model to
retreat to: the cross-occasion blocks are lagged auto-correlations of the
same scales, which Browne's (1992) single-circumplex family does not model;
inventing a "circumplex + cross-occasion structure" family is new
methodological research, not a diagnostic plumbing choice.

### Why (c) (shrinkage) is not needed and is directionally dangerous

Ledoit–Wolf-type shrinkage with any practical base-R target (identity,
diagonal, or block-diagonal) shrinks the off-diagonal cross-blocks toward
zero — i.e., biases the plug-in population toward *independence*, precisely
the dependence-collapse failure the M29 guard exists to prevent, with the
error direction flipping with `sign(cos Δd)` per the RR06 efficiency
analysis. A dependence-preserving structured target would itself be a novel
modeling choice with no validated default. Given Derivations 1–2 show the
relevant functionals are already well-estimated, shrinkage buys nothing,
adds a tuning surface, and risks a new dependency. Reject.

### Consequences for the API surface

- The occasions path should **error informatively when the user explicitly
  supplies `structure = "cpm"` or a `cpm =` object** (there is no coherent
  joint Browne population to honor), and otherwise proceed with the stacked
  observed covariance, recording `details$structure = "observed"` — do not
  silently reinterpret an explicit request (the M18 lesson: refuse, don't
  coerce). With the default call (`structure` unsupplied), run.
- Use **per-group** `Σ̂_g`, not a pooled matrix: the MC engine's asymptotic
  object is per-group (`cov(cs_g)/n_g`, `R/ssm_montecarlo.R:111`), and the
  diagnostic's contract is to replay the engine. The classic path's pooling
  (`Rw`) was a Z&W-fidelity choice tied to the single-circumplex CPM; it has
  no analogous justification here, and the projection argument removes the
  df motivation for pooling.
- Document that assessing an occasions object occasion-by-occasion via
  `scales =` (the guard's old advice) uses the CPM default and can therefore
  give slightly different per-occasion verdicts than the joint occasions run
  — a structure-sensitivity fact, not a bug.

**Storage (per the settled store-at-analysis-time decision):** the minimal
sufficient object per group is `n_g`, the stacked `k·p` mean vector, and the
stacked `k·p × k·p` sample covariance (plus `k`, occasion labels, and the
stacked column names in occasion-block order). Store the covariance directly
— do not decompose into correlation + SDs (that decomposition only existed to
interpose the CPM). Do **not** store the raw wide person matrix: under the
MVN-population design `(n, μ̂, Σ̂)` is sufficient for everything the
diagnostic computes, and the raw matrix is larger and carries participant
data into a returned object for no benefit. Give the occasions `suff_stats` a
shape-distinguishing field (e.g., `occ_k`) so `ssm_suff_stats()` consumers
cannot silently treat it as the `p × p` shape (the pooled-`Rw` loop at
`R/ssm_ci_accuracy.R:319-327` must branch *before* that arithmetic).

---

## Q2. Small-`n` stability and silent rank-deficiency

**Answer: (iii) accept — the plug-in noise is the diagnostic's remit — with a
transparency guard from (ii): warn (never refuse) when any `n_g ≤ k·p`, and
record the population covariance's rank/min-eigenvalue in `details`.**

Reasoning:

- **Rank-deficient populations are legitimately sampleable.** `S` has rank
  `min(n_g − 1, k·p)`; `mvn_root()`'s clamp is not papering over an error
  here — a singular `Σ` defines a proper (degenerate) MVN, and
  `mvn_root(S)` draws from it exactly (`E[x x'] = S` by construction). At
  `n = 10 < k·p = 16` my check gives `rank(S) = 9`; the projected `3k`-dim
  covariance `A S A'` — the only part the tallied parameters see — is
  preserved exactly in the draws, including its cross-occasion blocks. So
  rank deficiency does **not** "collapse the cross-occasion dependence": the
  projected cross-block `B'S₁₂B` rides through untouched.
- **Does it understate variability?** No systematic direction: `S` is an
  unbiased estimate of `Σ`, and the projected quadratic forms are unbiased
  with relative SE `sqrt(2/(n−1))` (Derivation 2, measured). What a
  rank-deficient population *does* distort is the full-`p` residual
  geometry: simulated profiles have zero variance in `S`'s null directions,
  so the **fit statistic's** simulated distribution (and the fit-pass
  descriptive) is evaluated on a degenerate population. That is a
  descriptive column, not a coverage verdict — a documented caveat, not a
  refusal ground.
- **Why not refuse or shrink:** the diagnostic matters most at exactly the
  small `n` a refusal would exclude; the common case (`k = 2`, `p = 8`,
  `n = 25` → `n > k·p`) is full-rank anyway; and shrinkage attenuates the
  dependence under test (Q1). The replayed procedures' own small-`n`
  pathologies (Wishart noise in the MC engine's `Σ̂`, spec §2.2's
  anticonservatism caveat) are the *thing being measured* — regularizing the
  population would partially hide them.

**Concrete rule:** at construction, if `any(n_g <= k * p)`, warn:
"group `<g>`'s stacked population covariance is rank-deficient
(`n = <n_g>` persons, `k·p = <kp>` dimensions); parameter coverage remains
well-defined, but simulated fit statistics describe a degenerate population."
Record `min(eigenvalues)/max(eigenvalues)` and the rank per group in
`details` (parallel to `max_psd_delta`). No new refusal; the existing
engine-level guards (`n_g ≥ 2`, all-replicates-failed) stay the hard stops.

---

## Q3. Amplitude-ladder coherence for occasions

**The ladder remains a valid family of populations, essentially for free.**

(i) *Means scaled, covariance fixed:* the MVN family has freely varying
`(μ, Σ)`; `MVN(μ(c), Σ̂_g)` is a coherent population for every `c`. Holding
`Σ` fixed while scaling the mean's first-harmonic content is not a
distortion of the dependence — it is the *point*: the ladder isolates the
amplitude regime while holding the noise-and-dependence structure constant,
exactly as the shipped mean path holds `D P D` fixed while shifting
profiles. The dependence being tested (the cross-blocks) is untouched at
every rung, so ladder conclusions about the near-zero-amplitude regime are
made *at the estimated dependence*, which is what the diagnostic promises.
The correction vector is per occasion row and block-local
(`ssm_ci_ladder_correction()` operates on one `p`-vector at a time), so the
any-angle-spacing exactness carries over unchanged; verified numerically:
`e` preserved to 4 decimals and `a(c) = c·â` exactly at
`c ∈ {0.5, 0.25, 0}`, `d` unchanged for `c > 0`, `d = NA` at `c = 0`.

(ii) *Contrast truths under the ladder:* with occasion rows scaled jointly by
the same `c` (the existing "applied to every profile row jointly" contract):
`Δe(c) = Δe(1)` (elevations pinned), `Δa(c) = c·Δa(1)` (both amplitudes
scale), `Δd(c) = Δd(1)` for `c > 0` (angles preserved), and at `c = 0` both
`d_j` are undefined so the recomputed `Δd` truth is `NA` — coverage reported
`NA`, which is right. Recomputing the contrast truth per condition via
`param_diff(truths[T2,], truths[T1,])` is exactly correct and — with the
occasion-minor row order within a single group — honors the
second-listed-minus-first convention (D-013). The `c = 0` structural flag
behaves sensibly: it applies per occasion row (`a` truth 0, percentile
interval of positive replicates cannot contain it — theorem), and the code
already excludes contrast rows from `Structural` (`r <= n_prof`), which is
correct here too: `Δa` is a signed difference whose interval can and should
contain 0 at `c = 0`. The margin-rung logic (`a_est < a_half`) extends
row-wise without change.

**One flagged limitation (docs, not code):** joint scaling means the ladder
never visits the *asymmetric* regime — one occasion certified, the other
near zero — which is the spec §2.2 paired-interpretability danger zone. The
joint-certification descriptive (Q4) is the run's only lens on that regime.
State this in the roxygen; do not add per-occasion ladder factors (it would
square the condition grid for a regime the guardrail columns already
describe).

---

## Q4. Contrast row + certification-conditional coverage

**Confirmed on both counts, with one occasions-specific note.**

- The occasions contrast is a within-person paired difference; its amplitude
  parameter `Δa` is a signed difference, not a prototypicality measure, and
  `print.circumplex_ssm()` applies no certification gate to any contrast row
  (verified: the guardrail notes at `R/ssm_oop.R:186-202` are inside
  `!is_contrast_row`). M15-D1's stance therefore transfers verbatim: verdict
  on `Parameter = "d"` unconditional; `Coverage_conditional`/`Cert_rate`
  retained as joint-certification descriptives no display consumes;
  guardrail `Caution` NA'd for the contrast row at `c = 0`.
- Per-occasion profile rows are ordinary profiles (their amplitude *is* a
  prototypicality measure); they get the standard D-007
  certification-conditional displacement treatment, unchanged.
- **Occasions-specific subtlety:** the joint-certification descriptive
  `cert[contrast] = cert[T1] && cert[T2]` is *exactly* the spec §2.2
  paired-interpretability condition ("Δd CI interpretable only when both
  occasions' amplitudes are reliably nonzero") made quantitative — for
  occasions objects this retained column stops being a curiosity and becomes
  the operating characteristic of the documented caveat. It should gate
  nothing (consistency with M15-D1 and with what the package prints), but
  the occasions roxygen/summary text should point at it: docs-plus-pointer,
  not a new gate. Optionally, `summary()` may add one sentence when the
  `c = 1` joint-certification rate is materially below `1 − α` — a
  breadcrumb, not a verdict input.

---

## Q5. The degenerate-dependence invariant oracle (AC3)

**Zeroing the off-diagonal cross-occasion blocks of the population
covariance is valid — and is the correct reading of "independent re-paired
blocks" for this diagnostic — but it is only half of a discriminating
battery. As specified, AC3 (and AC2) cannot detect the central failure mode
M29 exists to prevent. Add a width-based discrimination arm.**

*Validity of the zeroed construction.* Setting `Σ₁₂ = 0` yields
`blockdiag(Σ̂₁₁, Σ̂₂₂)`, automatically PSD (principal blocks of a PSD matrix)
— no repair, no fragility. Because the diagnostic draws **fresh persons from
a specified population each replicate**, independence imposed in the
*population* genuinely changes the paired estimator's sampling distribution
— the M25 trap (within-sample re-pairing, mean-invariant for a mean-based
estimator) structurally cannot arise: there is no drawn sample being
permuted. This is the population-level analogue of M25's fixed
"fresh-persons" arm.

*The non-discrimination theorem the battery must respect.* Both replayed CI
procedures are adaptive: they estimate their spread from each simulated
dataset. A *correct* paired procedure applied to data from an independent
population produces independence-appropriate intervals and covers at
nominal — demonstrated by the committed M25 `base_repaired` /
`reversal_repaired` cells (.938–.956 through the occasions code path on
independent data). By the same token, a *buggy* diagnostic that silently
dropped the cross-blocks from its plug-in population would simulate from an
independent population, replay the (correct, adaptive) paired procedure on
it, and report ≈ nominal coverage — matching both the truth and the
independent baseline. **Coverage is therefore blind to population-dependence
errors; the observable that carries the dependence signal is interval
width/estimator variance** (M25's measured ratios: Var paired/independent =
0.526 at `Δd = 30°`, 1.365 at `135°`, vs theory `1 − ρ cos Δd` = 0.480 /
1.424). A coverage-only AC3 discriminates against bugs that spuriously
*introduce* dependence (miswired stacking, RNG leakage across arms), not
against bugs that *drop* it.

*Recommended AC3 construction (three arms, one harness):*

1. **Arm A (dependent):** occasions diagnostic on a fixture population with
   known `ρ ≠ 0` and a contrast `Δd` chosen off 90° (include the reversal
   side, e.g. 135°, per the M23 lesson: give the oracle a cell that expects
   the reversal).
2. **Arm B (independence):** same `μ` and diagonal blocks, cross-blocks
   zeroed; run through the *occasions* diagnostic.
3. **Arm C (reference):** the existing, already-validated two-group
   diagnostic (`structure = "observed"`, `contrast = TRUE`) on a synthetic
   two-group mean-based object whose group means/SDs/correlations equal the
   two occasion blocks and `n_g = n` each.

   Invariant: **B ≈ C** per Profile × Parameter × Condition on coverage
   *and* `Median_width`, within a pre-registered SE-based tolerance.
   Discrimination: **A vs B contrast-row `Median_width` ratio** tracks
   `sqrt(1 − ρ_proj cos Δd)` (for `Δd`/`Δa`; `ρ_proj` computed from the
   plug-in `Σ`'s projected blocks), and for `Δe` the **closed-form** target
   `sqrt(w'Σw / w'Σ₀w)` with `w` the paired-elevation contrast weights and
   `Σ₀` the zeroed matrix — a deterministic target computable from the
   population before any simulation, adding a genuine third oracle type
   (closed-form) to the battery.

*Honesty notes for the milestone file:* (1) B ≈ C is exact for the
*estimators* (block-diagonal MVN ⇒ the paired contrast of means is
distributed as an independent two-group contrast) but only asymptotic for
the *procedures*: the paired bootstrap shares multinomial weights across
occasions and the paired MC estimates a cross-covariance whose truth is 0
but whose estimate is `O_p(n^{−1/2})`. Run the AC3 cell at a moderate `n`
(≈100 — it tests code correctness, not small-`n` behavior) with an SE-based
band; never expect bit equality. (2) AC3's label "deterministic oracle" is
therefore a misnomer — it is an *invariant* oracle type (which is what the
doctrine requires); the genuinely deterministic ingredient is the
closed-form `Δe` width target above. (3) AC1's structural assertion
(off-diagonal blocks nonzero in the population object) remains worth keeping
— it is the cheap structural guard; the width arm is the behavioral one.

---

## Q6. Degenerate/boundary contract (AC4)

**Contract: refuse the whole run when any occasion's profile is exactly flat
— the existing `:314-317` refusal extended row-wise, with the error naming
the offending occasion (and group). Near-zero-amplitude and pole-straddling
occasions run and report. This conflicts with AC4's current wording, which
should be amended.**

Reasoning:

- **Replay fidelity forbids dropping the occasion.** The diagnostic's
  contract is to replay *the object's own procedure*; simulating a
  `(k−1)`-occasion analysis assesses a different procedure than the one the
  user ran. Drop/flag is incoherent with the diagnostic's definition.
- **Run-and-NA is defensible but corrupts the verdict surface.** A flat
  occasion has `a = 0` *as estimated*, so the `c = 1` condition — "the
  as-estimated condition that the verdict is keyed to" — is itself a
  structural-zero row: its amplitude coverage is a theorem (0), its
  displacement truth is `NA`, and the per-profile worst-of verdict row would
  either need a new "structural at c = 1" escape or would report
  "inadequate (under)" for a population where under-coverage is not a
  measurement. The ladder also cannot move a flat row (its correction vector
  is 0), so every rung would repeat the same degenerate row. The existing
  design resolved exactly this at the `c = 0` rung by *making the guardrail
  carry the inferential weight there* — but that resolution presumes the
  as-estimated row is non-degenerate. Extending run-and-NA semantics to
  `c = 1` would fork the verdict semantics for one pathological input.
- **Consistency with precedent.** The shipped diagnostic refuses the whole
  run when *any* profile row is flat — including multi-group objects where
  the other group would be assessable. Occasions should not be more
  permissive than groups for the same event without reopening that decision.
  A flat mean profile in real data is almost always a degenerate-input
  signal (constant scores), and the refusal costs the user nothing but a
  clear message.
- **What still runs (and must be tested):** a *near-zero*-amplitude occasion
  (including one paired with a pole-straddling partner) is not flat — it
  runs, the margin rung fires, the `Structural` flags and D-007
  certification columns do the honest reporting, and `ssm_ci_d_cover()`'s
  mod-360 arc membership handles the pole per D-003 with no special-casing.
  Flat profiles arising *inside simulated replicates* are already handled by
  the NA-conditional interval machinery and the degenerate-replicate
  accounting; that stays.

**AC4 amendment (flagged explicitly, per the constraints):** AC4 currently
requires a flat occasion to produce a "non-erroring" diagnostic. Amend to:
"a pole-straddling occasion produces a correctly-wrapped, non-erroring
diagnostic; a flat/zero-variance occasion is refused up front with an error
naming the occasion (consistent with the shipped flat-profile refusal); a
near-zero-amplitude occasion runs, flags `Structural` rows, and reports
certification honestly; all tested." Spec §2.3 item 4's "flat/zero-variance
occasion" battery belongs to the *analysis* path (where `ssm_analyze()`
correctly reports NA rows) — the diagnostic's contract for the same input is
refusal, and the two contracts are consistent: analyze permissively, refuse
to *simulate* from a degenerate population.

---

## Beyond the brief

1. **Row↔group mapping sites assume one row per group on the mean path.**
   `run_one()` writes `t0_prof[g, ]` and reads `pc$profiles[g, ]` per group;
   `row_n` maps `g <- r`; the `sds` list is per group. With occasions,
   `n_prof = G·k` (occasion-minor within group, `occ_scores()`
   `out[(g-1)*k + j, ]`), so every such site needs the
   `g = (r−1) %/% k + 1` mapping. The correlation path's `q`-block
   arithmetic (`(g-1)*q + mm`) is the exact template — follow it rather than
   inventing a parallel scheme.
2. **`ssm_suff_stats()`/pooled-`Rw` ordering trap.** `stats_ss` is consumed
   at `:319-327` assuming `p × p` cormats. The occasions branch must
   dispatch on the suff-stats shape *before* that loop, or a future
   refactor will silently pool `k·p` matrices as if they were `p × p`.
   Give occasions suff_stats an explicit `occ_k` field and make the classic
   path's consumers refuse it (positive capability check, the `:250`
   pattern).
3. **Legacy M25-era occasions objects** (created with
   `suff_stats = NULL` before M29's storage lands, dev-line only, never
   released): recommend refusing with "re-run `ssm_analyze()` under this
   package version" rather than building an occasions-aware `data =`
   recomputation path now. The fallback machinery
   (`ssm_compute_suff_stats()`) is single-occasion-shaped; extending it for
   objects that cannot exist in the wild is scope without benefit. (If the
   dev line has external users, the extension is mechanical: recompute
   stacked `(n, μ̂, Σ̂)` from the wide data with listwise deletion and check
   against stored scores.)
4. **AC2's phrasing has the same blind spot as AC3** ("reported coverage
   tracks the true empirical coverage"): by the Q5 adaptivity argument, a
   dependence-dropping population bug passes a coverage-tracking test. AC2
   remains the right primary oracle for what it does test (truths, `n`,
   replay fidelity, boundary wrapping); just ensure the battery's
   dependence-sensitivity lives in the Q5 width arm, and that at least one
   AC2 cell includes the contrast row (for `G ≥ 2` no-contrast occasions
   objects, every reported coverage number is a *marginal* per-row quantity
   that provably does not depend on the cross-blocks at all — per-occasion
   cells cannot detect dependence errors even in principle).
5. **`structure`-argument ergonomics:** detect an explicit `structure`/`cpm`
   supply via `missing()`/`is.null()` so the plain `ssm_ci_accuracy(obj)`
   call runs on occasions objects while explicit CPM requests refuse with
   the Q1 rationale. Record `details$structure = "observed"` so
   `summary()`'s structure note stays truthful.
6. **RNG:** the occasions path draws different stream lengths per replicate
   than the classic path; no cross-path seed-reproducibility should be
   promised (the existing per-job CMRG substream bracket already isolates
   jobs — reuse it unchanged).
7. **Documentation follow-through:** the roxygen Limitations section's "one
   circumplex structure shared by groups" sentence needs an occasions
   sibling: populations are MVN with the *observed* stacked covariance (no
   CPM idealization on this path), fit statistics under rank-deficient
   populations are descriptive only (Q2), and the joint-certification
   pointer (Q4).

---

## Recommendations

| # | Recommendation | Disposition |
|---|---|---|
| R1 | **Adopt construction (a): per-group observed stacked `k·p` covariance + stacked occasion-profile means, drawn via `mvn_root()`, at all `n`; no CPM anywhere in the occasions path.** | **apply** |
| R2 | Error informatively when `structure = "cpm"` or `cpm =` is explicitly supplied for an occasions object; default call proceeds with `details$structure = "observed"`. | **apply** |
| R3 | Store per group: `n_g`, stacked mean, stacked covariance (+ `occ_k`, occasion labels, stacked column names); not the raw person matrix; not a correlation+SD decomposition. Shape-tag so classic-path consumers refuse it. | **apply** |
| R4 | Rank-deficiency guard: warn (never refuse) when any `n_g ≤ k·p`; record per-group rank / min-eigenvalue ratio in `details`; document the fit-statistic caveat. No shrinkage. | **apply** |
| R5 | Keep the amplitude ladder as-is (joint `c`, per-occasion-row corrections, covariance fixed, contrast truths recomputed per condition); document the asymmetric-regime limitation (ladder never visits one-certified/one-flat). | **apply** |
| R6 | Contrast row: unconditional verdict (`Parameter = "d"`), joint-certification retained as descriptive; occasion rows: standard D-007 conditional treatment; occasions docs point the §2.2 caveat at the joint-certification columns. | **apply** |
| R7 | AC3: implement as the three-arm construction — zeroed-cross-blocks occasions run ≡ reference two-group diagnostic (SE-based band, moderate `n`) **plus** the dependent-vs-zeroed contrast `Median_width` discrimination arm with the closed-form `Δe` width target and a reversal-side `Δd` cell. Relabel AC3 "invariant + closed-form", not "deterministic". | **apply** |
| R8 | Amend AC4: flat occasion ⇒ informative refusal naming the occasion (extend `:314-317`); pole-straddling and near-zero occasions run and are tested as non-erroring/correctly wrapped. | **apply** (flags disagreement with AC4 as drafted) |
| R9 | Ensure ≥1 AC2 cell exercises the contrast row; treat per-occasion-only cells as dependence-blind (they are, provably). | **apply** |
| R10 | Fix every row↔group mapping site (`run_one`, `row_n`, `sds`, `build_pop`) with the `(r−1) %/% k + 1` mapping, following the correlation path's block-arithmetic template. | **apply** (implementation guidance) |
| R11 | Refuse legacy `suff_stats = NULL` occasions objects with a re-run message instead of building an occasions `data =` fallback. | **apply** (revisit only if the dev line has external users) |
| R12 | One-sentence `summary()` note when the `c = 1` joint-certification rate is materially below `1 − α` (breadcrumb for the §2.2 caveat; gates nothing). | **consider** |
| R13 | Occasions-aware `data =` recomputation fallback for pre-M29 objects. | **reject** — no such objects can exist outside the dev line; machinery cost without a user (see R11) |
| R14 | Construction (b), CPM-diagonal + observed cross. | **reject** — 98% non-PSD at the target `n`; median repair 0.020 > the 0.01 realism bar; repair perturbs the cross-blocks under test and destroys the CPM diagonals, yielding an unvalidated hybrid |
| R15 | Construction (c), shrinkage/Ledoit–Wolf. | **reject** — attenuates the estimand-relevant cross-dependence toward independence (error direction flips with `sign(cos Δd)`); unnecessary by the projection argument; new tuning surface and possible dependency |
| R16 | Pooling the stacked covariance across groups. | **reject** — the replayed engine is per-group; pooling has no Z&W-fidelity rationale here and the df motivation dissolves under the projection argument |
