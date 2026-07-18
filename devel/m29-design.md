# M29 design note — `ssm_ci_accuracy()` occasions extension

Implementation blueprint for the occasions plug-in population. Decisions and
their statistical justification live in `cairn/DECISIONS.md` D-017 (from the
Fable review RR07, archived at
`cairn/reviews/archive/RR07-occasions-ci-accuracy-population.md`); this note is
the *how*, not the *why*. Written before code (M29 T1).

## 1. The population and the simulation loop

For an occasions object with `k` occasions, `p` scales, `G` groups
(grouping is time-invariant → `G` person-groups), and per-group person count
`n_g`, the diagnostic's plug-in population for group `g` is

```
persons ~ MVN( μ_g , Σ̂_g )        drawn via mvn_root()  (the shared draw root)
```

- `μ_g` = the stacked `k·p` vector of the group's `k` occasion mean profiles
  (occasion-minor within group, matching `occ_scores()` row order
  `out[(g-1)*k + j, ]`).
- `Σ̂_g` = `stats::cov()` of the group's stacked `k·p` person score vectors
  (complete cases across occasions — listwise-only guarantees this). The
  within-person cross-occasion dependence is the off-diagonal `p×p` blocks.

Per replicate: draw `n_g` persons per group → wide `n_g × k·p` matrix → re-run
the full occasions analysis (`occ_scores` → `ssm_by_group` → replay the object's
own interval engine, bootstrap or MC, at the object's `boots`/`interval`) →
tally coverage/width/certification exactly as the classic path does.

**No CPM anywhere on the occasions path.** The classic path's pooled-`Rw` + CPM
smoothing (`R/ssm_ci_accuracy.R:319-355`) is *skipped* for occasions. Only the
fit statistic escapes the `3k`-dim harmonic projection through which everything
else factors (RR07 Derivation 1), so the raw `k·p` covariance's small-`n`
conditioning is irrelevant to the tallied coverage; do not regularize.

## 2. Storage (populated in `ssm_analyze_occasions`, R/ssm_analysis.R)

M25 stored `suff_stats = NULL` for occasions. M29 stores, per group, the minimal
sufficient object. Shape (occasion-block order preserved):

```r
details$suff_stats <- list(
  occ_k       = k,                       # SHAPE TAG — see §4
  occ_labels  = occ_labels,
  scale_names = stems,                   # p display names, one occasion block
  stacked_cols = colnames(bs_input)[seq_len(k * p)],   # k*p, occasion-major
  groups = list(                         # one entry per group, sorted level
    <level> = list(n = n_g, mean = <k*p vector>, cov = <k*p x k*p matrix>)
  )
)
```

Store the covariance directly (not correlation+SD — that decomposition only
existed to interpose CPM). Not the raw person matrix: `(n, μ̂, Σ̂)` is sufficient
and keeps participant data out of the returned object. `bs_input` (the wide
person-rows, listwise-deleted, group as last col) is already assembled at
`R/ssm_analysis.R:748-777`; compute the per-group mean/cov there.

## 3. `ssm_ci_accuracy()` occasions branch (R/ssm_ci_accuracy.R)

- **Remove** the `details$occasions` error guard (`:197-207`).
- **Structure-argument refusal (RR07 R2):** detect an *explicit*
  `structure = "cpm"` or `cpm =` supply (via `missing()` / non-`NULL`) on an
  occasions object and error informatively (refuse-don't-coerce, M18 lesson).
  The default call runs; record `details$structure = "observed"`.
- **Branch before the pooled-`Rw` loop** (`:319-327`): dispatch on the
  `occ_k` shape tag. The occasions branch builds `μ_g`, `Σ̂_g` per group from the
  stored `suff_stats$groups`, skips CPM, and sets `P`/`cpm_obj` unused.
- **Legacy `suff_stats = NULL` occasions objects (RR07 R11):** refuse with
  "re-run `ssm_analyze()` under this package version" — do **not** build an
  occasions `data =` recomputation path (no such objects exist off the dev line).

## 4. Row↔group mapping (RR07 "Beyond the brief" 1; the load-bearing refactor)

The classic mean path assumes one profile row per group: `run_one()` writes
`t0_prof[g, ]` and reads `pc$profiles[g, ]`; `row_n` maps `g <- r`; `sds` is
per group. With occasions `n_prof = G·k` (occasion-minor within group). Every
such site uses the mapping

```r
g <- (r - 1) %/% k + 1        # occasion row r → its group g
```

exactly mirroring the correlation path's `q`-block arithmetic (`(g-1)*q + mm`,
`(r-1) %/% q + 1`). Audit sites: `build_pop`, `run_one` (simulate/estimate
loops), the `sds` list construction, `row_n`, and the population record. Follow
the correlation-path template rather than inventing a parallel scheme; give the
occasions `suff_stats` the `occ_k` tag so a classic-path consumer refuses it
(positive-capability check, the `:250` pattern) instead of silently pooling a
`k·p` matrix as if `p×p`.

## 5. Amplitude ladder, contrast, certification, boundary — all unchanged

- **Ladder (RR07 Q3):** applied per occasion row (joint `c`), covariance held
  fixed — a coherent `MVN(μ(c), Σ̂_g)` family. Contrast truths recompute per
  condition via `param_diff(truths[T2,], truths[T1,])` (second-listed-minus-
  first; occasion-minor order). Docs note the ladder never visits the
  asymmetric one-certified/one-flat regime (the joint-certification descriptive
  is the lens on that).
- **Contrast & certification (RR07 Q4):** the occasions contrast is a paired
  difference — unconditional verdict (`Parameter = "d"`), joint-certification
  `cert[T1] && cert[T2]` retained as a descriptive (it *is* the §2.2
  both-occasions-nonzero caveat quantified; gates nothing). Per-occasion rows
  get standard D-007 conditional treatment. All identical to the measure
  contrast (M15-D1).
- **Boundary (RR07 Q6 / AC4):** a flat occasion → refuse up front naming the
  occasion (extend the flat-profile refusal `:314-317` row-wise, reporting which
  occasion). Pole-straddling → runs, `ssm_ci_d_cover()` mod-360 arc membership
  handles it (D-003). Near-zero-amplitude → runs, `Structural` flags + margin
  rung fire.
- **Rank-deficiency (RR07 Q2 / R4):** if `any(n_g ≤ k·p)`, warn (never refuse)
  naming the group and the fit-statistic caveat; record per-group
  rank / min-eig-ratio in `details`.

## 6. Oracle plan

- **AC2 (T3) simulation-coverage:** `devel/m29-ci-accuracy-occasions-oracle.R` +
  committed `devel/m29-*-results.rds`, seeded, cell-indexed by level
  (M19 lesson), smoke-first. Cells: ≥1 interior (known cross-occasion ρ), a
  boundary cell (pole-straddling or near-zero occasion), ≥1 cell exercising the
  paired-contrast row. Pre-registered acceptance in the script header; testthat
  reads the rds and asserts the bands.
- **AC3 (T4) discrimination:** three arms (RR07 Q5). Arm A dependent
  (ρ≠0, Δd off 90° incl. a 135° reversal cell); arm B independence (same μ +
  diagonal blocks, cross-blocks zeroed) through the *occasions* diagnostic;
  arm C reference (the existing two-group `structure="observed"`,
  `contrast=TRUE` diagnostic on a synthetic 2-group object with the two occasion
  blocks' marginals, `n_g = n`). Invariant: **B ≈ C** on coverage *and*
  `Median_width` (SE-based band, moderate n≈100). Discrimination: **A vs B**
  contrast `Median_width` ratio tracks `sqrt(1 − ρ_proj cos Δd)` (Δa/Δd) and the
  **closed-form** `sqrt(w′Σw / w′Σ₀w)` for Δe — a deterministic pre-simulation
  target = the genuine third oracle type. Honesty: B≈C is exact for the
  estimators, only asymptotic for the procedures (shared multinomial weights;
  estimated cross-cov whose truth is 0 but estimate is `O_p(n^{-1/2})`) — never
  expect bit equality.

## 7. Docs (T5)

NEWS: `ssm_ci_accuracy()` errors → runs on occasions objects (v2.0.0/M7).
Roxygen Limitations gains an occasions sibling: MVN with the *observed* stacked
covariance (no CPM idealization), fit statistics under rank-deficient
populations are descriptive only, the joint-certification pointer, and the
occasion-by-occasion `scales=` structure-sensitivity note. Update the
informative-error test (occasions now runs; explicit-CPM still errors).
