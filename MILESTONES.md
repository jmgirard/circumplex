# Active milestone

## M4 — Circumplex fit & structure statistics (flagship; own CRAN slot)

Source: ROADMAP.md Milestone 4. Branch: `m4-fit-statistics`, cut from the
GitHub-complete-but-held v1.3.0 state (master @ c8525a3, 2026-07-06). The
v1.3.0 (M2+M3) CRAN submission remains held on master; this branch must not
assume it shipped.

Design inputs (all committed, reviewed, and revised during the 2026-07 Fable
window — read before implementing the corresponding task):

- `devel/m4-browne-design.md` — Brief A + A-review integrated: the
  `cpm_fit()` / `circumplex_cpm` design (model, identification, discrepancy,
  gradient, optimizer, CIs, fit indices, validation strategy §6, phasing §8).
- `devel/m4-ci-accuracy-spec.md` — Brief B + B-review revision: the
  `ssm_ci_accuracy()` spec (plug-in coverage simulation, amplitude ladder,
  guardrail operating characteristics, verdict rules, A↔B contract §8).
- `devel/fit-drafts-method-review.md` — Brief D: per-test verdicts on the
  `devel/fit_analysis.R`/`fit_oop.R` drafts (Acton & Revelle 2004 provenance,
  bugs, cutoff re-derivation requirement).
- `devel/m5-m6-design-questions.md` §[blocked on M4] — questions M4's harness
  must eventually answer (context only).

Decisions already made (Jeff, 2026-07-03; do not relitigate): native R-first
backend, C++ port gated on profiling with R as permanent oracle; bootstrap
default CIs on the raw-data path, analytic only on the cormat path with an
N-conditional `summary()` caution; `ssm_ci_accuracy()` is simulation-only
(no Z&W lookup module); the diagnostic assesses the shipped display-coupled
guardrail rule as-is (`round(a_lci, 3) > 0`), principled replacement is a
recorded post-M4 follow-up.

Adopted-by-default (proposed in the design docs, reversible until release;
flag to Jeff in the first review): names `cpm_fit()` / `circumplex_cpm` /
`cpm_simulate()` (Brief A §9.1); model variants B–D ship in the first cut
(§9.2); amplitude ladder c ∈ {1, .5, .25, 0} (Brief B §12.1); class
`circumplex_ci_accuracy` with the embedded CPM fit exposed as `$cpm`
(§12.2); per-group CPM structure deferred (§12.3); CPM-analytic-CI
assessment deferred to a later `ssm_ci_accuracy.circumplex_cpm` method
(§12.4).

Cross-cutting guardrails for every task:

- The oracle rule (Brief A §6.1): **no expected numerical value enters a test
  from memory or from `devel/g2xx1.txt`** (un-vetted, banned). Published
  oracle values arrive only by transcription from the cited source, entered
  as blank templates first.
- Angles are the danger zone: every estimation task carries the CLAUDE.md
  boundary suite (0°/360° peaks, straddling CIs, ±180° contrasts, flat
  profiles) adapted per Brief A §6.6.
- RNG contract (DESIGN.md): stochastic-output entry points only consume the
  global stream; multi-start jitter and internals deterministic;
  `.Random.seed`-untouched pins for the default `cpm_fit()` path.
- Dependency policy: zero net-new hard dependencies. OpenMx/lavaan/psych in
  Suggests as test oracles only.
- Model tiers: Opus implements against the specs; Fable reviews
  estimator-critical code (gradient/optimizer/canonicalization, guardrail
  measurement, oracles); Sonnet for mechanical edits/doc runs.
  `/statistical-validation` after every task touching `ssm_*` statistics or
  `src/`.

### Tasks — Browne model (anchor feature)

- [x] **B1. CPM engine core (R).** Model-implied correlation matrix (§1),
  unconstrained parameterization (logit ζ, softmax β, free angles; §3.3), ML
  discrepancy F (§3.1), analytic gradient (§3.4), nlminb optimization with
  deterministic multi-start, scaled-gradient-norm acceptance, β-boundary
  polish at 1e-2, reflection canonicalization toward the theoretical
  configuration (§2.3, §3.5). Variants A–D with the §1.4 df table.
  *Accept:* finite-difference gradient test at ≥25 random feasible points
  (rel err per §6.3); exact-recovery round trip to ~1e-10; §3.2
  scale-invariance identity exact; circulant DFT check; df table pinned at
  p=8, m=3 for all four variants; default path RNG-silent
  (`.Random.seed` untouched); boundary suite green.
- [x] **B2. `cpm_fit()` API + `circumplex_cpm` class.** Input handling (raw
  data / cormat, listwise-only; §4), reference-angle fix, fit indices from
  the discrepancy (χ², RMSEA + CI with λ_U=0 guard, SRMR with pinned
  convention, CFI/TLI, AIC/BIC; §5.3), analytic CIs on the cormat path
  (§5.2), Heywood/multimodality diagnostics (§2.5), print/summary with the
  N-conditional analytic-CI caution. Defaults per §7.
  *Accept:* `circumplex_cpm` object matches the §5.4 sketch (incl. the
  Brief-B contract fields); print/summary snapshot tests; invalid input
  errors via `is_*()` helpers; roxygen with runnable examples;
  `devtools::document()` no-diff after.
- [x] **B3. Bootstrap CIs (raw-data default).** Warm-started replicates with
  the per-replicate mirror guard (§5.2, A-review F10), acceptance keyed on
  scaled gradient norm (F2), circular handling for angle CIs via the
  existing circular-quantile machinery, seed convention documented.
  *Accept:* CI plumbing validated on a fast-settings run; angle CIs
  straddling 0°/360° handled; discarded-replicate accounting surfaced;
  seeded reproducibility test.
- [x] **B4. `cpm_simulate()`.** Simulation from a fitted model per the
  Brief-B contract; resolves A-side gaps G1–G3 (return contract, corr-path
  reduction to `matrices$Phat`, dimnames pinned) in code and records the
  resolution in `devel/m4-browne-design.md` §11.
  *Accept:* return contract documented and pinned by tests; consumed
  successfully by a prototype of the Z1 loop; RNG contract row added.
- [x] **B5. `plot.circumplex_cpm` on the M3 extension.** Estimated item
  angles/communalities on the `ggcircumplex()` canvas. Fold in the deferred
  viz-robustness findings (ROADMAP continuous track): one plottability
  predicate across the degenerate-profile filters, `StatSsmArc` 0-row frame
  fix, documented `geom_ssm_arc()` displacement-range validation.
  *Accept:* vdiffr snapshots for the new plot; existing ssm_plot snapshots
  byte-identical or individually justified; robustness findings closed with
  tests.
- [x] **B6. CPM validation battery.** Brief A §6 in full: published
  CIRCUM/CircE oracle transcription (blank templates → transcribed values),
  OpenMx/lavaan cross-implementation oracles in Suggests, the simulation
  coverage oracle + T-calibration (A-review F1 — the test that separates
  "matches CIRCUM" from "actually covers"), §6.5 convention traps as tests.
  *Accept:* all §6.4 internal oracles automated in the suite; published-
  oracle mismatches triaged per the §6.3 checklist; coverage-oracle results
  recorded in DESIGN.md (they justify the bootstrap-default decision);
  `/statistical-validation` pass.
  *Inputs from B3's validation (2026-07-06), for the coverage oracle to
  quantify properly:* smoke test at N = 300, boots = 200–300, octant truth
  (ζ = .75, β = .45/.35/.15/.05), nominal .95: angle coverage ≈ .84, zeta
  coverage ≈ .80 with misses 15:1 one-sided (interval above truth — ζ̂ biased
  toward the boundary; percentile CIs inherit the bias; if confirmed at scale,
  a BCa follow-up may be warranted). Also observed: resampled likelihood
  surfaces at octant-like truths have genuinely competing near-tied basins
  (warm-started and cold multi-start optima differ with |ΔF| up to ~0.02 on
  ~1/3 of replicates, both directions, all passing the acceptance criterion,
  even on clean N = 1000 data whose sample fit is χ²-consistent) — the
  warm-start same-basin tracking of §5.2 is doing real work there.

### Tasks — SSM CI trustworthiness (Zimmermann & Wright 2017)

- [ ] **Z0. `ssm_analyze()` sufficient-statistics storage** (prerequisite;
  Brief B §8.3). Store per-group n, scale SDs, and correlation matrices in
  `circumplex_ssm$details`, with a `data =` fallback + consistency check for
  old objects.
  *Accept:* new fields populated on both analysis paths; old-object fallback
  tested; no change to any estimate or seeded pin.
- [ ] **Z1. `ssm_ci_accuracy()` core loop.** Spec §3: one `cpm_fit()` on
  pooled within-group correlations defines the population; reps×boots
  replay of the user's own CI procedure at their n; angular-membership
  coverage for displacement; certification-conditional displacement
  coverage; Wilson 95% intervals vs Bradley's liberal band. Fold in the
  deferred MC-engine efficiency findings (psi loop, batched
  `group_parameters()`, name-driven correlation-path indexing) — this task
  hammers that path.
  *Accept:* seeded end-to-end run on octant data within the §11 cost
  envelope; L'Ecuyer-CMRG save/restore of caller RNG state; machinery pins
  (c=0 amplitude-coverage identity) green; parallel path yields identical
  results to serial at fixed seed.
  *Cost input from B6 (2026-07-06/07):* bootstrap refits on data simulated
  from octant-like, near-boundary truths cost ~20–35 ms each (3–5× the
  clean-data ~6.5 ms measured in B3) — Heywood drift, softmax-tail stalls,
  and acceptance restarts. Z1 simulates from the user's own fitted CPM,
  which for real octant instruments (jz2017 included) is often exactly this
  regime, so budget against the slow figure and expect the Phase-2
  RcppArmadillo trigger (design §8) to fire at or before Z1 scale.
- [ ] **Z2. Amplitude-near-zero module + verdict.** Spec §4–§5: the joint
  row-amplitude ladder via the 3×3 estimator-functional solve (B-review
  F3), one-sided amplitude-CI miss decomposition, guardrail
  false-certification measurement of the shipped rule (digits pinned 3,
  implied threshold echoed), contrast branch-pathology frequency on the
  joint ladder (F2), `summary()` plain-language verdict per §5 with the
  §5.2 wording bar (never "significance test").
  *Accept:* §10 validation strategy executed (known-good/known-bad direction
  oracles, off c=0 for amplitude); false-certification caution line present;
  verdict classification tested at band edges; Fable review of the
  guardrail-measurement module and oracles (per the B-revision tier note).

### Tasks — Structure tests (Acton & Revelle 2004)

- [ ] **T1. Base-R principal-axis loadings + shared infrastructure.**
  Replace `psych::fa()` with an internal PAF; fix the ridge-on-wrong-matrix
  bug (ridge to R, not the data; no ridge⇒ML conflation); psych → Suggests
  as a test oracle. Rewrite in package style (base R, `is_*()` validation —
  the drafts predate the tidyverse-ectomy).
  *Accept:* internal loadings match `psych::fa()` oracle within tolerance on
  reference data; ridge applied to the correlation matrix with a test.
- [ ] **T2. Cutoff re-derivation simulation (nv=8).** One simulation under
  A&R's generating model (their Eqs. 11.1–11.3) re-derives all test cutoffs
  at the package's scale-level nv=8 use — fixes the Gap nv-dependence and
  the VT/RT threshold/grid provenance in one run. Committed as a
  reproducible script (devel/ or data-raw/), cutoffs stored as package
  constants with provenance comments.
  *Accept:* script reruns to the stored constants under its seed; A&R's
  published nv-conditions reproduced within simulation error as a sanity
  gate; every threshold in T3–T6 traces to this run or a cited page.
- [ ] **T3. Fisher test of equal axes.** Sound per review — port with
  citation, scoring-keyed cutoffs (raw vs deviation declared), T2 thresholds.
  *Accept:* matches A&R Eq. 6 on transcribed reference values; cutoff
  keyed to declared scoring; roxygen cites A&R.
- [ ] **T4. Gap test of equal spacing.** Fix the wrap-around omission (the
  0°/360° gap must participate) and the fragile `sign·acos` at 180°/h²=0;
  correct the "equal axes" roxygen error.
  *Accept:* regression test where the wrap-around gap is the largest gap
  (pre-fix code gets it wrong); boundary tests at 180° and h²≈0; T2 cutoffs.
- [ ] **T5. Variance test (VT2) + rotation test.** Implement the *effective*
  VT2 variant (not the draft's raw-loading no-op) with the correct 0.58-
  derived threshold from T2; fix the RT `criterion[0]` indexing bug and the
  orientation-dependent 0–45° grid (re-derived grid from T2).
  *Accept:* VT2 computes the A&R-defined quantity (transcribed check
  values); RT regression test pinning rotation-grid/label alignment; both
  keyed to T2 cutoffs.
- [ ] **T6. RANDALL correspondence index + randomization test.** Index is
  correct in the draft; implement the actual randomization inference (the
  draft bootstrapped simulated MVN data with no p-value).
  *Accept:* randomization p-value validated on a case with a known/derivable
  answer; index matches draft on reference data; seed convention documented.
- [ ] **T7. Fit-statistics API.** `ssm_fit()`-style user-facing wrapper(s)
  returning a typed object with print/summary/plot (plots on the M3
  extension), consistent with `circumplex_ssm` conventions.
  *Accept:* one coherent entry point documented with runnable examples;
  print/summary snapshots; pkgdown reference section added.

### Tasks — Ship

- [ ] **W1. Vignette: "Evaluating Circumplex Structure".** Fit statistics,
  CI trustworthiness, when to trust SSM parameters, ipsatization guidance;
  Z&W Studies 1–5 transcribed as cited context (re-confirm the grid
  characterization at transcription time and log it — spec §2/F8).
  *Accept:* builds clean; exported API only; statistical-precision bar
  (CLAUDE.md); Z&W numerics transcribed, never from memory. Also the natural
  point to confirm/veto the B6 analytic-CI caution wording (see the
  pre-release open-items list below).
- [ ] **W2. Ship-time documentation.** DESIGN.md RNG entry-point list gains
  `cpm_fit(ci_method="bootstrap")`, `cpm_simulate()`, `ssm_ci_accuracy()`
  rows; update `ssm_analyze()`'s "only function that consumes R's RNG"
  roxygen (false once these ship); NEWS.md flagship entry; record the
  guardrail-replacement follow-up (B §12.5) and the F6 0-vs-360 pole-snap
  alignment decision (still parked) in ROADMAP's continuous track.
  *Accept:* document() no-diff; DESIGN.md consistent; follow-ups recorded
  where ROADMAP says they live.

Open items to resolve before the M4 release (do not block later M4 tasks):

- **B6 published-oracle re-read.** The CircE (Grassi et al. 2010) fixture
  values in `tests/testthat/helper-cpm-oracles.R` were transcribed via two
  automated channels (visual + pdftotext) but still need the §6.1 protocol's
  *second independent human re-read* against the paper before release. Only a
  transcription typo is at risk (the cross-implementation and simulation
  oracles are transcription-independent and already agree). Fold into the
  pre-release `/code-review high`.
- **B6 analytic-CI caution — Jeff to confirm/veto** (reversible until
  release, per the adopted-by-default policy above). The marker set for the
  N∈[2000,50000) boundary caution (`cpm_boundary_markers()` in R/cpm_fit.R:
  Heywood / removed harmonic / min β̂ < .10 / condition > 1e8 / multimodal)
  and its `summary()` wording are an adopted default; confirm or adjust when
  W1 documents CI trustworthiness (natural review point).

Release: after all tasks, `/code-review max` minimum (flagship —
`/code-review ultra` only if Jeff asks), then `/release-checklist` for the
M4 CRAN slot. The held v1.3.0 must ship first or be folded in — Jeff's call
at release time, not this branch's.

## Log

- 2026-07-07 — B6 CPM validation battery (Fable, plan-first; /statistical-
  validation; 3-finder×8-angle /code-review high, 6 confirmed findings fixed).
  **Published oracles:** the full text of Grassi, Luccio & Di Blas (2010, BRM
  42, 55–73 — oracle O2, which reanalyzes Browne 1992's own vocational data
  and states its m=1..3 results coincide with CIRCUM, transitively covering
  O1) was obtained and transcribed via two diffed channels (visual page read
  + pdftotext layer; second independent human re-read pending: Jeff) into
  tests/testthat/helper-cpm-oracles.R fixtures. Triage finding (design-doc
  §11 entry): CIRCUM/CircE fit Browne's *free-scaling covariance* structure
  (their published variance ratios .963–1.042 prove it), so the §3.2
  "σ̂ = 1 when fitted to R" claim is false at finite N and published
  comparisons carry a documented model difference (same df, nested families,
  F_ours ≥ F_pub asserted both ways); our discrepancy function reproduces
  their published F̂ = 0.089815 at their reconstructed Σ̂ to ~4e-7. Their m=2
  double-Heywood and m=3 β₃→0 boundary rows reproduce in our engine (flags +
  polish). Convention traps decoded and pinned: CircE ζ-CIs are ln-v-Wald
  back-transforms (shape-different from ours by design), SRMR is
  diagonal-inclusive (ours ×√(6/8) matches), published F₀ is truncated not
  rounded, CIRCUM-compat free-scaling mode recorded in ROADMAP continuous
  track. **Cross-implementation oracles** (OpenMx + lavaan added to Suggests,
  test-only): OpenMx on our diag-constrained model matches the engine to
  dF ≈ 3e-14 / ≤5e-3° / ≤1e-5 (found+neutralized: OpenMx cov-path applies an
  internal (N−1)/N rescale that *shifts* the diag-constrained optimum — feed
  R·N/(N−1)); OpenMx on the free-scaling model reproduces published CircE to
  its printed precision (closing the attribution loop); lavaan constrained
  3-factor m=1 lands on our optimum to ~4e-7 in F̂. **Simulation oracles:**
  sampling consistency + suite-level T-calibration (KS, N=2000,
  well-identified truth) green; the full coverage oracle
  (devel/m4-coverage-oracle.R, seeded scheduling-independent, 500 reps ×
  {250,500,1000} × {boundary,interior β} × boots=1000, ~3.2 h, + an
  analytic-only ladder to N=50000) is committed with results
  (devel/m4-coverage-oracle-*.rds) and recorded in DESIGN.md. Headline:
  bootstrap-default AFFIRMED (dominates analytic for angles/ζ) but the
  [.90,.98] acceptance band FAILS — ζ under-covers at N ≤ 500 (one-sided,
  boundary bias) and β at near-boundary truths under-covers ~.77 flat in N
  (structural percentile failure; BCa follow-up already in ROADMAP);
  analytic CIs recover at N=2000 for interior truths but only at N≈50000
  for near-boundary ones; T = n·F̂ is NOT χ²_df at octant-like truths at
  field N (KS rejects 5/6 cells; Heywood rates .21–.91 — W1 vignette
  material); Heywood solutions are the NORM at field N for octant truths.
  **Calibration shipped** (R/cpm_fit.R, R/cpm_oop.R): summary()'s analytic
  caution is now unconditional below N=2000 and marker-conditional to
  N=50000 via new cpm_boundary_markers() (Heywood / removed harmonic /
  min β̂ < .10 / condition > 1e8 / multimodal), named in the caution text.
  **Review fixes** (6 confirmed): Inf (singular) Hessian excluded by
  is.finite() in both the new marker AND the pre-existing B1 engine warning
  (fixed together via shared cpm_hessian_condition_warn constant, flagged
  here as a pre-existing defect fixed in place); NA-β crash surface in
  summary(); knife-edge fixture (truth β₃ exactly at the .10 marker,
  fitted margin 1e-9 — cpm_clean_truth trailing β now .15);
  expect_no_match needed testthat 3.1.8 vs declared 3.0.0 (replaced;
  pre-existing stale floor recorded in ROADMAP); dangling "see the
  diagnostics above" (markers now printed inline); stale summary() roxygen.
  Coverage-script hardening quantified before adoption: whole-worker try()
  + error accounting (recorded run had zero errors, provably unaffected);
  est-anchored circular membership replaced by the anchor-free span rule and
  the most-affected cell re-run — every number reproduced to all printed
  decimals. Suite 984/984; check 0/0/0; document() no-diff beyond
  man/summary.circumplex_cpm.Rd. NEWS updated. Z1 cost note added above.
  (DESCRIPTION, R/cpm_fit.R, R/cpm_oop.R, DESIGN.md, NEWS.md, ROADMAP.md,
  devel/m4-browne-design.md, devel/m4-coverage-oracle.R [new],
  devel/m4-coverage-oracle-results.rds [new],
  devel/m4-coverage-oracle-analytic.rds [new],
  tests/testthat/test-cpm_oracles.R [new],
  tests/testthat/helper-cpm-oracles.R [new], tests/testthat/test-cpm_api.R,
  man/summary.circumplex_cpm.Rd, MILESTONES.md.)
- 2026-07-06 — B5 `plot.circumplex_cpm` + viz-robustness findings (Opus,
  test-first; inline /code-review high, 1 low-severity finding recorded to
  ROADMAP not fixed). New exported `plot()` method (R/cpm_oop.R): draws each
  scale on the `ggcircumplex()` canvas at its estimated angle and a radius =
  communality (ζ²), canvas spokes at the theoretical angles, and a joint
  angle×communality CI wedge where estimable; communality CI = squared
  [0,1]-clamped ζ bounds (monotone map); a single fill aesthetic + `limits =
  levels` so the reference scale's zero-width wedge (which drops from the arc
  layer's computed data) keeps its Set2 colour rather than training last as
  grey; scales with an inestimable/full-circle interval draw as a point only
  and are named in a warning. Folded in the three ROADMAP continuous-track
  viz-robustness findings (R/geom_ssm.R): one plottability predicate pair
  (`ssm_has_location`/`ssm_has_region`) now shared by `GeomSsmPoint`,
  `StatSsmArc`, `ssm_plot_circle()`, and the new plot; `StatSsmArc`'s all-rows-
  dropped case routes the empty frame through the parent (rep_len(0, nr) not a
  scalar) so it returns the parent's structure, not the raw input columns; and
  a shared `ssm_arc_span()` documents+validates the displacement input range
  (CCW min→max, min>max = seam-crossing, span must be <360° or the stat errors)
  — geometry byte-identical for valid input (end = ggrad(min+span) = ggrad(upper)
  as before), existing ssm_plot vdiffr snapshots unchanged. Verified: ssm's own
  displacement CIs stay in [0,360) with span <360 (unwrap-around-mean bound;
  worst observed 335° over noisy sims), so the new stat validation never fires
  on `ssm_plot_circle()`; cpm's analytic angle CIs can exceed 360° span but are
  pre-filtered to point-only. Recorded to ROADMAP continuous track (not fixed):
  Set2 caps at 8 colours, so both circle plots degrade for >8 keyed levels — a
  package-wide palette policy, not a per-plot patch. Tests: test-cpm_plot.R
  (+5: build/geometry, vdiffr hero + no-legend snapshots, inestimable-CI
  point-only warning, arg validation); test-geom_ssm.R (+3: parent-structured
  empty arc frame, sub-circle span rejection + seam-crossing still accepted,
  defined-estimate/undefined-CI renders point-no-wedge). Suite 892/892; check
  0/0/0; document() no-diff (pre-existing internal-link warnings only). NEWS
  updated. (R/cpm_oop.R, R/geom_ssm.R, R/ssm_plot.R,
  tests/testthat/test-cpm_plot.R [new], tests/testthat/_snaps/cpm_plot/ [new],
  tests/testthat/test-geom_ssm.R, man/plot.circumplex_cpm.Rd [new],
  man/geom_ssm_arc.Rd, NAMESPACE, NEWS.md, ROADMAP.md, MILESTONES.md.)
- 2026-07-06 — B4 `cpm_simulate()` (Opus, test-first; inline /code-review,
  clean). New exported `cpm_simulate(object, n)` in R/cpm_fit.R: draws n
  standardized rows from the fitted P̂ via the exact-PSD factor form
  x = Λz + (I − D_ζ²)^{1/2} ε, with Λ/ζ/β rebuilt from the stored canonicalized
  post-polish γ̂/spec (cpm_unpack) so the generative covariance equals
  matrices$Phat to machine precision (independently verified in-test to 1e-10);
  a polished-out harmonic has β_k = 0 ⇒ zero Λ columns, no dropping. Resolves
  the three §8.2 A-side gaps: G1 return contract (numeric n×p, fitted scale
  order, colnames = scales, rownames NULL, zero-mean unit-variance margins so
  cor→P̂, one RNG consumption — scores then uniques, fixed order); G2 (documented
  mean-based-path-only; correlation path reduces to matrices$Phat, B augments
  itself — no signature change); G3 (dimnames on matrices$R/Phat/residuals in
  fitted order, set in cpm_fit()). RNG contract in a @section Reproducibility
  (DESIGN.md master-list row deferred to W2, matching B3). Tests
  (test-cpm_api.R, +12): shape/type/names contract, factor-form covariance
  identity 1e-10, large-n cor→P̂ + standardized margins, seed reproducibility +
  sensitivity + .Random.seed consumption, 0/360-pole boundary, polished-harmonic
  covariance preservation, a Z1 mean-based-loop prototype (rescale to μ/SD →
  ssm_analyze recovers the profile), inherits()/is_count() validation; updated
  the existing Phat-diagonal test for the new dimnames. Suite 869/869; check
  0/0/0; document() no-diff. NEWS + design-doc §5.4/§11 updated.
  (R/cpm_fit.R, tests/testthat/test-cpm_api.R, man/cpm_simulate.Rd [new],
  man/, NAMESPACE, NEWS.md, devel/m4-browne-design.md, MILESTONES.md.)
- 2026-07-06 — B3 bootstrap CIs, raw-data default (Fable, test-first;
  /statistical-validation; 8-finder /code-review high). New internal
  `cpm_bootstrap()` + `cpm_mirror_guard()` in R/cpm_fit.R: full index array
  drawn up front in one RNG block (boot::boot convention — discards don't
  shift the stream), per-replicate Pearson R with NA/non-PD refusal, warm
  start from the canonicalized γ̂ under the post-polish spec, acceptance
  keyed ONLY on the §3.5 scaled gradient norm (F2; one deterministic restart
  from a stalled point before excluding — rescues ~1/3 of the ~1% of stalls),
  mirror guard per A-review F10 (reflect when circularly closer to −γ̂ via
  angle_dist; F-isometry + involution pinned by tests), angle CIs through
  quantile.circumplex_radian (straddling CIs wrapped, lci > uci, displacement
  convention), ζ/β percentile CIs, ssm_analyze-style exclusion warning +
  boots_used/degenerate/nonconvergent/reflected accounting in details and a
  summary() diagnostic line. `ci_method` default now path-conditional
  (bootstrap on raw data, analytic on cormat) per §5.2; roxygen gains
  Reproducibility + expanded CI sections (seed convention: only the bootstrap
  consumes RNG; point estimates seed-invariant). Timing: default 2000-rep
  bootstrap on jz2017 octants ≈ 13 s (under the ~30 s Phase-2 C++ trigger;
  ~6.4 ms/replicate). Validation: circular-quantile rotation invariance
  ≤ 2e-15; every pooled replicate confirmed a genuine local optimum by an
  independent BFGS oracle (worst improvement 7.7e-08); jz2017 end-to-end
  circular bracketing; coverage smoke + resample-multimodality findings
  recorded under B6 (they are the coverage oracle's to quantify). Review:
  8-finder swarm; 2 low-severity findings fixed (complete.cases over all
  three replicate matrices — quantile's na.rm=FALSE would silently corrupt
  on a future NA; boots_reflected now counted over used replicates);
  refuted with evidence: dropping the eigen PD check (load-bearing — the
  gradient never sees ln|R|, nlminb would silently pool a singular-resample
  fit) and keying acceptance on the nlminb code (F2 forbids). Suite 842/842;
  check 0/0/0; document() clean. NEWS updated. (R/cpm_fit.R, R/cpm_oop.R,
  tests/testthat/test-cpm_api.R, tests/testthat/_snaps/cpm_api.md,
  man/cpm_fit.Rd, NEWS.md, MILESTONES.md.)
- 2026-07-06 — B2 `cpm_fit()` API + `circumplex_cpm` class (Opus, test-first;
  inline /code-review). New exported `cpm_fit()` wrapping the B1 engine: raw-data
  (Pearson R, listwise-only) and cormat paths with `is_*()`/`stopifnot()`
  validation (exactly-one-of data/cormat, symmetric unit-diagonal cormat, n > p,
  angle-length match, PD refusal via the engine); reference-angle fix; the §5.3
  fit indices (T = (N−1)·F̂, χ², RMSEA + 90% CI with BOTH λ guards, off-diagonal
  SRMR, CFI/TLI, AIC/BIC with ln N); §5.2 analytic (Wald) CIs from
  avar = (2/n)H⁻¹ (fresh FD-of-analytic-gradient Hessian at the reported par;
  logit/softmax/angle delta method); the §5.4 `circumplex_cpm` object
  (results/betas/fit/corfun/matrices/details, incl. the Brief-B contract fields
  θ/ζ/β, N, m + programmatic spec/par handles). New R/cpm_oop.R: `new_cpm()`
  constructor (new_ssm style) + print/summary with the boundary/convergence
  diagnostics and the N-conditional (< 2000) analytic-CI caution. Scope split
  with B3: bootstrap CIs deferred; `ci_method` defaults to "analytic" and an
  explicit bootstrap request errors (B3 flips the raw-data default to bootstrap
  per §5.2/§10). Review finding fixed: `Angle_theory` echoed the engine's wrapped
  angle, misreporting the LM top pole as 0 — now echoes the user's supplied
  angles (LM = 360 per CLAUDE.md). Design-doc correction recorded (§11): the
  §5.3 RMSEA lower-guard inequality was stated backwards; implemented the
  standard `pchisq(T,df) < .95 → λ_L = 0` (reproduces the section's own
  [0,0] example). Independent validations in-test: brute-force delta-method SE
  cross-check to ~1e-12 (ζ/β), hand-derived N−1 multiplier, SRMR denominator
  (vs the p(p+1)/2 trap), RMSEA noncentral-tail reconstruction, degrees/numeric
  API equivalence, raw↔cormat agreement, Heywood→NA-CI, df=0 saturated warning;
  print/summary snapshots. Suite 805/805; check 0/0/0; document() no-diff.
  NEWS updated (user-facing). (R/cpm_fit.R, R/cpm_oop.R [new],
  tests/testthat/test-cpm_api.R [new], tests/testthat/_snaps/cpm_api.md [new],
  NEWS.md, NAMESPACE, man/, devel/m4-browne-design.md, MILESTONES.md).
- 2026-07-06 — B1 CPM engine core (Opus implementation against the Brief-A
  spec, test-first; Fable statistical review + fixes; Sonnet/Opus finder
  swarm for /code-review high). New R/cpm_fit.R (internal only, nothing
  exported): rho/rho', implied P, variants A-D with df table, logit/softmax
  unconstrained parameterization, ML discrepancy + analytic gradient,
  deterministic multi-start (mirror start only when angles are free),
  scaled-gradient-norm acceptance (nlminb code advisory), beta boundary
  polish via a shared cpm_spec_reduce() (df increases, m-as-fitted decreases
  iff the top harmonic drops), theory-first canonicalization with CCW
  tie-break, diagnostics (Heywood, Hessian condition, multimodality).
  Fable review found+fixed: diag(scalar) crash when polish removes ALL
  harmonics; false multimodality flag on exact-octant data (mirror detection
  now circular via angle_dist — the ±pi atom); logit-scale comparison
  exploding near Heywood boundaries (natural-scale now); B/D duplicate
  mirror start making "reproduced" vacuous; m reported as nominal after
  top-harmonic removal; plus a DESIGN DEVIATION recorded in
  devel/m4-browne-design.md sec. 11 and flagged for Jeff: the multimodality
  flag now fires only on COMPETITIVE distinct optima (near-tied), not on any
  strictly-worse jitter basin (the spec's literal rule fired on clean octant
  data). Refuted after analysis: "reproduced uses pre-polish Fs" (nested-
  model gate pins the polished F-hat transitively; argument now in the
  code comment). /statistical-validation: 16/16 independent checks pass
  (scratch factor-form P, Cholesky-route F, scratch-objective BFGS optimizer
  agreement to 7e-13, fft() circulant oracle, Richardson-FD gradient 2e-11,
  pole/general-factor/unequal-spacing boundaries, SSM pipeline untouched).
  Deferred with rationale: objective/gradient shared-factorization cache
  (Phase-2 profiling gate, R stays the audit oracle). Suite 735/735;
  check 0/0/0. No NEWS (internal; user-facing entry lands with B2).
  (R/cpm_fit.R [new], tests/testthat/test-cpm_fit.R [new],
  devel/m4-browne-design.md, MILESTONES.md).
- 2026-07-06 — M4 milestone opened (Fable). Branched `m4-fit-statistics` off
  master @ c8525a3 (the held v1.3.0 state). Moved the completed M3 section +
  full log to MILESTONES-ARCHIVE.md (before release, deviating from the
  on-release convention, so this file holds the single active milestone;
  archive entry notes v1.3.0 is still held). Drafted the M4 task list above
  from ROADMAP M4 + the three committed design docs; recorded the
  adopted-by-default decisions (names, variants B–D, ladder, class naming)
  for Jeff to veto in the first review. (MILESTONES.md, MILESTONES-ARCHIVE.md).

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0). When the active
milestone ships, `/release-checklist` moves it there too. This file stays
scoped to the active milestone so it is cheap to re-read at the start of
each task.
