# Active milestone

## M4 — Browne model & SSM CI trustworthiness

Source: ROADMAP.md Milestone 4. Branch: `m4-fit-statistics`, cut from the
GitHub-complete-but-held v1.3.0 state (master @ c8525a3, 2026-07-06).
**Rescoped 2026-07-07 (Jeff):** the structure tests (T1–T7) split off into
M4.5 (queued section below) — M4 closes with the Browne model, the CI
diagnostic, and their vignette/ship docs. **Release change (same day):** no
own CRAN slot; the held v1.3.0 is never submitted separately — M2 through M5
fold into one v2.0.0 targeted ~2026-08-02 with a ~2026-07-26 code freeze,
and M6 ships later as its own release (see ROADMAP's CRAN release strategy,
which also now holds the v2.0.0 pre-release items formerly listed at the
bottom of this section).

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
(§12.4). Added by Z1 (2026-07-07): a contrast row's certification event =
both constituent profile rows certified (this conditions the contrast's
conditional-displacement coverage — the spec left it unpinned);
`amplitude_factors` must include 1 (the verdict rung); the verdict table
classifies e/a/d-conditional only, with x/y reported in `coverage` but not
verdict-driving (spec §5.1 as written); `structure=` selects one population
per call — the §3.2 cpm-vs-observed sensitivity comparison is two calls,
with any cross-call `summary()` wording left to Z2. Added by Z2
(2026-07-07): the §4.1 margin rung is one joint rung at the LARGEST
half-width/â ratio among near-zero rows (the neediest row lands exactly at
its margin, others at or above; on the correlation path a rung that pushes a
population correlation to ±1 is dropped with a warning, not fatal); the
`Structural` coverage flag keys off the population displacement truth being
undefined (exactly the mean-path zero-amplitude case; contrast rows never
flagged — their amplitude difference is unconstrained); the §3.2 cpm-vs-
observed cross-call comparison ships as wording only (each `summary()` names
the other configuration as a sensitivity check; no cross-call state); the
§5.2 structure-note fit benchmarks are RMSEA ≤ .08 / > .10 (Browne & Cudeck,
1993) and SRMR ≤ .08 (Hu & Bentler, 1999), as constants with provenance
comments.

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

- [x] **Z0. `ssm_analyze()` sufficient-statistics storage** (prerequisite;
  Brief B §8.3). Store per-group n, scale SDs, and correlation matrices in
  `circumplex_ssm$details`, with a `data =` fallback + consistency check for
  old objects.
  *Accept:* new fields populated on both analysis paths; old-object fallback
  tested; no change to any estimate or seeded pin.
- [x] **Z1. `ssm_ci_accuracy()` core loop.** Spec §3: one `cpm_fit()` on
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
- [x] **Z2. Amplitude-near-zero module + verdict.** Spec §4–§5: the joint
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
  *Scope note (2026-07-07):* the §10 Z&W-reproduction gate (O5 bridge) is
  transcription-bound (all values TBT under the two-session protocol) and
  moves to W1, where the Z&W transcription happens anyway — recorded in the
  W1 bullet below; everything else in §10 executed here or at Z1.

### Tasks — Ship

- [x] **W1. Vignette: "Evaluating Circumplex Structure".** Fit statistics,
  CI trustworthiness, when to trust SSM parameters, ipsatization guidance;
  Z&W Studies 1–5 transcribed as cited context (re-confirm the grid
  characterization at transcription time and log it — spec §2/F8). Also
  carries the §10 O5 bridge deferred from Z2: once the Z&W generating
  conditions are transcribed, run the diagnostic at (at least) one of their
  conditions and compare to their published coverage within combined MC
  error — a conditional gate (spec F7): if their generating process is not
  MVN-expressible, re-scope and document, never silently loosen.
  *Rescoped by the 2026-07-07 split:* covers the M4 content only (CPM
  fitting + CI trustworthiness); the structure-test section is M4.5's
  vignette-extension task. "Fit statistics" here means the CPM fit indices,
  not the A&R tests.
  *Accept:* builds clean; exported API only; statistical-precision bar
  (CLAUDE.md); Z&W numerics transcribed, never from memory. Also the natural
  point to confirm/veto the B6 analytic-CI caution wording (now tracked in
  ROADMAP's v2.0.0 pre-release items).
- [x] **W2. Ship-time documentation.** DESIGN.md RNG entry-point list gains
  `cpm_fit(ci_method="bootstrap")`, `cpm_simulate()`, `ssm_ci_accuracy()`
  rows; update `ssm_analyze()`'s "only function that consumes R's RNG"
  roxygen (false once these ship); NEWS.md flagship entry; record the
  guardrail-replacement follow-up (B §12.5) and the F6 0-vs-360 pole-snap
  alignment decision (still parked) in ROADMAP's continuous track.
  *Accept:* document() no-diff; DESIGN.md consistent; follow-ups recorded
  where ROADMAP says they live.

Milestone close: when W1+W2 land, follow ROADMAP's between-releases
practice — milestone-close `/code-review` over M4's full cumulative diff
(`max`: this milestone is statistically risky — the CPM estimator and the
CI diagnostic), dev-version bump, lightweight `m4-complete` tag, archive
this section to MILESTONES-ARCHIVE.md (GitHub-complete; the CRAN
review/checklist belongs to the v2.0.0 release), and promote the queued
M4.5 section below to active — copying the cross-cutting guardrails block
into it, since M4.5 references it and archiving M4 would otherwise orphan
that reference.

# Queued milestone: M4.5 — Structure tests (Acton & Revelle 2004)

Split from M4 on 2026-07-07. Becomes the active milestone when M4's W tasks
close (kept here rather than ROADMAP so the drafted acceptance criteria
survive verbatim; ROADMAP carries the milestone-level summary). The M4
cross-cutting guardrails above (oracle rule, boundary suites, RNG contract,
dependency policy, model tiers) apply unchanged.

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
- [ ] **T8. Vignette extension.** Add the structure-test section to the
  "Evaluating Circumplex Structure" vignette written in M4/W1 (which covers
  CPM fitting and CI trustworthiness): what each A&R test asks, the nv=8
  cutoff provenance (T2), and how the tests complement the CPM fit indices.
  *Accept:* builds clean; exported API only; statistical-precision bar
  (CLAUDE.md); every reported threshold traces to T2 or a cited page.

## Log

- 2026-07-07 — M4 milestone-close review (Opus; `/code-review max` over the
  full cumulative diff `master..HEAD`, per ROADMAP's between-releases practice
  — pinned at max because the milestone is statistically risky). Five parallel
  finder angles + inline verification against source; two angles (fit indices,
  resampling/RNG) were execution-backed and returned clean, and the shared
  Monte Carlo refactor was reconfirmed byte-identical to the pre-refactor
  engine (pin at 1e-10). **No release-blocking bug.** Verified sound: the
  analytic CPM gradient FD-checks to ~1e-9 across all variants/boundaries; the
  sec. 5.3 fit indices numerically (T=(N−1)F̂, RMSEA CI, analytic-vs-bootstrap
  SE ratios, Heywood→NA, LM=360 echo); the RNG contract on error paths; the
  c=0 amplitude-coverage identity; pole/branch-safe displacement coverage.
  9 findings reported; Jeff's disposition: fix the safe set, defer the
  statistical/judgment ones. **Fixed (test-first for the behavioral ones):**
  (i) CFI/TLI now return 1 rather than NaN (0/0) / Inf when the independence
  baseline has no misfit — near-independence + good-fitting model
  (R/cpm_fit.R `cpm_fit_indices`; regression test in test-cpm_api.R);
  (ii) `cpm_gradient()` no longer recomputes `outer(theta,theta)`/`cpm_rho()`
  twice on the hottest path (P built inline from a single Rho — provably
  bit-identical, FD-reconfirmed to ~1e-9, all gradient/oracle tests green);
  (iii) `ssm_suff_stats()` gained an `envir` argument and `ssm_ci_accuracy()`
  forwards `parent.frame()`, so the pre-Z0 `data=` fallback resolves the
  recorded call's scales/measures/grouping symbols from the *user's* scope,
  not the function's frame (regression test in test-ci_accuracy.R);
  (iv) the unreachable "Hessian is singular" else-if was folded into the
  ill-conditioned branch (message adapts on finiteness); (v) `cpm_simulate()`
  was factored into `cpm_sim_root()`/`cpm_sim_draw()` so `ssm_ci_accuracy()`
  precomputes the CPM loadings once per call instead of once per replicate
  (draw order preserved → seeded ci-accuracy pins byte-identical, snapshot
  unchanged); (vi) `boots`/`interval` validation switched to
  `is_count()`/`is_num()` per house style. **Deferred to ROADMAP's continuous
  track (2026-07-07 M4 entry):** the free-angle "reproduced" vacuous-
  acceptance guard (Fable), the contrast-certification print/diagnostic
  consistency (Jeff's call), and the analytic-path double-Hessian
  (reclassified during fixing as not behavior-neutral — the SE path is
  test-pinned to `cpm_hessian_fd`). Full suite 0 failed / 0 skipped
  (NOT_CRAN; 3 pre-existing ill-conditioned-Hessian warnings on real octant
  data); `document()` no man/NAMESPACE diff. (R/cpm_fit.R, R/ssm_analysis.R,
  R/ssm_ci_accuracy.R, tests/testthat/test-cpm_api.R,
  tests/testthat/test-ci_accuracy.R, ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — W2 ship-time documentation (doc-only task, no plan/test-first
  steps per next-task's step-2 exemption). DESIGN.md's RNG-consuming
  entry-point inventory (Reproducibility section) updated from one entry to
  four: `ssm_analyze()`'s existing table kept as-is, and new prose entries
  added for `cpm_fit(ci_method = "bootstrap")` (bootstrap-only stream
  consumption, resample indices drawn in one master-stream block matching
  `ssm_analyze()`'s convention), `cpm_simulate()` (direct stream consumption,
  factor scores then unique deviates in fixed order), and `ssm_ci_accuracy()`
  (one `sample.int()` draw seeds an internal L'Ecuyer-CMRG generator with
  per-replicate substreams, caller state restored on exit) — content matches
  each function's own `@section Reproducibility` roxygen (added in B3/B4/Z1).
  `ssm_analyze()`'s roxygen "only function in the package that consumes R's
  random number stream" claim (now false) corrected to name the other three
  RNG-consuming entry points; `devtools::document()` re-run, diff confined to
  that one section of `man/ssm_analyze.Rd` (pre-existing internal-link
  warnings only, unrelated). NEWS.md gained a short flagship-summary
  paragraph atop the development-version section naming `cpm_fit()`
  (CircE replacement) and `ssm_ci_accuracy()` as the release's headline
  additions, pointing to the new vignette — ahead of the existing detailed
  per-feature bullets, which are unchanged. ROADMAP's continuous track
  gained two recorded follow-ups per the W2 acceptance criteria: the
  guardrail certification-rule replacement (B-review F1.ii / spec §12.5–§13,
  Jeff's 2026-07-03 ship-now-redesign-later decision, informed by Z2's
  near-100% false-certification measurement) and the 0-vs-360 pole-snap
  alignment decision (estimator-audit-2026-07-fable.md F6 — point estimator
  emits exactly 360.0 at the pole, `quantile.circumplex_radian()` still
  snaps a re-wrapped CI endpoint to 0, confirmed still live by reading
  R/ssm_bootstrap.R; cosmetic per F6, single-snap-direction fix parked).
  Suite 1206/1206 (unchanged, doc-only); `document()` diff confined to the
  intended section. (DESIGN.md, NEWS.md, R/ssm_analysis.R, man/ssm_analyze.Rd,
  ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — W1 vignette + Z&W transcription + O5 bridge (Fable,
  plan-first; 8-angle /code-review high, 10 findings fixed + 3 refuted/
  declined with rationale, 1 verified-coincidence recorded). **Transcription
  (oracle rule, two-session protocol):** Zimmermann & Wright (2017) Studies
  1–5 transcribed from the publisher PDF into devel/m4-zw-transcription.md
  (channel 1 visual read + channel 2 pdftotext, diffed; one text-layer
  artifact — Eq. A7's √2 — resolved by reproducing all five published
  scaling factors and all three Eq. 3 worked values from the transcribed
  parameters; Table 4 PARPD/OCPD rows recorded with the jz2017
  reproduction cross-check, every point estimate matching published
  rounding; Jeff's independent human re-read pending, folded into the
  ROADMAP pre-release re-read item). F8 grid characterization re-confirmed
  and logged ("coarse and fixed" stands: 2 matrices, 7-value n grid,
  octants only, one engine). **Transcription finding: the article has NO
  supplemental materials** (Europe PMC + text itself) — the spec's assumed
  per-condition value source never existed, so the O5 bridge was re-scoped
  per the F7 mechanism (documented in the record, never loosened):
  published per-condition coverage values don't exist; the sharpest
  published anchors are Bradley-band accurate/inaccurate classifications
  and the Eq. 3 frontier. **O5 bridge (devel/m4-zw-bridge.R + results
  rds):** MVN gate passes (their generation is explicitly MVN); exact
  Study-3 populations (Note 3 circulant + cosine target row, E=0, DOM=0,
  R²=1, peak at our 0/360 pole) fed through structure="observed" via
  exact-sample-correlation data; 4 conditions × 5000 reps (their own MC
  precision), both matrices × clearly-accurate/clearly-inaccurate per the
  frontier: **4/4 published classifications reproduced** (accurate
  conditions 93.7–94.5% inside the band; inaccurate conditions one-sided
  truth-below under-coverage a 88.9/81.1%, d|cert 85.9/80.4%; elevation
  adequate everywhere at n=100). **Vignette** (evaluating-circumplex-
  structure.Rmd, M4 scope only — CPM + CI trustworthiness, no A&R):
  cpm_fit on jz2017 with fit-index benchmarks (constants match the
  package's cited ones), variant comparison qualitatively reproducing
  Z&W's CircE pair (our .956 CFI free-angles vs their .958; model
  difference documented per B6), B6 cautions taught (boundary solutions
  common, χ² descriptive only), ssm_ci_accuracy demo at n=250
  (PARPD-vs-OCPD contrast; 100% false-certification guardrail line as the
  central teaching point), Z&W thresholds table + Eq. 3 as transcribed
  cited context, when-to-trust checklist, ipsatization section (raw-vs-
  ipsatized comparison chunk; elevation .250→.007). Wording bar held (no
  "significan*", grep-asserted). Analytic-CI caution wording CONFIRMED
  (advisory) — recorded on the ROADMAP pre-release item, Jeff's veto open.
  Review fixes: dangling ".25" cross-reference now computed in-document;
  OCPD .012 made record-traceable; bridge summary borderline/NA logic
  robust (stored results unaffected, still 4/4); pkgdown reference section
  for the M4 exports (pre-existing index gaps for instrument pages left
  for W2/release); NEWS devel-path citation genericized; seed-brittle
  prose made rerender-robust; boots=500 in the demo (vignette build 28s →
  12s); precision hedges (essentially unbiased; MC-SE claim qualified near
  nominal). Refuted/declined with rationale: per-call set.seed flagged as
  anti-convention (intro vignette line 359 explicitly endorses it; matches
  the new APIs' Reproducibility docs), circulant-loop vectorization and
  single-vapply table (validation-script/teaching clarity preferred).
  Checks: devtools::check 0/0/0 (pre-fix run; post-fix run repeated),
  vignette renders clean, exported API only. (vignettes/evaluating-
  circumplex-structure.Rmd [new], devel/m4-zw-transcription.md [new],
  devel/m4-zw-bridge.R [new], devel/m4-zw-bridge-results.rds [new],
  NEWS.md, _pkgdown.yml, ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — Between-releases working practice adopted (Jeff + Fable):
  real versions/annotated tags/GitHub Releases only at CRAN submissions;
  per milestone close — dev-suffix bump, lightweight `mX-complete` tag, and
  a milestone-close /code-review over the cumulative milestone diff (high
  normally, max for statistically risky milestones) so the release review
  verifies reviewed strata + seams instead of first-passing everything.
  Recorded in ROADMAP's release-strategy section; M4's close note updated
  to follow it (M4 close review pinned at max). (ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — v2.0.0 scope refined (Jeff + Fable, same discussion as the
  split): the train is M2–M5, not M2–M6 — M6 is deliberately excluded (its
  paired/dependent circular resampling and displacement growth models need
  a design brief and their own release, ~v2.1.0, informed by v2.0.0 field
  feedback). Freeze rule adopted: code freeze ~2026-07-26; milestones
  GitHub-complete and reviewed by freeze ship, the rest ride the next
  release — scope is the variable, never the date or the statistics.
  (ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — M4 split + release-strategy change (Jeff's call; Fable
  executed the restructure). M4 rescoped to the Browne model + CI
  trustworthiness + their vignette/ship docs (W1 rescoped accordingly);
  structure tests T1–T7 moved verbatim, with acceptance criteria, to a
  queued M4.5 section in this file (ROADMAP gets the milestone-level summary
  and a new M4.5 entry). CRAN plan superseded: the held v1.3.0 is never
  submitted separately — M2 through M6 fold into one v2.0.0 targeted
  ~2026-08-02 (one month after the v1.2.0 submission, CRAN-approved
  2026-07-02), on the strength of much-faster-than-planned progress; the
  M4 pre-release open items (CircE second re-read, analytic-CI caution
  confirm/veto) moved to a v2.0.0 pre-release list in ROADMAP's CRAN
  release-strategy section, which also carries the release-review depth
  (`/code-review max` minimum; ultra only if Jeff asks) and the single
  `/release-checklist` run. (ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — Z2 amplitude-near-zero module + verdict (Fable, plan-first,
  test-first; /statistical-validation 8/8; 8-angle /code-review high, 8
  findings fixed + 2 refuted with rationale). The spec §4–§5 analysis layer
  over Z1's tables. In ssm_ci_accuracy(): the §4.1 degenerate-ladder margin
  rung — when a profile row's â is below half its own CI width, one absolute
  rung at the certification margin (c = halfwidth/â; exact by ladder
  linearity for any c and spacing; max over affected rows on the joint
  ladder; a corr-path |r|≥1 refusal drops the rung with a warning while user
  rungs still fail hard — policy stated in a comment) with
  details$margin_rung/near_zero_rows/conditions; the guardrail table gains
  Cert_lci/Cert_uci (95% Wilson), N_reps, and the STORED false-certification
  Caution decision (c=0 rung: Wilson lower bound > the (1−interval)/2
  user-expectation benchmark — never banded as a nominal level); the
  coverage table gains N_conditional (certified-replicate count behind
  conditional-d coverage) and Structural (the §4.2 theorem flag, keyed to an
  undefined displacement truth ⟺ mean-path zero amplitude; contrast rows
  never flagged — their amplitude difference is unconstrained); details adds
  row_n and max_psd_delta (threshold shared with the construction warning
  via ssm_ci_psd_warn). New §5.2 reporting layer (R/ssm_ci_oop.R): shared
  per-profile verdict blocks in print()+summary() (coverage lines with
  miss-direction phrasing for inadequate amplitude; the guardrail caution
  line consuming the stored decision; plain-language CAUTION/BORDERLINE/
  ADEQUATE paragraph — the printed headline elevates to CAUTION when the
  caution fires, documented against the coverage-only verdict table);
  structure note with cited fit benchmarks (RMSEA ≤.08/>.10, Browne & Cudeck
  1993; SRMR ≤.08, Hu & Bentler 1999; constants with provenance comments),
  acceptance/marker/PSD downgrade annotations, and the observed-vs-cpm
  sensitivity advice (the cross-call wording Z1 deferred: each call names
  the other configuration, no cross-call state); near-zero-regime note;
  structural-zero footnote; wording bar held (no "significan*" anywhere,
  asserted by test). New plot() method: coverage vs ladder, six facets
  (e/x/y/a/d/d-certified; drop=FALSE so a never-certified panel shows empty
  rather than vanishing), Wilson error bars, Bradley band shaded, nominal
  dashed, structural zeros as open symbols; vdiffr snapshot. §10 executed:
  known-good oracle (elevation adequate, seeded n=300/boots=1000/reps=1000);
  known-bad direction oracle OFF c=0 (amplitude coverage below nominal by
  one-sided binomial test with truth-below misses at c=.15; c=0 false-cert
  Wilson lower bound > benchmark, directional only); band-edge unit tests;
  seeded contrast branch-pathology counter > 0 at the near-zero rungs
  (measured rare, ~0.3–0.7% even where it lives — test sums the c=.05 and
  c=0 rungs); engine parity spot-check (bootstrap vs Monte Carlo coverage
  within 0.12); the Z&W O5 bridge is transcription-bound (oracle rule) and
  moved to W1 — recorded in both task bullets. /statistical-validation 8/8:
  Wilson ≡ prop.test to 1e-16 incl. k=0/k=n; Bradley classifier brute-forced
  against an independent prop.test rule over every k for n ∈ {20,100,500,
  1000} × 3 nominal levels; caution trigger ≡ independent rule; c>1 ladder
  linearity by hand 2/n·Σ formulas ≤ 5.7e-14 (equal/unequal/pole); end-to-
  end margin-rung truth ≡ observed half-width to 2.8e-17; guardrail columns
  ≡ prop.test from the table's own counts; verdict↔coverage accounting
  identity; Structural rows exactly the zero-truth mean-path a-rows. Review
  (8 finder angles, self-verified with evidence): fixed — stored-vs-printed
  verdict disagreement (Caution now in the guardrail table), summary ladder
  line now enumerates all simulated conditions (details$conditions), facet
  drop=FALSE, PSD threshold+max stored once, contrast identity from
  details$contrast rather than NA-n, unused digits arg dropped, guardrail-
  line/rounding duplication folded; refuted with rationale — ssm_ci_pct vs
  str_percent (floor-vs-round semantics, different consumers) and Set2
  palette (8-level cap; package palette policy is the recorded ROADMAP
  follow-up); no-action, recorded — a large margin rung legitimately
  stretches the plot x-axis (it reflects a true â ≪ half-width regime), and
  duplicate-label lookup risk is the object's pre-existing unique-label
  keying. Suite 1206/1206; check 0/0/0; document() clean. NEWS extended.
  (R/ssm_ci_accuracy.R, R/ssm_ci_oop.R, NAMESPACE,
  man/plot.circumplex_ci_accuracy.Rd [new],
  man/summary.circumplex_ci_accuracy.Rd [new], man/, NEWS.md,
  tests/testthat/test-ci_accuracy.R, tests/testthat/_snaps/ci_accuracy.md
  [new], tests/testthat/_snaps/ci_accuracy/ [new], MILESTONES.md.)
- 2026-07-07 — Z1 `ssm_ci_accuracy()` core loop (Fable, plan-first, test-first;
  /statistical-validation; 8-angle /code-review high, 6 findings fixed + 1
  refuted by benchmark). New exported `ssm_ci_accuracy()` (R/ssm_ci_accuracy.R)
  + `circumplex_ci_accuracy` class with print/summary (R/ssm_ci_oop.R): spec §3
  in full — pooled within-group R_w (Wishart-df n device), one analytic
  `cpm_fit()` (or `structure = "observed"`, or a validated pre-fit via `cpm=`),
  plug-in populations per group (mean path: `cpm_simulate()` rescaled by stored
  SDs; corr path: joint matrix with scale block ← P̂, eigenvalue-clamp PSD
  repair with max|ΔJ| recorded and a >.01 warning, |r|≥1 refusal), per-rung
  truth recomputation, and a reps×conditions replay of the object's own CI
  procedure. The §4.1 amplitude-ladder *construction* ships here (needed for
  the c=0 pin): the estimator-functional 3×3 solve, exact for any spacing
  (machine-precision tests incl. unequal angles; independent lm()/hand-formula
  validation; the naive decomposition demonstrably fails off equal spacing) —
  §4's analysis layer (miss-decomposition reporting, false-cert benchmark line,
  §5.2 verdict wording, plot) stays Z2. Replay: bootstrap via per-group
  multinomial resample counts (exact same law as boot's iid index draw; one
  BLAS crossproduct per replicate for means, centered weighted moments for
  correlations with a relative variance floor mapping true-constant resamples
  to NA) or the extracted `ssm_mc_replicates()` — the MC engine core now
  SHARED with the production path, where the three deferred ROADMAP efficiency
  findings landed (vectorized psi with precomputed squares, one batched
  `group_parameters()` call, name-driven block keys hardened by a uniqueness
  guard); user-path MC output pinned byte-identical to a pre-refactor seeded
  fixture (tests/testthat/fixtures/mc-seeded-pins.rds). Interval assembly via a
  lean radian-domain assembler pinned equal to `ssm_replicate_intervals()`
  (shared quantile.circumplex_*radian methods, same contrast branch-alignment);
  displacement coverage is angular arc membership mod 360 (pole + branch-shift
  safe, validated against independent geometry over 20k cases); certification
  via new shared `ssm_certified()` (R/ssm_oop.R — print and diagnostic now one
  rule); Wilson-95 (validated vs prop.test) × Bradley band verdicts at c=1,
  d conditional on certification. RNG: one documented `sample.int()` from the
  caller's stream seeds a CMRG master, per-(condition×rep) `nextRNGStream`
  substreams, caller `.Random.seed`/kind restored on exit;
  serial ≡ multicore ≡ snow at fixed seed (multicore tested; full-scale
  serial≡parallel also verified on jz2017). Machinery pins green: c=0
  amplitude coverage ≡ 0, all misses truth-below, d coverage NA, guardrail
  rates still produced. Cost (clean, seeded): n=300 defaults 111 s — inside
  the §11 envelope at the spec's own cost-model scale; full jz2017 (n=1166)
  427 s serial / 274 s ncpus=4 → the §8/§11 Phase-2 trigger fires at
  full-jz2017 scale; recorded in ROADMAP with the profiling constraint (75%
  of the loop is rmultinom RNG draws; sample.int+tabulate benchmarked only
  ~10% faster, so a straight C++ port won't clear it — needs a draw-strategy
  decision). jz2017 verdict at n=1166: e/a/d-cond all adequate (94.7/95.0/
  95.8%). /statistical-validation: 10/10 independent checks (lm + hand-formula
  ladder, prop.test Wilson, PSD-repair properties, 20k-case membership
  geometry, independent flat-script end-to-end coverage replication at a pole
  truth agreeing within MC error, elevation near-nominal both ways, psi ==
  fresh Hampel loop). Review fixes: undeclared `parallel` dependency
  (DESCRIPTION); contrast-row Fit_pass_rate NaN → NA; name-key ambiguity now
  errors instead of silently first-matching; clear error when pooled n ≤ p;
  hard-coded parameter columns → `ssm_param_names()`-driven; shared
  `mvn_root()` (engine + population generator can't drift); refuted with
  evidence: sample.int+tabulate swap (benchmarked ~10%, not the claimed 3-8×,
  and it would re-key seeds). Deliberate, recorded: lean-vs-real assembler
  duplication (perf; pinned by test, more oracles in Z2), bootstrap replay not
  driving boot::boot (spec §3.4 freedom — the §11 cost note exists for exactly
  this), micro-optimizations ≤3% declined. Suite 1156/1156; check 0/0/0;
  document() no-diff. NEWS updated (diagnostic + MC speedup). (DESCRIPTION,
  R/ssm_ci_accuracy.R [new], R/ssm_ci_oop.R [new], R/ssm_montecarlo.R,
  R/ssm_oop.R, NAMESPACE, man/ssm_ci_accuracy.Rd [new], man/,
  tests/testthat/test-ci_accuracy.R [new], tests/testthat/fixtures/ [new],
  tests/testthat/test-ssm_montecarlo.R, NEWS.md, ROADMAP.md, MILESTONES.md.)
- 2026-07-07 — Z0 sufficient-statistics storage (Opus, test-first; 2-angle
  /code-review high, 1 confirmed correctness bug fixed). New internal
  `ssm_compute_suff_stats()` in R/ssm_analysis.R computes per-group n, per-scale
  SDs (mean path only), and the within-group correlation matrix (scale-only for
  the mean path; joint scales+measures for the correlation path), keyed by
  sorted factor level (= ssm_analyze() row order) on within-group complete cases
  (assessed-as-listwise per spec §9). Both analysis paths now store the result
  as `details$suff_stats` (a pure list addition — no estimate or seeded pin
  changed; print/summary read only named fields). New internal
  `ssm_suff_stats(object, data =)` returns the stored stats, or for objects
  predating storage recomputes from re-supplied data (recovering scales/measures/
  grouping/listwise from the recorded `match.call()`, so name-normalized
  regardless of how args were passed) and rejects the wrong dataset via a 1e-8
  profile-vector consistency check against `object$scores`. **Review-fixed
  correctness bug:** the fallback's profile recomputation skipped the up-front
  listwise `na.omit` that ssm_analyze() applies before the C++ estimator, so
  under the default `listwise = TRUE` any NA in a scale/measure turned recomputed
  profiles NaN and made the check reject the *correct* data; fixed by omitting
  over scales(+measures)+group before the estimator, and gated the profile pass
  behind `compute_profiles =` so the live path (which already holds the profiles
  as obs_scores) skips the redundant C++ estimator entirely. Tests
  (test-suff_stats.R, +9): field contract on both paths, contrast keyed by real
  groups only, internal profile↔scores consistency, stored-vs-fallback identity,
  no-data + wrong-data errors, and the listwise+NA regression on both paths.
  Suite 1016/1016; check 0/0/0; document() no-diff. Internal only — no NEWS
  (user-facing entry lands with Z1). (R/ssm_analysis.R,
  tests/testthat/test-suff_stats.R [new], MILESTONES.md.)
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
