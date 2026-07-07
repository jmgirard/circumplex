# circumplex — completed milestones (archive)

Finished milestones with their full task lists and running logs, moved here on
completion so MILESTONES.md holds only the active milestone. Newest milestone
first. This is historical record — the authoritative statement of what shipped
is NEWS.md and the git tags; forward direction lives in ROADMAP.md.

---

## M4 — Browne model & SSM CI trustworthiness (v2.0.0) — GitHub-complete 2026-07-07 (folds into v2.0.0; the CRAN review/checklist belongs to that release)

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

### Log

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


---

## M3 — Visualization layer: ggplot2 circumplex extension (v1.3.0) — GitHub-complete 2026-07-03 (bundled into v1.3.0 with M2; CRAN submission held, see ROADMAP)

Source: ROADMAP.md Milestone 3. Turn the internal, single-purpose plotting
code into a public ggplot2 extension so users (and later milestones) can
compose arbitrary layers in circumplex space instead of rebuilding the
circular canvas from scratch. Sequenced before the fit-statistics/SEM
milestones, whose visualizations should build on it.

Scope decision (2026-07-02, with Jeff): **full extension** as ROADMAP specifies
— exported canvas constructor, custom ggproto geoms/stats, and scale helpers —
not the lighter "public canvas + ggforce" alternative. Rationale: maximal
composability for the M4+ visualizations that will depend on this layer.

Per ROADMAP.md's CRAN release strategy, M3 is bundled with the (already
GitHub-complete) M2 into a single v1.3.0 CRAN submission. Keep both on GitHub
until M3 is done, then run `/release-checklist` once.

Cross-cutting guardrails for every task below:
- **Behavior of the three public `ssm_plot_*()` functions must not change**
  until the explicit refactor task (V4); their vdiffr snapshots in
  `tests/testthat/_snaps/ssm_plot/` are the regression pins — any snapshot
  delta must be justified as an intended rendering change, not accepted blindly.
- **Dependency policy** (DESIGN.md): new user-facing API is base R + ggplot2;
  keep `ggforce` only where it genuinely simplifies arcs/circles. No tidyverse.
- Everything exported gets roxygen with a runnable `@examples` block and enters
  `_pkgdown`/reference cleanly (`devtools::document()` no-diff after).

### Tasks

- [x] **V1. Public circular canvas.** Promote `circle_base()`
  (`R/ssm_plot.R:469`) to an exported, documented API: a `ggcircumplex()`
  constructor and/or `annotation_circumplex()` (rings, spokes, scale labels,
  amplitude gridlines), with instrument-aware labeling from
  `circumplex_instrument` objects.
  *Accept:* exported + documented; a call reproducing the current
  `circle_base(angles, amax, labels)` output is vdiffr-identical to a
  snapshot of today's canvas (or the delta is justified); instrument input
  auto-labels angles from the instrument's scales; invalid input errors via
  the `is_*()` helpers.
- [x] **V2. Polar-native geoms/stats (ggproto).** `geom_ssm_point()` /
  `geom_ssm_arc()` (or a unifying `stat_ssm()`) that accept
  amplitude/displacement aesthetics directly and internalize the
  degree→canvas transform (`ggrad()`), amplitude rescaling
  (`* 10/(2*amax)`), and wrap-around arc handling now inline in
  `ssm_plot_circle()` (`R/ssm_plot.R:75-84`).
  *Accept:* a plot built from `ggcircumplex() + geom_ssm_*()` on an
  `ssm_analyze()` result is vdiffr-equivalent to the corresponding
  `ssm_plot_circle()` output (same arcs, points, wrap-around at the 0/360
  boundary); boundary case — a profile arc spanning the 0/360 seam renders as
  one contiguous arc; degenerate/NA-displacement rows are dropped or handled
  without error.
- [x] **V3. Scales.** `scale_*_circumplex()` helpers for angle-labeled axes
  and amplitude gridlines, with defaults matching the current appearance.
  *Accept:* helpers produce the current tick/label placement on both the
  circular canvas and the curve plot's angle axis; custom `angle_labels`
  and instrument labels flow through; documented with examples.
  *Scope refined during implementation (2026-07-02):* only the curve plot's
  linear angle axis is a genuine ggplot scale (`scale_x_circumplex()`). The
  circular canvas's angle labels and amplitude rings are drawn geometry under
  `theme_void()`, owned by `ggcircumplex()` (V1) — not ggplot scale breaks — so
  they are NOT re-expressed as `scale_*` (that abstraction fits poorly over
  drawn geometry and would jeopardize V4's snapshot stability). Consistency
  across the two contexts is instead guaranteed by a shared internal
  `resolve_circumplex_labels()` used by both `scale_x_circumplex()` and
  `ggcircumplex()`, so identical angle/label/instrument inputs yield matching
  labels on the axis and the canvas (asserted in tests). No `scale_y_*` shipped
  (no linear circumplex plot has an amplitude axis; would be speculative API).
- [x] **V4. Refactor existing plots onto the extension.** Reimplement
  `ssm_plot_circle()`, `ssm_plot_curve()`, `ssm_plot_contrast()` on top of
  V1–V3 with **behavior unchanged**.
  *Accept:* every existing vdiffr snapshot in
  `tests/testthat/_snaps/ssm_plot/` stays byte-identical, or each change is
  individually justified and re-approved; `chkDots()`/argument surfaces of the
  three functions are preserved; full suite green.
- [x] **V5. Vignette: "Advanced Circumplex Visualization."** The third
  vignette, already announced as "still in progress" in the intermediate
  vignette (`vignettes/intermediate-ssm-analysis.Rmd:276`). Demonstrate
  composing raw data, SSM results, and annotations via the new extension.
  *Accept:* builds clean; uses only exported API; teaching prose meets the
  statistical-precision bar (CLAUDE.md — e.g., never describe an angular CI
  excluding 0° as a significance test); intermediate vignette's "in progress"
  note updated to point at it.
- [x] **V6. Design review vs. ggplot2 extension best practices.** Check
  ggproto lifecycle, `after_stat()`/`after_scale()` usage, theme integration,
  and the `ggforce` dependency decision (keep iff it simplifies arcs).
  *Accept:* a short written verdict appended to DESIGN.md (a "Visualization
  extension" subsection) recording the ggproto/scale architecture and the
  ggforce keep/drop decision with rationale.

### Pre-release review fixes (v1.3.0 bundle)

From a `/code-review high` pass over `v1.2.0..HEAD` (2026-07-03, Fable finders +
verifiers). 10 findings survived verification (9 CONFIRMED, 1 PLAUSIBLE); the
statistical core — Monte Carlo covariance math, bootstrap named-column refactor,
`ssm_param_names()`↔C++ ordering, circular-quantile/branch machinery — verified
clean. All correctness findings sit at the new/changed **public API surface**
introduced by M2+M3, so they must be fixed (with regression tests) before the
v1.3.0 CRAN submission. Each fix ships with a test reproducing the reviewer's
executed failure.

Correctness (fix first):

- [x] **R1. `ssm_score()` silently drops unnamed `...` args** — v1.2.0 forwarded
  them positionally to `ssm_parameters()` (a bare `"IIP_"` filled `prefix`); the
  vectorized path inspects only `names(extra_args)` + `modifyList()`, discarding
  unnamed elements with no error. (`R/ssm_analysis.R:689`)
  *Accept:* the v1.2.0 call form errors or works as before (decision below), never
  silently returns unprefixed columns; regression test at the executed example.
- [x] **R4. `ssm_score()` lost scalar validation on `prefix`/`suffix`/labels** —
  non-scalar/non-character values recycle through `paste0()` into interleaved
  garbage column names instead of erroring via `is_char(x, n = 1)`.
  (`R/ssm_analysis.R:715`) *Accept:* `prefix = c("a_","b_")` errors again; test added.
  (R1+R4 are one root cause — the refactor's `...` handling — fix together.)
- [x] **R3. `ggcircumplex(amin=)` mislabels the amplitude axis** — `amin`
  relabels rings on an `amin..amax` scale while the geoms map amplitude as
  `a*5/amax` (amin=0), so any nonzero `amin` silently misplaces every point/arc;
  `amin > amax` unvalidated. New/exported this cycle (unreachable in v1.2.0).
  (`R/ssm_plot.R:510`, `R/geom_ssm.R:11`) *Accept:* per the decision below —
  geoms honor `amin`, or `amin` is gated/removed — plus a test that the labeled
  ring radius and a same-amplitude point's radius agree.
- [x] **R2. Flat / zero-amplitude profiles silently vanish from
  `ssm_plot_circle()`** — geoms drop NA-displacement rows with no message;
  v1.2.0 drew the point at the canvas origin with a "Removed N rows" warning.
  (`R/geom_ssm.R:62`) *Accept:* per the display decision below; a flat-profile
  test asserts the chosen behavior (origin point and/or explicit message).
- [x] **R5. `ggcircumplex()` rounds angles to integers** —
  `as.integer(round(resolved$angles))` shifts fractional angles (round-half-even)
  on the canvas while geoms/scale use exact values, so 22.5° spokes draw at 22°.
  (`R/ssm_plot.R:508`) *Accept:* fractional-angle canvas places spokes/labels at
  exact angles; test on `seq(22.5, 337.5, 45)` (16-scale); ssm_plot snapshots stay
  byte-identical (details$angles are integer octants, so no snapshot delta).
- [x] **R6. `scale_x_circumplex()` rounds fractional angle labels** —
  `sprintf("%.0f°", x)` labels 22.5→"22°", 67.5→"68°" while breaks sit at exact
  angles. (`R/scale_circumplex.R:55`) *Accept:* fractional angles label without
  misleading rounding (format decision with R5); test added. (R5+R6 share the
  rounding root cause — fix together.)

Cleanup (fold in where the bug fixes already touch the file; no behavior change):

- [x] **C2. Duplicated engine-dispatch block** — the
  `if (method=="montecarlo") ssm_montecarlo() else ssm_bootstrap()` branch is
  near-verbatim in `ssm_analyze_means()` and `ssm_analyze_corrs()`
  (`R/ssm_analysis.R:348,486`); extract one dispatcher (method already validated
  once in `ssm_analyze()`). *Accept:* single helper, both callers routed, all
  seeded pins byte-identical.
- [x] **C4. Monte Carlo recomputes `t0` scores** — `ssm_montecarlo()` re-runs
  `mean_scores()`/`corr_scores()` on the same unmutated `bs_input` the caller
  already computed (`R/ssm_montecarlo.R:43`); pass the caller's matrix in.
  *Accept:* one fewer O(n·p·q) pass; results byte-identical.
- [x] **C3. Repel-branch trig duplication** — `ssm_plot_circle()`'s repel branch
  hand-recomputes the polar→canvas transform duplicated in
  `GeomSsmPoint$setup_data()` (`R/ssm_plot.R:192` vs `R/geom_ssm.R:64`); extract a
  shared `ssm_to_cartesian()` helper. *Accept:* both sites call it; snapshots
  byte-identical. (Natural companion to R2/R3, same files.)
- [x] **C1. Monte Carlo contrast re-derives `param_diff()`** — the MC replicate
  contrast inlines "second minus first, disp via `angle_dist`" (`R/ssm_montecarlo.R:110`)
  that `param_diff()` owns for the bootstrap path. PLAUSIBLE, not a bug: fix by
  generalizing `param_diff()` to an R×6 matrix (named via `ssm_param_names()`) and
  calling it in both places. *Touches the contrast convention → run
  `/statistical-validation` and boundary tests (±180°) after.* *Accept:* MC and
  bootstrap contrasts share one convention implementation; boundary tests green;
  seeded pins unchanged.

### Estimator-core audit findings (Brief C, Fable — fix before v1.3.0)

From the Brief C independent estimator audit (2026-07-03; full report
`devel/estimator-audit-2026-07-fable.md`). These are **pre-existing** (present in
v1.2.0 on CRAN), not M2/M3 regressions, but decision (Jeff 2026-07-03): fix F1–F3
before the v1.3.0 submission rather than shipping again over a reachable crash and
a violated angular invariant. F4–F6 (nits) were addressed as a follow-up cleanup
(2026-07-03), except the F6 0-vs-360 pole-snap *alignment* decision, which is a
convention call parked for Jeff (cosmetic; every consumer handles the wrap). Each
fix is test-first with a regression test reproducing the audit's executed failure.

- [x] **F1. `col_means()` crashes on an all-NA resampled column** — under
  `listwise = FALSE`, a bootstrap resample can draw a scale column that is
  entirely NA; `col_means()` in `src/parameters.cpp` calls `mean()` on the empty
  post-`na.omit` vector and aborts (`mean(): object has no elements`), where
  `pairwise_r()` already guards this. Deterministic repro at seed 123 (6-row,
  4/6-missing scale). *Touches `src/` → rebuild C++, run the boundary suite +
  `/statistical-validation`.* *Accept:* the repro returns NA (or the documented
  degenerate handling) instead of aborting; regression test added.
- [x] **F3. `angle_dist()` range is [−180, 180), not (−180, 180]** — exactly
  opposed profiles yield a contrast `d_est` of exactly −180 rather than +180,
  violating the CLAUDE.md invariant (contrasts reported in (−180°, 180°]) and an
  existing test's asserted contract. Contrast-convention/danger-zone change.
  *Touches the contrast convention → `/statistical-validation` + ±180° boundary
  tests after; recommend a Fable review of the fix.* *Accept:* opposed-profile
  contrasts report +180 on the documented branch; boundary tests green; seeded
  pins reconciled (any snapshot delta justified).
- [x] **F2. Model-fit caveat is under-documented and fit can leave [0,1]** — with
  unequally spaced angles the closed-form fit (R²) is unbounded below (−107
  observed); the equal-spacing/Gurtman caveat lives only on `ssm_analyze()`.
  Doc-only. *Accept:* the caveat (equal-spacing assumption; fit may be negative
  and is not a bounded R² off equal spacing) is stated on `ssm_parameters()` and
  `ssm_score()` too; no math change; no snapshot delta.

## Log

- 2026-07-03 — Brief E: M5/M6 statistical design-questions memo (background Fable
  subagent, Opus-reviewed). Wrote `devel/m5-m6-design-questions.md` — questions +
  recommended directions only (not specs). Highlights: M5 CI construction must not
  delegate a/d intervals to lavaan (delta method exact for e/x/y, but Rice/boundary
  for amplitude and 1/a² Jacobian + branch-blindness for displacement) — route
  (e,x,y) draws through the existing circular-quantile pipeline instead; the
  invariance-constrained multi-group contrast is a *different estimand*
  (disattenuated + conditional on invariance), ship as a separate named workflow.
  M6: model the Cartesian (x,y) trajectory bivariately to sidestep the 0/360
  boundary; person-level case (cluster) bootstrap for paired timepoint contrasts;
  Bayes only for hierarchical/intraindividual pooling, in-package footprint limited
  to a posterior-draws adapter + vignette (Stan in a companion if ever). Three
  questions flagged [blocked on M4] (coverage of cheap MVN propagation, hybrid
  estimated-angle workflows, displacement-trajectory usability — all need the M4
  ssm_ci_accuracy harness / cpm_fit). Reviewed clean (sound math, real citations,
  no fabricated figures). Closes the 2026-07 Fable-window brief queue (A, B+review+
  revision, C, D, E). Design only; nothing else touched. (devel/m5-m6-design-questions.md,
  MILESTONES.md).
- 2026-07-03 — `/code-review max` over v1.2.0..HEAD (the v1.3.0 bundle) + the one
  finding acted on (Opus orchestration: 6 finder angles → verify → sweep). Result:
  **no confirmed wrong-number correctness bugs.** Three high-looking candidates
  were REFUTED under runtime verification: MC contrast wrong-pair (pairing correct
  for every contrast shape ssm_analyze permits), MC all-NA CI on a constant scale
  (cov()=0 not NA; mvn_draws is PSD-tolerant), MC nonsense CI at n=2 (the |r|>=1
  guard errors first with a "use bootstrap" message). 10 low-severity findings
  reported (plot NA-filter/exported-geom robustness, Monte Carlo engine efficiency,
  minor cleanup, one doc nuance) — recorded in the ROADMAP continuous/infra track,
  to be folded in when the relevant code is next touched (mostly M4). Acted on now:
  the C++ `group_parameters()` fixed 6-wide stride was tied to `ssm_param_names()`
  only by the literal 6, with no assertion — a future 7th-parameter edit on one
  side would silently misalign every parameter column. Added a test pinning
  `length(ssm_parameters_cpp) == length(ssm_param_names())` and the
  `group_parameters` width (fails at check on any desync), a defensive `stopifnot`
  in `reshape_params`, and a cross-reference comment at the C++ stride. Suite
  589/589; check 0/0/0. No NEWS (internal guard; no user-facing behavior change).
  (src/parameters.cpp, R/utils.R, tests/testthat/test-RcppExport.R.R, MILESTONES.md).
- 2026-07-03 — Brief B-revision: revised the `ssm_ci_accuracy()` spec against the
  B-review findings (FRESH Fable session — not the B author, not the reviewer).
  Resolved the review's F1–F9 with a per-finding revision log (fixed / rejected-
  with-reason). Notably: adopted the *shipped* guardrail rule verbatim
  (`round(a_lci, 3) <= 0`, R/ssm_oop.R:159) and redefined the contrast module on
  the row-amplitude ladder (the regime where the branch pathology actually
  occurs). **No new A-side interface gaps** — G1–G4 stand as previously flagged;
  F6 was B's own omission (A's signature already carries `scales`/`angles`). One
  NEW package-side decision surfaced for Jeff (§12.5 / F1.ii): the shipped
  guardrail's certification threshold is a display-precision artifact (~0.0005
  amplitude units at default digits, moving with a `print` argument) — keep the
  display-coupled rule and just measure it, or give `print.circumplex_ssm()` a
  principled print-independent rule. Deferred to a `print.circumplex_ssm()`
  decision, outside B's scope. Revision only; nothing committed by the session.
  Tier for the eventual build (future M4, once §12 decisions settle): Opus against
  the spec, with the §4.3 guardrail-measurement module + §10 oracles reviewed by
  Fable (the remaining plausible-but-wrong spots). (devel/m4-ci-accuracy-spec.md,
  devel/fable-briefs-2026-07.md, MILESTONES.md).
- 2026-07-03 — Brief B-review: adversarial review of the ssm_ci_accuracy()
  spec (FRESH Fable session, no involvement in A/B/A-review). Report:
  `devel/m4-ci-accuracy-spec-review.md`. Verdict: NEEDS CHANGE (F1–F3 before
  implementation; F4–F9 cheap). Z&W number hygiene — the brief's top suspect
  — came up CLEAN: every Z&W value TBT, illustrative numbers labeled, all
  fixed numbers traced to shipped code/citations/arithmetic. A↔B contract
  clean: every consumed field exists in A §5.4; gaps G1–G4 are genuine A
  gaps, correctly flagged. Required changes: (1) HIGH — the certification
  event "amplitude lci > 0" is degenerate (percentile lower bound of a
  strictly positive statistic is > 0 a.s., so false-certification ≡ 1 at
  a₀=0 and the §4.3 power curve ≡ 1) AND differs from the shipped guardrail,
  which is round(a_lci, digits=3) <= 0 (R/ssm_oop.R:159) — a scale- and
  display-precision-dependent threshold; the "nominal α/2" duality also
  fails at the boundary. Spec must adopt the shipped rule verbatim and
  surface the guardrail-threshold question to Jeff as a package decision.
  (2) HIGH — the contrast ladder (profiles converge, rows stay realistic)
  targets a regime where the branch pathology cannot occur: verified by
  seeded simulation against the package — spec's regime gives a 14° contrast-d
  CI; the actual pathology regime is ROW amplitude ≈ 0 vs sampling noise
  (326° CI). Redefine the contrast module on the row ladder. (3) MED — the
  ladder's truth claims (a₀=0 at c=0, a₀=c·â) hold only for equally spaced
  angles; fix via the estimator functional (2×2 solve) or per-rung truths.
  Plus: c=0 amplitude coverage is a theorem (≡0), not a measurement (F4);
  Wilson level unpinned (F5); pinned cpm_fit() call omits scales/angles
  (F6); Z&W-reproduction gate assumes MVN-reproducible generating process
  (F7); (a)-vs-(b) rationale leans on remembered qualitative Z&W properties
  — re-confirm at transcription (F8); multi-row ladder under-specified (F9).
  Review only — spec substance untouched, nothing committed. Next: Fable
  revision pass on the spec (F1.ii changes what the shipped guardrail
  means; estimator-adjacent decision-rule design), Sonnet for F5/F6/F9 if
  split out. (devel/m4-ci-accuracy-spec-review.md, MILESTONES.md).
- 2026-07-03 — Brief B: `ssm_ci_accuracy()` CI-trustworthiness spec (FRESH
  Fable session, per the brief's context-hygiene rule; builds on the
  committed Brief A design, decisions taken as given). Wrote
  `devel/m4-ci-accuracy-spec.md`. Core design: a plug-in coverage simulation
  — one `cpm_fit()` on the pooled within-group scale correlations defines
  the population (structure = P̂), then reps×boots replays of the user's own
  CI procedure (same engine/boots/interval) at the user's exact n tally
  empirical coverage per parameter, with angular-membership coverage for
  displacement (0/360-safe by construction) and certification-conditional
  displacement coverage as the verdict-driving estimand. Amplitude-near-zero
  module (the absorbed-M2 target): a first-harmonic-only amplitude ladder
  c ∈ {1,.5,.25,0} holding residual harmonics fixed, measuring one-sided
  amplitude-CI miss decomposition, guardrail false-certification rate at
  a₀=0 vs nominal α/2, power up the ladder, and the contrast branch-pathology
  frequency (ROADMAP F3-review note folded in). Verdicts classified against
  Bradley's (1978) liberal band via Wilson intervals. **Central decision
  surfaced for Jeff and DECIDED same day (spec §2/§13): simulation only in
  code, with Z&W Studies 1–5 content as transcribed vignette context rather
  than a nearest-condition lookup** (their grid is coarse; mapping quietly
  becomes extrapolation; every hard ROADMAP requirement needs the
  simulation; spec §6 retained as the requirements record if a lookup-lite
  is ever revisited).
  All Z&W numerics marked TBT under the oracle rule — none reproduced from
  memory. A↔B contract pinned to A §5.4 fields with four flagged gaps
  feeding back to A (G1 cpm_simulate return contract unspecified; G2 no
  augmented scales+measures simulation path — cpm_simulate suffices for the
  mean-based path only, proposed fix reduces the corr-path contract to
  matrices$Phat; G3 dimnames unpinned; G4 A §8's "Brief-B ≥10⁴ refits"
  Phase-2 trigger mis-anticipates B, which refits the CPM zero times) plus
  one gap on the ssm side: `circumplex_ssm$details` stores no per-group n,
  scale SDs, or correlation matrices, so a companion `ssm_analyze()`
  sufficient-statistics storage change is a prerequisite task (with a
  `data =` fallback + consistency check for old objects). Design only — no
  package code. Remaining §12 open items (amplitude ladder default, naming,
  per-group structure, CPM-CI assessment method) can wait for
  implementation. Next: Brief E (M5/M6 design questions, Fable, time-boxed)
  closes the window queue.
  (devel/m4-ci-accuracy-spec.md [new], MILESTONES.md).
- 2026-07-03 — RNG-contract restatement in DESIGN.md (Fable, follow-up to the
  A-review integration; Jeff asked for it now rather than at M4 ship time).
  Replaced the frozen-inventory sentence ("ssm_analyze() is the package's
  only entry point that consumes R's global RNG stream") with the invariant
  it stood for — a function consumes the global stream iff its statistical
  output is stochastic (resampling/simulation); such entry points document it
  and follow the set.seed() convention; internals (multi-start jitter,
  tie-breaks) must be deterministic and leave .Random.seed untouched — plus
  an enumerated entry-point list (currently one: ssm_analyze(), with the
  existing per-engine table beneath it) noting M4's planned additions.
  ssm_analyze()'s roxygen "only function" sentence is deliberately untouched
  (true today; updating it is queued in devel/m4-browne-design.md §8 for ship
  time to avoid man/-churn). Doc-only, internal memory file — no package
  code, no check needed. (DESIGN.md, devel/m4-browne-design.md §8,
  MILESTONES.md).
- 2026-07-03 — Brief A-review integration (Fable, same session as Brief A).
  All A-review findings F1–F10 integrated into `devel/m4-browne-design.md`
  (change log §11 added to the doc): simulation coverage oracle +
  T-calibration added as required §6.4 validation (the test that separates
  "matches CIRCUM" from "actually covers"); **default ci_method decided with
  Jeff: bootstrap on the raw-data path, analytic only on the cormat path
  with an N-conditional summary() caution** (F1); convergence acceptance
  respecified on a scaled gradient norm with the nlminb code advisory-only
  (F2); reflection canonicalization now toward the theoretical configuration,
  CCW rule demoted to tie-break (F3); multi-start jitter made deterministic —
  default cpm_fit() path is RNG-silent, pinned by a planned .Random.seed
  test (F4); RMSEA CI λ_U=0 guard (F5); SRMR/CI-shape/BIC conventions pinned
  (F6); m-cap justification corrected, floor(p/2) allowed for fixed-angle
  variants (F7); tolerance/gradient-test criteria fixed (F8); harmonic-removal
  polish retriggered at 1e-2 with χ²-mixture rationale note (F9);
  per-replicate mirror guard for the warm-started bootstrap + equal-F̂
  multimodality flag (F10). Also queued a ship-time DESIGN.md task: restate
  the "only ssm_analyze() consumes the RNG" contract as the principle
  (stochastic-output functions only; internals deterministic) with an
  entry-point table, since cpm_fit(bootstrap)/cpm_simulate() will make the
  current sentence false. Design docs only — no package code. Next: Brief B
  (Z&W ssm_ci_accuracy spec), which now also owns CPM analytic-CI
  trustworthiness per F1. (devel/m4-browne-design.md, MILESTONES.md).
- 2026-07-03 — Brief A-review: adversarial review of the Browne design (FRESH
  Fable subagent, no memory of writing the doc). Report:
  `devel/m4-browne-design-review.md`. Verdict: NEEDS CHANGES before
  implementation, but the core is computationally verified correct — analytic
  gradient incl. logit/softmax chains vs finite differences (25 random points,
  max rel err 1.9e-6), df table (10/17/17/24 at p=8,m=3), F₀=−ln|R|, exact
  reflection invariance, exact-recovery round trip (1e-10), the §3.2
  scale-invariance identity (exact), large-N CI calibration. Backend decision
  (native, R-first) CONFIRMED with direct evidence (~100 lines of base R,
  sub-second fits). Required changes: (1) HIGH — analytic Hessian CIs
  mis-cover at field-typical N (ζ coverage 66–86% at N=500 with a small third
  harmonic; over-covers with all-interior β; exact only by N~50k) and the
  CIRCUM-CI gate cannot detect it since CIRCUM shares the asymptotics → add a
  simulation-based coverage oracle to §6.4 and revisit the analytic-default
  decision; (2) HIGH/easy — nlminb "singular convergence (7)" is the normal
  exit for 65–96% of demonstrably good fits at the doc's tolerances →
  acceptance must key on scaled gradient norm, code advisory only, else the
  bootstrap discards most replicates; (3) MED — the CCW canonicalization rule
  mirrors theory for clockwise-keyed instruments → canonicalize toward the
  theoretical configuration, CCW as tie-break; (4) MED — multi-start jitter
  must not consume the global RNG on the default path (DESIGN.md contract);
  RMSEA CI missing the λ_U=0 branch (uniroot errors on very good fits); SRMR
  denominator convention unpinned (~12% at p=8, breaks CircE validation);
  minor: even-p Nyquist harmonic is identified (m-cap merely conservative),
  RMSEA tolerance looser (0.005), gradient-test tolerance flaky as specced,
  per-replicate mirror guard for warm-started bootstrap. Review only — no
  package code, design doc untouched. Next: integrate revisions into
  devel/m4-browne-design.md. (devel/m4-browne-design-review.md, MILESTONES.md).
- 2026-07-03 — Brief A: M4 Browne-model estimation design (Fable, Jeff-steered).
  Wrote `devel/m4-browne-design.md`, the CircE-replacement design doc for M4's
  anchor feature: the model (Fourier correlation function with the Herglotz
  β≥0 / Σβ=1 constraints, communality index, factor representation), ML
  discrepancy with unconstrained reparameterization (logit ζ, softmax β,
  angles free in ℝ, wrap at report only), analytic gradients + mandatory
  finite-difference gradient test, identification (reference-angle fix,
  reflection canonicalization, β-boundary polish with df reduction, Heywood
  flags), df table for the four model variants, fit indices (χ², RMSEA+CI,
  SRMR, CFI/TLI, AIC/BIC) defined from the discrepancy, `cpm_fit()` API
  sketch (`circumplex_cpm`, print/summary/plot on the M3 extension,
  `cpm_simulate()` as the Brief-B contract), and the validation strategy:
  published CIRCUM/CircE oracles as blank transcription templates (no
  expected values from memory or local files — g2xx1.txt explicitly banned)
  plus internal oracles (exact-recovery round trip, circulant DFT check,
  OpenMx/lavaan in Suggests as cross-implementation test oracles). Backend
  DECIDED with Jeff: native optimization, R-first (nlminb), C++ port gated on
  profiling with R kept as permanent oracle. Design only — no package code.
  Next: Brief A-review in a fresh Fable session. (devel/m4-browne-design.md,
  MILESTONES.md).
- 2026-07-03 — F4–F6 nit cleanup (Sonnet, propose-not-commit; Opus review + commit).
  F4 (wording only, no behavior change): the degenerate-resample warning
  (`R/ssm_bootstrap.R`) and the DESIGN.md degenerate-profiles row overstated
  exclusion as per-replicate; reworded to per-parameter (only the undefined
  displacement/fit is dropped, via `na.rm` per column; well-defined params still
  enter their CIs — per-row exclusion would bias the near-zero amplitude CI). F5
  (`src/circular.cpp` `angle_median()`, test-first): returned 0 for all-NA/empty
  input due to a default-initialized `{0.0}` candidate; added an `n == 0 ->
  NA_REAL` guard and changed `candidates(1)` to `candidates(0)`; regression test
  in test-RcppExport.R.R (both inputs -> NA). F6 (DESIGN.md factual correction
  only): the pole displacement is *exactly* 360.0 (modu fmod-at-edge), not
  "≈359.9999". Deliberately NOT done: the F6 0-vs-360 snap *alignment* (convention
  decision parked for Jeff) — snap direction, test pin, and quantile code
  untouched. Suite 587/587; check 0/0/0. No NEWS (F5 internal/non-exported; F4 a
  warning-wording precision fix; F6 internal doc). (src/circular.cpp,
  R/ssm_bootstrap.R, DESIGN.md, tests/testthat/test-RcppExport.R.R, MILESTONES.md).
- 2026-07-03 — F3 fix: `angle_dist()` ±180° branch (Fable tests + review, Opus
  fix, test-first). The plain wrap `((x-y+π) %% 2π) - π` has range [−π, π), so an
  exact half-turn (exactly opposed profiles) reported −180 instead of the
  documented (−180, 180] +180, violating the CLAUDE.md invariant. Fix: remap the
  bit-exact −π atom to +π (`d[!is.na(d) & d == -pi] <- pi`) — minimal, no tolerance
  band, byte-identical off the atom (so no seeded-pin drift). Process: the F3
  implementer subagent (Fable) wrote the tests then hit its session limit before
  the fix; Opus completed the one-liner by inferring intent from the tests and
  verifying (independent complex-Arg oracle to 5e-15; suite 585/585). A FRESH
  Fable adversarial review then attacked it (~440k half-turn constructions,
  25-seed×2-engine sweep) and returned CLEAN — with one accuracy correction now
  applied: the pipeline's `modu()` wrap ([0,2π)) leaves ~16% of true half-turns
  1–2 ulp off the atom, landing harmlessly just inside the branch (never rounding
  to −180), so the "bit-exact" comment was softened. Fixed-oracle branch-alignment
  test untouched. Pre-existing out-of-scope observation surfaced (recorded
  separately): in a degenerate zero-amplitude-contrast MC regime the estimate can
  sit geometrically outside its wide CI (fix strictly improves the pre-fix case).
  ALL F1–F3 pre-release fixes now done. (R/utils.R, tests/testthat/test-utils.R,
  test-ssm_bootstrap.R, test-ssm_montecarlo.R, NEWS.md, MILESTONES.md).
- 2026-07-03 — F2 fix: model-fit caveat documentation (Opus, doc-only). The
  closed-form estimator is the OLS projection (fit ∈ [0,1]) only for equally
  spaced angles; off equal spacing it is the Gurtman estimator and the reported
  fit can go negative (audit observed −107 through the full API). The
  equal-spacing caveat previously lived only on `ssm_analyze()`'s `@param angles`
  and never stated the fit-bounds consequence. Mirrored the caveat onto
  `ssm_parameters()` and `ssm_score()` `@param angles`, added the "not a bounded
  R² in [0,1]; can fall below 0" clause to all three, and noted it on
  `ssm_parameters()`'s `f_label`. No math change; no snapshot delta. `document()`
  regenerated only ssm_analyze/ssm_parameters/ssm_score .Rd; check 0/0/0. NEWS.md
  bullet added. (R/ssm_analysis.R, man/ssm_analyze.Rd, man/ssm_parameters.Rd,
  man/ssm_score.Rd, NEWS.md, MILESTONES.md).
- 2026-07-03 — F1 fix: `col_means()` all-NA-column crash (Opus, test-first). Under
  `listwise = FALSE` a bootstrap resample can leave a scale column with no finite
  values; `arma::mean()` on the empty post-`find_finite` vector aborted the whole
  `ssm_analyze()` call (`mean(): object has no elements`). Guarded `col_means()`
  in `src/parameters.cpp` to return `NA_REAL` for a zero-finite-element column,
  mirroring `pairwise_r()`'s guard — the resample then degrades to a degenerate
  profile absorbed by the existing exclusion + warning. Test-first: a tight unit
  pin (`col_means` on an all-NA column → NA) and the audit's exact integration
  repro (seed 123, 4/6-missing scale) — both failed pre-fix, pass post-fix.
  `/statistical-validation` run: estimator math provably unchanged (col_means vs
  colMeans to 4e-16 over 200 NA matrices; mean_scores PWD vs manual 1-/2-group;
  SSM params vs lm() OLS to 1e-13; end-to-end ssm_analyze == ssm_parameters).
  Suite 555/555, 0 warnings. NEWS.md bullet added. (src/parameters.cpp,
  tests/testthat/test-RcppExport.R.R, test-ssm_bootstrap.R, NEWS.md, MILESTONES.md).
- 2026-07-03 — Brief C estimator/angular-core audit (Fable). Wrote
  `devel/estimator-audit-2026-07-fable.md`: 6 findings, none critical — F1
  reachable crash (`col_means()` on an all-NA resampled column under
  `listwise = FALSE`, `mean(): object has no elements`), F2 fit statistic
  unbounded below (−107 observed) with unequally spaced angles and the
  Gurtman-vs-OLS caveat documented only at `ssm_analyze()`, F3 `angle_dist()`
  range is [−180, 180) not the documented (−180, 180] (exact −180 reachable
  via sign-flipped groups), F4–F6 wording/consistency nits. Verified clean:
  scale-aware tolerance across 0.1–1e6 scales, degenerate taxonomy, circular
  CI machinery incl. 0/360 straddles and ±180 branch alignment, full Monte
  Carlo covariance derivation (Hampel IF, Fisher-z delta) + empirical
  bootstrap agreement on skewed data. Audit only — no package code touched.
  Triage (Jeff): F1–F3 are now v1.3.0 pre-release fixes (added as tasks above,
  test-first; F1 touches `src/` and F3 the contrast convention → both get
  `/statistical-validation`); F4–F6 nits deferred. (devel/estimator-audit-2026-07-fable.md,
  MILESTONES.md, ROADMAP.md unchanged).
- 2026-07-03 — Pre-release review fixes R3 + C1–C4 (Opus). R3 (decision Jeff):
  removed the `amin` argument from the exported `ggcircumplex()` (kept
  `circle_base()`'s internal `amin = 0`), since it relabelled the rings on an
  `amin..amax` scale the geoms never honored; recorded in DESIGN.md that a
  configurable amplitude center belongs in the future radial scale/coord (the
  same deferred CoordCircumplex as the `amax`-per-layer trade-off), not the
  constructor. Test asserts `amin` is now rejected and rings are 0-centered.
  Cleanups (all behavior-preserving, seeded pins byte-identical): C2 extracted
  `ssm_estimate_intervals()` so both analysis paths share one engine dispatcher
  (method already validated once in `ssm_analyze()`); C4 passes the caller's
  observed score matrix into `ssm_montecarlo()` via a new `obs_scores` param,
  dropping a duplicate `mean_scores()`/`corr_scores()` pass; C3 extracted
  `ssm_to_cartesian()` shared by `GeomSsmPoint$setup_data()` and
  `ssm_plot_circle()`'s repel branch; C1 generalized `param_diff()` to an R×6
  matrix (named displacement column via `ssm_param_names()`, dropping the magic
  `[[5]]`) and routed the Monte Carlo replicate contrast through it, so bootstrap
  and MC share one contrast-convention implementation. C1 touches the contrast
  convention → ran independent statistical-validation of the angular contrast
  path (angle_dist vs a hand-rolled signed-distance reference, the skill's named
  boundary cases, and param_diff's matrix displacement near ±180°): all agree to
  ~1e-14. Suite 550/550, 0 warnings; `devtools::check()` run. No NEWS (C1–C4
  internal; `amin` never shipped to CRAN). ALL 10 review findings resolved.
  (R/ssm_analysis.R, R/ssm_bootstrap.R, R/ssm_montecarlo.R, R/ssm_plot.R,
  R/geom_ssm.R, R/utils.R, man/ggcircumplex.Rd, DESIGN.md, tests/testthat/
  test-ssm_plot.R, test-ssm_bootstrap.R, MILESTONES.md).
- 2026-07-03 — Pre-release review fixes R1/R4/R5/R6/R2 (Opus, test-first). Five
  API-surface correctness bugs from the `/code-review high` pass, each with a
  regression test that failed on the pre-fix code. R1+R4 (`R/ssm_analysis.R`
  `ssm_score()`): unnamed `...` args now error (was: silently dropped, losing a
  column prefix vs v1.2.0) and each label/prefix/suffix is re-validated as a
  length-1 string via `is_char()` (was: vectors recycled into garbled column
  names). Decision (Jeff): error on unnamed rather than restore v1.2.0's fragile
  positional forwarding. R5+R6 (`R/scale_circumplex.R`, `R/ssm_plot.R`): extracted
  a shared `circumplex_degree_labels()` and routed both the canvas
  (`ggcircumplex()`/`circle_base()`) and the axis (`scale_x_circumplex()`) through
  it; removed the `as.integer(round())` coercion in `ggcircumplex()` AND the twin
  at `ssm_plot_circle()`'s internal angle line, so fractional angles (e.g. 22.5°)
  render exactly instead of rounding to 22°. R2 (`R/ssm_plot.R`): `ssm_plot_circle()`
  now detects undefined-displacement profiles (flat/zero-amplitude, `d_est = NA`)
  up front, warns naming them, and removes them cleanly (decision (Jeff): drop +
  named warning, not v1.2.0's origin point) — also fixes the messy NA-row the
  low-fit filter otherwise produced. No estimator math touched → no
  `/statistical-validation` needed. All ssm_plot/geom_ssm vdiffr snapshots
  byte-identical (built-in angles are integers). Suite 543/543, 0 warnings.
  NEWS.md bullets added. R3 (amin) pending Jeff's confirm; C1–C4 cleanups next.
  (R/ssm_analysis.R, R/scale_circumplex.R, R/ssm_plot.R, tests/testthat/
  test-ssm_analysis.R, test-scale_circumplex.R, test-ssm_plot.R, NEWS.md,
  MILESTONES.md).
- 2026-07-02 — V6 Extension design review (Opus, doc-only). Audited the V1–V4
  ggproto code against ggplot2 extension idioms; appended a "Visualization
  extension" section to DESIGN.md recording architecture + verdict. Findings:
  (1) after_stat/after_scale correctly unused (the arc Stat feeds GeomArcBar's
  aes directly, ggforce-style; nothing needs post-scale remap). (2) ggforce =
  KEEP (the acceptance's "iff it simplifies arcs"): StatSsmArc inherits its
  annular-wedge polygon tessellation (StatArcBar/arcPaths) instead of
  reimplementing a wrap-aware tessellator, and geom_circle draws the rings;
  already a mature Import. (3) Recorded known trade-offs, each deliberate and
  each risky to "fix" because it would threaten V4 byte-identical snapshots:
  amax is a per-layer param not shared state (idiomatic fix = a CoordCircumplex
  owning amax + the polar transform, deferred); the theme_void canvas doesn't
  respond to themes; na.rm is effectively always TRUE (minor convention
  deviation); the GeomSsmPoint/StatSsmArc generators aren't exported (cheap
  future add for subclassers). No code/test/NEWS change (DESIGN.md is
  .Rbuildignore'd internal memory). Verdict claims verified against code/
  NAMESPACE. M3 COMPLETE — all V1–V6 done. (DESIGN.md, MILESTONES.md).
- 2026-07-02 — V5 Advanced Visualization vignette (Sonnet). New
  vignettes/advanced-visualization.Rmd: builds custom circumplex figures by
  composing the exported extension — bare/instrument-labeled ggcircumplex()
  canvas; geom_ssm_arc()+geom_ssm_point() on an ssm_analyze() result (with the
  amax-must-match rule made explicit); a composed custom layer (per-person
  ssm_score() point cloud behind a group point — the payoff no built-in
  produces); scale_x_circumplex() for a linear angle axis; and a closing note
  that ssm_plot_* are built on these same pieces. Every chunk verified to run;
  render produces no warnings (per-person degenerate row filtered, chunk
  warning=FALSE) and no chunk errors. Statistical-precision pass (CLAUDE.md):
  the arc described as displaying two separate marginal CIs (amplitude radial,
  displacement angular) shown together — explicitly NOT a joint confidence
  region with its own coverage, NOT a significance test; angular extent framed
  as plausible directions since 0 deg is an arbitrary reference, not a null
  (consistent with the D6 intro-vignette fix and DESIGN.md). Intermediate
  vignette's "still in progress" note replaced with a concrete pointer.
  _pkgdown.yml: added the vignette to the navbar AND a new "Visualization Layer"
  reference section for ggcircumplex/geom_ssm_point/geom_ssm_arc/
  scale_x_circumplex (V1–V3 had exported these without listing them — they were
  orphaned on the site and would trip a pkgdown missing-topics warning). Vignette
  ASCII-clean; check builds all vignettes 0/0/0; suite 529/529 (doc-only).
  (vignettes/advanced-visualization.Rmd [new],
  vignettes/intermediate-ssm-analysis.Rmd, _pkgdown.yml, NEWS.md, MILESTONES.md).
- 2026-07-02 — V4 Refactor plots onto the extension (Opus). ssm_plot_circle:
  removed the inline amplitude/displacement→canvas transform (rescale + ggrad +
  0/360 wrap); now circle_base→ggcircumplex (V1), ggforce::geom_arc_bar→
  geom_ssm_arc, geom_point(x_est,y_est)→geom_ssm_point(a_est,d_est) (V2); the
  repel branch recomputes canvas coords from a_est/d_est via ssm_radius (the
  formerly-precomputed x_est/y_est no longer exist post-transform-removal).
  ssm_plot_curve: scale_x_continuous(breaks,labels)→scale_x_circumplex (V3);
  dropped the inline degree-label function (the scale supplies it). All palette/
  vary_shapes/drop_lowfit/guides/theme logic untouched. ssm_plot_contrast: NOT
  refactored — it is a Cartesian faceted point-range difference plot with no
  circular canvas, polar geom, or angle axis, so nothing in V1–V3 applies
  (honest scope call, not an omission). Behavior-preserving: ALL 11 existing
  ssm_plot vdiffr snapshots stayed byte-identical (incl. the stochastic repel
  one and the cross-zero arc) — the V2 geometric-equality proof held at full
  render. -7 net lines; ggrad no longer referenced in ssm_plot.R (still used
  internally by geom_ssm.R); circle_base still reached via ggcircumplex. Suite
  529/529; check 0/0/0. No NEWS (behavior unchanged; the plots are now built on
  the public extension, but nothing user-visible changed). (R/ssm_plot.R,
  MILESTONES.md).
- 2026-07-02 — V3 Scales (Opus): exported `scale_x_circumplex()`, a ggplot2
  continuous position scale for the angle axis of linear circumplex plots (the
  ssm_plot_curve score-by-angle axis). Breaks at the scale angles; default
  labels = degrees (sprintf "%.0f\\U00B0", matching ssm_plot_curve exactly);
  accepts a labels vector or a circumplex_instrument (abbreviations). Extracted
  a shared internal `resolve_circumplex_labels(angles, labels, instrument)` and
  routed BOTH the new scale and `ggcircumplex()` (V1) through it, so identical
  inputs label the linear axis and the circular canvas consistently — asserted
  by a test comparing the scale's get_labels() to the canvas's drawn label
  layer. ggcircumplex refactor verified output-identical (0 snapshot changes).
  Scope call recorded in the V3 task entry: circular-canvas gridlines/labels
  are theme_void drawn geometry (ggcircumplex's job), not ggplot scales, so no
  scale_* is forced over them; no speculative scale_y_*. Non-ASCII degree sign
  written as \\U00B0 per the R/ convention (avoids the R CMD check non-ASCII
  note). Tests via standalone Scale$get_labels() (device-independent) plus a
  build-level check that the curve plot's axis labels match. Suite 529/529;
  check 0/0/0. NEWS.md added. (R/scale_circumplex.R [new], R/ssm_plot.R
  [ggcircumplex refactor], man/scale_x_circumplex.Rd [new], NAMESPACE,
  tests/testthat/test-scale_circumplex.R [new], NEWS.md, MILESTONES.md).
- 2026-07-02 — V2 Polar-native geoms (Opus): exported `geom_ssm_point()` and
  `geom_ssm_arc()`, ggplot2 layers taking amplitude/displacement aesthetics and
  internalizing the polar transform formerly inline in ssm_plot_circle (radius
  = amplitude*5/amax, angle = ggrad(displacement), 0/360 wrap = +360 when
  d_max<d_min). Architecture: `GeomSsmPoint` subclasses GeomPoint and computes
  x/y in setup_data (runs before scale training, so the canvas range picks the
  points up); `StatSsmArc` subclasses `ggforce::StatArcBar`, injecting
  x0/y0/r0/r/start/end in an overridden compute_panel then delegating to the
  parent (ggproto_parent) for the arcPaths polygon expansion — reuses ggforce's
  arc machinery rather than reimplementing it. `amax` is a layer param (ggplot
  can't share canvas state with a geom; documented). NA-displacement/degenerate
  rows dropped in setup_data/compute_panel (StatArcBar needed an nrow==0 guard —
  scalar assignment to a 0-row frame errors). `extra_params` needed on the geom
  so ggplot2 accepts `amax`. Correctness proven device-independently: the arc
  and point layers' built x/y are byte-equal to ssm_plot_circle's (layers 6/7 in
  both, since both share circle_base's 5 canvas layers) on single- and
  multi-profile results and the cross-zero case; plus a synthetic wrap test
  (350→10 arc has ~same vertex count as 170→190, not the ~17x of a long-way
  span). ssm_plot_circle untouched → its 11 vdiffr snapshots unchanged (verified
  via git). One example bug (Ampl/Disp vs a_est/d_est) caught before check.
  Suite 518/518; check 0/0/0. NEWS.md added. (R/geom_ssm.R [new],
  man/geom_ssm_point.Rd [new], man/geom_ssm_arc.Rd [new], NAMESPACE,
  tests/testthat/test-geom_ssm.R [new],
  tests/testthat/_snaps/geom_ssm/*.svg [new], NEWS.md, MILESTONES.md).
- 2026-07-02 — V1 Public circular canvas (Opus): exported `ggcircumplex()`,
  a documented ggplot2 canvas constructor, as a thin public wrapper over the
  existing internal `circle_base()` (left untouched, so all 11 existing
  ssm_plot vdiffr snapshots are structurally unable to regress — verified: git
  shows only the two NEW ggcircumplex snapshots added, no existing snap
  modified). Signature `ggcircumplex(angles, labels, amin, amax, font_size,
  instrument)`; when an `instrument` is supplied it derives angles from
  `Scales$Angle` and defaults labels to `Scales$Abbrev` (validated:
  ggplot_build data of the instrument path == the explicit angles+labels path,
  device-independent; LM=360 scale labels correctly). Deferred
  `annotation_circumplex()` to V2 where the ggproto layer machinery is built
  (a half-baked annotation now would be worse than focused). Dropped a
  would-be single-member `@family`; used `@seealso ssm_plot_circle()` instead.
  Validation via is_* helpers (labels length, is_instrument, scalar numerics).
  Test-first (failed on missing function); one self-inflicted test bug fixed
  mid-task (duplicate vdiffr snapshot name → replaced the second render with a
  ggplot_build data-equality assertion). Suite 509/509; check 0/0/0. NEWS.md
  added. (R/ssm_plot.R, man/ggcircumplex.Rd, NAMESPACE,
  tests/testthat/test-ssm_plot.R, tests/testthat/_snaps/ssm_plot/*.svg [new],
  NEWS.md, MILESTONES.md).

---

## M2 — Inference quality (v1.3.0) — GitHub-complete 2026-07-02 (bundled into v1.3.0 with M3)

Source: ROADMAP.md Milestone 2. Upgrades to the existing bootstrap machinery;
no new statistical scope. Per ROADMAP.md's CRAN release strategy, this
milestone is bundled with M3 (ggplot2 extension) into a single v1.3.0 CRAN
submission — keep both milestones' work on GitHub until both are done, then
run `/release-checklist` once.

### Tasks

- [x] **Parallel bootstrapping** via `boot`'s built-in `parallel`/`ncpus`
  arguments, exposed through `ssm_analyze()`.
- ~~**BCa confidence intervals**~~ **DROPPED 2026-07-02** — see log entry and
  ROADMAP.md; amplitude-coverage question folded into M4's CI-trustworthiness
  diagnostic.
- [x] **Monte Carlo alternative to bootstrapping**: sample SSM parameters from
  the asymptotic sampling distribution of the mean vector / correlation
  vector (multivariate normal with estimated covariance), propagate through
  the parameter transformation. Validate against bootstrap results on
  `jz2017`.
- [x] **Vectorize `ssm_score()`** (currently row-wise `apply` + `rbind` of
  data frames): elevation/x/y are single matrix products; amplitude,
  displacement, and fit follow element-wise.
- [x] Seed/reproducibility documentation for all resampling paths.
- [x] **Continuous-track item to do first**: named, long-format internal
  results assembly (`ssm_bootstrap()` identifies displacement columns by
  positional arithmetic `d_vars <- 1:(ncol/6)*6 - 1`; `reshape_params()`
  assumes a fixed 6-parameter block). Replace with named columns / one-row-
  per-parameter internal format before starting the interval work above,
  which touches exactly this code (per ROADMAP.md continuous track).

### Log

- 2026-07-02 — Continuous-track refactor (Opus): replaced positional parameter
  arithmetic with name-driven assembly. New single source of truth
  `ssm_param_names()` (canonical C++ order e/x/y/a/d/fit); `reshape_params()`
  derives block width + names from it; `ssm_bootstrap()` names replicate columns
  `<param>_<group>` and locates displacement via `param_of_col == "d"` (dropping
  `1:(ncol/6)*6-1` and `contrast_d_vars <- ncol-1`). Behavior-preserving:
  name-based selection is column-identical to the old arithmetic in both
  contrast and non-contrast paths; all seeded bootstrap pins byte-identical.
  Chose the named-columns route (not a full one-row-per-parameter melt) — lower
  churn, and the interval work only needs to locate columns by name. Test-first:
  added contract pins for `ssm_param_names()`/`reshape_params()` + a 3-group
  non-contrast case exercising multi-block name selection. Suite 432/432; check
  0/0/0. No NEWS.md (internal only). Unblocks BCa / Monte Carlo / parallel.
  (R/utils.R, R/ssm_bootstrap.R, tests/testthat/test-ssm_bootstrap.R,
  MILESTONES.md).
- 2026-07-02 — Vectorize `ssm_score()` (Sonnet): replaced row-wise
  `apply(FUN = ssm_parameters) + do.call(rbind, ...)` (per-row data frame
  construction and rbind, O(n) R-level overhead) with a single call to the
  existing `group_parameters()` C++ routine (already used by
  `ssm_bootstrap()`) plus one `matrix()` reshape keyed off
  `ssm_param_names()`. Deliberately reused the already-tested compiled
  degenerate-profile/tolerance logic (B4) rather than re-deriving it in R —
  duplicating that tolerance math was the likelier place for a boundary bug,
  per CLAUDE.md's correctness bar. Bit-for-bit identical to the pre-refactor
  output on `aw2009` (max abs diff 0); ~68x faster at n=5000 (0.65s -> 0.0096s).
  Two behavior changes made deliberately: (1) degenerate-row warnings
  consolidated from one-per-row to a single "`n` of `total`" warning,
  matching the precedent set by `ssm_bootstrap()`'s resample warning; (2)
  found and fixed a regression introduced by the first draft of this refactor
  — using `modifyList()` for label/prefix/suffix `...` forwarding silently
  swallowed unrecognized argument names, where the old `apply(FUN =
  ssm_parameters, ...)` used to error "unused argument" on a typo (verified
  against pre-refactor code); added an explicit unknown-name check to restore
  the error, with a regression test. Test-first: added coverage for label/
  prefix forwarding (previously untested), the consolidated warning, and the
  unused-argument error. Suite 441/441; check 0/0/0. No NEWS.md (internal
  perf only, no API change). (R/ssm_analysis.R, tests/testthat/test-ssm_analysis.R,
  MILESTONES.md). [Correction, same day: a NEWS.md bullet WAS added during the
  parallel-bootstrapping task — the consolidated warning is user-visible and
  the speedup worth announcing; "internal only" was the wrong call.]
- 2026-07-02 — Parallel bootstrapping (Fable): `ssm_analyze()` gains
  `parallel`/`ncpus` (validated via match.arg + boots-style stopifnot),
  threaded explicitly through ssm_analyze_means/corrs -> ssm_bootstrap ->
  boot::boot(). Key statistical fact, verified against the installed boot
  source AND empirically: for this nonparametric bootstrap, boot draws the
  full resample index array in the master process before dispatch and our
  statistic is deterministic, so seeded results are BYTE-IDENTICAL for any
  parallel/ncpus setting (tested: snow + multicore vs serial on the grouped
  contrast path and the correlation path; master .Random.seed state identical
  after serial vs parallel; B4 degenerate-resample warning + results identical
  under PSOCK workers). Docs state the reproducibility guarantee on @param
  parallel; multicore documented as ignored on Windows (boot silently falls
  back to serial). Defaults unchanged -> all seeded pins intact. Suite
  447/447; check 0/0/0. NEWS.md bullet added (plus the missing ssm_score
  bullet, see correction above). (R/ssm_analysis.R, R/ssm_bootstrap.R,
  man/ssm_analyze.Rd, tests/testthat/test-ssm_analysis.R, NEWS.md,
  MILESTONES.md).
- 2026-07-02 — BCa task DROPPED (decision: Jeff, on Fable analysis; no code).
  Deciding fact: BCa is undefined for circular displacement — z0 =
  qnorm(P(t* < t0)) and the jackknife acceleration are order-statistic
  concepts requiring a linear scale; on a circle "below" depends on an
  arbitrary branch cut. So any BCa option is necessarily mixed-method
  per-parameter (BCa e/x/y/a, percentile d) forever. Costs judged not worth
  it: per-parameter method labels on every CI surface (print/summary/
  ssm_table/plots/vignettes) and in users' methods sections; against field
  convention (Z&W 2017 percentile); boot.ci incompatible with our circular
  quantiles and B4 NA-filtered degenerate resamples (hand-rolled BCa =
  classic plausible-but-wrong trap); opt-in-only feature doubling the CI test
  surface. Steelman acknowledged: amplitude (nonnegative, upward-biased,
  skewed near zero, drives the G1 guardrail) is the one real beneficiary —
  that question moved to M4's ssm_ci_accuracy diagnostic (ROADMAP.md updated
  both ends). Monte Carlo task remains as the independent cross-check on
  percentile CIs. (MILESTONES.md, ROADMAP.md).
- 2026-07-02 — Monte Carlo engine (Fable): `ssm_analyze(method = "montecarlo")`.
  Design decisions, each validated: (1) EMPIRICAL influence-function covariance
  for correlations (psi_i = z_x z_y − (r/2)(z_x²+z_y²), acov = crossprod(psi)/n²)
  instead of normal-theory Pearson–Filon — on non-normal simulated data psi
  tracks direct simulation at max err .022 (n·acov units) where PF errs .39
  (17x worse); matters because jz2017 measures are skewed counts. Verified psi
  ≡ PF on MVN data (.003) and var(r) ≡ (1−ρ²)². (2) JOINT draws across measures
  within group (they share the sample): measure-contrast e-CI width — bootstrap
  .0741, MC joint .0759, independent draws would be .098 (32% too wide; the
  main trap of this task). (3) Fisher-z sampling for correlations (delta-method
  cov, tanh back-transform) — keeps draws in (−1,1). (4) PSD-safe eigen-based
  MVN sampler (ipsatized/singular covariance tested). (5) Scope: MC + missing
  data requires listwise (informative error); n_g ≥ 2 required; |r| = 1 errors.
  Reuse: extracted ssm_replicate_intervals() from ssm_bootstrap() (behavior-
  preserving; label param keeps bootstrap warning byte-identical) so MC shares
  the validated circular-quantile/branch/degenerate machinery; propagation via
  vectorized group_parameters(). t0 computed identically to boot's ⇒ point
  estimates byte-equal. Validation: MC ≡ bootstrap CIs on jz2017 (means, corr
  + measure contrast, group contrast; all endpoints within 15% of interval
  width, encoded as tests); MC e/x CIs ≡ closed-form analytic normal-theory
  CIs (<2% of width); rotation equivariance; 0/360 straddle; contrast branch
  harmony at ±180 (est inside CI); flat data → NA + count warning via shared
  machinery. details$method recorded; summary() label conditional ("Monte
  Carlo Draws"), old objects default to bootstrap label. Suite 499/499; check
  0/0/0; seeded pins untouched (default engine unchanged). NEWS.md added.
  (R/ssm_montecarlo.R [new], R/ssm_bootstrap.R, R/ssm_analysis.R, R/ssm_oop.R,
  man/ssm_analyze.Rd, tests/testthat/test-ssm_montecarlo.R [new], NEWS.md,
  MILESTONES.md).
- 2026-07-02 — Seed/reproducibility documentation (Sonnet, doc-only). Added a
  DESIGN.md "Reproducibility" section: a per-engine table (serial bootstrap,
  parallel bootstrap, Monte Carlo) of what a fixed seed guarantees and exactly
  what RNG each consumes (index-array-then-dispatch for bootstrap; one
  rnorm() block per group, groups jointly across measures, in group_ids order
  for Monte Carlo — traced from R/ssm_montecarlo.R, not assumed), plus an
  explicit "what this does NOT mean" list (no cross-engine agreement from a
  shared seed; no stability across `boots`; ordinary cross-R-version caveat).
  Refreshed DESIGN.md's data-flow diagram, stale since the continuous-track
  refactor and Monte Carlo addition (now shows both engines and the shared
  ssm_replicate_intervals() assembly). Fixed a second stale line found in the
  same table (BCa listed as "planned" — dropped last task). Added a matching
  `@section Reproducibility` to `?ssm_analyze` (was previously scattered
  across the `parallel`/`method` @param entries only). Vignette: the
  "randomness inherent to bootstrapping" sentence in the introduction
  vignette was actually imprecise (implied the PANO()/octants() shortcuts
  caused the CI difference between `results`/`results2`; they return
  identical values to the manual vectors — the real cause is both calls
  sharing one un-reseeded RNG stream from the vignette's single top-level
  set.seed()) — corrected per CLAUDE.md's vignette-precision bar, with a
  cross-reference to the new roxygen section. No code changes; doc-only, no
  NEWS.md bullet. Suite 499/499 (unchanged); vignette re-rendered clean;
  check 0/0/0. M2 COMPLETE (all tasks checked or explicitly dropped with
  rationale). (DESIGN.md, R/ssm_analysis.R, man/ssm_analyze.Rd,
  vignettes/introduction-to-ssm-analysis.Rmd, MILESTONES.md).

## M1 — Correctness & robustness patch (v1.2.0) — released 2026-07-02, CRAN-approved

Source: ROADMAP.md Milestone 1 (2026-07 audit). Every bug fix landed with a
regression test that failed on the pre-fix code.

### Bugs

- [x] **B1. `ssm_score()` forwards `angles`** — `R/ssm_analysis.R:517`
  passes `...` to `apply()` but never `angles`.
  *Accept:* `ssm_score(aw2009, scales = PANO(), angles = rotated)` differs
  from octant results and matches row-wise `ssm_parameters(x, rotated)`;
  4-scale case with `poles()` works; existing tests still pass.
- [x] **B2. `is_null_or_char()` honors `n`** — `R/utils.R:146` passes
  `n = NULL`.
  *Accept:* `ssm_analyze(..., measures = c("A","B"), measures_labels = "one label")`
  errors informatively; NULL still accepted; audit other call sites
  (`caption`, `angle_labels`) for behavior changes.
- [x] **B3. NA grouping values handled** — NA in `grouping` with
  `listwise = FALSE` crashes in `mean_scores()` (`unique(): detected NaN`).
  *Accept:* NA-group rows dropped with a `message()` reporting the count, in
  both deletion modes; results match manually pre-filtered data.
- [x] **B4. Degenerate profiles return NA + warning** — zero-variance scores
  give `Fit = -Inf` and noise displacement (`src/parameters.cpp`).
  *Accept:* flat profile returns NA displacement/fit with one warning;
  near-zero amplitude documented behavior decided and tested; bootstrap
  containing some degenerate replicates doesn't error.
- [x] **B5. `norm_standardize()` robust matching** — exact float equality on
  `Angle` vs norms table (`R/tidying_functions.R:181-186`).
  *Accept:* 0° vs 360° convention mismatch either works or errors with a
  message naming the expected angles; duplicate-angle norms error clearly.
- [x] **B6. Contrast displacement branch harmony at ±180°** — point estimate
  in (-180°, 180°] can disagree with CI branch from circular centering.
  *Accept:* simulated contrast near ±180° has estimate inside its CI;
  test added at the boundary.

### Guardrails & UX

- [x] **G1.** `print`/`summary.circumplex_ssm` note when fit < .70 or the
  amplitude CI includes 0 (displacement not interpretable).
- [x] **G2.** Document displacement boundary convention (0° prints as 360°),
  or normalize; decide once, record in DESIGN.md.
- [x] **G3.** `inherits()` instead of `class(x) ==` everywhere; fix or drop
  matrix input support in `ssm_analyze()`/`ssm_score()`. Also (found during
  B1 review): `ssm_score()` validates `is.character(scales)` and so rejects
  numeric column indexes, contradicting both its own roxygen ("variable names
  or column numbers") and `ssm_analyze()`'s `is_var()` validation — align on
  `is_var()`.
- [x] **G4.** Consider warning on unused `...` in plot functions.

### Docs

- [x] **D1.** Purge `ssm_plot()` references (intermediate vignette line ~271,
  introduction vignette line ~409).
- [x] **D2.** Fix `angle_lables` typo in `ssm_plot_curve()` example.
  (Done with G4, since the typo would otherwise trip the new warning.)
- [x] **D3.** `instruments()` count: says 14, lists 15.
- [x] **D4.** Document contrast direction in `?ssm_analyze`.
- [x] **D5.** Document equal-spacing assumption of the closed-form estimator.
- [x] **D6.** Intro vignette: fix "displacement significantly different from
  zero" phrasing.
- [x] **D7.** Delete stale `CRAN-SUBMISSION` file. (`.Rbuildignore` already
  updated for the md files and `.claude` — done 2026-07-02.)
- [x] **D8.** NEWS.md cleanup: remove the duplicated `# circumplex 1.1.0`
  heading (lines 3/5); skim the rest for similar artifacts.

### Release

- [x] R CMD check clean on CI matrix; NEWS.md updated per user-facing change;
  version to 1.2.0; `/release-checklist`.

### Final log

- 2026-07-02 — Milestone opened from audit. Scaffolding added (CLAUDE.md,
  DESIGN.md, ROADMAP.md, MILESTONES.md, skills), `.Rbuildignore` updated.
- 2026-07-02 — ROADMAP revised: added CI-trustworthiness diagnostic (Z&W 2017
  via CircE replacement, now M4), inserted ggplot2 extension as M3 (before
  fit stats so later milestones plot through it), renumbered M4-M6, added
  refactor verdict + targeted refactor list to continuous track.
- 2026-07-02 — B1: `ssm_score()` now forwards `angles` to `ssm_parameters()`;
  regression tests incl. 0°/360°-peak boundary; validated vs OLS at ~1e-13;
  check clean 0/0/0 (R/ssm_analysis.R, tests/testthat/test-ssm_analysis.R,
  NEWS.md). Review found pre-existing `scales` validation inconsistency →
  noted in G3. NB: dev env had lost ggforce/htmlTable + stale .so; reinstalled
  and rebuilt via clean_dll().
- 2026-07-02 — B2: `is_null_or_char()` now forwards `n`; call-site audit
  (measures_labels, angle_labels, caption) confirmed all tightenings only
  reject previously-wrong inputs; check clean (R/utils.R, tests, NEWS.md).
  Collateral fix: seeded the five unseeded vdiffr blocks in test-ssm_plot.R
  and regenerated 11 snapshots — they had depended on RNG state leaking from
  earlier test files, so ANY upstream test that consumes RNG broke them
  (diagnosed when B2's bootstrap tests did exactly that; only arc coordinates
  changed, rendering verified unchanged via the seeded cross-zero snapshot).
  NB: test-ssm_plot.R is stored with CRLF line endings (repo outlier) —
  preserved; normalize deliberately someday if desired.
- 2026-07-02 — B3 (Opus): NA `grouping` rows now dropped in the `ssm_analyze()`
  dispatcher (on the user's real grouping column, once) with a count message +
  empty-data guard; fixes the pairwise `unique(): detected NaN` crash. No src/
  change needed — the R-layer guard keeps NaN out of Armadillo. Review moved
  the drop from a per-subfunction helper up to the dispatcher, which also
  fixed a would-be column-name collision (a scale named "Group" vs the renamed
  grouping column). Regression tests cover both modes, contrast, the collision
  (expect_no_message), and the all-NA clean error. Check clean 0/0/0
  (R/ssm_analysis.R, R/utils.R, tests, NEWS.md).
- 2026-07-02 — B4 (Fable): degenerate-profile handling. C++ detects flat
  (sd ≤ 8·ε·n·max|s| — cannot test var==0 exactly; constant 0.1 gives ~2e-34)
  → NA disp/fit, and zero-amplitude-with-variance (pure higher harmonic) →
  NA disp, fit exactly 0. C++ silent; R warns once for observed profiles and
  once with a count for degenerate bootstrap resamples (quantiles now na.rm;
  CIs conditional on estimability, disclosed). Decision: NO threshold beyond
  machine noise — small real amplitudes keep point estimates (validated to
  1e-9 amplitude); their uncertainty is the CI's/G1's job. Validation: 15/15
  incl. NA-excluded CI == independent boot+filter reference (1e-10, 16/300
  degenerate). Seeded pins unchanged. Documented in roxygen + DESIGN.md
  (src/parameters.cpp, R/ssm_analysis.R, R/ssm_bootstrap.R, tests, NEWS.md).
- 2026-07-02 — B5 (Opus): `norm_standardize()` matches scale→norm row by
  circular angular distance (`pmin(|Δ| %% 360, 360 - ...) < 1e-6`) instead of
  exact `==`, so 0≡360 just works; clear errors for zero matches (names
  available angles) and >1 match (duplicate-angle norms). Fixes cryptic
  "replacement has length zero". Seeded values unchanged; check 0/0/0; review
  clean. Note: still uses `class(instrument) ==` — G3 scope.
  (R/tidying_functions.R, man/norm_standardize.Rd, tests, NEWS.md).
- 2026-07-02 — B6 (Fable): contrast displacement CI now reported on the
  estimate's branch. Defect: near ±180° the angle_dist estimate and the
  circular-mean-centered CI could land on opposite branches (est +179.4 vs CI
  (−196.6, −159.0) at data seed 70 — reproduced through the real pipeline
  after a seed search; flip probability ~10% per boundary dataset, hence
  intermittent). Fix: shift both CI endpoints by 2πk, k = round((est −
  mid)/2π), in ssm_bootstrap before degree conversion — identity (k=0) away
  from the boundary (all seeded pins byte-identical), width/contiguity
  preserved, cannot fabricate coverage (|est − mid| ≤ π ⇒ k=0 for wide CIs).
  Validation: pkg CI == independent reimplementation (same RNG stream) to
  ~1e-13 on 3 boundary seeds; numeric ≡ geometric membership on 25 seeds;
  rotation equivariance. Review: 1 finding (stale CLAUDE.md invariant bullet)
  fixed. ALL M1 BUGS COMPLETE. (R/ssm_bootstrap.R, tests, CLAUDE.md,
  DESIGN.md, NEWS.md).
- 2026-07-02 — G1 (Opus): print/summary.circumplex_ssm now note when a profile
  has fit < .70 ("interpret only elevation") or amplitude CI includes 0
  ("displacement not interpretable"). Profile rows only (contrast fit/amplitude
  are differences, not prototypicality). "Includes 0" operationalized as
  round(a_lci, digits) <= 0 since amplitude is structurally >= 0 (real profiles
  ~0.003; flat ~6e-17) — note tracks the displayed precision, so it stays
  consistent with the printed table. summary() inherits via print(). Non-ASCII
  R-squared written as ² (check 0/0/0). Review: inline (proportionate to a
  ~20-line print change); no other snapshot/expect_output affected; vignette
  summaries will gain notes on low-fit profiles (non-breaking).
  (R/ssm_oop.R, tests/testthat/test-ssm_oop.R, NEWS.md).
- 2026-07-02 — G2 (Opus): DECISION = document, do not normalize. Profile
  displacement range is [0°, 360°) (estimator modu(atan2,2π)). A peak exactly
  at the boundary reports ≈360° deterministically (y ≈ −2.78e-17 → atan2 small
  negative → wraps just under 2π), equivalently ≈0°, same pole. Not
  canonicalized: measure-zero float artifact, any snap is an arbitrary
  tie-break, ≈360 matches LM=360. Recorded in DESIGN.md conventions table,
  ?ssm_analyze return docs, and intro vignette. B1 boundary test already
  accepts {~0,~360}, so no test change. Doc-only; check 0/0/0.
  (DESIGN.md, R/ssm_analysis.R, man/ssm_analyze.Rd, intro vignette).
- 2026-07-02 — G3 (Opus): DECISION = support matrix input (not drop). Coerce
  `if (is.matrix(data)) data <- as.data.frame(data)` at entry of ssm_analyze,
  ssm_score, ipsatize, score, norm_standardize, self_standardize (guarded, so
  data.frame path byte-identical → seeded pins unchanged). ssm_score scales
  validation `is.character` → `is_var` (now accepts numeric indexes per its
  roxygen). All 5 `class(x) ==` sites → `inherits()` (tidying×2, ssm_table,
  ssm_plot_curve, is_instrument). Roxygen @param data aligned to "data frame
  or matrix". Regression tests: matrix≡data.frame for ssm_score/ssm_analyze/
  self_standardize/ipsatize, numeric scales for ssm_score; edge-checked
  matrix+grouping and matrix+append. Review inline (mechanical + input
  coercion). check 0/0/0. (R/ssm_analysis.R, R/tidying_functions.R,
  R/instrument_oop.R, R/ssm_table.R, R/ssm_plot.R, man/*, tests, NEWS.md).
- 2026-07-02 — G4 + D2 (Opus): DECISION = warn (not silent). Added base R
  `chkDots(...)` to ssm_plot_circle/curve/contrast (`...` is a pure sink in all
  three — no forwarding — so any arg landing there is a genuine typo; partial
  matching routes valid abbreviations to formals first, so no false positives).
  chkDots immediately surfaced a real latent bug: an existing test passed
  `drop_xy = TRUE` to ssm_plot_circle (which has no such arg — silently
  ignored); removed it (snapshot unchanged). Also fixed the D2 typo
  `angle_lables` → `angle_labels` in the ssm_plot_curve example (would have
  tripped the new warning). @param ... docs updated. check 0/0/0.
  (R/ssm_plot.R, man/*, tests/testthat/test-ssm_plot.R [CRLF preserved],
  NEWS.md).
- 2026-07-02 — D1 (Sonnet): purged the two stale `ssm_plot()` references
  (deleted function, split into `ssm_plot_circle()`/`_curve()`/`_contrast()`).
  Intermediate vignette line 271: `ssm_plot(results6)` → `ssm_plot_contrast(results6)`
  (results6 is a contrast result, matching the `ssm_plot_contrast(results6)`
  call already used earlier for the same object at line 221). Introduction
  vignette line 409: prose updated to name `ssm_table()`, `ssm_plot_circle()`,
  and `ssm_plot_curve()` instead of the single deleted function, matching the
  plot3/plot4 code chunks that follow. Left NEWS.md's historical `ssm_plot()`
  changelog entries untouched (accurate past-tense references) and the
  gitignored `doc/` build artifacts alone (regenerate on next vignette build).
  Doc-only; full test suite still 424/424 pass. (vignettes/intermediate-ssm-analysis.Rmd,
  vignettes/introduction-to-ssm-analysis.Rmd).
- 2026-07-02 — D3-D8 (Sonnet): remaining Docs batch.
  D3: `instruments()` said "14 instruments" but listed 15 (verified against 15
  `.rda` instrument files in data/); fixed the count string, regenerated the
  vdiffr/testthat snapshot. D4: documented contrast direction on `@param
  contrast` in `?ssm_analyze` — verified against code, not just restated from
  CLAUDE.md: for two groups, second level minus first is alphabetical unless
  `grouping` is already a factor (code coerces via `factor()`, which preserves
  existing level order — R/ssm_analysis.R:243); for two measures, it's simply
  `measures[2] - measures[1]` in the order given, never reordered
  (R/ssm_analysis.R:373) — corrected an over-generalized first draft that
  wrongly implied `measures` could also be alphabetized. D5: documented on
  `@param angles` that the closed-form estimator equals OLS only for equally
  spaced angles, wording matched to DESIGN.md's reviewed table entry (dropped
  an unverified "may not be minimally biased" claim from a first draft — not
  asserted anywhere in DESIGN.md, so cut per the statistical-correctness bar).
  D6: intro vignette no longer describes the displacement CI as a
  "significantly different from zero" test (displacement is angular; 0 degrees
  is an arbitrary reference direction, not a null value) — elevation/amplitude
  (linear, zero is meaningful) still described that way. D7: deleted the
  stale, untracked `CRAN-SUBMISSION` file (recorded the old 1.1.0 submission;
  already `.Rbuildignore`d). D8: removed the duplicated `# circumplex 1.1.0`
  heading in NEWS.md (only duplicate found — checked all version headings).
  Doc/roxygen-only; devtools::document() regenerated man/ssm_analyze.Rd; full
  suite 424/424 pass. (R/instrument_oop.R, R/ssm_analysis.R, man/ssm_analyze.Rd,
  tests/testthat/_snaps/instrument_oop.md, vignettes/introduction-to-ssm-analysis.Rmd,
  NEWS.md, CRAN-SUBMISSION [deleted]). ALL M1 DOCS COMPLETE.
- 2026-07-02 — Release prep (Sonnet, `/release-checklist`): pre-flight clean
  (working tree clean, all M1 Bugs/Guardrails/Docs boxes checked,
  `devtools::document()` no-diff). Verification: `devtools::test()` 424/424;
  `devtools::check(args = "--no-manual")` 0/0/0 locally. Ran
  `/statistical-validation` as a final consolidated pass since B1/B4/B5/B6 all
  touched estimation-adjacent code since v1.1.0: 22 independent reference
  checks (OLS equivalence at equal spacing, hand-computed 2/n Gurtman formula
  at unequal spacing, circular-quantile rotation invariance, angle_dist
  sign/antisymmetry, C++ helpers vs base R, end-to-end jz2017 sanity, plus all
  5 CLAUDE.md-mandated boundary cases: 0°/360° peak, flat profile, contrast
  near +/-180°) — all passed at ~1e-9 to ~1e-16. No CRAN revdeps
  (`tools::package_dependencies(reverse = TRUE)` returns none). Version bumped
  1.1.0.9000 -> 1.2.0 (DESCRIPTION); NEWS.md dev heading renamed to `# circumplex
  1.2.0` (no breaking changes to flag); cran-comments.md rewritten with test
  environments, revdep summary, and a change summary.
  NOT checking the Release box yet: local branch is 15 commits ahead of
  `origin/master` (nothing from this milestone has been pushed), so the actual
  GitHub Actions CI matrix has not run against this code — only the local
  macOS/R-4.6.1 check has. Awaiting user decision on push before that box can
  be honestly checked. (DESCRIPTION, NEWS.md, cran-comments.md).
- 2026-07-02 — Release box checked (Sonnet): user approved commit + push.
  Committed DESCRIPTION/NEWS.md/cran-comments.md/MILESTONES.md (f08248e),
  pushed 16 commits to `origin/master`. GitHub Actions R-CMD-check matrix
  green on all 5 legs (macos-latest/release, windows-latest/release,
  ubuntu-latest/devel, ubuntu-latest/release, ubuntu-latest/oldrel-1);
  test-coverage.yaml and pkgdown.yaml also green. M1 fully complete —
  package is CRAN-submission-ready pending the user's own
  `devtools::submit_cran()` (never run by the assistant).
- 2026-07-02 — win-builder R-devel clean; cran-comments.md updated to record
  it. ROADMAP.md gained a CRAN release strategy section (decouple GitHub
  milestones from CRAN submissions; M1 solo, bundle M2+M3, flagship M4).
  Fixed a NEWS.md line-wrap artifact (lone "0" digit on its own line).
- 2026-07-02 — **v1.2.0 approved by CRAN.** Post-acceptance: tagged `v1.2.0`;
  deleted the regenerated `CRAN-SUBMISSION` file; DESCRIPTION bumped to
  `1.2.0.9000`; NEWS.md gained a fresh `# circumplex (development version)`
  heading; milestone moved here to Completed; M2 promoted to the active slot.
