# Active milestone

## M5 — SEM-based SSM

Source: ROADMAP.md Milestone 5. Promoted to the active milestone on M4.5's
close (2026-07-07); folds into the v2.0.0 release train (M2–M5, target
~2026-08-02, code freeze ~2026-07-26 — see ROADMAP's CRAN release strategy).
Builds on the lavaan explorations in `devel/lavaan_ssm.Rmd` and
`devel/circum_lavaan.Rmd`.

The statistical design questions are already scoped in **Brief E**
(`devel/m5-m6-design-questions.md`, §M5): Q5.1 (per-parameter CIs from a
lavaan measurement model — delta vs bootstrap, and why nonlinear-parameter
interval construction must **not** be delegated to lavaan), Q5.2 (the
distinct estimand of invariance-constrained latent contrasts vs the current
observed contrast), and Q5.3 (M5 assumes **fixed theoretical angles**; freely
estimated circumplex-constrained angles are Browne's model, which `cpm_fit()`
already owns). Brief E is a scoping memo, not a spec — T1 turns it into one.

Cross-cutting guardrails (inherited from M4/M4.5, adapted for the SEM layer —
inlined so the reference survives archiving):

- The oracle rule (Brief A §6.1): **no expected numerical value enters a test
  from memory or from `devel/g2xx1.txt`** (un-vetted, banned). Any published
  oracle (e.g. a lavaan fit reproduced from a source) arrives only by
  transcription from the cited source, entered as blank templates first.
- Angles are the danger zone: every task touching displacement or a contrast
  carries the CLAUDE.md boundary suite (0°/360° peaks, straddling CIs, ±180°
  contrasts, flat/near-zero-amplitude profiles). The SEM layer adds a
  specific trap (Brief E Q5.1): a lavaan `:=` `atan2` value and its Wald or
  naive-percentile interval live on whatever branch `atan2` returned —
  displacement interval construction must go through the package's existing
  circular-quantile machinery, never lavaan's generic output.
- Reuse the tested circular pipeline, don't re-derive it: extract draws of
  (e, x, y) from lavaan (delta vcov → MVN draws, or `se = "bootstrap"`
  replicates) and push them through the package's `ssm_parameters` transform
  + `ssm_replicate_intervals()` / `quantile.circumplex_radian()` path — the
  same architecture the Monte Carlo engine uses, with lavaan supplying the
  mean and covariance (Brief E Q5.1 "Recommended direction").
- The closed-form-equals-OLS-only-for-equally-spaced-angles invariant applies
  **inside** the SEM exactly as outside it: any `circumplex_instrument` →
  lavaan-syntax generator must emit the correct non-orthogonal OLS weights (or
  restrict to equally spaced instruments), never the hard-coded equal-spacing
  cosine weights of `devel/lavaan_ssm.Rmd` (Brief E Q5.1 "A trap to write down
  now").
- RNG contract (DESIGN.md): stochastic-output entry points only consume the
  global stream; deterministic internals; document the seed convention wherever
  a bootstrap/MVN-draw path is exposed.
- Dependency policy: **zero net-new hard dependencies.** `lavaan` moves to
  **Suggests** and every SEM feature degrades gracefully without it (a clear
  error, not a hard failure at load); OpenMx/lavaan in Suggests only.
- Model tiers (Brief E's own guidance): **Fable** designs/reviews the
  estimator-and-CI-critical code (Q5.1/Q5.2 — where plausible-but-wrong
  statistics are possible: interval construction, the delta-vs-bootstrap
  per-parameter call, the branch handling, the estimand distinction);
  **Opus** for the API and syntax-generation plumbing; **Sonnet** for
  mechanical edits and doc runs. `/statistical-validation` after every task
  that produces SSM parameters or their intervals.

- [x] **T1. Design spec: latent-variable SSM + SEM contrasts (Fable).**
  Turn Brief E §M5 (Q5.1/Q5.2/Q5.3) into an implementation spec the way
  Brief B did for `ssm_ci_accuracy()`: the draws-through-the-transform CI
  architecture; the per-parameter delta-vs-bootstrap decision (e exact, a with
  the guardrail caveat, a/d intervals via the replicate pipeline); the
  two-estimand framing for contrasts (observed vs invariance-constrained
  latent) and the invariance-sequence gating; the fixed-angle scope boundary
  (Q5.3); the coverage-validation plan via the `ssm_ci_accuracy()` harness +
  `cpm_simulate()` plug-in populations; and the API surface and a T2+ phasing
  plan. Committed as a `devel/` brief.
  *Accept:* a committed `devel/m5-sem-design.md` covering estimator/CI design,
  the API surface, the validation strategy, and phasing; every downstream task
  (T2–T5) traces to a section of it; no code yet.
- [x] **T2. lavaan-syntax generator from `circumplex_instrument` (Opus).**
  Tooling to emit a fixed-theoretical-angle circumplex measurement model
  (Q5.3's `model0` shape) from an instrument object, emitting the correct OLS
  scale weights per the Q5.1 equal-spacing trap. `lavaan` → Suggests with
  graceful degradation.
  *Accept:* generated syntax fits under `lavaan` on a reference instrument;
  a test asserting the emitted weights (the always-present weights
  attribute, plus `:=` lines when emitted) equal the closed form at equally
  spaced angles and equal the OLS weights — differing from the closed
  form — at an angle set violating the harmonic-balance condition (spec
  §2.1/§3.5: equal spacing is sufficient, not necessary, for closed-form ≡
  OLS, so the test's "differs" arm must use a balance-violating set); a
  clear error (not a load failure) when `lavaan` is absent.
  *(Wording amended 2026-07-07 per the fresh-session review F9: the
  original "equal the closed-form only when angles are equally spaced" is
  falsified by balanced-but-unequal angle sets — see
  devel/m5-sem-design-review.md and spec §2.1.)*
- [x] **T3. Latent-variable SSM estimation + circular-aware CIs
  (Fable-critical).** Estimate SSM parameters from the fitted measurement
  model and construct CIs by the inherited draws-through-the-transform
  route: all parameters reported as estimate + interval via
  `ssm_replicate_intervals()` + circular quantiles (no printed SEs — the
  package has no SE surface; delta-method status per parameter is
  documentation guidance, spec §5.2, where elevation's delta SE is
  asymptotic, not exact, under the disattenuated estimand — Brief E's
  "exact" held only for its linear saturated sketch). Full boundary suite.
  *Accept:* coverage validated with the `ssm_ci_accuracy()` **machinery**
  (its Bradley/Wilson verdict conventions and plug-in-population design)
  via a seeded `devel/` harness per spec §8.3 — the exported function
  itself replays observed-data procedures and cannot assess a latent
  estimand; populations built from `cpm_fit()`'s P̂ with joint
  scales+measure draws (`cpm_simulate()` is scale-only per the B-spec G2
  contract); delta/MVN-propagation vs bootstrap compared, per Q5.1's
  blocked-on-M4 question, now answerable; boundary tests at a ≈ 0 and d
  near 0°/360°; the spec §7.3 method audit (summary override,
  ssm_ci_accuracy guard); `/statistical-validation` run.
  *(Wording amended 2026-07-07 with T1: the original "printed delta/Wald SE
  for elevation (exact)" and "validated with `ssm_ci_accuracy()`" were
  inherited from Brief E's saturated sketch and are unmeetable/imprecise
  under the spec's estimand — see devel/m5-sem-design.md §5.2, §8.3 and its
  review change log.)*
- [x] **T4. Multi-group invariance-constrained latent contrasts
  (Fable-critical; Opus plumbing).** A separately named workflow (not a
  replacement for the observed contrast) computing the contrast on latent SSM
  parameters under a configural→metric→scalar invariance sequence; the
  displacement contrast via `angle_dist(d2, d1)` on latent displacements with
  the (−180°, 180°] branch and circular-contrast CI machinery; an honest
  "cannot compare" path when invariance fails.
  *Accept:* both estimands documented side by side; ±180° branch-cut boundary
  tests on the latent displacement contrast; the invariance-failure path
  returns a stated non-comparison rather than a number; `/statistical-
  validation` run.
- [x] **T5. Vignette: "SEM-based SSM Analysis" (Opus/Sonnet).** Adapt
  `devel/lavaan_ssm.Rmd` into a vignette teaching both estimands, the CI
  architecture (why nonlinear-parameter intervals go through the package, not
  lavaan), and the boundary caveats.
  *Accept:* builds clean; exported API only; statistical-precision bar
  (CLAUDE.md); every reported CI/estimand claim traces to T1's spec or a cited
  source. Draw on the post-T3 literature notes (spec §3.2 and
  devel/m5-wendt-discrepancies.md, incl. its §8 source verification):
  cite Wendt et al. (2019) for the strict tier's CFA-PC correspondence
  (their R Code S25; "perfect circumplex" terminology via Gurtman &
  Pincus 2003, whose own confirmatory model is Browne's CIRCUM — the
  `cpm_fit()` family), for a real-data magnitude of the fixed-angle
  model's approximation (CFA-PC RMSEA .075–.111), and for the scaled
  tier's g ⊥ plane assumption being violated on IIP data (replicated
  g–agency r ≈ −.3; the strict tier estimates it); the CFA-PC ≡ m = 1
  CIRCUM equivalence at φ_g = 0 as the bridge between the package's two
  model families; Moss (2026) for why standardization/reliability
  uncertainty must be propagated (his Hunter–Schmidt coverage collapse
  to ~0.35), minding the both-sides vs scale-side disattenuation
  estimand difference; and position the latent SSM estimand as novel
  (no prior latent-level SSM work found, 2026-07-07 search).

## Log

- 2026-07-08 — M5 milestone-close `/code-review max` over the full cumulative
  diff (ci-cross-platform...HEAD; 10 finder angles + verify + sweep).
  **Statistics confirmed clean:** both heavyweight correctness angles
  returned empty after empirical validation (free-parameter indexing vs an
  independent implied-moments computation at 5.5e-17; MVN draws satisfy
  lavaan's equality constraints ~1e-9; contrast direction exercised live
  under non-alphabetical group appearance; every subclass consumer seam
  traced with real executions). 15 findings reported, all in the
  doc/guard/display layer; 9 fixed same-day: (1) the falsified
  "closed-form ≡ OLS only when equally spaced" necessity claim corrected in
  4 CRAN-facing roxygen passages (ssm_analyze/ssm_parameters/ssm_score —
  the F11 sharpening had stopped at the memory files); (2) `sem_fmt_p()` so
  no verdict/ladder/global-fit line ever prints the improper "p = 0"
  (now "p < 0.001"); (3) ssm_plot_contrast() on a gate-rejected latent
  contrast now restates the invariance verdict instead of a bare stopifnot
  (the old loose test regex matched the cryptic message — pin
  strengthened); (4) ssm_sem_parameters() now runs the shared
  sem_health_gate() (post.check caution parity with ssm_sem, spec §4.5);
  (5/6) actionable engine-precondition errors for se="none"+mvn and
  summary-moment+boot; (7) advisory when a bootstrap-covariance fit meets
  the mvn engine; (8) intentional-equality hedge in the cross-group
  equality guard's message; (9) vignette chunk gating corrected so
  lavaan-free content (ssm_sem_syntax, the estimand table) builds without
  lavaan — verified by a simulated lavaan-less render. One finding
  no_change_needed (the is_count() inline form matches ssm_analyze(), the
  direct family sibling; uniform helper deferred). Five cleanup findings +
  below-cap batch deferred to ROADMAP M5 follow-up bullets (vectorization
  and harness-algebra items need FP-parity/oracle re-record care, wrong
  week pre-freeze). Two candidates refuted as deliberate design
  (spec-mandated point-vs-draw thresholds; independent oracle weights in
  tests). New regression tests: sem_fmt_p unit, both engine-precondition
  errors, the bootstrap-vcov advisory, strict non-comparison plot-refusal
  pins. Full suite green post-fix; both vignette build modes render.
  (R/ssm_sem.R, R/ssm_plot.R, R/ssm_analysis.R,
  tests/testthat/test-ssm_sem.R, tests/testthat/test-ssm_sem_groups.R,
  vignettes/sem-based-ssm-analysis.Rmd, man/ssm_analyze.Rd,
  man/ssm_parameters.Rd, man/ssm_score.Rd, ROADMAP.md, MILESTONES.md.)
- 2026-07-08 — T5: added the "SEM-Based SSM Analysis" vignette
  (vignettes/sem-based-ssm-analysis.Rmd), adapting devel/lavaan_ssm.Rmd into
  a teaching document. Covers both estimands (P1 latent profile via
  `ssm_sem()`, disattenuated analog of the observed correlation profile; P2
  invariance-gated latent contrast) with worked jz2017 examples: NARPD
  latent vs observed profile (larger amplitude, higher fit, shifted d — the
  removal of attenuation and reliability heterogeneity, §4.2/§4.3); the
  generated measurement model (`ssm_sem_syntax()`) with the fixed-angle
  direction constraints and the φ_g = 0 / atan2 NOTE; the CI architecture
  (why a/d intervals go through the package's circular quantiles, not
  lavaan's delta/percentile — spec §2.2/§5, mvn+sandwich default per §5.1);
  the two-estimand side-by-side table (§6.1); a computed two-measure
  contrast and the Gender non-comparison path (metric invariance rejected,
  Δχ²(14)=54.78 — the honest "cannot compare" output); §10's limitations
  verbatim in spirit. Literature per the T5 acceptance: Wendt et al. (2019)
  strict-tier/CFA-PC correspondence, RMSEA .075–.111 benchmark, replicated
  g–agency r ≈ −.3; the CFA-PC ≡ m=1 CIRCUM equivalence at φ_g=0 bridging
  the two model families; Moss (2026) coverage-collapse (~.35) motivating
  propagation, with the both-sides vs scale-side disattenuation caveat;
  latent SSM positioned as novel. Doc-only (no R/ or src/ change): exported
  API only (no `:::`), lavaan-gated so it builds without the Suggests dep,
  renders clean. Also listed in the pkgdown navbar (_pkgdown.yml) and a NEWS
  bullet added. Statistical-precision fixes during self-review: RMSEA
  "similar range" softened to an honest not-like-for-like benchmark; the
  amplitude-gate wording changed from "reliable" to "well-defined direction"
  (term collision); Moss reference marked advance-online (Jeff supplied the
  full citation + DOI 10.1177/01466216261440511, folded in).
  (vignettes/sem-based-ssm-analysis.Rmd,
  _pkgdown.yml, NEWS.md, MILESTONES.md.)
- 2026-07-08 — T4: shipped the multi-group invariance-gated latent contrast
  workflow. Fable core: group-aware estimand maps (per-group ρ*_g; the
  latent-mean path μ*_g = ν + Λα_g), the configural→metric→scalar ladder
  fitted per rung with lavaan's own nested test (scaled under MLR) and
  CUMULATIVE gating (every tested rung ≤ the path's required rung must be
  retained; rungs above are reported, never required), the honest
  non-comparison path (verdict + separate configural profiles, no contrast
  rendered), joint-draw contrasts through the unchanged
  ssm_replicate_intervals machinery, and print's invariance block. Opus
  agents: multi-group syntax emission (per-group c() labels, ==-constraint
  reference fixing, mean structure per rung, df-verified counting; the
  repeated-vector label form silences lavaan's intentionality warning) and
  the docs/NEWS surfaces (side-by-side estimand documentation per §6.1).
  **Design amendment opened with the task (spec §6.2 change log): scaled-tier
  g–plane covariances fixed 0 in ALL groups at ALL rungs — the T3 φ_g flip
  made the originally pinned free-φ_g non-reference block non-nested against
  configural, which would have invalidated every ladder Δχ² test; nesting
  restored via the rescaling argument, g-lean comparisons are strict-tier.**
  /statistical-validation (10 checks): ladder Δχ²/p ≡ lavaan::anova exactly;
  contrast rotation-invariance and constructed-truth identity at population
  moments (1e-6°); relabeling antisymmetry — **which caught a real bug:
  lavaan orders groups by appearance, not factor levels; fixed by passing
  group.label = levels() (CLAUDE.md contrast contract), regression-tested**;
  configural ≡ single-group fits; reference-choice invariance (mean path).
  Coverage harness extended with two-group ±180° measure-path and mean-path
  cells replaying the shipped procedure incl. gating: zero inadequate
  verdicts; contrast coverage 0.925–0.964; **gate Type I measured at
  0.030–0.070 vs nominal α = .05** (the Δχ² gate is calibrated under MLR).
  /code-review high (6 angles, 10 findings, all fixed): the cumulative-gating
  bug (gate tested only its own increment — anti-conservative on the mean
  path, falsely-required above the gate), NA-p verdicts asserting
  never-run tests, the plain-label accidental cross-group equality guard
  (spurious ~0 contrasts through the escape hatch), empty-group-after-
  listwise, include_defined multi-group no-op, mandated multi-group boundary
  tests (pole straddle, flat group), stale-docs batch, first-class
  contrast_requested state, helper single-sourcing. Full suite 1724 pass;
  R CMD check 0/0/0. Boundary suite: ±180° contrast on-branch, pole
  straddle, flat group, metric-violating non-comparison, cumulative-gating
  and reported-only-rung regressions. (R/ssm_sem.R, R/ssm_sem_syntax.R,
  tests/testthat/test-ssm_sem_groups.R, tests/testthat/test-ssm_sem.R,
  tests/testthat/test-ssm_sem_syntax.R, devel/m5-coverage-oracle.R + .rds,
  devel/m5-sem-design.md, NEWS.md, man/, MILESTONES.md.)
- 2026-07-07 — Primary-source verification pass (Jeff supplied Wendt et
  al.'s supplements, Moss 2026, Cheung & Rensvold 2002, and Gurtman &
  Pincus 2003 in full): equal-g ridge-blocking inference confirmed
  verbatim (supplement R Code S25); spec §3.2 pedigree corrected — G&P
  2003's confirmatory model is Browne's CIRCUM, so the fixed-cosine CFA
  is Wendt et al.'s own construction, and CFA-PC ≡ m = 1 equal-ζ CIRCUM
  at φ_g = 0 (the meeting point of the package's two model families,
  candidate T5/M6 cross-model pin); C&R 2002 ΔGFI criteria transcribed
  (devel/cr2002-transcription.md — resolves spec §12.2's TBT; the
  source's internally contradictory p. 251 sentence and its
  two-group/plain-ML/normality scope caveats documented); Moss 2026 read
  in full (independent endorsement of the fit-and-propagate
  architecture; citable Hunter–Schmidt coverage-collapse magnitudes;
  both-sides vs scale-side disattenuation estimand caution for T5). Docs
  only — no code changed. (devel/m5-wendt-discrepancies.md §8,
  devel/cr2002-transcription.md, devel/m5-sem-design.md, MILESTONES.md.)
- 2026-07-07 — Wendt et al. (2019) discrepancy evaluation (post-T3;
  devel/m5-wendt-discrepancies.md): all departures from the closest
  published neighbor assessed — one fixed (ssm_sem() defaults to
  estimator = "MLR"; print reports robust/scaled fit indices with
  fallback; vcov verified bit-identical to the prior default, so T3's
  coverage evidence stands), one earlier spec-note misattribution
  corrected (their CFA-QC did not consistently improve fit), the rest
  justified by the estimand difference. Equal-g middle tier recorded as
  spec §12.6 (deferred; needs its own identification check). Web search
  found no prior latent-level SSM work (nearest: Weide et al. 2021, Moss
  2026); T5 to position the layer as novel. (R/ssm_sem.R,
  tests/testthat/test-ssm_sem.R, NEWS.md, devel/m5-wendt-discrepancies.md,
  devel/m5-sem-design.md, MILESTONES.md, man/ssm_sem.Rd.)
- 2026-07-07 — T3: shipped the latent-variable SSM estimation layer —
  exported `ssm_sem()` (fits the generated syntax, estimates the
  disattenuated profile, all parameters as estimate + interval via
  `ssm_replicate_intervals()` + circular quantiles, no printed SEs) and
  `ssm_sem_parameters()` (user-fit adapter with structural compatibility,
  fitted-angle, multi-group, and unidentified-configuration guards);
  `circumplex_ssm_sem` subclass with print/summary overrides; engine-side
  admissibility filter (per-cause warning, >5% escalation); method audit
  complete (ssm_ci_accuracy refusal + positive capability check; all other
  inherited consumers verified). **Two statistical findings, both spec
  amendments:** (1) the scaled tier's free g–plane covariances are locally
  unidentified exactly at φ_g = 0 (exact first-order ridge; verified
  analytically + numerically) — default flipped to 0-fixed per the spec's
  pre-decided fallback, no `free_g_plane` switch, g-lean modeled via the
  strict tier (spec §3.1/§12.3); (2) Q5.1 answered — `"mvn"` confirmed as
  default engine but only with lavaan's sandwich vcov
  (`se = "robust.huber.white"` default): plain-ML mvn undercovered
  displacement (0.88, N-stable) under the realism cell's misspecification
  (spec §5.1 ANSWERED block). Coverage validated with the ssm_ci_accuracy
  machinery via the seeded devel/m5-coverage-oracle.R harness (7 cells ×
  N ∈ {250, 1000} × both engines, shipped-procedure replay): zero
  inadequate verdicts, mvn 0.916–0.970 everywhere, realism-d 0.948/0.920.
  /statistical-validation run (transform ≡ lm() at both spacings 1e-14;
  estimand map ≡ lavaan implied moments 1e-16; pole/±180°/flat/rotation
  boundary checks all pass). Full boundary suite in tests; /code-review
  high (8 angles): fixes landed for the lavaan bootstrapLavaan NA-row
  contract, the boot inadmissibility denominator, the double-Heywood point
  guard, angles/multi-group/unidentified-fit guards, stale docs, and
  harness arc-rule reuse (ssm_ci_d_cover) + all-fail robustness. Full
  suite + R CMD check clean. NEWS.md entry added (SEM feature family now
  end-to-end); CLAUDE.md/DESIGN.md harmonic-balance sharpening and
  lavaan-runtime-Suggests amendments applied (spec §7.4, overdue from T2).
  (R/ssm_sem.R, R/ssm_sem_syntax.R, R/ssm_ci_accuracy.R,
  tests/testthat/test-ssm_sem.R, tests/testthat/helper-ssm-sem.R,
  tests/testthat/test-ssm_sem_syntax.R, devel/m5-coverage-oracle.R,
  devel/m5-coverage-oracle-results.rds, devel/m5-sem-design.md, NEWS.md,
  DESIGN.md, CLAUDE.md, man/, NAMESPACE.)
- 2026-07-07 — T2: shipped `ssm_sem_syntax()`, the exported lavaan-syntax
  generator for the fixed-theoretical-angle circumplex measurement model
  (scaled + strict tiers, optional external measures), plus the always-present
  OLS `weights` attribute and the `require_lavaan()` graceful-degradation gate.
  Encodes fixed angles via a *direction constraint* per scale
  (`0 == sin(θ)·lx − cos(θ)·ly`), which pins each scale's plane angle while
  freeing its saturation with no tan singularity at axis-aligned scales.
  Bug found and fixed during review: the emitted scaled model was silently
  mis-fit by lavaan's default `auto.fix.first` (it fixed each factor's first
  loading to 1, colliding with the unit-variance identification, giving df=12
  instead of the intended 10); fixed by making the syntax self-identifying
  (leading `NA*<first scale>` frees the first loading), so the bare string fits
  correctly under default `cfa()`/`sem()`. Tests strengthened to pin the
  *intended* model (df = moments − `sem_free_params()`; recovered plane
  directions equal the theoretical angles), not merely "converges". Symbolic
  identification gate refuses under-identified requests at generation time
  (scaled needs p ≥ 6; the empirical local-ID check rides with T3's fit
  function). Weights identity verified at octants (equals the closed form),
  at a harmonic-balance-violating set (differs; OLS still recovers an exact
  cosine profile where the closed form does not), and as a left inverse of B.
  48 new tests; full suite 1454 pass; `R CMD check` clean (0/0/0).
  Scope deferred to later tasks (non-breaking): `grouping`/`invariance`
  multi-group emission → T4; the inspection `:=` lines are emitted only under
  strict+measure (linear there) and refused if forced under scaled. Per spec
  §8.5, T2 produces no SSM parameters/intervals, so `/statistical-validation`
  runs at T3/T4, not here. NEWS.md entry deferred until T3 makes the SEM
  feature a usable end-to-end workflow. (R/ssm_sem_syntax.R,
  tests/testthat/test-ssm_sem_syntax.R, man/ssm_sem_syntax.Rd, NAMESPACE.)
- 2026-07-07 — T1: committed devel/m5-sem-design.md — the M5 implementation
  spec (estimand = profile-then-transform on model-implied disattenuated
  profiles; OLS weights with the harmonic-balance identity; scaled/strict
  fixed-angle model tiers; MVN-draws/bootstrap dual engine feeding
  ssm_replicate_intervals verbatim; adapted invariance ladder with an
  explicit non-comparison path; API, validation, phasing, open decisions).
  Same-day 4-angle review: 16 findings, all accepted and folded in (see the
  spec's review change-log entry) — including two statistical corrections
  (closed-form ≡ OLS is harmonic-balance, not iff-equal-spacing;
  exact-cosine mean recovery is strict-tier-only) and two MILESTONES T3
  wording amendments (no printed SEs; ssm_ci_accuracy machinery via a
  devel/ harness). Next: T2 (Opus). (devel/m5-sem-design.md,
  MILESTONES.md.)
- 2026-07-07 — T1 fresh-session review (the Brief A/B house pattern):
  committed devel/m5-sem-design-review.md, verdict ACCEPT WITH CHANGES —
  architecture, estimand, weights, identification, CI reuse, invariance
  gating, and all shipped-code claims held under adversarial checking; 12
  findings (1 high: §4.3's direction-integrity claim — stationarity
  necessary but not sufficient, heterogeneous saturations rotate d* with
  fit still high) all resolved into the spec same day (see its revision
  log). MILESTONES T2 acceptance amended per F9 (harmonic-balance test
  aim). T2 is unblocked. (devel/m5-sem-design.md,
  devel/m5-sem-design-review.md, MILESTONES.md.)
- 2026-07-07 — M4.5 PR (#28) CI fix: the milestone-close PR surfaced a
  pre-existing **red on master's R-CMD-check** (not caused by M4.5) — the
  `summary.circumplex_cpm` "Largest absolute residual" line used `which.max()`
  over residuals that `misfit_octant_P()`'s symmetric `v %o% v` pattern ties
  four-ways to ~1e-16, so the reported pair broke on sub-ULP BLAS differences
  and differed local (DE-LM) vs CI (PA-HI). Fixed to take the first tied
  maximum in fixed column-major order (PA-HI everywhere); snapshot regenerated
  (value unchanged), platform-independent regression test added. Full suite
  1406 pass. (R/cpm_oop.R, tests/testthat/test-cpm_api.R,
  tests/testthat/_snaps/cpm_api.md.)
- 2026-07-07 — M5 opened as the active milestone on M4.5's close. Promoted the
  ROADMAP Milestone 5 summary into an active task list (T1–T5: design spec →
  lavaan-syntax generator → latent-variable estimation+CIs → invariance
  contrasts → vignette), grounded in Brief E (`devel/m5-m6-design-questions.md`
  §M5) and inlining M4/M4.5's cross-cutting guardrails adapted for the SEM
  layer (the `atan2` branch-cut trap, the OLS-weight equal-spacing invariant
  inside the SEM, lavaan → Suggests with graceful degradation, Brief E's
  Fable/Opus/Sonnet tier split). M4.5 (structure tests, T1–T8) archived to
  MILESTONES-ARCHIVE.md with its full log; DESCRIPTION dev suffix bumped
  1.3.0.9000 → 1.3.0.9001; lightweight `m4.5-complete` tag cut at the close
  commit; ROADMAP M4.5 → GitHub-complete, M5 → active. The milestone-close
  `/code-review max` over M4.5's full cumulative diff is **done** (2026-07-07;
  entry in the archived M4.5 log): core statistics confirmed correct, 6
  fix-now findings landed, the rest deferred to the ROADMAP M4.5 follow-up
  bullets for the v2.0.0 train. First task is **T1** (Fable — the estimator/CI
  design where plausible-but-wrong statistics are possible).
  (MILESTONES.md, MILESTONES-ARCHIVE.md, DESCRIPTION, ROADMAP.md.)

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0; M4 and M4.5 →
GitHub-complete 2026-07-07, both fold into v2.0.0). When the active milestone
completes, the milestone-close archive step (or `/release-checklist` at a CRAN
release) moves it there too. This file stays scoped to the active milestone so
it is cheap to re-read at the start of each task.
