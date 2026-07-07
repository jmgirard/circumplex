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

- [ ] **T1. Design spec: latent-variable SSM + SEM contrasts (Fable).**
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
- [ ] **T2. lavaan-syntax generator from `circumplex_instrument` (Opus).**
  Tooling to emit a fixed-theoretical-angle circumplex measurement model
  (Q5.3's `model0` shape) from an instrument object, emitting the correct OLS
  scale weights per the Q5.1 equal-spacing trap. `lavaan` → Suggests with
  graceful degradation.
  *Accept:* generated syntax fits under `lavaan` on a reference instrument;
  a test asserting the emitted weights equal the closed-form only when angles
  are equally spaced and the OLS weights otherwise; a clear error (not a load
  failure) when `lavaan` is absent.
- [ ] **T3. Latent-variable SSM estimation + circular-aware CIs
  (Fable-critical).** Estimate SSM parameters from the fitted measurement
  model and construct CIs by the inherited draws-through-the-transform route:
  printed delta/Wald SE for elevation (exact) and amplitude (guardrail
  caveat); a and d interval construction via `ssm_replicate_intervals()` +
  circular quantiles. Full boundary suite.
  *Accept:* coverage validated with `ssm_ci_accuracy()` against a
  `cpm_simulate()` plug-in population (delta/MVN-propagation vs bootstrap
  compared, per Q5.1's blocked-on-M4 question, now answerable); boundary tests
  at a ≈ 0 and d near 0°/360°; `/statistical-validation` run.
- [ ] **T4. Multi-group invariance-constrained latent contrasts
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
- [ ] **T5. Vignette: "SEM-based SSM Analysis" (Opus/Sonnet).** Adapt
  `devel/lavaan_ssm.Rmd` into a vignette teaching both estimands, the CI
  architecture (why nonlinear-parameter intervals go through the package, not
  lavaan), and the boundary caveats.
  *Accept:* builds clean; exported API only; statistical-precision bar
  (CLAUDE.md); every reported CI/estimand claim traces to T1's spec or a cited
  source.

## Log

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
