# Active milestone

## M4.5 — Structure tests (Acton & Revelle 2004)

Source: ROADMAP.md Milestone 4.5. Split from M4 on 2026-07-07 and promoted to
the active milestone on M4's close (same day). The drafted task list and
acceptance criteria are kept here verbatim (ROADMAP carries only the
milestone-level summary). Revive and modernize the drafts in
`devel/fit_analysis.R` / `devel/fit_oop.R` (method-review:
`devel/fit-drafts-method-review.md`).

The cross-cutting guardrails below are copied from M4 on close — M4.5 inherits
them unchanged, and inlining them keeps the reference intact now that M4 is
archived:

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

- [x] **T1. Base-R principal-axis loadings + shared infrastructure.**
  Replace `psych::fa()` with an internal PAF; fix the ridge-on-wrong-matrix
  bug (ridge to R, not the data; no ridge⇒ML conflation); psych → Suggests
  as a test oracle. Rewrite in package style (base R, `is_*()` validation —
  the drafts predate the tidyverse-ectomy).
  *Accept:* internal loadings match `psych::fa()` oracle within tolerance on
  reference data; ridge applied to the correlation matrix with a test.
- [x] **T2. Cutoff re-derivation simulation (nv=8).** One simulation under
  A&R's generating model (their Eqs. 11.1–11.3) re-derives all test cutoffs
  at the package's scale-level nv=8 use — fixes the Gap nv-dependence and
  the VT/RT threshold/grid provenance in one run. Committed as a
  reproducible script (devel/ or data-raw/), cutoffs stored as package
  constants with provenance comments.
  *Accept:* script reruns to the stored constants under its seed; A&R's
  published nv-conditions reproduced within simulation error as a sanity
  gate; every threshold in T3–T6 traces to this run or a cited page.
  *Accept-note (what "reproduced" turned out to mean):* A&R's cutoffs are
  one-sided conservative plot reads, so the gate checks their claims'
  logical content (F_competing ≤ .031 at "almost" cutoffs; CDF ratio ≥ k/√2
  at "k-times" cutoffs) — 14/17 published claims reproduce; three left-tail
  "almost" claims are documented reproduction limits (two marginal, one a
  genuine tail difference traceable to CIRC_STRUC's unstated extraction
  pipeline; see the script's KNOWN_LIMITS block and
  devel/ar2004-transcription.md).
- [ ] **T3. Fisher test of equal axes.** Sound per review — port with
  citation, scoring-keyed cutoffs (raw vs deviation declared), T2 thresholds.
  *Accept (amended by T2's empirical adjudication):* the statistic is the CV
  of **vector lengths** √h², not Eq. 6's printed CV of h² — the T2 gate
  showed the published cutoffs only reproduce on the vector-length scale
  (devel/ar2004-transcription.md "Empirical adjudications"; method-review §1
  addendum). Internal `structure_fisher()` + nv=8 cutoffs already exist from
  T2; T3 wraps them with scoring-keyed cutoff interpretation and roxygen
  citing A&R (including the scale adjudication).
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

- 2026-07-07 — T2 (Fable, test-first): transcribed A&R 2004 under the
  two-channel protocol (devel/ar2004-transcription.md; no between-channel
  discrepancies; ε_v≡0 and repeated-Z ambiguities documented); implemented
  the four criterion statistics as internals with closed-form oracles and
  the adapted boundary suite (wrap-around-gap regression, exact-axis 180°
  sign·acos guard, orientation invariance via full-period grids, unified
  NA-not-NaN degeneracy policy); wrote data-raw/structure-test-cutoffs.R
  (seed 20260707), which empirically adjudicated two transcription
  ambiguities (standardized uniqueness; Fisher = CV of vector lengths,
  overturning method-review §1 — addendum added there), gate-reproduced
  14/17 published claims (3 documented left-tail limits), and derived the
  nv=8 cutoffs into the nv-keyed `structure_cutoffs` constant (Gap raw
  "almost" moves .01→.35 vs the published nv=64/128 value; Fisher barely
  moves, cross-validating A&R's nv claim). Slim derivation record committed
  (data-raw/structure-test-cutoffs.rds) with a testthat pin; seed-stability
  confirmed across two full independent script executions. T3's acceptance
  amended per the adjudication. /code-review high: 10 findings, all fixed
  (incl. a vacuous 180° test guard and a VT 0/0 NaN on bipolar scale
  pairs). Full suite + check clean. Internal only, no NEWS bullet.
  (R/fit_structure.R, tests/testthat/test-fit_structure.R,
  data-raw/structure-test-cutoffs.R, data-raw/structure-test-cutoffs.rds,
  devel/ar2004-transcription.md, devel/fit-drafts-method-review.md,
  devel/fit_analysis.R, devel/fit_oop.R, .gitignore, ROADMAP.md.)
- 2026-07-07 — T1 (Opus, test-first): added shared structure-test
  infrastructure. New internal `paf2()` (iterated base-R principal-axis
  factoring of the first two unrotated factors: SMC start, eigen-based,
  NPD-safe eigenvalue clip + Heywood cap, psych sign convention) and
  `structure_loadings()` (scale selection + correlation + ridge). Fixed the
  method-review §7 ridge bug: ridge is now added to the diagonal of the
  correlation matrix and rescaled to unit diagonal via `cov2cor()`, not added
  to the raw data; extraction is always PA (dropped the draft's ridge⇒ML
  conflation). `psych` added to Suggests as a test oracle only (no hard dep).
  Validated three ways: psych::fa oracle match on jz2017 (tol 0.01), a
  psych-independent exact-two-factor communality-recovery oracle, and
  fixed-point self-consistency; plus the NPD ipsatized matrix repaired by ridge
  and a row-order-invariance regression guarding the old data-perturbation bug.
  14 new tests, full suite 1236 pass, check clean. Not user-facing yet (exported
  API is T7), so no NEWS bullet. (R/fit_structure.R,
  tests/testthat/test-fit_structure.R, DESCRIPTION.)
- 2026-07-07 — M4 review #1 fix (Fable, test-first; /statistical-validation):
  the convergence-acceptance "reproduced" criterion in `cpm_engine()` counted
  the g0/mirror start pair as two starts, but reflection is an exact
  F-isometry (rho even), so the pair self-certified start-dependent local
  optima on ~8% of random noisy p=8 matrices with no warning. Now counts
  independent start groups (g0+mirror share one; each jitter its own); B/D
  unchanged. Point estimates byte-identical before/after (seed-pinned engine
  fits, all variants, and full `cpm_fit()` on jz2017) — only the
  `accepted` flag/warning moves. Regression test pins seed 19 (g0+mirror at
  min F, all jitters strictly worse). `m4-complete` tag moved to include this
  and review #3. (R/cpm_fit.R, tests/testthat/test-cpm_fit.R.)
- 2026-07-07 — M4.5 opened as the active milestone on M4's close. Promoted the
  queued M4.5 section (T1–T8 + vignette extension) to active, copying M4's
  cross-cutting guardrails block inline so archiving M4 did not orphan the
  reference. M4 (Browne model + CI trustworthiness) archived to
  MILESTONES-ARCHIVE.md with its full log; DESCRIPTION dev version bumped to
  1.3.0.9000; lightweight `m4-complete` tag cut at the close commit. No M4.5
  task work yet — first task is T1 (base-R principal-axis loadings). (DESCRIPTION,
  MILESTONES.md, MILESTONES-ARCHIVE.md.)

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0; M4 → GitHub-complete
2026-07-07, folds into v2.0.0). When the active milestone completes, the
milestone-close archive step (or `/release-checklist` at a CRAN release) moves
it there too. This file stays scoped to the active milestone so it is cheap to
re-read at the start of each task.
