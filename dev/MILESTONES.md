# Active milestone

## v2.0.0 release preparation

Source: ROADMAP.md CRAN release strategy. M2–M5 are all GitHub-complete
and accumulate on the v2.0.0 train (target ~2026-08-02, code freeze
~2026-07-26). No new feature milestone is active — M6 (longitudinal) is
deliberately deferred to its own ~v2.1.0 — so the active unit of work is
the release gate itself. M5 (SEM-based SSM) closed 2026-07-08 and is
archived in MILESTONES-ARCHIVE.md with its full log; its milestone-close
`/code-review max` is done (statistics confirmed clean, 9 findings
fixed, 5 deferred to ROADMAP’s M5 follow-up bullets).

**R1. cpm_pack β-boundary fix (Fable-tier; release blocker).** The sole
remaining red on the cross-platform CI matrix (the `ci-cross-platform`
branch / draft PR \#29). **Verified green 2026-07-08: PR \#29 all 7
checks SUCCESS on the real runners (all three ubuntu R-CMD-check jobs +
covr’s test-coverage, the four that carried the cpm_pack red since M4,
plus macOS/Windows/pkgdown).** Linux-only `cpm_pack: all(b_keep > 0)`
error when the CPM optimizer lands a harmonic weight exactly on the β =
0 boundary; the handoff brief is `devel/cpm-pack-boundary-brief.md`. Not
reproducible on the macOS dev machine, so it needs a Linux reproduction
(rocker/r-ver container or a CI debug run) before designing the fix.
Blocks both PR \#29’s green merge and M5’s merge to master (M5 is
stacked on ci-cross-platform). *Accept:* the four `test-cpm_fit.R` tests
pass on the ubuntu R-CMD-check jobs and under `covr`; a
platform-independent β = 0 (vanishing-harmonic) regression test; point
estimates on interior fits byte-identical (parity test);
`/statistical-validation` run.

**R2. Run `/release-checklist` for v2.0.0.** After R1 lands and CI is
green across platforms: bump DESCRIPTION to 2.0.0, rename NEWS.md’s
development heading to 2.0.0, refresh cran-comments.md (test
environments, 0/0/0 check, no revdeps, and the Moss DOI 403 = SAGE
bot-block note from the urlchecker run), run win-builder / R-devel, then
hand `submit_cran()` to Jeff. Do not submit autonomously. **Groundwork
staged 2026-07-08 and HELD for CRAN cadence** (v1.2.0 was approved
2026-07-02 — only ~1 week ago; CRAN wants ~1 month between submissions,
ROADMAP target ~2026-08-02): the accurate cran-comments.md summary and
the urlchecker DOI note are already written; the version bump / NEWS
heading rename / win-builder / submit are deliberately NOT done yet.
Repo stays at the 1.3.0.9002 dev version until the submission window
opens.

## Log

- 2026-07-09 — **CPM sim engine: Fable ratification APPLIED; ready to
  run (post-M4 charter; not release-gating; still NOT RUN as the
  study).** Worked the engine-review Ratification section. DECIDE-1:
  reviewer ratified re-keying the §2.4 guard off the engine’s `accepted`
  flag but corrected the rationale (accepted = grad_ok && reproduced;
  the *reproduction* limb failed, not Heywood) and rejected the
  underfit-interior estimand as genuinely ill-defined (symmetry-broken
  C₈ cyclic orbit + a second basin ≤1.1 deviance units across the N
  grid). Applied: N1 corrected the guard comment; N2 rebuilt the guard
  to the ratified 4-part form — (i) convergence, (ii) KKT sign check at
  ceiling communalities, (iii) statistical unimodality via an 11-start
  basin scan with (N_max−1)·ΔF≥10, (iv) circulance/symmetry keyed to the
  population matrix — applied only to projection-source estimands;
  correct-spec cells drop only on numerical non-identification at F≈0;
  N3 re-pinned the underfit generating config interior→trail_t010 (s1 +
  s3d pair {trail_t010, b0_dominant}). DECIDE-2: adopted the numerical
  parity tripwire (not the source hash) in selftest.R — asserts the
  percentile arm is byte-identical to cpm_fit(ci_method=“bootstrap”);
  data seed calibrated to k=4 (exercises mirror-guard + nonconvergent
  branches; degeneracy unreachable at N=60, NOTEd). Plan §2.4 guard +
  §3.1/§3.4d underfit re-pin amended to match (change log updated).
  Verification (not the study run): selftest 29/29 incl. the
  byte-identical tripwire; config table 553 kept / 7 dropped reproducing
  the reviewer’s exact figures (5 b0_dominant×perturbed×ζ.5
  non-identified + 2 perturbed-OOF near-tied); underfit/overfit/wrongfix
  arms all 15 cells restored; underfit projection + variant-B
  wrong-fixed smoke clean. Next: stage-0 throughput benchmark on the
  20-worker box, then the staged run. (devel/cpm-sim/\*,
  devel/cpm-simulation-paper-plan.md, MILESTONES.md.)
- 2026-07-09 — **§2.4 guard change + reconstruction tripwire
  RATIFICATION delivered (Fable follow-up; post-M4 charter; engine
  untouched, nothing committed).** Appended the dated ratification
  section to `devel/cpm-simulation-engine-review.md`. DECIDE 1: re-key
  away from `accepted` RATIFIED in principle (its `reproduced` limb
  certifies sample-fit trust, not estimand existence — and the config
  comment’s “bundles Heywood” rationale is factually wrong, N1),
  boundary pseudo-truths legitimate (KKT verified: natural dF/dζ =
  −4.2e-3 at ζ*=1 while the logit gradient is Jacobian-squashed to
  4e-7); BUT the admitted underfit-*interior\* estimand REJECTED as
  ill-defined by measurement: the circulant population’s projection is a
  symmetry-broken C₈ orbit (rotated copies at ΔF≈4e-13) plus a second
  distinct basin at ΔF=2.17e-5 ⇒ ≤1.1 deviance units over the whole N
  grid (invisible to `multimodal` at 1e-6). Redesign measured and
  prescribed: underfit generating config → trail_t010 (single basin
  across 11 starts, symmetric interior ζ*=.642, RMSEA .055); b0_dominant
  underfit already clean; exact §2.4 amendment wording supplied
  (converged + ceiling-KKT + (N_max−1)·ΔF≥10 basin scan +
  circulance/symmetry check). DECIDE 2: numerical parity fixture ADOPTED
  over source hash — full spec (fixture, byte-identity asserts,
  accounting asserts, calibration rule, failure text) in the review;
  `sim_replicates` re-diffed verbatim post-fixes. Bonus:
  t005/b0_dominant underfit projections share F* to 5e-15 (shared ζ²β₃
  on the balanced grid) — proposed as a build-time self-check. Config
  table rebuilt: 553/7 confirmed, OOF RMSEA .0494 (S1 fix works).
  (devel/cpm-simulation-engine-review.md, MILESTONES.md.)
- 2026-07-09 — **CPM simulation engine review fixes APPLIED (post-M4
  charter; not release-gating; still NOT RUN).** Worked the full
  independent Fable review (`devel/cpm-simulation-engine-review.md`,
  verdict “needs change before run”): all 6 must-fix (M1 seed-overflow →
  SEED_MULT scheme + range assert; M2 overfit infeasible-m crash →
  m2_truth generating config + project_truth tryCatch; M3 fit_prop
  phantom coverage; M4 records-by-default + θ̂/β̂/endpoints schema; M5
  Wald-θ miss side; M6 stage-2/3 selection wired as code incl. per-axis
  admission, B-sensitivity cell, 3a full-vs-grouped jackknife
  validation, 3b studentized), 11 should-fix (S1 OOF bracket + RMSEA
  assert; S2 BCa saturation vs NA; S3 one-sided fold+denom; S4
  secondary-level β fold; S5 studentized feasibility gate; S6 smoke
  jackknife floor; S7 worst-case bound / error flag / geometry /
  marker-conditional; S8 fork try-error guard; S9 tolerances 1e-6; S10
  stage-3 d/f scope; S11 cluster hoist), and the hygiene set (config
  cache, dead-code removal, abs pkg path, benchmark filter, stage-1
  large-N subset). One design decision EMERGED while applying M2 and is
  flagged for the pre-run Fable review: pseudo-truth well-definedness
  now keys on convergence + non-multimodality, NOT the engine’s
  `accepted` flag (which bundles Heywood) — otherwise the underfit m=2
  projection, which converges at the ζ→1 boundary, wrongly dropped the
  whole misspec arm; §2.4 already carries a boundary-status column, so
  boundary projections are recorded, not dropped. Verification (not the
  study run): selftest 27/27 (added M1/M3/M5/S2 regressions); config
  table builds clean (553 cells kept, 7 dropped = genuine multimodal;
  overfit truth zero-pads; OOF RMSEA 0.049; underfit arm restored,
  boundary-status recorded); a 3-rep single-cell kernel smoke exercises
  fit_and_score → summarize_cell with all new fields populating
  (worst-case, one-sided, contrasts, marker-conditional, secondary
  levels, geometry, studentized-infeasible gate, jack-validate
  grouped-vs-full). No factorial/smoke/benchmark run as the study. Next:
  pre-run Fable ratification of the §2.4 guard change (and the \#1 hash
  tripwire), then the stage-0 benchmark. (devel/cpm-sim/\*,
  MILESTONES.md.)
- 2026-07-08 — **CPM simulation engine WRITTEN (post-M4 charter; not
  release-gating; NOT RUN).** Built the plan-§10 engine under
  `devel/cpm-sim/` from the registered plan: `common.R` (BASE_SEED
  20260710, span-rule coverage, Bradley bands), `config.R` (config-table
  factorial + pseudo-truth projection γ*(P₀) with §2.4 guards / F* /
  population RMSEA / boundary-status column; ill-defined-estimand cells
  dropped at design time), `intervals.R` (shared replicate-matrix
  generator reimplementing `cpm_bootstrap`’s loop verbatim so it can
  *return* the raw replicates, + percentile/basic/BCa/studentized/Wald/
  circular-θ constructors with grouped-jackknife acceleration and
  z₀-saturation/ clamping accounting), `kernel.R` (per-fit fit-and-score
  → per-fit record, §2.5 scoring incl. removed-harmonic single score +
  one-sided decomposition), `summarize.R` (cluster-level MC intervals,
  Bradley verdict, paired contrasts over kept params, region
  aggregation), `run.R` (portable PSOCK/fork driver, per-cell
  checkpoint/resume, stage-0 benchmark, pre-registered stage-2/3
  selection rules as code; guarded so it launches nothing without
  CPM_SIM_GO). No package code changed (drives shipped API + the B6
  internal entry points). Evidence: `selftest.R` 22/22 green (span-rule
  boundary/pole cases, BCa accel vs Efron + rescaling invariance,
  BCa→percentile at a=z0=0, saturation/clamp accounting, cluster CI,
  Bradley + region verdicts); all six modules parse+load clean (24
  functions defined). Deliberately did NOT build the config table or run
  any cell/smoke/benchmark (each invokes the estimator). Five
  translation choices flagged in README for a pre-run Fable review
  (percentile- as-reconstruction coupling, Cholesky sim, no-delete-d
  accel, T=(N−1)F̂, secondary-level β removed-harmonic folding). Next:
  Fable review of those, then stage-0 benchmark on the 20-worker box.
  (devel/cpm-sim/\*, MILESTONES.md.)
- 2026-07-08 — **H-revision re-check delivered (post-M4 charter; not
  release-gating): revision CLOSES the review.** The independent
  H-review session verified the H-revision finding by finding: R1–R11
  all RESOLVED, none NOT-RESOLVED, no new defects introduced (active
  RESOLVED-BUT sweep). Both reviser judgment calls verified hard: (1)
  the clustered-angle-set substitution — the original “three displaced
  scales, one 90° gap” prose confirmed geometrically unrealizable by
  exhaustive enumeration of all 56 three-slot vacancy patterns (max-gap
  90° occurs only as multiple gaps); the pinned set’s gap arithmetic
  checks (unique 90° max gap, 20° cluster) and preserves the A-§2.5
  hazard; (2) the §6.1 region-aggregation default judged sound (errs
  conservative — fails-to-claim, never false-claims; forking paths
  closed) and rightly surfaced as §12 item 8. R2’s independent
  re-derivation confirmed correct (sharper than the review’s: t̄−t₍ᵢ₎ =
  (Sᵢ−S̄)/(N−d) exactly); R3’s symmetry fix verified coherent across
  §2.5/§4.2/§6.2 AND the analytic side (cpm_analytic_se returns SE=0 for
  polished harmonics — same verdict, R/cpm_fit.R:916/975). Two
  non-blocking wording nits noted (R7 “deduplicated”, R10 B-invariance
  overstatement). Verdict table appended to
  devel/cpm-simulation-paper-design-review.md as a dated “Re-check of
  the H-revision” section. **Design is ready for Jeff’s §12 decisions (8
  items: venue, compute, region rule chief among them).** Design not
  edited; nothing committed.
  (devel/cpm-simulation-paper-design-review.md, MILESTONES.md.)
- 2026-07-08 — **Brief H-revision delivered (post-M4 charter; not
  release-gating).** Fresh-session revision of
  `devel/cpm-simulation-paper-design.md` resolving all H-review findings
  R1–R11; none rejected. R2’s grouped-jackknife acceleration
  independently re-derived before adoption (agrees: plain BCa skewness
  formula on delete-group pseudo-values, no delete-d correction — the
  1/(N−d) constant cancels in the scale-invariant a; block-sum moments
  give skew(L)/(6√N)). Must-fixes: one MC inference interval
  (cluster-level t on per-fit proportions, Bernoulli arithmetic demoted
  to labeled planning bound); §4.3 pins the acceleration formula +
  jackknife-refit failure rule (g_used \< 50 ⇒ NA) + corrected ties
  rationale vs shipped `cpm_bootstrap`; polished-out harmonics scored
  once, attributed identically across bootstrap-family methods, excluded
  from paired contrasts (§4.2 point-mass guard scoped to kept
  parameters). Also: stage-3(g) large-N bootstrap cells (t=.05 × octants
  × N∈{5000,10000}) for RQ2’s structural claim; raw-scored
  basic/studentized with counted truncation; pre-registered
  region-aggregation rule (new §12.8 confirm/veto for Jeff);
  deterministic stage-2/3 selection scalar; jz2017 applied illustration
  (§8); cold/warm throughput split (stage 1 ~4–10 h); B=2000 sensitivity
  cell; BASE_SEED=20260710 pinned; angle sets pinned numerically
  (clustered prose was geometrically unrealizable — replaced by a pinned
  set preserving intent); θ tie rule; variant-D exclusion sentence.
  Review file untouched; nothing committed. Next: Jeff’s §12 decisions
  (now 8 items), then optional H-review re-check of the revision by the
  critic session. (devel/cpm-simulation-paper-design.md, MILESTONES.md.)
- 2026-07-08 — **Brief H-review delivered (post-M4 charter; not
  release-gating).** Fresh-session adversarial review of
  `devel/cpm-simulation-paper-design.md`; verdict **needs change —
  targeted revisions; architecture stands** (staging, estimands,
  interval set, venue all confirmed). Wrote
  `devel/cpm-simulation-paper-design-review.md`: 3 must-fix (R1
  Wilson-vs-cluster MC-interval contradiction §6.1/§6.2; R2 BCa
  grouped-jackknife acceleration formula unstated — derivation shows the
  plain formula on delete-group pseudo-values is first-order exact with
  NO delete-d correction — plus jackknife-refit failure rule and a
  factually wrong ties rationale vs shipped `cpm_bootstrap` mechanics;
  R3 polished-out harmonics scored asymmetrically across percentile/BCa
  in the paired contrasts), 5 should-fix (R4 no bootstrap cell above
  N=2000 under the “structural” RQ2 claim; R5 basic-interval
  raw-vs-truncated scoring flips verdicts at boundary truths; R6
  region-claim aggregation rule; R7 selection -rule ranking metric; R8
  BRM applied illustration), 3 hygiene (R9–R11). Design’s
  shipped-behavior claims verified against R/cpm_fit.R (post-polish
  replicate spec ~1017; pmax variance clamp ~940). Review only — design
  not edited, nothing committed.
  (devel/cpm-simulation-paper-design-review.md, MILESTONES.md.)
- 2026-07-08 — **Brief H delivered (post-M4 charter; not
  release-gating).** Wrote `devel/cpm-simulation-paper-design.md`: the
  publication-grade CPM confidence-interval simulation-study design
  extending the B6 coverage oracle (6 RQs with estimands
  incl. pseudo-truth under misspecification; staged factorial with
  pre-registered stage-2/3 selection rules; competitor intervals —
  percentile/basic/BCa/Wald, studentized targeted, BCa via grouped
  jackknife with a full-jackknife validation gate; circular θ excluded
  from order-statistic refinements by the M2 BCa-drop geometry;
  cluster-level MC error budget and pre-registered exclusion rules;
  ~1-week compute budget with knobs; venue recommendation BRM with the
  Assessment SSM-layer companion parked). Design only — no code, no
  runs. Seven §12 decisions surfaced for Jeff (venue, compute appetite,
  optional arms, compendium form).
  (devel/cpm-simulation-paper-design.md, MILESTONES.md.)
- 2026-07-08 — **M4.5 pre-freeze deferral block CLEARED (v2.0.0
  train).** Worked the entire ROADMAP M4.5 close-review follow-up set
  (not R1/R2; a pre-freeze quality pass on the unreleased structure-test
  feature). \#4: `structure_rt()`’s degeneracy guard moved off the
  loadings⁴ rotation-profile scale onto the communalities
  (`all(h2 < DEGEN_TOL)`, matching Fisher), restoring RT’s scale
  invariance — it was voiding valid-but-weak circumplexes to NA that
  Fisher/VT still define (regression test: RT of rescaled loadings).
  \#5: missing-data policy = listwise deletion by default via a new
  `listwise` arg matching
  [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  (jz2017 octants are complete, so every pin and snapshot is unchanged).
  \#13:
  [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  now errors up front when nv \> 9 without `n_perm`, before computing
  the four criteria, instead of erroring inside RANDALL. Test/quality: a
  `structure_cutoffs` almost\<thrice\<twice ordering test + a
  `stopifnot` in the derivation script; a marginal-circumplex *interior*
  MC-p reproducibility test (index ≈ .04, p ≈ .40, not pinned at the
  add-one floor); an exact-path `.Random.seed`-not-created test; the
  `data[scales]` matrix-misindex fixed via a shared `structure_cormat()`
  (which also single-sources the correlation construction, retiring the
  double `cor`); and the summary/plot angle-communality geometry DRY’d
  into `structure_geometry()`. Evidence: full suite 0F/1773P,
  print/summary snapshots byte-identical, R CMD check 0/0/0,
  `/code-review` high-recall pass clean. (R/fit_structure.R,
  R/fit_structure_oop.R, data-raw/structure-test-cutoffs.R,
  tests/testthat/test-fit_structure.R,
  tests/testthat/test-fit_structure_api.R, NEWS.md,
  man/fit_structure.Rd, ROADMAP.md.)
- 2026-07-08 — **B6 analytic-CI caution CLOSED.** Jeff ratified the
  marker-validation memo’s verdict: ship `cpm_boundary_markers()`
  unchanged (β = 0.10 cut and the `multimodal` marker both confirmed
  empirically; see the prior log entry and
  `devel/cpm-marker-validation.md`). No code, constant, or
  [`summary()`](https://rdrr.io/r/base/summary.html) wording change
  follows. ROADMAP.md’s B6 item marked RATIFIED, CLOSED. This was the
  last open piece of the v2.0.0 pre-release B6 item.
- 2026-07-08 — Marker-validation run executed (Fable, per the brief):
  the measurement the B6 pre-release item was waiting on is done,
  verdict **ship-unchanged** — the item itself stays open until Jeff
  ratifies. 70k analytic-only cormat-path fits (7 configs × 5 N × 2000
  reps, ~1 h) measured per-marker conditional coverage across the
  2000–50000 band. Both judgment calls survive: β = 0.10 is the only cut
  that discriminates in the right direction (0.05 is affirmatively
  backwards — mis-coverage peaks *near* the boundary at trailing β ≈
  .05, not at it; at-boundary truths cover nominally), and `multimodal`
  fits mis-cover ζ (.815 vs .933) with ~zero false alarms. New finding:
  heywood/illcond fits’ Wald CIs are 65–78% NaN (indefinite Hessian) —
  the markers flag CIs that often don’t exist. `removed` is a predictive
  null but behaviorally inert (implies the β marker) — kept. Any-marker
  discrimination vanishes exactly at the 50000 gate. Scope addition per
  the brief’s provision: one ζ = 0.97 provocation config, since
  heywood/illcond fire ≤ 4% under ζ = 0.75. Awaiting Jeff’s sign-off on
  the memo verdict; no code change follows.
  (devel/cpm-marker-validation.R, .md, -summary.rds; per-fit record
  regenerable from BASE_SEED 20260708.)
- 2026-07-08 — B6 analytic-CI caution partially ratified +
  marker-validation brief written (release-gating, pre-freeze). Jeff
  confirmed the two N thresholds (2000 / 50000) and the caution tone;
  the open piece is the `cpm_boundary_markers()` set as a runtime
  predictor (the β = 0.10 cut and the reasoned-in `multimodal` marker),
  which he lacked the expertise to adjudicate by judgment. Reframed as
  an empirical question and scoped a release-sized, analytic-only run
  (no bootstrap, ~10–20 min vs the 3–4 h B6 run) that measures coverage
  conditional on each fired marker across the 2000–50000 band, plus
  β-cut and multimodality sweeps. Wrote the Fable brief and queued it.
  Fallback if not run by freeze: ship the conservative marker superset
  as-is (the caution is advisory).
  (devel/cpm-marker-validation-brief.md, devel/fable-briefs-2026-07.md,
  ROADMAP.md.)
- 2026-07-08 — R1 VERIFIED + M5 landed on master. PR \#29 (cpm_pack
  fix + CI-portability skips) merged green; PR \#30 (all of M5, rebased
  on \#29) opened, and its first pass through the full CI matrix — which
  the CI-blocked m5-sem-ssm branch had never had — surfaced three real,
  previously-hidden portability defects, each fixed and reproduced under
  reference (netlib) BLAS in a rocker/r-ver container: (a) the three
  `ssm_sem*` exports were missing from `_pkgdown.yml`’s reference index
  (build_reference_index() error); (b) a knife-edge boundary test built
  a population with ρ\*\_1 == 1 exactly, recovered as 1 ± ~1e-7, so
  which sec-4.5 guard fired (point-guard vs draw escalation) flipped by
  platform — rebuilt so ρ\*\_1 ≈ 1.05, robustly over the boundary; (c)
  [`ssm_sem_syntax()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_syntax.md)
  emitted mathematically-zero cos/sin loadings (e.g. cos 90°) as ~1e-16
  libm noise whose low bits differ across platforms’ math libraries,
  breaking the byte-identical emission on Windows — added `snap_trig()`
  to snap exact 0/±1 loadings (cleaner and more correct too; verified
  the snapped syntax still fits). Also hardened the bootstrap-covariance
  advisory test to be deterministic (a se=“standard” fit relabeled to
  “bootstrap”) after lavaan’s small-sample bootstrap vcov recomputation
  threw an internal “model is NULL” on generated-syntax models in the
  full-suite RNG state. PR \#30 merged green across the full matrix
  (macOS/Windows/ubuntu ×3/covr/pkgdown); master is now green on all
  platforms for the first time since M4, clearing the ROADMAP
  CI-portability release blocker. Only R2 (the v2.0.0 release checklist)
  remains before the CRAN submission. (R/cpm_fit.R, R/ssm_sem_syntax.R,
  tests/testthat/test-cpm_fit.R, tests/testthat/test-ssm_sem.R,
  tests/testthat/test-ssm_sem_syntax.R, \_pkgdown.yml, ROADMAP.md,
  MILESTONES.md.)
- 2026-07-08 — R1 (local + container evidence complete; box stays open
  until the ubuntu CI jobs verify it post-push): diagnosed and fixed the
  cpm_pack β-boundary error. Root cause found by container reproduction
  (rocker/r-ver amd64, R 4.6.1): the LS start coefficient for a harmonic
  absent from the population is analytically zero, and the BLAS decides
  its floating-point fate — exact 0.0 under the reference netlib BLAS
  (the ubuntu runners; reproduced: raw LS β₃ = 0.0e0 on the pole
  population), ±1e-16 under OpenBLAS/Accelerate (verified: +1.7e-16).
  The start-value clamp `beta0[beta0 < 0] <- 0.01` missed exact zeros,
  which reached cpm_pack’s softmax inverse
  (`stopifnot(all(b_keep > 0))`) — brief question 2 answered: case (c)
  start values, the ONLY hole (optimizer works in softmax space,
  interior by construction; converged solutions are never re-packed —
  the three production cpm_pack callers are all start sites); question
  3: the stopifnot is the right invariant, fix belongs upstream. Fix:
  extracted `cpm_beta_start_interior()`, flooring surviving exact zeros
  to 0.01 (same treatment as their analytically identical negative
  twins), clamp order chosen so every previously non-crashing input is
  byte-identical (negatives, all-zero fallback, NA fallback preserved
  exactly). Evidence: byte-identical parity on seeded raw-bootstrap +
  cormat + engine fits vs saved pre-fix references; container full suite
  under reference BLAS with CI=true green (0 fail / 101 skip / 1143 pass
  — the runner config); OpenBLAS + macOS suites green (local 1744 pass);
  R CMD check 0/0/0; /statistical-validation 7/7 on both platforms vs a
  hand-written implied-correlation reference (one initial sweep failure
  was the validation script violating cpm_spec’s identification cap —
  invalid reference, corrected); platform-independent regression tests
  pin the exact-zero helper contract and the vanishing-harmonic start
  invariant. Fix committed on ci-cross-platform (924601c, where PR
  \#29’s CI verifies it); m5-sem-ssm rebased onto it, m5-complete
  retagged to the rebased tip. /code-review high: no findings.
  (R/cpm_fit.R, tests/testthat/test-cpm_fit.R, MILESTONES.md.)

# Completed milestones

Archived with their full logs to **MILESTONES-ARCHIVE.md** (M1 → v1.2.0;
M2+M3 → GitHub-complete, bundled into the held v1.3.0; M4 and M4.5 →
GitHub-complete 2026-07-07, M5 → GitHub-complete 2026-07-08; M4–M5 all
fold into v2.0.0). When the active milestone completes, the
milestone-close archive step (or `/release-checklist` at a CRAN release)
moves it there too. This file stays scoped to the active milestone so it
is cheap to re-read at the start of each task.
