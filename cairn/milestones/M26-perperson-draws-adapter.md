# M26: Longitudinal Build B — per-person layer + draws adapter + Bayesian vignette

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** `m26-perperson-draws-adapter` · https://github.com/jmgirard/circumplex/pull/50

## Goal

Ship the intraindividual descriptive layer (per-person SSM scoring with
circular summaries) and the `ssm_draws()` adapter (parameter- and
profile-draw shapes), finished with the precomputed brms vignette, per the
binding D-013 spec (`devel/longitudinal-ssm-spec.md` §§3, 5, §7 Build B).

## Scope

**In:**
- Exported per-person scoring wrapper (spec §3.2 shape:
  `ssm_parameters_id(data, scales, angles, id = NULL)`; **name final at the
  pre-implementation gate** — an exported name is permanent
  (RB tripwire: irreversible-api)), returning the per-person parameter table
  (e_i, x_i, y_i, a_i, d_i) with the standing degenerate-profile NA
  semantics and NA counts reported as a column, never a silent drop.
- Group-level summaries of per-person parameters: circular mean + resultant
  length for d (never arithmetic angle means), NA d_i stripped with count
  reported before circular aggregation (`angle_mean()` has no `na.rm`);
  aggregation caveat documented (spec §3.1/§3.3).
- `ssm_draws(draws, angles = NULL, interval = 0.95)` per spec §5.1: shape A
  (parameter draws, columns (e, x, y)) and shape B (profile draws + angles);
  dispatch rules incl. the **explicit `type` requirement in the
  ncol = 3 / no-angles cell**; column-mapping message when colnames are
  unrecognized; summaries via the existing `ssm_replicate_intervals()` path
  with d draws classed `circumplex_radian`; the four managed leaks
  (`replicate_label = "posterior draws"`, per-parameter NA semantics
  documented, `t0` = the adapter's own point summaries, shape A synthesizes
  the 6-column `ssm_param_names()` layout with `fit = NA`); point summaries
  = medians + circular mean for d, coherence caveat documented.
- Oracles per spec §3.3 + §5.5 (detailed in the ACs).
- `bayesian-ssm-analysis.Rmd` (spec working name `bayesian_ssm.Rmd`):
  precomputed (brms never runs on CRAN builders); brms →
  `Suggests` (pre-cleared at the 2026-07-16 plan gate; **D-entry recorded in
  this milestone**); the devel sketch treated as untrusted (its line-43
  derivation comment has swapped atan2 args); mapping derived fresh;
  prior-predictive simulation exhibiting the Rayleigh-shaped induced prior
  on a (spec §5.2/§5.3).
- NEWS + docs: induced-prior footnote, mean-resultant ≤ mean-amplitude
  caveat, circular-mean-of-d_i ≠ group-profile-d documentation.

**Out:**
- Pooled/shrunken per-person estimates → Stan-companion stay-out criteria
  (spec §5.4; ROADMAP candidate).
- Growth helpers, vignette, and the per-t amplitude certification caution →
  M27 (the caution is exercised per-t there; any small adapter-internal
  helper it needs lands with it).
- occasions API → M25.

## Acceptance criteria

- [x] AC1 — Per-person wrapper reproduces hand-computed closed-form fixtures
      (arithmetic in test comments) for 2–3 synthetic persons incl. one
      exactly-flat and one pure-second-harmonic person (NA semantics); NA
      rate reported as a column.
- [x] AC2 — Invariants per spec §3.3, test-evidenced on heterogeneous data:
      linearity (mean of per-person (e_i, x_i, y_i) equals the group path's
      (e, x, y) exactly); Jensen (group amplitude ≤ mean per-person
      amplitude, strict under directional dispersion); identical-profiles
      reproduction; circular mean recomputed by hand in the test (atan2 of
      summed sines/cosines, never via `angle_mean()`); the differ-fixture
      (circular mean of d_i ≠ displacement of the group mean profile on a
      heterogeneous sample) as the anti-confusion regression.
- [x] AC3 — Dispatch contract per spec §5.1, each branch tested: angles
      supplied → shape B requiring `ncol(draws) == length(angles)`;
      `angles = NULL` + ncol ≠ 3 → error naming both shapes;
      `angles = NULL` + ncol == 3 → explicit `type` required (error naming
      the ambiguity); unrecognized colnames → assumed-mapping message.
- [x] AC4 — Adapter oracles per spec §5.5: feeding the bootstrap replicate
      matrix of an existing `ssm_analyze()` run reproduces that run's
      intervals **exactly**; shape B equals shape A applied to the per-row
      (e, x, y) (exact); hand-computed 4-row fixtures incl. a pole-straddling
      draw pair (CI must wrap) and an all-flat matrix (all-NA contract);
      exact-pole summaries report 360 (D-003/M20).
- [x] AC5 — Summary honesty, snapshot-evidenced: warnings say "posterior
      draws" (never "bootstrap resamples"); `t0` is the adapter's own point
      summaries; a summarized by median, d by circular mean, with the
      coherence caveat in the docs.
- [x] AC6 — `bayesian-ssm-analysis.Rmd` builds under `devtools::check()`
      precomputed; brms in `Suggests` with the D-entry recorded; the
      vignette contains the prior-predictive Rayleigh exhibit and a fresh
      (not sketch-copied) atan2 derivation pinned by a known-direction
      fixture.
- [x] AC7 — NEWS documents both features; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T3, T5
- AC5 → T4
- AC6 → T6
- AC7 → T7

## Tasks

- [x] **T1** — Per-person wrapper, tests first (closed-form fixtures);
      exported name settled at the pre-implementation gate
      (RB tripwire: irreversible-api). `ssm_score()`/`ssm_parameters()`
      (`R/ssm_analysis.R:779,854`) are the building blocks.
- [x] **T2** — Summary layer: circular stats, NA stripping + counts, the
      §3.3 invariant suite (linearity, Jensen, hand-recomputed circular
      mean, differ-fixture).
- [x] **T3** — `ssm_draws()` core: shape A/B transforms + dispatch rules,
      tests first for every dispatch branch.
- [x] **T4** — Adapter summary path through `ssm_replicate_intervals()`
      (`R/ssm_bootstrap.R:136-144`) with the managed leaks; print/summary
      snapshots.
- [x] **T5** — Adapter oracle suite: bootstrap-replicate exact reproduction,
      shape A/B consistency, 4-row fixtures, boundary regressions
      (pole-straddle wrap, all-flat, pole = 360).
- [x] **T6** — `bayesian-ssm-analysis.Rmd` precomputed + brms `Suggests` +
      D-entry (dependency gate satisfied at the 2026-07-16 plan gate);
      untrusted-sketch note honored; prior-predictive exhibit.
- [x] **T7** — Docs + NEWS; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (Build B of the D-013 contract; promoted from the "Longitudinal SSM build family" candidate row); brms `Suggests` pre-cleared at the plan gate (D-entry lands with T6).
- 2026-07-16: implementation started; branch cut; pre-implementation gate resolved the wrapper name, summary home, and draws return class (see Decisions).
- 2026-07-16: T1 done — `ssm_parameters_id()` tests-first (closed-form fixtures incl. flat + pure-second-harmonic, id aggregation, na_rate column, NA-id error, 0-row edge); full suite green.
- 2026-07-16: T2 done — `summary.circumplex_ssm_id()` (circular mean + resultant length, NA-d strip with count) + the §3.3 invariant suite (linearity, Jensen, identical-profiles, hand-recomputed circular mean, differ-fixture).
- 2026-07-16: T3+T4 done — `ssm_draws()` with the §5.1 dispatch contract (every branch tested first); `ssm_replicate_intervals()` gained honest-wording args (t0_warning, interval_label, structural_na; byte-identical defaults); print/summary snapshots.
- 2026-07-16: T5 done — oracle suite: run's bootstrap replicates reproduce intervals bit-exactly; shape B ≡ shape A; pole-straddle wrap fixture (hand type-7 arithmetic); all-flat all-NA contract; exact-pole 360; found+fixed R `%%` vs kernel modu() tiny-negative pole disparity (single-correction wrap, regression-tested).
- 2026-07-16: T6 done — precomputed vignette (only the `brm()` chunk frozen; executable atan2 known-direction pin + Rayleigh prior-predictive exhibit live); seeded fixture `vignettes/bayesian_ssm_draws.rds` + generator `data-raw/bayesian_ssm_draws.R`; brms → Suggests (D-015); pkgdown navbar; dev-namespace render verified; minor amendment: filename kebab-cased per repo convention (spec's `bayesian_ssm.Rmd` was a working name), AC6/Scope wording updated.

- 2026-07-16: T7 done — NEWS (both features), DESIGN.md data-flow/class/RNG rows; `devtools::check()` clean (0 errors / 0 warnings / 0 notes, 4m28s, vignettes rebuilt); pkgdown::check_pkgdown() clean; status → review.
- 2026-07-16 (review): plan-owned body hit the 150-line cap at the gate; the standalone minor-amendment work-log line was folded losslessly into the T6 line (same facts, one fewer line); cairn_validate green after.

## Decisions

- 2026-07-16 (pre-implementation gate): exported per-person wrapper named
  `ssm_parameters_id()` (spec §3.2 sketch confirmed; the irreversible-api
  tripwire resolved without escalation). Group-level circular summaries live
  in a `summary()` method on the lightly classed per-person table (class
  `circumplex_ssm_id` on a plain data frame; no second exported name).
  `ssm_draws()` returns a standalone `circumplex_ssm_draws` object with its
  own print/summary saying "posterior draws"/"credible interval";
  `circumplex_ssm` subclassing deferred as a non-breaking future addition.

## Review

Fresh evidence gathered 2026-07-16 (this session, by command; branch
m26-perperson-draws-adapter @ 1fc3af0, PR #50 draft).

- AC1 evidence: `test-ssm_parameters_id.R` fresh run — 74 pass / 0 fail.
  Closed-form fixture test covers 3 synthetic persons with arithmetic in
  comments (pure first harmonic e=2,x=3,y=4,a=5,d=53.13,fit=1; exactly-flat
  → d/fit NA; pure second harmonic → d NA, fit 0) and asserts the na_rate
  column (0, 1/16, 2/16 cases in the missingness tests).
- AC2 evidence: same fresh run. Linearity asserted exactly against
  `ssm_parameters(colMeans(...))` on seeded heterogeneous data; Jensen via
  strict `expect_lt`; identical-profiles reproduction parameter-by-
  parameter; circular mean recomputed by hand (atan2 of summed sin/cos,
  never `angle_mean()`); differ-fixture asserts 135° (equal-weight circular
  mean) vs 101.31° (group-profile displacement), gap > 30°.
- AC3 evidence: `test-ssm_draws.R` fresh run — 112 pass / 0 fail. Each
  dispatch branch has its own test: shape-B ncol/angles length error;
  both-shapes error naming "parameter draws"+"profile draws"+`angles`;
  ambiguity error requiring `type`; contradictions (profiles w/o angles,
  parameters w/ angles, parameters ncol≠3); colname-mapping message matrix
  (recognized brms names silent, odd names messaged, unnamed silent,
  mapping invariant to names).
- AC4 evidence: same run. Bootstrap-replicate oracle reproduces an
  `ssm_analyze()` run's e/x/y/a/d interval endpoints via `expect_identical`
  (bit-exact); shape B ≡ shape A via `expect_identical` on draws + all
  est/lci/uci; 4-row pole-straddle fixture with hand type-7 arithmetic
  (lci 340.75 > uci 19.25, wrap asserted); all-flat matrix → d/fit
  est+CI all NA with e/x/y/a defined; exact-pole draws → est/lci/uci all
  360; tiny-negative modu-parity regression (360, never 0).
- AC5 evidence: same run + snapshots (`_snaps/ssm_draws.md` shows
  "Posterior Draws" basis and "Lower/Upper CrI" columns). Warning tests
  assert "1 of 3 posterior draws" + "credible interval" and
  `expect_no_match` on "bootstrap"/"confidence"; t0-medians test asserts
  each point summary equals the hand median (+ hand circular mean for d)
  and the marginal-coherence inequality; coherence caveat present in
  `ssm_draws.Rd` and the vignette.
- AC6 evidence: `devtools::check(args = "--no-manual")` this session —
  0 errors / 0 warnings / 0 notes, "re-building of vignette outputs ... OK"
  (package files at check time identical to HEAD; only cairn/ tracking
  changed after). brms in Suggests (DESCRIPTION:43); D-015 recorded
  (DECISIONS.md:376); vignette contains the Rayleigh prior-predictive
  exhibit (lines 160–176) and the fresh atan2 derivation pinned by two
  executable `stopifnot()` known-direction checks (correct → 90, swapped
  → not 90).
- AC7 evidence: NEWS.md documents both features (lines 3–38); the same
  check run is clean 0/0/0 (4m27.7s). Full suite fresh run: 2539 pass /
  0 fail (4 warnings are the pre-existing baseline).

Consistency gate (2026-07-16, by command):
- `cairn_validate.py`: all checks passed (after trimming the plan-owned
  body back under the 150-line cap — two same-day work-log lines merged
  losslessly; logged below).
- No IP/GP changed → `cairn_impact` skipped.
- Toolchain slot: `document()` no-diff CLEAN; `pkgdown::check_pkgdown()`
  "No problems found"; README.Rmd untouched by the diff and in its last-
  reviewed state; NEWS entry present; no new top-level files (check ran
  0 notes); full check clean (above).
- Review-side cap fix logged: work-log amendment fold (see work log).

Independent review: [pending — three-lens fan-out running]
