# M27: Longitudinal Build C — growth-model support on displacement

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M25, M26
- **Principles touched:** —
- **Branch/PR:** `m27-growth-model-support`

## Goal

Ship growth-model support on displacement — `angle_unwrap()`, the joint
(x, y) growth-recipe vignette with draws-adapter propagation and the per-t
amplitude certification caution — coverage-oracle validated per the binding
D-013 spec (`devel/longitudinal-ssm-spec.md` §4, §7 Build C).

## Scope

**In:**
- Exported `angle_unwrap(x)` helper: degrees in, wrapped to [0, 360) first
  (any reals accepted), cumulative `angle_dist()` between successive points;
  exact-180° steps ascend (+180 convention); NA propagates from the missing
  wave onward (spec §4.3 build pins).
- Per-t D-007 amplitude certification caution: at each t the summary applies
  the shipped scale-free rule (`a_lci/(a_uci − a_lci) ≥ 0.35`) to the a(t)
  draws and flags uncertified t; the vignette states d(t) intervals at
  uncertified t are not interpretable (spec §4.1).
- Growth vignette: the **joint** (x, y) reference recipe — **glmmTMB**, a
  gated spec amendment (spec §4.1 names nlme; the reviewed *holding* is
  joint-fitting, engine-agnostic; Jeff's plan-gate preference 2026-07-16;
  D-entry with the `Suggests` addition) — with nlme named in one line as the
  base-R alternative; fixed-effect draws → `ssm_draws()` → (a(t), d(t))
  circular summaries; REML small-N caution + user-side remedy named;
  unwrap-then-LMM as a documented recipe with failure modes stated;
  bpnreg referenced as the model-based upgrade, not wrapped; the two Brief E
  caveats (direction-of-mean vs mean-of-directions; a(t) shrinkage)
  documented.
- glmmTMB → `Suggests` (dependency gate satisfied at the 2026-07-16 plan
  gate; D-entry recorded in this milestone).
- Coverage oracle per spec §4.2: simulate from the same model family the
  reference recipe fits; three named cells — pole-crossing (350°→10°, the
  boundary headline), low-amplitude/origin-proximal (the statistical danger
  cell; the caution must demonstrably fire), strong x–y fixed-effect
  correlation (the independent-univariate-fits shortcut must *fail* it);
  concentrated-regime unwrap-vs-(x, y) agreement invariant; two-occasion
  zero-slope consistency check against M25's paired machinery at a
  pre-registered tolerance (one large-n well-specified cell — different
  estimators, asymptotic agreement only). Results rds + seeded regeneration
  script committed as `devel/m27-*` (level-indexed seeds).

**Out:**
- Fitting mixed models in-package (no lme4/nlme/glmmTMB Import — minimal-deps
  doctrine; spec §4.1). `Suggests` + vignette only.
- Projected-normal (bpnreg) wrapper / Stan companion → spec §5.4 stay-out
  criteria (ROADMAP candidate).
- Any occasions-API or adapter change beyond a small internal helper the
  per-t caution needs → M25/M26 own those surfaces.

## Acceptance criteria

- [ ] AC1 — `angle_unwrap()` closed-form fixtures pass: 350°→10°→30° unwraps
      to 350, 370, 390; exact-180° step ascends; NA propagates onward;
      arbitrary reals wrapped first. Docs state the conventions.
- [ ] AC2 — Coverage oracle green and discriminating: d(t) pointwise CIs
      cover the true direction at nominal rate (band pre-registered before
      the run) in the pole-crossing cell; the low-amplitude cell shows the
      per-t caution firing at the degraded t (its coverage reported, not
      hidden); the strong-correlation cell **fails** under the
      independent-univariate-fits shortcut and passes under the joint
      recipe. `devel/m27-*` rds + regeneration script committed; ≥ 2
      independent oracle types per numeric result (coverage + invariants +
      closed-form fixtures).
- [ ] AC3 — Invariants at pre-registered tolerances: unwrap-then-LMM vs
      (x, y)-framing d(t) agree in the concentrated common-branch regime;
      two-occasion zero-slope consistency vs M25's paired contrast at one
      large-n well-specified cell.
- [ ] AC4 — Vignette builds under `devtools::check()`: joint glmmTMB recipe
      (grep evidence: no independent-univariate fit presented as valid);
      per-t certification demonstrated on the worked example; REML caution +
      remedy; unwrap failure modes; both Brief E caveats; nlme alternative
      named. glmmTMB in `Suggests` with the D-entry (incl. the engine-swap
      spec amendment) recorded.
- [ ] AC5 — NEWS documents the feature; `devtools::check()` clean
      (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T1
- AC2 → T3, T4
- AC3 → T4
- AC4 → T3, T5
- AC5 → T5

## Tasks

- [x] **T1** — `angle_unwrap()` + closed-form fixtures, tests first
      (`angle_dist()` at `R/utils.R:65-69` is the building block).
- [x] **T2** — Per-t certification caution mechanism (small helper applying
      the D-007 rule to a(t) draw summaries) + tests that it flags a
      constructed uncertified t and stays silent on a certified one.
- [x] **T3** — Reference joint glmmTMB recipe + adapter pipeline as a dev
      script; record the engine-swap gated amendment (work-log line per
      D-013 re-trigger) and the glmmTMB `Suggests` D-entry.
- [x] **T4** — Coverage oracle: seeded script (smoke-first, level-indexed
      seeds, pre-registered band/tolerances), three cells + both invariants,
      committed `devel/m27-*` rds + analysis; deterministic pieces into
      testthat.
- [ ] **T5** — Growth vignette (precomputed or conditional chunks per the
      r-package profile) + docs + NEWS; full `devtools::check()`.

## Work log

- 2026-07-16: created by /milestone-plan (Build C of the D-013 contract;
  promoted from the "Longitudinal SSM build family" candidate row). Depends
  on M26 (adapter) **and M25** (the §4.2 two-occasion consistency check
  needs the paired machinery — a plan-level refinement of spec §7's
  "depends on B"). Engine choice glmmTMB-over-nlme made at the plan gate
  (Jeff); recorded as a gated spec amendment + D-entry at T3.
- 2026-07-16: implement gate (Jeff): per-t caution surfaces as a
  print/summary note + stored flag on `circumplex_ssm_draws` (D-007 rule);
  growth vignette uses live conditional glmmTMB chunks; vignette named
  `growth-ssm-analysis`. Branch cut; spec's DESIGN oracle-registry gap
  already closed at M25.
- 2026-07-16: T1 done — `angle_unwrap()` exported (degree-exact arithmetic,
  +180 half-turn convention, NA-propagation), 22 fixtures green; full suite
  2562 pass / 4 pre-existing warnings (test-ci_accuracy.R).
- 2026-07-16: T2 done — `circumplex_ssm_draws` gains `details$certified`
  (D-007 rule via `ssm_certified()`, single definition) + print/summary
  caution note; 5 new tests (certified silent / uncertified flags /
  zero-width fails closed / both shapes / flag≡rule); suite 2571 green.
- 2026-07-16: T3 done — reference joint glmmTMB recipe end-to-end in
  `devel/m27-growth-recipe.R` (joint fit carries Cov(x̂,ŷ) ≠ 0; trajectory
  tracks truth; all waves certified). **Gated spec amendment** (D-013
  re-trigger): spec §4.1/§4.2 reference engine nlme → glmmTMB, joint-fitting
  holding untouched; glmmTMB → Suggests; D-016 recorded.
- 2026-07-17: T4 done — coverage oracle green on all 5 pre-registered gates
  (`devel/m27-coverage-oracle.{R,md}` + results rds): pole .948–.962;
  lowamp caution fires .98 at the degraded wave (its coverage .854,
  reported); xycor joint .932–.944 vs univariate-shortcut .856 (fails, as
  designed); unwrap agreement ≤0.34° mean; two-occasion vs M25 paired
  0.021° mean diff. **Run-1 miss recorded**: first full run had lowamp
  truth a(2)=0.02 (~0.9 SE, rule's power-onset region) → cert rate .058 >
  .05 gate; cell truth moved to 0.01 (the named a→0 regime), gates
  unchanged, re-run green. Deterministic miniatures →
  `test-growth_invariants.R` (5 tests); suite 2576 green.

## Decisions

## Review
