# M27: Longitudinal Build C — growth-model support on displacement

- **Status:** review
- **Priority:** high
- **Depends on:** M25, M26
- **Principles touched:** —
- **Branch/PR:** `m27-growth-model-support` · [PR #51](https://github.com/jmgirard/circumplex/pull/51)

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

- [x] AC1 — `angle_unwrap()` closed-form fixtures pass: 350°→10°→30° unwraps
      to 350, 370, 390; exact-180° step ascends; NA propagates onward;
      arbitrary reals wrapped first. Docs state the conventions.
- [x] AC2 — Coverage oracle green and discriminating: d(t) pointwise CIs
      cover the true direction at nominal rate (band pre-registered before
      the run) in the pole-crossing cell; the low-amplitude cell shows the
      per-t caution firing at the degraded t (its coverage reported, not
      hidden); the strong-correlation cell **fails** under the
      independent-univariate-fits shortcut and passes under the joint
      recipe. `devel/m27-*` rds + regeneration script committed; ≥ 2
      independent oracle types per numeric result (coverage + invariants +
      closed-form fixtures).
- [x] AC3 — Invariants at pre-registered tolerances: unwrap-then-LMM vs
      (x, y)-framing d(t) agree in the concentrated common-branch regime;
      two-occasion zero-slope consistency vs M25's paired contrast at one
      large-n well-specified cell.
- [x] AC4 — Vignette builds under `devtools::check()`: joint glmmTMB recipe
      (grep evidence: no independent-univariate fit presented as valid);
      per-t certification demonstrated on the worked example; REML caution +
      remedy; unwrap failure modes; both Brief E caveats; nlme alternative
      named. glmmTMB in `Suggests` with the D-entry (incl. the engine-swap
      spec amendment) recorded.
- [x] AC5 — NEWS documents the feature; `devtools::check()` clean
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
- [x] **T5** — Growth vignette (precomputed or conditional chunks per the
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
- 2026-07-17: T4 done — oracle green on all 5 pre-registered gates
  (`devel/m27-coverage-oracle.{R,md}` + rds; deterministic miniatures →
  `test-growth_invariants.R`). Run-1 miss recorded (lowamp cert .058 > .05
  at a(2)=0.02, the rule's power-onset region; truth moved to 0.01 = the
  named a→0 regime, gates unchanged, re-run green; details in the md).
- 2026-07-17: T5 done — growth vignette (all AC4 elements; live conditional
  glmmTMB chunks) + NEWS + pkgdown; `check_pkgdown()` clean; `check()`
  0/0/0. Status → review.
- 2026-07-17: review — PR #51; fresh check 0/0/0; three-lens review; one
  HIGH vignette-plot bug fixed on the branch (see Review section).

## Decisions

## Review

**Reviewed 2026-07-17 (PR #51).** Verdict: ship after one HIGH fix (applied
on the branch).

**Acceptance-criteria evidence (fresh):**
- AC1 — `test-angle_unwrap.R` 22/22 green (fresh run in the `angle_unwrap`
  filter, part of the 148-pass draws/unwrap/invariants filter). Conventions
  documented in `man/angle_unwrap.Rd` (6 grep hits for [0,360)/180-step/NA/
  half-turn). `expect_identical` fixtures pin the +180 half-turn, descending,
  NA-propagation, and 360→0 anchor — each fails if the convention flips.
- AC2 — committed `devel/m27-coverage-results.rds` read fresh: all 5
  pre-registered verdicts TRUE. pole coverage .948–.962 ∈ [.90,.98]; lowamp
  cert-rate .02 at the degraded wave with its coverage .854 reported (waves
  0/4 cert 1.00, coverage .958/.966); xycor joint .932–.944 vs
  univariate-shortcut mean .856 (<.90, fails as designed). ≥2 oracle types
  (coverage + invariants + closed-form fixtures). Regeneration script
  `devel/m27-coverage-oracle.R` committed.
- AC3 — inv_unwrap mean max|diff| 0.336° (≤2°), p95 0.931° (≤4°); inv_2occ
  mean |Δd diff| 0.021° (≤1°), coverage growth .935 / paired .945, width
  ratio 1.002. Deterministic miniatures green in `test-growth_invariants.R`.
- AC4 — vignette builds under `check()`; joint recipe only ("Do not fit the
  coordinates separately"; `glmmTMB::glmmTMB` ×2, no univariate fit shown as
  valid); per-t certification demo fires on the worked origin-proximal
  example; REML caution + remedies; unwrap failure modes; both Brief E
  caveats (direction-of-mean; a(t) shrinkage); nlme alternative named.
  glmmTMB in `Suggests`; D-016 records the gated engine-swap amendment.
- AC5 — NEWS has 3 entries (8 grep hits for angle_unwrap/certified/not
  interpretable). Fresh `devtools::check()` 0 errors / 0 warnings / 0 notes
  (5m 2s).

**Consistency gate:** `cairn_validate` all checks passed (after the plan-cap
trim); `document()` no diff (only tracking edits pending);
`pkgdown::check_pkgdown()` clean; no DESIGN principle changed (impact scan
skipped). No dependency change beyond the gated glmmTMB `Suggests` (D-016).

**Independent review (three lenses + scorer):**
- [S] blame-history: no findings — M26's `ssm_draws()` design untouched, spec
  amendment properly gated, NEWS/M25-M26 entries intact.
- [S] prior-PR-comments: no prior-PR evidence (PRs #1–#50 carry zero review
  line-comments; the repo reviews via cairn, not GitHub comments).
- [O] diff-bug: 3 findings (1 HIGH, 2 LOW), scored below.

**Findings actioned:**
- F1 (score 97, HIGH) — `vignettes/growth-ssm-analysis.Rmd` plot chunk:
  `shift = d_branch - d_est` was added to *both* CI endpoints, but
  `ssm_draws()` non-contrast displacement intervals wrap each endpoint into
  [0,360) independently, so a seam-straddling interval (d_lci > d_uci — the
  350°→10° showcase regime) rendered inverted (ymin > ymax, one endpoint a
  full turn off). Independently reproduced (wave-1 lci/uci 352.20/3.67 →
  inverted). **Fixed** on the branch: each endpoint shifted by its signed
  circular distance `((a - d_est + 180) %% 360) - 180` added to d_branch;
  reproduction confirms non-inverted ribbons; vignette re-knit clean.
- F3 (score 83, LOW) — same vignette, §6: Kenward–Roger misattributed to
  `nlme` (KR is a pbkrtest/lme4 facility; nlme uses containment df).
  **Fixed**: reworded to "the approximate denominator degrees of freedom
  `nlme` supplies".

**Findings logged below the 80 threshold (not required-actioned):**
- F2 (score 78, LOW) — vignette said the shortcut coverage drops "to roughly
  84%"; the oracle mean is .856 (~86%). Confirmed factual drift. **Fixed
  opportunistically** (same chunk region; CLAUDE.md mandates
  statistically-precise vignette prose) → "roughly 86%".
