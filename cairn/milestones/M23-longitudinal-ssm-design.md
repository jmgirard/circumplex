# M23: Longitudinal & intraindividual SSM — Fable-reviewed design + build-ready spec

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m23-longitudinal-ssm-design · https://github.com/jmgirard/circumplex/pull/47

## Goal

Turn Brief E's longitudinal/intraindividual design directions
(`devel/m5-m6-design-questions.md` Q6.1–Q6.3, now unblocked — the M4
dependencies are discharged) into one Fable-reviewed, build-ready spec, so
build milestones can be planned without re-opening design questions.

## Scope

**In:**
- Design/spec doc `devel/longitudinal-ssm-spec.md` covering five interlocking
  components, each with an argued decision and API sketch:
  1. repeated-measures API shape (id/timepoint structure in `ssm_analyze()`
     or a new entry point; resampling unit = person),
  2. person-level case (cluster) bootstrap for timepoint contrasts
     (strata = group, unit = person, extending the `boot::boot` design) +
     the Monte Carlo stacked-timepoint analogue + missing-wave semantics
     (deliberate extension of `listwise`),
  3. growth models on displacement — bivariate (x, y) framing primary per
     Brief E; unwrap-then-LMM as a documented recipe; projected-normal noted
     as the Bayesian upgrade,
  4. intraindividual SSM (per-person parameters from intensive longitudinal
     data, multilevel summaries),
  5. Bayesian boundary: thin draws adapter (posterior (e, x, y) draws → SSM
     draws + circular summaries) + finished brms vignette in; Stan companion
     package out, with decision criteria recorded.
- Validation strategy in the spec: named oracle strategy per numeric
  component, ≥2 independent oracle types (simulation-coverage primary for
  interval methods, generalized over time) per the validation doctrine.
- Independent Fable review via the RB/RR protocol (RB06), ingestion, and the
  cross-cutting design D-entry.
- Build-milestone candidate rows registered in ROADMAP (remainder routing).

**Out:**
- All implementation → build milestones planned after this spec lands
  (ROADMAP candidate row until then; not merge-gated behind M7 per D-012).
- Stan companion package → decision criteria in the spec; decided when the
  intraindividual build is scoped.
- SEM/M5 territory (shipped in the v2.0.0 bundle); re-opening Brief E's
  settled recommendations without a reviewed reason.

## Acceptance criteria

- [x] `devel/longitudinal-ssm-spec.md` exists at build-ready detail: each of
      the five In-scope components has an argued decision, an API sketch,
      and a named oracle strategy (≥2 independent oracle types per numeric
      result; simulation-coverage for interval methods).
- [x] Independent Fable review completed via RB/RR (RB06→RR06, archived);
      every finding weighed in a spec revision log (fixed /
      rejected-with-reason), none silently dropped.
- [x] Cross-cutting design decision recorded in `cairn/DECISIONS.md`;
      ROADMAP carries the build-milestone candidate row(s) with lineage.
- [x] Docs/design only: the PR diff touches no `R/`, `src/`, `tests/`,
      `man/`, or `NAMESPACE` (release surface unaffected).

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T3, T4
- AC4 → T1, T2, T3, T4 (verified by PR diff at review)

## Tasks

- [x] **T1** — Draft `devel/longitudinal-ssm-spec.md` from Brief E's
      recommended directions + code reading (`R/ssm_bootstrap.R` strata
      design, `R/ssm_montecarlo.R`, `quantile.circumplex_contrast_radian`,
      `devel/bayesian_ssm.Rmd` incl. its 360-boundary TODO).
      (RB tripwire: no-oracle — circular growth-model/dependent-resampling
      design; RB tripwire: irreversible-api — repeated-measures API shape.)
- [x] **T2** — `/milestone-brief` RB06: adversarial Fable review of the spec
      (fresh session; per-instance approval gate at spawn).
- [x] **T3** — Ingest RR06: weigh each finding (fix or push back with
      reason, revision log), finalize the spec, record the design D-entry.
- [x] **T4** — Register build-milestone candidate row(s) in ROADMAP
      (split per the spec's own sizing), each noting D-012.

## Work log

- 2026-07-16: created by /milestone-plan — promotion of the legacy
  "Milestone 6" candidate row; design-gate milestone mirroring M17. Plan-gate
  decisions (Jeff): one unified spec; Bayesian = draws adapter + vignette
  only; builds NOT merge-gated behind M7 (D-012); Fable review via RB06.

- 2026-07-16: T1 done — spec drafted (8 sections, 5 components + oracle plan
  + build-cut recommendation + 5 open questions for RB06). Key moves: wide
  person-rows make the case bootstrap the existing row resampler; one draws
  adapter serves Bayesian + growth pipelines; listwise-only for occasions
  (pairwise deferred); flagged the missing DESIGN.md oracle-registry pointer
  for the build milestone. Docs-only, no verify run needed.
- 2026-07-16: blocked on RB06 (adversarial Fable review of the spec, T2);
  brief committed on the milestone branch per RB05 precedent.
- 2026-07-16: T2 done — Fable spawned (user-approved), RR06 returned:
  "needs change (targeted)", architecture confirmed. T3 done — spec revised
  (§9 revision log: 12 applied incl. 4 promoted from consider, 3 rejections
  accepted); D-013 recorded. T4 done — build-family candidate row refined
  (A/B/C cut) + ssm_ci_accuracy-occasions extension noted. RB06/RR06
  archived; status → review.

## Decisions

- 2026-07-16 (RR06 ingestion): all 8 apply recommendations adopted; R9–R12
  promoted from consider to apply (one-line costs, real discriminating
  power); R13–R15 rejections accepted with Fable's reasons. Author's call
  where RR06 offered either: `Occasion` result column is
  conditional-presence, not always-present-NA (avoids soft-breaking every
  existing `results` consumer in a minor release; in-package consumers
  branch for occasions anyway). Headline statistical correction: the
  paired-efficiency claim is conditional (∇g₂ᵀC∇g₁ > 0; reverses at
  |Δd| > 90°) — promoted to D-013 as part of the binding build contract.

## Review

### Acceptance-criteria evidence (2026-07-16, fresh)

- **AC1** — `devel/longitudinal-ssm-spec.md` exists (9 sections); §1–§5 each
  carry an argued decision + API sketch; §2.3/§3.3/§4.2/§5.5 each name ≥2
  independent oracle types (simulation-coverage + invariant + closed-form
  for stochastic components; closed-form + strengthened invariants for the
  deterministic per-person layer). Independently confirmed by the [O]
  diff-bug reviewer ("AC1: Satisfied", 8/8 file:line citations verified,
  all statistical statements re-derived and confirmed).
- **AC2** — RB06 + RR06 present in `cairn/reviews/archive/`; spec §9 maps
  all 15 RR06 recommendations (12 applied incl. 4 promoted from consider;
  3 rejections accepted) + 6 beyond-the-brief items; reviewer confirmed no
  finding silently dropped and the refuted unconditional efficiency claim
  appears nowhere in the revised spec.
- **AC3** — D-013 recorded (`cairn/DECISIONS.md:311`); ROADMAP carries the
  refined build-family candidate row (A/B/C cut, lineage noted). Blame
  reviewer verified D-013 contradicts no prior D-entry and the candidate
  rewrite conserves the old row's full content.
- **AC4** — `git diff --name-only master..HEAD`: 5 `cairn/` files +
  `devel/longitudinal-ssm-spec.md` only; no `R/`, `src/`, `tests/`, `man/`,
  `NAMESPACE`. Both dirs are `.Rbuildignore`d (`^devel$`, `^cairn$`).

### Consistency gate

- `cairn_validate.py`: all checks pass (exit 0).
- No DESIGN.md principle changed → `cairn_impact` n/a.
- Profile (r-package) gate: `document()` no-diff clean; pkgdown
  `check_pkgdown()` no problems; full `devtools::check(--no-manual)`
  **0 errors / 0 warnings / 0 notes** (4m23s); NEWS n/a (no user-visible
  change — docs-only in build-ignored dirs); no new top-level files.

### Independent review (3 lenses → scorer)

- [O] diff-bug: 2 findings; [S] blame-history: 0; [S] prior-PR-comments:
  no prior-PR evidence, 0. Scorer ([S]): F1 = 85, F2 = 25.
- **F1 (85, fixed now):** spec §6 pinned build artifacts to the stale
  legacy prefix `devel/m6-*-results.rds`; corrected to the producing build
  milestone's own ID (`m<NN>-*`).
- **F2 (25, logged, not actioned):** CLAUDE.md's contrast invariant will
  need an occasion-order clause once Build A ships — self-labeled optional,
  unmodified file; owned by Build A's own review gate.
