# M23: Longitudinal & intraindividual SSM — Fable-reviewed design + build-ready spec

- **Status:** blocked
- **Priority:** high
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** m23-longitudinal-ssm-design

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

- [ ] `devel/longitudinal-ssm-spec.md` exists at build-ready detail: each of
      the five In-scope components has an argued decision, an API sketch,
      and a named oracle strategy (≥2 independent oracle types per numeric
      result; simulation-coverage for interval methods).
- [ ] Independent Fable review completed via RB/RR (RB06→RR06, archived);
      every finding weighed in a spec revision log (fixed /
      rejected-with-reason), none silently dropped.
- [ ] Cross-cutting design decision recorded in `cairn/DECISIONS.md`;
      ROADMAP carries the build-milestone candidate row(s) with lineage.
- [ ] Docs/design only: the PR diff touches no `R/`, `src/`, `tests/`,
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
- [ ] **T2** — `/milestone-brief` RB06: adversarial Fable review of the spec
      (fresh session; per-instance approval gate at spawn).
- [ ] **T3** — Ingest RR06: weigh each finding (fix or push back with
      reason, revision log), finalize the spec, record the design D-entry.
- [ ] **T4** — Register build-milestone candidate row(s) in ROADMAP
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

## Decisions

## Review
