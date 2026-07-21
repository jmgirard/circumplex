# M46: Prospect notes for the four forward-looking shelf sources

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Characterize the four sources deliberately shelved as future research material
so a later planner starts warm — what each contains, why it could become a
reference, oracle, or feature, and what a taking-up milestone would verify.

## Scope

**In:** a forward-looking prospect brief per source — nagy2019 (covariate
extension of Browne's CPM), weide2021 (Bayesian + ML circumplex IIP modeling),
rogoza2021 (three-step circumplex procedure), tracey2000 (analysis-of-circumplex
chapter) — each summarizing content, naming the milestone it could seed, and
stating what that milestone must verify. A `candidate` ROADMAP row per source
pointing at its brief. These are **prospect** material, honestly flagged as not
yet relied upon (no shipped code cites them), kept separate from the
relied-upon reference pages.

**Out:** building the features/oracles they could seed (each is its own future
milestone). Full formula extraction and code-cross-reference validation (owed
only if and when a milestone relies on a source — the M45 bar, deliberately not
applied here). Adding these to `INDEX.md`'s committed-page list (they have no
`Traces to`). The relied-upon RANDALL pair → M45.

## Acceptance criteria

- [ ] A prospect brief exists for each of the four sources (nagy2019, weide2021,
      rogoza2021, tracey2000), each stating: a content summary, the
      reference/oracle/feature it could seed, and what a taking-up milestone
      would have to verify. Claims that cite a specific value or page are
      page-anchored; each brief is flagged "not yet relied upon (no `Traces
      to`)."
- [ ] Four `candidate` ROADMAP rows are registered, one per source, each naming
      the seedable milestone and marked "on shelf, forward-looking, no reference
      page owed until relied upon."
- [ ] The prospect material is stored separately from the relied-upon reference
      pages and is **not** added to `INDEX.md`'s committed-page list; its home is
      stated in the milestone.
- [ ] `cairn_validate` is clean (no unindexed-page advisory raised by the new
      prospect files).

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T5
- AC3 → T1, T6
- AC4 → T6

## Tasks

- [ ] T1 — Establish the prospect home (default: `cairn/references/prospects/`,
      out of `INDEX.md` and the tarball like all of `cairn/`); author the
      nagy2019 brief.
- [ ] T2 — Author the weide2021 brief (flag as a potential
      Bayesian/ML inference-comparison oracle or benchmark).
- [ ] T3 — Author the rogoza2021 brief (potential method / vignette material).
- [ ] T4 — Author the tracey2000 brief (potential reference; note it currently
      appears only as CAIS data provenance in `R/instrument_data.R` /
      `data-raw/cais.R`).
- [ ] T5 — Register four `candidate` ROADMAP rows, one per source, each
      pointing at its brief.
- [ ] T6 — Run `cairn_validate`; commit.

## Work log

- 2026-07-20: created by /milestone-plan (Jeff split the forward-looking sources out of M45 into their own scope — shelved on purpose as future references / oracles / research material; see D-023).

## Decisions

## Review
