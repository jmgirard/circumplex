# M46: Prospect notes for the four forward-looking shelf sources

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m46-forward-source-prospects

## Goal

Triage the four deliberately-shelved forward-looking sources into one committed
survey synthesis note plus four `candidate` ROADMAP rows — per the supply-push
exploration doctrine — so a later planner starts warm without owing any
per-source reference page.

## Scope

**In:** one committed **survey synthesis note**
(`cairn/references/forward-source-prospects.md`, authored from
`synthesis-note.md`) triaging nagy2019 (covariate extension of Browne's CPM),
weide2021 (Bayesian + ML circumplex IIP modeling), rogoza2021 (three-step
circumplex procedure), tracey2000 (analysis-of-circumplex chapter) — one
stable-ID ledger row per source giving a content characterization, the
oracle/method/reference it could seed, and what a taking-up milestone must
verify; its `INDEX.md` line; and a Disposition mapping each row to a `candidate`
ROADMAP row. Four `candidate` rows, one per source. Correcting the stale
`INDEX.md` comment that says prospects stay off the committed-page list.

**Out:** per-source `<citekey>.md` pages (demand-pull — owed only when a
milestone comes to rely on a source → future milestones). Full formula
extraction, verbatim value banking, and code-cross-reference validation (the
M45 relied-upon bar; owed only on graduation). Building the oracles/methods/
features these could seed (each its own future milestone). The invented
`cairn/references/prospects/` home (dropped — the doctrine adds no new write
location; see D-024).

## Acceptance criteria

- [ ] A survey synthesis note exists at
      `cairn/references/forward-source-prospects.md`, authored from
      `synthesis-note.md`, with one stable-ID ledger row per source (nagy2019,
      weide2021, rogoza2021, tracey2000) stating a content characterization, the
      oracle/method/reference it could seed, and what a taking-up milestone must
      verify. No source's verbatim values are banked (supply-push altitude); any
      claim citing a specific value/page is page-anchored. Its `Extraction:`
      status is a derived/first-hand form on one physical line with an unbolded
      verb, and every repo-state claim carries `— observed <date>`.
- [ ] The note carries its `INDEX.md` line (filename as link text, per the
      references check); the stale `INDEX.md` comment saying the four prospects
      stay off the committed-page list is corrected to point at the survey note;
      no per-source `<citekey>.md` page is created for the four sources.
- [ ] Four `candidate` ROADMAP rows are registered, one per source, each naming
      the seedable oracle/method/reference and pointing at the synthesis note;
      the note's Disposition maps every ledger row to its candidate row.
- [ ] `cairn_validate` is clean — the new note raises no unindexed-page,
      `references staleness`, or dated-observation advisory.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5

## Tasks

- [x] T1 — Read the four shelf PDFs
      (`cairn/references/sources/{nagy2019,weide2021,rogoza2021,tracey2000}.pdf`)
      at survey altitude; draft one ledger row per source (content, seedable
      oracle/method/reference, what a taking-up milestone must verify).
      tracey2000 (the 2000 handbook chapter) is **uncited** by shipped code —
      the CAIS `@source` is Sodano & Tracey (2006) at `R/instrument_data.R:5`,
      a different work; the plan's "appears only as CAIS provenance" note was
      wrong and is corrected here (M40/M41 re-check-the-claim lesson).
- [x] T2 — Author `cairn/references/forward-source-prospects.md` from
      `synthesis-note.md`: Provenance (derived, ingested date, M46), Scope +
      tracking disclaimer, Evidence snapshot (each `— observed`), a neutral
      "what these four are" section, the E1–E4 ledger, and a Disposition mapping
      each row to its candidate row. Derived/first-hand `Extraction:` status on
      one physical line.
- [x] T3 — Add the note's `INDEX.md` line (filename as link text); correct the
      stale `INDEX.md` prospects comment to point at the survey note; confirm no
      per-source page was created.
- [x] T4 — Register four `candidate` ROADMAP rows, one per source, each pointing
      at the synthesis note.
- [ ] T5 — Run `cairn_validate`; confirm clean; commit.

## Work log

- 2026-07-20: created by /milestone-plan (Jeff split the forward-looking sources out of M45 into their own scope — shelved on purpose as future references / oracles / research material; see D-023).
- 2026-07-20: re-planned by /milestone-plan — this milestone predated the official supply-push exploration doctrine (tracking-rules "Exploring prospective sources", shipped by cairn M103). Superseded D-023's per-source-prospect-brief mechanism with that doctrine's output: dropped the invented `cairn/references/prospects/` home, replaced the four per-source briefs with one committed survey synthesis note + four candidate rows, recorded D-024. Content unchanged in substance (still capture-by-intent); only the mechanism conforms. See D-024.
- 2026-07-20: T1 — read the four shelf PDFs at survey altitude; pinned citations/DOIs. Corrected the plan's tracey2000 note: it is uncited (CAIS `@source` is Sodano & Tracey 2006, `R/instrument_data.R:5`), a false repo-state claim caught by the M40/M41 re-check lesson.
- 2026-07-20: T2 — authored `cairn/references/forward-source-prospects.md` (survey synthesis note): provenance/scope/evidence-snapshot, neutral per-source characterization, E1–E4 prospect ledger (kind: oracle/method/reference/feature), Disposition → four candidate rows, dated open questions. No verbatim values banked; one-physical-line derived Extraction status.
- 2026-07-20: T3 — added the survey note's `INDEX.md` committed-page line (filename link text); corrected the stale INDEX comment (D-023 mechanism superseded by D-024) to point at the one survey note; confirmed no per-source `<citekey>.md` page was created.
- 2026-07-20: T4 — registered four `candidate` ROADMAP rows (nagy2019 E1, weide2021 E2, rogoza2021 E3, tracey2000 E4), each naming its seedable oracle/method/reference/feature and pointing at `forward-source-prospects.md`; each notes no page owed until relied upon (D-024).

## Decisions

## Review
