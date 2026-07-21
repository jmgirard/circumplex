# M49: Fit-index guidance — the two source-backed caveats

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m49-fitindex-source-caveats

## Goal

Add the two caveats its own cited sources state to the fit-index guidance in `vignettes/evaluating-circumplex-structure.Rmd` — Hu & Bentler's small-*n* overrejection and Browne & Cudeck's "subjective judgment" hedge.

## Scope

**In:** The `### Reading the fit indices` section of `vignettes/evaluating-circumplex-structure.Rmd` (currently lines 90–121). Two source-literature caveats, banked verbatim in `cairn/references/hu1999.md` (p. 1 abstract) and `cairn/references/browne1992a.md` (p. 239), inserted precisely and attributed.

**Out:** Any package-code change → not here (the constants and estimation are untouched; those trace-sites are already correctly attributed per the source notes). The other fit-index candidate — ΔCFI as a labeled invariance criterion in `ssm_sem()` — stays a candidate row (needs a `cheung2002` source note). No new source reading: both quotes are already ingested and extraction-verified.

## Acceptance criteria

- [ ] The section states that TLI and RMSEA tend to overreject true-population models at small sample size, attributed to Hu & Bentler (1999), and **explicitly notes CFI is not among the indices the source flags** — matching the `hu1999.md` abstract quote "the ML-based TLI, Mc, and RMSEA tend to overreject true-population models at small sample size"; the prose ties this to the modest-*n* circumplex context (`hu1999.md` records `zimmermann2017.md` placing several SSM accuracy thresholds at n = 50–200).
- [ ] The section carries Browne & Cudeck's own characterization of their RMSEA cutoffs as "based on subjective judgment… cannot be regarded as infallible or correct", attributed to Browne & Cudeck (1993), matching the `browne1992a.md` p. 239 verbatim quote.
- [ ] Both caveats read as external-literature caveats attached to the benchmark paragraph, kept **distinct from** the existing "Two circumplex-specific cautions, both from this package's own validation simulations" list — provenance not blurred.
- [ ] `devtools::check(args = "--no-manual")` builds the vignette clean with no new NOTE/WARNING, and the `## References` list is unchanged (both sources already listed at `:613` and `:625`).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T2
- AC4 → T3

## Tasks

- [ ] T1 — Insert the Hu & Bentler small-*n* overrejection caveat into `### Reading the fit indices` (at/after the benchmark paragraph, lines 91–97): scoped to TLI and RMSEA, explicitly excluding CFI, attributed to Hu & Bentler (1999), tied to the modest-*n* circumplex context. Verify wording against `hu1999.md`'s abstract quote.
- [ ] T2 — Insert Browne & Cudeck's "subjective judgment / not infallible" characterization, attributed to Browne & Cudeck (1993), as a source-literature caveat distinct from the package-simulation cautions list. Verify against `browne1992a.md`'s p. 239 quote.
- [ ] T3 — Build via `devtools::check(args = "--no-manual")` (authoritative build, not a standalone `render()` — M21/M34); confirm no new NOTE/WARNING and an unchanged References list; check the edited region's bytes for leaked scaffolding (M34).

## Work log

- 2026-07-21: created by /milestone-plan. Absorbs the "fit-index guidance omits two caveats" candidate row (ROADMAP); lineage: `hu1999.md` open question deferred this from M41. Gate decisions: overrejection caveat scoped to TLI+RMSEA only (CFI excluded, per source); both caveats added.

## Decisions

## Review
