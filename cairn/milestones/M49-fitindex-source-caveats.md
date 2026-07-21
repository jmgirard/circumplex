# M49: Fit-index guidance — the two source-backed caveats

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m49-fitindex-source-caveats · https://github.com/jmgirard/circumplex/pull/75

## Goal

Add the two caveats its own cited sources state to the fit-index guidance in `vignettes/evaluating-circumplex-structure.Rmd` — Hu & Bentler's small-*n* overrejection and Browne & Cudeck's "subjective judgment" hedge.

## Scope

**In:** The `### Reading the fit indices` section of `vignettes/evaluating-circumplex-structure.Rmd` (currently lines 90–121). Two source-literature caveats, banked verbatim in `cairn/references/hu1999.md` (p. 1 abstract) and `cairn/references/browne1992a.md` (p. 239), inserted precisely and attributed.

**Out:** Any package-code change → not here (the constants and estimation are untouched; those trace-sites are already correctly attributed per the source notes). The other fit-index candidate — ΔCFI as a labeled invariance criterion in `ssm_sem()` — stays a candidate row (needs a `cheung2002` source note). No new source reading: both quotes are already ingested and extraction-verified.

## Acceptance criteria

- [x] The section states that TLI and RMSEA tend to overreject true-population models at small sample size, attributed to Hu & Bentler (1999), and **explicitly notes CFI is not among the indices the source flags** — matching the `hu1999.md` abstract quote "the ML-based TLI, Mc, and RMSEA tend to overreject true-population models at small sample size"; the prose ties this to the modest-*n* circumplex context (`hu1999.md` records `zimmermann2017.md` placing several SSM accuracy thresholds at n = 50–200).
- [x] The section carries Browne & Cudeck's own characterization of their RMSEA cutoffs as "based on subjective judgment… cannot be regarded as infallible or correct", attributed to Browne & Cudeck (1993), matching the `browne1992a.md` p. 239 verbatim quote.
- [x] Both caveats read as external-literature caveats attached to the benchmark paragraph, kept **distinct from** the existing "Two circumplex-specific cautions, both from this package's own validation simulations" list — provenance not blurred.
- [x] `devtools::check(args = "--no-manual")` builds the vignette clean with no new NOTE/WARNING, and the `## References` list is unchanged (both sources already listed at `:613` and `:625`).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T1, T2
- AC4 → T3

## Tasks

- [x] T1 — Insert the Hu & Bentler small-*n* overrejection caveat into `### Reading the fit indices` (at/after the benchmark paragraph, lines 91–97): scoped to TLI and RMSEA, explicitly excluding CFI, attributed to Hu & Bentler (1999), tied to the modest-*n* circumplex context. Verify wording against `hu1999.md`'s abstract quote.
- [x] T2 — Insert Browne & Cudeck's "subjective judgment / not infallible" characterization, attributed to Browne & Cudeck (1993), as a source-literature caveat distinct from the package-simulation cautions list. Verify against `browne1992a.md`'s p. 239 quote.
- [x] T3 — Build via `devtools::check(args = "--no-manual")` (authoritative build, not a standalone `render()` — M21/M34); confirm no new NOTE/WARNING and an unchanged References list; check the edited region's bytes for leaked scaffolding (M34).

## Work log

- 2026-07-21: created by /milestone-plan. Absorbs the "fit-index guidance omits two caveats" candidate row (ROADMAP); lineage: `hu1999.md` open question deferred this from M41. Gate decisions: overrejection caveat scoped to TLI+RMSEA only (CFI excluded, per source); both caveats added.
- 2026-07-21: T1+T2 — inserted both source-literature caveats as one prose paragraph after the benchmark paragraph, kept distinct from the package-simulation cautions list (relabeled "Two further cautions are circumplex-specific"). Quotes verified verbatim against `browne1992a.md` (p. 239) and `hu1999.md` (abstract); CFI excluded from the overrejection claim.
- 2026-07-21: T3 — `devtools::check(args = "--no-manual")` clean (0/0/0); log shows "re-building of vignette outputs ... OK" (vignette genuinely rebuilt). Diff is the one inserted paragraph + relabel; References list unchanged. Status → review.
- 2026-07-21: review — PR #75 (draft). Consistency gate clean (cairn_validate exit 0, document() no-diff, pkgdown clean, full check() 0/0/0). 3-lens fan-out + scorer: F1 (score 90) RMSEA "comparative-fit" mislabel fixed on branch; F2 (score 78) subjective-judgment quote-scope logged below threshold. Re-ran full check() post-fix: 0/0/0.

## Decisions

## Review

**AC evidence (fresh, this session):**
- AC1 — `evaluating-circumplex-structure.Rmd:103-107`: "the ML-based TLI and RMSEA tend to *overreject* true-population models when the sample is small (CFI is not among the indices they flag)… the SSM accuracy thresholds in Section 3 span roughly $n = 50$ to $200$." Attributed Hu & Bentler (1999); CFI excluded; matches `hu1999.md` abstract quote (Mc dropped — not computed by `cpm_fit()`, plan-sanctioned). Verbatim/scoping confirmed by all three lenses.
- AC2 — `:99-101`: "based on subjective judgment," … "cannot be regarded as infallible or correct" (Browne & Cudeck, 1993). Verbatim against `browne1992a.md` p. 239 (all three lenses). See logged Finding 2 on quote scope.
- AC3 — new source-literature paragraph ("Two caveats come from the benchmark sources themselves") kept distinct from the relabeled package-simulation list ("Two further cautions are circumplex-specific, both from this package's own validation simulations"). Provenance clean (blame-history lens confirmed).
- AC4 — `devtools::check()` full run clean **0/0/0** (re-run post-fix; "re-building of vignette outputs … OK"); References list untouched (git diff: 0 ref-list lines changed).

**Consistency gate:** `cairn_validate` exit 0 (coverage complete PASS; 47 advisories all M7's pre-existing work-log wraps). `document()` no generated-file diff. `pkgdown::check_pkgdown()` clean. Full `devtools::check()` 0/0/0. No principle changed → `cairn_impact` skipped. NEWS: no entry owed — the "Evaluating Circumplex Structure" vignette is new-in-2.0.0 (already logged under `## Documentation`), so M49 refines unreleased content with no release-relative delta.

**Independent review — 3 lenses + scorer:**
- [O] diff-bug (Opus): 2 findings. **F1 (score 90, actioned/fixed):** the caveat sentence called RMSEA "the comparative-fit benchmark", a category error contradicting the vignette's own terminology (RMSEA = approximate fit; comparative fit reserved for CFI/TLI). Fixed on branch → "these benchmarks are least dependable at small samples". **F2 (score 78, below threshold — logged, not actioned):** the "subjective judgment / infallible" quote attaches to Browne & Cudeck's 0.05 close-fit figure specifically, while the vignette presents the .08/.10 cutoffs; the passage is subjective in spirit throughout and AC2 framed the target as the cutoffs plural. Surfaced to the maintainer at the approval gate.
- [S] blame-history (Sonnet): no findings — edit resolves M41's explicitly-deferred caveat, undoes/contradicts nothing; independently flagged the same RMSEA/comparative-fit wording (folds into F1).
- [S] prior-review (Sonnet): no prior-review evidence (GitHub comment probe empty; archived Review sections orthogonal to this vignette text); zero findings.
