# M49: Fit-index guidance — the two source-backed caveats

**Status:** done (2026-07-21, PR #75 https://github.com/jmgirard/circumplex/pull/75)

**Goal:** Add the two caveats its cited sources state to the fit-index guidance in `vignettes/evaluating-circumplex-structure.Rmd` — Hu & Bentler's small-*n* overrejection and Browne & Cudeck's "subjective judgment" hedge.

**Outcome:** Docs-only (vignette prose + tracking). The `### Reading the fit indices` section gained one source-literature paragraph, kept distinct from the package-simulation cautions list (relabeled "Two further cautions are circumplex-specific"): Browne & Cudeck (1993) call their RMSEA cutoffs "based on subjective judgment… cannot be regarded as infallible or correct"; Hu & Bentler (1999) found TLI and RMSEA overreject true-population models at small *n* (CFI explicitly excluded — the source doesn't flag it). Quotes verbatim against `hu1999.md` / `browne1992a.md`; no package code; References list unchanged; full `check()` 0/0/0. Absorbed the ROADMAP "fit-index caveats" candidate; resolves the caveat M41 deferred (`hu1999.md` open question).

**Decisions:** none (gate decisions: overrejection caveat scoped to TLI+RMSEA only, CFI excluded; both caveats added).

**Review:** 3-lens fan-out + scorer. F1 (score 90, fixed on branch): RMSEA mislabeled "comparative-fit" (it is an approximate-fit index) → corrected to "these benchmarks". F2 (score 78, below threshold, logged): the "subjective judgment" quote is 0.05-anchored in source vs the .08/.10 cutoffs shown — maintainer chose merge as-is. Blame-history + prior-review lenses: no findings. One lesson captured; none retired.
