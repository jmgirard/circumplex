# M48: Fit-index and uncited shelf sources (browne1993 twin + strack2013 prospect)

**Status:** done (2026-07-21, PR #74 https://github.com/jmgirard/circumplex/pull/74)

**Goal:** Account for the two remaining unaccounted shelf sources — cross-reference browne1993 as the cited chapter-twin of the banked `browne1992a.md`, and capture strack2013 as a deliberate research prospect.

**Outcome:** Documentation/tracking only (diff entirely inside the `.Rbuildignore`d `cairn/` tree). `browne1992a.md` gained an affirmative twin cross-reference: the repo's user-facing "Browne & Cudeck (1993)" citation named at its three sites (`vignettes/evaluating-circumplex-structure.Rmd:93` & `:613`, `tests/testthat/_snaps/ci_accuracy.md:38`), the 1993 Bollen & Long chapter identified as carrying the same RMSEA cutoffs on chapter p. 144, and the snapshot site added to Traces. Extraction re-verified 2026-07-21 against `sources/browne1993.pdf` (two channels). No duplicate `browne1993.md` page. strack2013 (Strack, Jacobs & Grosse Holtforth 2013, *Reliability of Circumplex Axes*; CFA axes-reliability, RANDALL-adjacent) captured as a prospect `candidate` ROADMAP row; no per-source page (D-024).

**Decisions:** none (design decisions were made at the M48 plan gate; prospect capture follows D-023/D-024).

**Review:** 3-lens fan-out + scorer. F1 (diff-bug, Opus, scored 90, CONFIRMED, fixed on branch): the chapter-twin re-check line mis-anchored the m=2 worked-example row to chapter p. 144; it is Table 6.1 on chapter p. 152 (PDF p. 17) — anchor split (cutoffs → p. 144). Blame-history and prior-review lenses clean (M41-D1 caveat preserved). No lessons captured; nothing graduated/retired.
