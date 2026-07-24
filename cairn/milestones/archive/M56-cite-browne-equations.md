# M56: Cite the published Browne equations `R/cpm_fit.R` implements

**Status:** done (2026-07-24, PR #82 https://github.com/jmgirard/circumplex/pull/82)

**Goal:** Give the CPM engine's implementing lines local provenance — inline comments naming the published Browne equation and page each line implements.

**Outcome:** Inline `#` comments added to `R/cpm_fit.R` citing the equation + page each line implements — eqs. 34/3/2/5/6/8 and the Heywood definition (`browne1992.md`), eqs. 13/14 (`browne1992a.md`) — sourced from the notes' Traces-to tables. Honest non-attribution comments mark the two package-own departures (variant C, the m-cap) as the package's choices, not Browne's. Browne (1992) Table 11 (p. 494) added as a direct co-anchor for the m=1 oracle in `test-cpm_oracles.R`. The comment insertions shifted line numbers, so `R/cpm_fit.R:<line>` anchors were re-synced across five source notes (browne1982/1992/1992a/hu1999/INDEX). Comment-only; no runtime change; `test()` FAIL 0 | PASS 3170.

**Decisions:** none.

**Review:** Three-lens + scorer. diff-bug (Opus) 0 findings. blame-history and prior-review lenses converged on one defect — three passages still asserting the code carries "no citation" (`browne1992a.md:51`,`:145` scored 85; `browne1992.md` Open-questions bullet scored 72), which M56's own comments falsified; all three fixed in review (past-tense / "Resolved by M56"). `check` 0/0/0, pkgdown clean, `cairn_validate` OK.
