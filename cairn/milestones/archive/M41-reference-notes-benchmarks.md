# M41: Source notes for the fit-index benchmark pair (done 2026-07-19)

**Goal / outcome.** Committed `cairn/references/` pages for the two fit-index
sources the package cites together wherever it reports RMSEA, SRMR, CFI, or
TLI: `browne1992a.md` (Browne & Cudeck 1992, SMR 21(2) 230–258) and
`hu1999.md` (Hu & Bentler 1999, SEM 6(1) 1–55), plus `INDEX.md` entries and a
corrected owes-no-page ledger. Docs-only, no package file changed. PR #67,
squash `0dc74f32`, CI 9/9, `check(--no-manual)` 0/0/0, `cairn_validate` 15/15.

**Re-scoped at its own plan gate.** Planned for all seven unshelved sources;
all seven arrived, so T1's inventory ran at the plan gate and M41 split three
ways — M41 kept the fit benchmarks (and its Browne & Cudeck work log), M42
took Browne 1992's CPM spec + Browne 1982, M43 took Acton & Revelle + Wendt.

**Key findings.** Cheung & Rensvold (2002) has no shipped reliance (zero
hits in `R/`, `vignettes/`, `tests/`) and owes no page; its unexercised ΔCFI
question became a candidate row. p. 239 carries three RMSEA thresholds, not
the two the plan described, and the package ships .08 and 0.1 — AC2 amended at
the gate. Review **F1 (95)**: the page claimed nothing in the repo computes
Browne & Cudeck's equations while `R/cpm_fit.R:1049,1011-1028` had implemented
eqs. 13/14 all along, unattributed — both now banked verbatim, though the code
still carries no citation. F2 (85), F3 (78) fixed; F4 (63) logged. **M41-D1:**
channel 2 is the implementing session's read of a rendered page image —
independent of the text layer, but **not** a human attestation; neither page
has been read by a human, a dated open question on both.
