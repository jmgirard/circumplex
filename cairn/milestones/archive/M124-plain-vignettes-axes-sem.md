# M124: The axes-reliability and SEM vignettes read as plain English

**Status:** done (2026-09-14, PR #157 https://github.com/jmgirard/circumplex/pull/157)

**Goal:** The vignettes `axes-reliability` and `sem-based-ssm-analysis` read on one pass for an applied researcher, with every statistical claim kept.

**Outcome:** The prose of both pages is rewritten under the M123 rules, and each `.Rmd.orig` edit is copied by hand into its shipped `.Rmd`. Chunks and output are unchanged. `tools/prose-sweep.R` now drops a knitr table (a `Table:` caption line and the pipe rows under it), because a chunk-printed table on the SEM page held a semicolon. A plant test in `tests/testthat/test-prose-sweep.R` covers the change. The axes page gains the missing Hu and Bentler (1999) reference. NEWS extends the M123 bullet to five vignettes. Ledger rows are in `cairn/references/plain-vignettes.md` under M124.

**Decisions:** none cross-cutting. Mid-implement gate: the sweep fix instead of a kable-to-gt switch, which became a candidate row.

**Review:** three-lens fan-out plus four fresh readers at the head. The claims readers listed 22 items. Six were fixed at the gate: the five-component count (it followed a data-raw comment against `strack2013.md` p. 4), an unsupported robust-SE clause, a large-sample gloss, an implied-matrix gloss, the contrast rule's scope and the "identified" gloss. The other 16 were matched by the ledger or rejected. A fresh re-read of the six fixes found three problems, fixed with base wording or the re-reader's wording. The one-read readers listed 63 paragraphs, all rejected with grouped reasons at the maintainer's choice. Diff-bug lens: 7 findings, 3 shared with the claims items, 2 sweep gaps added to "Known sweep gaps", 2 rejected. Blame lens: 1 (same as the robust-SE item). Prior-review lens: no evidence. The first CI wait hit the 590 s ceiling while R CMD check ran, and the resumed session merged on green. The last two branch commits (PR URL and AC3 evidence) were unpushed, so they did not reach the squash.
