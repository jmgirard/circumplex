# M123: The Bayesian, growth and visualization vignettes read as plain English, checked by a prose sweep

**Status:** done (2026-09-14, PR #156 https://github.com/jmgirard/circumplex/pull/156)

**Goal:** The three vignettes `bayesian-ssm-analysis`, `growth-ssm-analysis` and `advanced-visualization` read on one pass for an applied researcher, with every statistical claim kept.

**Outcome:** The prose of the three pages is rewritten, and each `.Rmd.orig` edit is copied byte for byte into its shipped `.Rmd`. Chunks and output are unchanged. `tools/prose-sweep.R` (build-ignored) flags sentences over 25 words, dashes, semicolons and tracking ids, and prints `--prose`, `--chunks` and a sign-keeping `--inventory`. It refuses invalid UTF-8 input with exit 3 and works in a C locale. `tests/testthat/test-prose-sweep.R` plants each finding kind. `cairn/references/plain-vignettes.md` holds the reader profile, rules, precision list, sweep definition, known sweep gaps and the ledger. One NEWS bullet.

**Decisions:** none cross-cutting. Plan gate: a 25-word cap, review-only sweep runs (no CI gate yet), the six newer vignettes split across M123 to M125.

**Review:** three-lens fan-out plus fresh claims and one-read readers. The claims reader listed 23 items, and 13 were rejected or matched by the ledger. The other 10 were fixed where the rewrite drifted from a base claim (the nlme "formulation", the signed-distance premise, "the layer supplies the ordering"). The one-read reader listed 62 paragraphs: the top items glossed, the rest rejected with grouped reasons. Diff-bug lens: 22 findings, 7 fixed, 12 deferred to the prose-sweep candidate row, 3 rejected. Blame lens: 1 (same as a claims item). Prior-review lens: no evidence. A re-read of the first fix commit found 17 more, among them a C-locale crash and four wrong or claim-adding glosses. Of those, 12 were fixed and 4 rejected. One became a `/hotfix` candidate row for the `ssm_plot_trajectory()` help page. CI was re-run once by a pushed tracking commit. The last two local work-log lines (a CI timeout and a re-approval to merge the green head unpushed) did not reach the squash.
