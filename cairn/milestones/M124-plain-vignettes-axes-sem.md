# M124: The axes-reliability and SEM vignettes read as plain English

- **Status:** planned
- **Priority:** normal
- **Depends on:** M123
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes ship in the package and on the pkgdown site
- **Branch/PR:** —

## Goal

The vignettes `axes-reliability` and `sem-based-ssm-analysis` read on one pass for an applied researcher, with every statistical claim kept.

## Scope

**In:** A prose rewrite of `vignettes/axes-reliability.Rmd.orig` and `vignettes/sem-based-ssm-analysis.Rmd.orig`, with each edit copied into the shipped `.Rmd`. The two pages are the "pages" below. Uses the sweep and rules page M123 ships. Adds an M124 ledger section to `cairn/references/plain-vignettes.md`. One NEWS bullet, or an extension of M123's.

**Out:**
- Changes to `tools/prose-sweep.R` beyond a parse fix this page set needs. A fix goes in this milestone with a plant test. Wider sweep work goes to the candidate row "The prose sweep as a merge gate".
- Other vignettes → M123, M125, and the candidate row "Plain-English pass over the three older vignettes".
- Code chunks, chunk options, figure captions and alt text: unchanged, because the pass is about prose.

## Acceptance criteria

- [ ] AC1: For each page and its shipped `.Rmd`, `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0. So the swept prose has no sentence over 25 words, no dash, no semicolon, and no milestone, decision or review id. `cairn/references/plain-vignettes.md` defines "swept prose", "sentence" and "dash".
- [ ] AC2: No code changed. For each page and its shipped `.Rmd`, `tools/prose-sweep.R --chunks` prints the same text at the base commit and at the head.
- [ ] AC3: Each shipped `.Rmd` is what its `.Rmd.orig` knits to: the `vignette-precompute` workflow passes on the PR head.
- [ ] AC4: For each page, every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items.
- [ ] AC5: A fresh-context reader compares each page's base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [ ] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs of each page that it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
- [ ] AC7: `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and 0 notes, and `Rscript -e 'devtools::test()'` reports no failures.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2, T4
- AC4 → T3
- AC5 → T3
- AC6 → T3
- AC7 → T4

## Tasks

- [ ] T1: Rewrite `vignettes/axes-reliability.Rmd.orig` prose and copy each edit into `vignettes/axes-reliability.Rmd`. The estimator's corrections (standard errors, fit statistics, the correlation metric) keep every qualifier. Check each one against `?axes_reliability` and `cairn/references/strack2013.md`.
- [ ] T2: Rewrite `vignettes/sem-based-ssm-analysis.Rmd.orig` prose and copy each edit into `vignettes/sem-based-ssm-analysis.Rmd`. Fit-index and contrast statements keep their scope conditions. Check them against `?ssm_sem`.
- [ ] T3: Run the base-versus-head inventory comparison and both fresh readers (claims, then one-read). Fix or disposition every item, and add the M124 ledger rows. Update NEWS.
- [ ] T4: Run check and test. Push, and confirm the `vignette-precompute` workflow passes.

## Work log

- 2026-09-14: created by /milestone-plan. The gate choices and the criteria audit are recorded in M123's work log. This milestone repeats M123's AC1-AC7 for its own two pages.

## Decisions

## Review
