# M124: The axes-reliability and SEM vignettes read as plain English

- **Status:** review
- **Priority:** normal
- **Depends on:** M123
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes ship in the package and on the pkgdown site
- **Branch/PR:** m124-plain-vignettes-axes-sem

## Goal

The vignettes `axes-reliability` and `sem-based-ssm-analysis` read on one pass for an applied researcher, with every statistical claim kept.

## Scope

**In:** A prose rewrite of `vignettes/axes-reliability.Rmd.orig` and `vignettes/sem-based-ssm-analysis.Rmd.orig`, with each edit copied into the shipped `.Rmd`. The two pages are the "pages" below. Uses the sweep and rules page M123 ships. Adds an M124 ledger section to `cairn/references/plain-vignettes.md`. One NEWS bullet, or an extension of M123's.

**Out:**
- Changes to `tools/prose-sweep.R` beyond a parse fix this page set needs. A fix goes in this milestone with a plant test. Wider sweep work goes to the candidate row "The prose sweep as a merge gate".
- Other vignettes → M123, M125, and the candidate row "Plain-English pass over the three older vignettes".
- Code chunks, chunk options, figure captions and alt text: unchanged, because the pass is about prose.

## Acceptance criteria

- [x] AC1: For each page and its shipped `.Rmd`, `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0. So the swept prose has no sentence over 25 words, no dash, no semicolon, and no milestone, decision or review id. `cairn/references/plain-vignettes.md` defines "swept prose", "sentence" and "dash".
- [x] AC2: No code changed. For each page and its shipped `.Rmd`, `tools/prose-sweep.R --chunks` prints the same text at the base commit and at the head.
- [ ] AC3: Each shipped `.Rmd` is what its `.Rmd.orig` knits to: the `vignette-precompute` workflow passes on the PR head.
- [x] AC4: For each page, every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items.
- [x] AC5: A fresh-context reader compares each page's base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [x] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs of each page that it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
- [x] AC7: `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and 0 notes, and `Rscript -e 'devtools::test()'` reports no failures.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2, T4
- AC4 → T3
- AC5 → T3
- AC6 → T3
- AC7 → T4

## Tasks

- [x] T1: Rewrite `vignettes/axes-reliability.Rmd.orig` prose and copy each edit into `vignettes/axes-reliability.Rmd`. The estimator's corrections (standard errors, fit statistics, the correlation metric) keep every qualifier. Check each one against `?axes_reliability` and `cairn/references/strack2013.md`.
- [x] T2: Rewrite `vignettes/sem-based-ssm-analysis.Rmd.orig` prose and copy each edit into `vignettes/sem-based-ssm-analysis.Rmd`. Fit-index and contrast statements keep their scope conditions. Check them against `?ssm_sem`.
- [x] T3: Run the base-versus-head inventory comparison and both fresh readers (claims, then one-read). Fix or disposition every item, and add the M124 ledger rows. Update NEWS.
- [x] T4: Run check and test. The push and the `vignette-precompute` run happen at `/milestone-review`, which opens the PR after approval.

## Work log

- 2026-09-14: created by /milestone-plan. The gate choices and the criteria audit are recorded in M123's work log. This milestone repeats M123's AC1-AC7 for its own two pages.
- 2026-09-14: implement started on branch `m124-plain-vignettes-axes-sem`. Question gate skipped: the one open choice (NEWS) takes the plan's second option, an extension of M123's bullet. A local re-knit is not used to copy prose into the shipped `.Rmd`, because the installed circumplex is 2.0.0. Edits are copied by hand, as in M123.
- 2026-09-14: T1 done. `axes-reliability` sweep exits 0 on both files, `--chunks` matches base, and the only inventory change is the added introduction-vignette link. New glosses (FIML, MCAR, `xi1`, `xi2`, the `block_specificity` row name, the shared-value restriction) were read against `R/axes_reliability.R`. The `.Rmd` copy is the `.Rmd.orig` diff applied as a one-line-context patch, not Edit calls, and its changed lines equal the source's.
- 2026-09-14: T2 done. `sem-based-ssm-analysis.Rmd.orig` sweep exits 0, `--chunks` matches base on both files, and the only inventory change is the added introduction-vignette link. Copied into the `.Rmd` the same way as T1. Open: the shipped `.Rmd` sweep exits 1 on one semicolon. The semicolon is in a table that the `estimand-table` chunk prints ("conditional on measurement invariance; not computed"), so AC1 and AC2 conflict on this page.
- 2026-09-14: mini gate on the AC1/AC2 clash. The user asked about moving tables from kable to gt. Answer: not in M124, because it adds a dependency and changes code and output. The user chose the in-scope sweep fix plus a gt candidate row. `tools/prose-sweep.R` now drops a knitr table (a `Table:` caption line and the pipe table under it). The new plant test failed before the fix with the caption and cell findings, and passes after it (40 passes). All four M124 files and M123's five swept files exit 0. The rules page gains the definition and a known gap. Criteria text is unchanged. Note for T3: the head sweep drops the kable output numbers from the shipped SEM `.Rmd` inventory, so AC4 compares base and head with the head sweep on both.
- 2026-09-14: T3 done. Inventory, base `29b64530` against head with the head sweep: nothing removed on either page or copy; added `vignette("introduction-to-ssm-analysis")` (both), `sd` (axes), `lx`, `ly` (SEM), all ledgered. [O] claims reader: 3 items (A1, S1 fixed, A2 kept). [O] one-read reader: 57 items (28 axes, 29 SEM); 31 fixed with glosses read against `R/`, the rest rejected or kept in grouped ledger rows. An [O] re-read of the fix-round glosses checked 45 edits and found 7 problems. 2 were factual (fit gets no interval, and the population item error is fixed), and all 7 were fixed. The 7 replacements have not had a further fresh read. E26 (a base claim broader than the code) went to a candidate row. The Hu & Bentler (1999) reference was added. NEWS extends M123's bullet to the two vignettes. The sweep exits 0 on all four files, and `--chunks` matches base.
- 2026-09-14: first full check failed with 2 test failures. `test-axes-corrected-se.R` AC7 and `test-axes-scaled-fit.R` AC11 pin the vignette phrases "it is a calibration, not an exactness guarantee" and "measured .06 to .11 at three populations chosen to bracket", and the rewrite had split both. Both phrases are restored in the vignette (under 25 words, no dash), and the tests are unchanged. Filtered run: 0 failures, 2974 passes.
- claim audit: 44 claims read, 1 corrected — NEWS.md, tools/prose-sweep.R, tests/testthat/test-prose-sweep.R, vignettes/axes-reliability.Rmd(.orig), vignettes/sem-based-ssm-analysis.Rmd(.orig)
- 2026-09-14: the corrected claim is the simulated-population sentence. It numbered block specificity as the fourth of five components, but `data-raw/simulated_items.R` counts the two axes separately and has no block component. The auditor's replacement text is applied. The header comment of `tools/prose-sweep.R` now lists knitr tables. The corrected sentence was not re-read, because it takes the auditor's own wording.
- 2026-09-14: T4 done. `devtools::check(args = "--no-manual")` at `f51067f3` gave 0 errors, 0 warnings and 0 notes, with tests included. The head commit after it changes only vignette prose and a comment in the build-ignored script. At head, the vignette-reading and sweep tests give 0 failures and 3017 passes. Minor amendment: T4's push and `vignette-precompute` confirmation move to `/milestone-review`, because the git model opens the PR only after approval. AC3 evidence therefore comes from the PR run. Status set to review.
- 2026-09-14: review ran the four fresh readers and three lenses, and the gate-directed claim fixes are at `39c4b54e`.
- step-7 approval: m124-plain-vignettes-axes-sem approved for merge

## Decisions

## Review

Review at `53d94913`, base `29b64530` (master has not moved). Scratch outputs in the session scratchpad, not committed.

- AC1 evidence (2026-09-14): `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R` exits 0 on all four files (243, 243, 251 and 253 `--prose` sentences). Discrimination: the same head script on the base copies exits 1 with 96, 96, 99 and 99 finding lines.
- AC2 evidence (2026-09-14): `--chunks` output at base and head is byte-identical on all four files (36, 21, 99 and 67 lines). The base script on the base copies prints the same text.
- AC3 evidence: pending. The named procedure is the `vignette-precompute` run on the PR head, which exists only after the step-8 push.
- AC4 evidence (2026-09-14): head-script `--inventory`, base against head. Removed: nothing on any file. Added: `sd` and `vignette("introduction-to-ssm-analysis")` (axes, both copies), `lx`, `ly` and the same link (SEM, both copies), all ledgered. With the base script on the base shipped SEM `.Rmd`, 13 numbers also drop (-0.01 to 104.5). They are cells of the two `kable()` tables the chunks print, which the head sweep now skips, not prose. Other three files: nothing removed under either script.
- AC7 evidence (2026-09-14): `devtools::check(args = "--no-manual")` at `53d94913`: 0 errors, 0 warnings, 0 notes (6 min 53 s). `devtools::test()`: 0 failures, 0 errors, 1 skip, 9319 passes.
- Consistency gate (2026-09-14): `cairn_validate.py` exit 0. No principle changed, so `cairn_impact` skipped. `devtools::document()` gives no diff and 0 `resolve link` lines. `pkgdown::check_pkgdown()` no problems. README not touched. NEWS extends the M123 bullet, no milestone ids. No new top-level files. Master watches: newest push runs of `R-CMD-check.yaml` and `test-coverage.yaml` on master (`f594bd2c`) are `success`. `check-master-red-alert.R`, `master-red-alert-dryrun.R` and `check-branch-protection.R` exit 0. The manual step (`devtools::check()` without `--no-manual`) was not run, because `man/` is unchanged on this branch.
- Fresh readers (2026-09-14, [O], at `53d94913`): AC5 claims readers found 12 axes items (A1-A12) and 10 SEM items (S1-S10). AC6 one-read readers found 30 axes paragraphs (R1-R30) and 33 SEM paragraphs (E1-E33). Review lenses: [O] diff-bug 7 findings (F1-F7), [S] blame-history 1 finding (same as S1), [S] prior-review none. Dispositions follow the gate.
- Clarifying gate (2026-09-14): the maintainer chose all six claim fixes and chose to reject the one-read residual with reasons.
- Findings and dispositions. Fix now: A1 (five components miscounted, contradicting `strack2013.md` p. 4 and `?simulated_items`), A2 = F2 (new "in large samples" gloss), A4 = F4 (implied matrix called a correlation matrix), S1 = F1 = blame finding (unsupported robust-SE clause), S3 (contrast rule narrowed to the example), S9 (loose "identified" gloss). Follow-up: F6 and F7 (sweep-tool gaps for knitr tables), added to "Known sweep gaps", which the merge-gate candidate row points to. Rejected with reasons in the ledger: A3, A5 to A12, S2, S4 to S8, S10, F3, F5. Also rejected: the NEWS "no semicolons" line against a semicolon in chunk-printed table output (generated output, not prose), and the `data-raw/simulated_items.R` component comment (not user-facing, and it names the same parts).
- Fix verification (2026-09-14): an [O] fresh re-read of the six fixes passed A1, A4 and S1. It found that A2's mean match is a large-sample result, that S3 left a dangling "It", and that S9's "values" was loose. A2 now has the base wording with no gloss, and S3 and S9 use the re-reader's wording. Those three replacements had no further re-read. After the fixes, the sweep exits 0 on all four files and `--chunks` still equals base. The inventory removes nothing and adds only the ledgered items. Each `.Rmd` edit equals its `.Rmd.orig` edit. The filtered vignette and sweep tests give 0 failures and 3948 passes.
- AC5 evidence (2026-09-14): 22 claims items. 6 fixed, 3 matched by ledger rows, 13 rejected with a reason. Rows are in `cairn/references/plain-vignettes.md` under M124 with the V prefix.
- AC6 evidence (2026-09-14): 63 one-read paragraphs (30 axes, 33 SEM). All 63 are rejected with a reason in grouped ledger rows under the same section. None is fixed.
