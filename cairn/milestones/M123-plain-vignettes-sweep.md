# M123: The Bayesian, growth and visualization vignettes read as plain English, checked by a prose sweep

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes ship in the package and on the pkgdown site
- **Branch/PR:** m123-plain-vignettes-sweep, https://github.com/jmgirard/circumplex/pull/156

## Goal

The three vignettes `bayesian-ssm-analysis`, `growth-ssm-analysis` and `advanced-visualization` read on one pass for an applied researcher, with every statistical claim kept.

## Scope

**In:** A prose sweep script, `tools/prose-sweep.R`, with its test. A rules page, `cairn/references/plain-vignettes.md`, that defines the reader, the rules and the sweep, and holds a ledger. A prose rewrite of the three pages. A page is the prose source: `vignettes/<name>.Rmd.orig` where one exists, else `vignettes/<name>.Rmd`. Where a page has a `.Rmd.orig`, the same prose edits go into its shipped `.Rmd`. One NEWS bullet.

**Out:**
- `axes-reliability` and `sem-based-ssm-analysis` → M124.
- `evaluating-circumplex-structure` → M125.
- `introduction-to-ssm-analysis`, `intermediate-ssm-analysis` and `using-instruments` → candidate row "Plain-English pass over the three older vignettes".
- A CI workflow or testthat run of the sweep over the real vignettes → candidate row "The prose sweep as a merge gate".
- Code chunks, chunk options, figure captions and alt text: unchanged here, and no row, because the pass is about prose.
- Help pages (`man/`) and the README: not in this series. No row until the user asks.

## Acceptance criteria

- [x] AC1: For each page, `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <page>` exits 0. Where the page has a `.Rmd.orig`, the same command on its shipped `.Rmd` also exits 0. So the swept prose has no sentence over 25 words, no dash, no semicolon, and no milestone, decision or review id. `cairn/references/plain-vignettes.md` defines "swept prose", "sentence" and "dash".
- [x] AC2: No code changed. For each page, and for the shipped `.Rmd` of each page that has a `.Rmd.orig`, `tools/prose-sweep.R --chunks` prints the same text at the base commit and at the head. That output is every fenced block with its opening line, minus `#>` output lines.
- [x] AC3: Where a page has a `.Rmd.orig`, its shipped `.Rmd` is what the source knits to: the `vignette-precompute` workflow passes on the PR head.
- [x] AC4: For each page, every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items. `cairn/references/plain-vignettes.md` holds the precision list.
- [x] AC5: A fresh-context reader compares each page's base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [x] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs of each page that it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
- [x] AC7: `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings and 0 notes, and `Rscript -e 'devtools::test()'` reports no failures.

## Coverage

- AC1 → T1, T3, T4, T5, T6
- AC2 → T1, T4, T5, T6
- AC3 → T5, T6, T8
- AC4 → T1, T3, T7
- AC5 → T7
- AC6 → T3, T7
- AC7 → T2, T8

## Tasks

- [x] T1: Write `tools/prose-sweep.R`. It takes files, or `-` for standard input. It drops the YAML header, HTML comments (multi-line too), the References section and every fenced block (backtick or tilde, with or without an info string). A code span and a `$...$` math span each count as one word. A heading, list item, table cell, blockquote line and paragraph end each end a sentence. A period after a single capital letter does not end one. Report mode prints each sentence over 25 words, each dash (U+2014, `---`, ` -- `, `&mdash;`), each semicolon outside code spans, math and HTML entities, and each match of `\b(M[0-9]{2,3}|D-[0-9]{3}|RR[0-9]{2})\b`. It exits 0 clean, 1 on a finding, 2 when a file has no sentences, 3 on a usage error. `--prose` prints one sentence per line. `--chunks` prints fenced blocks as AC2 states. `--inventory` prints the AC4 items, one per line, sorted.
- [x] T2: Write `tests/testthat/test-prose-sweep.R`. It skips outside the source tree. Plant each finding kind in more than one form and place: a long sentence in a list item, heading, blockquote and table cell; each dash form; semicolons in prose (flagged) and in a code span, math and `&amp;` (not flagged); an id in prose, link text and a URL. Plant a long sentence, semicolon and dash inside each fence form and a multi-line comment, and assert silence. Assert which finding each plant produces, and that a clean fixture exits 0.
- [x] T3: Write `cairn/references/plain-vignettes.md` and its `INDEX.md` line. It holds the reader profile, the rules, the precision list, the sweep definition from T1 and a ledger section per milestone. The reader knows R, data frames, correlation, regression and confidence intervals. The rules: 25-word sentences; active voice and simple tenses; define a term at first use or link the introduction vignette; change form, not claims (tidymedia's rule 6); keep every statistical qualifier. Precision-list terms include interval, credible, confidence, significant, contrast, displacement, amplitude, elevation and fit. Also record the effect of a vignette re-knit on AC2.
- [x] T4: Rewrite `vignettes/bayesian-ssm-analysis.Rmd` prose.
- [x] T5: Rewrite `vignettes/growth-ssm-analysis.Rmd.orig` prose and copy each edit into `vignettes/growth-ssm-analysis.Rmd`.
- [x] T6: Rewrite `vignettes/advanced-visualization.Rmd.orig` prose and copy each edit into `vignettes/advanced-visualization.Rmd`.
- [x] T7: Run the base-versus-head inventory comparison and both fresh readers (claims, then one-read). Fix or disposition every item, and add ledger rows. Add one NEWS bullet.
- [x] T8: Run check and test. Re-knit the pre-computed pages locally and run the staleness guard. `/milestone-review` pushes, opens the PR and confirms the `vignette-precompute` workflow passes on its head.

## Work log

- 2026-09-14: created by /milestone-plan.
- 2026-09-14: criteria audit (full mode, [O] fresh reader) returned 10 findings, all fixed before the gate. Dash forms beyond U+2014; both fence forms and the References list; no `.Rmd.orig` for the Bayesian page; chunk headers and `#>` lines in AC2; AC3 limited to pages with a `.Rmd.orig`; plants varied by form and place; a mechanical inventory under the claims reader; the one-read criterion in nestedtune D-061's shape.
- 2026-09-14: plan gate chose a 25-word cap over 30 (nestedtune's) because it matches tidymedia and the user's plain-English rules; falsified by a page where a 25-word split loses a statistical qualifier that a 30-word sentence keeps.
- 2026-09-14: plan gate chose running the sweep at review only over a CI gate in this milestone, because nestedtune needed two follow-up milestones to fix its gate's parser; falsified by a rewritten vignette regressing past the sweep before the gate row is promoted.
- 2026-09-14: plan gate chose the six newer vignettes over all nine, because the older three show fewer long sentences (8, 15, 30); falsified by a reader finding the older pages as hard to follow.
- 2026-09-14: plan gate chose to proceed over holding for the ebook candidate, because that project is not open; falsified by the user opening the ebook project before M125 ships.
- 2026-09-14: plan split the six pages across three milestones rather than one, because one milestone would exceed ~10 tasks and the 150-line cap; falsified by M123's review showing one page per task takes far less than a session.
- 2026-09-14: implement started on branch m123-plain-vignettes-sweep. Question gate skipped, because the plan left nothing open.
- 2026-09-14: T1 done. `tools/prose-sweep.R` as specified, plus three refinements the plan did not name. Latin abbreviations (`e.g.`, `i.e.`, `et al.`, `vs.`, `cf.`) do not end a sentence. Horizontal rules and table alignment rows hold no prose. Code and math spans are also skipped for dashes. The rules page records all three.
- 2026-09-14: T2 done. `test-prose-sweep.R` passes 25 expectations. Its sentence-split test caught a regex that never split sentences (no `perl = TRUE`), fixed before commit. Full `devtools::test()` 0 failures.
- 2026-09-14: T3 done. `cairn/references/plain-vignettes.md` and its INDEX line. The re-knit note records that knitr rewrites chunk openings to ` r` and that `--chunks` drops `#>` lines.
- 2026-09-14: T4 done. Bayesian page sweep exits 0 (was 11 long sentences, 11 dashes, 2 semicolons). `--chunks` unchanged. Inventory loses nothing and gains one link to the introduction vignette, added where the page first names the three parameters.
- 2026-09-14: T5 done. Growth page sweep exits 0 on both `.Rmd.orig` and `.Rmd` (was 29 long sentences, 28 dashes, 6 semicolons). `--chunks` unchanged on both. The added and removed lines of the two diffs are identical. Inventory gains only the introduction-vignette link.
- 2026-09-14: T6 done. Visualization page sweep exits 0 on both `.Rmd.orig` and `.Rmd` (was 33 long sentences, 15 dashes, 5 semicolons). `--chunks` unchanged on both, the two diffs carry identical edits, and the inventory is unchanged. Two glosses added in place: "resultant (the average vector)" and "munches (splits into short pieces)".
- 2026-09-14: T7 in progress (checkpoint). Inventory comparison: no item lost on any page. The [O] claims reader listed 20 items (1 medium), and the [O] one-read reader listed 37 paragraphs. Fixed 16 claims items and 33 one-read items on all five files, with each gloss checked against the code. The other 8 kept items are ledger rows. NEWS bullet drafted. An [O] re-read of the fix diff is still running.
- 2026-09-14: T7 done. The [O] re-read of the fix diff (one pass) found 6 items (1 medium: the reason given for correlated person effects did not imply correlation). All 6 are fixed. All five files sweep clean, `--chunks` is unchanged, and each `.Rmd.orig` and `.Rmd` pair carries identical edits.
- 2026-09-14: T8 in progress (checkpoint). Re-knitting growth and visualization at 3526c38b on a scratch worktree left the tree byte-identical, and `tools/check-vignette-staleness.R` exits 0. Opening the PR moves to review (git model), so the `vignette-precompute` run on the PR head is confirmed there.
- 2026-09-14: claim audit: 81 claims read, 3 corrected — tools/prose-sweep.R, NEWS.md, vignettes/advanced-visualization.Rmd.orig (and its .Rmd). Re-read of the three corrections pending.
- 2026-09-14: claim-audit re-read: NEWS and the visualization wording hold. The `--chunks` header claim was still wrong, because a page with no fenced block crashed `writeLines()`. A regression test reproduced that exact error, and the fix (`as.character()`) makes it pass. `test-prose-sweep.R` now passes 27 expectations, so the header claim holds.
- 2026-09-14: T8 done. At f84cb531, `devtools::check(args = "--no-manual")` gives 0 errors, 0 warnings, 0 notes, and `devtools::test()` gives 0 failures. Minor amendment: T8's push step moves to `/milestone-review`, because the git model opens the PR only after approval. AC3 is unchanged. Status set to review.
- 2026-09-14: review started. No PR existed, so resume route (d). The default branch had not moved. Five fresh reviewers ran: claims reader (23 items), one-read reader (62 paragraphs), diff-bug (22), blame-history (1), prior-review (no evidence).
- 2026-09-14: triage questions at the gate. The user chose: fix all claim drift now, gloss the top one-read items and reject the rest with reasons, fix five sweep gaps and defer the rest to the prose-sweep candidate row.
- 2026-09-14: fix-now commits ce770a4e and 5e7b38a9 on the branch. An [O] re-read of ce770a4e found 17 items, 12 fixed in 5e7b38a9. b5674f31 merged the two plain-English candidate rows and added the `ssm_plot_trajectory()` help-page row, to keep ROADMAP under 60 lines and 24,000 bytes.
- 2026-09-14: step-7 approval: m123-plain-vignettes-sweep approved for merge
- 2026-09-14: PR #156 opened. The CI watch hit its timeout: `vignette-precompute`, `pkgdown` and `matrix` pass, and the three R-CMD-check release jobs are still pending. Session stopped before merge. Marker `cairn/.merge-approved` written for PR #156.

## Decisions

## Review

Base commit 840fb0b0 (the merge base, equal to `origin/master`, so the default branch did not move). The five files are the Bayesian `.Rmd`, and the `.Rmd.orig` and shipped `.Rmd` of the growth and visualization pages. AC1, AC2 and AC4 were first run at f6f167e7 and re-run after the gate-directed fixes, at 5e7b38a9 (vignette text unchanged since). The lines below give the re-run.

- AC1 (2026-09-14): `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0 on all five head files. Discrimination: the same command on the five base copies exits 1 each, with 24, 63, 63, 53 and 53 finding lines.
- AC2 (2026-09-14): `--chunks` output of each base copy and head file compares byte-identical with `cmp` (69, 160, 145, 172 and 142 lines, so no empty domain). Both runs exit 0 on every file. The added and removed lines of each `.Rmd.orig` diff equal those of its shipped `.Rmd` diff.
- AC4 (2026-09-14): `--inventory` at base and head (43, 69, 69, 75 and 77 base items, with the precision list as extended at review), compared with `comm`, loses no item on any of the five files. Gains: `vignette("introduction-to-ssm-analysis")` and the degree and number 45 on every page, and `term: unwrap` on the Bayesian page. No removal needs a reason. The first two match ledger rows in `cairn/references/plain-vignettes.md`. The third is the unwrap step of the circular-quantile description, which matches `R/ssm_bootstrap.R` and claims item C15 below.
- AC5 (2026-09-14): a fresh [O] claims reader compared base and head of the three pages and listed C1 to C23. Dispositions: C1 fixed (the added "a signed distance is never more than a half-turn" premise removed). C2 fixed (the base reason "profile tilts are rarely aligned with an axis" restored, and the sentence split to fit 25 words). C3 fixed ("stacked-outcome formulation ... `varIdent` and `corSymm` machinery" restored). C4 fixed ("the layer supplies the ordering, not the drawing" restored). C5 fixed ("trains it from the data" restored). C6 fixed ("validation simulations of this near-origin case ... the degraded wave"). C7 fixed ("amplitude well above zero" replaced by "origin of the (x, y) plane"). C8 rejected: the head wording equals `a_lci / (a_uci - a_lci) >= 0.35` in `R/ssm_oop.R`. C9 fixed ("the same large-sample (asymptotic) step that the package's Monte Carlo engine takes"). C10 fixed ("gaps in the argument" restored). C11 fixed ("makes the branch of every later wave ambiguous"). C12 rejected: "average vector" names the same averaging as "resultant" and matches the mean. C13 rejected: the "not a significance test" qualifier is intact and the contrast with a linear-parameter interval is the base's. C14 rejected: the parameter list matches `R/ssm_draws.R`. C15 rejected: the unwrap step matches `R/ssm_bootstrap.R` and the CLAUDE.md circular-quantile rule. C16 matched by ledger row C16. C17 to C23 rejected: added glosses and cross-references, each checked by the reader against `R/` or the chunks.
- AC6 (2026-09-14): a fresh [O] reader with the rules-page profile listed R1 to R62. The gate chose: gloss the highest-ranked items, reject the rest with a reason. Fixed: R1 (posterior distribution, posterior draws, circular statistics), R2 (prior information, derived-quantity example), R3 (outcome indicator, person-level random effects), R4 (random intercept, fixed effects, fixed-seed subsample), R5 (fixed effects, fixed-effect covariance matrix, evaluate step), R6 (fitted model draws), R7 (credible interval, circular mean, unwrapped), R8 (pointwise coverage, sentence split), R9 (marginal summary), R10 (why separate fits set the covariance to 0), R12 (`ssm_plot_trajectory()` named), R13 (why `d_lci > d_uci`), R14 (induced prior), R15 (draw propagation), R16 (shortest signed rotation), R22 (origin of the (x, y) plane, by the C7 fix). Partly fixed, remainder rejected: R4 (`jz2017` is described by its help page), R5 (circular means and equal-tailed intervals, ledger G7), R6 (`varIdent` and `corSymm`: a gloss restates the C3 claim), R8 ("our validation simulations" has no citation to add in a form-only pass), R11 (parametric bootstrap glossed, and a degrees-of-freedom gloss added a claim, so it was removed), R14 (the prior-predictive gloss was wrong, so it was removed, and Rayleigh stays unnamed). Rejected because the page's method needs Bayesian, mixed-model or simulation background that a form-only pass cannot teach: R17, R23, R28, R29, R30, R32, R33, R34, R35, R53, R57, R58, R59. Rejected because the linked introduction vignette or the next chunk defines the term: R18, R21, R26, R27, R31, R37, R43, R49, R50, R51, R56, R60. Rejected because the visualization page's reader builds ggplot2 figures and knows its vocabulary: R24, R25, R38, R40, R44. Rejected as readable on one pass (a minor referent or wording point): R19, R20, R36, R39, R41, R42, R45, R46, R47, R48, R52, R54, R55, R61, R62.

Independent review (2026-09-14), three lenses plus one re-read of the fix commit, each fresh-context:

- [O] diff-bug, 22 findings. Fixed: F1 (invalid UTF-8 truncated input silently, now exit 3 with a test), F2 (precision list gains significance, hypothesis, unwrap, nominal, certif, simultaneous), F3 (inventory kept no minus sign, now kept with a test), F11 (non-UTF-8 locale crashed with exit 1, now works with a test, rules page corrected), F13 (= C3), F14 (= C9), F21 (NEWS "no dashes" now "no em dashes"). Follow-up to the prose-sweep candidate row, listed on the rules page under "Known sweep gaps": F5, F6, F7, F8, F9, F10, F12, F15, F16, F17, F18, F19. Rejected: F4 (presence, not count, is the inventory AC4 names, now stated on the rules page, and the AC5 reader covers counts), F20 (the work log is append-only history), F22 ("linear parameters" follows the code's grouping in `R/ssm_draws.R`).
- [S] blame-history, 1 finding: the "layer supplies the ordering, not the drawing" sentence from 15ed598c was reversed. Fixed (= C4).
- [S] prior-review: no prior-review evidence (archived Reviews of M26, M34, M50 and M120 checked, no PR review comments on GitHub).
- [O] fix re-read of ce770a4e, 17 findings. Fixed: F1 and F2 (`--inventory` still crashed in a C locale on U+2212, constants now marked UTF-8, test added and shown failing on the ce770a4e script), F3 ("no jump at 360°" was wrong, now "no jump near the mean"), F4 ("one distribution for each parameter" now "one distribution over the parameters"), F5 (wrong prior-predictive gloss removed), F6 ("widens intervals" gloss removed), F7 ("wrapped into [0°, 360°)" now "wrapped onto the circle", since a bound at the pole reads 360), F9 (appositive after "structural parameters" removed), F11 ("random samples" now "samples"), F12 (fitted model draws gloss reworded), F13 ("Such a design" referent restated), F16 (ledger row wording). Follow-up: F8, the `ssm_plot_trajectory()` help page states the signed-distance recipe the vignette calls wrong, now the candidate row "`ssm_plot_trajectory()` help misstates bound placement" for `/hotfix`. Rejected: F10 (the sign rule is stated on the rules page), F14 (a base claim, outside a form-only pass), F15 (notes only), F17 (C-locale printing is cosmetic). The second fix round (5e7b38a9) had no further re-read. Each of its vignette edits removes a gloss or narrows wording.
- AC7 (2026-09-14): at f6f167e7, `devtools::check(args = "--no-manual")` reported 0 errors, 0 warnings, 0 notes, and `devtools::test()` 0 failures with 9306 passes. Re-run at b5674f31, after the fixes: check 0 errors, 0 warnings, 0 notes. Tests: 0 failures, 9314 passes, 9 warnings, 1 skip (`test-axes-scaled-fit.R`, not the sweep test). `devtools::document()` still prints no `resolve link` line and leaves no diff.
- AC3 (2026-09-14): the `vignette-precompute` check passes on PR #156, whose head is 70e7d2d3 (the approval commit).
- Consistency gate (2026-09-14): `cairn_validate.py` exits 0. No DESIGN principle changed, so no impact report. `devtools::document()` leaves no diff and prints no `resolve link` line. `pkgdown::check_pkgdown()` finds no problems. README untouched. NEWS has one Documentation bullet with no milestone id. No new top-level file (`tools/` is in `.Rbuildignore`). Newest master push runs of `R-CMD-check.yaml` and `test-coverage.yaml` (125b76db) are `success`. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0.
