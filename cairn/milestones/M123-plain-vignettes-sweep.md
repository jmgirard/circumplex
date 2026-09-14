# M123: The Bayesian, growth and visualization vignettes read as plain English, checked by a prose sweep

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes ship in the package and on the pkgdown site
- **Branch/PR:** m123-plain-vignettes-sweep

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
- [ ] AC3: Where a page has a `.Rmd.orig`, its shipped `.Rmd` is what the source knits to: the `vignette-precompute` workflow passes on the PR head.
- [x] AC4: For each page, every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items. `cairn/references/plain-vignettes.md` holds the precision list.
- [ ] AC5: A fresh-context reader compares each page's base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [ ] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs of each page that it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
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

## Decisions

## Review

Base commit 840fb0b0 (the merge base, equal to `origin/master`, so the default branch did not move). Head f6f167e7. The five files are the Bayesian `.Rmd`, and the `.Rmd.orig` and shipped `.Rmd` of the growth and visualization pages.

- AC1 (2026-09-14): `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0 on all five head files. Discrimination: the same command on the five base copies exits 1 each, with 24, 63, 63, 53 and 53 finding lines.
- AC2 (2026-09-14): `--chunks` output of each base copy and head file compares byte-identical with `cmp` (69, 160, 145, 172 and 142 lines, so no empty domain). Both runs exit 0 on every file. The added and removed lines of each `.Rmd.orig` diff equal those of its shipped `.Rmd` diff.
- AC4 (2026-09-14): `--inventory` at base and head, compared with `comm`, loses no item on any of the five files. Gains: `vignette("introduction-to-ssm-analysis")`, the degree and number 45 on every page, and `ssm_analyze(method = "montecarlo")` on the growth page. No removal needs a reason. The gains match ledger rows in `cairn/references/plain-vignettes.md`.
- AC7 (2026-09-14, at f6f167e7): `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings, 0 notes. `devtools::test()` reports 0 failures, 9306 passes, 9 warnings and 1 skip (`test-axes-scaled-fit.R`, not the sweep test).
- AC3: not yet verifiable. The `vignette-precompute` workflow runs on `pull_request`, and the PR opens only after the merge approval. The CI wait before merge supplies this evidence.
- Consistency gate (2026-09-14): `cairn_validate.py` exits 0. No DESIGN principle changed, so no impact report. `devtools::document()` leaves no diff and prints no `resolve link` line. `pkgdown::check_pkgdown()` finds no problems. README untouched. NEWS has one Documentation bullet with no milestone id. No new top-level file (`tools/` is in `.Rbuildignore`). Newest master push runs of `R-CMD-check.yaml` and `test-coverage.yaml` (125b76db) are `success`. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0.
