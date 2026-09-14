# M125: The circumplex-structure vignette reads as plain English

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M123
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignette ships in the package and on the pkgdown site
- **Branch/PR:** m125-plain-vignette-structure

## Goal

The vignette `evaluating-circumplex-structure` reads on one pass for an applied researcher, with every statistical claim kept.

## Scope

**In:** A prose rewrite of `vignettes/evaluating-circumplex-structure.Rmd.orig` (the "page" below), with each edit copied into the shipped `.Rmd`. It is the longest vignette, with 322 swept sentences. Uses the sweep and rules page M123 ships. Adds an M125 ledger section to `cairn/references/plain-vignettes.md`. One NEWS bullet, or an extension of an earlier one.

**Out:**
- The `precompute:volatile-numbers` markers and the chunks inside them: unchanged, because `tools/check-vignette-staleness.R` reads them.
- Changes to `tools/prose-sweep.R` beyond a parse fix this page needs. A fix goes in this milestone with a plant test. Wider sweep work goes to the candidate row "The prose sweep as a merge gate".
- Other vignettes → M123, M124, and the candidate row "Plain-English pass over the three older vignettes".
- Code chunks, chunk options, figure captions and alt text: unchanged, because the pass is about prose.

## Acceptance criteria

- [ ] AC1: For the page and its shipped `.Rmd`, `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0. So the swept prose has no sentence over 25 words, no dash, no semicolon, and no milestone, decision or review id. `cairn/references/plain-vignettes.md` defines "swept prose", "sentence" and "dash".
- [ ] AC2: No code changed. For the page and its shipped `.Rmd`, `tools/prose-sweep.R --chunks` prints the same text at the base commit and at the head.
- [ ] AC3: The shipped `.Rmd` is what the `.Rmd.orig` knits to: the `vignette-precompute` workflow passes on the PR head.
- [ ] AC4: Every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items.
- [ ] AC5: A fresh-context reader compares the base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [ ] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
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

- [x] T1: Rewrite the first half of the page's prose, by section, and copy each edit into the shipped `.Rmd`. Coverage, CPM and fit-index statements keep their measured conditions. Check them against `cairn/DESIGN.md` "Statistical conventions and their rationale".
- [x] T2: Rewrite the second half the same way.
- [x] T3: Run the base-versus-head inventory comparison and both fresh readers (claims, then one-read). Fix or disposition every item, and add the M125 ledger rows. Update NEWS.
- [ ] T4: Run check and test. Push, and confirm the `vignette-precompute` workflow passes.

## Work log

- 2026-09-14: created by /milestone-plan. The gate choices and the criteria audit are recorded in M123's work log. This milestone repeats M123's AC1-AC7 for its one page. It depends on M123 only, so it can run before or after M124.
- 2026-09-14: implement started on branch m125-plain-vignette-structure. Question gate skipped, because the plan left no choice open.
- 2026-09-14: T1 done. Sections 1 and 2 rewritten in the page and copied into the shipped `.Rmd`. Both files sweep clean up to section 3, `--prose` output matches between them, and `--chunks` output matches base.
- 2026-09-14: T2 done. Sections 3 to 6 rewritten and copied. Both files sweep with exit 0, `--prose` matches between them, `--chunks` matches base. The checklist's "see Section 4" for ipsatizing now says Section 5, the section that covers it. A ledger row at T3 records this.
- 2026-09-14: T3 done. Inventory base-to-head: nothing removed, 2 items added (the intro link and "50" rejoined on one line). [O] claims reader: 10 items (K1 empty, K3 changed meaning), fixed K3, K4, K5, K9, K10. [O] one-read reader: 39 paragraphs, fixed 13, rejected 26 in grouped ledger rows. [O] gloss re-read of the fix round: 14 points, 2 problems (G8 ladder gloss, G13 RMSEA direction), both fixed with its wording. The full test run then failed `test-cpm_boundary_vignette.R:150`, because that test pins the phrase "what has been measured about the markers covers analytic intervals only". The phrase is restored, and that file passes with NOT_CRAN=true (43 pass). NEWS bullet extended to six vignettes. Ledger rows added.
- 2026-09-14: claim audit: 41 claims read, 1 corrected — vignettes/evaluating-circumplex-structure.Rmd.orig, vignettes/evaluating-circumplex-structure.Rmd, NEWS.md. A38 ("three categories") fixed to four, and the A11 ambiguity split. The same reader re-read both, and both hold. A25 ("withholds" the displacement) is base wording, so it went to the doc-bug candidate row and a ledger row. Checkpoint: T4 still open, and `devtools::check()` is running.

## Decisions

## Review
