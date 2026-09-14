# M125: The circumplex-structure vignette reads as plain English

- **Status:** review
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

- [x] AC1: For the page and its shipped `.Rmd`, `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R <file>` exits 0. So the swept prose has no sentence over 25 words, no dash, no semicolon, and no milestone, decision or review id. `cairn/references/plain-vignettes.md` defines "swept prose", "sentence" and "dash".
- [x] AC2: No code changed. For the page and its shipped `.Rmd`, `tools/prose-sweep.R --chunks` prints the same text at the base commit and at the head.
- [ ] AC3: The shipped `.Rmd` is what the `.Rmd.orig` knits to: the `vignette-precompute` workflow passes on the PR head.
- [x] AC4: Every number, degree value, code-span name and precision-list term in the base prose also appears in the head prose, or the Review section records its removal with a reason. The `--inventory` output of `tools/prose-sweep.R` at the base commit and at the head, compared, lists these items.
- [x] AC5: A fresh-context reader compares the base and head prose, section by section. It lists each claim added, dropped, or changed in meaning, a lost statistical qualifier included. The Review section gives each listed item one disposition: fixed, rejected with a reason, or matched by a ledger row with its evidence.
- [x] AC6: A fresh-context reader with the reader profile from `cairn/references/plain-vignettes.md` lists the paragraphs it cannot follow on one read. The Review section records each listed paragraph as fixed or as rejected with a reason.
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

- [x] T1: Rewrite the first half of the page's prose, by section, and copy each edit into the shipped `.Rmd`. Coverage, CPM and fit-index statements keep their measured conditions. Check them against `cairn/DESIGN.md` "Statistical conventions and their rationale".
- [x] T2: Rewrite the second half the same way.
- [x] T3: Run the base-versus-head inventory comparison and both fresh readers (claims, then one-read). Fix or disposition every item, and add the M125 ledger rows. Update NEWS.
- [x] T4: Run check and test. Re-knit locally and run the staleness check. `/milestone-review` pushes, opens the PR and confirms that the `vignette-precompute` workflow passes on its head.

## Work log

- 2026-09-14: created by /milestone-plan. The gate choices and the criteria audit are recorded in M123's work log. This milestone repeats M123's AC1-AC7 for its one page. It depends on M123 only, so it can run before or after M124.
- 2026-09-14: implement started on branch m125-plain-vignette-structure. Question gate skipped, because the plan left no choice open.
- 2026-09-14: T1 done. Sections 1 and 2 rewritten in the page and copied into the shipped `.Rmd`. Both files sweep clean up to section 3, `--prose` output matches between them, and `--chunks` output matches base.
- 2026-09-14: T2 done. Sections 3 to 6 rewritten and copied. Both files sweep with exit 0, `--prose` matches between them, `--chunks` matches base. The checklist's "see Section 4" for ipsatizing now says Section 5, the section that covers it. A ledger row at T3 records this.
- 2026-09-14: T3 done. Inventory base-to-head: nothing removed, 2 items added (the intro link and "50" rejoined on one line). [O] claims reader: 10 items (K1 empty, K3 changed meaning), fixed K3, K4, K5, K9, K10. [O] one-read reader: 39 paragraphs, fixed 13, rejected 26 in grouped ledger rows. [O] gloss re-read of the fix round: 14 points, 2 problems (G8 ladder gloss, G13 RMSEA direction), both fixed with its wording. The full test run then failed `test-cpm_boundary_vignette.R:150`, because that test pins the phrase "what has been measured about the markers covers analytic intervals only". The phrase is restored, and that file passes with NOT_CRAN=true (43 pass). NEWS bullet extended to six vignettes. Ledger rows added.
- 2026-09-14: claim audit: 41 claims read, 1 corrected — vignettes/evaluating-circumplex-structure.Rmd.orig, vignettes/evaluating-circumplex-structure.Rmd, NEWS.md. A38 ("three categories") fixed to four, and the A11 ambiguity split. The same reader re-read both, and both hold. A25 ("withholds" the displacement) is base wording, so it went to the doc-bug candidate row and a ledger row. Checkpoint: T4 still open, and `devtools::check()` is running.
- 2026-09-14: T4 done. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes. `devtools::test()`: FAIL 0, PASS 9319. Local `tools/precompute-vignettes.R evaluating-circumplex-structure` then `tools/check-vignette-staleness.R`: all 7 up to date. The re-knit changed one masked `#>` line only, and the committed copy is restored. Minor amendment: T4's push step moves to `/milestone-review`, which opens the PR after approval (tracking rules). AC3 is read on the PR head there.
- 2026-09-14: review started. Five fresh readers ran. Gate triage: five prose fixes and one ledger fix made, two base doc bugs added to the candidate row, 38 one-read paragraphs rejected in groups. AC1, AC2, AC4, AC5, AC6 verified. Checkpoint: AC7 test run in progress, AC3 waits for the PR head.
- 2026-09-14: step-7 approval: m125-plain-vignette-structure approved for merge

## Decisions

## Review

Base commit 68874adf (master, unchanged since the branch was cut, so no sync merge). All evidence below is from 2026-09-14 on the branch head after the gate fixes, unless a line says otherwise.

- AC1: `LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R` exits 0 on `vignettes/evaluating-circumplex-structure.Rmd.orig` and on the shipped `.Rmd`.
- AC2: For both files, `cmp` finds the `tools/prose-sweep.R --chunks` output at 68874adf and at head byte-identical. That is 98 lines for the page and 95 for the `.Rmd`.
- AC4: `--inventory` at base and at head, compared under C-locale sort, removes nothing in either file. It adds two items, `vignette("introduction-to-ssm-analysis")` and degree 50, and both have ledger rows. (A first compare under the default locale showed `VT2` as removed and added, a sort-order artifact.)
- AC5: A fresh [O] claims reader compared base and head prose by section. It listed RK1 to RK17 and found no dropped claim and no lost qualifier. Dispositions follow.
  - RK2 ("For example" made the thresholds an example): fixed at the gate, "For example" dropped.
  - RK1, RK3 to RK13 (new glosses and explicit links): kept. The reader checked each one against `R/`, and I re-read RK1 against `R/cpm_oop.R`, which prints "not every marker was measured".
  - RK14 to RK17: matched by the M125 ledger rows (intro link, Section 5 cross-reference, K8 and K2).
  - The diff-bug lens's RD2, RD3, RD6 and RD8 were also claim-level and were fixed at the gate (see the findings list). None of the fixes adds a gloss, so no gloss re-read was owed under rule 9.
- AC6: A fresh [O] one-read reader with the reader profile listed 38 paragraphs, RP1 to RP38. At the gate the maintainer chose to reject all of them under grouped reasons, as in M124.
  - A gloss needs a fact the page does not state: RP2, RP3, RP4, RP6, RP9, RP10, RP12, RP13, RP14, RP16, RP25, RP26, RP28, RP32, RP33, RP37, RP38.
  - Base wording or base structure, where a table, a reason or a new referent is a new claim: RP1, RP5, RP7, RP8, RP15, RP17, RP18, RP19, RP20, RP21, RP22, RP24, RP27, RP29, RP30, RP31, RP36. (RP21's "next section" was fixed at the gate as RD8.)
  - The linked introduction vignette defines the term, or the term is standard for the reader: RP11, RP34, RP35.
  - RP23 ("weak" matches no listed category): a base claim, sent to the doc-bug candidate row with RD4.
- AC7: `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings, 0 notes (7 min). It ran on 1989191d, before the gate fixes, which changed vignette prose only. `devtools::test()` on the fixed tree reports FAIL 0, WARN 9, SKIP 1, PASS 9319.
- AC3: not yet read. It needs the `vignette-precompute` run on the PR head, which opens at merge approval. Local proxy: `--prose` of the page and of the shipped `.Rmd` match except for the three knitr `<img>` lines.

Consistency gate: `cairn_validate.py` passes all checks. `devtools::document()` prints 0 `resolve link` lines and leaves `man/` and `NAMESPACE` unchanged. `pkgdown::check_pkgdown()` finds no problems. NEWS carries the six-vignette bullet with no milestone id. The master-red alert audit, its dry run and the branch-protection check exit clean. The newest master push runs of `R-CMD-check.yaml` and `test-coverage.yaml` are both `success` (26664a3a). No principle changed, so no impact report.

Independent review, a three-lens fan-out because the tier is user-facing. The maintainer triaged at the gate.

- [O] diff-bug lens, RD1 to RD10. No statistical error and no failed criterion.
  - RD1 (ledger "Items not in a row were fixed" also covered 12 confirming gloss points): fixed, ledger line reworded.
  - RD2 (the split sentence lost "analytic" on the large-N caution): fixed, "the analytic caution".
  - RD3 ("angles and weights" beside "all three families"): fixed, the sentence names all three families.
  - RD6 ("It" after "their Eq. 3"): fixed, "$f_a$ is about .55".
  - RD8 ("(next section)" points at a subsection): fixed, "(next subsection)", with a ledger row.
  - RD4 ("weak" is not a printed label) and RD5 (`cpm_fit()` "commits to the theoretical angles", but the default model estimates them): follow-up. Both are base claims, added to the doc-bug candidate row, with a ledger row.
  - RD7 (two added claims with no row): kept, same items as RK10 and RK5, verified.
  - RD9 (source lines over 80 characters), RD10 (short NEWS line): rejected, cosmetic with no rendered effect.
- [S] blame-history lens: nothing undoes past intent. The RR05 "not identical" wording, the pinned test phrases and the Hu and Bentler and Browne and Cudeck caveats are intact. Its item 1 is claim audit A25, already on the doc-bug row. Its item 5 (Wilson interval gloss, "a category that the next subsection defines") matches RK8 and RK13, kept.
- [S] prior-review lens: no findings. The M123 and M124 archive lessons (NEWS count, doc bugs to a candidate row, known sweep gaps) hold. The GitHub probe returned no review threads.
