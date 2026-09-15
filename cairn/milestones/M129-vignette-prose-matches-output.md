# M129: Vignette prose matches the package's code and printed output

- **Status:** review
- **Priority:** normal
- **Depends on:** M127, M128
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — corrects prose and table layout in shipped vignettes
- **Branch/PR:** `m129-vignette-prose-matches-output`

## Goal

Four known wrong vignette claims are corrected, the SEM results tables fit on one line per estimate, and each sentence that quotes printed output matches the output after M127's print changes.

## Scope

**In:** the SEM latent results table layout, doc claims (ii) to (v) from the former doc-bug candidate row, the prose that reads M127's new accuracy summary and invariance ladder print, and the search for dashes in printed output. These came from the M128 plan before its re-cut. Every precomputed vignette is re-knitted.

**Out:** hiding simulation and table-building code, and the simulated datasets → M128. Print layouts themselves → M127. kable to gt → its candidate row. The three older vignettes' plain-English pass → its candidate row.

## Acceptance criteria

- [x] AC1: In the article from `pkgdown::build_article("sem-based-ssm-analysis")`, viewed 1280 pixels wide, each SSM results table has each estimate and its interval on one line.
- [x] AC2: Take each vignette whose `#>` output lines differ between 73218afa, before M127 and M128, and the branch head: the precomputed `.Rmd` files by their `#>` lines, and `bayesian-ssm-analysis` and `using-instruments` by the `#>` lines of their `pkgdown::build_article()` HTML built at both commits. In its `tools/prose-sweep.R --prose` output, each sentence that holds a number, a code span or a quoted label and names a value, column, row, line or label of a chunk's printed output names one that the re-knitted output shows.
- [x] AC3: Four vignette claims are checked against the code and corrected where wrong. (ii) `sem-based-ssm-analysis.Rmd.orig` says the plane factors are "fixed isotropic and orthogonal", but `R/ssm_sem_syntax.R` fixes them only under the scaled tier. (iii) `evaluating-circumplex-structure.Rmd.orig` says the output "withholds" the OCPD displacement and its interval, but `print.circumplex_ssm` (`R/ssm_oop.R`) prints both with a not-interpretable note. (iv) It names a "weak" classification, but `R/fit_structure_oop.R` prints "not clearly supported" or "unsupported". (v) It says `cpm_fit()` "commits to" the theoretical angles, but its default quasi-circumplex model (`R/cpm_fit.R`) estimates them. Each line that `grep -n "withholds\|isotropic\|commits to\|weak"` returns over those two files either is corrected or states what the code does.
- [x] AC4: The search `grep -nE -- '^#>.*(-- |--$|—)' vignettes/*.Rmd` returns only lines that also match `^#>.*[0-9.]\s+--\s*$`, the missing-value cells of the axes-reliability tables. The same search over the `#>` lines of the text extracted, with entities decoded, from the `bayesian-ssm-analysis` and `using-instruments` HTML built by `pkgdown::build_article()` returns no line.
- [ ] AC5: `tools/prose-sweep.R` exits 0 on each touched source. `tools/check-vignette-staleness.R` passes after `tools/precompute-vignettes.R`, and the `vignette-precompute` CI job passes on the PR. `devtools::test()` and `devtools::check(args = "--no-manual")` report no failure, warning or note that is new relative to master.

## Coverage

- AC1 → T2
- AC2 → T1, T4
- AC3 → T3
- AC4 → T4
- AC5 → T1, T5

## Tasks

- [x] T1: Cut the branch after M127 and M128 merge. Build the `bayesian-ssm-analysis` and `using-instruments` articles with pkgdown at 73218afa as AC2's baseline. Run `devtools::install()` so the knit reads the new package (lesson M21 family).
- [x] T2: Make the SEM latent table text smaller with non-breaking spaces or an inline style in its hidden `kable()` chunk, and add `drop_xy = TRUE` if that fits the prose. Build the article with pkgdown and view it at 1280 px.
- [x] T3: Fix doc claims (ii) to (v), checking each against its `R/` file, and re-read each corrected sentence against the code (plain-vignettes rule 9). Keep the phrases that `tests/testthat/test-cpm_boundary_vignette.R` matches.
- [x] T4: Re-knit every precomputed vignette and build the two knit-at-build articles. List each sentence AC2 selects with the output line it names, in the work log. Update the prose that no longer matches, and run AC4's search.
- [x] T5: Run `tools/prose-sweep.R`, `tools/check-vignette-staleness.R`, `devtools::test()` and `devtools::check(args = "--no-manual")`. Add a NEWS.md documentation entry.

## Work log

- 2026-09-14: created by /milestone-plan as the remainder of the M128 re-cut: the SEM table layout, doc claims (ii) to (v), output-dash search and M127 prose from the earlier M128 plan. The earlier gate's choices carry over: kable with a hidden render chunk over gt, and the doc claims fixed here over /hotfix.
- 2026-09-14: criteria audit (full mode, fresh [O] reader, shared with the M128 re-cut) found 4 items on this draft, all fixed: phrase anchors in place of line numbers (AC3), numbers and a recorded branch-cut SHA (AC2), knit-at-build HTML and an exact exemption pattern (AC4), and "SSM results table" with a narrower goal (AC1).
- 2026-09-14: second fresh [O] audit found 4 items on this file, all fixed. AC2's baseline is 73218afa, so that vignettes M127 re-knits stay in scope, and knit-at-build HTML is built at both commits (T1). AC3 settles each grep hit. AC4 reads decoded HTML text.
- 2026-09-15: implement started, branch cut from master at 714d5c78. T1: pkgdown built the baseline `bayesian-ssm-analysis` and `using-instruments` in a 73218afa worktree against a 73218afa scratch-library install, saved outside the repo. The branch package is installed with `R CMD INSTALL`.
- 2026-09-15: question gate: the SEM latent table uses `drop_xy = TRUE` with non-breaking spaces (maintainer choice over keeping X/Y with sideways scroll or smaller text).
- 2026-09-15: T3 fixed four claims against the code. (ii) The plane constraint is scoped to the scaled tier, and the strict tier frees the factor covariance (`R/ssm_sem_syntax.R`). (iii) Three sentences say print shows the displacement with a not-interpretable note (`R/ssm_oop.R`). (iv) "weak" became "not clearly supported" (print) and "unsupported" (summary), per `R/fit_structure_oop.R`. (v) The `cpm_fit()` default estimates angles, and constrained-angles fixes them (`R/cpm_fit.R`). Grep hits kept as true: sem 448 (`sem_dcfi_flag()` gives no verdict out of scope), evaluating 180 (`R/cpm_fit.R` Hessian marker text), 238 and 405 (simulation and literature findings), 573 (amplitude bias). prose-sweep exits 0 on both sources. The T2 chunk edit rides in this commit, not yet verified.
- 2026-09-15: T2: the SEM latent table uses `drop_xy = TRUE` and non-breaking spaces, and the shown call matches. The pkgdown article, served locally at 1280 px with site CSS, gives 1 line per cell (table 776 px in an 800 px column). A control with the old 7-column spaced markup in the same column wrapped 5 cells to 2 lines.
- 2026-09-15: T4 and T5 in progress. `tools/check-vignette-staleness.R` exits 0 on all 7 pre-computed vignettes. AC4 `.Rmd` search returns only the 3 axes-reliability missing-value lines, and the HTML search over both knit-at-build articles (35 and 275 `#>` lines) returns none. Neither knit-at-build article's `#>` lines differ from 73218afa, so AC2 covers the 5 changed pre-computed vignettes. A NEWS.md documentation entry is drafted, and `ssm_sem()` defaults to `model = "scaled"` (`R/ssm_sem.R`).
- 2026-09-15: T4 ledger, delegated to a fresh [O] reader and checked by the session against output and code. 101 sentences were selected (advanced-visualization 6, axes-reliability 15, evaluating 52, growth 13, sem 15). 7 did not match and were fixed, 6 in evaluating and 1 in axes-reliability. They were the removed guardrail table (now the `cert` column and `Guardrail` lines), `VT2` (prints `Variance`), `inadequate` (prints `INADEQUATE`), the amplitude miss direction, "about the benchmark rate" (1.0% vs 2.5%), the verdict block labels, and axes `NA` (prints `--`). 3 doubts needed no change: the short ladder (prose names the argument), `block_specificity` symbol (returned object, no chunk), Wilson interval (named in the printed header).
- 2026-09-15: T5 tests. `devtools::test()` gives FAIL 0, WARN 9, SKIP 1, PASS 9512. The warnings come from `test-ci_accuracy.R`, `test-pole-values.R` and `test-ssm_sem.R`, which read no vignette, and the branch leaves `R/` and `tests/` identical to master. `test-cpm_boundary_vignette.R` with `NOT_CRAN=true` passes 43 expectations after the T4 edits. The staleness check exits 0 on all 7 vignettes after the T4 commit.
- 2026-09-15: claim audit: 27 claims read, 3 corrected — NEWS.md, vignettes/evaluating-circumplex-structure.Rmd.orig
- 2026-09-15: the fresh [O] reader found the `cpm_fit()` angle claim imprecise in the vignette and NEWS (one reference angle stays fixed) and "no more often than the benchmark" imprecise (the caution rule reads the interval's lower bound). Its one re-read cleared all 3. The split sentence passes prose-sweep.
- 2026-09-15: T5: `devtools::check(args = "--no-manual")` gives 0 errors, 0 warnings, 0 notes (Status: OK). Its tarball was built at 72acdfcf, before the prose-only audit corrections in 4d705fa3, which change no code or chunk. prose-sweep exits 0 on the three touched sources. The `vignette-precompute` CI job runs on the PR at review. Status set to review.

- 2026-09-15: review checkpoint, half done. AC1 to AC4 hold on fresh evidence and are ticked. AC5 waits on a `devtools::check()` run in the primary checkout. The worktree run gave 0 errors, 0 warnings and 1 note, and that note names `.git`, which a worktree checkout creates.
- 2026-09-15: step-7 approval: m129-vignette-prose-matches-output approved for merge, after the fix-now findings F1 to F7 land and pass prose-sweep, the staleness check and check() again.

## Decisions

## Review

Fresh evidence, 2026-09-15, at branch head 00799721 with master at 714d5c78.
The branch already contains master, so no merge was needed.

- AC1: pkgdown built `sem-based-ssm-analysis` in a clean worktree at the
  branch head. The build read a scratch-library install of that same commit.
  The article was served over HTTP with its built site CSS and measured at a
  1280 px viewport. The latent SSM results table is 776 px wide in an 800 px
  column, and no cell wraps. Its one data row reads `NARPD | 0.25 (0.21,
  0.29) | 0.23 (0.19, 0.27) | 92.1 (82.5, 104.5) | 0.975`. The article's
  other table, "Two estimands for a group difference", holds prose only. It
  carries no estimate and no interval, so it is not an SSM results table.
  PASS.
- AC2: the selection was recomputed. Five pre-computed vignettes have `#>`
  lines that differ from 73218afa: advanced-visualization, axes-reliability,
  evaluating-circumplex-structure, growth-ssm-analysis and
  sem-based-ssm-analysis. Both knit-at-build articles were rebuilt at 73218afa
  and at the branch head, each against a scratch-library install of its own
  commit. Their decoded `#>` lines are identical across the two commits, 35
  lines for bayesian-ssm-analysis and 275 for using-instruments, so neither
  enters the selection. A fresh [O] reader then read the `--prose` output of
  the five selected vignettes against their `#>` lines. It selected 92
  sentences, 8 in advanced-visualization, 16 in axes-reliability, 38 in
  evaluating-circumplex-structure, 15 in growth-ssm-analysis and 15 in
  sem-based-ssm-analysis. It found no mismatch. The session recomputed its
  harder arithmetic and agreed. PASS.
- AC3: `grep -n "withholds\|isotropic\|commits to\|weak"` over the two sources
  returns 6 lines. Each was read against the code. `sem...orig:432` matches
  `R/ssm_sem_syntax.R:395-407` and `:425-430`, where the scaled tier emits
  `cx ~~ 0*cy` with unit plane variances and the strict tier frees them.
  `sem...orig:448` matches `R/ssm_sem.R:782`, where `sem_dcfi_flag()` returns
  `NA_character_` outside the two-group ML scope. `evaluating...orig:180`
  matches the marker text at `R/cpm_fit.R:876` and its label at `:1429`. The
  other 3 hits state a literature or simulation finding, not a code claim. The
  four named claims were also checked at their corrected sites.
  `R/ssm_oop.R:198` prints the displacement with a not-interpretable note.
  `R/fit_structure_oop.R:59` and `:85` give "not clearly supported" and
  "unsupported". `R/cpm_fit.R:1481` makes `"quasi-circumplex"` the default.
  That variant frees every angle but the reference scale's. PASS.
- AC4: the `.Rmd` search returns 3 lines, all in axes-reliability. Each also
  matches the exemption pattern `^#>.*[0-9.]\s+--\s*$`, so each is a
  missing-value cell. The same search over the decoded `#>` lines of both
  branch-head knit-at-build articles returns no line. PASS.
- AC5, local clauses: `tools/prose-sweep.R` exits 0 on each of the 3 touched
  `.Rmd.orig` sources. In a clean worktree at the branch head,
  `tools/precompute-vignettes.R` exits 0 and `tools/check-vignette-staleness.R`
  then exits 0, with all 7 pre-computed vignettes up to date.
  `devtools::test()` gives FAIL 0, WARN 9, SKIP 1, PASS 9512. The 9 warnings
  come from `test-ci_accuracy.R`, `test-pole-values.R` and `test-ssm_sem.R`.
  The branch leaves `R/`, `src/` and `tests/` identical to master, so no test
  result is new. `devtools::check(args = "--no-manual")` in the primary
  checkout gives 0 errors, 0 warnings, 0 notes. A first run in a worktree gave
  1 note naming `.git`, because a worktree's `.git` is a plain file that
  `.Rbuildignore` does not exclude. AC5, CI clause: no PR exists before the
  approval gate, so the `vignette-precompute` job on the PR is not yet
  evidenced. The box stays unticked until that job passes at the merge step.

Consistency gate: `cairn_validate.py` exits 0 with every check passing and no
`release window` advisory. No DESIGN.md principle changed, so `cairn_impact`
was skipped. In the worktree, `devtools::document()` with `cli.width = 500`
leaves no diff and prints no `resolve link` line. `pkgdown::check_pkgdown()`
finds no problem. README files and top-level files are untouched, and NEWS.md
has 3 documentation entries. `tools/check-master-red-alert.R`,
`tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit
0. Master watches: the newest `R-CMD-check.yaml` push run with a verdict is
66f8c2ed, success, and the run on 8d8cbb93 was still in progress. The newest
`test-coverage.yaml` push run is 8d8cbb93, success. PASS.

Independent review: [O] diff-bug, [S] blame-history and [S] prior-review
lenses, plus the [O] AC2 reader's doubts. The prior-review lens found no
prior-review evidence against the diff, and its PR-comment probe returned
nothing. The blame-history lens found no undone intent. Findings, most severe
first, with proposed dispositions for the gate:

- F1 [O]: `evaluating...orig:538` says the rule "holds false-certification
  near its intended rate". D-007 rejects a nominal level for the rule. The
  line is unchanged, but it sits in a paragraph this branch corrected.
  Proposed: fix now.
- F2 [O]: the NEWS table entry reads as a line-wrap fix. The session rendered
  both markups with `html_vignette` at 1280 px. The old 7-column table was
  700 px wide and already one line per cell, so only the pkgdown article
  wrapped. Proposed: fix now.
- F3 [O]: in the new `cert` bullet, "that table" has no clear antecedent.
  Proposed: fix now.
- F4 [O]: "That note comes before any coverage question is asked" can read as
  page position, but the note prints last. Proposed: fix now.
- F5 [O]: the NEWS axes-reliability change is filed under "shorter printed
  reports", but `--` predates those changes. An older bullet says the
  vignettes' code did not change, and this branch adds `drop_xy = TRUE`.
  Proposed: fix now.
- F6 [O] and AC2 reader doubt 8: the edited line says `Gap`, `Variance` and
  `Rotation` measure even angular spread. The printed output and the
  vignette's own section call the last two interstitiality. Proposed: fix now.
- F7 [O]: `cairn/references/plain-vignettes.md:148,166,184,187` still point at
  the removed doc-bug candidate row. Proposed: fix now.
- F8 [O]: the shown `ssm_table()` call renders an htmlTable with another
  caption than the hidden kable chunk. This predates the branch. Proposed:
  reject, pre-existing.
- F9 [O]: the non-breaking-space substitution also runs over the `Profile`
  column. No cell is corrupted. Proposed: reject, no current effect.
- F10 [O]: over-long lines, unwrapped paragraphs and repeated sentence
  openers. Proposed: reject, style nit.
- F11 [O]: the FIML section still says the comparison "is `NA`" without the
  printed `--`. That line is unmodified. Proposed: reject.
- F12 [O] and [S]: `elapsed 8.3s` is a machine timing inside a masked region.
  Proposed: reject, no information.
- F13 [S]: "the `reference` scale's" puts a backtick on a noun phrase.
  Proposed: reject, style nit.
- AC2 reader doubts 1 to 5, 7, 9 and 10: each sits on a line this branch did
  not modify, or in a vignette it did not touch. Proposed: reject,
  unmodified lines. Doubt 6 sits on an unmodified line too. Proposed: reject.

No finding shows an acceptance criterion failing, so no return is due.

Gate triage: the maintainer took F1 to F7 as fix now. Every finding proposed
for rejection is rejected for the reason given beside it.

Fix-now re-check at f8a23a76: `prose-sweep` exits 0 on the edited source, and
the re-knit changes no `#>` line. The lint counts of NEWS.md and
plain-vignettes.md are unchanged. A second re-knit, then the staleness check,
passes on all 7 vignettes. `devtools::check(args = "--no-manual")` gives 0
errors, 0 warnings, 0 notes. The second re-knit changed only `elapsed 8.3s`
to `8.2s`, and that change was discarded. The AC4 search still returns only
the 3 missing-value lines.

