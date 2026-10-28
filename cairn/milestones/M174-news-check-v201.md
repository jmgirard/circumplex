<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M174: Check NEWS against v2.0.1 before release

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3, GP5   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — NEWS.md ships to CRAN users   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m174-news-check-v201   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Before the CRAN release walk, NEWS.md names every interface change since v2.0.1 and states each user-visible change that a read of the code commits finds.

## Scope
<!-- owner: plan · create/amend-via-gate -->

These terms apply below. The development section is the lines of NEWS.md before the `# circumplex 2.0.1` line. A bullet is a line of that section that starts with `* `, plus its next lines up to the next bullet or heading. The plan commit is the commit that adds this file. The merge head is the last commit of the milestone branch.

**In:** a script, `tools/news-interface-check.R`, that lists the interface differences between a git ref and the current code and matches each one in NEWS.md. A read of the 44 code commits since v2.0.1. Edits to the development section for each gap that the script or the read finds, within 180 lines.

**Out:** the truth of sentences that this milestone does not change (M173 audited 85 claims in four review passes). The version bump, `cran-comments.md` and the release checks, which belong to `/cairn-release`. The NEWS sections of released versions.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: Each difference between v2.0.1 and the merge head in the five sets below is matched in the development section. The one exception is an export documented in `man/circumplex-ggproto.Rd` whose `geom_*()` constructor is matched. The sets are:
  1. the `export()` and `S3method()` lines of NAMESPACE,
  2. the argument names and default expressions of each function exported at both refs and each S3 method registered at both refs, read with `parse()` from top-level `name <- function(...)` assignments in `R/*.R`,
  3. the file names in `data/`, without `.rda`,
  4. the `VignetteIndexEntry` titles of `vignettes/*.Rmd`,
  5. the package names and version floors in the Depends, Imports and Suggests fields of DESCRIPTION.

  A backtick span names X if, after a leading `?` is dropped, its text is X, or begins with `X(`, or begins with X, optional spaces and `=`. These are the match rules:
  - Export, dataset, or added or removed package: one bullet holds a span that names it.
  - Argument difference: one bullet holds spans that name the function and the argument. For an S3 method, the generic stands for the function.
  - Added or removed S3 method: one bullet holds spans that name the generic and an export whose `man/*.Rd` file has the `\alias{}` of the method. The `man/` files come from the merge head for an added method and from v2.0.1 for a removed one. This rule checks that the two names appear together. It does not check that the bullet describes the method.
  - Version floor change: one bullet holds a span that names the package, and the same bullet holds the new floor as a whole token, with no digit or `.` on either side.
  - Vignette: the section holds the title in straight double quotes, after line breaks and runs of spaces become one space.

  The evidence is the output of `Rscript tools/news-interface-check.R v2.0.1`, run at the merge head with a clean working tree.
- [ ] AC2: Each sentence of the development section at the merge head that is not in it at the plan commit is true of the package at the merge head. A sentence about earlier behavior is true of v2.0.1 instead. Sentences are compared after line breaks and runs of spaces become one space.
- [x] AC3: Each backtick span and each double-quoted title in the development section at the plan commit is also in it at the merge head (a script compares the two sets).
- [x] AC4: `sed -n '/^# circumplex 2.0.1/,$p' NEWS.md` gives the same bytes at the plan commit and at the merge head.
- [x] AC5: At the merge head, `awk '/^# circumplex 2.0.1/{exit} {n++} END{print n}' NEWS.md` prints 180 or less.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T4, T5, T6
- AC2 → T3, T4, T5, T6
- AC3 → T4, T5
- AC4 → T5
- AC5 → T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits). -->

- [x] T1: Write `tools/news-interface-check.R`. The `^tools$` line of `.Rbuildignore` already keeps it out of the build. The script takes a base git ref and reads that ref's files with `git show` and `git ls-tree`. It applies the AC1 sets and match rules to the current code and prints each difference as matched, exempt or missing. If a difference is missing, it exits 1. It finds names only by the AC1 span rule, so `ssm_trajectory` is not found inside `ssm_plot_trajectory`.
- [x] T2: Show that the script reads a domain that is not empty and that it can fail. Against v2.0.1, it lists at least one difference in each set. Then run it on scratch copies of NEWS.md. Each plant below must be reported missing, with exit 1:
  - a name that is present only without backticks,
  - a name that is present only inside a longer name,
  - a floor change whose package name is present without the new floor,
  - an S3 method whose generic is `print`, with the generic removed from its bullet,
  - a vignette title in curly quotes,
  - a function span and its argument span in different bullets,
  - a package span and its floor in different bullets,
  - a name present only in the 2.0.1 section, or only in a heading,
  - a floor present only inside a longer number (`3.1.70`),
  - an added package present only without backticks,
  - a bare argument beside a backticked function.

  Each form below must still match: `f()`, `?f`, `arg = value`, a title split across two lines, and an S3 method whose generic is also the export.
- [x] T3: List the commits with `git log --first-parent --oneline v2.0.1..<plan commit> -- R src data inst vignettes DESCRIPTION NAMESPACE`. Leave out the version bump `76f253fb` and the `plan` and `review` tracking commits (44 commits on 2026-10-28). Read the diff of each commit against the development section. An owed change is a change that users can see, that the section does not state, and that no page named in its bullet states. For each owed change, add or edit a bullet. Write each new sentence from a read of the code in the same session. Write one work-log line with the counts.
- [x] T4: For each difference that the script reports missing, and each owed change from T3, edit NEWS.md. To stay within 180 lines, shorten other bullets, and restate each condition of a shortened bullet from the code. When you rewrap, keep each backtick span and each quoted title whole. Keep every span and title of the plan-commit section.
- [x] T5: Run the checks at the merge head with a clean tree. Run the script, and check its set 1 and set 2 lists against a separate enumeration. Compare the spans and titles with the plan commit, compare the released sections, and count the lines. For each new sentence, record in the Review section a file and line that make it true.
- [x] T6: Review return 1. Reword the `rbind()`/plot sentence so it does not imply a stack plots, and move the reserved `time` name refusal to Breaking changes. In the script, skip a top-level call whose head is not a name, compare dependencies per field, and strip quotes from NAMESPACE names. Rerun T2's probes and the T5 checks.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-28: created by /milestone-plan.
- 2026-10-28: tier check: milestone — 44 code commits since v2.0.1 to read and a new script, more than one sitting.
- 2026-10-28: idea vet: one concern, that no procedure lists every change a user would notice and M173's hand ledger failed review three times; the user chose a scripted interface check plus a commit read, with no criterion that promises the read is complete.
- 2026-10-28: lessons used: 2026-10-28 (M173) NEWS compression lesson, which shaped T4's restate-from-code rule, the span-whole rewrap rule, and binding AC1 to a script; 2026-07-19 (M7 family), which shaped T2's non-empty-domain check.
- 2026-10-28: question set: check scope — script plus commit read (recommended option).
- 2026-10-28: question set: NEWS length — hold the development section at 170 lines (user chose this over the recommended 190).
- 2026-10-28: question set: the script is committed to `tools/` for reuse at later releases.
- 2026-10-28: criteria audit (full mode, fresh Opus reader): 12 findings, all fixed. AC1 now binds NEWS, not the script's exit, with one closed exemption (a ggproto object beside its matched constructor). AC1 gained span forms (`f()`, `?f`, `arg =`), whole-token matching, the set-2 domain (functions at both refs, generic for a method), a floor that must appear with the package, whitespace-collapsed straight-quote titles, and a co-mention scope for S3 methods. AC2 now covers sentences new since the plan commit, allows v2.0.1 truth for before-statements, and needs a cited file and line. AC3 and AC4 name their commands. T2 gained form and positive probes, and T5 runs on a clean tree.
- 2026-10-28: collision check: absorbs candidate "Check NEWS against v2.0.1 before release" in full (split from M173); M173 (done) shortened NEWS and dropped its ledger criterion; no D-entry covers NEWS checks. Release tripwire not fired, since no version ships here.
- 2026-10-28: inbox sweep: 0 open issues, 0 open PRs.
- 2026-10-28: plan gate chose a scripted interface check plus an unbound commit read over a per-commit ledger criterion, because the ledger failed review three times in M173; falsified by a review finding an owed change that the commit read missed.
- 2026-10-28: plan gate held 170 lines over 190 at the user's choice; falsified by a needed bullet that cannot fit without dropping a condition.
- 2026-10-28: plan checkpoint (half-done): committed while a fresh Opus re-audit of the repaired AC1-AC3 and of AC4 at 170 runs; its findings land in a follow-up plan commit before implement starts.
- 2026-10-28: re-audit (full mode, fresh Opus reader): 9 findings with repairs, 5 clean. Fixed: set 2 limited to exports and registered methods (it reached 9 internal helpers), span rule reworded to "is X, or begins with `X(` or X and `=`" with T1 using the same rule, exemption tied to `man/circumplex-ggproto.Rd`, floor as a whole token, AC2's citation clause moved to T5, new AC3 keeping every plan-commit span and title (guards M173's lost-condition failure under the 170-line hold; old AC3-AC4 now AC4-AC5), T2 gained 7 location and form plants, T5 checks the script's lists against a separate enumeration. It found 6 AC1 gaps today: 3 S3 methods (`print`/`rbind` on the trajectory, `print` on the growth formula), bare `nlme`, and the testthat floor.
- 2026-10-28: release-window advisory overridden: `cairn_validate` flags M174 by its title, but it ships no version and the user asked for it by name; the release itself stays with `/cairn-release`.
- 2026-10-28: implement started on branch m174-news-check-v201.
- 2026-10-28: T1 done: `tools/news-interface-check.R` (base R; strips CR from `git show` output because some R/ files are CRLF, and marks text UTF-8 because Rscript runs here under `en_US`). Against v2.0.1 it lists 26 differences, 20 matched, 1 exempt (`GeomSsmEllipse`), 5 missing: `print`/`rbind` on the trajectory, `print` on the growth formula, `nlme`, the testthat floor; this equals the re-audit's independent count. It takes an optional news-file argument for T2's scratch copies, and finds the section as the lines before the second `# ` heading.
- 2026-10-28: T2 done (scratchpad `t2-plants.R`, each probe asserting the named item's status and the exit code): against v2.0.1 the script lists differences in all five sets (6, 4, 3, 5 and 2 items); a scratch NEWS with one added bullet covering the 5 missing items exits 0. Positive forms stay matched: `f()`, `?f`, `arg = value`, `arg=value`, a bare-name span, a title split over two lines, a method whose generic is the export. Twelve plants each turn their item MISSING with exit 1, one per form T2 lists (two `print` methods counted under one plant). Two plants first left the text unchanged (a title split across lines; a `?cpm_fit` pointer still in the bullet) and were rewritten until the scratch file showed the change.
- 2026-10-28: delegation: T3's read of the 44 commits split across four fresh Opus readers (11 commits each, read-only), each told to judge the final state at HEAD against v2.0.1; every reported item re-read in the code by the session before any NEWS edit.
- 2026-10-28: T3 done: 44 commits read, 39 owe nothing; 6 owed changes found, all confirmed in code. Clear: `items()` wraps its prefix and suffix lines (`R/instrument_oop.R:118-119`; M173's cut lost the phrase). Borderline, taken: `ssm_plot_trajectory()` on a data frame refuses a `time` column named `xend`, `yend` or `Interpretable` (`R/ssm_trajectory.R:185,223`); `print()` of a multi-group `ssm_sem()` fit says an untested contrast rung, and prints rejected higher rungs on an `Also:` line when the groups are not comparable (`R/ssm_sem.R:956-990`); `GeomSsmEllipse` named in NEWS, since the 2.0.0 section names its siblings. Borderline, declined: the lower-discrepancy paragraph in "CPM Fits at a Boundary", since the feature's bullet and `?cpm_fit` cover it. One false sentence found: "The stored verdict text does not change" (`R/ssm_sem.R:977-986` against v2.0.1's `:1018`, "additionally" became "also"), corrected to "changes only in wording".
- 2026-10-28: T4 checkpoint (half-done): NEWS edits for the 5 interface gaps and the T3 items are in. Interface check 26 of 26 matched, every plan-commit span and title present (117, 0 missing), released sections identical, but the section is 175 lines against AC5's 170. Rewording four bullets and a reflow at width 80 saved 1 line; going lower would cut conditions from other bullets. Stop for the user on AC5's figure. The lint hook's hits in NEWS.md are all in released sections, which AC4 freezes.
- 2026-10-28: substantive amendment: AC5's figure 170 → 180 lines, with Scope In ("within 180 lines") and T4 (minor) to match. The plan gate's falsifier for holding 170 fired (needed bullets could not fit without dropping conditions), and the user chose "Allow 180 lines" at the stop over dropping the two borderline additions and cutting wording. The section is at 175.
- 2026-10-28: re-audit: AC5 (full) — nothing on all six questions; jointly satisfiable with AC1-AC4 at 175 lines; noted the falsifier line above and that Scope is plan-owned (amended here under the gate).
- 2026-10-28: T4 done: section at 175 lines (AC5 180); edits are the growth, ellipse, items, SEM and plot bullets, with the dash bullet merged into the SEM bullet.
- 2026-10-28: T5 done on a clean tree at 8c5c2ffa plus this commit's tracking: interface check exit 0, 26 of 26 matched; separate enumeration (NAMESPACE diff, 12 export/S3method lines; a `parse()` formals diff of exports, 4 argument changes) equals the script's set 1 and set 2; 117 plan-commit spans and titles, 0 missing; released sections identical to the plan commit; 175 lines. T5's per-sentence citations go to the Decisions section, since Review is review-owned.
- 2026-10-28: claim audit: 27 claims read, 3 corrected — tools/news-interface-check.R header (set 2 also reads `=` assignments and skips a function missing at one ref or both; the S3 rule can be met by one name when the generic is the export; floor-removed and `Geom`-prefix behavior stated); the 10 NEWS claims hold; the same reader re-read the corrections once, and its one remaining wording fix (one ref or both) was applied as given.
- 2026-10-28: verify slot: no file under R/, src/ or tests/ changed on the branch (`git diff --stat master...HEAD`), so `devtools::test()` is not triggered; the interface check reruns green after the header edit. Implement done, status review.
- 2026-10-28: review return 1: AC2 fails on sentence S2 ("`rbind()` stacks its tables, and `ssm_plot_trajectory()` plots them" implies a stack plots, but repeated times are refused, `R/ssm_trajectory.R:305-315`); with it fix the review's other fix-now findings: move the `xend`/`yend`/`Interpretable` refusal to Breaking changes (diff-bug #5), and in `tools/news-interface-check.R` guard a top-level call whose head is a call (#2), compare dependencies per field (#3), and strip quotes from NAMESPACE names (#4). Gate otherwise clean (check 0/0/0).
- 2026-10-28: T6 added for review return 1 (minor amendment; Coverage AC1 and AC2 gain T6).
- 2026-10-28: T6 done: S2 now reads "`ssm_plot_trajectory()` plots the result, and `rbind()` stacks results."; the reserved `time` name refusal moved to the second Breaking changes bullet. Script: `is.name()` guard before reading a call's head; dependencies keyed by field and package, so a move reports a removal and an addition; quotes stripped from NAMESPACE names; and a new fix found while testing, `methods` built empty when a NAMESPACE has no `S3method()` line (`paste0()` of two empty vectors gave "."). A scratch git repo (scratchpad `t6/`) with `utils::globalVariables()`, `export("%>%")`, `S3method("[",foo)` and ggplot2 moved from Suggests to Imports lists 4 differences, all missing on an empty NEWS (exit 1) and all matched with a bullet naming them (exit 0). T2's probes, repointed at the plan-commit NEWS with the new set-5 labels, pass 20 of 20. Checks: AC1 exit 0 (26 of 26), AC3 0 missing, AC4 identical, AC5 176 lines. Runs against v2.0.0, v1.0.0 and v0.3.10 finish with no error. Status review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

- 2026-10-28 (T5) **Citations for new NEWS sentences (AC2).** A scratchpad splitter lists 11 sentences of the development section not in it at the plan commit; two of them (the `--` dash sentence and the hollow-point sentence) keep their words and differ only by where a bullet or heading now joins. Each of the other nine, with the source that makes it true at HEAD:
  - S1 `ssm_growth_formula()` ... its `print()` shows the fit call: `R/ssm_growth_formula.R:125` (`print` cats `growth_formula_text()`, whose header and fit call start at `:129`).
  - S2 `rbind()` stacks its tables, and `ssm_plot_trajectory()` plots them: `R/ssm_trajectory_helper.R:493` (`rbind.circumplex_ssm_trajectory`), `:526` (the trajectory plot method).
  - S3 Suggests gains `nlme` and raises `testthat` to (>= 3.1.7): `DESCRIPTION:50`, `:56`; v2.0.1 had no `nlme` and `testthat (>= 3.0.0)`.
  - S4 `GeomSsmEllipse` is exported: `NAMESPACE:44`.
  - S5 `items()` also wraps its prefix and suffix lines: `R/instrument_oop.R:118-119` (`cat_item()`); v2.0.1 `:105-106` pasted them unwrapped.
  - S6 a comparable fit with no contrast asked for that stops below the contrast's rung says the rung was not tested: `R/ssm_sem.R:953-970`, printed through `labeled("Result", ...)` at `:828`.
  - S7 rungs rejected above the required one print on an `Also:` line, comparable or not: `R/ssm_sem.R:975-983` sets `facts$also` either way, printed at `:829`; v2.0.1 named them only in a comparable fit's verdict (`v2.0.1:R/ssm_sem.R:1016-1023`).
  - S8 the stored verdict changes only in wording: each verdict phrase at v2.0.1 is still in `R/ssm_sem.R` except "additionally rejected (reported only -- not required", which became "also rejected (reported only and not required" (`:977-986`), a `grep -cF` count of seven phrases at both refs.
  - S9 given a data frame, `ssm_plot_trajectory()` refuses a `time` column named `xend`, `yend` or `Interpretable`: `R/ssm_trajectory.R:185` (reserved names), `:223-233` (the refusal), reached from the data-frame method at `:555` through `:571`; v2.0.1's reserved list `:179-186` lacks the three.
- 2026-10-28 (T6) **Citations after review return 1.** S2 is now "`ssm_plot_trajectory()` plots the result, and `rbind()` stacks results.": the trajectory plot method `R/ssm_trajectory_helper.R:526` and `rbind.circumplex_ssm_trajectory` at `:493`; it no longer says a stack plots, which `R/ssm_trajectory.R:305-315` refuses for repeated times. S9 moved, unchanged in substance, to the Breaking changes bullet as "Given a data frame, `ssm_plot_trajectory()` refuses a `time` column named `xend`, `yend` or `Interpretable`.", with the same citations.

## Review
<!-- owner: review · exclusive -->

Pass 1, 2026-10-28, at d07eb836 (branch level with `origin/master` 077ab172, clean tree).

- AC1: `Rscript tools/news-interface-check.R v2.0.1` at d07eb836, clean tree: exit 0, "26 differences since v2.0.1: 26 matched, 0 exempt, 0 missing" (set 1: 6 exports, 6 S3 methods; set 2: 4 arguments; set 3: 3 datasets; set 4: 5 titles; set 5: `nlme` added, testthat floor). T5's separate enumeration (NAMESPACE diff 12 lines, a `parse()` formals diff with 4 changes) agrees; T2's 12 plants and 8 positive forms passed (work log).
- AC3: scratchpad `spans.R 077ab172`: 117 backtick spans and double-quoted titles in the plan-commit development section, 0 missing at d07eb836.
- AC4: `cmp` of `sed -n '/^# circumplex 2.0.1/,$p' NEWS.md` at 077ab172 and d07eb836: identical.
- AC5: `awk '/^# circumplex 2.0.1/{exit} {n++} END{print n}' NEWS.md` prints 175 (≤ 180).
- AC2: fails. Sentence S2, "`rbind()` stacks its tables, and `ssm_plot_trajectory()` plots them", reads as plotting a stack, but the plot refuses repeated time values (`R/ssm_trajectory.R:305-315`), so `rbind()` of two tables over the same times cannot be plotted. The other eight new sentences hold (diff-bug, blame-history, prior-review and ripple each re-read them; the claim audit read them before review).
- Gate so far: `cairn_validate` all checks passed; master `R-CMD-check.yaml` and `test-coverage.yaml` newest push runs success at e4ab911b (the later master commits are `cairn/`-only, path-ignored); `check-master-red-alert.R`, `master-red-alert-dryrun.R`, `check-branch-protection.R` exit 0. `devtools::check()` still running at this record; it then finished at d07eb836's tree: 0 errors, 0 warnings, 0 notes (11m 32s). `devtools::document()` (cli width 500): 0 `resolve link` lines, no diff; `pkgdown::check_pkgdown()`: no problems; README.Rmd and README.md untouched by the branch; NEWS entry present (this milestone's deliverable).
- spawned: diff-bug, blame-history, prior-review, ripple.
- diff-bug #1: the `rbind()`/plot sentence implies the stack plots, but repeated times are refused — fix now (AC2 failure, floor return).
- diff-bug #2: the script errors on a top-level call whose head is a call, such as `utils::globalVariables()`, because `as.character(e[[1L]])` has length 3 — fix now (a common R/ shape; none in R/ today).
- diff-bug #3: a package moved between Depends, Imports and Suggests reports no difference, since `read_deps()` merges the fields — fix now (a silent miss of a set-5 difference).
- diff-bug #4: quoted NAMESPACE names (`export("%>%")`, `S3method("[", foo)`) keep their quotes, so they can never match — fix now (none in NAMESPACE today; roxygen writes this form).
- diff-bug #5: the new `ssm_plot_trajectory()` refusal of a `time` column named `xend`, `yend` or `Interpretable` refuses input v2.0.1 accepted, but sits under Minor improvements — fix now (move it to Breaking changes, as for the other new refusals).
- diff-bug #6: `print.circumplex_ssm_trajectory` matches by co-mention, not description — reject (planned change: AC1's S3 rule states it checks co-mention only, and `?ssm_trajectory`, named in that bullet, documents printing).
- diff-bug #7: a removed floor is always reported missing — reject (planned change: AC1's floor rule needs the new floor in the bullet, which a removed floor cannot meet, and the header says so).
- diff-bug #8: the ggproto exemption accepts a constructor named in any bullet, and needs a `Geom` prefix — reject (false: "matched" for an export is the export match rule the script applies, and a `geom_*()` constructor only exists for a `Geom*` object).
- diff-bug #9: `(== x)` floors, a prose `VignetteIndexEntry` before the header, and a `# ` line in a fenced block in the section — reject (edge: DESCRIPTION pins with `==`, vignette headers after prose, and code fences in NEWS are outside this repo's use and common use; each gives a false MISSING, an exit 1, not a false pass).
- blame-history #1: the `rbind()` sentence drops the mixed-shape caution M173's earlier text carried — reject (false: `?ssm_trajectory`, named in the bullet, states it, `man/ssm_trajectory.Rd:81-82`); the plot implication is diff-bug #1.
- blame-history #2: "The stored verdict changes only in wording" drops M173-era "the warning and the error that quote it do not change" — reject (false as a defect: the sentence holds for the stored verdict, and the warning and error quote only non-comparable verdicts, which never carry the changed clause, `R/ssm_sem.R:986`).
- blame-history #3: M173 cut the `Test:`/`Result:` label list, the 80-column note and "for the same posterior draws" — reject (planned change: M173's chosen cut, outside this milestone's diff).
- prior-review: no regression of a prior review point; one record note on M173's "removed" wording, which names the graduated row — noted.
- ripple #1: `?ssm_plot_trajectory` names no reserved `time` column names, so a reader of the help page will not find the refusal NEWS states — follow-up (candidate row "Document reserved trajectory time names").
- ripple #2: the title of `tests/testthat/test-ssm_sem_groups.R:1194` says the stored verdict does not change — reject (false: that test compares two HEAD fits, which NEWS's v2.0.1 comparison does not contradict).
