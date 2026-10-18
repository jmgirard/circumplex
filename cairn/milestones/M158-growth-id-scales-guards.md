<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M158: Merged-id and repeated-scale guards for ssm_growth_data()

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP2   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — changes what an exported function refuses and what its help page says   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m158-growth-id-scales-guards   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

`ssm_growth_data()` refuses two inputs that today give a wrong long table with no error: distinct `id` values that become one person in the output, and `scales` that select one column more than once.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** two `stop(call. = FALSE)` guards in `R/ssm_growth_data.R`, placed before the unchanged scoring and stacking; their tests in `tests/testthat/test-ssm_growth_data.R`; the help page's `data` and `scales` text, the code comment at `R/ssm_growth_data.R:150-152`, and the development-version NEWS refusal list. Measured 2026-10-17 on `simulated_growth`: 152 distinct double ids gave 151 output levels with no error, and `c("PA", "PA", ...)` and repeated column numbers were scored with no error.

**Out:** a repeated-scale guard in `ssm_parameters_id()` and the other scorers that take `scales` (`ssm_analyze()` and the rest) stays on the "Growth id and scales edge inputs" candidate row. Every guard M157 shipped stays as it is.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets.
     Every item opens with its positional label — `ACn:` — the item's
     position counted top-to-bottom, the number Coverage cites; an
     insertion, removal, or reorder renumbers the labels and the Coverage
     lines together.
     Driving RR set → its Binding criteria appear VERBATIM here (binding-
     criteria check), each ingested as a numbered criterion carrying its tag
     — `- [ ] ACn (BCm): <verbatim>` — with its own Coverage line, since
     coverage-complete counts AC checkboxes positionally (M107); departures:
     a "Deviations from RR<NN>" table ends this section. -->

- [x] AC1: `ssm_growth_data()` stops with an error when two distinct values of the `id` column (distinct by `unique()`) become one level of the output's `<id>` factor. This check runs before the repeated-row check, and its message gives the row numbers of one such pair and the label they share. Tests in `tests/testthat/test-ssm_growth_data.R` assert that message for the double ids `0.1 + 0.2` and `0.3`, once at two different times and once at the same time (the same-time case replaces the existing "more than one row" assertion for that pair), and for the complex ids `0.3+0i` and `(0.1 + 0.2)+0i`.
- [x] AC2: The id check accepts ids that keep one level per distinct value. Tests assert that `ssm_growth_data()` returns its table without error for integer, character and factor ids built from `simulated_growth`; for the doubles `0.3` and `0.300000000000001` at two different times, which give two levels; and for the doubles `0` and `-0` at two different times, which give one level (`unique()` counts them as one value).
- [x] AC3: `ssm_growth_data()` stops with an error when `scales` selects one column more than once: by a repeated name, or by numbers that reach the same column, a non-integer number included (`1.5` reaches column 1, since R's indexing truncates). The message names the column. Tests assert that message for a repeated name, for a repeated number, and for `1` with `1.5`. A repeated `NA` name and repeated numbers that reach no column (`NA`, or beyond `ncol(data)`) are not judged by this check and still reach the scorer's "undefined columns" error, which tests assert for `c(NA_character_, NA_character_, ...)` and `c(99, 99, ...)`.
- [x] AC4: The `ssm_growth_data()` help page (`@param data` and `@param scales` in `R/ssm_growth_data.R`, rendered to `man/ssm_growth_data.Rd`) and the development-version entry of `NEWS.md` state both new refusals. A grep for `significant digits` and `one person` over `R/` (comments included), `man/`, `NEWS.md`, `vignettes/` and `tests/testthat/test-ssm_growth_data.R` finds no text saying that distinct ids count as one person.
- [x] AC5: `Rscript -e 'devtools::test()'` passes with 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that names a file this milestone changes.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T3
- AC5 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. Every item opens with its positional label —
     `Tn:` — the item's position counted top-to-bottom, the number Coverage
     cites; an insertion, removal, or reorder renumbers the labels and the
     Coverage lines together. -->

- [x] T1: Tests first in `tests/testthat/test-ssm_growth_data.R`: the AC1 refusals (replace the same-time assertion and its comment at lines 339-346), the AC2 accepted cases, the AC3 refusals and the two scorer-error cases. Each refusal asserts its message, naming argument, rows or column. Run and see the new refusal tests red for the reason that the guard is missing.
- [x] T2: Add both guards in `R/ssm_growth_data.R`. The scales check judges a character `scales` by name (non-`NA` entries) and a numeric `scales` by `seq_along(data)[scales]` (non-`NA` positions), and sits beside the id/time selection checks. The id check compares `length(unique(id_col))` with `nlevels(factor(id_col))` after the `anyNA(id_col)` check and before the repeated-row check. Rewrite the comment at lines 150-152. Tests green.
- [x] T3: Rewrite `@param data` (drop the 15-digit merge text, state the refusal) and `@param scales`; run `devtools::document()` and revert any `Config/roxygen2/version` change in `DESCRIPTION`; add both refusals to the NEWS refusal list; run the AC4 grep.
- [x] T4: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates.
     EXEMPT from the 150-line cap (D-046): history under D-045, never edited,
     so the cap must never demand a trim here. Wrapped entries get a WARN.
     The rejected-alternative record (/milestone-plan step 4) takes this form:
     `- YYYY-MM-DD: plan gate chose <approach> over <alternative> because
     <reason>; falsified by <evidence class>.` — one per approach choice the
     gate actually weighed, none where it weighed none, and it is the record
     `/milestone-review`'s thrash trigger (b) reads. It lives here rather than
     below so an instantiated file inherits no placeholder to delete. -->

- 2026-10-17: created by /milestone-plan.
- 2026-10-17: tier check: not run — work named at the question set.
- 2026-10-17: question set: which backlog item to plan — "Growth id and scales guards" (offered as `ssm_growth_data()` only); plan then implement, or plan only — plan, then implement in this session.
- 2026-10-17: collision check: absorbs part of the candidate row "Growth id and scales edge inputs" (both items for `ssm_growth_data()`); the cross-scorer part of item (ii) stays on that row. Extends M157 (done); no conflict with D-064 or D-060. Function unreleased (2.0.1.9000), so no deprecation cycle.
- 2026-10-17: inbox sweep: 1 open issue (#180, a master-red alert whose R-CMD-check is green on master since b0b6410f, no overlap; left for /milestone triage), 0 PRs after the filter.
- 2026-10-17: lessons applied: M157 (factor() matches doubles at 15 digits; names judged after conversion), M85 (revert the roxygen version after document()).
- 2026-10-17: criteria audit (full mode, fresh Opus reader): 9 findings, all fixed: guard order and the replaced same-time assertion (AC1), row numbers named (AC1), complex ids added (AC1), a distinct-staying double pair added (AC2), numbers reaching no column left to the scorer (AC3), the code comment and test file added to the grep domain (AC4), the review-recording clause dropped (AC4, an instrument property), notes limited to files this milestone changes (AC5, machine-dependent notes).
- 2026-10-17: plan gate chose an error over a warning for merged ids because the table would hold the wrong persons (GP2 wrong object, as M157's repeated-row refusal); falsified by a user design that relies on ids merging at 15 digits.
- 2026-10-17: plan chose a guard in `ssm_growth_data()` only over a shared helper in every scorer because the selection named this function and the scorers include `ssm_analyze()`; falsified by a repeated scale giving a wrong result through another scorer.
- 2026-10-17: model tier advice: Opus (input-guard code and tests, no estimator change).
- 2026-10-17: implement started on branch m158-growth-id-scales-guards, cut from master at 1583e8c2.
- 2026-10-17: T1 done: three new test blocks plus the replaced same-time assertion; red as expected (5 failures: the two-times pair and three repeated-scale cases pass silently, the same-time pair hits the old "more than one row" message, which aborts the block before the complex case).
- 2026-10-17: T2 done: the scales check sits before the id/time selection checks, and the id check after the infinite-time check; the id message gives positional row numbers rather than row names, because a subset's row names are not ones the user set; growth tests 92 pass, 0 fail.
- 2026-10-17: T3 done: `@param data` and `@param scales` rewritten, `man/ssm_growth_data.Rd` regenerated, DESCRIPTION unchanged by document(); NEWS refusal list names both refusals; AC4 grep hit 12 lines, none saying distinct ids count as one person (the growth hits are the new comment and test text). The simple-english hook flagged 429 issues in NEWS.md, all but the one sentence split here in older entries, which are out of scope.
- 2026-10-17: claim audit: 27 claims read, 2 corrected — R/ssm_growth_data.R, man/ssm_growth_data.Rd (the "15 significant digits" text now says "for most numbers, though a large whole number can keep more", since `as.character(1234567890123456)` keeps 16 digits; the same reader re-read both fixes as correct). LESSONS.md's M157 line states the same 15-digit rule unqualified; correct it in place at review hygiene.
- 2026-10-17: T4 done: devtools::test() FAIL 0, WARN 11, SKIP 1, PASS 14507 (the growth file alone: WARN 0); devtools::check(--no-manual) 0 errors, 0 warnings, 0 notes, built after the claim-audit fix. Status set to review.
- 2026-10-17: review pass 1: AC1-AC4 evidenced and ticked, gate clean; 18 findings settled (8 fixed in caa3020c, 3 to a new candidate row, 7 rejected); AC5's fresh test and check run in progress (checkpoint, AC5 not yet verified).

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md.
     EXEMPT from the 150-line cap (D-074) because D-045 makes it history like the work log — dated dispositions, never edited — so the cap must never demand a trim here either.
     Entries carry their rationale; the counterweight `decisions format`
     advisory watches for pasted output, not for entry length (D-075). -->

## Review
<!-- owner: review · exclusive; evidence per criterion, consistency-gate
     results, review findings + triage. EXEMPT from the 150-line cap (M55),
     as are the work log (D-046) and the decisions section (D-074); evidence
     never scrambles plan-owned content. -->

Review pass 1, 2026-10-17, on the branch at caa3020c (master not moved since the cut at 1583e8c2).

- AC1 evidence: `devtools::test(filter = "ssm_growth_data")` FAIL 0, PASS 96. The block "refuses distinct ids that become one output level" asserts, with `fixed = TRUE`, the message "`id` values in rows 2 and 3 of `data` (by position, not row name) differ but share the label "0.3"" for `0.1 + 0.2`/`0.3` at two times and at one time, and the label "0.3+0i" for the complex pair; the fixture's row names are 10, 20, 30, so positions are what it reads. The guard sits after the infinite-time check and before the repeated-row check (`R/ssm_growth_data.R`). With the R file reverted to a345f702 this block errors, so it fails without the current guard.
- AC2 evidence: the same run; the block "accepts ids that keep one level per distinct value" passes for integer, double, character and factor ids over all of `simulated_growth` (152 levels each), for `0.3`/`0.300000000000001` (3 levels) and for `0`/`-0` (2 levels).
- AC3 evidence: the same run; the block "refuses scales that select one column twice" asserts "`scales` selects the column "PA" more than once" for a repeated name, `c(3, 3, 5:10)` and `c(3, 3.5, 5:10)`, and "undefined columns selected" for the repeated `NA` name, a repeated absent name `zzz`, and `c(99, 99, 5:10)`.
- AC4 evidence: `R/ssm_growth_data.R` `@param data` and `@param scales` and `man/ssm_growth_data.Rd` (regenerated, `document()` leaves no diff) state both refusals; `NEWS.md` lines 14-17 name both. `grep -rn 'significant digits\|one person'` over `R/`, `man/`, `NEWS.md`, `vignettes/` and `tests/testthat/test-ssm_growth_data.R` gave 10 hits: the new comment at `R/ssm_growth_data.R:169`, the test title at line 318 ("two rows for one person at one time"), and eight unrelated lines in `ssm_parameters_id`, `axes_*` and older NEWS entries; none says distinct ids count as one person. A second grep for `count as one`, `as one person`, `are one person` (wrapped text) found only the two `ssm_parameters_id` lines.
- AC5 evidence: at caa3020c, `devtools::test()` FAIL 0, WARN 11, SKIP 1, PASS 14511; `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes.

Consistency gate: `cairn_validate.py` all checks passed; no principle text changed, so `cairn_impact` skipped. `document()` with `cli.width = 500`: 0 `resolve link` lines, no diff. `pkgdown::check_pkgdown()`: no problems. README.Rmd untouched. NEWS has the entry (no milestone numbers). No new top-level files. Master watches: `R-CMD-check.yaml` newest verdict success at b0b6410f (the f62f24a5 run still in progress, no verdict); `test-coverage.yaml` success at f62f24a5. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R`, `tools/check-branch-protection.R` all exit 0.

spawned: diff-bug, blame-history, prior-review

- diff-bug #1: the id message's "rows 2 and 3" are positions, but a reader can take them for row names, and the test used rows 1 to 3 where the two agree — fix now (message says "of `data` (by position, not row name)", fixture rows 10, 20, 30), fixed caa3020c.
- diff-bug #2: the repeated-scale check ran before the id/time selection checks, so `c("person", "person", ...)` got the less specific message — fix now (check moved after them; test added), fixed caa3020c.
- diff-bug #3: a repeated name that is also a duplicated data name gets M157's "occurs more than once in `data`" message — reject (false as a defect: that message is accurate for the input, and M157's check is meant to run first).
- diff-bug #4: `scales` with 0 or a number truncating to 0 drops a column and the scorer fails with a matrix-dimension error — follow-up, new candidate row "Growth scales selection edge inputs".
- diff-bug #5: negative `scales` select by exclusion, so `length(scales) == length(angles)` no longer means one angle per column — follow-up, same new row.
- diff-bug #6: a list-column `id` fails inside `factor()` with no named refusal — follow-up, same new row.
- diff-bug #7: no double-id acceptance case covered all of `simulated_growth` — fix now (double kind added to the AC2 block), fixed caa3020c.
- diff-bug #8: the error path converts all n labels to text — reject (style: cosmetic, error path only).
- diff-bug #9: "a large whole number can keep more" could suggest large whole ids are safe — reject (false: the text is accurate, and the same sentence says ids sharing a label are an error).
- blame-history #1: a repeated name absent from `data` (`c("zzz", "zzz", ...)`) got "selects the column "zzz" more than once" instead of the scorer's "undefined columns" — fix now (only names in `data` are judged; test added), fixed caa3020c.
- blame-history #2: same as diff-bug #2 — fix now, fixed caa3020c.
- blame-history #3: the M157 "more than one row" assertion for the merged pair was replaced — reject (planned change: AC1 names the replacement).
- blame-history #4: M157's acceptance of 15-digit-merged ids is reversed — reject (planned change: the Goal).
- blame-history #5: the test comment said the doubles "differ within 15 significant digits" — fix now ("at the 15th significant digit"), fixed caa3020c.
- blame-history #6: the Rd was not confirmed as `document()` output — reject (false: the gate's `document()` run leaves no diff).
- prior-review #1: the id-message test pattern is unanchored, like M157's "`time` 1" finding — reject (false: the pattern is followed by fixed text, and it now matches with `fixed = TRUE`).
- prior-review #2: "undefined columns selected" is a generic pattern — reject (false: it names which failure, the scorer's own, which AC3 requires).
- prior-review #3: an Rd line is 81 characters — reject (style: roxygen output).
