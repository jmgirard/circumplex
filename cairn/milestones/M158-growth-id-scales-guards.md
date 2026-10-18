<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M158: Merged-id and repeated-scale guards for ssm_growth_data()

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
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

- [ ] AC1: `ssm_growth_data()` stops with an error when two distinct values of the `id` column (distinct by `unique()`) become one level of the output's `<id>` factor. This check runs before the repeated-row check, and its message gives the row numbers of one such pair and the label they share. Tests in `tests/testthat/test-ssm_growth_data.R` assert that message for the double ids `0.1 + 0.2` and `0.3`, once at two different times and once at the same time (the same-time case replaces the existing "more than one row" assertion for that pair), and for the complex ids `0.3+0i` and `(0.1 + 0.2)+0i`.
- [ ] AC2: The id check accepts ids that keep one level per distinct value. Tests assert that `ssm_growth_data()` returns its table without error for integer, character and factor ids built from `simulated_growth`; for the doubles `0.3` and `0.300000000000001` at two different times, which give two levels; and for the doubles `0` and `-0` at two different times, which give one level (`unique()` counts them as one value).
- [ ] AC3: `ssm_growth_data()` stops with an error when `scales` selects one column more than once: by a repeated name, or by numbers that reach the same column, a non-integer number included (`1.5` reaches column 1, since R's indexing truncates). The message names the column. Tests assert that message for a repeated name, for a repeated number, and for `1` with `1.5`. A repeated `NA` name and repeated numbers that reach no column (`NA`, or beyond `ncol(data)`) are not judged by this check and still reach the scorer's "undefined columns" error, which tests assert for `c(NA_character_, NA_character_, ...)` and `c(99, 99, ...)`.
- [ ] AC4: The `ssm_growth_data()` help page (`@param data` and `@param scales` in `R/ssm_growth_data.R`, rendered to `man/ssm_growth_data.Rd`) and the development-version entry of `NEWS.md` state both new refusals. A grep for `significant digits` and `one person` over `R/` (comments included), `man/`, `NEWS.md`, `vignettes/` and `tests/testthat/test-ssm_growth_data.R` finds no text saying that distinct ids count as one person.
- [ ] AC5: `Rscript -e 'devtools::test()'` passes with 0 failures, and `Rscript -e 'devtools::check(args = "--no-manual")'` reports 0 errors, 0 warnings, and no note that names a file this milestone changes.

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
- [ ] T4: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.

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
