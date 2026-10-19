<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M164: Vignette width guard gaps

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP4   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — it changes printed output, a message that users see, and the website's vignettes   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m164-vignette-width-guard-gaps   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Every line in a column-1 code fence of every vignette fits the website's code box, as a guard that reads all of them checks, and the CPM fit line wraps to the console width.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** Candidate item (i): the width guard reads the three live vignettes (`axes-reliability-caveats`, `bayesian-ssm-analysis`, `using-instruments`) by knitting them. Item (ix): the guard measures every line inside a code fence. It then reads output under another comment prefix, indented output, a fence that a `results = "asis"` chunk writes, and echoed source. The lines that this finds too wide are fixed. These are the `instruments()` list (82 columns on `using-instruments`), the `norm_standardize()` disclosure message (173 columns there), and two echoed source lines (81 and 83 columns in `sem-based-ssm-analysis` and `advanced-visualization`). Item (x): the CPM `Fit:` line wraps at its `;` separators.

**Out:** Table and data-frame output is never wrapped (the user's limit at the plan gate). If the guard finds such a line too wide, it goes to `EXEMPT` with its reason. Text that a `results = "asis"` chunk writes outside a code fence renders as prose or a table, not in a code box. Fences indented by 1 to 3 spaces and 4-space-indented code blocks: no vignette has them (grep on 2026-10-19). Both stay a candidate row. The other wide console lines in the width ledger of `tests/testthat/test-print-width.R` (test-statistic lines and headings) stay in that ledger.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: Run as `Rscript tools/check-vignette-width.R`, the guard reads every file that `list.files("vignettes", pattern = "\\.Rmd$")` returns. It reads `vignettes/<name>.Rmd` as it stands on disk for each name in `VIGNETTES` of `tools/precompute-vignettes.R`. It knits any other vignette with the installed package into a temporary copy of `vignettes/`. It prints one summary line per file, starting with the file name, so the number of summary lines equals the length of that call's result (14 on 2026-10-19).
- [x] AC2: In each file read, the guard measures the display width of every line inside a code fence. A fence opens at a line that starts at column 1 with three or more backticks or tildes. It closes at the next line that starts at column 1 with the same character, at least as many of them, and nothing else. A fence that never closes makes the guard exit non-zero. The guard measures echoed source and printed output alike, whatever comment prefix or indent the output carries. This includes a fence that a `results = "asis"` chunk writes. The guard exits non-zero for each measured line wider than 80 columns that no `EXEMPT` entry matches, and names its file, line number and width. It does not measure a line outside a fence.
- [ ] AC3: With the branch's package installed (`R CMD INSTALL .`) and the pre-computed vignettes re-rendered, `Rscript tools/check-vignette-width.R` exits 0 on the branch. `EXEMPT` then holds only the two loading-line entries of `sem-based-ssm-analysis`. The width step of the `vignette-precompute` workflow passes on the PR head.
- [x] AC4: `instruments()` prints to `getOption("width")`. At widths 60, 80 and 120, every printed line fits the width. The continuation lines of each entry are indented under the entry's text. No line opens with a space before an entry number. Every instrument that `instrument_names()` returns appears with its abbreviation, name and code, words in order. A `tests/testthat` test asserts each of these.
- [x] AC5: The disclosure message of `norm_standardize()` wraps to `getOption("width")`. The cases are every instrument, every normative sample that `norm_sample_usable()` accepts for it, and widths 60 and 120. In each case, every line of the message fits the width. The words of the message, in order, equal those of the one-line message. A `tests/testthat` test asserts both.
- [x] AC6: The CPM fit line that `print()` of a `cpm_fit()` result prints wraps to `getOption("width")`. It breaks only after a `;` and indents each continuation line by five spaces. The cases are the saturated-model line, the 89-column fit line that the width ledger records (`tests/testthat/test-print-width.R`), and one constructed fit whose one-line form exceeds 100 columns, at widths 60, 77 and 120. In each case, every line fits the width and every break follows a `;`. The lines, with the indent removed and joined by a space, equal the one-line text. A `tests/testthat` test asserts each of these.
- [x] AC7: The development section of NEWS.md has an entry that names the three printed-text changes: the CPM fit line, the `instruments()` list, and the `norm_standardize()` message. `devtools::test()` and `devtools::check(args = "--no-manual")` report 0 failures, 0 errors and 0 warnings.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T5
- AC2 → T5, T6
- AC3 → T3, T4, T5, T7
- AC4 → T1, T3
- AC5 → T1, T4
- AC6 → T1, T2
- AC7 → T8

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Write the regression tests for AC4 to AC6 first, and see each fail before its fix.
- [x] T2: Make `cpm_fit_line()` (R/cpm_oop.R:31) wrap through `wrap_prose(atomic = TRUE)`, with one unit per `;` clause and a five-space continuation. Remove the `fit line` entry and kind from the width ledger in `tests/testthat/test-print-width.R`. Update the CPM snapshots.
- [x] T3: Make `instruments()` (R/instrument_oop.R:240) print each entry with `cat_item()`. Stop the separator space of `cat()` from opening lines 2 and later. Update snapshots.
- [x] T4: Send the `norm_standardize()` message (R/tidying_functions.R:358) through `wrap_prose()`. Tests that match message text across a break match it with whitespace tolerance.
- [x] T5: Rewrite the guard to the `list.files()` domain, the knit of live vignettes into a temporary copy, and the fence rule of AC2. Rewrite its header comment, which says "two live vignettes" and reads only `#>` lines. Update the comment in `.github/workflows/vignette-precompute.yaml` that says the same.
- [x] T6: Write a plant script under `tools/` and run it in the vignette-precompute workflow. Each plant must fail the guard. The plants are: a wide line in a backtick fence and in a tilde fence, a 4-backtick fence that holds an inner 3-backtick line, a fence that never closes, wide output under `comment = "##"` and `comment = ""`, an indented `#>` line, a line wide only after tab expansion, a line of double-width characters, a wide line first and last in a block, a wide echoed source line, a fence that an asis chunk writes, and a wide line in a knitted live vignette. Each control must pass: a wide prose line outside a fence, and the unchanged tree.
- [x] T7: Reflow the two wide echoed source lines in the `.Rmd.orig` sources. Re-render every pre-computed vignette that the staleness check reports stale. Install the branch and run the guard to exit 0.
- [x] T8: Add the NEWS entry. Run the tests and the check.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-19: created by /milestone-plan.
- 2026-10-19: tier check: milestone — it changes the printed output and a message of exported functions and rewrites a CI guard, over more than one sitting.
- 2026-10-19: collision: candidate row "Vignette width guard gaps" absorbed in full (items i, ix, x); its started token appended. Item (ix)'s trigger (a line reaching the site) and item (x)'s trigger (the next milestone opening `R/cpm_oop.R`) had not fired; the user chose to include both.
- 2026-10-19: collision: M131, M132 and M146 are done; this extends them. M132's reason for no live-vignette guard (one needs brms to render) no longer holds: the `brm()` chunk is `eval = FALSE` against a committed `.rds`, and the live set is now three vignettes, not two.
- 2026-10-19: collision: rows "Ladder print test gaps and edge cases", "M120 instrument strength" and "Vignette prose follow-ons" checked; distinct scopes, not absorbed. D-056 and D-057 read: a layout change that keeps every word and number needs no deprecation cycle, so this works under GP4.
- 2026-10-19: checker-regress shape not hit: the guard checks the shipped vignettes, which are user-facing, not repo-internal artifacts.
- 2026-10-19: inbox sweep: 1 open issue (#180, master-red alert) and 0 open PRs; no overlap.
- 2026-10-19: lessons harvested: M21 family (a knit outside `load_all()` reads the installed package, so AC3 names the install), M120 (the staleness guard diffs against HEAD, so commit the re-render), test-craft M13 family (prove the guard by plants).
- 2026-10-19: criteria audit (full mode, fresh Opus reader): 10 findings. Fixed: AC4 dropped width 40 (the 57-column header cannot fit); AC6 named its cases (no procedure listed "each fixture"); AC3 names the branch install; AC2 and the Goal narrowed to column-1 fences, the closing rule and the unclosed-fence exit stated; T6 plants now vary fence form, comment prefix, width source and position; AC1 says "summary line" and "on disk"; T5 adds the workflow comment and Coverage maps AC3 to T5; AC6 states the five-space indent; AC5 names `norm_sample_usable()`. Answered, no change: AC1 and AC2 bind the guard, which Scope makes a deliverable; D-056 covers the message change.
- 2026-10-19: question set: the norms message — the user wants no data-frame-style output broken by wrapping; the message is prose, so it wraps, and the Out item keeps every table and data-frame line unwrapped.
- 2026-10-19: question set: scope — include items (ix) and (x) (user choice).
- 2026-10-19: plan gate chose knitting the live vignettes inside the guard over pre-computing them, because it adds no `.Rmd.orig` files or staleness surface for pages that fit no models; falsified by a live vignette whose site render differs from the guard's knit.
- 2026-10-19: plan gate chose measuring every fenced line over adding more `#>` prefix patterns, because one rule covers every output form and echoed code; falsified by a code-box line on the site that knitr writes outside a column-1 fence.
- 2026-10-19: plan gate chose wrapping the fit line at its `;` clauses over a fixed two-line layout, because the line stays unchanged where it fits; falsified by a reader report that the wrapped line reads worse.
- 2026-10-19: the fence rule's narrowing leaves indented fences unread; recorded as a candidate row ("Indented code fences in the width guard").
- 2026-10-19: T1: `tests/testthat/test-print-wrap-lines.R` written; before any fix it fails 4 of 6 blocks (fit line 4+2 failures, instruments() 3, norms message 46+1); the other two blocks are guards that pass by design. Width set by a base-R helper, since withr is not in Suggests.
- 2026-10-19: correction to the T1 line: before the fixes 5 of 6 blocks failed (fit line 4, break-at-60 guard 2, instruments() 3, norms message 46, long-message break 1); only the >100-column check passed.
- 2026-10-19: T2: `cpm_fit_line()` cuts its text at "; " into clauses and wraps them with `wrap_prose(atomic = TRUE)` and a five-space continuation; the `fit line` ledger entry and kind are gone from test-print-width.R; cpm_api and cpm_summary_markers snapshots moved one line break each. Full `devtools::test()` (with T3 and T4 in the tree) exits 0 with no failure.
- 2026-10-19: T3: `instruments()` prints its header, then each entry through `cat_item()` with lead "N. ", one `cat()` per entry, so no separator space opens a line; the instrument_oop snapshot lost the leading space on every entry and nothing else.
- 2026-10-19: T4: the `norm_standardize()` message goes through `wrap_prose()` and is joined with newlines; a `disclosure_messages()` helper in helper-norms.R collapses whitespace before matching, used at the seven `capture_messages()` sites in test-norms-disclosure.R and test-norms-kind.R that had failed on phrases split by a break (10 failures before).
- 2026-10-19: correction to the T4 line: the helper replaced eight `capture_messages()` sites (seven in test-norms-disclosure.R, one in test-norms-kind.R), not seven.
- 2026-10-19: T5: the guard reads every `list.files(dir, "\\.Rmd$")` file, knits the three live ones into a temp copy (`knitr::knit`, installed package), and measures every line inside a column-1 fence per AC2; an unclosed fence stops it. Run against the master build before T7, it read 14 files and failed on exactly the four known lines (83, 81, 82 and 173 columns). The workflow header comment and step comment now describe it, and a plants step follows it.
- 2026-10-19: T6: `tools/check-vignette-width-plants.R` appends one plant per run to a fresh copy of vignettes/ and requires a non-zero guard exit whose report names the planted file and the line's width and text (or "never closes"); controls require exit 0 and the "all 14 vignettes" line. 14 plants and 2 controls, about 28 s.
- 2026-10-19: T7: the two echoed calls are split over three lines in their `.Rmd.orig`; after `R CMD INSTALL .` and a full re-render, only those two code lines changed in the rendered `.Rmd` files (the re-rendered figure PNGs were restored, since their bytes are device-dependent and unchecked). The guard exits 0 over 14 files with only the two `sem-based-ssm-analysis` entries exempting; all 14 plants fail it and both controls pass.
- 2026-10-19: T6 discrimination: five one-line mutations of the committed guard, restored by copy and checked equal to HEAD. Measuring only `#>` lines made 6 plants WRONG (both comment prefixes, indented, tab, source, asis fence); closing a fence at any length, the 4-backtick plant only; no tab expansion, the tab plant only; `type = "chars"`, the double-width plant only; skipping live vignettes, the 4 live plants and both controls. Swapping in master's guard made all 16 WRONG, through its different report text.
- 2026-10-19: T8: NEWS.md has the new wrap entry under Minor improvements, and the M131 entry now says test-statistic lines rather than fit lines stay unwrapped. Full `devtools::test()` exits 0 with no failure section; `devtools::check(args = "--no-manual")` reports 0 errors, 0 warnings, 0 notes.
- 2026-10-19: claim audit: 65 claims read, 4 corrected — NEWS.md (the M131 sentence now names the unwrapped fit-index lines of `summary()`), R/cpm_oop.R and man/print.circumplex_cpm.Rd ("one-line fit summary"), tests/testthat/test-print-width.R (ledger header and kinds comment still said fit lines and "four"); the same reader re-read all 4 as holding, and `test-print-width.R` ran with no failure.
- 2026-10-19: review fix-now: the guard knits with `error = FALSE` and restores `options()` after each knit; a knit-error plant joins the plant script, whose file check now reads the planted file's own report block; plant copies are deleted and the guard runs under `R.home("bin")`'s Rscript; the plants CI step runs only after green steps; `instruments()` wraps its header (test width 40 added); the norms test asserts no blank line; allowlist comments in the workflow and `tools/check-ci-deps.R` name the guard's knit; `cairn/references/hu1999.md` line pointers replaced by function names. Mutations: dropping `error = FALSE` makes only the knit-error plant WRONG; a summary that always says "all fit" makes all 13 wide-line plants WRONG.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive -->

Pass 1, 2026-10-19. Evidence head `1a6c9128` (after the fix-now commit), branch build installed with `R CMD INSTALL .`.

- AC1 evidence: `ls vignettes/*.Rmd` gives 14 files; `Rscript tools/check-vignette-width.R` printed 14 summary lines, one per file, each opening with `<name>.Rmd`, three marked `(knitted)` (axes-reliability-caveats, bayesian-ssm-analysis, using-instruments), the other 11 read from disk.
- AC2 evidence: `Rscript tools/check-vignette-width-plants.R` exit 0: the guard exited non-zero and named the planted file's line (width and text), unclosed fence, or knit error on all 15 plants (backtick, tilde and 4-backtick fences, unclosed fence, `comment = "##"` and `""`, indented `#>`, tab-expanded, double-width, first and last in block, echoed source, asis-written fence, knitted live vignette, knit error), and exited 0 on both controls (wide prose outside a fence, unchanged tree). Mutations (work log): `#>`-only measuring, any-length close, no tab expansion, `type = "chars"`, skipped live knit, default knit errors and an always-"all fit" summary each turn their own plants WRONG.
- AC3 evidence (local half): with the branch installed and `Rscript tools/check-vignette-staleness.R` reporting all 11 pre-computed vignettes up to date, the guard exited 0 ("all 14 vignettes print within 80 columns in their code boxes"); `EXEMPT` holds 2 entries, both `sem-based-ssm-analysis` (`^#> cx =~ `, `^#> cy =~ `), exempting lines 116 and 117. The CI half (the width step on the PR head) is read at the merge step; the box stays unticked until then.
- AC4 evidence: `tests/testthat/test-print-wrap-lines.R` block "instruments() wraps each entry to the console width" passes (93 expectations, 0 failed) at widths 40, 60, 80 and 120: every line within the width, no line opening with a space before a number, 15 entry starts for the 15 `instrument_names()`, continuation lines under the entry text (at least one entry wraps), each entry's words equal to "N. Abbrev: Name (code)". It failed 3 times before T3 (work log).
- AC5 evidence: block "norm_standardize()'s message wraps to the console width" passes (254 expectations, 0 failed): for every instrument and every sample `norm_sample_usable()` accepts (case count asserted above 0), at widths 60 and 120, every message line fits, no line is blank, and the words equal the width-10000 one-line message; the fixed opening "Standardized against <Abbrev> normative sample <s>: N = " is asserted apart. Block "a long norms message does break at width 60" passes (IIP-SC sample 1 spans more than one line). 46 + 1 failures before T4.
- AC6 evidence: block "the CPM fit line wraps after a semicolon to the console width" passes (35 expectations) over the saturated line, the 89-column ledger line and a constructed 116-column line (`digits = 6`; block 1 asserts > 100) at widths 60, 77 and 120: every line fits, line 1 opens "Fit: ", every non-final line ends with ";", continuation lines open with exactly five spaces, and the trimmed lines joined by a space equal the literal one-line text written in the test. Block "the ledger fit line and the wide one do break at width 60" passes. `print()` path: the cpm_api and cpm_summary_markers snapshots show the wrapped line, and test-print-width.R passes with the `fit line` ledger entry gone.
- AC7 evidence: NEWS.md:189 (development section, Minor improvements) names the `Fit:` line of `print()` for a `cpm_fit()` result, `instruments()`, and the `norm_standardize()` message. On `1a6c9128`, `devtools::test()` exited 0 with no failure section, and `devtools::check()` reported 0 errors, 0 warnings, 0 notes (tests and vignette re-build OK).

Consistency gate: `cairn_validate.py` all checks passed; no principle text changed, so `cairn_impact` skipped; `options(cli.width = 500); devtools::document()` left no diff and printed 0 `resolve link` lines; README.Rmd calls none of the changed functions; `pkgdown::check_pkgdown()` no problems; NEWS entry present; no new top-level file; `devtools::check()` clean (above); master watches: newest push verdicts for R-CMD-check.yaml and test-coverage.yaml are `success` at `9c21436f` (later master pushes touch `cairn/` only, which both ignore); `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` exit 0.

spawned: diff-bug, blame-history, prior-review, ripple (on `2e964c31`).

- diff-bug #1: `knitr::knit()` defaults to `error = TRUE`, so a live vignette whose chunk errors passed the guard with short error lines — fix now, fixed 1a6c9128.
- diff-bug #2: the plant script's file-name check could not fail, since every report names every file — fix now (check reads the planted file's own block), fixed 1a6c9128.
- diff-bug #3: R options carried over between live knits in one process — fix now (`options()` restored after each knit), fixed 1a6c9128.
- diff-bug #4: a closing fence with trailing spaces does not close — reject, planned change: AC2 states "nothing else" on the closing line.
- diff-bug #5: chunks guarded by `eval = has_lavaan` skip silently where lavaan is missing — fix now (guard header says so; CI installs it), fixed 1a6c9128.
- diff-bug #6: the AC5 word check compares with the same function's width-10000 output — reject, false as a gap: the block also asserts the fixed opening phrase stated apart from the code, the independent fact test-craft asks for.
- diff-bug #7: `disclosure_messages()` collapses whitespace — reject, false: the reviewer found it keeps every NOT-match meaningful and no message text has irregular spacing.
- diff-bug #8: the `instruments()` header did not wrap below 57 columns — fix now (header through `cat_prose()`, test width 40 added), fixed 1a6c9128.
- diff-bug #9: plant copies stayed on disk, and `Rscript` came from PATH — fix now (copies unlinked, `R.home("bin")` Rscript), fixed 1a6c9128.
- diff-bug #10: the plants step ran after any earlier failure — fix now (step runs only after green steps), fixed 1a6c9128.
- diff-bug #11: a single fit clause wider than the console stays on its own over-wide line — reject, planned change: `wrap_prose()`'s documented rule, and AC6's cases all fit.
- blame-history #1: the live knits have no CI run yet — reject, false as a defect: AC3's CI half reads the width step on the PR head before merge.
- blame-history #2: `instruments()` loses its number and name alignment — reject, planned change: AC4 requires no space before an entry number, and NEWS says the entries no longer start with a space.
- blame-history #3: the fit line now wraps against M131's ledger reason — reject, planned change (candidate item x, D-056).
- blame-history #4: the norms helper tolerates a break inside a phrase — reject, false as a weakening: that tolerance is the change's purpose, and NOT-matches stay meaningful.
- prior-review #1: the χ² literals have no UTF-8 skip — reject, false: `test-print-wrap-lines.R` passes all 6 blocks under `LC_ALL=C`.
- prior-review #2: the norms word check cannot see an added blank line — fix now (no-blank-line assertion), fixed 1a6c9128.
- ripple #1: `cairn/references/hu1999.md` pointed at `R/cpm_oop.R:44`, which moved — fix now (function names, marked corrected M164), fixed 1a6c9128.
- ripple #2: the ROADMAP candidate row still lists items i, ix and x — reject, planned change: the started row graduates at post-merge hygiene.
- ripple #3: vignette setup comments say "Text printed with cat() does not wrap" — reject, false: `cat()` itself still does not wrap; `instruments()` wraps before it calls `cat()`.
- ripple #4: the allowlist comments in the workflow and `tools/check-ci-deps.R` did not name the guard's knit — fix now, fixed 1a6c9128.

Totals: 21 findings: 10 fixed now, 11 rejected, 0 follow-up. No amendment returns, no defect returns.
