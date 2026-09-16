# M131: Printed cautions and notes wrap to the reader's width

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Resolves:** —
- **Surface tier:** user-facing — the cautions are text users read in the console and in the vignettes
- **Branch/PR:** `m131-printed-cautions-wrap`

## Goal

Every prose caution and note the package prints wraps to `getOption("width")`.

## Scope

**In:** the prose caution and note emitters in `R/cpm_oop.R`, `R/ssm_oop.R`,
`R/ssm_draws.R`, `R/fit_structure_oop.R`, `R/axes_reliability_oop.R` and
`R/ssm_ci_oop.R`. Four wrapping mechanisms are in use there today. Some
cautions are hand-wrapped at a fixed column. Some are wrapped greedily at a
hardcoded 70 (`cpm_oop.R:250`). Some are wrapped at a hardcoded 78
(`ssm_ci_oop.R:29`, `:39`). Some are not wrapped at all. One shared internal
helper replaces all four. Width counts display columns
(`nchar(type = "width")`), because the cautions carry `²`, `ζ` and `ℹ`.

**Out:** tables, column headers, fit lines and section headings. Wrapping
them destroys their columns, and none of them is over-long today (plan gate).
The instrument printers in `R/instrument_oop.R` go to a candidate row. They
emit item text of arbitrary length that comes from the instrument data.
`R/ssm_sem.R` is left alone. It already wraps its prose to
`getOption("width")` at `:796`, `:810` and `:1892`. Warnings and messages
need no change, because knitr and the console already wrap them (T1 census).
The vignette width setting, the re-render and the width guard go to M132.

## Acceptance criteria

- [x] AC1: No caution or note line printed by `print()` or `summary()`
      exceeds the set width, measured as display columns. This holds at
      `options(width = 60)` and at `options(width = 120)`. It holds for each
      fixture in `tests/testthat/test-print-width.R`. That file holds one
      fixture per caution in the T1 census, including the three cautions
      that the census records as assembled at run time.
- [x] AC2: Every caution and note that master `26bd64ac` printed still
      prints. The words and their order stay the same after line breaks
      collapse to single spaces. A script compares output from a
      `git archive` of `26bd64ac` against the branch over the AC1 fixtures.
- [x] AC3: The AC1 test goes red against each of four planted defect forms,
      applied one at a time. Form one is an unwrapped emitter. Form two is
      an emitter wrapped at a hardcoded width. Form three is an emitter
      wrapped at `width + 1`. Form four is an emitter whose indent is not
      counted against the width. Each red run names the emitter.
- [ ] AC4: The pre-image for every comparison below is master `26bd64ac`.
      In each of the four snapshot files the T1 census names, every line
      that no census row emits stays byte-identical and in order. Blank
      lines count as lines. Each line that does change is attributed to a
      census row number in the work log. Outside those four files, every
      existing `_snaps/*.md` snapshot passes with no update. The same
      byte-identity check covers the full `print()` and `summary()` output
      of the axes-reliability fixtures at `width = 80`, which no snapshot
      records. The check goes red against a planted defect that changes a
      table's column widths without changing its words. It goes red against
      a second defect that drops a blank line before a note.
- [x] AC5: `Rscript -e 'devtools::test()'` is clean. Running
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports no error,
      warning or note that master `26bd64ac` does not also report. NEWS.md
      records the wrapping change.

## Coverage

- AC1 → T1, T2, T3, T9
- AC2 → T4, T7
- AC3 → T3
- AC4 → T4, T5, T7, T8
- AC5 → T5, T6, T10

## Tasks

- [x] T1: Census the prose caution and note emitters in the six files. For
      each one record the file:line, the current wrapping mechanism, and the
      fixture that fires it. Mark the ones assembled at run time, such as the
      marker-label loop at `cpm_oop.R:243-255`. Write the table into this
      file's `## Decisions` section.
- [x] T2: Add the shared wrapping helper to `R/utils.R`. It wraps prose to
      `getOption("width")`, counts display columns, and counts any indent or
      leader against the width. Leave tables, headers and fit lines alone.
- [x] T3: Write `tests/testthat/test-print-width.R` first, one fixture per
      census row, at widths 60 and 120. Run the four planted defect forms of
      AC3 and record each red run.
- [x] T4: Move each census emitter onto the helper. Then run the
      `git archive` comparison of AC2 and record its result.
- [x] T5: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
      Compare the note list against master `26bd64ac`.
- [x] T6: Add the NEWS.md entry. Sweep `?` help pages and vignette prose for
      any claim about the old fixed-width layout.
- [x] T7: Make `wrap_prose()` treat each element of `x` as its own paragraph
      in non-atomic mode, as the `strwrap()` it replaced did. Restore the
      three settings sentences at `R/ssm_ci_oop.R:472` to their own lines,
      correct the NEWS.md claim about them and the stale comment at
      `R/ssm_ci_oop.R:487`, and add multi-element tests to
      `test-wrap-prose.R`. (Review findings O1, O2.)
- [x] T8: Strengthen `tools/m131-line-identity.R` so its marker exit cannot
      excuse a dropped blank line, or a changed line outside the caution's
      own text, inside an attributed group. Re-run both of AC4's instrument
      probes. (Review findings O3, O4.)
- [ ] T9: Fix the two ledger guards in `test-print-width.R`: check entries
      against every known caution rather than only registered fixtures'
      markers, and remove guard two's dependence on the 41 fixture blocks
      having run first. (Review findings O5, O6.)
- [ ] T10: Validate `wrap_prose()`'s `width` and `atomic` arguments as it
      validates the other three, and restore a direct pin on the fired-marker
      caveat's continuation indent in `test-cpm_summary_markers.R`. (Review
      findings O8, O9.)

## Work log

- 2026-09-15: created by /milestone-plan.
- 2026-09-15: plan gate chose prose cautions only over also wrapping the instrument printers and the headers. No measured over-long line comes from either, and wrapping a table destroys its columns. Falsified by a user report of an over-long instrument or header line.
- 2026-09-15: plan gate chose a shared helper over per-site strwrap calls, because the T1 census found four wrapping mechanisms across six files. Falsified by a caution whose layout the shared helper cannot express.
- 2026-09-15: T1 census done: 41 emitters, 4 layout classes, not the 6 the plan assumed. Delegated to a Sonnet reader, spot-verified against the source. Table in Decisions.
- 2026-09-15: T2 done: `wrap_prose()` and `cat_prose()` added to `R/utils.R`, with direct tests in `tests/testthat/test-wrap-prose.R` (33 pass). Four planted defects each turn the tests red: an uncounted prefix, an off-by-one width, a character count in place of a column count, and a split atomic unit.
- 2026-09-15: claim audit: 39 claims read, 7 corrected — NEWS.md, R/utils.R, tests/testthat/test-print-width.R, tests/testthat/test-cpm_summary_markers.R, tests/testthat/helper-caution-fixtures.R, tools/m131-caution-word-parity.R
- 2026-09-15: all tasks done, status to review. Final `devtools::check(args = "--no-manual")` on the finished tree: Status OK, 0 errors, 0 warnings, 0 notes, test suite OK.
- 2026-09-15: claim audit re-read, the one the step allows. Four of the seven corrections cleared. Three needed a further pass, all of them prose. The ledger guard checks a caution's marker text, not its full text, so a lone continuation line sits outside it. The list of raw-output assertions omitted two. And `26bd64ac` changed no R code itself. Its tree is unchanged since `dba3f96e`, which was verified as the last commit touching `R/` or `src/`. All three are corrected.
- 2026-09-15: claim audit finding, acted on. The header of `test-print-width.R` claimed the ledger cannot hide an unwrapped caution, on the grounds that the line comparison catches it. That is false. An unwrapped caution prints exactly as it did before, so the comparison reports no change. Two tests now guard the ledger directly: no entry carries a known caution's text, and every entry is a line some fixture really prints. Both go red when a caution is parked in the ledger, and the second also goes red on a dead entry.
- 2026-09-15: T5 done. `devtools::check(args = "--no-manual")` reports Status OK, 0 errors, 0 warnings, 0 notes, so nothing is reported that master `26bd64ac` does not also report. `devtools::document()` produces no diff.
- 2026-09-15: T6 done. NEWS.md records the wrapping change under Minor improvements and fixes. The sweep of help pages and vignette prose found no claim about the old fixed-width layout. Every `wrap` in the vignettes is about angular wrapping.
- 2026-09-15: T4 done. All 41 emitters wrap to `getOption("width")`. The two `ssm_ci_oop.R` helpers were converted here. The 25 Class 1 emitters were delegated to an Opus reader and reviewed. All 41 width blocks pass at widths 60 and 120. The full suite passes: 1109 tests, 0 failures, 1 skip.
- 2026-09-15: T4 defect found in the helper itself. `cat()` appends its separator after the last element too, so `cat_prose()` was adding a blank line after every caution. The delegated reader found it by counting blank lines against master. Fixed, and `test-wrap-prose.R` now has a test that fails if it returns.
- 2026-09-15: T4 word parity (AC2): 41 of 41 fixtures print the same words as master `26bd64ac`. The oracle discriminates: changing `Heywood-type` to `Heywood` in one caution reddened 10 rows and named the word.
- 2026-09-15: T4 line identity (AC4). Changed lines, all attributed: `ci_accuracy.md` 10 groups, 6 naming row27, row28 and row35, 4 pure re-wraps. `cpm_api.md` 4 groups, 3 naming row06 and row08, 1 re-wrap. `cpm_summary_markers.md` 4 groups, 3 naming row02 and row08, 1 re-wrap. `fit_structure_api.md` identical. The axes-reliability printers, rows 16 to 26, every group attributed. Nothing unattributed.
- 2026-09-15: T4 AC3 plants, one at a time, each at a different emitter. An unwrapped emitter reddened row11. A hardcoded width reddened row08. A `width + 1` in the shared paragraph helper reddened all 15 of its rows. An indent printed but not counted reddened row16. The tree is green again after each.
- 2026-09-15: T4 pre-existing tests updated. Three assertion sites pinned the old hardcoded line breaks or matched a phrase that now straddles a break. They now collapse whitespace first, which pins the words and their order and makes the negative assertions stronger. `test-cpm_summary_markers.R`, `test-axes-fiml.R` and `test-axes-reliability.R`.
- 2026-09-15: T3 written first: 41 fixtures in `helper-caution-fixtures.R`, one per census row, driven by `test-print-width.R` at widths 60 and 120. All 41 fire their caution. All 41 fail the width check before the migration. Fixture building was delegated to a Sonnet reader and reviewed here.
- 2026-09-15: T3 design note. The width check excuses lines in a committed ledger of out-of-scope headers, fit lines and table rows. A shape pattern was rejected, because it can also excuse an unwrapped caution. The ledger is empty until T4 fills it. AC4's byte-identity check is what stops a missed caution from hiding in it.
- 2026-09-15: amendment, AC4. The plan's AC4 promised that every existing snapshot passes with no update. That state is unreachable. The two `ssm_ci_oop.R` helpers also wrap output carrying no caution, so re-wrapping them moves line breaks in snapshots the old wording protected. The gate chose to amend rather than hold the wording, because holding it defeats the goal. AC4 now compares against master `26bd64ac` by byte identity outside the census rows. It also covers the axes-reliability printers, which no snapshot records. Coverage moves from AC4 → T2, T5 to AC4 → T4, T5.
- 2026-09-15: re-audit: AC4 (full) — five findings: a caution-based partition the scope makes unreachable, 29 graphics snapshots pulled into the promise, a "six classes" count no procedure produced, a table-and-heading claim no procedure enumerates, and a diff-reading act bound as a promise.
- 2026-09-15: re-audit: AC4 (full) — six findings: an undecidable "fit line" category, no named baseline for "changes", a fourth sentence AC1 already entails, a word-collapse check blind to column structure, no probe of AC4's own instrument, and eleven axes-reliability emitters that no snapshot records.
- 2026-09-15: criteria audit ran in full mode and returned ten findings. Five were fixed before writing. A grep clause passed only by changing `fit_est < 0.70` or a preserved caution. An "only wrapping mechanism" claim was already false at `ssm_sem.R:796`. The nchar type was unstated against multibyte cautions. The probe family varied only location. Five clauses bound an instrument. A `0 notes` clause reddens for unrelated reasons, and a width-40 case was unsatisfiable against a 15-column leader.
- 2026-09-15: review gate, AC4 FAILED, status back to in-progress. Two counts, both inside the domain AC4's own procedure names. (1) `tools/m131-line-identity.R` does not go red against a dropped blank line before a note: removing the `cat("\n")` at `R/axes_reliability_oop.R:213` drops the blank line in the printed output and the tool still exits 0. Its `attributable()` marker exit puts the dropped line in the note's own changed group and excuses it. (2) AC4's byte-identity promise is violated in `_snaps/ci_accuracy.md`: the three settings sentences emitted at `R/ssm_ci_oop.R:472` belong to no census row, so they had to stay byte-identical, and `wrap_prose()` merged them into one flowed paragraph because it collapses a multi-element `x`. The same instrument excused that as a re-wrap. NEWS.md:25 now carries a false claim about those three sentences. AC1, AC2, AC3 and AC5 pass with evidence recorded in the Review section. Defect-return count: 1.
- 2026-09-15: resumed after the AC4 return. Minor amendment: T7 to T10 added for the review findings the gate took, Coverage lines updated, T3 ticked (it was done at the time and left unticked). No criterion text changed.
- 2026-09-15: question gate chose a paragraph-aware `wrap_prose()` over a loop at the one multi-element call site, because the collapse is a silent divergence from the `strwrap()` it replaced and the next caller would hit it again. Falsified by a caution that needs several elements flowed into one paragraph.
- 2026-09-15: question gate chose to take findings O5, O6, O8 and O9 in this milestone, because all four sit in files already open here and O5 and O6 bear on whether the AC1 width test means what it claims. O7 (`ssm_sem.R` wraps one column differently, by a character count) and O11 (spliced failure reasons can break mid-phrase) go to candidate rows.
- 2026-09-15: T7 done (review findings O1, O2). `wrap_prose()` now treats each element of `x` as its own paragraph in non-atomic mode, as `strwrap()` did; atomic mode keeps element-as-unit semantics. Five tests written first in `test-wrap-prose.R`, four of them red before the change. The three settings sentences at `R/ssm_ci_oop.R:472` print on their own lines again and `_snaps/ci_accuracy.md` is byte-identical to master `26bd64ac` on those lines. AC2 word parity: still 41 of 41. AC4 line identity: `ci_accuracy.md` falls from 10 changed groups to 9, and from 4 re-wraps to 3, the merged-paragraph group being gone. NEWS.md needed no edit: its claim that the settings "print as three short sentences, usually on three lines" is true again once the behavior is restored. The stale comment at `:487` was corrected. `devtools::test()`: 0 failures, 10714 passing.
- 2026-09-15: T8 done (review findings O3, O4). The marker exit in `tools/m131-line-identity.R` no longer excuses a group on the marker alone: the group must also carry the same words in the same order and the same number of blank lines. AC4's two instrument probes now both go red. The dropped blank line before the boundary-solution note reports `UNATTRIBUTED ... carries a caution marker, but its words or blank lines moved` and exits 1; the re-aligned table columns report 11 unattributed groups and exit 1. On the real tree the tool still exits 0.
- 2026-09-15: T8, O3 answered by measurement rather than by strengthening the tool. A destroyed paragraph boundary carries the same words in a different line partition, which is exactly the definition of a legitimate re-wrap, so the printed text cannot tell the two apart; the paragraph structure lives in the emitter. Planting the O1 defect again and running the suite reddens `test-ci_accuracy.R` "print and summary snapshots (seeded)" through the committed snapshot, which pins the three settings sentences on three lines. The tool's header now records the limit and names the snapshot and `test-wrap-prose.R` as what covers it.

## Decisions

### T1 census: the prose caution and note emitters (2026-09-15)

41 emitters in the six scope files, in 4 layout classes, not the 6 the plan
assumed. None wraps to `getOption("width")`. Mechanism codes:

- **H** hand-placed `\n` at a fixed column
- **U** unwrapped, one long line
- **G70** greedy wrap at a hardcoded 70
- **S78** `strwrap()` at a hardcoded 78
- **L** a 15-column label field at indent 4, then `strwrap()` at `78 - 4 - 15`

`rt` marks text assembled at run time. `fix` marks an emitter that an existing
test already prints. The rest need a fixture built here.

Class 1, plain indented paragraph, 2-space leader, all **H** or **U**:

| # | Site | Emitter | rt | fix |
|---|---|---|---|---|
| 1 | `cpm_oop.R:53` | convergence acceptance | rt | — |
| 2 | `cpm_oop.R:60` | Heywood communality | — | fix |
| 3 | `cpm_oop.R:65` | variance-ratio pathology | — | — |
| 4 | `cpm_oop.R:72` | removed harmonics | rt | — |
| 5 | `cpm_oop.R:79` | near-tied optima | — | — |
| 6 | `cpm_oop.R:88` | discarded bootstrap replicates | rt | — |
| 8 | `cpm_oop.R:282` | analytic CI, N unconditional | rt | fix |
| 9 | `cpm_oop.R:292` | analytic CI, marker conditional | rt | fix |
| 10 | `cpm_oop.R:307` | free-scaling, no interval | — | fix |
| 11 | `ssm_oop.R:189` | low model fit | — | fix |
| 12 | `ssm_oop.R:196` | amplitude CI not certified | — | fix |
| 13 | `ssm_draws.R:290` | draws not certified | — | fix |
| 14 | `fit_structure_oop.R:153` | uncalibrated nv | rt | fix |
| 15 | `fit_structure_oop.R:105` | heuristic-classification caveat | — | fix |
| 16 | `axes_reliability_oop.R:214` | boundary solution | — | — |
| 17 | `axes_reliability_oop.R:225` | equal axes reliability | — | — |
| 18 | `axes_reliability_oop.R:239` | N-B NA, cormat path | — | fix |
| 19 | `axes_reliability_oop.R:247` | N-B NA, FIML path | — | fix |
| 20 | `axes_reliability_oop.R:256` | N-B NA, single item | — | fix |
| 21 | `axes_reliability_oop.R:285` | SE correction failed | rt | fix |
| 22 | `axes_reliability_oop.R:70` | correlation-as-covariance metric | rt | fix |
| 23 | `axes_reliability_oop.R:91` | SE-corrected numbers | — | fix |
| 24 | `axes_reliability_oop.R:135` | FIML SE caveat | — | fix |
| 25 | `axes_reliability_oop.R:110` | scaled fit | — | fix |
| 26 | `axes_reliability_oop.R:314` | fit scaling failed | rt | fix |

Class 2, atomic item list then a paragraph: row 7, `cpm_oop.R:247`, the
bootstrap fired-marker note, **G70** over whole marker labels then an **H**
tail, rt, fix.

Class 3, `strwrap()` paragraph at 78, indent 0 or 2, via `ssm_ci_cat_para()`
(`ssm_ci_oop.R:39`), all **S78**: rows 29 `:297` occasions structure note, 30
`:305` occasions rank deficient (rt), 31 `:317` observed-structure
sensitivity, 32 `:325` CPM structure fit (rt, fix), 33 `:331` CPM not
converged (fix), 34 `:336` CPM poor fit (rt), 35 `:344` CPM adequate fit (rt,
fix), 36 `:352` CPM marginal fit (rt), 37 `:360` boundary markers (rt, fix),
38 `:369` PSD repair (rt), 39 `:494` near-zero amplitude regime (rt, fix), 40
`:523` structural coverage zero (fix), 41 `:542` paired-contrast caveat (rt).

Class 4, label field plus wrapped text, via `ssm_ci_cat_line()`
(`ssm_ci_oop.R:29`), **L**: rows 27 `:127` guardrail false certification (rt,
fix) and 28 `:144` plain-language verdict paragraph (rt, fix).

All caution prose is defined in the file that prints it. These methods read
only short label vocabulary and bare numeric thresholds from elsewhere. Those
are `cpm_marker_labels()` and `cpm_boundary_markers()` at `cpm_fit.R:1422` and
`:1434`, the two N thresholds at `cpm_fit.R:1395`, and the fit thresholds at
`ssm_ci_accuracy.R:1021`.

`ssm_ci_cat_line()` and `ssm_ci_cat_para()` also carry output that is not a
caution. That output is the per-statistic coverage lines and the `Guardrail`
line at `ssm_ci_oop.R:98`, `:108` and `:127`. Re-wrapping those two helpers
therefore moves line breaks in snapshot output that holds no caution. AC4 was
amended on 2026-09-15 for that reason, to bind its no-update promise to this
census rather than to the presence of a caution.

Four of the eight `tests/testthat/_snaps/*.md` files record output that this
change can re-wrap: `ci_accuracy.md`, `cpm_api.md`, `cpm_summary_markers.md`
and `fit_structure_api.md`. The other four hold no caution, no verdict
paragraph and no `Guardrail` line. Nine snapshot directories hold 29 `.svg`
graphics snapshots, which nothing here touches.

`²`, `ζ` and `ℹ` are each one display column wide. On the
package's own caution text a character count and a column count therefore
agree. The helper counts columns anyway, because that is correct in general.
The property is tested with a double-width character, which is the only input
that can tell the two counts apart.

## Review

_Fresh evidence, gathered 2026-09-15 on branch `m131-printed-cautions-wrap` at
`d1efbd5e`, synced with `origin/master` (10 ahead, 0 behind)._

**AC1 — width compliance.** `testthat::test_file("tests/testthat/test-print-width.R")`
on the branch: 43 blocks, 628 assertions, 0 failures, 0 errors. 41 of those
blocks are the census fixtures, one per census row 1 to 41, each asserting at
`width = 60` and `width = 120` that the caution's marker fired and that no
printed line exceeds the width in display columns. Two warnings surfaced
(row01 and row32 emit an ill-conditioned-Hessian warning by design); neither
is a failure. The remaining two blocks are the ledger guards.

**AC2 — word parity against master `26bd64ac`.** `Rscript
tools/m131-caution-word-parity.R`: 41 of 41 fixtures print the same words in
the same order as a `git archive` build of `26bd64ac`; 0 differ. The oracle
discriminates: changing `Heywood-type` to `Heywood` in one caution
(`R/cpm_oop.R:70`) turned 10 rows to `WORDS DIFFER` and named the word and its
position. The plant was reverted and the tree verified clean.

**AC3 — four planted defect forms, one at a time, fresh runs.** Each plant was
applied alone, `test-print-width.R` run, and the plant reverted.

| Form | Plant | Result |
|---|---|---|
| 1. unwrapped emitter | `R/ssm_oop.R` low-model-fit caution emitted with plain `cat()` at a hand-placed indent | 1 block red, named `row11_low_model_fit` |
| 2. hardcoded width | `width = 78` passed to the `R/cpm_oop.R` analytic-CI caution | 1 block red, named `row08_analytic_ci_n` |
| 3. `width + 1` | `room <- max(width - disp_width(lead), 1) + 1` in `wrap_prose()` | 34 blocks red, each named |
| 4. uncounted indent | `room <- max(width, 1)` in `wrap_prose()`, so the prefix is printed but not charged | 34 blocks red, each named |

All four forms go red and name the emitter, so AC3 holds as written.

**A sensitivity limit found while planting form 4, carried to the gate.** A
weaker, emitter-local version of the same defect did not go red. Setting
`prefix = ""` on the `row16` boundary-solution caution while printing the two
indent columns with a separate `cat("  ")` left the widest line at 57 columns
at `width = 60`, because that caution's greedy fill happened to leave three
columns of slack. The test's promise is only that no line exceeds the width,
so a defect that overflows by fewer columns than the slack on the widest line
passes. This is a property of the criterion, not a failure of it, and the
helper-level plant above catches the same defect class. Recorded as a finding
at the gate rather than silently.

**AC4 — line identity against master `26bd64ac`. FAILS on its own second
instrument probe.**

The comparison itself passes. `Rscript tools/m131-line-identity.R` exits 0:
`ci_accuracy.md` 10 changed groups (6 attributed by marker, 4 pure re-wraps),
`cpm_api.md` 4 groups (3 by marker, 1 re-wrap), `cpm_summary_markers.md` 4
groups (3 by marker, 1 re-wrap), `fit_structure_api.md` identical. The
axes-reliability surface at `width = 80`, which no snapshot records, reports
every changed group attributed across rows 16 to 26. Nothing is unattributed.

The first instrument probe passes. Changing `right = FALSE` to `right = TRUE`
in the per-axis reliability table at `R/axes_reliability_oop.R:211` re-aligns
the table's columns without changing a word. The tool reports `1 NOT
attributed` on eleven fixtures and exits 1.

**The second instrument probe fails.** AC4's last sentence requires the check
to go red against a defect that drops a blank line before a note. Removing the
`cat("\n")` that precedes the boundary-solution note at
`R/axes_reliability_oop.R:213` does drop the blank line. Verified in the
printed output: at `width = 80` the note's first line follows the last table
row with no blank line between them. `tools/m131-line-identity.R` nonetheless
exits 0 and reports `0 NOT attributed` on every fixture.

The cause is in the instrument, not the criterion. `changed_groups()` puts the
dropped blank line in the same changed group as the note's own text, because
they are adjacent. That group then takes the marker exit in `attributable()`,
which asks only whether the group's joined text carries a known caution
marker. It does, so the group is excused, blank line and all. The tool's own
comment above `is_rewrap()` anticipates this case for the re-wrap exit
("adding or dropping a blank line leaves no words on either side, which would
otherwise compare equal and be excused") but the marker exit has no equivalent
guard.

The criterion is satisfiable as written: the marker exit can require that a
group preserve its blank-line count, or blank-line changes can be held out of
the marker exit altogether. The promise is sound; the instrument does not meet
it. AC4 stays unticked.

**AC5 — suite and package check.** `Rscript -e 'devtools::check(args =
"--no-manual")'` on the branch at `d1efbd5e`, run on a clean tree: `Status:
OK`, duration 7m 2.4s, 0 errors, 0 warnings, 0 notes. The test suite ran
inside that check and passed. Master `26bd64ac` reports no error, warning or
note that this does not, because this reports none at all. NEWS.md records the
wrapping change. AC5 holds. (A separate NEWS.md defect is recorded as finding
O1 below; it is not an AC5 failure, because AC5 asks only that the change be
recorded.)

### Consistency gate

`python3 cairn_validate.py`: exit 0, all checks passed, no advisory fired.
`devtools::check(args = "--no-manual")`: Status OK, as above. The remaining
toolchain-slot checks were not reached, because AC4 had already failed.

### Independent review — three lenses

Full three-lens fan-out, as the diff touches executable surface at a
user-facing tier.

**[S] blame-history: no findings.** The hardcoded 70 and 78 carry no
documented rationale in history and are named as defects by the plan. D-056
and D-059 are not contradicted. Every deliberate comment citing a past
milestone (M62 F2, RR09, M61-D1, D-009, D-010) survives verbatim. The
relaxed assertions in the three pre-existing test files lose no guard.

**[S] prior-review record: no findings.** The GitHub probe returned `[]`, so
the thread walk was skipped. Archived `## Review` sections for M94, M127,
M129, M110, M78, M48, M40 and M10 were read against the diff. M94's two
corrections and M129's wording pins survive; M127's own wrap-to-width work is
completed rather than contradicted.

**[O] diff-bug: eleven findings.** Ranked as reported, with disposition. Four
were verified here; the rest are recorded as reported and go back unverified.

| # | Finding | Verified | Disposition |
|---|---|---|---|
| O1 | `summary.circumplex_ci_accuracy()` passes a 3-element vector to `ssm_ci_cat_para()`. `strwrap()` treated each element as its own paragraph; `wrap_prose()` collapses them, so three settings sentences now flow as one. NEWS.md:25 still claims "settings print as three short sentences, usually on three lines", which is now false, and the comment at `R/ssm_ci_oop.R:487` is stale. | confirmed | fix now |
| O2 | `wrap_prose()` collapses a multi-element `x` into one paragraph. Undocumented in its header, and `test-wrap-prose.R` has no multi-element non-atomic case. Root cause of O1. | confirmed | fix now |
| O3 | The AC4 oracle counts a destroyed paragraph boundary as a pure re-wrap, so it scored O1 as one of `ci_accuracy.md`'s four re-wraps. | confirmed | fix now |
| O4 | `attributable()` excuses a whole changed group when any caution marker appears anywhere in it, with no check on the words outside the caution. Same root cause as the AC4 probe failure above. | confirmed | fix now |
| O5 | The ledger's first guard checks entries only against registered fixtures' markers, so a caution with no census row can be parked in the ledger and both guards pass. | reported | fix now |
| O6 | Ledger guard two reads an environment the 41 fixture blocks fill, so it depends on within-file ordering and on earlier blocks' side effects. An isolation run here was inconclusive. | reported | fix now |
| O7 | `R/ssm_sem.R` still wraps prose with `strwrap()`, which breaks at `< width` and counts characters, while `wrap_prose()` admits exactly `width` and counts columns. The package prints prose two ways, one column apart. | reported | candidate row (scope leaves `ssm_sem.R` out) |
| O8 | `wrap_prose()` silently repairs a bad `width` to 80 while `stopifnot()`-validating its other arguments, and `is_flag()` admits `atomic = NA`. | reported | fix now |
| O9 | `test-cpm_summary_markers.R` lost its only direct pin on the caveat's continuation indent; it survives only in snapshots. | reported | fix now |
| O10 | AC4 and T3 unchecked while the log reads "all tasks done". | confirmed | bookkeeping, back with the return |
| O11 | Interpolated failure reasons at `axes_reliability_oop.R:307` and `:338` can now break mid-phrase, unlike the marker labels, which are atomic. | reported | maintainer's call at the next gate |

The reviewer also verified, and this review accepts: no snapshot lost content
(word-level diffs empty, blank-line counts unchanged at 17/40/58/19), marker
labels are atomic at both `cpm_oop.R` sites, no hand-placed `\n` prose remains
in the six scope files, and `tools/` is Rbuildignored.

### Gate outcome: returned to `in-progress`

AC4 fails on two independent counts, both inside the domain of the procedure
AC4 names, and both repairable without changing what AC4 promises.

1. The instrument does not go red against a dropped blank line before a note,
   which AC4's last sentence requires (evidence above).
2. AC4's byte-identity promise is violated in `ci_accuracy.md`. The three
   settings sentences at `R/ssm_ci_oop.R:472` are emitted by no census row, so
   AC4 requires them to stay byte-identical. They did not (finding O1). The
   instrument excused the change as a re-wrap (finding O3).

This is a defect return, not an amendment return: AC4's promise is sound and
satisfiable as written, and the repair is to the instrument and to the
emitter, not to the criterion. Defect-return count for this milestone: 1. The
AC4 amendment of 2026-09-15 stays on the separate amendment track and is not
counted here.

