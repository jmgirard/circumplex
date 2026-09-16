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

- [ ] AC1: No caution or note line printed by `print()` or `summary()`
      exceeds the set width, measured as display columns. This holds at
      `options(width = 60)` and at `options(width = 120)`. It holds for each
      fixture in `tests/testthat/test-print-width.R`. That file holds one
      fixture per caution in the T1 census, including the three cautions
      that the census records as assembled at run time.
- [ ] AC2: Every caution and note that master `26bd64ac` printed still
      prints. The words and their order stay the same after line breaks
      collapse to single spaces. A script compares output from a
      `git archive` of `26bd64ac` against the branch over the AC1 fixtures.
- [ ] AC3: The AC1 test goes red against each of four planted defect forms,
      applied one at a time. Form one is an unwrapped emitter. Form two is
      an emitter wrapped at a hardcoded width. Form three is an emitter
      wrapped at `width + 1`. Form four is an emitter whose indent is not
      counted against the width. Each red run names the emitter.
- [ ] AC4: No table row, column header, fit line or heading changes. Every
      existing snapshot passes with no update. Each of the six classes in
      the T1 census has at least one snapshot or printed-output test that
      runs.
- [ ] AC5: `Rscript -e 'devtools::test()'` is clean. Running
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports no error,
      warning or note that master `26bd64ac` does not also report. NEWS.md
      records the wrapping change.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T4
- AC3 → T3
- AC4 → T2, T5
- AC5 → T5, T6

## Tasks

- [x] T1: Census the prose caution and note emitters in the six files. For
      each one record the file:line, the current wrapping mechanism, and the
      fixture that fires it. Mark the ones assembled at run time, such as the
      marker-label loop at `cpm_oop.R:243-255`. Write the table into this
      file's `## Decisions` section.
- [x] T2: Add the shared wrapping helper to `R/utils.R`. It wraps prose to
      `getOption("width")`, counts display columns, and counts any indent or
      leader against the width. Leave tables, headers and fit lines alone.
- [ ] T3: Write `tests/testthat/test-print-width.R` first, one fixture per
      census row, at widths 60 and 120. Run the four planted defect forms of
      AC3 and record each red run.
- [ ] T4: Move each census emitter onto the helper. Then run the
      `git archive` comparison of AC2 and record its result.
- [ ] T5: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
      Compare the note list against master `26bd64ac`.
- [ ] T6: Add the NEWS.md entry. Sweep `?` help pages and vignette prose for
      any claim about the old fixed-width layout.

## Work log

- 2026-09-15: created by /milestone-plan.
- 2026-09-15: plan gate chose prose cautions only over also wrapping the instrument printers and the headers. No measured over-long line comes from either, and wrapping a table destroys its columns. Falsified by a user report of an over-long instrument or header line.
- 2026-09-15: plan gate chose a shared helper over per-site strwrap calls, because the T1 census found four wrapping mechanisms across six files. Falsified by a caution whose layout the shared helper cannot express.
- 2026-09-15: T1 census done: 41 emitters, 4 layout classes, not the 6 the plan assumed. Delegated to a Sonnet reader, spot-verified against the source. Table in Decisions.
- 2026-09-15: T2 done: `wrap_prose()` and `cat_prose()` added to `R/utils.R`, with direct tests in `tests/testthat/test-wrap-prose.R` (33 pass). Four planted defects each turn the tests red: an uncounted prefix, an off-by-one width, a character count in place of a column count, and a split atomic unit.
- 2026-09-15: criteria audit ran in full mode and returned ten findings. Five were fixed before writing. A grep clause passed only by changing `fit_est < 0.70` or a preserved caution. An "only wrapping mechanism" claim was already false at `ssm_sem.R:796`. The nchar type was unstated against multibyte cautions. The probe family varied only location. Five clauses bound an instrument. A `0 notes` clause reddens for unrelated reasons, and a width-40 case was unsatisfiable against a 15-column leader.

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
