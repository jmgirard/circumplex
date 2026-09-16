# M134: Simplify the vignette width guard

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — a CI checker over the rendered vignettes that no package user runs
- **Branch/PR:** m134-simplify-vignette-width-guard

## Goal

The vignette width guard holds its one exemption as a named list inside the
checker, in place of region markers in the vignette source.

## Scope

M132 shipped `tools/check-vignette-width.R`. It reads every `#>` line of the
seven pre-rendered vignettes, and CI fails on a line over 80 display columns.
The one exempt region wraps the 42 output lines of the `syntax` chunk in
`sem-based-ssm-analysis`. Only its `cx =~` and `cy =~` lines are over 80
columns (86 each). The M132 review findings P1, O3, O5, O6, O9 and O10 are
in that milestone's archive and the candidate row this plan absorbs.

**In:** the checker drops its region-marker parser for an exemption list of
two entries, each naming a vignette and a pattern anchored at line start. An
entry that matches no over-wide line is an error, as a stale region is today.
The checker expands tabs to 8-column stops before it measures. Its header
narrows its promise to `#>` lines and names the output it does not read. The
markers leave `sem-based-ssm-analysis.Rmd.orig` and its render, and the
workflow comment that names them is reworded. `tools/m132-planted-defects.R`
is deleted.

**Out:** reading output under another knitr `comment` prefix,
`results = "asis"` output or indented output. None exists today, and the
header states the gap (plan gate chose simplify over harden). A width guard
for the two live vignettes stays item (i) of the output-width candidate row.
A committed plant script is not kept. The plants run once and the work log
records them (plan gate).

## Acceptance criteria

- [x] AC1: `Rscript tools/check-vignette-width.R` exits 0 on the committed
      vignettes. Its report shows two exempted lines, both in
      `sem-based-ssm-analysis`: the lavaan `cx =~` and `cy =~` loading lines.
- [x] AC2: `grep -rn "vignette-width:exempt" vignettes tools .github` prints
      nothing, and `tools/m132-planted-defects.R` no longer exists.
- [x] AC3: The checker holds its exemptions as a list. Each entry names a
      vignette and a pattern anchored at the start of the line. An exemption
      covers only the lines its pattern matches in its vignette. An entry that
      matches no `#>` line wider than 80 columns after tab expansion stops the
      checker with an error that names the entry. The checker measures each line after it expands tabs to
      8-column stops.
- [x] AC4: The first sentence of the checker's header claims a width limit
      for `#>` output lines only. The header names three kinds of output the
      checker does not read: output under another knitr `comment` prefix,
      `results = "asis"` output, and indented output.
- [x] AC5: After `sem-based-ssm-analysis` is re-rendered with
      `tools/precompute-vignettes.R`,
      `git diff master -- vignettes/sem-based-ssm-analysis.Rmd` shows only the
      two marker lines removed.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T1, T3
- AC4 → T1
- AC5 → T2

## Tasks

- [x] T1: Rewrite `tools/check-vignette-width.R`. Replace `START`, `END` and
      the region logic in `scan_widths()` with an exemption list of
      (vignette, anchored pattern, reason). Error on an entry that matches no
      over-wide line. Expand tabs to 8-column stops before
      `nchar(type = "width")`. Rewrite the header per AC4 and remove its marker
      instructions. Keep the per-file report and the empty-domain error.
- [x] T2: Remove the two marker comments from
      `vignettes/sem-based-ssm-analysis.Rmd.orig` (lines 98 and 104) and
      re-render that vignette. Reword the marker sentence in the
      `.github/workflows/vignette-precompute.yaml` header comment (lines
      15-17). Delete `tools/m132-planted-defects.R`. Run the width checker, the
      AC2 grep and the AC5 diff.
- [x] T3: In temporary copies of `vignettes/`, plant each defect and record
      the checker's exit status and message in the work log. (a) An 81-column
      line directly after the `cy =~` line: red, line named. (b) The `cx =~`
      line shortened to 80 columns: red, entry named. (c) A tab line of 80 or
      fewer columns by `nchar(type = "width")` and over 80 with tabs expanded:
      red, line named. (d) A tab line of 80 or fewer columns with tabs
      expanded: green. (e) An 86-column `#> cx =~` line in another vignette:
      red, line named.

## Work log

- 2026-09-16: created by /milestone-plan. Absorbs the "Vignette width-guard hardening" candidate row.
- 2026-09-16: criteria audit (reduced mode, [O] fresh reader) returned four findings, all fixed before the gate. The plant list moved from AC3 to T3. AC4 drops restated behavior and narrows the header's first sentence. T2 edits the workflow comment the AC2 grep hits. The chunk size is 42 output lines, not 37.
- 2026-09-16: plan gate chose simplifying the checker over hardening it, because no unread output kind exists today; falsified by an over-wide line of an unread kind reaching the site.
- 2026-09-16: plan gate chose simplifying over deleting the guard, because the print-width tests do not read whole vignettes; falsified by the guard catching nothing those tests miss.
- 2026-09-16: plan gate chose a one-off plant run over a committed plant script, because CI never ran the M132 script; falsified by a checker regression that a rerun of the plants catches.
- 2026-09-16: AC5 and plant (e) were added after the gate. A second [O] re-audit of AC3 and AC5 returned two findings, both fixed. AC3 now limits a stale entry to `#>` lines measured after tab expansion. AC5 was a staleness check, which fails before the render is committed and proves nothing after. It is now a diff that shows only the two marker lines removed.
- 2026-09-16: T1 done. The checker holds a two-entry EXEMPT list and expands tabs before it measures. On the committed vignettes it exits 0 and exempts `sem-based-ssm-analysis` lines 94 and 95, 86 columns each, while the old markers are still in place. Test suite: 0 failed, 0 errors.
- 2026-09-16: T2 done. Markers removed and `sem-based-ssm-analysis` re-rendered with the current package installed. The diff against master removes only the two marker lines. The checker exits 0 and exempts lines 93 and 94. The AC2 grep prints nothing, and the plant script is deleted. No package code changed, so the test suite was not re-run.
- 2026-09-16: T3 plant (a), an 81-column line after `cy =~` in `sem-based-ssm-analysis`: exit 1, "line 95 (81 columns)", the two exempt lines still exempted.
- 2026-09-16: T3 plant (b), the `cx =~` line cut to 80 columns: exit 1, "the exemption `^#> cx =~ ` matches no output line wider than 80 columns".
- 2026-09-16: T3 plant (c), `#>`, a tab and 77 x in `introduction-to-ssm-analysis` (79 columns unexpanded): exit 1, "line 73 (85 columns)".
- 2026-09-16: T3 plant (d), `#>`, a tab and 72 x (80 columns expanded): exit 0, and the report reads 66 output lines, so the planted line was read.
- 2026-09-16: T3 plant (e), an 86-column `#> cx =~` line in `axes-reliability`: exit 1, "line 68 (86 columns)", 0 exempted in that file.
- 2026-09-16: claim audit: not owed — internal tier.

## Decisions

## Review

Branch synced: `origin/master` is an ancestor of HEAD, no merge needed (2026-09-16).

- AC1: `Rscript tools/check-vignette-width.R` exit 0. Report: `sem-based-ssm-analysis` 147 output lines, 2 exempted (lines 93, 94), the `#> cx =~` and `#> cy =~` loading lines at 86 columns. The other six vignettes show 0 exempted, all fit.
- AC2: `grep -rn "vignette-width:exempt" vignettes tools .github` printed nothing (exit 1). `ls tools/m132-planted-defects.R` reports no such file.
- AC3: source read. `EXEMPT` is a list of two entries (vignette, pattern, reason), and a pattern not starting with `^` stops the checker. `scan_widths()` receives only the entries for its vignette and matches them against over-wide `#>` lines, measured by `display_width()` after `expand_tabs()` to 8-column stops. The five plants re-ran on scratch copies. Plant (a), an 81-column line after `cy =~`: exit 1, "line 95 (81 columns)". Plant (b), `cx =~` cut to 80 columns: exit 1, "the exemption `^#> cx =~ ` matches no output line wider than 80 columns". Plant (c), a tab line of 80 unexpanded columns: exit 1, "line 73 (85 columns)". Plant (d), a tab line of 80 expanded columns: exit 0, 66 output lines read in that file. Plant (e), the `cx =~` line in `axes-reliability`: exit 1, "line 68 (86 columns)", 0 exempted there.
- AC4: header read. Its first sentence reads: no "#>" output line in a pre-computed vignette is wider than the website's code box. A later paragraph names output under another knitr `comment` prefix, output of a chunk with `results = "asis"`, and indented output as not read.
- AC5: `Rscript tools/precompute-vignettes.R sem-based-ssm-analysis` re-rendered the file (exit 0, installed circumplex 2.0.1.9000) and left the working tree clean. `git diff master -- vignettes/sem-based-ssm-analysis.Rmd` shows two deletions, the `vignette-width:exempt start` and `end` comment lines, and nothing else.

Consistency gate (2026-09-16): `cairn_validate.py` exit 0, all checks passed. `devtools::document()` left no diff and printed 0 `resolve link` lines. `pkgdown::check_pkgdown()` found no problems. README.Rmd is not newer than README.md. No NEWS entry is owed, because the diff changes no user-visible behavior, and it adds no top-level file. On master, the newest R-CMD-check push run with a verdict is `success` (7127001b). The newest test-coverage push run is `success` (58c56f3b). `check-master-red-alert.R`, `master-red-alert-dryrun.R` and `check-branch-protection.R` all exit 0.

Reviewers: [O] diff-bug, [S] blame-history, [S] prior-review. The prior-review probe found no PR review comments, so that lens read the archived M132 and M133 Review sections only. No finding shows an acceptance criterion failing.

- O1 (ranked first): an exempt line has no width cap, so a `cx =~` line of 200 columns still exits 0.
- O2: an entry covers every matching line in the whole vignette, not only the `syntax` chunk.
- O3: the anchoring check reads only the first character, so `^#> cx|foo` passes with an unanchored second branch.
- O4: the script runs only from the repo root. This was already true on master.
- O5: a stale-entry error stops the run before the other vignettes are scanned.
- O6: two entries that match one line list that line twice in the report. Pass or fail is unaffected.
- O7: an empty `VIGNETTES` list exits 0. This was already true on master.
- O8: ANSI color codes add to the measured width, so the error is a false failure, not a missed one.
- O9: the header says the files read are under `vignettes/`, but with a directory argument they are under that directory.
- O10: the render now has two blank lines in a row where the start marker was. Markdown output is the same.
- O11: the ticked boxes were uncommitted when the reviewer read HEAD. Checkpoint e17ed798 committed them.
- S1: deleting the plant script leaves no committed trip-wire for the guard. The plan chose this.
- S2: a regex metacharacter typo in a pattern changes which lines it matches. No such typo exists today.
- P1: the M132 O9 byte-versus-display-width plant left with the plant script. Fresh plant on a scratch copy: a line of `αβ²` repeats at 80 display columns and more than 80 bytes exits 0, and the same line at 81 columns exits 1, "line 68 (81 columns)".
- The [S] blame-history report also claims `vignette-width:exempt` still appears in the checker's error text. The AC2 grep refutes this.
