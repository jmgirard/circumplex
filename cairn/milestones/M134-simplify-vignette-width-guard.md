# M134: Simplify the vignette width guard

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — a CI checker over the rendered vignettes that no package user runs
- **Branch/PR:** —

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

- [ ] AC1: `Rscript tools/check-vignette-width.R` exits 0 on the committed
      vignettes. Its report shows two exempted lines, both in
      `sem-based-ssm-analysis`: the lavaan `cx =~` and `cy =~` loading lines.
- [ ] AC2: `grep -rn "vignette-width:exempt" vignettes tools .github` prints
      nothing, and `tools/m132-planted-defects.R` no longer exists.
- [ ] AC3: The checker holds its exemptions as a list. Each entry names a
      vignette and a pattern anchored at the start of the line. An exemption
      covers only the lines its pattern matches in its vignette. An entry that
      matches no `#>` line wider than 80 columns after tab expansion stops the
      checker with an error that names the entry. The checker measures each line after it expands tabs to
      8-column stops.
- [ ] AC4: The first sentence of the checker's header claims a width limit
      for `#>` output lines only. The header names three kinds of output the
      checker does not read: output under another knitr `comment` prefix,
      `results = "asis"` output, and indented output.
- [ ] AC5: After `sem-based-ssm-analysis` is re-rendered with
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

- [ ] T1: Rewrite `tools/check-vignette-width.R`. Replace `START`, `END` and
      the region logic in `scan_widths()` with an exemption list of
      (vignette, anchored pattern, reason). Error on an entry that matches no
      over-wide line. Expand tabs to 8-column stops before
      `nchar(type = "width")`. Rewrite the header per AC4 and remove its marker
      instructions. Keep the per-file report and the empty-domain error.
- [ ] T2: Remove the two marker comments from
      `vignettes/sem-based-ssm-analysis.Rmd.orig` (lines 98 and 104) and
      re-render that vignette. Reword the marker sentence in the
      `.github/workflows/vignette-precompute.yaml` header comment (lines
      15-17). Delete `tools/m132-planted-defects.R`. Run the width checker, the
      AC2 grep and the AC5 diff.
- [ ] T3: In temporary copies of `vignettes/`, plant each defect and record
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

## Decisions

## Review
