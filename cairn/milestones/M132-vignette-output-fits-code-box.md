# M132: Rendered vignette output fits the website's code box

- **Status:** planned
- **Priority:** normal
- **Depends on:** M131
- **Driving RR:** —
- **Principles touched:** GP5
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes are the package's teaching material on the website
- **Branch/PR:** —

## Goal

No output line in a pre-rendered vignette makes the website's code box scroll
sideways.

## Scope

The pkgdown code box was measured on the live site on 2026-09-15. It is
774 px wide at every desktop viewport. At 1440 px and above the monospace
font is 15.75 px, so the box fits 81 characters. Its `overflow-x` is `auto`,
so a longer line scrolls sideways rather than being cut. knitr prefixes each
output line with `#> `, so the package's own line fits in 77 characters.

**In:** the setup chunk of each of the seven pre-rendered vignette sources
gets `options(width = 77)`. The two live vignettes get the same setting. The
introduction vignette stops passing the deprecated ggplot2 argument
`label.size` at its five sites. All seven vignettes are re-rendered. A new
`tools/check-vignette-width.R` guards the rendered files, and
`.github/workflows/vignette-precompute.yaml` runs it after the re-render.

**Out:** the two live vignettes are outside the guard's reach (plan gate).
`bayesian-ssm-analysis.Rmd` and `using-instruments.Rmd` are not pre-rendered,
so their shipped file holds no output line for a guard to read. They get the
width setting and a candidate row, not a criterion. The generated lavaan
syntax at `sem-based-ssm-analysis.Rmd:93-94` stays long behind a declared
exemption. Its length follows from the scale names, so no width setting
reaches it, and its emitter is pinned by stored snapshots. Wrapping the
package's own cautions is M131.

## Acceptance criteria

- [ ] AC1: `Rscript tools/check-vignette-width.R` exits 0. Every line in the
      seven pre-rendered `vignettes/*.Rmd` files that begins `#> ` and sits
      outside a declared exemption is 80 display columns or fewer. The 23
      lines over 81 columns that this plan measured at `26bd64ac` are gone.
- [ ] AC2: The guard goes red against each of four planted defect forms,
      applied one file at a time across all seven. Form one is an
      81-column ASCII line. Form two is an 81-column line built from `ζ`
      and `²`. Form three is a long line just outside an exemption marker.
      Form four is a long line that arrives from a re-render rather than
      from an edit. A long line just inside an exemption stays green.
- [ ] AC3: The guard's domain is not empty. It reports, for each file, how
      many `#> ` lines it read and how many exemptions it honored. Both
      counts appear in the milestone's review evidence, and the line count
      is above zero for all seven files.
- [ ] AC4: Re-running `Rscript tools/precompute-vignettes.R` and then
      `Rscript tools/check-vignette-staleness.R` reports no difference
      against the committed render.
- [ ] AC5: The rendered introduction vignette contains no line matching
      `deprecated`, and `grep -n "label.size" vignettes/` returns no hit.
- [ ] AC6: Exactly one exemption exists, at
      `sem-based-ssm-analysis.Rmd:93-94`. Its marker records that the line
      length follows from the scale names. The guard's exemption count of
      AC3 is 1.
- [ ] AC7: `Rscript -e 'devtools::test()'` is clean. Running
      `Rscript -e 'devtools::check(args = "--no-manual")'` reports no error,
      warning or note that master does not also report. The workflow run on
      the branch is green.

## Coverage

- AC1 → T1, T2, T4
- AC2 → T3
- AC3 → T3, T4
- AC4 → T4
- AC5 → T2
- AC6 → T1, T3
- AC7 → T5, T6

## Tasks

- [ ] T1: Write `tools/check-vignette-width.R`. Read every `#> ` line, count
      display columns, honor an exemption marker, and report per-file line
      and exemption counts. Add the exemption marker to
      `sem-based-ssm-analysis.Rmd.orig`.
- [ ] T2: Add `options(width = 77)` to all nine setup chunks. Replace
      `label.size = NA` in `introduction-to-ssm-analysis.Rmd.orig` at lines
      64, 84, 302, 315 and 336 with the current ggplot2 argument.
- [ ] T3: Run the four planted defect forms of AC2 and the inside-exemption
      control. Record each result.
- [ ] T4: Re-render the seven vignettes, commit the render, then run the
      width guard and the staleness guard.
- [ ] T5: Wire the guard into `.github/workflows/vignette-precompute.yaml`
      after the re-render step. Add `tools/check-vignette-width.R` to
      `.Rbuildignore`.
- [ ] T6: Run `devtools::test()` and `devtools::check(args = "--no-manual")`.
      Sweep vignette prose for any claim about the old output width.

## Work log

- 2026-09-15: created by /milestone-plan.
- 2026-09-15: plan gate chose an exemption marker for the generated lavaan lines over wrapping them or raising the guard's limit to 86. Wrapping changes code readers copy, and a global limit of 86 stops the guard catching real regressions. Falsified by a second exemption becoming necessary.
- 2026-09-15: plan gate chose the width setting without a guard for the two live vignettes, over adding them to the pre-render set. One of them needs brms, which makes the render job slow and fragile. Falsified by an over-long line reaching the site from either vignette.
- 2026-09-15: criteria audit ran in full mode. Its most serious finding concerned the guard's domain. The guard reads committed file text. That text is disjoint from the rendered output of the two live vignettes. A promise over all nine files therefore passes without checking two of them. The scope now names seven. The audit also replaced a single plant form with four, split an unsatisfiable CI promise, and dropped a mandated marker wording from a criterion.

## Decisions

## Review
