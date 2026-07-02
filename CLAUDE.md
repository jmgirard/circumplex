# circumplex — assistant instructions

R package for circumplex data analysis (SSM: Structural Summary Method).
On CRAN. Maintainer: Jeff Girard. Statistical correctness outranks all other
concerns; angular/boundary behavior is where bugs hide.

## Commands

- Test: `Rscript -e 'devtools::test()'`
- Check: `Rscript -e 'devtools::check(args = "--no-manual")'` (slow; run before commits touching R/ or src/)
- Document: `Rscript -e 'devtools::document()'` (after changing any roxygen)
- Rebuild C++ after editing src/: `Rscript -e 'Rcpp::compileAttributes(); devtools::load_all()'`
- Never edit generated files by hand: `R/RcppExports.R`, `src/RcppExports.cpp`, `man/*.Rd`, `NAMESPACE`

## Statistical invariants (do not violate; test at these boundaries)

- Angles: **degrees [0, 360) in the user API, LM = 360 not 0** (norms tables
  and `octants()` both use 360). Radians internally via
  `circumplex_degree`/`circumplex_radian` S3 classes.
- Contrasts: **second minus first** factor level (alphabetical unless factor),
  reported in (-180°, 180°] via `angle_dist()`.
- Displacement CIs use circular quantiles (center on circular mean, unwrap,
  quantile, re-wrap); contrast CIs may legitimately be negative.
- Any change touching displacement, contrasts, or `src/` requires tests at:
  profiles peaking at 0°/360°, CIs straddling 0°/360°, contrasts near ±180°,
  flat (zero-variance) profiles.
- Closed-form SSM estimator equals OLS **only for equally spaced angles**.

## Workflow

Memory files: `ROADMAP.md` (multi-release direction), `MILESTONES.md` (active
milestone: tasks, acceptance criteria, running log), `DESIGN.md` (architecture,
conventions, decision rationale). Read MILESTONES.md before starting work;
append to its log after finishing anything.

Process for each unit of work — follow this loop, don't freestyle:

1. **Pick/plan**: use `/next-task` (or read MILESTONES.md and plan against its
   acceptance criteria). Non-trivial statistical changes get a written plan
   before code.
2. **Implement test-first** (testthat; regression test reproducing the bug
   before the fix).
3. **Validate**: if the change touches estimation code (`ssm_*` statistics,
   `src/`), run `/statistical-validation`.
4. **Review**: `/code-review` before committing; `/code-review ultra` before
   CRAN releases.
5. **Log**: check the box and append one line to the MILESTONES.md log
   (date, what, files). Update NEWS.md for user-facing changes.

Releases: use `/release-checklist`.

Model tiers (advisory, for the human choosing models): Fable for estimator
design/review and anything where plausible-but-wrong statistics are possible;
Opus for general implementation; Sonnet for mechanical edits, doc updates,
and running checks.

**Always recommend a tier when proposing or handing off a task** (e.g., in a
`/next-task` "what's next" line, or whenever suggesting the next unit of work):
name the tier and give a one-line why, mapped to the risk of the task. The
human chooses the model; your job is to make the call explicit every time, not
to wait to be asked.

## Style

- Base R + minimal deps (rlang, ggplot2, boot, Rcpp/RcppArmadillo); no
  tidyverse in package code. Match existing code style: roxygen2 markdown,
  `stopifnot()` validation with the `is_*()` helpers in R/utils.R,
  prefer `inherits()` over `class(x) ==`.
- Vignettes are teaching documents — keep prose statistically precise
  (e.g., never describe an angular CI excluding 0° as a significance test).
