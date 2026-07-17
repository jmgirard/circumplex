# circumplex — assistant instructions

R package for circumplex data analysis (SSM: Structural Summary Method).
On CRAN. Maintainer: Jeff Girard. Statistical correctness outranks all
other concerns; angular/boundary behavior is where bugs hide.

## Commands

- Test: `Rscript -e 'devtools::test()'`
- Check: `Rscript -e 'devtools::check(args = "--no-manual")'` (slow; run
  before commits touching R/ or src/)
- Document: `Rscript -e 'devtools::document()'` (after changing any
  roxygen)
- Rebuild C++ after editing src/:
  `Rscript -e 'Rcpp::compileAttributes(); devtools::load_all()'`
- Never edit generated files by hand: `R/RcppExports.R`,
  `src/RcppExports.cpp`, `man/*.Rd`, `NAMESPACE`

## Statistical invariants (do not violate; test at these boundaries)

- Angles: **degrees \[0, 360) in the user API, LM = 360 not 0** (norms
  tables and
  [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)
  both use 360). Radians internally via
  `circumplex_degree`/`circumplex_radian` S3 classes.
- Contrasts: **second minus first** factor level (alphabetical unless
  factor), reported in (-180°, 180°\] via `angle_dist()`. **Occasion
  contrasts: second listed minus first listed** (`names(occasions)` list
  order, temporal), never alphabetical — a `T10`/`T2` pair must not
  flip.
- Displacement CIs use circular quantiles (center on circular mean,
  unwrap, quantile, re-wrap); contrast CIs may legitimately be negative,
  and are reported on the same branch as their estimate (endpoints may
  exceed ±180° near the boundary; see cairn/DESIGN.md).
- Any change touching displacement, contrasts, or `src/` requires tests
  at: profiles peaking at 0°/360°, CIs straddling 0°/360°, contrasts
  near ±180°, flat (zero-variance) profiles.
- Closed-form SSM estimator equals OLS **for equally spaced angles**
  (exact condition: first+second harmonic balance — equal spacing is
  sufficient, not necessary; cairn/DESIGN.md). The SEM layer
  (`ssm_sem*`) always uses the OLS projection instead.

## Development workflow

Statistical-correctness doctrine (survives the cairn migration):

1.  **Non-trivial statistical changes get a written plan** before code.
2.  **Implement test-first** (testthat; a regression test reproducing
    the bug before the fix).
3.  **Validate estimation-code changes** (`ssm_*` statistics, `src/`)
    against an independent oracle. The repo-local
    `/statistical-validation` skill was entombed by the cairn migration
    to `cairn/legacy/statistical-validation/`; its validation battery
    still applies, and cairn’s oracle doctrine (`tracking-rules.md`
    “Validation doctrine”) reinforces it.
4.  **Review** with `/code-review` before committing; `/code-review max`
    for a statistically risky release. The billed cloud
    `/code-review ultra` is reserved for a flagship release and only
    when the user asks.

Model tiers (advisory, for the human choosing models): Fable for
estimator design/review and anything where plausible-but-wrong
statistics are possible; Opus for general implementation; Sonnet for
mechanical edits, doc updates, and running checks. **Always recommend a
tier when proposing or handing off a task**, with a one-line why mapped
to the task’s risk.

The pre-cairn skills `/next-task` and `/release-checklist` were entombed
to `cairn/legacy/`; their cairn-era replacements are `/milestone`
(status/what’s next) and `/cairn-release` (CRAN release walk). Project
status now lives in `cairn/ROADMAP.md`, not this file — see the Project
tracking section below.

## Style

- Base R + minimal deps (rlang, ggplot2, boot, Rcpp/RcppArmadillo); no
  tidyverse in package code. Match existing code style: roxygen2
  markdown, [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)
  validation with the `is_*()` helpers in R/utils.R, prefer
  [`inherits()`](https://rdrr.io/r/base/class.html) over `class(x) ==`.
- Vignettes are teaching documents — keep prose statistically precise
  (e.g., never describe an angular CI excluding 0° as a significance
  test).

## Project tracking (cairn)

This repo uses the cairn plugin. **Before acting on any request,
classify it and route** — the tracking rulebook only loads once a cairn
skill fires, so starting work in plain conversation silently bypasses
the work tiers and the git model. Classify first:

- **Trivial** (no runtime surface — typo, comment, tracking edit):
  commit directly to the default branch.
- **User-visible bug**: invoke `/hotfix`.
- **New work, a design decision, or more than one sitting**: invoke
  `/milestone-plan` (then `/milestone-implement` → `/milestone-review`).
- **Status, “what’s next”, or unsure which tier**: invoke `/milestone`.
- **Never implement code on the default branch** outside a
  milestone/hotfix branch; nothing reaches it without the user’s
  explicit approval at the review gate.

Whenever the request is anything but trivial, invoke the skill *first*
so the full rulebook (the plugin’s `skills/shared/tracking-rules.md`)
and its conduct load — do not reconstruct the rules here from memory.
All project state lives under `cairn/` (**Architecture → DESIGN · Status
→ ROADMAP · Tasks → milestone files · Decisions → DECISIONS · History →
archive + git**); never record status or TODOs in this file. Claude’s
persistent memory never holds project state; `cairn/` files win any
conflict.
