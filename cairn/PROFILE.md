# Toolchain profile: r-package

The R-package toolchain: devtools/roxygen/testthat/pkgdown, CRAN release.
Selected by `cairn-init` when a `DESCRIPTION` file is present.

## verify
Run by `/milestone-implement` (per task) and `/hotfix` (gate-lite):
- After roxygen changes: `Rscript -e 'devtools::document()'`.
- After code changes, before a task is checked off: `Rscript -e 'devtools::test()'` clean.
- `/hotfix` gate-lite: `devtools::test()` clean; `devtools::document()` if
  roxygen changed; `devtools::check()` if anything structural was touched.

## consistency-gate
Toolchain checks `/milestone-review` runs *in addition to* the universal
cairn-file checks (`cairn_validate`, coverage completeness, `cairn_impact`):
- `devtools::document()` produces no diff, and emits no unresolved-link warning:
  run `Rscript -e 'options(cli.width = 500); devtools::document()'` and require
  zero output lines matching `resolve link` (the width is pinned because cli
  wraps its output, and a break falling inside the phrase hides a live warning
  from a line-based grep). roxygen reads `[...]` in a markdown block as link
  syntax, so bare interval notation (`[0, 360]`) and references to unexported
  functions both warn; protect them with backticks or an escaped closing
  bracket (`[0, 360\]`).
- Generated files are never hand-edited: `NAMESPACE`, `man/`, and `data/*.rda`
  regenerate from roxygen and `data-raw/` scripts (the no-diff `document()`
  check catches drift).
- README.md is knitted from README.Rmd; present and out of sync with README.md → `devtools::build_readme()`, commit.
- pkgdown site present → `pkgdown::check_pkgdown()` passes (catches exports missing from `_pkgdown.yml`).
- NEWS.md has an entry for this milestone's user-visible changes (no milestone numbers in user-facing text).
- New top-level files have `.Rbuildignore` entries (check `check()` NOTEs).
- Full check at review: `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; justify NOTEs).
- Master watches (`gh run list --workflow=<file> --branch=<default> --event=push`):
  the newest push run of `R-CMD-check.yaml` on the default branch must have
  concluded success — red OR ABSENT is a gate failure (M93). The newest
  `test-coverage.yaml` run that concluded success or failure must also be a
  success (M95); there alone, an absent run is no verdict rather than a failure
  (`paths-ignore` — say so at the gate). Both read the newest *verdict*, not the
  newest completed: `cancelled` is completed without being one (run 32187677266).
  A red is cleared via `/hotfix` (gate-lite skips this slot), so this cannot
  deadlock against "never implement on the default branch". The coverage watch
  reads one milestone LATE, catching a covr-only regression at the NEXT gate;
  `covr` perturbs optimizer results, so its environment is distinct (M59).
  Cross-check any RED answer: on 2026-08-18 this returned a ten-day-stale red
  set while the same query without `--event` showed green (M96).
- Master-red alert audits (M96): `Rscript tools/check-master-red-alert.R` and
  `Rscript tools/master-red-alert-dryrun.R` both exit clean — nothing else runs
  them, and the alert workflow fires only when master is already broken. They
  need `yaml` and `jq` (neither a package dependency), and say so if absent.

## test-doctrine
R-mechanical test expectations layered on the universal "What gets a test"
rules in tracking-rules:
- Tests are written for `testthat` edition 3 (3e).
- Every exported function: happy path, every `cli_abort()` branch fired, R
  edge cases — zero rows, `NA`, length-one, factor vs. character, empty strings.
- New user-facing conditions use `cli::cli_abort()` / rlang, not assertthat.
- Indirect by default: internal helpers (direct tests only for independent logic).
- Never test print cosmetics beyond meaningful snapshots, trivial pass-throughs,
  dependency behavior, or plots except `vdiffr` when the plot is the product.
- The `covr` coverage NUMBER is a diagnostic, never a gate — no percentage
  target. The coverage JOB's pass/fail does gate (the master coverage watch):
  a red run means tests failed under instrumentation, not that coverage fell.
- GitHub Actions CI began as the standard usethis pair and has since diverged.
  `R-CMD-check.yaml` checks across platforms on a matrix `tools/ci-matrix.R`
  computes per run (M93); `test-coverage.yaml` runs `covr` and uploads to
  Codecov. Neither is stock — edit the files, not the usethis generators, which
  would overwrite both. Codecov posts nothing on a PR either way (`codecov.yml`
  sets `comment: false`, both statuses `informational: true`), and since M95 the
  coverage workflow runs on push only, so its result reaches a PR by no route.
  Give `.github/` an `.Rbuildignore` `^\.github$` entry so it stays out of the
  built package.
- Change governance renders here as: the dependency surface is DESCRIPTION
  Imports/Suggests; a breaking-change deprecation cycle warns via `lifecycle`
  (`deprecate_warn()`) before removal. The gates themselves — question-gate +
  D-entry for dependencies, pre-1.0 waiver rule — are universal
  (tracking-rules "Universal tracking rules").
- Every newly exported object gets a `_pkgdown.yml` reference-index row in the same commit.
- Every committed test fixture carries reproducible provenance: its source and
  the committed generator (a `data-raw/` script) that regenerates it from scratch, plus any seed —
  the R-mechanical form of the universal Reproducibility hard-stop. The required
  content is fixed; the shape is the repo's choice — a `provenance` attribute,
  embedded `.rds`/`.rda` fields, or a header comment naming source + generator + seed.

## release-walk
Followed by `/cairn-release` — a CRAN release walk (never self-submits):
- Version decision (patch/minor/major) from NEWS.md; pre-1.0 conventions per DESIGN.md.
- NEWS consolidation: retitle the dev heading to the version; group entries; prune noise.
- Full local verification: `devtools::document()` (no diff), `devtools::test()`
  and `devtools::check()` clean, `devtools::build_readme()`, `pkgdown::check_pkgdown()`,
  `urlchecker::url_check()`.
- Wide checks as applicable: `devtools::check_win_devel()` and/or R-hub; `revdepcheck` if dependents exist.
- Update `cran-comments.md` (test environments, check results, NOTE justifications, revdep summary).
- Bump `Version:` in DESCRIPTION.
- Handoff checklist (user runs): `devtools::submit_cran()`, confirm the CRAN
  email, then `usethis::use_github_release()` + `usethis::use_dev_version()`.

## changelog
The repo's changelog file, read by `/hotfix`, the release-walk, and the
consistency-gate: **`NEWS.md`** (the R-package convention).

## init-detection
Recognized by `cairn-init` when a **`DESCRIPTION` file is present** at the repo
root. Carries the `.Rbuildignore` `^cairn$` entry (keeps the tracking dir out
of the built package).

## greenfield-openers
Language-specific opener `cairn-init` asks in a new/empty R package. The
universal openers — distribution ambition (rendered here as **CRAN intent**) and
numeric-work-needs-oracle-verification — are asked by cairn-init's universal
layer, so they are not repeated here.

- **Compiled code?** Will the package include compiled code
  (Rcpp / RcppArmadillo / C / C++ / Fortran)?
  - Options: **pure R** (reversible default) · Rcpp · RcppArmadillo.
  - Consequence: compiled ⇒ a `src/` dir, `LinkingTo`, a C/C++ toolchain, and
    `R CMD check` compiling on every check. Adding compiled code later is
    additive, so the reversible default is pure R.
  - Lands in: DESIGN Conventions (a "compiled code via <pkg>" line) and informs
    the `verify` / `test-doctrine` check surface.
