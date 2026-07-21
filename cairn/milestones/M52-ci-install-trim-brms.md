# M52: Trim brms/Stan from CI dependency installs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m52-ci-install-trim-brms · https://github.com/jmgirard/circumplex/pull/78

## Goal

Exclude brms and its transitive Stan toolchain from both CI workflows' dependency
installs, cutting per-run install cost with zero loss of check or coverage surface.

## Scope

**In:** In `R-CMD-check.yaml` and `test-coverage.yaml`, stop installing `brms`
(and everything only it pulls in — `rstan`, `StanHeaders`, `RcppParallel`, `BH`,
the heaviest compile in the tree). Switch each `setup-r-dependencies` step from
whole-Suggests resolution to hard deps + an explicit Suggests allowlist that
mirrors DESCRIPTION Suggests **minus brms**, with `_R_CHECK_FORCE_SUGGESTS_: false`
on the check job so an absent Suggest degrades (skip) rather than ERRORs. brms is
safe to drop because it is never loaded by package code, tests, or vignette build:
the Bayesian vignette's `brm()` chunk is `eval = FALSE` and ships a committed
`.rds` fixture (D-015).

**Out:**
- Excluding OpenMx / glmmTMB / lavaan or any other Suggest from either job —
  Jeff chose brms-only. Those stay installed (OpenMx/glmmTMB `skip_on_cran`
  oracles run under coverage; glmmTMB and lavaan evaluate the growth/SEM vignette
  chunks). Further trimming stays the M51-lineage candidate row (narrowed, not
  retired).
- Changing DESCRIPTION `Suggests` — brms remains a declared Suggest (CRAN + the
  vignette's documented reproduction path); only the CI install is trimmed.
- Caching/preinstall schemes → the r-lib action's pak cache already caches; the
  lever here is exclusion.

## Acceptance criteria

- [ ] AC1 — brms and its Stan-only transitive deps (`rstan`, `StanHeaders`) are
      absent from the installed dependency set in **both** workflows, evidenced by
      the milestone PR's Actions "install dependencies" step logs not fetching or
      compiling them.
- [ ] AC2 — `R-CMD-check` stays green on the PR job (0 errors / 0 warnings; NOTEs
      no worse than the pre-change baseline): all eight vignettes build (the
      Bayesian vignette knits its `eval = FALSE` brms chunk; the growth and SEM
      vignettes still evaluate their chunks because glmmTMB and lavaan remain
      installed) and the full test suite passes with its `skip_*` guards intact.
- [ ] AC3 — the `test-coverage` job stays green and reported coverage does not
      drop versus the pre-change baseline (nothing under coverage depends on brms).
- [ ] AC4 — a before/after wall-time comparison of the dependency-install step,
      from this milestone's own CI runs, documents a real reduction. Primary
      measurement is the PR's ubuntu job (RSPM binaries — the removed
      brms/rstan/StanHeaders binary fetch); the larger macOS/Windows
      source-compile saving is noted and confirmed at the post-merge push matrix.
- [ ] The profile `verify` slot is satisfied trivially — no package files change
      (only `.github/workflows/`), so `devtools::document()` yields no diff and the
      local suite is unaffected; the substantive R CMD check evidence is the CI run.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T1, T2

## Tasks

- [x] T1 — In `.github/workflows/R-CMD-check.yaml`, replace the `needs: check`
      resolution with `dependencies: '"hard"'` + an `extra-packages` allowlist of
      every DESCRIPTION Suggest except brms (plus `any::rcmdcheck`); add
      `_R_CHECK_FORCE_SUGGESTS_: false` to the job env if the check ERRORs on the
      absent brms. Add an inline comment: brms is excluded because it is never
      loaded (D-015), and the allowlist must mirror DESCRIPTION Suggests minus brms.
- [x] T2 — Apply the same allowlist trim to `.github/workflows/test-coverage.yaml`
      (`extra-packages: any::covr, any::xml2` + the same Suggests-minus-brms list).
- [ ] T3 — Push the branch, open the PR, and verify: R-CMD-check + coverage green;
      capture the "install dependencies" step durations before/after and confirm
      brms/rstan/StanHeaders are gone from the install logs (AC1); record the
      measured delta and the coverage figure in the work log.

## Work log

- 2026-07-21: created by /milestone-plan. Promotes the `CI dependency-install
  cost` candidate (M51 scope note, 2026-07-21); Jeff chose brms-only trim + a
  measured-delta merge bar at the plan gate. Load-bearing evidence: D-015 (brms
  inert — precomputed vignette), D-016 (glmmTMB eval-guarded, kept).
- 2026-07-21: T1+T2 done — both workflows switched from `needs: check` /
  `needs: coverage` (whole-Suggests resolution) to `dependencies: '"hard"'` + an
  explicit 14-pkg allowlist = DESCRIPTION Suggests minus brms (check job adds
  rcmdcheck; coverage job adds xml2). `_R_CHECK_FORCE_SUGGESTS_: false` on the
  check job so the absent brms is skippable, not a check WARNING under --as-cran.
  Both files YAML-validated; brms confirmed absent from both installs. Only
  `.github/workflows/` touched, so the profile verify slot holds trivially
  (no package-file diff). T3 (CI-green + measured delta) needs the PR's runs.
- 2026-07-21: T3 in progress — PR #78 opened against master; R-CMD-check
  (ubuntu-release PR job), test-coverage, and pkgdown running. Baseline for the
  AC4 delta: the untrimmed workflow on the identical plan commit `6d45597b`
  (master run 29845320357), whose ubuntu-release job is the before-counterpart.
  Awaiting CI settle to capture install-step timings and confirm brms/Stan absent.

## Decisions

## Review
