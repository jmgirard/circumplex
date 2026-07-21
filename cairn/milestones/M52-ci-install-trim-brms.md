# M52: Trim brms/Stan from CI dependency installs

- **Status:** review
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

- [x] AC1 — brms and its Stan-only transitive deps (`rstan`, `StanHeaders`) are
      absent from the installed dependency set in **both** workflows, evidenced by
      the milestone PR's Actions "install dependencies" step logs not fetching or
      compiling them.
- [x] AC2 — `R-CMD-check` stays green on the PR job (0 errors / 0 warnings; NOTEs
      no worse than the pre-change baseline): all eight vignettes build (the
      Bayesian vignette knits its `eval = FALSE` brms chunk; the growth and SEM
      vignettes still evaluate their chunks because glmmTMB and lavaan remain
      installed) and the full test suite passes with its `skip_*` guards intact.
- [x] AC3 — the `test-coverage` job stays green and reported coverage does not
      drop versus the pre-change baseline (nothing under coverage depends on brms).
- [x] AC4 — a before/after wall-time comparison of the dependency-install step,
      from this milestone's own CI runs, documents a real reduction. Primary
      measurement is the PR's ubuntu job (RSPM binaries — the removed
      brms/rstan/StanHeaders binary fetch); the larger macOS/Windows
      source-compile saving is noted and confirmed at the post-merge push matrix.
- [x] The profile `verify` slot is satisfied trivially — no package files change
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
- [x] T3 — Push the branch, open the PR, and verify: R-CMD-check + coverage green;
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
- 2026-07-21: T3 DONE — all CI green on PR #78 (R-CMD-check ubuntu-release 11m46s, test-coverage 11m58s success, pkgdown pass). AC1: brms/rstan/StanHeaders each have 0 download/`.tar.gz` install lines in the trimmed install vs 2 each in the baseline (master run 29845320357, untrimmed, same commit) — the Stan stack is no longer installed; RcppParallel remains but as an OpenMx dep, not a brms one. AC2: trimmed check `Status: OK` (0 errors/0 warnings/0 notes; with `_R_CHECK_FORCE_SUGGESTS_: false` the absent brms raised no note), `checking re-building of vignette outputs ... OK` (all 8 build, incl. the Bayesian eval=FALSE brms chunk and the glmmTMB/lavaan growth/SEM chunks); baseline check also `Status: OK` → notes no worse. AC3: coverage job success; OpenMx/glmmTMB/lavaan stay installed so the skip_on_cran oracles still ran, and no test references brms, so the executed-test set (hence coverage) is unchanged. AC4: setup-r-dependencies step 60s (baseline, warm cache + full set) → 41s (trimmed, cold cache + 21 pkgs), and the Stan stack is categorically no longer downloaded/installed. HONEST CAVEAT for review: pak installs Stan as RSPM prebuilt binaries with caching, so the install step is <1 min of a ~12-min job — the wall-time win is real but modest, NOT the CI-dominating cost the candidate premised; the timing is also cache-confounded (baseline warm/full vs trimmed cold), so steady-state warm-trimmed installs will be <=41s. Status in-progress -> review.

## Decisions

## Review

Reviewed 2026-07-21. PR [#78](https://github.com/jmgirard/circumplex/pull/78).
Branch diff = 2 workflow files + 2 cairn tracking docs; **zero package/generated
files**, so the r-package consistency-gate's package-level checks (document
no-diff, generated-file integrity, README knit, pkgdown, `check()` clean) are
no-ops by construction and the authoritative `R CMD check` ran on CI. Evidence
is from PR #78's runs on byte-identical workflow files (the commits after the
last green run touched only `cairn/`); the current-HEAD re-run was required green
at the merge gate.

### Acceptance-criterion evidence

- AC1 — trimmed ubuntu-release install log (job 88686148549): **0** download/
  `.tar.gz` lines for each of brms, rstan, StanHeaders; untrimmed baseline
  (master run 29845320357, job 88684406858, same commit `6d45597b`) shows **2**
  each. Stan stack no longer installed; RcppParallel remains as an OpenMx dep.
- AC2 — trimmed `R CMD check` **Status: OK** (0 errors / 0 warnings / 0 notes);
  `checking re-building of vignette outputs ... OK` (all 8 build incl. the
  Bayesian `eval=FALSE` brms chunk and the glmmTMB/lavaan growth/SEM chunks).
  Baseline check also `Status: OK` → notes no worse. `_R_CHECK_FORCE_SUGGESTS_:
  false` made the absent brms raise no note.
- AC3 — test-coverage job 88686148996 **success**; OpenMx/glmmTMB/lavaan stay
  installed so the `skip_on_cran` oracles still ran, and no test references brms,
  so the executed-test set (hence coverage) is unchanged.
- AC4 — `setup-r-dependencies` step **60s** (baseline, warm cache + full set) →
  **41s** (trimmed, cold cache + 21 pkgs); Stan stack categorically dropped.
  Honest caveat carried from the work log: pak installs Stan as RSPM binaries +
  cache, so the install step is <1 min of a ~12-min job — the win is real but
  modest, not CI-dominating; timing is cache-confounded (warm/full vs cold), so
  steady-state warm-trimmed installs ≤41s.
- AC5 / profile verify — `git diff --name-only origin/master..HEAD` = the two
  workflows + two cairn docs; no package files, so `document()` no-diff is
  trivial and the local suite is unaffected.

### Consistency gate

`cairn_validate` green (advisory work-log-wrap WARNs only; cleared when the live
file is replaced by the archive summary). No principle change → `cairn_impact`
skipped. Package-level r-package checks no-op (no package files touched); the
substantive `R CMD check` ran green on CI.

### Independent review — three lenses, zero findings

- [O] diff-bug — no findings. Allowlist = DESCRIPTION Suggests minus brms exactly
  (13 + `rcmdcheck` / `covr`+`xml2`); `dependencies: '"hard"'` and force-suggests
  placement correct; YAML valid; M51 concurrency/paths-ignore/matrix preserved;
  D-015/D-016 consistent.
- [S] blame-history — no findings. M51's additions intact; no resurrected bug;
  consistent with D-015/D-016.
- [S] prior-review — no regressions. M51's archived Review had zero findings;
  GitHub PR-comment probe empty (no threads); no prior finding to reintroduce.

No finding reached the scorer (clean no-op); none below-80 to log.

### Note for future maintenance (not a finding)

`_R_CHECK_FORCE_SUGGESTS_: false` removes the error that would otherwise fire if
a future Suggest is added to DESCRIPTION but forgotten from the CI allowlist — it
would silently skip instead. Intentional tradeoff of the chosen mechanism,
documented in the workflow comment; captured as a LESSONS line at archive.
