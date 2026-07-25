# M58: Finish the post-M52 CI trim — pkgdown parity + an allowlist drift guard

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m58-ci-trim-pkgdown-parity`

## Goal

Close out the post-M52 CI cleanup by giving `pkgdown.yaml` the dependency and
run-triggering discipline the other two workflows already carry, backed by a
mechanical allowlist/DESCRIPTION sync check.

## Scope

**In:**
- `pkgdown.yaml`: `dependencies: '"hard"'` + explicit allowlist (DESCRIPTION
  `Suggests` minus brms, plus `any::pkgdown`, `local::.`), replacing
  `needs: website` — its lockfile currently carries brms 2.23.0 + the Stan
  stack M52 removed from the other two jobs.
- `pkgdown.yaml`: M51-shape concurrency (`cancel-in-progress: true`, stable
  per-ref group) — today's PR group key is `github.run_id`, unique per run, so
  superseded runs never cancel.
- `paths-ignore: [cairn/**, man/**, README.md]` on the `push` trigger of all
  three workflows, and on `pkgdown.yaml`'s `pull_request` trigger (it has
  none). No branch protection on `master` (verified 2026-07-25), so no
  required check is left pending.
- `tools/check-ci-deps.R` (base R) asserting each workflow allowlist equals
  DESCRIPTION `Suggests` minus that file's documented exclusions; run as a
  step in `R-CMD-check.yaml` after `setup-r`. `^tools$` → `.Rbuildignore`.

**Out:**
- Removing OpenMx or glmmTMB from any workflow — declined outright at this
  milestone's plan gate and recorded as D-029, not deferred.
- Any DESCRIPTION `Suggests` change: none needed; D-015/D-016 stand untouched.
- Further `R-CMD-check.yaml` matrix or runner changes → M51 settled those;
  reopening needs its own milestone.
- Retiring M52's LESSONS drift line → `/milestone-review` post-merge hygiene's
  call once AC5 proves the guard has teeth.

## Acceptance criteria

- [ ] AC1 — The pkgdown job resolves and installs none of brms, rstan,
      StanHeaders, RcppParallel, BH: its live job log's resolution output
      contains no match for them, and the site build is green.
- [ ] AC2 — pkgdown's allowlist equals DESCRIPTION `Suggests` minus brms, and
      the built site's growth, SEM, and axes-reliability articles show their
      fitted results (glmmTMB/lavaan chunks evaluated, not the not-installed
      note).
- [ ] AC3 — Two pushes to the branch in quick succession leave exactly one
      pkgdown run uncancelled; the superseded run's conclusion is `cancelled`.
- [ ] AC4 — A `cairn/**`-only push to `master` (the review's own post-merge
      hygiene commit) triggers zero runs of all three workflows, per
      `gh run list` taken after it.
- [ ] AC5 — `tools/check-ci-deps.R` is proven by mutation, not by eye: a
      `Suggests` entry injected without an allowlist update turns the
      R-CMD-check job red with a message naming the package and the file, and
      the revert turns it green. Both run URLs in the work log.
- [ ] AC6 — `devtools::check()` clean (0 errors / 0 warnings / 0 notes) and
      the full CI matrix green on `master` after merge.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T1, T5, T6
- AC6 → T2, T3, T4, T5

## Tasks

- [ ] **T1** — Write `tools/check-ci-deps.R`: `read.dcf` for `Suggests`, parse
      each workflow's `extra-packages:` block by indentation, compare against
      per-file documented exclusions, `stop()` naming package + file on drift.
      Add `^tools$` to `.Rbuildignore`. Run locally: true state passes,
      injected drift fails.
- [ ] **T2** — Rewrite `pkgdown.yaml`'s `setup-r-dependencies` step (drop
      `needs: website`; `dependencies: '"hard"'` + allowlist), carrying an
      M52-style comment naming the brms exclusion and pointing at T1's guard.
- [ ] **T3** — `pkgdown.yaml` concurrency → `group: ${{ github.workflow }}-${{
      github.ref }}`, `cancel-in-progress: true`.
- [ ] **T4** — Add `paths-ignore` to the `push` trigger of all three
      workflows and to `pkgdown.yaml`'s `pull_request` trigger.
- [ ] **T5** — Wire `Rscript tools/check-ci-deps.R` into `R-CMD-check.yaml`
      as a step after `setup-r`, before `setup-r-dependencies`.
- [ ] **T6** — Prove the guard by mutation on the branch: inject a `Suggests`
      entry, push, observe red naming it, revert, observe green; record both
      run URLs.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-25: status planned->in-progress; branch `m58-ci-trim-pkgdown-parity` cut from master@425fd294.

## Decisions

## Review
