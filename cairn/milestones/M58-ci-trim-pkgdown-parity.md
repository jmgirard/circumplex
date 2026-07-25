# M58: Finish the post-M52 CI trim — pkgdown parity + an allowlist drift guard

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m58-ci-trim-pkgdown-parity` / [PR #84](https://github.com/jmgirard/circumplex/pull/84)

## Goal

Close out the post-M52 CI cleanup by giving `pkgdown.yaml` the dependency and
run-triggering discipline the other two workflows already carry, backed by a
mechanical allowlist/DESCRIPTION sync check.

## Scope

**In:**
- `pkgdown.yaml`: `dependencies: '"hard"'` + explicit allowlist (DESCRIPTION
  `Suggests` minus brms, plus `any::pkgdown`, `local::.`), replacing
  `needs: website` — its lockfile currently carries brms 2.23.0 and rstan,
  which the site never uses. (M52's "brms + its Stan stack
  rstan/StanHeaders/RcppParallel/BH" framing misattributed three of those:
  BH/RcppParallel/StanHeaders are OpenMx's, not brms's.)
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

- [ ] AC1 — The pkgdown job no longer resolves or installs brms or rstan:
      `any::brms` appears zero times in the job's `.github/pkg.lock`, and
      neither package appears in its install plan. BH, RcppParallel and
      StanHeaders are deliberately NOT part of this criterion — they are
      OpenMx `LinkingTo`/`Imports` dependencies, and D-029 keeps OpenMx, so
      they are installed by design. A session-info listing is not evidence
      either way: it reports the restored cache's library, not this run's
      install plan.
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

- [x] **T1** — Write `tools/check-ci-deps.R`: `read.dcf` for `Suggests`, parse
      each workflow's `extra-packages:` block by indentation, compare against
      per-file documented exclusions, `stop()` naming package + file on drift.
      Add `^tools$` to `.Rbuildignore`. Run locally: true state passes,
      injected drift fails.
- [x] **T2** — Rewrite `pkgdown.yaml`'s `setup-r-dependencies` step (drop
      `needs: website`; `dependencies: '"hard"'` + allowlist), carrying an
      M52-style comment naming the brms exclusion and pointing at T1's guard.
- [x] **T3** — `pkgdown.yaml` concurrency → `group: ${{ github.workflow }}-${{
      github.ref }}`, `cancel-in-progress: true`.
- [x] **T4** — Add `paths-ignore` to the `push` trigger of all three
      workflows and to `pkgdown.yaml`'s `pull_request` trigger.
- [x] **T5** — Wire `Rscript tools/check-ci-deps.R` into `R-CMD-check.yaml`
      as a step after `setup-r`, before `setup-r-dependencies`.
- [x] **T6** — Prove the guard by mutation on the branch: inject a `Suggests`
      entry, push, observe red naming it, revert, observe green; record both
      run URLs.

## Work log

- 2026-07-25: created by /milestone-plan.
- 2026-07-25: status planned->in-progress; branch `m58-ci-trim-pkgdown-parity` cut from master@425fd294.
- 2026-07-25: T1 done — `tools/check-ci-deps.R` (base R, handles block + inline `extra-packages` forms); `^tools$` added to .Rbuildignore. Local teeth check: flags pkgdown's real drift, and an injected `tibble` Suggest is named per-file; exit 1 both, clean revert.
- 2026-07-25: T2-T5 done — pkgdown gets the hard-deps+allowlist install (brms/Stan out, glmmTMB/lavaan kept), workflow-level `cancel-in-progress`, and paths-ignore; push triggers on all three workflows gain paths-ignore; guard wired into R-CMD-check after setup-r. Guard now exits 0; all three YAMLs parse; `devtools::test()` 0 FAIL / 3247 PASS / 0 SKIP.
- 2026-07-25: AMENDMENT (substantive, user-gated) — AC1 and the pkgdown Scope bullet narrowed from five packages to brms+rstan. BH/RcppParallel/StanHeaders are OpenMx LinkingTo/Imports deps (PR #84 pkgdown log, `"ref": "any::OpenMx"` block), not brms's, so D-029 guarantees their presence and the original AC1 was unsatisfiable. Misattribution inherited from M52's framing; M52's archive left untouched (history, IP4) and its R-CMD-check comment left alone at the user's choice.
- 2026-07-25: T6 done — guard proven by mutation on CI. RED: injecting `tibble` into Suggests failed run 30165493362 AT the guard step (step 5 failure, steps 6-7 SKIPPED, so it fired before the install), message naming tibble and all three workflow paths. GREEN: revert a2d2dd2b, run 30165840887 pass. Baseline green run 30165014607.
- 2026-07-25: AC3 evidence came free from the T6 push pair — push A (07e25550) had all three runs `cancelled` (pkgdown 30165483647) when push B (9af8d3f8) superseded it; exactly one pkgdown run uncancelled per sha.
- 2026-07-25: AC1 evidence (final run 30165840898): `"ref": "any::brms"` count 0; install plan 21 pkgs, 13 named, no brms/rstan; pak's direct-ref set is 12 entries, none of them brms/rstan. knitr/rmarkdown/RColorBrewer verified PRESENT in the final library (not silently dropped) despite not appearing as direct refs.
- 2026-07-25: CAVEAT recorded honestly — brms/rstan are still PRESENT in the restored library cache (session info lists them as RSPM) because the cache archives the whole library and was seeded from the pre-change cache. The lockfile no longer requests them, so a cold cache or the next key rotation drops them, but the wall-clock install saving is not realized today (dep step 54s -> 69s across the change; the 69s run reinstalled 13 pkgs after the lockfile key changed). The durable win is correctness; the speed wins in this milestone are paths-ignore and cancel-in-progress.
- 2026-07-25: `devtools::check(args="--no-manual")` 0 errors / 0 warnings / 0 notes. `^tools$` confirmed effective: a built tarball contains 0 `circumplex/tools` entries (probe sanity: 32 `circumplex/R/` entries). PDF-manual step did not run (--no-manual) — no roxygen was touched this milestone, so the M7/M57 manual gap does not apply.
- 2026-07-25: all tasks done; status in-progress->review.

## Decisions

## Review
