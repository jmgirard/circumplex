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

- [x] AC1 — The pkgdown job no longer resolves or installs brms or rstan:
      `any::brms` appears zero times in the job's `.github/pkg.lock`, and
      neither package appears in its install plan. BH, RcppParallel and
      StanHeaders are deliberately NOT part of this criterion — they are
      OpenMx `LinkingTo`/`Imports` dependencies, and D-029 keeps OpenMx, so
      they are installed by design. A session-info listing is not evidence
      either way: it reports the restored cache's library, not this run's
      install plan.
- [x] AC2 — pkgdown's allowlist equals DESCRIPTION `Suggests` minus brms, and
      the built site's growth, SEM, and axes-reliability articles show their
      fitted results (glmmTMB/lavaan chunks evaluated, not the not-installed
      note).
- [x] AC3 — Two pushes to the branch in quick succession leave exactly one
      pkgdown run uncancelled; the superseded run's conclusion is `cancelled`.
- [ ] AC4 — A `cairn/**`-only push to `master` (the review's own post-merge
      hygiene commit) triggers zero runs of all three workflows, per
      `gh run list` taken after it.
- [x] AC5 — `tools/check-ci-deps.R` is proven by mutation, not by eye: a
      `Suggests` entry injected without an allowlist update turns the
      R-CMD-check job red with a message naming the package and the file, and
      the revert turns it green. Both run URLs in the work log.
- [x] AC6 — `devtools::check()` clean (0 errors / 0 warnings / 0 notes) and
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
- 2026-07-25: Actions cache purge at user request — 25 `Ubuntu…4.6.1…x86_64-1-` caches deleted (4.1 GB); the 25 other-platform caches left alone. Needed because `restore-keys` is a PREFIX, so deleting only pkgdown's entry would have fallen back to a brms-carrying check-job cache.
- 2026-07-25: FINDING (wider than M58) — the check job's library still carried brms 2.23.0 / rstan / StanHeaders four days after M52 removed them from its allowlist (run 30165840887 session info). The cache archives the whole library and reseeds itself each run, so a package stops being REQUESTED but is never EVICTED. M52's 60s->41s was a resolution effect; the Stan stack sat in every Ubuntu cache continuously. Only a cache purge sheds it.
- 2026-07-25: post-purge measurement, pkgdown job (run 30166582895). Cold: dep-install 161s, job 6.3 min (one-off). Warm after: dep-install 69s, job 4.7 min. Warm before (brms in cache): 69s / 4.3 min. Pre-change: 54s / 4.3 min. HONEST BOTTOM LINE — removing brms buys NO measurable install-step time; the measurable win is cache size, 177MB -> 117MB (-34%). M58's actual speed wins are paths-ignore and cancel-in-progress; the brms removal is a correctness/durability win.
- 2026-07-25: cold run verified brms, rstan, StanHeaders and BH all ABSENT from the pkgdown library (probe sanity: lavaan/OpenMx/glmmTMB all present). Refines AC1's parenthetical: BH and StanHeaders are OpenMx LinkingTo and are NOT installed when OpenMx resolves to an RSPM binary; only RcppParallel (Imports) persists. AC1's operative clause (brms+rstan absent) is met and then some.
- 2026-07-25: FINDING — `paths-ignore` on a `pull_request` trigger is evaluated against the WHOLE PR diff, not the pushed commit, so a cairn-only commit mid-PR still runs all three workflows (observed: 8daae06c touched only `cairn/**` and triggered all three). This predates M58 — M51's PR paths-ignore never skipped mid-PR tracking commits either. AC4 is unaffected: it tests a PUSH event, where the filter does read that push's own files.

## Decisions

## Review

Reviewed 2026-07-25 on branch `m58-ci-trim-pkgdown-parity`, PR #84. Master in
sync (0/0 vs origin), branch 0 behind — no merge needed, evidence not stale.

**AC1 — pkgdown no longer resolves/installs brms or rstan. VERIFIED.**
Latest pkgdown job (run 30166582895, conclusion success): `"ref": "any::brms"`
0 occurrences, `"ref": "any::rstan"` 0. The 12 direct `any::` refs pak was
asked for are OpenMx, covr, ggrepel, glmmTMB, kableExtra, lavaan, pkgdown,
psych, roxygen2, sessioninfo, testthat, vdiffr — neither package among them.
Post cache-purge both are absent from the built library too (stronger than the
criterion asks). RcppParallel remains, correctly: OpenMx `Imports`, kept by D-029.

**AC2 — allowlist mirrors Suggests; articles keep their fits. VERIFIED.**
Allowlist-vs-DESCRIPTION compared by an independent parser, NOT by
`tools/check-ci-deps.R` (the artifact under test): allowlist minus its declared
extras (`pkgdown`, `local::.`) == Suggests minus brms, exact set equality.
Same run wrote 10 article pages with 0 degradation notes — grepped for the exact
strings the vignettes emit when a package is missing
(`vignettes/growth-ssm-analysis.Rmd:51`, `axes-reliability.Rmd:25`) — and
glmmTMB, lavaan and OpenMx are all present in the library.

**AC3 — superseded runs cancel. VERIFIED.**
Push 07e25550 then 9af8d3f8 in quick succession: all three of the former's runs
have conclusion `cancelled` (pkgdown 30165483647); the latter's pkgdown run
30165483368/30165493368 ran to success. Exactly one uncancelled pkgdown run
across the pair.

**AC5 — guard proven by mutation. VERIFIED, step-level.**
RED run 30165493362 with `tibble` injected into Suggests: job `failure`, step 5
"Check CI dependency allowlists match DESCRIPTION" `failure`, steps 6 and 7
`skipped` — it fired BEFORE the dependency install, as designed. Message named
tibble against all three workflow paths. GREEN run 30165840887 after revert
a2d2dd2b: step 5 `success`, steps 6-7 `success`, job `success`. DESCRIPTION is
byte-identical to master (empty diff) — the mutation pair cancels out.

**AC6 — full check clean. VERIFIED (local half).**
Fresh `devtools::check(args="--no-manual")` on the review branch: `Status: OK`,
`0 errors | 0 warnings | 0 notes`, 6m24s, zero N/W lines. The "full CI matrix
green on master after merge" clause is inherently post-merge and is completed
at step 9.

**AC4 — NOT YET VERIFIABLE (by its own wording).**
The criterion names "the review's own post-merge hygiene commit" as its
evidence, so it cannot be executed before the merge. Ticked at step 9 against
`gh run list` taken after that commit. Baseline for contrast: the pre-change
plan commit 425fd294 was a `cairn/**`-only push to master and triggered all
three workflows.

**Consistency gate — all clean.**
`cairn_validate`: 15 PASS (incl. `coverage complete`, `weight caps`), 0 FAIL;
47 advisories, all pre-existing M7 work-log wrapping (history, IP4). Profile
`r-package` slot: `devtools::document()` no diff; `pkgdown::check_pkgdown()`
"No problems found"; README.md in sync and untouched; `^tools$` .Rbuildignore
entry verified by tarball probe (0 `circumplex/tools` entries against 32
`circumplex/R/`); `devtools::check()` clean. NEWS: no entry owed — every
non-tracking file changed (`.github/`, `tools/`) is Rbuildignored, so nothing
reaches users.

**Independent review — three lenses + scorer.**
[S] prior-PR-comments: zero findings; PR-comment probe returned `[]`, so the
GitHub surface was correctly not walked. [S] blame-history: zero findings; it
established that pkgdown's old job-level concurrency block was inherited
r-lib scaffold (2024, pre-cairn), never a circumplex decision, so replacing it
reverses nothing deliberate. [O] diff-bug: six findings, scored below.

- **F1 (80, ACTIONED — fixed on branch).** pkgdown concurrency keyed on
  `github.ref` split a release deploy (`refs/tags/<tag>`) from a master-push
  deploy (`refs/heads/master`), letting two gh-pages deploys race where the old
  catch-all key serialized them; `workflow_dispatch` additionally gained
  cancel-in-progress against an in-flight deploy. Fixed: PRs keyed per ref with
  cancellation (AC3 preserved), all deploying events on one shared key with
  cancellation off, so they queue.
- **F2 (55, ACTIONED at user direction — fixed on branch).** The guard reads only `extra-packages:`, never
  `dependencies:`/`needs:`. Reproduced in a sandbox: restoring
  `dependencies: '"all"'` + `needs: website` returns brms and the Stan stack
  while the guard prints "in sync" and exits 0.
- **F3 (78, ACTIONED at user direction — fixed on branch).** `man/**` and `README.md` are pkgdown site inputs
  (`_pkgdown.yml:18` `reference:` over 74 Rd files; README.md is the home
  page), so ignoring them on pkgdown lets an Rd- or README-only push leave the
  published site stale.
- **F4 (68, logged).** `man/**` on R-CMD-check's push trigger means an Rd-only
  commit to master runs zero jobs; Rd is real check input. Scorer noted zero
  `man/**`-only commits in repo history.
- **F5 (25, logged).** The guard's policy list is a hardcoded three-path
  whitelist; a future fourth workflow would be unguarded silently.
- **F6 (40, logged).** Token normalization does not strip pak's `@ref` pin
  suffix, so `any::glmmTMB@1.1.9` fails spuriously (loudly, self-correcting).

Verification gap noted, not a defect: the guard step has only ever executed on
ubuntu (the PR matrix is a single ubuntu/release leg). Its first windows/macos
run happens on the merge push, covered by AC6's post-merge clause.

**Post-review fixes (F2, F3) — user directed the two sub-threshold findings fixed.**
F2: the guard now also asserts, per policy file, that the setup-r-dependencies
step carries `dependencies: '"hard"'` and carries NO `needs:` key, scoped to
that key's own `with:` mapping. Proven three ways — true state exits 0; the
sandbox sabotage (`dependencies: '"all"'` + `needs: website`) now exits 1 with
both messages, where before the fix it exited 0 reporting "in sync"; and a
job-level `jobs.<id>.needs` at a different indentation does NOT false-positive
(exits 0), confirming the block scoping.
F3: pkgdown's paths-ignore reduced to `cairn/**` alone, dropping `man/**` and
`README.md` because both are site inputs. The other two workflows keep the full
three-entry list, which is correct for them. AC4 is unaffected — a `cairn/**`
-only push still matches pkgdown's filter.
AC6 not re-run: the only files changed after its evidence was gathered are
`.github/workflows/pkgdown.yaml` and `tools/check-ci-deps.R`, both matched by
`.Rbuildignore` (`^\.github$`, `^tools$`), so the built package is unchanged.
F4, F5 and F6 remain logged and unactioned at the user's direction.
