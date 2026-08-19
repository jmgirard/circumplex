# M95: Stop running the suite twice on every pull request

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m95-ci-trigger-economy · https://github.com/jmgirard/circumplex/pull/124

## Goal

Retire the dead `pr-commands.yaml` workflow and take `test-coverage.yaml` off
the pull-request trigger, moving its master-side result under the review gate's
watch.

## Scope

Surface tier: **internal** — the deliverables are GitHub Actions configuration
and `cairn/PROFILE.md`; `.github/` ships in no built tarball and no external
consumer of the package reads either.

**In:**
- Delete `.github/workflows/pr-commands.yaml`. Its `/document` and `/style`
  comment triggers have never matched: all 9 runs since 2025-07-21 concluded
  `skipped`, while each fired a runner on an unrelated issue comment. It is
  also the one workflow outside the allowlist discipline — its
  `remotes::install_deps(dependencies = TRUE)` would install brms and the Stan
  stack that D-015 keeps out of CI, and `styler` is in no dependency list.
- `.github/workflows/test-coverage.yaml`: drop the `pull_request` trigger,
  keeping the `push` block's `branches` and `paths-ignore` values byte-
  unchanged. The now-dead `fail_ci_if_error` event conditional at line 75
  simplifies to `true`.
- `cairn/PROFILE.md` consistency-gate: a `test-coverage.yaml` master-run watch
  bullet parallel in form to the `R-CMD-check.yaml` one M93 shipped, so a red
  coverage run on master fails the next review gate rather than sitting unseen.
- `cairn/PROFILE.md` test-doctrine: correct the stranded claim that Codecov
  annotates the PR. It is doubly wrong after this change and already half wrong
  before it — `codecov.yml` sets `comment: false`.

**Out:**
- An automated alert on a red master run → M96 (planned now, depends on M95).
- GitHub-native branch protection on master → stays the standing ROADMAP
  candidate row on its own promotion condition; declined here at the plan gate
  as a repository-settings change needing Jeff's own authorization, independent
  of any file in this diff.
- Any change to a test, including the M65-D3 replicate count that is 28% of
  suite runtime → not planned; the measured concentration is recorded in the
  work log for whoever opens that question.
- Trimming the dependency allowlists → declined outright by D-029 on measured
  grounds; untouched here.

## Acceptance criteria

- [x] AC1: `.github/workflows/pr-commands.yaml` is absent, and a repo-wide
      `grep -rn "pr-commands" . --exclude-dir=.git` returns matches only inside
      `cairn/` records this milestone authored.
- [x] AC2: `.github/workflows/test-coverage.yaml`'s `on:` mapping has exactly
      one key, `push` — read by parsing the `on:` block's keys, not by grepping
      the file, since `pull_request` also occurs at line 75 as a value.
- [x] AC3: that file's `push` block's `branches` and `paths-ignore` values are
      byte-unchanged against the merge base (`git diff` shows no line inside
      the block), and `fail_ci_if_error` reads `true` with no event conditional.
- [x] AC4: `cairn/PROFILE.md`'s consistency-gate slot contains a
      `test-coverage.yaml` watch bullet naming `--workflow=test-coverage.yaml
      --branch=<default> --event=push`, parallel in form to the existing
      `R-CMD-check.yaml` bullet.
- [x] AC5: `grep -rn "pull_request\|the PR\|annotates" cairn/PROFILE.md
      .github/workflows/test-coverage.yaml codecov.yml` returns no text stating
      that the coverage workflow runs on, or annotates, pull requests. (Scope is
      these three files; the criterion claims nothing about other surfaces.)
- [x] AC6: `Rscript tools/check-ci-deps.R` exits clean, and
      `Rscript -e 'devtools::check(args = "--no-manual")'` is clean (0 errors,
      0 warnings; NOTEs justified).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T2
- AC4 → T3
- AC5 → T3, T4
- AC6 → T5

## Tasks

- [x] T1: Delete `.github/workflows/pr-commands.yaml`; confirm no reference
      survives (`tools/check-ci-deps.R` lists only the other three workflows).
- [x] T2: Edit `.github/workflows/test-coverage.yaml` — remove the
      `pull_request:` trigger block, simplify `fail_ci_if_error` to `true`, and
      update the workflow's header comment to state what now triggers it.
- [x] T3: Edit `cairn/PROFILE.md` — add the consistency-gate watch bullet;
      correct the test-doctrine Codecov-annotates-the-PR sentence and the
      stock-`usethis`-pair sentence that no longer describes this repo.
- [x] T4: Run AC5's grep over the three named files; fix any surviving claim.
- [x] T5: Run `tools/check-ci-deps.R` and the full `devtools::check()`.

## Work log

- 2026-08-18: created by /milestone-plan.
- 2026-08-18: plan gate chose restricting the coverage workflow to `push` plus a review-gate master watch over leaving it on pull requests, because `codecov.yml` already sets `comment: false` and both statuses `informational: true`, so the PR-side loss is the covr test environment rather than any reporting; falsified by a defect reaching master that the covr environment would have caught pre-merge and the watch did not surface.
- 2026-08-18: plan gate chose a static parse of the `on:` mapping over a live `gh run list` demonstration on this milestone's own PR, because the reduced criteria audit found the live check bound acceptance to external CI state on an internal-tier milestone; falsified by a PR-triggered coverage run appearing despite a clean static parse.
- 2026-08-18: reduced criteria audit ([O], fresh context) returned three findings — a live-CI universal in the PR-runs-no-coverage criterion, a merge-blocking dependence on a green master run in the profile-watch criterion, and an exemption registry over a `coverage|covr|codecov` keyword sweep the reader measured at 1381 hits, almost all the unrelated statistical sense of "coverage". All three had one clear answer and were fixed before the criteria were written; the same reader caught that `grep -c "pull_request"` cannot return 0 because line 75 carries the string as a value, which became AC2's parse-the-mapping wording and T2's `fail_ci_if_error` simplification.
- 2026-08-18: measured on 2026-08-18 from runs 32184165512 (coverage, pull_request, 27 min) and 32184165497 (R-CMD-check, pull_request, escalated, 38 min); the last recorded figures were 13.0 and 14.5 min on 2026-07-25 (D-029), so the coverage job has roughly doubled as the FIML suite landed. Suite runtime measured locally the same day at 580 s under `NOT_CRAN=true`, 48% of it `test-axes-fiml.R` and 28% the single M65-D3 fixture-staleness test — recorded for the parked replicate-count question, not acted on here.
- 2026-08-18: T1 — deleted `.github/workflows/pr-commands.yaml`; `grep -rn "pr-commands"` over the tree returns only M95's own record, and `tools/check-ci-deps.R`'s `policy` list never named it, so no checker loses a target. No R code changed, so the profile's `devtools::test()` is deferred to T5 where AC6's full `check()` covers every change at once.
- 2026-08-18: T2 — removed the `pull_request` trigger from `test-coverage.yaml`, simplified `fail_ci_if_error` to a bare `true`, and gave the file a header stating what now triggers it and what the PR side does and does not lose. AC2 verified by parsing the `on:` mapping with `yaml::read_yaml()` — one key, `push`; the same parse reads `fail_ci_if_error` as logical `TRUE`. AC3's byte-unchanged half verified by extracting the `push` block from `git show master:` and from the working tree and hashing both: 51dce08235cd5670b78bae7a367c6857 on each side.
- 2026-08-18: T3 — `cairn/PROFILE.md` gains a master coverage watch beside M93's matrix watch (AC4), and the test-doctrine slot's stranded claims are corrected: it described the CI as the stock usethis pair, which neither workflow has been since M93, and said Codecov annotates the PR, which `codecov.yml`'s `comment: false` already made wrong before this milestone. File at 117 lines against the 120 cap.
- 2026-08-18: T4 — AC5's sweep over the three named files returns two hits, both in the `test-coverage.yaml` header, both stating what the PR side lost rather than claiming it still runs there; `annotates` no longer occurs in any of the three. `cairn_validate` all green.
- 2026-08-18: T5 — `tools/check-ci-deps.R` exits 0 (allowlists in sync with the 14 DESCRIPTION Suggests). `devtools::check(args = "--no-manual")` is Status: OK — 0 errors, 0 warnings, 0 notes over the whole log, testthat running 535s and vignettes re-building in 38s, tree clean afterward. `--no-manual` is CLAUDE.md's documented check command, so the PDF-manual step is deliberately out rather than silently skipped.
- 2026-08-18: all tasks done, status to review. AC boxes left unticked: review ticks them against its own fresh evidence.
- 2026-08-18: PR #124 CI green — matrix 37s, pkgdown 4m34s, ubuntu-latest release 27m19s (run 32196729867); `gh run list --workflow=test-coverage.yaml --event=pull_request --branch=m95-ci-trigger-economy` returns nothing, so the trigger change is confirmed live as well as by AC2's parse.
- 2026-08-18: payoff restated against that measurement — the plain single-ubuntu check job is 27m19s and the coverage job was 27m, and the two ran in PARALLEL, so this milestone saves ~27 min of runner time per PR and does NOT shorten the wall-clock wait. The plan's framing of the coverage job as "the slower of the two" holds only against a three-platform escalation (38 min), not against a plain PR.
- 2026-08-18: the first CI watch used `timeout 1800 gh pr checks --watch`; macOS ships no `timeout`, so the command failed and a trailing `echo` reported success — caught by re-reading the authoritative `gh run list` state. Candidate LESSONS line for review's hygiene pass.
- 2026-08-18: review in progress — AC1-AC5 verified with fresh evidence and ticked; consistency gate green (cairn_validate, document() no-diff, check_pkgdown, both master watches). Two of three review lenses reported, both with no findings. AC6 and the [O] diff-bug lens still outstanding at this checkpoint.
- 2026-08-18: amendment return: AC5 — "`grep -rn \"pull_request\|the PR\|annotates\" cairn/PROFILE.md .github/workflows/test-coverage.yaml codecov.yml` returns no text stating that the coverage workflow runs on, or annotates, pull requests. (Scope is these three files; the criterion claims nothing about other surfaces.)" — the original claimed the three files were the complete set carrying such a claim, which `.github/CONTRIBUTING.md:25` falsifies; narrowed rather than widened per the widening test, at the maintainer's selection. AC5 unticked pending re-verification.
- 2026-08-18: the amended wording went to a fresh-context [O] reader in reduced mode before being written. It passed proportionality and found the promise itself bounded, but caught a false justification clause in the proposed parenthetical — `codecov.yml` receives no edit on this branch and `.github/CONTRIBUTING.md` does — so the clause was replaced by the reader's own repair, a plain scope disclaimer. First and only defect return on this milestone; the thrash rule does not fire.
- 2026-08-18: fix-now work from the review gate — F3/F6 (the header called covr "the slower of the two" then cited 27 vs 38, and claimed nothing was lost six lines before saying what was): rewritten to state the two ran in PARALLEL, so the saving is runner minutes not wall clock, and to narrow the no-loss claim to REPORTING. F2/F7: the watch now reads the newest run concluding success or failure, not the newest completed, since `cancelled` is completed without being a verdict, and an absent run is no verdict rather than a failure. F1: the bullet names `/hotfix` as the way to clear a red one, so it cannot deadlock against the never-implement-on-master rule. F8: it says the watch reads one milestone LATE. F4: the test-doctrine line now separates the coverage NUMBER (never a gate) from the JOB's pass/fail (which does gate). F9: the M92 citation is softened in both places to the instrumentation, since M92's own remedy made the vignette guards skip.
- 2026-08-18: F13 rescoped after checking the file — `.github/CONTRIBUTING.md` never referenced the deleted workflow's `/style` or `/document` commands, so its `styler` advice is not stranded by this diff and the "only in-repo mechanism" premise was about the automation rather than about what the guide promises. No edit made; both the Travis/AppVeyor line and the styler advice go to a candidate row as pre-existing. F14 (plaintext Codecov token, out of diff since M12) likewise to a candidate row, rotation being Jeff's to do.
- 2026-08-18: `cairn/PROFILE.md` hit its 120-line cap during these fixes and was compressed within the two slots this milestone wrote, never by trimming pre-existing content; now 119.
- 2026-08-18: re-verified after the fixes — AC5 (amended) one hit, stating what the PR side lost; AC2 one key `push`; AC3 push block still 51dce08235cd5670b78bae7a367c6857 and `fail_ci_if_error: true`; AC4 bullet still names its command.
- 2026-08-18: checker-regress shape considered and not fired — the consistency-gate watch reads GitHub run conclusions, which are external state, not the repo-internal artifacts the shape is defined over.

## Decisions

## Review

Fresh evidence, 2026-08-18, branch m95-ci-trigger-economy at 37348919 against origin/master.

- **AC1** ✔ `.github/workflows/pr-commands.yaml` absent from the tree; `grep -rn "pr-commands" . --exclude-dir=.git` returns hits in one file only, `cairn/milestones/M95-ci-trigger-economy.md` — this milestone's own record.
- **AC2** ✔ `yaml::read_yaml()` on the workflow: the trigger key parses as the logical `TRUE` (YAML 1.1 reads bare `on` as boolean), and its mapping has exactly one key, `push` (n = 1). A first attempt indexed `d[["on"]]`, got NULL and reported "n = 0" — vacuous, and re-run before it was recorded.
- **AC3** ✔ The `push` block extracted from `git show origin/master:` and from the working tree hashes to 51dce08235cd5670b78bae7a367c6857 on both sides. `fail_ci_if_error: true` at line 90; `github.event_name` occurs 0 times in the file.
- **AC4** ✔ `cairn/PROFILE.md:36` carries the Master coverage watch bullet naming `gh run list --workflow=test-coverage.yaml --branch=<default> --event=push`, parallel in form to M93's matrix bullet at line 32. The command run as written returns `status=completed conclusion=success` for run 32189244160.
- **AC5** ✔ The sweep over the three named files returns two hits, both in the `test-coverage.yaml` header (lines 9 and 14), both stating what the PR side lost; neither claims the workflow runs on or annotates pull requests. `annotates` occurs in none of the three.

Post-amendment re-verification, 2026-08-18:

- **AC5 (amended)** ✔ The sweep now returns one hit, `.github/workflows/test-coverage.yaml:16`, which states what the PR side lost; it does not claim the workflow runs on or annotates pull requests. `annotates` occurs in none of the three files.
- **AC6** ✔ `Rscript tools/check-ci-deps.R` exits 0 (allowlists in sync with the 14 DESCRIPTION Suggests). `devtools::check(args = "--no-manual")` Status: OK — 0 errors, 0 warnings, 0 notes across the whole log, testthat 10m/11m, duration 12m34s, working tree clean at completion. The four files edited after this check began are all `.Rbuildignore`'d (`^\.github$`, `^cairn$`), so none reaches the built tarball.
- **AC2 / AC3 / AC4** re-checked after the header and PROFILE rewrites: `on:` still one key `push`; the push block still hashes 51dce08235cd5670b78bae7a367c6857 against origin/master with `fail_ci_if_error: true`; the watch bullet still names its `gh run list` command.

### Consistency gate

- `cairn_validate` — all checks passed (47 advisory work-log-format warnings, pre-existing across M7 and others, never gate failures).
- `devtools::document()` — no diff, `git status` clean afterward; 0 lines matching `resolve link` at `cli.width = 500`.
- `pkgdown::check_pkgdown()` — no problems found.
- README.md in sync with README.Rmd; neither is touched by the diff.
- NEWS entry — not owed: the diff touches no `R/`, `src/`, `man/`, `NAMESPACE` or `DESCRIPTION` surface.
- `.Rbuildignore` — no top-level files added.
- Master matrix watch (M93) — latest master push run of `R-CMD-check.yaml` concluded success (run 32189244110).
- Master coverage watch (M95's own, first exercise) — latest master push run of `test-coverage.yaml` concluded success (run 32189244160).

### Independent review

Three fresh-context lenses, none having seen the implementation.

- **[S] prior-PR-comments** — no prior-review evidence. No archived `## Review` finding across the CI milestones bears on these files, and the existence probe `gh api .../pulls/comments?per_page=1` returned `[]`, so the per-PR thread walk was skipped per the probe gate. Clean no-op, as the standing M33 lesson predicts for this repo.
- **[S] blame-history** — no findings. Established that the `pull_request` trigger and the `fail_ci_if_error` conditional both date to commit 302cf928 (2024-10-27, pre-cairn), not to any deliberate cairn-era decision; that `paths-ignore` on that trigger was M51's work, so removing the trigger retires part of M51's addition while continuing M51's own CI-economy purpose; that `pr-commands.yaml` was never deliberately retained or restored; that M93's PR-blocking matrix gate is untouched and never depended on the coverage workflow; and that the removed "Codecov annotates the PR" sentence was already wrong when written in 0b417270, since `codecov.yml` has carried `comment: false` since before cairn. It independently confirmed the M59 and M92 citations in the new comment and PROFILE prose against those milestones' archives.

- **[O] diff-bug** — 14 findings, ranked. It verified AC1, AC2, AC3 and AC6 independently, confirmed master carries no branch protection (so removing the PR trigger leaves no required check hanging), and found nothing against D-015, D-029, the statistical invariants, or any IP/GP.

Triage (every finding logged; verified against the artifacts, not against the reviewer's account of them):

| # | Finding | Disposition |
|---|---|---|
| F1 | The coverage gate blocks the only fix for the condition it detects — a red master run fails the next review gate, and CLAUDE.md forbids implementing on the default branch | fix now: name the `/hotfix` escape in the bullet |
| F2 | A *cancelled* run is `completed` and not `success`, and this repo cancels master coverage runs in practice | fix now — **verified**: run 32187677266 concluded `cancelled` 2026-08-18T21:25:56Z, the second-newest master coverage run, under `cancel-in-progress: true` against a ~26 min job |
| F3 | The header calls covr "the slower of the two (27 min against the check job's 38 min)" — 27 < 38, so its own numbers say the opposite | fix now: my error |
| F4 | `PROFILE.md` says "covr is a diagnostic, never a gate" twenty lines from a bullet making a red covr run a gate failure | fix now: my error, distinguish percentage from conclusion |
| F5 | The plan-gate audit's merge-blocking finding "was not actually fixed" | reject: conflates criterion with deliverable — the criterion was fixed, the deliverable's gate is what the plan gate chose; substance rides F1/F2 |
| F6 | "Nothing was lost on the PR side by stopping" is contradicted six lines later | fix now: my error, narrow to *reporting* |
| F7 | "a red or **absent** run is a gate failure" — `paths-ignore` means tracking-only pushes create no run, and runs age out at ~90 days | fix now: fold into F2's wording |
| F8 | "this is the only gate its environment reaches" drops the "next" the workflow header carries — the run read at review predates the branch under review | fix now: my error |
| F9 | The M92 signal is largely spent — its remedy made the vignette guards *skip* under covr, so that axis now skips rather than catches | fix now: soften; M59's optimizer perturbation still stands |
| F10 | The literal `true` is a trap for M96 adding a trigger here | reject: M96 adds a separate workflow file watching via `workflow_run`; it adds no trigger to this file |
| F11 | AC2's recorded method returns NULL by key name (`on` parses as boolean) | reject as already handled: self-caught during review and recorded in AC2's evidence line above |
| F12 | AC5's "the complete set of files carrying a claim" is false — `.github/CONTRIBUTING.md` tells contributors to check build status, and README carries a `test-coverage` badge | **amendment return** — see below |
| F13 | `.github/CONTRIBUTING.md` recommends `styler`, whose only in-repo mechanism this diff deletes; line 25 still names Travis and AppVeyor | fix now (styler half, newly stranded by this diff); Travis/AppVeyor half is pre-existing → candidate row |
| F14 | `codecov.yml:36` commits a Codecov upload token in plaintext while the workflow also passes `secrets.CODECOV_TOKEN` | out of diff, pre-existing since M12 (7480a67a) — maintainer's call at the gate; recommend rotate + drop |

**Amendment return on AC5.** The criterion asserts the three files it greps are "the complete set of files carrying a claim about when the coverage workflow runs or what it annotates". That is an author-recall enumeration claiming completeness, and it is false: `.github/CONTRIBUTING.md:25` tells contributors to "Look at the ... build status before and after making changes". The finding falsifies the criterion outside its own procedure's domain, and the only repair available to it widens a recalled enumeration — the widening test. The repair is therefore to narrow the promise to what the procedure actually swept, not to lengthen the file list.

