# M95: Stop running the suite twice on every pull request

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m95-ci-trigger-economy

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

- [ ] AC1: `.github/workflows/pr-commands.yaml` is absent, and a repo-wide
      `grep -rn "pr-commands" . --exclude-dir=.git` returns matches only inside
      `cairn/` records this milestone authored.
- [ ] AC2: `.github/workflows/test-coverage.yaml`'s `on:` mapping has exactly
      one key, `push` — read by parsing the `on:` block's keys, not by grepping
      the file, since `pull_request` also occurs at line 75 as a value.
- [ ] AC3: that file's `push` block's `branches` and `paths-ignore` values are
      byte-unchanged against the merge base (`git diff` shows no line inside
      the block), and `fail_ci_if_error` reads `true` with no event conditional.
- [ ] AC4: `cairn/PROFILE.md`'s consistency-gate slot contains a
      `test-coverage.yaml` watch bullet naming `--workflow=test-coverage.yaml
      --branch=<default> --event=push`, parallel in form to the existing
      `R-CMD-check.yaml` bullet.
- [ ] AC5: `grep -rn "pull_request\|the PR\|annotates" cairn/PROFILE.md
      .github/workflows/test-coverage.yaml codecov.yml` — the complete set of
      files carrying a claim about when the coverage workflow runs or what it
      annotates — returns no text stating that it runs on, or annotates, pull
      requests.
- [ ] AC6: `Rscript tools/check-ci-deps.R` exits clean, and
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
- 2026-08-18: checker-regress shape considered and not fired — the consistency-gate watch reads GitHub run conclusions, which are external state, not the repo-internal artifacts the shape is defined over.

## Decisions

## Review
