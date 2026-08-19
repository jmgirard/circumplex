# M96: Say something when master goes red

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M95
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m96-master-red-alert`

## Goal

Open a GitHub issue automatically when a push run of `R-CMD-check.yaml` or
`test-coverage.yaml` on the default branch concludes failure.

## Scope

Surface tier: **internal** — the deliverable is GitHub Actions configuration
and its issue output; `.github/` ships in no built tarball and no external
consumer of the package reads it.

M93 parked this remedy in its Out with the promotion condition "a master red
outliving a review cycle unnoticed", which has not fired. Jeff overrode the
parking at this plan gate, as at M94's. It is a deferral rather than a
rejection on the merits, so no superseding decision entry is owed. M95 makes it
more load-bearing: with the coverage job off pull requests, its master push run
is the only place that environment reports at all.

**In:**
- A new workflow triggered by `workflow_run` on the completion of
  `R-CMD-check.yaml` and `test-coverage.yaml`, filtered to the default branch
  and the `push` event, that opens an issue when the conclusion is `failure`.
- Deduplication: before opening, search for an open issue carrying this
  workflow's marker label and naming the same workflow; comment on it rather
  than opening a second. A red that persists across pushes must not accumulate
  one issue per push.
- The issue body names the failing workflow, the run URL, the head SHA, and
  the conclusion — each read from the `workflow_run` payload, never composed.
- `permissions:` grants `issues: write` and nothing else.

**Out:**
- Alerting on `pkgdown.yaml` → not planned; a failed site build leaves the
  published site stale rather than the package broken, and M58's paths-ignore
  work already governs when it runs. Reopen on a stale published site outliving
  a review cycle.
- Alerting on pull-request runs → not planned; those are already visible to
  whoever opened the PR, and the review gate blocks a red merge.
- Closing the issue automatically when master goes green again → not planned in
  this milestone; a human closing it is the acknowledgement the alert exists to
  produce. Reopen if stale issues accumulate.
- GitHub-native branch protection → the standing ROADMAP candidate row, on its
  own promotion condition.

## Acceptance criteria

- [ ] AC1: the workflow file exists, its `on: workflow_run` block names exactly
      `R-CMD-check.yaml` and `test-coverage.yaml`, and its job carries an `if:`
      requiring `conclusion == 'failure'`, `event == 'push'`, and the default
      branch — read by parsing the file's `on:` and `if:` values.
- [ ] AC2: `permissions:` at the workflow or job level grants `issues: write`
      and no other write scope, read by parsing the `permissions:` mapping.
- [ ] AC3: every value substituted into the issue body resolves to a
      `workflow_run` payload path — enumerated by scanning both the body
      template and the alert's script body for every interpolation site
      (`${{ }}` expressions, `context.payload.*` reads, shell `$VAR`
      substitutions) and checking each. The scan must find at least the four
      fields the body names (workflow, run URL, head SHA, conclusion), so an
      empty enumeration fails rather than passing vacuously.
- [ ] AC4: a dry run against a synthetic failure payload produces exactly one
      issue on an empty issue list, exactly one comment when a matching open
      issue already exists, and a create-label call before the dedupe search on
      the label-absent path — demonstrated by running the alert's script body
      against all three fixtures locally, with the GitHub calls stubbed and the
      stub recording every call made.
- [ ] AC5: the alert creates the marker label if absent before searching for
      it, read by parsing the workflow for a create-label step ordered before
      the search step. (A search on a nonexistent label returns empty and would
      silently defeat the dedupe.)
- [ ] AC6: the new workflow file contains no `setup-r`,
      `setup-r-dependencies`, `extra-packages`, or `install.packages` step,
      read by grepping the file for those keys — so it needs no allowlist
      entry. Separately, as an unchanged-regression guard only:
      `Rscript tools/check-ci-deps.R` exits clean. Its green is not evidence
      about the new file — it loops `names(policy)` (tools/check-ci-deps.R:156)
      over a hand-list of three workflows and cannot see a fourth.

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T3
- AC5 → T4
- AC6 → T5

## Tasks

- [ ] T1: Author `.github/workflows/master-red-alert.yaml` — the
      `workflow_run` trigger over the two workflows, the failure/push/default-
      branch `if:`, and `permissions: issues: write`.
- [ ] T2: Write the issue body template, interpolating only `workflow_run`
      payload fields; run AC3's scan over both the template and the script body
      and check every site it returns.
- [ ] T3: Extract the open-or-comment logic into a script body the milestone
      can exercise locally, and run it against three fixtures — no existing
      issue, a matching open issue, and an absent marker label — with the
      GitHub calls stubbed and the stub recording every call.
- [ ] T4: Add the create-label-if-absent step ahead of the dedupe search.
- [ ] T5: Grep the new file for the four dependency-install keys; run
      `tools/check-ci-deps.R` as a regression guard and the profile's verify
      slot.

## Work log

- 2026-08-18: created by /milestone-plan.
- 2026-08-18: plan gate chose a `workflow_run`-triggered alert over extending each watched workflow with its own failure step, because a single alert workflow keeps the issue logic in one place and cannot itself redden the run it watches; falsified by `workflow_run` proving unable to read a conclusion the per-workflow step could.
- 2026-08-18: plan gate chose comment-on-existing over open-per-failure, because a persistent red would otherwise open an issue on every push; falsified by a second distinct failure being silently absorbed into an open issue for an unrelated one.
- 2026-08-18: M93's parking of this remedy overridden by Jeff at this plan gate; its stated promotion condition (a master red outliving a review cycle unnoticed) had not fired. Logged per the user-override rule; the M93 Out entry is a deferral, not a rejection on the merits, so no superseding decision entry is owed.
- 2026-08-18: reduced criteria audit ([O], fresh context) returned four findings, all fixed before the criteria were written: AC3's `${{ }}` scan was a proxy that would pass vacuously if the body is composed inside the script body, so the domain now spans both and must find at least four fields; AC5 bound acceptance to the live repository label set, so its first disjunct was dropped for a parse of step order; AC6's reliance on `check-ci-deps.R` was vacuous because that script loops a hand-list of three workflows (tools/check-ci-deps.R:156) and cannot see a fourth, so the claim about the new file now rests on its own grep and the script is demoted to a regression guard. AC1, AC2 and AC4 passed both questions.
- 2026-08-18: widening `tools/check-ci-deps.R` to glob `.github/workflows/` considered and left out — it is the checker-regress shape M93's plan gate already declined once for that script, and AC6's per-file grep gets this milestone what it needs without widening the checker's promise.
- 2026-08-18: implement gate chose bash + `gh` for the alert body over `actions/github-script`, because no Node is installed on the maintainer machine and AC4 requires exercising the body locally; marker label `master-red`; the dry-run harness lands as a standalone `tools/` script, matching `tools/check-ci-deps.R`, not inside the test suite.

## Decisions

## Review
