# M96: Say something when master goes red

- **Status:** review
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

- [x] T1: Author `.github/workflows/master-red-alert.yaml` — the
      `workflow_run` trigger over the two workflows, the failure/push/default-
      branch `if:`, and `permissions: issues: write`.
- [x] T2: Write the issue body template, interpolating only `workflow_run`
      payload fields; run AC3's scan over both the template and the script body
      and check every site it returns.
- [x] T3: Extract the open-or-comment logic into a script body the milestone
      can exercise locally, and run it against three fixtures — no existing
      issue, a matching open issue, and an absent marker label — with the
      GitHub calls stubbed and the stub recording every call.
- [x] T4: Add the create-label-if-absent step ahead of the dedupe search.
- [x] T5: Grep the new file for the four dependency-install keys; run
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
- 2026-08-18: T1 — `.github/workflows/master-red-alert.yaml` authored: `workflow_run` over the two watched workflows, an `if:` requiring failure + push + default branch, `permissions: issues: write` (plus `contents: read` for checkout, no second write scope). `tools/check-master-red-alert.R` parses the file for AC1/AC2; each of its three assertions confirmed to fire against a mutated copy (dropped workflow, dropped push condition, added `contents: write`).
- 2026-08-18: T2 — issue body written as a table over the four payload fields, all carried in through the step `env:`. The AC3 scan lands in `tools/check-master-red-alert.R`: it enumerates every `${{ }}` site, every `context.payload.*` read, and every shell `$VAR` in the body heredoc, resolves each through the env map and local assignments, and requires the four named fields; it reports 4 fields and nothing else. Confirmed to fire against three mutations (body citing `GH_REPO`, body dropping the head-SHA row, run URL recomposed from `github.server_url`).
- 2026-08-18: minor amendment — T4 taken before T3 (task reorder only, no scope or criterion change): the dry-run harness T3 builds must exercise a finished shell body, including the label path T4 adds, so the whole body (label probe/create, dedupe search, create-or-comment) landed at T4 and T3 exercises it.
- 2026-08-18: T4 — the shell body now probes `gh label list` and creates `master-red` when absent, ahead of the `gh issue list` dedupe search; dedupe is per watched workflow via the title. AC5's ordering check added to `tools/check-master-red-alert.R`; confirmed to fire against a copy with the label block moved below the search.
- 2026-08-18: T3 — `tools/master-red-alert-dryrun.R` lifts the shell body out of the workflow file (no second copy to drift), puts a recording `gh` stub on PATH with `jq` left real, and runs three fixtures: label present + no issue -> one create; label present + matching issue -> one comment on #42, no create; label absent -> label create before the search, then one create. All three pass. Each is confirmed discriminating by a mutant: always-create, a dedupe filter inverted to `.title != $t`, and a deleted `gh label create` each fail in the fixture that should catch them.
- 2026-08-18: T5 — AC6's grep for `setup-r`, `setup-r-dependencies`, `extra-packages` and `install.packages` added to `tools/check-master-red-alert.R`; the alert file carries none of the four, and the check fires against a copy with a `setup-r` step spliced in. `tools/check-ci-deps.R` green as an unchanged-regression guard (14 Suggests in sync), which says nothing about the new file. Verify slot clean: `devtools::test()` [ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ] (the warnings are the suite's pre-existing lavaan/optimizer notes, untouched by this branch). Both audit scripts also hardened to locate the alert step by its `run:` body rather than by position, found while mutating.
- 2026-08-18: all tasks complete; `devtools::check(args = "--no-manual")` Status OK (0 errors, 0 warnings, 0 notes) — the branch touches only `.github/` and `tools/`, both `.Rbuildignore`d, so nothing it adds enters the built package. Status -> review.

## Decisions

## Review
