# M96: Say something when master goes red

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M95
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m96-master-red-alert` / https://github.com/jmgirard/circumplex/pull/125

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

- [x] AC1: the workflow file exists, its `on: workflow_run` block names exactly
      `R-CMD-check.yaml` and `test-coverage.yaml`, and its job carries an `if:`
      requiring `conclusion == 'failure'`, `event == 'push'`, and the default
      branch — read by parsing the file's `on:` and `if:` values.
- [x] AC2: `permissions:` at the workflow or job level grants `issues: write`
      and no other write scope, read by parsing the `permissions:` mapping.
- [ ] AC3: every value substituted into the issue body resolves to a
      `workflow_run` payload path — enumerated by scanning both the body
      template and the alert's script body for every interpolation site
      (`${{ }}` expressions, `context.payload.*` reads, shell `$VAR`
      substitutions) and checking each. The scan must find at least the four
      fields the body names (workflow, run URL, head SHA, conclusion), so an
      empty enumeration fails rather than passing vacuously.
- [x] AC4: a dry run against a synthetic failure payload produces exactly one
      issue on an empty issue list, exactly one comment when a matching open
      issue already exists, and a create-label call before the dedupe search on
      the label-absent path — demonstrated by running the alert's script body
      against all three fixtures locally, with the GitHub calls stubbed and the
      stub recording every call made.
- [x] AC5: the alert creates the marker label if absent before searching for
      it, read by parsing the workflow for a create-label step ordered before
      the search step. (A search on a nonexistent label returns empty and would
      silently defeat the dedupe.)
- [x] AC6: the new workflow file contains no `setup-r`,
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
- 2026-08-18: defect return 1 (review gate) — AC3 fails inside the domain of the procedure it names: `tools/check-master-red-alert.R` scans only the `BODY=` heredoc, not "the alert's script body", and its site regex sees neither `$( )` nor backticks, so a composed title (`$(hostname)`) and a composed body row both pass the audit. Twelve further findings logged in Review ([O] diff-bug F1-F13, [S] blame-history F1); F2, F3, F4, F5, F7, F9, F11, F12 join the return, F6/blame-F1 go to the next question gate, F8 rejected as written. Status review -> in-progress.

## Review

_Evidence gathered 2026-08-18 on branch `m96-master-red-alert`, PR #125._

- AC1 — PASS. `yaml::read_yaml()` on the workflow: the `on:` block names `workflow_run` and nothing else, its `workflows:` list is exactly `R-CMD-check.yaml`, `test-coverage.yaml` (types: `completed`), and the single job's `if:` parses to `github.event.workflow_run.conclusion == 'failure' && github.event.workflow_run.event == 'push' && github.event.workflow_run.head_branch == github.event.repository.default_branch`. `tools/check-master-red-alert.R` asserts all four and is confirmed to fire on a dropped workflow and on a dropped push condition.
- AC2 — PASS. The parsed `permissions:` mapping is `contents=read | issues=write`, with no job-level override; `issues` is the only write scope. The audit's assertion fires on a copy where `contents` is raised to `write`.
- AC3 — **FAIL (superseded; the PASS line below was written before the fan-out reported F1/F10).** The scan's domain is the `BODY=` heredoc only, not "the alert's script body" the criterion names, and its site regex matches `$VAR`/`${VAR}` only. Re-verified here on copies of the workflow: a title composed as `TITLE="master is red: $(hostname)"`, and a body row carrying `$(gh api /user --jq .login)`, both leave the audit exiting 0 and still printing "interpolates 4 workflow_run payload field(s) and nothing else". The criterion fails inside the domain of the procedure it names.
- AC3 — superseded PASS line, retained: The scan enumerates every `${{ }}` site (all six live in the step's `env:` mapping — no stray), every `context.payload.*` read (none), and every shell `$VAR` in the `BODY=` heredoc, resolving each through the env map and local assignments. It reports 4 payload fields and nothing else: `conclusion`, `head_sha`, `html_url`, `name` — meeting the four-field floor, so an empty enumeration could not pass. Confirmed to fire on a body citing `GH_REPO`, on a dropped head-SHA row (3 fields, refused), and on a run URL recomposed from `github.server_url`.
- AC4 — PASS. `tools/master-red-alert-dryrun.R` lifts the shell body out of the YAML, stubs `gh` (recording every call, `jq` left real) and runs three fixtures: label present + no open issue -> `label list -> issue list -> issue create` (exactly one create, no comment); label present + matching open issue -> `label list -> issue list -> issue comment` on #42 (exactly one comment, no create); label absent -> `label list -> label create -> issue list -> issue create`, the create ahead of the search. Each fixture is confirmed discriminating by a mutant that only it catches (always-create; dedupe filter inverted to `.title != $t`; `gh label create` deleted).
- AC5 — PASS. Parsing the workflow puts `gh label list` at line 76 and `gh label create` at line 77, both ahead of the `gh issue list` dedupe search at line 85. The audit's ordering assertion fires on a copy with the label block relocated below the search.
- AC6 — PASS. `grep -nE 'setup-r|extra-packages|install\.packages'` over the new workflow returns nothing; the audit's own grep over all four keys agrees, and fires on a copy with a `setup-r` step spliced in. Separately, as the unchanged-regression guard only: `Rscript tools/check-ci-deps.R` exits clean (14 Suggests in sync) — which, as the criterion states, is not evidence about the new file.

### Consistency gate (2026-08-18)

- `cairn_validate` — exit 0, all checks PASS; 47 advisory `work-log format` warnings, all pre-existing M7 wrapped lines, none from this branch.
- `cairn_impact` — skipped; no DESIGN.md principle changed.
- `devtools::document()` — no diff (working tree clean apart from this milestone file), zero `resolve link` warnings at `cli.width = 500`.
- `pkgdown::check_pkgdown()` — "No problems found."
- `devtools::check(args = "--no-manual")` — Status OK, 0 errors / 0 warnings / 0 notes, run at 467bc71e; `git diff 467bc71e..HEAD` outside `cairn/` is empty, so the checked package content is the content under review.
- README.Rmd/README.md — untouched by this branch.
- NEWS.md — no entry owed: the surface tier is internal and nothing user-visible changed.
- `.Rbuildignore` — no new top-level files; `^\.github$` and `^tools$` already present.
- Master matrix watch — newest push run of `R-CMD-check.yaml` on master is 32202243374 (2026-08-19T00:42:20Z), conclusion success.
- Master coverage watch — newest push-run verdict of `test-coverage.yaml` on master is 32202243432 (2026-08-19T00:42:20Z), conclusion success; the intervening 32187677266 is `cancelled`, which is not a verdict.
- Gate-command note: the first invocation of the profile's `gh run list --workflow=R-CMD-check.yaml --branch=master --event=push` returned three 2026-08-07 `failure` runs, the newest of them ten days stale. The identical command re-run, the same query without `--event`, and the raw API (`actions/workflows/3740495/runs?branch=master&event=push`) all agree on the green newest run above. Recorded because the stale answer was the red one: a gate that reads this command once could fail a milestone on a phantom red, or — with the staleness in the other direction — pass one on a phantom green.

### Independent review (fresh-context fan-out)

Reviewers: [O] diff-bug (13 findings), [S] blame-history (1 finding), [S] prior-PR-comments (no findings; its probe `gh api repos/{owner}/{repo}/pulls/comments?per_page=1` returned empty, so the repo carries no inline review threads at all — the same no-op this lens has recorded since M33).

Every finding below is logged with its disposition; the return-floor finding is F10.

- **F10 (floor return) — AC3's scan covers the body heredoc but not the script body the criterion names.** `TITLE` reaches `gh issue create --title` and is the dedupe key, yet sits outside the scanned region. Re-verified: `TITLE="master is red: $(hostname)"` passes the audit. Fails AC3 inside its named domain -> status returns to `in-progress`.
- **F1 — the same scan cannot see command substitution.** `vars_in()` matches `$VAR`/`${VAR}` only, so `$( )`, backticks and `${VAR:-default}` are invisible; re-verified with a `$(gh api /user --jq .login)` body row. Fix with F10.
- **F2 — the permissions check reads the wrong mapping.** `perms <- doc$permissions; if (is.null(perms)) perms <- job$permissions` inverts GitHub's precedence, where a job-level block REPLACES the workflow-level one. Re-verified: a job-level `contents: write` passes the audit while it prints "no other write scope". Fix on return.
- **F3 — the label-create block can abort the alert under `set -e`.** `gh label create` without `--force` exits non-zero on an existing label (confirmed in `gh label create --help`: "or update an existing one with `--force`"), and `gh label list` defaults to 30 labels (confirmed in `--help`), so three paths reach it with the label present: two simultaneous alert runs racing on the first-ever failure, a repo past 30 labels, and a pipefail/SIGPIPE inversion on the probe pipeline. Each kills the alert in exactly the case it exists for. Fix on return.
- **F4 — no `concurrency:` group, so two near-simultaneous failures can double-post.** Both sibling workflows carry one; this one does not. Fix on return.
- **F5 — `on.workflow_run.workflows` matches a workflow's `name:`, not its filename.** It works only because both siblings set `name:` equal to their filename; the audit's `WATCHED` literal is never compared against those files. Fix on return by reading each watched file's `name:`.
- **F9 — the stray-`${{ }}` check compares values, not sites.** An expression duplicating one already in `env:` passes wherever it appears, including directly in the shell body — the standard injection shape the check exists to prevent. Fix with F10.
- **F11 — the dry-run stub always exits 0**, so no fixture exercises a failing GitHub call and F3's abort path is invisible to it. Fix on return with a fourth fixture.
- **F12 — `job <- jobs[[1L]]` runs unconditionally after the zero-jobs check**, turning a diagnostic into a subscript error. Fix on return.
- **F13 — AC5's evidence cites workflow-file line numbers while the audit compares indices into the dedented `run:` body.** Same ordering, different coordinate systems. Fix the evidence wording on re-review.
- **F6 + blame-history F1 (follow-up, maintainer call at the next question gate) — neither audit script is invoked by anything, and `PROFILE.md`'s consistency-gate was not updated.** Their cited precedent `tools/check-ci-deps.R` IS run in CI. Wiring them in raises a real dependency question: the dry run needs `jq` and both need the `yaml` package, which is not in Suggests.
- **F7 (fix on return) — two comments describe an `actions/checkout` step that does not exist.** The header claims the workflow runs "with the default branch checked out" and the permissions comment justifies `contents: read` as what checkout needs; there is no checkout step, and with the body inline nothing needs `contents` at all. Found independently in-session before the fan-out reported it.
- **F8 (reject as written; candidate row on merge) — only `conclusion == 'failure'` alerts**, so `startup_failure` and `timed_out` leave master red and silent. AC1 requires exactly that equality, so widening it here would breach the criterion under review; it is a scope question for a follow-up, not a defect in this milestone.
