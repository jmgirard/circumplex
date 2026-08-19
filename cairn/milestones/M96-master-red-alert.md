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
  the conclusion — each carried in from the `workflow_run` payload through the
  step's `env:` block, and reaching the issue as reported text rather than
  composed text under the dry run's fixtures.
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

- [ ] AC1: the workflow file exists and declares exactly one job; its `on:`
      block names `workflow_run` and nothing else; that block's `workflows:`
      list is exactly `R-CMD-check.yaml` and `test-coverage.yaml`, and each of
      those two files declares `name:` equal to its own filename
      (`workflow_run` matches the name, not the filename); and the job's `if:`,
      with every run of whitespace collapsed to a single space and the ends
      trimmed, is byte-identical to `github.event.workflow_run.conclusion ==
      'failure' && github.event.workflow_run.event == 'push' &&
      github.event.workflow_run.head_branch ==
      github.event.repository.default_branch` — read by parsing this file's
      `on:`, `jobs:` and `if:` values and the `name:` of the two watched files.
- [x] AC2: `permissions:` at the workflow or job level grants `issues: write`
      and no other write scope, read by parsing the `permissions:` mapping.
- [ ] AC3: the issue text the alert produces is fixed boilerplate plus
      `workflow_run` payload values and nothing else, decided by two bounded
      checks together. (a) The alert step's parsed `env:` mapping is exactly
      `ALERT_WORKFLOW`, `ALERT_RUN_URL`, `ALERT_HEAD_SHA`, `ALERT_CONCLUSION`
      (plus `GH_TOKEN`, `GH_REPO`), each byte-identical to
      `${{ github.event.workflow_run.<name|html_url|head_sha|conclusion> }}`.
      This is the only place the values are tied to the payload; no check
      downstream of it can see that binding. (b) For one synthetic payload
      whose four values are non-empty and are substrings neither of one another
      nor of the boilerplate, the dry run captures the `--title` and `--body`
      arguments of every `gh issue create` and `gh issue comment` call its stub
      records, in every fixture, replaces each payload value in each capture
      with its field name, and requires each result to be byte-identical to a
      committed expected template — committed once as a reviewed expectation,
      never regenerated from a failing capture. Each of the four payload values
      must occur in each raw capture, so an empty or truncated capture fails
      rather than passing vacuously; `--body` is the last argument of each such
      call, so its multi-line value has an unambiguous end in the recorded
      vector. (b) decides the text produced under the dry run's environment,
      not the workflow's source: a construct that expands to nothing locally
      and to text on a runner is out of its reach. Accepted at internal tier —
      the source-scanning guarantee is deliberately descoped.
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

- AC1 → T1, T9
- AC2 → T1
- AC3 → T9
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

- [x] T6: Repair AC3's scan — cover the whole shell body and the issue title, not only the body heredoc; refuse command substitution in anything that reaches the issue; make the `${{ }}` check site-based rather than value-based.
- [x] T7: Harden the alert body — idempotent label creation that cannot abort the job, a label probe that cannot invert on a pipeline, a `concurrency:` group, and the unused `contents:` grant and its false checkout comments removed.
- [x] T8: Repair the audits' own defects — effective (job-over-workflow) permissions precedence, watched-workflow `name:` equality, the zero-jobs crash, a dry-run fixture where a GitHub call fails; name both scripts in the profile's consistency-gate slot.

- [ ] T9: Delete the source scanner from `tools/check-master-red-alert.R`,
      including its "and nothing else" output line; build AC3(a)'s `env:`
      comparison and AC1's exact `if:`, one-job, `on:`-exclusivity and sibling
      `name:` checks in its place.
- [ ] T10: Build AC3(b) in `tools/master-red-alert-dryrun.R` — capture the
      `--title`/`--body` of every recorded issue create and comment in every
      fixture, substitute the payload values back out, compare against a
      committed template; fix the `set -e` abort on a failing `gh label list`.

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
- 2026-08-18: minor amendment — three repair tasks (T6-T8) added for the review return's findings; no criterion, scope or Coverage change (each repair sits under an existing criterion).
- 2026-08-18: return question gate — the two audit scripts get named in `PROFILE.md`'s consistency-gate slot rather than wired into CI, which would need `yaml` in Suggests; F8's other red conclusions (`startup_failure`, `timed_out`) hold out of the criteria set and become a candidate row at merge, per the return-adjacent direction rule.
- 2026-08-18: T6 (return, AC3) — the scan's reported region is now the `TITLE=` assignment as well as the `BODY=` heredoc; anything composed there (command substitution, backticks, defaulted expansion) is refused outright rather than enumerated; the `${{ }}` check is site-based, requiring each expression to sit on a line of the step's `env:` block; and every remaining site in the shell body must classify as an env value, an assigned variable or a `jq --arg` name. Re-run against the three mutations that defeated the old scan — composed `TITLE="master is red: $(hostname)"`, a `$(gh api /user --jq .login)` body row, and `${{ }}` interpolated straight into the body — each now fails, as do the two earlier regressions (body citing `GH_REPO`, dropped head-SHA row). No package code touched, so the verify slot runs once at T8.
- 2026-08-18: T7 (return, F3/F4/F7) — label creation is now `--force` (updates instead of erroring when the label exists, so a race on the first-ever failure cannot abort the loser), the probe takes `--limit 200` (past `gh label list`'s 30-item default) and tests a captured string with a bash pattern instead of a `| grep -q` pipeline that `pipefail` can report as failed on EPIPE; a `concurrency:` group keyed on the watched workflow with `cancel-in-progress: false` serializes alerts so two near-simultaneous failures cannot both search-then-create; the unused `contents: read` grant is gone and the two comments describing an `actions/checkout` step that never existed are corrected. All three fixtures still pass and the audit's permissions, label-ordering and expression checks still fire against mutants.
- 2026-08-18: T8 (return, F2/F5/F11/F12 + the gate wiring) — the permissions check now reads the effective mapping (job-level replaces workflow-level in GitHub) and refuses a file declaring both; each watched workflow's own `name:` is read and required to equal the string in `workflows:`, since `workflow_run` matches the name and not the filename; a zero-jobs file stops with a message instead of a subscript error; the dry run gained a fourth fixture where `gh label create` is refused. That fixture found a further defect and fixed it: a refused create still aborted the job under `set -e`, so the alert now falls back to posting an unlabeled issue with a `::warning::` saying it will not dedupe. A concurrency guard was added to the audit so T7's fix cannot be silently dropped. `PROFILE.md`'s consistency-gate slot now names both scripts (the two master-watch bullets merged and compressed in one pass to hold the 120-line cap, and carrying the stale-`gh run list` caution the review gate hit). Verify slot: `devtools::test()` [ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ].
- 2026-08-18: return fixes complete (T6-T8); `devtools::check(args = "--no-manual")` Status OK (0/0/0). Status -> review.
- 2026-08-18: defect return 2 (review gate) — AC3 falsified again inside its own domain by two new routes, and AC1's procedure shown not to test what it claims. Verified on copies: `--body "$BODY runner=$(hostname)"` at the `gh` call site, and `BODY="$BODY runner: $(hostname)"` re-composed after the heredoc, both pass the audit and the dry run; `&&` -> `||` in the job's `if:` passes both while the mutated workflow would alert on every push run including green ones. Production defect alongside them: a failing `gh label list` aborts the alert under `set -e` (verified, exit 1, no issue posted) — the returned F3 still live on the read call, since T8's fallback covers only `gh label create`. Thrash rule (b) fires: AC3 has now failed twice, each by a new mechanism of the same shape. Status review -> in-progress; AC1 and AC3 unticked.
- 2026-08-18: amendment return: AC1 — "the job's `if:`, with every run of whitespace collapsed to a single space and the ends trimmed, is byte-identical to `github.event.workflow_run.conclusion == 'failure' && github.event.workflow_run.event == 'push' && github.event.workflow_run.head_branch == github.event.repository.default_branch`"
- 2026-08-18: amendment return: AC3 — "the issue text the alert produces is fixed boilerplate plus `workflow_run` payload values and nothing else, decided by two bounded checks together"
- 2026-08-18: descope chosen by Jeff at the second return, over a third audit-hardening pass and over a Fable escalation: the deliverable is the workflow, not the audit script. AC1 and AC3 narrowed (the source-scanning guarantee deleted outright, not widened), the Scope In bullet narrowed to match, T9/T10 added, Coverage remapped AC3 → T9 and AC1 → T1+T9. F5 is fixed as a real production bug; F4 and F6-F14 are won't-fix by that same call — logged in Review, and F4 means a dropped `--label` on the search or the create is caught by nothing.
- 2026-08-18: the amended wording was audited by a fresh-context [O] reader that did not author it, before it was written to the file. It returned three defects, all adopted: AC3(b) quantified over one capture site where the body reaches `gh` at three (the returned F2 shape, one branch over); AC3 as first drafted tied nothing to the payload at all, since the dry run supplies the values itself, which also stranded the Scope bullet — hence clause (a) over the `env:` mapping; and AC1 had silently dropped the `on:`-exclusivity, sibling `name:` equality and one-job checks the existing audit already performs.

## Review

### First pass (2026-08-18) — returned

_Evidence gathered 2026-08-18 on branch `m96-master-red-alert`, PR #125. This pass returned the milestone at AC3; the evidence and findings below are kept as the record of that return. The second pass follows._

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

### Second pass (2026-08-18)

_Fresh evidence on `m96-master-red-alert` after the return fixes (T6-T8), PR #125._

- AC1 — PASS. `yaml::read_yaml()`: the `on:` block names `workflow_run` alone, its `workflows:` list is exactly `R-CMD-check.yaml`, `test-coverage.yaml` (types `completed`), and the single job's `if:` parses to `github.event.workflow_run.conclusion == 'failure' && github.event.workflow_run.event == 'push' && github.event.workflow_run.head_branch == github.event.repository.default_branch`. `tools/check-master-red-alert.R` asserts all four, and now also reads each watched file's own `name:` — both declare `name:` equal to their filename, which is what `on.workflow_run.workflows` actually matches. Fires on a dropped watched workflow, a dropped push condition, and a renamed sibling.
- AC2 — PASS. The effective `permissions:` mapping is `issues=write` alone; the job declares none, so the workflow-level block governs. The audit now reads job-level in preference to workflow-level (GitHub's own precedence — a job block replaces rather than merges) and refuses a file declaring both; re-verified by mutation, a job-level `contents: write` that the first pass's check let through now fails.
- AC3 — PASS. The scan's domain is both regions that reach the issue — the `TITLE=` assignment and the `BODY=` heredoc — plus the whole shell body, where every remaining `$VAR` must classify as an `env:` value, a variable the body assigns, or a `jq --arg` name. Anything composed in the reported regions (command substitution, backticks, defaulted expansion) is refused outright, and `${{ }}` inside the `run:` body is refused as the injection shape it is. It reports 4 payload fields and nothing else: `conclusion`, `head_sha`, `html_url`, `name`. Re-verified against the three mutations that defeated the first pass's scan — `TITLE="master is red: $(hostname)"`, a `$(gh api /user --jq .login)` body row, and `${{ github.event.workflow_run.head_sha }}` interpolated into the body — each now fails, as do the earlier two (body citing `GH_REPO`, dropped head-SHA row).
- AC4 — PASS. `tools/master-red-alert-dryrun.R` lifts the shell body out of the YAML, stubs `gh` (recording every call; `jq` left real) and runs four fixtures: label present + no open issue -> `label list -> issue list -> issue create`, exactly one create and no comment; label present + matching open issue -> `label list -> issue list -> issue comment` on #42, exactly one comment and no create; label absent -> `label list -> label create -> issue list -> issue create`, the create ahead of the search. The fourth is beyond the criterion and was added on return: with `gh label create` refused, the alert still posts (`label list -> label create -> issue create`, unlabeled, with a `::warning::`). Each fixture is confirmed discriminating by a mutant only it catches — always-create, a dedupe filter inverted to `.title != $t`, a deleted `gh label create`, and a re-fatal label failure.
- AC5 — PASS. Parsing the workflow, the audit compares positions within the dedented `run:` body: `gh label create` at body line 45, the `gh issue list` dedupe search at body line 55 (file lines 100 and 110 — the audit measures in body coordinates, not file coordinates). The ordering assertion fires on a copy with the label block relocated below the search.
- AC6 — PASS. `grep -nE 'setup-r|extra-packages|install\.packages'` over the new workflow returns nothing, and the audit's own grep over all four keys agrees; it fires on a copy with a `setup-r` step spliced in. Separately, as the unchanged-regression guard only: `Rscript tools/check-ci-deps.R` exits clean (14 Suggests in sync) — not evidence about the new file, as the criterion itself states.

**Consistency gate (second pass).** `cairn_validate` exit 0, every check PASS (47 advisory `work-log format` warnings, all pre-existing M7 wrapped lines). `cairn_impact` skipped — no principle changed. `devtools::document()` no diff, zero `resolve link` warnings at `cli.width = 500`. `pkgdown::check_pkgdown()` clean. `devtools::check(args = "--no-manual")` Status OK (0/0/0) at 9e365d53; `git diff 9e365d53..HEAD` outside `cairn/` is empty, so that is the content under review. NEWS.md: no entry owed (internal tier, nothing user-visible). README untouched; no new top-level files. Master matrix watch: newest push run of `R-CMD-check.yaml` on master 32202243374, success. Master coverage watch: newest verdict 32202243432, success (32187677266 is `cancelled`, not a verdict). PR #125 checks all green (matrix, ubuntu-latest release, pkgdown).

**Fan-out, second pass.** [S] prior-PR-comments: no findings — its probe (`gh api .../pulls/comments?per_page=1`) returned empty again, and it independently matched every first-pass fix-on-return finding to the code that now carries it. [S] blame-history: one finding, fixed at the gate.

- **blame-history F1 (fixed on the branch) — the `PROFILE.md` compression lost the scoping of the absent-run rule.** Pre-branch the two watch bullets disagreed deliberately: M93's said of `R-CMD-check.yaml` "a red **or absent** run is a gate failure", while M95's said of `test-coverage.yaml` "No qualifying run is likewise no verdict, **not a failure** — `paths-ignore` means tracking-only merges produce none". Merging them left the second clause unscoped immediately after the first, so a gate-runner reading it would treat an absent `R-CMD-check.yaml` run as no verdict — reversing what M93 was built to close. Verified against `git show master:cairn/PROFILE.md` and both archives. The clause is now scoped ("there alone"), and M93's rule reads "red OR ABSENT is a gate failure".
- blame-history F2/F3/F4 (no action) — the 120-line cap justifying the compression checks out (119 post-fix); every other M93/M95 operative fact survives with its attribution; and the alert's own `concurrency:` block does not fight the watched workflows' `cancel-in-progress: true` groups, nor does leaving `tools/check-ci-deps.R` unwidened contradict the plan-gate decision that chose it.

[O] diff-bug, second pass — 14 findings. Five verified on copies in-session before triage.

- **F1 (return) — the `if:` check is three independent substring greps, so `&&` -> `||` survives.** Verified: both scripts exit 0 on the mutated file, which would open an issue on every push run of either workflow, green ones and PR runs included. AC1's claim is not tested by the procedure AC1 names.
- **F2 (return) — AC3's composition refusal stops short of the shell body it claims to cover.** `reported` is the `BODY=` heredoc plus the one `TITLE=` line, so composition at the `gh` call site is invisible: `--body "$BODY runner=$(hostname)"` passes both scripts. Same defect class as the first pass's F1/F10, one level out.
- **F3 (return) — `BODY` can be re-composed after the heredoc and nothing sees it.** `BODY="$BODY\n\nrunner: $(hostname)"` between the heredoc and the label block passes both: the heredoc is still unique and `BODY` is in `assigned`. `TITLE` is guarded by an exactly-one assertion; `BODY` is not. A composed title also survives via an intermediate local (`SUF="$(hostname)"; TITLE="...${SUF}"`).
- **F4 (return) — dedupe correctness is asserted by neither script.** Deleting `--label "$LABEL"` from the `gh issue list` search and/or from `gh issue create` leaves both at exit 0, individually and together; either mutation means one new issue per failed push. The stub ignores flags, so call-sequence and count assertions cannot see it.
- **F5 (return) — a failing `gh label list` kills the alert outright.** Verified by running the lifted body with that call refused: exit 1, no issue posted. `gh issue list` and `gh issue create` abort the same way. T8's `LABEL_OK=0` fallback covers only `gh label create`.
- **F6 (return) — T7's three hardenings have no regression guard.** Removing `--force`, removing `--limit 200`, and reverting the probe to a `| grep -q` pipeline each leave both scripts at exit 0, while a `concurrency:` guard was added for T7's fourth fix — inconsistent rather than principled, and each revert re-opens a documented abort path.
- **F7 (return) — the concurrency comment is wrong about what `cancel-in-progress: false` protects.** GitHub cancels a *pending* run when a newer one queues; the flag only spares the in-progress run. The behaviour is acceptable; "never cancel one — a queued alert still needs to post" is not, and the audit's message repeats it.
- **F8 (return) — a comment describes a `case` over a captured string; the code is a `[[ ... != *pattern* ]]` test.** The reasoning is right, the construct named is not the one used.
- **F9 (return) — `set -euo pipefail` can be deleted with both scripts still green**, though every safety comment in the body is written on the premise that it is there.
- **F10 (fix on return) — the first pass's evidence lines are stale against the current file** (AC5 cites body lines 76/77/85; they are now 98/100/110). Retained deliberately as the record of the first return and headed as such, but the AC5 line should say so on its face.
- F11 (no action) — AC3's checkbox was `- [ ]` when the reviewer read the file; it had been ticked against second-pass evidence before this return unticked it again.
- F12 (return) — stub fidelity: it keys only on `"$1 $2"`, ignores every flag, and never fails a read call, so its conclusions transfer for ordering and counts but not for filter correctness (F4) or read-call failure (F5). The script header should say so, now that the profile sells it as a gate check.
- F13 (no action) — the audit enforces "no other *write* scope" rather than the Scope section's "`issues: write` and nothing else"; AC2 is worded to match the audit, and the file grants only `issues: write`.
- F14 (no action) — the title hardcodes "master is red" while the `if:` is generic over the default branch; if the branch is ever renamed the title lies, though the dedupe key is unaffected.
