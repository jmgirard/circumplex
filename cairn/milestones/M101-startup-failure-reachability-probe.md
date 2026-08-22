# M101: Find out whether a run that never starts reaches the master-red alert

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m101-startup-failure-reachability-probe · https://github.com/jmgirard/circumplex/pull/130

## Goal

Establish by deliberate experiment whether a push run of a watched workflow
that fails to start on the default branch delivers a `workflow_run` event
`.github/workflows/master-red-alert.yaml` can match, and record the measured
answer where the alert's readers will find it.

## Scope

Surface tier: **internal** — the deliverable is CI alerting configuration and
a recorded finding; no consumer of the R package relies on either.

**In:** one probe repository under the maintainer's account whose alert
configuration reproduces this repo's byte for byte, driven through two
`startup_failure` cases — a workflow file whose YAML does not parse (so no
`name:` is readable) and one that parses and declares `name:` but is rejected
by workflow-schema validation. What each case produced is recorded, the alert
workflow's header paragraph is rewritten from "unresolved" to what was
measured, and the ROADMAP candidate row carrying the question is
dispositioned.

**Out:** the scheduled-sweep second detector → its existing ROADMAP candidate
row, where M99's plan gate deferred it as its own design, audit and dedupe
surface. Any change to the alert's `if:` expression or `types:` filter → M99's,
unchanged here unless the probe shows them wrong. Adding `actionlint` or any
local GitHub-Actions validator → M99 review F3, a dependency decision needing
its own gate. GitHub-native branch protection → its existing candidate row.
Hardening `tools/check-master-red-alert.R` → out; this milestone only requires
it still pass unchanged.

## Acceptance criteria

- [ ] AC1: A probe environment exists whose `on.workflow_run` block and job
      `if:` expression are byte-identical to
      `.github/workflows/master-red-alert.yaml`'s at the commit named in the
      work log, verified by a recorded diff of the two extracted blocks.
- [ ] AC2: The probe drives at least the two named cases on the probe's
      watched workflow — (i) a file whose YAML does not parse, (ii) a file
      that parses and declares `name:` but is rejected by workflow-schema
      validation — and the work log records, per case, the run's `status` and
      `conclusion` as reported by `gh run list`, and whether the alert job was
      triggered.
- [ ] AC3: For each of the two cases in AC2, the work log states whether an
      alert issue was opened, read from the probe repo's issue list, with the
      query recorded.
- [ ] AC4: `.github/workflows/master-red-alert.yaml`'s header comment is
      rewritten so that, for each of the two cases AC2 drove, it states the
      observed outcome (event delivered and alert job triggered / event
      delivered but not matched / no event observed) and attributes it to the
      probe run, naming the probe repo and run URL. The comment retains one
      sentence stating that cases other than those two remain untested. The
      header comment's full prior text and full new text are both quoted in
      the work log.
- [ ] AC5: The ROADMAP candidate row carrying this question is dispositioned
      — closed, or narrowed to the sub-case AC2 left unsettled — and the row's
      text after the edit is quoted in the work log.
- [ ] AC6: `Rscript tools/check-master-red-alert.R` and `Rscript
      tools/master-red-alert-dryrun.R` both exit 0 at the end of the
      milestone, and `git diff` shows
      `.github/workflows/master-red-alert.yaml`'s `on:` block and job `if:`
      unchanged from master.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: Stand up the probe repository (`jmgirard/gha-startup-failure-probe`,
      public) with an alert workflow copied verbatim from
      `.github/workflows/master-red-alert.yaml` and a watched workflow named
      `R-CMD-check.yaml` — the same name this repo uses, so the alert's
      `on.workflow_run` block copies untouched. Record the source commit,
      extract both sides' `on.workflow_run` block and job `if:`, and record
      the diff.
- [x] T2: Drive the positive control first — a watched workflow that runs and
      exits non-zero, which must open an alert issue; a silent probe would
      otherwise make a null result in case (i) uninterpretable. Then drive
      case (i) — push a watched-workflow file whose YAML does not parse to the
      probe's default branch. Record, for the control and for case (i), the
      run's `status` and `conclusion` from `gh run list`, whether an alert run
      appeared, and whether an issue was opened (with the issue query).
- [x] T3: Drive case (ii) — push a watched-workflow file that parses and
      declares `name:` but fails workflow-schema validation. Record the same
      four things.
- [x] T4: Rewrite the alert header's "What M99's widening does NOT establish"
      paragraph against what T2 and T3 measured, quoting the full prior and
      full new text in the work log.
- [x] T5: Disposition the ROADMAP candidate row and quote its post-edit text.
- [x] T6: Re-run both alert audits and diff the alert's `on:` block and job
      `if:` against master.

## Work log

- 2026-08-21: created by /milestone-plan; absorbs the M99-remainder candidate row (M99 review, scoped out at that plan gate).
- 2026-08-21: plan gate chose a separate probe repository over probing inside circumplex, because the alert only reacts to its own default branch and probing here would put knowingly-broken YAML on the distribution channel while mutating the alert's watch list and the checker's `WATCHED` pin; falsified by evidence the probe repo's result does not transfer — a setting, default-branch name, or workflow inventory difference that changes event delivery.
- 2026-08-21: plan gate chose recording the measurement over shipping the scheduled sweep here, because the sweep is a second detector with its own design, alerting and dedupe surface, deferred on those grounds at M99's plan gate; falsified by the probe showing the gap real AND a master break going unannounced before the sweep is planned.
- 2026-08-21: plan gate chose driving both `startup_failure` cases over the unparseable one alone, because a one-sided result establishes that some case is missed without establishing where the boundary sits, and AC4 has to say which sub-cases the alert still catches; falsified by the two cases proving indistinguishable at the event layer.
- 2026-08-21: reduced criteria audit ([O], internal tier) ran over the drafted criteria and returned one finding — the original AC4 promised that "each surviving open sub-case is named as still open with its reason", a universal over a domain no named procedure enumerates (its membership fixed by author recall), and it silently carried a cross-repository demonstration into a claim about this repo. Fixed by adopting the auditor's narrower wording verbatim: the promise now quantifies over exactly the two cases AC2 drives, the residual is one blanket sentence, and the probe's provenance is stated rather than absorbed. AC1, AC2, AC3, AC5, AC6 passed both questions.
- 2026-08-21: collision sweep — no `DECISIONS.md` entry has ruled on this question (the two `reachab` hits are norms-audit machinery, unrelated); the M96 archive does not mention it; the only prior state is the candidate row this milestone absorbs and M99's archive, which records the question as deliberately left open. GitHub's own `workflow_run` documentation was checked at plan time and answers none of the three sub-questions (is `workflows:` required, does it match `name:` or filename, is an event delivered for `startup_failure`), which is why the answer has to be measured.
- 2026-08-21: creating the probe repository is an outward-facing action; authorized by Jeff at this plan gate. Keep it (private) rather than delete it at milestone end, so AC4's run URLs stay resolvable.
- 2026-08-21: started by /milestone-implement; branch `m101-startup-failure-reachability-probe` cut from master at `2b2c841d`.
- 2026-08-21: implement gate — Jeff chose `jmgirard/gha-startup-failure-probe` PUBLIC over private, so the recorded run URLs stay readable without auth; the repo is kept after the milestone rather than deleted, per the plan gate. Creating it is the outward-facing action he authorized at the plan gate, now with a name.
- 2026-08-21: implement gate — Jeff chose running a positive control before the two cases in question. Recorded as a T2 refinement rather than a new task, so the Coverage map's positional numbering is untouched; AC2 promises "at least the two named cases", so a control sits inside the criterion as written.
- 2026-08-21: T1 refinement (minor) — the plan's T1 said to adjust the watched workflow's name, which would have edited the very `on.workflow_run` block AC1 requires to be byte-identical. Resolved by naming the probe's watched workflow `R-CMD-check.yaml`, as here; the alert's `test-coverage.yaml` entry simply never matches in the probe repo, which changes nothing about the case under test.
- 2026-08-21: T1 — `jmgirard/gha-startup-failure-probe` created (public) with `.github/workflows/master-red-alert.yaml` copied from circumplex at commit `0b8863f760b95e3938a1fe32710d753e6cfa5c74` and a watched workflow named `R-CMD-check.yaml`. `diff` of the two alert files is EMPTY — the whole file is identical, not merely the `on.workflow_run` block and job `if:` AC1 asks for; both were also diffed separately and are identical. Probe default branch is `main`; the alert's `if:` compares `head_branch` to `github.event.repository.default_branch`, so the branch name is not a divergence. The alert's `test-coverage.yaml` watch entry has no counterpart in the probe and simply never matches. Negative control, unplanned but free: the baseline push made `R-CMD-check.yaml` conclude `success` (run 32540334449), the alert run WAS delivered (32540340661, event `workflow_run`) and its job `alert` concluded `skipped`, opening no issue — so the `if:` is live and the copy is wired. Note for the record: a delivered alert RUN is not a fired alert JOB, since the `if:` is job-level; the two are recorded separately from here on.
- 2026-08-21: T2 — POSITIVE CONTROL: watched workflow exits 1; run 32540381494 `name=R-CMD-check.yaml event=push status=completed conclusion=failure`; the alert run WAS delivered (32540393479, `event=workflow_run`) and its job `alert` concluded `success`, opening issue #1 `master is red: R-CMD-check.yaml` (label `master-red`), read via `gh issue list --repo jmgirard/gha-startup-failure-probe --state all`. The control fires for the reason it should — a `failure` conclusion on a push run of the default branch — so a null result in case (i) is not a dead probe. #1 was then CLOSED before case (i): the alert dedupes against an OPEN labelled issue, so leaving it open would have turned a fired alert into a comment rather than a new issue.
- 2026-08-21: T2 — CASE (i), the watched workflow's YAML made unparseable (an unclosed flow sequence; `yaml::read_yaml()` confirms `Parser error: while parsing a flow sequence at line 4, column 5 did not find expected ',' or ']' at line 5, column 11`, so the file is genuinely unreadable rather than merely disliked by GitHub). Result, read from `gh api repos/jmgirard/gha-startup-failure-probe/actions/runs`: run 32540432744 `name=.github/workflows/R-CMD-check.yaml event=push status=completed conclusion=failure`, 0 jobs. NO alert run was delivered — the run list holds no `workflow_run` entry after it — and no issue was opened; the issue list still shows only the closed #1, re-read 90s after the run completed.
- 2026-08-21: T2 — two facts the plan did not anticipate, both from case (i). FIRST: the conclusion is `failure`, NOT `startup_failure`. M99 widened the alert to admit `startup_failure`, but an unparseable workflow does not produce that conclusion here at all, so the widening is not what this case turns on. SECOND: the run's `name` is the PATH, `.github/workflows/R-CMD-check.yaml`, where the control's was the declared `R-CMD-check.yaml` — and `on.workflow_run.workflows` lists `R-CMD-check.yaml`. The two runs differ in conclusion not at all (`failure` both) and in name exactly, so the name is the discriminator, which is what the M99 header hypothesized. The alert misses this case, and misses it for the naming reason rather than the conclusion reason.
- 2026-08-21: T3 — CASE (ii), a watched workflow that parses and declares its name but fails workflow-schema validation (the `probe` job carries `steps` and no `runs-on`; `yaml::read_yaml()` parses it and reports `name: R-CMD-check.yaml`, job keys `steps`). Result: run 32540622138 `name=.github/workflows/R-CMD-check.yaml event=push status=completed conclusion=failure`, `jobs.total_count` 0. NO alert run delivered and no issue opened — the issue list still shows only the closed #1.
- 2026-08-21: T3 — the two cases answer identically, and the mechanism is now pinned. Declaring `name:` is NOT sufficient: case (ii)'s file parses and names itself, yet GitHub still reports the run's `name` as the PATH `.github/workflows/R-CMD-check.yaml`, exactly as case (i) did. The control run 32540381494, whose file was valid, reported `name=R-CMD-check.yaml`. So GitHub resolves a workflow's declared name only for a workflow it can validate, and `on.workflow_run.workflows` — which lists `R-CMD-check.yaml` — therefore matches neither broken case. Both are invisible to the alert.
- 2026-08-21: T3 — neither case produced conclusion `startup_failure`; both produced `failure` with zero jobs. M99 widened the alert's `if:` to admit `startup_failure`, and that widening is untouched by this result: these runs would pass the `if:` if they arrived, and they never arrive. Whatever DOES produce a `startup_failure` conclusion is outside AC2's two cases and remains untested here — AC4's residual sentence carries that, and T5 files it as a candidate row.
- 2026-08-21: T4 — the alert header's open-question paragraph replaced. PRIOR TEXT, in full: "# What M99's widening does NOT establish — observed 2026-08-21, and unresolved: / # a run that never starts may deliver no `workflow_run` event at all, and / # `on.workflow_run.workflows` below matches each watched workflow's declared / # `name:`, which a workflow file too malformed to parse cannot declare. So the / # gate now admits `startup_failure`, but whether such a run ever reaches this / # gate is untested — nothing checkable from inside this repo decides it. The / # ROADMAP candidate row that graduated into M99 keeps that question open, with / # a deliberate live probe or a scheduled sweep as the two candidate remedies." NEW TEXT, in full: "# A BROKEN watched workflow is invisible here — measured 2026-08-21 (cairn / # M101) in https://github.com/jmgirard/gha-startup-failure-probe, a repo whose / # copy of this file is byte-identical. Two cases were driven on a watched / # workflow, and both were MISSED — no `workflow_run` event reached this / # workflow at all, so no alert run, no issue: / #   * YAML that does not parse — run .../actions/runs/32540432744 / #   * parses and declares `name:` but fails schema validation, a job with no / #     `runs-on` — run .../actions/runs/32540622138 / # Neither concluded `startup_failure`; both concluded `failure` with zero jobs. / # The discriminator is the NAME, not the conclusion: GitHub reported each / # broken run's name as the PATH `.github/workflows/R-CMD-check.yaml`, while the / # probe's control run — the same file, valid, exiting non-zero — reported the / # declared `R-CMD-check.yaml` and DID reach the alert (issue opened). So / # `on.workflow_run.workflows` below matches a declared name that GitHub does / # not resolve for a workflow it cannot validate. M99's widening is untouched by / # this: such a run would pass the `if:` if it arrived, and it never arrives. / # Cases other than those two — including whatever does produce a / # `startup_failure` conclusion — remain untested. A scheduled sweep is the / # candidate remedy, and it is a ROADMAP candidate row, not a plan." Both alert audits re-run exit 0 against the rewritten file; comment-only, no behaviour moved.
- 2026-08-21: T5 — the candidate row's open remainder dispositioned CLOSED. Post-edit text of the whole row, verbatim: "- ~~The master-red alert stays silent on `startup_failure` and `timed_out`~~ **graduated 2026-08-21 → M99** (M96 review, [O] diff-bug F8 across two passes); its promotion condition had not fired — Jeff promoted it at the M99 plan gate, as at M94's and M96's. **Open remainder, scoped out of M99 at that gate:** whether a run that never starts delivers a `workflow_run` event at all, and under what name — `on.workflow_run.workflows` matches a workflow's declared `name:`, which an unparseable file cannot declare, so M99 widens the gate for that case without establishing it is reachable. Promote on a malformed workflow YAML reaching the default branch unalerted, or into whichever milestone next opens the alert workflow; **that remainder was CLOSED 2026-08-21 by M101** (probe repo `jmgirard/gha-startup-failure-probe`, alert copy byte-identical): a broken watched workflow is invisible to the alert, and the mechanism is name resolution rather than the conclusion — GitHub reports a run's name as the file PATH for a workflow it cannot validate, so `on.workflow_run.workflows`, which lists a declared name, never matches; both an unparseable file and a schema-invalid one concluded `failure` with zero jobs and delivered no event, while the valid control was matched and alerted. M99's `startup_failure` widening is untouched: such a run would pass the `if:` if it arrived. Surviving remainder, now the whole of it: the **scheduled sweep as a second detector**, declined at the M99 gate as its own design, audit and dedupe surface and left out of M101 at its plan gate — the gap is now measured real, so promote it when a master break actually goes unannounced, or into whichever milestone next opens the alert workflow. **Also open, from M101:** what actually produces a `startup_failure` conclusion, and whether the alert can see it — both cases M101 drove concluded `failure` with zero jobs, so M99's widening still has no measured instance; whatever reaches that conclusion may or may not carry a resolvable workflow name, which is what decides visibility. Same promotion condition; the probe repo is kept public and can drive further cases."
- 2026-08-21: T5 — the residual M101 did not settle (what produces `startup_failure` at all, and whether it carries a resolvable name) was folded into that same lineage row rather than filed as its own candidate row: a separate row took ROADMAP to 60 lines against the <60 cap, and search-first found the lineage already owned by this row. Folding is the cap remedy the rules name (graduate or prune candidates) applied to a row that is one lineage, not two.
- 2026-08-21: T6 — `Rscript tools/check-master-red-alert.R` exit 0 and `Rscript tools/master-red-alert-dryrun.R` exit 0 against the rewritten header. `diff` of the alert's `on:` block and of its job `if:` against `git show master:` are both EMPTY, and `git diff master...HEAD -- .github/` filtered to non-comment added/removed lines is empty, so every workflow change on this branch is comment text.
- 2026-08-21: probe repo restored to a valid, succeeding workflow now the experiment is done; the four states it passed through remain in its git history, and the cited run URLs are unaffected. The repo is kept public per the plan gate.
- 2026-08-21: all tasks done; status → review. No R code, roxygen, or test file touched, so the profile's `verify` slot is vacuous on this diff; `devtools::test()` run anyway for a fresh number — FAIL 0 | WARN 5 | SKIP 3 | PASS 8395, the same 8395 M99 and M100 recorded, the 5 warnings lavaan's and pre-existing.

## Decisions

## Review
