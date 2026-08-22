# M101: Find out whether a run that never starts reaches the master-red alert

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m101-startup-failure-reachability-probe

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

- [ ] T1: Stand up the probe repository (private, under the maintainer's
      account) with a watched workflow and an alert workflow copied from
      `.github/workflows/master-red-alert.yaml`; adjust only the watched
      workflow's name. Record the source commit, extract both sides'
      `on.workflow_run` block and job `if:`, and record the diff.
- [ ] T2: Drive case (i) — push a watched-workflow file whose YAML does not
      parse to the probe's default branch. Record the run's `status` and
      `conclusion` from `gh run list`, whether an alert run appeared, and
      whether an issue was opened (with the issue query).
- [ ] T3: Drive case (ii) — push a watched-workflow file that parses and
      declares `name:` but fails workflow-schema validation. Record the same
      four things.
- [ ] T4: Rewrite the alert header's "What M99's widening does NOT establish"
      paragraph against what T2 and T3 measured, quoting the full prior and
      full new text in the work log.
- [ ] T5: Disposition the ROADMAP candidate row and quote its post-edit text.
- [ ] T6: Re-run both alert audits and diff the alert's `on:` block and job
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

## Decisions

## Review
