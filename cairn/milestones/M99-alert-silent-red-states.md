<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M99: Alert on the red states the gate ignores

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m99-alert-silent-red-states`

## Goal

Make the master-red alert fire on every way a watched workflow's push run can
end badly on the default branch, not only `conclusion == 'failure'`.

## Scope

Surface tier: **internal** — the deliverable is CI alerting tooling and the
hand-run audits over it; nothing an external consumer of the package installs,
loads, or calls depends on it.

**In:** replacing the job gate's single conclusion equality in
`.github/workflows/master-red-alert.yaml:41` with a negated membership test
over a committed benign list, so `timed_out` and `startup_failure` alert
alongside `failure` and any conclusion GitHub adds later alerts by default;
re-pinning `EXPECTED_IF` in `tools/check-master-red-alert.R:74`; a mutation
battery over the rewritten gate; and a dated note in the workflow's header
recording what the widening does not establish.

**Out:**
- Whether GitHub delivers a `workflow_run` event at all for a run that never
  started, and under what name — `on.workflow_run.workflows` matches a
  workflow's declared `name:`, which an unparseable file cannot declare, so no
  local check settles this → ROADMAP candidate row (a live probe, or a
  scheduled sweep as a second detector).
- New fixtures in `tools/master-red-alert-dryrun.R` for the added
  conclusions → declined at the plan gate, not deferred: the shell body has no
  conclusion-dependent branch, so the existing template comparison already
  proves the value reaches both cells it appears in.
- GitHub-native branch protection → stays the ROADMAP candidate row it is; a
  repository settings change appears in no diff this milestone produces.
- Which workflows are watched, the dedupe scheme, and the issue text.

## Acceptance criteria

- [ ] AC1: the job's `if:` no longer tests a single conclusion value — its
      conclusion clause is a negated membership test whose operand list is
      exactly the committed benign list. `Rscript tools/check-master-red-alert.R`
      compares the whole whitespace-collapsed expression against its
      `EXPECTED_IF` literal and exits clean; the audit reddens when the
      conclusion clause, the `event == 'push'` conjunct, and the
      default-branch conjunct are each mutated in turn (one mutant per run,
      three runs, each restored by copy, the workflow re-hashed after each).
- [ ] AC2: the benign list's operands are exactly the five conclusions
      `success`, `cancelled`, `skipped`, `neutral`, `stale`, so `failure`,
      `timed_out` and `startup_failure` are each admitted by exclusion. AC1's
      whole-expression comparison is the pin. (The per-value reasons are
      workflow prose promised by no criterion — `yaml::read_yaml` discards
      comments, so nothing could enumerate them.)
- [ ] AC3: the workflow's header comment states, as a dated observation, what
      this widening does not establish — that a run which never starts may
      deliver no `workflow_run` event at all, and that
      `on.workflow_run.workflows` matches a workflow's declared `name:`, which
      an unparseable workflow file cannot declare — and points at the ROADMAP
      candidate row carrying that open question.
- [ ] AC4: `Rscript tools/check-master-red-alert.R` and
      `Rscript tools/master-red-alert-dryrun.R` both exit clean unchanged
      (`cairn/PROFILE.md` consistency-gate), and the profile's `verify` slot is
      clean.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2
- AC3 → T4
- AC4 → T5

## Tasks

- [x] T1: re-pin `EXPECTED_IF` in `tools/check-master-red-alert.R:74` to the
      negated-membership expression; run the audit and record in the work log
      that it fails against the unmodified workflow (the red before the fix).
- [x] T2: rewrite the job gate at
      `.github/workflows/master-red-alert.yaml:40-43` to
      `!contains(fromJSON('[...]'), github.event.workflow_run.conclusion) && …`,
      write the five per-value reasons into the comment above it, and re-run
      the audit clean.
- [x] T3: mutation battery on the rewritten gate — one mutant per invocation
      over the committed file, restored by copy and re-hashed after each:
      drop a benign operand, negate the membership test, break the `push`
      conjunct, break the default-branch conjunct. Record which reddened.
- [x] T4: write the header's limitation note as a dated observation, pointing
      at the never-started reachability remainder the M96-review candidate row
      keeps open.
- [ ] T5: run both alert audits and the `verify` slot; check `git status` clean
      before recording the gate.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: plan gate chose alert-unless-benign (a negated membership test over five conclusions) over an allowlist of the three named conclusions, because an allowlist re-opens this same silence for any conclusion GitHub adds; falsified by a benign conclusion actually reaching the gate and opening a spurious issue.
- 2026-08-21: plan gate chose leaving `tools/master-red-alert-dryrun.R` untouched over adding `timed_out`/`startup_failure` fixtures, because the shell body has no conclusion-dependent branch and the committed-template comparison already proves the value reaches both cells; falsified by the body gaining any branch on `ALERT_CONCLUSION`.
- 2026-08-21: plan gate chose widening the gate plus a recorded doubt over building a scheduled sweep now, because a second detector needs its own design, audit, and dedupe against this one; falsified by a never-started run on the default branch going unalerted after this ships.
- 2026-08-21: T1 — `EXPECTED_IF` re-pinned to the negated-membership expression. The audit now exits 1 with exactly one problem, the `if:` mismatch, quoting the pinned expression against the workflow's surviving `conclusion == 'failure'` equality: the red before the fix.
- 2026-08-21: T2 — gate rewritten to `!contains(fromJSON(...), ...)` over the five benign conclusions, with a per-value reason above it; both alert audits exit 0. Swept the claim the widening falsified at four live sites (the workflow header, the `gh label create --description` text, and both audit scripts' headers, one of which also carried an M96-stale fixture count now stated as a derivation); the M96 archive and the M93 ROADMAP tombstone are history and stand.
- 2026-08-21: T3 — mutation battery, one mutant per invocation over the committed file, restored by copy and re-hashed after each: dropping the `stale` operand, dropping the `!` from the membership test, `push` -> `pull_request`, and the default-branch comparand -> a literal `master` each made the audit exit 1, and each with the `if:` mismatch problem rather than some other check. Unmutated control exits 0; final hash matches pristine, tree clean.
- 2026-08-21: T4 — limitation note written into the workflow header as a dated observation (2026-08-21), naming both halves of the doubt (the event may never be delivered; the `workflows:` filter matches a `name:` an unparseable file cannot declare) and pointing at the candidate row that keeps it open. Both audits still exit 0.
- 2026-08-21: criteria audit (reduced mode, internal tier) returned findings on two of five drafted criteria — AC1 quantified over a conclusion set no named procedure enumerates, AC2 named the YAML audit as pin for comment prose the parser discards. Both narrowed at the gate; two criteria dropped by the dry-run decision above.

## Decisions

## Review
