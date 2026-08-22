<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M102: Separate a filtered-out alert event from one never delivered

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Establish by deliberate experiment on the existing probe repo whether the
`workflow_run` event for a broken watched workflow is delivered and then
excluded by the alert's `workflows:` filter or never delivered at all, and
measure what conclusion deliberately-broken workflows actually reach.

## Scope

Surface tier: **internal** — the deliverable is a measurement and its record in
a CI workflow comment and tracking rows; no consumer of the R package relies on
it.

**In:** On `jmgirard/gha-startup-failure-probe`, add a second `workflow_run`
subscriber whose `workflows:` lists the file path
`.github/workflows/R-CMD-check.yaml`, re-drive M101's unparseable-YAML case, and
record whether that subscriber produced a run. Separately drive deliberately-
broken cases aimed at a `startup_failure` conclusion and record the conclusion
each actually reached, plus whether an alert run appeared. Write the measured
outcomes into `.github/workflows/master-red-alert.yaml`'s header comment and
update the ROADMAP lineage row's open items (b) and (c).

**Out:** Changing this repo's alert `on:`/`if:` blocks or `tools/check-master-red-alert.R`
→ a separate milestone, planned once this measurement exists (plan gate, measure-only).
An unfiltered `workflow_run` subscriber as a purer delivery test → stays out on
the self-retrigger hazard; if the path-spelling arm produces no run, the
delivered-vs-never-delivered question returns to the ROADMAP lineage row with
that hazard recorded. A scheduled sweep as a second detector (item (a)) → stays
a candidate row on its existing promotion condition, untouched.

## Acceptance criteria

- [ ] AC1 A `workflow_run` subscriber whose `workflows:` lists the path
      `.github/workflows/R-CMD-check.yaml` is on the probe repo's default
      branch, and M101's unparseable-YAML case is re-driven there; for that
      driving push, whether each subscriber produced a run is recorded with the
      driving run's URL, read from
      `gh run list -R jmgirard/gha-startup-failure-probe`.
- [ ] AC2 `.github/workflows/master-red-alert.yaml`'s header comment states, for
      the case-(b) outcome, which of the two explanations M101 left open the
      result rules out and which it leaves open, carrying the driving run URL
      and the date measured.
- [ ] AC3 At least one deliberately-constructed case aimed at a
      `startup_failure` conclusion is driven on the probe repo; for every run
      that driving produced, enumerated from
      `gh run list -R jmgirard/gha-startup-failure-probe` over the driving
      window, the conclusion actually reached is recorded with its run URL, and
      where any reached `startup_failure`, whether an alert run was created for
      it.
- [ ] AC4 The ROADMAP lineage row's items (b) and (c) each carry their measured
      disposition — answered, or narrowed to a restated remainder with its
      promotion condition; item (a) is byte-unchanged.
- [ ] AC5 Every probe-repo run URL cited on the branch resolves: each URL
      matched by `grep -o 'https://github.com/jmgirard/gha-startup-failure-probe/actions/runs/[0-9]*'`
      over the branch's changed files returns a run from `gh api`.
- [ ] AC6 `cairn/PROFILE.md`'s `verify` slot clean, and its consistency-gate
      checks clean at review.

## Coverage

- AC1 → T1, T2
- AC2 → T6
- AC3 → T4
- AC4 → T7
- AC5 → T8
- AC6 → T8

## Tasks

- [ ] T1 On the probe repo's default branch, add a second `workflow_run`
      subscriber (`path-match-probe.yaml`) whose `workflows:` lists
      `.github/workflows/R-CMD-check.yaml` and whose job is a no-op `echo`;
      confirm it is on the default branch before driving anything.
- [ ] T2 Replace the probe's `R-CMD-check.yaml` with the M101 unparseable-YAML
      case, push to the default branch, and capture the full run list for that
      push — the driving run, and a run or no run for each of the two
      subscribers.
- [ ] T3 Restore the probe's `R-CMD-check.yaml` to a valid succeeding file and
      confirm it goes green before the next case.
- [ ] T4 Construct and drive candidate `startup_failure` cases (start with a
      top-level `uses:` naming a nonexistent reusable workflow); for each,
      record the conclusion actually reached and whether an alert run appeared.
- [ ] T5 Restore the probe repo: valid succeeding `R-CMD-check.yaml`, and remove
      `path-match-probe.yaml`; leave the repo public so cited URLs resolve.
- [ ] T6 Write the measured outcomes into `.github/workflows/master-red-alert.yaml`'s
      header comment, per case, with run URLs and the date measured — claiming
      only what the driven cases show, in the register M101's header already uses.
- [ ] T7 Update the ROADMAP lineage row's items (b) and (c) to their measured
      dispositions.
- [ ] T8 Verify every cited probe-repo run URL resolves, then run the profile's
      verify and consistency-gate checks.

## Work log

- 2026-08-21: created by /milestone-plan.
- 2026-08-21: criteria audit ran in REDUCED mode (internal tier) and IN-SESSION rather than in a fresh-context reader, because this session carries a standing no-subagent instruction; the auditor authored the criteria, weaker than doctrine intends. Two findings, both fixed before the gate: a universal negative over the header comment's assertions with no enumerating procedure, narrowed to a positive statement of what the header must say; and a criterion binding the probe repo's own validity and visibility, an instrument property, moved to T5.
- 2026-08-21: plan gate chose measure-only over measuring and applying the `workflows:` path fix in the same milestone because the fix would be committed to before its measurement exists and a null result would leave the milestone half-empty; falsified by the path-spelling arm firing and the resulting one-line change proving to need no separate design.
- 2026-08-21: plan gate chose the path-spelling subscriber over an unfiltered `workflow_run` subscriber because an unfiltered subscriber matches its own completion and can retrigger in a loop on a live repo, while the path arm answers the question that changes what we would do; falsified by a loop guard shown to hold, or by the path arm producing no run and leaving delivery undecided.

## Decisions

## Review
