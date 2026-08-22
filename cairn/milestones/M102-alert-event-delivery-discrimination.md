<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M102: Separate a filtered-out alert event from one never delivered

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m102-alert-event-delivery-discrimination`

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

- [x] T1 On the probe repo's default branch, add a second `workflow_run`
      subscriber (`path-match-probe.yaml`) whose `workflows:` lists
      `.github/workflows/R-CMD-check.yaml` and whose job is a no-op `echo`;
      confirm it is on the default branch before driving anything.
- [x] T2a Positive control for the subscriber itself, isolating name
      resolution from file validity: make the probe's `R-CMD-check.yaml` VALID
      but drop its `name:` declaration, so GitHub resolves the run's name to
      the path; push and record whether the path subscriber fired. Without
      this, a silent subscriber in T2 cannot be told from a subscriber that
      never works.
- [x] T2 Replace the probe's `R-CMD-check.yaml` with the M101 unparseable-YAML
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
- 2026-08-21: implement started; branch cut from master at bd08b980.
- 2026-08-21: implement gate chose a bounded set of three `startup_failure` constructions over an open-ended hunt (a null result is itself recordable), and removing the extra subscriber from the probe repo afterward over leaving it in place.
- 2026-08-21: verified before driving anything — the probe repo's alert copy is still functionally current: its `on:` block is byte-identical to this repo's and its job `if:` differs only in line numbers.
- 2026-08-21: T1 done — `path-match-probe.yaml` pushed to the probe repo's default branch (probe commit d0cc1cd), listing only `.github/workflows/R-CMD-check.yaml`; `gh workflow list` shows it active. That push's own valid R-CMD-check run (32545535964, `success`) produced NO path-subscriber run, as expected for a run whose name resolves to the declared name.
- 2026-08-21: T2a first attempt was itself unparseable and must not be read as its cell — a plain YAML scalar cannot contain ": " and the `run:` value held one; identity verified by `yaml::read_yaml`, a scanner error at line 12 column 31, and the run (32545583419) concluded `failure` with 0 jobs, the broken-case signature, not the nameless-valid signature intended.
- 2026-08-21: T2a done (corrected) — valid file declaring no `name:`, run 32545706555 concluded `success` with 1 job, name resolved to the PATH; the path subscriber FIRED (run 32545711782, 8s later) and the declared-name subscriber created no run. Positive control established: `workflows:` can match a full path, so a silent path subscriber elsewhere is not a subscriber that never works.
- 2026-08-21: T2 done — M101 case (i) re-driven verbatim from probe commit e6cf376, parse failure re-verified locally before pushing; run 32545779577 concluded `failure` with 0 jobs and NEITHER subscriber produced a run, rechecked 3+ minutes after the driving run against case A's 8-second latency.
- 2026-08-21: minor amendment — added T2a, a positive control that makes the watched file VALID but nameless so GitHub resolves its name to the path. M101's confound was that name and validity varied together; without this control a silent subscriber in T2 is indistinguishable from one that never works. No acceptance criterion changed.
- 2026-08-21: criteria audit ran in REDUCED mode (internal tier) and IN-SESSION rather than in a fresh-context reader, because this session carries a standing no-subagent instruction; the auditor authored the criteria, weaker than doctrine intends. Two findings, both fixed before the gate: a universal negative over the header comment's assertions with no enumerating procedure, narrowed to a positive statement of what the header must say; and a criterion binding the probe repo's own validity and visibility, an instrument property, moved to T5.
- 2026-08-21: plan gate chose measure-only over measuring and applying the `workflows:` path fix in the same milestone because the fix would be committed to before its measurement exists and a null result would leave the milestone half-empty; falsified by the path-spelling arm firing and the resulting one-line change proving to need no separate design.
- 2026-08-21: plan gate chose the path-spelling subscriber over an unfiltered `workflow_run` subscriber because an unfiltered subscriber matches its own completion and can retrigger in a loop on a live repo, while the path arm answers the question that changes what we would do; falsified by a loop guard shown to hold, or by the path arm producing no run and leaving delivery undecided.

## Decisions

- 2026-08-21 (M102, milestone-local): the four-cell result reverses M101's leading explanation, and the reversal — not the alert's configuration — is what this milestone records. M101 read name resolution as the likely reason a broken watched workflow went unalerted, because GitHub reported the broken runs' `name` as the file path while the matched control reported its declared name; name and validity varied together in every M101 cell, so it stood as a correlation. M102 varied them independently. A VALID workflow declaring no `name:` also has its name resolved to the path, and the path-spelling subscriber DID fire for it (driving run 32545706555, subscriber run 32545711782) while the declared-name subscriber created no run — so a path spelling in `workflows:` matches, and name resolution alone does not suppress a subscriber. The unparseable file, whose name resolves to the same path, produced NO run under either spelling (driving run 32545779577). Since the path spelling is exactly the one that would have matched the broken run's reported name and it did not fire, a `workflows:`-side fix is ruled out for the broken case: either no `workflow_run` event is delivered for a run that fails to start, or one is delivered carrying a name different from the one the API reports for that run. Item (a), a scheduled sweep, is left as the only remaining remedy shape.

## Review
