# M103: Record what the alert's per-run ledger implies about its watched-workflow list

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m103-alert-ledger-reading`

## Goal

Write into `.github/workflows/master-red-alert.yaml`'s header the reading of M102's
per-run ledger that M102 banked the measurement for but deliberately left unrecorded.

## Scope

Deliverable tier: **internal** — the master-red alert is repo-internal CI tooling; no
external consumer of the package relies on it.

**In:** deriving, from the committed ledger and a fresh re-read of the cited runs, what
the M102 window shows about whether a `workflows:`-side change would have caught the
window's broken runs; recording that reading and the limits bounding it in the alert's
header comment; shrinking the ROADMAP lineage row to a pointer.

**Out:** changing the alert's `workflows:` list or any executable line of the workflow —
the measurement says a path-spelled entry would have caught none of the five broken runs,
so the diff stays comment-only (plan gate, 2026-08-22). A scheduled sweep as a second
detector → stays its ROADMAP candidate (item (a)). Driving an unfiltered `workflow_run`
subscriber to settle delivery in principle → stays its ROADMAP candidate (item (b)). A
`DECISIONS.md` entry → declined at the plan gate; the reading is a finding, and the
alert's header owns the alert's rationale.

## Acceptance criteria

- [ ] AC1 — The header records, for each of the five broken watched-workflow runs that
      ran while the path-spelled subscriber was live (`32545583419`, `32545779577`,
      `32545943649`, `32545999116`, `32546052474`), that a subscriber whose `workflows:`
      value was that run's own reported name produced no run for it; and cites run
      `32545706555` — a valid run in the same window whose reported name was likewise the
      path — as the same-window control that subscriber did match.
- [ ] AC2 — Every claim the added text makes about a run's behaviour names the run id it
      rests on. Domain enumerated by `git diff master -- .github/workflows/master-red-alert.yaml`:
      each added line asserting run behaviour carries an id from that diff hunk.
- [ ] AC3 — The recorded reading states these four limits: (i) no run in the window
      reported the conclusion `startup_failure`; (ii) the two spellings were driven as two
      separate subscriber files each listing one spelling, and a single subscriber listing
      both was never driven; (iii) the measurement is on `jmgirard/gha-startup-failure-probe`,
      not this repo; (iv) the path-spelled subscriber's live window — which ledger runs it
      could and could not have matched.
- [ ] AC4 — The reading carries its dated observation inline (`— observed YYYY-MM-DD`) on
      the cell values it cites, per the standing-facts-vs-dated-observations rule.
- [ ] AC5 — The branch's change to `.github/workflows/master-red-alert.yaml` is
      comment-only: `git diff master` shows no change to the `on:` block or the job `if:`
      expression, and `Rscript tools/check-master-red-alert.R` and
      `Rscript tools/master-red-alert-dryrun.R` each exit 0.
- [ ] AC6 — `cairn/ROADMAP.md`'s alert lineage row no longer describes the reading as
      owed, points at the header comment for it, and restates open item (b) as what
      remains unmeasured; `wc -l` and `wc -c` show under 60 lines and under 24,000 bytes.
- [ ] AC7 — The profile's `verify` slot is clean: `devtools::test()` PASS and
      `devtools::check(args = "--no-manual")` Status OK.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3, T4
- AC3 → T2, T3
- AC4 → T1, T3
- AC5 → T3, T5
- AC6 → T4, T5
- AC7 → T5

## Tasks

- [x] T1 — Re-read every run the reading will cite from the GitHub REST API
      (`gh api repos/jmgirard/gha-startup-failure-probe/actions/runs/<id>` for `name`,
      `conclusion`, `path`; the run's jobs endpoint for job count), and confirm the window
      holds exactly the nine push runs the ledger lists
      (`gh api ".../actions/runs?event=push"`, filtered by `created_at`). Record the
      observation date. Any disagreement with the committed ledger stops the milestone and
      is reported, not written around.
- [x] T2 — Derive the reading from T1's values: the discrimination the path subscriber's
      live window supports, and the four limits of AC3. Draft it in the milestone file
      first, not in the workflow, and map each drafted sentence to the run id or recorded
      limit licensing it (the mapping is the gate procedure for AC2, kept in the work log).
- [x] T3 — Hand the drafted reading and the ledger to a fresh-context [O] reader that
      authored neither, asking of each sentence which ledger row licenses it and whether
      it claims more than that row carries. Two prior attempts at this reading were
      returned for overstatement, so this runs before the text reaches the workflow file.
- [x] T4 — Write the surviving text into `.github/workflows/master-red-alert.yaml`'s
      header immediately after the ledger, and shrink the ROADMAP lineage row to a pointer,
      restating item (b) as what stays unmeasured. Re-read both aimed sites after the edit.
- [x] T5 — Gate: `git diff master` on the workflow shows comment-only change, both alert
      audit scripts exit 0, `wc -l`/`wc -c` on the ROADMAP under both caps, profile verify
      slot clean, `cairn_validate` clean.

## Work log

- 2026-08-22: created by /milestone-plan.
- 2026-08-22: criteria audit ran in REDUCED mode (internal tier), fresh-context [O] reader; returned two findings — AC2 and AC4 each bound an instrument property, AC4 additionally disproportionate (live-API set equality across an environment boundary); both fixed before writing, AC2 narrowed to a property of the added text with the sentence-to-evidence mapping moved to T2, AC4 narrowed to the reading's dated observation with the API re-read moved to T1.
- 2026-08-22: plan gate chose recording the reading with no change to the alert's `workflows:` list over adding the file-path spelling, because the window's own control shows a path-spelled subscriber matched a valid run and none of the five broken ones; falsified by a broken zero-job run that a path-spelled subscriber does match.
- 2026-08-22: plan gate chose the header comment plus the ROADMAP row over a `DECISIONS.md` entry, because the reading is a finding about evidence rather than a choice and the header already owns the alert's rationale; falsified by a later milestone needing the reading to bind a decision outside the alert file.
- 2026-08-22: plan gate chose re-reading the cited runs from the live API over trusting the committed ledger, because the prose failed twice on overstatement and fresh observation is cheap here; falsified by the API no longer resolving the probe's runs.
- 2026-08-22: T1 — re-read all nine window runs via `gh api` (`name`, `path`, `conclusion`, jobs `total_count`); every field matches the committed ledger, no disagreement. `?event=push` reports exactly those nine in 02:10:55Z–02:23:56Z. `?event=workflow_run` reports one `path-match-probe.yaml` run in the window (32545711782, from 32545706555) and four `master-red-alert.yaml` runs, all from the three declared-name runs plus one; none from any broken run. Path subscriber live 02:10:51Z (probe commit d0cc1cd) to 02:23:53Z (bbf43b2), covering runs 2-8. Observed 2026-08-22.

- 2026-08-22: correcting the T1 line above — the API reports SIX `master-red-alert.yaml` runs in the probe's history, of which THREE fall in the M102 window (heads d0cc1cd, 6422872, bbf43b2); the other three are M101's earlier window. The line's "four" was wrong; nothing else in it changes.
- 2026-08-22: T2 — drafted in the session scratchpad rather than the milestone file (task wording said the milestone file; a multi-line draft cannot live in an append-only one-line work log) — minor deviation, the text lands in the workflow at T4.
- 2026-08-22: T2 — sentence-to-evidence map for the added text (AC2's gate procedure): presence window <- probe commits d0cc1cd/bbf43b2 plus the nine head shas; matched cell <- 32545706555 producing 32545711782; unmatched cells <- 32545583419, 32545779577, 32545943649, 32545999116, 32546052474; outside-the-comparison cells <- 32545535964, 32545892860, 32546138873; mechanism refusal <- M101's 32540622138; the three refusals and the two closing limits restate limits already recorded (M102 ledger; ROADMAP item (b)) and assert no run behaviour of their own.
- 2026-08-22: T3 — three adversarial rounds, each a fresh-context [O] reader that had seen no earlier draft. Round 1 (ten findings) killed the central overstatement: the draft had slid from "this one-spelling subscriber matched none of the five" to "adding the path spelling to the alert's list would have caught none of the five" — a counterfactual no cell prices. Round 2 (six) fixed provenance overreach, an asymmetric claim about the subscriber's add/remove boundaries, and a sha attributed to the wrong repo. Round 3 (four) required the five broken runs' head shas be shown inside the presence window and re-attributed the byte-identity record from M102 to M101. Round 4 returned SOUND AS WRITTEN with every cell independently re-verified.
- 2026-08-22: T4 — reading written into `.github/workflows/master-red-alert.yaml` immediately beneath the ledger (62 added lines, every one a comment; zero deletions, so `on:` and the job `if:` are byte-identical to master), and the ROADMAP lineage row struck to a pointer with item (b) restated. Both aimed sites re-read after the edit.
- 2026-08-22: T5 — gate clean. `git diff master` on the workflow: 62 added lines, all comments, 0 deletions. `Rscript tools/check-master-red-alert.R` and `Rscript tools/master-red-alert-dryrun.R` both exit 0 (dry-run: 5/5 synthetic payloads reduce to the template). `cairn/ROADMAP.md` 59 lines / 23,645 bytes. `devtools::test()` FAIL 0 | WARN 5 | SKIP 3 | PASS 8395. `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes. `cairn_validate` 0 failed checks.
- 2026-08-22: status -> review. OPEN FOR THE GATE: the Scope `Out:` clause still justifies the no-code-change decision with the counterfactual the T3 audit rejected ("a path-spelled entry would have caught none of the five broken runs"). The decision is unchanged and the header text does not repeat it; only the plan-owned rationale overclaims, and it is amend-via-gate.

## Decisions

## Review
