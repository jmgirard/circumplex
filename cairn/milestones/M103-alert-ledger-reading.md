# M103: Record what the alert's per-run ledger implies about its watched-workflow list

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m103-alert-ledger-reading` / https://github.com/jmgirard/circumplex/pull/132

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
what was driven was a separate subscriber file listing one spelling, so no measurement
prices an edit to this alert's own list; the list is left unchanged as a choice not to act
on an untested counterfactual, and the diff stays comment-only (plan gate, 2026-08-22;
rationale narrowed at the T3 audit, decision unchanged). A scheduled sweep as a second
detector → stays its ROADMAP candidate (item (a)). Driving an unfiltered `workflow_run`
subscriber to settle delivery in principle → stays its ROADMAP candidate (item (b)). A
`DECISIONS.md` entry → declined at the plan gate; the reading is a finding, and the
alert's header owns the alert's rationale.

## Acceptance criteria

- [x] AC1 — The header records, for each of the five broken watched-workflow runs that
      ran while the path-spelled subscriber was live (`32545583419`, `32545779577`,
      `32545943649`, `32545999116`, `32546052474`), that a subscriber whose `workflows:`
      value was that run's own reported name produced no run for it; and cites run
      `32545706555` — a valid run in the same window whose reported name was likewise the
      path — as the same-window control that subscriber did match.
- [x] AC2 — Every per-run cell the added text states — a run's reported name, conclusion,
      job count, head sha, or whether it produced a subscriber or alert run — appears in a
      sentence that names that run's id, or whose subject is a back-reference to an
      antecedent sentence naming it. Domain enumerated by
      `git diff master -- .github/workflows/master-red-alert.yaml`, read sentence by sentence.
- [x] AC3 — The recorded reading states these four limits: (i) no run in the window
      reported the conclusion `startup_failure`; (ii) the two spellings were driven as two
      separate subscriber files each listing one spelling, and a single subscriber listing
      both was never driven; (iii) the measurement is on `jmgirard/gha-startup-failure-probe`,
      not this repo; (iv) the path-spelled subscriber's live window — which ledger runs it
      could and could not have matched.
- [x] AC4 — The reading carries its dated observation inline (`— observed YYYY-MM-DD`) on
      the cell values it cites, per the standing-facts-vs-dated-observations rule.
- [x] AC5 — The branch's change to `.github/workflows/master-red-alert.yaml` is
      comment-only: `git diff master` shows no change to the `on:` block or the job `if:`
      expression, and `Rscript tools/check-master-red-alert.R` and
      `Rscript tools/master-red-alert-dryrun.R` each exit 0.
- [x] AC6 — `cairn/ROADMAP.md`'s alert lineage row no longer describes the reading as
      owed, points at the header comment for it, and restates open item (b) as what
      remains unmeasured; `wc -l` and `wc -c` show under 60 lines and under 24,000 bytes.
- [x] AC7 — The profile's `verify` slot is clean: `devtools::test()` PASS and
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
- 2026-08-22: amendment at the review gate, user-selected — Scope `Out:` rationale narrowed from "the measurement says a path-spelled entry would have caught none of the five broken runs" to what the T3 audit left standing: no measurement prices an edit to this alert's own list, so it is left unchanged as a choice not to act on an untested counterfactual. Narrowing only; the decision, the acceptance criteria and the shipped header text are unchanged.
- 2026-08-22: review return 1 (defect) — [O] diff-bug finding 1: the header's "What follows is the LEDGER" paragraph still says the reading is "deliberately NOT recorded here" and "owed by a follow-up milestone", five lines above the reading itself. Status -> in-progress. Also actioned in this return: undated dated-observations outside the provenance preamble's two buckets, the "As run," garble, and the ROADMAP item (b) clause "and under what name" dropped from its bolded title.
- 2026-08-22: review return — AC2 judged NOT MET by a fresh-context [O] reader, which judged the criterion the primary defect: its line-level operational test is unsatisfiable by hard-wrapped prose. Routed to the gated criterion-amendment protocol; AC4's tick withdrawn to be disposed at the same gate. The plan-gate criteria audit ran in reduced mode (internal tier), which omits the satisfiability question — that omission is why this reached review.
- 2026-08-22: amendment return: AC2 — "Every per-run cell the added text states — a run's reported name, conclusion, job count, head sha, or whether it produced a subscriber or alert run — appears in a sentence that names that run's id, or whose subject is a back-reference to an antecedent sentence naming it. Domain enumerated by `git diff master -- .github/workflows/master-red-alert.yaml`, read sentence by sentence."
- 2026-08-22: AC2's amended wording went to two fresh-context [O] readers in reduced mode (internal tier) before being written — the sentence-level draft passed all three questions but was found indeterminate on which sentences fall in the domain, so the one permitted re-entry replaced "asserting run behaviour" with the five named cell types; the second reader passed all three questions and judged it satisfiable. Further churn on AC2 goes to the user.
- 2026-08-22: mini gate — AC4 HELD as written at the user's selection rather than widened to cover every claim (the return-adjacent direction rule's recommended option); the four uncovered dated observations are repaired in the text instead, and the preamble now carries the `— observed 2026-08-22` form AC4 names.
- 2026-08-22: return-1 fixes — (1) the ledger preamble no longer says the reading is "deliberately NOT recorded here"; it now says M102 recorded the ledger alone and names the reading that follows. (2) The provenance preamble states three claim classes, covering run-level cells, subscriber-run outcomes, the subscriber file's tree presence and commit range and its own text, and the inherited M101/M102 equivalence claims. (3) "As run," became "In the state they ran," — the load-bearing comma is gone. (4) The "So:" sentence and the two head-sha claims now name 32545706555 and fold the commit-range fact into the id-naming bullets; the `startup_failure` limit back-references "the nine runs listed above". (5) ROADMAP item (b) has "and under what name" restored to its bolded title.
- 2026-08-22: everything from `on:` to EOF is byte-identical to master (`diff` on that span, empty), so the alert's executable half is untouched; both audit scripts exit 0.
- 2026-08-22: verify slot after the return-1 fixes — `devtools::test()` FAIL 0 | WARN 5 | SKIP 3 | PASS 8395; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes (14m40s). Status -> review; the Review section's prior evidence lines are superseded by the re-review that follows, since the text they cite has changed.
- 2026-08-22: correcting the T4 and T5 lines above — both record "62 added lines ... 0 deletions" for the workflow diff, true when written and stale after the return-1 fixes; the branch diff is now 72 added / 5 deleted, still 0 non-comment changed lines, with `on:` to EOF byte-identical to master ([O] round-2 finding 7).
- 2026-08-22: gate fixes applied at the user's selection, all comment-only — provenance preamble rebuilt as three explicitly-scoped classes covering M101's run 32540622138 and the watched file's declared-`name:` property, with one unsplit `— observed 2026-08-22`; the summary sentence names its five ids inline; the `startup_failure` limit names its prose antecedents; the never-measured clause distinguishes the documented default-branch-version rule from what this experiment measured; the ledger tail says "the reading below" rather than deferring to a future milestone. `on:` to EOF re-verified byte-identical; both audits exit 0.

## Decisions

## Review

**Round 1 (SUPERSEDED — the text these lines cite was changed by the return-1 fixes; round 2 below is the operative evidence).** Evidence gathered 2026-08-22 on branch `m103-alert-ledger-reading` at c1f57bcb; PR #132 (draft).

- **AC1 — met.** All five broken run ids (32545583419, 32545779577, 32545943649, 32545999116, 32546052474) and the control 32545706555 with its subscriber run 32545711782 each appear twice in `.github/workflows/master-red-alert.yaml` (once in the M102 ledger, once in the reading). The claim sentence reads: "for each of those five broken runs, a subscriber listing that run's own reported name produced no run for it, while a valid run reporting the same name in the same window did produce one."
- **AC2 — NOT TICKED, referred to review.** The added text carries ids on every per-run cell, but two sites assert run behaviour without an id on the asserting line: "all three are `success` with 1 job" (its own sentence names all three ids) and the "So:" summary sentence (its five ids are listed four lines above). Whether AC2 as written is met turns on reading it claim-wise or line-wise; the criterion is not reinterpreted here — the [O] diff-bug reviewer was asked for an explicit MET/NOT-MET verdict and the disposition is taken at the gate.
- **AC3 — met.** All four limits present, one grep hit each: the `startup_failure` limit; "a SEPARATE file listing ONE spelling"; "the measurement is on jmgirard/gha-startup-failure-probe, not this repo"; and the presence window ("entered the probe's default-branch tree at probe commit d0cc1cd ... until bbf43b2 removed it"), with the three outside-the-comparison runs and the two unseparated boundary cases stated.
- **AC4 — met.** The dated observation is inline on the cited cell values at the head of the reading: "was re-read from the GitHub REST API, observed 2026-08-22", scoped explicitly to run-level cells with the probe-equivalence claims marked as inherited from M101/M102 rather than re-measured.
- **AC5 — met.** `git diff master -- .github/workflows/master-red-alert.yaml`: 62 added lines, 0 of them non-comment, 0 deletions — so the `on:` block and job `if:` are byte-identical to master. `Rscript tools/check-master-red-alert.R` exit 0; `Rscript tools/master-red-alert-dryrun.R` exit 0 (5/5 synthetic payloads reduce to the committed template).
- **AC6 — met.** `cairn/ROADMAP.md` no longer contains "Reading that ledger is OWED"; the row now reads "**recorded 2026-08-22 by M103**, in the same header beneath the ledger" and restates item (b) as "M103's reading shows only that no subscriber listing such a run's own reported name produced a run; whether an event existed is undecided." `wc -l` 59 (< 60), `wc -c` 23,640 (< 24,000).
- **Consistency gate — universal.** `cairn_validate` exit 0, no failed checks. No `DESIGN.md` principle changed (`Principles touched: —`), so `cairn_impact` is skipped by its own condition.
- **Consistency gate — toolchain (`r-package`).** Master watches: newest verdict-reaching push run on master is `success` for both `R-CMD-check.yaml` (32552061475) and `test-coverage.yaml` (32552061423), both at 6ec6816. Alert audits both exit 0 (above). NEWS.md: no entry owed — the milestone's only shipped change is comment text in a CI workflow plus tracking, with no user-visible behaviour. No new top-level files.

### Findings and triage (three fresh-context reviewers, 2026-08-22)

Routing: full three-lens fan-out — the declared tier is internal but the diff touches a workflow YAML, which is executable surface, so the docs-only single-reviewer path did not apply.

**[O] diff-bug.** Independently re-verified every cited cell against the live API; all true. Findings:
1. **FIX — RETURN TRIGGER.** "The paragraph immediately above the new text now contradicts it and was not updated (`.github/workflows/master-red-alert.yaml`, the "What follows is the LEDGER" block): it still reads "What it implies for the `workflows:` list below is deliberately NOT recorded here … so the reading is owed by a follow-up milestone" — and the reading now follows it directly, so a reader of the shipped header is told the deliverable is absent one line before encountering it. The diff has zero deletions, which is how this survived; AC5 forbids touching the `on:` block and the job `if:`, not editing a stale comment, so the fix is in scope." Verified at the source: the paragraph reads exactly as quoted. This is the M56-family shape — a change making a fact newly false strands prose elsewhere.
2. **FIX.** "Four claims in the added text fall outside the provenance preamble's own two buckets … the subscriber's whole-history run count, the d0cc1cd–bbf43b2 presence window and strict-between commit membership, the negative "produced no run from that subscriber, and no alert run either," and the `path-match-probe.yaml` file properties (no job `if:`, own name, empty `permissions:`) belong to neither — all are true and all are dated observations carrying no date."
3. **DISPOSE AT THE AMENDMENT GATE.** "AC4 is checked but its literal form is not in the shipped text: it requires the dated observation inline as `— observed YYYY-MM-DD` "on the cell values it cites," and what shipped is a single comma-form ", observed 2026-08-22" in a blanket preamble." AC4's tick is withdrawn pending that gate.
4. **FIX.** ""The window's other three runs sit outside that comparison. As run, 32545535964, 32545892860 and 32546138873 each reported…" is a garbled construction whose comma is load-bearing — drop or misread that comma and the sentence parses as "As run 32545535964, …" and asserts something different."
5. **REJECTED, with reason.** "The T5 work-log gate line misreports the ROADMAP byte count as 23,645 where the committed file is 23,640." Not a misreport: 23,645 was correct when measured; the file then lost exactly 5 bytes when the status mirror went `in-progress` → `review` (11 chars → 6). The figure is stale by one status transition, not wrong at its stated time.
6. **REJECTED.** Past-tense ROADMAP row against a `review` status — normal for a pre-merge branch, self-correcting on merge, as the finding itself notes.
7. **NOT A FINDING.** AC2/AC7 unticked — the state of the mid-review checkpoint the reviewers were convened to close.

**[S] blame-history.** Re-queried all nine runs independently; every cell matched. One finding actioned:
2. **FIX.** "ROADMAP item (b)'s bolded title dropped the clause "and under what name." … Flagging only because a past milestone's specific phrase quietly disappeared from the place it had lived since M99/M101." Its finding 1 (the milestone's own gate incomplete) is **REJECTED** as an artifact of reviewing the mid-review checkpoint; findings 3–7 are its own non-findings, all confirming.

**[S] prior-PR-comments.** No live findings. `gh api repos/jmgirard/circumplex/pulls/comments?per_page=1` returned `[]`, so the per-PR walk was skipped and `cairn/milestones/archive/` was the evidence base. It confirms the two mistakes M101's and M102's reviews taught — asserting non-delivery, and the "RULED OUT" overclaim — are both actively avoided in the shipped text.

### AC2 verdict and disposition

The [O] reviewer was asked for an explicit verdict and returned **NOT MET**, judging the criterion the primary defect: "AC2's second sentence is its operational test and it is line-level … Applied as written to the 62 added lines, the failures are far broader than the two sites you named … because the prose is hard-wrapped at ~76 columns and an id lands on whichever line the wrap put it on. No readable prose can satisfy that test without repeating a 12-digit id on nearly every line." At claim level it splits the two sites: site (i) MET ("all three" is bound by the three ids named in its own subject), site (ii) NOT MET ("a back-reference is not naming").

This is the M114 shape — a criterion unsatisfiable as written — and the plan-gate criteria audit ran in **reduced** mode (internal tier), which by rule omits the satisfiability question. That omission is what let it through.

**Disposition: amendment return on AC2**, per the never-reinterpret rule, plus a defect return on [O] finding 1. Status → `in-progress`; the amendment and the four actioned fixes are the work convened; review stops here and re-runs after.

### Round 2 evidence (2026-08-22, at 27f2f524, PR #132)

Every criterion re-executed against the repaired text; the [O] diff-bug reviewer independently re-verified every cited cell against the live GitHub API and returned an explicit MET verdict on AC1–AC6.

- **AC1 — met.** Five broken ids appear together in the second bullet with "produced no run from that subscriber, and no alert run either"; 32545706555 is cited as the matched same-window control in the first bullet and again by name in the "So:" sentence; each id resolves twice in the file (ledger + reading), 32545706555 three times.
- **AC2 — met.** Reviewer walked the added text sentence by sentence: the provenance preamble states cell *types* not cells (out of domain); each bullet names its ids inline with their cells; "In the state they ran, 32545535964, 32545892860 and 32546138873 …; all three are `success` with 1 job" names all three in one sentence; the 6422872, boundary-pair and 32540622138 sentences each name their ids. Two sites pass on a reading rather than mechanically — logged as findings 3 and 4 below.
- **AC3 — met.** All four limits located: the `startup_failure` limit; "a SEPARATE file listing ONE spelling"; "the measurement is on jmgirard/gha-startup-failure-probe, not this repo"; and the presence window d0cc1cd→bbf43b2 with the six in-range runs, 6422872 in-tree and silent, and the two boundary runs marked unseparable.
- **AC4 — met as written.** The em-dash form appears twice — "was re-read from the GitHub REST API — observed 2026-08-22" and "and its own text — observed 2026-08-22" — with the subject enumerating exactly the cell values cited. The reviewer states the round-1 objection "no longer holds" against AC4's literal words.
- **AC5 — met.** 72 added / 5 deleted on the workflow, 0 non-comment changed lines; `diff` of master's `on:`-to-EOF span against HEAD's is empty, so the executable half is byte-identical. Both audit scripts exit 0.
- **AC6 — met.** "OWED" absent; the row reads "**recorded 2026-08-22 by M103**, in the same header beneath the ledger", item (b) restated with "and under what name" restored and the open count corrected to three. 59 lines, 23,661 bytes.
- **AC7 — met.** `devtools::test()` FAIL 0 | WARN 5 | SKIP 3 | PASS 8395; `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes (14m40s), run after the return-1 fixes on this tree.
- **Consistency gate — universal.** `cairn_validate`: all checks passed, 47 advisories (M7's pre-M28 work-log WARNs). No principle changed, so `cairn_impact` no-ops by its own condition.
- **Consistency gate — toolchain (`r-package`).** `options(cli.width = 500); devtools::document()`: no diff, zero `resolve link` lines. `pkgdown::check_pkgdown()`: no problems found. Master watches green (32552061475 / 32552061423 at 6ec6816). Both alert audits exit 0. No NEWS entry owed — no user-visible behaviour changes. No new top-level files.

### Round 2 findings and triage (three fresh-context reviewers, 2026-08-22)

Full three-lens fan-out again (workflow YAML is executable surface). **[S] blame-history: no defects found** — the rewritten M102 paragraph preserves the two-returned-attempts history, the 0b8863f7 byte-identity fact is correctly attributed to M101 (verified by `git log -S`), the ROADMAP row drops nothing, no D-entry bears on the change. **[S] prior-PR-comments: no regressions** — the probe `gh api repos/jmgirard/circumplex/pulls/comments?per_page=1` returned `[]` so the archive was the evidence base; each round-1 finding traced into the current text as fixed, and neither M101's non-delivery overstatement nor M102's "RULED OUT" overclaim has returned. **[O] diff-bug: MET on AC1–AC6** after independent API re-verification of every cell, with eight findings:

1. **FIXED.** "Provenance class 1 over-reaches past what T1 recorded, and one cell falls outside all three classes … the text also asserts run-level cells for `32540622138`; and that sentence's other half, "declared `name:`" … belongs to none of the three declared classes." Both cells were in fact re-read from the API this session; the preamble now scopes class 1 to every run cited (naming 32540622138) and class 2 to the watched file's declared-`name:` property.
2. **FIXED.** "The "never measured" limit sits against a mechanism the header elsewhere states as standing fact … a reader meets an asserted rule and then an "unknown" for what looks like the same question." The clause now says the documented rule settles which VERSION runs, not whether a file added or removed by the triggering push is live for that push's own event.
3. **FIXED.** "AC2's weakest site is the `startup_failure` sentence, whose antecedent "the nine runs listed above" resolves to ledger lines that carry ids inside URLs rather than to a prose sentence naming them." Now "the six in the two bullets and the three that reported the declared name".
4. **FIXED.** "AC2's "So:" sentence carries its back-reference in a fronted adjunct, not in its grammatical subject." The five ids are now named inline in that sentence.
5. **FIXED.** "The ledger tail still speaks of the reading prospectively." Now "which the reading below inherits".
6. **FIXED.** "The milestone file's Review section is superseded but still asserts verdicts on text that has since changed." Round 1's block is marked SUPERSEDED in place.
7. **FIXED by appended correction.** "T4/T5 work-log figures are stale … the branch diff is now 72 added / 5 deleted." Work logs are append-only, so a correcting line was appended rather than the originals edited.
8. **NOT A DEFECT.** "AC2, AC4 and AC7 remain unticked" — bookkeeping, done in the round-2 evidence block above.

Also fixed at the gate, found by the reviewing session rather than a reviewer: the first `— observed 2026-08-22` was split across a line break, hiding it from a line-based grep — the trap `LESSONS.md` records twice. The preamble now carries one unsplit instance.

**Post-fix re-verification.** `on:` to EOF byte-identical to master (`diff` on that span, empty); 0 non-comment changed lines; `Rscript tools/check-master-red-alert.R` and `Rscript tools/master-red-alert-dryrun.R` both exit 0; `cairn_validate` all checks passed. AC7's evidence stands unchanged: `git diff --name-only master..HEAD -- R/ src/ tests/ man/ DESCRIPTION NAMESPACE` is empty, so the gate fixes cannot move `test()` or `check()`.
