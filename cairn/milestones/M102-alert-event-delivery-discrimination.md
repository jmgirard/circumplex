<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M102: Separate a filtered-out alert event from one never delivered

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m102-alert-event-delivery-discrimination` / https://github.com/jmgirard/circumplex/pull/131

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

- [x] AC1 A `workflow_run` subscriber whose `workflows:` lists the path
      `.github/workflows/R-CMD-check.yaml` is on the probe repo's default
      branch, and M101's unparseable-YAML case is re-driven there; for that
      driving push, whether each subscriber produced a run is recorded with the
      driving run's URL, read from
      `gh run list -R jmgirard/gha-startup-failure-probe`.
- [ ] AC2 `.github/workflows/master-red-alert.yaml`'s header comment states, for
      the case-(b) outcome, which of the two explanations M101 left open the
      result rules out and which it leaves open, carrying the driving run URL
      and the date measured.
- [x] AC3 At least one deliberately-constructed case aimed at a
      `startup_failure` conclusion is driven on the probe repo; for every run
      that driving produced, enumerated from
      `gh run list -R jmgirard/gha-startup-failure-probe` over the driving
      window, the conclusion actually reached is recorded with its run URL, and
      where any reached `startup_failure`, whether an alert run was created for
      it.
- [ ] AC4 The ROADMAP lineage row's items (b) and (c) each carry their measured
      disposition — answered, or narrowed to a restated remainder with its
      promotion condition; item (a) is byte-unchanged.
- [x] AC5 Every probe-repo run URL cited in
      `.github/workflows/master-red-alert.yaml` resolves: each URL matched by
      `grep -o 'https://github.com/jmgirard/gha-startup-failure-probe/actions/runs/[0-9][0-9]*'`
      over that file returns a run from `gh api`.
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
- [x] T3 Restore the probe's `R-CMD-check.yaml` to a valid succeeding file and
      confirm it goes green before the next case.
- [x] T4 Construct and drive candidate `startup_failure` cases (start with a
      top-level `uses:` naming a nonexistent reusable workflow); for each,
      record the conclusion actually reached and whether an alert run appeared.
- [x] T5 Restore the probe repo: valid succeeding `R-CMD-check.yaml`, and remove
      `path-match-probe.yaml`; leave the repo public so cited URLs resolve.
- [x] T6 Write the measured outcomes into `.github/workflows/master-red-alert.yaml`'s
      header comment, per case, with run URLs and the date measured — claiming
      only what the driven cases show, in the register M101's header already uses.
- [x] T7 Update the ROADMAP lineage row's items (b) and (c) to their measured
      dispositions.
- [x] T8 Verify every cited probe-repo run URL resolves, then run the profile's
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
- 2026-08-21: T3 done — baseline restored (valid, declares `name:`); run 32545892860 concluded `success` with 1 job, name resolved to the DECLARED name, and the alert subscriber created a run again (32545896997, `skipped` on `success`). Second positive control, this one for the declared-name spelling.
- 2026-08-21: T4 done — the gate's bounded set of three, each verified to PARSE locally before pushing so none is a repeat of the case-B parse failure: C1 job `uses:` a nonexistent LOCAL reusable workflow (run 32545943649), C2 malformed Actions expression in a job-level `if:` (32545999116), C3 job `uses:` a reusable workflow in a nonexistent EXTERNAL repo (32546052474). All three concluded `failure` with 0 jobs; NONE reached `startup_failure`, and none produced a run under either subscriber. Null result recorded as the finding.
- 2026-08-21: T4 side finding — C1/C2/C3 each PARSE and DECLARE `name: R-CMD-check.yaml`, yet every one of their runs reported `name` as the PATH. The path fallback therefore applies to any run that fails to start, not only to a file too malformed to declare a name, which is broader than M99's original reasoning for the gap.
- 2026-08-21: run enumeration for the whole driving window is `gh run list -R jmgirard/gha-startup-failure-probe` filtered to `createdAt >= 2026-08-22T02:10Z`; it returns exactly the eleven runs the cells account for, so "no subscriber run" is read off an enumeration rather than off an absence noticed by eye.
- 2026-08-21: T5 done — probe repo restored to its two-workflow baseline (`R-CMD-check.yaml` valid/named/green, run 32546138873 `success`, alert run 32546143683 `skipped`), `path-match-probe.yaml` removed, repo still PUBLIC so the cited run URLs resolve.
- 2026-08-21: T6 done — the alert header now carries the six M102 cells with run URLs and the date measured, and states what the result settles. Verified comment-only: `git diff master` on the workflow shows no non-comment line changed, so the `on:` block and job `if:` are byte-identical and `EXPECTED_IF`'s pin is unmoved.
- 2026-08-21: T6 correction before commit — a drafted header line said "all five broken-start runs"; the enumeration shows six (M101's two cells plus M102's four broken cells), so the line was rewritten to derive the count from the cells rather than assert a number.
- 2026-08-21: T7 done — the ROADMAP lineage row carries (b) and (c) answered. Two claims in that row were current knowledge M102 falsified and were fixed in place, not appended to: "a `workflows:`-side fix is not ruled out" and "name resolution is the leading explanation". Item (a) verified byte-identical to master by extracting its 271-byte span from both versions and diffing.
- 2026-08-21: ROADMAP is 23,431 of its 24,000-byte budget after T7 — 569 bytes of headroom, tighter than the 1,106 it had before; flagged for the review hygiene stamp.
- 2026-08-21: SUBSTANTIVE amendment at the mini gate — AC5 was unsatisfiable as written: it swept the branch's changed files for run URLs, and the milestone file quotes AC5's own grep pattern, so the procedure matched its own literal text and returned a bare prefix carrying no run id. Narrowed, not widened: the domain is now `.github/workflows/master-red-alert.yaml` alone and the pattern is `[0-9][0-9]*` so a bare prefix cannot match. Jeff chose the narrowing over a carve-out for the quoting line and over a return to planning. The amended wording was re-asked the reduced-mode audit questions IN-SESSION rather than by a fresh-context reader, the same standing no-subagent deviation recorded at plan.
- 2026-08-21: T8 done — the amended AC5 procedure returns ten URLs from the alert header, all resolving. Beyond AC5's promise, all fifteen run ids cited anywhere on the branch were resolved and each one's conclusion and reported `name` match what the records claim.
- 2026-08-21: T8 caught a header defect before the amendment: an explanatory line written as `.../runs/N` produced a bare URL-prefix match; the six cells were rewritten to carry full URLs, as M101's header already does, so the artifact satisfies the criterion rather than the criterion bending to the artifact.
- 2026-08-21: T8 tooling note — the first run of AC5's sweep used an unquoted `$FILES` in zsh, which does not word-split, so the grep read one nonexistent filename and reported nothing; caught by the shell's own warning and re-run with a `while read` loop. The LESSONS M95-family line already warns of exactly this.
- 2026-08-21: AC6 verify half — `devtools::test()` clean: FAIL 0 | WARN 5 | SKIP 3 | PASS 8395, the same pass count M99 recorded; the branch changes no R code. `tools/check-master-red-alert.R` and `tools/master-red-alert-dryrun.R` both exit 0 (5 synthetic payloads ok), so the `EXPECTED_IF` pin is unmoved by the comment-only diff. `cairn_validate` all checks pass, 47 advisories — the pre-existing M7 multi-line work-log WARNs. Consistency-gate half runs at review.
- 2026-08-21: all tasks checked; status → review.
- 2026-08-21: REVIEW RETURN 1 (defect) — returned to `in-progress`. What failed: F1, the header's "a `workflows:`-side fix is RULED OUT" overclaims what six cells support, since the header's own remaining disjunct (an event delivered under a name the API does not report) is a case where some spelling could still match; and F2, the ROADMAP calls the unfiltered-subscriber variant "moot" and deletes the self-retrigger hazard, contradicting this milestone's own Scope/Out. Eight further record defects actioned, two rejected; full table in the Review section.
- 2026-08-21: correction to the T8 enumeration line above — the window `createdAt >= 2026-08-22T02:10Z` returns THIRTEEN runs, not eleven, and 32545540389 (the alert subscriber's `skipped` run from the T1 push at 02:11:03) is accounted for nowhere in the records. The "no subscriber run" reading for case B is unaffected — 32545540389 predates case B's 02:16:04 driving run — but the completeness claim the line rested on was wrong as written.
- 2026-08-21: return-1 repairs done, all ten actioned findings. Header (F1/F4/F5/F6/F7/F13): the RULED-OUT verdict narrowed to "no spelling derivable from a broken run's REPORTED name closes the gap", with both open possibilities stated and the untested one-subscriber-two-spellings arrangement named; cell 1 now cites driving run 32545892860 AND subscriber run 32545896997 and records the path subscriber's null; the "any run that fails to start" universal bounded to the cells; the `startup_failure` count replaced by a stated derivation that includes the accidental seventh run; the probe-equivalence claim reworded to assert identical `if:` TEXT. ROADMAP (F1/F2/F8/F9): (b) restated as narrowed-not-closed with the unfiltered-subscriber experiment restored as live and its self-retrigger hazard recorded, "moot" removed; (c) un-struck and marked NARROWED with both halves open; the falsified `name:`-only sentence removed outright by the compression below, its substance kept as "M99's premise ... was too narrow". `tools/check-master-red-alert.R` (F10): a refinement paragraph added recording that a workflow declaring no `name:` resolves to the path and that `workflows:` matches that spelling, stating explicitly that the check below is unchanged. Milestone Decisions (F15): the same narrowing, and the entry says why it stays milestone-local. Workflow diff re-verified comment-only; both alert audits still exit 0.
- 2026-08-21: ROADMAP hit 24,440 bytes after the return-1 rewrite — OVER the 24,000 budget. Remedied per the tracking-rules widest-row rule by compressing the lineage row's M99/M101 narration (982 bytes) into a pointer at the alert header and the M101/M102 archives, which own that detail; 23,458 bytes now.
- 2026-08-21: the AC4 item-(a) evidence recorded at the first review gate was weaker than it read — it extracted item (a) with a GREEDY regex running to "alert workflow.", a phrase the rewritten row also contains, so the span compared was not reliably item (a). Re-verified with a bounded extraction from "(a) **Scheduled sweep" to the "(b) " marker: 271 bytes on both sides, identical. The first result was right by luck, not by construction; the re-review must use the bounded form.
- 2026-08-21: verify after the return-1 repairs — the branch touches no package code at all (`git diff --name-only master..HEAD` returns only the alert workflow, `tools/check-master-red-alert.R`, `cairn/ROADMAP.md` and this file; nothing under `R/`, `src/`, `tests/`, `man/` or `data/`). `devtools::test()` FAIL 0 / PASS 8395 and `devtools::check()` Status: OK were measured on this branch before the repairs, and every commit since changed only comments and tracking prose; the re-review re-runs both rather than inheriting them. Both alert audits exit 0 after the repairs, `cairn_validate` all checks pass. Status → review.
- 2026-08-21: REVIEW RETURN 2 (defect) + AMENDMENT RETURN on AC2. Defect: R2-1, the ROADMAP row headline still declares the remainder ANSWERED while item (b) says undecided — introduced by the return-1 compression, the same overclaim class as F1. amendment return: AC2 — wording to be drafted and audited before it is written. Jeff chose DESCOPE over another retry: the interpretation leaves M102.
- 2026-08-21: correction to the F13 repair record — the work-log line above that reads "byte-identical to this repo's and its job `if:` differs only in line numbers" was NOT corrected by the return-1 append, which named only the header. The probe's `if:` is identical in TEXT; only its position in the file differs. (R2-11.)
- 2026-08-21: correction to the ROADMAP byte line above — 23,431 was correct at commit 4203a91f; `wc -c` returns 23,426 at review because the status cell changed from `in-progress` to `review`, five bytes shorter. The figure is a dated measurement, not a standing fact.
- 2026-08-21: minor amendment — added T2a, a positive control that makes the watched file VALID but nameless so GitHub resolves its name to the path. M101's confound was that name and validity varied together; without this control a silent subscriber in T2 is indistinguishable from one that never works. No acceptance criterion changed.
- 2026-08-21: criteria audit ran in REDUCED mode (internal tier) and IN-SESSION rather than in a fresh-context reader, because this session carries a standing no-subagent instruction; the auditor authored the criteria, weaker than doctrine intends. Two findings, both fixed before the gate: a universal negative over the header comment's assertions with no enumerating procedure, narrowed to a positive statement of what the header must say; and a criterion binding the probe repo's own validity and visibility, an instrument property, moved to T5.
- 2026-08-21: plan gate chose measure-only over measuring and applying the `workflows:` path fix in the same milestone because the fix would be committed to before its measurement exists and a null result would leave the milestone half-empty; falsified by the path-spelling arm firing and the resulting one-line change proving to need no separate design.
- 2026-08-21: plan gate chose the path-spelling subscriber over an unfiltered `workflow_run` subscriber because an unfiltered subscriber matches its own completion and can retrigger in a loop on a live repo, while the path arm answers the question that changes what we would do; falsified by a loop guard shown to hold, or by the path arm producing no run and leaving delivery undecided.

## Decisions

- 2026-08-21 (M102, milestone-local): the four-cell result reverses M101's leading explanation, and the reversal — not the alert's configuration — is what this milestone records. M101 read name resolution as the likely reason a broken watched workflow went unalerted, because GitHub reported the broken runs' `name` as the file path while the matched control reported its declared name; name and validity varied together in every M101 cell, so it stood as a correlation. M102 varied them independently. A VALID workflow declaring no `name:` also has its name resolved to the path, and the path-spelling subscriber DID fire for it (driving run 32545706555, subscriber run 32545711782) while the declared-name subscriber created no run — so a path spelling in `workflows:` matches, and name resolution alone does not suppress a subscriber. The unparseable file, whose name resolves to the same path, produced NO run under either spelling (driving run 32545779577). Since the path spelling is exactly the one that would have matched the broken run's reported name and it did not fire, no spelling derivable from a broken run's REPORTED name closes the gap. Corrected at review return 1: that is narrower than "a `workflows:`-side fix is ruled out", which this entry first claimed — two possibilities stay open and nothing measured separates them, (1) no `workflow_run` event is delivered at all for a run that fails to start, or (2) one is delivered carrying a name the run's API record does not show, and under (2) listing that name would itself be a `workflows:`-side fix. The probe also never drove ONE subscriber listing both spellings, the arrangement a real fix would use. Item (a), a scheduled sweep, is therefore the remaining remedy shape that needs none of this resolved — not the only remedy possible. This entry stays milestone-local: it constrains no future milestone's design while its own verdict is this provisional.

## Review

**Round 1 — WITHDRAWN by review return 1. The three verdicts below quote text
that no longer exists (AC2's "RULED OUT", AC4's "both are ANSWERED there", AC5's
ten URLs). Kept as the record of a returned round, superseded by Round 2 below;
not a current verdict on any criterion.**

**Evidence gathered fresh at review 2026-08-21; commands re-run, never recalled.**

- **AC1 — met.** `path-match-probe.yaml`, whose `workflows:` lists only
  `.github/workflows/R-CMD-check.yaml`, landed on the probe repo's default
  branch at probe commit `d0cc1cd`; `git merge-base --is-ancestor d0cc1cd
  758ab1b` confirms it preceded the case-B commit, and `git branch -r
  --contains d0cc1cd` shows it on `origin/main`, the probe's default branch.
  M101's unparseable case was re-driven verbatim as run 32545779577. For that
  driving push `gh run list` over the window returns no run for either
  subscriber — the next run in the enumeration is 32545892860, a later push.
- **AC2 — met.** The alert header carries "So a `workflows:`-side fix is RULED
  OUT for a run that fails to start" with the alternative it leaves open
  ("Either no `workflow_run` event is delivered … or one is delivered carrying
  a name different from the one the API reports"), the driving run URL, and
  "measured 2026-08-21 local, the cited runs stamped 2026-08-22 UTC".
- **AC3 — met.** Three constructions driven: C1 nonexistent LOCAL reusable
  workflow (32545943649), C2 malformed job `if:` expression (32545999116), C3
  nonexistent EXTERNAL reusable workflow (32546052474). Each conclusion is
  recorded with its run URL; all three concluded `failure` with 0 jobs and none
  reached `startup_failure`, so the clause conditioned on that conclusion is
  vacuous rather than unmet. The run enumeration is the `gh run list` window
  the criterion names.
- **AC4 — met.** The lineage row carries "both graduated 2026-08-21 → M102 and
  both are ANSWERED there" with (b)'s ruled-out disposition and (c)'s null
  result plus its reopening condition. Item (a) verified byte-identical to
  master: its 271-byte span extracted from both versions and compared with
  `cmp`, no difference.
- **AC5 — met.** The amended procedure — `grep -o` for the run-URL pattern with
  `[0-9][0-9]*` over `.github/workflows/master-red-alert.yaml` — returns ten
  URLs, and every one resolves via `gh api` (three from M101, seven from M102).

**Independent review 2026-08-21 — three fresh-context readers, full fan-out**
([O] diff-bug, [S] blame-history, [S] prior-PR-comments). Jeff lifted the
session's standing no-subagent constraint for this step at the review gate.

- **[S] prior-PR-comments: clean.** Its GitHub probe found no real inline review
  threads on this repo, so archived `## Review` sections were the primary
  surface, as doctrine expects. It judged the diff to APPLY the M101 and M99
  lessons rather than repeat them, and reported no regression.
- **[S] blame-history: no silent regressions.** Confirmed the deleted M101 prose
  is explicitly superseded rather than dropped, M101's archive is untouched
  (history not edited), item (a) byte-identical, and the `on:`/`if:` unmoved.
  Its AC6 finding is mid-review state, not a defect. Its byte-count finding is
  F14 below.
- **[O] diff-bug: fifteen findings; ten actioned, five rejected.** Jeff chose a
  return over a gate-side patch. Verified against the implementation, not the
  reviewer's account: the window enumeration returns 13 runs, not 11;
  32545892860 is the driving run, not the subscriber run (32545896997); the
  falsified `name:`-only sentence is still readable unmarked in the ROADMAP row;
  `tools/check-master-red-alert.R:112` carries the same partial fact.

**Findings and dispositions**

| # | Finding | Disposition |
|---|---|---|
| F1 | "RULED OUT" overclaims — the header's own open disjunct ("delivered carrying a name different from the one the API reports") is a case where a `workflows:` spelling could still match | FIX — narrow to what the cells support |
| F2 | ROADMAP calls the unfiltered-subscriber variant "moot" and deletes the self-retrigger hazard, contradicting M102's Scope, which promised the question returns to the row with that hazard recorded | FIX — restore the remainder and the hazard |
| F3 | Work log claims the window enumerates "eleven runs"; it returns thirteen, with 32545540389 unaccounted | FIX — append a correction (work log is append-only) |
| F4 | Header cites 32545892860 as the declared-name subscriber's run; it is the driving run, and 32545896997 is uncited | FIX |
| F5 | "Any run that fails to start reports the path" — unhedged universal from six runs | FIX — bound to the cells |
| F6 | Construct-validity gap: the probe used two one-spelling subscribers; a real fix would be one subscriber listing both | FIX — acknowledge explicitly |
| F7 | "six runs" omits 32545583419, a seventh measured broken-start run; and a hand-pinned count should be a stated derivation (M87) | FIX |
| F8 | Item (c) struck through as ANSWERED while carrying a reopen condition — AC4's own wording makes it "narrowed" | FIX |
| F9 | The falsified "matches a declared `name:`, which an unparseable file cannot declare" left readable unmarked in the ROADMAP row | FIX in place |
| F10 | `tools/check-master-red-alert.R:112` states the same now-partial fact | FIX — comment-only, within measure-only |
| F13 | "differs only in line numbers" — an `if:` expression has no line numbers | FIX — assert text-identical, position differs |
| F14 | Recorded 23,431 bytes does not re-derive (23,426 now) | Accepted, no history edit: 23,431 was correct at commit 4203a91f; the 5-byte drop is the status cell `in-progress`→`review`. Append the derivation |
| F15 | The milestone-local Decisions entry constrains future milestones | FIX wording with F1/F2; not promoted to a D-entry while the verdict is under correction, per the reviewer's own advice |
| F11 | AC5's narrowing shrank coverage below "every citation on the branch" | REJECTED — all fifteen ids were verified beyond AC5's promise and recorded; a second amendment naming AC5 hits the one-per-criterion stop |
| F12 | AC6 unticked while its covering task T8 is checked | REJECTED — mid-review state by design; the consistency-gate half runs at review |

**Gate result: RETURNED to `in-progress`.** What failed: F1 — the milestone's
headline conclusion is stated more strongly than its own evidence supports, the
failure mode M101 was returned twice for — together with F2, a promise M102's
Scope made and the delivered record broke. AC2, AC4 and AC5 are unticked because
the artifacts they verify are being rewritten; AC1 and AC3 keep their evidence,
which the findings do not disturb. AC6 was never reached — `devtools::check()`
was still in its test phase at the return.

**Gate checks that DID pass, for the re-review to re-run rather than inherit:**
`cairn_validate` all checks pass (47 advisories, the pre-existing M7 work-log
WARNs); `document()` no-diff, zero unresolved-link warnings; `check_pkgdown()`
clean; both alert audits exit 0; master's newest push runs green on both watched
workflows; `devtools::test()` FAIL 0 / PASS 8395.

**Round 2 (post-return) — evidence re-gathered from scratch 2026-08-21; no
round-1 result inherited.**

- **AC1 — met.** Probe commit `d0cc1cd` (the path subscriber) verified an
  ancestor of `758ab1b` (case B) and present on `origin/main`, the probe's
  default branch. Case B is run 32545779577; the `gh run list` window the
  criterion names returns 13 runs, and no subscriber run falls between case B's
  driving run and the next push.
- **AC2 — met.** The header no longer contains "RULED OUT". It states the
  measured claim — "no spelling derivable from a broken run's reported name
  closes this gap" — says in terms that this is narrower than "no `workflows:`
  change can help", and names both possibilities it leaves open, including that
  under the second one listing that name would itself be a `workflows:`-side
  fix. Driving run URLs and "measured 2026-08-21 local" are carried.
- **AC3 — met.** C1 32545943649, C2 32545999116, C3 32546052474; each recorded
  with its run URL, all `failure` with 0 jobs, none `startup_failure`. The
  header now states the broken-start set as a derivation rather than a count.
- **AC4 — met.** (b) restated as narrowed-not-closed with the unfiltered-
  subscriber experiment live again and its self-retrigger hazard recorded;
  (c) un-struck and marked NARROWED with both halves open and its promotion
  condition. Item (a) re-verified with a BOUNDED extraction — from
  `(a) **Scheduled sweep as a second detector**` to the next `(b) ` — 271 bytes
  each side, identical. The round-1 greedy pattern is not reused.
- **AC5 — met.** The procedure returns eleven URLs from the alert header, all
  resolving, and each run's reported `name` matches the claim made about it —
  32545896997 is `master-red-alert.yaml`/`skipped`, as the corrected cell 1 says.
- **Gate.** `cairn_validate` all checks pass, 47 advisories. `document()`
  no-diff, zero unresolved-link warnings. `check_pkgdown()` clean. Both alert
  audits exit 0 after the `tools/` comment change. Master's newest push runs
  green on both watched workflows. `ROADMAP.md` 23,453 bytes and `LESSONS.md`
  19,996 — both inside budget; LESSONS has 4 bytes of headroom, so a lesson
  added at hygiene must retire or compress first.

**Round-2 independent readers — full three-lens fan-out, fresh context.**

- **[S] prior-PR-comments: no regressions.** Verified each of the ten actioned
  return-1 findings is reflected in the current artifacts, checked specifically
  for M101's "criteria recorded from a command other than the one they named"
  mode and found none, and confirmed M100's `EXPECTED_IF` co-edit note survived
  the header rewrite. Its note that AC2/AC4/AC5 stood unticked is closed by this
  round's evidence.

**Round-2 readers — full three-lens fan-out, fresh context.** [S] blame-history
and [S] prior-PR-comments both returned no blocking defects and independently
verified the ten return-1 repairs landed. [O] diff-bug re-verified all 16 run
ids against the API and reported the run citations, experimental design,
measure-only constraint, AC5 procedure, ROADMAP budget and probe-repo state all
clean — then found ten wording and cross-surface defects.

| # | Finding | Disposition |
|---|---|---|
| R2-1 | The ROADMAP row's headline still declares the remainder "ANSWERED 2026-08-21 by M101 and M102" while (b) 400 bytes later says the question is undecided — a now-false claim readable as fact, the same overclaim class as return 1's F1, and introduced by the return-1 compression itself | DESCOPE |
| R2-2 | Item (b) still struck through as a tombstone while its text says "narrowed, not closed"; F8 was applied to (c) but not (b) | DESCOPE |
| R2-3 | AC2 is not satisfiable by the repaired header: it demands the header say which explanation the result rules out, and the result rules out NEITHER | AMENDMENT RETURN — AC2 |
| R2-4 | "Name resolution is NOT why a broken run goes unalerted" is unqualified; under the header's own open possibility (2) a name mismatch IS why nothing matched | DESCOPE |
| R2-5 | Cell 2 omits the negative that makes it a control — that the declared-name subscriber created no run, which is what proves name and validity varied independently | FIX — it is record, not interpretation |
| R2-6 | The Decisions entry says "the four-cell result" where every other surface says six | FIX |
| R2-9 | No pointer survives to the removed `path-match-probe.yaml` for a future reader | FIX — cite probe commit 758ab1b |
| R2-10 | M101's inherited "for BOTH the outcome was 'no event observed'" clashes with the sharpened epistemics | FIX |
| R2-11 | A work-log line still carries F13's "differs only in line numbers" error, uncorrected by the append that fixed the header | FIX — append naming this line |
| R2-12 | The round-1 Review block's withdrawn "AC2 — met … RULED OUT" verdict reads as current | FIXED above |
| R2-13 | "no spelling derivable from a broken run's REPORTED name" — "derivable" is wider than the two spellings driven | DESCOPE |
| R2-7 | Recorded "271 bytes each side" claimed not to re-derive (reader got 269) | REJECTED — the reader compared characters to bytes; the span is 271 bytes and 269 characters, verified both ways. The recorded figure is correct |
| R2-8 | The hygiene stamp is stale at 22,577 bytes | REJECTED — the stamp is rewritten at post-merge hygiene by design |

**Gate result: RETURNED (defect return 2), and Jeff chose to DESCOPE rather than
retry.** Every return on this milestone has been about interpretive prose
overreaching, twice now, while the measurements themselves have been clean since
the first pass — R2-1 was introduced by the very commit repairing return 1's F1.
So the interpretation leaves M102: the milestone narrows to the experiment, its
cells and the run record, and what the result MEANS is planned separately with
fresh eyes. AC2 and AC4 are unticked pending their gated narrowing amendment;
AC1, AC3 and AC5 keep their round-2 evidence.

