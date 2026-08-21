# M100: Align the review gate's master watch with the alert's verdict set

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m100-master-watch-verdict-alignment

## Goal

Make `cairn/PROFILE.md`'s consistency-gate master-watch classify a push run's
conclusion the same way `.github/workflows/master-red-alert.yaml` does, so a
master run concluding `timed_out` or `startup_failure` cannot open a "master is
red" issue and be walked past by the next milestone's review gate.

## Scope

Surface tier: **internal** — the deliverable is `cairn/PROFILE.md`, the
developer-process file the review gate reads; no consumer of the R package
relies on it.

**In:** the two master-watch bullets in `PROFILE.md`'s consistency-gate slot.
Their no-verdict set becomes the alert's own benign list minus `success`, by
reference rather than restatement; `success` is the only green verdict and
every other conclusion is a red one. M95's absent-run carve-out and M96's
stale-query cross-check are each re-stated against the widened set.

**Out:** the alert workflow's `if:` expression — correct as of M99, and this
milestone changes no workflow behaviour. Whether a never-started run reaches
the alert at all → its existing ROADMAP candidate row (M99's remainder).
GitHub-native branch protection → its existing ROADMAP candidate row (M93 F1).
A checker pinning `PROFILE.md`'s prose against the workflow's `if:` → declined
at this plan gate; the pointer removes the drift surface instead.

## Acceptance criteria

- [ ] AC1: `PROFILE.md`'s master-watch bullets state a verdict rule by
      EXCLUSION under which a push run of a watched workflow on the default
      branch concluding `timed_out`, `startup_failure`, `action_required`, or
      any conclusion not named benign, is a gate failure — while `cancelled`,
      `skipped`, `neutral` and `stale` are no verdict.
- [ ] AC2: the benign set is not independently restated in `PROFILE.md`; the
      bullets name `.github/workflows/master-red-alert.yaml`'s job `if:` as its
      authority, and reading `PROFILE.md` end to end finds no second copy.
- [ ] AC3: the bullets distinguish "no run exists at all" — carved out as no
      verdict for the coverage watch only, per `paths-ignore` (M95), a gate
      failure for the `R-CMD-check.yaml` watch (M93) — from "a run concluded
      outside the verdict set", and say which watch each rule binds.
- [ ] AC4: the M96 stale-query cross-check applies to every red verdict, not
      only `failure`; its sentence names no conclusion that would restrict it.
- [ ] AC5: `cairn/PROFILE.md` and `.github/workflows/master-red-alert.yaml`
      are each dispositioned in the work log as updated by this milestone or
      left unchanged with a stated reason.
- [ ] AC6: `Rscript tools/check-master-red-alert.R` and `Rscript
      tools/master-red-alert-dryrun.R` both exit clean, unchanged by this
      milestone (no workflow behaviour moved).

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T1, T3
- AC4 → T1
- AC5 → T2
- AC6 → T3

## Tasks

- [x] T1: rewrite the two master-watch bullets in `cairn/PROFILE.md`'s
      consistency-gate slot — verdict-by-exclusion, the benign set carried by
      reference to the alert workflow's job `if:`, the M93 absent-run rule and
      M95 no-verdict rule each scoped to their own watch, and the M96
      cross-check sentence left binding on every red.
- [x] T2: sweep for surfaces that state how a run conclusion is classified —
      `grep -rn` over `git ls-files` for each of the nine `workflow_run`
      conclusion literals, plus the unliteral phrasings ("benign", "verdict",
      "no verdict", "red") — and disposition every hit in the work log. The
      sweep is the search; AC5 promises only the two files it must reach.
- [x] T3: walk the rewritten bullets against the record and log the reading
      for each of five outcomes — `success`, `failure`, a non-benign
      non-`failure` conclusion, `cancelled` (run 32187677266, still no
      verdict), and an absent run on each watch — then run both alert audits.

## Work log

- 2026-08-21: created by /milestone-plan; absorbs the M99-review F1 candidate row.
- 2026-08-21: plan gate chose mirroring the alert by EXCLUSION over enumerating `timed_out`/`startup_failure`/`action_required` as verdicts, because an allowlist of named bad conclusions reopens the same silence M99 closed for the alert; falsified by a conclusion GitHub adds that is genuinely no-verdict yet non-benign, which would make the watch red-fail spuriously.
- 2026-08-21: plan gate chose a pointer to the workflow's `if:` over restating the benign list in `PROFILE.md`, because two copies of a list M99 already widened once is the M56-family stranding shape; falsified by evidence a gate-runner cannot reach the workflow file when running the gate.
- 2026-08-21: plan gate chose no PROFILE-vs-workflow checker over extending `tools/check-master-red-alert.R` to pin both, because the scope hit the checker-regress shape (a checker M96 shipped and M99 hardened, over repo-internal artifacts) and the pointer removes the drift surface rather than instrumenting it; falsified by the two sides diverging again despite the pointer.
- 2026-08-21: reduced criteria audit ([O], internal tier) ran over the drafted criteria and returned one finding — the original AC4 promised "every tracked file" containing any of nine hand-listed conclusion literals, a per-rendering enumeration standing proxy for "every file whose instructions depend on how a conclusion is classified" (counterexamples: prose saying "the five benign conclusions" with no literal; a `TIMED_OUT` rendering). Fixed: the promise narrowed to the two files that carry the semantics (now AC5), and the repo-wide sweep kept as T2, which claims only what it swept.
- 2026-08-21: constraint for implement — `cairn/PROFILE.md` is 119 lines against the <120-line cap, so T1's rewrite must not grow it net; the pointer form (AC2) is what buys the room.
- 2026-08-21: T1 — `cairn/PROFILE.md`'s master-watch bullet rewritten (13 lines in, 13 out; file holds at 119 of the <120 cap). Rule is now by exclusion: only `success` is green, every other conclusion red, the NO-verdict set carried by reference to `.github/workflows/master-red-alert.yaml`'s job `if:` rather than restated. Fitting the full workflow path cost two clauses, both judged redundant where they sat: M59's "so its environment is distinct" (the same point is made by "reads one milestone LATE, catching a covr-only regression at the NEXT gate"), and the M96 cross-check's "while the same query" rephrased. The adjacent alert-audits bullet is untouched at 4 lines.
- 2026-08-21: T2 — sweep run as `git ls-files -z | xargs -0 grep -In` for the nine conclusion literals and the phrasings "benign", "no verdict", "newest completed". Dispositions. `.github/workflows/master-red-alert.yaml`: UPDATED — a four-line header comment saying `PROFILE.md`'s watch now derives its NO-verdict set from this `if:`, so widening the list widens what the gate walks past; no behaviour changed and both audits still pass. `cairn/LESSONS.md:45`: UPDATED — the M95-family lesson said "read the newest run concluding success OR failure", the exact narrow set T1 replaced; now "read the newest run reaching a VERDICT (M100)" (45 lines, 19,995 bytes, under both caps). `tools/check-master-red-alert.R`: UNCHANGED — it pins the workflow's `if:` literal, verifying the alert and not the gate; extending it to read `PROFILE.md` is the checker-regress option this plan gate declined. `cairn/ROADMAP.md:23` and the `cairn/milestones/archive/`, `cairn/legacy/`, `cairn/reviews/` hits: UNCHANGED — history, never edited (IP4). Every other hit is unrelated word use — `verdict`/`stale`/`neutral` in the statistical code, docs, and tests (`R/ssm_sem.R`, `NEWS.md`, `man/`, `vignettes/`, `tests/`, `devel/`, the `neutral`/`benign` hits in `DECISIONS.md` and `references/browne1992a.md`), plus `pkgdown.yaml`'s `cancelled`/`stale` about concurrency and site freshness, not run conclusions.
- 2026-08-21: T2 note — the sweep is the search, not the promise: AC5 binds only the two files that carry the conclusion semantics, per the plan-gate audit's narrowing of the original repo-wide criterion.
- 2026-08-21: T3 — the rewritten rule walked against the live record. Benign list read from the workflow as it now stands: `["success","cancelled","skipped","neutral","stale"]`. (1) `success` → the only green verdict; gate passes. (2) `failure` → red verdict on both watches, cleared via `/hotfix`. (3) a non-benign non-`failure` conclusion (`timed_out`, `startup_failure`, `action_required`, or any GitHub adds) → red BY EXCLUSION and a gate failure; before T1 it was no verdict and the gate read back past it, which is the divergence M100 closes. (4) `cancelled` → in the benign list, so no verdict; `gh run view 32187677266` returns `conclusion: cancelled, event: push, headBranch: master, workflowName: test-coverage.yaml`, confirming the M95 case still reads as it did. (5) absent → `R-CMD-check.yaml` a gate failure (M93), `test-coverage.yaml` no verdict, said at the gate (M95).
- 2026-08-21: T3 — live watch run, both workflows, `--branch=master --event=push`: newest is `in_progress` with a null conclusion on each (32528271421, 32528271428), then `success` (32518464682, 32518464711), then `success`, then `cancelled` (32227173810, 32227173824). Both watches answer GREEN on the newest verdict-reaching run. A still-running run has reached no conclusion, so "every other conclusion is red" does not quantify over it — the alert side pins `types: [completed]` for the same reason (M99 review F2). Making that explicit in the bullet was drafted and dropped: it did not fit 13 lines, and `PROFILE.md` is at the 119-of-120 cap; the phrase "reaching a *verdict*" already excludes it. Recorded here rather than contorting the file.
- 2026-08-21: T3 — AC6 evidence: `Rscript tools/check-master-red-alert.R` exit 0 and `Rscript tools/master-red-alert-dryrun.R` exit 0 (5/5 fixtures ok) against the workflow carrying T2's header comment, so no alert behaviour moved.
- 2026-08-21: AC1 reading recorded for review — the criterion asks for a rule by exclusion UNDER WHICH `timed_out`/`startup_failure`/`action_required` are gate failures, not for those literals to appear in `PROFILE.md`. Naming them there would also have put a second copy of the conclusion vocabulary in the file, which is what AC2 forbids.
- 2026-08-21: all tasks done; status → review. No R code, roxygen, or test file touched, so the profile's `verify` slot is vacuous on this diff; `devtools::test()` run anyway for a fresh number — FAIL 0 | WARN 5 | SKIP 3 | PASS 8395, the same 8395 M99 recorded, the 5 warnings lavaan's and pre-existing.

## Decisions

## Review
