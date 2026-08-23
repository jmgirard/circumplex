<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M105: Give master GitHub-native branch protection

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** m105-master-branch-protection · https://github.com/jmgirard/circumplex/pull/134

## Goal

Put master's destructive-operation and check gates into GitHub itself, where
they bind merges the cairn process guards cannot see.

## Scope

**Surface tier: internal** — the deliverable is repository settings plus an
in-repo record and checker over them; no consumer of the installed package
reaches it.

**In:** two rulesets on the default branch of `jmgirard/circumplex`. A
*destructive* ruleset with no bypass actors, carrying `deletion`,
`non_fast_forward` and `required_linear_history` — these bind the repository
admin too, which is the point: tracking-rules' enforcement boundary says every
cairn guard is a PreToolUse hook on *this session's* Bash calls, so a web-UI
merge, a merge queue, or a second clone is invisible to them. A *checks*
ruleset carrying `required_status_checks`, with the repository-admin role at
`bypass_mode: always`. The bypass is structural, not laxity: required status
checks block direct pushes as well as merges, and a `cairn/**`-only push runs
no workflow at all (PR #133 reported zero checks), so without it cairn's
docs-only direct-push carve-out deadlocks. A ruleset's bypass actor bypasses
every rule in that ruleset, which is why the destructive rules cannot share a
ruleset with the bypassed ones. Plus `tools/branch-protection.json` as the
committed source of truth, `tools/check-branch-protection.R` reading the live
API against it, a `PROFILE.md` consistency-gate line, and a `DECISIONS.md`
entry recording the two-ruleset split and the bypass rationale.

**Out:**
- Making required checks bite on the maintainer → not possible while the
  docs-only direct-push carve-out stands (plugin doctrine, not this repo's);
  revisit only if that carve-out changes.
- An always-reporting aggregator job in `R-CMD-check.yaml` (drop workflow-level
  `paths-ignore`, filter in the `matrix` job, add an `if: always()` gate job) →
  ROADMAP candidate row. Weighed and declined at the plan gate: it would make
  checks report on `cairn/**`-only PRs, but the maintainer still needs `always`
  bypass for direct pushes regardless, and it reopens M93's fail-closed
  classifier, which errors on an empty changed-files list by design.
- Requiring `windows-latest (release)` or `macos-latest (release)` → refused,
  not deferred: `tools/ci-matrix.R:23-31` puts them in the escalated and push
  matrices only, so requiring either would block every non-escalating PR.
- Rotating and deleting the plaintext Codecov token → its own standing
  candidate row; it turns on your Codecov account, not on this ruleset work.

## Acceptance criteria

- [x] AC1: `gh api repos/jmgirard/circumplex/rulesets` returns two rulesets
      targeting the default branch, both `enforcement: "active"`. Read whole via
      `gh api repos/jmgirard/circumplex/rulesets/<id>`, one has rule types
      exactly `{deletion, non_fast_forward, required_linear_history}` and an
      empty `bypass_actors`; the other has rule types exactly
      `{required_status_checks}` and a `bypass_actors` array holding exactly one
      entry, the repository-admin role at `bypass_mode: "always"`.
- [x] AC2: the checks ruleset's required status check contexts are exactly
      `["matrix", "ubuntu-latest (release)"]`, read from the AC1 output.
- [x] AC3: the single force-push attempt named in T6 is rejected by GitHub, and
      `git rev-parse origin/master` immediately afterwards equals the tip master
      held before the attempt.
- [x] AC4: at least one docs-only tracking commit is pushed directly to master
      after both rulesets are active, and `git log origin/master` shows it —
      i.e. the carve-out the bypass exists to preserve still works.
- [x] AC5: `tools/check-branch-protection.R` exits zero when the live rulesets
      agree with committed `tools/branch-protection.json`, and non-zero on a
      difference in any field named by its own `COMPARED_FIELDS` constant.
- [x] AC6: `cairn/PROFILE.md`'s consistency-gate section names
      `tools/check-branch-protection.R` as a check the review gate runs, and
      `wc -l cairn/PROFILE.md` is ≤ 120.
- [ ] AC7: `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::check()'` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T3, T4
- AC2 → T3, T4
- AC3 → T6
- AC4 → T7
- AC5 → T1, T2
- AC6 → T5
- AC7 → T8

## Tasks

- [x] T1: write `tools/branch-protection.json` — the two rulesets' intended
      shape, in the field layout `gh api .../rulesets/<id>` returns, so the
      comparison is against the API's own vocabulary rather than a translation.
- [x] T2: write `tools/check-branch-protection.R` (base R + `gh`, matching
      `tools/check-ci-deps.R`'s shape). A `COMPARED_FIELDS` constant names the
      fields compared — enforcement, target, rule types, required status check
      contexts, bypass actors — and the comparison iterates it, so the constant
      is the domain AC5 quantifies over rather than a list in the prose. Fail
      closed on a missing/unauthenticated `gh`, an absent ruleset, or an
      unparseable response. Prove the non-zero arm by mutating the committed
      JSON in each `COMPARED_FIELDS` field one at a time, restoring after each,
      and record each mutation's exit status in the work log.
- [x] T3: draft the two `gh api --method POST repos/jmgirard/circumplex/rulesets`
      calls and show them to Jeff for explicit authorization before running
      either — a repository settings change.
- [x] T4: on that authorization, create both rulesets; read each back with
      `gh api .../rulesets/<id>` and confirm AC1 and AC2 field by field.
- [x] T5: add the consistency-gate line to `cairn/PROFILE.md`. It is a
      one-line budget: 119 → 120 is the cap exactly, so the line must fit on
      one line or pay for itself by compressing another.
- [x] T6: hand Jeff the force-push demonstration and have him run it — after a
      docs-only commit lands, `git push --force origin <previous-sha>:master`,
      which must be rejected. This session cannot run it (the force_push_guard
      hook denies force-push to the default branch), and it is reversible if
      the ruleset fails: master rewinds one docs commit, restored by pushing it
      again. Quote the rejection text into the work log.
- [x] T7: after both rulesets are active, push one docs-only tracking commit
      directly to master and confirm it landed (AC4).
- [x] T8: sweep for prose the rulesets make newly false — grep `cairn/` and
      `.github/` for `branch protection`, `required status`, `force`,
      `enforcement boundary`; run `devtools::test()` and `devtools::check()`;
      write the `DECISIONS.md` entry for the two-ruleset split and the
      admin-bypass rationale, with its reopening condition.

## Work log

- 2026-08-22: created by /milestone-plan.
- 2026-08-22: plan gate chose two rulesets (no-bypass destructive + admin-bypassed checks) over one ruleset and over adding an always-reporting aggregator job to `R-CMD-check.yaml`; a ruleset's bypass actor bypasses every rule in it, so one ruleset cannot both hard-block an admin's force push and permit that admin's docs-only direct push, and the aggregator does not remove the direct-push bypass need while it does reopen M93's fail-closed classifier; falsified by evidence that a bypass actor can be scoped per-rule, or that direct docs-only pushes stop needing bypass.
- 2026-08-22: plan gate chose required contexts `matrix` + `ubuntu-latest (release)` over `ubuntu-latest (release)` alone; requiring the classifier means a fail-closed classification cannot be merged past, not only that the check job passed; falsified by a PR class where `matrix` reports and `ubuntu-latest (release)` cannot.
- 2026-08-22: plan gate chose a live-API checker wired into the review gate over a D-entry record alone; web-UI settings drift is invisible to the repo, and M58's lesson is to assert the enabling condition (`enforcement: active`) beside the thing it enables; falsified by the gate line proving unpayable inside PROFILE.md's 120-line cap.
- 2026-08-22: [O] criteria audit ran in REDUCED mode (surface tier: internal). Returned eight findings, all disposed as clear fixes, none escalated to the gate: AC5's promise was universal over the whole mismatch space while naming five sampled mutations, and bound the mutation battery itself — narrowed to the domain `COMPARED_FIELDS` enumerates, battery moved to T2; AC3 claimed force pushes are refused as a class from one attempt — narrowed to the named attempt; AC1, AC3 and AC6 each bound a recording act (quote the API output, quote the rejection text, strike the ROADMAP row) — quoting moved to the tasks and the gate procedure, the tombstone dropped as post-merge hygiene. AC2 and AC4 passed all three questions.

- 2026-08-22: T1 — `tools/branch-protection.json` written in GitHub's literal ruleset vocabulary rather than a projection of it, so each `rulesets` element is a complete POST body; what is committed is therefore what T4 creates, not a transcription of it, and the checker can project both sides through one extractor.

- 2026-08-22: T2 — `tools/check-branch-protection.R` written; `COMPARED_FIELDS` holds six fields (enforcement, target, ref_name_include, rule_types, required_status_check_contexts, bypass_actors) and one extractor set projects BOTH sides, so a projection bug cannot make them falsely agree in one direction only. Missing-ruleset arm proved live against the unprotected repo: exit 1, both committed rulesets reported absent.
- 2026-08-22: minor amendment (task reorder, no criterion touched): T2's six-field mutation battery moves to after T4. While no ruleset exists live the field-comparison loop is unreachable — every mutation would re-print the missing-ruleset message rather than a field mismatch — so the battery can only prove what AC5 claims once T4 has created them. T2 stays unchecked until it runs.

- 2026-08-22: T3/T4 — Jeff authorized both POST calls at the implement gate; rulesets created: `master-destructive` id 21216269, `master-checks` id 21216270. Read-back of each via `gh api .../rulesets/<id>` agrees with the committed intent in every COMPARED_FIELDS field — GitHub normalized nothing, so `tools/branch-protection.json` needed no correction. Checker exits 0 against the live pair.
- 2026-08-22: T2 battery (run after T4 per the reorder line above) — six mutations of the committed JSON, one per COMPARED_FIELDS field (enforcement→evaluate, target→tag, ref_name_include→refs/heads/master, rule_types minus non_fast_forward, contexts ubuntu release→devel, bypass_mode always→pull_request): every one exits 1 naming exactly the mutated field; JSON restored after each, clean exit-0 pass after restore.

- 2026-08-22: T5 — gate line added to PROFILE.md's consistency-gate beneath the alert-audit line; fits on one line, `wc -l` 119 → 120, cap held without compressing anything.

- 2026-08-22: T7 — docs-only tracking commit cd08db79 pushed directly to master under both active rulesets; the remote replied `Bypassed rule violations for refs/heads/master: 2 of 2 required status checks are expected`, i.e. the checks rule fired and the admin bypass carried the push — the carve-out works by the designed mechanism, not by accident (AC4).
- 2026-08-22: T6 — Jeff ran `git push --force origin 0b1e1957:master` (a one-docs-commit rewind); GitHub refused it: `remote: error: GH013: Repository rule violations found for refs/heads/master`, and `git rev-parse origin/master` immediately after equals cd08db79, the tip before the attempt (AC3). Server-side enforcement is real, not merely configured.
- 2026-08-22: branch synced with the moved master by rebase rather than merge-in — the session's merge guard denies all `git merge` forms, and the branch was unpushed, so the rebase is equivalent and keeps history linear.

- 2026-08-22: T8 in progress — sweep clean (no live prose outside archives claims master is unprotected or force-pushable); D-047 appended (two-ruleset split, bypass rationale, reopening conditions); PROFILE.md's 120-line overrun paid by compressing the greenfield-openers intro one line (cap is exclusive; `cairn_validate` all-OK at 119). `devtools::test()` FAIL 0 (5 lavaan WARNs, 3 SKIPs, standing baseline); `devtools::check()` running, its verdict is the checkpoint that closes T8.

- 2026-08-22: T8 closed — `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes (20m40s). All tasks done; status → review.

## Decisions

## Review

Review 2026-08-22 (PR #134). Evidence per criterion, fresh at review:

- AC1: `gh api repos/jmgirard/circumplex/rulesets` → exactly two branch rulesets, both `enforcement: "active"`: 21216269 `master-destructive` (rules deletion + non_fast_forward + required_linear_history, `bypass_actors` []) and 21216270 `master-checks` (rules required_status_checks only, one bypass actor RepositoryRole/5/always). Verified.
- AC2: 21216270's contexts read back as exactly `["matrix", "ubuntu-latest (release)"]`. Verified.
- AC3: T6's attempt (`git push --force origin 0b1e1957:master`, run by Jeff) was refused — `remote: error: GH013: Repository rule violations found for refs/heads/master` — and `git rev-parse origin/master` at review still equals cd08db79, the pre-attempt tip. Verified.
- AC4: docs-only commit cd08db79 pushed directly to master under both active rulesets is at `origin/master` tip at review; the remote's push reply showed the checks rule firing and the admin bypass carrying it. Verified.
- AC5: pass arm exit 0 against the live pair; fail arm re-proved at review with a SECOND mutation battery using different forms than implement's (enforcement→disabled, target→tag, include→refs/heads/main, an ADDED rule type, an ADDED context, bypass_actors emptied) — all six exit 1, each naming exactly the mutated COMPARED_FIELDS field; clean pass after restore. Verified.
- AC6: `cairn/PROFILE.md:49` names the checker in the consistency-gate; `wc -l` = 119 ≤ 120. Verified.

Consistency gate: `cairn_validate` all OK (WARN work-log format = M7's standing pre-M28 advisory). No principle change → `cairn_impact` skipped. `document()` no diff, zero resolve-link lines. README untouched. `pkgdown::check_pkgdown()` no problems. NEWS: no user-visible package change (internal tier — repo settings + tools/) → no entry owed. No new top-level files (`tools/` predates, `^tools$` in .Rbuildignore). Master watches: newest verdict-reaching push runs green on both workflows (ab83f30b success ×2; newer cairn-only pushes run none, per paths-ignore — said so per M95). Alert audits: check-master-red-alert.R and dryrun both clean. Branch-protection check: exit 0.
