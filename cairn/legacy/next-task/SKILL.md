---
name: next-task
description: Work the next (or a named) task from MILESTONES.md through the full plan → test-first → implement → validate → review → log loop. Use when the user says "next task", "work on B3", "continue the milestone", or asks to pick up milestone work.
---

# next-task

Drive one MILESTONES.md task through the complete workflow. One task per
invocation unless the user names several.

## Steps

1. **Select.** Read MILESTONES.md. If the user named a task (e.g., "B3"), use
   it; otherwise take the first unchecked task in the active milestone,
   preferring Bugs over Guardrails over Docs. State which task you're taking
   and restate its acceptance criteria.

2. **Plan.** Read the relevant code before proposing changes. For tasks
   touching estimation code (`ssm_*` statistical functions, `src/`), write a
   short plan first: what changes, what could break, which boundary cases from
   CLAUDE.md apply. For doc-only tasks, skip to step 4.

3. **Test first.** Write the regression test(s) implementing the acceptance
   criteria. Run them and confirm they FAIL against current code (for bugs)
   before writing the fix. If a test unexpectedly passes, stop and re-diagnose
   — the audit finding may be stale.

4. **Implement.** Smallest change that satisfies the criteria, matching
   existing style (see CLAUDE.md Style). If you edit roxygen, run
   `devtools::document()`. If you edit `src/`, recompile and note that
   `RcppExports.*` regenerate.

5. **Verify.** Run the full test suite. For estimation changes, invoke
   `/statistical-validation`. Run `devtools::check()` if R/ or src/ changed.

6. **Review.** Invoke `/code-review` on the diff. Fix confirmed findings.

7. **Log.** Check the task's box in MILESTONES.md and append one log line:
   `- YYYY-MM-DD — <task id>: <what changed> (<files>)`. Add a NEWS.md bullet
   under the development version heading if user-facing.

8. **Report.** Summarize what changed, test evidence, and what task is next.
   Always name the recommended model tier for that next task with a one-line
   why (Fable for plausible-but-wrong-statistics risk; Opus for general
   implementation; Sonnet for mechanical/doc work) — see CLAUDE.md model tiers.

9. **Offer to commit.** Do not commit automatically. End the report by offering
   a commit with a proposed message (subject `<what> (M<milestone>/<task id>)`,
   a short body, and the `Co-Authored-By` trailer), and wait for the user to
   confirm. They may want to batch, squash, reword, or hold. If the user has
   already said to commit (this task or as a standing instruction), just do it.
   When committing, keep unrelated already-staged work out, and note anything
   left uncommitted (e.g. untracked files not in scope).

## Rules

- Never weaken an existing test to make it pass; seeded bootstrap values in
  test-ssm_analysis.R are intentional regression pins — if they change, the
  statistics changed, and that needs explicit justification in the report.
- If acceptance criteria turn out to be wrong or incomplete, update
  MILESTONES.md to say what was actually done and why.
- **Task status lives only in MILESTONES.md.** ROADMAP.md carries direction and
  milestone-level status; never add or check task-level boxes there, and don't
  copy acceptance criteria or logs into it. If a task reveals new work that is
  out of scope for the active milestone, record it under the appropriate
  ROADMAP milestone as a plain bullet (a future intention), not a checkbox.
