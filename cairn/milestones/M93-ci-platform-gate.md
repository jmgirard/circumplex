# M93: Close the CI platform gate

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m93-ci-platform-gate`

## Goal

Make the release-platform matrix bear on merges — before merge for
code-touching PRs, and at the review gate for master's push runs — closing at
both ends the gap that let a Windows-only failure ride a green Ubuntu-only PR
gate onto master for three weeks (fixed by PR #121, 2026-08-17). Internal-tier
deliverable: CI and review-process tooling; no external consumer of the
package relies on it.

## Scope

**In:**
- A base-R classifier script `tools/ci-matrix.R` (Rbuildignored, no new
  dependencies): input = the event name and changed-file list; output = matrix
  JSON. Push → the five-config matrix; PR touching the escalation set →
  three-platform release matrix (windows-latest, macos-latest, ubuntu-latest,
  all R release, ~30 min wall clock ≈ today's single job); other PR → the
  single ubuntu-release job (M51's economy preserved). Errors fail the job,
  never silently fall back to a smaller matrix.
- Escalation set: `R/**`, `src/**`, `tests/**`, `vignettes/**`, `data/**`,
  `inst/**`, `DESCRIPTION`, `NAMESPACE`, `.github/workflows/R-CMD-check.yaml`,
  `tools/check-ci-deps.R`, `tools/ci-matrix.R`.
- `.github/workflows/R-CMD-check.yaml` gains a setup job computing the PR's
  changed files with git/gh built-ins and consuming the classifier's output.
- One consistency-gate line in `cairn/PROFILE.md`: review checks master's most
  recent completed R-CMD-check push run.

**Out:**
- Automated alert (issue-on-failure) on master's matrix → parked in the M93
  ROADMAP tombstone row; promote on a master red outliving a review cycle
  unnoticed.
- Extending `tools/check-ci-deps.R` to police the matrix wiring → declined at
  the plan gate (checker-regress shape; single-sourcing removes the drift
  class it would watch); revisit only on a matrix-wiring drift single-sourcing
  failed to prevent.
- Five-config PR escalation (R-devel / oldrel on PRs) → not planned: the extra
  16 min of wall clock buys version coverage the push matrix plus AC4's review
  check already provide; reopen on a version-only regression entering through
  a green three-platform PR gate.

## Acceptance criteria

- [ ] AC1 — `tools/ci-matrix.R` declares the PR-escalation path set, whose
      membership, read from that file, is exactly {`R/**`, `src/**`,
      `tests/**`, `vignettes/**`, `data/**`, `inst/**`, `DESCRIPTION`,
      `NAMESPACE`, `.github/workflows/R-CMD-check.yaml`,
      `tools/check-ci-deps.R`, `tools/ci-matrix.R`}; and one live PR whose
      diff touches a member of that set runs the three-platform release
      matrix (windows-latest, macos-latest, ubuntu-latest, all R release),
      with all three jobs listed in that run.
- [ ] AC2 — `tools/ci-matrix.R`, invoked locally on a file list containing no
      escalation-set member (exact command and file-list argument recorded in
      review evidence), emits exactly the single
      `{os: ubuntu-latest, r: release}` matrix; and the workflow's
      matrix-selection expression, read from `R-CMD-check.yaml`, routes every
      pull-request run through that classifier and applies the five-config
      matrix only on push to master. (The local invocation evidences the
      classifier's output; the single-job branch of the workflow wiring rests
      on the file read — the end-to-end live link is AC1's escalated run.)
- [ ] AC3 — Each os/R-version config literal appears exactly once across
      `R-CMD-check.yaml` and `tools/ci-matrix.R` combined; the single-job,
      escalated-PR, and push matrices are composed from those literals rather
      than repeated — verified by a read of both files.
- [ ] AC4 — `cairn/PROFILE.md`'s consistency-gate slot directs
      `/milestone-review` to check the most recent completed
      `R-CMD-check.yaml` run for the default branch's push trigger
      (`gh run list --workflow=R-CMD-check.yaml --branch=<default>
      --event=push`) and treat a red or absent run as a gate failure; this
      milestone's own review executes that check once as fresh evidence.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2, T3
- AC3 → T1, T2
- AC4 → T4

## Tasks

- [x] **T1** — Write `tools/ci-matrix.R` (base R only): event name +
      changed-file list in, matrix JSON out, from single-sourced config
      literals; escalation set declared as data at the top; unreadable or
      missing input stops with an error (fail closed, never a silent smaller
      matrix). Add to `.Rbuildignore` if `tools/` is not already covered.
- [ ] **T2** — Rework `R-CMD-check.yaml`: setup job computes the PR's changed
      files against its merge base (git/gh built-ins, no third-party
      changed-files action), calls the classifier, and the check job's matrix
      consumes its output; delete the inline M51 conditional it replaces.
      Lesson guard: count jobs in the PR's own live run via `gh run view` —
      never trust a bare green status (M7-family lesson).
- [ ] **T3** — Record the classifier demonstrations: one invocation per
      branch (non-escalating list → single job; escalating list →
      three-platform; push event → five-config), exact commands and outputs
      kept for review evidence.
- [ ] **T4** — Append the master-matrix check to `cairn/PROFILE.md`'s
      consistency-gate slot (PROFILE is at 100 of 120 lines — stay terse).

## Work log

- 2026-08-17: created by /milestone-plan (from the "PR check gate structurally blind to platforms" candidate row, promotion clause fired by the 2026-08-17 platform-only red, PR #121; extends M51's matrix design, keeping its single-job economy for non-code PRs).
- 2026-08-17: reduced criteria audit ([O] fresh reader, three rounds) — round 1 flagged AC1/AC2 as unbounded universals, narrowed to file-read membership + one live run; round 2 (gate's 3-platform amendment) clean; round 3 (classifier-script redesign) clean, and caught the escalation set omitting `tools/ci-matrix.R` itself — added.
- 2026-08-17: plan gate chose prevention + review-gate detection over adding an issue-on-failure alert because the review check covers the same gap and proving an alert needs a deliberate red; falsified by a master red outliving a review cycle unnoticed.
- 2026-08-17: plan gate chose the three-platform escalated matrix over the full five-config because the +16 min wall clock comes from ubuntu-devel, a version job, while the injury was platform-shaped and the platform jobs match today's wait; falsified by a version-only regression entering through a green three-platform PR gate.
- 2026-08-17: plan gate chose leaving `tools/check-ci-deps.R` untouched over extending it because widening a shipped checker is the checker-regress shape and single-sourcing removes the drift class; falsified by a matrix-wiring drift single-sourcing failed to prevent.
- 2026-08-17: plan chose an in-repo classifier script + setup job over a separate paths-triggered workflow (config duplication, required-check naming) and over a third-party changed-files action (new supply-chain dependency); decisive: a live non-escalated PR run is impossible pre-merge, so the non-escalated branch needs a locally invocable classifier; falsified by the setup job's changed-file computation misclassifying a real PR.
- 2026-08-17: plan gate chose the package+CI escalation path set over escalating every triggering PR because doc-tooling PRs would pay three platform jobs for changes the check does not exercise; falsified by a platform red introduced through a non-escalating path.

- 2026-08-17: T1 done — `tools/ci-matrix.R` written (config literals + escalation set as data, glob entries stored verbatim as `dir/**`); all seven branches exercised: push → 5 configs, escalating PR lists (`tools/check-ci-deps.R`; `R/utils.R`) → 3 platforms, non-escalating list (`.github/workflows/pkgdown.yaml` + `cairn/ROADMAP.md`) → single job, and empty list / missing file / unknown event / missing arg each exit 1. `^tools$` already Rbuildignored, so T1's ignore clause was a no-op.

## Decisions

## Review
