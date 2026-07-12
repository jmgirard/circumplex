---
name: release-checklist
description: Prepare a CRAN release of the circumplex package — checks, versioning, NEWS, cran-comments, submission steps. Use when the user says to prepare a release, submit to CRAN, or cut a version.
---

# release-checklist

Walk the package to CRAN-ready state. Do the steps in order; report each
outcome. Never submit to CRAN yourself — prepare everything and hand the final
`devtools::submit_cran()` / web-form step to the user.

## 1. Pre-flight

- [ ] Working tree clean apart from release changes; on master or a release branch.
- [ ] MILESTONES.md: active milestone complete (or explicitly descoped items
      moved back to ROADMAP.md with a note).
- [ ] `devtools::document()` produces no diff (docs in sync).

## 2. Verification (all must pass locally)

- [ ] `devtools::test()` — no failures, no new skips.
- [ ] `devtools::check(args = "--no-manual")` — 0 errors, 0 warnings; NOTEs
      individually justified in cran-comments.md.
- [ ] Run `/statistical-validation` if any estimation code changed since the
      last release.
- [ ] Suggest to the user a deep review of the release diff: **`/code-review high`**
      by default, or **`/code-review max`** for a large or statistically risky
      release (e.g. the M4 CircE work). Both run locally. Only mention the
      billed cloud `/code-review ultra` if the user asks for the deepest possible
      pass on a flagship release — it is expensive; do not default to it. Also
      suggest win-builder / R-devel via `devtools::check_win_devel()` (results
      arrive by email to the maintainer address).
- [ ] Reverse dependencies: check CRAN for revdeps
      (`tools::package_dependencies("circumplex", reverse = TRUE)`); if any,
      run revdepcheck or justify skipping.

## 3. Version & metadata

- [ ] DESCRIPTION: bump to release version (drop .9000 tail;
      patch/minor/major per ROADMAP milestone scope).
- [ ] NEWS.md: development heading renamed to the release version; entries
      complete, user-facing wording, breaking changes flagged prominently.
- [ ] cran-comments.md: updated with test environments, check results, NOTE
      justifications, revdep summary.
- [ ] If URLs changed anywhere, run `urlchecker::url_check()`.

## 4. Submission (user-driven)

- [ ] Present the release summary and stop for the user to submit
      (`devtools::submit_cran()` confirms via maintainer email).

## 5. Post-acceptance (run when the user reports acceptance)

- [ ] Git tag `vX.Y.Z`; delete `CRAN-SUBMISSION` if present.
- [ ] Bump DESCRIPTION to X.Y.Z.9000; add new development heading to NEWS.md.
- [ ] Move the finished milestone (its task list + full log) from MILESTONES.md
      to the top of MILESTONES-ARCHIVE.md (newest first). Update that
      milestone's ROADMAP.md **Status:** line to "shipped in vX.Y.Z". Then
      promote the next ROADMAP milestone into MILESTONES.md's active slot
      (copy its scope, expand into tasks with acceptance criteria, start a
      fresh log) so MILESTONES.md again holds exactly one active milestone.
- [ ] Rebuild pkgdown site if not automated (`pkgdown` GitHub Action handles
      it on push).
