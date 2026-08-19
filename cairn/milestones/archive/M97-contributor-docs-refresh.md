# M97: Make the contributor docs describe this repo

**Status:** done (2026-08-19, PR #126 https://github.com/jmgirard/circumplex/pull/126)

**Goal:** Rewrite the four `.github/` contributor documents so they describe this repo's
actual practice rather than the usethis tidyverse template they came from.

**Outcome:** `CONTRIBUTING.md` — tidyverse framing and the "someone from the team"
prerequisite → issue-for-bugs / discussion-for-features; Travis/AppVeyor → a CI paragraph
pointing at `tools/ci-matrix.R` rather than restating its escalation set, naming
`pkgdown.yaml` and the `paths-ignore` no-run case; styler → CLAUDE.md's "match existing
code style"; a new Generated files section. `SUPPORT.md` — issue-search link corrected from
`tidyverse/circumplex` to DESCRIPTION's `BugReports:`; help venues → Discussions/Issues, as
in `ISSUE_TEMPLATE.md`. `CODE_OF_CONDUCT.md` — Covenant 1.0.0 (no contact) → 2.1 from
usethis 3.2.1 with `me@jmgirard.com`. Two dead `.Rbuildignore` entries; one 404 repaired.

**Decisions:** two milestone-local. AC7's Covenant source is usethis 3.2.1's bundled
template, not contributor-covenant.org (the planned URL 404s; upstream raw markdown carries
Hugo front matter). `CODE_OF_CONDUCT.md:121`'s `[text][url]` link is malformed verbatim
upstream and unmodifiable under AC7's byte pin, so it is probe-exempt, probed by hand (200).

**Review:** three lenses; [S] blame-history clean, [S] prior-PR-comments one finding
(`pulls/comments` probe `[]`, confirming M91), [O] diff-bug 11 ranked. 12 total, 8 actioned,
4 rejected. Five converged on the CI paragraph — it listed 8 of `ESCALATION_SET`'s 11 paths
— fixed by removing the enumeration. Two gated amendments, each audited by a fresh reader.
