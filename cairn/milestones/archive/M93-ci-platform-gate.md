# M93: Close the CI platform gate

**Status:** done (2026-08-17, PR #122 https://github.com/jmgirard/circumplex/pull/122)

**Goal:** Make the release-platform matrix bear on merges — before merge for code-touching PRs, and at the review gate for master's push runs.

**Outcome:** New base-R classifier `tools/ci-matrix.R` (all os/R config
literals + the 11-path escalation set live there as data; fail-closed on
empty/truncated/unknown input) drives `R-CMD-check.yaml` via a `matrix` setup
job (`gh api pulls/N/files --paginate`, new + renamed-from paths, env-indirect
interpolations, own `setup-r` since ubuntu-24.04 ships no R). PR touching the
set → windows/macos/ubuntu R-release (≈ the old single job's wait); other PR →
single ubuntu job (M51's economy); push → the five configs, byte-identical.
`PROFILE.md` consistency-gate gains the master-matrix watch (review fails on
a red or absent latest push run).

**Decisions:** none milestone-local; plan-gate rejections (alert remedy, guard
extension, 5-config PR escalation) live in the file's Out with reopening
conditions — the alert remedy parked in the graduated candidate row.

**Review:** three-lens fan-out; six findings fixed pre-marker (missing
setup-r, confirmed live by a red first run failing closed; rename
under-escalation; 3000-file API-cap fail-open; env hardening; two comments),
three rejected, branch-protection absence → candidate row (maintainer's
choice). AC1 proven by live run 32096235315.
