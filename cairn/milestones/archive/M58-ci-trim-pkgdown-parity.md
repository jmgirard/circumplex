# M58: Finish the post-M52 CI trim — pkgdown parity + an allowlist drift guard

**Status:** done (2026-07-25, PR #84 https://github.com/jmgirard/circumplex/pull/84)

**Goal:** Give `pkgdown.yaml` the dependency and run-triggering discipline the
other two workflows already carry, backed by a mechanical sync check.

**Outcome:** pkgdown (untouched by M51/M52) moves `needs: website` ->
`dependencies: '"hard"'` + allowlist = Suggests minus brms: brms/rstan gone,
cache 177->117MB, install-step time unchanged at 69s. Concurrency splits — PRs
keyed per ref and cancelled, deploying events one shared key uncancelled so
gh-pages deploys queue rather than race. `paths-ignore` on all three push
triggers; pkgdown's list is `cairn/**` only, since `man/**` and README.md are
its own site inputs. New Rbuildignored `tools/check-ci-deps.R`, run before the
install, asserts allowlist == Suggests minus documented exclusions AND that the
step keeps `"hard"` with no `needs:` key.

**Decisions:** D-029 — OpenMx/glmmTMB stay installed (BC7 carries no
`skip_on_cran`; dep install is under 7% of a job). No milestone-local entries.

**Post-merge:** AC4 confirmed live — hygiene commit 77887f25 (`cairn/**` only)
triggered zero runs, against plan commit 425fd294 which triggered all three
pre-change. AC6 confirmed — full 5-config matrix green on master, the guard
step passing on windows and macos for the first time.

**Review:** prior-PR-comments and blame-history zero findings; diff-bug six,
scored 80/78/68/55/40/25. F1 actioned; F2+F3 actioned at maintainer direction
past the scorer threshold; F4/F5/F6 logged. Retired M52's allowlist-drift
LESSONS line — the new guard fails on exactly that mistake, proven by mutation.
