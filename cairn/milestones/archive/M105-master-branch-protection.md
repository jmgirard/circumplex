# M105: Give master GitHub-native branch protection

**Status:** done (2026-08-22, PR #134 https://github.com/jmgirard/circumplex/pull/134)

**Goal:** Put master's destructive-operation and check gates into GitHub
itself, where they bind merges the cairn process guards cannot see.

**Outcome:** Two live rulesets on the default branch: `master-destructive`
(21216269 — deletion, non_fast_forward, required_linear_history; no bypass,
binds the admin; force-push refused live, GH013) and `master-checks`
(21216270 — required checks `matrix` + `ubuntu-latest (release)`;
RepositoryRole/5 bypass at always, preserving the docs-only direct-push
carve-out, proved live). Committed intent `tools/branch-protection.json`
(POST-ready bodies); `tools/check-branch-protection.R` compares live API vs
committed over its COMPARED_FIELDS constant (seven fields incl. enforcement
and ref_name_exclude), fail-closed throughout; PROFILE consistency-gate line.

**Decisions:** D-047 (two-ruleset split by bypass scope; aggregator-job and
extra-context rejections; reopening conditions).

**Review:** three-lens fan-out; nine findings fixed at the gate — worst: the
exclude list was uncompared, a green checker over neutralized protection —
plus NA-safe sorts, dup-name refusal, gh-auth precondition, pagination,
fail-closed source_type, PROFILE rewrap, restored rationale clause, work-log
correction; two rejected with reasons; none met the return floor.
