# M87: Retire the norms-audit abort apparatus for a manifest check

**Status:** done (2026-08-15, PR #115 https://github.com/jmgirard/circumplex/pull/115)

**Goal:** Replace the abort-site registry, matcher-floor, cross-discrimination and
denylist machinery M81–M83 built around `data-raw/audit-norms.R` with one manifest test.

**Outcome:** `helper-norms-audit-manifest.R` holds a generated manifest of the audit
script's 33 abort sites keyed (kind, binding, key, ordinal); `test-norms-audit-manifest.R`
asserts it set-equal to a fresh parse walk, so a guard added with no entry reddens the
suite. `expect_audit_abort(expr, key)` replaced 17 `expect_abort_at_site()` calls —
resolves a key to one site, matches by kind, requires the message rendered by exactly one
key, folding in the retired matrix's cross-site property. Deleted:
`test-norms-audit-denylist.R`, `SCRIPT_ABORTS` and 20 blocks in
`test-norms-audit-markers.R` (1292→348), 19 helper definitions (756→358).
`tools/m82-gate-floor.R` re-pointed, two anchors broken since M86. `data-raw/` unchanged.

**Decisions:** D-042 — the retirement, its three surrendered properties (the denylist's
non-`stop()` sweep, the matrix's build-time pass, the guard being opt-in), and what reopens it.

**Review:** three lenses, 29 findings, 4 ≥80, none ≥90; return floor not reached. Fixed:
F22 (88) and F24 (85), false figures in D-042 — the line count is now procedural, its
first repair having gone stale within the hour; F4 (82) fail-closed refusal on an unknown
kind; F14 (82) a floor-headroom claim naming a file asserting neither floor. F13 (78)
folded into the M87-lineage row: surviving walk helpers lost their unit tests.
