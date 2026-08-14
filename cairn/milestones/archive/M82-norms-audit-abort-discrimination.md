# M82: Make the norms-audit abort registry discriminating

**Status:** done (2026-08-14, PR #110 https://github.com/jmgirard/circumplex/pull/110)

**Goal:** Identify and match every abort site so no fixture satisfies another's assertion, and sweep for the spellings M81 misses.

**Outcome:** Sites carry `(kind, enclosing binding, key, ordinal)` — registry
ordinals declared, collected computed — and `norms_audit_build_registry()`
refuses a duplicate declared identity. The two `source note not found` sites,
which no message or set comparison separates, are told apart by a
calling-handler frame capture asserting the innermost sourced binding.
`norms_audit_matcher()` builds matchers once at registry build: a
15-literal-character `stop`-key floor fails the build, a match-time stem floor
closes the one-character-stem hole, and `expect_abort_at_site()` consumes a
prebuilt matcher under one shared C-locale pin. `norms_audit_denied_calls()`
denies `rlang::abort`/`cli_abort` heads, `do.call` dispatch and non-head
aliasing, on a 16/9 partition asserting which rule fires. `data-raw/audit-norms.R`
is unchanged; `tools/m82-gate-floor.R` re-runs M81's five mutations.

**Decisions:** `Driving RR` stayed `—` (RR17's BC bullets are unparseable to
`cairn_validate`); BC6–BC11 ingested by substance, rules (ii)/(iii) widened.

**Review:** 16 findings, four actioned; F2/F3/F4 fixed at the gate, F1 (85, a
stem floor rejecting a correct site's own message) sent to a candidate row. AC2
was amended mid-implementation: its swap-reddens clause was false.
