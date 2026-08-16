# M88: Fence the norms-audit walk helpers M87 kept

**Status:** done (2026-08-15, PR #116 https://github.com/jmgirard/circumplex/pull/116)

**Goal:** Give the abort-site walk helpers that survived M87's retirement direct
tests, and delete the ordinal the manifest identity no longer earns.

**Outcome:** `tests/testthat/test-norms-audit-walk.R` fences four branches no shipped
site reaches — the truncation-marker discrimination (`norms_audit_stopifnot_stem()`,
`NORMS_AUDIT_VERDICT`), `audit_key_matches()`'s unknown-kind refusal and its
`stopifnot_named` branch, and both `refuse_unenumerable()` sites. The identity drops
to (kind, binding, key): `norms_audit_assign_ordinals()` is deleted, its separability
kept as `anyDuplicated()` refusals on both sides of the manifest comparison beside a
field-set assertion catching a fourth field under any spelling. All 33 sites were
ordinal 1, no triple duplicated.

**Decisions:** D-043 — the identity drops the ordinal for a duplicate refusal;
annotates D-042's four-part key, which otherwise stands.

**Review:** three lenses, 30 findings, 2 actioned, 28 logged. F1 (85) returned the
milestone: AC3's naming assertions were vacuous, `refuse_unenumerable()` echoing the
deparsed call so a bare name matched whatever the naming logic did. F2 (80) the AC1
key now calls the shipped derivation. AC6 nine mutants, nine killed, restores
hash-verified; a tenth retracted invalid, `squish()` making the key invariant to it.
