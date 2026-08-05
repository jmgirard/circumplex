# Design interview 2026-08-04 — banked-candidates ledger

_Working file for the interview's Phase 2 (principles). Scope: proto-principles
banked during Phase 1, awaiting IP/GP/skip classification. Deleted at Phase 2
write-out; it claims no other file's ownership._

Phase 1 facts are written into DESIGN.md "Purpose & scope" (same commit).

## Banked proto-principles

1. **Statistical correctness outranks all other concerns** (CLAUDE.md standing
   doctrine; every scope decision D-001→D-033 defers to it).
2. **Protective guardrails are commitments** — compute anything well-defined,
   caution loudly; refuse only ill-defined/wrong-object inputs
   (refuse-don't-coerce, M18 lesson; D-007; D-017).
3. **Contract boundary: circumplex constructs only** — never general circular
   statistics.
4. **Capability bar: published method + feasible independent oracle (≥2 types)**
   before shipping; research program a legitimate secondary driver.
5. **Minimal dependencies** — base R + few Imports, no tidyverse in package
   code, SE-only user API (D-006, D-014, D-020, D-022 lineage).
6. **Post-2.0 API stability** — breaks need statistical cause or a gated
   irreversible-api decision, plus a deprecation cycle.
7. **Angle conventions are invariant** — degrees [0,360) user-facing, LM = 360,
   contrast = second minus first in (−180, 180], radians internal
   (CLAUDE.md statistical invariants; D-003).
8. **RNG contract** — stochastic statistical output iff the global stream is
   consumed; everything else deterministic (DESIGN.md Reproducibility).
9. **Scope is the variable, never the statistics or the date**
   (D-001/D-008/D-012/D-018 doctrine).
10. **Vignette prose is statistically precise; teaching the field is mission**
    (CLAUDE.md style rule, elevated at interview).
11. **Instrument roster bar: published norms + provenance + permission.**
12. **Boundary behavior is where bugs hide** — estimation changes require tests
    at 0°/360°, ±180°, flat profiles (CLAUDE.md).

## Phase-2 additional sources still to run

- Mine git history for implicit principles (e.g., the recurring
  refuse-don't-coerce fixes; oracle-first milestone pattern).
- Derive candidates from the domain (irreplaceable published norms →
  provenance discipline; applied-audience → guardrail inviolability).
- Stress-test adopted set against Phase-1 decisions (e.g., "compute
  anything well-defined" vs. protective-guardrails commitment).
