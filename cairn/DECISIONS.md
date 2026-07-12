# Decisions

Append-only. Never renumber; supersede with a new entry.

**Pre-migration decisions:** the full decision rationale predating cairn lives
in `cairn/DESIGN.md` (kept verbatim at migration — a living design doc with an
embedded decision log and statistical conventions) and in the entombed
`cairn/legacy/`. Only still-governing, cross-cutting decisions are re-recorded
below, each citing its legacy anchor (Compromise A); full decision-log
extraction is deferred to a later `/design-interview` run. The statistical
invariants in CLAUDE.md / DESIGN.md "Statistical conventions" are hard
constraints whose IP/GP formalization is likewise deferred to
`/design-interview`, not forced at migration time.

### D-001 (2026-07-07, re-recorded): v2.0.0 bundles M2–M5 as one CRAN submission

**Context:** Progress outran the original tiered submission train; CRAN
discourages churn (~1 submission / 1–2 months).
**Decision:** Fold M2 (inference), M3 (viz), M4 (Browne + CI trustworthiness),
M4.5 (structure tests), and M5 (SEM) into a single v2.0.0 release (target
~2026-08-02; code freeze ~2026-07-26). Scope is the variable, never the date or
the statistics. M6 (longitudinal) is excluded → its own ~v2.1.0.
**Consequences:** GitHub milestones decouple from CRAN submissions; work
accumulates on master until the release train leaves. Source: legacy ROADMAP
"CRAN release strategy".

### D-002 (2026-07, re-recorded): BCa CIs dropped for circular displacement

**Context:** BCa's bias-correction/acceleration are order-statistic concepts
defined on a line, not a circle.
**Decision:** No BCa CIs; the one real beneficiary (amplitude coverage near
zero) is handled by M4's CI-trustworthiness diagnostic instead.
**Consequences:** Percentile/basic bootstrap + Monte Carlo only. Source: legacy
ROADMAP "Milestone 2".

### D-003 (2026-07, re-recorded): 0°/360° pole reported as exactly 360.0, not canonicalized

**Context:** A profile peaking on the pole yields `atol2`≈−3e-17 →
`modu(·,2π)`=2π=360.0 (an fmod-at-the-edge float artifact, measure-zero for real
data).
**Decision:** Do not canonicalize; 360.0 matches the package's LM=360
convention. Tests at the boundary accept either ~0 or ~360.
**Consequences:** A pole-hugging CI endpoint may still print 0.0 (the opposite
label) — a parked cosmetic follow-up. Source: DESIGN.md "Statistical
conventions" (G2 decision).

### D-004 (2026-07-07, re-recorded): between-release working practice

**Context:** Real version numbers are bound to CRAN submissions only.
**Decision:** At each milestone close: archive to MILESTONES-ARCHIVE (now
`cairn/`), bump the DESCRIPTION dev suffix, add a lightweight git tag, and run a
milestone-close `/code-review` over the cumulative diff (`high`, or `max` for
statistically risky milestones).
**Consequences:** `install_github` users can identify milestone state; the
CRAN-release review verifies already-reviewed strata rather than making a first
deep pass. Source: legacy ROADMAP "Between releases".
