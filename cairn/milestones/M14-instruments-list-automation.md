<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M14: Automate the instruments() list

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Derive `instruments()`' printed list and count from the package's
`circumplex_instrument` datasets so it can never drift from the data.

## Scope

**In:**
- Replace the hardcoded 15-line body + count in `instruments()`
  (`R/instrument_oop.R:195`) with a runtime enumeration of the package's
  `circumplex_instrument` datasets, pulling each entry's abbreviation and
  full name from its `$Details$Abbrev` / `$Details$Name`, numbered and sorted
  by dataset name (the current order), with the count computed from the data.
- Remove the standing `TODO` at `R/instrument_oop.R:197`.
- A test that ties the printed output to the datasets (drift guard).

**Out:**
- The three parked/opportunistic refactors — 0-vs-360 pole-snap cosmetic
  (D-003 park), analytic-CI Hessian reuse, strict-tier syntax single-sourcing
  — stay ROADMAP fold-ins, not touched here.
- Any change to the instrument data objects themselves or to `instrument()`
  (singular loader).
- Reworking the enumeration into a user-facing return value / data.frame API
  (`instruments()` stays a `cat()` informational printer).

## Acceptance criteria

- [ ] `instruments()` contains no hardcoded instrument names or count: the
      list and the "N instruments" count are computed at call time from the
      package's `circumplex_instrument` datasets (evidence: source inspection;
      the `TODO` is gone).
- [ ] A test asserts the printed output is data-derived — the count line
      equals the number of `circumplex_instrument` datasets, and every such
      dataset's `$Details$Abbrev` and `$Details$Name` appears in the output —
      so adding or removing an instrument would change `instruments()` without
      a code edit (evidence: test passes; demonstrably keys off the data).
- [ ] `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage

- AC1 → T2
- AC2 → T1, T2
- AC3 → T3

## Tasks

- [ ] **T1** — Test-first: add a testthat test that enumerates the
      `circumplex_instrument` datasets (via `utils::data(package = "circumplex")`,
      filtered by class) and asserts `instruments()` output contains the
      derived count line and each instrument's Abbrev/Name. Written against the
      intended data-derived output (note the IIP-SC string resolves to the
      data's `$Details$Name`, without the current comma). Fails against the
      present hardcoded body where the two diverge.
- [ ] **T2** — Rewrite `instruments()` (`R/instrument_oop.R:195`) to enumerate,
      sort by dataset name, format `"N. ABBREV: Name (obj)\n"`, and compute the
      count from the data; delete the hardcoded block and the `TODO`. Keep it a
      `cat()` printer returning invisibly as today.
- [ ] **T3** — `devtools::document()` if roxygen changed; `devtools::test()`
      then `devtools::check()` clean; update any snapshot.

## Work log

- 2026-07-12: created by /milestone-plan. Fresh item surfaced from the
  `R/instrument_oop.R:197` TODO during a cleanup sweep; the M8–M13 dedicated
  cleanup run is done and the three remaining refactor candidates were kept as
  opportunistic fold-ins at the user's direction (question gate). Derivation
  feasibility confirmed: all 15 entries reproduce from `$Details$Abbrev/$Name`;
  the only text delta is the IIP-SC comma (the drift this fixes).

## Decisions

## Review
