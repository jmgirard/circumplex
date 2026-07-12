# M14: Automate the instruments() list — done 2026-07-12

- **Outcome:** `instruments()` now derives its printed listing and count from
  the packaged `circumplex_instrument` datasets (enumerated via
  `utils::data()`, class-filtered, formatted from each object's
  `$Details$Abbrev/$Name`) instead of a hardcoded 15-item block; the standing
  8-year-old `TODO` is gone. The listing can no longer drift from the shipped
  data — the class of bug that once printed "14 instruments" over 15 rows.
  PR #38 (squash `6008aa3`); `check()` clean (0/0/0), full CI green (7 jobs).
- **Test:** new data-derived drift-guard (`test-instrument_oop.R`) asserts the
  count line == number of `circumplex_instrument` datasets and every
  Abbrev/Name appears; proven red against the old body first (teeth). Snapshot
  regenerated (one-line delta).
- **M14-D1:** the data is the source of truth for the printed strings. Only
  user-visible change: IIP-SC's name resolves to its `$Details$Name`
  "Inventory of Interpersonal Problems Short Circumplex" (no comma) vs. the old
  hand-typed "Problems, Short". Cosmetic console-text on an informational
  printer (no return/API change); not a breaking change. History confirms the
  comma lived only in the string, never the data.
- **Review:** 3 fresh-context lenses (diff-bug/blame/prior-PR), zero findings;
  non-defects dropped (pre-existing undeclared `utils::`; minor double
  dataset-load in an interactive printer).
- **Scope note:** the three parked refactors (pole-snap cosmetic D-003,
  analytic-CI Hessian reuse, strict-tier syntax single-sourcing) stay
  opportunistic ROADMAP fold-ins — untouched.
