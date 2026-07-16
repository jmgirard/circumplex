# M24 — Tidyverse-style NSE in the circumplex user API: evaluation memo

Deliverable of milestone M24 (`cairn/milestones/M24-nse-evaluation.md`).
Question: should circumplex's user-facing functions support tidyverse-style
non-standard evaluation (bare column names and/or tidyselect helpers), in
place of or alongside the current standard-evaluation (SE) interface
(character names or numeric indices, validated by `is_var()`, `R/utils.R:199`)?

Historical anchor: pre-1.0 circumplex **had** rlang tidy-eval NSE
(NEWS.md:660); the v1.0.0 breaking release deliberately removed it —
"Nearly all code rewritten/refactored to streamline and reduce dependencies
… Removed support for non-standard evaluation" (NEWS.md:412–416). This memo
evaluates whether that decision should stand, on four evidence strata.

## §1 Prior art: column-spec interfaces of comparable CRAN packages

Seven packages surveyed; versions are the locally installed ones whose
signatures/docs were inspected directly (2026-07-16), except rstatix (CRAN
page). "Formula-NSE" (classical S formulas) is distinguished from
tidy-eval/tidyselect NSE throughout — formulas are ubiquitous in statistical
R and are not what this question is about.

| Package (version) | Interface for variable/column spec | Style | Citation |
|---|---|---|---|
| lavaan (0.6-21) | model as a character syntax string; variables named as strings inside it (`cfa(model, data)`) | SE | `?lavaan::cfa` (args `model`, `data`) |
| psych (2.6.5) | matrices/data frames; `scoreItems(keys, items)` with keys as item names/numbers | SE | `?psych::fa`, `?psych::scoreItems` |
| lme4 (2.0.1) | `lmer(formula, data)` | formula-NSE | `?lme4::lmer` |
| survey (4.5) | `svydesign(ids = ~psu, strata = ~stype, …)` | formula-NSE | `?survey::svydesign` |
| OpenMx (2.22.11) | `mxModel(manifestVars = <character>)` | SE | `?OpenMx::mxModel` |
| datawizard (1.3.1) | `select` accepts literal names, strings, `"a:c"` ranges, formulas, positions, in-house `starts_with()`-style helpers, predicate functions — **without tidyselect** (Imports: insight, stats, utils only) | dual SE + in-house NSE | `?datawizard::extract_column_names` |
| rstatix (CRAN) | tidyverse-native throughout; Imports dplyr, tidyr, tidyselect (≥ 1.2.0), rlang, purrr, broom, tibble, … | tidy-eval NSE | CRAN rstatix page: "coherent with the 'tidyverse' design philosophy" |

**Reading.** Mainstream statistical *modeling* packages — including every
package circumplex already interoperates with (lavaan as SEM engine, OpenMx
as test oracle, psych as the closest substantive neighbor) — use SE strings/
indices or classical formulas. Tidy-eval NSE appears only where it is the
package's identity (rstatix) or where a package re-implemented selection
in-house at real complexity cost to avoid the dependency stack (datawizard
documents ~8 accepted `select` input forms — evidence both that some users
value flexible selection *and* that supporting it dependency-free is a large
ambiguity surface to own). circumplex's peer group is the modeling column,
not the tidyverse-workflow column.

## §2 Dependency delta

(to follow — T2)

## §3 Ergonomics and ambiguity on real call sites

(to follow — T3)

## §4 Synthesis and verdict

(to follow — T4)
