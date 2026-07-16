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

Method: recursive Imports closure from live CRAN metadata (2026-07-16),

```r
ap <- available.packages(repos = "https://cloud.r-project.org")
tools::package_dependencies("tidyselect", db = ap,
                            which = "Imports", recursive = TRUE)
```

then subtract base packages and circumplex's current Imports (DESCRIPTION:
boot, ggforce, ggplot2, htmlTable, parallel, Rcpp, rlang, stats; rlang is
imported today **only** for the ggplot2 `.data` pronoun — zero user-facing
NSE machinery in package code).

**Net-new Imports** (6): `tidyselect`, `vctrs`, `cli`, `glue`, `lifecycle`,
`withr`.

**R-version floors** (CRAN Depends fields): tidyselect ≥ 3.4, cli ≥ 3.4,
lifecycle ≥ 3.6, withr ≥ 3.6, **vctrs ≥ 4.0.0, glue ≥ 4.1** — adopting
tidyselect raises circumplex's floor from `R (>= 3.4)` to `R (>= 4.1)`.

Two aggravating precedents:

- **vctrs is in the closure.** D-006 (2026-07-12, Fable-reviewed RB01→RR01)
  explicitly refused a direct vctrs Import as breaching the minimal-deps /
  no-tidyverse-in-package-code doctrine (`cairn/DESIGN.md` "Dependency
  policy"). Taking it transitively via tidyselect lands the same dependency
  weight the same decision refused.
- **The no-dependency alternatives are not free.** (a) Bare-rlang NSE
  (`enquo()`/`eval_tidy`, no tidyselect) adds no packages — rlang is already
  imported — but delivers only bare-name capture (no `starts_with()`), while
  importing the full ambiguity surface of §3. (b) The datawizard route
  (in-house re-implementation) avoids the stack but means owning a parser
  for ~8 input forms in a package whose doctrine is that statistical
  correctness outranks all other concerns — maintenance surface in exactly
  the wrong place.

## §3 Ergonomics and ambiguity on real call sites

### 3.1 Shipped vignette call sites, rewritten in hypothetical NSE form

Site 1 — `introduction-to-ssm-analysis.Rmd:362` (the canonical call):

```r
# current SE                                   # hypothetical NSE
ssm_analyze(jz2017, scales = PANO())           ssm_analyze(jz2017, scales = c(PA, BC, DE, FG, HI, JK, LM, NO))
```

The instrument helper is *shorter than the NSE form*: the main ergonomic
argument for NSE ("stop typing quoted names") was already solved in 1.0.0 by
`PANO()`-style helpers, which every vignette teaches.

Site 2 — `intermediate-ssm-analysis.Rmd:66-68, 97-100` (measures/grouping):

```r
# current SE                                   # hypothetical NSE
ssm_analyze(jz2017, scales = PANO(),           ssm_analyze(jz2017, scales = PANO(),
  measures = c("NARPD", "ASPD"),                 measures = c(NARPD, ASPD),
  grouping = "Gender")                           grouping = Gender)
ssm_analyze(jz2017, scales = PANO(),           # NSE cannot express measures = 10:12
  grouping = "Gender", measures = 10:12)       #   without all_of()/positional helpers
```

Saving: four quote characters per call. Cost: the vignette's own
`measures = 10:12` numeric-index idiom now needs tidyselect position
semantics or a dual interface (§3.2's ambiguity).

Site 3 — `using-instruments.Rmd:83,108-110` (long item selections):

```r
# current SE                                   # hypothetical NSE
ipsatize(raw_iipsc, items = 1:32)              ipsatize(raw_iipsc, items = starts_with("IIP"))
score(raw_iipsc, items = 1:32, iipsc)          score(raw_iipsc, items = num_range("IIP", 1:32), iipsc)
```

This is NSE's best case — but it is *worse than useless here*: `score()`'s
contract requires items **in ascending instrument order**
(using-instruments.Rmd:105); `starts_with()` returns data-frame column
order, silently mis-scoring shuffled items that `items = 1:32`-style
explicit indexing forces the user to confront. The one site where
tidyselect helpers shine is the one site whose statistical contract they
undermine.

### 3.2 Runnable ambiguity spikes

Run 2026-07-16, R 4.x, tidyselect 1.2.1 / rlang (session library); output
verbatim.

Spike A — data-mask collision (env variable shadowed by a same-named
column). The user's `sel` holds the intended scale names, but the data
happen to contain a column named `sel`:

```r
library(tidyselect); library(rlang)
df  <- data.frame(PA = 1, BC = 2, sel = 3)
sel <- c("PA", "BC")
eval_select(expr(sel), df)          # sel
                                    #   3    <- silently selects COLUMN sel
eval_select(expr(all_of(sel)), df)  # PA BC
                                    #  1  2   <- correct, but only via all_of()
```

A wrong-columns silent selection feeding an SSM fit is a *statistical*
wrong-answer channel that the SE interface cannot produce (`data[sel]` uses
the env variable, always).

Spike B — programming against an NSE API requires embracing. A toy NSE
front-end and a user's perfectly natural wrapper around it:

```r
toy_nse       <- function(data, scales) names(eval_select(enquo(scales), data))
wrap_naive    <- function(data, v) toy_nse(data, v)
wrap_embraced <- function(data, v) toy_nse(data, {{ v }})
d2 <- data.frame(PA = 1, BC = 2, DE = 3)
toy_nse(d2, c(PA, BC))         # [1] "PA" "BC"
wrap_naive(d2, c(PA, BC))      # ERROR: object 'PA' not found
wrap_embraced(d2, c(PA, BC))   # [1] "PA" "BC"
```

Every user who loops SSM analyses over scale sets or wraps `ssm_analyze()`
in a lab utility — the package's research audience does exactly this — must
learn `{{ }}`. The current SE interface makes the naive wrapper just work.

## §4 Synthesis and verdict

(to follow — T4)
