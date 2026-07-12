# RR01: vctrs/S7 migration of the angle classes — review report

- **Date:** 2026-07-12
- **Brief:** `cairn/reviews/RB01-angle-class-vctrs-s7.md`
- **Reviewer:** independent Fable-tier design review (read-only; no code modified)
- **Verdict (Q7): DROP — keep S3.** Record a D-entry and remove/annotate the
  ROADMAP candidate row.

---

## 1. Is the migration justified at all?

**No material benefit. "Keep S3" is the recommendation.**

The concrete problems vctrs or S7 could solve for a vector class are, in
general: (a) class loss under `[`, `c()`, and `unlist()`; (b) silent
mixed-unit arithmetic (degree + radian without error); (c) undefined coercion
when classed vectors meet plain numerics in `c()`/comparisons; (d) validator
enforcement at construction; (e) nicer data-frame-column and tibble printing.

None of these is a live problem in this codebase, because the classes are
**transient boundary tags, not persistent data types**:

- Every consumer converts at the boundary with the
  `as_radian(as_degree(x))` / `as_degree(as_radian(x))` idiom and then does
  math on the result (`R/ssm_analysis.R:252,793,861`, `R/cpm_fit.R:515,689`,
  `R/ssm_sem.R:428`, `R/ssm_ci_accuracy.R:227`, `R/ssm_sem_syntax.R:278`).
  Several sites immediately strip the class with `as.numeric()` when a plain
  vector is wanted (`R/ssm_analysis.R:909`, `R/cpm_fit.R:1121,1330`).
- Both custom `quantile` methods `unclass(x)` as their first real statement
  (`R/ssm_bootstrap.R:174,187`) — the statistics are computed on bare
  doubles; the class exists only to *route dispatch into* the circular
  method and to tag the return value.
- Class loss under subsetting/`c()` never bites: nothing in the estimation
  path subsets a classed angle vector and then relies on dispatch. The one
  place class loss is actually *used* is `sapply(bs_t, quantile, ...)`
  (`R/ssm_bootstrap.R:119,124`), where simplification deliberately yields
  plain numerics for the CI assembly.
- Silent mixed-unit arithmetic is guarded by convention (convert-at-boundary
  idiom) plus the boundary test suite, not by the class system — and neither
  vctrs nor S7 would change that without writing `vec_arith`/method
  boilerplate whose whole job is to *forbid* operations the current code
  performs freely (see Q2).

Against this near-zero benefit stands the package's explicit doctrine:
statistical correctness outranks everything, minimal deps, and the class
weight is ~100 lines (`R/ssm_oop.R:1–100`) plus two quantile methods
(`R/ssm_bootstrap.R:172–192`). Migrating a working, tested, CRAN-shipped
100-line S3 tag system to a new object system is modernization for its own
sake — precisely the category of change whose only possible numeric outcome
is "identical or worse."

## 2. If justified, vctrs or S7 — which, and why?

Conditional answer (it is not justified, per Q1): **vctrs**, and clearly not
S7 — but the fit analysis itself is a second argument for keeping S3.

- **S7 is the wrong tool.** These are unit-tagged numeric *vectors*. S7's
  domain is record-style objects with formal properties and validators;
  S7's own guidance defers vector classes to vctrs. S7 gives no `[`, `c()`,
  or data-frame-column behavior for free, adds a second dispatch system the
  package otherwise never uses, and registering S7 methods on the S3
  generics `print`/`quantile` buys nothing over plain S3 methods. Reject.
- **vctrs fits the *shape* (unit-tagged vectors with coercion rules) but its
  central design value — strictness — is anti-fit for this code.** A
  `vctrs_vctr` subclass errors on arithmetic, `Math` group generics, and
  cross-type combination unless `vec_arith()`, `vec_math()`,
  `vec_ptype2()`, and `vec_cast()` methods are written. The estimation path
  relies on exactly the transparent numeric semantics vctrs removes:
  - `angle_dist()` does `((x - y + pi) %% (2*pi)) - pi` and `d == -pi`
    directly on classed `circumplex_radian` inputs (`R/utils.R:65–69`,
    called with classed args at `R/utils.R:23,25`, `R/cpm_fit.R:412,588,723`).
    Under vctrs this errors without `vec_arith` methods.
  - Trig and conversion arithmetic on classed vectors:
    `x * (180/pi)` inside the converters themselves (`R/ssm_oop.R:52,85`),
    `cos()`/`sin()` on converted angle vectors downstream, `round(x, digits)`
    in the print methods (`R/ssm_oop.R:92,99`).
  - `sapply()` simplification of classed quantile returns
    (`R/ssm_bootstrap.R:119,124`) — `unlist`/`simplify2array` over
    vctrs objects is not attribute-transparent the way it is for S3
    numeric subclasses.
  - Rcpp ingestion: classed S3 doubles pass into `ssm_parameters_cpp()`
    untouched; a vctrs class still stores a double but every call site's
    surrounding arithmetic must first be made vctrs-legal.

  Reproducing today's behavior would mean writing `vec_ptype2`, `vec_cast`,
  `vec_arith`, `vec_math`, `format`, and `quantile` methods for three
  classes — several times the current code — to end at behavioral parity.
  The S3 numeric subclass gives numeric-vector semantics (arithmetic, `NA`
  handling, `c()`, subsetting, data-frame columns) *for free* and is the
  best-preserving option of the three systems. That is the honest answer to
  the question as posed.

## 3. Dependency cost

- **vctrs:** transitively already in every user's install tree — ggplot2
  (Imports, `DESCRIPTION:36`) itself imports vctrs/cli/glue/lifecycle — so
  no new install weight. But a *direct* Import is a different thing: it
  couples the statistical core's class layer to tidyverse-infrastructure
  release cadence and API (vctrs is stable but actively evolved), and it
  breaches the "base R + minimal deps, no tidyverse in package code" design
  bar for the estimation path specifically. CRAN-stability: high. R floor:
  vctrs requires R ≥ 3.5; the package currently declares R ≥ 3.4
  (`DESCRIPTION:31–32`), so the floor rises.
- **S7:** dependency-light (no hard transitive deps) and RConsortium-backed,
  but pre-1.0 with an evolving API; seamless syntax/dispatch assumes modern
  R (native support landed in R 4.3), which is a real floor consideration
  for a package that has kept R ≥ 3.4. Adopting a still-maturing object
  system for three internal tag classes in a CRAN statistics package is a
  stability regression, not a modernization.
- **House rule:** either addition is a dependency decision requiring a
  D-entry, never unilateral. Given Q1's "no material benefit," neither
  clears the bar. The correct D-entry is the *negative* decision (keep S3),
  so the question is settled and not re-litigated each time someone reads
  the candidate row.

## 4. Exported-API preservation and deprecation path

**Correction to the brief:** the generics `as_degree()`/`as_radian()` are
**not exported**. `NAMESPACE` contains only `S3method()` registrations for
them (`NAMESPACE:3–8`); neither appears in the `export()` list
(`NAMESPACE:28–60`), no `man/` page or vignette mentions them (verified by
grep), and external code can only reach them via `:::`. The genuinely
exported dispatch surface is:

- `print.circumplex_degree` / `print.circumplex_radian` (`NAMESPACE:14,16`) —
  user-visible because classed vectors **leak into user-facing objects**:
  the exported `octants()`/`poles()`/`quadrants()` return
  `circumplex_degree` vectors (`R/convenience_functions.R:34,50,66`), the
  `circumplex_ssm` object carries `details$angles` as a degree vector
  (`R/ssm_analysis.R:410,532`) and `results$d_est/d_lci/d_uci` as degree
  columns (`R/ssm_bootstrap.R:148–151`), and `ssm_sem_syntax()` attaches a
  degree vector as an attribute (`R/ssm_sem_syntax.R:741`, documented at
  `:235`). Snapshot tests pin the print format
  (`tests/testthat/test-ssm_oop.R:120–121`,
  `tests/testthat/_snaps/ssm_oop.md`).
- `quantile.circumplex_radian` / `quantile.circumplex_contrast_radian`
  (`NAMESPACE:20–21`) — dispatchable by any user holding a classed vector.

So the `irreversible-api` tripwire is real but **smaller than briefed**.
What must survive any migration:

1. **Class identity**: `inherits(x, "circumplex_degree")` on `octants()`
   output and on the `d_*` results columns must keep returning `TRUE`. Both
   vctrs and S7 can prepend their machinery to the class vector while
   retaining the `circumplex_*` strings, so identity is preservable — but
   the class vector *changes* (`c("circumplex_degree", "vctrs_vctr",
   "numeric")` or S7 equivalents), which alters `class(x)[2]`-style checks
   and `expect_s3_class(..., exact = TRUE)` patterns in any downstream code.
2. **Print output**: vctrs' default `format`/`print` ("`<circumplex_degree[8]>`"
   header style) differs from the current `cat(round(x, 3), "\nDegrees\n")`;
   the methods would need reimplementing verbatim to keep snapshots and any
   user-facing output identical.
3. **Callers doing `circumplex:::as_degree(x)`** (the `:::` case): the
   "tag, don't convert" semantics of `as_degree.default`/`as_radian.default`
   (`R/ssm_oop.R:37–39,70–72`) must survive — see Q5.

Deprecation cycle if migrated: because the generics are internal, no
soft-deprecation of function signatures is needed; the obligation is
behavioral (class vector, print format, quantile dispatch) across one CRAN
release with a NEWS entry, plus a revdep check (CRAN revdeps may dispatch or
snapshot on these classes). Breakage candidates: any caller that (a) tests
`identical(class(x), c("circumplex_degree", "numeric"))`, (b) snapshots
printed output of `octants()` or `circumplex_ssm` internals, or (c) relies
on classed columns surviving `rbind`/`c()` with the current S3-attribute
semantics.

## 5. Statistical-invariant preservation (the hard stop)

Mapping the crux code onto either system is *mechanically* safe only because
both quantile methods immediately `unclass()` and compute on bare doubles:

- **`quantile.circumplex_radian`** (`R/ssm_bootstrap.R:172–181`): circular
  mean → center/unwrap to (−π, π] → `stats::quantile` → re-wrap to [0, 2π),
  then the **pole-snap** at `:179`
  (`out[abs(out - 2*pi) < .Machine$double.eps * 2] <- 0`). All of this is
  class-free arithmetic; the only migration-sensitive lines are the entry
  (`x <- unclass(x)` — becomes `vec_data(x)`; must strip *all* wrapper
  attributes) and the exit (`as_radian(out)` — must preserve the names that
  `stats::quantile` attaches, which vctrs constructors do not do by default;
  the CI assembly at `:119–124` consumes those values positionally via
  `sapply`, but name loss would still change object structure and any
  snapshot).
- **`quantile.circumplex_contrast_radian`** (`:185–192`): identical
  centering, returns `quantiles_centered + mean_angle` on the **continuous
  branch — deliberately unclassed and unwrapped** (negatives and >2π
  allowed). A vctrs re-tagging of this return, or any "canonicalize to
  [0, 2π)" cast, would destroy the branch-contiguity invariant
  (DESIGN.md:69). The method must keep returning a *plain* numeric.
- **Branch alignment** (`:131–146`): operates on plain-numeric data-frame
  columns (class already stripped by `sapply` at `:119,124`); shifts both CI
  endpoints by the same multiple of 2π. Untouched by any class migration —
  *provided* the class actually is stripped at `:119/124`; if a vctrs class
  survived `sapply` simplification differently, endpoints could arrive
  classed and the subsequent `as_degree(as_radian(x))` at `:148–151` would
  hit non-default methods. This is the subtlest silent-perturbation site.
- **The tag-don't-convert trap:** `as_radian.default` **tags without
  converting** (`R/ssm_oop.R:70–72`), and the pipeline leans on this —
  `:148–151` re-interprets plain radians via `as_degree(as_radian(x))`. A
  vctrs `vec_cast`-shaped design invites "cast = unit conversion"
  semantics, which would multiply by 180/π at the wrong times or make
  `c(degree, radian)` silently convert. Any migration must keep conversion
  **explicit-only** and must not define a degree↔radian `vec_ptype2` (no
  common type — combining units should stay an error or a class-drop, never
  a silent conversion).
- **D-003** (DECISIONS.md:36–45): the exactly-360.0 pole report originates
  in the C++ estimator (`modu(atan2(...), 2π)`), out of scope — no class
  system touches it. But note the *interaction*: the quantile pole-snap at
  `:179` maps ~2π CI endpoints to **0**, the opposite label from D-003's
  360.0 for point estimates. That asymmetry is a recorded, parked cosmetic
  (D-003 "Consequences"). A migration must not "harmonize" it in passing;
  D-003 stands.
- **Formatting perturbation:** vctrs default `format()` methods round/pillar
  differently; every print path (`R/ssm_oop.R:91–100`, the ssm print
  methods that consume degree columns) must be pinned by snapshot before
  and after.

**Oracles for byte-for-byte certification** (satisfies the ≥2-independent-
oracle-types doctrine):

1. **Old-vs-new twin harness** (self-oracle, type 1): seeded `ssm_analyze()`
   battery — means and correlations, contrast and non-contrast, both
   engines — on profiles peaking at 0°/360°, contrasts near ±180°, flat
   profiles, plus random profiles; `identical()` on class-stripped `results`
   plus explicit attribute/class-vector comparison. Same seeds ⇒ same
   resample indices ⇒ any difference is the migration's.
2. **Independent plain-R reimplementation** (type 2): a standalone script
   implementing center→unwrap→quantile→re-wrap (and the contrast
   continuous-branch variant) with no package code, run over the same
   replicate matrices; compare to machine precision. (Do **not** use
   `devel/g2xx1.txt` — flagged un-vetted.)
3. **Existing pinned regressions** (type 3): the seeded numerical
   regression tests (`tests/testthat/test-ssm_analysis.R`), the quantile
   boundary tests (`tests/testthat/test-ssm_bootstrap.R:1–32` — wrap-around
   at 0/360, all-NA, NA-mixed), and the print snapshots
   (`_snaps/ssm_oop.md`) must pass byte-identically with **zero snapshot
   updates accepted**.

## 6. Sizing and shape

If it were done despite Q1–Q3: **one milestone, one PR, 1–2 sessions**. The
surface is small — `R/ssm_oop.R:1–100`, two quantile methods, two inline
`structure()` construction sites (`R/ssm_bootstrap.R:111–113`,
`R/ssm_ci_accuracy.R:899–900`), nine consumer files using only the four
constructor/converter functions, plus tests/snapshots and the twin-harness
oracle. No split is warranted; a split would only smear the
byte-identity verification across PRs.

But the candidate's own "fold into the milestone that next touches the code"
(`cairn/ROADMAP.md:26`) is also not the right vehicle: folding a
class-system swap into an unrelated milestone couples that milestone's
review to a full statistical-equivalence certification, inflating its risk
for zero functional gain. If the answer were "migrate," standalone would be
the *only* defensible shape. Since the answer is "don't," the right action
is to resolve the candidate row, not park it.

## 7. Bottom line

**DROP.** Keep the S3 numeric-subclass design. Reasoning a `/milestone-plan`
run can act on directly:

1. No engineering problem exists that the migration solves (Q1): the classes
   are transient dispatch tags with a disciplined convert-at-boundary idiom;
   every generic-class-system benefit is either already achieved by
   convention+tests or actively unwanted (vctrs strictness would break
   `angle_dist` and the conversion arithmetic as written).
2. The best-fitting candidate system (vctrs) preserves numeric-vector
   semantics *worse* than the status quo and would multiply the code to
   reach behavioral parity (Q2); S7 is the wrong tool for vector classes.
3. The dependency bar is not cleared (Q3) — a direct vctrs Import couples
   the statistical core to tidyverse infrastructure against explicit
   doctrine; S7 is pre-1.0.
4. The only possible numeric outcome is "identical or worse" for code
   implementing the package's hardest invariants (pole-snap, continuous
   branch, branch alignment, D-003), on a CRAN package whose doctrine says
   statistical correctness outranks everything (Q5).
5. The exported surface, while smaller than briefed (generics are internal —
   Q4), still makes class identity and print format CRAN-visible, adding a
   deprecation/revdep obligation to a change with no user benefit.

**Re-trigger condition** (record with the D-entry): reopen only if a
concrete, test-demonstrated defect traceable to the S3 tag design appears —
e.g., a real mixed-unit bug that the convert-at-boundary idiom failed to
catch, or a hard requirement for angle vectors as first-class tibble columns
in a user-facing API. Modernization alone never re-triggers.

---

## Beyond the brief

- **B1 — Brief inaccuracy (context only):** the brief states DESCRIPTION
  Imports are "Rcpp, rlang, RcppArmadillo". Actual Imports are `boot,
  ggforce, ggplot2, htmlTable, parallel, Rcpp, rlang, stats`
  (`DESCRIPTION:33–41`); RcppArmadillo is LinkingTo (`:55–57`). This
  *strengthens* the drop verdict's nuance: vctrs is already transitive via
  ggplot2, so the cost is API coupling, not install weight — and the benefit
  is still nil.
- **B2 — Brief inaccuracy (material, Q4):** `as_degree()`/`as_radian()` are
  not exported; only their S3 methods are registered (`NAMESPACE:3–8` vs
  the `export()` list at `:28–60`). The `irreversible-api` tripwire is
  narrower than briefed. If keeping S3, consider *deciding* whether these
  generics should ever be exported (they are documented as a conversion API
  in DESIGN.md:51–53 but unreachable by users); either export-and-document
  or record them as intentionally internal.
- **B3 — Duplicate inline class construction:** the
  `circumplex_contrast_radian` class vector is assembled by hand at
  `R/ssm_bootstrap.R:111–113` and `R/ssm_ci_accuracy.R:899–900` instead of
  through a `new_contrast_radian()` constructor beside
  `new_degree`/`new_radian` in `R/ssm_oop.R`. A trivial-tier DRY cleanup,
  independent of any migration.
- **B4 — Type-inconsistent all-NA return:** both quantile methods return
  bare logical `NA` (not `NA_real_`, not classed) when all input is NA
  (`R/ssm_bootstrap.R:173,186`). Downstream `sapply` assembly tolerates it
  (tested at `test-ssm_bootstrap.R:22–25`), but it is a latent type wart any
  future class work would trip over; worth a comment or an `NA_real_` if
  ever touched.
- **B5 — `quantile.circumplex_radian` is also load-bearing for CPM CIs**
  (`R/cpm_fit.R:1119–1121` calls it directly, documented at `:1025`), not
  just SSM bootstrap — any future touch of these methods must include the
  CPM angle-CI path in its oracle battery.

## Recommendations

1. **Apply:** Drop the vctrs/S7 migration candidate. Record a D-entry
   ("keep S3 for angle classes") citing this report, with the B-re-trigger
   condition from Q7; remove or annotate the `cairn/ROADMAP.md:26` candidate
   row so the question is closed, not parked.
2. **Apply (trivial tier):** Add `new_contrast_radian()` to `R/ssm_oop.R`
   and use it at the two inline `structure()` sites (B3).
3. **Consider:** Decide the intended status of the unexported
   `as_degree`/`as_radian` generics (B2) — export-and-document or record as
   deliberately internal — in a future doc-touching milestone.
4. **Consider:** When any future work touches the quantile methods, extend
   the oracle battery to the CPM path (B5) and normalize the all-NA return
   type (B4) under test.
5. **Reject-with-reason:** Migrating to S7 — wrong tool for vector classes;
   adds a pre-1.0 dependency and a second dispatch system for zero benefit.
6. **Reject-with-reason:** Migrating to vctrs — best-in-class for vector
   types in general, but its strictness contradicts the transparent numeric
   semantics the estimation path relies on (`R/utils.R:65–69`,
   `R/ssm_bootstrap.R:119–151`), triples the class code to reach parity, and
   puts bit-identical statistical output at risk for a purely cosmetic
   modernization.

**Bottom-line verdict: DROP (keep S3).**
