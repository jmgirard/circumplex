# RB01: vctrs/S7 migration of the angle classes (pre-plan — no milestone yet)

- **Date:** 2026-07-12
- **Output required:** write findings to `cairn/reviews/RR01-angle-class-vctrs-s7.md`
- **RB tripwires:** `ip-touching`, `irreversible-api`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

This is a **pre-planning** brief: no milestone exists yet. The candidate row
"move degree/radian/contrast classes onto vctrs/S7" in `cairn/ROADMAP.md` is
flagged as needing this review *before* it is plannable. Your report will
decide whether a milestone is written at all, and if so, on what terms.

## Background

**circumplex** is a CRAN R package for Structural Summary Method (SSM)
analysis of circumplex data. It is deliberately built on base R plus minimal
dependencies (Rcpp/RcppArmadillo for the estimator core, rlang for
conditions, ggplot2 for plots). There is currently **no vctrs and no S7
dependency**; DESCRIPTION Imports are `Rcpp, rlang, RcppArmadillo`.

Angles are the package's central data type. The API convention: **all
user-facing angles are degrees in [0, 360); all trigonometry is radians**.
Three tiny S3 classes tag angular units and carry unit-aware behavior:

- `circumplex_degree` / `circumplex_radian` — a numeric vector tagged with
  its unit. Conversion via the `as_degree()` / `as_radian()` S3 generics.
- `circumplex_contrast_radian` — bootstrap replicates of a displacement
  *difference*, whose `quantile` method returns values on a continuous
  branch (may be negative or exceed 360°) so that a CI straddling 0° stays
  contiguous.

These classes are `structure(numeric, class = c("circumplex_<unit>",
"numeric"))` — numeric subclasses, nothing more. Their real weight is in two
**custom `quantile` methods** that implement the package's circular-CI
statistics (see Materials). The classes are woven through the estimation
path: `as_degree`/`as_radian`/`new_degree`/`new_radian` are used in 9 R
files (`convenience_functions.R`, `cpm_fit.R`, `ssm_analysis.R`,
`ssm_bootstrap.R`, `ssm_ci_accuracy.R`, `ssm_oop.R`, `ssm_sem.R`,
`ssm_sem_syntax.R`, `utils.R`).

**Why this needs independent review before planning.** Two tripwires fire:

1. `ip-touching` — the classes encode the package's statistical
   invariants (LM = 360°, displacement in [0, 360), contrast in (-180°,
   180°], the 0°/360° boundary behavior, the circular-quantile CI method).
   These are hard constraints; DESIGN.md "Statistical conventions" is the de
   facto inviolable set (formal IP/GP numbering is deferred to a future
   `/design-interview`). Any migration must reproduce every numeric result
   bit-for-bit or with a stated, oracle-validated tolerance.
2. `irreversible-api` — the generics `as_degree()` / `as_radian()`, the
   `print.circumplex_degree/radian` methods, and
   `quantile.circumplex_radian` / `quantile.circumplex_contrast_radian` are
   **exported** (S3method entries in NAMESPACE). The package ships on CRAN
   and is installed from GitHub HEAD (`pak::pak()`); a class-identity or
   method-dispatch change is an exported-behavior change with a deprecation
   obligation.

The motivating impulse is modernization: vctrs (prototype/coercion-based
vector classes) and S7 (the successor to S3/S4) are the current idiomatic
choices for new R class code. The open question is whether that benefit is
worth the dependency and migration cost *for these three classes*.

## Materials

Read these (repository root = the working directory):

- `R/ssm_oop.R` — the class constructors and methods. Lines 1–100 hold
  `new_s3_num`, `new_degree`/`as_degree` + methods, `new_radian`/`as_radian`
  + methods, and `print.circumplex_degree/radian`. (The rest of the file is
  the `circumplex_ssm` list class and its print/summary — out of scope
  except as a consumer.)
- `R/ssm_bootstrap.R` lines ~104–192 — the **statistical crux**:
  - lines 104–116: where displacement columns are tagged
    `circumplex_radian`, and the contrast column
    `circumplex_contrast_radian`.
  - lines 131–146: the contrast-CI branch-alignment fix (shift both CI
    endpoints by the same multiple of 2π so the estimate lies numerically
    inside the interval it is geometrically inside).
  - `quantile.circumplex_radian` (lines 172–181): circular-mean centering →
    unwrap to (-π, π] → linear quantile → re-wrap to [0, 2π), **with an
    explicit pole-snap** at line 179 (`out[abs(out - 2π) < 2·eps] <- 0`).
  - `quantile.circumplex_contrast_radian` (lines 185–192): same centering
    but returns on the **continuous branch** (no re-wrap, negatives
    allowed).
- `cairn/DESIGN.md` — "Class system" (lines 49–61) and "Statistical
  conventions and their rationale" (the table at lines 63–75). The
  displacement-boundary row (line 68) and the contrast-branch row (line 69)
  are the invariants most at risk.
- `cairn/DECISIONS.md` — D-003 (0°/360° pole reported as exactly 360.0, not
  canonicalized) and the preamble noting IP/GP formalization is deferred.
- `NAMESPACE` — confirm the exported S3 surface listed above.
- `DESCRIPTION` — confirm current Imports (no vctrs/S7).

To exercise current behavior: `Rscript -e 'devtools::load_all();
devtools::test()'`. The angle-class and boundary behaviors are tested in
`tests/testthat/` (grep for `circumplex_radian`, `as_degree`, boundary/pole
tests).

## Questions

1. **Is the migration justified at all?** State the concrete engineering
   problem vctrs or S7 solves that the current base-R numeric-subclass S3
   design does not, *for these three specific classes*. Weigh it against the
   package's explicit "base R + minimal deps" doctrine. If the honest answer
   is "no material benefit," say so — "keep S3" is an acceptable
   recommendation.

2. **If justified, vctrs or S7 — which, and why?** These are
   unit-tagged numeric vectors with (a) unit conversion and (b) two custom
   circular `quantile` methods. Assess fit: vctrs' prototype/coercion and
   `vec_*` machinery vs. S7's formal properties/validators/method dispatch.
   Which better preserves numeric-vector semantics (arithmetic,
   `NA` handling, `c()`, subsetting) that the estimation path relies on?

3. **Dependency cost.** Adding vctrs or S7 to Imports is a
   dependency decision (house rule: never unilateral; needs a D-entry).
   Assess the transitive weight, CRAN-stability, and R-version-floor
   implications of each, and whether the benefit from Q1 clears that bar.

4. **Exported-API preservation and deprecation path.** The generics
   `as_degree()`/`as_radian()`, the two `print` methods, and the two
   `quantile` methods are exported. How can the migration preserve the
   exported contract for downstream users and reverse dependencies — does
   the class identity (`inherits(x, "circumplex_radian")`) need to survive,
   and what is the deprecation cycle? Identify any change that would break a
   caller that does `as_degree(x)` or dispatches on these classes today.

5. **Statistical-invariant preservation (the hard stop).** Map the two
   custom `quantile` methods and the contrast branch-alignment onto the
   chosen system without altering any numeric output. Specifically address:
   the pole-snap at `ssm_bootstrap.R:179`; the contrast continuous-branch
   return; and the D-003 boundary (a pole profile reported as exactly
   360.0). What oracle(s) would certify byte-for-byte equivalence (per the
   repo's ≥2-independent-oracle-types validation doctrine)? Flag any place
   where vctrs/S7 coercion or formatting could silently perturb a value.

6. **Sizing and shape.** Is this one reviewable milestone (≈1–3 sessions,
   one PR) or must it split? If split, name the seams and the `Depends on:`
   ordering. Note the candidate's own guidance was "fold into the milestone
   that next touches the code" — assess whether a standalone migration is
   even the right vehicle versus an incremental fold-in.

7. **Bottom line.** Recommend one of: **proceed** (with system + scope +
   deprecation plan), **defer** (fold into a future class-touching
   milestone; state the trigger), or **drop** (keep S3; record why). Give
   the reasoning that a `/milestone-plan` run could act on directly.

## Constraints

Fixed — do not relitigate; flag disagreement explicitly rather than
silently working around:

- **Statistical invariants are inviolable.** Every convention in DESIGN.md
  "Statistical conventions" (LM = 360°; displacement [0, 360) for profiles;
  contrast in (-180°, 180°] reported on the estimate's branch; the
  circular-quantile CI method; the D-003 0°/360° boundary behavior) must be
  reproduced exactly. No numeric output may change without an explicit,
  oracle-validated, user-approved decision.
- **D-003 stands** (pole reported as exactly 360.0, not canonicalized)
  unless your report makes an affirmative case to supersede it — in which
  case say so explicitly; do not quietly alter it.
- **Minimal-deps doctrine.** base R + Rcpp/RcppArmadillo/rlang/ggplot2.
  Adding a dependency is a real cost that must be justified, not assumed.
- **No tidyverse in package code.** (vctrs is not tidyverse-exclusive, but
  the design bar is the same.)
- The estimator core in `src/` (RcppArmadillo) is **out of scope** — this is
  an R-layer class question only.

## Output format

In `RR01-angle-class-vctrs-s7.md`: answer each question by number with your
reasoning and evidence (cite `file:line`). List any additional findings
separately under "Beyond the brief." End with concrete recommendations, each
marked **apply / consider / reject-with-reason**, and a single bottom-line
verdict (proceed / defer / drop) for Q7.
