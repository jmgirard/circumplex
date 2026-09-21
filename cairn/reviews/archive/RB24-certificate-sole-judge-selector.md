# RB24: The certificate as the sole conditioning judge: licensing below eps, the selector's home, and the exact duplicate-pair check (M147)

- **Date:** 2026-09-21
- **Output required:** write findings to `cairn/reviews/RR24-certificate-sole-judge-selector.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for circumplex data analysis. `axes_reliability()`
fits a circumplex measurement model with lavaan and reports corrected standard
errors for its variance components and Satorra-Bentler-style scaled fit
statistics. Both reported quantities are priced at the fitted, standardized
matrix `cov2cor(Sigma-hat)` by one pipeline, `axes_pricing_core()`
(`R/axes_corrected_se.R`), which inverts the matrix and then the ML
information matrix `Delta'V Delta` (q x q, q = number of free parameters,
about 27 at p = 24 items).

Whether a fit is refused on conditioning is decided, since M111, by
`axes_degeneracy_refusal()` (same file): an a-priori eigenvalue floor on the
standardized matrix (`axes_sigma_degenerate()`) selects which fits are
checked, and for a selected fit the per-fit accuracy certificate
(`axes_accuracy_certificate()`, `R/axes_certificate.R`) replays the same
pipeline in compensated double-double arithmetic and estimates the relative
error the reported doubles carry (fields `se`, `cval`, `fiml_ratio`, worst-of
read by the predicate). Past the accuracy target delta_star = 1e-4 the fit
refuses `"uncertified"`, with the estimate in the warning; inside it, the
fit computes. The certificate returns a sentinel of exactly 1 on any route
failure ("no digits certified"), which refuses. The certificate is validated
against an exact-rational oracle (`devel/degeneracy-oracle/exact_oracle.py`,
Python `fractions`, no floating point), and the package measured its estimate at
10x the true error (its safety factor) at every committed geometry.

One older gate stayed beside this: `axes_pricing_core()` inverts the
information matrix with R's default `solve()` tolerance, which errors when
LAPACK's reciprocal condition estimate `rcond(info)` falls below
`.Machine$double.eps`, and the pipeline then returns the literal
`"unidentified"`. RR22 (2026-09-05) measured that estimate straddling eps
across real platforms at the committed counterexample B (`rcond(info)`
2.6e-16 on macOS/arm64 with reference BLAS, 2.05e-16 on linux-arm64 with
OpenBLAS; 97 of 300 one-ulp neighbours refuse), which cost release 2.0.1 a
CRAN pre-test rejection. The test suite now admits both outcomes at B through
a committed `rcond` band (D-055). RR22's recommendation 9 proposed `tol = 0`
on that inversion so the certificate becomes the sole judge, for a later
gate. Milestone M147 is that gate, and its plan decision D-061 states:

> The shipped pricing never refuses on conditioning. On the certified path
> the inversion runs with `tol = 0`, and a condition estimate below eps
> selects the fit for the certificate, as the floor does, whether or not the
> floor fired. The certificate then decides. `"unidentified"` keeps two
> grounds only: a pair of bit-identical derivative matrices, an exact check
> on the design with no tolerance, and an inversion that is non-finite or
> that LAPACK reports exactly singular. The raw lavaan-tie arm keeps the
> default tolerance.

D-061 rejected pure `tol = 0` with no selector because a design the floor
admits can carry a structurally singular information matrix that LU under
`tol = 0` prices on some platforms, so the fit reports numbers with no
certificate consulted. The measured instance: one item per scale with the
scale-specificity component zeta1 fitted, where the zeta1 derivative matrix
equals the identity, which is the sum of the p item-error derivative
matrices; `axes_reliability()` never fits zeta1 for that design
(`axes_fits_zeta1()`), so it is reachable only at the helper's contract
boundary.

M147's plan (`cairn/milestones/M147-certificate-sole-conditioning-judge.md`)
scheduled a sweep before the core change and this review after it. The sweep
has run at HEAD (before any core change). Its results are below and in
`devel/degeneracy-oracle/tol0-sweep-summary.md`.

**What the sweep measured (271 matrices, four families; script
`devel/degeneracy-oracle/tol0_sweep.R`, results
`devel/degeneracy-oracle/tol0-sweep-results.rds`, commit `7856f70d`,
macOS/arm64, reference BLAS).** Per matrix: the floor's answer,
`rcond(sigma)`, `rcond(info)`, the inversion outcome under the default
tolerance and under `tol = 0` (a replica of the shipped core with a `tol`
formal, built from the shipped function's own text), bit-identity of the
inverses across the two tolerances, the shipped surface's refusal literal,
the same surface's literal with the core rebound to `tol = 0` ("the tol0
world"), the certificate in the tol0 world, and, in the REGION (default
refused, `tol = 0` inverted), the exact oracle's true relative error of the
tol0 doubles for all three fields.

| finding | value |
|---|---|
| region matrices (default refused, tol = 0 inverted) | 85 (34 M106-family, 51 random) |
| both tolerances refused (exact zero pivot under tol = 0) | 0 |
| region matrices whose tol0 doubles refused after the inversion (`"indefinite"`, nonpositive quadratic form; p = 24, kappa 2e12 and 7e12) | 2, both refuse `"uncertified"` in the tol0 world |
| certificate / true error at every graded region matrix (83) | 9.9996 to 10.0005 |
| certificate sentinels in the region | 0 |
| region matrices that COMPUTE in the tol0 world | 37; largest true error among them 9.9e-6 (target 1e-4) |
| region matrices refused `"uncertified"` in the tol0 world | 46 |
| region `rcond(info)` range | 1.85e-16 down to 1.8e-25 |
| floor-admitted matrices with `rcond(info)` below eps, in any family | 0 |
| floor-admitted matrices with `rcond(sigma)` below eps | 0 |
| bit-identical `si`, `sim`, `acov` across tolerances at every default-inverted matrix (186) | yes |
| smallest `rcond(info)` at a floor-admitted design with four or more scales | 6.4e-9 |
| smallest `rcond(info)` at a floor-admitted design overall | 9.4e-13, at the p = 3 Q4 perturbation `t = 0.999975` (kappa 1.09e5, unreachable through the exported API, which needs four scales) |

Outside the families, one scratch measurement on the same machine: the
one-item-per-scale design with zeta1 forced fitted (p = 8, item error 0.3)
passes the floor (`rcond(sigma)` 0.071), has `rcond(info)` 1.7e-17, inverts
under `tol = 0` (no zero pivot here), and in the tol0 world with NO selector
computes with reason NULL while its certificate reads se 4.69, cval 2.87,
fiml_ratio 0.21. With D-061's selector it refuses `"uncertified"`. On the
plan's machine a different 8 x 8 toy with two identical rows and columns also
inverted under `tol = 0`; on this run the same construction hit an exact zero
pivot (`U[8,8] = 0`), which is the platform dependence D-061 describes.

## Materials

- `R/axes_corrected_se.R` lines 140-183: `axes_pricing_core()` as shipped
  (default tolerance). Lines 640-830: the floor, `axes_sigma_degenerate()`,
  `axes_degeneracy_refusal()` and its header comment (what M111 made of the
  floor), `axes_shared_refusal()` (the M117 once-per-fit seam), and
  `axes_check_shared_refusal()`. Lines 300-400: how `axes_corrected_se()`
  consults the refusal and then prices the cov2cor arm and the raw arm.
- `R/axes_scaled_fit.R` lines 55-60 (`axes_u_pricing()` reads the core) and
  lines 240-262 (the sibling surface's consultation of the refusal).
- `R/axes_certificate.R` lines 60-100 (what the certificate claims and its
  sentinel) and lines 330-360 and 440-445 (the double-double replay of the
  pipeline, including its own `dd_solve()` of the information matrix, and
  the shipped values it compares against).
- `R/axes_reliability.R` lines 1812-1830: the one call to
  `axes_shared_refusal()`.
- `devel/degeneracy-oracle/tol0_sweep.R`: the sweep, with its header's
  pre-registered acceptance (a)-(d). `TOL0_REPORT_ONLY=1 Rscript
  devel/degeneracy-oracle/tol0_sweep.R` re-renders the summary from the
  committed results in seconds; the full run takes about an hour.
  `devel/degeneracy-oracle/tol0-sweep-summary.md` is the rendered summary,
  with the full region table.
- `cairn/DECISIONS.md`: entries D-051 (the certificate), D-055 (the two
  admitted routes at counterexample B; its Reopens clause), D-061 (M147's
  plan decision). Read by heading; each is self-contained.
- `cairn/reviews/archive/RR22-certificate-platform-refusal.md` sections
  "The measurement that reframes the brief" and 2 (C′), and recommendation 9.
- `tests/testthat/test-axes-corrected-se.R` lines 1245-1300: the M89 T10
  one-scale construction, the one committed reach of `"unidentified"`.

## Questions

1. **Licensing below eps.** The certificate compares the shipped doubles
   against a double-double replay of the same pipeline, which inverts the
   same information matrix by its own elimination (`dd_solve()`). Where
   `rcond(info)` sits below eps, both routes work on a matrix the double LU
   barely resolves. Is there a mechanism by which the two routes agree to
   within the target while both are wrong, so the certificate under-reports
   there? The sweep found none at 83 graded matrices (ratio 10.0 throughout,
   true error at most 9.9e-6 where it computes). State whether that
   measurement, plus the certificate's construction, licenses the decision
   for the domain `axes_reliability()` reaches, and name any matrix class the
   four families do not cover that must be measured before shipping.

2. **The selector's home and threshold.** D-061 puts the selector in
   `axes_degeneracy_refusal()`: the core is priced there once, and a fit is
   sent to the certificate when the floor fired OR `rcond(info) < eps`; the
   core's result travels to both surfaces on the refusal object (M117 seam).
   (a) Is `rcond(info) < eps` the right selector threshold, given it is
   exactly the old gate's threshold (so the certificate is consulted at
   every matrix the old gate refused, and additionally at floor-admitted
   matrices such as the rank-deficient design), or does it belong higher
   (say `sqrt(eps)`), given the sweep shows every floor-admitted design with
   four or more scales at `rcond(info)` >= 6.4e-9? (b) Is
   `axes_degeneracy_refusal()` the right place, or does the selector belong inside
   `axes_pricing_core()`, reading `rcond(info)` there and returning a third literal? (c)
   Alternatively, is the certificate to be consulted on EVERY fit
   (removing both selectors)? It costs 0.18 s at a clean p = 24 fit against
   0.002 s for the pricing; every ordinary fit then pays it.

3. **The exact duplicate-pair check.** D-061 keeps `"unidentified"` for a
   pair of bit-identical matrices in `d$mats`, an exact check with no
   tolerance, so the M89 T10 one-scale construction (zeta1 identical to the
   all-ones xi2 matrix) keeps a platform-stable literal. Structural
   singularity can also arise from linear dependence that is not pairwise
   (the rank-deficient design above: zeta1 equals the sum of the p error
   matrices), which the pair check cannot see and the selector plus
   certificate handles. Is an exact pairwise check the right structural
   guard? Options: keep it as stated; replace it with a rank test on the
   stacked `vec(d$mats)` matrix (which reintroduces a tolerance and a
   platform-dependent QR); drop it and let every structural case route
   through the selector, so `"unidentified"` means only "LAPACK reported an
   exact zero pivot". Weigh the platform stability of the literal against
   the value of a design-level check that cannot generalize.

4. **Removal (this mechanism's second escalation, after RB22).** Put the
   removal of the conditioning gate as a whole among the options you weigh:
   (i) D-061 as stated (tol = 0 plus selector plus duplicate check); (ii)
   pure tol = 0 with no selector and no duplicate check, RR22's original
   recommendation 9, which D-061 rejected on the rank-deficient design; (iii)
   keep the default tolerance and the D-055 two-route test admission as the
   shipped state, with no change. State which you recommend and why.

5. **The sweep's acceptance (b).** The plan's criterion AC2 (b) reads: every
   floor-admitted matrix whose design fits zeta1 only with two or more items
   on every scale was inverted under both tolerances with `rcond(info)` at
   least four decades above eps. It fails on the committed run at exactly one
   matrix, the p = 3 Q4 perturbation (3.6 decades), a shape the exported API
   cannot reach. Is narrowing (b) to designs with four or more scales the
   right repair, or is the four-decade margin itself the wrong quantity to
   pre-register?

6. **The raw arm.** The uncorrected lavaan-tie arm keeps the default
   tolerance because the certificate never prices raw Sigma-hat (D-037) and
   the arm is never user-reported (its refusal lands in `naive_reason`, a
   silent details field). Is there a reason this asymmetry must not stand?

## Constraints

- D-061 is the milestone's plan decision; this review can recommend its
  amendment, and any departure it recommends is recorded as a superseding
  D-entry, never applied silently. D-055's file rule for the certificate
  suite stands (every assertion a property of a committed matrix, a
  bracketed own-measurement, or an exhaustive disposition; no frozen
  platform measurement). D-051's certificate mechanism, D-048/D-049's
  accuracy target delta_star = 1e-4 and the floor constant tau are not open
  here; an under-report finding reopens D-051 rather than this brief.
- An explicit `solve()` tolerance between 0 and eps is rejected on D-055's
  grounds (no platform-stable value exists inside the measured band); do
  not propose one.
- The raw arm's tolerance and D-037 are fixed unless question 6 finds a
  defect.
- Statistical correctness outranks every other concern in this package.

## Output format

In `RR24-certificate-sole-judge-selector.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`. Where requested: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its
tolerance. These are ingested VERBATIM into the constrained milestone's
acceptance criteria and mechanically diffed against this file; departures
are legal only through that milestone's shown "Deviations from RR24" table.
