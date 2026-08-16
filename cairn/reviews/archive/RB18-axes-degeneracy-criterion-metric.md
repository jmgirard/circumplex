# RB18: Which matrix should the fitted-matrix degeneracy criterion price? (M89)

- **Date:** 2026-08-15
- **Output required:** write findings to `cairn/reviews/RR18-axes-degeneracy-criterion-metric.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is a CRAN R package for circumplex data analysis. The function
under review, `axes_reliability()`, fits a structural model (via `lavaan`) to a
set of items arranged at known angles around a circle and reports axis
reliability plus model fit. Statistical correctness outranks every other
concern in this repo.

Two internal helpers consume the **model-implied (fitted) covariance matrix**
`lavaan::fitted(fit)$cov`, hereafter Σ̂:

- `axes_corrected_se()` — reports corrected component standard errors.
- `axes_scaling_factor()` — reports a Satorra–Bentler-style scaling factor `c`
  used to scale four fit statistics (`chisq`, `pvalue`, `rmsea`, `cfi`).

Both build an information matrix Δ′VΔ from an inverse of a matrix derived from
Σ̂ and then invert that too, so both are sensitive to how badly conditioned
their input is.

**The problem milestone M89 set out to fix.** Neither helper had a stated rule
for when Σ̂ is too degenerate to price. Each refused only when one of its own
internal `solve()` calls happened to fail, and they failed at different points.
A user could therefore receive `NA` corrected standard errors printed beside
*silently scaled* fit statistics computed from the same Σ̂ — the one failure
mode the package's own contract calls undetectable by the user, because a
scaled statistic that is really unscaled looks entirely normal.

**What M89 shipped.** One stated criterion, `axes_sigma_degenerate()`, applied
at both helpers ahead of any pricing:

> refuse as `"ill_conditioned"` when λmin(Σ̂) ≤ λmax(Σ̂)·sqrt(p·ε),
> where p = `nrow(Σ̂)` and ε = `.Machine$double.eps`

(≈ κ ≥ 1.4e7 at p = 24; ≈ κ ≥ 3.8e7 at p = 3.) One inequality also covers
indefinite (λmin < 0) and exactly singular (λmin = 0) matrices. The recorded
rationale is that both consumers build Δ′VΔ from Σ̂⁻¹ twice, so entries carry
relative error growing like p·κ(Σ̂)²·ε, and the floor is where that bound
reaches 1.

**Why this needs independent review.** The criterion prices the **raw** Σ̂. Two
independently measured counterexamples now show that choice failing in
*opposite* directions, which is why the question is being escalated rather than
patched. Both are reproduced in Materials below.

- **Counterexample A — false refusal.** `axes_scaling_factor()` normalizes with
  `cov2cor()` before it does anything else, so every quantity it computes is a
  function of `cov2cor(Σ̂)` alone. Its error is therefore governed by
  κ(`cov2cor(Σ̂)`), not κ(Σ̂). A pure diagonal rescaling Σ̂ = D S D leaves
  `cov2cor(Σ̂)` exactly unchanged — an operation this surface's estimand is
  exactly invariant under — yet raises κ(raw) arbitrarily. Measured: the
  criterion now refuses a matrix on which the pre-M89 code returned the correct
  factor.
- **Counterexample B — missed refusal, and the original failure mode
  returning.** Conversely, a Σ̂ that is well conditioned raw but degenerate
  *after* `cov2cor()` passes the criterion and then fails downstream — on one
  surface only. Measured at p = 3: finite corrected SEs with
  `se_correction_failed = NULL` beside `fit_scaling_failed = "indefinite"`
  NA-ing all four scaled statistics, from the same Σ̂. That is M89's own Goal
  failure mode with the roles swapped. *[Corrected at the RR18 ingest,
  2026-08-16 (RR18 F1, D-044): the exemplar behind this framing has an exactly
  unit diagonal, so `cov2cor(S) == S` and its κ = 6.654e6 is identical in both
  metrics — it is not a raw-vs-correlation counterexample, but evidence the
  cutoff was a thousand times too loose.]*

**The tension that makes this non-obvious.** "Just price `cov2cor(Σ̂)`" is not
an available answer as stated, because the regime M89 was built to close is
invisible in the correlation metric. Inflating one diagonal entry of the octant
probe by 10^k leaves `cov2cor(Σ̂)` at condition number 10.45 for **every** k
from 0 to 16 (measured), while κ(raw) grows without bound and the two surfaces
diverge from k = 7. A pure correlation-metric criterion is blind to exactly the
case that motivated the milestone.

**A further complication.** The package does not treat "which matrix" uniformly
even inside one helper. `axes_corrected_se()` prices its `naive` vector at the
raw Σ̂ and its `corrected` and `fiml_ratio` vectors at `cov2cor(Σ̂)`
(`R/axes_corrected_se.R:262` and `:264`). So a single criterion currently gates
three quantities computed at two different matrices.

**Reachability, and why this is not an emergency.** Neither counterexample is
reachable through the exported `axes_reliability()`. Counterexample A needs a
non-unit-diagonal Σ̂, but every code path fits lavaan a *correlation* matrix, so
the fitted matrix is near-unit-diagonal (κ measured 3.6–21.4 on the probe
fits). Counterexample B needs p = 3, and `axes_reliability()` refuses fewer than
four scales (`R/axes_reliability.R:1152`). Both live at the helpers' contract
boundary, which is where both helpers are directly tested. Treat this as a
design question about the criterion, not an incident.

## Materials

Read these files. Line numbers are at commit `2695f54f` on branch
`m89-fitted-matrix-degeneracy`.

- `R/axes_corrected_se.R` — `axes_sigma_degenerate()` at `:315` with its full
  recorded rationale at `:274-314`; the criterion's call site at `:259`, after
  the nonpositive-diagonal guard at `:244` and the `+Inf` guard at `:253`; the
  two pricing calls at `:262` (raw) and `:264` (`cov2cor`); and
  `axes_se_pricing()` at `:148-202`, which owns the `"singular"`,
  `"unidentified"` and `"indefinite"` returns.
- `R/axes_scaled_fit.R` — `axes_scaling_factor()` from `:75`; its diagonal
  guards at `:139-140`; the criterion's call site at `:149`; the `cov2cor()`
  normalization at `:152`; the closed-form trace and projection term; the
  scaling factor at `:217` (`cval <- (tr_vg - sum(acov * bmat)) / df`) and the
  `"indefinite"` refusal immediately below it.
- `R/axes_reliability.R` — the two consumer call sites at `:1727` and `:1836`;
  the ≥4-scale refusal at `:1152`; the reason-literal enumerations in the
  `details` list around `:1880` and `:1910`.
- `cairn/milestones/M89-fitted-matrix-degeneracy-criterion.md` — the milestone's
  Goal, Scope, acceptance criteria, its `## Decisions` entry recording the
  criterion and what was rejected, and its `## Review` section carrying both
  review rounds' findings with confidence scores.

To run anything: `Rscript -e 'devtools::load_all(); ...'` from the repo root.
Internal functions are reachable after `load_all()`. The octant probe fixture
`probe_octant()` is defined at `tests/testthat/test-axes-scaled-fit.R:183` and
can be sourced out of that file.

**Reproducing counterexample A** (false refusal; `pp <- probe_octant()`,
`S <- pp$sigma`, p = 24):

```r
D  <- diag(c(1e4, rep(1, p - 1)))
Sh <- D %*% S %*% D; dimnames(Sh) <- dimnames(S)
all.equal(cov2cor(Sh), S)          # TRUE — the estimand's input is unchanged
# kappa(raw) = 2.13e8, kappa(cov2cor) = 10.4
# pre-M89: axes_scaling_factor(...)$scale = 0.9563346, reason NULL
# HEAD:    scale = NA, reason "ill_conditioned"
```

**Reproducing counterexample B** (missed refusal + surface disagreement). A
captured exemplar is serialized at `cairn/reviews/rb18-counterexample-b.rds`
(a list with `S`, the item angles `ia`, the two observed reasons, and κ =
6.65e6). It was found by random search over near-collinear 3×3 correlation
matrices at randomly drawn angles, keeping only draws the criterion accepts,
and is committed because it must be read from the binary file: `dput()`
round-tripping loses the last bits and the case flips to `NULL`. With p = 3,
scales `c("A","B","C")`, `fit_zeta1 = FALSE`, `df = 1`:

```r
axes_sigma_degenerate(S)      # NULL — the criterion ACCEPTS it
# axes_corrected_se(...)$reason   -> NULL, all corrected SEs finite
# axes_scaling_factor(...)$reason -> "indefinite", scale NA
```

**A third, deterministic case in the same family** — a saturated model, where
`R/axes_scaled_fit.R:217` divides by `df` with no zero guard:

```r
S <- matrix(c(1,.5,.3, .5,1,.4, .3,.4,1), 3, 3)   # angles c(0,90,180)
# scales c("A","A","B"), fit_zeta1 = TRUE -> q = 6 = p*, df = 0
# axes_sigma_degenerate(S) is NULL; cval = Inf; reason "indefinite"
```

**The regime a correlation-metric criterion is blind to** (measured): with
`S[4,4] <- S[4,4] * 10^k`, `cov2cor(Σ̂)` has condition number 10.45 for every
k = 0..16, while the two surfaces diverge from k = 7.

## Questions

1. **Which matrix should the degeneracy criterion price** — the raw Σ̂ as
   shipped, `cov2cor(Σ̂)`, both under a conjunction, or some other derived
   quantity? Justify in terms of the numerical error that actually propagates
   into each reported statistic, not in terms of which is easier to compute.

2. If the answer to (1) is `cov2cor(Σ̂)` or a conjunction, **what catches the
   diagonal-inflation regime** that motivated M89, given that `cov2cor(Σ̂)`
   stays at condition 10.45 across all 16 decades of inflation? If nothing
   should catch it — i.e. if the inflated matrix is in fact priceable and the
   pre-M89 divergence was benign — say so explicitly and give the evidence,
   since that would mean M89's premise was wrong.

3. **Should the two surfaces share one criterion at all?** They price different
   matrices, and `axes_corrected_se()` internally prices its `naive` vector at
   raw Σ̂ while its `corrected` and `fiml_ratio` vectors are at `cov2cor(Σ̂)`
   (`:262`, `:264`). Is a single shared gate the right design, or should each
   quantity be gated at the matrix it is actually computed from — and if the
   latter, how is the user-visible contract "these NA together" preserved?

4. **Is the cutoff's form and dimension factor right?** The shipped floor is
   λmin/λmax ≤ sqrt(p·ε) with p = `nrow(Σ̂)`. The sandwich the rationale invokes
   runs over a p\* = p(p+1)/2-dimensional moment vector with q free parameters;
   neither p\* nor q appears. Note that both counterexamples above sit at p = 3,
   where the p factor makes the cutoff *loosest*. Is `p = nrow` the correct
   dimension, and is a κ-based floor the right instrument at all versus, say, a
   direct residual or backward-error check on the quantity being computed?

5. **The `df = 0` divide.** Should `R/axes_scaled_fit.R:217` carry an explicit
   `df == 0` guard with its own reason literal, or is a saturated model
   properly refused earlier (and if so, where)? Note that the surrounding code
   already validates `df` against its own derivative count.

6. **Is there an oracle** for any part of this — a published treatment of
   conditioning for Satorra–Bentler-type scaling factors or for
   correlation-structure-corrected standard errors, or an independent
   implementation (lavaan's own internals, or another package) whose
   degeneracy handling could serve as a check? The repo's doctrine requires
   numeric results be validated against at least two independent oracle types
   where one exists; M89 shipped its cutoff on an analytic error-bound argument
   plus measurement against its own probe grid, with no external oracle.

7. **The reason vocabulary.** M89 folded indefinite, exactly singular and
   merely ill-conditioned matrices into one user-visible literal
   `"ill_conditioned"`. Is that conflation acceptable for a user trying to
   diagnose their own model, or should an indefinite model-implied matrix —
   which is a statement about the *model*, not about arithmetic — keep a
   distinct literal?

## Constraints

Do not relitigate these; flag disagreement explicitly rather than working
around it silently.

- **D-036** — `axes_reliability()`'s global test statistic is *scaled*, not
  caveated. The scaling arithmetic itself is out of scope; this review is about
  when to refuse to compute it, not how it is computed.
- **D-037** — the FIML metric ratio is evaluated at `cov2cor(Σ̂)`, with `naive`
  deliberately left at the raw Σ̂ ("the only independent tie of the derivative
  set to lavaan's own implementation, fenced at 1e-7"). This is a *standing
  decision that a related "which matrix" question was already settled in favour
  of the correlation metric for the corrected quantities.* It is directly
  relevant precedent and you should engage with it — but note it deliberately
  keeps one raw-priced quantity, so it does not settle question (3) by itself.
  If your answer requires superseding any part of D-037, say so explicitly and
  state what replaces it.
- The four statistics M89 NAs together on refusal (`chisq`, `pvalue`, `rmsea`,
  `cfi`) and the "both surfaces NA together" user-visible contract are fixed
  unless you argue explicitly for changing them.
- No new package dependencies. Base R plus the existing Imports/Suggests
  (`rlang`, `ggplot2`, `boot`, `Rcpp`/`RcppArmadillo`; `lavaan`, `OpenMx` in
  Suggests). A dependency change requires its own user decision.
- The exported API's own gates stay: `axes_reliability()` refuses fewer than
  four equally spaced scales, and `axes_design()` drops a component collinear
  with another.
- Angles are degrees in [0, 360) in the user API with LM = 360.

## Output format

In `RR18-axes-degeneracy-criterion-metric.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately under
"Beyond the brief"; end with concrete recommendations, each marked apply /
consider / reject-with-reason.

Where findings bind implementation, also emit a `## Binding criteria` section:
numbered `BC1…`, each a measurable assertion checkable against evidence, with
any numeric projection stating its tolerance. These are ingested VERBATIM into
M89's acceptance criteria and mechanically diffed against this file; departures
are legal only through that milestone's shown "Deviations from RR18" table.
Keep the set jointly satisfiable — criteria that are individually reasonable
can still contradict each other — and make each one's domain enumerable by a
procedure it names, rather than quantifying over a domain nothing enumerates.
