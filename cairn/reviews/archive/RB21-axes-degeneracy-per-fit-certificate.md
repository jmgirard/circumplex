# RB21: Mechanism for an a-posteriori per-fit accuracy certificate (M108)

- **Date:** 2026-08-24
- **Output required:** write findings to `cairn/reviews/RR21-axes-degeneracy-per-fit-certificate.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for circumplex data analysis, on CRAN. The
function under discussion is `axes_reliability()`, which fits a structured
model to an item **correlation** matrix and reports, among other things,
component standard errors corrected for the correlation-vs-covariance metric
mismatch, and a scaled-fit correction factor `cval`.

Both quantities are computed from the **inverse** of the fitted model-implied
matrix `Sigma-hat`, and the information matrix `Delta'V Delta` is built from
that inverse twice. The resulting double-precision relative error grows like
`p * kappa(Sigma-hat)^2 * eps`. Because the package will not report a number
it cannot certify (IP3), it currently **refuses** — returns `NA` with an
`"ill_conditioned"` warning — whenever the fitted matrix is conditioned worse
than a fixed threshold.

That threshold is derived a priori:

```r
axes_degeneracy_delta_star      <- 1e-4   # largest tolerated reported rel. error
axes_degeneracy_calibration_ceiling <- 10 # how far the bound may undershoot
axes_degeneracy_tau <- delta_star / ceiling
floor_ <- sqrt(p * .Machine$double.eps / axes_degeneracy_tau)
# refuse when ev[p] <= ev[1] * floor_
```

**The problem this milestone exists to fix.** The a-priori bound
`p * kappa^2 * eps` is the only error estimate the package has, and in every
geometry users actually occupy it overstates the true error by **five to eight
decades**. Measured on 2026-08-22 across five reachable-geometry constructions,
attainment (true relative error divided by the bound) ran **6.8e-8 to 3.8e-7**.
The consequence is a refusal region full of fits whose numbers are in fact
accurate: a converged fit at `p = 8`, `kappa = 1.0e5` is refused while an
exact-rational oracle measures its corrected-SE relative error at **3.004e-12**
— eight decades inside the 1e-4 target.

Three prior escalations (RR18, RR19, RR20) each weighed removing the refusal
limb and each rejected it on the same ground: past the floor, the package has
**no shipped means of certifying the number**, and a replacement caution could
only carry the same overstated a-priori bound — it would cry "up to 3% error"
over numbers accurate to 1e-13. The recorded remedy in all three is an
**a-posteriori per-fit error certificate**: a number computed from the fit in
hand that estimates that fit's own error, so the refusal can shrink to what
the certificate cannot certify. D-050 recorded that the reopening trigger for
this remedy is now met in full, making it due rather than conditional.

**What this brief must settle.** RR20 §5 named two candidate mechanisms and
tiered the choice for independent review, because a certificate that is
plausible but wrong would license reporting numbers the package cannot in fact
certify — the exact failure IP3 exists to prevent:

- (a) **Two independent factorization routes compared** — price the same
  quantity two ways whose floating-point error is uncorrelated, and read the
  disagreement between them as the error estimate.
- (b) **On-demand exact-rational recomputation** of the corrected SEs and
  `cval`, comparing the exact answer to the double-precision one.

M108 builds only the certificate and validates it against the existing exact
oracle. **Nothing about what `axes_reliability()` returns or refuses changes in
this milestone** — the rewiring is a separate, dependent milestone (M111).

## Materials

Read these files. Line numbers are current as of commit `a5f2dda2` on branch
`m108-per-fit-certificate`.

1. **`R/axes_corrected_se.R`** — the whole file, but especially:
   - `:1-46` — header: why the correction exists, and that the implied
     covariance is **linear in the parameters**
     (`Sigma = xi1*C + xi2*J + zeta1*B + zeta2*K + diag(eps)`), so the ML
     estimator linearizes exactly. This linearity is the structural fact any
     candidate mechanism can exploit.
   - `:153-207` `axes_se_pricing(sigma, d, n)` — the single pricing of the
     whole sandwich at one matrix. Returns `list(naive =, corrected =)` or a
     bare string naming a failure (`"singular"`, `"unidentified"`,
     `"indefinite"`). It is **already called twice** by `axes_corrected_se()`,
     at the raw `Sigma-hat` and at `cov2cor(Sigma-hat)`, so a
     price-it-twice-and-compare primitive already exists in the code.
   - `:360-590` — the degeneracy criterion's full rationale block, including
     `WHICH MATRIX`, `WHY THIS CUTOFF`, `ONE CONSTANT, NOT SEVERAL`, and
     `WHY THE LIMB EXISTS AT ALL`, then the three constants.
   - `:622-635` `axes_sigma_degenerate(sigma)` — returns `NULL`,
     `"singular"`, `"indefinite"`, or `"ill_conditioned"`.
2. **`R/axes_scaled_fit.R`** — the scaling surface; `cval`'s arithmetic and
   its own degeneracy call at `:165`.
3. **`devel/degeneracy-oracle/exact_oracle.R`** (266 lines) and its companion
   **`devel/degeneracy-oracle/exact_oracle.py`** — the exact-rational oracle.
   Run it from the repo root with:

   ```
   Rscript devel/degeneracy-oracle/exact_oracle.R
   ```

   The Python side uses the standard library only (`fractions`); it is **not**
   a package dependency, because `devel/` is `.Rbuildignore`d. Note in
   particular:
   - `:19` `FIXTURE` still points at `cairn/reviews/rb18-counterexample-b.rds`;
     a packaged copy now exists at
     `tests/testthat/fixtures/rb18-counterexample-b.rds` and M108 repoints it.
   - `:172-266` — the **reachable-geometry family**: five model-implied
     constructions at dimensions the exported API actually reaches, each
     emitting both the SE relative error (`rel`) and the `cval` relative error
     (`cvr`) against the exact oracle. These five cases are what the
     certificate must be validated against.
   - `double_cval()` at `:98-125` — the double-precision `cval` computed
     before the shipped refusal discards it.
4. **`tests/testthat/helper-m106-degeneracy.R`** — the matrix builders the
   validation tests will use.
5. **`cairn/DESIGN.md`** — principles IP1, IP3, GP2, GP3 (lines ~88-131).
6. **`cairn/DECISIONS.md`** — entries D-044, D-048, D-049, D-050 (search for
   the `### D-0NN` headings).
7. **`cairn/reviews/archive/RR19-axes-degeneracy-accuracy-target.md`** and
   **`cairn/reviews/archive/RR20-axes-degeneracy-target-premise.md`** — the two
   prior reviews of this mechanism; RR20 §5 is where the two candidate
   mechanisms are named.
8. **`cairn/milestones/M108-per-fit-certificate.md`** — the milestone's goal,
   scope, and acceptance criteria. **The acceptance criteria are already
   written and audited; this review settles mechanism, not criteria** — but
   see question 3, which asks whether one of them is achievable.

## Questions

1. **Which mechanism should the certificate use?** Weigh (a) two independent
   factorization routes compared and (b) on-demand exact-rational
   recomputation, and propose a third if you judge one better — for instance a
   computable a-posteriori backward-error or residual bound, a condition
   estimate of the *specific functional* being reported rather than of
   `Sigma-hat`, or one step of iterative refinement whose correction size is
   the estimate. For each option state what it costs per fit, what it can and
   cannot detect, and how it fails. Constraint: option (b) must not add a
   package dependency (GP3), so say explicitly whether an exact-rational route
   is achievable in base R at acceptable cost, and if not, say so plainly
   rather than proposing it anyway.

2. **State the estimated quantity exactly.** The certificate must estimate the
   relative error carried by (i) the fit's corrected component SE vector and
   (ii) its `cval`. Define both precisely, including the norm or aggregation
   over the SE components (max over components? componentwise vector?), and
   whether the estimate is of the *committed* error or an *upper bound* on it.
   Note that the corrected SEs are `sqrt(quadratic_form / n)`, so their
   **relative** error is free of the typed sample size `n` by construction —
   confirm this, since an n-dependent target was explicitly refused by D-048
   and D-049 on the ground that it would make refusal a property of the
   yardstick rather than of the refused matrix.

3. **Is the milestone's validation window achievable by your recommended
   mechanism?** M108's AC2 requires, on each of the five reachable-geometry
   cases, that the estimate be **at least** the oracle's measured relative
   error and **at most 1e3 times** it, separately for the SE estimate and the
   `cval` estimate. AC3 additionally requires every estimate on those five
   cases to be below 1e-4, while at the committed counterexample
   (`rb18-counterexample-b.rds`, whose corrected SEs the oracle measures 3.4%
   wrong with every shipped guard green) the estimate must exceed 1e-4. Judge
   whether your recommended mechanism can meet both, and if not, state the
   window it *can* meet and why that window is still a real improvement over
   the a-priori bound it replaces.

4. **What second oracle type satisfies IP3's bar?** The package requires every
   shipped numeric result to be validated against **≥2 independent oracle
   types**, never two instances of one type. The five recognized types are
   *frozen*, *live*, *invariant*, *closed-form*, and *simulation-coverage*
   (defined in `cairn/DESIGN.md` and the validation doctrine). The
   exact-rational Python oracle is one type. Name a second, independent type
   for the certificate's own number and say what it would assert. Note the
   hazard: if the certificate's mechanism **is** "two internal routes agreeing",
   then an *invariant* oracle over those same two routes is not independent of
   the thing under test.

5. **Behavior across the whole admitted domain.** The certificate must return
   a finite, non-negative estimate for **every** matrix on which the
   `"singular"` and `"indefinite"` limbs of `axes_sigma_degenerate()` pass —
   which includes matrices far worse conditioned than anything currently
   computed, since M111 will use the certificate to decide whether to compute
   there. Say what your mechanism returns as conditioning degrades toward
   exact singularity, what guarantees finiteness and non-negativity, and
   whether the estimate degrades **conservatively** (erring toward reporting
   more error than there is) or can silently under-report. Under-reporting is
   the failure mode that matters: it would license reporting a wrong number.

6. **Removal, weighed a fourth time.** This mechanism — the
   `"ill_conditioned"` refusal limb — is on its fourth escalation (RB18, RB19,
   RB20, this brief), and the standing doctrine requires removal to be listed
   among the options each time. Prior grounds for keeping, all IP3-based, are
   quoted above and in D-048/D-049. Given that a working certificate would for
   the first time supply the certifying means whose absence was the sole
   ground for keeping, state whether the limb should be (i) kept and shrunk to
   what the certificate cannot certify, (ii) removed outright once the
   certificate ships, or (iii) removed now regardless of the certificate. Give
   your reasoning, not a restatement of the prior holdings.

7. **Cost.** The certificate runs once per `axes_reliability()` fit. The
   existing pricing is dense linear algebra at roughly `p x p` with `p` up to
   about 24 and `q` around 28, described in the code as "free". State an
   acceptable per-fit cost multiple for your recommended mechanism, and flag
   it if your recommendation is materially more expensive than that.

## Constraints

Flag disagreement with any of these explicitly rather than silently working
around it.

- **The constants do not move.** `axes_degeneracy_delta_star = 1e-4`, the
  calibration ceiling of 10, and their quotient `axes_degeneracy_tau` are
  fixed by D-048 and D-049 and are explicitly out of M108's scope. A proposal
  to move any of them is its own escalation, not this one.
- **D-044's metric choice is untouched:** `cov2cor(Sigma-hat)` for every
  user-reported quantity, raw `Sigma-hat` at the `naive` arm. RR19 declined to
  reopen it.
- **IP3** — every shipped numeric result is validated against ≥2 independent
  oracle types. The certificate is itself a shipped numeric result and carries
  this obligation.
- **IP1** — statistical correctness outranks release timing, API stability,
  convenience, and performance.
- **GP2** — compute anything well-defined; caution loudly; fail closed.
  Undecidable edge cases fail closed (not certified, not computed) rather than
  guessing.
- **GP3** — minimal dependencies. Few Imports; heavier functionality goes to
  Suggests with graceful degradation. A new dependency requires its own
  question gate and decision entry, so a mechanism needing one carries that
  cost explicitly.
- **No exported behavior changes in M108.** What `axes_reliability()` returns
  and refuses is unchanged by this milestone; the rewiring is M111's work.
  Recommendations about the rewiring are welcome and will be routed there.

## Output format

In `RR21-axes-degeneracy-per-fit-certificate.md`: answer each question by
number with your reasoning and evidence; list any additional findings
separately under "Beyond the brief"; end with concrete recommendations, each
marked apply / consider / reject-with-reason. Your report is advisory: this
brief does **not** request binding criteria, so emit recommendations only and
no `## Binding criteria` section.
