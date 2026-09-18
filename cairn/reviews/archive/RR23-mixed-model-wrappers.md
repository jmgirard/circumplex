# RR23: Wrapper functions that fit the growth model with glmmTMB or brms

- **Date:** 2026-09-17
- **Brief:** `cairn/reviews/RB23-mixed-model-wrappers.md`
- **Reviewed at:** `master` `2613b4a6` (the brief's line numbers, given for `3a4687a0`, still match)
- **Advisory.** The brief's header slot does not say `requested`, so this report has no binding criteria section.

## What I read and ran

I read all ten material items in the brief's order. I ran `Rscript devel/m27-growth-recipe.R` with glmmTMB 1.1.15 and brms 2.23.0 installed locally. It completed, every wave was certified, and the true d(t) sat inside every interval. I also ran a probe script (scratchpad only, not committed). The probe fit four variants of the reference model to simulated data: correlated (`us`), independent (`diag`), shared intercept (`(1 | person)`), and no random effects. It then inspected each fit object for what a mechanical joint-structure check can see. The numbers from that probe appear under Question 3.

## 1. Is a wrapper worthwhile at all?

No, not as a fitter. The convenience gain is real but small. The costs the spec named are the right ones.

**Who benefits.** The user class is the applied researcher who has never written a multivariate mixed model. Without help, this researcher fits `x` and `y` separately. That is exactly the user the joint-fit holding protects. But a wrapper serves this user only while the vignette's fixed model matches their design: person random intercepts, linear time, no covariates. Adding a covariate or a random slope breaks that match. Then the wrapper either refuses, and the user writes the raw call anyway, or it exposes formula fragments. In the second case it becomes a formula builder for an engine the package does not own (Question 3). Either way the vignette must keep teaching the raw six-line call. The raw call is the only path that covers real designs. A wrapper therefore adds a second path to teach. It does not replace the first.

**Is the gap real or imagined?** The six-line fit call in `vignettes/growth-ssm-analysis.Rmd` lines 118 to 126 is not the gap. It is short, it is printed in full, and a reader can copy it. The vignette omits the glue on either side. One omission is the reshaping of `coord` into `long` ("The reshaping code is omitted", line 116). The other is the loop that evaluates draws at each wave and stacks `results` and `details$certified` into the trajectory table (line 229, re-cut by M128). The reader gap, to the extent one exists, is there. The ROADMAP already parks a helper for the second omission on irreversible-export grounds (`cairn/ROADMAP.md` line 24). A fitter does not close that gap. A post-fit adapter does.

**The statistical-correctness argument under IP1.** IP1 says correctness outranks convenience and API stability. It does not tip toward a wrapper here, for three reasons.

1. The package ships no wrong number today. The shipped numeric path is `ssm_draws()`, which is correct and oracle-tested. The univariate shortcut is a hazard in the user's own code. IP1 governs what the package computes. The package's levers over user code are teaching (GP5) and refusal at its own input boundary (GP2). A fitter is neither.
2. A fitter protects only its own callers. It removes the shortcut by construction for users who stay inside it. It offers nothing to users outside it, and those are the same users a fixed wrapper cannot serve.
3. The hazard has a cheaper catch point. The package already reads the fit's fixed effects and their covariance on the way into the draws step. A check at that point detects the independent-fits structure mechanically (Question 3). That check lives on the output side, where the package already sits, and freezes nothing about the fit.

The honest weighing is this. A fitter buys a six-line saving for the vignette's exact design. The price is one or two post-2.0 export commitments (GP4). It is also a dependency on the engine's formula and object API across releases, and a second teaching path. The spec section 4.1 reasons hold.

## 2. If worthwhile, which engine?

I do not recommend any of the four, but they differ enough to record.

**glmmTMB only.** Check consequence: every example carries `@examplesIf requireNamespace("glmmTMB", quietly = TRUE)`. Every test carries `skip_if_not_installed("glmmTMB")`. The function gates on `requireNamespace()` with an install hint, exactly the lavaan pattern at `R/axes_reliability.R:1147`. A CRAN builder without glmmTMB sees skipped tests and skipped examples. A user without it sees the install-hint error. That is the standard Suggests story and it works. Two costs sit beyond the API commitment. First, glmmTMB is a compiled package whose runtime depends on TMB and Matrix binary compatibility. It has a history of version-mismatch warnings and refusals at load. That makes it a less stable runtime engine than lavaan, which is pure R. The vignette's conditional chunks absorb that today. An exported function surfaces it as a user-facing failure. Second, glmmTMB has no degrees-of-freedom-adjusted inference. The vignette's small-N remedy in section 7 names pbkrtest for lme4 fits and nlme's approximate df. A glmmTMB wrapper that owns the uncertainty step therefore has no in-engine small-sample remedy to offer (Question 5).

**brms only.** It cannot fit on CRAN builders (D-015). The only testable surface is what the wrapper constructs before it calls `brm()`. There is a constraint conflict here. D-015 says brms is "never loaded by package code, tests, or vignette build". The brief's constraint says a brms wrapper "must be testable ... by testing the formula, prior, and data it constructs". The two are compatible only under one design. The wrapper builds plain R objects (a formula, a data frame, a prior table as a data frame) that tests inspect without loading brms. It calls `brms::bf()`, `set_prior()`, and `brm()` only at the last step. The natural test is `brms::make_standata()`, which needs no compiler. My probe showed it reports `NC_1 = 3` correlation terms for the `(0 + dv | person)` structure. But that call loads brms in tests and needs a D-015 amendment. API cost: one export whose fit step is never tested in the package's own CI. That is a weak place for an exported function to live. It is also new statistical territory (Beyond the brief, item 4).

**Both behind one `engine =` argument.** The return type varies with the argument: a `glmmTMB` object or a `brmsfit`. The uncertainty semantics vary with it too: MVN draws from a REML covariance, or posterior draws. One signature that returns different classes with different inferential meaning is the worst shape for GP4. Any later change to either branch changes the one exported signature. Reject on shape alone.

**Both as separate functions.** Two export commitments, two gates, two documentation pages, and the brms fit step untestable. That is twice the cost of glmmTMB-only for the same convenience.

If a fitter is ever built despite Question 1, glmmTMB-only is the only defensible option. brms stays a vignette engine.

## 3. What must the wrapper fix, and what can it expose?

The reference recipe is at `devel/m27-growth-recipe.R` lines 73 to 78 and vignette lines 118 to 126. Its choices, one by one:

| Choice | Disposition | Reason |
|---|---|---|
| Joint stacked outcome (`value ~ 0 + dv + ...`) | Hard-code | D-013 holding (3). Not negotiable. |
| Correlated random intercepts (`us(0 + dv \| person)`) | Hard-code | This is the structure that makes the fit joint. An alternative here is the shortcut. |
| Per-outcome residual variance (`dispformula = ~ 0 + dv`) | Hard-code | `e` is on a different scale from `x` and `y`. A shared residual variance is a misspecification with no use case. |
| Linear `wave` | Expose narrowly, or hard-code | The only choice with a plausible user need (quadratic time, covariates). The minimal exposure is a one-sided time formula that the wrapper crosses with `dv` itself. Anything wider is a formula builder. |
| REML | Expose as a logical, default `TRUE` | Harmless. ML is needed for likelihood-ratio comparison of fixed effects. |
| MVN propagation from the fixed-effect vcov | A separate step, not part of the fit | See Question 5. It belongs in a post-fit function. |

**Random slopes.** They are not in the reference recipe. Adding `wave` to the random part (`us(0 + dv + dv:wave | person)`) gives a 6 x 6 unstructured covariance. That model often fails to converge at the vignette's N. A wrapper with a `random = "slopes"` switch owns that convergence problem and its diagnostics. Do not expose it.

**What to refuse under GP2.** A wrapper that hard-codes the random-effects structure has nothing to refuse at the fit stage. The joint structure is built in. Refusal becomes necessary the moment any formula fragment is accepted. The rule is then: refuse any random-effects term that does not put the `x` and `y` levels of `dv` in one covariance block with off-diagonal entries. That is a wrong-object input in GP2's sense. It estimates a different quantity. It is not a caution case.

**Is the joint structure checkable after the fact?** Yes, in glmmTMB, in two ways with different reach. My probe used glmmTMB 1.1.15, n = 80 persons, 4 waves, and correlated person intercepts:

| Fit | `blockCode` / `blockSize` | Largest cross term between `x` and `y` fixed effects in `vcov(fit)$cond` |
|---|---|---|
| `us(0 + dv \| person)` | 1 / 3 | 3.95e-05 |
| `diag(0 + dv \| person)` | 0 / 3 | 0 (exactly) |
| `(1 \| person)` | 1 / 1 | 1.84e-04 |
| no random effects | none | 0 (exactly) |

The public-API check is the covariance one. Independent per-outcome random effects, and no random effects, make the fixed-effect covariance block-diagonal across `dv`. The cross block is then identically zero, not small. That detects the univariate-shortcut equivalent exactly. It needs only `vcov(fit)$cond` and the coefficient names, which are stable generics. Its limit: it cannot tell a correct `us()` fit from the shared-intercept `(1 | person)` misspecification, which also produces a nonzero cross block. The structural check reads `fit$modelInfo$reStruc$condReStruc[[k]]$blockCode` (1 is `us`, 0 is `diag`) and `blockSize`. It can tell that the `x` and `y` levels share an unstructured block. That is more discriminating, but it is glmmTMB internal state, not a documented API. A wrapper that reads it takes on glmmTMB's internals as a dependency. For brms, `make_standata()` exposes the count of correlation terms (`NC_1`) before fitting. A fitted `brmsfit` exposes the group-level correlation names through `VarCorr()`. The former is the testable surface, subject to the D-015 tension in Question 2.

Two notes for whoever writes such a check. First, the recipe's own guard at `devel/m27-growth-recipe.R` line 86 is `stopifnot(abs(xy_cov) > 0)`. It is the right kind of test but its threshold is met trivially. The recipe run printed `Cov(b_x0_hat, b_y0_hat) = 8e-05`. In a balanced design with moderate person-effect correlation the cross covariance is small by construction. It becomes large only in the oracle's `xycor` cell. A test for exact zero is the sharp one. Second, a nonzero cross block proves that the fit was not the independent shortcut. It does not prove that the fit was the right model.

## 4. Return object and downstream contract

If a fitter ships, it must return the engine's fit object unchanged, with no added class and no wrapper. Two reasons. `fixef`, `vcov`, `anova`, `simulate`, `residuals`, and every diagnostic the user needs come from the engine. A wrapper class forces the package to re-export or proxy them. And a bare engine object is the one return shape that commits the package to nothing under GP4. A package class around the fit makes the class's structure an export commitment, and every engine change leaks through it.

So the fitter cannot also return the trajectory table. The per-wave step needs its own function. This is the ROADMAP's parked helper. The maintainer needs to notice that a fitter forces the parked decision open rather than sitting beside it. A fitter without the post-fit helper leaves the user exactly where the vignette leaves them, with the loop to write. So the fitter's convenience claim depends on the helper existing. The helper, not the fitter, is the function that carries value. It is also the natural home for the mechanical joint-structure check from Question 3. It refuses a fit whose `x`/`y` cross block is identically zero, because that is a wrong-object input for the d(t) interval. If built, its input contract is the engine-agnostic pair (fixed effects, their covariance) plus a way to evaluate the mean at each time. Then lme4 and nlme fits are also accepted. `model.matrix()` on the fixed-effect terms at `newdata` gives the per-time contrast matrix generically.

**Certification.** `details$certified` from `ssm_draws()` is per call, so per wave. Any shape that flattens waves into one table must carry a `certified` column. The trajectory-table contract already expects it (`R/ssm_trajectory.R`, `ssm_trajectory_table_frame()`, and the vignette table at lines 232 to 238). A returned engine object carries no certification because it carries no trajectory. That is correct. The certification belongs to the summary, not the fit.

**Does IP3 bind?** Not the delegated fit. The package does not ship the engine's numbers. IP3 binds every number the package does ship from the growth path: the trajectory table with a(t), d(t), their intervals, and `certified`. Oracle one exists already, the M27 simulation-coverage oracle. The natural second oracle for anything that reads a fit is cross-engine agreement. The same joint model fit in nlme (the spec's base-R alternative, with `varIdent` and `corSymm`) or lme4 gives the same fixed effects. To REML tolerance it gives the same covariance. The trajectory derived from each must agree within a pre-registered tolerance. A third, cheaper oracle for the fitter itself is identity with the hand-written call. The wrapper's fit must be `all.equal` to a literal `glmmTMB::glmmTMB(value ~ 0 + dv + dv:wave + us(0 + dv | person), dispformula = ~ 0 + dv, ...)` on the same data. All three run only with glmmTMB installed, so they skip on a builder without it. That is acceptable. The numbers they guard are also unreachable there.

## 5. Uncertainty propagation as a hidden default

Adopt (b): propagate with a documented caution, and keep the vocabulary aligned with `ssm_analyze()`.

Reasoning. GP2 says compute anything well-defined and caution loudly. MVN propagation from the fixed-effect covariance is well-defined. It is the same asymptotic move `ssm_analyze(method = "montecarlo")` already makes and documents (`R/ssm_analysis.R` lines 86 to 92 and 132 to 142). Its small-N anticonservativeness is a caution, not an ill-definedness. So refusing it, option (a), blocks a defensible analysis. Requiring a choice, option (d), is out of pattern for a package whose every other estimator has a documented default. The caution belongs where `ssm_draws()` prints the certification note, in `summary()` or in the printed table's footer. It names its trigger (the REML covariance conditions on estimated variance components) and the remedy.

On (c), a bootstrap alternative, two things. First, the existing vocabulary is `method = "bootstrap"` for case resampling of rows and `method = "montecarlo"` for MVN draws from an asymptotic covariance. In a growth design the row is the person. So the honest analog of `"bootstrap"` is a cluster case bootstrap over persons with a refit per replicate, not a parametric bootstrap. Naming a parametric bootstrap `"bootstrap"` gives the word a different meaning in one function than in the rest of the package. Second, either bootstrap is a refit loop inside the engine. It takes minutes at the vignette's N. It is also the one part of the pipeline that most needs the user's judgment, for convergence failures per replicate and how to treat them. Keep any bootstrap on the vignette side as a shown recipe rather than a package option. Note also that glmmTMB gives no Kenward-Roger or t-scale remedy. A wrapper on glmmTMB that owns this step has only the bootstrap to offer for small N. That is a further reason the propagation step must not be silent inside a fitter.

The brms path sidesteps the problem entirely. Posterior draws already integrate over the variance components. That is the principled answer to the section 7 caution, and it is the engine the package can neither fit nor test in CI.

## 6. Retirement option

**Keep as is.** Recommended. The current arrangement puts the package's effort exactly where its correctness lever is. It has a correct input adapter. It has a correct output adapter with per-t certification. It has a simulation oracle that discriminates against the documented wrong path. It has a vignette that shows the joint call in full and says in bold why. glmmTMB in Suggests costs nothing at runtime, since no package code loads it, and the vignette chunks are guarded. One change fits inside "keep as is". Make the vignette's protection against the shortcut visible in output rather than only in prose. Print `glmmTMB::VarCorr(fit)` after the fit so the reader sees the 3 x 3 correlated person block. Show the one-line zero-cross-block check from Question 3 as a habit the reader can carry into their own designs. Both are prose and chunk edits with no API surface.

**Add wrappers.** Not recommended, for the reasons in Questions 1 to 5 taken together. The six-line fit is not where the reader's gap is. A fixed wrapper covers only the vignette's design, and a flexible one becomes a formula builder. The return object must be the bare engine fit, so the fitter's convenience depends on a post-fit helper, which is the actually useful function. glmmTMB is a less stable runtime dependency than lavaan. The brms half cannot be tested where it matters. One candidate survives this review for a maintainer who wants to ship something: the post-fit helper. It maps a fit, or a (coef, vcov) pair, to a trajectory table, with the mechanical joint-structure refusal and the section 7 caution printed. The ROADMAP's own promotion criterion for it (readers cannot rebuild the table, or users ask) is the right gate.

**Retire the growth recipe.** Not recommended. Dropping glmmTMB from Suggests saves nothing measurable. No package code loads it, and the vignette's chunks are conditional. Reducing the vignette to the coordinate transform plus a pointer removes the package's only concrete, worked demonstration of the joint fit. That demonstration is the single piece of teaching that stands between the reader and the univariate shortcut. Retirement therefore makes the shortcut more likely, not less. It forfeits GP5's teaching mission for no dependency gain. `ssm_draws()` alone cannot carry the growth story. It summarizes draws but says nothing about how the draws must be produced. That "how" (joint, correlated) is the statistics.

## 7. Reopening evidence (GP7)

Record these as the classes of evidence that reopen the no-wrapper holding. Record separately the class that promotes the parked helper.

Reopens the wrapper question:

1. A published or circulated analysis, traceable to this package's vignette lineage, that fits the coordinates separately or with a shared person intercept. It must report d(t) intervals. One public instance is enough.
2. Repeated user requests, in issues or correspondence, for a fitter. I set the bar at three independent requesters, since a single request is the normal background rate for any convenience function. One request also counts on its own in one case: it comes with a design the vignette's raw call cannot express and a wrapper can.
3. glmmTMB or brms changing the formula syntax or the fit-object accessors the vignette relies on (`us()`, `dispformula`, `fixef()$cond`, `vcov()$cond`). That change makes the vignette's raw call the fragile thing, and a package-owned adapter possibly the more stable one.

Reopens in the other direction, toward retirement: the growth vignette's conditional chunks failing on CRAN builders twice in a row because of glmmTMB's own build or binary-compatibility state. That makes the Suggests entry a check liability rather than a free teaching aid.

Promotes the parked post-fit helper (already in the ROADMAP): readers demonstrably cannot rebuild the trajectory table from the shown `ssm_draws()` call, or users ask for it. Add the mechanical joint-structure check to that row's scope, so the helper is where the refusal lives.

## Beyond the brief

1. **The recipe's cross-covariance guard is not a discriminator.** `devel/m27-growth-recipe.R` line 86 asserts `abs(xy_cov) > 0` and the run printed 8e-05. The sharp test is exact zero (the block-diagonal case). Label the check as detecting the independent-fits structure only, not as testing the model.
2. **A second plausible-but-wrong path is not covered by the oracle.** The `xycor` cell discriminates against independent univariate fits. The shared-intercept model `(1 | person)` is at least as natural a mistake for lme4 users. It passes a nonzero-cross-block check, and it imposes equal variances and equal covariances across (e, x, y). Its effect on d(t) coverage is unmeasured. A vignette that names it as a wrong path needs an oracle cell for it. It is possibly worth a cell regardless, as it is the mistake the vignette's bold warning does not mention.
3. **D-015 versus the brief's brms testability constraint.** As written, D-015 forbids loading brms in tests. The brief asks that a brms wrapper's constructed formula, prior, and data be testable. The only reconciliation is to build plain R objects and defer every brms call to the end. `make_standata()` is off limits without a D-015 amendment. Any brms-facing plan needs to say which side gives.
4. **A brms growth model is new statistics, not a port.** The Bayesian vignette's brms model is a cross-sectional cosine regression with a person intercept. See `vignettes/bayesian-ssm-analysis.Rmd` lines 138 to 145. It is not a growth model. A brms growth wrapper needs priors on a 3 x 3 person-effect correlation (LKJ), per-outcome residual scales, and per-outcome slopes. The section 6 induced-prior disclosure then applies to a(t) at every t and to the slope-induced prior on the direction of change. None of that has an oracle in the repo. It is also the path that answers the REML caution properly (Question 5). That makes it the more interesting research direction and the less shippable function.
5. **glmmTMB as a runtime dependency is shakier than lavaan.** lavaan is pure R. glmmTMB is compiled against TMB and Matrix and has had version-skew episodes that produce warnings or refusals at load. The vignette's conditional chunks are the right place for that risk to sit. An exported gate moves it to users.

## Recommendations

1. **Keep the adapter-not-engine holding. Do not add a fitter.** Apply. Record it with the Question 7 reopening evidence.
2. **Show the joint structure in the growth vignette's output.** Print `glmmTMB::VarCorr(fit)` after the fit. Show the one-line exact-zero cross-block check on `vcov(fit)$cond` as a self-check the reader can reuse. Apply. Prose and chunk edit, no API.
3. **Sharpen the recipe's guard** at `devel/m27-growth-recipe.R` line 86 from `abs(xy_cov) > 0` to an exact-zero test on the full `x`/`y` cross block. Add a comment that it detects the independent-fits structure only. Apply. Trivial.
4. **Add the shared-intercept `(1 | person)` misspecification as a coverage-oracle cell** in `devel/m27-coverage-oracle.R`. Report its coverage. Name it in the vignette beside the univariate shortcut in case coverage fails. Consider. It is a full oracle run and needs a pre-registered acceptance line first.
5. **Fold the mechanical joint-structure refusal into the parked post-fit helper's scope** in `cairn/ROADMAP.md` line 24. State that its input contract is engine-agnostic (fixed effects, covariance, per-time contrast) so it never becomes a glmmTMB wrapper by accident. Consider. Tracking edit now. The helper's promotion criterion is unchanged.
6. **A glmmTMB-only fitter.** Reject. It covers only the vignette's design and forces the parked helper open. It returns a bare engine object or breaks GP4. It inherits glmmTMB's binary fragility as a user-facing error.
7. **A brms fitter, alone or behind an `engine` argument.** Reject. The fit step is untestable under D-015. A shared signature returns different types. The statistics are new and have no oracle.
8. **Retire the growth recipe.** Reject. It saves no dependency cost and removes the one demonstration that stands between readers and the shortcut.

## Verdict

The adapter-not-engine holding survives its second review. The convenience a fitter buys is a six-line call that the vignette already prints in full. The reader's real gap is the omitted glue around that call, which is the parked post-fit helper's territory and not a fitter's. A fixed wrapper covers only the vignette's design. A flexible one becomes a formula builder for an engine the package does not own. A correct one must return the bare engine object, so its value collapses onto the helper it needs beside it. The univariate-shortcut hazard is real, but it lives in user code. The package's honest levers there are the vignette's teaching and a mechanical exact-zero check on the fixed-effect cross block at the output boundary. Both cost no API. Keep the recipe, keep glmmTMB in Suggests, and do not ship a fitter for either engine. Reopen only on one of three events: a public misuse traceable to the shortcut, three independent requests for a fitter, or an engine API change that makes the raw call fragile.
