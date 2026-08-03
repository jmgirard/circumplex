# RR14: Review — does the mean-calibrated, tail-miscalibrated scaled χ² ship? (M68)

- **Date:** 2026-08-02
- **Brief:** `cairn/reviews/RB14-axes-reliability-scaled-chisq-calibration.md`
- **Reviewer stance:** independent; every load-bearing claim below was
  re-derived or re-measured in this review, not taken from the implementing
  session. New numerical work performed here: (i) an Olkin–Siotani
  reconstruction of `Γ_R` — a route sharing no arithmetic and no delta-method
  assembly with either the shipped code or the repo's vech oracle — checked
  against the delta-method construction, against a Monte Carlo, and carried
  end-to-end to an independent value of `c`; (ii) a fixed-`c_pop`
  decomposition of the tail excess from the committed fixture's per-replicate
  `chisq` and `cfactor` columns; (iii) an oracle mean-recentering diagnostic
  separating mean-shift from shape error in the small-N residual.

## Verdict in one paragraph

The derivation is right (Q1: confirmed through a fully independent route to
1e-15). The residual is the ML χ²'s finite-sample mean bias and nothing else
(Q2: the competing explanation the brief names — per-replicate noise in `ĉ` —
is ruled out by direct measurement, not just by the sweep; and recentering the
mean alone restores every cell to .048–.058). The adjusted-statistic shortcut
was harmless and the scope decision on `$fit$df` stays closed (Q3). All four
statistics should ship (Q4). AC3's rejection-rate clause as written demands
something the scaling correction never claimed and no scaling factor can
deliver; the honest replacement gates the tail where the theory makes the
claim, fences the small-N rates as regression tripwires, and puts the measured
small-N behaviour into the user-facing documentation with numbers (Q5, Binding
criteria).

## Q1. Is the derivation right? — Yes, confirmed independently at 1e-15.

The brief is correct that the 1e-15 oracle agreement cannot catch an error
shared by both routes: shipped code and vech oracle share (i) the conception
`Γ_R = J Γ_S J'` with the delta-method Jacobian, (ii) the evaluation point
`cov2cor(Σ̂)`, and (iii) `Γ_S = 2D⁺(Σ⊗Σ)D⁺'` with `V` pinned to it only
through the mutual check `VΓ_S = I` (which a reciprocal scalar error in the
pair would survive — though such an error cancels in `c`, since `U` is linear
in `V` and `Γ_R` linear in `Γ_S`). This review therefore rebuilt `Γ_R` from
the classical **Olkin–Siotani** closed formula for `n·cov(r_ij, r_kl)` under
normality — a published expression the repo never uses anywhere — and checked:

- **Γ_R itself.** At an arbitrary (non-model-implied) p = 6 correlation
  matrix, the Olkin–Siotani `Γ_R` equals the delta-method `J Γ_S J'` to
  3.3e-16, entrywise, including all off-diagonals — the cells the repo's own
  Pearson–Filon diagonal check never touches. A 4-variable Monte Carlo
  (4000 draws of n = 2000) agrees within Monte-Carlo error. The delta-method
  assembly both routes share is correct.

- **(a) The closed form at `R/axes_scaled_fit.R:127`.** At the same arbitrary
  matrix, `Σ_{k<l}[1 − (Σ⁻¹)_kl ρ_kl (1 − ρ_kl²)]` equals the literal
  `tr{V Γ_R}` built from the Olkin–Siotani matrix to 1e-14. Two structural
  corollaries also hold: at Σ = I the form gives p(p−1)/2 (correct, since
  `Γ_R` is the identity on off-diagonal coordinates there), and substituting
  `Γ_S` gives p\* exactly. Confirmed.

- **(b) The baseline collapse at `:172`.** The vanishing of the independence
  model's projection term is **exact and structural**, not approximate:
  `Δ_b' V_b Γ_R = 0` to machine zero. The reason is clean — `V_b` at the
  identity is diagonal in vech coordinates, so `V_b Δ_b` lies entirely in the
  diagonal-moment coordinates, and `Γ_R`'s diagonal-moment rows are
  identically zero (a sample correlation's diagonal does not vary). Hence
  `U_b = V_b` on the support of `Γ_R` and the trace collapses to
  `Σ_{i<j}(1−ρ_ij²)² / baseline_df = mean((1−ρ²)²)`. Confirmed, and the
  shipped `c_b` equals the independent literal-`U_b` value exactly.

- **End-to-end.** On the canonical octant probe, an independent computation of
  `c` — literal vech `V`, `U`, and Olkin–Siotani `Γ_R` — reproduces the
  shipped `c = 0.9563346` to a relative 1.9e-15. Separately, the absolute
  normalization of `c` is corroborated by simulation twice over: RR13's
  independently measured `E[T] = 261.1` against `df·c_pop = 261.07`, and the
  sweep's `mean(T_s)/df → 1.0016` at N = 4800.

- **(c) M68-D2's `cov2cor(Σ̂)` pricing.** Sound, and closer to forced than to
  chosen: every formula in `Γ_R` (and the Jacobian `J`) presupposes an exact
  unit diagonal — `(1 − ρ²)²` is not a meaningful quantity at ρ = 1.0017 —
  so the factor must be priced at a genuine correlation matrix, and
  `cov2cor(Σ̂)` is the model's own consistent estimate of the population
  correlation matrix under H0. The file comment's scalar-invariance argument
  is accurate but secondary; the operative argument is the estimand one, and
  it is right. One convention worth naming (not a defect): `Γ_R` is priced at
  the **model-implied** correlation matrix rather than at the sample R (lavaan
  prices its own SB `Γ` at sample moments). Under H0 both are consistent and
  the model-implied choice is smoother — the measured relative sd of `ĉ`
  (≤ 0.24%, Q2) reflects that. Under misspecification neither yields a
  chi-square statistic anyway. No change recommended.

One boundary to keep in view: everything above is **normal-theory**. `Γ_R`
here is the normal-theory acov of Pearson correlations; the factor corrects
the correlation-vs-covariance metric and nothing about non-normality. A
lavaan-literate user who reads "Satorra–Bentler scaled" will assume the
robustness meaning. The current documentation nowhere claims robustness, but
it also never fences the reading — see BC6.

## Q2. Is the residual really the ML χ²'s finite-sample bias? — Yes, and it is now established rather than inferred.

The sweep alone was suggestive but had a named hole: per-replicate estimation
of `ĉ` also vanishes with N, so monotone convergence could not by itself
separate "finite-sample bias in T" from "noise in `ĉ`" — both explanations
predict the same sweep. The committed fixture stores per-replicate `chisq`
and `cfactor`, so the decomposition is directly measurable, and this review
measured it:

| cell | rej(T/ĉ) | rej(T/c_pop) | sd(T/ĉ)/√(2df) | sd(T/c_pop)/√(2df) | rel sd(ĉ) | cor(T, ĉ) |
|---|---|---|---|---|---|---|
| strong N=600 | .0790 | .0780 | 1.0350 | 1.0346 | 0.09% | −.02 |
| weak N=600 | .0630 | .0630 | 1.0365 | 1.0372 | 0.24% | .05 |
| antic N=600 | .1070 | .1075 | 1.0248 | 1.0254 | 0.07% | .05 |
| sweep N=600–4800 | .092→.054 | .0925→.054 | 1.037→0.997 | identical to 4th decimal | ≤0.09% | ≤.03 |

Replacing the per-fit `ĉ` with the population `c_pop` changes the rejection
rate by **at most .0005** in any cell and the dispersion ratio in the fourth
decimal; `ĉ` is essentially unbiased (mean .9562 vs .9563) and nearly
uncorrelated with T. **The competing explanation contributes nothing
measurable.** The excess dispersion and the tail excess live entirely in T
itself.

Second, the residual's *shape*: recentering the scaled statistic by its own
empirical mean (`T_s · df/mean(T_s)`, an oracle Bartlett-type correction)
restores the rejection rate to **.0500 / .0475 / .0510** at the three N = 600
populations and .0515–.0575 across the sweep. So the residual is a **pure
mean shift** — the exact signature of the ML χ²'s well-documented
finite-sample upward bias (the thing Bartlett/Swain corrections exist for) —
with only a small remaining dispersion excess (sd ratio 1.002–1.022 after
recentering) that contributes at most ~.008 to the rejection rate.

Third, the mechanism that makes a 2% mean bias look like a .107 rejection
rate: the tail excess scales like Φ⁻¹-shift ≈ (relative bias)·√(df/2), so at
fixed N the over-rejection **grows with instrument size**. That is why the
antic corner (df = 627, bias 2.3%, shift ≈ 0.40 sd) rejects at .107 while the
weak population (df = 118, bias 1.4%, shift ≈ 0.11 sd) rejects at .063 —
same cause, different amplification. This belongs in the documentation
(BC5(iii)), because it tells a user *which* analyses are affected: large
instruments at moderate N, not small ones.

The implementing session's reading is therefore **correct**, and with the two
decompositions above it is established by direct measurement rather than by
the sweep's asymptotic argument. The one refinement: the work-log sentence
"the residual is the ML chi-square's own finite-sample upward bias" is right,
and can now cite that the mean shift accounts for essentially all of it.

## Q3. Would the adjusted statistic have been better? — No; the shortcut was harmless and the conclusion stands.

The population-`d'` shortcut does bias the comparison slightly in the adjusted
statistic's favor (it removes the estimation noise a real per-fit `d̂'` would
carry), yet even this best-case adjusted statistic barely improved (.0740 /
.0590 / .1030). Three reasons the shortcut cannot have changed the answer:

1. **There is almost no variance mismatch to correct.** `d' = 266.1` against
   `df = 273` — a 2.5% eigenvalue-dispersion effect — and the measured
   asymptotic dispersion ratio is 0.9974 at N = 4800: the scaled statistic's
   variance already matches its reference. The adjusted statistic's entire
   advantage over the scaled one is variance matching, and the margin here is
   a rounding error.
2. **A per-fit `d̂'` would add noise of the same negligible order as `ĉ`**
   (both are smooth functionals of the same `Σ̂`; `ĉ`'s measured relative sd
   is ≤ 0.24%). It could move the adjusted rates by O(.001), not by the
   .02–.05 needed to reach the band.
3. **Decisive:** the residual is a mean shift (Q2), and the adjusted statistic
   matches T's asymptotic mean exactly as the scaled one does — both inherit
   the finite-sample mean bias of T **in full**. No statistic of the form
   (multiplier)·T with an asymptotically-derived multiplier can remove it.

So the comparison's conclusion — eigenvalue dispersion is not the cause — is
correct, though the work-log's stated inference was incomplete (it is correct
*because* dispersion is measured small, not merely because the adjusted rates
barely differ). **Do not reopen `$fit$df`.** The scope decision that df stays
an integer count of overidentifying restrictions survives; fractional df would
mutate a documented field's meaning to buy nothing.

## Q4. Should this ship? — Yes, all four.

The choice is not "calibrated vs conservative". It is between:

- **Unscaled:** rejection .020–.027 at N = 600, moving **away** from nominal
  as N grows (.0145 at N = 4800; the asymptote is ≈ P(χ²_df > q95/c_pop) ≈
  .01, permanently miscalibrated), with the near-nominal small-N appearance
  being two errors partially cancelling. Its error direction — under-rejection
  of a goodness-of-fit test — is the harmful one: bad models pass, and the
  reliability numbers read off them inherit unflagged misfit. And the error is
  structural: the p-value is referred to a distribution the statistic does not
  have.
- **Scaled:** exact in mean everywhere tested, exactly nominal in the tail
  once p\*/N ≲ 0.1, over-rejecting .06–.11 at N = 600 with the excess a pure
  finite-sample mean shift that shrinks with N. Its error direction —
  flagging misfit that is not there — sends a user to inspect a model, which
  is recoverable; and `details$fit_uncorrected` sits beside it for exactly
  that comparison.

Per statistic:

- **`chisq`** — ship. As a descriptive quantity `T_s` is within 2.3% of its
  reference mean at N = 600 at every population; the unscaled statistic is
  ~4% low forever.
- **`pvalue`** — ship, with BC5's documentation. This is the only one of the
  four where the small-N behaviour bites, and it bites in the conservative
  (misfit-flagging) direction while the alternative is wrong in the
  flattering direction and does not converge. A p-value near .05 at moderate
  N deserves the documented caution; a p-value from the wrong reference
  distribution deserves replacement.
- **`rmsea`** — ship. The small-N mean excess translates to
  √(0.02·df/(df·N)) ≈ **0.006** at N = 600 — invisible against the .05/.08
  conventions — while the uncorrected version is persistently deflated (the
  exact "flattered" direction, on the index users compare to cutoffs), and
  under real misspecification the finite-sample bias is swamped by true
  misfit.
- **`cfi`** — ship. Both numerator and denominator are scaled on the same
  metric; the finite-sample excess enters both `T_s − df` and the (huge)
  baseline noncentrality, and its effect on the ratio is negligible. The
  mixed-calibration alternative (scaled χ² beside unscaled CFI on one
  printed line) is the harm RR13 B-2 named.

The no-user-switch stance and D-035/D-036's correct-not-caveat doctrine both
survive this review; nothing here recommends superseding either.

## Q5. What must AC3 become, and what must the documentation say?

AC3's rejection-rate clause as written gates the N = 600 tail at ±2.8 MC SE
of nominal — a property the scaling correction never claimed (satorra1994
p. 407 promises agreement **in mean**; the tail claim is asymptotic) and that
no multiplicative factor can deliver at p\*/N = 0.5 in the presence of T's own
finite-sample mean bias. The criterion was wrong as written, not the code:
it asked M68 to remove an error that is not the one it corrects, and the
evidence that the error it *does* correct is removed is complete. Amending it
after the result is exactly the move that needed outside scrutiny; having
re-derived the factor independently, decomposed the residual, and confirmed
the attribution, this review endorses the amendment — in the specific form of
the Binding criteria below (gate the mean everywhere, gate the tail where the
claim lives, fence the small-N rates as tripwires, and put the small-N
numbers in front of users).

Documentation substance (see BC5/BC6): the current prose (roxygen "matches
its reference chi-square in mean; it is not exact", vignette "a calibration,
not an exactness guarantee") is honest but incomplete — it does not tell a
user with N = 400 and a 36-item instrument that the χ² test will over-reject,
by how much, or that the direction of error flags rather than flatters. Those
are the three facts a user can act on. **Prose, not a runtime warning:** the
nominal-tail regime p\*/N ≲ 0.1 requires N ≥ 10·p\* (N ≥ 3000 for a 24-item
instrument), so a threshold warning would fire on essentially every real
dataset this function will ever see — an always-on warning is prose in a
worse place, and it would train users to ignore the warnings that matter
(this package uses warnings for failed corrections, which must stay audible).

## Beyond the brief

1. **The repo's oracle validates `Γ_R` only on its diagonal.** The vech
   oracle's independent check on `Γ_R` is the Pearson–Filon variance
   (`test-axes-scaled-fit.R:71-76`); the off-diagonal cells — most of
   `tr{U Γ_R}` — are validated only by the two delta-method routes agreeing
   with each other. This review closed that gap externally (Olkin–Siotani,
   3.3e-16, all cells, at a non-model matrix); the suite should close it
   internally (BC7) so the check survives this review's session.
2. **The amplification mechanism** (tail excess ≈ Φ-shift of
   (relative bias)·√(df/2)) predicts the cross-population ordering of the
   over-rejection and identifies large-df instruments at moderate N as the
   affected case. Worth one sentence in the vignette (folded into BC5(iii)).
3. **The mean-recentering result sharpens the future-work case:** an oracle
   mean correction alone restores .048–.058 in **every** cell, so a
   Swain/Bartlett-type small-sample correction to T would essentially close
   the small-N gap here — it is the right fix for the residual, where a
   better scaling factor is not. Filed under future work, per the brief's
   constraint.
4. **The work log's adjusted-statistic inference** ("so eigenvalue dispersion
   is not the cause") reached the right conclusion by an incomplete argument;
   the complete one is Q3's (dispersion measured at 2.5%, sd ratio → 1.00).
   Cosmetic; no action beyond this record.

## Recommendations

1. **Apply** — Ship M68 with all four statistics scaled, as wired. Replace
   AC3 with BC1–BC6 and BC8 below; add BC7 to AC2's oracle.
2. **Apply** — Documentation additions of BC5 and BC6 (roxygen + vignette;
   no runtime warning).
3. **Consider** — ROADMAP candidate: Swain/Bartlett-type small-sample mean
   correction to T, motivated by finding 3 above; must carry its own oracle
   and its own calibration cells when planned. Not an M68 requirement.
4. **Consider** — Report `p*/N` (or p\* and N) in `details` so a careful user
   can locate themselves on the documented calibration curve without
   computing it. Cheap; not gating.
5. **Reject** — Per-fit Satterthwaite adjusted statistic / fractional
   `$fit$df`: the residual is a mean shift the adjustment cannot touch
   (Q3); it would mutate a documented integer field to buy a measured ~.004.
6. **Reject** — Runtime small-sample warning: fires on essentially every
   realistic input (Q5), degrading the package's warning channel.
7. **Reject** — Any retreat to the unscaled statistics or a user-facing
   switch: the unscaled test is asymptotically miscalibrated in the
   flattering direction and worsens with N; D-035/D-036 stand.

## Binding criteria

- BC1: At each of the three AC3 populations (strong-axes, Strack COC S16
  Other weak-axes, anti-conservative corner), N = 600, ≥ 2000 replicates
  produced by the seed-pinned generator `devel/m68-scaled-fit-cells.R` with
  its per-replicate summary committed at
  `tests/testthat/fixtures/m68-scaled-fit-cells.rds`:
  `mean(T_s)/df ∈ [0.97, 1.03]`. (Measured: 1.0204 / 1.0139 / 1.0227.)
- BC2: At the strong-axes population, N = 4800, ≥ 2000 replicates from
  the same seed-pinned generator, stored in the same committed fixture:
  empirical rejection rate of `$fit$pvalue` at α = .05 within `[.036, .064]`
  (nominal ± 2.8 MC SE at 2000 replicates). (Measured: .0540 ± .0051;
  independent 3000-replicate run .0500 ± .0040.)
- BC3: At each of the three populations, N = 600, the scaled and
  unscaled rejection rates at α = .05 — computed from the committed fixture's
  per-replicate `p` and `p_unscaled` columns, not stored as separate scalars
  — are reported in the milestone (committed scaled: .0790 / .0630 / .1070;
  committed unscaled: .0270 / .0200 / .0215). A same-environment rerun of the
  generator (same seeds, same R and lavaan versions) must reproduce each rate
  exactly (agreement to ≤ 1e-12); for a regeneration under a changed
  environment (R or lavaan version drift) or with new seeds, each rate must
  lie within ±.021 (≈ 3 MC SE at 2000 replicates) of its committed value.
  These are regression fences, not calibration claims; a breach escalates
  rather than being re-fenced, and an escalation that accepts new values must
  update BC5's documented numbers in the same change.
- BC4: From the committed fixture's per-replicate `chisq` and `cfactor`
  columns, at each N = 600 cell: |rej(T/ĉ) − rej(T/c_pop)| ≤ .005 at
  α = .05, recording that the tail excess is not factor-estimation noise.
  `c_pop` for each population is the fixture's own
  `population_diagnostics$*$cfactor`. (Measured: ≤ .0005 in every cell;
  relative sd(ĉ) ≤ .0024.)
- BC5: Three user-facing surfaces carry the small-sample behaviour, at
  two depths. The `axes_reliability()` roxygen Details and the vignette's
  scaled-fit section each state, with these numbers: (i) the scaled statistic
  is calibrated in mean and its test is asymptotically exact, approaching the
  nominal rate as p\*/N falls — measured at the strong-axes population as
  .092 / .079 / .062 / .054 at p\*/N = 0.50 / 0.25 / 0.12 / 0.06, reaching
  the nominal band by p\*/N ≈ 0.06 (a single-population sweep; not stated as
  a universal threshold); (ii) at N = 600 the scaled χ² test over-rejects at
  α = .05 — measured .06–.11 at three populations chosen to bracket the
  accepted input space — while the uncorrected statistic under-rejects
  (.02–.03) and moves further from nominal as N grows; (iii) the
  over-rejection at fixed N grows with instrument size (df) and shrinks with
  N, so p-values near a chosen threshold at moderate N should be read
  cautiously, with the error direction being over-flagging rather than
  flattering; (iv) the rejection-rate evidence is complete-data — the FIML
  path's scaled statistic is calibrated in mean (AC4) but its tail behaviour
  is unmeasured, and the prose must not extend the rejection-rate claims to
  it. All documented rates are the committed fixture's values (rounded) and
  move only with it (BC3). Third surface: the printed note
  `axes_fit_scaled_note` (`R/axes_reliability_oop.R`), which `summary()`
  prints beside the χ²/RMSEA/CFI line, gains one sentence giving the
  direction of the small-sample error (the test can modestly over-reject at
  typical sample sizes) and pointing to `?axes_reliability` — direction and
  pointer only, no rates, so the printed note cannot drift from the fixture.
  No runtime warning is added for this.
- BC6: No user-facing surface describes the scaling as a robustness
  correction for non-normal data, established by an AC5-shaped sweep:
  `grep -rin` over `R/`, `man/`, `vignettes/`, `NEWS.md` and
  `tests/testthat/` for `robust`, `non-normal`, `nonnormal`,
  `distribution-free`, `kurtosis` and `ADF`, with every hit dispositioned in
  the work log as (a) updated, (b) a historical reference inside a NEWS entry
  for an already-released version, or (c) an unrelated use (e.g.
  `ssm_sem()`'s robust estimators), listed and left untouched. Additionally,
  the `axes_reliability()` roxygen Details block carries at least one
  sentence stating the factor is normal-theory and corrects the
  correlation-versus-covariance metric only, whose presence the sweep log
  records.
- BC7: The AC2 vech oracle gains an independent off-diagonal check on
  `Γ_R`: at least one probe map's `Γ_R` is compared entrywise (all cells,
  not the diagonal only) against the closed normal-theory formula for
  `n·cov(r_ij, r_kl)`, written out in the test itself:
  `½ρ_ij ρ_kl (ρ_ik² + ρ_il² + ρ_jk² + ρ_jl²) + ρ_ik ρ_jl + ρ_il ρ_jk −
  ρ_ij(ρ_ik ρ_il + ρ_jk ρ_jl) − ρ_kl(ρ_ik ρ_jk + ρ_il ρ_jl)`, agreeing to
  ≤ 1e-12 absolute. This is an internal test-side recomputation, not shipped
  formula code, so AC6's source-note requirement does not attach: the shipped
  statistic nowhere relies on this identity, the formula is fully specified
  above rather than by citation, and its own correctness is established by
  the required agreement with the repo's independent delta-method route
  (measured 3.3e-16 in this review) — a disagreement fails the suite rather
  than shipping a wrong number. (Attribution to Olkin–Siotani may appear in a
  comment; no PDF shelving gates this criterion.)
- BC8: The regression evidence stands in the suite, not only in the work
  log: a test file reads the committed fixture and asserts, from its stored
  per-replicate columns, BC1's three means, BC2's rejection rate, BC3's six
  rates against their fences, and BC4's ≤ .005 bound; and a fast live smoke
  cell (following M65's harness pattern, ≤ ~20 replicates at one population)
  runs the generator's replicate function end-to-end so a regression in the
  wiring is caught without the 5-minute full run. Checkable:
  `grep -rn m68-scaled-fit-cells tests/` is non-empty and the named
  assertions are present.
