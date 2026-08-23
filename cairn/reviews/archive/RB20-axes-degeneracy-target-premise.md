# RB20: is the accuracy target's sampling-variability premise right, and what sample size is it calibrated for? (M106)

- **Date:** 2026-08-23
- **Output required:** write findings to `cairn/reviews/RR20-axes-degeneracy-target-premise.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN for circumplex data analysis. Its
exported `axes_reliability()` fits a structural model to an item correlation
matrix via `lavaan` and reports component reliabilities with corrected
standard errors, plus scaled fit statistics. Users reach it two ways: with
raw `data`, or with `cormat = <correlation matrix>` and a `n` they type
themselves.

Both surfaces refuse to report numbers computed from a fitted covariance
matrix that is too degenerate to price. The refusal is one stated criterion:
the smallest eigenvalue of `cov2cor(Sigma-hat)`, relative to its largest, must
exceed `sqrt(p * .Machine$double.eps / tau)`; at or below that, the surfaces
return `NA` with a named reason. The rationale is that the corrected branch
inverts the priced matrix twice, so its entries carry relative error growing
like `p * kappa^2 * eps`, and the floor is where that bound reaches an
accuracy target.

**The history of this constant, in three escalations.** RB18/RR18 (archived)
settled *which matrix* is priced — `cov2cor(Sigma-hat)` for every
user-reported quantity, raw `Sigma-hat` only for an internal arm — after
measuring a committed counterexample on which the shipped corrected standard
errors were wrong by 3.4% while the reported reason was `NULL`. That is the
package's first measured silent wrong number in this subsystem. RB19/RR19
(archived) set the *accuracy target*: it derived `delta_star = 1e-4` from the
standard error's own sampling variability, kept the oracle's measured
calibration ceiling `C = 10`, and made the shipped constant
`tau = delta_star / C = 1e-5`. That moved the refusal threshold outward
(kappa 1.37e4 → 4.3e4 at p = 24), which is what let a legitimate
near-duplicate item pair at r = .9999 compute instead of being refused.

Milestone M106 implemented RR19's recommendation. Its second review round
found that the derivation now written beside the constant rests on a step
that is asserted rather than argued, and that a consequence RR19 named is
acknowledged in the text and then not acted on. **The accuracy target, and
therefore the shipped constant and the whole loosening, depend on the
answer.** That is why this is escalated rather than settled in-session: the
implementing session cannot grade its own statistical argument, and there is
no oracle for "what error may a reported standard error carry".

## Materials

Read these. Line numbers are as of commit `475120c5` on branch
`m106-degeneracy-accuracy-target`; if they have drifted, search for the
quoted text.

1. **`R/axes_corrected_se.R:398-474`** — the comment block under
   `THE TARGET AND THE CEILING`, and the three constants it stands behind
   (`axes_degeneracy_delta_star`, `axes_degeneracy_calibration_ceiling`,
   `axes_degeneracy_tau`). Lines 404-425 carry the derivation this brief is
   about. Read the whole block from line 336 for the criterion's full
   context, including `WHY THIS CUTOFF` and `WHAT THE BOUND IS AND IS NOT`.
2. **`R/axes_corrected_se.R:602-612`** — `axes_sigma_degenerate()`, the
   criterion itself, and the M90 partition comment above it (from line 590)
   that decides `"indefinite"` versus `"ill_conditioned"`.
3. **`R/axes_corrected_se.R:150-330`** — `axes_corrected_se()`, so you can
   see what the corrected standard error actually is: which matrices are
   inverted, and what the reported quantity is a functional of.
   `axes_se_pricing()` is the sandwich; `axes_se_derivs()` in
   `R/axes_reliability.R` builds the derivative set.
4. **`R/axes_reliability_oop.R:31`** — `axes_fmt()`, the print formatter the
   derivation's first cross-check appeals to. Note it takes `digits = 3` and
   passes them to a rounding call: confirm for yourself whether that is three
   decimal places or three significant digits, because the derivation's claim
   depends on which.
5. **`cairn/reviews/archive/RR19-axes-degeneracy-accuracy-target.md`** —
   sections 1 (the target), 4 (one constant or several), and 5 (removal).
   This is the review whose derivation is now under question. Section 1 is
   what M106 transcribed into the comment block.
6. **`cairn/reviews/archive/RR18-axes-degeneracy-criterion-metric.md`** —
   the measured 3.4%-wrong-SE counterexample and the `p * kappa^2 * eps`
   envelope.
7. **`devel/degeneracy-oracle/exact_oracle.R`** — an exact-rational oracle
   driver. Run it with `Rscript devel/degeneracy-oracle/exact_oracle.R` from
   the repo root (needs `python3`; it exits 0 and prints three PASS lines).
   It measures the double-precision corrected standard errors against exact
   arithmetic at the RB18 fixture and over five model-implied "reachable"
   geometries at p = 4, 8 and 9. Use it if you want your own numbers.
8. **`cairn/DECISIONS.md`** — entries D-037, D-044 and D-048 (scan the
   `### D-` headings; read each whole).
9. **`cairn/DESIGN.md`** — the numbered principles, in particular IP3.

## Questions

1. **Is the corrected standard error's relative sampling standard deviation
   correctly priced at `1/sqrt(2*(n-1))`?** The block argues: "The corrected
   SE is a smooth plug-in functional of Sigma-hat, so its relative sampling
   SD is of order 1/sqrt(2*(n-1)): for (n-1)s^2/sigma^2 ~ chi^2_{n-1} the
   relative SD of s^2 is sqrt(2/(n-1)), which the delta method halves for the
   square root." The objection: the corrected standard error is a functional
   of all `p(p+1)/2` entries of `Sigma-hat`, not of a single variance.
   Averaging over many entries can shrink a functional's sampling standard
   deviation relative to any one of them — plausibly by something like the
   square root of the number of effectively independent directions its
   gradient loads on. Give the correct order for this quantity, with the
   delta-method argument written out: what is the gradient of the reported
   corrected standard error with respect to `Sigma-hat`, and what does the
   sampling covariance of `Sigma-hat` (Wishart-type, order `1/n`) give for its
   variance? If the honest answer is a range rather than a number, give the
   range and its endpoints' assumptions.

2. **Does the answer to question 1 move `delta_star = 1e-4`?** State the
   target the corrected derivation implies, and by what factor it moves. The
   figure that matters for the milestone: M106's own falsifier, recorded at
   its planning gate, is a target at or below `6e-6` — below that, the
   loosening this milestone shipped is not justified and the earlier
   `tau = 1e-6` was closer to right. The current derivation's margin is a
   factor of 10 (numerical bias held to one tenth of statistical noise), so a
   correction larger than about 17x consumes it. Say plainly whether the
   shipped `delta_star = 1e-4`, and therefore `tau = 1e-5`, stands, moves, or
   cannot be settled from what is here.

3. **What sample size is the target calibrated for, and what holds outside
   it?** The block calibrates at `n = 5e5` and says: "Smaller n only loosens
   the requirement, which is why the absence of a minimum sample size is the
   harmless direction and the cormat path's unbounded n is the binding one."
   It then fences nothing. On the `cormat` path `n` is typed by the user and
   is not bounded above; at `n = 1e8` the sampling relative SD is about
   7.1e-5, so a numerical error at `delta_star = 1e-4` would exceed the
   statistical noise it is priced against rather than sitting a tenth below
   it. Is that a real exposure or an academic one for this package's users?
   If real, what is the right response — a documented upper calibration bound,
   a warning above some `n`, an `n`-dependent target (note RR19 section 4
   rejected an `n`-dependent **tau**; say whether your answer contradicts that
   and why), or something else? If the honest answer is that nothing is
   needed, say so and say why the acknowledgement in the text should then
   change.

4. **Two cross-checks in the block are alleged wrong. Are they?** Both are
   offered as `n`-free corroboration of `delta_star = 1e-4`; if they fall, the
   target rests on question 1 alone.
   (a) "print.circumplex_axes_reliability() formats at 3 decimals (axes_fmt,
   R/axes_reliability_oop.R), so print resolution is at least 1e-3 relative at
   the largest printable SE" — the objection is that rounding to three
   *decimals* is absolute resolution `1e-3`, so relative resolution is
   `1e-3/SE`, which is *worst* at the largest standard errors, inverting the
   claim.
   (b) "a relative SE error delta moves nominal 95% Wald coverage by about
   0.23*delta, so 1e-4 shifts coverage by 0.002 points" — the objection is
   that `0.23 * 1e-4` is `2.3e-5` in coverage probability, i.e. 0.0023
   *percentage* points, so "0.002 points" is about 87x too large.
   Confirm or refute each, and say whether either, once corrected, still
   corroborates the target.

5. **Removal.** This mechanism is now on its third escalation (RB18, RB19,
   this brief), so removal of the ill-conditioning limb is put to you
   explicitly rather than assumed away. RR19 section 5 weighed removal and
   rejected it narrowly, on three grounds: past the floor the package has no
   shipped means of certifying the number to the target (IP3); measured
   accuracy in the reachable set is not a bound; and the only a-priori error
   estimate a replacement caution could carry is the same `p * kappa^2 * eps`
   envelope, which overstates the actual error by 5 to 8 decades in every
   reachable geometry, so it would warn of percent-level error over numbers
   accurate to `1e-13`. D-048 records reopening evidence for that holding:
   (i) a converged fit this criterion refuses while an exact oracle shows its
   standard errors and `cval` within target, or (ii) field reports on real
   data. **Trigger (i) is now half-met and you should weigh that**: M106's
   test case at p = 8, kappa 1.0e5 against a 7.5e4 floor, is refused through a
   genuinely converged fit while the oracle measures that identical
   construction's standard-error relative error at `3.0e-12` — eight decades
   inside `delta_star = 1e-4`. The `cval` half is simply unmeasured; nothing
   in the oracle asserts on it outside the p = 3 fixture. Given that, does
   your answer to questions 1-3 change RR19's removal verdict? Weigh at least:
   keep as is; keep but recalibrate; replace the limb with an a-posteriori
   per-fit error estimate (RR19's own named remedy on reopening); retire the
   limb entirely, retaining refusal only for indefiniteness and exact
   singularity. If you recommend anything other than "keep as is", say what
   would have to be built and what the user-visible behaviour becomes.

6. **Anything the derivation should say and does not.** The criterion this
   block must satisfy is that it state *every premise the derivation rests
   on*. Beyond question 1's premise, list any other load-bearing assumption
   the block uses silently.

## Constraints

Flag disagreement with any of these explicitly rather than silently working
around it. None is beyond challenge, but each is on record and reversing one
takes a superseding decision entry, not an assumption.

- **D-044 (metric choice) is not reopened.** `cov2cor(Sigma-hat)` is the
  priced matrix for every user-reported quantity; the raw matrix is priced
  only at the internal `naive` arm. RR19 explicitly declined to reopen this.
  Question 1 is about the *target*, not about which matrix is measured.
- **D-048** is the entry M106 wrote; it supersedes D-044's floor only, and
  states the reopening evidence quoted in question 5.
- **The `sqrt(p * eps)` indefiniteness band** — the separate constant
  deciding `"indefinite"` versus `"ill_conditioned"` — belongs to milestone
  M90 and its own escalation. Do not recalibrate it here.
- **IP3** in `cairn/DESIGN.md` binds: read it and say if any recommendation
  you make sits against it.
- **The API requires four scales**, so the minimum reachable `df` is 4. The
  RB18 fixture is p = 3 with df = 1 and is not reachable through any exported
  call; RR19 section 3 measured that the error bound's only attainment is at
  that unreachable fixture. Treat "reachable" as model-implied matrices at
  p >= 4.
- **This is a pre-1.0-style dev line for the change in hand**: the constant
  moved on an unreleased line, so changing it again costs a NEWS edit, not a
  deprecation cycle. Do not weigh backward compatibility for the constant
  itself.

## Output format

In `RR20-axes-degeneracy-target-premise.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`. It does not, so recommendations only.
