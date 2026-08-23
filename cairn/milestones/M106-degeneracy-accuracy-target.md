# M106: Price the degeneracy refusal region on a stated statistical argument

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2, GP4
- **Branch/PR:** `m106-degeneracy-accuracy-target`

## Goal

`axes_degeneracy_tau <- 1e-6` (`R/axes_corrected_se.R:398`) caps the relative
error a reported corrected SE may carry at ~1e-5, but that cap is calibrated
numerically only — against machine epsilon and the oracle's measured
`p·κ²·ε` bound — so nothing states what error a reported SE can actually
tolerate and no argument distinguishes τ = 1e-6 from τ = 1e-3. Give τ a stated
statistical footing, set it from that footing, and fence the κ band it governs.
**User-facing tier:** `axes_reliability()` is exported and τ decides whether a
user is handed numbers or NA.

## Scope

**In:** A stated derivation of the largest relative error a reported corrected
SE may carry, priced against the SE's own sampling variability rather than
machine epsilon alone, written beside the constant and restated at the two
exported roxygen sites. τ set from that derivation. Tests across
κ ∈ [1.4e4, 1.4e7] — the band M89's tightening newly refused — straddling the
committed threshold, with p varied. The `cormat` near-duplicate path as a named
case. A superseding `DECISIONS.md` entry if τ moves (D-044 states τ = 1e-6 and
its thresholds in its own text; history is superseded, never edited).

The measured motivation (M89 round-3 review, F3 at 68): an item pair at
r = .9999 fits cleanly, reaches κ(cov2cor(Σ̂)) = 3.3e4, and is refused, though
its error bound is ~5.8e-6. Over the two recorded exemplars, τ ∈ (6e-6, 3e-2)
— less the oracle's factor-of-10 slack — computes the near-duplicate while
still refusing RR18's 3.4%-wrong exemplar (κ = 6.65e6, p = 3). τ = 1e-6 sits
below that window; the derivation, not the window, decides where τ lands, and
τ may legitimately stay where it is.

**Out:** Which matrix is priced (`cov2cor(Σ̂)` vs raw) — settled by D-044, not
reopened. The separate `sqrt(p·ε)` indefinite/`ill_conditioned` band constant →
stays M90's, its own escalation. Making τ user-settable → candidate row if the
derivation shows one target cannot serve every design. Release sequencing →
M7's own gate.

## Acceptance criteria

- [ ] AC1 — The comment block beside `axes_degeneracy_tau` derives the largest
      relative error a reported corrected SE may carry from the SE's own
      sampling relative standard error, stating every premise the derivation
      rests on; each published result it uses is cited `citekey (p. N)`, or the
      block states in so many words that the derivation uses no published
      source. The same target is restated at `R/axes_reliability.R:720` and
      `:1030`. *(RB tripwire: no-oracle)*
- [ ] AC2 — `axes_degeneracy_tau` is committed as `δ*/C` = 1e-5, with the
      accuracy target δ* = 1e-4 and the calibration ceiling C = 10 named and
      documented beside it as separate quantities, so the constant's stated
      definition and its enforced cap no longer differ by the slack factor.
- [ ] AC3 — `axes_sigma_degenerate()` returns `NULL` at 1.05× the committed
      floor and `"ill_conditioned"` at 0.95×, at p = 3, 12 and 24, and across
      three spectral forms at 0.95×: positive λmin, λmin negative but inside
      `-λmax·sqrt(p·ε)` (still `"ill_conditioned"`), and decisively negative
      (`"indefinite"`).
- [ ] AC4 — Through `axes_reliability()`, three κ in [1e4, 1e7] at three
      different p, straddling the committed threshold (4.3e4 at p = 24):
      one strictly below the committed threshold returns numbers,
      one strictly above returns NA with its reason named, one within a factor
      of 2 of the threshold resolves as AC1's derivation implies. The upper
      cases reach the band through the `axes_fitted_cov` seam, since no
      converged fit is known to (`R/axes_corrected_se.R:444-449`).
- [ ] AC5 — `axes_reliability(cormat = ...)` on an item set carrying one pair at
      r = .9999 returns the outcome AC1's derivation implies, with
      `details$se_correction_failed` and `details$fit_scaling_failed` each
      asserted to the value the derivation states for this input; a second
      near-duplicate radius brackets the committed threshold from the other
      side — and where the refusal does fire, its `"ill_conditioned"` warning
      names κ and the dominant collinear item pair, not a bare reason code.
- [ ] AC6 — Over the sites
      `grep -rniE 'tau|accuracy target|double\.eps|kappa' R/ tests/ NEWS.md`
      returns, plus the two comment blocks read whole
      (`R/axes_corrected_se.R:336-398`, `R/axes_scaled_fit.R:245-270`), every
      site stating the criterion's numeric claim carries the committed value;
      one stale value planted per spelling class — numeric literal, prose "tau
      floor", derived κ threshold — is caught by that sweep. `cairn/` is
      excluded deliberately: D-044 is superseded, never edited.
- [ ] AC8 — `devel/degeneracy-oracle/` gains a reachable-geometry family
      (model-implied, p ≥ 4, including one near-duplicate construction from
      RR19 §3a) with its own pass window asserting attainment stays at least
      three decades below 1; `Rscript devel/degeneracy-oracle/exact_oracle.R`
      passes both that window and the existing fixture window.
- [ ] AC7 — `devtools::document()` no diff, `devtools::test()` and
      `devtools::check()` clean (0/0/0), per `cairn/PROFILE.md`.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC8 → T8
- AC7 → T7

## Tasks

- [x] T1 — Escalate the accuracy target via `/milestone-brief` (RB): what
      relative error may a reported corrected SE carry, priced against its own
      sampling variability? Ingest the RR. *(RB tripwire: no-oracle)*
- [x] T2 — Write the derivation beside `axes_degeneracy_tau`; set the constant;
      restate the target at both roxygen sites.
- [x] T3 — Threshold pins at p = 3/12/24 × three spectral forms; mutate
      `axes_degeneracy_tau` and record each pin's observed failure.
- [ ] T4 — Band cases at three κ / three p through `axes_reliability()`, upper
      cases via the `axes_fitted_cov` seam; committed `data-raw/` generator
      with a seed.
- [ ] T5 — The two `cormat` near-duplicate radii, same generator and seed;
      extend the `"ill_conditioned"` warning to carry κ and the dominant
      collinear pair (the smallest eigenvector's two dominant loadings).
- [ ] T6 — Run AC6's sweep, update every stale site and NEWS.md, then plant one
      stale value per spelling class and confirm the sweep catches each.
- [ ] T8 — Add the reachable-geometry family to `exact_oracle.R` from RR19
      §3a's stated constructions, with its own pass window.
- [ ] T7 — If τ moved: superseding `DECISIONS.md` entry against D-044. Then
      `document()`, `test()`, `check()`.

## Work log

- 2026-08-22: created by /milestone-plan. Full criteria audit (user-facing tier) ran on a fresh-context [O] reader and returned 12 findings across 6 criteria; 11 fixed before writing, the AC4 seam question fixed on the code's own recorded ground. Audit also corrected a draft mis-citation of IP4 (the RNG contract, not the never-edit-history convention).
- 2026-08-22: plan gate chose loosening-is-live over assuming τ is already right because the two recorded exemplars leave a ~3-decade τ window in which the near-duplicate computes and RR18's exemplar still refuses; falsified by a derivation whose target lands at or below 6e-6.
- 2026-08-22: plan gate chose deriving τ from the SE's own sampling variability over keeping τ and rewording the refusal, because a refusal message cannot make a defensible analysis computable (GP2); falsified by evidence that every κ in the reopened band is a design defect rather than a usable fit.
- 2026-08-22: plan gate chose pre-build Fable escalation over letting implement offer it, at Jeff's choice, so the constant and its tests are built against a reviewed target; falsified by the RR returning no argument the session could not have reached.
- 2026-08-22: status planned→blocked on RB19 (T1); the accuracy target the constant and its tests are built against comes from that review.
- 2026-08-22: RR19 returned and ingested; status blocked→planned. Oracle re-run reproduced both anchors and the Q4 sweep before any new measurement (ANCHORS PASS, SWEEP PASS).
- 2026-08-22: AC/task amendment after the RR19 ingest, at Jeff's gate choice. AC2 restated as δ*/C with both quantities named (the review found the old wording's definition and enforced cap differed by the slack factor); AC4's literal band re-centred on the new 4.3e4 threshold; AC5 absorbed RR19 rec 4 (the warning names κ and the collinear pair) rather than taking a ninth criterion; AC8/T8 added for rec 5's reachable-geometry oracle family.
- 2026-08-22: gate chose folding rec 4 into AC5 over a separate criterion because both test the same input path at the same geometry, and a ninth criterion would trip the sizing tripwire into a split the work does not need; falsified by the warning work turning out to need its own fixture family.
- 2026-08-22: `cairn_validate` sizing advisory notes 8 acceptance criteria against the >7 tripwire. Not split: AC7 is the profile's mandated verify criterion rather than scope, so the substantive count is 7, and separating AC8's oracle family would ship the recalibration without the reachable-geometry evidence that justifies its calibration ceiling. Tasks are 8 of a 10 tripwire.
- 2026-08-22: status planned→in-progress on branch `m106-degeneracy-accuracy-target`, cut from master at b712a007.
- 2026-08-22: T1 done — discharged by RR19 before implementation started, so its no-oracle tripwire is closed rather than open.
- 2026-08-22: T2 done. `axes_degeneracy_tau` is now `axes_degeneracy_delta_star / axes_degeneracy_calibration_ceiling` = 1e-4/10 = 1e-5, with the derivation and the reachable-geometry finding written beside it; both exported roxygen sites restate the target and the ceiling. The regression test went in first and failed for the right reason — `"ill_conditioned"` on RR19's family-B geometry at r = .9999, whose SEs the review measured accurate to 2.0e-13. Two M89 tests hard-coded the old literal and were re-pinned; the near-threshold probe keeps its floor literal and asserts it against the constant separately, so a future edit fails the probe rather than being tracked by it. `document()` no diff beyond the regenerated Rd, zero unresolved-link warnings. test-axes-scaled-fit.R, -corrected-se.R, -reliability.R and -fiml.R all pass.
- 2026-08-22: minor plan refinement — T4/T5's "committed data-raw generator with a seed" is unnecessary: RR19's families are closed-form model-implied matrices with no RNG, so they are built deterministically in `tests/testthat/helper-m106-degeneracy.R` rather than committed as fixtures. No committed fixture means nothing whose bit-exactness is unfalsifiable on the authoring machine.
- 2026-08-22: helper defect found and fixed before it reached a commit — the family-B builder appended a hard-coded 360 as the duplicate angle, but `octants()` starts at 90, so item 9 paired with scale 7 while the driven-down errors sat on items 1 and 9 (r = 0.57, not 0.9999). It now duplicates `octants()[1]` whatever that is.
- 2026-08-22: T3 done. Pins at p = 3/12/24 across three spectral forms (positive both sides of the floor; lambda_min negative inside the noise band; decisively negative), 13 assertions. Mutation-proved with four mutants, each red at a different pin family and restored by copy with the source blob re-hashed to 14cb2a23 after every one: dropping the floor's p factor reddens the 0.95x positive pins at all three p; loosening delta_star a decade reddens the constant assertion and those pins; widening the indefiniteness band 1000x reddens the decisively-negative pins; narrowing it 1000x reddens the roundoff-level pins. The last two exercise the forms M89's probe never reached — it varied only p and which side of the floor a positive spectrum sat on.

## Decisions

- 2026-08-22 (RR19 §1): the accuracy target is **δ\* = 1e-4** — the largest relative error a reported corrected SE may carry. Derived from the SE's own sampling relative SD ≈ `1/sqrt(2(n-1))` with the numerical bias held to one tenth of it, calibrated at n = 5e5 (a decade past any published circumplex sample; small n only loosens the channel). Cross-checked against the 3-decimal print resolution (≥1e-3 relative) and Wald coverage sensitivity (0.23 per unit relative error). RR19 states that no citekey on the repo's shelf carries the sampling-SD result and declines to manufacture one.
- 2026-08-22 (RR19 §2): **τ = δ\*/C = 1e-5**, with C = 10 the oracle's calibration ceiling kept as measured. Refusal threshold κ moves 1.37e4 → 4.3e4 at p = 24 (7.5e4 at p = 8, 1.06e5 at p = 4). Both recorded exemplars then land correctly: the r = .9999 near-duplicate computes, counterexample B still refuses. Doc defect to repair with it: τ is documented as the largest tolerated reported error but enforced as ~10τ — restate as δ\* and C, two documented numbers behind one shipped constant. Applied as T2; the superseding D-entry against D-044 is T7.
- 2026-08-22 (RR19 §3): new oracle measurement over reachable geometries (p = 4, 8, 9, model-implied unit-diagonal, N = 600, including the near-duplicate pair) — actual SE relative error sits **5–8 decades below** `p·κ²·ε`, attainment ratios ≤ 4e-6. The bound's only measured attainment is the RR18 fixture, which is outside the production input space twice over: p = 3 with df = 1 (the API requires four scales, minimum reachable df = 4) and off the model manifold by 25 units at its own configuration. The real driver is coupling of near-null directions into the component rows, indexed by df (sign flip at df = 1, 1.1e-8 at df = 4, 1.1e-13 at df = 26), which κ does not price. No measured regime shows the model optimistic beyond the fixture's 3.3×. **D-044 is not reopened** — `cov2cor(Σ̂)` stays the priced matrix; what is recalibrated is the bound's sharpness inside that metric.
- 2026-08-22 (RR19 §4): **one n-free τ, shared by both surfaces.** An n-dependent τ would make refusal a property of the yardstick rather than the refused matrix — the same matrix computing at n = 200 and refusing at n = 20,000, gameable on the `cormat` path where n is user-typed — and buys only n^(1/4) of threshold movement. A df-dependent constant would split D-044's one-criterion contract.
- 2026-08-22 (RR19 §5): **the ill-conditioning limb is kept; removal rejected, narrowly.** Measured accuracy in the reachable set is not a bound; IP3 cannot be satisfied past the floor (at the one refused point where truth is known the value is measurably wrong); and the only a-priori error estimate a replacement caution could carry overstates the actual error by 5–8 decades, so it would cry "up to 3% error" over numbers accurate to 1e-13. The GP2 violation in the near-duplicate case was τ's miscalibration, not the mechanism. Reopening evidence, for the T7 D-entry: (i) a converged-fit Σ̂ the recalibrated criterion refuses while an exact oracle shows its SEs and `cval` within δ\*; (ii) field reports of `"ill_conditioned"` on real data. On either, the remedy is an a-posteriori per-fit estimator, not another decade on τ.
- 2026-08-22 (RR19 §6): the r = .9999 case is a **defensible analysis over almost-certainly-defective data** — statistically well-defined, model identified, fit converged, and its computed SEs measured accurate to 2.0e-13. At τ = 1e-5 it computes; the refusal onset moves to about r ≈ .99992 at p = 24. Where the criterion does fire on the `cormat` path, whole-fit refusal stays right (the corrupted object is the one shared information matrix) but the warning should name κ and the dominant collinear pair.
- 2026-08-22 (RR19 ingest triage): **apply** — recs 1 (τ = 1e-5 + δ\*/C restatement + NEWS + regression tests → T2/T6/T7), 2 (keep the limb, reopening evidence in the D-entry → T7), 3 (one n-free constant, ruling recorded → this section). **Reject, per RR19's own reasoning** — recs 6 (removal) and 7 (n-dependent or per-surface τ). **Consider, put to the maintainer** — recs 4 (warning carries κ and the collinear pair) and 5 (reachable-geometry family in the oracle driver, its own pass window; RR19 B2 calls the present window a property of the fixture, not the criterion). **Deferred, evidence-gated** — rec 8 (a-posteriori per-fit estimator), promoted only on §5's reopening evidence.
- 2026-08-22 (RR19 B3, recorded not scheduled): df is the exposure axis, and the four-scale minimum is what keeps the reachable set out of the fixture's regime. Any future change lowering the minimum scale count must re-run the exact oracle at the new minimum before shipping.

## Review
