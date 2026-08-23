# M106: Price the degeneracy refusal region on a stated statistical argument

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2, GP4
- **Branch/PR:** `m106-degeneracy-accuracy-target` / https://github.com/jmgirard/circumplex/pull/135

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

- [x] AC1 — The comment block beside `axes_degeneracy_tau` derives the largest
      relative error a reported corrected SE may carry from the SE's own
      sampling relative standard error, stating every premise the derivation
      rests on; each published result it uses is cited `citekey (p. N)`, or the
      block states in so many words that the derivation uses no published
      source. The same target is restated at `R/axes_reliability.R:720` and
      `:1030`. *(RB tripwire: no-oracle)*
- [x] AC2 — `axes_degeneracy_tau` is committed as `δ*/C` = 1e-5, with the
      accuracy target δ* = 1e-4 and the calibration ceiling C = 10 named and
      documented beside it as separate quantities, so the constant's stated
      definition and its enforced cap no longer differ by the slack factor.
- [ ] AC3 — `axes_sigma_degenerate()` returns `NULL` at 1.05× the committed
      floor and `"ill_conditioned"` at 0.95×, at p = 3, 12 and 24, and across
      three spectral forms at 0.95×: positive λmin, λmin negative but inside
      `-λmax·sqrt(p·ε)` (still `"ill_conditioned"`), and decisively negative
      (`"indefinite"`).
- [x] AC4 — Through `axes_reliability()`, three κ in [1e4, 1e7] at three
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
- [x] AC6 — Over the sites
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
- [x] AC7 — `devtools::document()` no diff, `devtools::test()` and
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
- [x] T4 — Band cases at three κ / three p through `axes_reliability()`, upper
      cases via the `axes_fitted_cov` seam; committed `data-raw/` generator
      with a seed.
- [x] T5 — The two `cormat` near-duplicate radii, same generator and seed;
      extend the `"ill_conditioned"` warning to carry κ and the dominant
      collinear pair (the smallest eigenvector's two dominant loadings).
- [x] T6 — Run AC6's sweep, update every stale site and NEWS.md, then plant one
      stale value per spelling class and confirm the sweep catches each.
- [x] T8 — Add the reachable-geometry family to `exact_oracle.R` from RR19
      §3a's stated constructions, with its own pass window.
- [x] T7 — If τ moved: superseding `DECISIONS.md` entry against D-044. Then
      `document()`, `test()`, `check()`.
- [ ] T9 — Work review return 1: the fix-dispositioned findings, the AC3
      amendment, and the ubuntu-only CI repair. Re-run `document()`, `test()`,
      `check()`.

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
- 2026-08-22: T5 done. `axes_degeneracy_hint()` computes the conditioning and, where one pair dominates the near-null direction, names it; called at both `na_out()` sites so the two surfaces' warnings stay in agreement, leaving `axes_sigma_degenerate()`'s bare-literal return untouched (Jeff's gate choice). Both `cormat` radii resolve: r = .9999 (kappa 2.87e4) computes with both failure fields NULL, r = .9999714 (kappa 1.01e5) refuses with both naming `ill_conditioned` and both warnings carrying the pair.
- 2026-08-22: three defects found and fixed inside T5, none reaching a commit. (i) `%.4f` printed r = .99999 as `1.0000`, reporting a near-duplicate pair as a perfectly collinear one — now `%.6g`. (ii) The pair printed in eigenvector-loading order (`i9 and i1`) rather than the caller's column order — now sorted. (iii) The eigenvector-mass gate alone passed a rotated planted eigenvalue whose two dominant items correlate at 0.48, so the warning asserted "nearly collinear" about a pair that is not; a second gate now requires |r| >= 0.99, and the diffuse case is pinned by test.
- 2026-08-22: two plan assumptions corrected against measurement. The planned second radius r = .99999 is unreachable through the exported path — lavaan stops converging and `axes_reliability()` errors before the criterion runs — so the bracketing radius is r = .9999714 (kappa 1.01e5), the deepest measured that both converges and refuses. And `components$SE`'s `item`/epsilon row is NA at every radius including kappa = 289, so it is not evidence about the criterion; the assertion covers the three priced components.
- 2026-08-22: T4 done. Three cases at three p, all inside [1e4, 1e7]: p = 4 at kappa 1.2e4 against a 1.06e5 floor computes; p = 8 at kappa 1.0e5 against a 7.5e4 floor refuses, a ratio of 1.33 so it discriminates where the floor sits rather than only that one exists; p = 24 at kappa 7.2e5 against a 4.33e4 floor refuses. The first two reach the criterion through real converged fits; only p = 24 needed the `axes_fitted_cov` seam, and it needed it for the measured reason AC4 anticipated — lavaan does not converge on that matrix.
- 2026-08-22: T8 done. `exact_oracle.R` gains five reachable-geometry cases — family A at p = 8 (two eps), family C at the p = 4 API minimum, and the near-duplicate geometry at r = .9999 and .99999 — each measured against the exact-rational oracle, with its own window (`REACHABLE_WINDOW = 1e-3`) asserting the OPPOSITE property to the fixture sweep's: that in reachable geometry the bound stays decades away from the error. Measured attainment 3.4e-8 to 3.3e-7, independently reproducing RR19 s3a's 8.1e-9 to 6.3e-7 on constructions written from its stated parameters. The window is live, not vacuous: tightened to 1e-9 it FAILs and the script exits 1; restored it exits 0.
- 2026-08-22: T6 done. The concept-token sweep found one genuinely stale site, `NEWS.md:65-67`, now restated with the target, the ceiling and the new warning content. It also surfaced two prose sites a value-literal grep would have missed entirely — `R/axes_scaled_fit.R:262` and `R/axes_reliability.R:2021`, both reading "the tau floor" — which carry no figure and stay true. That is the case for the concept-token form: the plan's original literal grep would have found neither, and after the edit it could no longer match `1e-6` at all.
- 2026-08-22: T6 planted-defect check, one per spelling class, each caught by the sweep and each restored: a doc site reverted to `1e-6` (numeric literal, caught at `R/axes_reliability.R:1036`); a prose clause given a stale `tau = 1e-6` (caught at `R/axes_corrected_se.R:384`); the derived threshold left at `1.4e4` (caught at `R/axes_corrected_se.R:352`). Sweep silent and tree clean afterwards.
- 2026-08-22: T7 — D-048 appended, superseding D-044's floor only; its metric choice stands and RR19 declined to reopen it. The entry carries the decision and its rationale and cites M106 and the constant's own block for the figures, rather than restating the measurements.
- 2026-08-22: T7 done. `document()` no diff and zero unresolved-link warnings; `devtools::test()` FAIL 0 / PASS 8451 / SKIP 3 / WARN 5; `devtools::check(args = "--no-manual")` Status OK, 0/0/0.
- 2026-08-22: the 5 test warnings are pre-existing, verified rather than assumed — the four `test-ci_accuracy.R` diagnostics the M7 record already names, plus a lavaan poor-marker notice at `test-ssm_sem.R:708`. That fifth one was checked out on master and reproduces byte-identical there. (A first attempt to check it used a grep pattern that does not match testthat's summary format and returned 0 warnings on master; that was an artifact of the pattern, not a measurement, and the comparison was redone.)
- 2026-08-22: **PDF-manual path unverified locally, and not silently.** This branch changed roxygen, and the repo's check command carries `--no-manual` — the exact skip that hid a CRAN-blocking LaTeX error at M7 — so `R CMD Rd2pdf` was run deliberately. It exits 1 for a missing TeX: `pdflatex` and `texi2dvi` are absent from this machine, so the LaTeX-to-PDF step cannot run here at all. What did run and pass is the Rd-to-LaTeX conversion. The added roxygen is pure ASCII and `git diff master -- man/` adds no non-ASCII line; the two in `man/axes_reliability.Rd` are pre-existing em dashes, which the M7 record measured as inputenc-safe and win-builder did not flag. The PDF build still needs a machine with TeX, at CI or the release walk.
- 2026-08-22: status in-progress→review. Every task checked; AC1–AC8 have evidence on the branch but are unticked, since ticking them is review's act against fresh evidence.
- 2026-08-22: review gate — status review→in-progress, defect return 1 of this milestone. What failed: AC5 on findings 1 and 2 (the hint fires on every degeneracy literal, printing a negative "condition number" on an indefinite refusal; and its pair-naming gates both misfire under eigenvalue multiplicity — naming an arbitrary pair among identical triplets and naming nothing on genuine duplicate pairs); AC8 on finding 3 (the oracle prices the near-duplicate cases with `FIT_ZETA1 = FALSE` while `axes_fits_zeta1()` is TRUE for that design). Findings 5–14 ride along: one further defect, six unsupported claims in comments and NEWS, two test gaps.
- 2026-08-22: amendment return: AC3 — "across three spectral forms at 0.95×" is unsatisfiable, since a negative λmin makes the ratio negative rather than 0.95× a positive floor; the box was ticked against evidence matching the test's sensible resolution rather than the criterion's words, and has been unticked.
- 2026-08-22: F17 parked as a ROADMAP candidate row at Jeff's gate choice, not absorbed into M106 — D-048 names that evidence class as its own reopening trigger, so it is settled deliberately or not at all.
- 2026-08-22: CI finding, joins the return list — `R-CMD-check` red on **ubuntu only** (run 32615474776, head 0e3c5148; matrix, windows and macos all green). `test-axes-scaled-fit.R:2209` errors "The lavaan model did not converge": AC4 case 3's CARRIER fit, `m106_family_a(1e-2, per_scale = 3L)` at κ = 721, is a stiff matrix whose convergence is platform-dependent. Its conditioning is irrelevant to what the test asserts — it exists only so the seam has a converged fit to ride on — so choosing a near-degenerate one was the defect. Repair: carry the case on `probe_octant()` (p = 24, κ = 10.45, the figure D-044 cites for the probe fits), already exercised on all three platforms across the M89/M90 suites; keep injecting `r24_bad` at the seam unchanged. Local `test()`/`check()` could not have caught this — the failure is platform-specific and only a live run shows it (the M93 lesson).

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

**PR:** https://github.com/jmgirard/circumplex/pull/135 · reviewed 2026-08-22 against master at b712a007 (unmoved since the branch was cut, so no merge was needed).

**AC1 — derivation beside the constant, restated at both roxygen sites.** `R/axes_corrected_se.R:386-424` derives the target from the reported SE's own sampling relative SD (`1/sqrt(2(n-1))`, from the chi-square anchor, halved by the delta method), holds the numerical bias to a tenth of it, calibrates at n = 5e5, and names the two n-free cross-checks. It states in so many words that no shelf citekey carries the sampling-SD result rather than citing one it does not have — the criterion's stated alternative. Restated at `R/axes_reliability.R:720-728` and `:1036-1039`.

**AC2 — the constant is the quotient.** Measured under `load_all()`: `delta_star = 1e-4`, `C = 10`, `tau = 1e-5`, and `identical(tau, delta_star/C)` is TRUE. Not a third number that happens to agree.

**AC3 — UNTICKED at the gate.** The criterion as written is unsatisfiable: it asks for three spectral forms *at 0.95x the floor*, but a negative λmin makes the ratio negative rather than 0.95x a positive floor, so forms 2 and 3 cannot sit where the wording puts them. The test resolves it sensibly (forms 2 and 3 sit at the M90 band), but that is a charitable reading of a criterion, which review may not do. Routes to a gated amendment. The underlying evidence, for the record: 13 assertions green at p = 3/12/24 across positive-spectrum both sides of the floor, roundoff-level negative, and decisively negative. Teeth shown by four mutants, each red at a different pin family and each restored by copy with the source blob re-hashed to 14cb2a23: floor's p factor dropped; delta_star loosened a decade; indefiniteness band widened 1000x; band narrowed 1000x.

**AC4 — three kappa, three p, straddling the floor.** p = 4 at kappa 1.2e4 (floor 1.06e5) computes; p = 8 at kappa 1.0e5 (floor 7.5e4, ratio 1.33) refuses; p = 24 at kappa 7.2e5 (floor 4.33e4) refuses. All three inside [1e4, 1e7], asserted in-test. The first two reach the criterion through real converged fits; only p = 24 used the `axes_fitted_cov` seam, for the measured reason the criterion allows.

**AC5 — UNTICKED at the gate** on review findings 1 and 2 (below): the diagnostic this criterion certifies is defective outside the single-pair input the criterion names. Evidence as recorded: r = .9999 (kappa 2.87e4) computes with both failure fields NULL; r = .9999714 (kappa 1.01e5) refuses with both fields `"ill_conditioned"`, two warnings, both naming the pair. All four `axes_degeneracy_hint()` branches pinned: conditioning present, pair named in column order, `%g` so .9999714 does not print as 1.0000, dimnames absent falls back to conditioning alone, and a diffuse near-null direction (dominant pair correlating 0.48) makes no collinearity claim.

**AC6 — every site carries the committed value.** The concept-token sweep returns clean. One genuinely stale site was found and fixed (`NEWS.md:65-72`); two prose sites reading "the tau floor" carry no figure and stay true. Planted-defect check, one per spelling class, each caught and restored: numeric literal at `R/axes_reliability.R:1036`, prose `tau = 1e-6` at `R/axes_corrected_se.R:384`, derived threshold at `R/axes_corrected_se.R:352`.

**AC8 — UNTICKED at the gate** on review finding 3: the near-duplicate cases are priced with `FIT_ZETA1 = FALSE`, but `axes_fits_zeta1()` is TRUE for that design, so the case M89 F3 is about is measured under a model the exported path would not fit — not "reachable geometry" as the criterion claims. Evidence as recorded: `Rscript devel/degeneracy-oracle/exact_oracle.R` exits 0 with ANCHORS PASS, SWEEP PASS and REACHABLE PASS. Five model-implied cases at p = 4, 8, 9 measure attainment 3.4e-8 to 3.3e-7, independently reproducing RR19 §3a from its stated parameters. The window is live: tightened to 1e-9 the script FAILs and exits 1.

**AC7 — toolchain verify.** `document()` no diff, zero lines matching `resolve link` at `cli.width = 500`. `devtools::test()` FAIL 0 / PASS 8451 / SKIP 3 / WARN 5, the five warnings verified byte-identical on master. `devtools::check(args = "--no-manual")` Status OK, 0/0/0.

**Consistency gate.** `cairn_validate` all checks pass, 48 advisories (47 pre-existing M7 work-log WARNs plus M106's sizing advisory, dispositioned at plan). `pkgdown::check_pkgdown()` no problems. README.Rmd not newer than README.md. No new top-level files. Master watches both green on the newest push run reaching a verdict (809e7d6a); the three `cairn/**`-only commits after it trigger no workflow by design, which is the M105 open remainder, not a new gap. `tools/check-master-red-alert.R`, `tools/master-red-alert-dryrun.R` and `tools/check-branch-protection.R` all exit 0.

**Carried forward, unverified here.** The PDF manual has not been built: this branch changed roxygen and the repo's check command carries `--no-manual`, so `R CMD Rd2pdf` was run deliberately and exits 1 because no TeX binary exists on this machine. Rd-to-LaTeX conversion passed and the diff adds no non-ASCII to `man/`. Needs a machine with TeX, at CI or the release walk.

### Review findings (three fresh-context lenses, 2026-08-22)

**[S] prior-PR-comments:** no prior-review evidence of a regression. M91's decoupling contract, M90's arm ordering and band rationale, and M89's threshold-pin discipline all verified intact; GitHub inline-comment probe returned empty, that surface correctly skipped. One out-of-scope observation (the `cval <= 0` backstop lacks the hint) — merged into [O] finding 5.

**[S] blame-history:** confirmed the recalibration does not resurrect M89's fixed bug (new floor κ ≈ 1.2e5 at p = 3 against the fixture's 6.65e6) and that D-044's metric choice is untouched in the code, not merely in D-048's claim. Its one defect finding is [O] finding 1, found independently.

**[O] diff-bug:** 19 findings. Dispositions below; every one logged, none dropped. Findings 1, 2, 3, 7, 8, 9, 10 independently reproduced by the reviewing session before triage.

- **F1 (defect, reproduced).** `axes_degeneracy_hint()` is attached to every degeneracy literal, not only `"ill_conditioned"`, so an indefinite refusal prints a negative "condition number" — measured `(indefinite: condition number -1.94e+05)`. Exceeds RR19 rec 4's stated scope and blurs the model-vs-numerics distinction M90 built. → **fix**
- **F2 (defect, reproduced).** The pair-naming gates fail in both directions under eigenvalue multiplicity. Measured: eight identical triplets at p = 24 name "i1 and i2" and advise dropping one, which is wrong advice among 24 equally-guilty items; eight genuine duplicate *pairs* — the case the feature exists for — fail gate 1 and name nothing. The comment's claim that gate 1 establishes "THIS pair drives the degeneracy" is false under multiplicity. → **fix; needs a design choice, hence a gated return**
- **F3 (AC8 gap, reproduced).** The oracle prices the near-duplicate cases with `FIT_ZETA1 = FALSE` while `axes_fits_zeta1()` is TRUE for that design (two items on one scale); re-measured with TRUE, attainment rises 5.47e-8 → 3.82e-7. Window still passes, but the case is measured off the exported path. → **fix**
- **F4.** The oracle feeds the p = 3 fixture's `DF`/`BASELINE_DF` globals to every new case; no current assertion reads `EXACT_CVAL`, so nothing is wrong today, but a future cval line there would be silently wrong. → **fix (cheap) or candidate row**
- **F5 (defect).** `R/axes_scaled_fit.R:273`'s cval backstop returns `na_out("ill_conditioned")` with no hint, so NEWS's "Where the refusal is ill-conditioning, the warning names the condition number" is false on that path. → **fix**
- **F6.** Scope mismatch across the helper comment, NEWS and D-048: all three scope the diagnostic to one literal; the code over-applies (F1) and under-applies (F5) it. → **fix with F1/F5**
- **F7 (reproduced).** `%.6g` does not fix what its comment claims: `sprintf("%.6g", 0.9999999)` is `"1"`, the same rounding failure two digits deeper, at radii reachable at large p. → **fix**
- **F8 (reproduced).** "a hundred thousand times looser" is wrong for its own subject: the floor ratio is `1/sqrt(1e-5)` = 316, not 1e5. The unit silently switched from the floor to τ while the subject stayed the floor. → **fix**
- **F9 (reproduced).** "they hand over a matrix, not items" is false — `axes_reliability(cormat=)` requires `items`, and errors without it. The same wrong claim is in NEWS. → **fix**
- **F10 (reproduced).** The "positions would mislead" rationale does not hold: both call sites realign `sigma` to `item_names` (`axes_corrected_se.R:218`, `axes_scaled_fit.R:84`) before the hint runs. Dimnames are still right; the stated reason is not. → **fix**
- **F11.** `0.8` and `0.99` decide user-facing text with no derivation and no escalation note, unlike every other constant in the file. → **fix with F2**
- **F12.** The oracle comment says "measured attainment of 1e-9 to 1e-6"; the script prints 3.41e-8 to 3.31e-7. → **fix**
- **F13.** The AC5 diffuse case discriminates gate 2 for the right reason (top-2 mass 0.966, pair entry 0.483), but the entry the comment calls a correlation is a covariance. → **fix (comment)**
- **F14.** AC4 case 2 asserts no convergence, so a non-convergence route to the same literal would pass silently. → **fix (add the assertion)**
- **F15.** AC3's literal wording is unsatisfiable (see above). → **amendment return**
- **F16.** The AC2 over-loosening guard reads a `.Rbuildignore`d fixture, so only the loosening half runs under `R CMD check`. Pre-existing pattern, but it now guards a τ that just moved. → **candidate row**
- **F17 (open question, for the maintainer).** This branch may ship half of D-048's own reopening trigger (i): AC4 case 2 is refused through a converged fit while the new oracle measures the identical construction at rel.err 3.0e-12, eight decades inside δ* = 1e-4. Only the cval half is missing, and F4 is why. → **maintainer decision**
- **F18, F19.** Confirmations, no action: AC6's sweep re-verified clean, AC2's arithmetic reproduced (δ* = 0.1/sqrt(2·(5e5−1)) = 1.0e-4; thresholds 43318.6 / 75030.0 / 106108.4 at p = 24/8/4), the `hint = NULL` threading behaviour-preserving for every non-degeneracy refusal, M71 and M89 contracts untouched, no reachable error path in the helper, no NAMESPACE/DESCRIPTION drift, no RNG use.
- 2026-08-22: T9 opened for return 1; minor plan refinement, no criteria touched. Finding 3 fixed first: the oracle's reachable cases now read `fit_zeta1` off each case's own item map with `axes_fits_zeta1()` instead of inheriting the p = 3 fixture's FALSE. The two near-duplicate cases put two items on scale 1, so they were priced under a model the exported path would not fit; re-measured, their attainment moves 5.47e-8 → 3.82e-7 and 3.41e-8 → 6.82e-8, reproducing the review's own re-measurement. Finding 12 fixed with it — the window comment claimed attainment "1e-9 to 1e-6" against a script that prints 6.8e-8 to 3.8e-7, now stated as measured and pinned to this script and date. `exact_oracle.R` exits 0, ANCHORS/SWEEP/REACHABLE all PASS.
