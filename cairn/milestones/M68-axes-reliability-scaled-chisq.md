# M68: Scaled global test statistic for `axes_reliability()`

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR14
- **Principles touched:** —
- **Branch/PR:** `m68-axes-reliability-scaled-chisq` / https://github.com/jmgirard/circumplex/pull/94

## Goal

Report a correlation-metric-calibrated global test statistic for
`axes_reliability()` in place of the normal-theory values that flatter fit by
roughly 4%, using a Satorra–Bentler-type scaling factor built from the same Γ
machinery M66 built for the component standard errors.

## Scope

**In:** `R/axes_scaled_fit.R` computing `c = tr(U Γ_R)/df` at the fitted Σ̂ and
the independence model's own `c_b`; `$fit$chisq`, `$pvalue`, `$rmsea` and
`$cfi` become the scaled values on all three input paths (raw-listwise,
`cormat`, `missing = "fiml"`), with lavaan's unscaled six in
`details$fit_uncorrected` and the factors in `details$scaling_factor`; source
notes for `satorra1994` and `cudeck1989`; every caveat surface rewritten. The
full scope statement is D-036 and is not restated here.

**Out:** a scaled *difference* test — no such comparison exists in this API
(candidate row); SRMR and `$fit$df`, neither a test statistic with a reference
distribution to recalibrate; `ssm_sem()`, which lives on the covariance metric
and is unimplicated (D-035); a Swain/Bartlett-type small-sample mean correction
to `T`, which RR14 shows would close the residual small-N gap but which is
future work, not M68 (candidate row).

## Acceptance criteria

AC1-AC6 are plan-owned; AC7-AC14 are RR14's binding criteria BC1-BC8, ingested
verbatim and mechanically diffed against `cairn/reviews/RR14-*.md`, with no
departures and so no "Deviations from RR14" table. The plan's original AC3
(a rejection-rate band at N = 600) is **superseded** by AC7-AC9 and AC14 —
RR14 Q2/Q5 showed it demanded what no scaling factor can deliver — and the
remaining plan-owned criteria are renumbered contiguously, so AC4-AC7 of the
original plan are AC3-AC6 here.

- [x] **AC1** — All three input paths compute `$fit$chisq`, `$pvalue`, `$rmsea`
      and `$cfi` from `T_s = T / c` (`$cfi` also using `c_b`); `$fit$df` and
      `$fit$srmr` stay bit-identical; `details$fit_uncorrected` and
      `details$scaling_factor` present on every path; no path returns a scaled
      statistic beside an unscaled one among the four, verified by a test
      reading all four from each path.
- [x] **AC2** — Two independent oracle types. *Closed-form:* an explicit
      vech-space routine forming `Γ_R`, `W`, `Δ` and `U` as literal matrices,
      agreeing with the shipped trace-identity implementation to ≤ 1e-8 relative
      on the octant probe, a 6-scale map and a one-item-per-scale map, citing
      `satorra1994 (p. N)`, and carrying AC13's entrywise `Γ_R` check.
      *Simulation-coverage:* AC7-AC9. RR13's `E[T] = 261.1` against `df = 273`
      corroborates to ≤ 0.5 but does not gate — RR13 ships no reproduction code,
      so a miss escalates.
- [x] **AC3** — On the FIML path, regenerating from the M65 fixture's 2/5/10 %
      MCAR seeds and the M66 fixture's 201-replicate M1 MAR cell,
      `mean(T_s)/df` ∈ [0.95, 1.05] in every cell; and the `em_stalled` refusal
      still fires before any scaled statistic is computed.
- [x] **AC4** — No user-facing surface still says the global fit statistics are
      uncorrected: `grep -rn` over `R/`, `man/`, `vignettes/`, `NEWS.md` and
      `tests/testthat/` for `flattered`, `not corrected`, `261.1`, `approximate`,
      every hit dispositioned in the work log as (a) updated, (b) historical
      inside a released-version NEWS entry, or (c) unrelated and listed. Re-run
      after AC11's edits.
- [x] **AC5** — `satorra1994.md` and `cudeck1989.md` exist with provenance
      blocks and page anchors, both carry `INDEX.md` lines, and
      `R/axes_scaled_fit.R` / `R/axes_corrected_se.R` cite them as
      `citekey (p. N)`.
- [x] **AC6** — `devtools::document()` no diff; `devtools::test()` and
      `devtools::check()` clean (0 errors, 0 warnings; NOTEs justified).
- [x] **AC7** (BC1): At each of the three AC3 populations (strong-axes, Strack COC S16
      Other weak-axes, anti-conservative corner), N = 600, ≥ 2000 replicates
      produced by the seed-pinned generator `devel/m68-scaled-fit-cells.R` with
      its per-replicate summary committed at
      `tests/testthat/fixtures/m68-scaled-fit-cells.rds`:
      `mean(T_s)/df ∈ [0.97, 1.03]`. (Measured: 1.0204 / 1.0139 / 1.0227.)
- [x] **AC8** (BC2): At the strong-axes population, N = 4800, ≥ 2000 replicates from
      the same seed-pinned generator, stored in the same committed fixture:
      empirical rejection rate of `$fit$pvalue` at α = .05 within `[.036, .064]`
      (nominal ± 2.8 MC SE at 2000 replicates). (Measured: .0540 ± .0051;
      independent 3000-replicate run .0500 ± .0040.)
- [x] **AC9** (BC3): At each of the three populations, N = 600, the scaled and
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
- [x] **AC10** (BC4): From the committed fixture's per-replicate `chisq` and `cfactor`
      columns, at each N = 600 cell: |rej(T/ĉ) − rej(T/c_pop)| ≤ .005 at
      α = .05, recording that the tail excess is not factor-estimation noise.
      `c_pop` for each population is the fixture's own
      `population_diagnostics$*$cfactor`. (Measured: ≤ .0005 in every cell;
      relative sd(ĉ) ≤ .0024.)
- [x] **AC11** (BC5): Three user-facing surfaces carry the small-sample behaviour, at
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
- [x] **AC12** (BC6): No user-facing surface describes the scaling as a robustness
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
- [x] **AC13** (BC7): The AC2 vech oracle gains an independent off-diagonal check on
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
- [x] **AC14** (BC8): The regression evidence stands in the suite, not only in the work
      log: a test file reads the committed fixture and asserts, from its stored
      per-replicate columns, BC1's three means, BC2's rejection rate, BC3's six
      rates against their fences, and BC4's ≤ .005 bound; and a fast live smoke
      cell (following M65's harness pattern, ≤ ~20 replicates at one population)
      runs the generator's replicate function end-to-end so a regression in the
      wiring is caught without the 5-minute full run. Checkable:
      `grep -rn m68-scaled-fit-cells tests/` is non-empty and the named
      assertions are present.

## Coverage

- AC1 → T3, T4, T14 · AC2 → T2, T9 · AC3 → T4, T7 · AC4 → T5, T12
- AC5 → T1, T15 · AC6 → T8 · AC7 → T6 · AC8 → T6 · AC9 → T6, T10, T14
- AC10 → T10 · AC11 → T11, T13 · AC12 → T12 · AC13 → T9 · AC14 → T10, T14

## Tasks

Completed tasks are compressed to one line each; the work log carries their
detail.

- [x] **T1** — Source notes `satorra1994.md` and `cudeck1989.md` + `INDEX.md` lines.
- [x] **T2** — `R/axes_scaled_fit.R`: `axes_scaling_factor()` and
      `axes_scale_fit_measures()`, test-first, with AC2's vech-space oracle.
- [x] **T3** — Wire listwise + `cormat`; unscaled six to `details$fit_uncorrected`;
      fit-measure membership guard extended.
- [x] **T4** — FIML path, on the same complete-data factor (M68-D1), with the
      `em_stalled` refusal kept strictly ahead of any scaling.
- [x] **T5** — Caveat surfaces rewritten; AC5's sweep run and dispositioned.
- [x] **T6** — Complete-data simulation: seed-pinned `devel/` generator, three
      populations plus the N-sweep, committed `.rds` summary.
- [x] **T7** — FIML simulation cells: regenerate from the M65 fixture's
      2/5/10 % MCAR seeds and the M66 fixture's M1 MAR seeds, store the T_s
      summaries beside AC7's.
- [x] **T9** — AC13: extend the vech oracle's `Γ_R` check from the diagonal to
      every cell, against the closed normal-theory covariance formula written
      out in the test.
- [x] **T10** — AC10/AC14: a suite test reading the committed fixture and
      asserting AC7's means, AC8's rate, AC9's six rates and AC10's bound,
      plus the fast live smoke cell (M65 pattern).
- [x] **T11** — AC11: the small-sample behaviour on three surfaces — roxygen
      Details, the vignette's scaled-fit section, and one direction-and-pointer
      sentence in `axes_fit_scaled_note`.
- [x] **T12** — AC12: the six-token robustness sweep with AC5's three-way
      disposition, plus the normal-theory fencing sentence in the roxygen.
- [x] **T8** — `document()`, `test()`, `check()`; NEWS entry.
- [x] **T13** — Review F1/F2/F16: the metric note claims only the corrections that
      happened; the scaled-fit note moves beside `summary()`'s fit line; CFI is 1, not `NaN`, at 0/0.
- [x] **T14** — Review F4/F6/F7: `$fit$cfi` recomputed on every path; the harness moves to
      `tests/testthat/helper-m68-cells.R`; AC9's exact-reproduction arm in the suite + generator `verify` mode.
- [x] **T15** — Review F5: `R/axes_corrected_se.R` anchors `cudeck1989 (p. 323)`.

## Work log

- 2026-08-02: created by /milestone-plan. Promoted from the ROADMAP candidate "Satorra–Bentler-style scaled test statistic for the axes-reliability χ²" (RR13 B-1), which M66 left explicitly uncorrected. Supersedes D-035's holding that the fit indices "keep their caveat" — see D-036.
- 2026-08-02: criteria audit ([O], fresh context) returned 13 findings; 5 clear-fixes applied before the gate — AC1's srmr/df clause contradicted its own no-mixture clause; AC3's [.035,.065] band at 500 reps was ±1.55 MC SE (a calibrated statistic fails ~12% of the time), raised to 2000 reps and the RR13 Q5 ±2.8 MC SE band; AC4 conflated the M65 fixture (no χ² stored, 5-rep M1 cell) with M66's 201-rep M1 cell; AC5's grep over-caught unrelated uses (`evaluating-circumplex-structure.Rmd:93`, `ssm_analysis.R:111`), so a third disposition was added; AC2's 261.1 was demoted from gate to corroboration because RR13 ships no reproduction code for it. Four judgment findings went to the question gate.
- 2026-08-02: plan gate chose scaling all three paths over shipping listwise+`cormat` first with FIML as a dependent milestone, because a path-dependent `$fit$chisq` is the exact trap the M65 SRMR fix cured (`R/axes_reliability.R:1635-1653`); falsified by the FIML calibration of AC4 missing its band under both candidate `Γ_R` constructions.
- 2026-08-02: plan gate chose scaling CFI via the independence model's own `c_b` over leaving CFI uncorrected, because `summary()` prints χ², RMSEA and CFI on one line (`R/axes_reliability_oop.R:257-262`) and RR13 B-2 named mixed-calibration comparison as the harm on the SE side; falsified by `c_b` proving unidentified or unstable on any accepted input.
- 2026-08-02: implementation question gate — both recommendations accepted; recorded as M68-D1 (FIML uses the complete-data `Γ_R` at Σ̂ on M66's multiplicative-composition precedent; a failed factor NAs the four statistics with a stored reason rather than falling back to unscaled). The T4 tripwire is settled here, not escalated.
- 2026-08-02: T1 done — `satorra1994.md` and `cudeck1989.md` authored from the source-note template with INDEX lines. Both PDFs are Paper Capture OCR scans; satorra1994's text layer drops eqs. 16.21/16.22 entirely (the M42-D1 trap), so both were read from rendered page images. Anchors banked: U (eq. 16.18, p. 406), T̄ = c⁻¹T and c = trace{UΓ/r} (eqs. 16.21/16.22, p. 407), the any-moments licensing sentence (p. 401), Cudeck's scale-invariance definition (p. 319) and Table 4's 48% SE discrepancy (p. 323). Two claims the notes fence rather than assert: the axes model's non-scale-invariance is derived in-repo (Cudeck never treats a circumplex), and Cudeck's Error (b) is a different error from the one M68 corrects.
- 2026-08-02: T2 done — `R/axes_scaled_fit.R` ships `axes_scaling_factor()` (satorra1994 eqs. 16.18/16.21/16.22) and `axes_scale_fit_measures()`. The trace is evaluated through p x p identities, never a p* x p* matrix: `tr(V Gamma_R) = sum_{k<l}[1 - (Sigma^-1)_kl rho_kl (1 - rho_kl^2)]`, derived twice (two derivations reconcile iff `tr(Sigma^-1 Sigma) = p`), and the baseline factor collapses to `mean((1 - rho^2)^2)` because the independence model's free parameters are the variances, whose sample correlations do not vary. AC2's literal vech-space oracle agrees to 1e-15 on all three maps; 1/c = 1.0457 against RR13's measured 273/261.1 = 1.0456. Recorded as M68-D2: pricing at `cov2cor(Sigma-hat)` rather than lavaan's raw Sigma-hat, plus a ROADMAP candidate for the same assumption in `axes_corrected_se()`. 463 assertions.
- 2026-08-02: T3+T4 done in one edit — M68-D1 makes the FIML path use the same complete-data factor, so the three input paths share one wiring at `R/axes_reliability.R:1674-1723`. `want` gains `baseline.chisq`/`baseline.df`/`ntotal` (fed to the scaler, never reported) and they pass the same membership guard as the reported six. `details` gains `fit_uncorrected`, `scaling_factor` and `fit_scaling_failed`. The `em_stalled` refusal already sat far above the scaling; T4's ordering test makes that falsifiable by stubbing `axes_scaling_factor()` to `stop()` and asserting the EM message wins, so it discriminates ORDER rather than re-asserting that the refusal fires.
- 2026-08-02: T5 done — AC5's sweep (`grep -rn` for `flattered`, `not corrected`, `261.1`, `approximate` over R/, man/, vignettes/, NEWS.md, tests/testthat/) dispositioned in full. (a) UPDATED: roxygen `R/axes_reliability.R` (the corrected-contract paragraph, the `@return` fields, the FIML section's new factor-choice paragraph, a Satorra-Bentler `@references` entry); `R/axes_reliability_oop.R`'s printed note, split into three pieces because the two corrections can now fail independently and a note asserting a correction that did not happen is worse than none; `vignettes/axes-reliability.Rmd` section 5 + References; `NEWS.md` (the M66 entry's trailing not-corrected clause removed, a new entry added); and the five guard blocks in `tests/testthat/test-axes-corrected-se.R`, each falsified claim moved to an absence with a paired positive so it cannot be satisfied by deletion. (b) HISTORICAL-IN-NEWS: none — every NEWS hit sits inside the unreleased 2.0.0 section, so no released version ever carried the caveat, which is what D-036 shipping alongside M66 was for. (c) UNRELATED, listed and untouched: `R/fit_structure.R:345,721`, `R/ssm_analysis.R:111`, `R/ssm_oop.R:121`, `R/axes_corrected_se.R:17`, the quasi-circumplex "refused rather than approximated" trio (`R/axes_reliability.R:736`, `man/axes_reliability.Rd:198`, `vignettes/axes-reliability.Rmd:291`), `NEWS.md:190,262`, `vignettes/growth-ssm-analysis.Rmd:307`, `vignettes/evaluating-circumplex-structure.Rmd:93,126,184,229,449`, `tests/testthat/test-axes-reliability.R:506,881`, `man/fit_structure.Rd:71`, `man/ssm_analyze.Rd:140,155`.
- 2026-08-02: suite after T5 — 0 failures, 4946 passing (baseline before M68 was 4421); the 4 pre-existing warnings are unchanged.
- 2026-08-02: T6 harness written (`devel/m68-scaled-fit-cells.R`, seed-pinned, committed summary `tests/testthat/fixtures/m68-scaled-fit-cells.rds`, 2000 replicates per cell) and RUN, but T6 is NOT complete: AC3's rejection-rate clause is not satisfiable as written and is under escalation, so the suite's smoke assertions are held until the disposition is settled.
- 2026-08-02: AC3 finding — the mean criterion passes at all three populations (`mean(T_s)/df` = 1.0204 / 1.0139 / 1.0227, band [0.97, 1.03]); the rejection-rate criterion does not (.0790 / .0630 / .1070, band [.036, .064]). The Satterthwaite-adjusted statistic does not fix it either (.0740 / .0590 / .1030), so eigenvalue dispersion is not the cause. The sample-size sweep at the strong-axes population isolates it: as N runs 600 → 1200 → 2400 → 4800, `mean(T)/df` falls monotonically to `c_pop` = 0.9563 (0.9755, 0.9695, 0.9623, 0.9579), `mean(T_s)/df` → 1.0016, the sd ratio → 0.9974, and the scaled rejection rate → .0540 (inside the band) — while the factor itself is a function of the population matrix and does not move with N. So the residual is the ML chi-square's own finite-sample upward bias, not an error in the factor, which AC2's closed-form oracle pins to 1e-15 independently. The asymmetry that matters for the ship decision: the UNSCALED rate moves AWAY from nominal as N grows (.0260 → .0145) while the scaled one moves toward it.
- 2026-08-02: escalation — the implementation gate offered (a) amend AC3 to gate the rejection rate only where `p*/N` is small and record the small-N rates, (b) a Fable-level second opinion, or (c) park the milestone. The maintainer chose (b). No criterion was amended; AC3 stands as written and the milestone stays `in-progress` pending the review. Routing to `/milestone-brief`.
- 2026-08-02: blocked on RB14 — AC3's rejection-rate criterion escalated for independent review; the brief asks five questions, three of them attacks on the implementing session's own reading (the derivation, the finite-sample-bias attribution, and the population-matrix shortcut in the adjusted-statistic comparison).
- 2026-08-02: RR14 returned and its binding criteria were audited before ingestion ([O], fresh context, per the ingest-audit rule). The substance passed — Q1 confirmed the derivation through a fully independent Olkin-Siotani route at 1e-15, Q2 established the finite-sample-bias attribution by direct decomposition (replacing per-fit c-hat with c_pop moves the rejection rate by <= .0005, ruling out factor noise; oracle mean-recentering alone restores .048-.058 in every cell), and BC1-BC7 are jointly satisfiable and violate no frozen scope or standing decision. The criteria as WRITTEN did not pass: the audit found AC3's `fast live smoke cell in the suite` clause dropped with nothing replacing it (no test reads the fixture today, so BC1-BC4 could be satisfied in narrative alone), BC5 mandating two overclaims into user-facing prose and omitting the `summary()` printed note where the harm actually occurs, BC6 making a universal claim over a domain its `grep -rin robust` does not enumerate, BC3 ambiguous on `recorded in the committed fixture` and fencing a seed-pinned regeneration with a Monte-Carlo tolerance, and BC7 mandating a published formula with no shelved source against the primary-sources hard stop. Sent back to the reviewer for revision at the maintainer's direction rather than ingested verbatim with a Deviations table.
- 2026-08-02: RR14 revised at the maintainer's direction rather than ingested with a Deviations table — the reviewer closed all ten audit findings in place and added BC8, restoring the `fast live smoke cell in the suite` clause the criteria had dropped. Ingested verbatim as AC7-AC14; `cairn_validate`'s `binding criteria` string-compare PASSES, so there are no departures and no Deviations table.
- 2026-08-02: amendment return: AC3 — superseded outright by AC7-AC9 and AC14; the plan's remaining criteria renumbered contiguously (original AC4-AC7 are now AC3-AC6) because `coverage complete` counts AC checkboxes positionally and a gap reds the check.
- 2026-08-02: RB14/RR14 format note — the RR's binding-criteria bullets were rewritten from `- **BCn** — ` to `- BCn: ` (text byte-identical) because `cairn_validate`'s `binding criteria` check parses only the latter and otherwise reported it would `silently bind nothing`.
- 2026-08-02: weight-cap overrun accepted at the maintainer's direction (200 plan-owned lines against a <150 cap, and both sizing tripwires tripped at 14 criteria / 12 tasks). A compression pass ran first — Scope cross-referenced to D-036, completed tasks reduced to one line each, the six plan-owned criteria rewritten in a single pass — taking the body from 221 to 200; the remaining 51 lines are RR14's verbatim criteria, which may not be edited. The alternative on offer was splitting the milestone, which either ships the scaled statistic without its small-sample documentation or without its regression harness; the maintainer chose the logged overrun over shipping a half-validated correction. `weight caps` will FAIL again at the review gate and is expected to.
- 2026-08-02: T9 done — the vech oracle's `Γ_R` check extended from the diagonal to every cell against the closed normal-theory covariance formula written out in the test (RR14 finding 1: the off-diagonal cells carry most of `tr{U Γ_R}` and were pinned only by the two delta-method routes agreeing). Agreement 1e-12 on the octant probe and on a non-model random correlation matrix, with a perturbation check so the comparison is capable of failing.
- 2026-08-02: T10 done — suite regression over the committed fixture: AC7's three means (each paired with the negative that the unscaled ratio is outside the same band), AC8's nominal rejection rate at N = 4800 (paired with the unscaled .0145 at the same N), AC9's six fenced rates, AC10's factor-noise decomposition, plus a 12-replicate live smoke cell asserting direction rather than calibration. `grep -rn m68-scaled-fit-cells tests/` is now non-empty. 540 assertions.
- 2026-08-02: T11 done (AC11) — the small-sample behaviour documented on three surfaces at two depths: the roxygen gains a `How well calibrated is the test` section and the vignette a matching subsection, both carrying the sweep (.092/.079/.062/.054 at p*/N 0.50/0.25/0.12/0.06, flagged as one population's sweep and not a general threshold), the N = 600 range (.06-.11 scaled against .02-.03 unscaled, with the unscaled moving further from nominal as N grows), the direction (over-flagging, the safer error), and the complete-data scoping that keeps the rates off the FIML path. The printed `summary()` note carries direction and a `?axes_reliability` pointer only — deliberately no rates, since nothing would tie a number there to the fixture. Four guards, including one asserting the published ranges bracket the fixture's own rates so documentation and evidence cannot drift apart.
- 2026-08-02: T12 done (AC12) — the six-token sweep (`robust`, `non-normal`, `nonnormal`, `distribution-free`, `kurtosis`, `ADF`) over R/, man/, vignettes/, NEWS.md and tests/testthat/ returned ~90 hits, every one dispositioned (c) unrelated: all are `ssm_sem()`'s genuine robust estimators and their scaled statistics, `ssm_ci_accuracy()`'s Bradley robustness band, CPM/plotting uses of the word, or test assertions about those. None describes M68's scaling. `R/axes_corrected_se.R:26` is the only hit on an axes surface and already fences the misreading from M66's side (robust/sandwich SEs `measured no fix`). Zero hits needed (a) or (b). Added the required fencing sentence to the roxygen Details — normal-theory throughout, corrects the correlation-versus-covariance metric only, explicitly unrelated to `ssm_sem()`'s robust scaled statistics — with a guard pinning both halves.
- 2026-08-02: AC4 re-run after the T11/T12 edits — every surviving hit is either the corrected-contract text describing the distortion being removed or an unrelated use (the quasi-circumplex `refused rather than approximated` trio, `NEWS.md:190,262`). No surface reverted.
- 2026-08-02: T7 done (AC3) — `devel/m68-fiml-scaled-cells.R` regenerates from the M65 fixture's MCAR seeds and the M66 fixture's M1 MAR seeds (read off those fixtures rather than restated, so the three sets of evidence are the SAME draws), 95 min for 801 FIML fits. `mean(T_s)/df` = 1.0187 / 1.0221 / 1.0267 / 1.0107 at 2/5/10 % MCAR and M1 MAR — all inside AC3's [0.95, 1.05], so **M68-D1 is not falsified**. The four track the complete-data N = 600 value (1.0204) closely, which is what a metric-only factor predicts, and the M1 cell at N = 2400 lands nearest to 1, the same finite-sample bias shrinking with N. A second test pins the D-1 consequence directly: the factor does not drift across a fivefold change in the missingness rate (a saturated-information factor would).
- 2026-08-02: T8 done — NEWS entry extended with the small-sample direction and a pointer to `?axes_reliability`. `devtools::document()` produces no diff. `devtools::test()`: 0 failures, 5031 passing, 4 warnings (all pre-existing, unchanged from the 4421-pass pre-M68 baseline). `devtools::check(args = "--no-manual")`: **Status: OK** — 0 errors, 0 warnings, 0 notes.
- 2026-08-02: caught at T8 by the repo's own `test-rd-latex-safe.R` guard, not by review — the new roxygen section used literal `chi-squared` and `alpha` glyphs, which the Rd must carry as `\eqn{}` math. Rewritten; the guard is the reason a non-ASCII Rd never reaches win-builder. Worth noting that the filtered test runs during T11 never exercised it, so only the full suite found it.
- 2026-08-02: all tasks complete; status -> review.
- 2026-08-02: review round 1 RETURNED to in-progress. Failed: AC1 (no test reads `$fit$cfi` on any path, so a wiring regression assigning lavaan's unscaled cfi would pass the whole file), AC5 (`R/axes_corrected_se.R:22` still carries the unpaged `(Cudeck, 1989)` the criterion requires anchored), AC9 (the same-environment exact-reproduction arm is unimplemented; only the +-.021 drift fence runs), AC11 (the printed note sits two blocks above the fit line `summary()` prints, not beside it), AC14 (the smoke cell re-implements the replicate inline on different seeds instead of running the generator's own function). Also actioned: F1 at 92 (`print()` emits `both sides of that mismatch are corrected` even when one correction failed — the exact failure its own comment says the three-way split exists to prevent) and F2 at 82 (scaled CFI returns NaN where lavaan returns 1, when both the model and baseline chi-squares fall at or under their df). Passed with fresh evidence: AC2, AC3, AC4, AC6, AC7, AC8, AC10, AC12, AC13. Consistency gate clean apart from the logged weight-cap exception. Blame-history and prior-review lenses returned zero findings each.
- 2026-08-02: plan chose replacing `$fit`'s values and retaining the unscaled six in `details$fit_uncorrected` over adding parallel `*_scaled` fields, following M66's `details$se_uncorrected` precedent, so the default-read number is the calibrated one; falsified by a user needing both side by side in printed output.
- 2026-08-02: round-2 question gate — both recommendations accepted: the scaled-fit note MOVES to `summary()`'s fit line (rather than printing in both places or amending RR14's criterion), and AC9's exact-reproduction arm is implemented as a per-replicate replay in the suite plus a full-scale `verify` mode on the generator run once by hand. Tasks T13-T15 added for the return (minor amendment) and the Coverage map extended to name them.
- 2026-08-02: T13 done (F1/F2/F16) — `axes_metric_note` became a function of which corrections are live, because its own "both sides of that mismatch are corrected" clause was an unconditional assertion, the exact failure the three-way split existed to prevent, re-introduced one level up; three variants now say only what is true of the object. `axes_fit_scaled_note` and the scaling-failure note moved out of `print()` into `summary()`, directly under the chi-square/RMSEA/CFI line (BC5 asks for "beside"; `print()` reports no fit statistic, so nothing is orphaned and the sentence appears once). Scaled CFI now returns 1 rather than `NaN` when model and baseline both fall at or under their df, mirroring lavaan's own `lav_fit_cfi()` and checked against that function directly. The AC11 placement test asserts POSITION, not just presence: nothing but blank lines may sit between the fit line and the note.
- 2026-08-02: T14 done (F4/F6/F7) — the harness's populations, seed formula and replicate function moved from `devel/m68-scaled-fit-cells.R` into `tests/testthat/helper-m68-cells.R` (the existing `helper-ssm-sem.R` / `devel/m5-coverage-oracle.R` pattern), so the generator source()s them and the suite calls the generator's OWN `m68_one_rep()`; the smoke cell now runs it on the fixture's own first 12 strong-axes seeds and requires the committed rows back to 1e-12, where before it re-implemented the replicate inline on unrelated seeds and could not see harness/package drift at all. AC9's exact-reproduction arm: the suite asserts the six rates equal their committed constants to 1e-12 (not the ±.021 fence) and replays two replicates per population from their seeds, guarded on the R and lavaan versions matching the fixture's. `$fit$cfi` is now read on every input path in `expect_scaled_contract()` and recomputed independently by inverting lavaan's own uncorrected CFI for the baseline chi-square `details` does not store — with a paired assertion that the scaled and unscaled values differ, so reporting lavaan's number cannot pass.
- 2026-08-02: AC9 full-scale evidence — `Rscript devel/m68-scaled-fit-cells.R 2000 8 verify` (new mode: regenerates and compares instead of writing) reproduced all seven committed cells at max|diff| = 0.000e+00 over 14000 fits, same R 4.6.1 / lavaan 0.6.21, 4.8 min; all seven rejection rates identical to the committed values.
- 2026-08-02: round-2 gate re-run — `devtools::document()` no diff; `devtools::test()` 0 failures, 5077 passing (5031 before the round), 4 warnings all pre-existing; `devtools::check(args = "--no-manual")` **Status: OK** (0/0/0, 13m). All round-1 tasks re-verified; status -> review for round 2.
- 2026-08-02: T15 done (F5) — `R/axes_corrected_se.R` anchors `cudeck1989 (p. 323)` twice, at the Error (c) scope sentence and at the attribution paragraph, which now fences what the article actually supplies: Cudeck states a correction is needed and points at Browne (1982, section 1.6), prints no formula, and the Browne pages he means are not on the shelf — so the shipped formula is derived in-repo and the citation licenses the premise, not the algebra. The `cudeck1989.md` note's own "cited without a page today" line corrected in place, since it had become false.

## Decisions

**M68-D1 (2026-08-02): the FIML path's scaling factor uses the complete-data
`Γ_R` at Σ̂, and a failed factor NAs the four statistics rather than falling
back.** Settles the T4 `(RB tripwire: no-oracle)` question D-036 left open, at
the implementation question gate rather than by escalation.

*The `Γ_R` choice.* The two candidates were the complete-data `Γ_R` evaluated at
the fitted Σ̂, and RR13 §4's saturated observed-information acov
delta-transformed to the correlation metric. Chosen: the complete-data form, on
M66's precedent rather than on new theory. M66's FIML SEs compose
**multiplicatively** — `se_uncorrected * (corrected/naive)`
(`R/axes_reliability.R:1610-1620`) — precisely so lavaan's own missing-data
pricing survives and only the metric error is removed. The test statistic sits
in the same position: lavaan's FIML `T` is already referenced against the FIML
saturated loglikelihood, so it already prices missingness, and the normal-theory
reference for `c` is exactly 1, which makes `c` a metric-only ratio by
construction. The saturated-information form would price missingness a second
time, inside a factor applied to a statistic that has already priced it.
*Falsified by:* AC4's cells missing [0.95, 1.05], which is the only oracle
either construction has; a miss escalates via `/milestone-brief` rather than
being patched.

*The failure contract.* When `axes_scaling_factor()` returns a `reason` instead
of a factor, `$fit$chisq`, `$pvalue`, `$rmsea` and `$cfi` are `NA` with the
reason stored in `details$fit_scaling_failed`, and `details$fit_uncorrected`
still carries lavaan's six. `$fit$df` and `$fit$srmr` are unaffected — they
never depended on the factor. Rejected: reporting lavaan's unscaled values with
a warning, which is the one failure a user could not detect, and is the same
call M66 made for the SEs (`R/axes_corrected_se.R:88-92`).

**M68-D2 (2026-08-02): the factor is priced at `cov2cor(Σ̂)`, not at Σ̂ as
lavaan returns it.** Discovered at T2 while building AC2's oracle, and
load-bearing: pricing at the raw Σ̂ moved `c` by 0.3% and broke oracle agreement
at the 1e-4 level.

`lavaan::fitted(fit)$cov` carries lavaan's `sample.cov.rescale`, so the fitted
diagonal comes back at `(N−1)/N` — 0.998333 at n = 600 — and `Γ_R`'s entries are
functions of correlations, where `(1 − ρ²)²` is meaningless at ρ > 1. A single
scalar does not undo it either: under misspecification the implied diagonal is
not even constant (measured range 0.951–1.026 on a deliberately perturbed
probe). Normalizing is exact rather than approximate — `T` is invariant to a
scalar rescaling of both matrices, and pricing `U` and `Γ_R` at the same implied
*correlation* matrix is the coherent reading of an estimand defined on the
correlation metric. With it, the shipped trace identity and the literal
vech-space oracle agree to 1e-15.

Noted, not acted on: `axes_corrected_se()` prices at the raw Σ̂ with the same
unit-diagonal assumption in its `wc` construction (`R/axes_corrected_se.R:141-143`).
Its `naive` branch reproduces lavaan's own SEs to 1e-7 *because* it matches
lavaan's Σ̂, so the two are internally consistent there; the corrected branch
carries an O(1/n) discrepancy this milestone does not touch. Out of M68's scope
— changing it changes shipped standard errors — and filed as a ROADMAP candidate
instead.

**M68-D3 (2026-08-02): RR14 confirms the derivation and the ship decision; the
small-N residual is documented, not corrected.** Ingests
`cairn/reviews/RR14-axes-reliability-scaled-chisq-calibration.md`.

*Q1 — derivation confirmed.* An independent route the repo does not use
(Olkin–Siotani closed formula for `n·cov(r_ij, r_kl)`, verified against the
delta-method construction at 3e-16 on a non-model matrix, against a Monte
Carlo, and carried end-to-end) reproduced the shipped `c = 0.9563346` at
relative 2e-15. The baseline collapse to `mean((1 − ρ²)²)` is exact and
structural — `Δ_b′V_bΓ_R = 0` — and M68-D2's `cov2cor(Σ̂)` pricing is right.

*Q2 — attribution established, not merely inferred.* Replacing the per-fit `ĉ`
with `c_pop` in the committed fixture moves the rejection rate by ≤ .0005 in
every cell (relative `sd(ĉ)` ≤ .0024), ruling out factor-estimation noise;
oracle mean-recentering alone restores .048–.058 in every cell, proving the
residual is a pure mean shift. Mechanism: tail excess ≈ Φ-shift of (relative
bias)·√(df/2), which predicts why the large-`df` anti-conservative corner
rejects worst.

*Q3 — `$fit$df` stays an integer.* Eigenvalue dispersion measured at 2.5%; a
mean shift is not something a multiplicative statistic can remove, so a per-fit
Satterthwaite adjustment would mutate a documented field to buy ~.004.

*Q4 — ship all four.* The unscaled test is asymptotically miscalibrated in the
flattering direction and worsens with N; the scaled test's small-N error
over-flags and shrinks with N. RMSEA/CFI effects are negligible (~0.006 RMSEA
at N = 600).

*Rejected by RR14, recorded so they are not re-litigated:* a per-fit adjusted
statistic, a runtime small-sample warning (it would fire on essentially every
realistic input, degrading the warning channel), and any retreat to the
unscaled statistics or a user-facing switch (D-035/D-036 stand).

## Review

**Round 1 (2026-08-02) — RETURNED to `in-progress`.** Five acceptance criteria
fail as literally written, plus two actioned defects that map to no criterion.
Evidence below; criteria that passed are ticked in the AC block above, those
that failed are not.

### Criterion evidence (fresh, by command)

- **AC1 — FAIL.** All three paths do scale (listwise/cormat `chisq` 296.128 ->
  309.566 at `c` = 0.95659, fiml 297.011 -> 310.480 at 0.95662; `df` 273 and
  `srmr` 0.049503 identical to the uncorrected copy on every path; `$fit`
  carries exactly the six documented fields). But the criterion's own
  verification clause is unmet — see F4.
- **AC2 — PASS.** Vech-space oracle agrees with the shipped trace identities on
  three probe maps; a third independent route (numerical differentiation of
  `cov2cor`, by the diff reviewer) agrees to 9e-12 relative. Corroboration:
  `261.1 / c_pop` = 273.022 against `df` = 273, gap 0.022 <= the 0.5 bar.
- **AC3 — PASS.** FIML `mean(T_s)/df` = 1.0187 / 1.0221 / 1.0267 / 1.0107 at
  2/5/10 % MCAR and M1 MAR (200/200/200/201 replicates), all inside
  [0.95, 1.05]. The `em_stalled` ordering test asserts the refusal message with
  the scaler stubbed to `stop()`.
- **AC4 — PASS.** Sweep re-run post-documentation: every surviving hit is either
  corrected-contract text or an unrelated use, dispositioned in the work log.
- **AC5 — FAIL.** See F5.
- **AC6 — PASS.** `document()` no diff; `test()` 0 failures / 5031 passing / 4
  pre-existing warnings; `check(args = "--no-manual")` **Status: OK**.
- **AC7 — PASS.** `mean(T_s)/df` = 1.0204 / 1.0139 / 1.0227 over 2000
  replicates each, inside [0.97, 1.03]; unscaled 0.9757 / 0.9423 / 0.9798.
- **AC8 — PASS.** Rejection at N = 4800 = .0540 +- .0051 over 2000 replicates,
  inside [.036, .064]; unscaled .0145 at the same N.
- **AC9 — FAIL.** The +-.021 drift fence holds (.0790/.0630/.1070 scaled,
  .0270/.0200/.0215 unscaled), but the criterion's exact-reproduction arm is
  unimplemented — see F6.
- **AC10 — PASS.** |rej(T/c-hat) - rej(T/c_pop)| = .00100 / .00000 / .00050,
  all <= .005; relative sd(c-hat) <= .00243.
- **AC11 — FAIL.** See F16.
- **AC12 — PASS.** Six-token sweep over five trees; no surface describes the
  scaling as a robustness correction; the roxygen fencing sentence is present
  and guarded.
- **AC13 — PASS.** Entrywise `Gamma_R` agreement to 1e-12 on the octant probe
  and on a non-model random correlation matrix, with a perturbation check.
- **AC14 — FAIL.** See F7.

### Consistency gate

`cairn_validate` exits 1: **`weight caps` FAIL** (200 plan-owned lines against
a <150 cap) plus `sizing` WARN (14 criteria, 12 tasks). Both are the
maintainer's logged exception of 2026-08-02, taken over splitting the milestone;
they are not new. All 15 other checks PASS, including `coverage complete` and
`binding criteria` (the AC block string-compares clean against RR14).
Toolchain gate: `document()` no diff, `pkgdown::check_pkgdown()` no problems,
README in sync, NEWS entry present, `check()` OK.

### Independent review — three lenses, then a scorer

Blame-history lens: **zero findings** (every deletion traced to the D-entry
authorizing it; the M65 SRMR fix comment verified untouched). Prior-review lens:
**zero findings** (checked against M59-M67 archived `## Review` sections and
`LESSONS.md`; the GitHub inline-comment probe returned empty, so no per-PR walk).
Diff-bug lens: 19 findings. Scored by a fresh agent; 7 scored >= 80.

**Actioned (>= 80), verbatim titles:**

- F1 (92) — `print()` asserts both corrections happened when only one did.
- F4 (85) — AC1's "verified by a test reading all four from each path" is not
  met for `cfi`.
- F6 (85) — AC9's exact-reproduction arm is unimplemented and unevidenced.
- F7 (85) — AC14's smoke cell does not run "the generator's replicate function".
- F16 (83) — BC5's "prints beside the chi-squared/RMSEA/CFI line" is only
  loosely met.
- F5 (82) — AC5 is not satisfied: `R/axes_corrected_se.R` carries no
  `citekey (p. N)` citation.
- F2 (82) — scaled CFI is `NaN` where lavaan returns 1.

**Logged below threshold (12), not actioned:** F8 (68) the AC13 check validates
a duplicate of the Jacobian rather than the oracle's own; F19 (68) the FIML
cells tolerate ~5% silent scaling failures where the complete-data cells
tolerate none; F17 (58) the vignette names Satorra-Bentler with no
non-normality fence (AC12 requires it only in the roxygen); F11 (50) an item
name absent from `sigma`'s dimnames raises a subscript error rather than the
documented refusal; F10 (45) the printed-note test scans the whole transcript
and could collide with a formatted value; F12 (45) `df_mismatch` warns where the
sibling lavaan guard errors; F14 (45) the 536 KB fixture ships in the CRAN
tarball; F3 (40) `axes_scale_fit_measures()` guards `cf$scale` but not
`cf$baseline` or its sign, unreachable through the real pipeline; F15 (35) the
information matrix is computed twice per call; F18 (35) AC7-AC10 read stored
columns rather than re-running the estimator; F9 (30) no scaled-fit test
exercises `item_block`/zeta2; F13 (30) `c_b` prices `Gamma_R` at the
model-implied matrix (measured effect on CFI: 6e-5).

### Return

Return #1 for this milestone on the defect track (the earlier AC3 supersession
was an amendment return and is counted separately). Five criterion failures
(F4/AC1, F5/AC5, F6/AC9, F7/AC14, F16/AC11) plus F1 at 92 on user-facing
behaviour take the return floor. F2 is actioned in the same pass.


---

**Round 2 (2026-08-03).** The five criteria that failed round 1 (AC1, AC5, AC9,
AC11, AC14) re-verified with fresh evidence, and the nine that passed re-run
rather than carried over — the code moved under all of them.

### Criterion evidence (fresh, by command)

- **AC1 — PASS.** All three paths scale all four statistics: listwise/cormat
  `chisq` 296.128 -> 309.566, `p` .16073 -> .06323, `rmsea` .016804 -> .021130,
  `cfi` .991775 -> .988847 at `c` = 0.956590; fiml 297.011 -> 310.480 at
  0.956621. `df` = 273 and `srmr` bit-identical to the uncorrected copy on every
  path (`identical()` TRUE). Round 1's F4 gap is closed: `expect_scaled_contract()`
  now reads `$fit$cfi` on each path and recomputes it by inverting lavaan's own
  uncorrected CFI for the baseline chi-square; verified outside the suite too,
  agreeing at 0.0e+00 / 0.0e+00 / 1.1e-16, and the branch was confirmed to FIRE
  on all three paths rather than being guarded away.
- **AC2 — PASS.** Oracle tests green in a full run of the file. Corroboration
  re-measured: `261.1 / c_pop` = 273.022 against `df` = 273, gap 0.022 <= 0.5.
- **AC3 — PASS.** FIML `mean(T_s)/df` = 1.0187 / 1.0221 / 1.0267 / 1.0107 at
  2/5/10 % MCAR and M1 MAR (200/200/200/201 replicates), all inside [0.95, 1.05].
- **AC4 — PASS.** Four-token sweep re-run over five trees: 51 hits, every one
  either corrected-contract text (the vignette's "fit is flattered" describing
  the distortion being removed), an absence-guard pinning falsified wording, or
  an unrelated use. No surface reverted.
- **AC5 — PASS.** Both source notes exist with `Provenance` blocks and page
  anchors, both carry `INDEX.md` lines, and both R files now cite with pages:
  `R/axes_scaled_fit.R` at `satorra1994 (pp. 401/406/407)` and
  `cudeck1989 (pp. 322-323)`; `R/axes_corrected_se.R` at `cudeck1989 (p. 323)`
  twice and `satorra1994 (pp. 406-407)`. Round 1's F5 gap is closed.
- **AC6 — PASS.** `document()` no diff; `test()` 0 failures / 5077 passing / 4
  pre-existing warnings; `check(args = "--no-manual")` **Status: OK** (0/0/0,
  13m 2.5s).
- **AC7 — PASS.** `mean(T_s)/df` = 1.0204 / 1.0139 / 1.0227 over 2000
  replicates each, inside [0.97, 1.03]; unscaled 0.9757 / 0.9423 / 0.9798.
- **AC8 — PASS.** Rejection at N = 4800 = .0540 +- .0051 over 2000 replicates,
  inside [.036, .064]; unscaled .0145 at the same N.
- **AC9 — PASS.** Both arms now hold. The +-.021 drift fence: scaled
  .0790 / .0630 / .1070, unscaled .0270 / .0200 / .0215. The exact-reproduction
  arm, unimplemented at round 1 (F6), is implemented twice over: in the suite as
  exact equality of all six rates to their committed constants at 1e-12 plus a
  per-replicate replay from pinned seeds, environment-guarded and confirmed NOT
  skipped (the file runs 0 skips); and at full scale by the generator's new
  `verify` mode, which regenerated all seven cells over 14000 fits under the
  fixture's own R 4.6.1 / lavaan 0.6.21 and reproduced every stored column at
  **max|diff| = 0.000e+00**, with all seven rejection rates identical.
- **AC10 — PASS.** |rej(T/c-hat) - rej(T/c_pop)| = .00100 / .00000 / .00050,
  all <= .005; relative sd(c-hat) = .00089 / .00243 / .00067. See the
  projection-vs-outcome note below on the .0005 figure BC4 records.
- **AC11 — PASS.** All three surfaces carry the behaviour, and the third one now
  sits where BC5 asks. `summary()` prints the note directly under the
  chi-square/RMSEA/CFI line with nothing but blank lines between them — asserted
  as a POSITION, by line index, not merely as presence — and `print()` no longer
  carries it, so it appears exactly once. The Rd and vignette carry the sweep
  (.092/.079/.062/.054 at p*/N 0.50/0.25/0.12/0.06, with its
  single-population disclaimer), the N = 600 range, the direction, and the
  complete-data scoping; the printed note still carries direction and pointer
  only, with rates pinned absent. Round 1's F16 is closed.
- **AC12 — PASS.** Six-token sweep re-run: 111 hits over five trees, one on an
  axes surface (`R/axes_corrected_se.R:41`, `ssm_sem()`-adjacent rejected-routes
  prose) plus the roxygen's own fencing paragraph. The fencing sentence is
  present and guarded.
- **AC13 — PASS.** Entrywise `Gamma_R` agreement to 1e-12 on the octant probe
  and on a non-model random correlation matrix, with the perturbation check.
- **AC14 — PASS.** `grep -rn m68-scaled-fit-cells tests/` non-empty; the suite
  test asserts BC1's three means, BC2's rate, BC3's six rates and BC4's bound.
  Round 1's F7 gap is closed: the populations, seed formula and replicate
  function moved to `tests/testthat/helper-m68-cells.R`, which
  `devel/m68-scaled-fit-cells.R` source()s and binds as its own `one_rep`, so
  the smoke cell calls the generator's own function on the generator's own first
  12 strong-axes seeds and requires the committed rows back at 1e-12. The
  blame-history lens independently diffed the move and found the populations and
  seed formulas content-identical.

### Projection vs outcome (Driving RR: RR14)

Measured against RR14's own recorded figures, both numbers verbatim:

- `c` at the strong-axes population: measured 0.9563346 against projected
  0.9563346 (RR14 Q1's independent Olkin-Siotani route, relative 2e-15).
- BC1 mean ratios: measured 1.0204 / 1.0139 / 1.0227 against projected
  1.0204 / 1.0139 / 1.0227.
- BC2 rejection rate at N = 4800: measured .0540 +- .0051 against projected
  .0540 +- .0051.
- BC3 six rates: measured .0790/.0630/.1070 and .0270/.0200/.0215 against the
  same six projected values.
- BC5 sweep: measured .0920 / .0785 / .0615 / .0540 against projected
  .092 / .079 / .062 / .054.
- **BC4 factor-noise bound: measured <= .00100 against projected <= .0005.**
  The discrepancy is in RR14's own parenthetical, not in the deliverable: BC4's
  operative bar is <= .005 and the strong cell's .00100 sits well inside it, and
  round 1 measured the same .00100 and ticked the criterion on the same
  reading. Recorded rather than reconciled — no re-run of RR14's decomposition
  is available to say which of the two figures its note described, and the
  conclusion it supports (the tail excess is a mean shift, not factor noise) is
  unchanged at either value. Relative sd(c-hat) matches exactly: measured
  <= .00243 against projected <= .0024.

### Consistency gate

`cairn_validate` exits 1: **`weight caps` FAIL** (205 plan-owned lines against a
<150 cap; heaviest Acceptance criteria 130, of which RR14's verbatim block may
not be edited) plus `sizing` WARN (14 criteria, 15 tasks). Both are the
maintainer's logged exception of 2026-08-02, now 5 lines larger than when it was
taken — the three tasks the return added. All 15 other checks PASS, including
`coverage complete` (the map was amended to name T13-T15) and `binding criteria`.
The 47 `work-log format` warnings are all on M7 and pre-existing.
Toolchain gate: `document()` no diff, `pkgdown::check_pkgdown()` no problems,
README in sync, NEWS entry present, `check()` **OK**.

### Independent review — three lenses, then a scorer (round 2)

Blame-history lens: **zero findings** — it traced every round-2 edit to the
round-1 finding that demanded it, and independently diffed the harness move,
confirming the population parameters and seed formulas content-identical (one
cosmetic note: a test title still says "print() WIRING" though it now also
asserts `summary()` wiring). Prior-review lens: **no regression found**; the
GitHub inline-comment probe returned empty, so no per-PR walk. It records one
advisory: F1's defect shape — an unconditional positive claim printed beside a
state that may have failed — is now the THIRD occurrence in this file (M62 F3,
M66 F3, M68 F1), which is a lesson rather than a defect. Diff-bug lens: 12
findings. Scored by a fresh agent holding the diff and the plan; 3 scored >= 80.

**Actioned (>= 80), verbatim titles, all fixed in this round:**

- F1 (92) — AC14's smoke cell asserts bit-level reproduction of the fixture with
  no environment guard — a portability/CRAN failure waiting to happen.
- F2 (85) — AC9's exact-rate arm is over-guarded and will silently vanish on the
  next R release.
- F11 (80) — Two `satorra1994` citations do not use AC5's literal
  `citekey (p. N)` form.

Fixes: the environment predicate moved into `helper-m68-cells.R` as
`m68_env_matches()` and is now applied where each belongs — AC14's smoke cell
gates only its bit-exact comparison on it and keeps always-on loose bars
(1e-4 on `chisq`, 1e-6 on `cfactor`) that still catch harness/package drift,
while AC9's frozen-fixture arithmetic arm was moved OUT from behind the gate
(it cannot be moved by a version bump) and the gate now sits only on the live
replay. Verified: the predicate returns FALSE on a doctored lavaan version, and
the always-on bars are met at 0.000e+00. The two citations now read
`satorra1994 (p. 407)` and `satorra1994 (p. 409)`.

**Return floor.** None of the three takes it. F11 is a criterion-shaped miss but
was fixed in this round rather than returned, so AC5 passes as written. F1
scored >= 90 but is a defect in the verification harness's portability, not in
what `axes_reliability()` does for its users — the floor's excluded class — and
no criterion demanded bit-exactness of the smoke cell. Defect-return count for
this milestone stays at 1 (round 1); the AC3 supersession remains on the
separate amendment track.

**Logged below threshold (9), not actioned:** F6 (66) the shipped `cfi`/`rmsea`
are lavaan's *scaled* variants and no user-facing surface says which variant is
reported, while the file header claims agreement with `lav_fit_cfi()`; F9 (65)
the CFI recomputation sits inside a conditional with nothing asserting the
conditional was entered; F8 (62) `seq(fit_at + 1L, note_at - 1L)` counts
backwards if the note ever lands adjacent to the fit line; F12 (58) the scaled-fit
note's "that metric" now has a distant antecedent; F7 (72) the AC11 no-rates test
now scans output containing formatted fit numbers, so a collision is possible
where it was structurally impossible; F10 (55) the test's CFI 0/0 corner uses
`t2 == 0` where the shipped code uses `all.equal(t2, 0)`; F3 (45)
`axes_metric_note(FALSE, FALSE)` would return a false claim, unreachable because
the sole call site guards it; F5 (42) an object built before M68 has no
`fit_scaling_failed` field and would read as a successful scaling, unreachable
because 2.0.0 is unreleased; F4 (30) the both-failed `print()` state emits no
metric note, which the surrounding comment states as the intended design.

### CI as a fourth lens

The draft PR's first CI run **failed on `ubuntu-latest`**, and it is worth
recording what that bought, because the local environment could not have found
any of it. Two of the three failures were the diff-bug lens's own findings,
confirmed empirically rather than by argument:

- `test-axes-scaled-fit.R:813` — the AC14 bit-exact bar failed on a different
  platform. That is F1's predicted failure, observed. The fixture was generated
  on this machine, so the assertion passed here by construction and could never
  have failed locally.
- `test-axes-scaled-fit.R:837` — AC9 SKIPPED wholesale ("different R or lavaan
  version"), taking the frozen-fixture arithmetic arm with it. That is F2's
  predicted silent-vanishing, observed one release earlier than expected.
- `test-axes-scaled-fit.R:475` — **a third defect neither lens nor the local
  suite found**: the F2 regression test called the unexported
  `lavaan:::lav_fit_cfi()` by argument name, and CI's lavaan takes them under
  different names, erroring the test. The existence probe guarding it checked
  that the symbol resolved, not that it accepted these arguments — the same
  false-coverage shape as a prose-guard that asserts a phrase occurring
  elsewhere. Fixed by removing the dependence from the load-bearing assertions
  (the misfitting case is now recomputed in closed form) and probing the call
  itself, skipping if it is not callable.

Recorded as a lesson rather than a process change: a fixture generated on the
authoring machine makes every exactness assertion over it locally unfalsifiable,
and an unexported function's argument names are not a contract.

### Return

No return. All 14 criteria pass with fresh evidence; the three actioned findings
were fixed in-round and re-verified, as was the CI-only defect above.
Re-verified after the fixes: `devtools::check(args = "--no-manual")`
**Status: OK** (0/0/0, 13m 21.7s), test phase clean.
