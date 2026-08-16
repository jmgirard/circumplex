# M89: Price the degeneracy criterion in the metric the reported numbers live in

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR18
- **Principles touched:** IP1, GP2, GP4
- **Branch/PR:** `m89-fitted-matrix-degeneracy` / https://github.com/jmgirard/circumplex/pull/117

## Goal

Move `axes_reliability()`'s fitted-matrix degeneracy criterion off the raw
matrix and onto `cov2cor(Sigma-hat)` -- the metric every reported number is
actually computed in -- and tighten its floor to a stated accuracy target, so
the package stops refusing matrices it can price exactly and starts refusing
the ones on which it currently returns a wrong standard error.

## Scope

Surface tier: **user-facing** -- `axes_reliability()`'s `$fit`, `$components`
and printed output are exported surfaces, and the reason literals are
documented and printed verbatim.

This milestone was re-cut on 2026-08-16 after RR18, which measured that half
its first cut's premise was wrong (diagonal inflation is benign for every
reported statistic) and that the real defect is a floor a thousand times too
loose, returning corrected SEs wrong by 3.4% at `reason = NULL`. The branch's
shipped code is the starting point, not the deliverable.

**In:** evaluating the criterion at `cov2cor(Sigma-hat)` for every reported
quantity, and at both arms of the SE helper, whose three vectors refuse as a
unit (the retained cost in the Deviations table); the tau = 1e-6 floor; the
cross-surface contract as nested refusals; the exact-rational oracle as
committed validation material; the documented reason enumerations, the in-code
rationale, and NEWS.

**Out:** the reason-vocabulary split, the `df == 0` guard, the `cval <= 0`
relabel (RR18 BC4-BC6), and RR18 rec 7's decoupling of `naive` -> **M90**,
planned in this run, `Depends on: M89`. The scaling arithmetic -> D-036
stands. `naive`'s raw pricing and its lavaan tie -> D-037 stands.

## Acceptance criteria

- [x] **AC1 (BC1)** — `axes_scaling_factor()` evaluates the shared degeneracy
      criterion on `cov2cor(Σ̂)` (the realigned fitted matrix, after its
      existing diagonal guards). On the counterexample-A construction — the
      probe-octant fitted Σ̂ congruence-scaled by D = diag(1e4, 1, …, 1) — it
      returns `reason = NULL` and a `scale` equal to the unscaled matrix's
      `scale` to within 1e-9 relative.
- [x] **AC2 (BC2)** — `axes_corrected_se()` evaluates the same criterion on both the
      raw realigned Σ̂ and `cov2cor(Σ̂)` and refuses (all three vectors NA, one
      reason) when either trips. On the BC1 counterexample-A construction it
      refuses with the criterion's conditioning literal. Nestedness: over the
      probe grid the revised AC2 test enumerates, plus the committed exemplar
      B, every matrix `axes_scaling_factor()` refuses for degeneracy is also
      refused by `axes_corrected_se()` with the same literal.
- [x] **AC3 (BC3)** — The criterion's floor is λmin ≤ λmax·sqrt(p·ε/τ) with
      τ = 1e-6 recorded as a named constant beside the criterion (equivalently:
      the shipped floor × 1000). At p = 3 it refuses the committed exemplar
      `cairn/reviews/rb18-counterexample-b.rds` (λmin/λmax = 1.503e-7 ≤
      2.581e-5); it accepts all three probe-map fitted matrices
      (`probe_octant()`, `probe_six()`, `probe_single()` fits at p = 24/12/8,
      whose κ(cov2cor(Σ̂)) measure 10.45/4.85/4.08 — this review — against
      floors at κ ≈ 1.37e4/1.94e4/2.37e4).
- [x] **AC4 (BC7)** — An offline exact-rational oracle script lives with the
      validation materials (no package dependency) and reproduces, from
      `rb18-counterexample-b.rds`: true `cval` = 0.05554788 ± 1e-7 and true
      corrected SEs 0.1476340 ± 1e-6 and 0.1443740 ± 1e-6; and reproduces the
      Q4 sweep (S_t = t·S_B + (1−t)·I, t ∈ {1−2.5e-5, 1−2.5e-4, 1−2.5e-3})
      showing double-precision SE relative error within a factor of 10 of
      p·κ(R)²·ε at each t.
- [x] **AC5 (BC8)** — The documented reason enumerations, the criterion's in-code
      rationale, and NEWS state the revised contract: the criterion prices
      `cov2cor(Σ̂)` (plus raw for the SE helper's `naive` arm), the two
      surfaces' degeneracy refusals are nested with exact agreement on
      unit-diagonal inputs, and the raw-metric rationale sentence at
      `R/axes_corrected_se.R:299-308` ("...transforms of a matrix that never
      was one") is corrected to the inertia-invariance argument. The tracking
      record correction of F1 (exemplar B is not a metric counterexample) is
      made wherever RO2's claim is recorded.
- [x] **AC6** — AC4's oracle is re-derivable from committed material alone: the
      script names every setting its anchors need that the fixture does not
      carry (`n`, item scales, `item_block`, both zeta flags, `df`,
      `baseline_df`), reads no uncommitted file, and on a clean checkout
      reproduces all of AC4's anchors — the three values at AC4's tolerances
      and the three-point sweep. AC4 is verified through this, not beside it.
      (Ingest audit, findings 7 and 9.)
- [x] **AC7** — AC1's invariance is verified across the family it claims. A test
      sweeps positive-diagonal congruences over `probe_octant()`, `probe_six()`
      and `probe_single()`, asserting NULL `reason` and `scale` within 1e-9
      relative at every point, and varies every axis the claim is free in:
      magnitude (10^k, k ∈ {2,4,8}), direction (deflation as well as
      inflation), location (two diagonal positions), multiplicity (a D moving
      several entries), and ratio (one D with max/min < 10). The `p` factor is
      thereby exercised at p = 24, 12 and 8. (Ingest audit, finding 5.)
- [x] **AC8** — AC2's nestedness is verified over a grid this criterion fixes,
      not one the implementer chooses: both diagonal positions × k = 0..16 of
      the inflation form on each of the three probe maps, plus exemplar B, plus
      ≥1 non-unit-diagonal indefinite and ≥1 near-singular matrix per map.
      (Ingest audit, finding 1.)
- [x] **AC9** — The record this milestone contradicts is superseded, not left
      standing: its own 2026-08-15 `## Decisions` entry (criterion "on the raw
      fitted matrix"; "Rejected: any correlation-metric test") carries a dated
      superseding annotation naming RR18, and the metric choice is recorded in
      `cairn/DECISIONS.md` on the footing D-036 and D-037 set. AC5's record
      correction is made at every site `grep -rn "well conditioned raw" cairn/`
      returns, that command being this criterion's enumerating procedure.
      (Ingest audit, findings 9 and 10.)

### Deviations from RR18

| BC | Departure | Why |
|---|---|---|
| BC4 | Deferred to M90 | The `df == 0` guard and its `"saturated"` literal price the model's df, not the fitted matrix; it is a distinct defect from the metric question and M89's Scope In does not reach it (ingest audit, finding 7). |
| BC5 | Deferred to M90 | The `"indefinite"`/`"ill_conditioned"` split changes the literal AC2 (BC2) asserts agreement on; sequencing it after the metric move keeps AC2's nestedness structural rather than contingent on a non-congruence-invariant threshold (ingest audit, finding 3). |
| BC2 | Retained cost recorded, not removed | AC2's all-three-vectors refusal means a raw-arm failure also NAs `corrected`/`fiml_ratio`, which RR18 rec 8 rejects in principle and rec 7 fixes by decoupling `naive`. Accepted here at Jeff's 2026-08-16 gate decision, deferred to M90: no real call reaches such an input, and the decoupling changes the helper's return contract. |
| BC6 | Deferred to M90 | The `cval ≤ 0` relabel is unreachable once BC3 and BC4 land, so it needs BC4's guard in place and a direct probe to be verifiable at all (ingest audit, finding 6). |

## Coverage

- AC1 → T3, T4
- AC2 → T3, T4
- AC3 → T2, T4
- AC4 → T1
- AC5 → T6
- AC6 → T1
- AC7 → T5
- AC8 → T3
- AC9 → T6

## Tasks

- [x] **T1** — Commit the exact-rational oracle as validation material, with
      every setting its anchors depend on named in the script, and pin its five
      figures. It is the evidence base for T2, so it lands first.
- [x] **T2** — Choose and record τ = 1e-6 beside the criterion, with the
      oracle's error table as its calibration. *(RB tripwire: no-oracle —
      RR18 supplies the oracle; re-escalate only if τ's calibration turns out
      to rest on something the oracle cannot measure.)*
- [x] **T3** — Test-first: the AC8 grid asserting the nested contract, red
      against the branch's current code at the counterexample-A construction.
- [x] **T4** — Move the criterion to `cov2cor(Σ̂)` at the scaling surface and to
      both arms at the SE helper; apply the τ floor.
- [x] **T5** — The AC7 invariance sweep: all five axes, all three probe maps.
- [x] **T6** — Roxygen enumerations, the corrected in-code rationale,
      `devtools::document()`, NEWS, the AC9 supersession (in-file annotation +
      `DECISIONS.md` entry), and the AC9 record correction at both grep sites.
- [x] **T7** — Full `devtools::check()`.

## Work log

- 2026-08-15: created by /milestone-plan. Graduates two ROADMAP candidate rows — the finite-degenerate scaling row and the `+Inf` reason-label row, the latter taken in by the plan gate's wide-scope choice.
- 2026-08-15: criteria audit ([O], fresh context) returned seven findings; six fixed before the gate (numbering as shipped): AC1's `grep` procedure selected four lines of which two were comments, replaced with expression-pinned sites; AC3's ordering constraint stated; AC6 restated at the helper-plus-assembly boundary with a constructed matrix, no real fit being known to reach the regime, and narrowed to the four statistics D-036 scales; AC4 widened past one exemplar to two positions plus a non-inflation form; AC5 added for the documented enumerations the user-facing tier obliges. The two judgment calls went to the gate as one scope-width question; the draft's AC2 and AC3 were merged after the gate to hold the criteria count under the split tripwire.
- 2026-08-15: plan gate chose the wide scope — one stated criterion gating both surfaces, emergent `solve()`-based refusals relaxed, one shared reason vocabulary — over the narrow scope that adds the criterion and leaves the existing refusals and labels alone, because the narrow version leaves the `+Inf` case still disagreeing and meets the Goal only half. Falsified by evidence that relaxing the emergent refusals lets a genuinely unpriceable matrix through, which would argue those guards were load-bearing.
- 2026-08-15: plan gate chose to decide the cutoff in the build over escalating it to a written Fable review, because the choice is a numerical-conditioning call rather than a new statistical quantity and the build can justify it in code. Falsified by the build finding the cutoff turns on a statistical property of the estimator rather than on conditioning.
- 2026-08-15: T1–T2 red against HEAD as planned — the pairwise reason grid splits from k = 7 at both positions (se "unidentified"/"singular" against sf NULL), and the AC4 opposite-direction form is the near-zero positive diagonal with off-diagonals kept (raw pricing survives, both correlation-metric surfaces refuse); near-collinear pairs fail both surfaces at the same eps in every scan, so they cannot serve as the opposite-direction probe.
- 2026-08-15: T3–T5 done — criterion `axes_sigma_degenerate()` chosen and recorded beside its rationale (see Decisions), applied at both consumers ahead of any pricing; se relabels "nonpositive_diagonal" → "singular" and adopts "infinite_diagonal" for +Inf; M71 block byte-unchanged (diff hunks fall at lines 359, 1228, and 1301 only); full suite clean (7215 pass, 0 fail). Also added `axes_fitted_cov()` as the single seam both consumer call sites read, for T6's constructed-matrix injection.

- 2026-08-15: T6 done — assembly test injects a constructed degenerate matrix at the `axes_fitted_cov()` seam via `local_mocked_bindings()`; both surfaces warn with the shared literal, component SEs and the four D-036 statistics are NA together, df/srmr and the point estimates unaffected.

- 2026-08-15: T7 done — roxygen names the criterion and the shared literal at the scaled-fit details and the return-value enumeration; NEWS bullet added; `devtools::document()` warning-free, regenerating only `man/axes_reliability.Rd` (plus the roxygen2 8.1.0 version stamp in DESCRIPTION, a generated line from the updated local toolchain).

- 2026-08-15: T8 done — `devtools::check(args = "--no-manual")` clean, 0 errors / 0 warnings / 0 notes. All tasks complete; status → review.

- 2026-08-15: /milestone-review — PR #117 opened draft; all seven criteria verified with fresh evidence (AC4's pre-milestone reds re-measured against a scratch `origin/master` checkout); consistency gate green (`cairn_validate` exit 0, no DESIGN principle change so `cairn_impact` skipped, `document()` warning-free and diff-free, check Status OK). Three-lens review returned 21 findings, 1 actioned at >= 80 (O1, 85 — the criterion is not invariant under a pure diagonal rescaling), routed to the graduated ROADMAP row rather than patched: no criterion fails, it is unreachable through the exported API, and the repair re-opens the criterion's design as an `no-oracle` tripwire. 20 below-bar findings logged in the Review section.

- 2026-08-15: merge approval WITHHELD at the review gate — Jeff chose to close the NEWS gap first (finding O5, scored 75, below the action bar and so logged rather than actioned by the review). Logged as T9; status review → in-progress. Not a defect return: no acceptance criterion failed and no finding met the return floor, so this does not count toward the thrash rule.

- 2026-08-15: T9 done — the NEWS vocabulary sentence widened from two literal changes to all four, each verified by measurement against `origin/master` rather than composed: an exactly singular fitted matrix moves `"singular"` → `"ill_conditioned"` and an indefinite one `"indefinite"` → `"ill_conditioned"`, both on both surfaces (measured at λmin = −9.32e−16 and −0.382 on the octant probe). All four are pinned by named tests. `devtools::test()` FAIL 0 / PASS 7231 and `devtools::check(args = "--no-manual")` Status OK. All tasks complete; status → review. Note for the next review pass: the recorded AC5 and AC7 evidence predates this NEWS edit and needs re-gathering.

- 2026-08-15: T10 done — `"unidentified"` is fired as a returned reason on BOTH surfaces by a new test each, with its condition asserted and a passing control that passes for the claim's reason (dropping `fit_zeta1` is the only change). The probe is a degenerate Δ rather than a degenerate Σ̂: a single-scale map makes `zeta1` identical to the all-ones `xi2`, which the criterion cannot see (`axes_sigma_degenerate()` returns NULL on that Σ̂, asserted in both tests). `"indefinite"` has no construction left and the reachability claim at `R/axes_reliability.R` is corrected in place rather than tested: measured, an indefinite Σ̂ (λmin = −0.382) answered `"indefinite"` on both surfaces pre-M89 and answers `"ill_conditioned"` now, and 1500 random PD correlation matrices returned c ∈ [0.94, 1.29] with no refusal. Suite FAIL 0 / PASS 7250 (up 19); check Status OK; `document()` warning-free, no `man/` diff. Status → review.

- 2026-08-15: /milestone-review round 2 — all seven criteria re-verified at 8778ae06, consistency gate green. 23 findings, 2 actioned: RO1 (92) falsified the reachability comment T10 added and is fixed here (with the same wording in both tests and RO5's dead assertion); RS5 (85) is round 1's O1, disposition unchanged. RO2 (70), the mirror-image half of O1, is recorded on the ROADMAP row — verified, helper-boundary only. Not a defect return: no acceptance criterion failed, and RO1 is an internal comment rather than a defect in what the package does for users.

- 2026-08-15: merge approval WITHHELD at the round-2 gate — Jeff chose a Fable review of the criterion's metric choice over merging as it stands, the per-instance escalation approval D-004 requires. Logged as T11; status review → in-progress. Not a defect return: no acceptance criterion failed and nothing met the return floor, so this does not count toward the thrash rule.

- 2026-08-15: blocked on RB18 (`cairn/reviews/RB18-axes-degeneracy-criterion-metric.md`) — which matrix the degeneracy criterion should price, with both counterexamples and the correlation-metric blindness of the inflation regime stated, and D-037 surfaced as the standing precedent the answer must engage.

- 2026-08-16: RR18 ingested — answers and both verified measurements recorded in Decisions; ingest audit ([O], fresh context) run before any criteria change, per protocol, and it returned a blocking collision rather than a clean set. RB18/RR18 stay LIVE in `cairn/reviews/` rather than archiving now: the binding criteria are not yet ingested, and the re-cut is what ingests them. Archive the pair once it does.

- 2026-08-16: **M89 re-cut at Jeff's decision at the ingest gate.** RR18 falsifies half the milestone's premise (the diagonal-inflation regime is benign for every reported statistic, so M89's refusals there are false NAs on correct numbers) and relocates the real defect to a cutoff a thousand times too loose, measured as a 3.4% wrong `components$SE` at `reason = NULL`. Its replacement criteria are jointly unsatisfiable with the shipped AC2, and its recommended metric is what this milestone's own Decisions entry explicitly rejected — so the criteria set needs authoring whole, not extending, and that entry needs superseding. Status → planned for `/milestone-plan`. Counted as a re-cut for the thrash rule: this is M89's first, and it follows an escalation rather than a defect return (the two review returns before it were both withheld approvals, not gate failures).

- 2026-08-16: re-cut authored. Task numbering restarts at T1 — the work-log entries above naming T6-T10 belong to the superseded cut and are history, not this plan's tasks.

- 2026-08-16: criteria audit ([O], fresh context, over the drafted wording) returned 13 findings. Twelve fixed before the gate: AC7 widened past magnitude-only probes to direction, multiplicity and scale-ratio; AC8 added to fix AC2's nestedness grid, which was otherwise a domain the implementer chooses; AC6 rewritten to subsume AC4 (satisfiable by already-committed material) and to name `item_block`, which its enumeration omitted; AC9 added for the supersession of this milestone's own contrary Decisions entry and for the record correction, whose sites `grep -rn "well conditioned raw" cairn/` enumerates; and in M90, AC4 extended to AC3's relabel, AC5 given an evidentiary standard for its unreachability escape, AC6 moved to near-threshold anchors (its far-field pair passed with no `p` in the code at all), and AC7 added so the vocabulary split cannot silently kill M89's cross-surface contract. The thirteenth went to the gate.

- 2026-08-16: gate chose to accept AC2's all-three-vectors refusal as a recorded cost and defer RR18 rec 7's decoupling of `naive` to M90, over taking the decoupling now. Rejected because it changes `axes_corrected_se()`'s return contract mid-milestone and no real call reaches the input that makes the cost bite. Falsified by any evidence a reachable fit trips the raw arm while the cov2cor arm computes.

- 2026-08-16: gate chose to keep M89 whole at 9 criteria over splitting the oracle or the documentation out, because the metric move is meaningless without the floor and the floor is unjustifiable without the oracle that calibrates it. Falsified by implementation finding the oracle work separable in practice — it would then have been its own milestone.

- 2026-08-16: T1 done — the exact-rational oracle lands at `devel/degeneracy-oracle/` (an R driver naming every setting the fixture does not carry, plus a stdlib-only Python core for the arithmetic; `devel/` is .Rbuildignore'd so it is dev tooling, not a dependency). It runs from committed material alone and self-checks: anchors PASS (cval +0.0555478790711 against 0.05554788 ± 1e-7; SEs 0.147633962893 and 0.144373995369 against ± 1e-6) and the Q4 sweep PASSes at ratios 3.28 / 2.4 / 1.27, reproducing RR18's 3.3 / 2.4 / 1.25. The ad-hoc `cairn/reviews/rb18-exact-oracle.py` is deleted with it: it read an uncommitted dump and hard-coded n and df, which is exactly what AC6 forbids. No package code changed — `devel/` is outside the build — so the suite cannot have moved and was not re-run for this task.

- 2026-08-16: T2 done — τ = 1e-6 recorded as `axes_degeneracy_tau` beside the criterion, its calibration comment citing the oracle's Q4 error table (T1's measured ratios 3.28/2.4/1.27 against p·κ²·ε) and the 3.4% counterexample; the tripwire's re-escalation condition did not fire — the calibration rests entirely on what the oracle measures. Suite clean (FAIL 0, PASS 7249).

- 2026-08-16: T3–T4 done, one commit so every commit's suite stays green (the red test alone would break the per-task verify). T3's red measured against the pre-move code: the AC8 grid's scaling-surface NULL pins fail from k = 7 ("ill_conditioned" where the estimand is invariant) and the counterexample-A test fails at the sf refusal — the metric defect, verified as such. T4 moves the criterion to `cov2cor(Σ̂)` at the scaling surface (finiteness arm hoisted ahead of `cov2cor()` to keep M71's one-warning contract) and to both arms at the SE helper, floor `sqrt(p·ε/τ)`; assembly tests split — near-singular injection refuses both surfaces, inflation injection NAs SEs alone with fit computing; the falsified "criterion prices the raw matrix" comments in both T10 tests and `R/axes_reliability.R` corrected in place. Suite FAIL 0 / PASS 7326 (up 77); oracle re-run ANCHORS PASS, SWEEP PASS — the sweep's κ = 1.1e4 point computes at error 2.0e-7 and its κ = 1.1e5 point is refused at error 2.6e-5, bracketing the τ floor.

- 2026-08-16: T5 done — the AC7 sweep asserts NULL `reason` and scale within 1e-9 relative on all three probe maps across magnitude (10^±k, k ∈ {2,4,8}), direction, two locations, a three-entry multiplicity D, and a max/min = 8 ratio D. Suite FAIL 0 / PASS 7413 (up 87).

- 2026-08-16: T6 done — roxygen (details block + return enumeration) and NEWS state the revised contract (cov2cor pricing, τ floor, nested refusals, unit-diagonal agreement); `devtools::document()` warning-free, regenerating only `man/axes_reliability.Rd`; the in-code rationale was corrected at T4. AC9: the 2026-08-15 raw-criterion Decisions entry carries its dated superseding annotation; the metric choice recorded as D-044 on D-036/D-037's footing; RO2's falsified framing annotated at the Review-section site and at RB18 (RR18 already states the correction at its own two grep sites, and the ROADMAP row carried it since the ingest — that row now also carries a dated resolution note, since "the metric choice is the open question" was stale current knowledge). Suite FAIL 0 / PASS 7413.

- 2026-08-16: T7 done — `devtools::check(args = "--no-manual")` Status OK, 0 errors / 0 warnings / 0 notes, 7m26s. All tasks complete; status → review.

## Decisions

- 2026-08-15 — **The stated criterion is a relative smallest-eigenvalue floor on the raw fitted matrix: refuse as `"ill_conditioned"` when λmin(Σ̂) ≤ λmax(Σ̂)·sqrt(p·eps), evaluated after each surface's diagonal guards.** (≈ κ ≥ 1.4e7 at p = 24.) Grounds: both consumers build the information matrix from Σ̂⁻¹ twice, so its entries carry relative error growing like p·κ²·eps, and the floor is exactly where that bound reaches 1. Measured fit: every pre-M89 divergence point sits at or above it (the inflation grid splits at κ = 2.1e7; the emergent near-collinear failures begin at κ = 7.9e8) and every measured accurately-computing point sits below it (κ ≤ 8.6e6 on the probe grids), so the criterion refuses nothing the surfaces were pricing accurately. One inequality also covers indefinite and exactly singular matrices (λmin ≤ 0) — needed: an indefinite Σ̂ (λmin = −0.11) sailed through both surfaces with reason NULL and scale 0.95 before M89. Rejected: any correlation-metric test (cov2cor of the inflated matrix stays at condition 10.45 at every magnitude 10⁰–10¹⁶ — blind, the plan's T3 note); the bare eps^(−1/2) ≈ 6.7e7 cutoff without the dimension factor (leaves the measured k = 7 divergence point, κ = 2.1e7, computing on one surface while emergently refused on the other — the exact disagreement M89 exists to remove). Recorded in code beside `axes_sigma_degenerate()` (R/axes_corrected_se.R). *[Superseded 2026-08-16 by RR18 and the M89 re-cut (D-044): the criterion now prices `cov2cor(Σ̂)` — with the SE helper's raw arm nested — under a τ = 1e-6 floor. The correlation-metric rejection recorded here rested on the premise RR18 falsified: the inflation regime the raw pricing existed to catch is benign for every reported statistic, so nothing should catch it at the scaling surface at all.]*

- 2026-08-15 — **RR18 ingested (Fable, spawned at the round-2 gate). Its answers, recorded before any criteria change.** (1) The criterion should price `cov2cor(Σ̂)` for every user-reported quantity on both surfaces, and the raw Σ̂ only where it is actually inverted — the SE helper's `naive` arm, which is test-only. Grounds: every number `axes_reliability()` reports is a function of `cov2cor(Σ̂)` alone, and `corrected`/`fiml_ratio` are invariant to ≤ 6.4e-16 across eight decades of diagonal inflation that move κ(raw) to 2.1e8. By Sylvester's law of inertia the congruence `cov2cor()` preserves eigenvalue signs exactly, so indefiniteness and exact singularity are metric-invariant and no model-statement content is lost. (2) **Half of M89's premise was wrong**: the diagonal-inflation regime is benign for every reported statistic, and nothing should catch it there — no scaled statistic was ever silently unscaled in that regime. (3) The cutoff needs a stated accuracy target, τ = 1e-6, giving λmin ≤ λmax·sqrt(p·ε/τ) — the shipped floor × 1000. (4) `df == 0` needs its own guard and literal; today it reaches `cval = Inf` and reports `"indefinite"`. (5) The `cval ≤ 0` refusal must stop saying `"indefinite"`, since exact tr(UΓ) ≥ 0 always. (6) No published treatment of conditioning for Satorra–Bentler scaling factors exists; the exact-rational oracle the review built is the second independent oracle type and should join the validation battery. (7) `p = nrow` is correct; p\* and q are rejected as no p\*-dimensional matrix is ever inverted.

- 2026-08-15 — **RR18's two measurements, both reproduced independently by the review session before being recorded.** (a) **The committed counterexample B is not a metric counterexample.** Its diagonal is exactly `c(1,1,1)`, so `cov2cor(S) == S` and κ = 6.654e6 in *both* metrics — verified. The round-2 finding RO2's framing, "well conditioned raw and degenerate in the correlation metric", is **false for that exemplar** and is corrected here (M89 round 2, corrected at the RR18 ingest) wherever it was recorded. B is evidence the cutoff is too loose, not evidence about which matrix to price. (b) **B is a silent wrong reported number, not merely a surface disagreement.** An exact rational-arithmetic recomputation of the whole pipeline — written independently by this session from the R source, agreeing with RR18 to every printed digit — gives true corrected SEs 0.147633962893 and 0.144373995369 against the shipped 0.142594879411 and 0.139478432053: **wrong by 3.41% and 3.39% with `reason = NULL`**, in `components$SE`, a documented user-facing field. The true `cval` is **+0.0555478790711**, not the −0.216 the doubles produce, so the `"indefinite"` refusal at B is a sign flip from catastrophic cancellation rather than a statement about the model. This is the package's first measured silent wrong number in this subsystem, and it is what makes the cutoff question severe independently of the metric question.

- 2026-08-15 — **The RR18 binding-criteria ingest audit ([O], fresh context) returned a blocking collision, raised rather than softened.** BC1 requires `axes_scaling_factor()` to ACCEPT the counterexample-A construction; BC2 requires `axes_corrected_se()` to REFUSE it; M89's already-verified **AC2 requires the two surfaces to return non-NULL `reason` at exactly the same grid points**, and counterexample A sits on that grid. BC1–BC8 and AC1–AC7 are therefore **jointly unsatisfiable as written** — no implementation can pass both. RR18's own F5 names the consequence but its BC set never amends AC2. The audit further found: BC1/BC3 mandate what M89's own recorded Decision explicitly rejected ("any correlation-metric test"), so that entry needs superseding, not just extending; BC2's same-literal nestedness is an independent unverified claim that BC5's non-congruence-invariant threshold can falsify, and BC2's verification domain is a test the implementer writes; BC6 becomes unreachable once BC3 and BC4 land, leaving it satisfiable by a string edit; BC4 ships a new user-visible literal `"saturated"` that no criterion documents and that M89's Scope In does not cover; BC7's anchors are not reproducible from the fixture it names (n, item scales, both zeta flags and df are all unstated); and BC1/BC5's probes stand one exemplar in for a family at one p, re-importing the `p`-factor coverage gap prior rounds scored twice. Every numeric anchor the audit could check reproduced exactly. Disposition goes to the user: the criteria set cannot be ingested verbatim without shipping a contradiction.

## Review

### Round 3 — 2026-08-16, at 792b05f4 (the re-cut), PR #117

`origin/master` unmoved since the branch was cut (0 behind); all evidence below
gathered fresh at this HEAD against the re-cut's AC1–AC9, superseding rounds 1–2
(whose evidence verified the superseded criteria set).

- **AC1** — `axes_scaling_factor()`'s helper evaluates the criterion on
  `cov2cor(sigma)` after the diagonal guards (`R/axes_scaled_fit.R:152-154`,
  guards at `:139-140`). The counterexample-A test
  (`test-axes-scaled-fit.R`, "AC1/AC2: a pure diagonal rescaling…") passes:
  `reason` NULL and `scale` within 1e-9 relative of the unscaled fit's.
- **AC2** — the SE helper evaluates the criterion on the raw realigned Σ̂ and,
  when that passes, on `cov2cor(sigma)` (`R/axes_corrected_se.R:264-268`),
  refusing all three vectors through the single `na_out()`. At counterexample A
  it refuses `"ill_conditioned"` (same test). Nestedness holds over the AC8
  grid (3 maps × 2 positions × k = 0..16 + indefinite + near-singular per map)
  and exemplar B; whole suite FAIL 0.
- **AC3** — floor is `ev[p] <= ev[1] * sqrt(p * eps / axes_degeneracy_tau)`
  with `axes_degeneracy_tau <- 1e-6` beside the criterion
  (`R/axes_corrected_se.R`). Measured this round: exemplar B λmin/λmax =
  1.503e-7 ≤ floor 2.581e-5 → refused; the three probe-map fitted matrices
  measure κ(cov2cor(Σ̂)) = 10.45 / 4.849 / 4.077 against floor-κ 1.37e4 /
  1.937e4 / 2.373e4 → all accepted (and pinned by the AC3 test).
- **AC4 / AC6** — the oracle was run from a fresh `git clone` of the branch in
  a scratch directory: ANCHORS PASS (cval +0.0555478790711 against projected
  0.05554788 ± 1e-7; SEs 0.147633962893 / 0.144373995369 against projected
  0.1476340 ± 1e-6 / 0.1443740 ± 1e-6) and SWEEP PASS at ratios 3.28 / 2.4 /
  1.27 against RR18's projected 3.3 / 2.4 / 1.25 — every anchor reproduced
  from committed material alone; the script's header names n, item scales,
  item_block, both zeta flags, df, and baseline_df. AC4 verified through AC6's
  clean-checkout run, as AC6 requires.
- **AC5** — `sqrt(p * .Machine$double.eps / 1e-6)` appears in the roxygen (2
  sites), `man/axes_reliability.Rd` (2), and `NEWS.md` (1); the nested-refusal
  contract sentence ("nest the scaling surface's") in NEWS and the Rd; the
  "transforms of a matrix that never was one" sentence is gone from
  `R/axes_corrected_se.R` (grep 0), replaced by the inertia-invariance
  rationale. F1's record correction verified under AC9.
- **AC7** — the five-axis congruence sweep (magnitude 10^±{2,4,8}, both
  directions, two positions, a three-entry multiplicity D, a max/min = 8 ratio
  D; p = 24/12/8) passes with NULL reasons and scale drift < 1e-9 everywhere.
- **AC8** — the grid test passes at HEAD; its red was measured pre-move at the
  counterexample-A construction (work log, T3–T4: sf refused "ill_conditioned"
  where the AC1 pins NULL, from k = 7).
- **AC9** — the 2026-08-15 raw-criterion Decisions entry carries the dated
  superseding annotation naming RR18 (grep 1); D-044 recorded on D-036/D-037's
  footing (grep 1); `grep -rn "well conditioned raw" cairn/` returns 8 sites,
  each carrying or adjoining its correction: milestone 106/185 quote the
  enumerating command itself, 209 is the correcting Decisions entry, 291/302
  the annotated RO2 bullet, RB18:64 the annotated brief claim, RR18:28/332 the
  review's own statements of the falsity.

Projection-vs-outcome (Driving RR18): every numeric projection above is shown
beside its measured value — cval, both SEs, the three sweep ratios, exemplar
B's eigenvalue ratio, and the three probe-κ measurements; no shortfall
anywhere (sweep ratio 1.27 vs 1.25 sits inside the oracle's own
factor-of-10 acceptance band, which is the criterion's stated tolerance).

### Consistency gate (round 3)

- `cairn_validate` exit 0, all checks PASS; 49 advisory WARNs, all pre-existing
  M7 work-log lines.
- No `DESIGN.md` change on the branch → `cairn_impact` skipped.
- `r-package` slot: `document()` emits 0 `resolve link` lines and leaves
  `man/`/`NAMESPACE`/`DESCRIPTION` diff-free; NEWS entry present (no milestone
  numbers); `devel/` covered by `^devel$` in `.Rbuildignore`; README sources
  untouched by this branch; `pkgdown::check_pkgdown()` "No problems found";
  `devtools::test()` FAIL 0 | WARN 5 | SKIP 3 | PASS 7413 (the 5 warnings in
  files this branch does not touch); `devtools::check(args = "--no-manual")`
  Status OK 0/0/0 at this code state (T7, tracking-only commits since).

### Round 2 — 2026-08-15, at 8778ae06, PR #117

Re-reviewed after T9 and T10 landed. `origin/master` has not moved since the
branch was cut (0 behind, nothing unpushed), so no sync merge; all evidence
below is re-gathered at this HEAD, superseding round 1's.

- **AC1** — `axes_sigma_degenerate()` defined once (`R/axes_corrected_se.R:315`)
  beside its rationale, called at both named consumers' helpers
  (`R/axes_corrected_se.R:259`, `R/axes_scaled_fit.R:149`) for the
  `corrected <- axes_corrected_se(...)` and `scaling <- axes_scaling_factor(...)`
  expressions (`R/axes_reliability.R:1727`, `:1836`). One definition, two sites.
- **AC2** — the grid test passes (78 assertions, 0 failures); pre-milestone the
  same grid diverges at 20 of 68 points. `+Inf` → `"infinite_diagonal"` on both;
  `-Inf` → `"singular"` on both.
- **AC3** — guards precede the criterion in both files
  (`R/axes_scaled_fit.R:139,140,149`; `R/axes_corrected_se.R:244,253,259`);
  master's `test-axes-scaled-fit.R:1258-1300` block is present verbatim at
  HEAD:1262 and passes.
- **AC4** — re-measured against a scratch `origin/master` checkout: the
  inflation form diverges from k = 7 at **both** positions 4 and 20
  (`"unidentified"` against `NULL`); the non-inflation form (`sigma[4,4] <-
  1e-3`) has the raw-priced branch surviving with finite naive SEs while
  `axes_scaling_factor()` refuses — AC4's stated direction — and its
  `"ill_conditioned"` pin fails pre-milestone (both answer `"indefinite"`).
- **AC5** — `"ill_conditioned"` in roxygen (5 sites), `man/axes_reliability.Rd`
  (2), and `NEWS.md` (3). After T9 the NEWS entry names all six literals in
  play: `"nonpositive_diagonal"`, `"singular"`, `"unidentified"`,
  `"infinite_diagonal"`, `"indefinite"`, `"ill_conditioned"`.
- **AC6** — the assembly test passes (16 assertions): both warnings name the
  shared reason, `components$SE` and all four scaled statistics NA together,
  `df`/`srmr` finite and equal to `details$fit_uncorrected`.
- **AC7** — `devtools::test()` **FAIL 0 | WARN 5 | SKIP 3 | PASS 7250**;
  `devtools::check(args = "--no-manual")` **Status: OK** (0/0/0, test phase OK
  at 401s). `document()` warning-free, `man/`/`NAMESPACE` diff-free.

### Consistency gate (round 2)

- `cairn_validate` exit 0, all 16 CHECKs PASS; advisory WARNs are M7 work-log
  lines only.
- No `DESIGN.md` principle changed → `cairn_impact` skipped.
- `r-package` `consistency-gate` slot: `document()` emits 0 `resolve link`
  lines and no diff; `NEWS.md` entry present; no new top-level files or exports.

### Independent fresh-context review (round 2)

Three distinct-evidence reviewers over the updated branch, then a fresh scorer.
23 findings: 10 from the [O] diff-bug lens, 8 from the [S] blame lens, 5 from
the [S] prior-review lens. Round-1 findings re-reported by the lenses were
re-scored on their own merits. Two actioned at >= 80.

**RO1 (92) — the `"indefinite"` reachability comment T10 added was FALSE, and
is corrected.** It claimed "No construction reaching it has been found since",
resting on 1500 random PD matrices at p = 24 only. The criterion admits kappa up
to 1/sqrt(p*eps), which is 3.8e7 at p = 3, and c goes negative in that admitted
band. Verified two ways by this review: deterministically, a saturated model
(p = 3, df = 0) makes `R/axes_scaled_fit.R:217` divide by zero, giving
`cval = Inf` -> `"indefinite"` on a matrix `axes_sigma_degenerate()` returns
NULL for; and on a captured exemplar at kappa = 6.65e6, two orders below the
p = 3 cutoff. **Fixed now**: the comment no longer claims unreachability. It
states both measured routes, names the assembly gates that stop them
(`axes_reliability()` refuses < 4 scales; `axes_design()` drops a collinear
component), and says explicitly that this is a fact about the assembly and not
about the criterion. The same falsified "only" wording in both T10 tests is
corrected with it, and RO5's vacuous `expect_false(any(is.nan(...)))` — dead by
construction after `na_out()` — is removed.

**RS5 (85) — round 1's O1, restated by the blame lens.** Disposition unchanged:
follow-up on the graduated ROADMAP row, not fixed here.

**Below the action bar (21), logged not actioned.** Highest first:

- RO4 (75) / RS3 (74) — T10 fires `"unidentified"` through a single-scale map,
  which `axes_reliability()` refuses upstream, so the literal is fired only at
  the helper contract boundary. Both tests now say so in as many words.
- RO2 (70) — **the mirror of RS5/O1, and the more serious half.** Because the
  criterion prices raw Sigma-hat while both surfaces price `cov2cor(Sigma-hat)`,
  a matrix well conditioned raw and degenerate in the correlation metric passes
  the door and fails later: at p = 3 over ~8,200 criterion-accepted draws, 36
  returned finite corrected SEs with `se_correction_failed = NULL` beside
  `fit_scaling_failed = "indefinite"` — this milestone's Goal failure mode with
  the roles swapped. Reproduced by this review at kappa = 6.65e6. Scored 70
  because `axes_reliability()` refuses fewer than 4 scales and the reviewer
  found 0 disagreements at p = 4, 5, 6, 8, so no user path reaches it. Recorded
  on the ROADMAP row beside O1: together the two say the raw-vs-correlation
  metric choice is the open question, not the cutoff value. *[Corrected at the
  RR18 ingest, 2026-08-16 (D-044): the κ = 6.65e6 exemplar this finding rests
  on has an exactly unit diagonal, so its κ is identical in both metrics — it
  is not a "well conditioned raw and degenerate in the correlation metric"
  case at all, but evidence the cutoff was a thousand times too loose. Both
  question halves were settled by RR18 and this re-cut.]*
- RO3 (68) — the T10 tests' "only a degenerate Delta" prose was the same
  overstatement that produced RO1; corrected with it.
- RP1 (66) — the ROADMAP row's round-1 note went stale when T9 and T10 closed
  two of the three findings it listed; corrected in place and marked.
- RO7 (65) / RS8 (62) — the load-bearing `p` factor is exercised at p = 24 only;
  sharpened by round 2, whose counterexamples sit at p = 3 where that factor
  makes the cutoff loosest. Carried on the ROADMAP row.
- RP5 (62) — the sibling `se_correction_failed` comment still carries a
  "never fired" caveat that round 2 shows was already stale on master.
- RS1 (55) / RP3 (55) — M70's declined reason-code parity overturned with no
  recorded supersession.
- RS2 (55) / RP4 (55) — M71's declined `+Inf` label, likewise.
- RO5 (52) — vacuous NaN assertion in the new SE test; removed with RO1's fix.
- RS6 (50) — T4's "relax the emergent refusals" never happened; they are
  shadowed, and round 2 shows they remain reachable.
- RS7 (48) — the NA/NaN warning-count collapse is still absent from NEWS.
- RO8 (40) — all four NEWS literal claims verified accurate; two wording gaps.
- RO9 (38) — `p` used before it is defined in the generated Rd prose.
- RS4 (35) — the rewritten `na.rm` comment drops M70's attribution.
- RO6 (34) — `eigen(symmetric = TRUE)` folds to the symmetric part.
- RO10 (20) — the `Config/roxygen2/version` stamp.
- RP2 (5) — explicitly not a finding (records that no regression was found).

**Re-verified after the RO1 fix:** `devtools::test()` FAIL 0 | PASS 7249;
`devtools::check(args = "--no-manual")` Status OK; `document()` 0 `resolve link`
lines, no `man/` diff.

### Round 1 — 2026-08-15, at 416cb655, PR #117.
Branch 5 commits ahead of `origin/master`, 0 behind — no sync merge needed.

### Acceptance-criterion evidence

- **AC1** — `axes_sigma_degenerate()` is defined once, at
  `R/axes_corrected_se.R:315`, beside the rationale block at `:274-314`
  (criterion, cutoff derivation, why it prices the raw matrix, return
  vocabulary). It is called at both named consumers' helpers ahead of any
  pricing: `R/axes_corrected_se.R:259` for the `corrected <-
  axes_corrected_se(...)` expression (`R/axes_reliability.R:1727`) and
  `R/axes_scaled_fit.R:149` for the `scaling <- axes_scaling_factor(...)`
  expression (`R/axes_reliability.R:1836`). One definition, two call sites,
  no second criterion anywhere in `R/`.
- **AC2** — the AC2 grid test (`tests/testthat/test-axes-scaled-fit.R:1330`)
  runs both diagonal positions × k = 0..16 × both forms and compares the two
  `reason` fields pairwise; it passes on HEAD with 0 failures. The same grid
  measured against pre-milestone code diverges at 20 of its 68 points. `+Inf`
  now returns `"infinite_diagonal"` on both surfaces (pre-milestone:
  `"unidentified"` / `"infinite_diagonal"`); `-Inf` returns `"singular"` on
  both (pre-milestone: `"nonpositive_diagonal"` / `"singular"`).
- **AC3** — ordering measured by grep: in both files the `<= 0` and
  `is.infinite()` diagonal guards precede the criterion
  (`R/axes_scaled_fit.R:139,140,149`; `R/axes_corrected_se.R:244,253,259`).
  The M71 AC1/AC2 block at master's `test-axes-scaled-fit.R:1258-1300` is
  present byte-for-byte in HEAD's file (verified by exact substring match; it
  sits at HEAD:1262 after an upstream hunk's +4 shift) and passes.
- **AC4** — the probes were run against a scratch checkout of `origin/master`
  with only the new test files copied in. Inflation form: the pre-milestone
  surfaces diverge from k = 7 at **both** diagonal positions 4 and 20
  (`se = "unidentified"` against `sf = NULL`), so the AC2 pins fail
  pre-milestone at two distinct positions. Non-inflation form
  (`sigma[4,4] <- 1e-3`, off-diagonals kept): measured pre-milestone, the
  raw-priced branch **survives** — `axes_se_pricing()` returns finite naive
  SEs, not a failure string — while the correlation-metric surface refuses,
  the opposite direction from the inflation form; the AC4 test's
  `"ill_conditioned"` pin fails pre-milestone (both surfaces answer
  `"indefinite"`). Whole-file run against pre-milestone code: red, capped at
  testthat's 10-failure limit with 23 more reported.
- **AC5** — `"ill_conditioned"` appears in the roxygen at
  `R/axes_reliability.R` (4 sites: the scaled-fit details block, the
  `se_correction_failed` and `fit_scaling_failed` return enumerations, and
  the internal enumeration comment), in the regenerated
  `man/axes_reliability.Rd` (2 sites), and in `NEWS.md`, whose bullet names
  the literal, the NA condition, and both relabelings
  (`"nonpositive_diagonal"` → `"singular"`, `"unidentified"` →
  `"infinite_diagonal"`).
- **AC6** — the assembly test (`tests/testthat/test-axes-reliability.R:3083`)
  injects a constructed degenerate matrix at the `axes_fitted_cov()` seam and
  passes: both warnings name `"ill_conditioned"`, `se_correction_failed` and
  `fit_scaling_failed` both carry it, `components$SE` and all four of
  `chisq`/`pvalue`/`rmsea`/`cfi` are NA, and `df`/`srmr` match
  `details$fit_uncorrected` and stay finite.

- **AC7** — `devtools::test()`: **FAIL 0 | WARN 5 | SKIP 3 | PASS 7231** (the
  5 warnings are in `test-ci_accuracy.R` and `test-ssm_sem.R`, files this
  branch does not touch). `devtools::check(args = "--no-manual")`:
  **Status: OK** — 0 errors, 0 warnings, 0 notes, 8m45s, test phase OK.
  `devtools::document()` warning-free and `man/`/`NAMESPACE` diff-free beyond
  the intended `man/axes_reliability.Rd` regeneration.

### Consistency gate

- `cairn_validate` exit 0 — all 16 CHECKs PASS. 47 advisory WARNs, every one a
  `work-log format` line in **M7**'s hard-wrapped log; none in M89.
- No `DESIGN.md` principle changed on this branch → `cairn_impact` skipped.
- Toolchain (`r-package` profile `consistency-gate` slot):
  `options(cli.width = 500); devtools::document()` emits **0** lines matching
  `resolve link` and leaves `man/` and `NAMESPACE` diff-free. `NEWS.md`
  carries the user-visible entry. No new top-level files, no new exports.

### Independent fresh-context review

Three distinct-evidence reviewers, then a fresh scorer that generated none of
the findings. 21 findings reported: 14 from the [O] diff-bug lens, 7 from the
[S] blame-history lens, 0 from the [S] prior-review lens — whose GitHub
inline-comment probe returned empty, so it read the archived `## Review`
sections on the touched files and found no point this diff reintroduces or
contradicts.

**Actioned (≥80): one.**

- **O1 (85) — `axes_scaling_factor()` refuses matrices it can price exactly;
  the criterion measures a property that surface never uses.** The rationale
  recorded at `R/axes_corrected_se.R:274-314` says both consumers build the
  information matrix from Σ̂⁻¹, so their error grows like p·κ(Σ̂)²·eps. That is
  false for `axes_scaling_factor()`: it runs `cov2cor()` first and every
  quantity it computes is a function of `cov2cor(Σ̂)` alone, so its error is
  governed by κ(cov2cor(Σ̂)). Reproduced independently at this review on HEAD:
  Σ̂ = D S D with D = diag(1e4, 1, …, 1) leaves `cov2cor(Σ̂)` identical to the
  well-conditioned S (κ = 10.4) while κ(raw) = 2.13e8 — pre-M89 the surface
  returned `scale = 0.9563346` (correct), HEAD returns `NA` /
  `"ill_conditioned"`. A pure diagonal rescaling, which the estimand is exactly
  invariant under, is now refused.
  **Triaged: follow-up, not fixed here.** Not an acceptance-criterion failure —
  AC1 asks that a single criterion be recorded and applied, AC2's grid
  agreement holds, and the Goal is met. It is also unreachable through the
  exported API today: every `axes_reliability()` path fits a correlation
  matrix, so `axes_fitted_cov(fit)` is near-unit-diagonal (κ measured 3.6–21.4
  on the probe fits, six orders below the 1.4e7 floor). And the repair is not a
  patch: pricing `cov2cor(Σ̂)` instead is exactly what the plan's T3 note ruled
  out as blind to the inflation regime M89 exists to close, so choosing a
  scale-invariant degeneracy measure re-opens the criterion's design — an
  `no-oracle` RB-tripwire question, not review-side work. Routed to the
  ROADMAP row this milestone graduated.

**Below the action bar (20), logged not actioned.** Highest first:

- O5 (75) — NEWS names two of the four user-visible literal changes; the
  exactly-singular (`"singular"` → `"ill_conditioned"`) and indefinite
  (`"indefinite"` → `"ill_conditioned"`) relabelings are unmentioned.
- O3 (65) — after the criterion runs first, no test asserts `"unidentified"`
  or `"indefinite"` as a returned reason from either surface; the two that did
  were flipped to `"ill_conditioned"`.
- O7 (60) — the load-bearing `p` factor in the cutoff is exercised only at
  p = 24; `probe_six()` and `probe_single()` go unused, so dropping `p` or
  writing `p^2` passes the whole M89 suite.
- S1 (55) — M70's declined reason-code parity is overturned with no recorded
  supersession link.
- S2 (55) — M71's declined `+Inf` relabel likewise.
- S7 (55) — the cutoff is analytically derived and self-validated, with no
  DECISIONS.md entry and no independent oracle.
- O6 (50) / S3 (50) — T4's "relax the emergent `solve()`-based refusals" never
  happened; `axes_se_pricing()` is byte-identical to master and the guards are
  merely made unreachable.
- O11 (50) — the NA/NaN diagonal now emits one warning instead of two; a
  user-visible console change absent from NEWS.
- O8 (45) — 64 of the AC2 grid's 68 points are non-discriminating.
- O10 (45) — the `df_mismatch` guard runs before the criterion on one surface
  only; pre-existing and unreachable through `axes_reliability()`.
- O4 (40) — `"ill_conditioned"` conflates indefinite, singular and ill-scaled;
  the milestone's Decision calls that one inequality carrying three cases.
- S5 (35) — a rewritten comment drops M70's `na.rm` fix attribution.
- O9 (35) — `eigen(symmetric = TRUE)` silently reads only the lower triangle.
- O14 (25) — `eigen()` runs twice per call on the same matrix.
- O12 (25) — the `Config/roxygen2/version` bump is a local-toolchain artifact.
- S4 (25) — the reviewer's own conclusion is that M69's contract is preserved.
- O2 (25) — claims AC4 unmet; AC4's own gloss ("`axes_scaling_factor()`
  refusing while the raw-priced branch survives") is satisfied as measured.
- S6 (20) — an M69 attribution comment now points at a moved route.
- O13 (15) — a cosmetic line-wrap artifact in the generated Rd.
