<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M147: The certificate is the sole conditioning judge

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2, GP4
- **Resolves:** —
- **Surface tier:** user-facing — it changes which refusal literal and warning `axes_reliability()` emits on an ill-conditioned fit below the floor
- **Branch/PR:** m147-certificate-sole-conditioning-judge

## Goal

The per-fit accuracy certificate alone decides whether an `axes_reliability()` fit computes or refuses on conditioning, so the platform-dependent `"unidentified"` refusal that RR22 measured at counterexample B leaves the shipped package.

## Scope

**In:** `axes_pricing_core()` (R/axes_corrected_se.R:164) inverts the information matrix with `solve(info, tol = 0)` on the certified path. A reciprocal condition estimate below `.Machine$double.eps` no longer refuses. It selects the fit for the certificate, as the floor on `sigma` does since M111, whether or not that floor fired. The selector lives in `axes_degeneracy_refusal()` behind the M117 once-per-fit seam. A pair of bit-identical derivative matrices in `d$mats` refuses `"unidentified"` by an exact check with no tolerance, so the structural case keeps a platform-stable literal. A sweep oracle with the exact-rational driver shows the certificate under-reports at no matrix the old gate refused. The counterexample-B test admits a refusal only as an exact zero pivot, and the committed `rcond` band and its constant admission predicate are deleted. NEWS, roxygen and the refusal-vocabulary comments follow.

**Out:** the raw lavaan-tie arm of `axes_corrected_se()` keeps the default tolerance, because the certificate never prices raw Sigma-hat (D-037) and the arm is never user-reported. `solve(sigma)`'s default tolerance is unchanged; the sweep records `rcond(sigma)` so the claim that it never bites above the floor is measured, and a hit becomes a candidate row. The five remaining accuracy-certificate fragilities in DESIGN.md → M148. An `ubuntu-24.04-arm` CI job → the RR22 follow-ons candidate row. A platform survey of zero pivots at counterexample B beyond the arm64 container → not done; the two-route test shape covers it.

## Acceptance criteria

- [ ] AC1: `axes_pricing_core()` returns `"unidentified"` from two sites in its body and no other, which a source read of the function enumerates. The first site fires on a pair of bit-identical matrices in `d$mats`. The second fires on a non-finite or exact-singular inversion of the information matrix under `solve(info, tol = 0)`. On the certified path a reciprocal condition estimate of the information matrix below `.Machine$double.eps` no longer refuses. It routes the fit to `axes_accuracy_certificate()` through `axes_degeneracy_refusal()`, once per fit, whether or not the floor on `sigma` fired. The raw arm of `axes_corrected_se()` keeps the default tolerance, and its refusal still lands in `naive_reason`. Tests at both surfaces (`axes_corrected_se()` and `axes_scaling_factor()`) assert the identity of each ground. The duplicate-matrix ground is asserted at the one-scale construction of the M89 T10 tests and at three variants: two duplicate pairs, a non-adjacent pair, and a pair reaching the raw arm. The routed ground is asserted at a rank-deficient design with no duplicate pair (one item per scale with zeta1 fitted, so the error matrices sum to the zeta1 matrix). That design refuses under `"uncertified"`, or under `"unidentified"` only on an exact-singular inversion, and the test asserts which identity the route it took carries.
- [ ] AC2: A seeded sweep script `devel/degeneracy-oracle/tol0_sweep.R` enumerates its own domain in four families: the six committed certificate cases, the RR18 Q4 fixture-perturbation sweep, the M106 reachable and near-duplicate families on a kappa grid crossing the floor, and a seeded random model-implied family at p in {4, 8, 9, 16, 24}. Per matrix it records the answer of the floor, `rcond(sigma)`, `rcond(info)`, the default-tolerance inversion outcome, the `tol = 0` outcome, the shipped refusal predicate's answer and, where the default tolerance refused and `tol = 0` inverted, the exact-rational oracle's true relative error and the certificate's estimate. Its pre-registered acceptance is stated in the script header and holds on the committed run (`tol0-sweep-results.rds` beside it, regenerable by the script). (a) At every matrix the default tolerance refused and `tol = 0` inverted, the certificate's estimate is not below the true relative error, so each such matrix refuses `"uncertified"` or computes with a true error inside delta_star. Every matrix both tolerances refused is refused `"unidentified"`. (b) Every matrix the floor admits (NULL) whose design fits zeta1 only with two or more items on every scale was inverted under both tolerances with `rcond(info)` at least four decades above `.Machine$double.eps`. (c) Every matrix inverted under the default tolerance returns bit-identical `si`, `sim` and `acov` under `tol = 0`. (d) No family is empty.
- [ ] AC3: In `tests/testthat/test-axes-certificate.R` the committed `rcond` band and the admission predicate are gone. A refusal at any case, counterexample B included, is admitted only as an exact-singular inversion. The refusing route asserts `"unidentified"` from both pricings and that `solve(info, tol = 0)` on the same information matrix raises LAPACK's exact-singular condition. The priced route and the detector's requirement (an anchor priced and cxb in {priced, refused}) hold as before.
- [ ] AC4: Every site the search `grep -rn "unidentified" R/ tests/ vignettes/ NEWS.md` returns is restated for the new grounds or dispositioned as unchanged. NEWS.md states the user-visible change. Below the floor, a fit that a platform's inversion used to refuse `"unidentified"` now refuses `"uncertified"` with the estimated error in the warning, or computes when its certificate passes. A design with duplicate derivative matrices still refuses `"unidentified"`. No number previously reported changes, with AC2 (c) as the evidence.
- [ ] AC5: `devtools::test()` and `devtools::check(args = "--no-manual")` are clean. `tools/arm64/testfile.sh` runs `test-axes-certificate.R`, `test-axes-certificate-refusal.R`, `test-axes-corrected-se.R` and `test-axes-scaled-fit.R` green in the linux-arm64 container. If the image is absent, it is built per `tools/arm64/README.md` first.

## Coverage

- AC1 → T3, T4
- AC2 → T1, T5
- AC3 → T3, T6
- AC4 → T7
- AC5 → T8

## Tasks

- [x] T1: Write `devel/degeneracy-oracle/tol0_sweep.R` with design-agnostic columns (the floor, `rcond(sigma)`, `rcond(info)`, both tolerances, the current predicate, the exact oracle and the certificate in the refused-then-inverted region). State the pre-registered acceptance (AC2 (a) to (d)) in the header. Run it at HEAD, commit the results and a short `.md` summary. Families and their seeds are named in the script.
- [ ] T2: `/milestone-brief` on the selector design with T1's numbers (RB tripwire: ip-touching). Questions for the brief: whether the certificate is licensed where `rcond(info) < eps`, whether the selector belongs in `axes_degeneracy_refusal()`, and whether the exact duplicate check is the right structural guard. Ingest the RR; amend the plan through the gate if it departs.
- [ ] T3: Regression tests first. Duplicate-pair identity at both surfaces with the three variants of AC1; the rank-deficient no-duplicate case with its two admitted identities; the raw-arm refusal landing in `naive_reason`; the counterexample-B two-route re-grounding of AC3; a source guard pinning `tol = 0` and the two return sites.
- [ ] T4: The core change. The duplicate-pair check; `solve(info, tol = 0)` on the certified path with the default tolerance on the raw arm; the `rcond` selector in `axes_degeneracy_refusal()` with the core's result threaded to both surfaces so a fit is inverted once (M117 seam, `axes_shared_refusal()` R/axes_corrected_se.R:842); the refusal-vocabulary comments at R/axes_reliability.R:2044 and R/axes_scaled_fit.R:291 restated. (RB tripwire: ip-touching)
- [ ] T5: Re-run the sweep against the shipped predicate. Commit the results and confirm (a) to (d).
- [ ] T6: Delete `cert_admission` and `cert_refusal_admitted()`; correct the DESIGN.md Known-fragilities paragraph in place (`corrected M147`) so the constant-predicate item is retired and the count reads five.
- [ ] T7: The prose sweep of AC4, roxygen for `?axes_reliability`, `devtools::document()`, the NEWS entry.
- [ ] T8: The arm64 container run of AC5 (build the image first if absent), then `devtools::test()` and `devtools::check(args = "--no-manual")`.

## Work log

- 2026-09-21: created by /milestone-plan from the candidate row "Let the certificate be the sole conditioning judge" (RR22 rec 9, C′; D-055 lineage; M111, M117, M122 archives).
- 2026-09-21: measured at planning: the M89 T10 one-scale construction gives `rcond(info)` exactly 0 and an exact zero pivot with or without `tol = 0` here, but an 8x8 toy with two identical rows and columns priced under `tol = 0` on this machine; a one-item-per-scale design with zeta1 fitted passes the floor at `rcond(info)` 2.9e-19; `solve(a)` and `solve(a, tol = 0)` agree bit for bit where both succeed; the certificate costs 0.18 s at a clean p = 24 fit against 0.002 s for the pricing.
- 2026-09-21: criteria audit ran in full mode ([O] fresh reader). Seven findings. Five fixed at the gate: the raw arm keeps the default tolerance and its refusal is asserted; AC2 (b) restated to designs with two or more items per scale under zeta1 (the rank-deficient design falsified the draft); "no computed number changes" restated as "no number previously reported changes"; the duplicate-pair probes vary form; instrument clauses moved to tasks. Two posed at the gate: certificate licensing where `rcond(info) < eps` (answered by the exact oracle in AC2 (a) and the brief in T2) and the counterexample-B test shape.
- 2026-09-21: plan gate chose the selector design over pure `tol = 0` as RR22 wrote it because a floor-admitted design exists at `rcond(info)` 2.9e-19 and `tol = 0` priced a singular toy matrix here; falsified by a proof that LU under `tol = 0` refuses every structurally singular information matrix on every platform.
- 2026-09-21: plan gate chose two admitted routes at counterexample B with the band retired over a single priced route because a zero pivot there is a platform outcome the suite's own rule forbids asserting away; falsified by a survey showing no zero pivot at B under `tol = 0` on every CRAN flavor.
- 2026-09-21: plan gate chose the default tolerance on the raw arm over `tol = 0` there because the certificate never prices raw Sigma-hat; falsified by a certificate extended to the raw arm.
- 2026-09-21: plan gate chose a Fable brief after the sweep (T2) over Fable at review only because the certificate's behavior where `rcond(info) < eps` is measured, not proven; a timing choice, falsified by nothing.
- 2026-09-21: plan gate chose M148 for the five remaining fragilities over folding them in, on the sizing tripwire.
- 2026-09-21: implement started on branch m147-certificate-sole-conditioning-judge. Question gate: the refusal object carries the pricing core to both surfaces (a `core` argument on the pricing helpers); `axes_pricing_core()` gains `tol = 0` and the raw arm passes `.Machine$double.eps`; the core returns `rcond(info)` for the selector; T2's brief runs after T1 as planned.
- 2026-09-21: T1 done. Sweep at HEAD over 271 matrices (`tol0-sweep-summary.md`): (a), (c), (d) pass; 85 region matrices, the certificate over the true error by a factor of 10.0 at every graded one, none under-reported, 37 of them compute in the tol0 world with true error at most 9.9e-6; no floor-admitted matrix in any family has `rcond(info)` below eps. (b) FAILS at one matrix, the p = 3 Q4 perturbation `t=0.9999750` (floor-admitted, `rcond(info)` 9.4e-13, 3.6 decades above eps), a design the exported API cannot reach; at p >= 4 the minimum is 6.4e-9. AC2 (b) needs an amendment (four or more scales) at the gate after T2's ingestion. Scratch measurement for the brief: the one-item-per-scale design with zeta1 forced fitted passes the floor at `rcond(info)` 1.7e-17, inverts under tol = 0 and computes with reason NULL while its certificate reads se 4.69; the selector is what refuses it.

## Decisions

## Review
