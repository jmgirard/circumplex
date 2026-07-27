# M65: FIML item-level missing data for `axes_reliability()` — the build

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** RR12
- **Principles touched:** —
- **Branch/PR:** `m65-axes-reliability-fiml-build` / —

## Goal

Give `axes_reliability()` a `missing = "fiml"` path built on the FIML
correlation metric, so item-level missingness is handled honestly under MAR
instead of forcing listwise deletion.

## Scope

**In:** `missing = c("listwise", "fiml")` on `axes_reliability()`, `"listwise"` default and bit-identical to
shipped; standardization by saturated-FIML (EM) moments on the `sqrt(N_used/(N_used − 1))` convention feeding one
structured one-stage fit through `sem_fit_cfa()`; OLS shadow and PD refusal retargeted to R̂; the six-clause refusal
contract; reporting and derived-quantity contracts; the nine-cell evidence bar; the roxygen and vignette rewrites.
Three RR12 §8 "Consider" items taken up at the plan gate: a soft warning on thin minimum pairwise joint coverage,
folding the reviewer probes into `devel/m64-fiml-probe.R`, and one doc sentence on the finite-sample diagonal
departure (M64-D3: expected restricted-ML behavior, not a defect).

**Out:** planned-missingness designs and lavaan's `missing = "two.stage"` → both stay ROADMAP candidate rows,
promoted on a concrete use case. Block membership on `circumplex_instrument` → its own candidate row.
Pairwise-deletion correlations stay banned (RR09 BC13, upheld by D-033). RR12 §9's four rejections (per-item
unit-total-variance constraint, post-hoc component rescaling, any scalar effective-N repair, pairwise correlations)
are standing and not revisited here.

## Acceptance criteria

- [ ] AC1 (BC1): `axes_reliability()` gains `missing = c("listwise", "fiml")` with `"listwise"` the default, matching `ssm_sem()`'s
      spelling; the `"fiml"` → lavaan `"ml"` translation goes through `sem_fit_cfa()`. Every pre-M64 test passes unchanged, and the
      listwise path's numbers are bit-identical to shipped.
- [ ] AC2 (BC2): Under `missing = "fiml"` the items are standardized by the saturated-model FIML (EM) means and by the FIML SDs
      rescaled by `sqrt(N_used/(N_used − 1))` — never by available-case `scale()` moments — and the reported fit is a single
      structured `lavaan::cfa(missing = "ml", orthogonal = TRUE)` on those columns. On complete data the standardized matrix must
      equal `scale(mat)` within 1e-12 elementwise.
- [ ] AC3 (BC3): On data with no missing cells, `missing = "fiml"` must reproduce `missing = "listwise"`'s ξ1, ξ2, ζ1 (and ζ2 when
      fitted), reliability, and SEm within 1e-8 each (measured: 5.6e-17 on ξ1 for the probe fixture).
- [ ] AC4 (BC4): A test must assert on the fitted FIML lavaan object that `lavInspect(fit, "options")$information[1] == "observed"`.
- [ ] AC5 (BC5): The reported component SEs and fit measures must come from the one-stage FIML fit; no SE or χ² computed from a
      correlation matrix with `sample.nobs` set to the total N may appear in `results`, `components`, `fit`, or any print/summary
      output.
- [ ] AC6 (BC6): Under `missing = "fiml"`, the OLS shadow (start values and stored cross-check) and the positive-definiteness
      refusal (min eigenvalue ≤ 1e-8, retained) must consume the saturated FIML correlation matrix R̂. On complete data R̂ must
      equal `cor(mat)` within 1e-12 elementwise (measured: 8.9e-16).
- [ ] AC7 (BC7): Each of the following must refuse informatively under `missing = "fiml"`, with a test per clause: (i) N_used ≤ p,
      where N_used counts rows with ≥1 observed item; (ii) an item with < 2 observed values, or zero variance among observed values;
      (iii) an item pair never jointly observed, naming at least one such pair (evidence V-F: lavaan silently fabricates the moment
      otherwise); (iv) saturated-stage non-convergence (mockable seam); (v) non-PD R̂; (vi) structured-fit non-convergence. Rows
      with no observed items are dropped with a message reporting the count and excluded from N_used.
- [ ] AC8 (BC8): Under `missing = "fiml"`: the startup message reports N_used, the complete-case count, any all-missing rows
      dropped, and the minimum pairwise joint coverage; `print()` reports the total N with the complete-case count alongside (the
      listwise path keeps `"Complete N:"`); `details` gains `missing` (read back from the fitted lavaan object via `lavInspect(fit,
      "options")$missing`, not echoed from the argument), `n_complete`, and the minimum pairwise coverage.
- [ ] AC9 (BC9): Under `missing = "fiml"`, `nb_reliability` is NA with `nb_reason` including `"fiml"` (accumulating with any other
      applicable reason), and `sd = "raw"` is refused with an informative error naming `"std"` and numeric SDs as the alternatives;
      `print()`/`summary()` state the reason.
- [ ] AC10 (BC10): On the probe population (8 octant scales × 3 items, ξ1 = .35, ξ2 = .10, ζ1 = .08, N = 600) at 2%, 5%, and 10%
      per-item MCAR, the mean ξ̂1 over ≥ 200 replicates must lie within 2 MC SEs of .35 in every cell, and the stored OLS shadow's
      ξ1 must agree with the CFA ξ̂1 within .05 in every replicate.
- [ ] AC11 (BC11): Under mechanism M1 (defined in this report's header: always-observed scale-1 anchors, P(miss) =
      plogis(qlogis(.12) + 1.5·x_anchor)), with ≥ 5 replicates at N = 2400 (or an MC-equivalent budget): the FIML-path mean ξ̂1 must
      lie within 3 MC SEs of .35, and the listwise mean ξ̂1 must differ from .35 by more than 3 MC SEs (measured: FIML-metric
      −0.0021 at MC SE 0.0023; listwise −0.0295 at MC SE 0.0067).
- [ ] AC12 (BC12): Under mechanism M2 (same-scale anchors, P(miss) = plogis(qlogis(.30) + 2.5·x_anchor_s)), paired over identical
      draws (≥ 4 replicates at N = 2000): mean[ξ̂1(available-case-standardized one-stage) − ξ̂1(shipped FIML path)] must be ≥ +0.010
      (measured +0.0167, paired SE 0.0006), and mean|ξ̂1(shipped FIML path) − ξ̂1(two-stage fit of the FIML correlation matrix)|
      must be ≤ 0.005 (measured 0.0008, paired SE 0.0012).
- [ ] AC13 (BC13): At 5% and 10% per-item MCAR on the probe population, the mean reported FIML SE of ξ1 must be smaller than the
      mean reported listwise SE, with the FIML/listwise ratio decreasing from 5% to 10%; and at 5% MCAR over ≥ 200 replicates, the
      ratio of the mean reported FIML SE to the empirical SD of ξ̂1 must lie in [0.85, 1.15]. If the ratio falls outside the band,
      the milestone must surface it in the "Deviations from RR12" table with a strengthened documented SE caveat — never widen the
      band silently.
- [ ] AC14 (BC14): On the F1b fixture (probe population, N = 600, 15% per-item MCAR, the pinned probe seed): `missing = "listwise"`
      refuses with the N ≤ p error; `missing = "fiml"` returns a converged, non-boundary estimate with |ξ̂1 − .35| ≤ .05 (measured
      ξ̂1 = 0.3573, SE 0.0174).
- [ ] AC15 (BC15): One pinned crossed-blocks cell (8 scales × 3 items, `axes_crossed_blocks()`, truth ξ1 = .30, ξ2 = .10, ζ1 = .06,
      ζ2 = .05, N = 2000, 5% per-item MCAR): the FIML path fits the five-component model with each of ξ̂1, ξ̂2, ζ̂1, ζ̂2 within 3
      reported SEs of its truth (measured: .2979/.1019/.0639/.0490 with SEs .0080/.0048/.0037/.0026).
- [ ] AC16 (BC16): The roxygen missing-data paragraph and the vignette caveat paragraph are rewritten to state: listwise remains the
      default; `missing = "fiml"` assumes MAR **and** multivariate normality; under MCAR listwise is consistent (inefficient, not
      biased); the FIML SEs are observed-information SEs on the standardized metric, conditional on the standardization constants,
      and approximate for the same correlation-as-covariance reason as the shipped path; and the FIML variant is certified by the
      package's synthetic oracle, not by Strack et al. (2013), who report no missing-data analyses.
- [ ] AC17: the profile's `verify` slot clean (`devtools::test()`, `document()` no diff) and, because this milestone
      edits roxygen, a full `devtools::check(manual = TRUE)` whose log carries `checking PDF version of manual ... OK`
      by name (M7/M57 lesson: a bare `check()` defaults to `manual = FALSE` and skips that step).

## Coverage

- AC2, AC6 → T1
- AC1, AC3, AC4, AC5 → T2
- AC7 → T3
- AC8, AC9 → T4
- AC11, AC12, AC14, AC15 → T5
- AC10, AC13 → T6
- AC16 → T7
- AC17 → T8

## Tasks

- [x] **T1** — Metric layer: saturated-FIML (EM) means/SDs with the `sqrt(N_used/(N_used − 1))` rescaling and R̂
      built from them (route per M65-D1); complete-data equality tests vs `scale(mat)`/`cor(mat)` at 1e-12.
- [x] **T2** — API + one-stage wiring: `missing =` on `axes_reliability()` (`R/axes_reliability.R:819`) through the
      existing `axes_fit(missing =)` → `sem_fit_cfa()` translation (`R/ssm_sem.R:749`); observed-information
      assertion; a test that no two-stage SE or χ² reaches `results`/`components`/`fit`/print.
- [x] **T3** — Six-clause refusal contract, all-missing-row drop, soft thin-coverage warning (M65-D2). Per the M62
      lesson, clause (iv)'s mock proves wiring only — assert each condition unmocked. Per M60, re-assert what the
      complete-case N ≤ p and PD checks refused *incidentally* before they move to N_used and R̂.
- [x] **T4** — Reporting and derived quantities: startup message, `print()`, `details` read-back via `lavInspect()`,
      `nb_reliability`/`nb_reason`, and the `sd = "raw"` hard error (D-034 correction 2 — an error, not an NA).
- [ ] **T5** — In-suite evidence cells: BC11 MAR reversal, BC12 metric falsification, BC14 headline, BC15 ζ2.
- [ ] **T6** — Heavy-cell harness (M65-D3): seed-pinned `devel/` script runs BC10's three cells and BC13's ≥200
      replicates once (~45 min measured), summary committed as `.rds`; a fast test asserts the stored summary against
      BC10/BC13 **and** re-runs ~10 replicates live. Comment what the live half does not cover.
- [ ] **T7** — Docs: roxygen missing-data paragraph (`R/axes_reliability.R:681-682`), vignette caveat
      (`vignettes/axes-reliability.Rmd:154-157`), extended SE caveat, diagonal-departure sentence, NEWS entry. Run
      the M63 **two-way** enumeration sweep — grep the old claim to delete *and* every enumeration to extend.
- [ ] **T8** — Fold reviewer probes into `devel/m64-fiml-probe.R`; full `check(manual = TRUE)`; post-merge hygiene.

## Work log

- 2026-07-26: created by /milestone-plan. Promotes the ROADMAP candidate GO'd by D-033 (corrected by D-034); ingests RR12 BC1–BC16 verbatim as AC1–AC16. Gate choices: one oversized milestone over a build/evidence split (the binding-criteria check binds all 16 to one file; M54 precedent); heavy BC10/BC13 cells as script + stored `.rds` + live smoke; all three RR12 §8 "Consider" items in scope.

- 2026-07-26: started (/milestone-implement). Branch `m65-axes-reliability-fiml-build` cut from master at 1552b031; no dependencies to verify. Status planned→in-progress. Implement-gate choices recorded as M65-D1 (saturated moments via lavaan `h1` EM — an explicit saturated fit misses BC2/BC6 by five orders, measured), M65-D2 (soft overlap warning at 30), M65-D3 (~10-replicate live smoke, never skipped).
- 2026-07-26: minor amendment (wording only, no scope change) — Tasks compressed from 25 lines to 18 by cutting duplicated lesson prose to pointers, after the M65-D1..D3 block left the plan-owned body at 148/149 with eight tasks still to run. No task added, removed, reordered, or rescoped.
- 2026-07-26: T1 done. `R/axes_fiml.R` adds `axes_fiml_h1()` (the EM seam) and `axes_fiml_moments()`; `tests/testthat/test-axes-fiml.R` adds 14 assertions. BC2 measures 2.2e-15 and BC6 1.1e-15 against the 1e-12 bar. Two findings, both from ablating rather than asserting: (a) `lavInspect(fit, "converged")` reports FALSE on a HEALTHY saturated fit — it describes the structured optimizer, which this stage never runs — so `axes_converged()` would have refused every dataset; the seam now listens for lavaan's EM iteration-cap warning and checks moment finiteness instead. (b) The `ordered = character(0)` pin is a measured no-op (0 difference on a 5-point integer Likert fixture; non-numeric input is refused upstream) — comment corrected to claim a pin, not a fix, per the M36 lesson. The N-1 rescaling is mutation-verified: removing it reddens BC2 at 1.8e-3, nine orders above the bar. Full suite 0 failures / 4076 passing / 4 pre-existing warnings, count unchanged.
- 2026-07-26: session paused at the T1 commit (user's gate choice). M65 stays `in-progress`; branch `m65-axes-reliability-fiml-build` is pushed at 1568a52f with T1 done and T2-T8 open. Resume at T2, which is entangled with T3 — both rewrite the same data-preparation region of `axes_reliability()` (`R/axes_reliability.R:967-1051`), since BC7's N_used ≤ p refusal replaces the complete-case one — so they are best taken together in one sitting.

- 2026-07-26: gated Coverage amendment — AC3 moves from T1 to T2 (`AC2, AC6 → T1`; `AC1, AC3, AC4, AC5 → T2`). AC3 compares the two `missing =` paths, and the argument does not exist until T2, so T1 could not have produced its evidence. No criterion text, task, or scope changed. User approved at the T2 implement gate.
- 2026-07-26: T2 done. `missing = c("listwise", "fiml")` on `axes_reliability()`, `match.arg`'d to `ssm_sem()`'s spelling; the FIML branch standardizes by `axes_fiml_moments()` and fits one `axes_fit(missing = "fiml")` through `sem_fit_cfa()`. `axes_fiml_coverage()` split out of `axes_fiml_moments()` because order is load-bearing — T3's pattern-readable refusals must fire before the EM stage, since lavaan fabricates an unidentified moment rather than failing (V-F). `missing = "fiml"` with `cormat` is refused (a seventh refusal, outside BC7's six: no rows to run EM over). AC3 measures agreement well inside 1e-8 on components, reliability, and SEm, four-component and five-component (zeta2) cells alike. AC4 asserts observed information on the object the function actually fitted, captured through the `axes_converged()` seam rather than on a parallel fit; the listwise branch is fired through the same capture and required to read back `"listwise"`, so the assertion cannot pass vacuously. AC5 pins the ban two ways: `axes_fit_cormat()` mocked to abort is never reached, and unmocked the reported chi-square and xi1 SE both differ from the banned two-stage refit of R-hat at total N. Full suite 0 failures / 4096 passing / 4 pre-existing warnings, count unchanged.

- 2026-07-26: T3 done. All six BC7 clauses refuse, plus the all-missing-row drop and M65-D2's soft warning at 30. Clauses (i)-(iii) fire before the EM stage, which is the load-bearing order: lavaan fabricates an unidentified moment rather than failing (V-F). Two findings, both from firing the clauses on real data rather than assuming. (a) M65-D4: lavaan's default `em.h1.iter.max = 500` made clause (iv) refuse estimable data — an item at 40/300 coverage stalled at the default and converges in 0.23 s with room; the cap is now 50000 and healthy data is unaffected because EM exits on tolerance. (b) Clause (v) is NOT reachable end to end under FIML: R-hat is a cov2cor of an EM ML covariance, hence PSD by construction, and the EM's 1e-5 tolerance leaves ~1e-8 of residual noise, so an exactly duplicated item pair gives R-hat[1,2] = 1 with minimum eigenvalue 1.12e-08 — just above the retained 1e-8 floor (3.9e-16 for the same data listwise). Such data is still refused, by clause (vi). Tested at the seam on the M62 precedent, with a second test recording the end-to-end landing so the mock is never read as an end-to-end guarantee; the floor stays 1e-8 per AC6, since a FIML-specific floor would be a calibration RR12 did not do. Also re-asserted (M60): the listwise path still refuses on its own complete-case N and its own `cor()`. Full suite 0 failures / 4126 passing / 4 pre-existing warnings, count unchanged.

- 2026-07-26: T4 done. Reporting: the BC8 startup message (landed with T3's row drop) reports N_used, complete cases, dropped rows, and minimum pairwise coverage; `print()` labels the FIML N `Total N:` with the complete-case count beside it, while listwise keeps `Complete N:` and cormat keeps `Sample N:` — three quantities, three labels. `details` gains `missing` (read off the fitted object via `axes_lav_missing()`, mirroring `sem_details()`, never echoed from the argument), `n_complete`, and `min_coverage`; the latter two are NA on the paths where they carry no information rather than absent, so a caller can read them unconditionally. Derived quantities: `nb_reason` gains `"fiml"` and accumulates with `cormat`/`single_item` (asserted on a single-item FIML instrument carrying two reasons at once), and `sd = "raw"` is a hard error naming both alternatives (D-034 correction 2) — the available-case composite SD is computable and plausible-looking, which is exactly why it must not be silently reported. Scale composites are no longer computed at all under FIML. Full suite 0 failures / 4150 passing / 4 pre-existing warnings, count unchanged.

## Decisions

- **M65-D1 (2026-07-26): saturated moments come from lavaan's `h1` EM estimate, not an explicit saturated fit.**
  BC2/BC6 demand 1e-12 agreement with `scale(mat)`/`cor(mat)` on complete data. Measured on the BC10 fixture
  (24 items, N = 600): an explicit saturated `lavaan::sem()` reaches only 1.3e-07 and costs 9.3 s — the general
  optimizer's convergence tolerance, five orders short, and no model tuning fixes that. `lavCor(dat, missing =
  "ml", output = "fit", meanstructure = TRUE)` → `lavInspect(f, "h1")` returns `$mean` and `$cov` from lavaan's
  EM in 0.11 s at 2.2e-15 / 1.1e-15, reproducing RR12's measured 8.9e-16, and is exported API (no `:::`). BC7
  clause (iv)'s mockable seam attaches to this call.
- **M65-D2 (2026-07-26): soft overlap warning at 30 jointly-observed respondents.** RR12 §7 binds no floor and
  says any positive constant is arbitrary, so 30 is taken as the *conventional* small-sample floor for a
  correlation, not a quantity derived here — the message names the number and the docs call it a convention with
  no inferential meaning. The hard zero-coverage refusal (BC7 iii) is untouched.
- **M65-D3 (2026-07-26): the BC10/BC13 live smoke runs ~10 replicates and is never skipped.** The ≥200-replicate
  statistics come from a seed-pinned `devel/` script with a committed summary; the in-suite half re-runs ~10
  replicates (~30 s at the measured 3.1 s/fit) with no `skip_on_cran()` — a skip flag here would repeat the
  failure this repo has hit four times (M7 `--no-manual`, M16 `skip_on_cran()`, M31 vdiffr, M39 CI baseline):
  green because it never ran. The test comments what the live half does *not* cover, so the stored summary is
  never mistaken for something CI re-derives.
- **M65-D4 (2026-07-26): the saturated EM runs to a 50000-iteration cap, not lavaan's default 500.**
  At the default, clause (iv) refused estimable data: on the 24-item probe population (N = 300), one item at 40/300 coverage stalls at 500
  and converges in 0.23 s at 2000; at 20/300, 0.35 s at 50000; at 25/300, ~50000 and 10.6 s. Healthy 10%-MCAR data is unaffected either way —
  EM exits on tolerance, not the cap. The refusal is retained and matters more, not less: at 25/300 the cap-10000 iterate's covariance sits
  3.93 from the converged answer on unit-variance items. Cost: a genuinely stuck dataset now waits ~11 s to be refused instead of 0.15 s.

## Review
