# M64: FIML on items for `axes_reliability()` — the estimator-metric question

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m64-axes-reliability-fiml-review` / —

## Goal

Settle under independent Fable review whether `axes_reliability()` can honestly
offer full-information maximum likelihood on item data, and record the verdict,
so the build is either cleared with binding criteria or dropped on stated
grounds.

## Scope

**In:**
- `devel/m64-fiml-probe.R`: the plan-gate probe, seed-pinned and committed, so
  RB12's reviewer and any later build can re-run its four findings.
- RB12 (`/milestone-brief`) on the one question this session cannot settle:
  whether available-case z-standardization puts this model on the correlation
  metric under incomplete item data, given that the *complete*-data fit already
  departs from a unit implied diagonal.
- RR12 ingestion per the brief protocol, every recommendation dispositioned.
- The GO/NO-GO `DECISIONS.md` entry. On GO it also narrowly supersedes D-001
  for this feature and takes up D-026's last deferral — the D-025/D-030/D-031/
  D-032 template — citing the measured deletion cost as the use case D-026
  asked for. No plan-time scope-admission entry is authored: M64 ships no
  feature, and whether the build rides v2.0.0 is the GO entry's to decide.

**Out:**
- Any package code, test, or NEWS entry — docs-only. The build is planned
  post-GO from RR12, as M54 was from RR09; on NO-GO it is dropped at AC5.
- Deciding the build's design. The three positions fixed at this plan gate
  (one-stage FIML through `sem_fit_cfa`; N–B and `sd = "raw"` reported
  unavailable-with-reason; a synthetic evidence bar carrying a non-MCAR cell)
  enter RB12 as positions for review to confirm or overturn, not as settled.
- Block membership on the `circumplex_instrument` class → stays on the
  axes-reliability candidate row. The quasi-circumplex refusal → untouched,
  standing on RR09 §4.

## Acceptance criteria

- [ ] **AC1** `devel/m64-fiml-probe.R` reproduces all four plan-gate findings
      from a clean `Rscript` session, seed-pinned, printing each figure RB12
      quotes: the saturated mean structure (24 free intercepts, df unchanged at
      273), the complete-data implied-diagonal departure (max |v − 1| = 0.046),
      one-stage vs two-stage agreement stated comparatively — the routes' ξ1
      differing by under 5% of ξ1's own SE at 2/5/10% per-item MCAR (measured
      0.9 / 3.6 / 2.8%), which is the discriminating bound since materially
      disagreeing routes would sit about one SE apart — and the deletion-cost
      shares (p = 64 → 0.53 / 0.27 / 0.038 complete cases at 1 / 2 / 5%).
- [ ] **AC2** `cairn/reviews/archive/RB12-*.md` is self-contained: it states the
      question, all four AC1 findings with the command producing each, the
      three design positions fixed at this gate, and what a verdict must decide
      — the metric, what the reported components mean under FIML, and whether
      the FIML-consistent correlation matrix is the right input for the OLS
      cross-check (`axes_ols_shadow()`) and the positive-definiteness refusal.
- [ ] **AC3** `cairn/reviews/archive/RR12-*.md` is committed and ingested per
      `/milestone-brief`'s protocol, with every recommendation carrying a
      disposition (apply / consider / reject) and a reason in this file's
      Decisions section.
- [ ] **AC4** A `cairn/DECISIONS.md` entry records GO or NO-GO. On GO it names
      the binding criteria the build ingests verbatim, supersedes D-001 narrowly
      for this feature, and takes up D-026's FIML deferral citing the measured
      use case. On NO-GO it records the refusal and its rationale.
- [ ] **AC5** `cairn/ROADMAP.md` reflects the verdict: on GO a build candidate
      row naming `Driving RR: RR12`; on NO-GO the FIML item retired with a
      pointer to AC4's entry. Either way FIML leaves the axes-reliability
      extensions row with its lineage noted.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T4

## Tasks

- [x] **T1** — Lift the three plan-gate scratchpad probes into
      `devel/m64-fiml-probe.R`: one section per finding, seed-pinned, each
      printing the figure AC1 quotes. Fix probe 1's dead `pe$free` filter
      (`parameterEstimates()` has no `free` column, so the intercept count read
      0 — the LESSONS (f) family) by counting `coef()` names.
- [x] **T2** — Author RB12 via `/milestone-brief` from T1's output, carrying the
      four findings, the three fixed positions, and the verdict questions in
      AC2. Note for the brief: `ssm_sem()` already exports
      `missing = c("listwise", "fiml")` (`R/ssm_sem.R:1303`), so the argument
      spelling is precedent, not a proposal.
- [x] **T3** — Ingest RR12: disposition every recommendation, and record the
      binding criteria verbatim for the build to ingest.
- [x] **T4** — Author the GO/NO-GO D-entry; update the ROADMAP per AC5 and
      graduate FIML off the axes-reliability extensions row.

## Work log

- 2026-07-26: created by /milestone-plan. Jeff chose escalation over a direct build at the gate, so this milestone produces the verdict and the build is planned from RR12 (the M53→M54 shape). D-026's FIML clause is a *deferral*, not a rejection (tracking-rules: "not now" is a ROADMAP fact), so planning this needs no supersession; the scope-admission + deferral takeup belong to the GO entry at T4. Investigation dissolved two of the candidate row's three worries: lavaan frees all 24 item intercepts itself under `missing = "ml"` (npar 27→51, df unchanged at 273), so the mean structure is saturated and imposes nothing; and the unit-variance concern is not FIML-specific — the complete-data fit's implied per-item variances already depart from 1 by up to 4.6%, against 4.8–6.5% under FIML, with the reported components still summing to 0.9994. What survives is the metric question RB12 asks.
- 2026-07-26: started (/milestone-implement). Branch `m64-axes-reliability-fiml-review` cut from master at `ec420791`; no dependencies to check. Status planned→in-progress. No pre-implementation question gate: the plan fixed the three design positions as RB12 inputs, and the Fable spawn is gated per-instance inside /milestone-brief at T2.
- 2026-07-26: T1 done. `devel/m64-fiml-probe.R` reproduces all four findings seed-pinned — F1 deletion shares, F1b the shipped refusal, F2 the saturated mean structure, F3 the implied-diagonal departure, F4 route agreement. Fixed the scratchpad probe's dead `pe$free` filter by counting `coef()` names instead: `parameterEstimates()` has no `free` column, so the intercept count printed 0 where the true count is 24 (LESSONS (f), the probe's own syntax lying). The committed script prints 12 complete cases at 15% MCAR where the scratchpad printed 16 — a different missingness seed, and no criterion quotes that figure.
- 2026-07-26: AC1 AMENDED (gated) — its one-vs-two-stage agreement bound read 5e-4 and the committed script measures 6.2e-4, so the criterion would have failed review as written. Replaced with a comparative bound at Jeff's gate choice: the routes' ξ1 differ by under 5% of ξ1's own SE (measured 0.9 / 3.6 / 2.8%), ~14× headroom to the one-SE disagreement it must rule out. The M59/M61 tolerance lesson recurring inside a plan that had cited it.

- 2026-07-26: status in-progress→**blocked** on RB12 (`cairn/reviews/RB12-axes-reliability-fiml-metric.md`), nine questions: the metric under FIML, the complete-data implied-diagonal departure, the saturated mean structure's effect on ξ1's SE, one-stage vs two-stage, the OLS-shadow/PD-refusal input, N–B and `sd = "raw"`, the FIML refusal set and the `"Complete N:"` label, the evidence bar, GO/NO-GO. Committed on the milestone branch rather than master: M64's whole deliverable is this RB/RR pair, so splitting it off would fragment the branch T1 already sits on.

- 2026-07-26: T2+T3 done. RB12 spawned to Fable at Jeff's gate; RR12 returned **GO under BC1–BC16** and is ingested — eight decisions recorded above, every recommendation triaged, pair archived. The review overturned the plan's standardization (available-case `scale()` is MAR-dishonest, measured ≈1 SE) while confirming one-stage FIML, closed question 2 without a spin-out, and strengthened the N–B ruling. One RR12 evidence note corrected against an independent re-run (M64-D6). Status blocked→in-progress.

- 2026-07-26: minor amendment — AC2/AC3 now name `cairn/reviews/archive/`, since the brief protocol archives the RB/RR pair at ingestion and the plan-time paths would have pointed review at files that no longer sit there. No change to what either criterion verifies.

- 2026-07-26: T4 done. **D-033 = GO** appended to `cairn/DECISIONS.md`: narrow D-001 supersession, takes up D-026's last deferral citing the measured deletion cost, carries the metric holding (saturated-FIML standardization, never available-case `scale()`), and fixes BC1–BC16 as binding on the build. ROADMAP gains the build candidate row (`Driving RR: RR12`, with M64-D8's replicate-cost note) and a post-build follow-ons row (planned-missingness designs; lavaan `missing = "two.stage"`); the extensions row's FIML lineage now records the GO and that D-026's deferral list is empty. M7 deliberately does not gain a dependency, on D-030's reading.

- 2026-07-26: status in-progress→review (/milestone-implement). T1–T4 done; the branch is four commits over five files and touches **no package surface** (`R/`, `tests/`, `man/`, `vignettes/`, `NAMESPACE`, `DESCRIPTION` and `NEWS.md` all untouched), so the profile's verify slot has nothing to run and there is no NEWS entry to owe — M64 is docs-only by design and the user-visible change ships with the build.

## Decisions

- **M64-D1 (2026-07-26, RR12 §1) — the metric is FIML, not available-case.** RR12 confirms one-stage FIML and **overturns the standardization** the plan submitted with it: available-case `scale()` is MCAR-honest but MAR-dishonest, because the standardized columns carry `k_i·k_j·ρ_ij` and the model has no free off-diagonal per-item parameter to absorb it. Measured under mechanism M2: +0.0167 above the FIML-metric estimate (paired SE 0.0006), ≈1 SE at N = 600, while the two metric-correct routes agree to +0.0008. The build standardizes by saturated-FIML (EM) moments with a `sqrt(N_used/(N_used − 1))` convention that reproduces `scale()` exactly on complete data.
- **M64-D2 (2026-07-26, RR12 §4) — one-stage only.** Two-stage R̂ is internal machinery; its SEs and χ² never surface. `sample.nobs = N_total` overstates information, and no scalar effective N repairs it (the loss is parameter- and pattern-specific). F4's near-equal SEs are an artifact of mild missingness and must not be cited as validating the convention (RR12 B-2).
- **M64-D3 (2026-07-26, RR12 §2) — the complete-data diagonal departure is not a defect.** Expected restricted-ML behavior, verified at the stationarity condition (weighted diagonal 8.0e-07 against a raw 0.0448) and vanishing on the population (2.95e-13, ξ1 recovered exactly). No correction milestone; M64's question 2 is closed rather than spun out.
- **M64-D4 (2026-07-26, RR12 §6) — N–B and `sd = "raw"` stay unavailable-with-reason,** and the brief's premise is corrected in the strengthening direction: `cronbach_alpha()` runs on **raw** scores, so an R̂ reconstruction would silently swap covariance alpha for standardized alpha under the same column name — the swap RR09 rec. 4 exists to prevent.
- **M64-D5 (2026-07-26) — BC1–BC16 bind the BUILD, not M64.** M64 ships no code, so its header keeps `Driving RR: —`; the archived RR12 `## Binding criteria` section is the single authority, and the build sets `Driving RR: RR12` and ingests all sixteen verbatim. T3's "record for the build to ingest" is discharged by that pointer, not by copying the criteria here — a second copy is a drift vector, and `cairn_validate`'s string-compare exists to keep one.
- **M64-D6 (2026-07-26) — correction to RR12's V-F evidence note, verified independently.** RR12 says lavaan fabricates a never-jointly-observed moment "silently … no error and no warning". It does fabricate it (independently reproduced: r(1,4) = 0 against a population 0.3475), but lavaan 0.6.21 **does** warn ("some pairwise combinations have zero coverage … use `lavInspect(fit, \"coverage\")`"). The finding stands and BC7(iii) is unchanged, because `axes_reliability()` fits inside `suppressWarnings()` (`R/axes_reliability.R:1069`), making it silent *in this function*. Two consequences for the build: use `lavInspect(fit, "coverage")` as the coverage source for BC7(iii)/BC8 rather than hand-rolling one, and do not restate RR12's "no warning" wording. RR12 itself is history and stays unedited (IP4).
- **M64-D7 (2026-07-26) — recommendation triage.** Apply: recs 1–7 (all sixteen BCs, via the build). Consider, each routed: fold the M1/M2 reviewer probes into a committed probe → the build, whose BC11/BC12 tests must implement them anyway (a devel/ duplicate would be a second record; RR12 states both mechanisms with seeds inline, so its evidence is already reproducible); a soft minimum-coverage warning threshold → build's discretion, BC8 already binds *reporting* it; one doc sentence on the finite-sample diagonal departure → build's discretion; planned-missingness support (B-4) and lavaan `missing = "two.stage"` → one candidate row at T4. Reject: all four of rec. 9's rejections stand as RR12 states them, and none contradicts a standing entry — D-026's equal-errors rejection generalizes to the determined-errors constraint, and RR09 BC13 is upheld (R̂ is a saturated FIML estimate, not a pairwise matrix).
- **M64-D8 (2026-07-26) — cost flagged for the build, not a deviation.** BC10 and BC13 ask ≥ 200 replicates per cell, i.e. several hundred FIML fits on 24 items. That will not fit an ordinary `devtools::test()` run; the build must solve it (a `devel/` oracle run with committed results, or `skip_on_cran`), and raise a "Deviations from RR12" row only if it cannot meet the replicate count at all.

## Review
