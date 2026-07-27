# M64: FIML on items for `axes_reliability()` — the estimator-metric question

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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
      one-stage vs two-stage agreement (ξ1 and its SE within 5e-4 at 2/5/10%
      per-item MCAR), and the deletion-cost shares (p = 64 → 0.53 / 0.27 /
      0.038 complete cases at 1 / 2 / 5%).
- [ ] **AC2** `cairn/reviews/RB12-*.md` is self-contained: it states the
      question, all four AC1 findings with the command producing each, the
      three design positions fixed at this gate, and what a verdict must decide
      — the metric, what the reported components mean under FIML, and whether
      the FIML-consistent correlation matrix is the right input for the OLS
      cross-check (`axes_ols_shadow()`) and the positive-definiteness refusal.
- [ ] **AC3** `cairn/reviews/RR12-*.md` is committed and ingested per
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

- [ ] **T1** — Lift the three plan-gate scratchpad probes into
      `devel/m64-fiml-probe.R`: one section per finding, seed-pinned, each
      printing the figure AC2 quotes. Fix probe 1's dead `pe$free` filter
      (`parameterEstimates()` has no `free` column, so the intercept count read
      0 — the LESSONS (f) family) by counting `coef()` names.
- [ ] **T2** — Author RB12 via `/milestone-brief` from T1's output, carrying the
      four findings, the three fixed positions, and the verdict questions in
      AC2. Note for the brief: `ssm_sem()` already exports
      `missing = c("listwise", "fiml")` (`R/ssm_sem.R:1303`), so the argument
      spelling is precedent, not a proposal.
- [ ] **T3** — Ingest RR12: disposition every recommendation, and record the
      binding criteria verbatim for the build to ingest.
- [ ] **T4** — Author the GO/NO-GO D-entry; update the ROADMAP per AC5 and
      graduate FIML off the axes-reliability extensions row.

## Work log

- 2026-07-26: created by /milestone-plan. Jeff chose escalation over a direct build at the gate, so this milestone produces the verdict and the build is planned from RR12 (the M53→M54 shape). D-026's FIML clause is a *deferral*, not a rejection (tracking-rules: "not now" is a ROADMAP fact), so planning this needs no supersession; the scope-admission + deferral takeup belong to the GO entry at T4. Investigation dissolved two of the candidate row's three worries: lavaan frees all 24 item intercepts itself under `missing = "ml"` (npar 27→51, df unchanged at 273), so the mean structure is saturated and imposes nothing; and the unit-variance concern is not FIML-specific — the complete-data fit's implied per-item variances already depart from 1 by up to 4.6%, against 4.8–6.5% under FIML, with the reported components still summing to 0.9994. What survives is the metric question RB12 asks.

## Decisions

## Review
