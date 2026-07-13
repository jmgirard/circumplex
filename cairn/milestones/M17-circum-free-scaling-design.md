<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M17: CIRCUM free-scaling — Fable-reviewed design decision + spec

- **Status:** blocked
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (no formal IP/GP ids yet; works under DESIGN.md "Statistical conventions" and "CPM confidence intervals: measured coverage")
- **Branch/PR:** m17-circum-free-scaling-design

## Goal

Produce a Fable-reviewed go/no-go decision and, if go, a complete implementable
spec for adding a free-scaling covariance estimation family (`Σ = D_σ P(γ) D_σ`)
to `cpm_fit()`, so that M18 can build it without re-opening design questions.

## Scope

**In:**
- A Review Brief (RB) escalating the design to Fable (`/milestone-brief`),
  self-contained, covering: (a) the go/no-go question — does the reproduction
  value of exact published-CIRCUM/CircE output justify a second fitted family
  (the legacy "decide post-M4" question, legacy ROADMAP CIRCUM entry); (b) the
  σ (scale-factor) parameterization — unconstrained map + Jacobian, and its
  identification (σ̂ = 1 at the correlation optimum is the nesting anchor, but
  σ is free at finite N; `devel/m4-browne-design.md` §11); (c) the covariance
  discrepancy `F` on `Σ` and, critically, the **new analytic gradient** — the
  current gradient's simplification "the parameterization holds `diag P = 1`
  fixed, only off-diagonal `∂P_ij` enter" (`devel/m4-browne-design.md:296`) no
  longer holds once σ is free, so the diagonal derivatives must be derived; (d)
  df / χ² / CI treatment for the extended parameter set (and whether the §3.2
  scale-invariance argument that justifies the analytic CIs still applies to
  the free family, or whether bootstrap is the only trustworthy path); (e)
  canonicalization/identification interaction with the existing reflection and
  angle canonicalization.
- Ingesting the resulting Review Report (RR) into `cairn/reviews/`.
- A written design spec (new `devel/circum-free-scaling-spec.md`, or a §-addendum
  to `devel/m4-browne-design.md`) that an implementer can build from without
  further derivation, and that names the validation plan concretely.
- A `cairn/DECISIONS.md` entry recording the go/no-go and its rationale.

**Out:**
- Any implementation, tests, or `cpm_fit()` code changes → M18.
- Multi-group free-scaling, OLS/GLS/ADF discrepancies, polychoric input →
  remain out of the CIRCUM scope entirely (documented, not promised;
  `devel/m4-browne-design.md` §8).

## Acceptance criteria

- [ ] A self-contained Review Brief escalating the free-scaling design (with the
      (a)–(e) targets above) exists, and its Review Report is ingested under
      `cairn/reviews/` — evidence: the RB and RR files.
- [ ] A `cairn/DECISIONS.md` entry records the Fable-attested go/no-go on the
      second fitted family, with rationale weighing reproduction value against a
      second estimation family. (No-go is a valid outcome: it retires M18 and the
      CIRCUM candidate.)
- [ ] **If go:** a written spec exists specifying, at implementable detail: the σ
      parameterization (map + Jacobian) and identification; the covariance
      discrepancy `F(S, Σ)`; the free-family analytic gradient (the diagonal
      terms derived, replacing the `diag P = 1` simplification); the df/χ²/CI
      treatment (including whether analytic CIs are trustworthy or bootstrap is
      mandated); and the validation plan naming the OpenMx free-scaling oracle
      (already in `tests/testthat/test-cpm_oracles.R`) and the Grassi et al.
      (2010, Appendix A) published CircE targets with tolerances. Spec criteria
      that cite a value name their source.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3

## Tasks

- [x] **T1** — Draft the Review Brief per `/milestone-brief`: assemble the
      free-scaling model context (`devel/m4-browne-design.md` §3.2, §11; the
      existing OpenMx free-scaling oracle in `test-cpm_oracles.R`), state the
      (a)–(e) design questions, and frame the gradient re-derivation as the
      central statistical-correctness risk. (RB tripwire: ip-touching — this task
      *is* the escalation.)
- [ ] **T2** — Escalate to Fable; ingest the RR into `cairn/reviews/`.
- [ ] **T3** — Author `devel/circum-free-scaling-spec.md` (or the §-addendum)
      from the RR: parameterization, discrepancy, gradient (with diagonal terms),
      identification/canonicalization, df/χ²/CI treatment, validation plan.
- [ ] **T4** — Record the go/no-go decision in `cairn/DECISIONS.md`
      (extend/supersede as the RR dictates); if no-go, retire M18 and the CIRCUM
      candidate in the same commit.

## Work log

- 2026-07-12: T1 done — drafted `cairn/reviews/RB04-circum-free-scaling.md`
  (self-contained: model context, the §11/B6 refutation of σ̂=1, the OpenMx
  free-scaling oracle at `test-cpm_oracles.R:329`, Grassi et al. 2010 App. A
  targets; six questions (a)–(e)+spec-adequacy, gradient re-derivation framed
  as the central risk). Status → blocked on RB04 pending Fable escalation.
- 2026-07-12: created by /milestone-plan. Promoted from the "CIRCUM free-scaling
  compatibility mode" ROADMAP candidate (legacy ROADMAP continuous track,
  surfaced 2026-07-06 by M4/B6 published-oracle triage). Split design-from-build
  per the sizing tripwires (greenfield, Fable-gated estimator family): this
  milestone is the design gate, M18 the build. In v2.0.0 scope per D-008 (which
  supersedes D-001's new-features-excluded clause for CIRCUM and removes the
  release date as a constraint).

## Decisions

## Review
