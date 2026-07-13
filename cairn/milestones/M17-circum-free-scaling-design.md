<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M17: CIRCUM free-scaling — Fable-reviewed design decision + spec

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Principles touched:** — (no formal IP/GP ids yet; works under DESIGN.md "Statistical conventions" and "CPM confidence intervals: measured coverage")
- **Branch/PR:** m17-circum-free-scaling-design · PR #41

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

- [x] A self-contained Review Brief escalating the free-scaling design (with the
      (a)–(e) targets above) exists, and its Review Report is ingested under
      `cairn/reviews/` — evidence: the RB and RR files.
- [x] A `cairn/DECISIONS.md` entry records the Fable-attested go/no-go on the
      second fitted family, with rationale weighing reproduction value against a
      second estimation family. (No-go is a valid outcome: it retires M18 and the
      CIRCUM candidate.)
- [x] **If go:** a written spec exists specifying, at implementable detail: the σ
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
- [x] **T2** — Escalate to Fable; ingest the RR into `cairn/reviews/`.
- [x] **T3** — Author `devel/circum-free-scaling-spec.md` (or the §-addendum)
      from the RR: parameterization, discrepancy, gradient (with diagonal terms),
      identification/canonicalization, df/χ²/CI treatment, validation plan.
- [x] **T4** — Record the go/no-go decision in `cairn/DECISIONS.md`
      (extend/supersede as the RR dictates); if no-go, retire M18 and the CIRCUM
      candidate in the same commit. → **GO** (D-009); M18 stays planned.

## Work log

_(Verbose same-day entries compressed to meet the 150-line cap; full detail in
git, D-009, RR04, and the spec.)_

- 2026-07-12: review — PR #41; ACs 1–3 verified; cairn_validate clean; 3-lens
  review found one spec slip (κ(Σ) → Hessian, scored 92), fixed on branch.
- 2026-07-12: T4 — recorded **GO** as DECISIONS.md D-009 (extends D-008); M18
  stays planned. Docs/design-only (no R/src), so verify slot not exercised. → review.
- 2026-07-12: T3 — authored `devel/circum-free-scaling-spec.md` (new standalone
  file over §-addendum); added a §11 change-log pointer in `m4-browne-design.md`.
- 2026-07-12: T2 — spawned Fable (user-approved), ingested RR04 (GO; gradient
  FD-verified 3.6e-9); archived RB04/RR04 pair; blocked → in-progress.
- 2026-07-12: T1 — drafted RB04 (self-contained; (a)–(e)+spec-adequacy, gradient
  re-derivation as the central risk). → blocked pending Fable.
- 2026-07-12: created by /milestone-plan from the CIRCUM free-scaling candidate;
  design-from-build split per sizing tripwires (this = design gate, M18 = build);
  v2.0.0 scope per D-008.

## Decisions

- 2026-07-12 (RR04, archived; full detail in DECISIONS.md **D-009** + RR04):
  Fable answered the (a)–(e) targets. **(a) GO** — reproducing published output
  requires fitting σ (B6 proved the diag family cannot); extension tame, anchor
  green. **(b)** σ = e^{s}, all p free, **no identification pin**; σ̂=1 only at
  perfect fit (finite-N ML preserves diag(Σ̂⁻¹R)=1). **(c)** `∂F/∂s_i =
  2(1−(Σ⁻¹R)_ii)`; γ blocks = §3.4 with A → Ã = D_σ A D_σ (A from Σ⁻¹, not P⁻¹);
  FD-verified. **(d)** df **unchanged**; **no analytic σ CIs**; bootstrap
  default; coverage-oracle extension a pre-ship gate. **(e)** σ invariant under
  rotation+reflection; 5 layout/guard pins (s before β). Beyond brief: rewrite
  design §3.2 at M18 doc time; T_diag-vs-T_free calibration a *consider* deferral.

## Review

**2026-07-12 (PR #41).** Design-gate milestone; diff touches **zero package
files** (only `cairn/` + `devel/` markdown) → built package byte-identical to
master.

**AC evidence (fresh):** **AC1** — RB04 + RR04 present under
`cairn/reviews/archive/` (RB covers (a)–(e); RR answers 1–6, GO); working
`reviews/` holds only `archive/`. **AC2** — `DECISIONS.md` D-009 records GO with
reproduction-value-vs-cost rationale; extends D-008; M18 stays planned. **AC3** —
`devel/circum-free-scaling-spec.md` gives σ map+Jacobian+identification (§2), the
full free-family gradient with σ block + diagonal terms (§3), df/χ²/CI treatment
(§4), canon pins (§5), and a validation plan (§6) naming the OpenMx oracle
(`test-cpm_oracles.R:329`) + Grassi (2010) App. A with tolerances; value-citing
criteria name sources. All ✓.

**Consistency gate:** `cairn_validate.py` all checks PASS; coverage complete
(AC1→T1,T2 / AC2→T3,T4 / AC3→T3); `cairn_impact` skipped (no IP/GP change);
toolchain check deferred to PR #41 CI (no package files changed).

**Independent review (3 lenses + scorer):** **[O] diff-bug** confirmed the spec
faithfully distills RR04 (σ-block gradient, Ã substitution, df-unchanged, no-σ-CI,
layout pin, fixture arithmetic) and D-009 doesn't contradict D-008/D-001; one
finding (scored **92**): spec §2 wrote "κ(Σ)" for the **Hessian** condition
number (distinct quantity, inconsistent with its §2.5 cross-ref) — **fixed on
branch**. **[S] blame-history** — clean, append-only, builds on the B6 entry.
**[S] prior-PR-comments** — no prior-PR evidence. No sub-80 findings.
