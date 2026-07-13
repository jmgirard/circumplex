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

- 2026-07-12: review — PR #41; ACs 1–3 verified with fresh evidence;
  cairn_validate clean; 3-lens independent review found one spec transcription
  slip (κ(Σ) → Hessian condition number, scored 92), fixed on branch.
- 2026-07-12: T4 done — recorded **GO** as `cairn/DECISIONS.md` D-009 (extends
  D-008; supersedes nothing). M18 stays `planned`. All tasks complete; no R/src
  code touched (docs/design-only), so the r-package test/check verify slot has
  no runtime surface to exercise — not re-run (nothing changed). Status → review.
- 2026-07-12: T3 done — authored `devel/circum-free-scaling-spec.md`
  (build-ready: model, σ map+Jacobian+identification, discrepancy, full
  free-family gradient with the σ block and Ã substitution, df/χ²/CI treatment
  incl. no-analytic-σ-CI + coverage-oracle gate, canonicalization pins,
  validation plan naming the OpenMx oracle + Grassi App. A targets, deferred
  items). Chose a new standalone spec file over a §-addendum (design doc already
  ~930 lines). Added a §11 change-log pointer in `m4-browne-design.md`.
- 2026-07-12: T2 done — spawned Fable (user-approved), ingested RR04. Verdict
  **GO**, gradient FD-verified (worst err 3.6e-9). Archived RB04/RR04 pair →
  `cairn/reviews/archive/`; status blocked → in-progress. Answers recorded in
  Decisions below; formal go/no-go D-entry lands at T4.
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

- 2026-07-12 (RR04, archived): Fable answers to the (a)–(e) brief targets.
  Cross-cutting go/no-go promoted to DECISIONS.md **D-009** (T4). Key findings:
  - **(a) GO.** Reproducing published CIRCUM/CircE output requires actually
    fitting σ (B6 proved the diag family cannot); extension found statistically
    tame, validation anchor already green.
  - **(b) σ = e^{s}**, all p free, **no identification pin** (map injective, F
    coercive in each σ_i). σ̂=1 only at perfect fit; finite-N ML preserves
    diag(Σ̂⁻¹R)=1, not diag Σ̂ — the precise content of the B6 refutation.
  - **(c) Gradient** `∂F/∂s_i = 2(1 − (Σ⁻¹R)_ii)`; γ blocks = §3.4 verbatim
    with A → Ã = D_σ A D_σ and A from Σ⁻¹ (not P⁻¹). "Only off-diagonal ∂P"
    still holds for θ/ζ/β; moving diagonal lives in the σ block. FD-verified.
  - **(d) df UNCHANGED** (covariance moment count p(p+1)/2; the brief's "df
    shrinks" premise was wrong). Wald-CI invariance argument is *stronger* for
    the free family (Cudeck exact); **no analytic σ CIs ever**; bootstrap stays
    default; free-family coverage-oracle extension is a mandatory pre-ship gate.
  - **(e)** σ invariant under rotation+reflection; canonicalization untouched;
    5 mechanical layout/guard pins (notably s block *before* β).
  - **Beyond brief:** §3.2's χ²-validity-via-scale-invariance claim is wrong
    for the *diag* family (its true home is the free family) — rewrite at M18
    doc time; ΔT is not a calibrated σ=1 test; T_diag-vs-T_free calibration is
    a *consider* future measurement (not this milestone).

## Review

**2026-07-12 (PR #41).** Design-gate milestone; diff touches **zero package
files** (only `cairn/` + `devel/` markdown), so the built package is
byte-identical to master.

Acceptance-criteria evidence (fresh):
- **AC1 — RB + RR ingested.** `cairn/reviews/archive/RB04-circum-free-scaling.md`
  and `RR04-circum-free-scaling.md` both present; working `cairn/reviews/` holds
  only `archive/`. RB covers all (a)–(e) targets; RR answers 1–6 with a GO
  verdict. ✓
- **AC2 — go/no-go recorded.** `cairn/DECISIONS.md` D-009 (line 188), "Decision:
  GO", rationale weighs reproduction value (diag family provably cannot
  reproduce published output) against the second-family cost (found tame).
  Extends D-008; M18 stays planned. ✓
- **AC3 — implementable spec.** `devel/circum-free-scaling-spec.md`: σ map +
  Jacobian + identification (§2), covariance discrepancy + full free-family
  gradient with the σ block and diagonal terms derived (§3), df/χ²/CI treatment
  incl. no-analytic-σ-CI + coverage-oracle gate (§4), canonicalization pins
  (§5), and a validation plan (§6) naming the OpenMx free-scaling oracle
  (`test-cpm_oracles.R:329`) and Grassi et al. (2010) App. A targets with
  same-model tolerances. Value-citing criteria name their source (RR04 §n,
  Grassi App. A). ✓

Consistency gate:
- `cairn_validate.py` — all 14 checks PASS (exit 0), incl. coverage-complete,
  single-in-progress, weight caps, terminal-row retention.
- Coverage completeness — AC1→T1,T2 / AC2→T3,T4 / AC3→T3; all mapped tasks
  exist. (cairn_validate "coverage complete" PASS.)
- `cairn_impact` — skipped (no IP/GP principle changed; header "Principles
  touched: —").
- Toolchain (r-package) consistency-gate — no package files changed, so the
  build is unchanged from green master; full R CMD check runs on PR #41 CI
  (required green at merge).

Independent fresh-context review (3 lenses + scorer):
- **[O] diff-bug** — verified the spec faithfully distills RR04 on every
  load-bearing point (σ-block gradient, Ã substitution, df-unchanged, no-σ-CI,
  layout pin, fixture arithmetic) and D-009 doesn't contradict D-008/D-001.
  One finding (scored **92**, actioned): spec §2 conditioning caveat wrote
  "κ(Σ)" where RR04 §2 / design-doc §2.5 mean the **Hessian** condition number
  (distinct quantity) — internally inconsistent with its own §2.5 cross-ref.
  **Fixed on branch** (`circum-free-scaling-spec.md` §2 now says Hessian, with
  the κ(Σ) distinction spelled out).
- **[S] blame-history** — no findings; the §11 change-log addition builds on
  (not contradicts) the 2026-07-06 B6 entry, D-009 properly extends D-008,
  all append-only.
- **[S] prior-PR-comments** — no prior-PR evidence (touching commits predate
  GitHub PRs; tracking files carry no line review comments).
- No findings scored below 80 (only one finding total).
