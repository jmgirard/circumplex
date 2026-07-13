<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M16: Print-independent, scale-free displacement-certification rule

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m16-cert-rule-replacement · https://github.com/jmgirard/circumplex/pull/40   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Replace the display-coupled `round(a_lci, digits) > 0` displacement-interpretability
guardrail with a print-independent, scale-free certification rule — its form
decided via a Fable Review Brief seeded by `ssm_ci_accuracy()`'s false-certification
output — propagated consistently across every certification surface, in time for v2.0.0.

## Scope

**In:**
- Redesign `ssm_certified()` (`R/ssm_oop.R:122`) as a print-independent,
  scale-free rule. The form is left open at plan time (property-based criteria)
  and decided by a Fable RB seeded by the diagnostic's amplitude-ladder
  false-certification output; the leading candidate family is a relative rule
  (`a_lci` as a fraction of the amplitude CI width — spec §12.5).
- Keep every certification surface consistent under the new rule (M15
  invariant): `print.circumplex_ssm()` note, `ssm_ci_accuracy()`
  certification-conditional coverage + false-certification, verdict wording,
  guardrail `Cert_rate`, and the plot "Displacement (certified)" panel.
- Update `ssm_ci_accuracy()`'s guardrail `digits` argument and the output text
  echoing the old scale-dependent threshold (`0.5·10^−digits` amplitude units),
  both meaningless under a print-independent rule.
- NEWS entry for the (major-version) behavior change; docs/pkgdown consistent.

**Out:**
- The amplitude/displacement estimators and CI methods — untouched; the rule
  reads `a_lci` and the amplitude CI, it does not recompute them.
- The model-fit (R² < .70) guardrail — unchanged; only the amplitude ⇒
  displacement certification rule is in scope.
- Z&W option (b) lookup module (spec §6) — stays a requirements record.
- Per-group CPM structure / `ssm_ci_accuracy.circumplex_cpm` (spec §12.3/§12.4)
  → a separate future cut, not this milestone.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] **AC1 (print-independence).** `ssm_certified()` no longer consumes a
      display `digits` argument, and certification of a given fit is invariant
      to print's `digits`. Evidence: property test — one fit certified
      identically across `digits ∈ {2, 3, 5}`.
- [x] **AC2 (scale-invariance).** Certification is unchanged under a positive
      rescaling of the score metric (amplitude and its CI scale together).
      Evidence: property test — rescaling input scores by `k > 0` flips no
      profile's certification.
- [x] **AC3 (form decided by review).** The rule's form is decided by a Fable
      RB seeded by `ssm_ci_accuracy()`'s amplitude-ladder false-certification
      output and recorded as a D-entry citing the RR
      (RB tripwire: no-oracle | irreversible-api). Evidence: RB/RR pair in
      `cairn/reviews/` + a `cairn/DECISIONS.md` entry naming the chosen form
      and the false-certification target.
- [x] **AC4 (calibration).** Under the new rule the false-certification rate at
      a truly-zero amplitude (`c = 0`), measured by `ssm_ci_accuracy()`, meets
      the target set in the AC3 D-entry, and certification-conditional
      displacement coverage stays adequate when certified. Evidence:
      simulation-coverage run via `ssm_ci_accuracy()` at `c = 0` and `c > 0`
      (oracle type: simulation-coverage; spec §4.3).
- [x] **AC5 (all surfaces consistent).** All five certification surfaces reflect
      the new rule and stay mutually consistent (M15 invariant): print note,
      `ssm_ci_accuracy()` coverage-conditional/false-cert, verdict wording,
      `Cert_rate`, plot "Displacement (certified)" panel. Evidence: refreshed
      snapshots + a cross-surface consistency test.
- [x] **AC6 (diagnostic API + docs).** `ssm_ci_accuracy()`'s guardrail `digits`
      argument and threshold-echo output are updated/removed to match the
      print-independent rule; NEWS documents the print behavior change; docs
      and `_pkgdown.yml` consistent. Evidence: NEWS entry, `grep` shows no
      stale `digits`/threshold surface.
- [x] **AC7.** `devtools::check()` clean (0 errors / 0 warnings / 0 notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T3, T4
- AC2 → T3, T4
- AC3 → T1, T2
- AC4 → T5
- AC5 → T6
- AC6 → T5, T7
- AC7 → T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1** — Assemble the RB seed (`ssm_ci_accuracy()` false-cert across the
      ladder × ≥2 metrics; candidate rule + target). `(RB tripwire: no-oracle | irreversible-api)`
      → `devel/m16-cert-rule-seed.{R,rds,md}`.
- [x] **T2** — Escalate via `/milestone-brief` to decide the form + target;
      ingest, record `cairn/DECISIONS.md`. `(RB tripwire: no-oracle)` → RB03/RR03,
      **D-007**: `r = a_lci/(a_uci − a_lci) ≥ 0.35`.
- [x] **T3** — Test-first (red): AC1 print-independence, AC2 scale-invariance,
      RR03 §Beyond-4 boundary battery (0°/360° angle-blind, `NA`→FALSE,
      zero-width→FALSE), and the near-zero certified→not-certified regression.
- [x] **T4** — Implement D-007 in `ssm_certified(a_lci, a_uci, k = 0.35)`
      (`R/ssm_oop.R`); drop `digits`; move the print call site + reword the
      note off "amplitude CI includes zero" (no significance-test framing).
- [x] **T5** — Verify AC4 two-part gate via `ssm_ci_accuracy()` (reps=1000,
      5 configs) + Rayleigh oracle (2nd type); remove the vestigial `digits`
      arg + `Threshold` column, echo `k`, rewrite guardrail wording; commit the
      `devel/` verification script.
- [x] **T6** — Propagate to the remaining surfaces (verdict/`Cert_rate`/plot
      panel) + both vignettes; refresh snapshots; add the AC5 consistency test.
- [x] **T7** — NEWS behavior-change bullet, docs (k pinned to 95% interval),
      full `devtools::check()` (AC6, AC7).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: T7 done → status **review**. NEWS documents the behavior change + near-zero flip; dev `ssm_ci_accuracy` docs updated off old-rule wording; k=0.35 pinned to 95% interval; `check(--no-manual)` clean (0/0/0); no new exports → no pkgdown change.
- 2026-07-12: T6 done. Certification wording rewritten across both vignettes (evaluating-structure narrative now shows the rule working); plot panel data-driven (RR03); AC5 cross-surface consistency test added; suite green. Fixed an errored AC5 draft (aw2009 too small for the CPM fit → jz2017/ASPD).
- 2026-07-12: T5 done. AC4 two-part gate PASSES at reps=1000 across 5 configs (false-cert@c=0 0.008–0.023, all ≤0.05, Caution off); Rayleigh oracle (0.0039) agrees as 2nd type (`devel/m16-cert-rule-verify.*`). Removed vestigial `digits`/`Threshold`, echo `k`, rewrote guardrail wording. Fixed a `cert_k`/loop-var collision (cosmetic echo only; numbers stand). Snapshot refreshed; 2 behavior tests re-pointed to the fixed behavior.
- 2026-07-12: T4 done. `ssm_certified(a_lci, a_uci, k = 0.35)` implements D-007; print note reworded; both call sites moved; `test-ssm_oop` green; OCPD false-cert@c=0 1.000→0.033.
- 2026-07-12: T3 done (tests-first, red before T4): unit rule + AC1/AC2 + near-zero regression + angle-blindness in `test-ssm_oop.R`; confirmed red (angle-blind already green as a structural guard).
- 2026-07-12: T2 done — RR03 ingested. Fable resolved the crux: `r=a_lci/(a_uci−a_lci)` is pivotal at c=0 (Rayleigh), so `r≥0.35` drives false-cert to ≈α/2 (old rule sat at 1.000). Decisions → D-007; RB03/RR03 archived; back to in-progress.
- 2026-07-12: T2 — blocked on RB03 (Fable escalation, no-oracle|irreversible-api). Deviation: kept the RB/RR cycle on-branch (T1 already committed there; committing the brief to master would diverge the milestone file).
- 2026-07-12: T1 done. Seed (`devel/m16-cert-rule-seed.*`, reps=500) shows the old rule false-certifies a zero amplitude 100% across all metrics (structural); scale-free `a_lci/CI_width` separates signal (2.58, 6.24) from noise (0.10). Candidate `a_lci/CI_width ≥ k`, target ≤ α/2.
- 2026-07-12: created by /milestone-plan. Promotes the guardrail cert-rule candidate; targets v2.0.0 (M7 gains Depends on: M16); form left open (property ACs), decided via Fable RB; both no-oracle + irreversible-api tripwires. Grounded in `devel/m4-ci-accuracy-spec.md` §3.4/§4.3/§12.5/§13.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (RR03 ingest): rule form + target decided → **D-007** (promoted
  cross-cutting). Rule `r = a_lci/(a_uci − a_lci) ≥ 0.35`, CI-pair-pure,
  contrast rows ungated (M15-D1).
- 2026-07-12 (RR03 triage): RR03 recommended *deprecating*
  `ssm_ci_accuracy(digits=)`; **corrected to clean removal** — the diagnostic
  is unreleased (new in the dev line toward v2.0.0; latest CRAN v1.2.0), so no
  lifecycle shim is owed. Confirms the implement-gate default.
- 2026-07-12 (RR03 triage): all other RR03 recommendations applied
  (form (a)/k=0.35, two-part target, wording rewrite, boundary tests,
  Rayleigh-tail oracle cross-check); replicate-vector/ROPE and form (b)
  rejected with RR03's reasons; k pinned to interval=0.95 with the
  k(interval) generalization deferred (noted in T7/D-007).

## Review
<!-- owner: review · exclusive -->

**PR:** https://github.com/jmgirard/circumplex/pull/40 (branch `m16-cert-rule-replacement`).

### Acceptance-criteria evidence (fresh, 2026-07-12)
- **AC1** ✓ `test-ssm_oop.R` "print-independent and scale-free (AC1, AC2)"
  passes; `formals(ssm_certified)` has no `digits`; note presence identical
  across `digits ∈ {2,3,5}`.
- **AC2** ✓ same test: ×1000 score rescale leaves the note verdict identical.
- **AC3** ✓ `cairn/reviews/archive/RB03,RR03` present; `D-007` records the form
  (`r = a_lci/(a_uci−a_lci) ≥ 0.35`) + two-part target.
- **AC4** ✓ `devel/m16-cert-rule-verify.rds` (reps=1000, 5 configs):
  false-cert@c=0 0.008–0.023 (all ≤0.05, below 0.025 benchmark), Caution off
  everywhere; closed-form Rayleigh oracle 0.0039 agrees as 2nd type.
- **AC5** ✓ `test-ci_accuracy.R` "single-sourced across print and the
  diagnostic (AC5)" passes; `ci_accuracy` snapshot refreshed; both vignettes
  updated.
- **AC6** ✓ `grep` finds 0 cert-`digits`/`Threshold` refs in
  `R/ssm_ci_accuracy.R`; NEWS behavior-change bullet present.
- **AC7** ✓ `devtools::check(--no-manual)` clean: 0 errors / 0 warnings / 0 notes.

### Consistency gate
- `cairn_validate.py`: all checks pass (fixed an over-cap milestone file at
  review by compressing the work-log/tasks to the one-line rule: 230 → 145).
- Coverage completeness: PASS (every AC maps to ≥1 existing task).
- Toolchain (R profile): `devtools::check()` clean (above); full `test_dir`
  0 failed / 0 error.
- No DESIGN.md principle touched (Principles slot `—`) → impact scan skipped.
